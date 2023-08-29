#include <lua.h>
#include <lauxlib.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#define LIBR_IMPLEMENTATION
#include "r.h"

#define UDTYPE_CONNECTION "x11"
#define UDTYPE_XRANDR "xrandr"
#define UDTYPE_XRANDR_OUTPUT "xrandr.output"
#define UDTYPE_XRANDR_SETUP "xrandr.setup"
#define UDTYPE_XRANDR_MONITOR "xrandr.monitor"
#define UDTYPE_XRANDR_MODE "xrandr.mode"

struct connection {
    Display* dpy;

    int scr;
    Window root;

    struct xrandr* xrandr;
};

struct xrandr {
    struct connection* con;

    int major;
    int minor;
};

static int output_mk(lua_State* L, RROutput xid, XRROutputInfo* oi)
{
    luaR_stack(L);

    lua_pushstring(L, oi->name);

    lua_createtable(L, 0, 9);
    if(luaL_newmetatable(L, UDTYPE_XRANDR_OUTPUT)) {
    }
    lua_setmetatable(L, -2);

    lua_pushvalue(L, -2);
    lua_setfield(L, -2, "name");

    switch(oi->connection) {
        case RR_Connected: lua_pushboolean(L, 1); break;
        case RR_Disconnected: lua_pushboolean(L, 0); break;
        case RR_UnknownConnection:
        default: lua_pushnil(L); break;
    }
    lua_setfield(L, -2, "connected");

    lua_pushinteger(L, xid);
    lua_setfield(L, -2, "xid");

    luaR_return(L, 2);
}

static int monitor_mk(lua_State* L, struct xrandr* xrandr, int output_index, const XRRMonitorInfo* mi)
{
    luaR_stack(L);

    char* name = XGetAtomName(xrandr->con->dpy, mi->name);
    if(!name) {
        failwith("XGetAtomName(%ld)", mi->name);
    }
    lua_pushstring(L, name);

    lua_createtable(L, 0, 10);

    if(luaL_newmetatable(L, UDTYPE_XRANDR_MONITOR)) {
    }
    lua_setmetatable(L, -2);

    lua_pushvalue(L, -2);
    lua_setfield(L, -2, "name");

    if(mi->noutput > 0) {
        lua_pushboolean(L, 1);
        lua_setfield(L, -2, "active");

        lua_pushinteger(L, mi->x);
        lua_setfield(L, -2, "x");

        lua_pushinteger(L, mi->y);
        lua_setfield(L, -2, "y");

        lua_pushinteger(L, mi->width);
        lua_setfield(L, -2, "width");

        lua_pushinteger(L, mi->height);
        lua_setfield(L, -2, "height");

        lua_pushinteger(L, mi->mheight);
        lua_setfield(L, -2, "mheight");

        lua_pushinteger(L, mi->mwidth);
        lua_setfield(L, -2, "mwidth");

        lua_createtable(L, mi->noutput, 0);
        for(int i = 0; i < mi->noutput; i++) {
            const RROutput oid = mi->outputs[i];

            lua_pushnil(L);
            while(lua_next(L, output_index - 4) != 0) {
                if(lua_getfield(L, -1, "xid") != LUA_TNUMBER) {
                    failwith("unexpected type");
                }
                lua_Integer xid = lua_tointeger(L, -1);
                if(oid == xid) {
                    lua_pop(L, 1);
                    lua_rawseti(L, -3, i + 1);
                    lua_pop(L, 1);
                    break;
                } else {
                    lua_pop(L, 2);
                }
            }
        }
        lua_setfield(L, -2, "outputs");
    } else {
        lua_pushboolean(L, 0);
        lua_setfield(L, -2, "active");
    }

    lua_pushboolean(L, mi->primary);
    lua_setfield(L, -2, "primary");

    luaR_return(L, 2);
}

static void figure_out_and_push_refresh_rate(lua_State* L, const XRRModeInfo* mi)
{
    // https://cgit.freedesktop.org/xorg/app/xrandr/tree/xrandr.c?h=xrandr-1.5.2#n553

    if(!mi->hTotal || !mi->vTotal) {
        lua_pushnil(L);
        return;
    }

    lua_Number q = 1.0;

    if(mi->modeFlags & RR_DoubleScan) {
        q *= 2;
    }

    if(mi->modeFlags & RR_Interlace) {
        q /= 2;
    }

    lua_pushnumber(L, (lua_Number)mi->dotClock / (q * (lua_Number)mi->hTotal * (lua_Number)mi->vTotal));
}

static int mode_mk(lua_State* L, const XRRModeInfo* mi)
{
    luaR_stack(L);

    lua_pushinteger(L, mi->id);

    lua_createtable(L, 0, 11);

    if(luaL_newmetatable(L, UDTYPE_XRANDR_MODE)) {
    }
    lua_setmetatable(L, -2);

    lua_pushvalue(L, -2);
    lua_setfield(L, -2, "id");

    lua_pushinteger(L, mi->width);
    lua_setfield(L, -2, "width");
    lua_pushinteger(L, mi->height);
    lua_setfield(L, -2, "height");

    lua_pushinteger(L, mi->dotClock);
    lua_setfield(L, -2, "dotclock");
    lua_pushinteger(L, mi->hTotal);
    lua_setfield(L, -2, "htotal");
    lua_pushinteger(L, mi->vTotal);
    lua_setfield(L, -2, "vtotal");

    figure_out_and_push_refresh_rate(L, mi);
    lua_setfield(L, -2, "refresh_rate");

    // mode flags
    XRRModeFlags flags = mi->modeFlags;

    if((flags & RR_HSyncPositive) && !(flags & RR_HSyncNegative)) {
        lua_pushinteger(L, 1);
    } else if(!(flags & RR_HSyncPositive) && (flags & RR_HSyncNegative)) {
        lua_pushinteger(L, -1);
    } else {
        debug("mode %lu does not specify hsync sign", mi->id);
        lua_pushnil(L);
    }
    flags &= ~(RR_HSyncPositive | RR_HSyncNegative);
    lua_setfield(L, -2, "hsync_sign");

    if((flags & RR_VSyncPositive) && !(flags & RR_VSyncNegative)) {
        lua_pushinteger(L, 1);
    } else if(!(flags & RR_VSyncPositive) && (flags & RR_VSyncNegative)) {
        lua_pushinteger(L, -1);
    } else {
        debug("mode %lu does not specify vsync sign", mi->id);
        lua_pushnil(L);
    }
    flags &= ~(RR_VSyncPositive | RR_VSyncNegative);
    lua_setfield(L, -2, "vsync_sign");

    lua_pushboolean(L, flags & RR_Interlace ? 1 : 0);
    flags &= ~RR_Interlace;
    lua_setfield(L, -2, "interlace");

    lua_pushboolean(L, flags & RR_DoubleScan ? 1 : 0);
    flags &= ~RR_DoubleScan;
    lua_setfield(L, -2, "double_scan");

    if(flags != 0) {
        warning("mode %lu has unexpected flags: %ld", mi->id, flags);
    }

    luaR_return(L, 2);
}

static int xrandr_fetch_setup(lua_State* L)
{
    luaR_stack(L);
    struct xrandr* xrandr = luaL_checkudata(L, 1, UDTYPE_XRANDR);

    lua_createtable(L, 0, 2);

    if(luaL_newmetatable(L, UDTYPE_XRANDR_SETUP)) {
    }
    lua_setmetatable(L, -2);

    XRRScreenResources* res = XRRGetScreenResources(xrandr->con->dpy, xrandr->con->root);
    if(!res) {
        failwith("XRRGetScreenResources failed");
    }

    // .modes
    lua_createtable(L, 0, res->nmode);
    for(int i = 0; i < res->nmode; i++) {
        mode_mk(L, &res->modes[i]);
        lua_settable(L, -3);
    }
    lua_setfield(L, -2, "modes");

    // .outputs
    lua_createtable(L, 0, res->noutput);
    lua_pushvalue(L, -1);

    for(int i = 0; i < res->noutput; i++) {
        RROutput xid = res->outputs[i];
        XRROutputInfo* oi = XRRGetOutputInfo(xrandr->con->dpy, res, xid);
        if(!oi) {
            failwith("XRRGetOutputInfo(%lu) failed", xid);
        }

        output_mk(L, xid, oi);
        lua_settable(L, -3);

        XRRFreeOutputInfo(oi);
    }

    lua_setfield(L, -3, "outputs");

    // .monitors
    int nmonitors;
    XRRMonitorInfo* mi = XRRGetMonitors(xrandr->con->dpy, xrandr->con->root, False, &nmonitors);
    if(!mi) {
        failwith("XRRGetMonitors failed");
    }

    lua_createtable(L, 0, nmonitors);

    for(int i = 0; i < nmonitors; i++) {
        monitor_mk(L, xrandr, -2, &mi[i]);
        lua_settable(L, -3);
    }

    lua_setfield(L, -3, "monitors");
    lua_pop(L, 1);

    XRRFreeMonitors(mi);
    XRRFreeScreenResources(res);

    luaR_return(L, 1);
}


static int xrandr_index(lua_State* L)
{
    luaR_stack(L);
    struct xrandr* xrandr = luaL_checkudata(L, 1, UDTYPE_XRANDR);
    const char* key = luaL_checkstring(L, 2);

    if(strcmp(key, "version") == 0) {
        char buf[128];
        int r = snprintf(LIT(buf), "%d.%d", xrandr->major, xrandr->minor);
        if(r >= sizeof(buf)) {
            failwith("buffer overflow");
        }
        lua_pushstring(L, buf);
        luaR_return(L, 1);
    } else if(strcmp(key, "fetch") == 0) {
        lua_pushcfunction(L, xrandr_fetch_setup);
        luaR_return(L, 1);
    } else {
        debug("indexing absent key %p[%s]", xrandr, key);
        lua_pushnil(L);
        luaR_return(L, 1);
    }
}

static int xrandr_call(lua_State* L)
{
    return xrandr_fetch_setup(L);
}

static int x11_xrandr(lua_State* L, struct connection* con)
{
    luaR_stack(L);
    if(con->xrandr == NULL) {
        struct xrandr* xrandr = con->xrandr = lua_newuserdatauv(L, sizeof(*xrandr), 0);
        xrandr->con = con;

        if(!XRRQueryVersion(xrandr->con->dpy, &xrandr->major, &xrandr->minor)) {
            failwith("XRRQueryVersion failed");
        }
        debug("Xrandr version: %d.%d", xrandr->major, xrandr->minor);

        if(luaL_newmetatable(L, UDTYPE_XRANDR)) {
            lua_pushcfunction(L, xrandr_index);
            lua_setfield(L, -2, "__index");

            lua_pushcfunction(L, xrandr_call);
            lua_setfield(L, -2, "__call");
        }
        lua_setmetatable(L, -2);

        lua_pushvalue(L, -1);
        lua_setiuservalue(L, 1, 1);
        luaR_return(L, 1);
    } else {
        lua_getiuservalue(L, 1, 1);
        luaR_return(L, 1);
    }
}

static int x11_close(lua_State* L)
{
    luaR_stack(L);
    struct connection* con = luaL_checkudata(L, 1, UDTYPE_CONNECTION);

    debug("closing connection %p", con);

    XCloseDisplay(con->dpy);

    luaR_return(L, 0);
}

static int x11_index(lua_State* L)
{
    luaR_stack(L);
    struct connection* con = luaL_checkudata(L, 1, UDTYPE_CONNECTION);
    const char* key = luaL_checkstring(L, 2);

    if(strcmp(key, "screen") == 0) {
        lua_pushinteger(L, con->scr);
        luaR_return(L, 1);
    } else if(strcmp(key, "root") == 0) {
        lua_pushinteger(L, con->root);
        luaR_return(L, 1);
    } else if(strcmp(key, "xrandr") == 0) {
        return x11_xrandr(L, con);
    } else {
        debug("indexing absent key %p[%s]", con, key);
        lua_pushnil(L);
        luaR_return(L, 1);
    }
}

static int x11_connect(lua_State* L)
{
    luaR_stack(L);
    int argc = lua_gettop(L);

    struct connection* con = lua_newuserdatauv(L, sizeof(struct connection), 1);
    memset(con, 0, sizeof(*con));

    char* d = XDisplayName(argc == 0 ? NULL : luaL_checkstring(L, 1));
    debug("creating connection %p to %s", con, d);

    con->dpy = XOpenDisplay(d);
    if(con->dpy == NULL) luaR_failwith(L, "unable to connect to display %s", d);

    con->scr = XDefaultScreen(con->dpy);
    con->root = XRootWindow(con->dpy, con->scr);

    if(luaL_newmetatable(L, UDTYPE_CONNECTION)) {
        lua_pushcfunction(L, x11_close);
        lua_setfield(L, -2, "__close");

        lua_pushcfunction(L, x11_index);
        lua_setfield(L, -2, "__index");
    }

    lua_setmetatable(L, -2);

    luaR_return(L, 1);
}

int luaopen_x11(lua_State* L)
{
    luaL_checkversion(L);
    luaR_stack(L);

    lua_newtable(L);

    lua_pushcfunction(L, x11_connect);
    lua_setfield(L, -2, "connect");

    luaR_return(L, 1);
}

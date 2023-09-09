#include <string.h>

#include <lua.h>
#include <lauxlib.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#include "r.h"
#include "x11.h"

#define TYPE_CONNECTION "x11"
#define TYPE_XRANDR "xrandr"
#define TYPE_XRANDR_CRTC "xrandr.crtc"
#define TYPE_XRANDR_OUTPUT "xrandr.output"
#define TYPE_XRANDR_OUTPUT_MODES "xrandr.output.modes"
#define TYPE_XRANDR_SETUP "xrandr.setup"
#define TYPE_XRANDR_MONITOR "xrandr.monitor"
#define TYPE_XRANDR_MODE "xrandr.mode"
#define TYPE_XRANDR_SCREEN "xrandr.screen"

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

static int crtc_mk(lua_State* L, int modes_index, RRCrtc id, XRRCrtcInfo* ci)
{
    luaR_stack(L);

    lua_createtable(L, 0, 8);

    if(luaL_newmetatable(L, TYPE_XRANDR_CRTC)) {
    }
    lua_setmetatable(L, -2);

    lua_pushinteger(L, id);
    lua_setfield(L, -2, "id");

    if(ci->mode != None) {
        lua_pushboolean(L, 1);
        lua_setfield(L, -2, "enabled");

        lua_pushinteger(L, ci->x);
        lua_setfield(L, -2, "x");

        lua_pushinteger(L, ci->y);
        lua_setfield(L, -2, "y");

        lua_pushinteger(L, ci->width);
        lua_setfield(L, -2, "width");

        lua_pushinteger(L, ci->height);
        lua_setfield(L, -2, "height");

        lua_pushnil(L);
        while(lua_next(L, modes_index - 2) != 0) {
            if(lua_getfield(L, -1, "id") != LUA_TNUMBER) {
                failwith("unexpected type");
            }
            int r;
            lua_Integer mode_id = lua_tointegerx(L, -1, &r);
            if(!r) {
                failwith("unable to fetch id");
            }
            lua_pop(L, 1);

            if(mode_id == ci->mode) {
                lua_setfield(L, -3, "mode");
                lua_pop(L, 1);
                break;
            } else {
                lua_pop(L, 1);
            }
        }
    } else {
        lua_pushboolean(L, 0);
        lua_setfield(L, -2, "enabled");
    }

    // outputs
    lua_createtable(L, ci->noutput, 1);
    for(int i = 0; i < ci->noutput; i++) {
        lua_pushinteger(L, ci->outputs[i]);
        lua_rawseti(L, -2, i + 1);
    }

    lua_createtable(L, ci->npossible, 0);
    for(int i = 0; i < ci->npossible; i++) {
        lua_pushinteger(L, ci->possible[i]);
        lua_rawseti(L, -2, i + 1);
    }
    lua_setfield(L, -2, "possible");

    lua_setfield(L, -2, "outputs");


    luaR_return(L, 1);
}

static int modes_is_preferred(lua_State* L)
{
    luaR_stack(L);

    luaR_checkmetatable(L, 1, TYPE_XRANDR_OUTPUT_MODES);
    luaR_checkmetatable(L, 2, TYPE_XRANDR_MODE);

    if(lua_getfield(L, 2, "id") != LUA_TNUMBER) {
        failwith("id field not present");
    }

    if(lua_getfield(L, 1, "preferred") != LUA_TTABLE) {
        failwith("preferred field not present");
    }

    lua_pushnil(L);
    while(lua_next(L, -2) != 0) {
        if(lua_getfield(L, -1, "id") != LUA_TNUMBER) {
            failwith("id field not present");
        }
        int r = lua_rawequal(L, -1, -5);
        if(r) {
            lua_pop(L, 5);
            lua_pushboolean(L, 1);
            luaR_return(L, 1);
        } else {
            lua_pop(L, 2);
        }
    }

    lua_pop(L, 2);

    lua_pushboolean(L, 0);
    luaR_return(L, 1);
}

static int output_mk(lua_State* L, int modes_index, int crtcs_index, RROutput id, XRROutputInfo* oi)
{
    luaR_stack(L);

    lua_pushstring(L, oi->name);

    lua_createtable(L, 0, 4);
    if(luaL_newmetatable(L, TYPE_XRANDR_OUTPUT)) {
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

    lua_pushinteger(L, id);
    lua_setfield(L, -2, "id");

    // modes
    lua_createtable(L, oi->nmode, 2);

    if(luaL_newmetatable(L, TYPE_XRANDR_OUTPUT_MODES)) {
        luaL_Reg l[] = {
            { "is_preferred", modes_is_preferred },
            { NULL, NULL },
        };
        luaL_newlibtable(L, l);
        luaL_setfuncs(L, l, 0);

        lua_setfield(L, -2, "__index");
    }
    lua_setmetatable(L, -2);

    lua_createtable(L, oi->npreferred, 0);

    for(int i = 0; i < oi->nmode; i++) {
        RRMode m = oi->modes[i];
        lua_pushinteger(L, m);
        int t = lua_rawget(L, modes_index - 5);
        if(t != LUA_TTABLE) {
            failwith("unexpected type for mode %lu: %s", m, lua_typename(L, t));
        }
        if(i < oi->npreferred) {
            lua_pushvalue(L, -1);
            lua_rawseti(L, -3, i + 1);
        }
        lua_rawseti(L, -3, i + 1);
    }

    lua_setfield(L, -2, "preferred");
    lua_setfield(L, -2, "modes");

    // .crtc and fix crtc.outputs
    lua_pushnil(L);
    while(lua_next(L, crtcs_index - 3) != 0) {
        if(lua_getfield(L, -1, "id") != LUA_TNUMBER) {
            failwith("unexpected type");
        }
        int r;
        lua_Integer crtc_id = lua_tointegerx(L, -1, &r);
        if(!r) {
            failwith("unable to fetch id");
        }
        lua_pop(L, 1);

        if(crtc_id == oi->crtc) {
            lua_pushvalue(L, -1);
            lua_setfield(L, -4, "crtc");
        }

        // fix outputs
        if(lua_getfield(L, -1, "outputs") != LUA_TTABLE) {
            failwith("unexpected type");
        }

        for(int i = 1, t = lua_rawgeti(L, -1, i); t != LUA_TNIL; lua_pop(L, 1), t = lua_rawgeti(L, -1, ++i)) {
            if(t == LUA_TNUMBER) {
                lua_Integer id0 = lua_tointeger(L, -1);
                if(id == id0) {
                    lua_pushvalue(L, -5);
                    lua_rawseti(L, -3, i);
                }
            }
        }
        lua_pop(L, 1);

        // fix outputs.possible
        if(lua_getfield(L, -1, "possible") != LUA_TTABLE) {
            failwith("unexpected type");
        }

        for(int i = 1, t = lua_rawgeti(L, -1, i); t != LUA_TNIL; lua_pop(L, 1), t = lua_rawgeti(L, -1, ++i)) {
            if(t == LUA_TNUMBER) {
                lua_Integer id0 = lua_tointeger(L, -1);
                if(id == id0) {
                    lua_pushvalue(L, -6);
                    lua_rawseti(L, -3, i);
                }
            }
        }
        lua_pop(L, 1);

        lua_pop(L, 2);

        lua_pop(L, 1);
    }

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

    if(luaL_newmetatable(L, TYPE_XRANDR_MONITOR)) {
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
                if(lua_getfield(L, -1, "id") != LUA_TNUMBER) {
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

    if(luaL_newmetatable(L, TYPE_XRANDR_MODE)) {
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

static int setup_mk_crtcs(lua_State* L, struct xrandr* xrandr, XRRScreenResources* res, int setup_index)
{
    luaR_stack(L);

    lua_createtable(L, res->ncrtc, 0);
    lua_getfield(L, setup_index - 1, "modes");

    for(int i = 0; i < res->ncrtc; i++) {
        RRCrtc id = res->crtcs[i];
        XRRCrtcInfo* ci = XRRGetCrtcInfo(xrandr->con->dpy, res, id);

        crtc_mk(L, -1, id, ci);

        lua_pushinteger(L, i);
        lua_setfield(L, -2, "index");

        lua_rawseti(L, -3, i + 1);

        XRRFreeCrtcInfo(ci);
    }

    lua_pop(L, 1);

    luaR_return(L, 1);
}

static int setup_mk_modes(lua_State* L, XRRScreenResources* res)
{
    luaR_stack(L);

    lua_createtable(L, 0, res->nmode);
    for(int i = 0; i < res->nmode; i++) {
        mode_mk(L, &res->modes[i]);
        lua_settable(L, -3);
    }

    luaR_return(L, 1);
}

static int setup_mk_outputs(lua_State* L, struct xrandr* xrandr, XRRScreenResources* res, int setup_index)
{
    luaR_stack(L);

    lua_createtable(L, 0, res->noutput);
    lua_getfield(L, setup_index - 1, "crtcs");
    lua_getfield(L, setup_index - 2, "modes");

    for(int i = 0; i < res->noutput; i++) {
        RROutput id = res->outputs[i];
        XRROutputInfo* oi = XRRGetOutputInfo(xrandr->con->dpy, res, id);
        if(!oi) {
            failwith("XRRGetOutputInfo(%lu) failed", id);
        }

        output_mk(L, -1, -2, id, oi);
        lua_settable(L, -5);

        XRRFreeOutputInfo(oi);
    }

    lua_pop(L, 2);

    luaR_return(L, 1);
}

static int setup_mk_monitors(lua_State* L, struct xrandr* xrandr, int setup_index)
{
    luaR_stack(L);

    int nmonitors;
    XRRMonitorInfo* mi = XRRGetMonitors(xrandr->con->dpy, xrandr->con->root, False, &nmonitors);
    if(!mi) {
        failwith("XRRGetMonitors failed");
    }

    lua_createtable(L, 0, nmonitors);
    lua_getfield(L, setup_index - 1, "outputs");

    for(int i = 0; i < nmonitors; i++) {
        monitor_mk(L, xrandr, -1, &mi[i]);
        lua_settable(L, -4);
    }

    lua_pop(L, 1);

    XRRFreeMonitors(mi);

    luaR_return(L, 1);
}

static int mk_screen(lua_State* L, struct xrandr* xrandr)
{
    luaR_stack(L);

    lua_createtable(L, 0, 6);

    if(luaL_newmetatable(L, TYPE_XRANDR_SCREEN)) {
    }
    lua_setmetatable(L, -2);

    Display* const dpy = xrandr->con->dpy;
    const Window window = xrandr->con->root;
    const int scr = XRRRootToScreen(dpy, window);

    lua_pushinteger(L, window);
    lua_setfield(L, -2, "window");

    lua_pushinteger(L, scr);
    lua_setfield(L, -2, "number");

    lua_pushinteger(L, XDisplayWidth(dpy, scr));
    lua_setfield(L, -2, "width");

    lua_pushinteger(L, XDisplayHeight(dpy, scr));
    lua_setfield(L, -2, "height");

    int minWidth, minHeight, maxWidth, maxHeight;
    if(!XRRGetScreenSizeRange(dpy, window, &minWidth, &minHeight, &maxWidth, &maxHeight)) {
        failwith("XRRGetScreenSizeRange failed");
    }

    lua_createtable(L, 0, 2);
    lua_pushinteger(L, minWidth);
    lua_setfield(L, -2, "width");
    lua_pushinteger(L, minHeight);
    lua_setfield(L, -2, "height");
    lua_setfield(L, -2, "min");

    lua_createtable(L, 0, 2);
    lua_pushinteger(L, maxWidth);
    lua_setfield(L, -2, "width");
    lua_pushinteger(L, maxHeight);
    lua_setfield(L, -2, "height");
    lua_setfield(L, -2, "max");

    luaR_return(L, 1);
}

static int xrandr_fetch_setup(lua_State* L)
{
    luaR_stack(L);
    struct xrandr* xrandr = luaL_checkudata(L, 1, TYPE_XRANDR);

    lua_createtable(L, 0, 5);

    if(luaL_newmetatable(L, TYPE_XRANDR_SETUP)) {
    }
    lua_setmetatable(L, -2);

    XRRScreenResources* res = XRRGetScreenResources(xrandr->con->dpy, xrandr->con->root);
    if(!res) {
        failwith("XRRGetScreenResources failed");
    }

    mk_screen(L, xrandr);
    lua_setfield(L, -2, "screen");

    setup_mk_modes(L, res);
    lua_setfield(L, -2, "modes");

    setup_mk_crtcs(L, xrandr, res, -1);
    lua_setfield(L, -2, "crtcs");

    setup_mk_outputs(L, xrandr, res, -1);
    lua_setfield(L, -2, "outputs");

    setup_mk_monitors(L, xrandr, -1);
    lua_setfield(L, -2, "monitors");

    XRRFreeScreenResources(res);

    luaR_return(L, 1);
}

static int xrandr_index(lua_State* L)
{
    luaR_stack(L);
    struct xrandr* xrandr = luaL_checkudata(L, 1, TYPE_XRANDR);
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

        if(luaL_newmetatable(L, TYPE_XRANDR)) {
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
    struct connection* con = luaL_checkudata(L, 1, TYPE_CONNECTION);

    debug("closing connection %p", con);

    XCloseDisplay(con->dpy);

    luaR_return(L, 0);
}

static int x11_index(lua_State* L)
{
    luaR_stack(L);
    struct connection* con = luaL_checkudata(L, 1, TYPE_CONNECTION);
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

    if(luaL_newmetatable(L, TYPE_CONNECTION)) {
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

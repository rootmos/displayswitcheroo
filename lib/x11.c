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

struct output {
    const char* name;
    int connection;
};

static int output_index(lua_State* L)
{
    luaR_stack(L);
    struct output* o = luaL_checkudata(L, 1, UDTYPE_XRANDR_OUTPUT);
    const char* key = luaL_checkstring(L, 2);

    if(strcmp(key, "name") == 0) {
        lua_pushstring(L, o->name);
        luaR_return(L, 1);
    } else if(strcmp(key, "connected") == 0) {
        switch(o->connection) {
            case 0: lua_pushboolean(L, 1); break;
            case 1: lua_pushboolean(L, 0); break;
            default: lua_pushnil(L); break;
        }
        luaR_return(L, 1);
    } else {
        debug("indexing absent key %p[%s]", o, key);
        lua_pushnil(L);
        luaR_return(L, 1);
    }
}

static int output_mk(lua_State* L, XRROutputInfo* oi)
{
    luaR_stack(L);

    struct output* o = lua_newuserdatauv(L, sizeof(*o), 0);

    if(luaL_newmetatable(L, UDTYPE_XRANDR_OUTPUT)) {
        lua_pushcfunction(L, output_index);
        lua_setfield(L, -2, "__index");
    }
    lua_setmetatable(L, -2);

    o->name = lua_pushstring(L, oi->name);
    lua_pop(L, 1);

    o->connection = oi->connection;

    luaR_return(L, 1);
}

#define set_str(f, s) do { \
    lua_pushliteral(L, f); \
    lua_pushstring(L, s); \
    lua_settable(L, -3); \
} while(0)

#define set_int(f, i) do { \
    lua_pushliteral(L, f); \
    lua_pushinteger(L, i); \
    lua_settable(L, -3); \
} while(0)

#define set_bool(f, b) do { \
    lua_pushliteral(L, f); \
    lua_pushboolean(L, b); \
    lua_settable(L, -3); \
} while(0)

static int monitor_mk(lua_State* L, struct xrandr* xrandr, const XRRMonitorInfo* mi)
{
    luaR_stack(L);

    lua_createtable(L, 0, 1);

    if(luaL_newmetatable(L, UDTYPE_XRANDR_MONITOR)) {
    }
    lua_setmetatable(L, -2);

    char* name = XGetAtomName(xrandr->con->dpy, mi->name);
    if(!name) {
        failwith("XGetAtomName(%ld)", mi->name);
    }
    set_str("name", name);

    if(mi->noutput > 0) {
        set_bool("active", 1);

        set_int("x", mi->x);
        set_int("y", mi->y);
        set_int("width", mi->width);
        set_int("height", mi->height);
        set_int("mheight", mi->mheight);
        set_int("mwidth", mi->mwidth);
    } else {
        set_bool("active", 0);
    }

    set_bool("primary", mi->primary);

    luaR_return(L, 1);
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

    // .outputs
    lua_pushliteral(L, "outputs");
    lua_createtable(L, res->noutput, 0);

    for(int i = 0; i < res->noutput; i++) {
        XRROutputInfo* oi = XRRGetOutputInfo(xrandr->con->dpy, res, res->outputs[i]);
        if(!oi) {
            failwith("XRRGetOutputInfo(%d) failed", i);
        }

        output_mk(L, oi);

        lua_rawseti(L, -2, i + 1);

        XRRFreeOutputInfo(oi);
    }

    lua_settable(L, -3);

    // .monitors
    int nmonitors;
    XRRMonitorInfo* mi = XRRGetMonitors(xrandr->con->dpy, xrandr->con->root, False, &nmonitors);
    if(!mi) {
        failwith("XRRGetMonitors failed");
    }

    lua_pushliteral(L, "monitors");
    lua_createtable(L, nmonitors, 0);

    for(int i = 0; i < nmonitors; i++) {
        monitor_mk(L, xrandr, &mi[i]);
        lua_rawseti(L, -2, i + 1);
    }

    lua_settable(L, -3);

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

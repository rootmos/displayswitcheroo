#include <lua.h>
#include <lauxlib.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#define LIBR_IMPLEMENTATION
#include "r.h"

#define UDTYPE_CONNECTION "x11"
#define UDTYPE_XRANDR "xrandr"

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

static int xrandr_index(lua_State* L)
{
    struct xrandr* xrandr = luaL_checkudata(L, 1, UDTYPE_XRANDR);
    const char* key = luaL_checkstring(L, 2);

    if(strcmp(key, "version") == 0) {
        char buf[128];
        int r = snprintf(LIT(buf), "%d.%d", xrandr->major, xrandr->minor);
        if(r >= sizeof(buf)) {
            failwith("buffer overflow");
        }
        lua_pushstring(L, buf);
        return 1;
    } else {
        debug("indexing absent key %p[%s]", xrandr, key);
        lua_pushnil(L);
        return 1;
    }
}

static int x11_xrandr(lua_State* L, struct connection* con)
{
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
        }
        lua_setmetatable(L, -2);

        lua_pushvalue(L, -1);
        lua_setiuservalue(L, 1, 1);
        return 1;
    } else {
        lua_getiuservalue(L, 1, 1);
        return 1;
    }
}

static int x11_close(lua_State* L)
{
    struct connection* con = luaL_checkudata(L, 1, UDTYPE_CONNECTION);

    debug("closing connection %p", con);

    XCloseDisplay(con->dpy);

    return 0;
}

static int x11_index(lua_State* L)
{
    struct connection* con = luaL_checkudata(L, 1, UDTYPE_CONNECTION);
    const char* key = luaL_checkstring(L, 2);

    if(strcmp(key, "screen") == 0) {
        lua_pushinteger(L, con->scr);
        return 1;
    } else if(strcmp(key, "root") == 0) {
        lua_pushinteger(L, con->root);
        return 1;
    } else if(strcmp(key, "xrandr") == 0) {
        return x11_xrandr(L, con);
    } else {
        debug("indexing absent key %p[%s]", con, key);
        lua_pushnil(L);
        return 1;
    }
}

static int x11_connect(lua_State* L)
{
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

    return 1;
}

int luaopen_x11(lua_State* L)
{
    luaL_checkversion(L);

    lua_newtable(L);

    lua_pushcfunction(L, x11_connect);
    lua_setfield(L, -2, "connect");

    return 1;
}

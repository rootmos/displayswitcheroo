#include <lua.h>
#include <lauxlib.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#define LIBR_IMPLEMENTATION
#include "r.h"

#define UDTYPE_CONNECTION "x11"

struct connection {
    Display* dpy;

    int scr;
    Window root;
};

static int x11_close(lua_State* L)
{
    struct connection* con = luaL_checkudata(L, 1, UDTYPE_CONNECTION);

    debug("closing connection %p", con);

    XCloseDisplay(con->dpy);

    return 0;
}

static int x11_connect(lua_State* L)
{
    int argc = lua_gettop(L);

    struct connection* con = lua_newuserdatauv(L, sizeof(struct connection), 0);

    char* d = XDisplayName(argc == 0 ? NULL : luaL_checkstring(L, 1));
    debug("creating connection %p to %s", con, d);

    con->dpy = XOpenDisplay(d);
    if(con->dpy == NULL) luaR_failwith(L, "unable to connect to display %s", d);

    con->scr = XDefaultScreen(con->dpy);
    con->root = XRootWindow(con->dpy, con->scr);

    if(luaL_newmetatable(L, UDTYPE_CONNECTION)) {
        lua_pushcfunction(L, x11_close);
        lua_setfield(L, -2, "__close");
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

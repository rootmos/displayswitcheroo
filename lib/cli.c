#include <stdlib.h>

#include "r.h"
#include "x11.h"

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <readline/readline.h>
#include <readline/history.h>

const char* prompt = "? ";

static void run_repl(lua_State* L)
{
    char* input = readline(prompt);
    if(!input) {
        debug("EOF");
        return;
    }

    debug("input: %s", input);

    int r = luaL_dostring(L, input);
    if(r != LUA_OK) {
        const char* msg = lua_tostring(L, -1);
        dprintf(2, "runtime error: %s\n", msg);
        lua_pop(L, 1);
    }

    free(input);

    run_repl(L);
}

static int openlibs(lua_State* L)
{
    luaR_stack(L);

    luaL_openlibs(L);

    lua_getglobal(L, "package");
    lua_getfield(L, -1, "preload");

    lua_pushcfunction(L, luaopen_x11);
    lua_setfield(L, -2, "x11");

    lua_pop(L, 2);

    luaR_return(L, 0);
}

int main(int argc, char* argv[])
{
    lua_State* L = luaL_newstate();
    CHECK_NOT(L, NULL, "unable to create Lua state");

    openlibs(L);

    run_repl(L);

    lua_close(L);

    return 0;
}

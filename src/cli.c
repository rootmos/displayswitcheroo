#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#include "r.h"
#include "version.h"
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

struct options {
    const char* input;
};

static void print_usage(int fd, const char* progname)
{
    dprintf(fd, "usage: %s [OPTION]... [INPUT]\n", progname);
}

#ifndef PROGNAME
#define PROGNAME (argv[0])
#endif

static void parse_options(struct options* o, int argc, char* argv[])
{
    memset(o, 0, sizeof(*o));

    int res;
    while((res = getopt(argc, argv, "hv")) != -1) {
        switch(res) {
        case 'v':
            print_version(PROGNAME);
            exit(0);
        case 'h':
        default:
            print_usage(res == 'h' ? 1 : 2, PROGNAME);
            exit(res == 'h' ? 0 : 1);
        }
    }

    if(optind < argc) {
        o->input = argv[optind];
        info("input: %s", o->input);

        struct stat st;
        int r = stat(o->input, &st);
        if(r == -1 && errno == ENOENT) {
            dprintf(2, "error; unable to access input file: %s\n", o->input);
            exit(1);
        }
        CHECK(r, "stat(%s)", o->input);
    }
}

int main(int argc, char* argv[])
{
    struct options o;
    parse_options(&o, argc, argv);

    lua_State* L = luaL_newstate();
    CHECK_NOT(L, NULL, "unable to create Lua state");

    openlibs(L);

    run_repl(L);

    lua_close(L);

    return 0;
}

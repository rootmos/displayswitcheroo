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

#ifndef PROGNAME
#define PROGNAME (argv[0])
#endif
char* progname;

#ifndef XDG_APP
#define XDG_APP "displayswitcheroo"
#endif
struct xdg* xdg;

static void xdg_init(void)
{
    debug("XDG app: %s", XDG_APP);
    xdg = xdg_new(XDG_APP);
}

static void xdg_deinit(void)
{
    xdg_free(xdg);
}

static void readline_init(void)
{
    rl_bind_key ('\t', rl_insert);
    rl_readline_name = progname;

    using_history();

    const char* history = xdg_resolves(xdg, XDG_STATE, "history", NULL);
    if(history) {
        debug("reading history from: %s", history);
        int r = read_history(history);
        CHECK(r, "read_history(%s)", history);
    }
}

static void readline_deinit(void)
{
    const char* history = xdg_prepares(xdg, XDG_STATE, "history", NULL);
    debug("writing history to: %s", history);

    int r = write_history(history);
    CHECK(r, "write_history(%s)", history);
}

static void run_repl(lua_State* L)
{
    readline_init();

    for(;;) {
        char* input = readline(prompt);
        if(!input) {
            debug("EOF");
            readline_deinit();
            return;
        }

        debug("input: %s", input);

        if(input[0]) {
            add_history(input);
        }

        int r = luaL_dostring(L, input);
        if(r != LUA_OK) {
            const char* msg = lua_tostring(L, -1);
            dprintf(2, "runtime error: %s\n", msg);
            lua_pop(L, 1);
        }

        free(input);
    }
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

static void print_usage(int fd)
{
    dprintf(fd, "usage: %s [OPTION]... [INPUT]\n", progname);
    dprintf(fd, "\n");
    dprintf(fd, "options:\n");
    dprintf(fd, "  -h       print this message\n");
    dprintf(fd, "  -v       print version information\n");
}

static void parse_options(struct options* o, int argc, char* argv[])
{
    progname = PROGNAME;

    memset(o, 0, sizeof(*o));

    int res;
    while((res = getopt(argc, argv, "hv")) != -1) {
        switch(res) {
        case 'v':
            print_version(progname);
            exit(0);
        case 'h':
        default:
            print_usage(res == 'h' ? 1 : 2);
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

    xdg_init();

    lua_State* L = luaL_newstate();
    CHECK_NOT(L, NULL, "unable to create Lua state");

    openlibs(L);

    run_repl(L);

    lua_close(L);

    xdg_deinit();

    return 0;
}

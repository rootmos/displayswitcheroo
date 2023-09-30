#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#include "r.h"
#include "version.h"
#include "x11.h"
#include "wait.h"

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

        if(input[0]) {
            debug("input: %s", input);
            add_history(input);

            int r = luaL_dostring(L, input);
            if(r != LUA_OK) {
                const char* msg = lua_tostring(L, -1);
                dprintf(2, "runtime error: %s\n", msg);
                lua_pop(L, 1);
            }
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
    const char* script;
    int interact;
    int wait;
    int once;
};

static void print_usage(int fd)
{
    dprintf(fd, "usage: %s [OPTION]... [SCRIPT]\n", progname);
    dprintf(fd, "\n");
    dprintf(fd, "options:\n");
    dprintf(fd, "  -i       enter interactive mode after executing SCRIPT\n");
    dprintf(fd, "  -w       wait for output connect/disconnects\n");
    dprintf(fd, "  -1       run once before waiting\n");
    dprintf(fd, "  -h       print this message\n");
    dprintf(fd, "  -v       print version information\n");
}

static void parse_options(struct options* o, int argc, char* argv[])
{
    progname = PROGNAME;

    memset(o, 0, sizeof(*o));

    int res;
    while((res = getopt(argc, argv, "iw1hv")) != -1) {
        switch(res) {
        case 'i':
            o->interact = 1;
            break;
        case 'w':
            o->wait = 1;
            break;
        case '1':
            o->once = 1;
            break;
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
        o->script = argv[optind];
    }
}

static const char* resolve_script(const struct options* o)
{
    if(o->script == NULL) return NULL;

    debug("trying to resolve as path: %s", o->script);
    struct stat st;
    if(stat(o->script, &st) == 0) {
        return o->script;
    }

    // config
    debug("trying to resolve relative to XDG config dirs: %s", o->script);
    const char* p = xdg_resolves(xdg, XDG_CONFIG, o->script, NULL);
    if(p) return p;

    static char buf[NAME_MAX];
    int r = snprintf(LIT(buf), "%s.lua", o->script);
    if(r >= sizeof(buf)) failwith("buffer overflow");
    debug("trying to resolve relative to XDG config dirs: %s", buf);
    p = xdg_resolves(xdg, XDG_CONFIG, buf, NULL);
    if(p) return p;


    // data
    debug("trying to resolve relative to XDG data dirs: %s", o->script);
    p = xdg_resolves(xdg, XDG_DATA, o->script, NULL);
    if(p) return p;

    r = snprintf(LIT(buf), "%s.lua", o->script);
    if(r >= sizeof(buf)) failwith("buffer overflow");

    debug("trying to resolve relative to XDG data dirs: %s", buf);
    p = xdg_resolves(xdg, XDG_DATA, buf, NULL);
    if(p) return p;

    // path
    debug("trying to resolve as path: %s", buf);
    if(stat(buf, &st) == 0) {
        return buf;
    }

    dprintf(2, "unable to resolve script: %s\n", o->script);
    exit(1);
}

void run(const struct options* o)
{
    const char* script = resolve_script(o);
    if(script) {
        debug("resolved script: %s", script);
    }

    lua_State* L = luaL_newstate();
    CHECK_NOT(L, NULL, "unable to create Lua state");

    openlibs(L);

    if(script) {
        info("running script: %s", script);
        int r = luaL_dofile(L, script);
        if(r == LUA_OK) {
            if(o->interact) {
                run_repl(L);
            }
        } else {
            const char* msg = lua_tostring(L, -1);
            dprintf(2, "%s: %s\n", progname, msg);
            lua_pop(L, 1);
        }
    } else {
        run_repl(L);
    }

    lua_close(L);
}

int main(int argc, char* argv[])
{
    struct options o;
    parse_options(&o, argc, argv);

    xdg_init();

    if(o.wait) {
        if(o.once) {
            run(&o);
        }
        run_wait_loop((void(*)(void*))run, &o);
    } else {
        run(&o);
    }

    xdg_deinit();

    return 0;
}

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

static int add_xdg_to_search_paths(lua_State* L)
{
    luaR_stack(L);

    int t = lua_getglobal(L, "package");
    if(t != LUA_TTABLE) {
        return luaL_error(L, "package has unexpected type: %s", lua_typename(L, t));
    }

    t = lua_getfield(L, -1, "path");
    if(t != LUA_TSTRING) {
        return luaL_error(L, "package.path has unexpected type: %s", lua_typename(L, t));
    }
    const char* orig_path = luaL_checkstring(L, -1);
    lua_pop(L, 1);

    t = lua_getfield(L, -1, "cpath");
    if(t != LUA_TSTRING) {
        return luaL_error(L, "package.cpath has unexpected type: %s", lua_typename(L, t));
    }
    const char* orig_cpath = luaL_checkstring(L, -1);
    lua_pop(L, 1);

    char path[4096];
    int l = snprintf(LIT(path), "%s", orig_path);
    if(l >= sizeof(path)) {
        failwith("buffer overflow");
    }

    char cpath[4096];
    int cl = snprintf(LIT(cpath), "%s", orig_cpath);
    if(cl >= sizeof(cpath)) {
        failwith("buffer overflow");
    }

    for(const char** p = xdg_dirs(xdg, XDG_CONFIG); *p != NULL; p++) {
        l += snprintf(&path[l], sizeof(path) - l, ";%s/?.lua;%s/?/init.lua", *p, *p);
        if(l >= sizeof(path)) {
            failwith("pathfer overflow");
        }

        cl += snprintf(&cpath[cl], sizeof(cpath) - cl, ";%s/?.so", *p);
        if(cl >= sizeof(cpath)) {
            failwith("pathfer overflow");
        }
    }

    for(const char** p = xdg_dirs(xdg, XDG_DATA); *p != NULL; p++) {
        l += snprintf(&path[l], sizeof(path) - l, ";%s/?.lua;%s/?/init.lua", *p, *p);
        if(l >= sizeof(path)) {
            failwith("pathfer overflow");
        }

        cl += snprintf(&cpath[cl], sizeof(cpath) - cl, ";%s/?.so", *p);
        if(cl >= sizeof(cpath)) {
            failwith("pathfer overflow");
        }
    }

    lua_pushstring(L, path);
    lua_setfield(L, -2, "path");

    lua_pushstring(L, cpath);
    lua_setfield(L, -2, "cpath");

    lua_pop(L, 1);

    luaR_return(L, 0);
}

static lua_State* new_lua_state(void)
{
    lua_State* L = luaL_newstate();
    CHECK_NOT(L, NULL, "unable to create Lua state");

    openlibs(L);
    add_xdg_to_search_paths(L);

    return L;
}

static const char* resolve_script(const char* script)
{
    if(script == NULL) return NULL;

    debug("trying to resolve as path: %s", script);
    struct stat st;
    if(stat(script, &st) == 0) {
        return script;
    }

    // config
    debug("trying to resolve relative to XDG config dirs: %s", script);
    const char* p = xdg_resolves(xdg, XDG_CONFIG, script, NULL);
    if(p) return p;

    static char buf[NAME_MAX];
    int r = snprintf(LIT(buf), "%s.lua", script);
    if(r >= sizeof(buf)) failwith("buffer overflow");
    debug("trying to resolve relative to XDG config dirs: %s", buf);
    p = xdg_resolves(xdg, XDG_CONFIG, buf, NULL);
    if(p) return p;


    // data
    debug("trying to resolve relative to XDG data dirs: %s", script);
    p = xdg_resolves(xdg, XDG_DATA, script, NULL);
    if(p) return p;

    r = snprintf(LIT(buf), "%s.lua", script);
    if(r >= sizeof(buf)) failwith("buffer overflow");

    debug("trying to resolve relative to XDG data dirs: %s", buf);
    p = xdg_resolves(xdg, XDG_DATA, buf, NULL);
    if(p) return p;

    // path
    debug("trying to resolve as path: %s", buf);
    if(stat(buf, &st) == 0) {
        return buf;
    }

    dprintf(2, "unable to resolve script: %s\n", script);
    exit(1);
}

static char* collect_fd(int fd, size_t* len)
{
    size_t l = 0, L = 4096;
    char* buf = malloc(L);
    CHECK_MALLOC(buf);

    for(;;) {
        int r = read(fd, &buf[l], L-l);
        CHECK(r, "read(%d)", 0);
        l += r;
        if(r == 0) {
            if(len) {
                *len = l;
            }
            return buf;
        }

        if(l >= L) {
            L <<= 1;
            buf = realloc(buf, L);
            CHECK_MALLOC(buf);
        }
    }
}

struct options {
    const char* script;
    int interact;
    int wait;
    int once;
    int global;
};

static void print_usage(int fd)
{
    dprintf(fd, "usage: %s [OPTION]... [SCRIPT]\n", progname);
    dprintf(fd, "\n");
    dprintf(fd, "options:\n");
    dprintf(fd, "  -i       enter interactive mode after executing SCRIPT\n");
    dprintf(fd, "  -w       wait for output connect/disconnects\n");
    dprintf(fd, "  -1       run once before waiting\n");
    dprintf(fd, "  -g       keep global state between runs\n");
    dprintf(fd, "  -h       print this message\n");
    dprintf(fd, "  -v       print version information\n");
}

static void parse_options(struct options* o, int argc, char* argv[])
{
    progname = PROGNAME;

    memset(o, 0, sizeof(*o));

    int res;
    while((res = getopt(argc, argv, "iw1ghv")) != -1) {
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
        case 'g':
            o->global = 1;
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

struct state {
    struct options o;

    const char* script;

    char* stdin_buf;
    size_t stdin_len;

    lua_State* L0;
};

static void run(struct state* st)
{
    lua_State* L = st->L0 ? st->L0 : new_lua_state();

    if(st->script || st->stdin_buf) {
        int r;
        if(st->script) {
            info("running script: %s", st->script);
            r = luaL_dofile(L, st->script);
        } else {
            info("running script from stdin");
            r = luaL_loadbuffer(L, st->stdin_buf, st->stdin_len, "stdin");
            if(r == LUA_OK) {
                r = lua_pcall(L, 0, LUA_MULTRET, 0);
            }
        }

        if(r == LUA_OK) {
            if(st->o.interact) {
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

    if(!st->L0) {
        lua_close(L);
    }
}

int main(int argc, char* argv[])
{
    struct state st;
    memset(&st, 0, sizeof(st));
    parse_options(&st.o, argc, argv);

    st.L0 = st.o.global ? new_lua_state() : NULL;

    xdg_init();

    if(st.o.script) {
        if(strcmp(st.o.script, "-") == 0) {
            st.stdin_buf = collect_fd(0, &st.stdin_len);
        } else {
            st.script = resolve_script(st.o.script);
        }
    }

    if(st.o.wait) {
        if(st.o.once) {
            run(&st);
        }
        run_wait_loop((void(*)(void*))run, &st);
    } else {
        run(&st);
    }

    xdg_deinit();

    return 0;
}

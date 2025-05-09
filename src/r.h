// libr 0.5.1 (a4a5e5613ad474d064921267df16cfc6c57b2498) (https://github.com/rootmos/libr.git) (2025-05-09T08:27:59+02:00)
// modules: logging now lua fail util xdg path sha1 nonblock

#ifndef LIBR_HEADER
#define LIBR_HEADER

#define LIBR(x) x
#define PRIVATE __attribute__((visibility("hidden")))
#define PUBLIC __attribute__((visibility("default")))
#define API PRIVATE


// libr: logging.h

#include <stdarg.h>

#define LOG_QUIET 0
#define LOG_ERROR 1
#define LOG_WARNING 2
#define LOG_INFO 3
#define LOG_DEBUG 4
#define LOG_TRACE 5

#ifndef LOG_LEVEL
#define LOG_LEVEL LOG_INFO
#endif

extern int LIBR(logger_fd);

#define __r_log(level, format, ...) do { \
    LIBR(logger)(level, __extension__ __FUNCTION__, __extension__ __FILE__, \
          __extension__ __LINE__, format "\n", ##__VA_ARGS__); \
} while(0)

#ifdef __cplusplus
void LIBR(dummy)(...);
#else
void LIBR(dummy)();
#endif

#if LOG_LEVEL >= LOG_ERROR
#define error(format, ...) __r_log(LOG_ERROR, format, ##__VA_ARGS__)
#else
#define error(format, ...) do { if(0) LIBR(dummy)(); } while(0)
#endif

#if LOG_LEVEL >= LOG_WARNING
#define warning(format, ...) __r_log(LOG_WARNING, format, ##__VA_ARGS__)
#else
#define warning(format, ...) do { if(0) LIBR(dummy)(); } while(0)
#endif

#if LOG_LEVEL >= LOG_INFO
#define info(format, ...) __r_log(LOG_INFO, format, ##__VA_ARGS__)
#else
#define info(format, ...) do { if(0) LIBR(dummy)(); } while(0)
#endif

#if LOG_LEVEL >= LOG_DEBUG
#define debug(format, ...) __r_log(LOG_DEBUG, format, ##__VA_ARGS__)
#else
#define debug(format, ...) do { if(0) LIBR(dummy)(); } while(0)
#endif

#if LOG_LEVEL >= LOG_TRACE
#define trace(format, ...) __r_log(LOG_TRACE, format, ##__VA_ARGS__)
#else
#define trace(format, ...) do { if(0) LIBR(dummy)(); } while(0)
#endif

void LIBR(logger)(
    int level,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt, ...)
__attribute__ ((format (printf, 5, 6)));

void LIBR(vlogger)(
    int level,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt,
    va_list vl
);

// libr: now.h

// returns current time formated as compact ISO8601: 20190123T182628Z
const char* LIBR(now_iso8601_compact)(void);

// libr: lua.h

#include <lua.h>

#define CHECK_LUA(L, err, format, ...) do { \
    if(err != LUA_OK) { \
        LIBR(failwith0)( \
            __extension__ __FUNCTION__, __extension__ __FILE__, \
            __extension__ __LINE__, 0, \
            format ": %s\n", ##__VA_ARGS__, lua_tostring(L, -1)); \
    } \
} while(0)

#define LUA_EXPECT_TYPE(L, t, expected, format, ...) do { \
    if(t != expected) { \
        LIBR(failwith0)( \
            __extension__ __FUNCTION__, __extension__ __FILE__, \
            __extension__ __LINE__, 0, \
            format ": unexpected type %s (expected %s)\n", \
            ##__VA_ARGS__, lua_typename(L, t), \
            lua_typename(L, expected)); \
    } \
} while(0)

#ifndef LUA_STACK_MARKER
#define LUA_STACK_MARKER LIBR(luaR_stack_marker)
#endif

#define luaR_stack(L) int LUA_STACK_MARKER = lua_gettop(L)
#define luaR_stack_expect(L, n) do { \
    int r = lua_gettop(L) - LUA_STACK_MARKER; \
    if(r < n) { \
        failwith("too few stack elements: found %d expected %d", r ,n); \
    } else if(r > n) { \
        failwith("too many stack elements: found %d expected %d", r ,n); \
    } \
} while(0)

#define luaR_return(L, n) do { \
    luaR_stack_expect(L, n); \
    return n; \
} while(0)

#define luaR_failwith(L, format, ...) \
    LIBR(luaR_failwith0)(L, \
            __extension__ __FUNCTION__, \
            __extension__ __FILE__, \
            __extension__ __LINE__, \
            format, ##__VA_ARGS__)

void LIBR(luaR_failwith0)(
    lua_State* L,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt, ...)
__attribute__ ((noreturn, format (printf, 5, 6)));

int LIBR(luaR_testmetatable)(lua_State* L, int arg, const char* tname);
void LIBR(luaR_checkmetatable)(lua_State* L, int arg, const char* tname);

// libr: fail.h

#define CHECK(res, format, ...) CHECK_NOT(res, -1, format, ##__VA_ARGS__)

#define CHECK_NOT(res, err, format, ...) \
    CHECK_IF(res == err, format, ##__VA_ARGS__)

#define CHECK_IF(cond, format, ...) do { \
    if(cond) { \
        LIBR(failwith0)(__extension__ __FUNCTION__, __extension__ __FILE__, \
            __extension__ __LINE__, 1, \
            format "\n", ##__VA_ARGS__); \
    } \
} while(0)

#define CHECK_MALLOC(x) CHECK_NOT(x, NULL, "memory allocation failed")
#define CHECK_MMAP(x) CHECK_NOT(x, MAP_FAILED, "memory mapping failed")

#define failwith(format, ...) \
    LIBR(failwith0)(__extension__ __FUNCTION__, __extension__ __FILE__, \
        __extension__ __LINE__, 0, format "\n", ##__VA_ARGS__)

#define not_implemented() do { failwith("not implemented"); } while(0)

void LIBR(failwith0)(
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const int include_errno,
    const char* const fmt, ...)
__attribute__ ((noreturn, format (printf, 5, 6)));

// libr: util.h

#ifndef LENGTH
#define LENGTH(xs) (sizeof(xs)/sizeof((xs)[0]))
#endif

#ifndef LIT
#define LIT(x) x,sizeof(x)
#endif

#ifndef STR
#define STR(x) x,strlen(x)
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

// libr: xdg.h

#include <stddef.h>
#include <stdarg.h>

// https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html

struct xdg;

enum xdg_kind {
    XDG_HOME = 0,
    XDG_DATA,
    XDG_CONFIG,
    XDG_STATE,
    XDG_CACHE,
    XDG_RUNTIME,
    XDG_KINDS,
};

struct xdg* LIBR(xdg_new)(const char* app);
void LIBR(xdg_free)(struct xdg* xdg);

const char* LIBR(xdg_dir)(struct xdg* xdg, enum xdg_kind k);
const char** LIBR(xdg_dirs)(struct xdg* xdg, enum xdg_kind k);

const char* LIBR(xdg_home)(struct xdg* xdg);
const char* LIBR(xdg_data_home)(struct xdg* xdg);
const char* LIBR(xdg_config_home)(struct xdg* xdg);
const char* LIBR(xdg_state_home)(struct xdg* xdg);
const char* LIBR(xdg_cache_home)(struct xdg* xdg);
const char* LIBR(xdg_runtime)(struct xdg* xdg);

const char** LIBR(xdg_data_dirs)(struct xdg* xdg);
const char** LIBR(xdg_config_dirs)(struct xdg* xdg);

const char* LIBR(xdg_resolve)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...);
const char* LIBR(xdg_resolvev)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps);
const char* LIBR(xdg_resolves)(struct xdg* xdg, enum xdg_kind k, ...);
const char* LIBR(xdg_resolvevs)(struct xdg* xdg, enum xdg_kind k, va_list ps);

const char* LIBR(xdg_preparev)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps);
const char* LIBR(xdg_prepare)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...);
const char* LIBR(xdg_preparevs)(struct xdg* xdg, enum xdg_kind k, va_list ps);
const char* LIBR(xdg_prepares)(struct xdg* xdg, enum xdg_kind k, ...);

// libr: path.h

#include <stdarg.h>
#include <stddef.h>
#include <sys/types.h>

size_t LIBR(path_join)(char* buf, size_t L, const char* p0, ...);
size_t LIBR(path_joinv)(char* buf, size_t L, const char* p0, va_list ps);

// using static buffer
#ifdef failwith
const char* LIBR(path_joins)(const char* p0, ...);
const char* LIBR(path_joinvs)(const char* p0, va_list ps);
#endif

int LIBR(makedirs)(const char* path, mode_t mode);

// libr: sha1.h

#include <stdint.h>
#include <stddef.h>

#define SHA1_DIGEST_LENGTH (160/8)

struct sha1_state {
    uint8_t digest[SHA1_DIGEST_LENGTH];
    uint8_t _internal[84];
};

void LIBR(sha1_init)(struct sha1_state* st);
void LIBR(sha1_update)(struct sha1_state* st, const void* buf, size_t len);
void LIBR(sha1_finalize)(struct sha1_state* st);

// libr: nonblock.h

void LIBR(set_blocking)(int fd, int blocking);
#endif // LIBR_HEADER

#ifdef LIBR_IMPLEMENTATION

// libr: logging.c

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef __cplusplus
API void LIBR(dummy)(...)
#else
API void LIBR(dummy)()
#endif
{
    abort();
}

int LIBR(logger_fd) API = 2;

API void LIBR(vlogger)(
    int level,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt, va_list vl)
{
    int r = dprintf(LIBR(logger_fd), "%s:%d:%s:%s:%u ",
        LIBR(now_iso8601_compact)(), getpid(), caller, file, line);
    if(r < 0) {
        abort();
    }

    r = vdprintf(LIBR(logger_fd), fmt, vl);
    if(r < 0) {
        abort();
    }
}

API void LIBR(logger)(
    int level,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);
    LIBR(vlogger)(level, caller, file, line, fmt, vl);
    va_end(vl);
}

// libr: now.c

#include <time.h>
#include <stdlib.h>

PRIVATE const char* LIBR(now_iso8601_compact)(void)
{
    static char buf[17];
    const time_t t = time(NULL);
    size_t r = strftime(buf, sizeof(buf), "%Y%m%dT%H%M%SZ", gmtime(&t));
    if(r <= 0) abort();
    return buf;
}

// libr: lua.c

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <lua.h>
#include <lauxlib.h>

API void LIBR(luaR_failwith0)(
    lua_State* L,
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const char* const fmt, ...)
{

    size_t N = 256;
    char* buf;
    while(1) {
        buf = alloca(N);

        int r = snprintf(buf, N, "(%s:%d:%s:%s:%u) ", now_iso8601_compact(), getpid(), caller, file, line);
        if(r >= N) {
            while(N < r) {
                N <<= 1;
            }
            continue;
        }

        va_list vl;
        va_start(vl, fmt);

        r += vsnprintf(buf+r, N-r, fmt, vl);
        if(r < N) {
            lua_pushstring(L, buf);
            lua_error(L); // "This function does a long jump, and therefore never returns [...]."
        }

        va_end(vl);
        N <<= 1;
    }
}

API int LIBR(luaR_testmetatable)(lua_State* L, int arg, const char* tname)
{
    if(lua_getmetatable(L, arg)) {
        luaL_getmetatable(L, tname);
        int r = lua_rawequal(L, -1, -2);
        lua_pop(L, 2);
        return r;
    }
    return 0;
}

API void LIBR(luaR_checkmetatable)(lua_State* L, int arg, const char* tname)
{
    luaL_argexpected(L, LIBR(luaR_testmetatable)(L, arg, tname), arg, tname);
}

// libr: fail.c

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

API void LIBR(failwith0)(
    const char* const caller,
    const char* const file,
    const unsigned int line,
    const int include_errno,
    const char* const fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);

    if(include_errno) {
        LIBR(logger)(LOG_ERROR, caller, file, line, "(%s) ", strerror(errno));
        if(vdprintf(LIBR(logger_fd), fmt, vl) < 0) {
            abort();
        }
    } else {
        LIBR(vlogger)(LOG_ERROR, caller, file, line, fmt, vl);
    }
    va_end(vl);

    abort();
}

// libr: xdg.c

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libgen.h>

struct xdg {
    char app[NAME_MAX];

    char* dir[XDG_KINDS];
    char** dirs[XDG_KINDS];
};

API struct xdg* LIBR(xdg_new)(const char* app)
{
    struct xdg* xdg = calloc(1, sizeof(*xdg));
    CHECK_MALLOC(xdg);

    if(app) {
        const size_t N = sizeof(xdg->app);
        size_t n = strnlen(app, N);
        if(n >= N) {
            failwith("application name truncated at %zu characters", n);
        }

        if(n > 0) {
            memcpy(xdg->app, app, n);
        }
    }

    return xdg;
}

API void LIBR(xdg_free)(struct xdg* xdg)
{
    if(xdg == NULL) return;

    for(int i = 0; i < XDG_KINDS; i++) {
        free(xdg->dir[i]);
        free(xdg->dirs[i]);
    }

    free(xdg);
}

static size_t xdg_check_path(const char* p)
{
    if(!p) return 0;
    size_t r = strnlen(p, PATH_MAX);
    if(r >= PATH_MAX) return 0;
    if(r == 0) return 0;
    if(p[0] != '/') return 0;
    return r;
}

static size_t xdg_append_app(const struct xdg* xdg, char* buf, size_t L, const char* p)
{
    if(xdg->app[0]) {
        size_t l = LIBR(path_join)(buf, L, p, xdg->app, NULL);
        if(l >= L) {
            failwith("buffer overflow");
        }

        return l;
    } else {
        return strlen(buf);
    }
}

static size_t LIBR(xdg_fallback_runtime_dir)(char* buf, size_t L)
{
    char template[NAME_MAX];
    size_t n = snprintf(LIT(template), "/tmp/xdg-runtime-fallback-%d-XXXXXX", geteuid());
    if(n >= sizeof(template)) {
        failwith("buffer overflow");
    }

    // mkdtemp(3): "The directory is then created with permissions 0700."
    char* tmp = mkdtemp(template);
    CHECK_NOT(tmp, NULL, "mkdtemp(%s)", template);
    warning("using fallback directory for an unset XDG_RUNTIME_DIR: %s", tmp);
    return snprintf(buf, L, "%s", tmp);
}

API const char* LIBR(xdg_dir)(struct xdg* xdg, enum xdg_kind k)
{
    if(xdg->dir[k]) return xdg->dir[k];

    if(k == XDG_HOME) {
        char* home = getenv("HOME");
        if(!home) {
            failwith("unable to resolve HOME variable");
        }

        xdg->dir[k] = strdup(home);
        CHECK_MALLOC(xdg->dir[k]);
        return xdg->dir[k];
    }

    char buf[PATH_MAX];
    size_t l;

    const char* v = NULL;
    switch(k) {
        case XDG_DATA: v = "XDG_DATA_HOME"; break;
        case XDG_CONFIG: v = "XDG_CONFIG_HOME"; break;
        case XDG_STATE: v = "XDG_STATE_HOME"; break;
        case XDG_CACHE: v = "XDG_CACHE_HOME"; break;
        case XDG_RUNTIME: v = "XDG_RUNTIME_DIR"; break;
        default: failwith("unexpected kind: %d", k);
    }

    const char* e = getenv(v);
    if(xdg_check_path(e)) {
        l = LIBR(path_join)(LIT(buf), e, NULL);
    } else {
        switch(k) {
            case XDG_DATA:
                l = LIBR(path_join)(LIT(buf), LIBR(xdg_home)(xdg), ".local", "share", NULL);
                break;
            case XDG_CONFIG:
                l = LIBR(path_join)(LIT(buf), LIBR(xdg_home)(xdg), ".config", NULL);
                break;
            case XDG_STATE:
                l = LIBR(path_join)(LIT(buf), LIBR(xdg_home)(xdg), ".local", "state", NULL);
                break;
            case XDG_CACHE:
                l = LIBR(path_join)(LIT(buf), LIBR(xdg_home)(xdg), ".cache", NULL);
                break;
            case XDG_RUNTIME:
                l = LIBR(xdg_fallback_runtime_dir)(LIT(buf));
                break;
            default:
                failwith("unexpected kind: %d", k);
        }
    }
    if(l >= sizeof(buf)) {
        failwith("buffer overflow");
    }

    xdg_append_app(xdg, LIT(buf), buf);

    xdg->dir[k] = strdup(buf);
    CHECK_MALLOC(xdg->dir[k]);

    return xdg->dir[k];
}

API const char* LIBR(xdg_home)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_HOME);
}

API const char* LIBR(xdg_data_home)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_DATA);
}

API const char* LIBR(xdg_config_home)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_CONFIG);
}

API const char* LIBR(xdg_state_home)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_STATE);
}

API const char* LIBR(xdg_cache_home)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_CACHE);
}

API const char* LIBR(xdg_runtime)(struct xdg* xdg)
{
    return LIBR(xdg_dir)(xdg, XDG_RUNTIME);
}

API const char** LIBR(xdg_dirs)(struct xdg* xdg, enum xdg_kind k)
{
    if(xdg->dirs[k]) return (const char**)xdg->dirs[k];

    const char* e = NULL;
    if(k == XDG_DATA || k == XDG_CONFIG) {
        const char* v = NULL;
        if(k == XDG_DATA) {
            v = "XDG_DATA_DIRS";
        } else if(k == XDG_CONFIG) {
            v = "XDG_CONFIG_DIRS";
        }

        e = getenv(v);
        if(e == NULL || *e == 0) {
            if(k == XDG_DATA) {
                e = "/usr/local/share:/usr/share";
            } else if(k == XDG_CONFIG) {
                e = "/etc/xdg:/etc";
            }
        }
    }

    struct {
        char* path;
        size_t len;
        void* next;
    }* dirs;

    {
        dirs = alloca(sizeof(*dirs));
        const char* h = LIBR(xdg_dir)(xdg, k);
        dirs->len = strlen(h);
        dirs->path = alloca(dirs->len+1);
        memcpy(dirs->path, h, dirs->len+1);
        dirs->next = NULL;
    }
    typeof(*dirs)** tail = (void*)&dirs->next;

    size_t n = 1;
    if(e) {
        const size_t L = strlen(e);
        char buf[L+1];
        memcpy(buf, e, L+1);
        size_t a = 0, b = 0;
        do {
            for(; buf[b] != ':' && buf[b] != 0; b++);
            buf[b] = 0;
            char* p = &buf[a];
            if(xdg_check_path(p)) {
                char q[PATH_MAX];
                size_t l = LIBR(xdg_append_app)(xdg, LIT(q), p);
                if(l >= sizeof(q)) {
                    failwith("buffer overflow");
                }

                typeof(*dirs)* t = alloca(sizeof(*dirs));
                t->len = l;
                t->path = alloca(l+1);
                memcpy(t->path, q, l+1);
                t->next = NULL;

                *tail = t;
                tail = (void*)&t->next;
                n += 1;
            }
            b += 1;
            a = b;
        } while(b < L);
    }

    size_t N = 0;
    for(typeof(*dirs)* p = dirs; p != NULL; p = p->next) {
        N += sizeof(char*);
        N += p->len+1;
    }
    N += sizeof(char*); // add space for the NULL guard

    void* buf = malloc(N);
    CHECK_MALLOC(buf);
    char** index = buf;

    index[n] = NULL;
    char* strings = (char*)&index[n+1];

    size_t i = 0;
    for(typeof(*dirs)* p = dirs; p != NULL; i++, p = p->next) {
        index[i] = strings;
        memcpy(strings, p->path, p->len + 1);
        strings += p->len + 1;
    }

    xdg->dirs[k] = buf;
    return buf;
}

API const char** LIBR(xdg_data_dirs)(struct xdg* xdg)
{
    return LIBR(xdg_dirs)(xdg, XDG_DATA);
}

API const char** LIBR(xdg_config_dirs)(struct xdg* xdg)
{
    return LIBR(xdg_dirs)(xdg, XDG_CONFIG);
}

API const char* LIBR(xdg_resolvev)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps)
{
    const char** dirs = LIBR(xdg_dirs)(xdg, k);
    for(size_t i = 0; dirs[i]; i++) {
        va_list qs;
        va_copy(qs, ps);
        size_t l = LIBR(path_joinv)(buf, L, dirs[i], qs);
        if(l >= L) {
            failwith("buffer overflow");
        }
        va_end(qs);

        struct stat st;
        int r = stat(buf, &st);
        if(r == -1 && (errno == EACCES || errno == ENOENT || errno == ENOTDIR)) {
            continue;
        }
        CHECK(r, "stat(%s)", buf);

        return buf;
    }

    return NULL;
}

API const char* LIBR(xdg_resolve)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...)
{
    va_list ps; va_start(ps, L);
    const char* p = LIBR(xdg_resolvev)(xdg, k, buf, L, ps);
    return va_end(ps), p;
}

API const char* LIBR(xdg_resolvevs)(struct xdg* xdg, enum xdg_kind k, va_list ps)
{
    static char buf[PATH_MAX];
    return LIBR(xdg_resolvev)(xdg, k, LIT(buf), ps);
}

API const char* LIBR(xdg_resolves)(struct xdg* xdg, enum xdg_kind k, ...)
{
    va_list ps; va_start(ps, k);
    const char* p = LIBR(xdg_resolvevs)(xdg, k, ps);
    return va_end(ps), p;
}

API void LIBR(xdg_makedirs)(const char* path)
{
    int r = LIBR(makedirs)(path, 0700);
    CHECK(r, "makedirs(%s, 0700)", path);
}

API const char* LIBR(xdg_preparev)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps)
{
    size_t l = LIBR(path_joinv)(buf, L, LIBR(xdg_dir)(xdg, k), ps);
    if(l >= L) {
        failwith("buffer overflow");
    }

    struct stat st;
    int r = stat(buf, &st);
    if(r == -1 && errno == ENOENT) {
        char d[l+1];
        memcpy(d, buf, l+1);
        LIBR(xdg_makedirs)(dirname(d));
        return buf;
    }
    CHECK(r, "stat(%s)", buf);

    return buf;
}

API const char* LIBR(xdg_prepare)(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...)
{
    va_list ps; va_start(ps, L);
    const char* p = LIBR(xdg_preparev)(xdg, k, buf, L, ps);
    return va_end(ps), p;
}

API const char* LIBR(xdg_preparevs)(struct xdg* xdg, enum xdg_kind k, va_list ps)
{
    static char buf[PATH_MAX];
    return LIBR(xdg_preparev)(xdg, k, LIT(buf), ps);
}

API const char* LIBR(xdg_prepares)(struct xdg* xdg, enum xdg_kind k, ...)
{
    va_list ps; va_start(ps, k);
    const char* p = LIBR(xdg_preparevs)(xdg, k, ps);
    return va_end(ps), p;
}

// libr: path.c

#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>

API size_t LIBR(path_joinv)(char* buf, size_t L, const char* p0, va_list ps)
{
    size_t n = strlen(p0);
    if(n < L) {
        memmove(buf, p0, n);
    }

    for(;;) {
        const char* p = va_arg(ps, const char*);
        if(!p) break;

        if(n < L) {
            buf[n] = '/';
        }
        n += 1;

        size_t l = strlen(p);
        if(n + l < L) {
            memcpy(&buf[n], p, l);
        }
        n += l;
    }

    if(n < L) {
        buf[n] = 0;
    }

    return n;
}

API size_t LIBR(path_join)(char* buf, size_t L, const char* p0, ...)
{
    va_list ps; va_start(ps, p0);
    size_t n = LIBR(path_joinv)(buf, L, p0, ps);
    return va_end(ps), n;
}

#ifdef failwith
API const char* LIBR(path_joinvs)(const char* p0, va_list ps)
{
    static char buf[PATH_MAX];
    size_t l = LIBR(path_joinv)(buf, sizeof(buf), p0, ps);
    if(l >= sizeof(buf)) {
        failwith("buffer overflow");
    }

    return buf;
}

API const char* LIBR(path_joins)(const char* p0, ...)
{
    va_list ps; va_start(ps, p0);
    const char* p = LIBR(path_joinvs)(p0, ps);
    return va_end(ps), p;
}
#endif

API int LIBR(makedirs)(const char* path, mode_t mode)
{
    const size_t L = strlen(path);
    char buf[L+1];
    memcpy(buf, path, L+1);

    for(size_t l = (buf[0] == '/' ? 1 : 0); l <= L; l++) {
        if(buf[l] == '/') {
            buf[l] = 0;
        }

        if(buf[l] == 0) {
            int r = mkdir(buf, mode);
            if(r == -1) {
                if(errno != EEXIST) {
                    return -1;
                }
            }

            if(l < L) {
                buf[l] = '/';
            }
        }
    }

    struct stat st;
    int r = stat(buf, &st);
    if(r != 0) return -1;

    if(!(st.st_mode & S_IFDIR)) {
        errno = EEXIST;
        return -1;
    }

    return 0;
}

// libr: sha1.c

#include <assert.h>
#include <endian.h>
#include <stdlib.h>
#include <string.h>

struct sha1_state_internal {
    uint32_t H[5];

    uint64_t len;

    uint8_t buf[512/8];
    uint8_t off;
};

static struct sha1_state_internal* sha1_to_internal_state(struct sha1_state* st)
{
    assert(sizeof(struct sha1_state) == sizeof(struct sha1_state_internal));
    return (struct sha1_state_internal*)st;
}

static uint32_t sha1_f(int t, uint32_t B, uint32_t C, uint32_t D)
{
    if(0 <= t && t <= 19) {
        return (B & C) | ((~B) & D);
    } else if(20 <= t && t <= 39) {
        return B ^ C ^ D;
    } else if(40 <= t && t <= 59) {
        return (B & C) | (B & D) | (C & D);
    } else if(60 <= t && t <= 79) {
        return B ^ C ^ D;
    } else {
        abort();
    }
}

static uint32_t sha1_K(int t)
{
    if(0 <= t && t <= 19) {
        return 0x5A827999;
    } else if(20 <= t && t <= 39) {
        return 0x6ED9EBA1;
    } else if(40 <= t && t <= 59) {
        return 0x8F1BBCDC;
    } else if(60 <= t && t <= 79) {
        return 0xCA62C1D6;
    } else {
        abort();
    }
}

static uint32_t sha1_S(uint32_t X, int n)
{
    return (X << n) | (X >> (32-n));
}

API void LIBR(sha1_init)(struct sha1_state* st_)
{
    struct sha1_state_internal* st = sha1_to_internal_state(st_);

    st->H[0] = 0x67452301;
    st->H[1] = 0xEFCDAB89;
    st->H[2] = 0x98BADCFE;
    st->H[3] = 0x10325476;
    st->H[4] = 0xC3D2E1F0;

    st->len = 0;
    st->off = 0;
}

static void sha1_process_block(struct sha1_state_internal* st, uint32_t block[16])
{
    uint32_t W[80];
    for(size_t i = 0; i < 16; i++) {
        W[i] = be32toh(block[i]);
    }

    for(size_t t = 16; t < 80; t++) {
        W[t] = sha1_S(W[t-3] ^ W[t-8] ^ W[t-14] ^ W[t-16], 1);
    }

    uint32_t A = st->H[0], B = st->H[1], C = st->H[2], D = st->H[3], E = st->H[4];

    for(size_t t = 0; t < 80; t++) {
        uint32_t T = sha1_S(A, 5) + sha1_f(t, B, C, D) + E + W[t] + sha1_K(t);
        E = D;
        D = C;
        C = sha1_S(B, 30);
        B = A;
        A = T;
    }

    st->H[0] += A;
    st->H[1] += B;
    st->H[2] += C;
    st->H[3] += D;
    st->H[4] += E;
}

API void LIBR(sha1_update)(struct sha1_state* st_, const void* buf, size_t len)
{
    struct sha1_state_internal* st = sha1_to_internal_state(st_);

    const uint8_t* b = buf;
    size_t l = len;

    while(l > 0) {
        size_t L = MIN(l, sizeof(st->buf) - st->off);

        memcpy(&st->buf[st->off], b, L);
        b += L;
        l -= L;
        st->off += L;

        if(sizeof(st->buf) == st->off) {
            sha1_process_block(st, (uint32_t*)st->buf);
            st->off = 0;
        }
    }

    st->len += len;
}

API void LIBR(sha1_finalize)(struct sha1_state* st_)
{
    struct sha1_state_internal* st = sha1_to_internal_state(st_);

    assert(st->len*8 <= 0xffffffffffffffff);

    size_t k = 0;
    for(; ((st->len*8 + 1 + k) % 512) != 448; k++);
    size_t l = (k+1)/8;
    uint8_t buf[l + 8];
    memset(buf, 0, sizeof(buf));

    *((uint64_t*)(&buf[l])) = htobe64(st->len*8);
    buf[0] |= 0x80;

    LIBR(sha1_update)(st_, LIT(buf));

    for(size_t i = 0; i < 5; i++) {
        st->H[i] = htobe32(st->H[i]);
    }
}

// libr: nonblock.c

#include <fcntl.h>

API void LIBR(set_blocking)(int fd, int blocking)
{
    int fl = fcntl(fd, F_GETFL, 0);
    if(blocking) {
        fl &= ~O_NONBLOCK;
    } else {
        fl |= O_NONBLOCK;
    }

    int r = fcntl(fd, F_SETFL, fl);
    CHECK(r, "fcntl(%d, F_SETFL, %d)", fd, fl);
}
#endif // LIBR_IMPLEMENTATION

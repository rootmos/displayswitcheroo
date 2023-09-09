// libr 0.3.0 (3e106adce5e0b2549a10daf7c82df05a81fe60d4) (https://github.com/rootmos/libr.git) (2023-09-07T09:38:42+02:00)
// modules: logging now lua fail util xdg path

#ifndef LIBR_HEADER
#define LIBR_HEADER

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

#define __r_log(level, format, ...) do { \
    r_log(level, __extension__ __FUNCTION__, __extension__ __FILE__, \
          __extension__ __LINE__, format "\n", ##__VA_ARGS__); \
} while(0)

#ifdef __cplusplus
void r_dummy(...);
#else
void r_dummy();
#endif

#if LOG_LEVEL >= LOG_ERROR
#define error(format, ...) __r_log(LOG_ERROR, format, ##__VA_ARGS__)
#else
#define error(format, ...) do { if(0) r_dummy(__VA_ARGS__); } while(0)
#endif

#if LOG_LEVEL >= LOG_WARNING
#define warning(format, ...) __r_log(LOG_WARNING, format, ##__VA_ARGS__)
#else
#define warning(format, ...) do { if(0) r_dummy(__VA_ARGS__); } while(0)
#endif

#if LOG_LEVEL >= LOG_INFO
#define info(format, ...) __r_log(LOG_INFO, format, ##__VA_ARGS__)
#else
#define info(format, ...) do { if(0) r_dummy(__VA_ARGS__); } while(0)
#endif

#if LOG_LEVEL >= LOG_DEBUG
#define debug(format, ...) __r_log(LOG_DEBUG, format, ##__VA_ARGS__)
#else
#define debug(format, ...) do { if(0) r_dummy(__VA_ARGS__); } while(0)
#endif

#if LOG_LEVEL >= LOG_TRACE
#define trace(format, ...) __r_log(LOG_TRACE, format, ##__VA_ARGS__)
#else
#define trace(format, ...) do { if(0) r_dummy(__VA_ARGS__); } while(0)
#endif

void r_log(int level,
           const char* const caller,
           const char* const file,
           const unsigned int line,
           const char* const fmt, ...)
    __attribute__ ((format (printf, 5, 6)));

void r_vlog(int level,
            const char* const caller,
            const char* const file,
            const unsigned int line,
            const char* const fmt, va_list vl);

// libr: now.h

// returns current time formated as compact ISO8601: 20190123T182628Z
const char* now_iso8601_compact(void);

// libr: lua.h

#include <lua.h>

#define CHECK_LUA(L, err, format, ...) do { \
    if(err != LUA_OK) { \
        r_failwith(__extension__ __FUNCTION__, __extension__ __FILE__, \
                   __extension__ __LINE__, 0, \
                   format ": %s\n", ##__VA_ARGS__, lua_tostring(L, -1)); \
    } \
} while(0)

#define LUA_EXPECT_TYPE(L, t, expected, format, ...) do { \
    if(t != expected) {\
        r_failwith(__extension__ __FUNCTION__, __extension__ __FILE__, \
                   __extension__ __LINE__, 0, \
                   format ": unexpected type %s (expected %s)\n", \
                   ##__VA_ARGS__, lua_typename(L, t), \
                   lua_typename(L, expected)); \
    } \
} while(0)

#ifndef LUA_STACK_MARKER
#define LUA_STACK_MARKER __luaR_stack_marker
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
    r_lua_failwith(L, \
            __extension__ __FUNCTION__, \
            __extension__ __FILE__, \
            __extension__ __LINE__, \
            format, ##__VA_ARGS__)

void r_lua_failwith(lua_State* L,
                    const char* const caller,
                    const char* const file,
                    const unsigned int line,
                    const char* const fmt, ...)
    __attribute__ ((noreturn, format (printf, 5, 6)));

int luaR_testmetatable(lua_State* L, int arg, const char* tname);
void luaR_checkmetatable(lua_State* L, int arg, const char* tname);

// libr: fail.h

#define CHECK(res, format, ...) CHECK_NOT(res, -1, format, ##__VA_ARGS__)

#define CHECK_NOT(res, err, format, ...) \
    CHECK_IF(res == err, format, ##__VA_ARGS__)

#define CHECK_IF(cond, format, ...) do { \
    if(cond) { \
        r_failwith(__extension__ __FUNCTION__, __extension__ __FILE__, \
                   __extension__ __LINE__, 1, \
                   format "\n", ##__VA_ARGS__); \
    } \
} while(0)

#define CHECK_MALLOC(x) CHECK_NOT(x, NULL, "memory allocation failed")
#define CHECK_MMAP(x) CHECK_NOT(x, MAP_FAILED, "memory mapping failed")

#define failwith(format, ...) \
    r_failwith(__extension__ __FUNCTION__, __extension__ __FILE__, \
               __extension__ __LINE__, 0, format "\n", ##__VA_ARGS__)

#define not_implemented() failwith("not implemented")

void r_failwith(const char* const caller,
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

struct xdg* xdg_new(const char* app);
void xdg_free(struct xdg* xdg);

const char* xdg_dir(struct xdg* xdg, enum xdg_kind k);
const char** xdg_dirs(struct xdg* xdg, enum xdg_kind k);

const char* xdg_home(struct xdg* xdg);
const char* xdg_data_home(struct xdg* xdg);
const char* xdg_config_home(struct xdg* xdg);
const char* xdg_state_home(struct xdg* xdg);
const char* xdg_cache_home(struct xdg* xdg);
const char* xdg_runtime(struct xdg* xdg);

const char** xdg_data_dirs(struct xdg* xdg);
const char** xdg_config_dirs(struct xdg* xdg);

const char* xdg_resolve(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...);
const char* xdg_resolvev(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps);
const char* xdg_resolves(struct xdg* xdg, enum xdg_kind k, ...);
const char* xdg_resolvevs(struct xdg* xdg, enum xdg_kind k, va_list ps);

const char* xdg_preparev(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps);
const char* xdg_prepare(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...);
const char* xdg_preparevs(struct xdg* xdg, enum xdg_kind k, va_list ps);
const char* xdg_prepares(struct xdg* xdg, enum xdg_kind k, ...);

// libr: path.h

#include <stdarg.h>
#include <stddef.h>
#include <sys/types.h>

size_t path_join(char* buf, size_t L, const char* p0, ...);
size_t path_joinv(char* buf, size_t L, const char* p0, va_list ps);

// using static buffer
#ifdef failwith
const char* path_joins(const char* p0, ...);
const char* path_joinvs(const char* p0, va_list ps);
#endif

int makedirs(const char* path, mode_t mode);
#endif // LIBR_HEADER

#ifdef LIBR_IMPLEMENTATION

// libr: logging.c

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef __cplusplus
void r_dummy(...)
#else
void r_dummy()
#endif
{
    abort();
}

void r_vlog(int level,
            const char* const caller,
            const char* const file,
            const unsigned int line,
            const char* const fmt, va_list vl)
{
    fprintf(stderr, "%s:%d:%s:%s:%u ",
            now_iso8601_compact(), getpid(), caller, file, line);

    vfprintf(stderr, fmt, vl);
}

void r_log(int level,
           const char* const caller,
           const char* const file,
           const unsigned int line,
           const char* const fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);
    r_vlog(level, caller, file, line, fmt, vl);
    va_end(vl);
}

// libr: now.c

#include <time.h>
#include <stdlib.h>

const char* now_iso8601_compact(void)
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

void r_lua_failwith(lua_State* L,
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

int luaR_testmetatable(lua_State* L, int arg, const char* tname)
{
    if(lua_getmetatable(L, arg)) {
        luaL_getmetatable(L, tname);
        int r = lua_rawequal(L, -1, -2);
        lua_pop(L, 2);
        return r;
    }
    return 0;
}

void luaR_checkmetatable(lua_State* L, int arg, const char* tname)
{
    luaL_argexpected(L, luaR_testmetatable(L, arg, tname), arg, tname);
}

// libr: fail.c

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

void r_failwith(const char* const caller,
                const char* const file,
                const unsigned int line,
                const int include_errno,
                const char* const fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);

    if(include_errno) {
        r_log(LOG_ERROR, caller, file, line, "(%s) ", strerror(errno));
        vfprintf(stderr, fmt, vl);
    } else {
        r_vlog(LOG_ERROR, caller, file, line, fmt, vl);
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

struct xdg* xdg_new(const char* app)
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

void xdg_free(struct xdg* xdg)
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
        size_t l = path_join(buf, L, p, xdg->app, NULL);
        if(l >= L) {
            failwith("buffer overflow");
        }

        return l;
    } else {
        return strlen(buf);
    }
}

static size_t xdg_fallback_runtime_dir(char* buf, size_t L)
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

const char* xdg_dir(struct xdg* xdg, enum xdg_kind k)
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
        l = path_join(LIT(buf), e, NULL);
    } else {
        switch(k) {
            case XDG_DATA:
                l = path_join(LIT(buf), xdg_home(xdg), ".local", "share", NULL);
                break;
            case XDG_CONFIG:
                l = path_join(LIT(buf), xdg_home(xdg), ".config", NULL);
                break;
            case XDG_STATE:
                l = path_join(LIT(buf), xdg_home(xdg), ".local", "state", NULL);
                break;
            case XDG_CACHE:
                l = path_join(LIT(buf), xdg_home(xdg), ".cache", NULL);
                break;
            case XDG_RUNTIME:
                l = xdg_fallback_runtime_dir(LIT(buf));
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

const char* xdg_home(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_HOME);
}

const char* xdg_data_home(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_DATA);
}

const char* xdg_config_home(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_CONFIG);
}

const char* xdg_state_home(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_STATE);
}

const char* xdg_cache_home(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_CACHE);
}

const char* xdg_runtime(struct xdg* xdg)
{
    return xdg_dir(xdg, XDG_RUNTIME);
}

const char** xdg_dirs(struct xdg* xdg, enum xdg_kind k)
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
        const char* h = xdg_dir(xdg, k);
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
                size_t l = xdg_append_app(xdg, LIT(q), p);
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

const char** xdg_data_dirs(struct xdg* xdg)
{
    return xdg_dirs(xdg, XDG_DATA);
}

const char** xdg_config_dirs(struct xdg* xdg)
{
    return xdg_dirs(xdg, XDG_CONFIG);
}

const char* xdg_resolvev(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps)
{
    const char** dirs = xdg_dirs(xdg, k);
    for(size_t i = 0; dirs[i]; i++) {
        va_list qs;
        va_copy(qs, ps);
        size_t l = path_joinv(buf, L, dirs[i], qs);
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

const char* xdg_resolve(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...)
{
    va_list ps; va_start(ps, L);
    const char* p = xdg_resolvev(xdg, k, buf, L, ps);
    return va_end(ps), p;
}

const char* xdg_resolvevs(struct xdg* xdg, enum xdg_kind k, va_list ps)
{
    static char buf[PATH_MAX];
    return xdg_resolvev(xdg, k, LIT(buf), ps);
}

const char* xdg_resolves(struct xdg* xdg, enum xdg_kind k, ...)
{
    va_list ps; va_start(ps, k);
    const char* p = xdg_resolvevs(xdg, k, ps);
    return va_end(ps), p;
}

void xdg_makedirs(const char* path)
{
    int r = makedirs(path, 0700);
    CHECK(r, "makedirs(%s, 0700)", path);
}

const char* xdg_preparev(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, va_list ps)
{
    size_t l = path_joinv(buf, L, xdg_dir(xdg, k), ps);
    if(l >= L) {
        failwith("buffer overflow");
    }

    struct stat st;
    int r = stat(buf, &st);
    if(r == -1 && errno == ENOENT) {
        char d[l+1];
        memcpy(d, buf, l+1);
        xdg_makedirs(dirname(d));
        return buf;
    }
    CHECK(r, "stat(%s)", buf);

    return buf;
}

const char* xdg_prepare(struct xdg* xdg, enum xdg_kind k, char* buf, size_t L, ...)
{
    va_list ps; va_start(ps, L);
    const char* p = xdg_preparev(xdg, k, buf, L, ps);
    return va_end(ps), p;
}

const char* xdg_preparevs(struct xdg* xdg, enum xdg_kind k, va_list ps)
{
    static char buf[PATH_MAX];
    return xdg_preparev(xdg, k, LIT(buf), ps);
}

const char* xdg_prepares(struct xdg* xdg, enum xdg_kind k, ...)
{
    va_list ps; va_start(ps, k);
    const char* p = xdg_preparevs(xdg, k, ps);
    return va_end(ps), p;
}

// libr: path.c

#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <limits.h>

size_t path_joinv(char* buf, size_t L, const char* p0, va_list ps)
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

size_t path_join(char* buf, size_t L, const char* p0, ...)
{
    va_list ps; va_start(ps, p0);
    size_t n = path_joinv(buf, L, p0, ps);
    return va_end(ps), n;
}

#ifdef failwith
const char* path_joinvs(const char* p0, va_list ps)
{
    static char buf[PATH_MAX];
    size_t l = path_joinv(buf, sizeof(buf), p0, ps);
    if(l >= sizeof(buf)) {
        failwith("buffer overflow");
    }

    return buf;
}

const char* path_joins(const char* p0, ...)
{
    va_list ps; va_start(ps, p0);
    const char* p = path_joinvs(p0, ps);
    return va_end(ps), p;
}
#endif

int makedirs(const char* path, mode_t mode)
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
#endif // LIBR_IMPLEMENTATION
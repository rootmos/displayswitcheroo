// libr 0.2.0 (b006e2eaf847df7c816030d8f2bfbee7fa0de225) (https://github.com/rootmos/libr.git) (2023-08-31T07:12:21+02:00)
// modules: logging now lua fail util

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
#endif // LIBR_IMPLEMENTATION

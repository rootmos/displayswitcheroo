// libr 0.2.0 (6cf70e68c571974f136d75bf11866659a15d4a43) (https://github.com/rootmos/libr.git) (2023-08-11T15:22:20+02:00)
// modules: logging now

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
#endif // LIBR_IMPLEMENTATION

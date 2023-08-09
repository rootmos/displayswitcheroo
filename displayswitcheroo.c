#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#define LIBR_IMPLEMENTATION
#include "r.h"

struct state {
    Display* dpy;
};

static int handle_x11_error(Display* d, XErrorEvent* e)
{
    char buf[1024];
    if(!XGetErrorText(d, e->error_code, LIT(buf))) {
        failwith("XGetErrorText failed");
    }
    error("x11: %s", buf);
    return 0;
}

static void x11_init(struct state* st)
{
    XSetErrorHandler(handle_x11_error);

    st->dpy = XOpenDisplay(NULL);
    if(st->dpy == NULL) failwith("unable to open display");

    int major, minor;
    if(!XRRQueryVersion(st->dpy, &major, &minor)) {
        failwith("XRRQueryVersion failed");
    }
    debug("Xrandr version: %d.%d", major, minor);
}

static void x11_deinit(struct state* st)
{
    XCloseDisplay(st->dpy);
}

int main(int argc, char* argv[])
{
    struct state st = {};

    x11_init(&st);

    info("hello");

    x11_deinit(&st);

    return 0;
}

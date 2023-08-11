#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

#define LIBR_IMPLEMENTATION
#include "r.h"

struct state {
    Display* dpy;
    int scr;
    Window root;
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

    st->scr = XDefaultScreen(st->dpy);
    st->root = XRootWindow(st->dpy, st->scr);

    int major, minor;
    if(!XRRQueryVersion(st->dpy, &major, &minor)) {
        failwith("XRRQueryVersion failed");
    }
    debug("Xrandr version: %d.%d", major, minor);
}

static void hello_xrandr(struct state* st)
{
    XRRScreenResources* res = XRRGetScreenResources(st->dpy, st->root);
    if(!res) {
        failwith("XRRGetScreenResources failed");
    }

    for(int i = 0; i < res->ncrtc; i++) {
        XRRCrtcInfo* ci = XRRGetCrtcInfo(st->dpy, res, res->crtcs[i]);
        if(!ci) {
            failwith("XRRGetCrtcInfo(%d) failed", i);
        }

        debug("CRTC %d: %dx%d+%d+%d", i, ci->width, ci->height, ci->x, ci->y);
        debug("  noutput: %d", ci->noutput);

        XRRFreeCrtcInfo(ci);
    }

    for(int i = 0; i < res->noutput; i++) {
        XRROutputInfo* oi = XRRGetOutputInfo(st->dpy, res, res->outputs[i]);
        if(!oi) {
            failwith("XRRGetOutputInfo(%d) failed", i);
        }

        const char* connection;
        switch(oi->connection) {
            case 0: connection = "connected"; break;
            case 1: connection = "disconnected"; break;
            case 2:
            default:
                connection = "unknown connection";
                warning("output %s: unknown connection", oi->name);
        }

        debug("Output %d (%s) %s", i, oi->name, connection);
        debug("  ncrtc: %d", oi->ncrtc);

        XRRFreeOutputInfo(oi);
    }

    XRRFreeScreenResources(res);

    return;

    XRRScreenConfiguration* sc = XRRGetScreenInfo(st->dpy, st->root);
    if(!sc) {
        failwith("XRRGetScreenInfo failed");
    }

    int ss_n;
    XRRScreenSize* ss = XRRConfigSizes(sc, &ss_n);

    for(int i = 0; i < ss_n; i++) {
        info("%d: %dx%d", i, ss[i].width, ss[i].height);
    }

    XRRFreeScreenConfigInfo(sc);
}

static void x11_deinit(struct state* st)
{
    XCloseDisplay(st->dpy);
}

int main(int argc, char* argv[])
{
    struct state st = {};

    x11_init(&st);

    hello_xrandr(&st);

    x11_deinit(&st);

    return 0;
}

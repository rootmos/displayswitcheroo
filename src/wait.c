#include "wait.h"
#include "r.h"

#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <string.h>
#include <sys/signalfd.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

struct state {
    int running;
    int pending_trigger;

    Display* dpy;
    Window root;
    int xrr_event_base;
    int xrr_error_base;

    int sfd;

    struct {
        RROutput id;
        char* name;
        Connection connection;
    } outputs[128];
};

static int handle_x11_error(Display* d, XErrorEvent* e)
{
    char buf[1024];
    XGetErrorText(d, e->error_code, LIT(buf));
    error("x11: %s", buf);
    return 0;
}

static void x11_init(struct state* st)
{
    XSetErrorHandler(handle_x11_error);

    char* dn = XDisplayName(NULL);
    debug("connecting to display: %s", dn);

    st->dpy = XOpenDisplay(dn);
    if(st->dpy == NULL) {
        failwith("unable to connect to display %s", dn);
    }

    int major, minor;
    if(!XRRQueryVersion(st->dpy, &major, &minor)) {
        failwith("XRRQueryVersion failed");
    }

    if(major > 1 || (major == 1 && minor >= 2)) {
        debug("Xrandr version: %d.%d", major, minor);
    } else {
        failwith("Xrandr has unsupported version: %d.%d < 1.2", major, minor);
    }

    if(!XRRQueryExtension(st->dpy, &st->xrr_event_base, &st->xrr_error_base)) {
        failwith("XRRQueryExtension failed");
    }

    st->root = XRootWindow(st->dpy, XDefaultScreen(st->dpy));
    info("tracking connection changes of root window: %lu", st->root);

    XRRSelectInput(st->dpy, st->root, RROutputChangeNotifyMask);

    XSync(st->dpy, False);
}

static void x11_deinit(struct state* st)
{
    XCloseDisplay(st->dpy);
}

static int x11_fd(const struct state* st)
{
    return XConnectionNumber(st->dpy);
}

static const char* connection2str(Connection c)
{
    switch(c) {
        case RR_Connected: return "connected";
        case RR_Disconnected: return "disconnected";
        case RR_UnknownConnection: return "unknown connection";
        default: failwith("unexpected connection value: %d", c);
    }
}

static void outputs_init(struct state* st)
{
    XRRScreenResources* res = XRRGetScreenResourcesCurrent(st->dpy, st->root);
    if(!res) {
        failwith("XRRGetScreenResourcesCurrent failed");
    }

    if(res->noutput > LENGTH(st->outputs)) {
        failwith("too many outputs!");
    }

    for(int i = 0; i < res->noutput; i++) {
        RROutput id = st->outputs[i].id = res->outputs[i];
        XRROutputInfo* oi = XRRGetOutputInfo(st->dpy, res, id);

        if(!oi) {
            failwith("XRRGetOutputInfo(%lu) failed", id);
        }

        char* name = st->outputs[i].name = strdup(oi->name);
        CHECK_MALLOC(name);
        st->outputs[i].connection = oi->connection;

        debug("initial output %s: %s", name, connection2str(st->outputs[i].connection));

        XRRFreeOutputInfo(oi);
    }

    for(int i = res->noutput; i < LENGTH(st->outputs); i++) {
        st->outputs[i].id = 0;
    }

    XRRFreeScreenResources(res);
}

static int outputs_update(struct state* st, RROutput id, Connection c)
{
    for(size_t i = 0; i < LENGTH(st->outputs); i++) {
        if(st->outputs[i].id == id) {
            if(st->outputs[i].connection == c) {
                debug("output %s: no change", st->outputs[i].name);
                return 0;
            } else {
                info("output %s: %s -> %s", st->outputs[i].name, connection2str(st->outputs[i].connection), connection2str(c));
                st->outputs[i].connection = c;
                return 1;
            }
        } else if(st->outputs[i].id == 0) {
            failwith("hot-plugging not implemented");
        }
    }

    failwith("too many outputs!");
}

static void x11_handle_event(struct state* st)
{
    while(XPending(st->dpy)) {
        XEvent ev;
        int r = XNextEvent(st->dpy, &ev);
        CHECK_IF(r != Success, "XNextEvent");

        const int rrnotify = st->xrr_event_base + RRNotify;
        if(ev.type == rrnotify) {
            XRRNotifyEvent* ne = (XRRNotifyEvent*)&ev;
            if(ne->subtype == RRNotify_OutputChange) {
                XRROutputChangeNotifyEvent* oc = (XRROutputChangeNotifyEvent*)ne;
                if(outputs_update(st, oc->output, oc->connection)) {
                    st->pending_trigger = 1;
                }
            } else {
                warning("ignored RRNotify event: subttype=%d", ne->subtype);
            }
        } else {
            warning("ignored event: type=%d", ev.type);
        }
    }
}


static int signalfd_init(struct state* st)
{
    sigset_t m;
    sigemptyset(&m);
    sigaddset(&m, SIGINT);
    sigaddset(&m, SIGTERM);

    int fd = st->sfd = signalfd(-1, &m, 0);
    CHECK(fd, "signalfd");

    int r = sigprocmask(SIG_BLOCK, &m, NULL);
    CHECK(r, "sigprocmask");

    set_blocking(fd, 0);

    return fd;
}

static int signalfd_fd(const struct state* st)
{
    return st->sfd;
}

static void signalfd_deinit(struct state* st)
{
    int r = close(st->sfd); CHECK(r, "close");
    st->sfd = -1;
}

static void signalfd_handle_event(struct state* st)
{
    while(1) {
        struct signalfd_siginfo si;

        ssize_t s = read(st->sfd, &si, sizeof(si));
        if(s == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
            break;
        }
        CHECK(s, "read");

        if(s != sizeof(si)) {
            failwith("unexpected partial read");
        }

        if(si.ssi_signo == SIGINT) {
            debug("SIGINT");
            st->running = 0;
        } else if(si.ssi_signo == SIGTERM) {
            debug("SIGTERM");
            st->running = 0;
        } else {
            warning("unhandled signal: %u", si.ssi_signo);
        }
    }
}

void run_wait_loop(void (*trigger)(void*), void* opaque)
{
    struct state st = {
        .running = 1,
        .pending_trigger = 0,
    };

    signalfd_init(&st);
    x11_init(&st);
    outputs_init(&st);

    struct pollfd fds[] = {
        { .fd = signalfd_fd(&st), .events = POLLIN },
        { .fd = x11_fd(&st), .events = POLLIN },
    };

    while(st.running) {
        int r = poll(fds, LENGTH(fds), -1);
        CHECK(r, "poll");

        if(fds[0].revents & POLLIN) {
            signalfd_handle_event(&st);
            fds[0].revents &= ~POLLIN;
        }

        if(fds[1].revents & POLLIN) {
            x11_handle_event(&st);
            fds[1].revents &= ~POLLIN;
        }

        for(size_t i = 0; i < LENGTH(fds); i++) {
            if(fds[i].revents != 0) {
                failwith("unhandled poll events: "
                         "fds[%zu] = { .fd = %d, .revents = %hd }",
                         i, fds[i].fd, fds[i].revents);
            }
        }

        if(st.pending_trigger) {
            debug("running trigger");
            trigger(opaque);
            st.pending_trigger = 0;
        }
    }

    debug("graceful shutdown");
    x11_deinit(&st);
    signalfd_deinit(&st);

    info("bye");
}

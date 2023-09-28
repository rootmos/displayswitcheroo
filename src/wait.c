#include "wait.h"
#include "r.h"

#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <sys/signalfd.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

struct state {
    int running;
    Display* dpy;

    int sfd;
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

    Window root = XRootWindow(st->dpy, XDefaultScreen(st->dpy));
    info("tracking connection changes of %lu", root);

    XRRSelectInput(st->dpy, root, RROutputChangeNotifyMask);
}

static void x11_deinit(struct state* st)
{
    XCloseDisplay(st->dpy);
}

static int x11_fd(const struct state* st)
{
    return XConnectionNumber(st->dpy);
}

static void x11_handle_event(struct state* st)
{
    while(XPending(st->dpy)) {
        XEvent ev;
        int r = XNextEvent(st->dpy, &ev);
        CHECK_IF(r != Success, "XNextEvent");

        warning("ignored event: type=%d", ev.type);
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

void run_wait_loop(void)
{
    struct state st = {
        .running = 1,
    };

    signalfd_init(&st);
    x11_init(&st);

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
    }

    debug("graceful shutdown");
    x11_deinit(&st);
    signalfd_deinit(&st);

    info("bye");
}

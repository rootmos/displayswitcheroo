local X11 = require("x11")
local dpy <close> = X11.connect()

local setup = dpy.xrandr:fetch();

local screen = setup.screen
print(string.format("screen %d: %dx%d (min %dx%d, max %dx%d) (window %d)",
    screen.number, screen.width, screen.height,
    screen.min.width, screen.min.height,
    screen.max.width, screen.max.height,
    screen.window))

for n, o in pairs(setup.outputs) do
    assert(n == o.name)
    if o.connected then
        local c = o.crtc
        print(string.format("output %s: %dx%d+%d+%d (%dmm x %dmm)", n,
            c.width, c.height, c.x, c.y,
            o.mmwidth, o.mmheight))
        local m0 = c.mode
        for _, m in ipairs(o.modes) do
            local pref = o.modes:is_preferred(m) and "+" or " "
            local cur = m == m0 and "*" or " "
            print(string.format(" %s%s%dx%d %fHz", cur, pref, m.width, m.height, m.refresh_rate))
        end
    else
        print(string.format("output %s", n))
    end
end

for n, m in pairs(setup.monitors) do
    assert(n == m.name)
    print(string.format("monitor %s:%s%s%s %dx%d+%d+%d (%dmm x %dmm)",
            n,
            m.active and " active" or "",
            m.primary and " primary" or "",
            m.automatic and " automatic" or "",
            m.width, m.height, m.x, m.y, m.mmwidth, m.mmheight))

    for _, o in ipairs(m.outputs) do
        print(string.format("  output %s", o.name))
    end
end

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
    print(string.format("output %s", n))
    if o.connected then
        local m0 = o.crtc.mode
        for _, m in ipairs(o.modes) do
            local p = o.modes:is_preferred(m) and "+" or " "
            local c = m == m0 and "*" or " "
            print(string.format(" %s%s%dx%d %fHz", c, p, m.width, m.height, m.refresh_rate))
        end
    end
end

for n, m in pairs(setup.monitors) do
    assert(n == m.name)
    print(string.format("monitor %s: %s%s%dx%d+%d+%d (%dmm x %dmm)",
            n,
            m.active and "active " or "",
            m.primary and "primary " or "",
            m.width, m.height, m.x, m.y, m.mwidth, m.mheight))

    for _, o in ipairs(m.outputs) do
        print(string.format("  output %s", o.name))
    end
end

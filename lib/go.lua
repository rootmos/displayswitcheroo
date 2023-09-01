local X11 = require("x11")
local dpy <close> = X11.connect()
print(string.format("root: %d", dpy.root))
print(string.format("screen: %d", dpy.screen))
print(string.format("xrandr: %s", dpy.xrandr.version))

local setup = dpy.xrandr:fetch();

for n, o in pairs(setup.outputs) do
    assert(n == o.name)
    print(string.format("output %s: connected=%s", n, o.connected))

    print(string.format("  crtc: %s", o.crtc))

    for _, m in ipairs(o.modes) do
        local p = o.modes:is_preferred(m) and "+" or " "
        print(string.format(" %s%dx%d %fHz", p, m.width, m.height, m.refresh_rate))
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

for i, c in ipairs(setup.crtcs) do
    assert(i == c.index + 1)
    if c.enabled then
        print(string.format("crtc %d: %dx%d+%d+%d %.2fHz", c.index,
            c.width, c.height, c.x, c.y, c.mode.refresh_rate))
    else
        print(string.format("crtc %d", c.index))
    end

    for _, o in ipairs(c.outputs) do
        print(string.format("  connected output %s", o))
    end

    for _, o in ipairs(c.outputs.possible) do
        print(string.format("  possible output %s", o))
    end
end

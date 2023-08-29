local X11 = require("x11")
local dpy <close> = X11.connect()
print(string.format("root: %d", dpy.root))
print(string.format("screen: %d", dpy.screen))
print(string.format("xrandr: %s", dpy.xrandr.version))

local setup = dpy.xrandr:fetch();

for n, o in pairs(setup.outputs) do
    assert(n == o.name)
    print(string.format("output %s: connected=%s", n, o.connected))

    for _, m in ipairs(o.modes) do
        print(string.format("  %dx%d %fHz", m.width, m.height, m.refresh_rate))
    end

    for _, m in ipairs(o.modes.preferred) do
        print(string.format(" *%dx%d %fHz", m.width, m.height, m.refresh_rate))
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

local X11 = require("x11")
local dpy <close> = X11.connect()
print(string.format("root: %d", dpy.root))
print(string.format("screen: %d", dpy.screen))
print(string.format("xrandr: %s", dpy.xrandr.version))

local setup = dpy.xrandr:fetch();
for i, o in ipairs(setup.outputs) do
    print(string.format("output %s: connected=%s", o.name, o.connected))
end

for n, m in pairs(setup.monitors) do
    assert(n == m.name)
    print(string.format("monitor %s: %s%s%dx%d+%d+%d (%dmm x %dmm)",
            n,
            m.active and "active " or "",
            m.primary and "primary " or "",
            m.width, m.height, m.x, m.y, m.mwidth, m.mheight))
end

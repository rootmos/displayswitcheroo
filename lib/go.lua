local X11 = require("x11")
local dpy <close> = X11.connect()
print(string.format("root: %d", dpy.root))
print(string.format("screen: %d", dpy.screen))
print(string.format("xrandr: %s", dpy.xrandr.version))

local setup = dpy.xrandr:fetch();
for i, o in ipairs(setup.outputs) do
    print(string.format("output %s: connected=%s", o.name, o.connected))
end

for i, m in ipairs(setup.monitors) do
    print(string.format("monitor %s", m.name))
end

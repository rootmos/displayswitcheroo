local X11 = require("x11")
local dpy <close> = X11.connect()
print(string.format("root: %d", dpy.root))
print(string.format("screen: %d", dpy.screen))
print(string.format("xrandr: %s", dpy.xrandr.version))
for i, o in ipairs(dpy.xrandr()) do
    print(string.format("output %s: connected=%s", o.name, o.connected))
end

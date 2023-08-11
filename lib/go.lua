local X11 = require("x11")
local dpy <close> = X11.connect(":1")
print(dpy)

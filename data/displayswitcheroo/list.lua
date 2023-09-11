local X11 = require("x11")
local dpy = X11.connect()

local setup = dpy.xrandr:fetch();

local screen = setup.screen
print(string.format("screen %d: %dx%d (min %dx%d, max %dx%d) (%dmm x %dmm) (window %d)",
    screen.number, screen.width, screen.height,
    screen.min.width, screen.min.height,
    screen.max.width, screen.max.height,
    screen.mmwidth, screen.mmheight,
    screen.window))

for n, o in pairs(setup.outputs) do
    if o.connected then
        local c = o.crtc
        local pri = o.primary and " primary" or ""
        print(string.format("output %s:%s %dx%d+%d+%d (%dmm x %dmm)", n, pri,
            c.width, c.height, c.x, c.y,
            o.mmwidth, o.mmheight))

        local s, w, h
        for _, m in ipairs(o.modes) do
            local flags = ""
            if m.interlace then
                flags = flags .. "i"
            end

            if m.double_scan then
                flags = flags .. "d"
            end

            if o.modes:is_preferred(m) then
                flags = flags .. "+"
            end

            if m == c.mode then
                flags = flags .. "*"
            end

            local t = string.format(" %.2f%s", m.refresh_rate, flags)

            if w == m.width and h == m.height then
                s = s .. t
            else
                if s then
                    print(s)
                end
                w = m.width
                h = m.height
                s = string.format("   %dx%d%s", w, h, t)
            end
        end
        if s then
            print(s)
        end
    else
        print(string.format("output %s", n))
    end
end

for n, m in pairs(setup.monitors) do
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

local dpy = require("x11").connect()
local setup = dpy.xrandr:fetch()

local int = setup.outputs["eDP-1"]
local ext = setup.outputs["HDMI-2"]

local function find_suitable_crtc(o)
    if o.crtc then
        return o.crtc
    end

    for _, c in ipairs(setup.crtcs) do
        if not c.enabled then
            for _, p in pairs(c.outputs.possible) do
                if o == p then
                    return c
                end
            end
        end
    end
end

local function one(o)
    local m = o.modes.preferred[1]
    setup:set_crtc {
        id = find_suitable_crtc(o).id,
        x = 0,
        y = 0,
        mode = m,
        outputs = { o },
    }

    for _, p in pairs(setup.outputs) do
        if o ~= p and p.crtc then
            setup:disable_crtc(p.crtc.id)
        end
    end

    if not o.primary then
        setup:set_primary(o.id)
    end

    setup:set_screen_size {
        width = m.width,
        height = m.height,
        mmwidth = o.mmwidth,
        mmheight = o.mmheight,
    }
end

local function left_right(l, r)
    local lm = l.modes.preferred[1]
    setup:set_crtc {
        id = find_suitable_crtc(l).id,
        x = 0,
        y = 0,
        mode = lm,
        outputs = { l },
    }

    local rm = r.modes.preferred[1]
    setup:set_crtc {
        id = find_suitable_crtc(r).id,
        x = lm.width,
        y = 0,
        mode = rm,
        outputs = { r },
    }

    for _, p in pairs(setup.outputs) do
        if l ~= p and r ~= p and p.crtc then
            setup:disable_crtc(p.crtc.id)
        end
    end

    if not l.primary then
        setup:set_primary(l.id)
    end

    setup:set_screen_size {
        width = lm.width + rm.width,
        height = math.max(lm.width, rm.width),
        mmwidth = math.max(l.mmwidth, r.mmwidth),
        mmheight = math.max(l.mmheight, r.mmheight),
    }
end

if ext.fingerprint == 0x7ad8cb74 then
    print("desktop")
    left_right(ext, int)
elseif ext.fingerprint == 0xc0e5ff0c then
    print("living room")
    one(ext)
else
    print("laptop")
    one(int)
end

dpy:sync()

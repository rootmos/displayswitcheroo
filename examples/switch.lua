local D = require("displayswitcheroo")()

local laptop = D["eDP-1"]
local desktop = D[0x7ad8cb74]
local livingroom = D[0xc0e5ff0c]

local cooloff = nil

if desktop then
    D.left_right(desktop, laptop)
    cooloff = 10
elseif livingroom then
    D.one(livingroom)
else
    D.one(laptop)
end

D.sync()

if cooloff then
    print(string.format("letting monitors settle: %ds...", cooloff))
    os.execute("sleep " .. cooloff)
    print("  ...and were (hopefully) stable")
end

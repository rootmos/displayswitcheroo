local dpy = require("x11").connect()
local setup = dpy.xrandr:fetch()

for _, o in pairs(setup.outputs) do
    if o.connected then
        local edid = o.EDID
        print(string.format("%s: (fpr 0x%x)", o.name, edid.fingerprint))
        print(string.format("  vendor: %s", edid.vendor))
        print(string.format("  product_code: %d", edid.product_code))
        print(string.format("  serial_number: %d", edid.serial_number))
        if edid.year_of_model then
            print(string.format("  year of model: %d", edid.year_of_model))
        else
            print(string.format("  week of manufacture: %d", edid.week_of_manufacture))
            print(string.format("  year of manufacture: %d", edid.year_of_manufacture))
        end
        print(string.format("  version: %d.%d", edid.version_major, edid.version_minor))
    end
end

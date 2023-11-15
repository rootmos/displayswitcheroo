displayswitcheroo
=================

[![Tests](https://github.com/rootmos/displayswitcheroo/actions/workflows/tests.yaml/badge.svg)](https://github.com/rootmos/displayswitcheroo/actions/workflows/tests.yaml)

A [Lua](https://lua.org) c-module wrapping the [XRandr](https://www.x.org/wiki/Projects/XRandR/) X11 extension,
and a bundled command-line application with the ~~batteries~~ module included.

(The previous [Haskell](https://www.haskell.org/) implementation can be found in the [haskell](https://github.com/rootmos/displayswitcheroo/tree/haskell) branch.)

## Usage
```
usage: displayswitcheroo [OPTION]... [SCRIPT]

options:
  -i       enter interactive mode after executing SCRIPT
  -w       wait for output connect/disconnects
  -1       run once before waiting
  -g       keep global state between runs
  -h       print this message
  -v       print version information
```

## Example: display current state
The [list.lua](data/displayswitcheroo/list.lua) script prints a [`xrandr(1)`](https://man.archlinux.org/man/xrandr.1)-like output:
```
screen 0: 1920x1080 (min 320x200, max 16384x16384) (531mm x 299mm) (window 1946)
output HDMI-1
output eDP-1: (309mm x 173mm) (fpr 0x57a9d7d6)
   1920x1080 59.98+ 59.97d 59.96 59.93
   1680x1050 59.95 59.88
   1400x1050 59.98
   1600x900 59.99d 59.94d 59.95 59.82
   1280x1024 60.02
   1400x900 59.96 59.88
   1280x960 60.00
   1440x810 60.00d 59.97d
   1368x768 59.88 59.85
   1280x800 59.99d 59.97d 59.81 59.91
   1280x720 60.00d 59.99d 59.86 59.74
   1024x768 60.04d 60.00
   960x720 60.00d
   928x696 60.05d
   896x672 60.01d
   1024x576 59.95d 59.96d 59.90 59.82
   960x600 59.93d 60.00d
   960x540 59.96d 59.99d 59.63 59.82
   800x600 60.00d 60.32 56.25
   840x525 60.01d 59.88d
   864x486 59.92 59.57
   700x525 59.98d
   800x450 59.95d 59.82d
   640x512 60.02d
   700x450 59.96d 59.88d
   640x480 60.00d 59.94
   720x405 59.51 58.99
   684x384 59.88d 59.85d
   640x400 59.88d 59.98d
   640x360 59.86d 59.83d 59.84 59.32
   512x384 60.00d
   512x288 60.00d 59.92d
   480x270 59.63d 59.82d
   400x300 60.32d 56.34d
   432x243 59.92d 59.57d
   320x240 60.05d
   360x202 59.51d 59.13d
   320x180 59.84d 59.32d
output DP-2
output HDMI-2: primary 1920x1080+0+0 (531mm x 299mm) (fpr 0x43b791a2)
   1920x1080 60.00+* 50.00 59.94 60.00i 50.00i 59.94i
   1680x1050 59.88
   1600x900 60.00
   1280x1024 75.02 60.02
   1280x800 59.91
   1152x864 75.00
   1280x720 60.00 50.00 59.94
   1024x768 75.03 60.00
   832x624 74.55
   800x600 75.00 60.32
   720x576 50.00 50.00i
   720x480 60.00 59.94 60.00i 59.94i
   640x480 75.00 60.00 59.94
   720x400 70.08
output DP-1
monitor HDMI-2: active primary automatic 1920x1080+0+0 (531mm x 299mm)
  output HDMI-2
```

## Example: an automated display switcher
This is the raison raison d'être for this project.
If you check closer at the output of the previous example you can find the non-standard `fpr` field for each connected output.
Combining these fingerprints and the `-w` flag that waits for output state changes: one gets a "monitor-aware" display switcher:
```lua
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
```
Notice how easy it became to work around the ~10 second startup time of my main desktop monitor!

The [`displayswitcheroo`](data/displayswitcheroo/displayswitcheroo.lua) module is a small module combining
the fetching of the current xrandr state and some helpers to organinze a basic multi-monitor setup.

## Installation and where do I put my own scripts?
There's a install script: runs `make` and `install`s the binary.
```
./install.sh
```
It also installs the [list.lua](data/displayswitcheroo/list.lua) script and the [`displayswitcheroo`](data/displayswitcheroo/displayswitcheroo.lua) helper module
(mainly as a runnable example to start hacking with).

This application and installer tries to follow the [XDG specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html):
* the binary will be installed at `$HOME/.local/bin/displayswitcheroo`
* the XDG data directories are added the lua search paths (see the [`add_xdg_to_search_paths`](https://github.com/rootmos/displayswitcheroo/blob/01db6c8a41886fbad86cf45ac4bc45cc157dcc22/src/cli.c#L111) function)
* when called with a `SCRIPT` argument will be resolved: (see the [`resolve_script`](https://github.com/rootmos/displayswitcheroo/blob/01db6c8a41886fbad86cf45ac4bc45cc157dcc22/src/cli.c#L180) function)
  - first as a path (then as `SCRIPT.lua`)
  - then tried as a path relative the XDG config directories (then again with `.lua` added)
  - then tried as a path relative the XDG data directories (the again again with `.lua` added)

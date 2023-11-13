displayswitcheroo
=================

A [Lua](https://lua.org) c-module wrapping the [XRandr](https://www.x.org/wiki/Projects/XRandR/) X11 extension,
and a bundled command-line application with the ~~batteries~~ module included.

(The previous [Haskell](https://www.haskell.org/) implementation can be found in the [haskell](https://github.com/rootmos/displayswitcheroo/tree/haskell) branch.)

## Usage
@include "usage.displayswitcheroo"

## Example: display current state
The [list.lua](data/displayswitcheroo/list.lua) script prints a [`xrandr(1)`](https://man.archlinux.org/man/xrandr.1)-like output:
```
@include "output.list"
```

## Example: an automated display switcher
This is the raison raison d'être for this project.
If you check closer at the output of the previous example you can find the non-standard `fpr` field for each connected output.
Combining these fingerprints and the `-w` flag that waits for output state changes: one gets a "monitor-aware" display switcher:
```lua
@include "../examples/switch.lua"
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

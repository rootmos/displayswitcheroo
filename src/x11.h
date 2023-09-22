#pragma once

#define TYPE_DISPLAY "x11.display"
#define TYPE_ATOM "x11.atom"
#define TYPE_XRANDR "x11.xrandr"
#define TYPE_EDID "x11.edid"
#define TYPE_XRANDR_CRTC "x11.xrandr.crtc"
#define TYPE_XRANDR_OUTPUT "x11.xrandr.output"
#define TYPE_XRANDR_OUTPUT_MODES "x11.xrandr.output.modes"
#define TYPE_XRANDR_SETUP "x11.xrandr.setup"
#define TYPE_XRANDR_MONITOR "x11.xrandr.monitor"
#define TYPE_XRANDR_MODE "x11.xrandr.mode"
#define TYPE_XRANDR_SCREEN "x11.xrandr.screen"

int luaopen_x11(lua_State* L);

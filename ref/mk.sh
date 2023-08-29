#!/bin/bash

set -o nounset -o pipefail -o errexit

SCRIPT_DIR=$(readlink -f "$0" | xargs dirname)
. "$SCRIPT_DIR/fetch.sh"

TARGET=${1-$SCRIPT_DIR}

fetch "https://cgit.freedesktop.org/xorg/app/xrandr/plain/xrandr.c?h=xrandr-1.5.2" \
    "a427b90d9da69b2decd307c45872be669b512c5c3dd5c7878e3ff61b5e4277e7" \
    "$TARGET/xrandr.c"

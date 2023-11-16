#!/bin/bash

set -o nounset -o pipefail -o errexit

SCRIPT_DIR=$(readlink -f "$0" | xargs dirname)

usage() {
    cat <<EOF 1>&2
Usage: $(basename "$0") [options]
Options:
    -h          show this message
    -d DESTDIR  set DESTDIR
    -u          user installation: DESTDIR=~/.local
    -s          systemwide installation: DESTDIR=/usr
    -a APP      application name
EOF
    exit "${1-0}"
}

SUDO=${SUDO-}
DESTDIR=${DESTDIR-$HOME/.local}
DIR_MODE=0700
APP=${APP-displayswitcheroo}
while getopts "dusah-" OPT; do
    case $OPT in
        d) DESTDIR=$OPTARG ;;
        u) DESTDIR=$HOME/.local; DIR_MODE=0700;;
        s) DESTDIR=/usr; DIR_MODE=0755 ;;
        a) APP=$OPTARG ;;
        h) usage ;;
        -) break ;;
        ?) usage 2 ;;
    esac
done
shift $((OPTIND-1))

make -C "$SCRIPT_DIR" clean build EXTRA_CFLAGS="-DXDG_APP='\"$APP\"'"


$SUDO mkdir -pm "$DIR_MODE" "$DESTDIR/bin"
$SUDO install -v "$SCRIPT_DIR/src/cli.exe" "$DESTDIR/bin/$APP"

$SUDO mkdir -pm "$DIR_MODE" "$DESTDIR/share/$APP"
$SUDO install -v -D "$SCRIPT_DIR/data/displayswitcheroo/list.lua" "$DESTDIR/share/$APP/list.lua"
$SUDO install -v -D "$SCRIPT_DIR/data/displayswitcheroo/displayswitcheroo.lua" "$DESTDIR/share/$APP/$APP.lua"

#!/bin/bash

set -o nounset -o pipefail -o errexit

SUDO=${SUDO-}
DISTRO=${DISTRO-}
UPDATE=${UPDATE-}
while getopts "ud:sS:-" OPT; do
    case $OPT in
        d) DISTRO=$OPTARG ;;
        u) UPDATE=1 ;;
        s) SUDO=sudo ;;
        S) SUDO=$OPTARG ;;
        -) break ;;
        ?) usage 2 ;;
    esac
done
shift $((OPTIND-1))

if [ -z "$DISTRO" ]; then
    DISTRO=$(lsb_release -is)
fi
echo "distro: $DISTRO" 1>&2

if [ "$DISTRO" = "Arch" ]; then
    if [ -n "$UPDATE" ]; then
        $SUDO pacman -Sy
    fi
    $SUDO pacman -S lua libxrandr
elif [ "$DISTRO" = "Ubuntu" ]; then
    if [ -n "$UPDATE" ]; then
        $SUDO apt-get update 1>&2
    fi
    $SUDO apt-get install --yes --no-install-recommends 1>&2 \
        make \
        liblua5.4-dev libxrandr-dev
    echo "LUA_PKG=lua54"
else
    echo "unconfigured distribution: $DISTRO" 1>&2
    exit 1
fi

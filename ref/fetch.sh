#!/bin/bash

fetch() { (
    SYMLINK=
    FETCH_STYLE=${FETCH_STYLE-default}
    OPTIND=0
    while getopts "sS:-" OPT; do
        case $OPT in
            s) SYMLINK=1 ;;
            S) FETCH_STYLE=$OPTARG ;; # mega, giga
            -) break ;;
            ?) return 2 ;;
        esac
    done
    shift $((OPTIND-1))

    URL=$1
    SHA256=$2
    TARGET=$3
    FETCH_CACHE=${FETCH_CACHE-/tmp}
    CACHE=$FETCH_CACHE/$SHA256

    if [ ! -f "$CACHE" ]; then
        UNVERIFIED=$FETCH_CACHE/unverified/$SHA256
        mkdir -p "$(dirname "$UNVERIFIED")"
        wget --progress=dot:$FETCH_STYLE \
            --output-document="$UNVERIFIED" "$URL"

        SHA256_UNVERIFIED=$(sha256sum "$UNVERIFIED" | cut -f1 -d' ')
        if [ "$SHA256_UNVERIFIED" = "$SHA256" ]; then
            mkdir -p "$(dirname "$CACHE")"
            mv "$UNVERIFIED" "$CACHE"
        else
            echo "sha256 checksum failed ($SHA256_UNVERIFIED != $SHA256): $URL" >&2
            return 1
        fi
    fi

    if [ -n "$SYMLINK" ]; then
        if [ -L "$TARGET" ] && [ "$CACHE" = "$(readlink -f "$TARGET")" ]; then
            true
        else
            ln -s "$CACHE" "$TARGET"
        fi
    else
        cp "$CACHE" "$TARGET"
    fi
) }

if ! (return 0 2>/dev/null); then
    fetch "$1" "$2" "$3"
fi

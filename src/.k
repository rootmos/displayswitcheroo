export XDG_DATA_DIRS=$K_DIR/../data:$XDG_DATA_DIRS

cli() {
    make LOG_LEVEL=DEBUG cli.exe && ./cli.exe "${1-list}"
}

xvfb() {
    make LOG_LEVEL=DEBUG cli.exe && xvfb-run ./cli.exe "$K_DIR/../data/displayswitcheroo/list.lua"
}


watch() {
    make LOG_LEVEL=DEBUG cli.exe && ./cli.exe -w go.lua
}

switch() {
    make LOG_LEVEL=WARN cli.exe && ./cli.exe switch.lua
}

go() {
    make LOG_LEVEL=DEBUG x11.so && lua "$K_DIR/../data/displayswitcheroo/list.lua"
}

libr() {
    "$HOME/git/libr/bundle" -o r.h logging lua xdg sha1 nonblock
}

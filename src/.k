export XDG_DATA_DIRS=$K_DIR/../data:$XDG_DATA_DIRS

cli() {
    make LOG_LEVEL=DEBUG cli.exe && ./cli.exe list
}

go() {
    make LOG_LEVEL=DEBUG x11.so && lua "$K_DIR/../data/displayswitcheroo/list.lua"
}

libr() {
    "$HOME/git/libr/bundle" -o r.h logging lua xdg
}

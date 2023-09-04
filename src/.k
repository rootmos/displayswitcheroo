cli() {
    make LOG_LEVEL=DEBUG cli.exe && ./cli.exe
}

go() {
    make LOG_LEVEL=DEBUG x11.so && lua go.lua
}

libr() {
    "$HOME/git/libr/bundle" -o r.h logging lua xdg
}

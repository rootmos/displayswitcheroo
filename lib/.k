go() {
    make LOG_LEVEL=DEBUG && lua go.lua
}

libr() {
    "$HOME/git/libr/bundle" -o r.h logging
}

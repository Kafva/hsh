#!/usr/bin/env bash
set -e
info() { printf "\033[34m*\033[0m $1\n" >&2; }


INPUTFILE="${1:-README.md}"

clang -w rfc/Md5.c -o Md5

info "Md5 (rfc):"
/usr/bin/time ./Md5 < $INPUTFILE

info "md5:"
/usr/bin/time md5 < $INPUTFILE

info "hsh:"
cabal build -v0
hsh=$(find dist-newstyle -type f -name hsh)
/usr/bin/time $hsh -a md5 < $INPUTFILE

#!/usr/bin/env bash
set -e
info() { printf "\033[34m*\033[0m $1\n" >&2; }

_check_md5() {
    info "md5:"
    /usr/bin/time md5 < $INPUTFILE

    info "Md5 (RFC):"
    clang -w rfc/Md5.c -o Md5
    /usr/bin/time ./Md5 < $INPUTFILE

    info "hsh:"
    cabal build -v0
    hsh=$(find dist-newstyle -type f -name hsh)
    /usr/bin/time $hsh -a md5 < $INPUTFILE
}

_check_sha1() {
    info "sha1sum:"
    sha1sum < $INPUTFILE

    info "Sha1 (RFC):"
    clang -w rfc/Sha1.c -o Sha1
    ./Sha1 < $INPUTFILE

    info "hsh:"
    cabal build -v0
    hsh=$(find dist-newstyle -type f -name hsh)
    $hsh -d -a sha1 < $INPUTFILE
}

INPUTFILE="${2:-README.md}"

case "$1" in
md5)
    _check_md5
    ;;
sha1)
    _check_sha1
    ;;
*)
    echo  "usage: $(basename $0) <md5|sha1> [file]" >&2
;;
esac

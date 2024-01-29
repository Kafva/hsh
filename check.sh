#!/usr/bin/env bash
set -e
info() { printf "\033[34m*\033[0m $1\n" >&2; }

_check_md5() {
    info "md5:"
    /usr/bin/time md5 < $INPUTFILE

    info "Md5 (RFC):"
    clang -w rfc/Md5.c -o Md5
    /usr/bin/time ./Md5 < $INPUTFILE
}

_check_sha1() {
    info "sha1sum:"
    /usr/bin/time sha1sum < $INPUTFILE

    info "Sha1 (RFC):"
    clang -w rfc/Sha1.c -o Sha1
    ./Sha1 < $INPUTFILE
}

_check_sha256() {
    info "sha256sum:"
    /usr/bin/time sha256sum < $INPUTFILE

    info "Sha256 (RFC):"
    echo TODO
}

ALG="$1"
INPUTFILE="${2:-README.md}"

case "$1" in
md5)
    _check_md5
    ;;
sha1)
    _check_sha1
    ;;
sha256)
    _check_sha256
    ;;
*)
    echo  "usage: $(basename $0) <md5|sha1> [file] [hsh args]" >&2
    exit 1
;;
esac

info "hsh:"
cabal build -v0
hsh=$(find dist-newstyle -type f -name hsh)
/usr/bin/time $hsh -a $ALG ${@:3} < $INPUTFILE


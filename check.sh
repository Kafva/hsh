#!/usr/bin/env bash
set -e

info() { printf "\033[34m*\033[0m $1\n" >&2; }
usage() {
    cat << EOF
usage: check.sh verify [file]
                run <alg> [file] [hsh args]

EOF
    exit 1
}

run_md5() {
    info "md5:"
    time md5 < $INPUTFILE

    info "Md5 (RFC):"
    clang -w rfc/Md5.c -o Md5
    time ./Md5 < $INPUTFILE
}

run_sha1() {
    info "sha1sum:"
    time sha1sum < $INPUTFILE

    info "Sha1 (RFC):"
    clang -w rfc/Sha1.c -o Sha1
    ./Sha1 < $INPUTFILE
}

run_sha256() {
    info "sha256sum:"
    time sha256sum < $INPUTFILE

    info "Sha256 (RFC):"
    if [ -d ../RFC-6234 ]; then
        make -C ../RFC-6234
        ../RFC-6234/shatest -s "$(cat $INPUTFILE)" -h2
    else
        echo "Clone https://github.com/Madricas/RFC-6234.git into ../RFC-6234"
    fi
}

run_sha224() {
    info "sha224sum:"
    time sha224sum < $INPUTFILE

    info "Sha224 (RFC):"
    if [ -d ../RFC-6234 ]; then
        make -C ../RFC-6234
        ../RFC-6234/shatest -s "$(cat $INPUTFILE)" -h1
    else
        echo "Clone https://github.com/Madricas/RFC-6234.git into ../RFC-6234"
    fi
}

verify_ok() {
    local alg="$1"
    local expected="$2"
    local hsh_out="$($HSH -a $alg < $INPUTFILE)"
    if [ "$hsh_out" = "$expected" ]; then
        printf "[ \033[92m OK \033[0m ] $alg $hsh_out\n"
    else
        printf "[ \033[91mFAIL\033[0m ] $alg $hsh_out\n"
        exit 1
    fi
}

CMDTYPE="$1"
ALG="$2"
INPUTFILE="${3}"
export TIMEFORMAT="time:    %Rs"

cabal build -v0
HSH=$(find dist-newstyle -type f -name hsh)

case "$CMDTYPE" in
verify)
    if [ ! -f "$INPUTFILE" ]; then
        INPUTFILE=$(mktemp)
        dd if=/dev/random of=$INPUTFILE bs=1K count=1 2> /dev/null
    fi

    verify_ok md5 "$(md5 < $INPUTFILE)"
    verify_ok sha1 "$(sha1sum < $INPUTFILE | awk '{print $1}')"
    verify_ok sha224 "$(sha224sum < $INPUTFILE | awk '{print $1}')"
    verify_ok sha256 "$(sha256sum < $INPUTFILE | awk '{print $1}')"
;;
*)
    cmdname="${CMDTYPE}_${ALG}"
    typing="$(type "$cmdname" 2> /dev/null | head -n1 || :)"
    if [ "$typing" = "$cmdname is a function" ]; then
        [ -f "$INPUTFILE" ] || usage
        $cmdname ${@:2}

        info "hsh:"
        time $HSH -a $ALG ${@:3} < $INPUTFILE
    else
        usage
    fi
;;
esac

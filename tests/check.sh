#!/usr/bin/env bash
set -e

info() { printf "\033[34m*\033[0m $1\n" >&2; }
usage() {
    cat << EOF
usage: check.sh [CMD]

COMMANDS:
   verify [file]                 | Verify all algorithms with [file] as input data
   run <alg> [file] [hsh args]   | Run specific algorithm
EOF
    exit 1
}

run_md5() {
    info "md5:"
    time md5 < $INPUTFILE

    info "Md5 (RFC):"
    time ./tests/bin/Md5 < $INPUTFILE
}

run_sha1() {
    info "sha1sum:"
    time sha1sum < $INPUTFILE

    info "Sha1 (RFC):"
    ./tests/bin/Sha1 < $INPUTFILE
}

run_sha256() {
    info "sha256sum:"
    time sha256sum < $INPUTFILE

    info "Sha256 (RFC):"
    tests/rfc/RFC-6234/shatest -s "$(cat $INPUTFILE)" -h2
}

run_sha224() {
    info "sha224sum:"
    time sha224sum < $INPUTFILE

    info "Sha224 (RFC):"
    tests/rfc/RFC-6234/shatest -s "$(cat $INPUTFILE)" -h1
}

run_hmac() {
    info "golang/x/crypto/hmac:"
    time tests/bin/hmac "$(cat $INPUTFILE)" $HMAC_KEY
}

verify_ok() {
    local alg="$1"
    local expected="$2"
    local tmpfile=$(mktemp)

    { time $HSH -a $alg -k $HMAC_KEY < $INPUTFILE; } &> $tmpfile
    local hsh_out=$(sed -n '1p' $tmpfile)
    local time_taken=$(sed -n '2p' $tmpfile)

    rm $tmpfile

    if [ "$hsh_out" = "$expected" ]; then
        printf "[ \033[92m OK \033[0m ] %-10s %-7s %s\n" \
            "$time_taken" $alg "$hsh_out"
    else
        printf "[ \033[91mFAIL\033[0m ] %-10s %-7s %s\n" \
            "$time_taken" $alg "$hsh_out"
        exit 1
    fi
}

HMAC_KEY=abc
CMDTYPE="$1"

HSH=$(find dist-newstyle -type f -name hsh)

case "$CMDTYPE" in
verify)
    export TIMEFORMAT="%Rs"
    INPUTFILE="$2"

    if [ ! -f "$INPUTFILE" ]; then
        INPUTFILE=$(mktemp)
        dd if=/dev/random of=$INPUTFILE bs=1K count=32 2> /dev/null
    fi

    verify_ok md5 "$(md5sum < $INPUTFILE | awk '{print $1}')"
    verify_ok sha1 "$(sha1sum < $INPUTFILE | awk '{print $1}')"
    verify_ok sha224 "$(sha224sum < $INPUTFILE | awk '{print $1}')"
    verify_ok sha256 "$(sha256sum < $INPUTFILE | awk '{print $1}')"
;;
*)
    export TIMEFORMAT="time:    %Rs"
    ALG="$2"
    INPUTFILE="$3"

    cmdname="${CMDTYPE}_${ALG}"
    typing="$(type -t "$cmdname" 2> /dev/null | head -n1 || :)"
    if [ "$typing" = "function" ]; then
        [ -r "$INPUTFILE" ] || usage
        $cmdname ${@:2}

        info "hsh:"
        time $HSH -a $ALG ${@:4} -k <(echo -n $HMAC_KEY) < $INPUTFILE
    else
        usage
    fi
;;
esac

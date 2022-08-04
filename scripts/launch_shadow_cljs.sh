#!/bin/bash

set -e

CI_DIR=$(dirname "${BASH_SOURCE[0]}")
CI_DIR=$(readlink -f $CI_DIR)
PROJ_DIR=$(dirname $CI_DIR)
shadow_cljs_pidfile=${PROJ_DIR}/.shadow-cljs/server.pid

eecho() {
    printf "\033[33m"
    echo -n "$@"
    printf "\033[m\n"
}

hecho() {
    printf "\033[36m"
    echo -n "$@"
    printf "\033[m\n"
}

function err_and_quit () {
    printf "\n\n\033[33mError occured. \nPlease fix possible issues and try again.\033[m\n\n"
    exit 1;
}

wait_with_timeout() {
    local name=$1
    local maxwait=$2
    local f=$3

    hecho -n "Waiting for $name to be ready ."

    local elapsed=0
    while true; do
        if [[ $elapsed -gt $maxwait ]]; then
            echo
            eecho "ERROR: $name not ready after $maxwait seconds."
            err_and_quit
        fi
        if $f; then
            echo
            break
        fi
        echo -n "."
        sleep 1
        elapsed=$((elapsed+1))
    done
}

check_shadow_cljs() {
    [[ -f $shadow_cljs_pidfile ]]
}

wait_for_shadow_cljs_server() {
    wait_with_timeout "shadow-cljs server" 180 check_shadow_cljs
}


cd $PROJ_DIR
rm -f $shadow_cljs_pidfile
pnpm shadow-cljs server &
wait_for_shadow_cljs_server

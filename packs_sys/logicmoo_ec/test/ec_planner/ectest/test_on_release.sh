#!/bin/bash -x

stty echo
export CMD_TIMEOUT=1m
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*[0-9]*.*p*"
(
lmoo-junit "$GLOB"
)
stty echo


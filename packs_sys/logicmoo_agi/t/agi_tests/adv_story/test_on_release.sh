#!/bin/bash -x

export CMD_TIMEOUT=5m
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*01.*"
(
lmoo-junit "$GLOB"
)
stty echo


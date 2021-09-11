#!/bin/bash -x

export CMD_TIMEOUT=5m 
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"


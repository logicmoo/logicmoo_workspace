#!/bin/bash -x

export CMD_TIMEOUT=3m 
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"


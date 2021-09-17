#!/bin/bash -x

export CMD_TIMEOUT=5m 
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*.plt"
lmoo-junit "$GLOB"


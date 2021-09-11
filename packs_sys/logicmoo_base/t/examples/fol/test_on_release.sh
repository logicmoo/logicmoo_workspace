#!/bin/bash -x

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"


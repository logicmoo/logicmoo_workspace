#!/bin/bash -x

#source `which junit_prolog.sh`
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*.*"
lmoo-junit "$GLOB"



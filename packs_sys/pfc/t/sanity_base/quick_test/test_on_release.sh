#!/bin/bash -x

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*.*"
lmoo-junit "$GLOB"



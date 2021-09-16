#!/bin/bash -x

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*_01.*"
lmoo-junit "$GLOB"


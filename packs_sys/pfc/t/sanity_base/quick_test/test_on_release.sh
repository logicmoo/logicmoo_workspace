#!/bin/bash -x

#source `which junit_prolog.sh`
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*.*"
source $LOGICMOO_WS/bin/junit_prolog.sh "$GLOB"



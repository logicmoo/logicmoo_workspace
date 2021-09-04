#!/bin/bash -x

#source `which junit_prolog.sh`
export GLOB="$*"
#[ -z "$GLOB" ] && GLOB="*_01.*"
source /opt/logicmoo_workspace/bin/junit_prolog.sh $GLOB



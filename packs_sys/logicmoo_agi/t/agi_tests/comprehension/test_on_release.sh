#!/bin/bash -x

export CMD_TIMEOUT=5m 
export GLOB="$*"
(
lmoo-junit "*_01*.pl"
)
stty echo


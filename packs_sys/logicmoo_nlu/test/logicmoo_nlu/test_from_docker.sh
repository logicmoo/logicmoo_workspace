#!/bin/bash -x

#source `which junit_prolog.sh`

export CMD_TIMEOUT=3m 
lmoo-junit "$*"


#!/bin/bash

RDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )
(
cd $RDIR
source ./logicmoo_env.sh
rm -f bin/pfc bin/clif bin/swipl-lm
swipl --pce=false -g "use_module(library(logicmoo_clif))" -t halt  2>&1 | grep -3 -i 'WARN\|ERROR'
chmod 777 bin/pfc bin/clif
)

#!/bin/bash

SCRIPT=$(readlink -f $0)


export LM_ARC_BASE=$(dirname $SCRIPT)
export PIDFILE=$LM_ARC_BASE/arc_web.pid
cd $LM_ARC_BASE

fuser -n tcp -k 7771
fuser -n tcp -k 7771
fuser -n tcp -k 1766
while [ true ]
do
 stty sane
 fuser -n tcp -k 7771
 echo LM_ARC_BASE=$LM_ARC_BASE
 cd $LM_ARC_BASE/butterfly
 pip install --force-reinstall .
 cd $LM_ARC_BASE
 export BFLYCMD="$LM_ARC_BASE/prolog/kaggle_arc/Kaggle_arc.sh -g user:bfly_startup ${@} -- LM_ARC_BASE=$LM_ARC_BASE"
 butterfly.server.py --port=7771 --unsecure --i-hereby-declare-i-dont-want-any-security-whatsoever --cmd="${BFLYCMD}" --force-unicode-width || stty sane
done
stty sane



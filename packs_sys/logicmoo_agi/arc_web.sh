#!/bin/bash

SCRIPT=$(readlink -f $0)


export LM_ARC_BASE=$(dirname $SCRIPT)
export PIDFILE=$LM_ARC_BASE/arc_web.pid
cd $LM_ARC_BASE

swipl -g "pack_install(predicate_streams)" -t halt
swipl -g "pack_upgrade(predicate_streams)" -t halt
swipl -g "pack_install(dictoo)" -t halt
swipl -g "pack_upgrade(dictoo)" -t halt
swipl -g "pack_install(logicmoo_utils)" -t halt
swipl -g "pack_upgrade(logicmoo_utils)" -t halt
swipl -g "pack_install(logicmoo_webui)" -t halt
swipl -g "pack_upgrade(logicmoo_webui)" -t halt
ls ~/.local/share/swi-prolog/pack -l

fuser -n tcp -k 7771
fuser -n tcp -k 7771
fuser -n tcp -k 1766
while [ true ]
do
 stty sane
 fuser -n tcp -k 7771
 echo LM_ARC_BASE=$LM_ARC_BASE
 cd $LM_ARC_BASE/butterfly ; pip install --force-reinstall . 
 #pip install butterfly --force-reinstall 

 cd $LM_ARC_BASE
 export BFLYCMD="$LM_ARC_BASE/prolog/kaggle_arc/Kaggle_arc.sh ${@} -- --bfly --no-xpce --www --no-swish LM_ARC_BASE=$LM_ARC_BASE"
 butterfly.server.py --port=7771 --host='0.0.0.0' --unsecure --i-hereby-declare-i-dont-want-any-security-whatsoever --cmd="${BFLYCMD}" --force-unicode-width || stty sane
done
stty sane



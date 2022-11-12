#!/bin/bash
[ -z "$TYPESCRIPT" ] && TYPESCRIPT=1 exec /usr/bin/script -f -e -a tee.ansi -c "TYPESCRIPT=1 $0 $@"

chmod 777 tee.ansi

SCRIPT=$(readlink -f $0)
export ARC_DIR=$(dirname $SCRIPT)
echo ARC_DIR=$ARC_DIR

if [[ $# -gt 2 ]] ; then
   fuser -n tcp -k 1766
fi



cd $ARC_DIR
mkdir -p out
chmod -R 777 out
#rm -rf out/?*.ansi.pl
# git checkout out

mkdir -p data
chmod -R 777 data


export BCMD="cd ${ARC_DIR} ; pwd ;  swipl -l kaggle_arc.pl ${@}"

echo BCMD=$BCMD
sleep 2

if id -u "norights" >/dev/null 2>&1; then
 sudo -u norights bash -l -c "git config --global --add safe.directory /opt/logicmoo_workspace/packs_sys/logicmoo_utils" || stty sane
 sudo -u norights bash -l -c "${BCMD}" || stty sane
else
 bash -l -c "${BCMD}" || stty sane
fi

if [[ $# -gt 2 ]] ; then
   fuser -n tcp -k 7771
   fuser -n tcp -k 1766
fi

killall -9 xterm

stty sane



#!/bin/bash

SCRIPT=$(readlink -f $0)
export ARC_DIR=$(dirname $SCRIPT)
echo ARC_DIR=$ARC_DIR

if [[ $# -gt 2 ]] ; then
  echo fuser -n tcp -k 17666
fi


cd $ARC_DIR
rm -rf out
git checkout out

export BCMD="cd ${ARC_DIR} ; pwd ;  swipl -l kaggle_arc.pl ${@}"

echo BCMD=$BCMD
sleep 2

if id -u "norights" >/dev/null 2>&1; then
 sudo -u norights bash -l -c "${BCMD}" || stty sane
else
 bash -l -c "${BCMD}" || stty sane
fi

if [[ $# -gt 2 ]] ; then
   fuser -n tcp -k 7771
   fuser -n tcp -k 17666
fi

killall -9 xterm

stty sane



#!/bin/bash

SCRIPT=$(readlink -f $0)
export ARC_DIR=$(dirname $SCRIPT)
cd $ARC_DIR
mkdir -p data ; chmod -R 777 data
mkdir -p out ; chmod -R 777 out
mkdir -p muarc_tmp ; chmod -R 777 muarc_tmp
mkdir -p muarc_cache ; chmod -R 777 muarc_cache
mkdir -p muarc_output ; chmod -R 777 muarc_output



if [ "${1}" == "--tee" ] ; then
shift 1
[ -z "$TEE_FILE" ] && export TEE_FILE="muarc_tmp/tee.ansi"
[ -z "$TYPESCRIPT" ] && TYPESCRIPT=1 exec /usr/bin/script -f -e -a ${TEE_FILE} -c "TYPESCRIPT=1 $0 --tee $@"
echo start $0 $@
fi

chmod 777 tee.ansi


echo ARC_DIR=$ARC_DIR

if [[ $# -gt 2 ]] ; then
   fuser -n tcp -k 1766
fi



cd $ARC_DIR
rm -f out/?*ansi.pl
#rm -rf out/?*.ansi.pl
# git checkout out


mkdir -p muarc_output
chmod -R 777 muarc_output/

mkdir -p muarc_cache
chmod -R 777 muarc_cache/
rm -f muarc_cache/?*ansi.pl

mkdir -p data
chmod -R 555 data/

chmod -R 777 muarc_tmp/

export SWIPL_OPTIONS="${@}"
if [[ $SWIPL_OPTIONS == *" -- "* ]];
then
    echo "$SWIPL_OPTIONS"
else
    SWIPL_OPTIONS="-- ${SWIPL_OPTIONS}"
fi

killall -9 xterm

export BCMD="cd '${ARC_DIR}' ; pwd ; export TEE_FILE='${TEE_FILE}' ; swipl -l kaggle_arc.pl ${SWIPL_OPTIONS}"

echo BCMD=$BCMD
sleep 2

# % 642,202,528 inferences, 104.676 CPU in 104.682 seconds (100% CPU, 6135124 Lips)
swipl -g "time(load_files([muarc_cache/arc_db_temp_cache],[qcompile(auto)])),halt."

if id -u "norights" >/dev/null 2>&1; then
 sudo -u norights bash -l -c "git config --global --add safe.directory /opt/logicmoo_workspace/packs_sys/logicmoo_utils" || stty sane
 sudo -u norights bash -l -c "${BCMD}" || stty sane
else
 bash -l -c "${BCMD}" || stty sane
fi

sleep 5

if [[ $# -gt 2 ]] ; then
   fuser -n tcp -k 7771
   fuser -n tcp -k 1766
fi

killall -9 xterm

stty sane



#!/bin/bash

#apt-get install eggdrop 
#apt-get install npm

SOURCED=0
if [ -n "$ZSH_EVAL_CONTEXT" ]; then 
    [[ $ZSH_EVAL_CONTEXT =~ :file$ ]] && SOURCED=1
elif [ -n "$KSH_VERSION" ]; then
    [[ "$(cd $(dirname -- $0) && pwd -P)/$(basename -- $0)" != "$(cd $(dirname -- ${.sh.file}) && pwd -P)/$(basename -- ${.sh.file})" ]] && SOURCED=1
elif [ -n "$BASH_VERSION" ]; then
    [[ $0 != "$BASH_SOURCE" ]] && SOURCED=1
elif grep -q dash /proc/$$/cmdline; then
    case $0 in *dash*) SOURCED=1 ;; esac
fi

fg > /dev/null 2>&1
fg > /dev/null 2>&1
fg > /dev/null 2>&1

if [[ "$SOURCED"=="1" ]] ; then
    echo "The script was sourced."
else
    echo "The script WAS NOT sourced."
fi

# #( mkdir -p /tmp/tempDir/ ; cp -a tempDir/?* /tmp/tempDir/?* ;  cd  /tmp/tempDir/ ; ln  -s * -r /home/prologmud_server/lib/swipl/pack/prologmud_samples/prolog/prologmud_sample_games/ )

#cls ; killall -9 swipl perl ; killall -9 swipl perl ;  swipl --irc --world --repl -g "[run_mud_server]" -s run_clio.pl
#cls ; killall -9 swipl-prologmud perl ; killall -9 swipl perl ;  swipl -l run.pl -l run_mud_server.pl --irc --world --clio

export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl

# export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server/
export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server

pathmunge () {
        if ! echo "$PATH" | grep -Eq "(^|:)$1($|:)" ; then
           if [ "$2" = "after" ] ; then
              PATH="$PATH:$1"
           else
              PATH="$1:$PATH"
           fi
        fi
}

# export LOGICMOO_WS=/opt/logicmoo_workspace
pathmunge $LOGICMOO_WS/bin:$PATH


mkdir -p /tmp/tempDir/
cp $LOGICMOO_WS/packs_xtra/golorp/*.txt $NEWPWD/ > /dev/null 2>&1

cp -a $NEWPWD/tempDir/* /tmp/tempDir/
rsync -avh $NEWPWD/tempDir /tmp/
ln -s /tmp/tempDir/* .
chmod a+w -R /tmp/tempDir
chmod a+w -R /tmp/tempDir/?*
chmod a+w -R /tmp/tempDir/?*/
#touch passwd
chmod 777 passwd


echo LOGICMOO_WS=$LOGICMOO_WS
echo LOGICMOO_BASE_PORT=$LOGICMOO_BASE_PORT


echo chroot /mnt/chroot/
echo su - prologmud_server



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
    echo "The script $0 WAS sourced."
else
    echo "The script $0 WAS NOT sourced."
fi

export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

pathmunge () {
        if ! echo "$PATH" | grep -Eq "(^|:)$1($|:)" ; then
           if [ "$2" = "after" ] ; then
              PATH="$PATH:$1"
           else
              PATH="$1:$PATH"
           fi
        fi
}

pathmunge $LOGICMOO_WS/bin:$PATH

mkdir -p /tmp/tempDir/

cp -a $NEWPWD/tempDir/* /tmp/tempDir/
#*/
rsync -avh $NEWPWD/tempDir /tmp/
ln -s /tmp/tempDir/* .
#*/
chmod a+w -R /tmp/tempDir
chmod a+w -R /tmp/tempDir/?*
#*/
chmod a+w -R /tmp/tempDir/?*/
#touch passwd
chmod 777 passwd

echo LOGICMOO_WS=$LOGICMOO_WS

#echo chroot /mnt/chroot/
#echo su - prologmud_server


#!/bin/bash

if [ -z ${STANFORD_JAR+x} ]; then export STANFORD_JAR="pack/logicmoo_nlu/prolog/stanford-corenlp3.5.2-ALL.jar"; fi
if [ -z ${JAVA_HOME+x} ]; then export JAVA_HOME=`find /usr -name java-8-oracle -printf "%p/jre"`; fi

if [ `echo $LD_LIBRARY_PATH || grep -v 'java' ` ]; then
  export LD_LIBRARY_PATH="${JAVA_HOME}/lib/amd64/server:${JAVA_HOME}/lib/amd64:${JAVA_HOME}/bin:${PATH}:${LD_LIBRARY_PATH}"
fi

export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/ && pwd )"
export RUNFILE=$NEWPWD/run_mud_server.pl --debug=debug_once
if [ $# -eq 0 ] 
 then
    echo "No arguments supplied"
 else
    export RUNFILE="$1"
fi


echo "You should not see this ever";
reset -w
echo -ne '\033]50;ClearScrollback\a'
echo -en "\ec\e[3J"
echo `pwd`
if [[ $EUID -eq 0 ]];
 then
    fg
 else
    killall -9 xterm swipl ; killall -9 xterm swipl ; fg
fi
             
echo "Hit CTRL+C ${BASH_SOURCE[0]} $RUNFILE ";
sleep 1;
cd $NEWPWD
swipl -L32G -G32G -T32G -f ${RUNFILE}
if [[ $EUID -eq 0 ]];
 then
    fg
 else
    killall -9 xterm swipl ; killall -9 xterm swipl ; fg
    killall -9 xterm swipl ; killall -9 xterm swipl ; fg
fi
cd $OLDPWD
echo exit $?

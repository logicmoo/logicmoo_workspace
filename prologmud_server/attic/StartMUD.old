#!/bin/bash

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

echo PATH=$PATH

./PreStartMUD.sh


export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"



. $LOGICMOO_WS/logicmoo_env.sh

pathmunge $LOGICMOO_WS/bin
pathmunge /opt/logicmoo_workspace/packs_web/butterfly
#pathmunge /opt/anaconda3/bin


if [[ -z "${LOGICMOO_BASE_PORT}" ]]; then
  LOGICMOO_BASE_PORT=4000
fi

touch /opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games/.swipl_history
chmod 777 /opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games/.swipl_history


export RL_PREFIX=''
# export RL_PREFIX='rlwrap -a -A -r -c -N -r'
export USE_NET=1
# export USE_KB=1
export KBFILE=""


for i in "$@" ; do
   if [[ $i == "--nonet" ]] ; then export  USE_NET=0; fi
   if [[ $i == "-x" ]] ; then export USE_KB=0; fi     
   if [[ $i == "--prc" ]] ; then export USE_KB=1; fi
   if [[ $i == "--nocyc" ]] ; then export USE_KB=0; fi
done

if [ $USE_KB == 1 ]; then

   export KBFILE='-x kb7166.prc'

   if [ ! -f ./kb7166.prc ]; then
      if [ ! -f ./kb7166.zip ]; then
      echo download https://www.dropbox.com/s/0dc1ec7ehse8vve/kb7166.zip?dl=1
         wget -O ./kb7166.zip -N https://www.dropbox.com/s/0dc1ec7ehse8vve/kb7166.zip?dl=1
      fi
      unzip -o ./kb7166.zip
   fi
   if [ ! -f ./kb7166.prc ]; then
      echo cant find $(pwd)/kb7166.prc
      export KBFILE=""
   fi
else
   export KBFILE=""
fi


. setup_env.sh

echo LOGICMOO_WS=$LOGICMOO_WS
echo LOGICMOO_BASE_PORT=$LOGICMOO_BASE_PORT

if [[ "$(pidof eggdrop)"=="" ]] ; then
 echo "Starting eggdrop:     $(date)" 
 # ( cd $LOGICMOO_WS/packs_sys/eggdrop/conf/ ; sudo -u prologmud_server -- eggdrop -m )
fi     

export SWIPL="$LOGICMOO_WS/bin/swipl"
SWIPL=/opt/logicmoo_workspace/bin/swipl
# SWIPL=+" -G18G -L18G -T18G"
# SWIPL=+" --signals=true --stack_limit=32g"
# SWIPL=+" --pce=false"

# export CMDARGS="-l run_mud_server.pl $* --all --world --lispsock --sumo --planner"
export CMDARGS="run_mud_server.pl -g prolog $*"
export CMDARGS="-l run_mud_server.pl $*"
# CMDARGS=+" --sigma --www --docs --cliop --swish --plweb --elfinder"
# CMDARGS=+" --tinykb --fullkb --rcyc --logtalk --nlu --pdt --irc"



. $LOGICMOO_WS/packs_web/butterfly/bin/activate

pip3 freeze > /tmp/requirements3a.txt 2>&1
pip freeze > /tmp/requirements2a.txt 2>&1

pip install tornado asyncio

function start_redirect {
   local PORT=$((00+$1))
   local PORT100=$((100 + $1))
   local HIST=$PWD/history_$1
   local COMP=$PWD/completion_$1
   touch $HIST

   touch $COMP
   echo "lsof -t -i:$PORT100 | xargs --no-run-if-empty kill -9"

    local BUTTERFLY="$LOGICMOO_WS/packs_web/butterfly/butterfly.server.py"
      BUTTERFLY="/usr/local/bin/butterfly.server.py"

#   local START_REDIR="nohup node app.js -p ${PORT100} -c rlwrap -a -A -r -c -N -r --file=${COMP} --history-filename=${HIST} -s 1000 telnet localhost ${PORT}"
#   local START_REDIR="nohup ttyd -r 100 -p ${PORT100}    rlwrap -a -A -r -c -N -r --file=${COMP} --history-filename=${HIST} -s 1000 telnet localhost ${PORT}"
      local START_REDIR=""
      START_REDIR="nohup $BUTTERFLY --i-hereby-declare-i-dont-want-any-security-whatsoever --unsecure --host=0.0.0.0 --port=${PORT100} --cmd=\"/usr/bin/rlwrap -a -A -r -c -N -r --file=${COMP} --history-filename=${HIST} -s 1000 /usr/bin/telnet localhost ${PORT}\" "
      START_REDIR="nohup $BUTTERFLY --i-hereby-declare-i-dont-want-any-security-whatsoever --unsecure --host=0.0.0.0 --port=${PORT100} --cmd=\"/usr/bin/telnet localhost ${PORT}\" "
   echo $START_REDIR   
  # eval $START_REDIR & 
}

function kill_redirect {
   echo "lsof -t -i:$((100+$1)) | xargs --no-run-if-empty $((100+$1)) kill -9"
}




if [ $# -eq 0 ] 
 then
    # //root
     if [[ $(id -u) == 0 ]]; then
        export RUNFILE="${RL_PREFIX} ${SWIPL} ${KBFILE} ${CMDARGS}"
        export USE_NET=1
     else
        export RUNFILE="${RL_PREFIX} ${SWIPL} ${KBFILE} ${CMDARGS}" 
     fi
 else
   # //other
    export RUNFILE="${RL_PREFIX} ${SWIPL} ${CMDARGS}"
fi

cls_putty() {
 	clear
   reset
   echo -en '\033]50;ClearScrollback\a'
   echo -en "\ec\e[3J"
   printf "\ec" #. This can also be 
   printf "\033c" #or 
   printf "\x1bc"  
   # putting everything together
   echo -en "\e[H\e[J\e[3J"
   printf '\033[3J'
   reset -w
}

[[ -z "$LOGTALKHOME" ]] && export LOGTALKHOME=/usr/share/logtalk
[[ -z "$LOGTALKUSER" ]] && export LOGTALKUSER=$LOGTALKHOME

export COMMAND_LAST=8
export RAN_ALREADY=0
export MY_PID=$$
export MY_PTTY=$(tty)



list_descendants ()
{

  local children=$(ps -o pid= --ppid $1)
  for pid in $children
  do
    list_descendants "$pid"
  done

  echo "$children"
} 

export WHOLE="gdb -x gdbinit -return-child-result --args ${RUNFILE}"
#export WHOLE="gdb -x gdbinit -return-child-result -ex \"set pagination off\" -ex run -ex quit --args ${RUNFILE}"
#export WHOLE="gdb -x gdbinit -return-child-result -ex \"set pagination off\" --args ${RUNFILE}"
export WHOLE="${RUNFILE}"

if [[ $UID == 0 ]]; then
  # export WHOLE="sudo -u prologmud_server ${WHOLE}"
  echo WHOLE=$WHOLE
fi

echo "LOGICMOO_BASE_PORT=${LOGICMOO_BASE_PORT}"

while [[ RAN_ALREADY -ne 1 ]] && [[ $COMMAND_LAST -ne 666 ]] && [[ $COMMAND_LAST -ne 9 ]] && [[ $COMMAND_LAST -ne 4 ]] && [[ $COMMAND_LAST -ne 137 ]];
do
         
   if [[ $COMMAND_LAST -ne 4 ]]; then
      echo "You should rarely see this";
      echo cls_putty
   fi
    echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    echo "~~~~~~~~~~~~~KILL PREV~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

    if [ `whoami` != 'root' ]
      then
        echo killall -9  swipl
        echo killall -9  /usr/bin/swipl
    fi
    echo kill -9 $(list_descendants $MY_PID)
    # kill -9 $(list_descendants $MY_PID)
    #swipl forks xterm making it not die until the xterm it launched is dead
    # killall --user $USER -9 xterm #perl
     if [[ "$USE_NET" == "1" ]]; then
       lsof -t -i:$LOGICMOO_BASE_PORT | xargs --no-run-if-empty kill -9
       kill_redirect $(($LOGICMOO_BASE_PORT+0))
       kill_redirect $(($LOGICMOO_BASE_PORT+1))
       kill_redirect $(($LOGICMOO_BASE_PORT+2))
       kill_redirect $(($LOGICMOO_BASE_PORT+3))
       kill_redirect $(($LOGICMOO_BASE_PORT+23))
       kill_redirect $(($LOGICMOO_BASE_PORT+25))
       kill_redirect $(($LOGICMOO_BASE_PORT+301))
       kill_redirect $(($LOGICMOO_BASE_PORT+804))
       
     fi

     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~LEFT PREV~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
         for i in $(fuser $MY_PTTY); do     ps -o pid= -o command= -p $i; done
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

     if [[ "$RAN_ALREADY" == "1" ]]; then
      echo "restarting... ";
      echo "Hit CTRL+C ${BASH_SOURCE[0]} 2 secs ";
      sleep 1
      echo "Hit CTRL+C ${BASH_SOURCE[0]} 1 secs ";
      sleep 1
     fi

   # RAN_ALREADY=1


    
wasdir=""

    if [[ "$USE_NET" == "1" ]]; then

         wasdir=$(dirname -- $0)
         # cd $LOGICMOO_WS/packs_web/wetty
         rm -f nohup.out
         pip3 freeze > /tmp/requirements3w.txt
         pip freeze > /tmp/requirements2w.txt
         start_redirect $(($LOGICMOO_BASE_PORT+0))
         start_redirect $(($LOGICMOO_BASE_PORT+1))
         start_redirect $(($LOGICMOO_BASE_PORT+2))
         start_redirect $(($LOGICMOO_BASE_PORT+3))
         start_redirect $(($LOGICMOO_BASE_PORT+4))
         start_redirect $(($LOGICMOO_BASE_PORT+23))
         start_redirect $(($LOGICMOO_BASE_PORT+25))
         #start_redirect $(($LOGICMOO_BASE_PORT+301))
         #start_redirect $(($LOGICMOO_BASE_PORT+804))
         cat nohup.out
         cd $wasdir
     fi

     #export LD_LIBRARY_PATH=".:/usr/lib/jvm/java-11-openjdk-amd64/lib:/usr/local/lib:/usr/lib"
     
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~CURRENT-P~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        for i in $(fuser $MY_PTTY); do     ps -o pid= -o command= -p $i; done
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo DISPLAY=$DISPLAY
     echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH
     echo LOGTALKHOME=$LOGTALKHOME
     echo LOGTALKUSER=$LOGTALKUSER
     echo LOGICMOO_WS=$LOGICMOO_WS
     echo LOGICMOO_BASE_PORT=$LOGICMOO_BASE_PORT
     echo "~~Run~~"
      FILE=./logicmoo_server
      if test -f "$FILE"; then
          WHOLE=$FILE
      fi 
     echo $WHOLE
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

      
     ( cd $NEWPWD && $WHOLE
      COMMAND_LAST=$?
      echo ""
      echo "~~~~~~~~~~~~~~~~~~~~~~~"
      echo "~~~~ EXIT CODE ${COMMAND_LAST} ~~~~"
      echo "~~~~~~~~~~~~~~~~~~~~~~~"
       if [[ "$USE_NET" == "1" ]]; then
        # lsof -t -i:$LOGICMOO_BASE_PORT | xargs --no-run-if-empty kill -9
         kill_redirect $(($LOGICMOO_BASE_PORT+0))
         kill_redirect $(($LOGICMOO_BASE_PORT+1))
         kill_redirect $(($LOGICMOO_BASE_PORT+2))
         kill_redirect $(($LOGICMOO_BASE_PORT+3))
         kill_redirect $(($LOGICMOO_BASE_PORT+23))
         kill_redirect $(($LOGICMOO_BASE_PORT+25))
         #kill_redirect $(($LOGICMOO_BASE_PORT+301))
         #kill_redirect $(($LOGICMOO_BASE_PORT+804))
       fi

      reset -c -Q -w -I -w
      sleep 2
      if [[ "$COMMAND_LAST" == "4" ]]; then
          cls
      fi
     )
done

return $COMMAND_LAST 2> /dev/null || exit $COMMAND_LAST






#!/bin/bash

echo hope you did chroot /mnt/chroot/ bash --login
echo or chroot /mnt/chroot su - prologmud_server -c bash --login
#cd /mnt/gggg/logicmoo_workspace/pack/prologmud_samples/prolog/prologmud_sample_games
#cd /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/t/examples/fol
#export DISPLAY=:0.0


#export HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)"
#export HOME=/tmp/tempDir
#rsync -avh tempDir/ $HOME/


#export PATH=$LOGICMOO_WS/bin:$SWI_HOME_DIR/bin/x86_64-linux:$PATH

#export DISPLAY=10.0.0.122:0.0
#export LOGTALKHOME=~/lib/swipl/packs_lib/logtalk/logtalk-3.10.9

#killall -9 swipl xterm # darn xpce xterms!
#killall -9 perl # we forked the flash server and it holds SWIPL alive
#killall -9 swipl # its ok since this is the prologmud_server user and not root

#swipl --irc --world --repl 

#echo -t "user:([library(logtalk)],[library(logicmoo_swish)],[library(logicmoo_swish)],[run_mud_server],prolog)" # -f /dev/null
#cls ; killall -9 perl swipl perl xterm; 
#cls ; kill -9 %1 %2 %3 %4 ; kill -9 %1 %2 %3 %4 
#swipl  -f /dev/null --irc --world --repl --www -l run_mud_server.pl -g "set_defaultAssertMt(prologMOO)"
#swipl  --irc --world --repl --www -g "profile(user:consult(run_mud_server))."


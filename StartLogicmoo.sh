#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
else

(
stty sane
. ./logicmoo_env.sh

export LOGICMOO_GAMES=$LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games
( cd $LOGICMOO_GAMES
( ./PreStartMUD.sh > /dev/null 2>&1 )

find -name "*.qlf" -exec touch '{}' +

echo whoami=`whoami`
echo PATH=$PATH
echo LOGICMOO_GAMES=$LOGICMOO_GAMES
echo LOGICMOO_WS=$LOGICMOO_WS
ln -s $LOGICMOO_WS/etc/profile.d/logicmoo_etc_profile_d.sh /etc/profile.d/
ln -s $LOGICMOO_WS/packs_web/logicmoo_webui/etc/apache2/sites-enabled/000-logicmoo.conf /etc/apache2/sites-enabled/000-logicmoo.conf
ln -s $LOGICMOO_WS/packs_web/logicmoo_webui/etc/apache2/conf-available/cliopatria_swish.conf /etc/apache2/conf-available/cliopatria_swish.conf


adduser --disabled-password --gecos "" prologmud_server --home $LOGICMOO_GAMES
chown -R prologmud_server ~prologmud_server/.?*

touch $LOGICMOO_GAMES/history_3804
touch $LOGICMOO_GAMES/completion_3804
chown prologmud_server $LOGICMOO_GAMES/completion_*
chown prologmud_server $LOGICMOO_GAMES/history_*
touch $LOGICMOO_GAMES/nohup.out
chown prologmud_server $LOGICMOO_GAMES/nohup.out
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/
# in case of symlinking
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/src~/

#chown -R prologmud_server $LOGICMOO_WS/packs_web/butterfly

mkdir -p /tmp/tempDir/
chown -R prologmud_server /tmp/tempDir/

(
sudo -u prologmud_server -- sh -c "${LOGICMOO_WS}/logicmoo_env.sh ; . ${LOGICMOO_WS}/packs_web/butterfly/bin/activate ; export LOGICMOO_WS=$LOGICMOO_WS ; cd ${LOGICMOO_GAMES} ; ./StartMUD.sh $*"
))
stty sane


)
fi


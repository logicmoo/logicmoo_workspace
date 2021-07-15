#!/bin/bash

cd ~/platform/

echo "I am $USER, with uid $UID, with HOME=$HOME and PWD=$PWD"

if screen -list | grep -q "${USER}_irc"; then

        screen -list
    # run bash script

else
        cd ~/platform/
        screen -dmS "${USER}_irc" bash
        sleep 2

        screen -S  "${USER}_irc" -X stuff "

cd ~prologmud_server/eggdrop/conf/
eggdrop

"
fi



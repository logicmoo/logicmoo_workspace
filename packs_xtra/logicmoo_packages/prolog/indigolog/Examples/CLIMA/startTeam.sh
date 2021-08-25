#!/bin/bash

xterm -T player1 -e pl2.sh -f main_swi.pl -t main1 &
xterm -T player2 -e pl2.sh -f main_swi.pl -t main2 &
xterm -T player3 -e pl2.sh -f main_swi.pl -t main3 & 
xterm -T player4 -e pl2.sh -f main_swi.pl -t main4 & 
xterm -T player5 -e pl2.sh -f main_swi.pl -t main5 &
xterm -T player6 -e pl2.sh -f main_swi.pl -t main6 &

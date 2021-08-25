#!/bin/bash

echo player1 > player1.txt
echo player2 > player2.txt
echo player3 > player3.txt
echo player4 > player4.txt
echo player5 > player5.txt
echo player6 > player6.txt
pl2.sh -f main_swi.pl -t main1 >> player1.txt 2>>player1.txt &
pl2.sh -f main_swi.pl -t main2 >> player2.txt 2>>player2.txt &
pl2.sh -f main_swi.pl -t main3 >> player3.txt 2>>player3.txt & 
pl2.sh -f main_swi.pl -t main4 >> player4.txt 2>>player4.txt & 
pl2.sh -f main_swi.pl -t main5 >> player5.txt 2>>player5.txt &
pl2.sh -f main_swi.pl -t main6 >> player6.txt 2>>player6.txt &

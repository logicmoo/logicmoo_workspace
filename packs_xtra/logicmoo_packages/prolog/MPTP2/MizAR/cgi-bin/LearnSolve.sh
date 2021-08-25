i=$1
cd ~/rsrch/leancop/nonconsistent/ChainyProblems
aport=`./InitAdv.pl $i 64| grep '\,'|sed -e 's/,##//'` 
sleep 8
echo "$i:$aport"
cd ~/rsrch/leancop/nonconsistent
time ./leancoporig60.sh ChainyProblems/$i.leancop localhost:$aport > ChainyProblems/$i.leancop.out

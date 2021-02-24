SWIPL=${SWIPL-swipl}
BASE=${BASE-$(hostname)/stable.csv}
GITVERSION="$($SWIPL -g print_version tabling.pl)"
CSV=${CSV-times-$GITVERSION.csv}

if [ -z "$1" ]; then
  tests="500fib
	 750fib
	 1000fib
	 10kfib
	 20krecognize
	 50krecognize
	 500naiveReverse
	 1000naiveReverse
	 2000shuttle
	 5000shuttle
	 20000shuttle
	 50000shuttle
	 10kpingpong
	 20kpingpong
	 50pDblFstLoop
	 100pDblFstLoop
	 50pDoubleFirst
	 100pDoubleFirst
	 200pDoubleFirst
	 500pDoubleFirst
	 pyramid500
	 bintree18
	 testJoins
	 mondial"
else
  tests="$(echo "$*" | sed 's/-swi.pl//g')"
fi

export CSV BASE
echo "# Reporting relative to $BASE; storing in $CSV"

rm -f $CSV
for f in $tests; do
# echo $SWIPL running test $f
  $SWIPL -O -g go,halt --stack_limit=8g $f-swi.pl tabling.pl || exit 1
done

$SWIPL -g total tabling.pl $CSV $BASE

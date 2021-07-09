#! /bin/sh

# $1 is expected to have xsb ececutable + command line options
EMU=$1
FILE=$2
CMD=$3
THREADS=$4

DIR=`pwd`
BASEDIR=`basename $DIR`

echo "--------------------------------------------------------------------"
echo "Testing $BASEDIR/$FILE w. $THREADS threads"
echo "$EMU"     # debug check: verify that options have been passed to xsb
# Note that testing predicates must do their own I/O.

for elt in ${FILE}_temp.*; do
    rm -f $elt
done

if test -f temp_old; then
    rm -f temp_old
fi

if [ $THREADS -eq 0 ]; then

$EMU << EOF

['../test_concurrent'].

[$FILE].

gen_result(${FILE}_old,${CMD}).

EOF

exit 0

else

ulimit -c unlimited

#$EMU -r -gc none -m 8192 << EOF
#$EMU << EOF
$EMU  -g none  << EOF

['../test_concurrent'].

[$FILE].

test_concurrent_goal(${THREADS},${FILE}_temp,${CMD}).

EOF

fi


status=0
nfiles=0

if test ! -f ${FILE}_old; then
    echo "error! No comparison file: ${FILE}_old"
    status=1
else
    sort ${FILE}_old > temp_old
fi
    
for elt in ${FILE}_temp.*; do
    nfiles=$[nfiles+1]
    sort $elt > temp_new
    diff -w temp_new temp_old
    result=$?
    if test "$result" = 0 ; then 
	echo "$BASEDIR/$elt tested"
	rm -f $elt
    else
	echo "$BASEDIR/$elt differs!!!"
	status=1
    fi
done

rm -f temp_new

rm -f temp_old

echo $nfiles output files tested for $THREADS threads

if test "$status" = 0 ; then 
   echo "$BASEDIR/$FILE fully tested"
else
   echo "Problem with $BASEDIR/$FILE"
fi





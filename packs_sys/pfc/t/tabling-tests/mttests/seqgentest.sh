#! /bin/sh

# $1 is expected to have xsb ececutable + command line options
EMU=$1
FILE=$2
CMD=$3

DIR=`pwd`
BASEDIR=`basename $DIR`

echo "--------------------------------------------------------------------"
echo "Testing $BASEDIR/$FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

$EMU -g none << EOF
[$FILE].
tell(temp).
$CMD
told.
EOF

# print out differences.
if test -f ${FILE}_new; then
	rm -f ${FILE}_new
fi
    
sort temp > ${FILE}_new
sort ${FILE}_old > temp

#-----------------------
# print out differences.
#-----------------------
status=0
diff -w ${FILE}_new temp || status=1
if test "$status" = 0 ; then 
	echo "$BASEDIR/$FILE tested"
	rm -f ${FILE}_new
else
	echo "$BASEDIR/$FILE differs!!!"
	diff -w ${FILE}_new ${FILE}_old
fi

rm -f temp

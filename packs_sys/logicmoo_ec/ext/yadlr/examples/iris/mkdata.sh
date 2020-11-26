#!/bin/bash

# This script transforms the Iris UCI dataset into suitable training example
# format for using Aleph with prodlr.
# Please download http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
# and then pipe it into this script.
#
# Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
# Created: 28-2-2008
# This file is in the public domain.


MIN_SEP_LEN=4.3
MAX_SEP_LEN=7.9
MIN_SEP_WID=2.0
MAX_SEP_WID=4.4
MIN_PET_LEN=1.0
MAX_PET_LEN=6.9
MIN_PET_WID=0.1
MAX_PET_WID=2.5

IFS=','

NUM=0;

CATEGORIES="setosa,versicolor,virginica"
for f in $CATEGORIES
do
    rm -f ${f}.f ${f}.n
done

rm -f ls.pl ws.pl lp.pl wp.pl

while read SEP_LEN SEP_WID PET_LEN PET_WID CAT
do
    NUM=$(( $NUM + 1 ))
    INSTANCE="iris$NUM"

    SEP_LEN_BC="scale=2; ($SEP_LEN - $MIN_SEP_LEN)/($MAX_SEP_LEN - $MIN_SEP_LEN)"
    SEP_LEN_NORM=$(echo "$SEP_LEN_BC" | bc -l | sed 's#^\.\(..\)$#0.\1#' )
    SEP_WID_BC="scale=2; ($SEP_WID - $MIN_SEP_WID)/($MAX_SEP_WID - $MIN_SEP_WID)"
    SEP_WID_NORM=$(echo "$SEP_WID_BC" | bc -l | sed 's#^\.\(..\)$#0.\1#' )
    PET_LEN_BC="scale=2; ($PET_LEN - $MIN_PET_LEN)/($MAX_PET_LEN - $MIN_PET_LEN)"
    PET_LEN_NORM=$(echo "$PET_LEN_BC" | bc -l | sed 's#^\.\(..\)$#0.\1#' )
    PET_WID_BC="scale=2; ($PET_WID - $MIN_PET_WID)/($MAX_PET_WID - $MIN_PET_WID)"
    PET_WID_NORM=$(echo "$PET_WID_BC" | bc -l | sed 's#^\.\(..\)$#0.\1#' )

    CAT=${CAT#Iris-}

    for C in $CATEGORIES
    do 
	if [[ $CAT == $C ]]
	then
	    echo "target( [($INSTANCE, 1.0)], [($INSTANCE, 0.51)] )." >> ${C}.f
	else
	    echo "target( [($INSTANCE, 1.0)], [($INSTANCE, 0.49)] )." >> ${C}.n
	fi
    done

    echo ":- add_to_concept( 'LongSepals', [($INSTANCE, $SEP_LEN_NORM)] )." >> ls.pl
    echo ":- add_to_concept( 'WideSepals', [($INSTANCE, $SEP_WID_NORM)] )." >> ws.pl
    echo ":- add_to_concept( 'LongPetals', [($INSTANCE, $PET_LEN_NORM)] )." >> lp.pl
    echo ":- add_to_concept( 'WidePetals', [($INSTANCE, $PET_LEN_NORM)] )." >> wp.pl
done

echo ":- declare_concept( 'LongPetals' )." > iris.bg
cat lp.pl >> iris.bg
echo "" >> iris.bg

echo ":- declare_concept( 'WidePetals' )." >> iris.bg
cat wp.pl >> iris.bg
echo "" >> iris.bg

echo ":- declare_concept( 'LongSepals' )." >> iris.bg
cat ls.pl >> iris.bg
echo "" >> iris.bg

echo ":- declare_concept( 'WideSepals' )." >> iris.bg
cat ws.pl >> iris.bg


rm ls.pl ws.pl lp.pl wp.pl

for C in $CATEGORIES
do
  ln -s iris.bg ${C}.b
done

exit 0


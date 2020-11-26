#!/bin/sh
#
# Test script for Prolog+CG
#
# Must have three arguments:
#
# prolog-file "goal" expected-output
#
JAVA=/usr/bin/java
PLGCGFILE=$1
#OUTFILE=`echo $PLGCGFILE | sed -e 's_.plgCG_.out_gi ; s_.prlg_.out_gi'`
OUTFILE=$3
GOAL="$2"
TMPFILE=/tmp/testPPCG.txt

$JAVA -cp classes PrologPlusCG/PrologPlusCGCLI $PLGCGFILE "$GOAL" >$TMPFILE 2>&1
mac2unix  $TMPFILE 2>/dev/null
DIFF=`diff $TMPFILE $OUTFILE`
if test "x$DIFF" = "x"; then
   echo "SUCCESS: $PLGCGFILE \"$GOAL\"";
else
   echo "FAILURE: $PLGCGFILE \"$GOAL\"";
   echo "------------------  output was  -------------------";
   cat $TMPFILE;
   echo "------------------   diff was  -------------------";
   diff $OUTFILE $TMPFILE 
   echo "------------------ end of diff -------------------";
fi


#! /bin/sh

echo "`date`"

#-----------------------
while test 1=1
do
    case "$1" in
     *-opt*)
	    shift
	    options=$1
	    shift
	    ;;
      *)
	    break
	    ;;
    esac
done

echo "Options are $options"

XEMU=$1
NUM=$2

if [ -z "$NUM" ]
then
	NUM=1
fi

#-----------------------

# get the xsb command name (without the options)

XEMUCOMMAND=`echo $XEMU | awk '{ print $1 }'`

if  test ! -f "$XEMUCOMMAND"; then
    echo "No emulator file: $XEMUCOMMAND"
    exit 1
fi

if  test ! -x "$XEMUCOMMAND"; then
    echo "Cant execute emulator: $XEMUCOMMAND"
    exit 1
fi

if [ -z "$XEMU" ]
then
	echo "Usage: $0 EMULATOR [NUM_THREADS]"
	echo "or $0 "EMULATOR [FLAGS]" [NUM_THREADS]"
	echo
	echo NUM_THREADS=0 generates test results
	exit 1
fi


if test -n "$USER"; then
   USER=`whoami`
   export USER
fi

#-----------------------

#-----------------------

GREP="grep -i"
MSG_FILE=/tmp/xsbmt_test_msg.$USER
LOG_FILE=/tmp/xsbmt_test_log.$USER
RES_FILE=/tmp/xsbmt_test_res.$USER

if test -f "$LOG_FILE"; then
  echo "There was an old $LOG_FILE"
  echo "removing..."
  rm -f $LOG_FILE
fi

#-----------------------

echo "Testing $XEMU"
echo "The log will be left in  $LOG_FILE"

echo "Log for  $XEMU $options" > $LOG_FILE
(echo "Date-Time: `date +"%y%m%d-%H%M"`" >> $LOG_FILE) || status=failed
if test -n "$status"; then
	echo "Date-Time: no date for NeXT..." >> $LOG_FILE
	NeXT_DATE=1
else
	NeXT_DATE=0
fi

# TLS: this says to send output to $LOG_FILE, and to send stderr to 
# stdout -- i.e. to $LOG_FILE
sh ./test_concurr_1.sh -opts "$options"  "$XEMU" $NUM >> $LOG_FILE 2>&1

#-----------------------------

touch $RES_FILE
coredumps=`find . -name core* -print`

if test -n "$coredumps" ; then
  echo "The following coredumps occurred during this test run:" >> $RES_FILE
  ls -1 $coredumps >> $RES_FILE
  echo "End of the core dumps list" >> $RES_FILE
fi
# check for seg fault, but not for segfault_handler
$GREP "fault" $LOG_FILE | grep -v "segfault_handler" >> $RES_FILE
# core dumped
$GREP "dumped" $LOG_FILE >> $RES_FILE
# when no output file is generated
$GREP "no match" $LOG_FILE >> $RES_FILE
# for bus error
$GREP "bus" $LOG_FILE >> $RES_FILE
# for really bad outcomes
$GREP "missing" $LOG_FILE >> $RES_FILE
# for wrong results
$GREP "differs!" $LOG_FILE >> $RES_FILE
$GREP "different!" $LOG_FILE >> $RES_FILE
# when xsb aborts... it writes something like ! Aborting...
$GREP "abort" $LOG_FILE >> $RES_FILE
# for overflows (check for Overflow & overflow)
#$GREP "overflow" $LOG_FILE >> $RES_FILE
# for ... missing command...
$GREP "not found" $LOG_FILE >> $RES_FILE
$GREP "abnorm" $LOG_FILE >> $RES_FILE
$GREP "denied" $LOG_FILE >> $RES_FILE
$GREP "no such file" $LOG_FILE >> $RES_FILE
$GREP "illegal" $LOG_FILE >> $RES_FILE
# sometimes after overflow the diff fails and a message with Missing
# is displayed
$GREP "missing" $LOG_FILE >> $RES_FILE
# 
$GREP "fatal" $LOG_FILE >> $RES_FILE
# some other problems that should highlight bugs in the test suite
$GREP "syntax error" $LOG_FILE >> $RES_FILE
$GREP "cannot find" $LOG_FILE >> $RES_FILE
echo "-----------------------------------------"

if test "$NeXT_DATE" = 1; then
	NEW_LOG=$LOG_FILE.$NeXT_DATE
	cp $LOG_FILE $NEW_LOG
else
	NEW_LOG=$LOG_FILE-`date +"%y.%m.%d-%H:%M:%S"`
	cp $LOG_FILE $NEW_LOG
fi

HOSTNAME=`hostname`

# -s tests if size > 0
if test -s $RES_FILE; then
	cat $RES_FILE
	echo "-----------------------------------------"
	echo "***FAILED testsuite for $XEMU on $HOSTNAME"
        echo "***FAILED testsuite for $XEMU on $HOSTNAME" > $MSG_FILE
	echo "Check the log file $NEW_LOG" >> $MSG_FILE
	echo "" >> $MSG_FILE
	echo "    Summary of the problems:" >> $MSG_FILE
	echo "" >> $MSG_FILE
	cat $RES_FILE >> $MSG_FILE
	mail $USER < $MSG_FILE
	rm -f $MSG_FILE
else
	echo "PASSED testsuite for $XEMU on $HOSTNAME"
	rm -f $NEW_LOG
fi

rm -f $RES_FILE

echo "Done"
echo ==============================================================

echo "`date`"

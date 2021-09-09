#!/bin/bash


[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whomai)
[ -z "$TESTING_TEMP" ] && [ -d "${LOGICMOO_WS}/test_results" ] && export TESTING_TEMP=${LOGICMOO_WS}/test_results/$(whomai)
[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)
export TESTING_TEMP
mkdir -p $TESTING_TEMP/


export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*_01.*"
[ -z "${TEST_STEM}" ] && export TEST_STEM=Report-$(echo "${GLOB}-Units" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -e "s/[^a-Z0-9_]/-/g" -e "s/--/-/g" -e "s/-/-/g"  -e "s/--/-/g" )
TEST_STEM=$(expr substr "${TEST_STEM}" 1 110)
echo "<!-- TEST_STEM=${TEST_STEM} -->"
[ -z "${TEST_STEM_PATH}" ] && export TEST_STEM_PATH=$TESTING_TEMP/$TEST_STEM
TEST_STEM_PATH=$(expr substr "${TEST_STEM_PATH}" 1 130)
echo "<!-- TEST_STEM_PATH=${TEST_STEM_PATH} -->"



SWIPL=swipl
if [ -z `which swipl` ]; then
    # default locations on OS X
    SWIPL=/Applications/SWI-Prolog.app/Contents/MacOS/swipl;
    if [ ! -e $SWIPL ]; then
        SWIPL=~/bin/swipl;
    fi
    if [ ! -e $SWIPL ]; then
        INFO PFC requires SWI-Prolog. Please download from http://www.swi-prolog.org/
        return 1 2>/dev/null ; exit 1
    fi
fi

OUTER_TEE=""
[ -t 1 ] && OUTER_TEE="1"

good_exit=7
exitcode=$good_exit

[ -z "${keep_going}" ] && export keep_going=""
[ "$*" == *"-k"* ] && export keep_going="-k"

runtime_testing=4
export next_cls=0
export test_completed=test_completed


if [ "$1" == "-k" ]; then
  keep_going="-k"
  runtime_testing=5
  shift
fi

#// For test_prolog  (no args)
 declare -a listOfNames=(
                        # // sanity tests
                           "*_01*.p*" "*_02*.p*"
                        # // full tests
                         "*_03*.p*" "*_04*.p*" "*_05*.p*" "*_06*.p*" "*_07*.p*"
						 "*_08*.p*" "*_09*.p*" "*_10*.p*" "*_11*.p*" "*_12*.p*"
                        # // feature tests
                        # "*_f01*.p*" "*_f02*.p*" "*_f03*.p*" "_f04*.p*" "*_f05*.p*" "*_f06*.p*" "*_f07*.p*" "*_f08*.p*" "*_f09*.p*" "*_f10*.p*" "*_f11*.p*" "*_f12*.p*"
) 

# kill old dead jobs
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null
#[ -z "${OUTER_TEE}" ] && echo "<!--" && cls && echo -e "\n-->"
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null

if [ $# -ne 0 ]
then
   listOfNames=( "$@" )
   if [ $# -eq 1 ]
   then
      [ -z "${OUTER_TEE}" ] && echo "<!-- test_completed=true -->"
      test_completed=test_completed
   else
      echo -e "" # [ -z "${OUTER_TEE}" ] && echo "<!--" && cls && echo -e "\n-->"
   fi
else
      echo -e ""
fi


parent-find() {
  local file="$1"
  local dir="$(realpath $2)"
  # echo parent-find "$file" "$(dirname "$dir")"
  test -e "$dir/$file" && echo "$dir" && return 0
  [ '/' = "$dir" ] && return 1
  parent-find "$file" "$(dirname "$dir")"
}

export PACK_DIR=$(parent-find "pack.pl" .  )
PACK_DIR=$(basename $PACK_DIR)

echo "<!-- PACK_DIR=${PACK_DIR} -->"

[ -z "${JUNIT_PACKAGE}" ] && export JUNIT_PACKAGE="$PACK_DIR.$(basename `realpath .. | sed -e 's|/[^.]/|/|g' `).$(basename `realpath .`)"

echo "<!-- JUNIT_PACKAGE=${JUNIT_PACKAGE} -->"

REPORT_STEM=$(echo "$TEST_STEM-${PWD}" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -re "s/[^_0123456789A-z]/-/g" -e "s/--/-/g" -e "s/_/-/g"  -e "s/--/-/g" )
REPORT_STEM=$(expr substr "${REPORT_STEM}" 1 120)

echo "<!-- REPORT_STEM=${REPORT_STEM} -->"

JUNIT_TESTS_GLOBBED="${TESTING_TEMP}/${REPORT_STEM}"
JUNIT_TESTS_GLOBBED=$(expr substr "${JUNIT_TESTS_GLOBBED}" 1 130)-glob
echo "<!-- JUNIT_TESTS_GLOBBED=${JUNIT_TESTS_GLOBBED} -->"
echo "" > $JUNIT_TESTS_GLOBBED

function JECHO {
 (echo -e "${*}\n") >> $JUNIT_TESTS_GLOBBED
}
function INFO {
   JECHO "<!-- ${*} -->"
 echo -e "${*}\n"
}

JECHO "<testsuite name=\"${REPORT_STEM}\">"
me="${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}"
INFO "Running Matching Tests: $me $keep_going ${listOfNames[*]}"

(
for ele2 in "${listOfNames[@]}"
  do
  	for ele in $ele2
	do
	  retry=1
	  while [ $retry == 1 ]
	   do
	    retry=0

        export FILENAME=${ele}
        export JUNIT_SHORTCLASS=`echo "${FILENAME^^}" | cut -d'.' -f1`
        export JUNIT_SUITE=$JUNIT_PACKAGE.$(echo "${JUNIT_SHORTCLASS^^}" | sed -e "s/_[0-9]//g" -e "s/[0-9]//g" )
        export JUNIT_CLASSNAME=$JUNIT_PACKAGE.$JUNIT_SHORTCLASS
        export FileTestCase="${JUNIT_SUITE} run $JUNIT_SHORTCLASS ( $FILENAME )"
        INFO "FileTestCase=$FileTestCase"
		  [[ "$ele" == *".ansi" ]] && continue
        [[ "$ele" == *".html" ]] && continue
        [[ "$ele" == *".xml" ]] && continue
        [[ "$ele" == *".sh" ]] && continue
        if [[ "$ele" == *".sh" && -x "$ele" ]]; then
         CMD="./${ele}"
        else
           if type pfc > /dev/null 2>&1; then
             SWIPL=pfc
           fi
           if type clif > /dev/null 2>&1; then
             SWIPL=clif
           fi
           if type ./swipl-junit > /dev/null 2>&1; then
             SWIPL=./swipl-junit
           fi
      		#// Runs the test -f .swiplrc
            #//CMD="swipl -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"thread_create(['${ele}'],Id),thread_join(Id),$test_completed\" "
            #//CMD="$SWIPL -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"(['${ele}'])\" -g \"$test_completed\" "
            CMD="$SWIPL ${ele}"
        fi

        [ -z "${CMD_TIMEOUT}" ] CMD_TIMEOUT="10s"
        [ -z "${CMD_WRAPPER}" ] CMD_WRAPPER="timeout --foreground --preserve-status -s SIGKILL -k ${CMD_TIMEOUT} ${CMD_TIMEOUT}"
        CMD="${CMD_WRAPPER} ${CMD}"

        INFO "CMD=$CMD"
        export TEE_FILE=$TESTING_TEMP/CMD_LAST.ansi
        export TEE_FILE2=${TEE_FILE}.too
        ####JECHO "<system-out><![CDATA["
        INFO "${date} (cd $PWD ; $CMD)" > $TEE_FILE
        INFO "${date} (cd $PWD ; $CMD)" > $TEE_FILE2
        startTime=$(date +%s);
        ( eval $CMD ) 2>&1 | sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" | tee -a $TEE_FILE | tee $TEE_FILE2
        exitcode=${PIPESTATUS[0]}
        endTime=$(date +%s);
        totalTime=$(($endTime-$startTime));        
        ####JECHO "]]></system-out>"

        if [ $exitcode -eq $good_exit ]; then
			[ "${next_cls}" == 1 ] && cls && next_cls=0			
         JECHO "<testcase name=\"$FileTestCase\" package='$JUNIT_PACKAGE' classname='$JUNIT_CLASSNAME' time='$totalTime'>"         
         JECHO "<system-out><![CDATA[$(cat $TEE_FILE2)]]></system-out>\n"
         JECHO "</testcase>"
         INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
			continue
	     fi
        JECHO "<testcase name=\"$FileTestCase\" package='$JUNIT_PACKAGE' classname='$JUNIT_CLASSNAME' time='$totalTime'>"
        JECHO " <failure message='FAILED: $0 ${keep_going} ${ele} (returned ${exitcode})'>"
           JECHO "<system-err><![CDATA[$(cat $TEE_FILE2)]]></system-err>\n"
        JECHO " </failure></testcase>"

        next_cls=0

      [ "$test_completed" == 'test_completed' ] && [ $exitcode -ne 7 ] && INFO "FAILED: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 7 ] && INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 0 ] && [ "$test_completed" == 'true' ] && INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 6 ] && retry=1 && continue


		# // 2 -> 1
		if [ $exitcode -eq 2 ]; then
         [ "$keep_going" == "-k" ] && INFO "...keep going..." && continue
         INFO "...NOT keep going..."
         exit 1
      fi
		
		# // Not Abort
		[ $exitcode -ne 1 ] && [ "$keep_going" == "-k" ] && continue

		echo "<!-- Do you wish to continue? [y]es, [a]lways [Up/r]etry or [N]o: "
		read -sN1 -r -t 0.0001 k1
		export k1
		
		while true
		do
			read -r -sn1 ans
			[ "$ans" == "" ] && break;
			case $ans in
			    A) break;;
				B) break;;
				r) break;;
            a) break;;
				y) break;;
				n) break;;
				e) break;;
				E) break;;
            D) break;;
			esac
			INFO "ans=$ans"
		done

      INFO "ans=$ans"

      [ "$ans" == '' ] && [ $exitcode -eq 0 ] && [ "$test_completed" == 'true' ]  && retry=1 && continue

      [ "$ans" == '' ] && [ $exitcode -eq 7 ] && retry=1 && cls && continue  # 7 + enter

		[ "$ans" == 'y' ] && continue
      [ "$ans" == 'a' ] && KEEP_GOING=1 && continue
		[ "$ans" == 'B' ] && continue # down arrow
		[ "$ans" == 'A' ] && retry=1 && cls && continue  # up arrow
		[ "$ans" == 'r' ] && retry=1 && continue

		INFO "Exiting the script. Have a nice day!"
		return $exitcode 2>/dev/null ; exit $exitcode
	  done
	done
  done
  return $exitcode 2>/dev/null ; exit $exitcode
) 

JECHO "</testsuite>\n\n\n\n"
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" $JUNIT_TESTS_GLOBBED > $JUNIT_TESTS_GLOBBED-junit.xml


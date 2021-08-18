#!/bin/bash


sourced=0
if [ -n "$ZSH_EVAL_CONTEXT" ]; then 
  case $ZSH_EVAL_CONTEXT in *:file) sourced=1;; esac
elif [ -n "$KSH_VERSION" ]; then
  [ "$(cd $(dirname -- $0) && pwd -P)/$(basename -- $0)" != "$(cd $(dirname -- ${.sh.file}) && pwd -P)/$(basename -- ${.sh.file})" ] && sourced=1
elif [ -n "$BASH_VERSION" ]; then
  (return 0 2>/dev/null) && sourced=1 
else # All other shells: examine $0 for known shell binary filenames
  # Detects `sh` and `dash`; add additional shell filenames as needed.
  case ${0##*/} in sh|dash) sourced=1;; esac
fi

if [[ sourced -ne 0 ]] ; then
   echo "Script is being sourced (bye)"
   sleep 2
else

SWIPL=swipl
if [ -z `which swipl` ]; then
    # default locations on OS X
    SWIPL=/Applications/SWI-Prolog.app/Contents/MacOS/swipl;
    if [ ! -e $SWIPL ]; then
        SWIPL=~/bin/swipl;
    fi
    if [ ! -e $SWIPL ]; then
        echo PFC requires SWI-Prolog. Please download from http://www.swi-prolog.org/
        exit 1
    fi
fi



# Symlink resolution: http://stackoverflow.com/a/697552/726581

# get the absolute path of the executable
SELF_PATH=$(cd -P -- "$(dirname -- "$0")" && pwd -P) && SELF_PATH=$SELF_PATH/$(basename -- "$0")

# resolve symlinks
while [[ -h $SELF_PATH ]]; do
    # 1) cd to directory of the symlink
    # 2) cd to the directory of where the symlink points
    # 3) get the pwd
    # 4) append the basename
    DIR=$(dirname -- "$SELF_PATH")
    SYM=$(readlink "$SELF_PATH")
    SELF_PATH=$(cd "$DIR" && cd "$(dirname -- "$SYM")" && pwd)/$(basename -- "$SYM")
done

PATH_TO_ME=`dirname $SELF_PATH`;
if [ -z "$BIOMAKE_PATH" ]; then
    BIOMAKE_PATH=$PATH_TO_ME/../prolog;
fi

#$PATH_TO_ME/swipl_wrap -L0 -G0 -T0 -q -p library=$BIOMAKE_PATH  -g 'assert(biomake_prog("'$0'")),main,halt' -t halt -s $BIOMAKE_PATH/biomake/cli -- "$@"

#[[ $_ != "$0" ]] && echo "Script is being sourced" && exit 9

me="${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}"
good_exit=7
exitcode=$good_exit


[ -z "${keep_going}" ] && export keep_going=""
if [[ "$*" == *"-k"* ]]; then 
  export keep_going="-k"
fi

runtime_testing=4
export next_cls=0
export on_complete=test_completed


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

kill -9  %2 %3 %4 &>/dev/null ; kill -9  %1 %2 %3 %4 &>/dev/null
[ -t 1 ] && echo "<!--" && cls && echo -e "\n-->"
kill -9  %1 %2 %3 %4 %5 %6 &>/dev/null

if [ $# -ne 0 ] 
then
   listOfNames=( "$@" )
   if [ $# -eq 1 ]
   then
      [ -t 1 ] && echo "<!-- on_complete=true -->"
      on_complete=test_completed
   else
      
      [ -t 1 ] && echo "<!--" && cls && echo -e "\n-->"
      echo -e "\\n\\n<testsuites>\\n\\n"
      EXIT_COMPLETE="\\n\\n</testsuites>\\n\\n"
   fi
else
      echo -e "\\n\\n<testsuites>\\n\\n"
      EXIT_COMPLETE="\\n\\n</testsuites>\\n\\n"
fi

  			 
echo -e "\\n<!--\\nRunning Matching Tests: $me $keep_going ${listOfNames[*]}\\n-->\\n"

for ele2 in "${listOfNames[@]}"
  do 
  	for ele in $ele2
	do 
	  retry=1
	  while [ $retry == 1 ]
	   do
	    retry=0
		  [[ "$ele" == *".ansi" ]] && continue
        [[ "$ele" == *".html" ]] && continue
        if [[ "$ele" == *".sh" && -x "$ele" ]]; then
         CMD="./${ele}"
        else
   		#// Runs the test -f .swiplrc 
         #CMD="swipl -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"thread_create(['${ele}'],Id),thread_join(Id),$on_complete\" "
         CMD="swipl -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"(['${ele}'])\" -g \"$on_complete\" "
        fi

        echo "<testsuite name=\"${ele}\">"
        echo "<system-out><![CDATA["
        echo $CMD >> /tmp/logicmoo_testing/CMD_LAST.ansi
        eval $CMD | tee -a /tmp/logicmoo_testing/CMD_LAST.ansi
        exitcode=$?
        echo "]]></system-out>"
        cat /tmp/logicmoo_testing/junit_single.xml
        echo -e "</testsuite>\n\n\n\n"
		
      echo -e "\n<!-- EXITCODE=$exitcode -->\n" >> /tmp/logicmoo_testing/CMD_LAST.ansi
       
        if [ $exitcode -eq $good_exit ]; then
			[ "${next_cls}" == 1 ] && cls && next_cls=0
			echo -e "\\n\\n<!-- SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode}) -->\\n\\n"
         cat /tmp/logicmoo_testing/CMD_LAST.ansi >> /tmp/logicmoo_testing/successes.ansi
			continue
	     fi
 
        cat /tmp/logicmoo_testing/CMD_LAST.ansi >> /tmp/logicmoo_testing/failures.ansi 

        next_cls=0

      [ "$on_complete" == 'on_complete' ] && [ $exitcode -ne 7 ] && echo -e "\\n<!--\\nFAILED: $0 ${keep_going} ${ele} (returned ${exitcode})\\n-->\\n"
      [ $exitcode -eq 7 ] && echo -e "\\n<!--\\nSUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})\\n-->\\n"
      [ $exitcode -eq 0 ] && [ "$on_complete" == 'true' ] && echo -e "\\n<!--\\nSUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})\\n-->\\n"
      [ $exitcode -eq 6 ] && retry=1 && continue 

      
		# // 2 -> 1
		if [ $exitcode -eq 2 ]; then
         [ "$keep_going" == "-k" ] && echo "...keep going..." && continue
         echo "<!--not keep going-->"
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
			echo "ans=$ans"
		done
            echo "ans=$ans"
            echo "-->"

      [ "$ans" == '' ] && [ $exitcode -eq 0 ] && [ "$on_complete" == 'true' ]  && retry=1 && continue 

      [ "$ans" == '' ] && [ $exitcode -eq 7 ] && retry=1 && cls && continue  # 7 + enter

		[ "$ans" == 'y' ] && continue
      [ "$ans" == 'a' ] && KEEP_GOING=1 && continue
		[ "$ans" == 'B' ] && continue # down arrow
		[ "$ans" == 'A' ] && retry=1 && cls && continue  # up arrow
		[ "$ans" == 'r' ] && retry=1 && continue 
      
		echo "<!-- Exiting the script. Have a nice day!-->"
      echo -e $EXIT_COMPLETE
		exit $exitcode    
	  done
	done
  done

echo -e $EXIT_COMPLETE
exit $exitcode  

fi



#!/bin/bash

[[ $_ != "$0" ]] && echo "Script is being sourced" && ./$0 $* 

export SWI_OPTS=''

me="${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}"
good_exit=4
exitcode=$good_exit

keep_going=""
runtime_testing=4

if [ "$1" == "-k" ]; then
  keep_going="-k"
  runtime_testing=5
  shift
fi



#// For test_prolog fc_08.pfc
if [ -f "$1" ] ; then 
   echo -e "\\n\\nRunning Single Test: " swipl ${SWI_OPTS} -g "\"set_prolog_flag(runtime_testing,${runtime_testing})\"" -g "\"['""$1""']\"" "${@:2}" -g "\"halt(4)\"" "\\n\\n"
   exec time swipl $SWI_OPTS -g "set_prolog_flag(runtime_testing,${runtime_testing})" -g "['""$1""']" "${@:2}" -g "halt(4)"
fi


exitPrompt(){
    read -r -n1 -t1 -p "" ans #// clear pending key
    read -r -n1 -t10 -p "Do you wish to continue? [y]es or [N]o: " ans
    if [ "$ans" == 'y' ]
    then
	    return 1
    else
        echo "Exiting the script. Have a nice day!"
        return 0         
    fi
}

#// For test_prolog  (no args)
 declare -a listOfNames=( 
                        # // sanity tests
                           "*_01*.p*" "*_02*.p*" 
                        # // full tests
                        # "*_02*.p*" "*_03*.p*" "_04*.p*" "*_05*.p*" "*_06*.p*" "*_07*.p*" "*_08*.p*" "*_09*.p*" "*_10*.p*" "*_11*.p*" "*_12*.p*" 
                        # // feature tests
                        # "*_f01*.p*" "*_f02*.p*" "*_f03*.p*" "_f04*.p*" "*_f05*.p*" "*_f06*.p*" "*_f07*.p*" "*_f08*.p*" "*_f09*.p*" "*_f10*.p*" "*_f11*.p*" "*_f12*.p*" 
)                           



if [ $# -ne 0 ]; then
    listOfNames=( "$@" )
fi

#cls=1
							 
echo -e "\\n\\nRunning Matching Tests: $me $keep_going ${listOfNames[*]}\\n\\n"

for ele2 in "${listOfNames[@]}"
  do 
  	for ele in $ele2
	do 
	  retry=1
	  while [ $retry == 1 ]
	   do
	    retry=0
		
		#// Runs the test
        "$0" $keep_going "${ele}"
        
		exitcode=$?                 
        if [ $exitcode -eq $good_exit ]; then
			[ $cls == 1 ] && cls
			echo -e "\\n\\nSUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})\\n\\n"		
			continue			
	    fi
        cls=0
		
		echo -e "\\n\\nFAILED: $0 ${keep_going} ${ele} (returned ${exitcode})\\n\\n"
		
		# // 2 -> 1
		[ $exitcode -eq 2 ] && exit 1
		 
		# // Not Abort
		[ $exitcode -ne 1 ] && [ "$keep_going" == "-k" ] && continue
		
        
		echo "Do you wish to continue? [y]es, [Up/r]etry or [N]o: "
		read -sN1 -t 0.0001 k1		
		
		while true
		do
			read -r -sn1 ans
			[ "$ans" == "" ] && break;
			case $ans in
			    A) break;;
				B) break;;
				r) break;;
				y) break;;
				n) break;;
				E) break;;
			esac
			echo ans=$ans
		done

		[ "$ans" == 'y' ] && continue
		[ "$ans" == 'B' ] && continue # down arrow
		[ "$ans" == 'A' ] && retry=1 && continue  # up arrow
		[ "$ans" == 'r' ] && retry=1 && continue
		echo "Exiting the script. Have a nice day!"
		exit $exitcode    
	  done
	done
  done
exit $exitcode  
   

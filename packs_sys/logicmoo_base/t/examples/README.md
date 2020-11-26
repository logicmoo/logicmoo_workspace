#!/bin/bash

# sanity_base files are ran with swiplb
alias swiplb='/usr/bin/swipl -x lmb.prc '
# sanity_user files are ran with swiplu
alias swiplu='/usr/bin/swipl -x lm_user.prc '
alias swipl='/usr/bin/swipl '


cls  || true

rm -f */*.log.html  || true

EXIT() {
  kill -9 $(jobs -p) %2 %3
  fg
  kj
  exit 34
}

set -o pipefail

rm -f ./ALL.log.html || true
rm -f ./sanity_base/ALL.log.html || true


echostatus(){
  if [ $1 -eq 7 ]; then
    echo -e "\n\n\nPASSED $2     !!!!!!!!! STATUS=$1 "
    return $1
  else 
    if [ $1 -ne 0 ]; then
         echo -e "\n\n\nABORT $2 STATUS=$1" 
	 return $1
      else
         echo -e "\n\n\nRESTART $2 STATUS=$1"
	 return $1
    fi
  fi
}

status=0

run_test_file() {

local status=0
trap 'EXIT' 1 2 3 4 5 6 7 8 9
 
 ( 
  echo -e "\n\n%=%=% Test $1 %=%=%\n\n"
  /usr/bin/swipl -x lmb.prc -f $1 -g "halt(7)." 
  status=$?
  echostatus $status $1
  ) 2>&1 |
  tee -i >( ./ansi2html.sh >> $1.log.html) 2>&1 |
  tee -i -a >( ./ansi2html.sh >> ./sanity_base/ALL.log.html)
  
  if [ $status -eq 7 ]; then
    return 7
  fi

  if [ $status -ne 0 ]; then
    EXIT 666
    exit 666
  fi
 
}

export -f run_test_file echostatus EXIT

if [ "$1" != "" ]; then

   find ./sanity_base/ -name "*.p*" -name "$1" -and \( -not -name "*htm*" \) -print -exec  bash -c 'run_test_file $0' '{}' \;

   #find ./ -name "*.p*" -and \( -not -name "*htm*" \) -print0  | grep "$1" | xargs -I % bash -c 'run_test_file '

else


PASS01() {
	run_test_file ./sanity_base/attvar_01.pl 
	run_test_file ./sanity_base/attvar_02.pfc 
	run_test_file ./sanity_base/attvar_03.pfc 
	run_test_file ./sanity_base/attvar_05.pfc 
	run_test_file ./sanity_base/attvar_06.pfc 
	run_test_file ./sanity_base/attvar_07.pfc 
	run_test_file ./sanity_base/attvar_08.pfc 
	run_test_file ./sanity_base/attvar_09.pfc 
	run_test_file ./sanity_base/attvar_10.pfc 
	run_test_file ./sanity_base/attvar_11.pfc 
	run_test_file ./sanity_base/chr_01.pl 
	run_test_file ./sanity_base/fc_03.pfc
	run_test_file ./sanity_base/fc_01.pfc
	
	run_test_file ./sanity_base/fc_04.pfc 
	run_test_file ./sanity_base/fc_05.pfc 
	run_test_file ./sanity_base/fc_06.pfc 
	run_test_file ./sanity_base/fc_07.pfc 
	run_test_file ./sanity_base/fc_08.pfc 
	run_test_file ./sanity_base/fc_09.pfc 
	run_test_file ./sanity_base/fc_10.pfc 
	run_test_file ./sanity_base/file_01.pfc 
	run_test_file ./sanity_base/file_02.pfc 
	run_test_file ./sanity_base/file_03.pfc 
	run_test_file ./sanity_base/if_missing_01.pfc 
	run_test_file ./sanity_base/if_missing_02.pfc 
	run_test_file ./sanity_base/if_missing_03.pfc 
	run_test_file ./sanity_base/nd_01.pl 
	run_test_file ./sanity_base/pl_01.pfc 
	run_test_file ./api/mpred_01.pl 
	run_test_file ./api/mpred_02.pl 
	run_test_file ./api/utils_01.pl 
	run_test_file ./api/utils_02.pl 
	run_test_file ./fol/zenls.pfc 
	run_test_file ./sanity_base/df_01.pfc 
	run_test_file ./sanity_base/df_03.pfc 
	run_test_file ./sanity_base/df_04.pfc 
	run_test_file ./sanity_base/df_05.pfc 
	run_test_file ./sanity_base/dl_01.pfc 

}

eval PASS01

FAIL01() {
    run_test_file ./sanity_base/bc_01.pfc
    run_test_file ./sanity_base/fc_02.pfc
    run_test_file ./sanity_base/if_missing_04.pfc
    run_test_file ./sanity_base/if_missing_05.pfc
    run_test_file ./sanity_base/mpred_pfc_test_01.pl
    run_test_file ./sanity_base/mpred_pfc_test_02.pl
    run_test_file ./sanity_base/neg_01.pfc
}

EASY_PASS03() {
	run_test_file ./sanity_base/attvar_04.pl
	run_test_file ./sanity_base/fc_11.pfc
	run_test_file ./pfc/pqr.pfc
}


HARD_PASS04() {

	run_test_file ./fol/dislikes.pfc
	run_test_file ./fol/family_inheritance.pfc
	run_test_file ./fol/family_regress.pfc
	run_test_file ./fol/fol_birdt.pfc
	run_test_file ./fol/sanity_fi_human.pfc
	run_test_file ./fol/sanity_fi_sk.pfc
	run_test_file ./fol/sanity_sk_human.pfc
	run_test_file ./fol/sanity_sk_two.pfc
	run_test_file ./pfc/bc.pfc
	run_test_file ./pfc/sanity_clauses.pfc
	run_test_file ./pfc/sanity_neg.pfc
	run_test_file ./sanity_base/df_02.pfc
    
    run_test_file ./csp/4ct.pfc.pl
    run_test_file ./csp/einstein.pfc.pl
    run_test_file ./csp/einstein_simpler.pfc.pl
    run_test_file ./fol/lefty.pfc
    run_test_file ./pfc/abd.pfc
    run_test_file ./pfc/adder.pfc
    run_test_file ./pfc/at.pfc
    run_test_file ./pfc/bdi.pfc
    run_test_file ./pfc/bible.pfc
    run_test_file ./pfc/bulb1.pfc
    run_test_file ./pfc/bulbs.pfc
    run_test_file ./pfc/clue.pfc
    run_test_file ./pfc/clue_game1.pfc
    run_test_file ./pfc/consistent.pfc
    run_test_file ./pfc/constraints.pfc
    run_test_file ./pfc/dcg_pfc.pl
    run_test_file ./pfc/default.pfc
    run_test_file ./pfc/default2.pfc
    run_test_file ./pfc/disj.pfc
    run_test_file ./pfc/equality.pfc
    run_test_file ./pfc/finin.pfc
    run_test_file ./pfc/g.pfc
    run_test_file ./pfc/income.pfc
    run_test_file ./pfc/kinship.pfc
    run_test_file ./pfc/kr.pfc
    run_test_file ./pfc/kr2.pfc
    run_test_file ./pfc/krpeople.pfc
    run_test_file ./pfc/lion.pfc
    run_test_file ./pfc/lion.pl
    run_test_file ./pfc/mid.pfc
    run_test_file ./pfc/midsummer.pfc
    run_test_file ./pfc/monkey.pfc
    run_test_file ./pfc/na.pfc
    run_test_file ./pfc/ndcg_pfc.pl
    run_test_file ./pfc/pfc_tests.pl
    run_test_file ./pfc/problems.pfc
    run_test_file ./pfc/sanity_birdt.pfc
    run_test_file ./pfc/sanity_sv.pfc
    run_test_file ./pfc/show.pfc
    run_test_file ./pfc/skolem.pfc
    run_test_file ./pfc/small.pfc
    run_test_file ./pfc/sum.pfc
    run_test_file ./pfc/test.pl
    run_test_file ./pfc/test2.pfc
    run_test_file ./pfc/test_grammar.pl
    run_test_file ./pfc/tmsex.pfc
}

fi


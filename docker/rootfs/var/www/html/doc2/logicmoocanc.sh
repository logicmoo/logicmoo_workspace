#!/bin/bash
#cd /devel/logicmoo/src_modules/candc
#cd "$(dirname "$(realpath "$0")")";
#cd /mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/candc
# echo "$*" > /tmp/candc_query.txt
cd /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/candc
bin/candc --models models/boxer --models models/questions --models models --candc-printer boxer --input $1 > $1.ccg

if [ ! "${4}" == "0" ]; then
echo ""
cat $1.ccg
fi

echo "% cmdargs: ${*} "

if [ ! "${3}" == "0" ]; then
echo -e "pos(\""
bin/pos --model models/pos --input $1 
echo -e "\")."
echo ""
fi

if [ ! "${2}" == "0" ]; then
echo ""         

BOXER_OPTS="--input ${1}.ccg --resolve --box --roles verbnet --tense --modal" # --theory drt"

bin/boxer --semantics drs --copula true $BOXER_OPTS 
echo ""
bin/boxer --semantics fol --copula true $BOXER_OPTS 
echo ""
echo ""

bin/boxer --semantics drs --copula false $BOXER_OPTS 
echo ""
bin/boxer --semantics fol --copula false $BOXER_OPTS 
echo ""
echo ""
fi

echo "end_of_file."

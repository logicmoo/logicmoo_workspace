
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- regulus_batch('$REGULUS/Examples/Toy1Alterf/scripts/toy1_specialised_alterf.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).

:- halt.



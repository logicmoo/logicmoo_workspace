
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- regulus_batch('$REGULUS/Examples/Toy1SpecialisedDynamic/scripts/toy1_specialised_dynamic.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).

:- halt.



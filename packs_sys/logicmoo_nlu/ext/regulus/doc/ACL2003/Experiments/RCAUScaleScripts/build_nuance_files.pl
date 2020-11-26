
% Load the config file to define library directories etc
:- ['$REGULUS/doc/ACL2003/Experiments/Prolog/config'].

% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [1]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_1.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [2]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_2.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [5]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_5.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [10]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_10.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [25]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_25.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [50]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_50.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nRCAU THRESHOLD: ~d~n~n', [100]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/RCAUScaleScripts/psa_rcau_100.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).

:- halt.



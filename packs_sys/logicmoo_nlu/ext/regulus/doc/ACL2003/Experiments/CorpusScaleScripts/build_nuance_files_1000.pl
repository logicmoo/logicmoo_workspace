
% Load the config file to define library directories etc
:- ['$REGULUS/doc/ACL2003/Experiments/Prolog/config'].

% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- format('~N~n~n**************************~nNUMBER OF TRAINING EXAMPLES: ~d~n~n', [1000]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/CorpusScaleScripts/psa_1000.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).

:- halt.



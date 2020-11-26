
% Load the config file to define library directories etc
:- ['$REGULUS/doc/ACL2003/Experiments/Prolog/config'].

% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [5]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_5.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [10]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_10.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [15]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_15.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [20]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_20.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [25]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_25.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [30]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_30.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [35]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_35.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [36]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_36.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [37]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_37.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [38]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_38.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [39]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_39.cfg', 
		 ["NUANCE"]).

:- format('~N~n~n**************************~nNUMBER OF FEATS: ~d~n~n', [40]).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/IterationScripts/psa_iteration_40.cfg', 
		 ["NUANCE"]).

:- halt.



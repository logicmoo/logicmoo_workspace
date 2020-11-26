
% Load the config file to define library directories etc
:- ['$REGULUS/doc/ACL2003/Experiments/Prolog/config'].

% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_original.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_house.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_travel_deals.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS + MED~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_med.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS + MED + CHECKLIST (NO SYN-FEATS)~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_checklist_no_syn.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS + MED + CHECKLIST + MOBILE-AGENTS (NO SYN-FEATS)~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_mobile_agents_no_syn.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS + MED + CHECKLIST (WITH SYN-FEATS)~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_checklist.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).


:- format('~N~n~n**************************~nGRAMMAR VERSION: PSA + HOUSE + TRAVEL_DEALS + MED + CHECKLIST + MOBILE-AGENTS (WITH SYN-FEATS)~n~n', []).

:- regulus_batch('$REGULUS/doc/ACL2003/Experiments/GrammarScaleScripts/psa_mobile_agents.cfg', 
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE"]).

:- halt.



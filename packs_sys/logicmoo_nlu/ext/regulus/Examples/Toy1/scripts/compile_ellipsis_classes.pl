
:- ['$REGULUS/Prolog/load.pl'].

:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_slt_ellipsis.cfg', 
		 ["LOAD", "LOAD_TRANSLATE", "COMPILE_ELLIPSIS_PATTERNS"]).

:- halt.


:- ['$REGULUS/Prolog/load'].

:- regulus_batch('$REGULUS/Examples/Toy1Specialised/scripts/toy1_specialised_slt.cfg',
		 ["LOAD_TRANSLATE"]).

:- compile('$REGULUS/Examples/Toy1Specialised/Prolog/toy1_slt_app').

:- toy1_slt_app(1975).

:- halt.

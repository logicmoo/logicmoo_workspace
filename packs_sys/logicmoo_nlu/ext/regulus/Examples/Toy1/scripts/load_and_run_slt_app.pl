
:- ['$REGULUS/Prolog/load'].

:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_slt.cfg', ["LOAD_TRANSLATE"]).

:- compile('$REGULUS/Examples/Toy1/Prolog/toy1_slt_app').

:- toy1_slt_app(1975).

:- halt.




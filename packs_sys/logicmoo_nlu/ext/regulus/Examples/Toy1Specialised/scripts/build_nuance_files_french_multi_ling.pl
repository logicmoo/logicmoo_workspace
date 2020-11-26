
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- regulus_batch('$REGULUS/Examples/Toy1Specialised/scripts/toy1_french_generation.cfg', 
		 ["NUANCE"]).

:- halt.



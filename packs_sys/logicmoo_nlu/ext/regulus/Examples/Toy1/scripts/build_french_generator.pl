
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile generator
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/french.cfg', ["LOAD_GENERATION"]).

:- halt.



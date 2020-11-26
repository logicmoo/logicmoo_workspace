
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile Regulus grammar to Nuance
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_corrections.cfg', ["NUANCE"]).

:- halt.



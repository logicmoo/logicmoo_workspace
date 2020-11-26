
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile Regulus grammar to Nuance
:- regulus_batch('$REGULUS/Examples/Toy1Strcat/scripts/toy1_with_macros.cfg', ["NUANCE"]).

:- halt.



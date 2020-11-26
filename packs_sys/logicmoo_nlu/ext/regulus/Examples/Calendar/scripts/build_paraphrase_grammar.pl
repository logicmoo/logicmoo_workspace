
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile generation grammar
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/paraphrase.cfg', ["LOAD_GENERATION"]).

:- halt.


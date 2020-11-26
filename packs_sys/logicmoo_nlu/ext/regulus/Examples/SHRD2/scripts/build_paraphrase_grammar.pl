
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile generation grammar
:- regulus_batch('$REGULUS/Examples/SHRD2/scripts/paraphrase.cfg', ["LOAD_GENERATION"]).

:- halt.


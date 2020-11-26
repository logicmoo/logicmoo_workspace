

% :- ensure_loaded('$REGULUS/PrologLib/compatibility').
% 
% % Compile utilities
% 
% :- compile('$REGULUS/PrologLib/utilities').
% :- compile('$REGULUS/Prolog/regulus_utilities').
% 
% % Compile the main Regulus code
% :- compile('$REGULUS/Prolog/regulus2nuance').
% :- compile('$REGULUS/Prolog/regulus_top').
% 
% % Compile EBL code
% :- compile('$REGULUS/Prolog/ebl_make_training_data').
% :- compile('$REGULUS/Prolog/ebl_train').
% :- compile('$REGULUS/Prolog/ebl_postprocess').

:- ensure_loaded('$REGULUS/Prolog/regulus_saved_state').

:- load_or_restore_regulus_creating_saved_state_if_necessary.


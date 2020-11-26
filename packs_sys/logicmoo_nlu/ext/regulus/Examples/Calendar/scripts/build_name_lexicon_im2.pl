
% Compile the script file
:- compile('$REGULUS/Examples/Calendar/Prolog/create_name_lexicon.pl').

% Build lexicon
:- create_name_lexicon('$REGULUS/Examples/Calendar/Prolog/database_im2.pl',
		       '$REGULUS/Examples/Calendar/Regulus/generated_names_im2.regulus').

:- halt.



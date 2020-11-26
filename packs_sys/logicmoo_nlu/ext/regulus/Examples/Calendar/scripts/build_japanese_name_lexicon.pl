
% Compile the script file
:- compile('$REGULUS/Examples/Calendar/Prolog/create_name_lexicon.pl').

% Build lexicon
go1 :- create_name_lexicon('$REGULUS/Examples/Calendar/Prolog/japanese_database.pl',
			   '$REGULUS/Examples/Calendar/Regulus/japanese_generated_names.regulus',
			   regulus(japanese)).

go2 :- create_name_lexicon('$REGULUS/Examples/Calendar/Prolog/japanese_database.pl',
			   '$REGULUS/Examples/Calendar/Prolog/japanese_generated_names.pl',
			   prolog).

:- go1.

:- go2.

:- halt.



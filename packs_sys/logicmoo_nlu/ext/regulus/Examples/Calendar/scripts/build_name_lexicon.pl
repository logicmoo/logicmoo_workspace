
% Compile the script file
:- compile('$REGULUS/Examples/Calendar/Prolog/create_name_lexicon.pl').

% Build lexicon
go :-
	load_speech_forms_of_words('$REGULUS/Examples/Calendar/Prolog/speech_forms_of_paraphrase_words.pl'),
	create_name_lexicon('$REGULUS/Examples/Calendar/Prolog/database1.pl',
			    '$REGULUS/Examples/Calendar/Regulus/generated_names.regulus').

:- go.

:- halt.



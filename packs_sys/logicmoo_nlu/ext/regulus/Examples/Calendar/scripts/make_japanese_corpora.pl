
:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

go :-
	transcriptions_file_to_sents_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/all_transcriptions.txt',
					  '$REGULUS/Examples/Calendar/Generated/JapDataCollSep2008_no_intro.pl'),
	
	cat_files(['$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008_intro.pl',
		   '$REGULUS/Examples/Calendar/Generated/JapDataCollSep2008_no_intro.pl'],
		  '$REGULUS/Examples/Calendar/Generated/JapDataCollSep2008.pl').

:- go.

:- halt.

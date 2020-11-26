
%---------------------------------------------------------------

:- module(batchrec,
	  [do_batchrec_multiple/4,
	   do_text_batchrec_multiple/3,
	   check_saved_rec_results_alist/1,
	   do_batchrec/4,
	   do_text_batchrec/3]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).

%---------------------------------------------------------------

/*

Simple interface to batchrec routines defined in home/speech/CambridgePrologLib/batchrec_tools.pl

Called by classfier_trainer.pl

The idea is to call the real or text batchrec routine several times, as specified by the 
RecParamsAlist, and put the results in the RecResultsAlist. See classifier_trainer.pl
for file formats.

*/

% do_batchrec_multiple(+Wavfiles, +Transcriptions, +RecParamsAlist, +RecResultsAlist)

do_batchrec_multiple(_Wavfiles, _Transcriptions, [], _RecResultsAlist) :-
	!.
do_batchrec_multiple(Wavfiles, Transcriptions, [Key-RecParams | RecParamsAlistR], RecResultsAlist) :-
	member(Key-RecResults, RecResultsAlist),
	do_batchrec(Wavfiles, Transcriptions, RecParams, RecResults),
	!,
	do_batchrec_multiple(Wavfiles, Transcriptions, RecParamsAlistR, RecResultsAlist),
	!.
do_batchrec_multiple(Wavfiles, Transcriptions, RecParamsAlistR, RecResultsAlist) :-
	format('~N*** Error: bad call: ~w~n', [do_batchrec_multiple(Wavfiles, Transcriptions, RecParamsAlistR, RecResultsAlist)]),
	fail.

%---------------------------------------------------------------

% do_text_batchrec_multiple(+Transcriptions, +RecParamsAlist, -RecResultsAlist)

do_text_batchrec_multiple(Transcriptions, RecParamsAlist, RecResultsAlist) :-
	do_text_batchrec_multiple1(Transcriptions, RecParamsAlist, RecResultsAlist, 1),
	!.
do_text_batchrec_multiple(Transcriptions, RecParamsAlist, RecResultsAlist) :-
	format('~N*** Error: bad call: ~w~n', [do_text_batchrec_multiple(Transcriptions, RecParamsAlist, RecResultsAlist)]),
	fail.

do_text_batchrec_multiple1(_Transcriptions, [], [], _N) :-
	!.
do_text_batchrec_multiple1(Transcriptions, [Key-RecParams | RecParamsAlistR], [Key-RecResults | RecResultsAlistR], I) :-
	join_with_underscore([text_based_rec_results, I], RecResults),
	do_text_batchrec(Transcriptions, RecParams, RecResults),
	I1 is I + 1,
	!,
	do_text_batchrec_multiple1(Transcriptions, RecParamsAlistR, RecResultsAlistR, I1).

%---------------------------------------------------------------

do_batchrec(Wavfiles, Transcriptions, RecParams, PrologFile) :-
	make_tmp_file(batchrec_trace, BatchrecTraceFile),
	make_tmp_file('', TmpDirectory),
	do_batchrec(Wavfiles, Transcriptions, RecParams, BatchrecTraceFile, PrologFile, TmpDirectory).

%------------------------------------------------------------------------------------

do_text_batchrec(Transcriptions, RecParams, PrologFile) :-
	make_tmp_file('', TmpDirectory),
	do_text_batchrec(Transcriptions, RecParams, PrologFile, TmpDirectory).

%--------------------------------------------------------------------------------------------

check_saved_rec_results_alist([]) :-
	!.
check_saved_rec_results_alist([_Tag-F | R]) :-
	check_saved_rec_results_file(F),
	check_saved_rec_results_alist(R),
	!.

check_saved_rec_results_file(RecResults) :-
	absolute_file_name(RecResults, AbsRecResults), 
	file_exists(AbsRecResults),
	format('~N-- Reading batchrec input from stored file ~w~n', [AbsRecResults]),
	print_rec_results_from_stored_batchrec_file(AbsRecResults),
	!.
check_saved_rec_results_file(RecResults) :-
	format('~N*** Error: unable to interpret ~w as name of existing stored batchrec file~n', [RecResults]),
	fail.

%--------------------------------------------------------------------------------------------

print_rec_results_from_stored_batchrec_file(File) :-
	open(File, read, SIn),
	print_rec_results_from_stream(SIn),
	close(SIn),
	!.
print_rec_results_from_stored_batchrec_file(File) :-
	format('~N*** Error: bad call: ~w~n', [print_rec_results_from_stored_batchrec_file(File)]),
	fail.

print_rec_results_from_stream(SIn) :-
	getline(SIn, Line),
	print_rec_results_from_stream1(Line, SIn).

print_rec_results_from_stream1(Line, _SIn) :-
	Line = eof,
	!.
print_rec_results_from_stream1(Line, SIn) :-
	(   append("%Rec Final Total", _, Line) ->
	    format('~N~n~s~n', [Line]) ;
	    true
	),
	print_rec_results_from_stream(SIn).
	
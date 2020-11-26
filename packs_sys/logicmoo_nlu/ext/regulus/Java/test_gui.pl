%%% test_gui.pl

:- module(test_gui, [main/0,my_predicate/2]).

:- use_module(library(prologbeans)).
:- use_module(library(charsio),[read_from_chars/2]).
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(system)).

%% Register acceptable queries and start the server (using default port)
main:-
    register_query(evaluate(C, P),
		   write_dummy_file(C, P)),
    format('~N~nReady to receive requests.~n~n', []),
    start.

%% In this case we know that we have received a list of characters
%% that needs to be converted into an expression!
write_dummy_file(Chars, P) :-
    (   is_prolog_string(Chars) ->
	format('~N~nReceived request: "~s"~n', [Chars])
    ;
	otherwise ->
	format('~N~nReceived request (not string - error!): "~w"~n', [Chars]),
	fail
    ),
    read_from_chars(Chars, X),
    X = [NumberOfLines, NumberToCountToPerLine],
    write_dummy_file1(NumberOfLines, NumberToCountToPerLine),
    format_to_atom('Wrote file "tmp.txt" with ~d lines, counted to ~d each time',
		   [NumberOfLines, NumberToCountToPerLine],
		   P),
    !.

write_dummy_file1(NumberOfLines, NumberToCountToPerLine) :-
	open('c:/java/regulus/java/tmp.txt', write, S),
	write_dummy_file1(1, NumberOfLines, NumberToCountToPerLine, S),
	close(S),
        sleep(1).
       

write_dummy_file1(I, NumberOfLines, _NumberToCountToPerLine, _S) :-
	I > NumberOfLines,
	!.
write_dummy_file1(I, NumberOfLines, NumberToCountToPerLine, S) :-
	I =< NumberOfLines,
	count_to_n(NumberToCountToPerLine),
	format(S, '~NLine ~d: counted to ~d~n', [I, NumberToCountToPerLine]),
	flush_output(S),
	I1 is I + 1,
	!,
	write_dummy_file1(I1, NumberOfLines, NumberToCountToPerLine, S).

count_to_n(N) :-
	N =< 0.
count_to_n(N) :-
	N1 is N - 1,
	count_to_n(N1).



    


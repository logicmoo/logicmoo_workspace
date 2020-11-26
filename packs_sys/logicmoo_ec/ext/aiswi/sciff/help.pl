:-module(help,
     [user_help/0, help/1, devman/1, devman/0]).

:- use_module(sciff_options).

:- dynamic(help/1).
%:- [browser]. 
% Marco Gavanelli:
% I use ensure loaded in each of the exported files,
% so the compilation is faster, in case the user
% does never issue any help command.

user_help :-
    ensure_loaded(browser),
    browse_html('userman.html',index).

help(Option):-
    ensure_loaded(browser),
    browse_html('userman.html',Option).
devman(Option):-
    ensure_loaded(browser),
    browse_html('devman.html',Option).
devman:- devman(index),!.
devman:- ensure_loaded(browser),
    list_topics('devman.html').
/*
%----------------------------------------------------------
% OPTION
%----------------------------------------------------------
help(option) :- help(options).
help(options) :-
	findall(Option, sciff_option(Option, _), ListOption),
	nl, write('options available:'), nl, nl,
	print_options_list(ListOption),
	nl.
print_options_list( []).
print_options_list( [ Option | T]) :-
	write(Option), nl,
	print_options_list(T).




%----------------------------------------------------------
% HOWTO
%----------------------------------------------------------
help(howto) :-
    write('*** SCIFF proof-procedure ***\n'),
    write('Create a directory with a name (say "name") containing the following files:\n'),
    write('1. "name_sokb.pl" containing your SOKB (i.e., your program)\n'),
    write('2. "name_ics.txt" containing the integrity constraints\n'),
    write('3. "name_history.txt" containig the history\n'),
    write('call the goal: "build(name)"\n'),
    write('recompile SCIFF\n'),
    write('call either "run" or "run_no_close"\n').



%----------------------------------------------------------
% SET_AN_OPTION
%----------------------------------------------------------
help(set_an_option) :-
    nl,
    write('Each option can be set using the predicate:\n'),
    write('set_option(Option,Value)\n'),
    write('where Option is the name of the option you want to modify,\n'),
    write('and Value is the new value you want to assign to it.'),
    nl.


%----------------------------------------------------------
% GET_AN_OPTION
%----------------------------------------------------------
help(get_an_option) :-
    nl,
    write('You can inspect the state of an option using the predicate:\n'),
    write('sciff_option(?Option, ?State).\n'),
    nl.


%----------------------------------------------------------
% SHOW_OPTION
%----------------------------------------------------------
help(show_options) :-
    nl,
    write('The predicate \'show_options\' shows all the options available and\n'),
    write('their state.\n'),
    nl.






%----------------------------------------------------------
% FULFILLER OPTION
%----------------------------------------------------------
help(fulfiller) :-
	nl,
	write('fulfiller option is '),
	sciff_option(fulfiller, Answer),
	write(Answer), nl, nl,
	write('fulfiller option enables the MarcoA\'s rule for generating histories (g-sciff).'), nl,
	write('This option should be set off unless you are using the g-sciff.'),
	nl, nl.



%----------------------------------------------------------
% FDET OPTION
%----------------------------------------------------------
help(fdet) :-
	nl,
	write('fdet option is '),
	sciff_option(fdet, Answer),
	write(Answer), nl, nl,
	write('fdet option enables the deterministic behaviour of the sciff.'), nl,
	write('It is very useful whenever you must achieve better performances in spite of completeness.'), nl,
	write('Usually is off.'), nl,
	nl, nl.
	
	

%----------------------------------------------------------
% SEQ_ACT OPTION
%
% There cannot be two events at the same time.
% Useful for the abductive Event Calculus
%----------------------------------------------------------
help(seq_act) :-
	nl,
	write('seq_act option is '),
	sciff_option(seq_act, Answer),
	write(Answer), nl, nl,
	write('seq_act option checks if two events are happening at the same time.'), nl,
	write('If seq_act is on, two events cannot happen at the same time.'), nl,
	write('If seq_act is off, more events can happen at the same time.'), nl,
	write('This option is primarly used in the abductive event calculus.'), nl,
	write('Usually is off.'), nl,
	nl, nl.



%----------------------------------------------------------
% FACTORING OPTION
%----------------------------------------------------------
help(factoring) :-
	nl,
	write('factoring option is '),
	sciff_option(factoring, Answer),
	write(Answer), nl, nl,
	write('Actually unknown, introduced by MarcoG'), nl,
	write('Usually is off.'), nl,
	nl, nl.



%----------------------------------------------------------
% SCIFF_DEBUG OPTION
%----------------------------------------------------------
help(sciff_debug) :-
	nl,
	write('sciff_debug option is '),
	sciff_option(sciff_debug, Answer),
	write(Answer), nl, nl,
	write('Print on screen debug messages.'), nl,
	write('Usually is off.'), nl,
	nl, nl.


%----------------------------------------------------------
% VIOLATION_CAUSES_FAILURE OPTION
%----------------------------------------------------------
help(violation_causes_failure) :-
	nl,
	write('violation_causes_failure option is '),
	sciff_option(violation_causes_failure, Answer),
	write(Answer), nl, nl,
	write('Decides if a violation of the protocol should induce a failure '),
	write('(and backtracking where possible) of the proof.'), nl,
	write('Allowed values are yes/no.'), nl,
	write('Default value is yes.'), nl,
	nl, nl.
	

%----------------------------------------------------------
% GRAPHVIZ OPTION
%----------------------------------------------------------
help(graphviz) :-
	nl,
	write('graphviz option is '),
	sciff_option(graphviz, Answer),
	write(Answer), nl, nl,
	write('Represents the sciff transition in form of a graph, using the graphviz library '), nl,
	write('Default value is off.'), nl,
	nl, nl.
	

%----------------------------------------------------------
% allow_events_not_expected OPTION
%----------------------------------------------------------
help(allow_events_not_expected) :-
	nl,
	write('allow_events_not_expected option is '),
	sciff_option(allow_events_not_expected, Answer),
	write(Answer), nl, nl,
	write('By default, sciff allows events to happen even if they are not expected.'), nl,
	write('By setting it to \'no\', sciff detects as violation if an'), nl,
	write('happened event does not have a corresponding expectation.'), nl,
	write('Default value is yes.'), nl,
	nl, nl.

help(min_viol_closed):-
    writeln('min_viol_closed/1'),
    writeln('amongst the various branches selects the one with the minimal'),
    writeln('number of violations. Returns the number of violations.'),
    writeln('Useful when used in conjunction with the option'),
    writeln('violation_causes_failure -> no.'),
    writeln('Known bug: the number of violations is not precise: often'),
    writeln('the reported number of violations should be increased by 1.').

help(min_viol_open):-
    writeln('min_viol_closed/1'),
    writeln('amongst the various branches selects the one with the minimal'),
    writeln('number of violations. Returns the number of violations.'),
    writeln('Useful when used in conjunction with the option'),
    writeln('violation_causes_failure -> no.'),
    writeln('Known bug: the number of violations is not precise: often'),
    writeln('the reported number of violations should be increased by 1.').

writeln(X):- write(X), nl.
*/

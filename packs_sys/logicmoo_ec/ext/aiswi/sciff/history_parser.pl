:-module(history_parser,
	 [translate_history/2,
      translate_histories/2]).

:- use_module(parser_utils).
:- use_module(debug).
%:- use_module(library(lists)).
:- [apache_log_parser].

translate_histories(InFiles,OutFile):-
    open(OutFile,write,Stream),
	write(Stream,':-module(history,[hap/2,history_is_empty/1]).'),nl(Stream),nl(Stream),
	translate_histories1(InFiles,Stream,Empty),
	write(Stream,'history_is_empty('),
    write(Stream,Empty),
    write(Stream,').'),nl(Stream),
% When the history is empty, SWI complains that the predicate hap/2 is not defined
% and raises an error. Thus, I add a definition that is always false 
    (Empty=yes -> write(Stream,'hap(whatever,_):- false.'), nl(Stream) ; true),
	close(Stream).
translate_histories1([],_,yes).
translate_histories1([InFile|Rest],Stream,Empty):-
    write_debug('Parsing file '), write_debug(InFile),
	once(parse_history(InFile,History)),
	writeln_debug(' --> OK'),
	(History = [] -> Empty=Empty1 ; Empty=no),
	nl(Stream),write(Stream,'%%%% '), write(Stream,InFile), write(Stream,' %%%%'), nl(Stream),
	write_history_to_stream(History,Stream),
    translate_histories1(Rest,Stream,Empty1).

translate_history(InFile,OutFile):-
	parse_history(InFile,History),
	open(OutFile,write,Stream),
	write(Stream,':-module(history,[hap/2,history_is_empty/1]).'),nl(Stream),nl(Stream),
	write_history_to_stream_1(History,Stream),
	close(Stream).


parse_history(FileName,History):- % Apache Log File: name ends with "access_log"
    atom_concat(_,'access_log',FileName),!, % Does not remove comments, spaces, ...
    read_file_to_string(FileName,FileString),
    phrase(history(History,1),FileString).
parse_history(FileName,History):-
	read_file_to_string(FileName,FileString),
	drop_comments(FileString,FileString2),
	drop_whites(FileString2,NoWhitesString),
	phrase(history(History,1),NoWhitesString).

history([],_) --> [].

history(Events,N) --> [10], history(Events,N). % In the Apache log file case, we cannot remove whites, so we remove them here
history(Events,N) --> [-1], history(Events,N). % In the Apache log file case, we cannot remove whites, so we remove them here
history([Event|MoreEvents],N) -->
	event(Event),
	!,
	{N1 is N+1},
	history(MoreEvents,N1).
history([_|_],N) -->
    {write_error('Error in Event number '), 
    write_error(N), write_error(' ***'), nl, 
    write('see help(history).'), nl, fail}.

event(hap(statePath(Content),Time)) -->
	funct('statePath'),
	opening_parenthesis,
	variable(Content),
	comma,
	time(Time),
	closing_parenthesis,
	full_stop,!.

event(hap(end,Time)) -->
	funct('end'),
	opening_parenthesis,
	time(Time),
	closing_parenthesis,
	full_stop,!.

% Accepts also events in the form hap(Event,Time)
event(hap(Event,Time)) -->
	funct('hap'),
	opening_parenthesis,
	term(Event),
	comma,
	time(Time),
	closing_parenthesis,
	full_stop,!.

event(hap(Content,Time)) -->
    apache_log_hap(Content,Time),!.

event(hap(Content,Time)) -->
	initial_functor(Act),
	society_id,
	dialog(Dialog),
	sender(Sender),
	receiver(Receiver),
	performative(Performative),
	performative_arguments(Arguments),
	event_time(Time),
	event_full_stop,
	{Content1=..[Performative|Arguments],
	 Content=..[Act,Sender,Receiver,Content1,Dialog]}.





event_full_stop --> full_stop.
event_full_stop -->
    {write_error('\n*** Could not find full stop: '), fail}.

initial_functor(Act) --> funct(Act), opening_parenthesis, !.
initial_functor(_) --> {write_error('\n*** Error in functor: '), fail}.

society_id -->	"[",atomic_constant_list(_),"]",comma,!.
society_id --> {write_error('\n*** Error in Society ID: should be a list containing an atom'),nl, fail}.

dialog(Dialog) --> atomic_constant(Dialog),comma,!.
dialog(_) --> {write_error('\n*** Error in Dialog: should be a non compound constant'),nl, fail}.

sender(Sender) --> atomic_constant(Sender),comma,!.
sender(_) --> {write_error('\n*** Error in Sender: should be a non compound constant'),nl, fail}.

receiver(Receiver) --> atomic_constant(Receiver),comma,!.
receiver(_) --> {write_error('\n*** Error in Receiver: should be a non compound constant'),nl, fail}.

performative(Performative) --> funct(Performative),comma,!.
performative(_) --> {write_error('\n*** Error in Performative: should be a non compound constant'),nl, fail}.

performative_arguments(Arguments) --> "[",term_list(Arguments),"]",comma,!.
performative_arguments(_) --> {write_error('\n*** Error in performative Arguments: should be a list of terms'),nl, fail}.

event_time(Time) --> time(Time),closing_parenthesis,!.
event_time(_) --> {write_error('\n*** Error in Time: should be an integer'),nl, fail}.

write_history_to_file(FileName,History):-
	open(FileName,write,Stream),
	write_history_to_stream(History,Stream),
	close(Stream).

write_history_to_stream_1([],Stream):-
	!,
	write(Stream,'history_is_empty(yes).'),
	nl(Stream).
write_history_to_stream_1(History,Stream):-
	write(Stream,'history_is_empty(no).'),
	nl(Stream),
	write_history_to_stream(History,Stream).

write_history_to_stream([],_).
write_history_to_stream([Event|MoreEvents],Stream):-
	write(Stream,Event),
	write(Stream,'.'),
	nl(Stream),
	write_history_to_stream(MoreEvents,Stream).


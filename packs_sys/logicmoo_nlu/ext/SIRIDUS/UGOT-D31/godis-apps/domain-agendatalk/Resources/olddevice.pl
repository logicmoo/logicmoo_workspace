
:- use_module( library(lists), [ member/2, select/3, append/3, nth0/3] ).
:-use_module( calendar, [day2date/2, inconsistent/2,weekday/2,next_weekday/2, today/1, tomorrow/1, ampm_disamb/3] ).
:- module(device_agendatalk, [ dev_set/2,
			 dev_get/2,
			 dev_do/2,
			 dev_query/3,
			 valid_parameter/1,
			 interpret_pragmatically/3
			] ).

:- dynamic variable_value/2.

:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:-ensure_loaded(semsort_agendatalk).


valid_parameter(add_more_info(X)):-
	yn_answer(X).

valid_parameter(take_down_event(X)):-
	yn_answer(X).

yn_answer(yes).
yn_answer(no).
%%% Actions (action(+Name,+Parameters))

action( 'AddEvent', [event_to_store,
		     date_to_store,
		     start_time_to_store, ampm_disamb, location_to_store ] ).

action( 'AddEvent', [event_to_store, date_to_store, start_time_to_store, ampm_disamb]).
action( 'GetEvent', [date_to_store, start_time_to_store]).
action( 'ChangeInfo', [which_info]).
action( 'MoreInfo', [location_to_store]).
action( 'ConsistentDate', []).
%action( 'GetTime', [event_to_get,date_to_get, start_time_to_get]).

%queries: query(+Query,+Parameters)
query(consistent_date(_YN), [date_to_store]).
query(usage,[]).

%%% Variable default values
default_value(location_to_store, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

%pretty weird hack
perform_query( usage, usage).

perform_query( consistent_date(_), Answer) :-
	  dev_get( date_to_store, Date),
	  (calendar:inconsistent(Date, yes),
	  
	  Answer=consistent_date(no)
	;
	 
	 Answer=consistent_date(yes)
	).


dev_set(ID,Value1) :-
	interpret_pragmatically(ID,Value1,Value),
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	format(' *** ~a <- ~w\n',[ID,Value]).



dev_get(ID,Value) :-
	( variable_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	)
	,format(' *** ~a -> ~w\n',[ID,Value]).


dev_do(Command,Commitments) :-
	action(Command,Vars),
	set_command_variables(Vars,Commitments,Values),
	output_upnp(Command,Values),
	perform_command(Command).


set_command_variables([],_,[]).
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-
	Com =.. [ Var, Val ],
	lists:member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).




perform_command( 'AddEvent' ) :-
	!,
	dev_get(location_to_store, Loc),
	dev_get( event_to_store, Event ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, StartTime ),
	dev_get( ampm_disamb, AMPM),
	calendar:ampm_disamb(StartTime, AMPM, Start), 
	
	concAtom(Date,Start,Start1),
	zerofy(Start1,Start2),
	
	
	%%oaag:solve(addEvent(Usr,Event,Start2,Loc, Res)),
	print(addEvent(Event,Start2, Loc)).


perform_command( 'GetEvent' ) :-
	!,
	dev_get( date_to_store, Usr ),
	dev_get( start_time_to_store, N )
%	oaag:solve(getEvent(Usr,N)).
.
perform_command( 'GetTime'):-
	!,
	dev_get(event_to_get, E),
	dev_get(date_to_get, D),
	dev_get(start_time_to_get, T)
	%ooag:solve(getTime(E,D,T))
	.

perform_command( _ ) :- true.






add(A,D,B) :-
	atom_chars(A,AC),
	number_chars(AN,AC),
	BN is AN + D,
	number_chars(BN,BC),
	atom_chars(B,BC).

try(G) :-
	( G -> true ; true ).


to_number(Atom,Number) :-
	number_atom(Atom),
	atom_chars(Atom,Cs),
	number_chars(Number,Cs).

number_atom(A) :-
	atomic(A),
	\+ number(A).


interpret_pragmatically(date_to_store, today, Date) :-!,
	calendar:today(date(_Y,M,D)),
	Date0 is M*100+D,
	number_chars(Date0,Date0Str),
	atom_chars(Date,Date0Str).

interpret_pragmatically(date_to_store, tomorrow, Date) :-!,
	calendar:tomorrow(date(_Y,M,D)),
	Date0 is M*100+D,
	number_chars(Date0,Date0Str),
	atom_chars(Date,Date0Str).

interpret_pragmatically(date_to_store, Weekday, Date):-
	sem_sort(Weekday, weekday),
	calendar:next_weekday(date(_Y,M,D),Weekday),
	Date0 is M*100+D,
	number_chars(Date0,Date0Str),
	atom_chars(Date,Date0Str).

	
interpret_pragmatically(date_to_store, Day, Date):-
	calendar:day2date(Day, date(_Y,M,D)),
	Date0 is M*100+D,
	number_chars(Date0,Date0Str),
	atom_chars(Date,Date0Str).

interpret_pragmatically(_,V,V).

date_field(N,[0'0,C]) :-
	N < 10,
	!,
	format_to_chars('~d',[N],[C]).

date_field(N,Cs) :-
	format_to_chars('~d',[N],Cs).

output_upnp(Command,Parameters) :-
	Term =.. [ Command | Parameters ],
	format('\n[UPnP] ~w\n\n',[Term]).



% adds a '0' to one digit time values
zerofy(In,Out):-
	name(In,[A,B]),!,
	atom_chars(Out,[A,B]).
zerofy(In,Out):-
	name(In,[A,B,C,D]),!,
	atom_chars(Out,[A,B,C,D]).
zerofy(In,Out):-
	name(In,[A,B,C]),
	lists:append([48],[A,B,C],Out2),
	atom_chars(Out,Out2).
zerofy(In,Out):-
	name(In,[A,B,C,D,E,F,G,H]),!,
	atom_chars(Out,[A,B,C,D,E,F,G,H]).
zerofy(In,Out):-
	name(In,[A,B,C,D,E,F,G]),
	lists:append([48],[A,B,C,D,E,F,G],Out2),
	atom_chars(Out,Out2).



concAtom(A,B,AB):-
	name(A,AStr),
	name(B,BStr),
	lists:append(AStr,BStr,ABStr),
	atom_chars(AB,ABStr).


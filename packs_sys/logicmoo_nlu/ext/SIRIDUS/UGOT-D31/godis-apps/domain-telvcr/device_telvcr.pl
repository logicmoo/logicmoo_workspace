
%:- module( vcr, [ var_set/2,
%		  consult_vcr/2,
%		  issue_command/2 ] ).

:- use_module( library(lists), [ member/2, select/3, append/3, nth0/3] ).
:-use_module( calendar, [weekday/2,next_weekday/2, today/1, tomorrow/1] ).
:- module(device_telvcr, [ dev_set/2,
			 dev_get/2,
			 dev_do/2,
			 dev_query/3,
			 valid_parameter/1
			] ).

:- dynamic variable_value/2.

:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:-ensure_loaded(library(oaag)).



valid_parameter( channel_to_store(C) ):-
	oaag:solve(getChannels(Cs)),
	lists:member(C,Cs).


%%% Actions (action(+Name,+Parameters))


action( 'AddRecording', [ channel_to_store,
			  date_to_store,
			  start_time_to_store,
			  stop_time_to_store ,
			  user_name] ).

action( 'DeleteRecording', [ rec_job_to_delete,user_name ] ).

%queries: query(+Query,+Parameters)
query(rec_job_exists(_PA),[user_name]).
query(rec_jobs(_Jobs),[user_name]).
query(channels(_Cs),[]).
query(valid_channel(_C),[]).
query(usage,[]).
query(valid_recjob(_C),[ channel_to_store,
			 date_to_store,
			 start_time_to_store,
			 stop_time_to_store ,
			 user_name ]).

%%% Variable default values
default_value( rec_jobs, [] ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

%pretty weird hack
perform_query( usage, usage).

perform_query( rec_job_exists(N), Answer) :-
	(
	  dev_get( user_name, Usr),
	  oaag:solve(getRecJobs(Usr,Jobs)),
	  length(Jobs,NumJobs),
	  N=<NumJobs,
	  N>0,
	  Answer=rec_job_exists(N)
	;
	 Answer=not(rec_job_exists(N))
	).

%perform_query( rec_job_exists(_), not(rec_job_exists(_))).

perform_query( rec_jobs(Jobs), rec_jobs(Jobs) ):-
	dev_get( user_name, Usr ),
	oaag:solve(getRecJobs(Usr,Jobs)),!.

perform_query( channels(X), channels(X) ):-
	oaag:solve(getChannels(X)),!.

perform_query( valid_channel(X), valid_channel(X) ):-
	oaag:solve(validChannel(X)),!.

%checks that recjob can be added Vcr:s answer in Res
perform_query( valid_recjob(Res),valid_recjob(Res) ):-	!,
	dev_get( channel_to_store, Program ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Start ),
	dev_get( stop_time_to_store, Stop ),
	dev_get( user_name, Usr ),
	concAtom(Date,Start,Start1),
	concAtom(Date,Stop,Stop1),
	zerofy(Start1,Start2),
	zerofy(Stop1,Stop2),
	oaag:solve(validRecJob(Usr,Program,Start2,Stop2,Res)),
	print(validRecJob(Usr,Program,Start2,Stop2,Res)).
	

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


perform_command( 'AddRecording' ) :-
	!,
	dev_get( channel_to_store, Program ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Start ),
	dev_get( stop_time_to_store, Stop ),
	dev_get( user_name, Usr ),
	concAtom(Date,Start,Start1),
	concAtom(Date,Stop,Stop1),
	
	zerofy(Start1,Start2),
	zerofy(Stop1,Stop2),
	
	oaag:solve(addJob(Usr,Program,Start2,Stop2,Res)),
	print(addJob(Usr,Program,Start2,Stop2,Res)).


perform_command( 'DeleteRecording' ) :-
	!,
	dev_get( user_name, Usr ),
	dev_get( rec_job_to_delete, N ),
	oaag:solve(removeJob(Usr,N)).



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
	calendar:next_weekday(date(_Y,M,D),Weekday),
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


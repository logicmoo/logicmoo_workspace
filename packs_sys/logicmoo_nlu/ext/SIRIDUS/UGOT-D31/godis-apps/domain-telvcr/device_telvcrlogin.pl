
:- use_module( library(lists), [ member/2, select/3, append/3] ).
:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).


:- module(device_telvcrlogin, [ dev_set/2,
				dev_get/2,
				dev_do/2,
				dev_query/3,
				valid_parameter/1
			] ).
:- dynamic variable_value/2.
:-ensure_loaded(library(oaag)).


%%% Actions (action(+Name,+Parameters))


action( 'CheckUser', [ user_name,
		       password] ).

query( validUser(_Usr,_Pwd,_Result),[]).
query( userExists(_User, _Res),[]).

%%% Variable default values
default_value( rec_jobs, [] ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

perform_query( validUser(User, Psswd, Res), validUser(User,Psswd, Res)) :- 
	oaag:solve(checkUser(User,Psswd,Res)).

perform_query( userExists(User, Res), userExists(User, Res)) :- 
	oaag:solve(checkUser(User,_,Res)).



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
	member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).


perform_command( 'LoginUser' ) :-
	!,
	dev_get( channel_to_store, Program ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Start ),
	dev_get( stop_time_to_store, Stop ),
	zerofy(Start,Start2),
	zerofy(Stop,Stop2),
	concAtom(Date,Start2,Start3),
	concAtom(Date,Stop2,Stop3),
	% get userId
	oaag:solve(addJob(godag,Program,Start3,Stop3,Res)),
	print(addJob(godag,Program,Start3,Stop3,Res)).



perform_command( _ ) :- true.


valid_parameter( rec_job_to_delete( N ) ):-
	dev_query( rec_job_exists(N),[] ,rec_job_exists(N)).

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

interpret_pragmatically(date_to_store,today,D) :-
	!,
	datime(datime(_,Month,Day,_,_,_)),
	date_field(Month,[Month1,Month2]),
	date_field(Day,[Day1,Day2]),
	atom_chars(D,[Month1,Month2,Day1,Day2]).

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
	append([48],[A,B,C],Out2),
	atom_chars(Out,Out2).
	

concAtom(A,B,AB):-
	name(A,AStr),
	name(B,BStr),
	append(AStr,BStr,ABStr),
	atom_chars(AB,ABStr).


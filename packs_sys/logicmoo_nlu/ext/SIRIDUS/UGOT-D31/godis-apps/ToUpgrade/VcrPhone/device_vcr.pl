%%% vcr: Panasonic NV-SD200
%%% by Alexander Berman, July 2001

%:- module( vcr, [ var_set/2,
%		  consult_vcr/2,
%		  issue_command/2 ] ).
:- module( device_vcr, [ dev_set/2,
		  dev_get/2,
		  dev_do/2,
		  dev_query/2] ).

:- use_module( library(lists), [ member/2, select/3 ] ).
:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- dynamic variable_value/2.

%%% Environment mode (simulation or real)

environment_mode(simulation).

%%% Actions (action(+Name,+Parameters))

action( 'Play', [] ).
action( 'Stop', [] ).
action( 'FF', [] ).
action( 'Rew', [] ).
action( 'PauseStill', [] ).
action( 'StillAdv', [] ).
action( 'Record', [] ).
action( 'PowerOn', [] ).
action( 'PowerOff', [] ).
action( 'IncreaseProgramPosition', [] ).
action( 'DecreaseProgramPosition', [] ).
action( 'SetProgramPosition', [ new_program_position ] ).
action( 'IncreaseVolume', [] ).
action( 'DecreaseVolume', [] ).
action( 'SetClock', [ new_clock ] ).
action( 'AutoInitialize', [] ).
action( 'AddProgram', [ program_position_to_store,
			date_to_store,
			start_time_to_store,
			stop_time_to_store ] ).
action( 'DeleteProgram', [ program_number ] ).

%%% Variable default values

default_value( program_position, '1' ).
default_value( play_status, stopped ).
default_value( programs, [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_vcr :-
	environment_mode(simulation),
	user:flag( visualize_devices, yes ),
	!,
	ensure_loaded(library(visualization)),
	gfx_set_initial_values(device_vcr,vcr).

init_vcr. %see end of file


%dev_get( Var, Val ):-
%	!,
%	var_get( Var, Val ).

%consult_vcr( val(Var), Val ) :-
%	!,
%	var_get( Var, Val ).

%consult_vcr( program_exists(PA), yes ) :-
%	var_get( programs, Programs ),
%	to_number( PA, P ),
%	member( P:_, Programs ),
%	!.
dev_query( program_exists(PA), yes ) :-
	dev_get( programs, Programs ),
	to_number( PA, P ),
	member( P:_, Programs ),
	!.

dev_query( program_exists(_), no ) :- !.

dev_query( available_program_slot, no ) :-
	dev_get( programs, Programs ),
	length( Programs, 8 ),
	!.

dev_query( available_program_slot, yes ).

dev_set(ID,Value1) :-
	environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	%format(' *** ~a <- ~w\n',[ID,Value]),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(vcr,ID,Value) ;
	    true ).

dev_get(ID,Value) :-
	environment_mode(simulation),
	( variable_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	)
	%,format(' *** ~a -> ~w\n',[ID,Value])
	.

dev_do(Command,Commitments) :-
	action(Command,Vars),
	set_command_variables(Vars,Commitments,Values),
	( environment_mode(simulation) ->
	    output_upnp(Command,Values) ;
	    true ),
	perform_command(Command).

set_command_variables([],_,[]).
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-
	Com =.. [ Var, Val ],
	member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).

perform_command( 'IncreaseProgramPosition' ) :-
	!,
	dev_get( program_position, P ),
	( P == '99' ->
	    P1 = '1' ;
	    add(P,1,P1) ),
	dev_set( program_position, P1 ).

perform_command( 'DecreaseProgramPosition' ) :-
	!,
	dev_get( program_position, P ),
	( P == '1' ->
	    P1 = '99' ;
	    add(P,-1,P1) ),
	dev_set( program_position, P1 ).

perform_command( 'AddProgram' ) :-
	!,
	dev_get( programs, ProgramsOld ),
	find_program_number( ProgramsOld, N ),
	dev_get( program_position_to_store, Program ),
	dev_get( date_to_store, Date ),
	dev_get( start_time_to_store, Start0 ),
	dev_get( stop_time_to_store, Stop0 ),
	fix4digit(Start0,Start),
	fix4digit(Stop0, Stop),
	insert_program( N, (Program,Date,Start,Stop), ProgramsOld, ProgramsNew ),
	dev_set( programs, ProgramsNew ),
	( user:flag(visualize_devices,yes) ->
	    gfx_add_program( N, Program, Date, Start, Stop ) ;
	    true ).
fix4digit(In,Out):-
	( atom_chars(In,[In1,In2,In3,In4]), Out=In );
	( atom_chars(In,[In2,In3,In4]),
	    atom_chars(Out,[48,In2, In3, In4]) ).
	
	

perform_command( 'DeleteProgram' ) :-
	!,
	dev_get( program_number, NA ),
	to_number( NA, N ),
	dev_get( programs, ProgramsOld ),
	select( N:_, ProgramsOld, ProgramsNew ),
	dev_set( programs, ProgramsNew ),
	( user:flag(visualize_devices,yes) ->
	    gfx_delete_program( N ) ;
	    true ).

perform_command( _ ) :- true.

find_program_number( Programs, N ) :-
	find_program_number( Programs, N, 1 ).

find_program_number( Programs, N, C ) :-
	member( C:_, Programs ),
	!,
	C1 is C + 1,
	find_program_number( Programs, N, C1 ).

find_program_number( _, N, N ).

insert_program( N, Info, [], [N:Info] ).

insert_program( N, Info, [ N1:Info1 | Rest ], [ N:Info, N1:Info1 | Rest ] ) :-
	N < N1, !.

insert_program( N, Info, [ P1 | Rest ], [ P1 | Rest1 ] ) :-
	insert_program( N, Info, Rest, Rest1 ).

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

:- init_vcr.

/*************************************************************************
         name: device_cellphone.pl
      version: Feb, 2005
  description: Device file for a mobile cellphone
      authors: Anna Olsson and Jessica Villing
*************************************************************************/

%:- module( vcr, [ var_set/2,
%		  consult_vcr/2,
%		  issue_command/2 ] ).
:- module( device_cellphone, [ dev_set/2,
			 dev_get/2,
			 dev_do/2,
			 %dev_query/2, 
			 dev_query/3, %DH 2003-03-21
			 valid_parameter/1
			] ).

:- use_module( library(lists), [ member/2, select/3 ] ).
:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- dynamic variable_value/2.

%%% Environment mode (simulation or real)

environment_mode(simulation).
%environment_mode(none).
 
%%% Actions (action(+Name,+Parameters))

action( 'Call_name', [name] ).
action( 'Call_number', [number] ).
action( 'Add', [name, number] ).
action( 'Search', [name] ).
action( 'Delete', [name] ).
action( 'Set_language', [language] ).
action( 'Set_security_code', [number] ).
action( 'Reset', [number] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_cellphone :-
	environment_mode(simulation),
	user:flag( visualize_devices, yes ),
	!,%trace,
	ensure_loaded(library(visualization)),
	gfx_set_initial_values(device_cellphone, cellphone).

init_cellphone. %see end of file

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

perform_command( _ ) :- true.

dev_set(ID,Value1) :-
	environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	%format(' *** ~a <- ~w\n',[ID,Value]),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(cellphone,ID,Value) ;
	    true ).

dev_get(ID,Value) :-
	environment_mode(simulation),
	( variable_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	).

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

:- init_cellphone.

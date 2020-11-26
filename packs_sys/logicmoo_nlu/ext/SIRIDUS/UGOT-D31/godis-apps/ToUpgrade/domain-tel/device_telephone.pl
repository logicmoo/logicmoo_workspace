:- module( device_telephone, [ dev_set/2,

			       dev_get/2,

			       dev_do/2,

			       dev_query/2,

			       valid_parameter/1

			     ] ).

:-use_module(library(lists)).



:- dynamic variable_value/2.

action( 'MakePhoneCall', [ destination ] ).

action( 'DivertCall', [ divert_to ] ).

action( 'CancelDivert', [ ] ).

action( 'ConferenceCall', [ first_person, second_person ] ).



valid_parameter(dummy).



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



perform_command( 'MakePhoneCall' ).

perform_command( 'DivertCall' ).

perform_command( 'CancelDivert' ).

perform_command( 'ConferenceCall' ).



dev_set(ID,Value1):-

	interpret_pragmatically(ID,Value1,Value),

	retractall(variable_value(ID,_)),

	assert(variable_value(ID,Value)).



dev_get(ID,Value) :-

	( variable_value(ID,CurrentValue) ->

	    Value = CurrentValue

	;

	    default_value(ID,Value)

	).



dev_query(dummy,dummy).





interpret_pragmatically(_,V,V).



output_upnp(Command,Parameters) :-

	Term =.. [ Command | Parameters ],

	format('\n[UPnP] ~w\n\n',[Term]).




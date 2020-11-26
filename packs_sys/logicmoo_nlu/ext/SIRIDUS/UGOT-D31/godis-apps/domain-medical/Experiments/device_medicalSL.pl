:- module( device_medical, [ dev_set/2,
			     dev_get/2,
			     dev_do/2,
				%dev_query/2, 
			     dev_query/3, %DH 2003-03-21
			     valid_parameter/1
			] ).

:- use_module( library(lists), [ member/2, select/3 ] ).
:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module( library(oaag),[solve/1]).

:- dynamic variable_value/2.

%%% Actions (action(+Name,+Parameters))

action( 'dummy', [parameter1] ).



%%% Variable default values

default_value( bla, hey ).


dev_query( best_labtest( _ ), _, best_labtest( fever ) ).

dev_query( lab_result( fever ), _, lab_result( fever ) ).

dev_query( info(X), _, info(X) ).

% special_purpose dev_query (DH)

dev_query( X^disease(X), Commitments, Answer):-

	not2neg(Commitments,Commitments0),
	
	
	oaag:solve(diagnose(Commitments0,
			    queryResponse(PossibleDiseases,_RelSyms,
					  _RelTests,_RelHist))),
	whatdisease0(PossibleDiseases,Answer).
	


dev_query( Q^bestq(Q), Commitments,Answer):-
	not2neg(Commitments,Commitments0),
	oaag:solve(diagnose(Commitments0,
			    queryResponse(PossibleDiseases,RelSyms,
					  RelTests,RelHist))),
	%if there is a best question Q the bestq(Q) else not(bestq(Q))
	
	( best_question(PossibleDiseases,RelSyms,RelTests,RelHist,Q) ->
	    Answer = bestq(Q);
	    Answer = not(bestq(Q)) ).


%_Väldigt_ special purpose-predikat
whatdisease0([],fail(X^disease(X),no_matches)).
whatdisease0([D],disease(D)):-!.
whatdisease0(_,fail(X^disease(X),too_many_matches)).

%Ännu mer special purpose
not2neg([],[]).
not2neg([not(P)|Ps],[neg(P)|Qs]):- !,
	not2neg(Ps,Qs).
not2neg([P|Ps],[P|Qs]):- 
	not2neg(Ps,Qs).



%detta predikat kan man ändra i för att få annan strategi
%nu kollar det först bland symptoms sedan bland history (inte bland tests -
%de är inte frågor) bryr sig inte om huruvida de är 'reguired' eller inte.
best_question(_,[symptom(S,_,present(yes))|_Ss],_,_,symptom(S)):-!.

best_question(_,_,_,[medicalHistory(H,_,present(yes))|_Hs],medicalHistory(H)).


		    

dev_set(ID,Value1) :-
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(vcr,ID,Value) ;
	    true ).

dev_get(ID,Value) :-
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
% 1-place propositions
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-
	Com =.. [ Var, Val ],
	member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).
% 0-place propositions
set_command_variables([Prop|Vars],Commitments,[1|Vals]) :-
	Com =.. [ Prop ],
	member( Com, Commitments ),
	dev_set( Prop, 1 ), % 1 for true
	set_command_variables(Vars,Commitments,Vals).
% 0-place propositions, negated
set_command_variables([Prop|Vars],Commitments,[0|Vals]) :-
	Com =.. [ not, Prop ],
	member( Com, Commitments ),
	dev_set( Prop, 0 ), % 0 for false
	set_command_variables(Vars,Commitments,Vals).


set_all_command_variables( [] ).

set_all_command_variables( [Com | Coms] ):-
	Com =.. [ Pred | _ ],
	set_command_variables( [Pred], [Com], _ ),
	set_all_command_variables( Coms ).


perform_command( 'IncreaseChannel' ) :-
	!,
	dev_get( program_position, P ),
	( P == 99 ->
	    P1 = 1 ;
	    add(P,1,P1) ),
	dev_set( program_position, P1 ).


valid_parameter( program_to_delete( N ) ):-
	dev_get( programs, Programs ),
	member( N:_, Programs ).

perform_command( _ ) :- true.


output_upnp(Command,Parameters) :-
	Term =.. [ Command | Parameters ],
	format('\n[UPnP] ~w\n\n',[Term]).


try(G) :-
	( G -> true ; true ).

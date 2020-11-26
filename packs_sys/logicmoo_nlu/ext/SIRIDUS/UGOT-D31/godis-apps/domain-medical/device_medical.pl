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

%do solve here, so oaa setup message doesnt show up in middle of dialogue
:- solve(disease_names(_));error:error('Can not find OAA device').
%%% Actions (action(+Name,+Parameters))

action( 'dummy', [parameter1] ).
%action( 'TakeTest', [best_labtest] ).


% This is needed if the device should be able to answer queries that resolve
%issues that depend on other issues. Not needed in this domain, but needed in
%e.g. telvcr. It is not pretty and it is not UPnP but I am not proposing
%some kind of extended UPnP... - DH 21/3-2003
%%% query(+Query, +Parameters)

query(X^bestq(X),[]).



%%% Variable default values

default_value( bla, hey ).


	


%special_purpose dev_query (DH)
dev_query(confirmed_by_interview,Com,confirmed_by_interview):-
	member(disease(D),Com),
	oaag:solve(description(D,disease(D,_,_,symptoms(S),_,history(H),_,_))),

	\+ ( member(symptom(S1,required(yes),_),S),
	    \+member(symptom(S1),Com) ),
	\+ ( member(medicalHistory(H1,required(yes),_),H),
	    \+member(medicalHistory(H1),Com) ),!.

dev_query(confirmed_by_interview,Com,not(confirmed_by_interview)):-
	member(disease(D),Com),
	oaag:solve(description(D,disease(D,_,_,symptoms(S),_,history(H),_,_))),

	( ( member(symptom(S1,required(yes),_),S),
	    member(not(symptom(S1)),Com) );
	( member(medicalHistory(H1,required(yes),_),H),
	    member(not(medicalHistory(H1)),Com) ) ),!.


%dev_query(confirmed_by_interview,_Com,not(confirmed_by_interview)).
dev_query(confirmed_by_interview,_Com,fail(confirmed_by_interview)).


dev_query(confirmed_by_tests,Com,confirmed_by_tests):-
	member(disease(D),Com),
	oaag:solve(description(D,disease(D,_,_,_,tests(T),_,_,_))),
	\+ ( member(diagnosticTest(T1,_,_,required(yes),_),T),
	    \+ member(test_result(T1),Com) ),!.

dev_query(confirmed_by_tests,Com,not(confirmed_by_tests)):-
	member(disease(D),Com),
	oaag:solve(description(D,disease(D,_,_,_,tests(T),_,_,_))),
	member(diagnosticTest(T1,_,_,required(yes),_),T),
	member(not(test_result(T1)),Com ),!.

dev_query(confirmed_by_tests,_Com,fail(confirmed_by_tests)).


dev_query(X^info(X),Com,info(X)):-
	( member(info_disease(D),Com);
	    member(disease(D), Com) ),
	oaag:solve(description(D,disease(D,X,_,_,_,_,_,_))),!.

%dev_query(X^info(X),_Com,notexist(info(X))).
dev_query(X^info(X),_Com,fail(info(X))).




	
dev_query( X^disease(X), Commitments, Answer):-
	not2neg(Commitments,Commitments0),
	oaag:solve(diagnose(Commitments0,
			    queryResponse(PossibleDiseases,_RelSyms,
					  _RelTests,_RelHist))),
	whatdisease0(PossibleDiseases,Commitments,Answer).


dev_query( Q^bestq(Q), Commitments,Answer):-
	member(disease(Disease),Commitments),
	oaag:solve(description(Disease,disease(Disease,
					       _Description,_Regions,
					       symptoms(Symptoms),
					       _Tests,
					       history(History),
					       _Treatment,
					       _Prevention))),!,
	( (
	  ( member(symptom(S,_,_),Symptoms),
	      \+member(symptom(S),Commitments),
	      \+member(not(symptom(S)),Commitments),
	      \+member(rejected(symptom(S)),Commitments),
	      Answer=bestq(symptom(S)) )
%	    false
	  ;
	    ( member(medicalHistory(H,_,_),History),
		\+member(medicalHistory(H),Commitments),
		\+member(not(medicalHistory(H)),Commitments),
		\+member(rejected(medicalHistory(H)),Commitments),
		Answer=bestq(medicalHistory(H)) ) );
	    Answer=fail(X^bestq(X)) ).
	
	
dev_query( Q^bestq(Q), Commitments,Answer):-
	\+ member( disease(_), Commitments ),
	not2neg(Commitments,Commitments0),
	oaag:solve(diagnose(Commitments0,
			    queryResponse(PossibleDiseases,RelSyms,
					  RelTests,RelHist))),
	%if there is a best question Q the bestq(Q) else not(bestq(Q))
	
	( best_question(Commitments,PossibleDiseases,RelSyms,RelTests,RelHist,Q) ->
	    Answer = bestq(Q);
%	    Answer = not(bestq(Q))
	    Answer = fail(Q^bestq(Q))
	).

%dev_query( Q^bestq(Q), _, fail(Q^bestq(Q)) ).


%best labtest returns a ynq 
dev_query(X^best_labtest(X),Commitments,Answer):-
	member(disease(Disease),Commitments),
	oaag:solve(description(Disease,disease(Disease,
					       _Description,_Regions,_Symptoms,
					       tests(Tests),_History,_Treatment,
					       _Prevention))),!,
	( best_labtest(Commitments,Tests,Best) ->
	    Answer = best_labtest(Best);
	    Answer = not(best_labtest(_))
	).
	

%dev_query(X^test_result(X),_Com,Answer1):-
dev_query(test_result(_),_Com,Answer):-
	result_store(Answer).

dev_do(take_test(T),_Com):-
%	member(best_labtest(T),Com),
	take_test(T).

%_Väldigt_ special purpose-predikat



whatdisease0([],_Com,fail(X^disease(X),no_matches)).
whatdisease0([D],_Com,disease(D)):-!.
whatdisease0(_,_Com,fail(X^disease(X),too_many_matches)).



%Ännu mer special purpose
not2neg([],[]).
not2neg([not(P)|Ps],[neg(P)|Qs]):- !,
	not2neg(Ps,Qs).
not2neg([P|Ps],[P|Qs]):- 
	not2neg(Ps,Qs).



%detta predikat kan man ändra i för att få annan strategi
%nu kollar det först bland symptoms sedan bland history (inte bland tests -
%de är inte frågor) bryr sig inte om huruvida de är 'reguired' eller inte
best_question(Com,_,Syms,_Tests,Hist,Q):-
	(
	  member(P,Syms),
	  P=symptom(C,_,_),
	  Q=symptom(C)
	;
	  member(P,Hist),
	  P=medicalHistory(C,_,_),
	  Q=medicalHistory(C)
	
	),
	\+member(rejected(Q),Com).


%there is a nonrejected test for which the result is ot known
best_labtest(Com,Tests,Best):-
	member(diagnosticTest(Best,_,_,required('yes'),_),Tests),

	\+member(test_result(Best),Com),%
	\+member(not(test_result(Best)),Com),%
	\+member(rejected(test_result(Best)),Com).





dev_set(ID,Value) :-
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


take_test( Test ):-
	retractall(result_store(_)),
	oaag:solve(tests(Tests)),!,
	member(diagnosticTest(Test,test(Type),result(Result),_,_),Tests),
	format('Test   : ~a\n',[Type]),
	format('Result : ~a - y/n:\n',[Result]),
	get_nurse_test_input(C),
	( C = y ->
	    assert(result_store(test_result(Test)))
	;    
	    assert(result_store(not(test_result(Test))))
	).

get_nurse_test_input(C):-
	get_char(C0),
	( 
	  member(C0,[y,n]),
	  C=C0;
	  write('please type \'y\' or \'n\''),!,nl,
	  get_nurse_test_input(C)).

	
valid_parameter( program_to_delete( N ) ):-
	dev_get( programs, Programs ),
	member( N:_, Programs ).

perform_command( _ ) :- true.


output_upnp(Command,Parameters) :-
	Term =.. [ Command | Parameters ],
	format('\n[UPnP] ~w\n\n',[Term]).


try(G) :-
	( G -> true ; true ).

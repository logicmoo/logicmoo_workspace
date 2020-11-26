/*************************************************************************

        name: resource_interfaces.pl 
 description: IBiS4 Resource interface type definitions
 
*************************************************************************/

/************************************************************************

HISTORY, started 031104

031104
- modifed "depends" to include the "bind" and "raise" plan constructs [SL]

***************************************************************************/

:- module(resource_interfaces, [resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3 ]).

:- multifile resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.

:- discontiguous resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.


:- use_module(library(lists)).

% dummy so resource_operation is defined somewhere

resource_operation( dummy, dummy, dummy, dummy ).


/*----------------------------------------------------------------------
     cfg
----------------------------------------------------------------------*/
is_resource_type( cfg ).
resource_variable_of_type( sr_grammar, cfg ).
resource_of_type( srg_tvgodis_svenska, cfg ).
resource_of_type( srg_tvgodis_svenska2, cfg ).

resource_relation( rule, [Grammar, R ] ):-
	Grammar:rule(R).
resource_relation_type( rule, [cfg, cfg_production_rule] ).

resource_relation( rules, [Grammar, Rs] ):-
	Grammar:rules(Rs).
resource_relation_type( rules, [cfg, set(cfg_production_rule)] ).

resource_relation( start_symbol, [Grammar, S] ):-
	Grammar:start_symbol(S).

resource_relation_type( start_symbol, [cfg, cfg_nonterminal ]).




/*----------------------------------------------------------------------
     lexicon
----------------------------------------------------------------------*/

is_resource_type( lexicon ).

resource_variable_of_type( lexicon, lexicon ).

resource_of_type( lexicon_travel_english, lexicon ).
resource_of_type( lexicon_autoroute_english, lexicon ).
resource_of_type( lexicon_travel_svenska, lexicon ).
resource_of_type( lexicon_cellphone_svenska, lexicon ).
resource_of_type( lexicon_telephone_english, lexicon ).
resource_of_type( lexicon_vcr_english, lexicon ).
resource_of_type( lexicon_vcr_svenska, lexicon ).
resource_of_type( lexicon_telvcr_english, lexicon ).
resource_of_type( lexicon_telvcr_svenska, lexicon ).
resource_of_type( lexicon_telvcrlogin_english, lexicon ).
resource_of_type( lexicon_telvcrlogin_svenska, lexicon ).

resource_relation( input_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : input_form( Phrase, Move ).
resource_relation_type( input_form, [ lexicon, string, dmove ] ).

resource_relation( output_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : output_form( Phrase, Move ).
resource_relation_type( output_form, [ lexicon, string, dmove ] ).

resource_relation( yn_answer, [Lexicon, A] ) :-
	Lexicon : yn_answer( A ).
resource_relation_type( yn_answer, [ lexicon, answer ] ).

/*----------------------------------------------------------------------
     domain
----------------------------------------------------------------------*/

is_resource_type( domain ).

resource_variable_of_type( domain, domain ).

resource_of_type( travel, domain ).
resource_of_type( autoroute, domain ).
resource_of_type( telephone, domain ).
resource_of_type( vcr, domain ).
resource_of_type( telvcr, domain ).
resource_of_type( telvcrlogin, domain ).
resource_of_type( tvgodis, domain ).

resource_relation( relevant, [Domain, Answer, Query ] ) :-
	relevant_answer( Query, Answer, Domain ).
resource_relation_type( relevant_answer, [domain, question, answer] ).

resource_relation( resolves, [Domain, Answer, Query ] ) :-
	resolves( Query, Answer, Domain ).
resource_relation_type( resolves, [domain, question, answer] ).

resource_relation( plan, [Domain, Task,stackset(Plan)] ) :-
	Domain : plan( Task, Plan ).
resource_relation_type( plan, [ ] ).

resource_relation( combine, [Domain, Q, A, P] ):-
	combine(Q, A, P, Domain ).
resource_relation_type( combine, [domain, question, answer, prop] ).

resource_relation( abstract, [Domain, A, P, Q] ):-
	abstract( A, P, Q, Domain ).
resource_relation_type( abstract, [domain, answer, prop, question] ).

resource_relation( incompatible, [Domain, P1, P2] ):-
	incompatible( P1, P2, Domain ).
resource_relation_type( incompatible, [domain, prop, prop] ).

resource_relation( proposition, [Domain, P] ):-
	Domain : sort_restr( P ).
resource_relation_type( proposition, [domain, prop] ).

resource_relation( question, [Domain, Q] ):-
	Domain : sort_restr( issue(Q) ).
resource_relation_type( question, [domain, question] ).

resource_relation( issue, [Domain, Q] ):-
	Domain : sort_restr( issue(Q) ).
resource_relation_type( issue, [domain, question] ).

resource_relation( action, [Domain, A] ):-
	Domain : sort_restr( action(A) ).
resource_relation_type( action, [domain, action] ).



%Q1 depends on Q
% modified to include "bind" and "raise" [SL031104]
resource_relation( depends, [Domain, Q1, Q] ):-
	Domain : plan( Q1, Plan ),
	( member( findout( Q ), Plan );
	    ( member( bind( Q ), Plan );
		member( raise( Q ), Plan ) ) ).

resource_relation( depends, [Domain, Q1, Q] ):-
	Domain : depends( Q1, Q ).
resource_relation_type( depends, [domain, question, question] ).



% default question
resource_relation( default_question, [Domain, Q] ):-
	Domain : default_question( Q ).
resource_relation_type( default_question, [domain, question] ).


% actions

resource_relation( postcond, [_Domain, Action, done(Action)] ).
resource_relation( postcond, [Domain, Action, Prop] ):-
	Domain : postcond( Action, Prop ).
resource_relation_type( postcond, [domain, action, prop ] ).


resource_relation( valid_parameter, [Domain, Prop] ) :-
	Domain : valid_parameter( Prop ).
resource_relation_type( valid_parameter, [domain, prop ] ).

resource_relation( dominates, [Domain, A1, A2] ) :-
	dominates( Domain, A1, A2).
resource_relation_type( dominates, [domain, action, action ] ).

/**************
help predicates
for domain
**************/


% relevance / aboutness

% all resolving answers are relevant
relevant_answer( Q, A, Domain ):-
	resolves( Q, A, Domain ).



% "maybe" answer to y/n question is relevant but not resolving
relevant_answer( YNQ, maybe, _Domain ):-
	YNQ \= _X^_PX, % not wh
	YNQ \= set(_AltList). % not alt

% negative answers to wh-questions and alt-questions are relevant
% (but not resolving)

% WH questions
relevant_answer( X^PX, not(PA), Domain ):- % PA is proposition
	PX =.. [P,X],
	PA =.. [P,_],
	Domain:sort_restr( PA ), !.
relevant_answer( X^PX, not(A), Domain ):- % A is elliptical answer
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

relevant_answer( X^PX, not(TA), Domain ):- % PA is proposition
	\+ var( PX ),
	PX =.. [P,X],
	TA =.. [T,A],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ), !.

				% Alt-questions, full answer; 
relevant_answer( AltList, not(Alt), Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 


% altq
% negated answer
relevant_answer( set(AltList), (not Alt), Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% resolves


% database entries

resolves( Q, db_entry( _, P ), Domain ):-
	resolves( Q, P, Domain ).
resolves( Q, db_entry( _, _, P ), Domain ):-
	resolves( Q, P, Domain ).


%%% Yes/no questions 

resolves( P, yes, Domain ) :- Domain:sort_restr(P).
resolves( P, no, Domain ) :- Domain:sort_restr(P).
resolves( P, P, Domain ) :- Domain:sort_restr(P).
resolves( P, not(P), Domain ) :- Domain:sort_restr(P).

resolves( P, unknown(P), _).


% WH questions

resolves( X^PX, PA, Domain ):- % PA is proposition
	\+ var( PX ),
	PX =.. [P,X],
	PA =.. [P,_],
	Domain:sort_restr( PA ), !.
resolves( X^PX, A, Domain ):- % A is elliptical answer
	\+ var( PX ),
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% subtyping for ISIS4 020404; T is the type of A, e.g. channel
resolves( X^PX, TA, Domain ):- % PA is proposition
	\+ var( PX ),
	PX =.. [P,X],
	TA =.. [T,A],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ), !.


resolves( X^PX, notexist(X,PX), _Domain).

% Alt-questions

	

%full answer; 
resolves( set(AltList), Alt, Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 


% issues don't have to be in list of alternatives
resolves( set(AltList), issue(A), Domain ):-
	member( issue(_), AltList ),
	Domain:sort_restr(issue(A)). 

% actions are either in list of alternatives
resolves( set(AltList), action(A), Domain ):-
	member( action(A), AltList ),
	Domain:sort_restr(action(A)).

% or is dominated by action in list of alternatives
resolves( set(AltList), action(A), Domain ):-
	Domain:sort_restr(action(A)),
	member( action(AltAction), AltList ),
	dominates( Domain, AltAction, A ).

% "no"/"neither"
resolves( set(_AltList), no, _Domain ). 
% "no"/"neither"
resolves( set(AltList), (not set(AltList) ), _Domain ). 


%DH short answer to altq reg. props
resolves( set(AltList), TA , Domain ):-
	member(PX,AltList),
	TA =.. [T,A],
	PX =.. [P,_X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr(PA).


% understanding-question
resolves( und(_DP*P), A, Domain ):-
	resolves( P, A, Domain ).

resolves( und(_DP*set(AltList)), P, Domain ):-
	resolves( set(AltList), P, Domain ).

/*----------------------------------------------------------------------
   dominates( T1, T2 )
   -- Task T1 dominates T2 in the menu hierarchy
----------------------------------------------------------------------*/

dominates( Domain, T1, T2 ):-
	dominates0( Domain, T1, T2 ).


dominates( Domain, T1, T3 ):-
	dominates0( Domain, T1, T2 ),
	dominates( Domain, T2, T3 ).

dominates0( Domain, T1, T2 ):-
	Domain : plan( T1, Plan ),
	member( findout( set( Ts ) ), Plan ),
	member( action( T2 ), Ts ).










% combine( +Q, +A, ?P)
% combine question Q and answer A to proposition P

% fail

combine( Q, fail(Q), fail(Q), _Domain ).

%%% Yes/no questions 

combine( P, yes, P, Domain ) :- Domain:sort_restr(P).
combine( P, no, not(P), Domain ) :- Domain:sort_restr(P).
combine( P, P, P, Domain ) :- Domain:sort_restr(P).
combine( P, not(P), not(P), Domain ) :- Domain:sort_restr(P).


% WH questions

% nonelliptical, positive
combine( X^PX, PA, PA, Domain ):- % PA is proposition
	PA =.. [P,_A],
	PX =.. [P,X],
	Domain:sort_restr( PA ).
% nonelliptical, negative
combine( X^PX, not(PA), not(PA), Domain ):- % PA is proposition
	PA =.. [P,_A],
	PX =.. [P,X],
	Domain:sort_restr( PA ).

% elliptical,positive
combine( X^PX, A, PA, Domain ):- % A is elliptical answer
%	\+ var(PX),
%	\+ var(A),!,
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).
% elliptical,negative
combine( X^PX, not(A), not(PA), Domain ):- % A is elliptical answer
%	\+ var(PX),
%	\+ var(A),!,
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).


% subtyping for ISIS4 020404; T is the type of A, e.g. channel
combine( X^PX, TA, PA, Domain ):- % PA is proposition
	TA =.. [T,A],
	PX =.. [P,X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ).

combine( X^PX, not(TA), not(PA), Domain ):- % PA is proposition
	TA =.. [T,A],
	PX =.. [P,X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ).


% Alt-questions, full answer;
% does not allow Q, i.e. AltList to
% unsitantiated, since there is an infinite amount of alt-questions
% matching a ceratin answer and proposition.

combine( set(AltList), Alt, Alt, Domain ):-
	\+ var(AltList),
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

combine( set(AltList), (not Alt), (not Alt), Domain ):-
	\+ var(AltList),
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

combine( set(AltList), no, (not set(AltList)), _Domain ):-
	\+ var(AltList).


%DH elliptical answer to altq reg. props:
combine( set(AltList), TA , PA, Domain ):-
	member(PX,AltList),
	TA =.. [T,A],
	PX =.. [P,_X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr(PA).
		


% understanding-question

%combine( und(DP*C), A, P, Domain ):-
%	combine( C, A, P, Domain ).

combine( und(DP*P), P, und(DP*P), Domain ):-
	combine( P, P, P, Domain ).
combine( und(DP*P), not(P), not(und(DP*P)), Domain ):-
	combine( P, not(P), not(P), Domain ).

%combine( und(DP*P), yes, und(DP*P), Domain ).
%combine( und(DP*P), no, not(und(DP*P)), Domain ).

combine( und(DP*set(AltList)), P, und(DP*P), Domain ):-
	combine( set(AltList), P, P, Domain ).




% incompatible(+P1, +P2)
%
% P1 and P2 are incompatible propositions

% order is irrelevant.
incompatible( P1, P2, Domain ):-
	incompatible1( P1, P2, Domain ),!.
incompatible( P1, P2, Domain ):-
	incompatible1( P2, P1, Domain).

% negation

incompatible1( P, not(P), _ ).

% this is really only valid for feature-like predicates, i.e.
% predicates with whose extension is a singleton set

incompatible1( P2, P1, Domain):-
	Domain : incompatible( P1, P2 ).


/*
incompatible1( PA, PB, Domain ):-
	PA =.. [P,A],
	PB =.. [P,B],
	Domain:sort_restr(PA),
	Domain:sort_restr(PB),
	A \= B.
*/

/*----------------------------------------------------------------------
     database
----------------------------------------------------------------------*/

is_resource_type( database ).

resource_variable_of_type( database, database ).

resource_of_type( travel, database ).
resource_of_type( autoroute, database ).

resource_relation( consultDB, [Database, Query, PropSet, Answer] ) :-
	Database : consultDB( Query, PropSet, Answer ), !.
%resource_relation( consultDB, [_, X^Q, _, notexist(Q) ] ).
resource_relation( consultDB, [_, X^Q, _, notexist(X,Q) ] ).
resource_relation( consultDB, [_, Q, _, unknown(Q) ] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).

resource_relation( consultDBx, [Database, Query, PropSet, AnswerSet] ) :-
	Database : consultDBx( Query, PropSet, AnswerSet ), !.
resource_relation( consultDBx, [_, X^Q, _, set([notexist(X,Q)]) ] ).
resource_relation( consultDBx, [_, Q, _, set([unknown(Q)])] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).


resource_relation( validDBparameter, [Database, Prop ] ) :-
	Database : validDBparameter( Prop ), !.
resource_relation_type( validDBparameter, [database, prop]).


/*----------------------------------------------------------------------
     device
----------------------------------------------------------------------*/

is_resource_type( upnp_dev ).

% note: "devices" is a record containing devices
% HACK 021818
%resource_variable_of_type( devices, record([vcr:upnp_dev, telephone:upnp_dev])).
resource_variable_of_type( devices, record([])).
%	findall( Var:upnp_dev, of_type( Var, upnp_dev ), R ).

% note: device_vcr etc. refers to actual devices ("tokens"), not types 

of_type( device_vcr, upnp_dev ).
of_type( device_telephone, upnp_dev ).
of_type( device_telvcr, upnp_dev ).
of_type( device_telvcrlogin, upnp_dev ).

resource_relation( dev_get, [ Dev, Var, VarVal ] ) :-
	Dev : dev_get( Var, Val ),
	VarVal =.. [ Var, Val ].
resource_relation_type( dev_get, [device, _, _]).

% resource_relation( dev_query, [ Dev, Query, Answer ] ) :-
% 	Dev : dev_query( Query, Answer ).
% resource_relation_type( dev_query, [device, _, answer]).


%DH 21/3-2003 - to allow for parameters in device queries
resource_relation( dev_query, [Dev, Query, set(PropList), Answer ] ) :-
	Dev : dev_query( Query, PropList, Answer ).
resource_relation_type( dev_query, [device, _ , set(prop), answer]).



resource_relation( valid_parameter, [ Dev, Prop] ) :-
	Dev : valid_parameter( Prop ).
resource_relation_type( valid_parameter, [device, prop ]).


resource_operation( dev_set, Dev, [ Var, Val ], Dev ) :-
	Dev : dev_set( Var, Val ).
resource_operation_type( dev_set, device, [ _, _], device ).

resource_operation( dev_do, Dev, [ Action, set(PropList) ], Dev ) :-
	Dev : dev_do( Action, PropList ).
resource_operation_type( dev_do, device, [ action, set(prop) ], device ).

/*----------------------------------------------------------------------
     asr_grammar
----------------------------------------------------------------------*/
is_resource_type( asr_grammar ).

resource_variable_of_type( asr_grammar, asr_grammar ).

resource_of_type( asrg_vcr_english, asr_grammar ).
resource_of_type( asrg_travel_english, asr_grammar ).
resource_of_type( asrg_telvcr_english, asr_grammar ).
resource_of_type( asrg_telvcrlogin_english, asr_grammar ).
resource_of_type( asrg_telvcr_svenska, asr_grammar ).
resource_of_type( asrg_telvcrlogin_svenska, asr_grammar ).


resource_relation( language, [Grammar, L] ):-
        Grammar:language(L).
resource_of_type( language, [asr_grammar, atom] ).

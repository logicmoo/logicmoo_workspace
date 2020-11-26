 
/*************************************************************************

        name: resource_interfaces.pl 
 description: Resource interface type definitions for GoDiS-basic
              and GoDiS-grounding
 
*************************************************************************/

:- module(resource_interfaces, [resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3 ]).

:- multifile resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.

:- discontiguous resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.


:- use_module(library(lists)).

% dummy so resource_operation is defined somewhere

resource_operation( dummy, dummy, dummy, dummy ).

/*----------------------------------------------------------------------
     lexicon
----------------------------------------------------------------------*/

is_resource_type( lexicon ).

resource_variable_of_type( lexicon, lexicon ).

resource_of_type( lexicon_travel_english, lexicon ).
resource_of_type( lexicon_autoroute_english, lexicon ).
resource_of_type( lexicon_travel_svenska, lexicon ).
resource_of_type( lexicon_cellphone_svenska, lexicon ).

resource_relation( input_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : input_form( Phrase, Move ).
resource_relation_type( input_form, [ lexicon, string, move ] ).

resource_relation( output_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : output_form( Phrase, Move ).
resource_relation_type( output_form, [ lexicon, string, move ] ).

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


resource_relation( incompatible, [Domain, P1, P2] ):-
	incompatible( P1, P2, Domain ).
resource_relation_type( incompatible, [domain, prop, prop] ).

% Q depends on Q1
resource_relation( depends, [Domain, Q1, Q] ):-
	Domain : plan( Q1, Plan ),
	member( findout(Q), Plan ).
resource_relation_type( incompatible, [domain, prop, prop] ).


% incomplete - questions can also be goals of plans
resource_relation( question, [Domain, Q] ):-
	Domain : plan( _Task, Plan ),
	member( findout(Q), Plan ).
resource_relation_type( question, [domain, _] ).


resource_relation( proposition, [Domain, P] ):-
	Domain : sort_restr( P ).
resource_relation_type( proposition, [domain, _] ).

resource_relation( short_answer, [Domain, A] ):-
	( Domain : sem_sort( A, top );
	    ( A = yes ; A = no ) ).
resource_relation_type( short_answer, [domain, _] ).

resource_relation( default_question, [Domain, Q] ):-
	Domain : default_question( Q ).
resource_relation_type( default_question, [domain, question] ).

% relevance / aboutness

% all resolving answers are relevant
relevant_answer( Q, A, Domain ):-
	resolves( Q, A, Domain ).



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

% fail

%resolves( Q, fail(Q), _Domain ).

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

% not( P or not P )
% = not P and P = false!!!
resolves( P, unknown(P), _).


% content-question
%resolves( und(_M), yes, _ ).
%resolves( und(_M), no, _ ).



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


resolves( X^PX, notexist(X,PX), _Domain).

% Alt-questions

%full answer; 
resolves( set(AltList), Alt, Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% "no"/"neither"
resolves( set(_AltList), no, _Domain ). 
% "no"/"neither"
resolves( set(AltList), (not set(AltList) ), _Domain ). 



% understanding-question

resolves( und(_DP*P), A, Domain ):-
	resolves( P, A, Domain ).

% HACK!!020411
resolves( und(_DP*_P), yes, _Domain ).
resolves( und(_DP*_P), no, _Domain ).

resolves( und(_DP*set(AltList)), P, Domain ):-
	resolves( set(AltList), P, Domain ).






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

% understanding-question

%combine( und(DP*C), A, P, Domain ):-
%	combine( C, A, P, Domain ).

combine( und(DP*P), P, und(DP*P), Domain ):-
	combine( P, P, P, Domain ).
combine( und(DP*P), not(P), not(und(DP*P)), Domain ):-
	combine( P, not(P), not(P), Domain ).

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

incompatible1( PA, PB, Domain ):-
	PA =.. [P,A],
	PB =.. [P,B],
	Domain:sort_restr(PA),
	Domain:sort_restr(PB),
	A \= B.

/*----------------------------------------------------------------------
     database
----------------------------------------------------------------------*/

is_resource_type( database ).

resource_variable_of_type( database, database ).

resource_of_type( travel, database ).
resource_of_type( autoroute, database ).

resource_relation( consultDB, [Database, Query, PropSet, Answer] ) :-
	Database : consultDB( Query, PropSet, Answer ), !.
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
resource_relation_type( consultDB, [database, prop]).



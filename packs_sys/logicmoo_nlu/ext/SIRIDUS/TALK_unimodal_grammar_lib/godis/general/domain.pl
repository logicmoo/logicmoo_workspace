
%Resource interface file for GoDiS domains. Contains quite a number of
%semantic definitions.


:- multifile is_resource_type/1,resource_relation/2, resource_relation_type/2.
:- discontiguous resource_relation/2, resource_relation_type/2.

is_resource_type(domain).
/*----------------------------------------------------------------------
     domain
----------------------------------------------------------------------*/

resource_relation( relevant, [Domain, Answer, Query ] ) :-
	relevant_answer( Query, Answer, Domain ).
resource_relation_type( relevant_answer, [domain, question, answer] ).

resource_relation( resolves, [Domain, Answer, Query ] ) :-
	resolves( Query, Answer, Domain ).
resource_relation_type( resolves, [domain, question, answer] ).

resource_relation( plan, [Domain, Task,stackset(Plan)] ) :-
	Domain : plan( Task, Plan ).
resource_relation_type( plan, [domain, _,_ ] ).

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

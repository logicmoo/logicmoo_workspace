/*************************************************************************

        name: godis_semantics.pl 
 description: GoDiS-AOD domain resource help predicates for default
              GoDiS semantics:
              - relevant_answer
	      - resolves
	      - dominates
	      - combine
	      - incompatible
     authors: Original code (June 2002) by Staffan Larsson (SL)
              Modifications by SL and David Hjelm (DH)
*************************************************************************/

/************************************************************************

HISTORY, started 050621

* [SL050621] fixed comments

*************************************************************************/


/*----------------------------------------------------------------------
     relevant_answer( +Question, +Answer, +Domain )
----------------------------------------------------------------------*/


% All resolving answers are relevant
relevant_answer( Q, A, Domain ):-
	resolves( Q, A, Domain ).

% YN-question, "maybe" answer: relevant but not resolving
relevant_answer( YNQ, maybe, _Domain ):-
	YNQ \= _X^_PX, % not wh
	YNQ \= set(_AltList). % not alt

% WH question, negative full answer
relevant_answer( X^PX, not(PA), Domain ):-
	PX =.. [P,X],
	PA =.. [P,_],
	Domain:sort_restr( PA ), !.

% WH question, negative short answer
relevant_answer( X^PX, not(A), Domain ):- 
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% WH question, negative underspecific answer
% Example: not(name(anna)) relevant to X^name_to_add(X) if name_to_add isa name
relevant_answer( X^PX, not(TA), Domain ):- 
	\+ var( PX ),
	PX =.. [P,X],
	TA =.. [T,A],
	Domain:isa( P, T ), % this assumes "isa" holds betweem predicates
	PA =.. [P,A],
	Domain:sort_restr( PA ), !.

% Alt-question, full answer; 
relevant_answer( AltList, not(Alt), Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% Alt-question, negative full answer
relevant_answer( set(AltList), (not Alt), Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 



/*----------------------------------------------------------------------
     resolves( +Question, +Answer, +Domain )
----------------------------------------------------------------------*/


% Database entry propositions
resolves( Q, db_entry( _, P ), Domain ):-
	resolves( Q, P, Domain ).
resolves( Q, db_entry( _, _, P ), Domain ):-
	resolves( Q, P, Domain ).

% Yes/no questions, short answers
resolves( P, yes, Domain ) :- Domain:sort_restr(P).
resolves( P, no, Domain ) :- Domain:sort_restr(P).

% Yes/no questions, full  answers
resolves( P, P, Domain ) :- Domain:sort_restr(P).
resolves( P, not(P), Domain ) :- Domain:sort_restr(P).

resolves( P, unknown(P), _).


% WH question, full answer
resolves( X^PX, PA, Domain ):-
	\+ var( PX ),
	PX =.. [P,X],
	PA =.. [P,_],
	Domain:sort_restr( PA ), !.

% Multiple WH-question, full answer [SL 050621]
resolves( X^Y^PXY, PAB, Domain ):-	
	\+ var( PXY ),
	PXY =.. [P,X,Y],
	PAB =.. [P,_,_],
	Domain:sort_restr( PAB ), !.

% WH-question, short answer
resolves( X^PX, A, Domain ):-
	\+ var( PX ),
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% WH-question, underspecific answer
% NB. This is where ontology is used in Q/A-relations
resolves( X^PX, TA, Domain ):- % PA is proposition
	\+ var( PX ),
	PX =.. [P,X],
	TA =.. [T,A],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ), !.

% WH-question, negative full answer
resolves( X^PX, notexist(X,PX), _Domain).

% Alt-question, full answer 
resolves( set(AltList), Alt, Domain ):-
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% Alt-question about issues, full answer
% NB. Issues don't have to be in the list of alternatives
resolves( set(AltList), issue(A), Domain ):-
	member( issue(_), AltList ),
	Domain:sort_restr(issue(A)). 

% Alt-question about actions, full answer;
% Action is in list of alternatives
resolves( set(AltList), action(A), Domain ):-
	member( action(A), AltList ),
	Domain:sort_restr(action(A)).

% Alt-question about actions, full answer;
% Action is dominated by action in list of alternatives
resolves( set(AltList), action(A), Domain ):-
	Domain:sort_restr(action(A)),
	member( action(AltAction), AltList ),
	dominates( Domain, AltAction, A ).

% Alt-question, negative short answer
resolves( set(_AltList), no, _Domain ). 

% Alt-question, negative full answer ("neither")
resolves( set(AltList), (not set(AltList) ), _Domain ). 

% Alt-question, underspecific answer [DH XXXXXX]
% Example: name(anna) resolves set([..., name_to_add(anna)])
% if name_to_add isa name
resolves( set(AltList), TA , Domain ):-
	member(PX,AltList),
	TA =.. [T,A],
	PX =.. [P,_X],
	Domain:isa( P, T ),  % this assumes "isa" holds between predicates
	PA =.. [P,A],
	Domain:sort_restr(PA).

% YN-question concerning understanding (ICM)
resolves( und(_DP*P), A, Domain ):-
	resolves( P, A, Domain ).
% (this is probably not used, since clarification-Q's are not ICM)
resolves( und(_DP*set(AltList)), P, Domain ):-
	resolves( set(AltList), P, Domain ).

% [added SL 050531: but seems wrong so removed]
%resolves( X^PX, not(PA), Domain ):- % PA is proposition
%	PX =.. [P,X],
%	PA =.. [P,_],
%	Domain:sort_restr( PA ), !.


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


/*----------------------------------------------------------------------
   combine( +Q, +A, ?P)
   -- P is the proposition resulting from combining question Q and answer A
----------------------------------------------------------------------*/


% fail
combine( Q, fail(Q), fail(Q), _Domain ).

% Yes/no question, short answer
combine( P, yes, P, Domain ) :- Domain:sort_restr(P).
combine( P, no, not(P), Domain ) :- Domain:sort_restr(P).

% Yes/no question, full answer
combine( P, P, P, Domain ) :- Domain:sort_restr(P).
combine( P, not(P), not(P), Domain ) :- Domain:sort_restr(P).

% WH-question, positive full answer 
combine( X^PX, PA, PA, Domain ):- % PA is proposition
	PA =.. [P,_A],
	PX =.. [P,X],
	Domain:sort_restr( PA ).

% Double WH-question, positive full answer [SL050621]
combine( X^Y^PXY, PAB, PAB, Domain ):- % PA is proposition
	PAB =.. [P,_,_],
	PXY =.. [P,X,Y],
	Domain:sort_restr( PAB ).

% WH-question, negative full answer 
combine( X^PX, not(PA), not(PA), Domain ):- % PA is proposition
	PA =.. [P,_A],
	PX =.. [P,X],
	Domain:sort_restr( PA ).

% WH-question, short answer
combine( X^PX, A, PA, Domain ):- % A is elliptical answer
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% WH-question, negative short answer
combine( X^PX, not(A), not(PA), Domain ):- % A is elliptical answer
	PX =.. [P,X],
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% WH-question, underspecific answer [SL020404]
combine( X^PX, TA, PA, Domain ):- % PA is proposition
	TA =.. [T,A],
	PX =.. [P,X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% WH-question, negative underspecific answer [SL020404]
combine( X^PX, not(TA), not(PA), Domain ):-
	TA =.. [T,A],
	PX =.. [P,X],
	Domain:isa( P, T ),
	PA =.. [P,A],
	Domain:sort_restr( PA ).

% Alt-question, full answer
% NB. Does not allow Q, i.e. AltList to be uninstantiated,
% since there is an infinite amount of alt-questions
% matching a ceratin answer and proposition.
combine( set(AltList), Alt, Alt, Domain ):-
	\+ var(AltList),
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% Alt-question, negative full answer
combine( set(AltList), (not Alt), (not Alt), Domain ):-
	\+ var(AltList),
	member( Alt, AltList ),
	Domain:sort_restr(Alt). 

% Alt-question, negative short answer
combine( set(AltList), no, (not set(AltList)), _Domain ):-
	\+ var(AltList).

% Alt-question, underspecific answer [DH XXXXXX]
combine( set(AltList), TA , PA, Domain ):-
	member(PX,AltList),
	TA =.. [T,A],
	PX =.. [P,_X],
	Domain:isa( P, T ), % this assumes "isa" holds between predicates
	PA =.. [P,A],
	Domain:sort_restr(PA).
		
% YN-question concerning understanding (ICM)
combine( und(DP*P), P, und(DP*P), Domain ):-
	combine( P, P, P, Domain ).
combine( und(DP*P), not(P), not(und(DP*P)), Domain ):-
	combine( P, not(P), not(P), Domain ).

%unused cases
%combine( und(DP*P), yes, und(DP*P), Domain ).
%combine( und(DP*P), no, not(und(DP*P)), Domain ).
%combine( und(DP*set(AltList)), P, und(DP*P), Domain ):-
%	combine( set(AltList), P, P, Domain ).


/*----------------------------------------------------------------------
   incompatible(+P1, +P2)
   -- P1 and P2 are incompatible propositions, i.e. they should not be
      in the same set of propositions. 
----------------------------------------------------------------------*/

% Order is irrelevant
incompatible( P1, P2, Domain ):-
	incompatible1( P1, P2, Domain ),!.
incompatible( P1, P2, Domain ):-
	incompatible1( P2, P1, Domain).

% Any proposition is incompatible with its negation
incompatible1( P, not(P), _ ).

% Domain-dependence
% NB. This is really only valid for feature-like predicates, i.e.
% predicates with whose extension is a singleton set
incompatible1( P2, P1, Domain):-
	Domain : incompatible( P1, P2 ).



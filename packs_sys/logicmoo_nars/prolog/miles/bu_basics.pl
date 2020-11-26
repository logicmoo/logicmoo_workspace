% MODULE bu_basics EXPORTS
:- module( bu_basics, 
	 [ addtolist/1,
           getlist/1,
           subs_build_clause/1,
           msg_build_long_clause/1,
           msg_build_heads/1,
           msg_build_body/1,
           annotate_redundancy/1,
           abs_process_proofs/2,
           abs_build_body/1,
           ident_process_proofs/2,
           ident_build_body/1,
           g1_build_clause/2,
           g1_process_proofs/2,
           idev_build_clause1/1,
           idev_build_clause/2,
	   process_new_literals/2,
           sat_build_clause/3,
           head/3,
	   body/3,
           assumption/3,
	   assert_absorptions/2,
           assert_clause/1,
	   assert_body/1,
	   assert_body_unique/1,
           assert_body_randomly/1,
           cover_assert_assumptions/1,
	   retract_body_literals/1,
           retract_literals/1,
           assert_literals/1,
	   clear_mngr/0,
	   reset_counts/0]).

% IMPORTS
:- use_module(home(div_utils),
              [contains_duplicates/1]).
:- use_module_if_exists(library(basics),[member/2]).
:- use_module_if_exists(library(random),[maybe/0]).

% METAPREDICATES
% none

:- dynamic head/3.     % head( Literal, {old,new}, Counter)
:- dynamic body/3.     % body( Literal, {old,new}, Counter)
                       % the second argument is used to check whether
                       % saturation/idev resulted in a new literal at all.
                       % the third argument is 0 for new literals that 
                       % resulted from sat/idev, and \= 0 for literals that
                       % have been used for sat/idev
:- dynamic assumption/3.

%***********************************************************************
%*	
%* module: bu_utils.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description:	utilities for bottom-up operators
%*		
%* see also:								
%*									
%***********************************************************************

%***********************************************************************
%*									
%* predicate:	process_new_literals/2							
%*									
%* syntax: process_new_literals(+[H:Proof|_],-Flag)
%*									
%* args: H = Lit/M where M is in {new_head,new_body}, or H = []
%*       Proof = [[Lit,N],..,[],...] where N in {head,body} 	
%*									
%* description:	asserts all new heads and bodies of matched clauses
%*              via head/3 and body/3
%*              Flag = 1 if at least one existed
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

process_new_literals([],_).


process_new_literals([ []:_Proof| Rest] , Flag):-
	process_new_literals(Rest,Flag).

process_new_literals([ L/new_body:_Proof| Rest] , Flag):-
	body(L,_,_) ,
	!,
	process_new_literals(Rest,Flag).

process_new_literals([ L/new_head:_Proof| Rest] , Flag):-
	head(L,_,_),
	!,
	process_new_literals(Rest,Flag).

process_new_literals([ (_L/_):Proof| Rest] , Flag):-
	contains_duplicates(Proof), %%eigentlich ein Filter: jedem Literal 
                                    %%im Parent entspricht eines in der Resolvente
	!,
	process_new_literals(Rest,Flag).

process_new_literals([ (L/new_head):Proof| Rest] , 1):-
	!,
	assert( head(L,new,0) ),
	annotate(Proof),
	process_new_literals(Rest,_).

process_new_literals([ (L/new_body):Proof| Rest] , 1):-
	assert( body(L,new,0) ),
	annotate(Proof),
	process_new_literals(Rest,_).

% for backtracking
process_new_literals([ (L/new_body):_Proof| Rest] , Flag):-
	retract( body(L,new,0) ),
	process_new_literals(Rest,Flag).


%***********************************************************************
%*									
%* predicate:	annotate/1							
%*									
%* syntax: annotate(+Proofs)								
%*									
%* args: Proofs = [L1,..,Ln] where Li = [] or Li = [Lit,N] and N in {head,body}
%*									
%* description: increments counter for each head/body literal in Proofs
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

annotate([]).

annotate([ [L,body] | Rest ]):-
	retract( body( L,OldNew,I)),
	J is I + 1,
	assert( body(L,OldNew,J) ),
	annotate(Rest).
annotate([ [L,head] | Rest ]):-
	retract( head( L,OldNew,I)),
	J is I + 1,
	assert( head(L,OldNew,J)) ,
	annotate(Rest).

annotate([ [] | Rest]):-
	annotate(Rest).



%***********************************************************************
%*									
%* predicate:	abs_process_proofs/2							
%*									
%* syntax: abs_process_proofs(+Proofs,-Head)
%*									
%* args: Proofs = [CL1,..,CLn] where CLi is a clause in list notation
%*       Head: a head literal								
%*									
%* description:	returns a head literal from one of the CLi, and retracts
%*              the according body literals body(L,_,_) of CLi (destructive
%*              absorption)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

abs_process_proofs([ [Head:p|_] | MoreHeads ] , NewHead ):-
        body(Head,_,_),
	!,
        abs_process_proofs(MoreHeads, NewHead ).

abs_process_proofs([ [_|Proof] | MoreHeads ] , NewHead ):-
        contains_duplicates(Proof), %%eigentlich ein Filter: jedem Literal 
                                    %%im Parent entspricht eines in der Resolvente
	!,
        abs_process_proofs(MoreHeads, NewHead ).

abs_process_proofs([ [Head:p|Proof] | _MoreHeads ] , Head ):-
        retract_body_literals(Proof).

% For Backtracking
abs_process_proofs([ [_Head:p|Proof] | MoreHeads ] , NewHead ):-
        assert_body(Proof),
        abs_process_proofs(MoreHeads, NewHead ).



%***********************************************************************
%*									
%* predicate:	ident_process_proofs/2						
%*									
%* syntax: ident_process_proofs(+[[H:Proof]|_],-Head)
%*									
%* args: H =	 = Lit/M where M is in {new_head,new_body}, or H = []
%*       Proof = [[Lit,N],..,[],...] where N in {head,body}
%*       Head: a literal
%*									
%* description:	returns a head literal from one of the H:Proof, and retracts 
%*              the according literals from Proof from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

ident_process_proofs([ [(Head/new_head):_] | MoreHeads ] , NewHead ):-
        head(Head,_,_),          %  
	!,
        ident_process_proofs(MoreHeads, NewHead ).

ident_process_proofs([ [(_/_):Proof] | MoreHeads ] , NewHead ):-
        contains_duplicates(Proof), %%eigentlich ein Filter: jedem Literal 
                                    %%im Parent entspricht eines in der Resolvente
	!,
        ident_process_proofs(MoreHeads, NewHead ).

ident_process_proofs([ [ (Head/new_head):Proof] | _MoreHeads ] , Head ):-
        retract_literals(Proof).

% for backtracking
ident_process_proofs([ [ (_Head/new_head):Proof] | MoreHeads ] , NewHead ):-
        assert_literals(Proof),
        % write('new kb'),nl,subsume_mngr:show_heads,subsume_mngr:show_bodies,nl,
        ident_process_proofs( MoreHeads, NewHead).



%***********************************************************************
%*									
%* predicate:	g1_process_proofs/2						
%*									
%* syntax: g1_process_proofs(+[[H:Proof]|_],-Lit)
%*									
%* args: H =	 = Lit/M where M is in {new_head,new_body}, or H = []
%*       Proof = [[Lit,N],..,[],...] where N in {head,body}
%*       Lit:  a literal and its sign
%*									
%* description:	returns the resolution literal from one of the H:Proof, and retracts 
%*              the according literals from Proof from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

g1_process_proofs([[]:_|R],Lit):-
       g1_process_proofs(R,Lit).

g1_process_proofs([ (Head/new_head):_ | MoreHeads ] , Lit ):-
        head(Head,_,_),           
	!,
        g1_process_proofs(MoreHeads, Lit ).


g1_process_proofs([ (Body/new_body):_ | MoreHeads ] , Lit ):-
        body(Body,_,_),            
	!,
        g1_process_proofs(MoreHeads, Lit ).

g1_process_proofs([ (_/_):Proof | MoreHeads ] , Lit ):-
        contains_duplicates(Proof),%%eigentlich ein Filter: jedem Literal 
                                    %%im Parent entspricht eines in der Resolvente
	!,
        g1_process_proofs(MoreHeads, Lit ).

g1_process_proofs([ (Lit/S0):Proof | _MoreHeads ] , Lit:S ):-
        (   S0 = new_head -> S = p ; S = n ),
        retract_literals(Proof).

% for backtracking
g1_process_proofs([ (_/_):Proof | MoreHeads ] , Lit ):-
        assert_literals(Proof),
        % write('new kb'),nl,subsume_mngr:show_heads,subsume_mngr:show_bodies,nl,
        g1_process_proofs( MoreHeads, Lit).


%***********************************************************************
%*									
%* predicate:	assert_absorptions/2							
%*									
%* syntax: assert_assorptions(+[CL|_],-Flag)
%*									
%* args: CL: clause in list notation
%*									
%* description: asserts heads H of all absorbed clauses, if new, as 
%*              body(H,_,_). Flag=1 if at least one existed	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************


assert_absorptions([],_F).
assert_absorptions([ [Head:p|_] |MoreHeads] ,F):-
        body( Head,_OldNew,_Count) ,
        !,
        assert_absorptions(MoreHeads,F).

assert_absorptions([ [Head:p|Proof]|MoreHeads],1 ):-
        assert( body(Head,new,0) ),
        annotate_redundancy(Proof),
        assert_absorptions(MoreHeads, 1).

% For Backtracking
assert_absorptions([ [Head:p|_Proof]|_MoreHeads],_ ):-
        retract( body(Head,new,0) ).


%***********************************************************************
%*									
%* predicate:	annotate_redundancy/1							
%*									
%* syntax: annotate_redundancy(+Proof)
%*									
%* args: Proof: clause in list notation
%*									
%* description:	increments counter for each body literal in Proof
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

annotate_redundancy([]).
annotate_redundancy([ L:_ | More ] ):-
        retract( body(L,OldNew,I) ),
        J is I + 1,
        assert( body(L,OldNew,J)),
        annotate_redundancy(More).


%***********************************************************************
%*									
%* predicate: assert_body_randomly/1
%*									
%* syntax: assert_body_randomly(+Clause_list)
%*									
%* args: +Clause_list ... Clause in list notation
%*									
%* description:	assert body literals in random order
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

assert_body_randomly([]):-!.

assert_body_randomly([H:p | More]):- 
       head(H,_,_),!,
       assert_body_randomly(More).

assert_body_randomly([H:p | More]):- 
       assert(head(H,_,_)),
       assert_body_randomly(More).

assert_body_randomly([L:_ | More]):-
       body(L,_,_),!,
       assert_body_randomly(More).

assert_body_randomly([L:_ | More]):- 
       maybe,
       asserta( body(L,old,0) ),
       assert_body_randomly(More).

assert_body_randomly([L:_ | More]):-
       assertz( body(L,old,0) ),  
       assert_body_randomly(More).

%***********************************************************************
%*									
%* predicate:	addtolist/1							
%*									
%* syntax: addtolist(+ToAdd)								
%*									
%* args: ToAdd .. Id or list of Id's
%*									
%* description: asserts list of Id's	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also: 							
%*									
%***********************************************************************

addtolist([L|IST]) :- 
	retract(id_list(List1)), 
	append(List1,[L|IST],List2),
	assertz(id_list(List2)), !.
addtolist(Id) :- retract(id_list(List1)), assertz(id_list([Id|List1])), !.
addtolist([L|IST]) :- assertz(id_list([L|IST])), !.
addtolist(Id) :- assertz(id_list([Id])), !.


%***********************************************************************
%*									
%* predicate:	getlist/1
%*									
%* syntax: getlist(-ID_list)								
%*									
%* args:								
%*									
%* description:	retracts the list of id's that has been asserted by
%*		addtolist/1							
%* 									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

getlist(List) :- retract(id_list(List)).


%***********************************************************************
%*									
%* predicate: cover_assert_assumptions
%*									
%* syntax: cover_assert_assumptions(+Clause_list)
%*									
%* args: Clause_list .. clause in list representation
%*									
%* description:	asserts for each literal L in Clause_list assumption(L,_,_)
%*              in the kb							
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

cover_assert_assumptions([]).
cover_assert_assumptions([L:_|More]):-
      assertz( assumption(L,_,_) ),
      cover_assert_assumptions(More).



%***********************************************************************
%*									
%* predicate: clear_mngr/0								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	retracts all head/3 and body/3 within the knowledge base
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

clear_mngr:- retractall( head(_,_,_) ), retractall( body(_,_,_) ).


%***********************************************************************
%*									
%* predicate:  retract_body_literals/1
%*									
%* syntax: retract_body_literals(+CL)
%*									
%* args: CL: clause in list notation
%*									
%* description:	retracts body(L,_,_) for each L:_ in CL
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

 retract_body_literals( [ L:_| More]):- 
	  retract( body(L,_,_) ),
	  retract_body_literals(More).
 retract_body_literals( []).


%***********************************************************************
%*									
%* predicate:  retract_literals/1
%*									
%* syntax: retract_literals(+[[Lit,N]|_])
%*									
%* args: N in {head,body}								
%*									
%* description:	retracts head(Lit,_,_)/body(Lit,_,_) for each [Lit,head]/
%*              [Lit,body] in the input							
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

retract_literals([]).
retract_literals([ [L,head] | Rest ]):- 
          retract( head(L,_,_)),!,
          retract_literals(Rest).
retract_literals([ [L,body] | Rest ]):- 
          retract( body(L,_,_)),!,
          retract_literals(Rest).
retract_literals([ [_,_] | Rest ]):-
          retract_literals(Rest).
retract_literals([ [] | Rest ]):-
          retract_literals(Rest).


%***********************************************************************
%*									
%* predicate:  assert_literals/1
%*									
%* syntax: assert_literals(+[[Lit,N]|_])
%*									
%* args: N in {head,body}							
%*									
%* description:	asserts each Lit with [Lit,N] in the input as N(Lit,_,_)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

assert_literals([]).
assert_literals([ [L,head] | Rest ]):- 
          asserta( head(L,_,_)),!,
          assert_literals(Rest).
assert_literals([ [L,body] | Rest ]):- 
          assertz( body(L,_,_)),!,
          assert_literals(Rest).
assert_literals([ [_,_] | Rest ]):-
          assert_literals(Rest).
assert_literals([ [] | Rest ]):-
          assert_literals(Rest).


%***********************************************************************
%*									
%* predicate:  assert_clause/1								
%*									
%* syntax: assert_clause(+CL)								
%*									
%* args: CL .. clause in list representation
%*									
%* description:	asserts positive literals L in CL as head(L,old,0),
%*              negative and redundant literals as body(L,old,0).
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

assert_clause(C):- assert_clause1(C).
assert_clause(_):- clear_mngr,!,fail. %on backtracking

 assert_clause1( []).
 assert_clause1([H|T]):- assert_clause1(H), assert_clause1(T).
 assert_clause1(H:p):- head(H,_,_).
 assert_clause1(H:p):- assertz( head(H,old,0) ).
 assert_clause1(H:S):- member(S,[n,r]), body(H,_,_).
 assert_clause1(H:S):- member(S,[n,r]), assertz( body(H,old,0) ).


%***********************************************************************
%*									
%* predicate:  assert_body/1								
%*									
%* syntax: assert_body(+CL)								
%*									
%* args: CL ... clause body in list representation (only negative and
%*              redundant literals)
%*									
%* description:	asserts each literal L in CL as body(L,old,0)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

 assert_body( []).
 assert_body([H|T]):- assert_body(H), assert_body(T).
 assert_body(H:S):- member(S,[n,r]), assertz( body(H,old,0) ).


%***********************************************************************
%*									
%* predicate:  assert_body_unique/1
%*									
%* syntax: assert_body_unique(+CL)
%*									
%* args: CL ... clause body in list representation (only negative and
%*              redundant literals)
%*									
%* description:	as assert_body/1, but tests whether body(L,_,_) is already
%*              in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

 assert_body_unique( []).
 assert_body_unique([H|T]):- assert_body_unique(H), assert_body_unique(T).
 assert_body_unique(H:S):- member(S,[n,r]), body(H,_,_).
 assert_body_unique(H:S):- member(S,[n,r]), assertz( body(H,old,0) ).



%***********************************************************************
%*									
%* predicate:  reset_counts/0							
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	for each kb-entry head(Lit,_,Count) and body(Lit,_,Count), 
%*              Count is set to 0
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

 reset_counts:- retract( body(L,O,_) ), assertz( body(L,O,0) ),fail.
 reset_counts:- retract( head(L,O,_) ), assertz( head(L,O,0) ),fail.
 reset_counts.

%**********************************************************************
%*									
%* predicate:	subs_build_clause/1							
%*									
%* syntax: subs_build_clause(-CL)
%*									
%* args: CL ... Horn clause in list representation
%*									
%* description:	retracts one entry head(H,_,_) and all entries 
%*              body(L,_,_) and builds clause [H:p,..,L:p,...]
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

subs_build_clause([H:p|Body]):-
	 retract( head(H,_,_) ),
	subs_build_clause( Body),!.
subs_build_clause([L:n|Body]):-
	 retract( body(L,_,_) ),
	 subs_build_clause( Body ).
subs_build_clause([]):- !.


%***********************************************************************
%*									
%* predicate:	sat_build_clause/3							
%*									
%* syntax: sat_build_clause(+H,+B,-CL)
%*									
%* args: H ... head
%*       B ... list of body literals
%*       CL ... Horn clause in list representation
%*									
%* description:	build clause CL = [H:p,...,L:M,....] for each L in B.
%*		M is n, if body(L,_,_) is true, else M is r
%* 									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

sat_build_clause(H,B,[H:p|B1]):-
        sat_build_body(B,B1).
sat_build_body([],[]).
sat_build_body( [L|B],[L:n|B1]):-          % nonredundant literal
        body( L,_,0),
        !,
        sat_build_body(B,B1).
sat_build_body( [L|B], [L:r|B1]):-         % (probably) superfluos literal
        sat_build_body( B,B1).



%***********************************************************************
%*									
%* predicate:	msg_build_long_clause/1							
%*									
%* syntax: msg_build_long_clause(-CL)
%*									
%* args: CL ... a general clause in list representation
%*									
%* description:	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************


msg_build_long_clause( Clause):-
    msg_build_heads( Heads ),
    msg_build_body( Body),
    append( Heads, Body, Clause),!.


%***********************************************************************
%*									
%* predicate:	msg_build_heads/1							
%*									
%* syntax: msg_build_heads(-CL)								
%*									
%* args: CL ... clause in list representation, consisting only of 
%*              positive literals
%*									
%* description:	collects all Literals L such that head(L,_,_) is in kb,
%*              and returns CL = [...,L:p,....]
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

msg_build_heads( [ H:p | More ]):-
    retract( head(H,F,I) ),
    msg_build_heads( More),
    asserta( head(H,F,I) ).
msg_build_heads([]):- !.


%***********************************************************************
%*									
%* predicate:	msg_build_body/1							
%*									
%* syntax: msg_build_body(-CL)								
%*									
%* args: CL ... clause in list representation, consisting only of 
%*              negative and redundant literals
%*									
%* description:	collects all Literals L such that body(L,_,_) is in kb,
%*              and returns CL = [...,L:M,....], M in {n,r}
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

msg_build_body( [ H:Sign | More ]):-
	retract( body(H,F,I) ),
	( I = 0 -> Sign = n ; Sign = r),
	msg_build_body(More),!,
	asserta( body(H,F,I) ).

msg_build_body([]):-!.


%***********************************************************************
%*									
%* predicate:	idev_build_clause1/1							
%*									
%* syntax: idev_build_clause1(-CL)
%*							
%* args: CL .. Horn clause in list representation
%*									
%* description:								
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_build_clause1( [H:p|More] ):-
	retract( head(H,F,0) ),!, % the head is unique !
	idev_build_body(More),
	asserta(head(H,F,0) ).

%***********************************************************************
%*									
%* predicate:	idev_build_body/1							
%*									
%* syntax: idev_build_body(-CL)								
%*							
%* args: CL ... clause in list represenation, only negative and redundant literals
%*									
%* description:	collects all L such that body(L,_,_) is in kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_build_body( [ H:Sign | More ]):-
	retract( body(H,F,I) ),
	( I = 0 -> Sign = n ; Sign = r),
	idev_build_body(More),!,
	asserta( body(H,F,I) ).

idev_build_body([]):-!.



%***********************************************************************
%*									
%* predicate:	idev_build_clause/2							
%*									
%* syntax: idev_build_clause(+H,-CL)
%*							
%* args: CL .. clause in list representation
%*       H ... preferred head of CL
%*									
%* description:	as idev_build_clause1/1, but with preferred head in CL
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_build_clause(PrefHead, [H:p|More]):-
    idev_build_head( PrefHead, H),
    idev_build_body(More),!.

%***********************************************************************
%*									
%* predicate:	idev_build_head/2							
%*									
%* syntax: idev_build_head(+PrefH,-H)
%*							
%* args: PrefH, H .. clause heads
%*									
%* description:	returns H such that head(H,_,_) in kb and PrefH and H
%*		unifiable. If none exists, the first H with head(H,_,_) in kb
%* 		is returned.							
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_build_head(PrefHead,PrefHead):-
    retract( head(PrefHead,F,N) ),!,
    asserta( head(PrefHead,F,N) ).

idev_build_head( _, Head):-
    head( Head,_,_).
    % retract( head(Head,F,N) ),
    % !,
    % asserta( head(H,F,N) )

 


%***********************************************************************
%*									
%* predicate:	ident_build_body/1							
%*									
%* syntax: ident_build_body(-CL)
%*									
%* args: CL ... clause in list notation, contains only negative literals
%*									
%* description:	CL = [...,L:n,...] for each L such that body(L,_,0) in kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

ident_build_body( [ L:n | Rest ]):-
	retract( body( L, _, 0) ) ,
	!,
        ident_build_body( Rest),
        assertz( body( L,old,0) ).    % for backtracking
ident_build_body([]):-!.



%***********************************************************************
%*									
%* predicate:	g1_build_clause/2							
%*									
%* syntax: g1_build_clause(+ResLit,-CL)
%*									
%* args: CL ... Horn clause in list notation
%*       ResLit ... the resolution literal
%*									
%* description:	CL is the second parent clause for the g1-operator
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

g1_build_clause( L:p, [ L:p | Body ]):-
        ident_build_body(Body),!.
g1_build_clause( L:n, [ H:p, L:n | Body ]):- 
        ident_build_body(Body),
	retract( head( H, _, 0) ) ,
	!,
        assertz( head( L,old,0) ).   


%***********************************************************************
%*									
%* predicate:	abs_build_body/1							
%*									
%* syntax: abs_build_body(-CL)								
%*									
%* args: CL .. clause in list representation, contains only negative literals
%*									
%* description:	CL = [...,L:n,....] for each L such that body(L,_,_) in kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************


abs_build_body( [ L:n | Rest ]):-
	retract( body( L, F, I) ) ,
	!,
        abs_build_body( Rest),
        assert( body(L,F,I) ).
abs_build_body([]):-!.

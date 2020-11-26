/*
:- module(solver,
    [is_constraint_functor/1,
    restriction_entailed/2,
    fd_or_num/1,
    reified_equality_solver/3,
    binary_domain/1,
    cstr_var/1,
    neq/2,lt/2,eq/2,gt/2,leq/2,geq/2,
    is_identical/2,
    impose_neg_constraints/3,
    solver_search/1,
    is_clp_functor/1,
    solver_rewrite_constraint/2,
    term_unify/2,
    opposite/2,
    rewrite_restriction/2,
    rewrite_restr_rules/2,
    add_default_domain/1,
    term_equality/2,
    is_number/1
    ]).
*/
:- use_module(reified_unif,[if_unary_substitute/4,inst/1,unify_constr/2]).
:- use_module(library(clpr)).
:- use_module(library(lists)).

neq(A,B):- {A=\=B}.
lt(A,B):- {A<B}.
eq(A,B):- {A=B}.
gt(A,B):- {A>B}.
leq(A,B):- {A=<B}.
geq(A,B):- {A>=B}.

is_identical(A,B):- entailed(A=B).

term_unify(X,Y):- unify_constr(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% used in ics_quant %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rel2clp(<>,neq).
rel2clp(<,lt).
rel2clp(=,eq).
rel2clp(>,gt).
rel2clp(=<,leq).
rel2clp(>=,geq).
rel2clp(clp_constraint,clp_constraint).
rel2clp(st,st). /* MG: This is necessary because in Unfolding we call
this module. Since there might remain some st, we have to recognize
it as a constraint, otherwise the quantification will be wrong */

is_clp_functor(C):- member(C,[<>,<,=,>,=<,>=,clp_constraint,st]),!.

solver_rewrite_constraint(Constraint,Constraint1):-
	Constraint=..[F,Arg1,Arg2],
	rel2clp(F,F1),
	Constraint1=..[F1,Arg1,Arg2].

%%%%%%%%%%%%%%%%%%%%%%%%%% used in sokb_parser %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_constraint_functor(F):-
    rel2clp(_,F).

%%%%%%%%%%%%%%%%%%%%%%%%%%% Used in quantif %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks if the restriction is entailed by a set
% of restrictions.
% It is by far not complete!!!
% Just a collection of simple rules in which entailment
% is easy.

% A restriction is entailed if it was already imposed
restriction_entailed(R,[R1|_]):-
    R == R1, !.
%restriction_entailed(R, [R1|_]):-
%   is_unary(R), is_unary(R1),
%   unary_restriction_entailed(R,R1).

%%%% MG (27 dec 2007) creazione di questo file.
%%%% Per ora tolgo questa regola. Se ci viene in mente un metodo migliore per verificare l'entailment
%%%% nel solver Q, possiamo metterla qui. Ci sarebbe una entailed/1, ma ha bisogno che i vincoli siano
%%%% imposti
%restriction_entailed(X in LowX..HighX,[X1 in Low1 .. High1|_]):-
%    X == X1,
%    leq(Low1,LowX),
%    leq(HighX,High1),!.
restriction_entailed(R,[_|T]):-
    restriction_entailed(R,T).

is_unary(R):-
    term_variables_bag(R,[_]).

rewrite_restriction(R,R).
rewrite_restr_rules(R,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%% Used in reified_unif %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fd_or_num(X) :- compound(X),!, X=rat(_,_). % is it a rational number? (clpq)
fd_or_num(X) :- number(X),!.
%fd_or_num(X) :- var(X),dump([X],_,[]),!.
fd_or_num(X) :-             %MG: it is a constrained variable if it has at least one of the attributes defined in the module clpr.
    var(X),
    clpr:dump([X],_,Store),
    Store \= [].

cstr_var(X) :- var(X),     clpr:dump([X],_,Store),
    Store \= [].

is_number(X) :- compound(X),!, X=rat(_,_). % is it a rational number? (clpq)
is_number(X) :- number(X),!.

term_equality(A,B):-
    fd_or_num(A), fd_or_num(B), !, eq(A,B).
term_equality(A,X):- A=X.

reified_equality_solver(X,Y,B):- entailed(X=Y),!, B=1.
reified_equality_solver(X,Y,B):- entailed(X=\=Y),!, B=0.
reified_equality_solver(X,Y,B):- {X=Y},B=1.
reified_equality_solver(X,Y,B):- {X=\=Y},B=0.

binary_domain(_).

% MarcoG: This version opens several choice points.
% Future work: improve it (possibly, as in the fd_solver version)
% NOTE: This versione does not check if the various constraints
% in disjunctions overlap or not. This is ok when we are inverting
% interval constraints (eg A<X<B, that becomes A>=X \/ X>= B),
% but in general this could give repeated solutions.
% A possibility is the following (buggy: fails too often, probably due to st(...))
/* Intended for SICStus 3
impose_neg_constraints(X,RL,Y):-
    choose_constraint(R,RL,Pos,[]),
    if_unary_substitute(X,R,Y,T),
    once(opposite(T,Opp)),
    call_constraints([Opp|Pos]).

call_constraints([]).
call_constraints([H|T]):-
    functor(H,F,_), %write(H),
    ((F=st;F=clp_constraint) -> call(H) ;
    (is_clp_functor(F)
        ->  {H}
        ;   call(H)
    )),
    call_constraints(T).

choose_constraint(X,[X],L,L):-!.
choose_constraint(X,[X,_|_],L,L).
choose_constraint(X,[Y|R],[Y|Lout],Lin):-
    choose_constraint(X,R,Lout,Lin).*/

impose_neg_constraints(X,RL,Y):-
    member(R,RL),
    if_unary_substitute(X,R,Y,T),
    once(opposite(T,Opp)),
    functor(Opp,F,_),
    (is_clp_functor(F)
        ->  {Opp}
        ;   call(Opp)
    ).

opposite({T},Opp):-
    opposite(T,Opp).
opposite(A=B,A=\=B).
opposite(A=\=B,A=B).
opposite(A<B,A>=B).
opposite(A=<B,A>B).
opposite(A>B,A=<B).
opposite(A>=B,A<B).

opposite(eq(A,B),neq(A,B)).
opposite(neq(A,B),eq(A,B)).
opposite(lt(A,B),geq(A,B)).
opposite(leq(A,B),gt(A,B)).
opposite(gt(A,B),leq(A,B)).
opposite(geq(A,B),lt(A,B)).

solver_search(L):- sumterm(L,X), %write(prima(LT1)),
    bb_inf(L,X,_,LT1,0.01),
    guided_labeling(L,LT1,X).

guided_labeling([],[],_).
guided_labeling([H|T],[H1|T1],F):-
    {H=H1},
    guided_labeling(T,T1,F).
guided_labeling([H|T],[H1|_],F):-
    {H>=H1+1},
    bb_inf([H|T],F,_,Sol,0.01),
    guided_labeling([H|T],Sol,F).
guided_labeling([H|T],[H1|_],F):-
    {H+1=<H1},
    bb_inf([H|T],F,_,Sol,0.01),
    guided_labeling([H|T],Sol,F).



sumterm([X],X):-!.
sumterm([X|T],X+Y):-
    sumterm(T,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%% used in sciff.pl %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_default_domain(T):-
    leq(T,1000),
    geq(T,-1000).

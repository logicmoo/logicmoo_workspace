:- use_module(prolog_version).
:- use_module(library(clpfd)).
:- use_module(library(terms)).
:- use_module(reified_unif,[if_unary_substitute/4,inst/1]).
:- (is_dialect(swi)
    -> compile(swi_clpfd)
    ;  compile(sicstus_clpfd)).

neq(A,B):- A#\=B.
lt(A,B):- A#<B.
eq(A,B):- A=B.
gt(A,B):- A#>B.
leq(A,B):- A#=<B.
geq(A,B):- A#>=B.

is_identical(A,X):- A==X.

term_unify(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% used in ics_quant %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rel2clp(<>,#\=).
rel2clp(<,#<).
rel2clp(=,#=).
rel2clp(>,#>).
rel2clp(=<,#=<).
rel2clp(>=,#>=).
rel2clp(clp_constraint,clp_constraint).
rel2clp(st,st). /* MG: This is necessary because in Unfolding we call
this module. Since there might remain some st, we have to recognize
it as a constraint, otherwise the quantification will be wrong */

is_clp_functor(C):- rel2clp(C,_).

solver_rewrite_constraint(Constraint,Constraint1):-
	Constraint=..[F,Arg1,Arg2],
	rel2clp(F,F1),
	Constraint1=..[F1,Arg1,Arg2].

% used in sokb_parser
is_constraint_functor(F):-
	atom_concat(#,_,F).

%%% Used in quantif
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
restriction_entailed(X in LowX..HighX,[X1 in Low1 .. High1|_]):-
    X == X1,
    leq_infty(Low1,LowX),
    leq_infty(HighX,High1),!.
restriction_entailed(R,[_|T]):-
    restriction_entailed(R,T).

is_unary(R):-
    term_variables_bag(R,[_]).

leq_infty(inf,_):-!.
leq_infty(_,sup):-!.
leq_infty(A,B):-
    number(A), number(B), A=<B.

rewrite_restriction(R,Rew):-
    (is_unary(R)
      ->    rewrite_restr_rules(R,Rew)
      ; R=Rew).

% Others can be added
rewrite_restr_rules((X #< Y),(X in inf..Y1)):-
    var(X), ground(Y),!,
    Y1 is Y-1.
rewrite_restr_rules((X #> Y),(X in Y1..sup)):-
    var(X), ground(Y),!,
    Y1 is Y+1.
rewrite_restr_rules((X #< Y),Rew):-
    var(Y), ground(X),!,
    rewrite_restr_rules((Y #> X),Rew).
rewrite_restr_rules((X #> Y),Rew):-
    var(Y), ground(X),!,
    rewrite_restr_rules((Y #< X),Rew).
rewrite_restr_rules(R,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%% Used in reified_unif %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fd_or_num(X) :- fd_var(X),!.
fd_or_num(X) :- number(X).

cstr_var(X) :- clpfd:fd_var(X).

is_number(X) :- number(X).

term_equality(A,A).

binary_domain(B):- B in 0..1.

% MarcoG 10 may 2005
% changed because or some reason the choice point gets lost.
% I upgrade to a better version (with reified constraints).
impose_neg_constraints(X,R,Y):-
    impose_neg_constraints(X,R,Y,0).

impose_neg_constraints(_,[],_,1):- !. % SICStus non ottimizza questo cut
impose_neg_constraints(X,[R|T],Y,B):-
    negate(X,R,Y,B1),
    (B1 == 0 -> B=0
    ;   solver_and(B,B1,B2),
        impose_neg_constraints(X,T,Y,B2)
    ).


%impose_neg_constraints(X,[R|_],Y):-
%    negate(X,R,Y).
%impose_neg_constraints(X,[_|T],Y):-
%    impose_neg_constraints(X,T,Y).
%negate(X,R,Y):- negate(X,R,Y,0).

negate(X,R,Y,B):-
    if_unary_substitute(X,R,Y,T),
    reification(T,B),
    inst(B).

solver_search(LT1):- labeling([ffc],LT1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% used in sciff.pl %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_default_domain(T):-
    (fd_var(T) -> true ; T in -1000..1000).

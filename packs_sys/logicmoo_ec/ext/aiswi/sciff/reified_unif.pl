%----------------------------------------------------------
% Reified unification constraint
% considering variable quantification
% in CHR
% Version 2: considers domain restrictions
% Marco Gavanelli
% 14 October 2003
%----------------------------------------------------------

:-module(reified_unif,[reif_unify/3, make_choice/0, inst/1,if_unary_substitute/4,unify_constr/2]).


:- use_module(library(chr)).
:- use_module(quantif).
%:- use_module(library(clpb)).
%:- use_module(library(clpfd)).
:- use_module(library(terms)).
:- use_module(library(lists)).
%:- ensure_loaded(solver). or ensure_loaded(load_solver).
:- use_module(solver).
%handler reified_unification.
:- ensure_loaded(my_chr_extensions).
%:- chr_option(debug,off).
%:- chr_option(optimize,full).

:- chr_constraint reif_unify/3, unify_constr/2, not_unify_constr/2, inst/1, make_choice/0.

% Reified Unification
% reif_unify(?Term1,?Term2,?Boolean)
% If Boolean=1, then Term1 and Term2 should unify
% If Boolean=0, then Term1 and Term2 should not unify



%yes @ reif_unify(T1,T2,B) <=> T1 == T2 | B=1.
yes @ reif_unify(T,T,B) <=> B=1.
% The following rule improves very much the efficiency, but MUST be after rule 'yes'
not_unif_syntactical @ reif_unify(T1,T2,B) <=> ?=(T1,T2), \+fd_or_num(T1), \+fd_or_num(T1) | B=0. 
reif_comb @ reif_unify(T1,T2,B) <=> nonvar(T1), nonvar(T2), 
    functor(T1,F1,N1), functor(T2,F2,N2) | 
    (F1=F2, N1=N2
      -> reif_unify_args(N1,T1,T2,B)
      ;  B=0).
    
unify_c @ reif_unify(T1,T2,1) <=> unify_constr(T1,T2).
not_unify_c @ reif_unify(T1,T2,0) <=> not_unify_constr(T1,T2).
% fd_or_num should be after the '&' (I don't know why, but otherwise it does not work).
% Remember that checking cstr_var is enough to infer that is existential only for the clpfd solver 
reif_unif_fd1 @ reif_unify(X,Y,B) <=> var(X), nonvar(Y), get_quant(X,existsf), cstr_var(X), fd_or_num(Y) | reified_equality_solver(X,Y,B).
reif_unif_fd2 @ reif_unify(X,Y,B) <=> var(Y), nonvar(X), get_quant(Y,existsf), fd_or_num(X), cstr_var(Y) | reified_equality_solver(X,Y,B).
reif_unif_fd3 @ reif_unify(X,Y,B) <=> var(Y), var(X), get_quant(Y,existsf), get_quant(X,existsf), cstr_var(X), cstr_var(Y) | reified_equality_solver(X,Y,B). 
reif_unif_nonflag_exists @ reif_unify(X,Y,B) <=> var(X), get_quant(X,exists), not_unifies_exists(Y) | B=0.
reif_unif_nonflag_exists @ reif_unify(Y,X,B) <=> var(X), get_quant(X,exists), not_unifies_exists(Y) | B=0.

not_unifies_exists(Y):- nonvar(Y),!.
not_unifies_exists(Y):- get_quant(Y,Q), Q\=forallf, Q\=forall.

% Mettere una regola che da successo se una variabile forall viene unificata con qualcosa!

reif_unify_forall_norestr @ reif_unify(X,Y,B) <=>
    is_universal(X), get_restrictions(X,[]) |
    Y=X, =(B,1).

/*
reif_unify_forall_1 @
    reif_unify(X,Y,B)
    <=>
    is_universal(X)
    |
    (X=Y -> B=1;B=0).


reif_unify_forall_2 @
    reif_unify(X,Y,B)
    <=>
    is_universal(Y)
    |
    (X=Y -> B=1;B=0).
*/

reif_unify_forall_norestr @ reif_unify(Y,X,B) <=>
    is_universal(X), get_restrictions(X,[]) |
    Y=X, =(B,1).


% MarcoG 28 sep 2006:
% if a universal variable (with quant restrictions) 
% is unified with a ground term, try the unification.
% MarcoG: Modified 1 dec 06
% This is complete if the quantifier restrictions are unary
% (do not involve other variables). If they involve exist.
% variables, subsequent propagations might insert choice
% points, so the cut will drop some success branches
reif_unify_forall_restr_ground @ reif_unify(Y,X,B) <=>
    is_universal(X), ground(Y), get_restrictions(X,R),
    term_variables(R,[Var]), Var == X
     |
    (Y=X -> =(B,1) ; =(B,0)).

reif_unify_forall_restr_ground @ reif_unify(X,Y,B) <=>
    is_universal(X), ground(Y), get_restrictions(X,R),
    term_variables(R,[Var]), Var == X
     |
    (Y=X -> =(B,1) ; =(B,0)).

reif_unify(_,_,B) ==> binary_domain(B).

:- chr_constraint and_reif_unify(?natural,?natural,?natural).
% This is a normal and(A,B,C), meaning that C = A /\ B,
% but we assume that the booleans A and B occur in the boolean of reif_unify.
% If we have an AND, and one of the arguments is 0, the output is 0 and we
% don't care about the other argument, so we can remove the constraints
% associated with it (in particular, reif_unify).
and_reif_unify(0,B,C), reif_unify(_,_,B) <=> C=0.
and_reif_unify(A,0,C), reif_unify(_,_,A) <=> C=0.
and_reif_unify(0,_,C) <=> C=0.
and_reif_unify(_,0,C) <=> C=0.
and_reif_unify(A,B,1) <=> A=1, B=1.
and_reif_unify(1,B,C) <=> B=C.
and_reif_unify(A,1,C) <=> A=C.

reif_unify_args(0,_,_,1):- !.
reif_unify_args(N,X,Y,B):-
    is_identical(B,0), 
    !, not_unify_args(N,X,Y).
reif_unify_args(N,X,Y,B) :-
    arg(N,X,Xn),
    arg(N,Y,Yn),
    reif_unify(Xn,Yn,Bn), %write(Bn), nl,
    (Bn == 0
        ->  B = 0 %, write(N), write(' ')
        ;   and_reif_unify(Bn,Bn1,B),
            %B #<=> (Bn #/\ Bn1),
            N1 is N-1,
            reif_unify_args(N1,X,Y,Bn1)
    ).

yes @ unify_constr(T1,T2) <=> T1 == T2 | true.
% The following rule improves very much the efficiency, but MUST be after rule 'yes'
not_unif_syntactical @ unify_constr(T1,T2) <=> ?=(T1,T2), \+fd_or_num(T1), \+fd_or_num(T2) | false. 
unify_constr(X,Y) <=> 
    nonvar(X), nonvar(Y)| 
    functor(X,F,N), functor(Y,F,N),
    unify_args(N,X,Y).

unif_symm @ unify_constr(X,Y) <=> nonvar(X), var(Y) | unify_constr(Y,X).



% Regola strana per risolvere il problema degli NE nel body...
% Probabilmente questa regola dovrebbe essere inserita anche nell'unificazione
% a basso livello (nella quantif.pl).
nonflagged_exists1 @ unify_constr(X,Y) <=> var(X), var(Y), % inutile: fallisce gia` get_quant se sono nonvar
    get_quant(X,QX), get_quant(Y,exists), QX \= forallf | false.
nonflagged_exists2 @ unify_constr(Y,X) <=> var(X), var(Y), 
    get_quant(X,QX), get_quant(Y,exists), QX \= forallf | false.
nonflagged_exists3 @ unify_constr(X,Y) <=> var(X), 
    nonvar(Y), get_quant(X,exists) | false.

unify_var_nonvar @ unify_constr(X,Y) <=> var(X), get_quant(X,Q), Q \==exists,  nonvar(Y) | term_equality(X,Y).

%% Aggiunto da Marco A:

univ_exist_1 @ unify_constr(X,Y) <=>
        is_universal(X),
        get_restrictions(X,[]),is_existential(Y) | term_equality(X,Y).
univ_exist_2 @ unify_constr(Y,X) <=>  is_universal(X),
    get_restrictions(X,[]),is_existential(Y) | term_equality(X,Y).

existsf_unif_existsf @ unify_constr(X,Y) <=>
is_existential(X),is_existential(Y) | term_equality(X,Y).

                %cannot happen (see rule unif_symm).
%unify_constr(Y,X) <=> var(X), nonvar(Y), get_quant(X,exists) | false.

%unify_constr(X,Y) <=> X=Y.

% Added MarcoG 9 may 2005
% from D8, page 165:
% a universaly quantified variabe can unify with an existentially quant. var.
% The result is that the quantifier restrictions become constraints.
% This is already achieved when the two variables are unified, so we only
% have to unify the variabls.
univ_exist_3 @ unify_constr(X,Y) <=>
        is_universal(X),
        get_quant(Y,existsf)
        | term_equality(X,Y).
univ_exist_3 @ unify_constr(Y,X) <=>
        is_universal(X),
        get_quant(Y,existsf)
        | term_equality(X,Y).

unify_args(0,_,_):-!.
unify_args(N,X,Y):-
    arg(N,X,Xn),
    arg(N,Y,Yn),
    unify_constr(Xn,Yn),
    N1 is N-1,
    unify_args(N1,X,Y).

no @ not_unify_constr(T1,T2) <=> T1 == T2 | false.
% The following rule improves very much the efficiency, but MUST be after rule 'no'
not_unif_syntactical @ not_unify_constr(T1,T2) <=> ?=(T1,T2) | true. 
nu1 @ not_unify_constr(X,Y) <=> nonvar(X), nonvar(Y),
    functor(X,F,N), functor(Y,F,N) |
    not_unify_args(N,X,Y).
nu2 @ not_unify_constr(X,Y) <=> nonvar(X), nonvar(Y) | X \= Y.
%nu3 @ not_unify_constr(X,Y) <=> X==Y | false.  Mi sembra identica alla "no"
% nu4 is occur check: we skip it (for now)
nu5a @ not_unify_constr(X,Y) <=> var(Y), nonvar(X) | not_unify_constr(Y,X).
% Qui dovrei anche considerare se X e` free
nu5b @ not_unify_constr(X,Y) <=> var(Y), get_quant(Y,forall), 
    var(X), get_quant(X,QX), QX \= forall, QX \=forallf | not_unify_constr(Y,X).
% Stessa cosa per le flagged
nu5bf @ not_unify_constr(X,Y) <=> var(Y), get_quant(Y,forallf), 
    var(X), get_quant(X,QX), QX \= forall, QX \=forallf | not_unify_constr(Y,X).
%nu6a @ not_unify_constr(X,_) <=> var(X), get_quant(X,forall), get_restrictions(X,[]) | false.
%nu6af @ not_unify_constr(X,_) <=> var(X), get_quant(X,forallf), get_restrictions(X,[]) | false.
%not_unify_constr(_,X) <=> var(X), get_quant(X,forall) | false.

nu6b @ not_unify_constr(X,Y) <=> var(X), 
    is_universal(X), existsf_or_ground(Y)  |
    get_restrictions(X,Rx),
    impose_neg_constraints(X,Rx,Y).




% Checks that unification of the existentially quantified vars
% is unfeasible.
% Notice that if the solver is not complete, then the \+ may
% fail even if the unification is impossible!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modified by Federico Chesani - 20060314 1530
% New Version:
nu6c @ not_unify_constr(X,Y) <=> var(X), var(Y),
    is_universal(X), is_universal(Y),
    get_restrictions(X,_Rx), get_restrictions(Y,_Ry) |
    \+( (existsf(E), E=X, E=Y) ).
% End New Version
% Old Version
/*
nu6c @ not_unify_constr(X,Y) <=> var(X), var(Y),
    is_universal(X), is_universal(Y),
    get_restrictions(X,Rx), get_restrictions(Y,Ry) |
    \+( (silly, existsf(E), E=X, E=Y) ).
*/
% End Old Version
% End Modification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nu_clpfd1 @ not_unify_constr(X,Y) <=> cstr_var(X), get_quant(X,existsf), cstr_var(Y), get_quant(Y,existsf) | neq(X,Y).
nu_clpfd1 @ not_unify_constr(X,Y) <=> cstr_var(X), get_quant(X,existsf), is_number(Y) | neq(X,Y).
%nu_clpfd2 @ not_unify_constr(X,Y) <=> cstr_var(X), number(Y) | X #\=Y.
nu_clpfd3 @ not_unify_constr(X,Y) <=> cstr_var(X), nonvar(Y), \+number(Y) |true.

%not_unify_args(0,_,_). Must fail
%not_unify_args(N,X,Y):-
%   arg(N,X,Xn),
%   arg(N,Y,Yn),
%   (not_unify_constr(Xn,Yn) ;
%   N1 is N-1,
%   not_unify_args(N1,X,Y)).

%not_unify_args(N,X,Y):-
%    reif_unify_args(N,X,Y,0).

not_unify_args(_,X,Y):-
    get_disunif_variables(X,Y,Lx,Ly),
    disunif_list(Lx,Ly).

% Creates in Lx and Ly the pairs of terms that should disunify (at least one
% pair should disunify). Each pair contains at least one variable.
get_disunif_variables(X,Y,Lx,Ly):-
    (var(X);var(Y)),!, Lx=[X], Ly=[Y]. 
get_disunif_variables(X,Y,Lx,Ly):-
    (ground(X),ground(Y)
     ; ?=(X,Y)
    ),
    !,
    X \= Y,
    Lx=[],Ly=[].
get_disunif_variables(X,Y,Lx,Ly):-
    X =.. [F|ArgX],
    Y =.. [F|ArgY],
    get_disunif_variables_args(ArgX,ArgY,Lx,Ly).
get_disunif_variables_args([],_,[],[]):-!. % Dovrebbero avere lo stesso numero di argomenti
get_disunif_variables_args([X|ArgX],[Y|ArgY],Lx,Ly):-
    (get_disunif_variables(X,Y,Lx1,Ly1) ; Lx1=[], Ly1=[]),!,
    get_disunif_variables_args(ArgX,ArgY,Lx2,Ly2),
    append(Lx1,Lx2,Lx),
    append(Ly1,Ly2,Ly).

disunif_list([X],[Y]):- !,not_unify_constr(X,Y).
disunif_list([X1,X2|Tx],[Y1,Y2|Ty]):-
    reif_unify(X1,Y1,B1),
    (B1 == 0 -> !
        ;   (B1 == 1 -> fail % undo the unification X1=Y1 and take next clause
            ;   !,disunif_list_constr(B1,[X2|Tx],[Y2|Ty])
            )
    ).
% X1 and Y1 necessarily unify: continue with the other  arguments
disunif_list([_X1,X2|Tx],[_Y1,Y2|Ty]):- 
    disunif_list([X2|Tx],[Y2|Ty]).

:- chr_constraint disunif_list_constr(?natural,?any,?any).
disunif_list_constr(0,_,_) <=> true.
disunif_list_constr(1,Lx,Ly) <=> disunif_list(Lx,Ly).



existsf_or_ground(Y):- ground(Y),!.
existsf_or_ground(Y):-
    get_quant(Y,existsf).



% if_unary_substitute(X,R,Y,T)
% If R is not a unary constraint with only variable X fails.
% Otherwise, it substitutes all the occurrences of X in R
% with Y, and returns the constraint in T

% MarcoG, 9 may 2005
% I extend it in order to deal with abductive EC.
% If all the other variables are existentially quantified,
% then it is still sensible to impose the opposite constraint.
if_unary_substitute(X,R,Y,T):- X == R,!, Y=T.
%if_unary_substitute(_,R,_,_):- var(R),!, fail.
if_unary_substitute(_,R,_,T):- var(R),get_quant(R,existsf), !, T=R.
if_unary_substitute(_,R,_,_):- var(R),get_quant(R,Q), 
    (Q=forall; Q=forallf ; Q= exists), !, 
    write('*** Unimplemented feature: disunification of '),
    write(Q), write(' with constrained forallf ***'), nl,
    fail.
if_unary_substitute(_,R,_,T):- ground(R),!, T=R.
% MG: if it is a st/1, remove it, otherwise the subsequente #<=> will raise an error
if_unary_substitute(X,st(R),Y,T):- !, if_unary_substitute(X,R,Y,T).
if_unary_substitute(X,R,Y,T):-
    functor(R,F,N), functor(T,F,N),
    if_unary_substitute_arg(N,X,R,Y,T).

if_unary_substitute_arg(0,_,_,_,_).
if_unary_substitute_arg(N,X,R,Y,T):-
    N>0, 
    arg(N,R,Rn),
    arg(N,T,Tn),
    N1 is N-1,
    if_unary_substitute(X,Rn,Y,Tn),
    if_unary_substitute_arg(N1,X,R,Y,T).
    
    
inst(X) <=> ground(X) | true.


grounding_inst @
make_choice \ inst(B) <=>
    var(B) |
    (B=1 ; B=0).
grounding_reif_unify @
make_choice, reif_unify(_,_,B) ==>
    var(B) |
    (B=1 ; B=0).

/*make_choice :-
    findall_constraints(inst(_),L),
    get_bool_inst(L,LT1,Tail),
    findall_constraints(reif_unify(_,_,_),L2),
    get_bool_reif(L2,Tail,[]),
    label_bools(LT1).*/

get_bool_inst([],X,X).
get_bool_inst([inst(T)|R],[T|RT],X):-
    get_bool_inst(R,RT,X).

get_bool_reif([],X,X).
get_bool_reif([reif_unify(_,_,T)|R],[T|RT],X):-
    get_bool_reif(R,RT,X).

label_bools([]).
label_bools([1|L]):-
    label_bools(L).
label_bools([0|L]):-
    label_bools(L).

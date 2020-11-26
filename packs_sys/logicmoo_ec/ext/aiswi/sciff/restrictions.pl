%QUESTO DEVE DIVENTARE IL MODULO CHE GESTISCEL'ATTRIBUTO RESTRICTIONS

%-------------------------------------
% Marco Gavanelli
% 3 October 2003
%-------------------------------------

% This module defines an attribute that tells if a variable
% is quantified existentially or universally.

% It also applies Quantifier Restrictions.

% The possible quantifications are:
% exists (existentially quantified variable whose scope is a single implication)
% existsf (existentially quantified variable whose scope is the whole tuple)
% forall (universally quantified variable whose scope is a single implication)
% forallf (universally quantified variable whose scope is the whole tuple)

% The quantifier restrictions are imposed through the predicate st/1 (such that)
% They can be any predicate: be warned that they will be called if all the
% involved variables are quantified (existsf). Typically, you may want to impose
% quantifier restrictions that have the same syntax as clpfd constraints.

% Suggested use:
% 1. impose the quantification
% 2. impose the quantifier restrictions.
% (should also work the other way around, but probably less efficiently
% or with less complete inferences).

% Example, to impose that X and Y are universally quantified, and we
% have a quantifier restriction X<Y:
% ?- forall(X), forall(Y), st(X#<Y).
% forall(X),
% forall(Y),
% st(X,X#<Y),
% st(Y,X#<Y) ? 
% yes

% If all the variables in a quantifer restriction become quantified existsf
% the quantifier restriction becomes a constraint. E.g.:
% ?- forall(X), st(X#>0), existsf(Y), X=Y.
% Y = X,
% existsf(X),
% X in 1..sup,
% st(X,X#>0) ? 
%
% As you see, the program is not very smart in removing quantifier
% restrictions that have become constraints. This should be only an issue
% of efficiency.

% The program tries to make some (not very smart) check of entailment among
% quantifier restrictions to avoid imposing redundant restrictions. E.g.:
% ?- forall(X), st(X in 1..sup), st(X in 3..10).
% forall(X),
% st(X,X in 1..sup) ? 
% yes
%
% Currently, it considers only constraint "in". E.g.:
% ?- forall(X), st(X #>0), st(X in 3..10).
% forall(X),
% st(X,X#>0),
% st(X,X in 3..10) ? 
% yes
%
% and it is order dependent:
% ?- forall(X), st(X in 3..10), st(X in inf..50).
% forall(X),
% st(X,X in 3..10),
% st(X,X in inf..50) ? 
% yes

:- module(restrictions,
    [
     set_restriction/1,
     set_restriction/2,
     get_restrictions/2,
     set_restriction_list/1,
     set_restriction_list/2,
     st/1, st/2]).

:- use_module(library(chr)).
%:- use_module(library(atts)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(terms)).
%:- use_module(domains).
%:-use_module(sets).
%:- writeln('CARICO REIFIED_UNIF DA QUANTIF').
:-use_module(reified_unif).
%:- writeln('CARICato REIFIED_UNIF DA QUANTIF').
%:- ensure_loaded(load_solver).
%:- writeln('CARICO solver DA QUANTIF').
:- use_module(solver).
%:- writeln('CARICato solver DA QUANTIF').
:- use_module(prolog_version).
:- (is_dialect(swi) -> use_module(swi_specific) ; true).
:- use_module(quant).

:- attribute(restrictions/1).

% SICStus Unification handler
verify_attributes(Var, Other, Goals) :-
%        (get_atts(Var, quant(Da))
%          ->    quant_handler(Var,Other,GoalsQuant,Da)
%          ; GoalsQuant=[]),
        (get_atts(Var, restrictions(Ra))
          ->    restrictions_handler(Var,Other,GoalsRestr,Ra)
          ; restrictions_handler(Var,Other,GoalsRestr,[])),
        Goals=GoalsRestr.
%        append(GoalsQuant,GoalsRestr,Goals).

restrictions_handler(Var,Other,Goals,RL):-
    nonvar(Other),!,
    Goals = [set_restriction_list(RL,Var)].

restrictions_handler(Var,Other,GoalsRestr,RL):-
    var(Other),
    %( (is_quantified(Var,existsf) ; is_quantified(Other,existsf) )
    %  ->   Goals1 = [turn_to_constraints(RL)]
    %  ;    Goals1 = []),
    get_restrictions(Other,OtherRes),
    append(OtherRes,RL,AllRes),
    %put_atts(Var,restrictions([])), Non indispensabile:
    %   potrebbero rimanere delle restrizioni sulle variabili existsf
    GoalsRestr=[set_restriction_list(AllRes,Var)].

% SWI Unification handler
attr_unify_hook(AttValue, Other):-
    (AttValue = restrictions(RL) -> true ; write('*** ERROR IN UNIFICATION OF RESTRICTIONS ***'), nl),
    (var(Other)
        ->  get_restrictions(Other,OtherRes),
            append(OtherRes,RL,AllRes),
            set_restriction_list(AllRes,Other)
        ;   set_restriction_list(RL,Other)
    ).

% This predicate is invoked by SICStus when the computation terminates
% to print the constraint store. We use it to print the quantification
% and the restrictions
attribute_goal(Var, T) :-     % interpretation as goal
    (get_quant(Var,Q)
      ->    Tq =.. [Q,Var]
      ;     true
    ),
    (get_atts(Var, restrictions(R))
      ->    put_st(R,List,Var),
            list_conj(List,Rc),
            (var(Tq) -> T=Rc
                ;   T =..[Q,Var,R]
            )
        ;   nonvar(Tq), T=Tq    % Se entrambi var, fallisci e non stampi niente
    ).
% Queste servono perche' altrimenti la attribute_goal (che serve a visualizzare
% il risultato) si rifiuta di mostrare un termine che non e` callable
forallf(_,_).
existsf(_,_).
forall(_,_).
exists(_,_).

list_conj([X],X):- !.
list_conj([X,Y|T],(X,Tc)):-
    list_conj([Y|T],Tc).

put_st([],[],_).
put_st([H|T],[st(V,H)|T1],V):-
    put_st(T,T1,V).


get_restrictions(X,Res):-
%   var(X),
				get_atts(X, restrictions(Res1)),
        !,
        Res=Res1.
get_restrictions(_,[]).

/*
% Adds a domain restriction to the variable
set_restriction(_,R):- var(R),!,write('Unbound Restriction Error'),fail.
set_restriction(_V,Res):-
    is_term_quantified(Res,existsf),!,
    call(Res).
set_restriction(V,R):-
    get_restrictions(V,RL),!,
    rewrite_restriction(R,Res),
    (restriction_entailed(Res,RL)
      ->    true
      ; put_atts(V,restrictions([Res|RL]))).
% No restriction has been imposed yet
set_restriction(V,Res):-
    put_atts(V,restrictions([Res])).
*/
                
% Adds a domain restriction to the variable
set_restriction(_,R):- var(R),!,write('Unbound Restriction Error'),fail.
set_restriction(_V,Res):-
%   var(V),
    is_term_quantified(Res,existsf),!,
    call(Res).
% MarcoG: 10 may 2005
% If V has become ground (it usedto be universal, but unified with
% existential may become ground), then we accept it.
% If the restrictions were all existential, they were checked by the previous
% clause. Otherwise, the restriction will continue to insist on the other
% (universally quant) variables it involves
set_restriction(V,_):-
    ground(V),!.
set_restriction(V,Res):-
%   var(V),
    get_restrictions(V,[]),!,
    put_atts(V,restrictions([Res])).
set_restriction(V,R):-
%   var(V),
    get_restrictions(V,RL),
    rewrite_restriction(R,Res),
    (restriction_entailed(Res,RL)
      ->    true
      ; put_atts(V,restrictions([Res|RL]))).
% No restriction has been imposed yet

% Imposes a list of restrictions on the variables V
set_restriction_list([],_).
set_restriction_list([H|T],V):-
    set_restriction(V,H),
    set_restriction_list(T,V).

% Imposes a list of restrictions on all the variables
% they involve.
set_restriction_list([]).
set_restriction_list([H|T]):-
    set_restriction(H),
    set_restriction_list(T).

% Adds the restriction to all the variables appearing in R
set_restriction(R):-
    term_variables_bag(R,Vars),
    multivar_restr1(Vars,R).

% Syntactic sugar: "such that"
st(V,Res):-
    set_restriction(V,Res).
st(Res):-
    set_restriction(Res).

multivar_restr1([],R):-
    !,
    call(R).
multivar_restr1(Vars,R):-
    multivar_restr(Vars,R).

multivar_restr([],_).
multivar_restr([H|T],R):-
    set_restriction(H,R),
    multivar_restr(T,R).



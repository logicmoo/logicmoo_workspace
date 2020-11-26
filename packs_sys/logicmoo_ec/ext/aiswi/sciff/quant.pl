% MODULO CHE GESTISCE L'ATTRIBUTO QUANT


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

:- module(quant,
    [quant/2,
     set_term_quantification/2,
     get_quant/2,
     set_quant/2,
     is_term_quantified/2,
     is_unquantified/1,
     forall/1, forallf/1, exists/1, existsf/1,
     is_universal/1,
     is_existential/1,
     print_quant/0, print_quant/1          %FOR THE DEBUGGER
    ]).

:- use_module(library(chr)).
:- use_module(prolog_version).
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
:- (is_dialect(swi) -> use_module(swi_specific) ; true).
:- use_module(sciff_options).

:- attribute(quant/1). %%% Needed by SICStus
% can be forall, exists (or free) or 
% forallf (forall flagged), existsf (exists flagged)

% Syntactic Sugar
% Let's skip one passage ....
forall(X):- put_atts(X, quant(forall)). %quant(X,forall).
forallf(X):- put_atts(X, quant(forallf)). %quant(X,forallf).
exists(X):- put_atts(X, quant(exists)). %quant(X,exists).
existsf(X):- put_atts(X, quant(existsf)), %quant(X,existsf),
    get_restrictions(X,Res),
    set_restriction_list(Res,X).


% Ver 1.1: Unification of quantifiers is given by the following rules
% unify_quant(+Q1,+Q2,-Qout)
unify_quant(X,X,X):- !.
unify_quant(forall,Y,Y):- !.
unify_quant(Y,forall,Y):- !.

unify_quant(forallf,Y,Y):- !.
unify_quant(Y,forallf,Y):- !.
unify_quant(existsf,_,existsf):- !.
unify_quant(_,existsf,existsf):- !.

% SICStus unification handler
verify_attributes(Var, Other, Goals) :-
        (get_atts(Var, quant(Da))
          ->    quant_handler(Var,Other,Goals,Da)
          ; Goals=[]).
%        (get_atts(Var, restrictions(Ra))
%          ->    restrictions_handler(Var,Other,GoalsRestr,Ra)
%          ; restrictions_handler(Var,Other,GoalsRestr,[])),
%        append(GoalsQuant,GoalsRestr,Goals).

quant_handler(_Var,Other,Goals,Da):-
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, quant(Db)) -> %   has a quantification?
                    unify_quant(Da,Db,Dc),
                    put_atts(Other,quant(Dc)),
                    Goals = []          % NON FA NIENTE!
                ;   Goals = [],
                    put_atts(Other, quant(Da))% rescue intersection
                )
            ;   update_term_quantification(Other,Da),
                Goals = []
            ).

% SWI unification handler
attr_unify_hook(AttValue, Other):-
%quant_handler(_Var,Other,Goals,Da):-
    AttValue = quant(Da),
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, quant(Db)) -> %   has a quantification?
                    unify_quant(Da,Db,Dc),
                    put_atts(Other,quant(Dc))
                ;   put_atts(Other, quant(Da))% rescue intersection
            )
            ;   update_term_quantification(Other,Da)
        ).

% This predicate is invoked by SICStus when the computation terminates
% to print the constraint store. We use it to print the quantification
% and the restrictions
attribute_goal(Var, T) :-     % interpretation as goal
    (get_atts(Var, quant(Q)), get_option(print_quant,on)
      ->    T =.. [Q,Var]
      ;     false             % if no attribute, fail (print nothing)
    ).


% Version for SWI
attribute_goals(Var, Out, Rest):-
    (get_atts(Var, quant(Q)), get_option(print_quant,on)
      ->    T =.. [Q,Var], [T|Rest]=Out
      ;     Out=Rest             % if no attribute, print nothing
    ).




%% MG 9 aug 09
%% The goal_expansion version was used in SICStus before adding the compatibility with SWI.
%% It could probably work, but there is probably some problem with modules.
%% we should probably remove module quantif and have only quant.pl and restrictions.pl
% goal_expansion(get_quant(X, Dom),_,_,get_atts(X, quant(Dom)),[]).
%% I restore the previous version, without goal_expansion
get_quant(X, Dom) :-
        get_atts(X, quant(Dom)).
% Reads or sets the quantification for a variable
/*quant(X, Dom) :- slightly slower
        var(Dom),
        var(X),!,
        get_atts(X, quant(Dom)).
quant(X,_Dom):- nonvar(X),!.
quant(X, Value) :-
    put_atts(X, quant(Value)).
*/
% set_quant(X,Dom): same as quant(X,Dom), but assumes that Dom is nonvar.
set_quant(X, Dom) :-
    (var(X)
      ->    put_atts(X, quant(Dom))
      ; true).
quant(X, Dom) :-
    (var(X)
      ->    (var(Dom)
             ->   get_atts(X, quant(Dom))
             ;    put_atts(X, quant(Dom))
            )
      ; true).

% update_term_quantiication(+Term,+Quantification),
% Updates the quantification for all the variables
% in a term. The difference wrt set_term_quantification/2
% is that set_... gives the new quantification
% to all the variables, while update_... uses a priority:
% existsf has higher priority than forallf
% Note that this problem come from not-allowed programs!
update_term_quantification(T,Q) :-
    var(T), !, update_quant(T,Q).
update_term_quantification(T,_) :-
    ground(T), !.
update_term_quantification([H|T],Q) :-!,
    update_term_quantification(H,Q),
    update_term_quantification(T,Q).
update_term_quantification(T,Q) :-
    T =.. [_|Args],
    update_term_quantification(Args,Q).

update_quant(T,Q):-
    (get_quant(T,Qold)
      ->    unify_quant(Q,Qold,Qnew)
      ;     Q=Qnew),
    set_quant(T,Qnew).


% Sets the quantification for all the variables
% in a term
set_term_quantification(T,Q) :-
    var(T), !, set_quant(T,Q).
set_term_quantification(T,_) :-
    atom(T), !.
set_term_quantification([H|T],Q) :-!,
    set_term_quantification(H,Q),
    set_term_quantification(T,Q).
set_term_quantification(T,Q) :-
    T =.. [_|Args],
    set_term_quantification(Args,Q).

% Checks if all the variables in Term are
% quantified Quant
%is_term_quantified(?Term,+Quant)
is_term_quantified(Term,Quant):-
    var(Term),!,
    get_quant(Term,Quant).
is_term_quantified(Term,Quant):-
    functor(Term,_,N),
    is_quantified_arg(N,Term,Quant).

is_quantified_arg(0,_,_):- !.
is_quantified_arg(N,Term,Quant):-
    arg(N,Term,Xn),
    is_term_quantified(Xn,Quant),
    N1 is N-1,
    is_quantified_arg(N1,Term,Quant).

% Checks if all the variables in Term are
% unquantified
%is_unquantified(?Term)
is_unquantified(Term):-
    var(Term),!,
    \+ get_quant(Term,_).
is_unquantified(Term):-
    functor(Term,_,N),
    is_unquantified_arg(N,Term).

is_unquantified_arg(0,_):- !.
is_unquantified_arg(N,Term):-
    arg(N,Term,Xn),
    is_unquantified(Xn),
    N1 is N-1,
    is_unquantified_arg(N1,Term).

/*is_universal(X):- this version is slightly slower
    var(X), 
    (get_quant(X,forall) 
      ->    true 
      ;     get_quant(X,forallf)).*/

is_universal(X):-
    var(X),
    get_quant(X,Q),
    (Q=forall ; Q=forallf),!.

/*is_existential(X):- this version is slightly slower 
    var(X), 
    (get_quant(X,exists) 
      ->    true 
      ;     get_quant(X,existsf)).*/

is_existential(X):-
    var(X),
    get_quant(X,Q),
    (Q=exists ; Q=existsf),!.

%MG: 9 aug 09: Questo non ricordo perche' c'e`.
% Puo` darsi che nella stampa del termine sotto SICStus lui trovi clp_constraint
% e si rifiuti di stamparlo se non e` callable, per cui deve essere definito come
% predicato.
clp_constraint(A):-
    call(A).

% QUESTO NON FUNZIONA, PER CUI L'HO TOLTO.
%% quantif=[113,117,97,110,116,105,102]
%debugger_command_hook(unknown([113,117,97,110,116,105,102],Warning),Actions):-
%    Actions=true,
%    print_goal.
%
%debugger_command_hook(unknown(Line,Warning),Actions):-
%    write(unknown(Line,Warning)), nl, write('ancora'),
%    execution_state([show(Show),goal(_:G)]),
%        execution_state([goal(G)]), nl,
%    write('current goal: '), write(G).
%
%%debugger_command_hook(unknown([0'N|ArgCodes],_), Actions) :-
%%        Actions = true, % don't change the action variables
%%                    % [] would mean [print,ask] :-).
%%    name_current_subterm(ArgCodes).
%
%print_goal:-
%    execution_state(break_level(BL)),
%    write(break(BL)),
%    execution_state(break_level(BL),user:goal(G)),
%    write('current goal: '), write(G).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%
% La seconda e` un po' piu` utile. Volevo estendere il debugger di Sicstus per
% visualizzare anche la quantificazione delle variabili e le quantifier
% restrictions. Non sono riuscito a fare quello che volevo, ma ho ottenuto
% un'approssimazione. Si usa cosi`:
% durante il trace (non il chr_trace!) hai un goal che puo` essere
% 
%          Call: modulo:predicato(A,B,C)
%
% e tu vuoi sapere come sono quantificate A, B e C. Allora fai un "b" (break
% level) e Sicstus ti permette di inserire un goal. Il goal da eseguire e`
% 
%         print_quant(modulo).
%
% o, se il modulo non c'e`, basta print_quant. Ho dovuto usare questa sintassi
% assurda per via del sistema dei moduli, che con i metapredicati diventa una
% pena. Lui dovrebbe stamparti la quantificazione di tutte le variabili che
% compaiono nel goal. A questo punto esci dal break level (^D in linux o ^Z in
% windows) e continui dov'eri.

print_quant:-
    print_quant(user).

print_quant(M):-
    execution_state(break_level(BL)),
    BL1 is BL-1,
    execution_state(break_level(BL1),M:goal(G)),
    print_quantified(G).

print_quantified(G):-
    var(G), get_quant(G,Q),!, write(Q), write('('), write(G), write(')'),
    restrictions:get_restrictions(G,R), write(R), nl.
print_quantified(G):-
    var(G),!, write('unquantified('), write(G), write(')\n').
print_quantified(G):-
    ground(G),!.
print_quantified(G):-
    functor(G,suspension,_),!.
print_quantified(G):-
    G =.. [_,X|Par],
    print_quantified(X),
    print_quantified(Par).

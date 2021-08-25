/* -*- Mode: Prolog -*- */

:- module(cl_transform,
          [
           make_variable_arity_predicates_unique/2
          ]).

%% make_variable_arity_predicates_unique(Text,Text2)
%
% e.g. if there are sentences for part_of(a,b) and part_of(a,b,c)
% then this will translate these to unique named part_of2 and part_of3
% (required for conversion to p9)
make_variable_arity_predicates_unique(Text,Text2) :-
        collect_preds(Text,Preds),
        solutions(Functor,
                  (   member(Functor/ArityA,Preds),
                      member(Functor/ArityB,Preds),
                      ArityA\=ArityB),
                  Functors),
        mvapu(Text,Text2,Functors).


collect_preds([],[]) :- !.
collect_preds([X|L],Preds) :-
        !,
        collect_preds(X,Preds1),
        collect_preds(L,Preds2),
        append(Preds1,Preds2,Preds).

collect_preds(X,[F/Arity|Preds]) :-
        compound(X),
        X =.. [F|Args],
        \+ reserved(F),
        !,
        length(Args,Arity),
        collect_preds(F,Preds1),
        collect_preds(Args,Preds2),
        append(Preds1,Preds2,Preds).

collect_preds(X,Preds) :-
        compound(X),
        !,
        X =.. [F|Args],
        collect_preds(F,Preds1),
        collect_preds(Args,Preds2),
        append(Preds1,Preds2,Preds).

collect_preds(_,[]) :- !.

mvapu([],[],_) :- !.
mvapu([X|L],[X2|L2],Fs) :-
        !,
        mvapu(X,X2,Fs),
        mvapu(L,L2,Fs).
mvapu(X,X2,Fs) :-
        compound(X),
        X =.. [F|Args],       % cheat and treat forall/2 etc as a pred
        member(F,Fs),
        !,
        length(Args,Arity),
        atom_number(Suffix,Arity),
        atom_concat(F,Suffix,F2),
        mvapu(Args,Args2,Fs),
        X2 =.. [F2|Args2].
mvapu(X,X2,Fs) :-
        compound(X),
        X =.. [F|Args],       % cheat and treat forall/2 etc as a pred
        mvapu(Args,Args2,Fs),
        X2 =.. [F|Args2].
mvapu(X,X,_) :- !.
        

reserved(forall).
reserved(exists).
reserved(and).
reserved(or).
reserved(not).


%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).
        



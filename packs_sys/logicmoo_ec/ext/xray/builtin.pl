%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  7/11/95   File: builtin.pl                   %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  7/11/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog builtin.pl                                                %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% List of builtin predicates that can appear in clause bodies.
%%% No extra arguments are added for ancestor goals or depth-first
%%% iterative-deepening search.  Also, if a clause body is
%%% composed entirely of builtin goals, the head is not saved
%%% as an ancestor for use in reduction or pruning.
%%% This list can be added to as required.

builtin(T) :-
	nonvar(T),
        functor(T,F,N),
        builtin(F,N).

builtin(!,0).
builtin(true,0).
builtin(fail,0).
builtin(succeed,0).
builtin(trace,0).
builtin(atom,1).
builtin(integer,1).
builtin(number,1).
builtin(atomic,1).
builtin(constant,1).
builtin(functor,3).
builtin(arg,3).
builtin(var,1).
builtin(nonvar,1).
builtin(call,1).
builtin(=,2).
builtin(\=,2).
builtin(==,2).
builtin(\==,2).
builtin(>,2).
builtin(<,2).
builtin(>=,2).
builtin(=<,2).
builtin(is,2).
builtin(display,1).
builtin(write,1).
builtin(nl,0).
builtin(infer_by,_).
builtin(write_proved,_).
builtin(search,_).
builtin(search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(inc_ncalls,0).
% --- compatibility predicates
builtin(justification,_).
builtin(compatible,_).
builtin(model_initialization,_).
% --- variable handling predicates
builtin(herbrand,1).
% hooks handling predicates
builtin(hook1,1).
builtin(hook2,1).
% --- lemma handling predicates
builtin(lemmatize,_).
builtin(dynamic_lemma,_).
builtin(static_lemma,_).
% --- misc
builtin(\+,1).

% --- special purpose predicates
builtin(P,_) :-
	builtin_predicate(P).
builtin_predicate(P) :-
	name(P,L),
        (name('_pl',L1);
	 name('_db',L1)
        ),
        append(_,L1,L).

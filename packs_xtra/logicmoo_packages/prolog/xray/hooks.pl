%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 20/06/96   File: hooks.pl                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 20/06/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog hooks.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(body_hooks_flag/0).
:- dynamic(pred_hooks_flag/0).

%%% ----------------------------------------------------------------------
%%% HOOKS CONFIGURATION

hook_configuration :-
	nl,write('HOOK CONFIGURATION:'),nl,nl,
	
	write("body hook handling     "),
	write(' = '),
	(body_hooks_flag -> write(on) ; write(off)),
	nl,

	write("predicate hook handling"),
	write(' = '),
	(pred_hooks_flag -> write(on) ; write(off)),
	nl.

%%% Hook handling is turned off by no_hook_handling

no_hook_handling :- no_body_hooks, no_pred_hooks.

%%% Body hooks are turned on by body_hooks.
%%% off by no_body_hooks.

body_hooks :-                          % enable body hooks
        retract(body_hooks_flag),
        fail.
body_hooks :-
        assert(body_hooks_flag).

no_body_hooks :-                       % disable body hooks
        retract(body_hooks_flag),
        fail.
no_body_hooks.

%%% Predicate hooks are turned on by pred_hooks.
%%% off by no_pred_hooks.

pred_hooks :-                          % enable predicate hooks
        retract(pred_hooks_flag),
        fail.
pred_hooks :-
        assert(pred_hooks_flag).

no_pred_hooks :-                       % disable predicate hooks
        retract(pred_hooks_flag),
        fail.
no_pred_hooks.

%%% SETTINGS for HOOKS HANDLING
%%%

:- compile(hooks_config).

%%% ----------------------------------------------------------------------
%%% BODY HOOKS

add_body_hooks((Head :- Body),(Head :- Body2)) :-
	body_hooks_flag,
	!,
	(bhook1_p(Head :- Body) ->
	    conjoin((bhook1(Head :- Body)),Body,Body1);
        %true ->
	    Body1=Body),
	(bhook2_p(Head :- Body) ->
	    conjoin(Body1,(bhook2(Head :- Body)),Body2);
        %true ->
	    Body2=Body1).
add_body_hooks((Head :- Body),(Head :- Body)).

%%% COMPILE-TIME conditions for body hook insertion

bhook1_p(Head :- Body) :-
	true.
bhook2_p(Head :- Body) :-
	true.

%%% RUN-TIME predicates for body hooks

bhook1(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b1:(P)),nl,nl;
        %true ->
	     true.
bhook2(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b2:(P)),nl,nl;
        %true ->
	     true.

%%% ----------------------------------------------------------------------
%%% BODY HOOKS

%%% COMPILE-TIME conditions for PREDICATE hook insertion

phook_tests(P,N,TestsA,Proc,ProcP) :-
	pred_hooks_flag,
	!,
	
	phook1_tests(P,N,Tests1),
	conjoin(Tests1,TestsA,Tests1A),
	
	phook2_tests(P,N,Tests2),
	conjoin(Tests1A,Tests2,Tests1A2),

	phook3_tests(P,N,Tests3),
	conjoin(Proc,Tests3,Proc3),

	conjoin(Tests1A2,Proc3,ProcP).
phook_tests(_,_,TestsA,Proc,ProcP) :-
	conjoin(TestsA,Proc,ProcP).

phook1_p(P,N) :-
	true.
phook2_p(P,N) :-
	true.
phook3_p(P,N) :-
	true.

%%% COMPILE-TIME predicates PREDICATE hook insertion

phook1_tests(P,N,Result) :-
	phook1_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p1:P),nl,fail),
	Result = (Head :- Body).
phook1_tests(_,_,true).

phook2_tests(P,N,Result) :-
	phook2_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p2:P),nl,fail),
	Result = (Head :- Body).
phook2_tests(_,_,true).

phook3_tests(P,N,Result) :-
	phook3_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p3:P),nl,fail),
	Result = (Head :- Body).
phook3_tests(_,_,true).

head(P,N,Head) :-
	P == query ->
                Head = query;
	%true ->
		functor(Head,P,N).

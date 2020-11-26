%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 25/03/96   File: debug.pl                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 25/03/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog debug.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(hooks).

:- body_hooks.

bhook1_p(Head :- Body) :-
	Body = (nl,write(_),nl,fail) ->
	     /* eliminates predicate hooks */
	     false;
	functor(Head,query,_) ->
	     false;
	%true ->
	     true.
bhook1(Head :- Body) :-
	functor(Head,gamma,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(trial:gamma(Arg1,Arg2)),nl;
        functor(Head,alpha,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(trial:alpha(Arg1,Arg2)),nl;
        Head = _ ->
	     Head =.. [Pred|_],
	     nl,write(trial:(Pred)),nl;
        %true ->
	     true.

bhook2_p(Head :- Body) :-
	bhook1_p(Head :- Body).
bhook2(Head :- Body) :-
	functor(Head,gamma,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(success:gamma(Arg1,Arg2)),nl;
        functor(Head,alpha,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(success:alpha(Arg1,Arg2)),nl;
        Head = _ ->
	     Head =.. [Pred|_],
	     nl,write(success:(Pred)),nl;
        %true ->
	     true.

:- pred_hooks.

phook1_p(P,N) :-
	P == query ->
                false;
	%true ->
		true.
phook2_p(P,N) :-
	phook1_p(P,N).
phook3_p(P,N) :-
	phook1_p(P,N).


phook1_tests(P,N,Result) :-
	phook1_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(enter*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(enter*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(enter*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook1_tests(_,_,true).

phook2_tests(P,N,Result) :-
	phook2_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(end_of_tests*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(end_of_tests*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(end_of_tests*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook2_tests(_,_,true).

phook3_tests(P,N,Result) :-
	phook3_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(failure*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(failure*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(failure*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook3_tests(_,_,true).

head3(P,N,Head,Head3) :-
	P == query ->
                Head = query;
	%true ->
		N3 is N - 3,
                functor(Head3,P,N3),
                Head3 =.. [P|Args3],
                append(Args3,[_,_,_],Args),
                Head =.. [P|Args].

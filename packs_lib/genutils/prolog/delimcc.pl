:- module(delimcc, [reset/2, p_shift/2, p_reset/3, pr_shift/2, pr_reset/3, ccshell/0]).
/** <module> Three alternative interfaces to delimited continuations

This module builds on the interface provided by reset/3 and shift/1 to
provide higher level facilities, including multiple prompts
(p_reset/3 and p_shift/2) and a more functional style shift operator with
automatic reinstallation of the prompt in the signal handler with pr_reset/3.

reset/2 and p_reset/2 both return a value of type =|cont(A)|= to describe
the status of the computation, where A is the type of the term that was
thrown by shift/1 or p_shift/2.
pr_reset/3 expects the shifted term to contain a handler predicate, which is 
invoked immediately with the continuation as a unary predicate and is expected
to produce a result. Hence, pr_reset/3 never produces a =|cont(_)|= term.
==
cont(A) ---> done; susp(A,pred)
handler(A) == pred(pred(-A),-A).
==

None of these shift produce continuations that reinstall the control context
that was created by the original reset. Hence, if the continuation is expecting
to capture more continuations, it should be called inside another reset. pr_reset/3
does, however, resintall the context before calling the continuation handler,
so the continuation can be called inside the handler, but if it is instead returned
to the wider program context, the context should again be created before calling
the continuation. There are other ways of handling the removal and replacing
of contexts, as described by Shan (2004).

[1] Chung-chieh Shan. Shift to control. In Proceedings of the 5th workshop on Scheme 
and Functional Programming, pages 99–107, 2004.
*/

:- use_module(library(typedef)).
:- use_module(library(lambdaki)).

:- set_prolog_flag(generate_debug_info, false).

:- type cont(A) ---> done; susp(A,pred).

:- meta_predicate reset(0,-).

%% reset(+G:pred, -C:cont(_)) is det.
%  Calls goal G as in reset/3, but combines the result into a single
%  algebraic data type =|cont(_)|=.
reset(G,S) :- reset(G,B,C), continue(C,B,S).

continue(0,_,done) :- !.
continue(Cont,Sig,susp(Sig,Cont)).

% --------------------------------------------------
% Multiprompt control

%% p_reset(Pr:prompt(A), P:pred, -C:cont(A)) is det.
%
%  Execution context for catching shifts directed to the given
%  prompt. If a p_shift/2 targets another prompt, the signal is passed
%  up, taking care to reinstate this prompt inside the continuation
%  that the outer reset will receive.

:- meta_predicate p_reset(+,0,-).

:- if((current_prolog_flag(version, VER), VER =< 70511)).

p_reset(Prompt, Goal, Result) :-
   reset(Goal, Ball, Cont),
   p_cont(Cont, Ball, Prompt, Result).

p_cont(0, _, _, done) :- !.
p_cont(Cont, Prompt-Signal, Prompt, susp(Signal, Cont)) :- !.
p_cont(Cont, Prompt1-Signal1, Prompt, Result) :-
   shift(Prompt1-Signal1),
   p_reset(Prompt, Cont, Result).

:- else.

p_reset(Prompt, Goal, Result) :-
   reset(Goal, Prompt-Signal, Cont),
   (Cont==0 -> Result=done; Result=susp(Signal, Cont)).

:- endif.

%% p_shift(Pr:prompt(A), S:A) is det.
%  Send the term S to the inner-most p_reset/3 with a matching prompt.
p_shift(Prompt, Signal) :- shift(Prompt-Signal).

% ---------------------------------------------------
% Functional style multiprompt

:- type handler(A) == pred(pred(-A),-A).

%% pr_reset(+Pr:prompt(handler(A)), +P:pred(-A), -X:A) is det.
%
%  Call generative unary predicate P in a context delimited by the given 
%  prompt. The value produced by P is returned in X. If pr_shift/2 is used
%  inside P, the continuation is captured as a unary predicate that
%  would have produced the result X, and passed to the binary handler
%  predicate given to pr_shift/2, which should then return the final
%  return value X. The prompt is reinstated before calling the handler.
:- meta_predicate pr_reset(+,1,-).
pr_reset(Prompt, Pred, Result) :-
   p_reset(Prompt, call(Pred, X), Status),
   pr_cont(Status, Prompt, X, Result).

%% pr_cont(+S:cont(handler(A)), +Pr:prompt(handler(A)), X:A, Y:A).
pr_cont(done, _, X, X).
pr_cont(susp(Handler, K), Prompt, X, Result) :-
   pr_reset(Prompt, call(Handler, delimcc:pr_reset(Prompt, \X^K)), Result).

%% pr_shift(Pr:prompt(handler(A)), +H:handler(A)) is det.
%
%  Send the given continuation handler to the innermost reset with
%  a matching prompt. The handler will be called immediately with
%  the captured continuation as a unary predicate. The handler will 
%  be called with the prompt reinstated around it. The continuation 
%  does not install the prompt.
:- meta_predicate pr_shift(+,2).
pr_shift(Prompt, Handler) :- shift(Prompt-Handler).

:- module_transparent ccshell/0.
ccshell :-
   '$toplevel':read_expanded_query(1, Query, Bindings),
   (   Query == end_of_file
   ->  print_message(query, query(eof))
   ;   '$toplevel':'$execute'(Query, Bindings),
		 ccshell
   ).


((startAll/get_why_uu(UU))==>why_startAll(UU)).


a==>b. a. a. \+ a.
:- mpred_test(b).
\+ a.
:- mpred_test(\+ b).


:- dynamic(bc_q/1).
:- dynamic(bc_p/1).

:- (ain((bc_q(N) <- bc_p(N)))).
:- listing(bc_q/1).

bc_p(a).
bc_p(b).
:- listing(bc_p/1).

%:- mpred_trace_exec.

:- mpred_test(bc_p(b)).

%= nothing cached ?
:- listing(bc_q/1).

:- mpred_test(\+ clause_u(bc_q(_),true)).

:- mpred_test((call_u(bc_q(b)))).

%= something cached
:- listing(bc_q/1).
:- mpred_test( clause_u(bc_q(_),true)).

:- mpred_test(fwc).


:- dynamic(meta_argtypes/1).
:- dynamic(most/1).

%:- expects_dialect(pfc).

meta_argtypes(most(ftAssertable)).

% BWD chaining
most((Q <- P))/mpred_literal(Q) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
most(P==>Q)/nonvar(Q) ==> (((P ==> most(Q)))).

% NEG chaining
most(~Q)/mpred_positive_literal(Q)  ==>  (( \+ Q ) ==> ~ Q ).

% POS chaining 1
most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> (  ( ( \+R /Test , (\+ ~ Q)) ==> Q )).

% POS chaining 2
most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> ( ((R/( \+(R=Q), Test)) ==> (\+ Q))).

% POS chaining 1+2
% most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> (  ( ( \+R /Test ) ==> Q ) ,((R/( \+(R=Q), Test)) ==> (\+ Q))).

% most(Q) ==> if_missing(Q,Q).

%(most(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, \+ R/Test) => Q).
%(most(P=>Q)/nonvar(Q)) ==> (P => most(Q)).


:-dynamic((a/1,b/1,c/1)).

a(X) ==> c(X).
most(c(X) ==> ~b(X)) .
a(1).


%:- listing([a/1,b/1,c/1,(==>)/2,most/1,ppt,nt,bt]).

:- mpred_test(~b(1)).


(default_01a(P)/mpred_literal(P))  ==>  (~( ~P) ==> P).

(default_01a((P ==> Q))/mpred_literal(Q)) ==> (P, \+( ~Q) ==> Q).

%:- set_prolog_flag(gc,false).

%

% birds fly by default_01a.
==> default_01a((bird(X) ==> fly(X))).

% here''s one way to do an type of hierarchy.
% zisa = subclass.

zisa(C1,C2) ==>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 ==> P2).

==> zisa(canary,bird).
==> zisa(penguin,bird).

% penguins do not fly.
penguin(X) ==> ( ~fly(X)).

%:- mpred_trace_exec.
% chilly is a penguin.
==> penguin(chilly).


% tweety is a canary.
==> canary(tweety).

:- mpred_test(penguin(chilly)).

:- mpred_test(~fly(chilly)).

:- mpred_test(fly(tweety)).

:- listing(fly).
:- listing(~).

:- mpred_notrace_exec.


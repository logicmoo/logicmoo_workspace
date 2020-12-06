%%
%%  sitcalc.pl:  Prolog Implementation of the Concurrent, Temporal
%%               Situation Calculus with Natural Actions.
%%
%%  Author:  Ryan Kelly (rfk)
%%
%%  Date Created:  28/07/05
%%
%%    This implementation is a slight modification of the work
%%    of Reiter ("Natural Actions, Concurrency and Continuous
%%    Time in the Situation Calculus"), implemented in SWI prolog.
%%    In this case, the temporal component is attached to situations
%%    instead of actions.  So do(A,S) becomes do(A,T,S), poss(A,S)
%%    becomes poss(A,T,S), etc.  I hypothesise that the resulting
%%    languages are equivalently expressive in terms of legal situations,
%%    but make no attempt at this stage to prove it.
%%
%%    The worlds modelled using this framework must conform to the
%%    following structural rules:
%%
%%       * All actions are instantaneous.
%%
%%       * All actions are either "natural actions" or performed
%%         by an agent.  Natural actions are those for which the
%%         predicate natural(A) is true.
%%
%%       * For actions which are not natural, the first argument of
%%         the term must indicate the agent which performs that action.
%%
%%       * There is a unique initial situation denoted by the atom s0.
%%
%%       * Concurrently occuring actions are represented as lists of
%%         primitive action terms.
%%
%%  To implement a domain axiomatisation using this framework, the following
%%  tasks must be completed:
%%
%%       * specify the primitive actions of the world using prim_action/1.
%%
%%       * specify which actions are natural using natural/1.
%%
%%       * specify the primitive fluents in the world using prim_fluent/1.
%%
%%       * specify the agents in the system using agent/1.
%%
%%       * specify the possibility axioms for primitive actions using poss/3.
%%
%%       * specify conflicting sets of concurrent actions using conflicts/3.
%%
%%       * specify the successor state axioms in terms of predicates for
%%         each fluent.
%%
%%       * specify the initial conditions using fluent predicates with the
%%         situation term set to s0.
%%


%%
%%  prim_action(A):  define a primitive action
%%
%%  This predicate specifies the terms which represent primitive actions
%%  in the domain.  The following below are examples of both an agent-initiated
%%  and a natural "no-op" action.  As the action as no effect, successor
%%  state axioms are not necessary.
%%
prim_action(noop(A)) :-
    agent(A).
prim_action(noop).

%%
%%  natural(A):  specify natural actions
%%
%%  For each natural action, this predicate must be defined to represent
%%  that fact.  Actions marked as natural must occur if it is possible for
%%  them to occur.
%%
natural(noop).


%%
%%  actor(Actn,Agt):  performing agent for Actions
%%
%%  This predicate binds Agt to the agent performing primitive action Actn.
%%  It requires that the action not be natural, and that the agent be the
%%  first argument to the action term.
%%
actor(Actn,Agt) :-
    prim_action(Actn), \+ natural(Actn), arg(1,Actn,Agt).


%%
%%  start(S,T):  start time of situation S
%%
%%  This predicate binds T to the start time of situation S.  This is
%%  defined as the occurance time of the last action performed in the
%%  situation.
%%
%%  The start time of s0 is not defined here, but could be defined by
%%  adding an additional clause for start/2.
%%
start(S,T) :-
    do(_,T,_) = S.


%%
%%  precedes(S1,S2):  ordering over situations
%%
%%  This predicate is true when S2 is reachable from S1 by some finite
%%  sequence of actions.  Note that no situation precedes s0, by definition.
%%
precedes(_,s0) :- fail.
precedes(S1,do(C,T,S2)) :-
    poss(C,T,S2), precedes_eq(S1,S2),
    start(S2,S2start), {S2start =< T}.

%%
%%  precedes_eq(S1,S2):  precedes-or-equals
%%
%%  This predicate is to precedes/2 as <= is to <, it allows for the
%%  two arguments to be equal.
%%
precedes_eq(S1,S2) :-
    S1 = S2 ; precedes(S1,S2).


%%
%%  legal(S1,S2):  checks legality of situation progression
%%
%%  This predicate is true if the situation S2 can legally be reached
%%  from situation S1.  This means that for each transition from S1
%%  to S2, the performed actions were possible and there are no natural
%%  actions that could have occured but didnt.
%%
legal(S,S).
legal(S1,do(C,T,S2)) :-
    legal(S1,S2),
    poss(C,T,S2), start(S2,S2start), {S2start =< T},
    \+ ( natural(NA), poss(NA,T2,S2), \+ member(NA,C), {T2 =< T} ).

%%
%%  legal(S):   checks legality of a situation
%%
%%  A situation is considered legal if it is legally reachable from the
%%  initial situation.  The initial situation itself is always legal.
%%
legal(s0) :- !.
legal(S) :-
    legal(s0,S).


%%
%%  poss(A,T,S):   possibility of executing an action
%%
%%  The predicate poss/3 must be true whenever it is possible to perform
%%  action A in situation S at time T.  A may be either a primitve action or
%%  a list of actions to be performed concurrently.
%%
%%  The domain axiomatiser is required to provide implementations of
%%  poss/3 for all primitive actions.  Concurrent actions are considered
%%  possible if each constituent action is possible, and conflicts/3
%%  does not hold.
%%
poss([A],T,S) :-
    poss(A,T,S).
poss([A|C],T,S) :-
    C \= [], poss_all([A|C],T,S), \+ conflicts([A|C],T,S).

%%
%%  poss_all(C,T,S):  all given actions are possible
%%
%%  This predicate checks that all primitive actions in concurrent action
%%  C are possible in situation S at time T.  It is the basic possibility
%%  check for concurrent actions.
%%
poss_all([],_,_).
poss_all([A|C],T,S) :-
    poss(A,T,S), poss_all(C,T,S).


%%
%%  conflicts(C,T,S):  test for conflicting actions
%%
%%  This predicate must be true if some of the primitive actions in concurrent
%%  action C cannot be executed together is situation S at time T.  The
%%  clause below provides that an empty action never conflicts, other
%%  clauses must be supplied as appropriate.
%%
conflicts([],_,_) :- fail.

%%
%%  lntp(S,T):   least-natural-time-point for a situation
%%
%%  This predicate determines the least natural time point (LNTP) T for
%%  the given situation S.  This is the earliest time at which it is possible
%%  for a natural action to occur in the situation.  It represents the time
%%  at which, given no outside influences, the situation will change.
%%
lntp(S,T) :-
    natural(A), poss(A,T,S), start(S,SStart), {SStart =< T},
    \+ (natural(A2), poss(A2,T2,S), {T2 < T}).

%%
%%  to_cact(A,C):   convert a primitive to a concurrent action
%%
%%  This predicate can be used as a "cast" operator to turn a primitve
%%  action A into concurrent action C by wrapping it in a list.  If A
%%  is already a concurrent action, it is simply unified with C.
%%
to_cact([],[]).
to_cact([H|T],[H|T]).
to_cact(A,C) :-
    prim_action(A), C = [A].



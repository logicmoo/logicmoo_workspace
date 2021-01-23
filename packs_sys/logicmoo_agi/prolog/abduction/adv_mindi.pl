% ---
%  main.pl:  Top-level prolog file for MIndiGolog
% ---
%  Author:  Ryan Kelly (rfk)
%  Author:  Douglas Miles (dmiles)
% ---
%  Date Created:  28/07/05
%  Modifications: 12/04/20
% ---
%    This file is the entry-point for an MIndiGolog program consisting
%    of the following files:
% ---
%      * Axioms of the Concurrent, Temporal Situation Calculus with
%        Natural Actions, from sitcalc.pl
%      * The MIndiGolog semantics, from mindigolog.pl
%      * A domain axiomatisation, from domain.pl
% ---
%    It imports the necessary prolog libraries and performs other
%    initialisation tasks.  It also provides the predicate main/1
%    which may be called to execute the MIndiGolog procedure named
%    'control' in an off-line fashion.
% ---

:- module(adv_mindi, [
 testsit_all/0,
 testsit_sanity/0
]).


% ---
%  Load an appropriate constraint solving library.
% ---
%  SWI provides linear constraint solving over the reals (clpr) and
%  rationals (clpq).  For the moment clpq is being used, as it seems
%  to allow the solver to infer the values of variables which have been
%  constrained to a constant value.
% ---
%  Most of these operators are native to prolog so we dont have to declare
%  them ourselves.  The all() and ext() terms takes a unique prolog-level
%  variable as its first argument; this variable will not be bound during the
%  proof search.
%

:- use_module(library('clpq')).


:- discontiguous((trans/4, final/2, prim_action/1, natural/1, poss/3,
                 conflicts/3, start/2,proc/2)).

% ---
%  Provide Syntactic operators for MIndiGolog programs
% ---
%  These operators form the "syntactic sugar" for MIndiGolog programs
% ---
:- op(660,xfy,/).  % Nondeterministic choice
:- op(650,xfy,:).  % Sequence
:- op(640,xfy,//). % Concurrent execution
:- op(640,xfy,>>). % Prioritised concurrency
:- op(620,fx,?).   % Test

% ---
%  Include the relevant definitions
% ---
%:- include(mindigolog).
%:- include(sitcalc).
%:- include(domain).
%:- include(program).


% ---
%  mindi_main(Args):  main entry-point for program execution
% ---
%  This predicate is designed as the entry-point for the program,
%  it calls the MIndiGolog procedure "control" in an off-line manner.
% ---
mindi_main(Args) :-
    ( length(Args,0) ->
        nl, do(control,s0,_S), nl
    ;
        nl, display('ERROR: No arguments can be given'), nl
    ).

% ---
%  mindigolog.pl:  IndiGolog for multiple agents in the Concurrent, Temporal
%                  Situation Calculus with Natural Actions
% ---
%    This is an extension of the transition semantics for ConGolog given
%    by De Giacomo et al ("ConGolog, a concurrent programming language
%    based on the situation calculus") to operate with the extented 
%    situation calculus defined in [sitcalc.pl].  Many of the transition
%    operators are unchanged from this work, simply implemented in CIAO
%    prolog.
% ---
%    This implementation will also incorporate a 'search' operator in
%    the manner of IndiGolog, but this has not yet been implemented.
% ---
%    TODO:  document how programs are formed
% ---
%    The execution of an MIndiGolog program is defined by a transition
%    semantics based on single-stepping and termination checking. For
%    a program D and a situation S, the program may be partially
%    executed to result in a new situation Sp and remaining program Dp
%    if the predicate trans(D,S,Dp,Sp) holds.  A program D may legally
%    terminate in situation S if the predicate final(D,S) holds.
% ---
%    It should be noted that many of the constructs in the Golog family
%    of languaged involve some nondeterminism, so trans/4 and final/2
%    are nondeterministic.
% ---
%    Programs can be executed in both an off-line and an on-line manner.
% ---
%    The predicate do(D,S,Sp) will take a program D and initial situation
%    S and find a situation Sp in which the program will legally terminate.
%    The actions performed by the program are available in the action
%    history of Sp.  This predicate will backtrack over possible transitions
%    until a legal execution is found, and will determine more legal
%    executions when it is backtracked.  This provides offline execution
%    (planning) for MIndiGolog programs.
% ---
%    The predicate ol_do(D,S) executes the program D in an on-line manner,
%    beginning in situation S.  This means that it will not backtrack over
%    the actions executed during operation of the program.  Online execution
%    is not guaranteed to succeed even if D has a possible legal execution.
%    For the moment, the actions executed are printed to standard output.
% ---
%    In general, the executions returned by do/3 will not be complete,
%    but will be in the form of a final situation where the occurance times
%    of actions are constrained in some way.  By contrast, ol_do/2 performs
%    each action at the earliest possible time.
% ---


% ---
%  final(D,S):  program termination is possible
% ---
%  The predicate final/2 is true when program D may legally terminate
%  in situation S.  It is typically defined recursively for higher-order
%  program constructs.  The specifics of each individual clause are
%  explained below.
% ---

%  It is always legal for an empty program to terminate
final(nil,_).

%  It is never legal for a program consisting of a single action
%  to terminate, hence there is no clause for this case.

%  It is never legal for a program consisting of a test to terminate,
%  hence there is no clause for this case.

%  Sequential performance of two programs may terminate only if both
%  programs may terminate.
final(seq(D1,D2),S) :-
    final(D1,S), final(D2,S).

%  Nondeterministic choice between two programs may terminate if either
%  program may terminate.
final(choice(D1,D2),S) :-
    final(D1,S)
    ;
    final(D2,S).

%  Nondeterministic choice of arguments may terminate if there is some
%  choice of arguments for which the program will terminate.
final(exists(V,D),S) :-
    sub(V,_,D,Dr), final(Dr,S).

%  Iteration of a program may always terminate, as it can legally be
%  executed zero times.
final(star(_),_).

%  Synchronised-if may terminate if the test is true and the true option
%  may terminate, or the test is false and the false option may terminate.
final(if(Cond,D1,D2),S) :-
    holds(Cond,S), final(D1,S)
    ;
    holds(neg(Cond),S), final(D2,S).

%  Synchronised-while may terminate if the test is false, or if the
%  loop program may terminate.
final(while(Cond,D),S) :-
    holds(neg(Cond),S)
    ;
    final(D,S).

%  Concurrent execution of two programs may terminate if both programs
%  may terminate.
final(conc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%  Prioritised concurrent execution of two programs may terminate if both
%  programs may terminate.
final(pconc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%  Concurrent iteration of a program may terminate in any situation, as
%  it may be executed zero times.
final(cstar(_),_).

%  A procedure call may terminate if the corresponding body, with arguments
%  substituted appropriately, may terminate.
final(pcall(PArgs),S) :-
    sub(now,S,PArgs,PArgsS), proc(PArgsS,P), final(P,S).

%  An offline search can terminate if the program to be searched can
%  terminate.
final(search(D),S) :-
    final(D,S).

%  A program is also final if it contains syntactic sugar, and is equivalent
%  to a program that is final.
final(D,S) :-
    syn_sugar(D,Ds),
    final(Ds,S).



% ---
%  trans(D,S,Dp,Sp):  program transition is possible
% ---
%  The predicate trans/4 is true when it is possible for situation S
%  to evolve to situation Sp by executing the first part of program D.
%  Dp is the part of D that remains to be executed in situation Sp.
% ---
%  Thus, trans/4 defines how execution of a program may be single-stepped
%  from one situation to another.  The specifics of each individual
%  clause are explained below.
% ---

%  It is never legal to transition an empty program, hence there
%  is no clause for this case.

%  A program consisting of a single (possibly concurrent) action may
%  transition in several ways, depending on the natural actions which
%  may occur in situation S.
% ---
%  * If situation S has no LNTP, perform the action at any time and
%    set Dp to the empty program
%  * If situation S has an LNTP, there are three possible transitions:
%      * If the action has no natural actions and it is possible to
%        do it before the LNTP, do so and set Dp to the empty program
%      * Do the natural actions at the predicted time, leaving the
%        program unaltered
%      * Do the given action and the natural actions concurrently
%        at the LNTP, and set Dp to the empty program
% ---
trans(C,S,Dp,Sp) :-
    sub(now,S,C,CS), to_cact(CS,CA), start(S,SStart),
    % TODO: Should a C containing LNTP actions be allowed after LNTP?
    ( lntp(S,LNTP) ->
      (
        % Get the list of LNTP actions
        findall(NA,(natural(NA),poss(NA,LNTP,S)),NActs),
        (
          % Can do them before the LNTP actions
          % This requires that no actions in the set are natural
          ( 
            \+ ( happens(A,CA), natural(A) ),
            {T >= SStart+1}, {T < LNTP}, poss(CA,T,S),
            Sp = do(CA,T,S), Dp = nil
          )
          ;
          % Can do them at the same time
          ( 
            union(CA,NActs,CANat),
            poss(CANat,LNTP,S),
            Sp = do(CANat,LNTP,S), Dp = nil
          )
          ;
          % Can do the LNTP actions first, leaving program unaltered
          % TODO: this poss() call should always be true, right?
          ( 
            poss(NActs,LNTP,S),
            Sp = do(NActs,LNTP,S), Dp = C
          )
        )
      )
    ;
      poss(CA,T,S), {T >= SStart+1}, Sp = do(CA,T,S), Dp = nil
    )
    .

%  A test may transition to the empty program if it holds, leaving the
%  situation unaltered.
trans(test(Cond),S,Dp,Sp) :-
    holds(Cond,S), S=Sp, Dp=nil
    ;
    lntp(S,LNTP),
    findall(NA,(natural(NA),poss(NA,LNTP,S)),NActs),
    Sp = do(NActs,LNTP,S), Dp = test(Cond).

%  Sequential execution of two programs may transition by transitioning
%  the first program, leaving the remainder the be executed in sequence
%  with the second.  If the first program may terminate, it is also legal
%  to transition the second program.
trans(seq(D1,D2),S,Dp,Sp) :-
    trans(D1,S,D1r,Sp), Dp = seq(D1r,D2).
trans(seq(D1,D2),S,Dp,Sp) :-
    final(D1,S), trans(D2,S,Dp,Sp).

%  Nondeterministic choice of programs may transition if either of the
%  programs may transition.
trans(choice(D1,D2),S,Dp,Sp) :-
    trans(D1,S,Dp,Sp) ; trans(D2,S,Dp,Sp).

%  Nondeterministic choice of arguments may transition if there is an
%  appropriate binding of the arguments for which the program may transition.
trans(exists(V,D),S,Dp,Sp) :-
    sub(V,_,D,Dr), step(Dr,S,Dp,Sp).

%  Iteration of a program may transition to the program followed by further
%  iteration, provided that the program may transition.
trans(star(D),S,Dp,Sp) :-
    Dp = seq(Dr,star(D)), trans(D,S,Dr,Sp).

%  Synchronised-if may transition if the test is true and the true option
%  may transition, or the test is false and the false option may transition.
trans(if(Cond,D1,D2),S,Dp,Sp) :-
    holds(Cond,S), trans(D1,S,Dp,Sp)
    ;
    holds(neg(Cond),S), trans(D2,S,Dp,Sp).

%  Syncrhonised-while may transition to the loop program in sequence
%  with another loop, as long as the test condition holds and the loop
%  program may transition.
trans(while(Cond,D),S,Dp,Sp) :-
    Dp = seq(Dr,while(Cond,D)), holds(Cond,S), trans(D,S,Dr,Sp).

%  Concurrent execution may transition in three ways:
% ---
%    * Transition the first program, leaving its remainder to be
%      executed concurrently with the second program
%    * Transition the second program, leaving its remainder to be
%      executed concurrently with the first program
%    * Find a concurrent action that will transition the first program
%      and a concurrent action that will transition the second program,
%      and perform both concurrently.  The remaining program is the
%      concurrent execution of the remainders of the individual programs
% ---

trans_true_conc(conc(D1,D2),S,Dp,Sp) :-
    step(D1,S,Dr1,do(C1,T,S)),
    step(D2,S,Dr2,do(C2,T,S)),
    % TODO:  if one part is waiting for a natural action, do the other first.
    %        This will produce solutions in which as much as possible is done
    %        while waiting for a natural actions.  Such solutions are already
    %        entailed but are not the first solution encountered.
    \+ ( happens(A,C1), happens(A,C2), actor(A,_) ),
    union(C1,C2,CT), trans(CT,S,nil,Sp),
    Dp = conc(Dr1,Dr2).

trans(conc(D1,D2),S,Dp,Sp) :-
    trans_true_conc(conc(D1,D2),S,Dp,Sp)
    ;
    Dp = conc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = conc(D1,Dr2), trans(D2,S,Dr2,Sp).

%  Prioritised concurrent execution may transition by transitioning the
%  first program, leaving its remainder to be executed in pconc with
%  the second program.  If it is not possible to transition the first
%  program, then it is also legal to transition the second program leaving
%  the first program to be executed in pconc with its remainder.
trans(pconc(D1,D2),S,Dp,Sp) :-
    Dp = pconc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = pconc(D1,Dr2), trans(D2,S,Dr2,Sp), \+ trans(D1,S,_,_).

%  Concurrent execution of a program may transition to the program
%  being concurrently executed with more concurrent iterations of it,
%  provided that the program can transition.
trans(cstar(D),S,Dp,Sp) :-
    Dp = conc(Dr,cstar(D)), trans(D,S,Dr,Sp).

%  A proceduce call may transition if the body program, with arguments
%  substituted in, may transition.
trans(pcall(PArgs),S,Dp,Sp) :-
    sub(now,S,PArgs,PArgsS),
    proc(PArgsS,P), trans(P,S,Dp,Sp).

%  TODO:  a search() operator similar to IndiGolog.
%trans(search(D),S,Dp,Sp) :-
%    do(D,S,S2),

%  A program may also transition if it contains syntactic sugar, and is
%  equivalent to a program that may transition.
trans(D,S,Dp,Sp) :-
    syn_sugar(D,Ds),
    trans(Ds,S,Dp,Sp).



% ---
%  syn_sugar(Din,Dout):  syntactic sugar for programs
% ---
%  This predicate is used to make writing MIndiGolog programs easer,
%  by providing a level of syntax transformation into the canonical
%  program form.  If Din is a program containing an element of this
%  sugar, Dout is unified with the canonicalised version.
% ---

%  Sequential execution can be represented by an infix :
syn_sugar(D1 : D2,seq(D1,D2)).
%  Nondeterministic choice can be represented by an infix /
syn_sugar(D1 / D2,choice(D1,D2)).
%  Concurrent execution can be represented by an infix //
syn_sugar(D1 // D2,conc(D1,D2)).
%  Prioritised concurrent execution can be represented by an infix >>
syn_sugar(D1 >> D2,pconc(D1,D2)).
%  Condition tests can be represented by a prefix ?
syn_sugar(?C,test(C)).
%  Procedure calls can be represented by name
syn_sugar(Proc,pcall(Proc)) :-
    proc(Proc,_).


% ---
%  holds(Cond,S):  check whether a condition holds in a situation
% ---
%  This predicate is used to evaluate reified condition terms from
%  MIndiGolog programs.  It recursively reduces the formula to equivalent
%  forms which can be tested directly by the prolog theorem prover,
%  and hence includes the standard prolog negation-as-failure semantics.
%  
holds(and(C1,C2),S) :-
    holds(C1,S), holds(C2,S).
holds(or(C1,C2),S) :- 
    holds(C1,S) ; holds(C2,S).
holds(all(V,C),S) :-
    holds(neg(some(V,neg(C))),S).
holds(some(V,C),S) :-
    sub(V,_,C,Cr), holds(Cr,S).
holds(neg(neg(C)),S) :-
    holds(C,S).
holds(neg(and(C1,C2)),S) :-
    holds(or(neg(C1),neg(C2)),S).
holds(neg(or(C1,C2)),S) :-
    holds(and(neg(C1),neg(C2)),S).
holds(neg(all(V,C)),S) :-
    holds(some(V,neg(C)),S).
holds(neg(some(V,C)),S) :-
    \+ holds(some(V,C),S).
holds(P_Xs,S) :-
    P_Xs \= and(_,_), P_Xs \= or(_,_), P_Xs \= neg(_), P_Xs \= all(_,_),
    P_Xs \= some(_,_), sub(now,S,P_Xs,P_XsS), P_XsS.
holds(neg(P_Xs),S) :-
    P_Xs \= and(_,_), P_Xs \= or(_,_), P_Xs \= neg(_), P_Xs \= all(_,_),
    P_Xs \= some(_,_), sub(now,S,P_Xs,P_XsS), \+ P_XsS.



% ---
%  sub(Name,Value,Old,New):  substitue values in a term
% ---
%  This predicate is true when New is equal to Old with all occurances
%  of Name replaced by Value - basically, a symbolic substitution
%  routine.  For example, it is usually used to produce a result such
%  as:
% ---
%      sub(now,S,fluent(now),fluent(S)).
% ---
sub(_,_,T,Tr) :-
    var(T), Tr = T.
sub(X,Y,T,Tr) :-
    \+ var(T), T = X, Tr = Y.
sub(X,Y,T,Tr) :-
    T \= X, T =.. [F|Ts], sub_list(X,Y,Ts,Trs), Tr =.. [F|Trs].

% ---
%  sub_list(Name,Value,Old,New):  value substitution in a list
% ---
%  This predicate operates as sub/4, but Old and New are lists of terms
%  instead of single terms.  Basically, it calls sub/4 recursively on
%  each element of the list.
% ---
sub_list(_,_,[],[]).
sub_list(X,Y,[T|Ts],[Tr|Trs]) :-
    sub(X,Y,T,Tr), sub_list(X,Y,Ts,Trs).



% ---
%  trans_c(D,S,Dp,Sp):  Transitive Closure of Transition Rules
% ---
%  This predicate is true if Dp,Sp are in the transitive closure of
%  the trans/4 predicate for D,S.  It is a simplistic encoding of the
%  transitive closure in prolog using a recursive definition.
% ---
trans_c(D,S,D,S).
trans_c(D,S,Dp,Sp) :-
    trans(D,S,Dr,Sr),
    trans_c(Dr,Sr,Dp,Sp).


% ---
%  step(D,S,Dp,Sp):  single-step a program
% ---
%  This predicate takes a program D and a situation S in which to execute it,
%  and returns a new situation Sp and remaining program Dp such that the
%  execution of D has progressed by a single action.  It may be used
%  repeatedly to find a possible next action to perform for a given program.
% ---
step(D,S,Dp,Sp) :-
    %  Naive implementation is simply:  trans_c(D,S,Dp,do(C,T,S))
    %  This implementation is more efficient as it does not generate
    %  transitions that go beyond one action from S (which will always fail).
    Sp = do(_,_,S), trans(D,S,Dp,Sp)
    ;
    trans(D,S,Dr,S), step(Dr,S,Dp,Sp).


% ---
%  do(D,S,Sp):  offline execution of MIndiGolog Programs
% ---
%  This predicate takes a program D and starting situation S, and
%  finds a new situation Sp which can be legally reached by executing
%  program D to termination.  The actions executed can be retreived
%  from the situation Sp.
% ---
%  This predicate is capable of backtracking over action choices to
%  find a legal execution, and so is not suitable for executing programs
%  on-line.
% ---
do(D,S,Sp) :-
    trans_c(D,S,Dp,Sp),
    %nl, nl, display(Sp), nl, nl, display(Dp), nl, get_code(_),
    final(Dp,Sp),
    show_action_history(Sp).

%  Temporary predicate for printing the action history of a situation,
%  for debugging purposes only.
show_action_history(s0) :-
    nl.
show_action_history(do(C,T,S)) :-
    show_action_history(S),
    ( inf(T,MinT) ->
        T = MinT
    ;
        start(S,SStart), T = SStart
    ),
    write('do '), write(C), write(' at '), write(T), nl.


% ---
%  ol_do(D,S):  execute a program in an on-line manner
% ---
%  This predicate is similar to do/4 except it does not backtrack
%  over the selection of actions to perform.  Thus, the actions
%  selected by this predicate are suitable to be performed in online
%  execution of the program.  However, this also means that successful
%  termination of the program is not guaranteed.
% ---
%  At this stage it simply prints each action performed to stdout,
%  performing each at the minimum possible time.
% ---
ol_do(D,S) :-
    %( ol_valid_step(D,S,Dr,Sr) ->
    ( step(D,S,Dr,Sr) ->
        Sr = do(C,T,S),
        ( inf(T,MinT) ->
            true
        ;
            start(S,SStart), MinT = SStart
        ),
        write('do '), write(C), write(' at '), write(MinT), nl,
        ( final(Dr,Sr) ->
            write('SUCCEEDED!')
        ;
            ol_do(Dr,do(C,MinT,S))
        )
    ;
        write('FAILED!'), nl, write('Remaining: '),
        write(D), nl
    ).


% If the step being called for contains a natural action, ensure that
% the resulting program is not waiting for that action.
ol_valid_step(D,S,Dr,do(C,T,S)) :-
    step(D,S,Dr,do(C,T,S)),
    %write('step: '), write(C), nl,
    ( happens(NA,C), natural(NA) ->
        %write('contains NA'), nl,
        \+ ( step(Dr,S,_,do(C2,_,S)),
             happens(NA,C2) %, write('which is broken'), nl
           )
      ;
        true
    ).

% ---
%  sitcalc.pl:  Prolog Implementation of the Concurrent, Temporal
%               Situation Calculus with Natural Actions.
% ---
%    This implementation is a slight modification of the work
%    of Reiter ("Natural Actions, Concurrency and Continuous
%    Time in the Situation Calculus"), implemented in SWI prolog.
%    In this case, the temporal component is attached to situations
%    instead of actions.  So do(A,S) becomes do(A,T,S), poss(A,S)
%    becomes poss(A,T,S), etc.  I hypothesise that the resulting
%    languages are equivalently expressive in terms of legal situations,
%    but make no attempt at this stage to prove it.
% ---
%    The worlds modelled using this framework must conform to the
%    following structural rules:
% ---
%       * All actions are instantaneous.
% ---
%       * All actions are either "natural actions" or performed
%         by an agent.  Natural actions are those for which the
%         predicate natural(A) is true.
% ---
%       * For actions which are not natural, the first argument of
%         the term must indicate the agent which performs that action.
% ---
%       * There is a unique initial situation denoted by the atom s0.
% ---
%       * Concurrently occuring actions are represented as lists of
%         primitive action terms.
% ---
%  To implement a domain axiomatisation using this framework, the following
%  tasks must be completed:
% ---
%       * specify the primitive actions of the world using prim_action/1.
% ---
%       * specify which actions are natural using natural/1.
% ---
%       * specify the primitive fluents in the world using prim_fluent/1.
% ---
%       * specify the agents in the system using agent/1.
% ---
%       * specify the possibility axioms for primitive actions using poss/3.
% ---
%       * specify conflicting sets of concurrent actions using conflicts/3.
% ---
%       * specify the successor state axioms in terms of predicates for
%         each fluent.
% ---
%       * specify the initial conditions using fluent predicates with the
%         situation term set to s0.
% ---


% ---
%  prim_action(A):  define a primitive action
% ---
%  This predicate specifies the terms which represent primitive actions
%  in the domain.  The following below are examples of both an agent-initiated
%  and a natural "no-op" action.  As the action as no effect, successor
%  state axioms are not necessary.
% ---
prim_action(noop(A)) :-
    agent(A).
prim_action(noop).

% ---
%  natural(A):  specify natural actions
% ---
%  For each natural action, this predicate must be defined to represent
%  that fact.  Actions marked as natural must occur if it is possible for
%  them to occur.
% ---
natural(noop).


% ---
%  actor(Actn,Agt):  performing agent for Actions
% ---
%  This predicate binds Agt to the agent performing primitive action Actn.
%  It requires that the action not be natural, and that the agent be the
%  first argument to the action term.
% ---
actor(Actn,Agt) :-
    prim_action(Actn), \+ natural(Actn), 
    functor(Actn,F,A),
    agent_of_act(Actn,F,Agt).
    
agent_of_act(Actn,actT,Agt):- !,
    arg(2,Actn,Agt).

agent_of_act(Actn,_,Agt):-   
    arg(1,Actn,Agt).


% ---
%  start(S,T):  start time of situation S
% ---
%  This predicate binds T to the start time of situation S.  This is
%  defined as the occurance time of the last action performed in the
%  situation.
% ---
%  The start time of s0 is not defined here, but could be defined by
%  adding an additional clause for start/2.
% ---
start(S,T) :-
    do(_,T,_) = S.

% ---
%  precedes(S1,S2):  ordering over situations
% ---
%  This predicate is true when S2 is reachable from S1 by some finite
%  sequence of actions.  Note that no situation precedes s0, by definition.
% ---
precedes(_,s0) :- fail.
precedes(S1,do(C,T,S2)) :-
    poss(C,T,S2), precedes_eq(S1,S2),
    start(S2,S2start), {S2start =< T}.

% ---
%  precedes_eq(S1,S2):  precedes-or-equals
% ---
%  This predicate is to precedes/2 as <= is to <, it allows for the
%  two arguments to be equal.
% ---
precedes_eq(S1,S2) :-
    S1 = S2 ; precedes(S1,S2).


% ---
%  legal(S1,S2):  checks legality of situation progression
% ---
%  This predicate is true if the situation S2 can legally be reached
%  from situation S1.  This means that for each transition from S1
%  to S2, the performed actions were possible and there are no natural
%  actions that could have occured but didnt.
% ---
legal(S,S).
legal(S1,do(C,T,S2)) :-
    legal(S1,S2),
    poss(C,T,S2), start(S2,S2start), {S2start =< T},
    \+ ( natural(NA), poss(NA,T2,S2), \+ happens(NA,C), {T2 =< T} ).

% ---
%  legal(S):   checks legality of a situation
% ---
%  A situation is considered legal if it is legally reachable from the
%  initial situation.  The initial situation itself is always legal.
% ---
legal(s0) :- !.
legal(S) :-
    legal(s0,S).


% ---
%  poss(A,T,S):   possibility of executing an action
% ---
%  The predicate poss/3 must be true whenever it is possible to perform
%  action A in situation S at time T.  A may be either a primitve action or
%  a list of actions to be performed concurrently.
% ---
%  The domain axiomatiser is required to provide implementations of
%  poss/3 for all primitive actions.  Concurrent actions are considered
%  possible if each constituent action is possible, and conflicts/3
%  does not hold.
% ---
poss([A],T,S) :-
    poss(A,T,S).
poss([A|C],T,S) :-
    C \= [], poss_all([A|C],T,S), \+ conflicts([A|C],T,S).

% ---
%  poss_all(C,T,S):  all given actions are possible
% ---
%  This predicate checks that all primitive actions in concurrent action
%  C are possible in situation S at time T.  It is the basic possibility
%  check for concurrent actions.
% ---
poss_all([],_,_).
poss_all([A|C],T,S) :-
    poss(A,T,S), poss_all(C,T,S).


% ---
%  conflicts(C,T,S):  test for conflicting actions
% ---
%  This predicate must be true if some of the primitive actions in concurrent
%  action C cannot be executed together is situation S at time T.  The
%  clause below provides that an empty action never conflicts, other
%  clauses must be supplied as appropriate.
% ---
conflicts([],_,_) :- fail.

% ---
%  lntp(S,T):   least-natural-time-point for a situation
% ---
%  This predicate determines the least natural time point (LNTP) T for
%  the given situation S.  This is the earliest time at which it is possible
%  for a natural action to occur in the situation.  It represents the time
%  at which, given no outside influences, the situation will change.
% ---
lntp(S,T) :-
    natural(A), poss(A,T,S), start(S,SStart), {SStart =< T},
    \+ (natural(A2), poss(A2,T2,S), {T2 < T}).

% ---
%  to_cact(A,C):   convert a primitive to a concurrent action
% ---
%  This predicate can be used as a "cast" operator to turn a primitve
%  action A into concurrent action C by wrapping it in a list.  If A
%  is already a concurrent action, it is simply unified with C.
% ---
to_cact([],[]).
to_cact([H|T],[H|T]).
to_cact(A,C) :-
    prim_action(A), C = [A].


% ---:- include('domain_cake.pl').

% ---
%  domain.pl:  Axiomatisation of the "Cooking Agents" domain for MIndiGolog
% ---
%    This file contains an axiomatisation of the "Cooking Agents" domain
%    in the Concurrent, Temporal Situation Calculus with Natural Actions.
% ---
%    The domain consists of several agents and inanimate objects of
%    different types (indicated by prim_obj/2) which in turn may
%    be part of super-types (indicated by super_type/2).
% ---
%    Agents may acquire objects, place them inside/on container objects,
%    and transfer the contents of one container object to another.  There
%    are also an unlimited supply of timers in the world which may be
%    set to ring at a specified time in the future.
% ---
%    The agents may also perform several continuous tasks, which have
%    durations and may span several situations.  Agents may only perform
%    one task at a time.
% ---
%    Depending on the tasks being performed, the contents of container
%    objects may evolve from one situation to another.  For example,
%    if the task actT(mix,Agt,bowl1,5) is being performed (mix the contents of
%    bowl1 for five minutees) then the contents might evolve from
%    [egg1,flour1,sugar1] to mixed([egg1,flour1,sugar1],5).
% ---


%  
%  agent(Agt):  specify agents in the system
% ---
%  This predicate is true when Agt is the name of an agent in the world.
% ---
agent(joe).
agent(richard).
agent(harriet).


% 
%  task(T):  specify the tasks that can be performed
% ---
%  This predicate is true when T is a task that the agents in the system
%  can perform.  Tasks are typically parameterised in terms of the
%  objects on which they operate.
% ---

%  actT(mix,Agt,Cont,Dur):  mix the contents of container Cont for duration Dur
task(actT(mix,_,Cont,_)) :- 
    obj_is_type(Cont,container).

%  actT(chop,Agt,Cont):  chop the contents of container Cont
task(actT(chop,_,Cont)) :-
    obj_is_type(Cont,container).

% ---
%  task_duration(Agt,Task,Dur):  specify duration of tasks
% ---
%  This predicate is true when Dur is the time taken by agent Agt
%  to perform the task Task.
% ---
task_duration(_,actT(mix,_,_,D),D).
task_duration(Agt,actT(chop,Agt,_),D) :-
    (Agt = richard ->
        D = 5
    ;
        D = 3
    ).


% ---
%  prim_obj1(Obj):  shortcut to check object names
% ---
%  This predicate is true if Obj is the name of a primite object,
%  regardless of its type.
% ---

prim_obj1(Obj) :-
    prim_obj(Obj,_).

% ---
%  prim_obj(Obj,Type):  specify primitive objects in the world
% ---
%  This predicate is true when Obj is the name of a primitive object
%  in the world, of type Type.

prim_obj(Obj,knife) :-
    member(Obj,[knife1,knife2,knife3]).
prim_obj(Obj,bowl) :-
    member(Obj,[bowl1,bowl2,bowl3]).
prim_obj(Obj,board) :-
    member(Obj,[board1,board2]).
prim_obj(Obj,oven) :-
    member(Obj,[oven1]).
prim_obj(Obj,flour) :-
    member(Obj,[flour1,flour2,flour3,flour4,flour5]).
prim_obj(Obj,sugar) :-
    member(Obj,[sugar1,sugar2,sugar3,sugar4,sugar5,sugar6]).
prim_obj(Obj,egg) :-
    member(Obj,[egg1,egg2,egg3]).
prim_obj(Obj,tomato) :-
    member(Obj,[tomato1,tomato2]).
prim_obj(Obj,lettuce) :-
    member(Obj,[lettuce1,lettuce2,lettuce3]).
prim_obj(Obj,carrot) :-
    member(Obj,[carrot1,carrot2,carrot3]).


% ---
%  super_type(SubType,SuperType):  specify type hierarchy
% ---
%  This predicate is true when all objects of type SubType are
%  also of type SuperType.
%  
super_type(Type,cooking_appliance) :-
    member(Type,[oven]).
super_type(Type,container) :-
    member(Type,[bowl,board,cooking_appliance]).
super_type(Type,ingredient) :-
    member(Type,[flour,egg,tomato,lettuce,sugar]).
super_type(Type,makable) :-
    member(Type,[salad,cake]).


type_is_type(Type,Super):- super_type(Type,Super).
type_is_type(Type,Type).

% ---
%  obj_is_type(Obj,Type):  check object types
% ---
%  This predicate is true when the object named Obj is of type
%  Type according to the hierarchy of super-types.
% ---
obj_is_type(Obj,Type) :-
    prim_obj(Obj,Type)
    ;
    super_type(SubType,Type), obj_is_type(Obj,SubType).


% ---
%  prim_action(Act):  specify primitive actions
% ---
%  This predicate is true when Act is the name of a primitive action
%  in the world.  Actions are typically parameterised in terms of the
%  objects they act on.  See the details of the MIndiGolog situation
%  calculus for further information.
% ---

%  actT(acquire_object,Agt,Obj):  agent acquires control of an object
prim_action(actT(acquire_object,Agt,Obj)) :-
    agent(Agt), prim_obj1(Obj).

%  actT(release_object,Agt,Obj):  agent releases an object it has acquired
prim_action(actT(release_object,Agt,Obj)) :-
    agent(Agt), prim_obj1(Obj).

%  actT(set_timer,Agt,ID,Dur):  agent sets timer with name ID to ring Dur
%  minutes in the future
prim_action(actT(set_timer,Agt,_,_)) :-
    agent(Agt).

%  event_ring_timer(ID):  timer with name ID rings, a natural action
prim_action(event_ring_timer(_)).
natural(event_ring_timer(_)).

%  actT(place_in,Agt,Conts,Dest):  agent places object Conts in container Dest
prim_action(actT(place_in,Agt,Conts,Dest)) :-
    agent(Agt), prim_obj1(Conts), obj_is_type(Dest,container).

%  actT(transfer_now,Agt,Source,Dest):  agent transfers contents of container Source
%  to container Dest
prim_action(actT(transfer_now,Agt,Source,Dest)) :-
    agent(Agt), obj_is_type(Source,container),
    obj_is_type(Dest,container).

%  actT(begin_task,Agt,Task):  agent starts performing task Task
prim_action(actT(begin_task,Agt,Task)) :-
    agent(Agt), task(Task).

%  event_end_task(Agt,Task):  agent finishes performing task, a natural action
prim_action(event_end_task(Agt,Task)) :-
    agent(Agt), task(Task).
natural(event_end_task(_,_)).


% ---
%  poss(A,T,S):  possibility of performing an action
% ---
%  This predicate is true when it is possible to perform action
%  A at time T in situation S.
% ---

%  Agents can only acquire an object if no agent already has it,
%  they arent doing a task, and the object hasnt been used.
poss(actT(acquire_object,Agt,Obj),_,S) :-
    \+ state_agent_has_object(_,Obj,S), \+ doing_task(Agt,_,_,S), \+ used(Obj,S).

%  Agents may only release objects that they have, when they arent
%  currently performing a task
poss(actT(release_object,Agt,Obj),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may set a timer as long as it hasnt already been set, and
%  they arent currently performing a task
poss(actT(set_timer,Agt,ID,_),_,S) :-
    \+ timer_set(ID,_,S), \+ doing_task(Agt,_,_,S).

%  It is only possible for a timer to ring once its remaining time
%  has precisely elapsed
poss(event_ring_timer(ID),T,S) :-
    timer_set(ID,Dur,S),
    start(S,SStart), {T = SStart + Dur}.

%  Agents may place an object in a container provided they have possession
%  of both, and arent currently doing a task
poss(actT(place_in,Agt,Conts,Dest),_,S) :-
    state_agent_has_object(Agt,Conts,S), state_agent_has_object(Agt,Dest,S),
    \+ obj_is_type(Conts,cooking_appliance),
    \+ doing_task(Agt,_,_,S).

%  Agents may transfer contents from one container to another as long
%  as they have possession of both, and arent doing a task
poss(actT(transfer_now,Agt,Source,Dest),_,S) :-
    state_agent_has_object(Agt,Source,S), state_agent_has_object(Agt,Dest,S),
    \+ doing_task(Agt,_,_,S).

%  Agents may begin the actT(mix,Agt,...) task as long as they arent doing another
%  task, and have possession of the container to be mixed in
poss(actT(begin_task,Agt,actT(mix,Agt,Obj,_)),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may begin the actT(chop,Agt,...) task as long as they arent doing another
%  task, and have possession of the container whose contents to chop
poss(actT(begin_task,Agt,actT(chop,Agt,Obj)),_,S) :-
    state_agent_has_object(Agt,Obj,S), \+ doing_task(Agt,_,_,S).

%  Agents may end a task only when precisely the remaining amount of
%  time has elapsed
poss(event_end_task(Agt,Task),T,S) :-
    doing_task(Agt,Task,Remain,S),
    start(S,SStart), {T = SStart + Remain}.


happens(E,L):- member(E,L).

% ---
%  conflicts(C,T,S):  concurrent actions conflict
% ---
%  This predicate must be true when concurrent actions in C conflict
%  and cannot be performed at time T in situation S.
% ---

%  Agents cannot do more than one action at a time
conflicts(C,_,_) :-
    happens(A1,C), actor(A1,Agt),
    happens(A2,C), actor(A2,Agt),
    A2 \= A1.

%  Two agents cannot acquire the same object
conflicts(C,_,_) :-
    happens(actT(acquire_object,A1,Res),C),
    happens(actT(acquire_object,A2,Res),C),
    A1 \= A2.
    
% ---
%  Fluents in the Domain
% ---
%  The fluents are specified in terms of their successor state axioms,
%  of the form "a fluent is true if it became true, or was previously
%  true did not become false".
% ---
%    fluent_holds(Args,do(A,T,S)) :-
%        fluent_becomes_true(Args,do(A,T,S))
%        ;
%        (
%          fluent_holds(Args,S),
%          \+ fluent_becomes_false(Args,do(A,T,S))
%        )
% ---

% ---
%  state_agent_has_object(Agt,Obj,S):  agent has an object
% ---
%  This fluent is true when agent Agt has possession of the object Obj
%  in situation S.  It can become true by acquiring the object, and
%  false by releasing the object or if it has become used.
% ---
state_agent_has_object(Agt,Obj,do(C,T,S)) :-
    happens(actT(acquire_object,Agt,Obj),C)
    ;
    state_agent_has_object(Agt,Obj,S),
    \+ (
       happens(actT(release_object,Agt,Obj),C)
       ;
       used(Obj,do(C,T,S))
    ).

% ---
%  used(Obj,S):  object is used in situation S
% ---
%  This fluent is true when an object has been used - for example,
%  an ingredient has been placed in a container.  Once an object has
%  been used, it cannot be used again.
% ---
used(Obj,do(C,_,S)) :-
    prim_obj1(Obj), obj_is_type(Obj,ingredient),
    (
      used(Obj,S)
      ;
      happens(actT(place_in,_,Obj,_),C)
    ).

% ---
%  timer_set(ID,Dur,S):  timer is set in situation S
% ---
%  This fluent indicates that the timer named ID is set to ring in Dur
%  minutes in situation S.  It becomes true as a result of a actT(set_timer,)
%  action, and is updated from situation to situation to reflect
%  the amount of time remaining.
% ---
timer_set(ID,Dur,do(C,T,S)) :-
    happens(actT(set_timer,_,ID,Dur),C)
    ;
    timer_set(ID,OldDur,S), start(S,SStart), {Dur = OldDur-(T-SStart)},
    \+ happens(event_ring_timer(ID),C).

% ---
%  contents(Obj,Conts,S):  object contents in a situation
% ---
%  This fluent indicates that object Obj contains the contents Conts
%  in situation S.  It can become true, become false, and change value
%  in a variety of ways, each of which is documented with its
%  implementation.
% ---
      % --- All the ways it can become true

%- It was previously empty, and contents were placed or transfered in
contents(Obj,Conts,do(C,_T,S)):- 
      (happens(actT(place_in,_,Conts,Obj),C)
         ; happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,Conts,S)),
      \+ contents(Obj,_,S).


%- It previously had contents, and more contents were placed or
%- transfered in.  Contents is then a list.
contents(Obj,Conts,do(C,T,S)):- 
      (happens(actT(place_in,_,NewConts,Obj),C)
         ; happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,NewConts,S)),
      contents(Obj,OldConts,S),
      ( OldConts = [_|_] -> OldContsL = OldConts ; OldContsL = [OldConts]),
      ( NewConts = [_|_] -> NewContsL = NewConts ; NewContsL = [NewConts]),
      union(OldContsL,NewContsL,Conts).

%- An agent is mixing the contents.  If they were previously
%- unmixed, they are encased in a mixed(conts,time) indicator.
%- If they were previously mixed, the mixing time is increased.
contents(Obj,Conts,do(C,T,S)):- 
      start(S,SStart),
      doing_task(_,actT(mix,_,Obj,_),_,do(C,T,S)), 
      contents(Obj,OldConts,S),
      (  OldConts = mixed(MixConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP)
         ;
             Conts = mixed(OldConts,0)
      ).


%- An agent just completed mixing the contents.  The mixing time
%- must be modified to take its final value.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      happens(event_end_task(_,actT(mix,_,Obj,_)),C), 
      contents(Obj,mixed(MixConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = mixed(MixConts,NewP).


%- An agent just completed chopping the contents.  They are
%- encased in a chopped() indicator.
contents(Obj,Conts,do(C,T,S)):- 
      happens(event_end_task(_,actT(chop,_,Obj)),C), 
      contents(Obj,OldConts,S),
      Conts = chopped(OldConts).


%- If the container is in an oven, its contents are baked.
%- If they are not encapsulated in a baked() indicator then do
%- so, otherwise update the baking time.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      \+ obj_is_type(Obj,cooking_appliance), 
      obj_is_type(Oven,oven),
      contents(Oven,Obj,do(C,T,S)), 
      contents(Obj,OldConts,S),
      (  OldConts = baked(BakedConts,OldP) ->
             {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP)
         ;
             Conts = baked(OldConts,0)
      ).


%- If the container was just taken out of the oven, updated
%- the content to reflect the total baking time.
contents(Obj,Conts,do(C,T,S)):- start(S,SStart),
      \+ obj_is_type(Obj,cooking_appliance), obj_is_type(Oven,oven),
      contents(Oven,Obj,S), happens(actT(transfer_now,_,Oven,_),C),
      contents(Obj,baked(BakedConts,OldP),S),
      {NewP = OldP+(T-SStart)}, Conts = baked(BakedConts,NewP). 

    % Or it was true, and didnt become false...
contents(Obj,Conts,do(C,T,S)):-
    contents(Obj,Conts,S), 
    \+ stateT_becomes_false(contents,Obj,Conts, do(C,T,S)).

%- --- All the ways it can become false

%- The contents were transfered out
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :-  happens(actT(transfer_now,_,Obj,_),C).

%- New contents were transfered in
stateT_becomes_false(contents,Obj,_,do(C,_,S)) :- happens(actT(transfer_now,_,Obj2,Obj),C), contents(Obj2,_,S).        

%- New contents were placed in
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(actT(place_in,_,_,Obj),C).

%- The contents are being mixed, hence will change
stateT_becomes_false(contents,Obj,_,CTS) :-  doing_task(_,actT(mix,_,Obj,_),_,CTS).

%- The contents just finished being mixed, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(event_end_task(_,actT(mix,_,Obj,_)),C).

%- The contents just finished being chopped, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,_)) :- happens(event_end_task(_,actT(chop,_,Obj)),C).

%- The object is in an oven, hence will change
stateT_becomes_false(contents,Obj,_Conts,CTS) :- 
  \+ obj_is_type(Obj,cooking_appliance), 
  obj_is_type(Oven,oven), 
  contents(Oven,Obj,CTS).

%- The object was just taken out of an oven, hence will change
stateT_becomes_false(contents,Obj,_,do(C,_,S)) :- 
  \+ obj_is_type(Obj,cooking_appliance), 
  obj_is_type(Oven,oven),
  contents(Oven,Obj,S),
  happens(actT(transfer_now,_,Oven,_),C).


% ---
%  doing_task(Agt,Task,Remain,S):  agent is performing a task
% ---
%  This fluent is true when agent Agt is performing task Task in situation
%  S, and has Remain minutes left before it is completed.  It becomes
%  true when an agent begins a task, becomes false when an agent
%  completes a task, and its value is updated from situation to situation
%  to reflect the remianing time till completion.
% ---
doing_task(Agt,Task,Remain,do(C,T,S)) :-
    happens(actT(begin_task,Agt,Task),C), task_duration(Agt,Task,Remain)
    ;
    doing_task(Agt,Task,OldRem,S), start(S,SStart),
    {OldRem = Remain-(T-SStart)}, \+ happens(event_end_task(Agt,Task),C).
    

% ---
%  history_length(N,S):  length of the action histoy in a situation
% ---
%  This simple fluent encodes in N the number of actions that have
%  taken place in the history of situation S.  It is used to make this
%  information easily available to agents.
% ---
history_length(N,do(_,_,S)) :-
    history_length(N1,S),
    N is N1 + 1.
history_length(0,s0).

% ---
%  Intial Conditions for the domain
% ---
%  The initial conditions are specified by additional clauses for
%  each fluent, with the situation term set to the atom s0.  For
%  the most part no fluents hold in the initial situation, so 
%  there arent many clauses here.
% ---
start(s0,0).

% ---
%  MIndiGolog procedures
% ---
%  The following are a collection of useful procedures in the domain,
%  from which larger programs can be built.
% ---


%  Ensure that the agent has control of an object
proc(ensure_agent_has_object(Agt,Obj),
     if(state_agent_has_object(Agt,Obj,now),nil,actT(acquire_object,Agt,Obj))
    ).

%  Carry out the necessary sequence of actions to place one object
%  inside another, releasing the destination container when done.
proc(doPlaceIn(Agt,Obj,Dest),
     ensure_agent_has_object(Agt,Obj) // ensure_agent_has_object(Agt,Dest)
     : actT(place_in,Agt,Obj,Dest)
     : actT(release_object,Agt,Dest)
    ).

%  Nondeterministically select an object of a given type, gain control
%  of it, and place it inside a container object.
proc(ensure_place_type_in(Agt,Type,Dest),
     exists(obj,?and(obj_is_type(obj,Type),not(used(obj,now)))
            : actT(acquire_object,Agt,obj)
            : doPlaceIn(Agt,obj,Dest))
    ).

%  Carry out the necessary actions to transfer the contents of one
%  container to another, relasing both when finished.
proc(ensure_xfrd(Agt,Source,Dest),
     ensure_agent_has_object(Agt,Source) // ensure_agent_has_object(Agt,Dest)
     : actT(transfer_now,Agt,Source,Dest)
     : actT(release_object,Agt,Source) // actT(release_object,Agt,Dest)
    ).

%  Make a simple cake mixture in the specified container.
%  The agents to perform the various steps are selected
%  nondeterministically.
proc(actT(make,Doer,mixFor(Cake,Doer),Dest),
     exists(agt,?agent(agt) : ensure_place_type_in(agt,egg,Dest))
     : exists(agt,?agent(agt) : ensure_place_type_in(agt,flour,Dest))
     : exists(agt,?agent(agt) : ensure_place_type_in(agt,sugar,Dest))
     : exists(agt, ?agent(agt) : actT(acquire_object,agt,Dest)
                           : actT(begin_task,agt,actT(mix,agt,Dest,5))
%                           : event_end_task(agt,actT(mix,agt,Dest,5))
                           : actT(release_object,agt,Dest))
     ) :- type_is_type(Cake,cake).

%  Make a cake in the specified container.  This involves
%  making cake mix in the container, then baking it in an oven.
proc(actT(make,Doer,Cake,Dest),
exists(agt,actT(make,agt,mixFor(Cake,Doer),Dest))
     : exists(myOven, ?obj_is_type(myOven,oven)
                  : exists(agt, ensure_agent_has_object(agt,myOven)
                            : ensure_agent_has_object(agt,Dest)
                            : actT(place_in,agt,Dest,myOven)
                            : actT(set_timer,agt,timerFor(Cake,Doer),35)
                      )
                  : event_ring_timer(timerFor(Cake,Doer))
                  : exists(agt,exists(myBoard, ?obj_is_type(myBoard,board)
                                       : ensure_xfrd(agt,myOven,myBoard)
                      ))
         )
     ) :- type_is_type(Cake,cake).



%  Chop the given item then place it in the given container.
%  Releases control of the container when done.  An empty chopping
%  board is selected nondeterministically.
proc(ensure_chop_into(Agt,Obj,Dest),
     ensure_agent_has_object(Agt,Obj)
     : exists(myBoard, ?and(obj_is_type(myBoard,board),neg(contents(myBoard,_,now)))
                   : ensure_agent_has_object(Agt,myBoard)
                   : actT(place_in,Agt,Obj,myBoard)
                   : actT(begin_task,Agt,actT(chop,Agt,myBoard))
%                   : event_end_task(Agt,actT(chop,Agt,myBoard))
                   : ensure_agent_has_object(Agt,Dest)
                   : actT(transfer_now,Agt,myBoard,Dest)
                   : actT(release_object,Agt,myBoard) // actT(release_object,Agt,Dest)
         )
    ).


%  Make a salad in the specified container.  This involves selecting
%  appropriate vegetables, chopping them, placing them in the container,
%  and mixing briefly.
proc(actT(make,Agt,salad,Dest),
       exists(agt,exists(obj, ?obj_is_type(obj,lettuce)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
         )
       >>
       exists(agt,exists(obj, ?obj_is_type(obj,tomato)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
         )
      //
      exists(agt,exists(obj, ?obj_is_type(obj,carrot)
                      : actT(acquire_object,agt,obj)
                      : ensure_chop_into(agt,obj,Dest)
                )
        )
    : exists(agt, ensure_agent_has_object(agt,Dest)
              : actT(begin_task,agt,actT(mix,agt,Dest,1))
%              : event_end_task(agt,actT(mix,agt,Dest,1))
              : actT(release_object,agt,Dest)
        )
    ).


%  Main control program - prepare a nice meal
proc(control,
     actT(make,joe,salad,bowl1)
    ).

proc(make_cake,
     actT(make,joe,cake,board1)
    ).

%  Tests the operation of the LNTP condition
proc(timerTest,
     actT(set_timer,joe,timer1,5)
     : actT(set_timer,richard,timer2,7)
     : event_ring_timer(timer2)
    ).

%  Tests the operation of concurrency with nondeterminism
proc(concTest,
     ensure_place_type_in(joe,egg,bowl1) // ensure_place_type_in(richard,egg,bowl2)
    ).

%  Test the operation of nondeterministic argument selection
proc(piTest,
     actT(acquire_object,joe,egg1)
     : exists(obj, ?and(obj_is_type(obj,egg),neg(state_agent_has_object(_,obj,now)))
               : actT(acquire_object,richard,board1)
               : actT(acquire_object,richard,obj)
         )
    ).

%  A simple little program for testing purposes
proc(simple(Agt),
     exists(obj, ?and(obj_is_type(obj,lettuce),not(used(obj,now)))
             : actT(acquire_object,Agt,obj))).


proc(control_fails,
     actT(make,_harriet,salad,bowl1)
    ).


testsit_sanity:- ol_do(control,s0).
testsit_all:- forall((proc(Test,Stuff),ground(Test),writeln(-), 
  wdmsg(proc(Test,Stuff))),
  (ol_do(Test,s0),writeln(-))).

:- fixup_exports.


/*

baseKB: ?-testsit_all.

-
% proc(control,actT(make,joe,salad,bowl1)).
do [actT(acquire_object,joe,lettuce1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,lettuce1,board1)] at 3
do [actT(begin_task,joe,actT(chop,joe,board1))] at 4
do [event_end_task(joe,actT(chop,joe,board1))] at 7
do [actT(acquire_object,joe,bowl1)] at 8
do [actT(transfer_now,joe,board1,bowl1)] at 9
do [actT(release_object,joe,board1)] at 10
do [actT(release_object,joe,bowl1)] at 11
do [actT(acquire_object,joe,tomato1),actT(acquire_object,richard,carrot1)] at 12
do [actT(acquire_object,joe,board1),actT(acquire_object,richard,board2)] at 13
do [actT(place_in,joe,tomato1,board1),actT(place_in,richard,carrot1,board2)] at 14
do [actT(begin_task,joe,actT(chop,joe,board1)),actT(begin_task,richard,actT(chop,richard,board2))] at 15
do [event_end_task(joe,actT(chop,joe,board1))] at 18
do [actT(acquire_object,joe,bowl1),event_end_task(richard,actT(chop,richard,board2))] at 26
do [actT(transfer_now,joe,board1,bowl1)] at 27
do [actT(release_object,joe,board1)] at 28
do [actT(release_object,joe,bowl1)] at 29
do [actT(acquire_object,richard,bowl1)] at 30
do [actT(transfer_now,richard,board2,bowl1)] at 31
do [actT(release_object,richard,board2)] at 32
do [actT(release_object,richard,bowl1)] at 33
do [actT(acquire_object,joe,bowl1)] at 34
do [actT(begin_task,joe,actT(mix,joe,bowl1,1))] at 35
do [event_end_task(joe,actT(mix,joe,bowl1,1))] at 36
do [actT(release_object,joe,bowl1)] at 37
SUCCEEDED!-
-
% proc(make_cake,actT(make,joe,cake,board1)).
do [actT(acquire_object,joe,egg1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,egg1,board1)] at 3
do [actT(release_object,joe,board1)] at 4
do [actT(acquire_object,joe,flour1)] at 5
do [actT(acquire_object,joe,board1)] at 6
do [actT(place_in,joe,flour1,board1)] at 7
do [actT(release_object,joe,board1)] at 8
do [actT(acquire_object,joe,sugar1)] at 9
do [actT(acquire_object,joe,board1)] at 10
do [actT(place_in,joe,sugar1,board1)] at 11
do [actT(release_object,joe,board1)] at 12
do [actT(acquire_object,joe,board1)] at 13
do [actT(begin_task,joe,actT(mix,joe,board1,5))] at 14
do [event_end_task(joe,actT(mix,joe,board1,5))] at 19
do [actT(release_object,joe,board1)] at 20
do [actT(acquire_object,joe,oven1)] at 21
do [actT(acquire_object,joe,board1)] at 22
do [actT(place_in,joe,board1,oven1)] at 23
do [actT(set_timer,joe,timerFor(cake,joe),35)] at 24
do [event_ring_timer(timerFor(cake,joe))] at 59
do [actT(transfer_now,joe,oven1,board1)] at 60
do [actT(release_object,joe,oven1)] at 61
do [actT(release_object,joe,board1)] at 62
SUCCEEDED!-
-
% proc(timerTest,
%      actT(set_timer,joe,timer1,5):actT(set_timer,richard,timer2,7):event_ring_timer(timer2)).
do [actT(set_timer,joe,timer1,5)] at 1
do [actT(set_timer,richard,timer2,7)] at 2
do [event_ring_timer(timer1)] at 6
do [event_ring_timer(timer2)] at 9
SUCCEEDED!-
-
% proc(concTest,
%      ensure_place_type_in(joe,egg,bowl1)//ensure_place_type_in(richard,egg,bowl2)).
do [actT(acquire_object,joe,egg1),actT(acquire_object,richard,egg2)] at 1
do [actT(acquire_object,joe,bowl1),actT(acquire_object,richard,bowl2)] at 2
do [actT(place_in,joe,egg1,bowl1),actT(place_in,richard,egg2,bowl2)] at 3
do [actT(release_object,joe,bowl1),actT(release_object,richard,bowl2)] at 4
SUCCEEDED!-
-
% proc(piTest,
%      actT(acquire_object,joe,egg1):exists(obj,
%           ?(and(obj_is_type(obj,egg),
%                 neg(state_agent_has_object(_,obj,now)))) : actT(acquire_object,richard,board1):actT(acquire_object,richard,obj))).
do [actT(acquire_object,joe,egg1)] at 1
do [actT(acquire_object,richard,board1)] at 2
do [actT(acquire_object,richard,egg2)] at 3
SUCCEEDED!-
-
% proc(control_fails,actT(make,_223812,salad,bowl1)).
do [actT(acquire_object,joe,lettuce1)] at 1
do [actT(acquire_object,joe,board1)] at 2
do [actT(place_in,joe,lettuce1,board1)] at 3
do [actT(begin_task,joe,actT(chop,joe,board1))] at 4
do [event_end_task(joe,actT(chop,joe,board1))] at 7
do [actT(acquire_object,joe,bowl1)] at 8
do [actT(transfer_now,joe,board1,bowl1)] at 9
do [actT(release_object,joe,board1)] at 10
do [actT(release_object,joe,bowl1)] at 11
do [actT(acquire_object,joe,tomato1),actT(acquire_object,richard,carrot1)] at 12
do [actT(acquire_object,joe,board1),actT(acquire_object,richard,board2)] at 13
do [actT(place_in,joe,tomato1,board1),actT(place_in,richard,carrot1,board2)] at 14
do [actT(begin_task,joe,actT(chop,joe,board1)),actT(begin_task,richard,actT(chop,richard,board2))] at 15
do [event_end_task(joe,actT(chop,joe,board1))] at 18
do [actT(acquire_object,joe,bowl1),event_end_task(richard,actT(chop,richard,board2))] at 26
do [actT(transfer_now,joe,board1,bowl1)] at 27
do [actT(release_object,joe,board1)] at 28
do [actT(release_object,joe,bowl1)] at 29
do [actT(acquire_object,richard,bowl1)] at 30
do [actT(transfer_now,richard,board2,bowl1)] at 31
do [actT(release_object,richard,board2)] at 32
do [actT(release_object,richard,bowl1)] at 33
do [actT(acquire_object,joe,bowl1)] at 34
do [actT(begin_task,joe,actT(mix,joe,bowl1,1))] at 35
do [event_end_task(joe,actT(mix,joe,bowl1,1))] at 36
do [actT(release_object,joe,bowl1)] at 37
SUCCEEDED!-
% with pending residual goals
{_49346=_49370}.

% ircEventNow("##logic","tolarz",say("i understand roughly")).
% unused(ircEventNow("##logic","tolarz",say("i understand roughly"))).

baseKB:  ?- 

*/

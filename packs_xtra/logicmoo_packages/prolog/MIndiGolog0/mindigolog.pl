%%
%%  mindigolog.pl:  transition semantics for MIndiGolog
%%
%%  Copyright 2005, Ryan Kelly
%%
%%    The predicate do(D,S,Sp) plans an execution of program D in an offline
%%    manner, printing the actions to be performed to standard output as well
%%    as returning them in the situation term Sp.
%%
%%    The predicate ol_do(D,S) executes the program D in an on-line manner,
%%    beginning in situation S.  This means that it will not backtrack over
%%    the actions executed during operation of the program.  Online execution
%%    is not guaranteed to succeed even if D has a possible legal execution.
%%    For the moment, the actions executed are printed to standard output.
%%
%%    In general, the executions returned by do/3 will not be complete,
%%    but will be in the form of a final situation where the occurance times
%%    of actions are constrained in some way.  By contrast, ol_do/2 performs
%%    each action at the earliest possible time.
%%


%%
%%  final(D,S):  program termination is possible
%%
%%  The predicate final/2 is true when program D may legally terminate
%%  in situation S.  It is typically defined recursively for higher-level
%%  program constructs.  The specifics of each individual clause are
%%  explained below.
%%

%%  It is always legal for an empty program to terminate
final(nil,_).

%%  It is never legal for a program consisting of a single action
%%  to terminate, hence there is no clause for this case.

%%  It is never legal for a program consisting of a test to terminate,
%%  hence there is no clause for this case.

%%  Sequential performance of two programs may terminate only if both
%%  programs may terminate.
final(seq(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Nondeterministic choice between two programs may terminate if either
%%  program may terminate.
final(choice(D1,D2),S) :-
    final(D1,S)
    ;
    final(D2,S).

%%  Nondeterministic choice of arguments may terminate if there is some
%%  choice of arguments for which the program will terminate.
final(pi(V,D),S) :-
    sub(V,_,D,Dr), final(Dr,S).

%%  Iteration of a program may always terminate, as it can legally be
%%  executed zero times.
final(star(_),_).

%%  Synchronised-if may terminate if the test is true and the true option
%%  may terminate, or the test is false and the false option may terminate.
final(if(Cond,D1,D2),S) :-
    holds(Cond,S), final(D1,S)
    ;
    holds(neg(Cond),S), final(D2,S).

%%  Synchronised-while may terminate if the test is false, or if the
%%  loop program may terminate.
final(while(Cond,D),S) :-
    holds(neg(Cond),S)
    ;
    final(D,S).

%%  Concurrent execution of two programs may terminate if both programs
%%  may terminate.
final(conc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Prioritised concurrent execution of two programs may terminate if both
%%  programs may terminate.
final(pconc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Concurrent iteration of a program may terminate in any situation, as
%%  it may be executed zero times.
final(cstar(_),_).

%%  A procedure call may terminate if the corresponding body, with arguments
%%  substituted appropriately, may terminate.
final(pcall(PArgs),S) :-
    sub(now,S,PArgs,PArgsS), proc(PArgsS,P), final(P,S).

%%  An offline search can terminate if the program to be searched can
%%  terminate.
final(search(D),S) :-
    final(D,S).

%%  A program is also final if it contains syntactic sugar, and is equivalent
%%  to a program that is final.
final(D,S) :-
    syn_sugar(D,Ds),
    final(Ds,S).



%%
%%  trans(D,S,Dp,Sp):  program transition is possible
%%
%%  The predicate trans/4 is true when it is possible for situation S
%%  to evolve to situation Sp by executing the first part of program D.
%%  Dp is the part of D that remains to be executed in situation Sp.
%%
%%  Thus, trans/4 defines how execution of a program may be single-stepped
%%  from one situation to another.  The specifics of each individual
%%  clause are explained below.
%%

%%  It is never legal to transition an empty program, hence there
%%  is no clause for this case.

%%  A program consisting of a single (possibly concurrent) action may
%%  transition in several ways, depending on the natural actions which
%%  may occur in situation S.
%%
%%  * If situation S has no LNTP, perform the action at any time and
%%    set Dp to the empty program
%%  * If situation S has an LNTP, there are three possible transitions:
%%      * If the action has no natural actions and it is possible to
%%        do it before the LNTP, do so and set Dp to the empty program
%%      * Do the natural actions at the predicted time, leaving the
%%        program unaltered
%%      * Do the given action and the natural actions concurrently
%%        at the LNTP, and set Dp to the empty program
%%
trans(C,S,Dp,Sp) :-
    sub(now,S,C,CS), to_cact(CS,CA), start(S,SStart),
    ( lntp(S,LNTP) ->
      (
        % Get the list of LNTP actions
        findall(NA,(natural(NA),poss(NA,LNTP,S)),NActs),
        (
          % Can do them before the LNTP actions
          % This requires that no actions in the set are natural
          ( 
            \+ ( member(A,CA), natural(A) ),
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

%%  A test may transition to the empty program if it holds, leaving the
%%  situation unaltered.
trans(test(Cond),S,Dp,Sp) :-
    holds(Cond,S), S=Sp, Dp=nil
    ;
    lntp(S,LNTP),
    findall(NA,(natural(NA),poss(NA,LNTP,S)),NActs),
    Sp = do(NActs,LNTP,S), Dp = test(Cond).

%%  Sequential execution of two programs may transition by transitioning
%%  the first program, leaving the remainder the be executed in sequence
%%  with the second.  If the first program may terminate, it is also legal
%%  to transition the second program.
trans(seq(D1,D2),S,Dp,Sp) :-
    trans(D1,S,D1r,Sp), Dp = seq(D1r,D2).
trans(seq(D1,D2),S,Dp,Sp) :-
    final(D1,S), trans(D2,S,Dp,Sp).

%%  Nondeterministic choice of programs may transition if either of the
%%  programs may transition.
trans(choice(D1,D2),S,Dp,Sp) :-
    trans(D1,S,Dp,Sp) ; trans(D2,S,Dp,Sp).

%%  Nondeterministic choice of arguments may transition if there is an
%%  appropriate binding of the arguments for which the program may transition.
trans(pi(V,D),S,Dp,Sp) :-
    sub(V,_,D,Dr), step(Dr,S,Dp,Sp).

%%  Iteration of a program may transition to the program followed by further
%%  iteration, provided that the program may transition.
trans(star(D),S,Dp,Sp) :-
    Dp = seq(Dr,star(D)), trans(D,S,Dr,Sp).

%%  Synchronised-if may transition if the test is true and the true option
%%  may transition, or the test is false and the false option may transition.
trans(if(Cond,D1,D2),S,Dp,Sp) :-
    holds(Cond,S), trans(D1,S,Dp,Sp)
    ;
    holds(neg(Cond),S), trans(D2,S,Dp,Sp).

%%  Syncrhonised-while may transition to the loop program in sequence
%%  with another loop, as long as the test condition holds and the loop
%%  program may transition.
trans(while(Cond,D),S,Dp,Sp) :-
    Dp = seq(Dr,while(Cond,D)), holds(Cond,S), trans(D,S,Dr,Sp).

%%  Concurrent execution may transition in three ways:
%%
%%    * Transition the first program, leaving its remainder to be
%%      executed concurrently with the second program
%%    * Transition the second program, leaving its remainder to be
%%      executed concurrently with the first program
%%    * Find a concurrent action that will transition the first program
%%      and a concurrent action that will transition the second program,
%%      and perform both concurrently.  The remaining program is the
%%      concurrent execution of the remainders of the individual programs
%%

trans_true_conc(conc(D1,D2),S,Dp,Sp) :-
    step(D1,S,Dr1,do(C1,T,S)),
    step(D2,S,Dr2,do(C2,T,S)),
    % TODO:  if one part is waiting for a natural action, do the other first.
    %        This will produce solutions in which as much as possible is done
    %        while waiting for a natural actions.  Such solutions are already
    %        entailed but are not the first solution encountered.
    \+ ( member(A,C1), member(A,C2), actor(A,_) ),
    union(C1,C2,CT), trans(CT,S,nil,Sp),
    Dp = conc(Dr1,Dr2).

trans(conc(D1,D2),S,Dp,Sp) :-
    trans_true_conc(conc(D1,D2),S,Dp,Sp)
    ;
    Dp = conc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = conc(D1,Dr2), trans(D2,S,Dr2,Sp).

%%  Prioritised concurrent execution may transition by transitioning the
%%  first program, leaving its remainder to be executed in pconc with
%%  the second program.  If it is not possible to transition the first
%%  program, then it is also legal to transition the second program leaving
%%  the first program to be executed in pconc with its remainder.
trans(pconc(D1,D2),S,Dp,Sp) :-
    Dp = pconc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = pconc(D1,Dr2), trans(D2,S,Dr2,Sp), \+ trans(D1,S,_,_).

%%  Concurrent execution of a program may transition to the program
%%  being concurrently executed with more concurrent iterations of it,
%%  provided that the program can transition.
trans(cstar(D),S,Dp,Sp) :-
    Dp = conc(Dr,cstar(D)), trans(D,S,Dr,Sp).

%%  A proceduce call may transition if the body program, with arguments
%%  substituted in, may transition.
trans(pcall(PArgs),S,Dp,Sp) :-
    sub(now,S,PArgs,PArgsS),
    proc(PArgsS,P), trans(P,S,Dp,Sp).

%%  TODO:  a search() operator similar to IndiGolog.

%%  A program may also transition if it contains syntactic sugar, and is
%%  equivalent to a program that may transition.
trans(D,S,Dp,Sp) :-
    syn_sugar(D,Ds),
    trans(Ds,S,Dp,Sp).



%%
%%  syn_sugar(Din,Dout):  syntactic sugar for programs
%%
%%  This predicate is used to make writing MIndiGolog programs easer,
%%  by providing a level of syntax transformation into the canonical
%%  program form.  If Din is a program containing an element of this
%%  sugar, Dout is unified with the canonicalised version.
%%

%%  Sequential execution can be represented by an infix :
syn_sugar(D1 : D2,seq(D1,D2)).
%%  Nondeterministic choice can be represented by an infix /
syn_sugar(D1 / D2,choice(D1,D2)).
%%  Concurrent execution can be represented by an infix //
syn_sugar(D1 // D2,conc(D1,D2)).
%%  Prioritised concurrent execution can be represented by an infix >>
syn_sugar(D1 >> D2,pconc(D1,D2)).
%%  Condition tests can be represented by a prefix ?
syn_sugar(?C,test(C)).
%%  Procedure calls can be represented by name
syn_sugar(Proc,pcall(Proc)) :-
    proc(Proc,_).


%%
%%  holds(Cond,S):  check whether a condition holds in a situation
%%
%%  This predicate is used to evaluate reified condition terms from
%%  MIndiGolog programs.  It recursively reduces the formula to equivalent
%%  forms which can be tested directly by the prolog theorem prover,
%%  and hence includes the standard prolog negation-as-failure semantics.
%%  
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



%%
%%  sub(Name,Value,Old,New):  substitue values in a term
%%
%%  This predicate is true when New is equal to Old with all occurances
%%  of Name replaced by Value - basically, a symbolic substitution
%%  routine.  For example, it is usually used to produce a result such
%%  as:
%%
%%      sub(now,S,fluent(now),fluent(S)).
%%
sub(_,_,T,Tr) :-
    var(T), Tr = T.
sub(X,Y,T,Tr) :-
    \+ var(T), T = X, Tr = Y.
sub(X,Y,T,Tr) :-
    T \= X, T =.. [F|Ts], sub_list(X,Y,Ts,Trs), Tr =.. [F|Trs].

%%
%%  sub_list(Name,Value,Old,New):  value substitution in a list
%%
%%  This predicate operates as sub/4, but Old and New are lists of terms
%%  instead of single terms.  Basically, it calls sub/4 recursively on
%%  each element of the list.
%%
sub_list(_,_,[],[]).
sub_list(X,Y,[T|Ts],[Tr|Trs]) :-
    sub(X,Y,T,Tr), sub_list(X,Y,Ts,Trs).



%%
%%  trans*(D,S,Dp,Sp):  Transitive Closure of Transition Rules
%%
%%  This predicate is true if Dp,Sp are in the transitive closure of
%%  the trans/4 predicate for D,S.  It is a simplistic encoding of the
%%  transitive closure in prolog using a recursive definition.
%%
trans*(D,S,D,S).
trans*(D,S,Dp,Sp) :-
    trans(D,S,Dr,Sr),
    trans*(Dr,Sr,Dp,Sp).


%%
%%  step(D,S,Dp,Sp):  single-step a program
%%
%%  This predicate takes a program D and a situation S in which to execute it,
%%  and returns a new situation Sp and remaining program Dp such that the
%%  execution of D has progressed by a single action.  It may be used
%%  repeatedly to find a possible next action to perform for a given program.
%%
step(D,S,Dp,Sp) :-
    %%  Naive implementation is simply:  trans*(D,S,Dp,do(C,T,S))
    %%  This implementation is more efficient as it does not generate
    %%  transitions that go beyond one action from S (which will always fail).
    Sp = do(_,_,S), trans(D,S,Dp,Sp)
    ;
    trans(D,S,Dr,S), step(Dr,S,Dp,Sp).


%%
%%  do(D,S,Sp):  offline execution of MIndiGolog Programs
%%
%%  This predicate takes a program D and starting situation S, and
%%  finds a new situation Sp which can be legally reached by executing
%%  program D to termination.  The actions executed can be retreived
%%  from the situation Sp.
%%
%%  This predicate is capable of backtracking over action choices to
%%  find a legal execution, and so is not suitable for executing programs
%%  on-line.
%%
do(D,S,Sp) :-
    trans*(D,S,Dp,Sp),
    final(Dp,Sp),
    show_action_history(Sp).

%%  Temporary predicate for printing the action history of a situation,
%%  for debugging purposes only.
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


%%
%%  ol_do(D,S):  execute a program in an on-line manner
%%
%%  This predicate is similar to do/4 except it does not backtrack
%%  over the selection of actions to perform.  Thus, the actions
%%  selected by this predicate are suitable to be performed in online
%%  execution of the program.  However, this also means that successful
%%  termination of the program is not guaranteed.
%%
%%  At this stage it simply prints each action performed to stdout,
%%  performing each at the minimum possible time.
%%
ol_do(D,S) :-
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


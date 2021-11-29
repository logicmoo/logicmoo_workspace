
:- multifile message_property/2.


compileAlternative(Branch, Guard, T, Code) :-
    compileBranch(Branch, Guard, T, Code).
compileAlternative((Branch;Alternative), Guard, T, Code) :-
    compileBranch(Branch, Guard, T, P1),
    compileAlternative(Alternative, Guard, T, P2),
    append(P1, P2, Code).

:- dynamic resource/3.
:- multifile resource/3.


:- dynamic goal_expansion/4.
:- multifile goal_expansion/4.


if(P, then(Q)) :-
    (   call(P)
    ->  call(Q)
    ;   true
    ).

remove(C, P) :-
    retractall(instance(C, P)).

react(Model) :-
    for_each((Stream(Thread), T:::Instruction),
             such_that(ist(Model(Stream)o Thread,
                           (clock(T), T:::Instruction))),
             do(Model(Stream)o Thread>>>(T:::Instruction))).

sense(Model) :-
    if(interrupt(Stream(Interrupt)),
       then((remove(Model(Stream)o _, clock(@_)), remove(Model(Stream)o _, excite(@_)), 
           remove(Model(Stream)o _, inhibit(@_)), remove(Model(Stream)o _, signal(@_)), 
           for_each(sensor(@X), 
              such_that(member(sensor(@X), Interrupt)), 
              do((set(Model(Stream)o sense(@X), clock(1)), 
                write(0:::sense(@X):::sensor(@X)), nl))), 
           set(Model(Stream), seq(1)), 
           remove(Model(Stream), _:::_:::_)))).

ist(C, true).
ist(C, P) :-
    (   instance(C, P)
    ;   instance(C, Q=>P),
        ist(C, Q)
    ).
ist(C,  (P, Q)) :-
    ist(C, P),
    ist(C, Q).

compileSequence([Instruction|Tree], Guard, T, Code) :-
    T1 is T+1,
    compileInstruction(Instruction, Guard, T, P1),
    compileTree(Tree, Guard, T1, P2),
    append(P1, P2, Code).

:- multifile prolog_predicate_name/2.


:- dynamic term_expansion/4.
:- multifile term_expansion/4.


:- dynamic library_directory/1.
:- multifile library_directory/1.


reflect(Model) :-
    for_each((Stream, I:::Thread:::Stimulus),
             such_that(ist(Model(Stream),
                           I:::Thread:::Stimulus)),
             do(Model(Stream)>>>(I:::Thread:::Stimulus))).

%   NOTE: system definition has been overruled for instance/2
:- dynamic instance/2.


run(Model) :-
    loop((sense(Model), react(Model), reflect(Model))).

Model(Stream)>>>(I:::Thread:::Stimulus) :-
    write(I:::Thread:::Stimulus),
    nl,
    remove(Model(Stream), I:::Thread:::Stimulus).
Model(Stream)oP(@X)>>>(T:::fire(Q(@Y))) :-
    T1 is T+1,
    set(Model(Stream)oQ(@Y), clock(1)),
    set(Model(Stream)oP(@X), clock(T1)).
Model(Stream)oP(@X)>>>(T:::resume(P(@X))) :-
    set(Model(Stream)oP(@X), clock(1)).
Model(Stream)oP(@X)>>>(T:::end) :-
    remove(Model(Stream)oP(@X), clock(T)).
Model(Stream)oP(@X)>>>(T:::send(Q(@Y))) :-
    T1 is T+1,
    if_not(ist(Model(Stream)oQ(@Y), clock(_)),
           then(set(Model(Stream)oQ(@Y), clock(1)))),
    if_not(ist(Model(Stream), weight(P(@X), Q(@Y))o W),
           then(if(ist(Model(Stream),
                       initial(P(@X), Q(@Y))o W),
                   then(set(Model(Stream),
                            weight(P(@X), Q(@Y))o W)),
                   else(set(Model(Stream),
                            weight(P(@X), Q(@Y))o 0))))),
    if_not(ist(Model(Stream)oP(@X), signal(send(Q(@Y)))),
           then(insert(Model(Stream)oP(@X),
                       signal(send(Q(@Y)))))),
    set(Model(Stream)oP(@X), clock(T1)).
Model(Stream)oP(@X)>>>(T:::receive(Q(@Y))) :-
    T1 is T+1,
    if(ist(Model(Stream)oQ(@Y), signal(send(P(@X)))),
       then(if((ist(Model(Stream), weight(Q(@Y), P(@X))o K), K>0),
               then(set(Model(Stream)oP(@X), clock(T1)))))).
Model(Stream)oP(@X)>>>(T:::merge(Q(@Y))) :-
    T1 is T+1,
    if_not(ist(Model(Stream)oP(@X), signal(merge(Q(@Y)))),
           then(insert(Model(Stream)oP(@X),
                       signal(merge(Q(@Y)))))),
    set(Model(Stream)oP(@X), clock(T1)).
Model(Stream)oP(@X)>>>(T:::join(Q(@Y))) :-
    T1 is T+1,
    if(ist(Model(Stream)oQ(@Y), signal(merge(P(@X)))),
       then(set(Model(Stream)oP(@X), clock(T1)))).
Model(Stream)o Thread>>>(T:::increment(weight(P(F(X)), Q(Y)))) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    if_not(ist(Model(Stream), weight(P(F(X)), Q(Y))o W),
           then(if(ist(Model(Stream),
                       initial(P(F(X)), Q(Y))o W),
                   then(set(Model(Stream),
                            weight(P(F(X)), Q(Y))o W)),
                   else(set(Model(Stream),
                            weight(P(F(X)), Q(Y))o 0))))),
    if((ist(Model(Stream), weight(P(F(X)), Q(Y))o W), W<1),
       then((W1 is W+1, insert(Model(Stream), I:::Thread:::weight(P(F(_)), Q(Y))o W1), set(Model(Stream), weight(P(F(_)), Q(Y))o W1)))),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::decrement(weight(P(F(X)), Q(Y)))) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    if_not(ist(Model(Stream), weight(P(F(X)), Q(Y))o W),
           then(if(ist(Model(Stream),
                       initial(P(F(X)), Q(Y))o W),
                   then(set(Model(Stream),
                            weight(P(F(X)), Q(Y))o W)),
                   else(set(Model(Stream),
                            weight(P(F(X)), Q(Y))o 0))))),
    if((ist(Model(Stream), weight(P(F(X)), Q(Y))o W), W>0),
       then((W1 is W-1, insert(Model(Stream), I:::Thread:::weight(P(F(_)), Q(Y))o W1), set(Model(Stream), weight(P(F(_)), Q(Y))o W1)))),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::step(Y)) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    if(Y=forward, then(F=right), else(F=left)),
    if(ist(Model(_), at(F(X))),
       then(X1 is X+1),
       else(X1 is 1)),
    if(X1<8,
       then(set(Model(_), at(F(X1)))),
       else(remove(Model(_), at(_)))),
    I1 is I+1,
    set(Model(Stream), seq(I1)),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::on(X)) :-
    T1 is T+1,
    if(ist(Model(_), at(X)),
       then(set(Model(Stream), on(X))),
       else(remove(Model(Stream), on(_)))),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::choice(X)) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    random(R, X),
    set(Model(Stream)o Thread, fetch(R)),
    insert(Model(Stream), I:::Thread:::fetch(R)),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::check(P(@X))) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    remove(Model(Stream)o Thread, excite(P(@X))),
    remove(Model(Stream)o Thread, inhibit(P(@X))),
    if(ist(Model(Stream), P(@X)),
       then((set(Model(Stream)o Thread, excite(P(@X))), insert(Model, Stream(I):::Thread:::excite(P(@X))), insert(Model(Stream), I:::Thread:::excite(P(@X))))),
       else((set(Model(Stream)o Thread, inhibit(P(@X))), insert(Model(Stream), I:::Thread:::inhibit(P(@X))), insert(Model, Stream(I):::Thread:::inhibit(P(@X)))))),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::scan(detect(F(X)), move(Y))) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    remove(Model(Stream)o Thread,
           sync(detect(F(X)), move(Y))),
    if((ist(Model, Stream(I1):::detect(F(X)):::excite(on(F(X)))), ist(Model, Stream(I2):::move(Y):::excite(at(F(X)))), I1=I2),
       then((set(Model(Stream)o Thread, sync(detect(F(X)), move(Y))), insert(Model(Stream), I:::Thread:::sync(detect(F(X)), move(Y)))))),
    set(Model(Stream)o Thread, clock(T1)).
Model(Stream)o Thread>>>(T:::effector(P)) :-
    T1 is T+1,
    ist(Model(Stream), seq(I)),
    insert(Model(Stream), I:::Thread:::effector(P)),
    set(Model(Stream)o Thread, clock(T1)).

threads(robot(cleaner(A, B))):::[
    thread(sense(F(X)), [merge(ltd(sense(F(X)), learn(F))), 
    merge(ltp(sense(F(X)), recall(Y))), 
    send(learn(F)), fire(detect(F(X))), 
    send(recall(Y))]), thread(detect(F(X)), 
    [on(F(X)), check(on(F(X))),  
    ((excite(on(F(X)))| [effector(clear(F(X))), 
      fire(synchro(detect(F(X)), move(Y)))]);
      (inhibit(on(F(X)))| [resume(detect(F(X)))]))]), 
    thread(learn(F), [receive(sense(F(X))), choice([A, B]),  
     ((fetch(A)| [fire(move(A))]);(fetch(B)| [fire(move(B))]))]),
     thread(synchro(detect(F(X)), move(Y)), [scan(detect(F(X)), move(Y)),  
     (sync(detect(F(X)), move(Y))| [fire(ltp(sense(F(X)), recall(Y)))])]), 
     thread(recall(Y), [receive(sense(F(X))), fire(ltd(sense(F(X)), learn(F))), 
     fire(move(Y))]), thread(move(Y), [step(Y), check(at(F(_))),  
     ((excite(at(F(_)))| [resume(move(Y))]);(inhibit(at(F(_)))|
     [effector(stop(Y))]))])].

threads(robot):::[thread(ltp(Q, R), [join(Q), increment(weight(Q, R))]), thread(ltd(Q, R), 
   [join(Q), decrement(weight(Q, R))])].

weights(robot(cleaner(A, B))):::[initial(sense(_), learn(_))o 1, initial(recall(_), do(_))o 1].


insert(C, P) :-
    assert(instance(C, P)).


compileTree([], Guard, T, [Guard=>T:::end]).
compileTree(Sequence, Guard, T, Code) :-
    compileSequence(Sequence, Guard, T, Code).
compileTree([Alternative], Guard, T, Code) :-
    compileAlternative(Alternative, Guard, T, Code).

compile(Tree, Code) :-
    compileTree(Tree, true, 1, Code).


interrupt(P) :- get_code(C),(   C=13 ->  nl, read(P) ;   false ).

for_each(X, such_that(F), do(P)) :-
    findall(X, F, L),
    forall(member(X, L), P).
for_each(X, from(F), do(P)) :-
    forall(F:::L, forall(member(X, L), P)).




set(C, F(@X)) :- remove(C, F(@_)), insert(C, F(@X)).

loop(P) :- repeat,call(( P, !)), fail.

new(C) :- retractall(instance(C, _)).

compileBranch((Guard1| Tree), Guard2, T, Code) :-
    ( Guard2=true ->  Guard=Guard1 ;   Guard=(Guard1, Guard2)),
    compileTree(Tree, Guard, T, Code).



load(Model) :-
    new(_),
    for_each(thread(Thread, Tree),
             from(threads(Model(Fiber))),
             do((compile(Tree, Code), 
               forall(member(P, Code), 
               insert(Model(Fiber)o Thread, P))))),
    for_each(thread(Thread, Tree),
             from(threads(Model)),
             do((compile(Tree, Code), 
               forall(member(P, Code), 
               insert(Model(_)o Thread, P))))),
    for_each(Weight(Thread1, Thread2)o@X,
             from(weights(Model(Fiber))),
             do(insert(Model(Fiber),
                       Weight(Thread1, Thread2)o@X))).

compileInstruction(P(@X), Guard, T, [Guard=>T:::P(@X)]) :-
    instruction(P).

random(X, L) :-
    length(L, N),
    K is random(N),
    nth0(K, L, X).

:- dynamic exception/3.
:- multifile exception/3.


:- dynamic term_expansion/2.
:- multifile term_expansion/2.


if(P, then(Q), else(R)) :-
    (   call(P)
    ->  call(Q)
    ;   call(R)
    ).

instruction(fire).
instruction(resume).
instruction(end).
instruction(exit).
instruction(send).
instruction(receive).
instruction(join).
instruction(merge).
instruction(increment).
instruction(decrement).
instruction(step).
instruction(on).
instruction(choice).
instruction(check).
instruction(scan).
instruction(effector).

if_not(P, then(Q)) :-
    (   call(P)
    ->  true
    ;   call(Q)
    ).

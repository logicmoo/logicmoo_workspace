load(Model) :-
    new(_),
    for_each([thread, Thread, Tree],
             from([threads, [Model, Fiber]]),
             do((compile(Tree, Code), forall(member(P, Code), insert([Model, Fiber|Thread], P))))),
    for_each([thread, Thread, Tree],
             from([threads, Model]),
             do((compile(Tree, Code), forall(member(P, Code), insert([Model, _|Thread], P))))),
    for_each([Weight, Thread1, Thread2|X],
             from([weights, [Model, Fiber]]),
             do(insert([Model, Fiber],
                       [Weight, Thread1, Thread2|X]))).
run(Model) :-
    loop(([sense, Model], react(Model), reflect(Model))).

[sense, Model] :-
    if(interrupt([Stream, Interrupt]),
       then((remove([Model, Stream|_], [clock, _]), remove([Model, Stream|_], [excite, _]), remove([Model, Stream|_], [inhibit, _]), remove([Model, Stream|_], [signal, _]), for_each([sensor, X], such_that(member([sensor, X], Interrupt)), do((set([Model, Stream, sense, X], [clock, 1]), write(0:::[sense, X]:::[sensor, X]), nl))), set([Model, Stream], [seq, 1]), remove([Model, Stream], _:::_:::_)))).

react(Model) :-
    for_each(([Stream, Thread], T:::Instruction),
             such_that(ist([Model, Stream|Thread],
                           ([clock, T], T:::Instruction))),
             do([Model, Stream|Thread]>>>(T:::Instruction))).

reflect(Model) :-
    for_each((Stream, I:::Thread:::Stimulus),
             such_that(ist([Model, Stream],
                           I:::Thread:::Stimulus)),
             do([Model, Stream]>>>(I:::Thread:::Stimulus))).
[Model, Stream]>>>(I:::Thread:::Stimulus) :-
    write(I:::Thread:::Stimulus),
    nl,
    remove([Model, Stream],
           I:::Thread:::Stimulus).
[Model, Stream, P, X]>>>(T:::[fire, [Q, Y]]) :-
    T1 is T+1,
    set([Model, Stream, Q, Y], [clock, 1]),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(_T:::[resume, [P, X]]) :-
    set([Model, Stream, P, X], [clock, 1]).
[Model, Stream, P, X]>>>(T:::end) :-
    remove([Model, Stream, P, X], [clock, T]).
[Model, Stream, P, X]>>>(T:::[send, [Q, Y]]) :-
    T1 is T+1,
    if_not(ist([Model, Stream, Q, Y], [clock, _]),
           then(set([Model, Stream, Q, Y], [clock, 1]))),
    if_not(ist([Model, Stream],
               [weight, [P, X], [Q, Y]|W]),
           then(if(ist([Model, Stream],
                       [initial, [P, X], [Q, Y]|W]),
                   then(set([Model, Stream],
                            [ weight,
                              [P, X],
                              [Q, Y]
                            | W
                            ])),
                   else(set([Model, Stream],
                            [weight, [P, X], [Q, Y]|0]))))),
    if_not(ist([Model, Stream, P, X],
               [signal, [send, [Q, Y]]]),
           then(insert([Model, Stream, P, X],
                       [signal, [send, [Q, Y]]]))),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(T:::[receive, [Q, Y]]) :-
    T1 is T+1,
    if(ist([Model, Stream, Q, Y],
           [signal, [send, [P, X]]]),
       then(if((ist([Model, Stream], [weight, [Q, Y], [P, X]|K]), K>0),
               then(set([Model, Stream, P, X],
                        [clock, T1]))))).
[Model, Stream, P, X]>>>(T:::[merge, [Q, Y]]) :-
    T1 is T+1,
    if_not(ist([Model, Stream, P, X],
               [signal, [merge, [Q, Y]]]),
           then(insert([Model, Stream, P, X],
                       [signal, [merge, [Q, Y]]]))),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(T:::[join, [Q, Y]]) :-
    T1 is T+1,
    if(ist([Model, Stream, Q, Y],
           [signal, [merge, [P, X]]]),
       then(set([Model, Stream, P, X], [clock, T1]))).
[Model, Stream|Thread]>>>(T:::[increment, [weight, [P, [F, X]], [Q, Y]]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if_not(ist([Model, Stream],
               [weight, [P, [F, X]], [Q, Y]|W]),
           then(if(ist([Model, Stream],
                       [ initial,
                         [P, [F, X]],
                         [Q, Y]
                       | W
                       ]),
                   then(set([Model, Stream],
                            [ weight,
                              [P, [F, X]],
                              [Q, Y]
                            | W
                            ])),
                   else(set([Model, Stream],
                            [ weight,
                              [P, [F, X]],
                              [Q, Y]
                            | 0
                            ]))))),
    if((ist([Model, Stream], [weight, [P, [F, X]], [Q, Y]|W]), W<1),
       then((W1 is W+1, insert([Model, Stream], I:::Thread:::[weight, [P, [F, _]], [Q, Y]|W1]), set([Model, Stream], [weight, [P, [F, _]], [Q, Y]|W1])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[decrement, [weight, [P, [F, X]], [Q, Y]]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if_not(ist([Model, Stream],
               [weight, [P, [F, X]], [Q, Y]|W]),
           then(if(ist([Model, Stream],
                       [ initial,
                         [P, [F, X]],
                         [Q, Y]
                       | W
                       ]),
                   then(set([Model, Stream],
                            [ weight,
                              [P, [F, X]],
                              [Q, Y]
                            | W
                            ])),
                   else(set([Model, Stream],
                            [ weight,
                              [P, [F, X]],
                              [Q, Y]
                            | 0
                            ]))))),
    if((ist([Model, Stream], [weight, [P, [F, X]], [Q, Y]|W]), W>0),
       then((W1 is W-1, insert([Model, Stream], I:::Thread:::[weight, [P, [F, _]], [Q, Y]|W1]), set([Model, Stream], [weight, [P, [F, _]], [Q, Y]|W1])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[step, Y]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if(Y=forward, then(F=right), else(F=left)),
    if(ist([Model, _], [at, [F, X]]),
       then(X1 is X+1),
       else(X1 is 1)),
    if(X1<8,
       then(set([Model, _], [at, [F, X1]])),
       else(remove([Model, _], [at, _]))),
    I1 is I+1,
    set([Model, Stream], [seq, I1]),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[on, X]) :-
    T1 is T+1,
    if(ist([Model, _], [at, X]),
       then(set([Model, Stream], [on, X])),
       else(remove([Model, Stream], [on, _]))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[choice, X]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    random_e(R, X),
    set([Model, Stream|Thread], [fetch, R]),
    insert([Model, Stream],
           I:::Thread:::[fetch, R]),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[check, [P, X]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    remove([Model, Stream|Thread], [excite, [P, X]]),
    remove([Model, Stream|Thread],
           [inhibit, [P, X]]),
    if(ist([Model, Stream], [P, X]),
       then((set([Model, Stream|Thread], [excite, [P, X]]), insert(Model, [Stream, I]:::Thread:::[excite, [P, X]]), insert([Model, Stream], I:::Thread:::[excite, [P, X]]))),
       else((set([Model, Stream|Thread], [inhibit, [P, X]]), insert([Model, Stream], I:::Thread:::[inhibit, [P, X]]), insert(Model, [Stream, I]:::Thread:::[inhibit, [P, X]])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[scan, [detect, [F, X]], [move, Y]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    remove([Model, Stream|Thread],
           [sync, [detect, [F, X]], [move, Y]]),
    if((ist(Model, [Stream, I1]:::[detect, [F, X]]:::[excite, [on, [F, X]]]), ist(Model, [Stream, I2]:::[move, Y]:::[excite, [at, [F, X]]]), I1=I2),
       then((set([Model, Stream|Thread], [sync, [detect, [F, X]], [move, Y]]), insert([Model, Stream], I:::Thread:::[sync, [detect, [F, X]], [move, Y]])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[effector, P]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    insert([Model, Stream],
           I:::Thread:::[effector, P]),
    set([Model, Stream|Thread], [clock, T1]).
new(C) :-
    retractall(instance(C, _)).
insert(C, P) :-
    assert(instance(C, P)).
remove(C, P) :-
    retractall(instance(C, P)).
set(C, [F, X]) :-
    remove(C, [F, _]),
    insert(C, [F, X]).
ist(_C, true).
ist(C, P) :-
    (   instance(C, P)
    ;   instance(C, Q=>P),
        ist(C, Q)
    ).
ist(C,  (P, Q)) :-
    ist(C, P),
    ist(C, Q).
loop(P) :-
    repeat,
    call(( P,
           !
         )),
    fail.
interrupt(P) :-
    get_code(C),
    (   C=13
    ->  nl,
        read(P)
    ;   false
    ).
if(P, then(Q)) :-
    (   P
    ->  Q
    ;   true
    ).
if_not(P, then(Q)) :-
    (   P
    ->  true
    ;   Q
    ).
if(P, then(Q), else(R)) :-
    (   P
    ->  Q
    ;   R
    ).
for_each(X, such_that(F), do(P)) :-
    findall(X, F, L),
    forall(member(X, L), P).
for_each(X, from(F), do(P)) :-
    forall(F:::L, forall(member(X, L), P)).
random_e(X, L) :-
    length(L, N),
    K is random(N),
    nth0(K, L, X).
:- dynamic bonzon_op/1.

bonzon_op(thread).
bonzon_op(threads).
bonzon_op(weights).
bonzon_op(sense).
bonzon_op(clock).
bonzon_op(excite).
bonzon_op(inhibit).
bonzon_op(signal).
bonzon_op(sensor).
bonzon_op(seq).
bonzon_op(fire).
bonzon_op(resume).
bonzon_op(send).
bonzon_op(weight).
bonzon_op(initial).
bonzon_op(receive).
bonzon_op(merge).
bonzon_op(join).
bonzon_op(increment).
bonzon_op(decrement).
bonzon_op(step).
bonzon_op(at).
bonzon_op(on).
bonzon_op(choice).
bonzon_op(fetch).
bonzon_op(check).
bonzon_op(choice).
bonzon_op(clock).
bonzon_op(decrement).
bonzon_op(detect).
bonzon_op(move).
bonzon_op(scan).
bonzon_op(sync).
bonzon_op(effector).
bonzon_op(excite).
bonzon_op(fetch).
bonzon_op(fire).
bonzon_op(increment).
bonzon_op(inhibit).
bonzon_op(initial).
bonzon_op(join).
bonzon_op(merge).
bonzon_op(move).
bonzon_op(on).
bonzon_op(receive).
bonzon_op(resume).
bonzon_op(scan).
bonzon_op(send).
bonzon_op(sense).
bonzon_op(sensor).
bonzon_op(seq).
bonzon_op(signal).
bonzon_op(step).
bonzon_op(sync).
bonzon_op(thread).
bonzon_op(threads).
bonzon_op(weight).


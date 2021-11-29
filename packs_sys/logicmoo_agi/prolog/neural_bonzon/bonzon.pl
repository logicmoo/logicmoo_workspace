compile(Tree, Code) :-
    compileTree(Tree, true, 1, Code).

lis_to_pterm(_, X, X) :-
    \+ compound(X),
    !.
lis_to_pterm(_, X, X) :-
    \+ is_list(X),
    !.
lis_to_pterm(_, [N|A], Y) :-
    atom(N),
    upcase_atom(N, N),
    downcase_atom(N, N),
    is_list(A),
    compound_name_arguments(Y, N, A).
lis_to_pterm(_, [A, @B], [AA|B]) :-
    !,
    maybe_var(A, AA).
lis_to_pterm(_, [A, @, B], [A|B]) :-
    !.
lis_to_pterm(_, ['$VAR', A], ['$VAR'(A)]) :-
    !.
lis_to_pterm(_, [N|A], [N|A]) :-
    atom(N),
    instruction(N),
    !.
lis_to_pterm(_, [o, A, B], NN) :-
    !,
    append(A, B, NN).
lis_to_pterm(Ar, [N|A], Y) :-
    atom(N),
    bonzon_builtin(N/Ar),
    !,
    assertion(( atom(N),
                is_list(A)
              )),
    compound_name_arguments(Y, N, A).
lis_to_pterm(_, [N|A], [NN|A]) :-
    maybe_var(N, NN),
    !.
lis_to_pterm(_, [N|A], [N|A]) :-
    !.

load(A) :-
    new(B),
    for_each([thread, C, D],
             from([threads, [A, E]]),
             do((compile(D, F), forall(member(G, F), insert([A, E|C], G))))),
    for_each([thread, C, D],
             from([threads, A]),
             do((compile(D, F), forall(member(G, F), insert([A, B|C], G))))),
    for_each([H, I, J|K],
             from([weights, [A, E]]),
             do(insert([A, E], [H, I, J|K]))).

lpa_expansion(I, O) :-
    (   \+ compound(I)
    ;   I=(:-_),
        !,
        I=O
    ).
lpa_expansion(I, O) :-
    sub_term(E, I),
    is_list(E),
    !,
    I=O.
lpa_expansion(I, O) :-
    prolog_load_context(variable_names, Vs),
    copy_term(I:Vs, II:VVs),
    lpa_implode_varnames(VVs),
    term_variables(II, TVs),
    '__aux_maplist/2_=+1'(TVs, _),
    pterm_to_sterm(II, OO),
    unnumbervars(OO, O),
    nop((II\==OO->wdmsg(II);true)),
    format('~N'),
    pc(OO),
    !.

bonzon_bi(lpa_call/1).
bonzon_bi(apply/2).
bonzon_bi(wdmsg/1).
bonzon_bi((->)/2).
bonzon_bi(!/1).
bonzon_bi('[|]'/2).
bonzon_bi(('|')/2).
bonzon_bi('$VAR'/1).
bonzon_bi((',')/2).
bonzon_bi((:::)/2).
bonzon_bi((;)/2).
bonzon_bi((=)/2).
bonzon_bi((>>>)/2).
bonzon_bi((\+)/1).
bonzon_bi(assert/1).
bonzon_bi(atom/1).
bonzon_bi(call/1).
bonzon_bi(compile/2).
bonzon_bi(compileAlternative/4).
bonzon_bi(compileBranch/4).
bonzon_bi(compileInstruction/4).
bonzon_bi(compileSequence/4).
bonzon_bi(compileTree/4).
bonzon_bi(compound_name_arguments/3).
bonzon_bi(else/1).
bonzon_bi(findall/3).
bonzon_bi(for_each/3).
bonzon_bi(forall/2).
bonzon_bi(functor/3).
bonzon_bi(get_code/1).
bonzon_bi(if/2).
bonzon_bi(if/3).
bonzon_bi(if_not/2).
bonzon_bi(insert/2).
bonzon_bi(instance/2).
bonzon_bi(instruction/1).
bonzon_bi(interrupt/1).
bonzon_bi((is)/2).
bonzon_bi(is_list/1).
bonzon_bi(is_list/2).
bonzon_bi(ist/2).
bonzon_bi(length/2).
bonzon_bi(append/3).
bonzon_bi(lpa_functor/1).
bonzon_bi(load/1).
bonzon_bi(loop/1).
bonzon_bi(maplist/3).
bonzon_bi(member/2).
bonzon_bi((+)/2).
bonzon_bi((=>)/2).
bonzon_bi(from/1).
bonzon_bi(do/1).
bonzon_bi(new/1).
bonzon_bi(nth0/3).
bonzon_bi(random/1).
bonzon_bi(random/2).
bonzon_bi(random_e/2).
bonzon_bi(react/1).
bonzon_bi(read/1).
bonzon_bi(reflect/1).
bonzon_bi(remove/2).
bonzon_bi(retractall/1).
bonzon_bi(run/1).
bonzon_bi(such_that/1).
bonzon_bi(set/2).
bonzon_bi(then/1).
bonzon_bi(write/1).

compileTree([], Guard, T, [Guard=>T:::end]).
compileTree(Sequence, Guard, T, Code) :-
    compileSequence(Sequence, Guard, T, Code).
compileTree([Alternative], Guard, T, Code) :-
    compileAlternative(Alternative, Guard, T, Code).

term_expansion(I, P, O, P) :-
    notrace(current_prolog_flag(allow_variable_name_as_functor, true)),
    compound(I),
    nonvar(P),
    prolog_load_context(term, T),
    T==I,
    lpa_expansion(I, O),
    !.

pterm_to_sterm(In, In) :-
    \+ compound(In),
    !.
pterm_to_sterm('$VAR'(In), '$VAR'(In)) :-
    !.
pterm_to_sterm(In, Out) :-
    is_list(In),
    !,
    maybe_fix_list(In, Out).
pterm_to_sterm(In, Out) :-
    sub_term(E, In),
    is_list(E),
    !,
    In=Out.
pterm_to_sterm(In, Out) :-
    compound_name_arguments(In, N, A),
    functor(In, N, Ar),
    '__aux_maplist/3_pterm_to_sterm+0'(A, AA),
    lis_to_pterm(Ar, [N|AA], Mid),
    maybe_fix_list(Mid, Out).

maybe_var(A, _) :-
    \+ atom(A),
    !,
    fail.
maybe_var(A, AA) :-
    downcase_atom(A, A),
    !,
    AA=A.
maybe_var(A, '$VAR'(A)).

bonzon_builtin(F/A) :-
    nonvar(F),
    bonzon_bi(F/A),
    !.
bonzon_builtin(F/A) :-
    current_predicate(system:F/A),
    format('%~~ ~N~q.~n', [bonzon_bi(F/A)]).

:- dynamic instance/2.


for_each(X, such_that(F), do(P)) :-
    findall(X, lpa_call(F), L),
    \+ ( member(X, L),
         \+ call(P)
       ).
for_each(X, from(F), do(P)) :-
    \+ ( lpa_call(F:::L),
         \+ \+ ( member(X, L),
                 \+ lpa_call(P)
               )
       ).

[A, B]>>>(C:::D:::E) :-
    write(C:::D:::E),
    nl,
    remove([A, B], C:::D:::E).
[A, B, C, D]>>>(E:::[fire, [F, G]]) :-
    H is E+1,
    set([A, B, F, G], [clock, 1]),
    set([A, B, C, D], [clock, H]).
[A, B, C, D]>>>(_:::[resume, [C, D]]) :-
    set([A, B, C, D], [clock, 1]).
[A, B, C, D]>>>(E:::end) :-
    remove([A, B, C, D], [clock, E]).
[A, B, C, D]>>>(E:::[send, [F, G]]) :-
    H is E+1,
    if_not(ist([A, B, F, G], [clock, _]),
           then(set([A, B, F, G], [clock, 1]))),
    if_not(ist([A, B],
               [weight, [C, D], [F, G]|I]),
           then(if(ist([A, B],
                       [initial, [C, D], [F, G]|I]),
                   then(set([A, B],
                            [ weight,
                              [C, D],
                              [F, G]
                            | I
                            ])),
                   else(set([A, B],
                            [weight, [C, D], [F, G]|0]))))),
    if_not(ist([A, B, C, D],
               [signal, [send, [F, G]]]),
           then(insert([A, B, C, D],
                       [signal, [send, [F, G]]]))),
    set([A, B, C, D], [clock, H]).
[A, B, C, D]>>>(E:::[receive, [F, G]]) :-
    H is E+1,
    if(ist([A, B, F, G],
           [signal, [send, [C, D]]]),
       then(if((ist([A, B], [weight, [F, G], [C, D]|I]), I>0),
               then(set([A, B, C, D], [clock, H]))))).
[A, B, C, D]>>>(E:::[merge, [F, G]]) :-
    H is E+1,
    if_not(ist([A, B, C, D],
               [signal, [merge, [F, G]]]),
           then(insert([A, B, C, D],
                       [signal, [merge, [F, G]]]))),
    set([A, B, C, D], [clock, H]).
[A, B, C, D]>>>(E:::[join, [F, G]]) :-
    H is E+1,
    if(ist([A, B, F, G],
           [signal, [merge, [C, D]]]),
       then(set([A, B, C, D], [clock, H]))).
[A, B|C]>>>(D:::[increment, [weight, [E, [F, G]], [H, I]]]) :-
    J is D+1,
    ist([A, B], [seq, K]),
    if_not(ist([A, B],
               [weight, [E, [F, G]], [H, I]|L]),
           then(if(ist([A, B],
                       [ initial,
                         [E, [F, G]],
                         [H, I]
                       | L
                       ]),
                   then(set([A, B],
                            [ weight,
                              [E, [F, G]],
                              [H, I]
                            | L
                            ])),
                   else(set([A, B],
                            [ weight,
                              [E, [F, G]],
                              [H, I]
                            | 0
                            ]))))),
    if((ist([A, B], [weight, [E, [F, G]], [H, I]|L]), L<1),
       then((M is L+1, insert([A, B], K:::C:::[weight, [E, [F, N]], [H, I]|M]), set([A, B], [weight, [E, [F, N]], [H, I]|M])))),
    set([A, B|C], [clock, J]).
[A, B|C]>>>(D:::[decrement, [weight, [E, [F, G]], [H, I]]]) :-
    J is D+1,
    ist([A, B], [seq, K]),
    if_not(ist([A, B],
               [weight, [E, [F, G]], [H, I]|L]),
           then(if(ist([A, B],
                       [ initial,
                         [E, [F, G]],
                         [H, I]
                       | L
                       ]),
                   then(set([A, B],
                            [ weight,
                              [E, [F, G]],
                              [H, I]
                            | L
                            ])),
                   else(set([A, B],
                            [ weight,
                              [E, [F, G]],
                              [H, I]
                            | 0
                            ]))))),
    if((ist([A, B], [weight, [E, [F, G]], [H, I]|L]), L>0),
       then((M is L-1, insert([A, B], K:::C:::[weight, [E, [F, N]], [H, I]|M]), set([A, B], [weight, [E, [F, N]], [H, I]|M])))),
    set([A, B|C], [clock, J]).
[A, B|C]>>>(D:::[step, E]) :-
    F is D+1,
    ist([A, B], [seq, G]),
    if(E=forward, then(H=right), else(H=left)),
    if(ist([A, I], [at, [H, J]]),
       then(K is J+1),
       else(K is 1)),
    if(K<8,
       then(set([A, I], [at, [H, K]])),
       else(remove([A, I], [at, I]))),
    L is G+1,
    set([A, B], [seq, L]),
    set([A, B|C], [clock, F]).
[A, B|C]>>>(D:::[on, E]) :-
    F is D+1,
    if(ist([A, G], [at, E]),
       then(set([A, B], [on, E])),
       else(remove([A, B], [on, G]))),
    set([A, B|C], [clock, F]).
[A, B|C]>>>(D:::[choice, E]) :-
    F is D+1,
    ist([A, B], [seq, G]),
    random_e(H, E),
    set([A, B|C], [fetch, H]),
    insert([A, B], G:::C:::[fetch, H]),
    set([A, B|C], [clock, F]).
[A, B|C]>>>(D:::[check, [E, F]]) :-
    G is D+1,
    ist([A, B], [seq, H]),
    remove([A, B|C], [excite, [E, F]]),
    remove([A, B|C], [inhibit, [E, F]]),
    if(ist([A, B], [E, F]),
       then((set([A, B|C], [excite, [E, F]]), insert(A, [B, H]:::C:::[excite, [E, F]]), insert([A, B], H:::C:::[excite, [E, F]]))),
       else((set([A, B|C], [inhibit, [E, F]]), insert([A, B], H:::C:::[inhibit, [E, F]]), insert(A, [B, H]:::C:::[inhibit, [E, F]])))),
    set([A, B|C], [clock, G]).
[A, B|C]>>>(D:::[scan, [detect, [E, F]], [move, G]]) :-
    H is D+1,
    ist([A, B], [seq, I]),
    remove([A, B|C],
           [sync, [detect, [E, F]], [move, G]]),
    if((ist(A, [B, J]:::[detect, [E, F]]:::[excite, [on, [E, F]]]), ist(A, [B, K]:::[move, G]:::[excite, [at, [E, F]]]), J=K),
       then((set([A, B|C], [sync, [detect, [E, F]], [move, G]]), insert([A, B], I:::C:::[sync, [detect, [E, F]], [move, G]])))),
    set([A, B|C], [clock, H]).
[A, B|C]>>>(D:::[effector, E]) :-
    F is D+1,
    ist([A, B], [seq, G]),
    insert([A, B], G:::C:::[effector, E]),
    set([A, B|C], [clock, F]).

set(A, [B, C]) :-
    remove(A, [B, _]),
    insert(A, [B, C]).

threads(robot(cleaner(_, _))):::[thread(sense('F'(A)), [merge(ltd(sense('F'(A)), learn(B))), merge(ltp(sense('F'(A)), recall(C))), send(learn(B)), fire(detect('F'(A))), send(recall(C))]), thread(detect('F'(A)), [on('F'(A)), check(on('F'(_))),  ((excite(on('F'(_)))| [effector(clear('F'(A))), fire(synchro(detect('F'(_)), move(_)))]);inhibit(on('F'(_)))| [resume(detect('F'(_)))])]), thread(learn(B), [receive(sense('F'(A))), choice([_, _]),  ((fetch(_)| [fire(move(_))]);(fetch(_)| [fire(move(_))]))]), thread(synchro(detect('F'(A)), move(C)), [scan(detect('F'(A)), move(C)), sync(detect('F'(_)), move(_)), fire(ltp(sense('F'(_)), recall(_)))]), thread(recall(C), [receive(sense('F'(A))), fire(ltd(sense('F'(_)), learn(_))), fire(move(_))]), thread(move(C), [step(C), check(at('F'(_))),  (excite(at('F'(_)))| [resume(move(_))];inhibit(at('F'(_)))| [effector(stop(_))])])].
threads(robot):::[thread(ltp(Q, R), [join(Q), increment(weight(Q, R))]), thread(ltd(_, _), [join(_), decrement(weight(_, _))])].
weights(robot(cleaner(_, _))):::[initial(sense(_), learn(_))o 1, initial(recall(_), do(_))o 1].



sense(A) :-
    if(interrupt([B, C]),
       then((remove([A, B|D], [clock, D]), remove([A, B|D], [excite, D]), remove([A, B|D], [inhibit, D]), remove([A, B|D], [signal, D]), for_each([sensor, E], such_that(member([sensor, E], C)), do((set([A, B, sense, E], [clock, 1]), write(0:::[sense, E]:::[sensor, E]), nl))), set([A, B], [seq, 1]), remove([A, B], D:::D:::D)))).

run(A) :-
    loop(([sense, A], react(A), reflect(A))).


reflect(A) :-
    for_each((B, C:::D:::E),
             such_that(ist([A, B], C:::D:::E)),
             do([A, B]>>>(C:::D:::E))).


react(A) :-
    for_each(([B, C], D:::E),
             such_that(ist([A, B|C],
                           ([clock, D], D:::E))),
             do([A, B|C]>>>(D:::E))).



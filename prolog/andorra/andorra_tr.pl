:- module(andorra_tr, [andorra_term_expansion/2]).

:- dynamic pred_replace/2.
:- dynamic pred_susp/2.
:- dynamic todelay/2.
:- dynamic pred_user/2.

:- include(library(andorra/andorra_builtins_exports)).

andorra_term_expansion(end_of_file, Cls) :-
    !,
    findall(CL,
            ( pred_replace(F,A),
              ( interpr(F, A, CL)
              ; interpr1(F, A, CL)
              )
            ; retract(pred_susp(H,B)),
              interpr2(H, B, CL)
            ; retract(todelay(F, A)),
              interpr3(F, A, CL)
            ), Cls, [end_of_file]),
    retractall(pred_replace(_, _)),
    retractall(pred_user(_, _)).
andorra_term_expansion((:- determinate(H,Cond)), Cls) :-
    !,
    teh_determinate(H, NH, HSusp, L, L1, Cls, Clt),
    tec_determinate(Cond, NH, HSusp, L, L1, Clt).
andorra_term_expansion((H :- B), []) :- !, te_cond_suspend(H, B).
andorra_term_expansion(H, []) :- !, te_cond_suspend(H, true).

te_cond_suspend(H, B) :-
    functor(H, F, A),
    ( pred_replace(F, A)
    ->true
    ; assertz(pred_replace(F, A))
    ),
    assertz(pred_susp(H, B)).

teh_determinate(H, NH, HSusp, L, L1, Cls, Clt) :-
    functor(H, F, A),
    H =.. [F|Arguments],
    assertz(pred_user(F, A)),
    NA is A + 3,
    atom_concat(F, '_andorra', NewF),
    functor(NH, NewF, NA),
    atom_concat(F, '_susp', NF),
    append(Arguments, [L, L2, In], ArgsH),
    NH =.. [NewF|ArgsH],
    append(Arguments, [L1, L2, In], ArgsS),
    HSusp =.. [NF|ArgsS],
    ( '$current_source_module'(M),
      predicate_property(M:H, exported)
    ->Cls = [(:- export(NewF/NA))|Clt]
    ; Cls = Clt
    ).

tec_determinate(true, NH, HSusp, L, L, [(NH :- HSusp)]) :- !.
tec_determinate(false, NH, HSusp, L, L1,
                [(NH :-
                      L= [S|L1],
                      suspend_andorra(S,[],HSusp,false,builtin))]) :- !.
tec_determinate(Cond, NH, HSusp, L, L1,
                [(NH :- 
                      simplify(Cond,NewCond),
                      ( NewCond = true
                      ->L=L1, HSusp
                      ; L=[S|L1],
                        % term_variables(Cond,LS),
                        obtain_vars(NewCond,LS),
                        !,
                        suspend_andorra(S,LS,HSusp,NewCond,builtin)
                      ))]).

interpr(F, A, (H :-
                   NH,
                   wakeup(L1,L2)
              )) :-
    functor(H, F, A),
    H =.. [Name|Args],
    atom_concat(Name, '_andorra', NewF),
    append(Args, [L1, L2, L1], ArgsH),
    NH =.. [NewF|ArgsH].

interpr3(F, A, (NH :- Body)) :-
    NA is A + 3,
    atom_concat(F, '_andorra2', NewF), %.. rra2
    functor(NH, NewF, NA),
    NH =.. [_|Args],
    once(append(Arguments, [L, L1, _], Args)),
    HOrig =.. [F|Arguments],
    atom_concat(F, '_andorra', BF),
    NH =.. [_ |BArgs],
    NB =.. [BF|BArgs],
    ( '$current_source_module'(M),
      predicate_property(M:NB, defined)
    ->Body = NB
    ; Body = (L = [S|L1], suspend_andorra(S, [], HOrig, false, builtin))
    ).

interpr1(F,A,NewClause) :-
    \+pred_user(F, A),
    functor(H, F, A),
    findall(guard_clause(H, _R, Bsusp), pred_susp(H, Bsusp), ListClaus),
    ( '$current_source_module'(M),
      predicate_property(M:H, exported),
      NewClause = (:- export(NewF/NA))
    ; ListClaus = [_]
    ->NewClause = (NH :- L = L1, HSusp)
    ; NewClause = (NH :-
                       verify_det(HOrig, LGyCs, NewLGyCs),
                       ( NewLGyCs = []
                       ->fail
                       ; NewLGyCs = [_]
                       ->L = L1, HSusp
                       ; L = [S|L1],
                         term_variables(HOrig, LS),
                         suspend_andorra(S, LS, HSusp, HOrig, LGyCs),
                         !
                       ))
    ),
    NA is A + 3,
    atom_concat(F, '_andorra', NewF),
    functor(NH, NewF, NA),
    atom_concat(F, '_susp', NF),
    NH =.. [_|ArgsH],
    once(append(ArgsO, [L, L2, In], ArgsH)),
    once(append(ArgsO, [L1, L2, In], ArgsS)),
    HOrig =.. [F|ArgsO],
    HSusp =.. [NF|ArgsS],
    maplist(replace_bodies(L1, L2, In), ListClaus, LGyCs).

replace_bodies(L1, L2, In, guard_clause(H, _, B), guard_clause(H, R)) :-
    prepare_body(H, B, L1, L2, _, R, In).

prepare_body(_, true, L0, L1, L0 = L1, [], _) :- !.
prepare_body(H, B, L0, L1, NB, R, In) :-
    interprbody(H, B, L0, L1, NB, R, _FC, In).

interpr2(H, B, CL) :-
    H =.. [Name|Arg],
    atom_concat(Name, '_susp', NewName),
    append(Arg, [L1, Lnp1, In], ArgH),
    NH =.. [NewName|ArgH],
    functor(NH, F, Ar),
    ( '$current_source_module'(M),
      predicate_property(M:H, exported),
      CL = (:- export(F/Ar))
    ; B = true
    ->CL = NH,
      Lnp1 = L1
    ; CL = (NH :- NB),
      interprbody(H, B, L1, Lnp1, NB, _, _, In)
    ).

interprbody(_, true, _, _, true, [], _, _) :- !.
interprbody(H, (A, B), L1, Lnp1, (NA, NB), R, FirstCut, In) :-
    !,
    interprcall(H, A, L1, L2, NA, R, FirstCut, In, Name, NIn, Rest),
    ( Name == !
    ->FirstCut = after
    ; true
    ),
    interprbody(H, B, L2, Lnp1, NB, Rest, FirstCut, NIn).
interprbody(H, A, L1, Lnp1, NA, R, FirstCut, In) :-
    interprcall(H, A, L1, Lnp1, NA, R, FirstCut, In, _, _, []).

interprcall(H,A,L1,L2,NA,R,FirstCut,In,Name,NIn,Rest) :-
    A =.. [Name|Arg],
    functor(A, F, Ar),
    ( sensitive(F/Ar)
    ->NA = (wakeup(In, L1), L1 = L2, A),
      NIn = L1
    ; NIn = In,
      ( unchained(F/Ar)
      ->NA = (L1 = L2, A)
      ; ( definite(F/Ar)
        ->atom_concat(Name, '_andorra', NewName)
        ; atom_concat(Name, '_andorra2', NewName)
        ),
        append(Arg, [L1, L2, In], ArgA),
        NA =.. [NewName|ArgA]
      )
    ),
    ( var(FirstCut),
      is_test(F/Ar),
      depends(A, H)
    ->R = [A|Rest]
    ; R = Rest
    ),
    ( definite(F/Ar)
    ->true
    ; todelay(F, Ar)
    ->true
    ; assertz(todelay(F, Ar))
    ).

sensitive(F/Ar) :- sensitive_builtin(_, F, Ar, _), \+pred_replace(F, Ar).

unchained(F/Ar) :- determinate_builtin(_, F, Ar, _), \+pred_replace(F, Ar) .

definite(F/Ar) :- pred_replace(F, Ar).
definite(F/Ar) :- builtin_export(_, F, Ar, _).
definite(F/Ar) :- determinate_builtin(_, F, Ar, _).
definite(F/Ar) :- sensitive_builtin(_, F, Ar, _).  

is_test(F/Ar) :- builtin_constrain(_, F, Ar, _), \+pred_replace(F, Ar). 


depends(A,H) :-
    term_variables(A, Vsa),
    term_variables(H, Vsh),
    nondisjuncts(Vsa, Vsh).

nondisjuncts([H|_],Vsh) :- is_in(H, Vsh), !.
nondisjuncts([_|T],Vsh) :- nondisjuncts(T, Vsh).

is_in(H,[Y|_]) :- H == Y, !.
is_in(H,[_|R]) :- is_in(H, R).

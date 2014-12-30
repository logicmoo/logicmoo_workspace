:- module(clambda, [op(201,xfx,+\)]).

:- use_module(library(compound_expand)).
:- use_module(library(maplist_dcg)).

remove_hats(^(H, G0), G) -->
    [H], !,
    remove_hats(G0, G).
remove_hats(G, G) --> [].

remove_hats(G0, G, EL) :-
    remove_hats(G0, G1, EL, T),
    '$expand':extend_arg_pos(G1, _, _, T, G, _).
    
cgoal_args(G0, G, AL, EL) :-
    G0 =.. [F|Args],
    cgoal_args(F, Args, G, Fr, EL),
    term_variables(Fr, AL).

cgoal_args(\,  [G1|EL],    G, [], EL) :- remove_hats(G1, G, EL).
cgoal_args(+\, [Fr,G1|EL], G, Fr, EL) :- remove_hats(G1, G, EL).

lambdaize_args(A0, M, VL, Ex, A) :-
    ( '$member'(E1, Ex),
      '$member'(E2, VL),
      E1==E2
    ->'$expand':wrap_meta_arguments(A0, M, VL, Ex, A)
    ; '$expand':remove_arg_pos(A0, _, M, VL, Ex, A, _)
    ).

goal_expansion(G0, G) :-
    callable(G0),
    cgoal_args(G0, G1, AL, EL),
    '$set_source_module'(M, M),
    expand_goal(G1, G2),
    lambdaize_args(G2, M, AL, EL, G3),
    G3 =.. [AuxName|VL],
    append(VL, EL, AV),
    G =.. [AuxName|AV].

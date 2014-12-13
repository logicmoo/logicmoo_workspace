:- module(clambda, [op(201,xfx,+\)]).

:- use_module(library(compound_expand)).
:- use_module(library(maplist_dcg)).

remove_hat(H, ^(H, G), G).

remove_hats(EL) -->
    maplist_dcg(remove_hat, EL).

cgoal_args(G0, G, AL, EL) :-
    G0 =.. [F|Args],
    cgoal_args(F, Args, G, AL, EL).

cgoal_args(\,  [G1|EL],    G, [], EL) :- remove_hats(EL, G1, G).
cgoal_args(+\, [AL,G1|EL], G, AL, EL) :- remove_hats(EL, G1, G).

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
    '$set_source_module'(M,M),
    lambdaize_args(G1, M, AL, EL, G2),
    G2 =.. [AuxName|VL],
    append(VL, EL, AV),
    G =.. [AuxName|AV].

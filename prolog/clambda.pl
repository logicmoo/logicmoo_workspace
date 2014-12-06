:- module(clambda, [op(201,xfx,+\)]).

:- use_module(library(compound_expand)).

remove_hats([], G, G).
remove_hats([V|L], ^(V, G0), G) :-
    remove_hats(L, G0, G).

cgoal_args(G0, G, AL, EL) :-
    G0 =.. [F|Args],
    cgoal_args(F, Args, G, AL, EL).

cgoal_args(\,  [G1|EL],    G, [], EL) :- remove_hats(EL, G1, G).
cgoal_args(+\, [AL,G1|EL], G, AL, EL) :- remove_hats(EL, G1, G).

lambdaize_args(A0, M, VL, Ex0, A) :-
    length(Ex0, N),
    (   A0 =.. [F|Args],
	length(Ex, N),
	'$append'(Args0, Ex, Args),
	Ex==Ex0,
	\+ ( '$member'(E1, Ex),
	     '$member'(E2, VL),
	     E1==E2
	   )
    ->  A =.. [F|Args0]
    ;   '$expand':wrap_meta_arguments(A0, M, VL, Ex0, A)
    ).

goal_expansion(G0, G) :-
    callable(G0),
    cgoal_args(G0, G1, AL, EL),
    '$set_source_module'(M,M),
    lambdaize_args(G1, M, AL, EL, G2),
    G2 =.. [AuxName|VL],
    append(VL, EL, AV),
    G =.. [AuxName|AV].

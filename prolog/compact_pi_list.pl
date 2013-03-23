:- module(compact_pi_list, [compact_pi_list/2]).

compact_pi_list([], []).
compact_pi_list([M:PI|MPIs], [M:PIs|CPIs]) :-
    compact_pi_list(MPIs, M, PI, PIs, CPIs).

compact_pi_list([],          _, PI, PI, []).
compact_pi_list([M:PI|MPIs], M, PI0, (PI0,PIs), CPIs) :- !,
    compact_pi_list(MPIs, M, PI, PIs, CPIs).
compact_pi_list([M:PI|MPIs], _, PI0, PI0, [M:PIs|CPIs]) :-
    compact_pi_list(MPIs, M, PI, PIs, CPIs).

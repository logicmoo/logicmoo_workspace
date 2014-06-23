:- module(sccs, [nodes_arcs_sccs/3]).

% Tarjan's scc algorithm
% http://en.wikipedia.org/wiki/Strongly_connected_component

%% Code copied from a post of Markus Triska in comp.lang.prolog --EMM
%
% Example:
% ?- nodes_arcs_sccs([a,b,c], [arc(a,b),arc(b,a)], Ss).
% @ Ss = [[a, b], [c]].

:- use_module(library(clpfd)).

nodes_arcs_sccs(Ns, As0, Ss) :-
    length(Ns, L),
    length(Vs, L),
    pairs_keys_values(Ps, Ns, Vs),
    maplist(attach_arc(Ps), As0),
    clpfd:scc(Vs, scc:successors),
    maplist(v_with_link(Ps), Vs, Ls0),
    keysort(Ls0, Ls1),
    group_pairs_by_key(Ls1, Ss0),
    pairs_values(Ss0, Ss).

v_with_link(Ps, V0, L-V) :-
    get_attr(V0, lowlink, L),
    member(V-X, Ps),
    X == V0,
    !.

:- public successors/2.
successors(V, Vs) :-
    ( get_attr(V, successors, Vs) -> true
    ; Vs = []
    ).

attach_arc(Ps, arc(X,Y)) :-
    memberchk(X-VX, Ps),
    memberchk(Y-VY, Ps),
    ( get_attr(VX, successors, Vs) -> true
    ; Vs = []
    ),
    put_attr(VX, successors, [VY|Vs]).

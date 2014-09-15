/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Strongly connected components of a graph.
   Written by Markus Triska (triska@gmx.at), May 2011
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(scc, [nodes_arcs_sccs/3]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Usage:

   nodes_arcs_sccs(+Ns, +As, -SCCs)

   where:

   Ns is a list of nodes. Each node must be a ground term.
   As is a list of arc(N1,N2) terms where N1 and N2 are nodes.
   SCCs is a list of lists of nodes that are in the same strongly
        connected component.

   Running time is O(|V| + log(|V|)*|E|).

   Example:

   %?- nodes_arcs_sccs([a,b,c,d], [arc(a,b),arc(b,a),arc(b,c)], SCCs).
   %@ SCCs = [[a,b],[c],[d]].

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(assoc)).

nodes_arcs_sccs(Ns, As, Ss) :-
        must_be(list(ground), Ns),
        must_be(list(ground), As),
        catch((maplist(node_var_pair, Ns, Vs, Ps),
               list_to_assoc(Ps, Assoc),
               maplist(attach_arc(Assoc), As),
               scc(Vs, successors),
               maplist(v_with_lowlink, Vs, Ls0),
               keysort(Ls0, Ls1),
               group_pairs_by_key(Ls1, Ss0),
               pairs_values(Ss0, Ss),
               % reset all attributes
               throw(scc(Ss))),
              scc(Ss),
              true).

% Associate a fresh variable with each node, so that attributes can be
% attached to variables that correspond to nodes.

node_var_pair(N, V, N-V) :- put_attr(V, node, N).

v_with_lowlink(V, L-N) :-
        get_attr(V, lowlink, L),
        get_attr(V, node, N).

successors(V, Vs) :-
        (   get_attr(V, successors, Vs) -> true
        ;   Vs = []
        ).

attach_arc(Assoc, arc(X,Y)) :-
        get_assoc(X, Assoc, VX),
        get_assoc(Y, Assoc, VY),
        successors(VX, Vs),
        put_attr(VX, successors, [VY|Vs]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tarjan's strongly connected components algorithm.

   DCGs are used to implicitly pass around the global index, stack
   and the predicate relating a vertex to its successors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

scc(Vs, Succ) :- phrase(scc(Vs), [s(0,[],Succ)], _).

scc([])     --> [].
scc([V|Vs]) -->
        (   vindex_defined(V) -> scc(Vs)
        ;   scc_(V), scc(Vs)
        ).

scc_(V) -->
        vindex_is_index(V),
        vlowlink_is_index(V),
        index_plus_one,
        s_push(V),
        successors(V, Tos),
        each_edge(Tos, V),
        (   { get_attr(V, index, VI),
              get_attr(V, lowlink, VI) } -> pop_stack_to(V, VI)
        ;   []
        ).

vindex_defined(V) --> { get_attr(V, index, _) }.

vindex_is_index(V) -->
        state(s(Index,_,_)),
        { put_attr(V, index, Index) }.

vlowlink_is_index(V) -->
        state(s(Index,_,_)),
        { put_attr(V, lowlink, Index) }.

index_plus_one -->
        state(s(I,Stack,Succ), s(I1,Stack,Succ)),
        { I1 is I+1 }.

s_push(V)  -->
        state(s(I,Stack,Succ), s(I,[V|Stack],Succ)),
        { put_attr(V, in_stack, true) }.

vlowlink_min_lowlink(V, VP) -->
        { get_attr(V, lowlink, VL),
          get_attr(VP, lowlink, VPL),
          VL1 is min(VL, VPL),
          put_attr(V, lowlink, VL1) }.

successors(V, Tos) --> state(s(_,_,Succ)), { call(Succ, V, Tos) }.

pop_stack_to(V, N) -->
        state(s(I,[First|Stack],Succ), s(I,Stack,Succ)),
        { del_attr(First, in_stack) },
        (   { First == V } -> []
        ;   { put_attr(First, lowlink, N) },
            pop_stack_to(V, N)
        ).

each_edge([], _) --> [].
each_edge([VP|VPs], V) -->
        (   vindex_defined(VP) ->
            (   v_in_stack(VP) ->
                vlowlink_min_lowlink(V, VP)
            ;   []
            )
        ;   scc_(VP),
            vlowlink_min_lowlink(V, VP)
        ),
        each_edge(VPs, V).

v_in_stack(V) --> { get_attr(V, in_stack, true) }.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   DCG rules to access the state, using right-hand context notation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

state(S), [S] --> [S].

state(S0, S), [S] --> [S0].

/*  Part of SWI-Prolog ARC

    Author:        Douglas Miles, Tom Schrijvers, Markus Triska and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2022, K.U.Leuven
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    mondification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(ndif,
          [ ndif/2                               % +Term1, +Term2
          ]).
:- autoload(library(lists),[append/3,reverse/2]).


:- set_prolog_flag(generate_debug_info, false).

/** <module> The ndif/2 constraint
*/

%!  ndif(+Term1, +Term2) is semidet.
%
%   Constraint that expresses that  Term1   and  Term2  never become
%   identical (==/2). Fails if `Term1 ==   Term2`. Succeeds if Term1
%   can  never  become  identical  to  Term2.  In  other  cases  the
%   predicate succeeds after attaching constraints   to the relevant
%   parts of Term1 and Term2 that prevent   the  two terms to become
%   identical.

ndif(X,Y) :-
    ?=(X,Y),
    !,
    X \== Y.
ndif(X,Y) :-
    ndif_c_c(X,Y,_).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The constraint is helt in  an   attribute  `ndif`. A constrained variable
holds a term  varndif(L1,L2)  where  `L1`   is  a  list  OrNode-Value for
constraints on this variable  and  `L2`   is  the  constraint list other
variables have on me.

The `OrNode` is a term nnode(Pairs), where `Pairs` is a of list Var=Value
terms representing the pending unifications. The  original ndif/2 call is
represented by a single OrNode.

If a unification related to an  OrNode   fails  the terms are definitely
unequal and thus we can kill all   pending constraints and succeed. If a
unequal related to an OrNode succeeds we   decrement  the `Count` of the
nnode. If the count  reaches  0  all   unifications  of  the  OrNode have
succeeded, the original terms are equal and thus we need to fail.

The following invariants must hold

  - Any variable involved in a ndif/2 constraint has an attribute
    varndif(L1,L2), Where each element of both lists is a term
    OrNode-Value, L1 represents the values this variable may __not__
    become equal to and L2 represents this variable involved in other
    constraints.  I.e, L2 is only used if a ndif/2 requires two variables
    to be ndifferent.
  - An OrNode has an attribute nnode(Pairs), where Pairs contains the
    possible unifications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ndif_unifiable(X, Y, Us) :-
    (    current_prolog_flag(occurs_check, error)
    ->   catch(unifiable(X,Y,Us), error(occurs_check(_,_),_), false)
    ;    unifiable(X, Y, Us)
    ).

%!  ndif_c_c(+X,+Y,!OrNode)
%
%   Enforce ndif(X,Y) that is related to the given OrNode. If X and Y are
%   equal we reduce the OrNode.  If  they   cannot  unify  we  are done.
%   Otherwise we extend the OrNode with  new pairs and create/extend the
%   varndif/2 terms for the left hand side of  the unifier as well as the
%   right hand if this is a variable.

ndif_c_c(X,Y,OrNode) :-
    (   ndif_unifiable(X, Y, Unifier)
    ->  (   Unifier == []
        ->  ornry_one_fail(OrNode)
        ;   ndif_c_c_l(Unifier,OrNode, U),
            subunifier_n(U, OrNode)
        )
    ;   ornry_succeed(OrNode)
    ).

subunifier_n([], _).
subunifier_n([X=Y|T], OrNode) :-
    ndif_c_c(X, Y, OrNode),
    subunifier_n(T, OrNode).


%!  ndif_c_c_l(+Unifier, +OrNode)
%
%   Extend OrNode with new elements from the   unifier.  Note that it is
%   possible that a unification against the   same variable appears as a
%   result of how unifiable acts on  sharing subterms. This is prevented
%   by simplify_ornry_node/3.
%
%   @see test 14 in src/Tests/attvar/test_ndif.pl.

ndif_c_c_l(Unifier, OrNode, U) :-
    extend_ornry_node(OrNode, List, Tail),
    ndif_c_c_l_aux(Unifier, OrNode, List0, Tail),
    (   simplify_ornry_node(List0, List, U)
    ->  true
    ;   List = List0,
        ornry_succeed(OrNode),
        U = []
    ).

extend_ornry_node(OrNode, List, Vars) :-
    (   get_attr(OrNode, ndif, nnode(Vars))
    ->  true
    ;   Vars = []
    ),
    put_attr(OrNode,ndif,nnode(List)).

ndif_c_c_l_aux([],_,List,List).
ndif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
    List = [X=Y|Rest],
    add_ornry_node(X,Y,OrNode),
    ndif_c_c_l_aux(Unifier,OrNode,Rest,Tail).

%!  add_ornry_node(+X, +Y, +OrNode)
%
%   Extend the varndif constraints on X and Y with the OrNode.

add_ornry_node(X,Y,OrNode) :-
    add_ornry_node_var1(X,Y,OrNode),
    (   var(Y)
    ->  add_ornry_node_var2(X,Y,OrNode)
    ;   true
    ).

add_ornry_node_var1(X,Y,OrNode) :-
    (   get_attr(X,ndif,Attr)
    ->  Attr = varndif(V1,V2),
        put_attr(X,ndif,varndif([OrNode-Y|V1],V2))
    ;   put_attr(X,ndif,varndif([OrNode-Y],[]))
    ).

add_ornry_node_var2(X,Y,OrNode) :-
    (   get_attr(Y,ndif,Attr)
    ->  Attr = varndif(V1,V2),
        put_attr(Y,ndif,varndif(V1,[OrNode-X|V2]))
    ;   put_attr(Y,ndif,varndif([],[OrNode-X]))
    ).

%!  simplify_ornry_node(+OrNode) is semidet.
%
%   Simplify the possible unifications left on the original ndif/2 terms.
%   There are two reasons for simplification. First   of all, due to the
%   way unifiable works we may end up with variables in the unifier that
%   do not refer to the original terms,   but  to variables in subterms,
%   e.g. `[V1 = f(a, V2), V2 = b]`.   As a result of subsequent unifying
%   variables, the unifier may end up   having  multiple entries for the
%   same variable, possibly having ndifferent values, e.g.,  `[X = a, X =
%   b]`.  As  these  can  never  be  satified  both  we  have  prove  of
%   inequality.
%
%   Finally, we remove elements from the list that have become equal. If
%   the OrNode is empty, the original terms   are equal and thus we must
%   fail.

simplify_ornry_node(OrNode) :-
    (   get_attr(OrNode, ndif, nnode(Pairs0))
    ->  simplify_ornry_node(Pairs0, Pairs, U),
        Pairs-U \== []-[],
        put_attr(OrNode, ndif, nnode(Pairs)),
        subunifier_n(U, OrNode)
    ;   true
    ).

simplify_ornry_node(List0, List, U) :-
    sort(1, @=<, List0, Sorted),
    simplify_ornry_node_(Sorted, List, U).

simplify_ornry_node_([], List, U) =>
    List = [],
    U = [].
simplify_ornry_node_([V1=V2|T], List, U), V1 == V2 =>
    simplify_ornry_node_(T, List, U).
simplify_ornry_node_([V1=Val1,V2=Val2|T], List, U), var(V1), V1 == V2 =>
    (   ?=(Val1, Val2)
    ->  Val1 == Val2,
        simplify_ornry_node_([V1=Val2|T], List, U)
    ;   U = [Val1=Val2|UT],
        simplify_ornry_node_([V2=Val2|T], List, UT)
    ).
simplify_ornry_node_([H|T], List, U) =>
    List = [H|Rest],
    simplify_ornry_node_(T, Rest, U).


%!  attr_unify_hook(+VarDif, +Other)
%
%   If two ndif/2 variables are unified  we   must  join the two varndif/2
%   terms. To do so, we filter the varndif terms for the ones involved in
%   this unification. Those that  are  represent   OrNodes  that  have a
%   unification satisfied. For the rest we  remove the unifications with
%   _self_, append them and use this as new varndif term.
%
%   On unification with a value, we recursively call ndif_c_c/3 using the
%   existing OrNodes.

attr_unify_hook(varndif(V1,V2),Other):-
  attr_unify_hook_real(varndif(V1,V2),Other).

attr_unify_hook_real(varndif(V1,V2),Other) :-
    (   get_attr(Other, ndif, varndif(OV1,OV2))
    ->  reverse_lookups_n(V1, Other, OrNodes1, NV1),
        ornry_one_fails(OrNodes1),
        reverse_lookups_n(OV1, Other, OrNodes2, NOV1),
        ornry_one_fails(OrNodes2),
        remove_obsolete_n(V2, Other, NV2),
        remove_obsolete_n(OV2, Other, NOV2),
        append(NV1, NOV1, CV1),
        append(NV2, NOV2, CV2),
        (   CV1 == [], CV2 == []
        ->  del_attr(Other, ndif)
        ;   put_attr(Other, ndif, varndif(CV1,CV2))
        )
    ;   var(Other)			% unrelated variable
    ->  put_attr(Other, ndif, varndif(V1,V2))
    ;   verify_compounds_n(V1, Other),
        verify_compounds_n(V2, Other)
    ).

remove_obsolete_n([], _, []).
remove_obsolete_n([N-Y|T], X, L) :-
    (   Y==X
    ->  remove_obsolete_n(T, X, L)
    ;   L=[N-Y|RT],
        remove_obsolete_n(T, X, RT)
    ).

reverse_lookups_n([],_,[],[]).
reverse_lookups_n([N-X|NXs],Value,Nodes,Rest) :-
    (   X == Value
    ->  Nodes = [N|RNodes],
        Rest = RRest
    ;   Nodes = RNodes,
        Rest = [N-X|RRest]
    ),
    reverse_lookups_n(NXs,Value,RNodes,RRest).

verify_compounds_n([],_).
verify_compounds_n([OrNode-Y|Rest],X) :-
    (   var(Y)
    ->  true
    ;   OrNode == (-)
    ->  true
    ;   ndif_c_c(X,Y,OrNode)
    ),
    verify_compounds_n(Rest,X).

%!  ornry_succeed(+OrNode) is det.
%
%   The ndif/2 constraint related  to  OrNode   is  complete,  i.e., some
%   (sub)terms can definitely not become equal.   Next,  we can clean up
%   the constraints. We do so by setting   the  OrNode to `-` and remove
%   this _dead_ OrNode from every varndif/2 attribute we can find.

ornry_succeed(OrNode) :-
    (   get_attr(OrNode,ndif,Attr)
    ->  Attr = nnode(Pairs),
        del_attr(OrNode,ndif),
        OrNode = (-),
        del_ornry_ndif(Pairs)
    ;   true
    ).

del_ornry_ndif([]).
del_ornry_ndif([X=Y|Xs]) :-
    cleanup_dead_ornry_nodes(X),
    cleanup_dead_ornry_nodes(Y),              % JW: what about embedded variables?
    del_ornry_ndif(Xs).

cleanup_dead_ornry_nodes(X) :-
    (   get_attr(X,ndif,Attr)
    ->  Attr = varndif(V1,V2),
        filter_dead_ornry_ors(V1,NV1),
        filter_dead_ornry_ors(V2,NV2),
        (   NV1 == [], NV2 == []
        ->  del_attr(X,ndif)
        ;   put_attr(X,ndif,varndif(NV1,NV2))
        )
    ;   true
    ).

filter_dead_ornry_ors([],[]).
filter_dead_ornry_ors([Or-Y|Rest],List) :-
    (   var(Or)
    ->  List = [Or-Y|NRest]
    ;   List = NRest
    ),
    filter_dead_ornry_ors(Rest,NRest).


%!  ornry_one_fail(+OrNode) is semidet.
%
%   Some unification related to OrNode succeeded.   We can decrement the
%   `Count` of the OrNode. If this  reaches   0,  the original terms are
%   equal and we must fail.

ornry_one_fail(OrNode) :-
    simplify_ornry_node(OrNode).

ornry_one_fails([]).
ornry_one_fails([N|Ns]) :-
    ornry_one_fail(N),
    ornry_one_fails(Ns).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is varndif/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is nnode/2. The second argument of nnode/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
    (   { get_attr(Var, ndif, varndif(Ors,_)) }
    ->  ornry_nodes(Ors, Var)
    ;   ornry_node(Var)
    ).

ornry_node(O) -->
    (   { get_attr(O, ndif, nnode(Pairs)) }
    ->  { eqns_lefts_rights(Pairs, As, Bs) },
        myndif(As, Bs),
        { del_attr(O, ndif) }
    ;   []
    ).

ornry_nodes([], _)       --> [].
ornry_nodes([O-_|Os], X) -->
    (   { get_attr(O, ndif, nnode(Eqs)) }
    ->  (   { Eqs = [LHS=_|_], LHS == X }
        ->  { eqns_lefts_rights(Eqs, As, Bs) },
            myndif(As, Bs),
            { del_attr(O, ndif) }
        ;   []
        )
    ;   [] % or-nnode already removed
    ),
    ornry_nodes(Os, X).

myndif([X], [Y]) --> !, ndif_if_necessary(X, Y).
myndif(Xs0, Ys0) -->
    { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
      X =.. [f|Xs], Y =.. [f|Ys]
    },
    ndif_if_necessary(X, Y).

ndif_if_necessary(X, Y) -->
    (   { ndif_unifiable(X, Y, _) }
    ->  [ndif(X,Y)]
    ;   []
    ).

eqns_lefts_rights([], [], []).
eqns_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
    eqns_lefts_rights(ABs, As, Bs).



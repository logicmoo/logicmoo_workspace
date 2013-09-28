:- module(regex_captures, [ new_captures/2
                          , empty_captures/1
                          , concatenate_captures/3
                          , push_captures/3
                          , set_captures/4
                          , finalize_captures/1
                          ]).
:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(assoc)).

/** <module> Internals for regex capturing

This module is intended for internal use only. The API is subject to
arbitrary change without notice.

A captures value is a convenient rearrangement of the captures
argument to regex/4. That argument is typically a plain list but working
with lists is inefficient. By creating a specialized structure
which shares variables with the original list, we can efficiently track
new captures as they're discovered while still giving the original user
exactly what he wants.

For example, we might represent `[A,B,C]` as a difference list
`dl([A,B,C], [B,C])` so that we have immediate access to the tail.

We currently use the following internal representations:

  * [] - no captures
  * num(List, Tail) - a difference list for managing numbered
  captures
  * Assoc - library(assoc) value for named captures

*/


%%  new_captures(+Sugar, -Captures) is det.
%
%   True if Sugar is represented by an opaque Captures value.
new_captures(Var, Captures) :-
    var(Var),
    !,
    Captures = num(Var, _Tail).
new_captures(List, Captures) :-
    List \== [],
    maplist(eq_pair, List, Pairs),
    !,
    list_to_assoc(Pairs, Captures).
new_captures(List, Captures) :-
    List == [],
    !,
    Captures = [].
new_captures(List, Captures) :-
    List = [_|_],
    !,
    Captures = num(List, _).

% to accommodate library(assoc) which needs -/2 terms
eq_pair(Eq, Name-Value) :-
    nonvar(Eq),
    Eq = (Name=Value).

% if key already exists, unify a value with it. otherwise,
% insert a new key-value pair.
insert_pair(Name-Value, Assoc, Assoc) :-
    get_assoc(Name, Assoc, CurrentValue),
    !,
    Value = CurrentValue.
insert_pair(Name-Value, Assoc0, Assoc) :-
    put_assoc(Name, Assoc0, Value, Assoc).

%%  empty_captures(?Captures) is semidet.
%
%   True if Captures represents an empty captures list.
empty_captures([]).
empty_captures(num([],[])).


%%  concatenate_captures(+Cap1, +Cap2, -Captures) is det.
%
%   Add contents of Cap2 to the back of Cap1 to produce Captures.
concatenate_captures([], Captures, Captures) :-
    !.
concatenate_captures(num(H,T), [], num(H,T)).
concatenate_captures(num(H1,T1), num(H2,T2), num(H1,T2)) :-
    !,
    T1 = H2.
concatenate_captures(C1, [], Captures) :-
    is_assoc(C1),
    !,
    Captures = C1.
concatenate_captures(C1, C2, C3) :-
    assoc_to_list(C2, Pairs),
    foldl(insert_pair, Pairs, C1, C),
    assoc_to_list(C, OutputPairs),
    assoc_to_list(C3, OutputPairs).


%%  push_captures(+Codes, +Cap1, -Captures) is det.
%
%   Add Codes as the last capture of +Cap1 to create Captures.
push_captures(Codes, [], num([Codes|T], T)).
push_captures(Codes, num(H,T0), num(H,T)) :-
    T0 = [Codes|T].

%%  set_captures(+Name, +Codes, +Cap1, -Captures) is semidet.
%
%   True if capture with Name can be unified with Codes.  Produces
%   a new Captures value in case Name is not yet present.
set_captures(Name, Codes, [], Captures) :-
    !,
    list_to_assoc([Name-Codes], Captures).
set_captures(Name, Codes, Cap1, Captures) :-
    insert_pair(Name-Codes, Cap1, Captures).


%%  finalize_captures(+Captures) is det.
%
%   Perform any final unification necessary before the original caller
%   sees the captured values.
finalize_captures(num(_,T)) :-
    !,
    T = [].
finalize_captures(_).

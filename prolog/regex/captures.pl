:- module(regex_captures, [ new_captures/2
                          , concatenate_captures/3
                          , push_captures/3
                          , finalize_captures/1
                          ]).

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

*/


%%  new_captures(+Sugar, -Captures) is det.
%
%   True if Sugar is represented by an opaque Captures value.
new_captures(Var, Var).


%%  concatenate_captures(+Cap1, +Cap2, -Captures) is det.
%
%   Add contents of Cap2 to the back of Cap1 to produce Captures.
concatenate_captures(Cap1, Cap2, Captures) :-
    append(Cap1, Cap2, Captures).


%%  push_captures(+Codes, +Cap1, -Captures) is det.
%
%   Add Codes as the last capture of +Cap1 to create Captures.
push_captures(Codes, Cap1, Captures) :-
    append(Cap1, [Codes], Captures).


%%  finalize_captures(+Captures) is det.
%
%   Perform any final unification necessary before the original caller
%   sees the captured values.
finalize_captures(_).

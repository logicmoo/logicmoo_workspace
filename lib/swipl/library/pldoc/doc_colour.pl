/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2013, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
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

:- module(pldoc_colours,
          [ colour_fragments/2          % +Source, -Fragments
          ]).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_colour)).
:- use_module(library(lists)).

/** <module> Source colouring support

Provide   hierarchical   colouring   information   on     top   of   the
library(prolog_colour). We need  ordered   hierarchical  information  to
create HTML fragments.
*/

:- thread_local
    fragment/3.                     % Start, Length, Class

:- create_prolog_flag(xref, false, [type(boolean)]).

%!  colour_fragments(+In, -Fragments:list) is det.
%
%   Create a list of colour fragments from In.
%
%   @param Fragments List of fragment(Start, End, Class, Subs)

colour_fragments(Source, Hierarchy) :-
    F = fragment(_,_,_),
    retractall(F),
    prolog_canonical_source(Source, SourceID),
    xref_source(SourceID, [silent(true)]),
    setup_call_cleanup(
        prolog_open_source(SourceID, Stream),
        prolog_colourise_stream(Stream, SourceID, assert_fragment),
        prolog_close_source(Stream)),
    findall(F, retract(F), Fragments0),
    sort(2, >=, Fragments0, Fragments1),
    sort(1, =<, Fragments1, Fragments2),
    fragment_hierarchy(Fragments2, Hierarchy0),
    include_fullstops(Hierarchy0, Hierarchy).

assert_fragment(Class, Start, Length) :-
    End is Start+Length,
    assert(fragment(Start, End, Class)).

%!  include_fullstops(+FragmentsIn, -FragmentsOut) is det.
%
%   Include fullstops into the term that preceeds them.

include_fullstops([], []).
include_fullstops([fragment(TS,FS,Class,Subs0),fragment(FS,FE,fullstop,[])|T0],
                  [fragment(TS,FE0,Class,Subs)|T]) :-
    !,
    append(Subs0, [fragment(FS,FE0,fullstop,[])], Subs),
    FE0 is FE-1,
    include_fullstops(T0, T).
include_fullstops([H|T0], [H|T]) :-
    include_fullstops(T0, T).


%!  fragment_hierarchy(+Fragments, -Hierarchy) is det.
%
%   Translate   list   of   fragment(Start,     End,   Class)   into
%   fragment(Start, End, Class, SubFragments).
%
%   @tbd    Detect improper nesting.  How to handle?

fragment_hierarchy([], []).
fragment_hierarchy([fragment(S,E,C)|Rest0], [fragment(S,E,C,Sub)|Rest]) :-
    sub_fragments(Rest0, E, Sub, Rest1),
    fragment_hierarchy(Rest1, Rest).

sub_fragments([], _, [], []).
sub_fragments([F|R0], End, Sub, Rest) :-
    F = fragment(SF,EF,C),
    (   EF =< End
    ->  Sub = [fragment(SF,EF,C,FSub)|RSub],
        sub_fragments(R0, EF, FSub, R1),
        sub_fragments(R1, End, RSub, Rest)
    ;   Sub = [],
        Rest = [F|R0]
    ).

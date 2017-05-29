/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(assrt_lib,
          [asr_head/2,
           assrt_type/1,
           assrt_status/1,
           assertion_records/4,
           asr_head_prop/7,
           curr_prop_asr/4,
           asr_aprop/4,
           prop_asr/4,
           prop_asr/7]).

:- use_module(library(assertions_op)).
:- use_module(library(extend_args)).
:- use_module(library(apply)).
:- use_module(library(extra_messages), []).
:- use_module(library(lists)).
:- use_module(library(list_sequence)).
:- use_module(library(implementation_module)).
:- use_module(library(subpos_utils)).
:- use_module(library(prolog_codewalk), []).

/** <module> Assertion reader for SWI-Prolog

- Notes: 
  asr_* declared multifile to allow extensibility. At this point you
  extend concrete assertions (not abstractions or fake ones, since they will be
  collected by the run-time checker, for instance)
*/

:- multifile
    asr_head_prop/7,
    asr_comm/3,
    asr_glob/4,
    asr_comp/4,
    asr_call/4,
    asr_succ/4,
    doc_db/4,
    nodirective_error_hook/1.

% asr_* declared dynamic to facilitate cleaning
:- dynamic
    asr_head_prop/7,
    asr_comm/3,
    asr_glob/4,
    asr_comp/4,
    asr_call/4,
    asr_succ/4,
    doc_db/4,
    nodirective_error_hook/1.

:- discontiguous
    term_expansion/2.

%  These predicates are intended not to be called directly by user-applications:

:- public
       asr_comm/3,
       asr_comp/4,
       asr_call/4,
       asr_succ/4,
       asr_glob/4.

%!  asr_head(?, ?) is det
%
%   Extract the Head for a given assertion identifier.  Note that for convention
%   the first and second arguments of the Assertion identifier contains the
%   module and the head respectively.

asr_head(Asr, M:Head) :-
    ( nonvar(Asr)
    ->arg(1, Asr, M),
      arg(2, Asr, Head)
    ; true
    ).

curr_prop_asr(head, M:P, From, Asr) :- asr_head_prop(Asr, M, P, _, _, _, From).
curr_prop_asr(stat,   P, From, Asr) :- asr_head_prop(Asr, _, _, P, _, _, From).
curr_prop_asr(type,   P, From, Asr) :- asr_head_prop(Asr, _, _, _, P, _, From).
curr_prop_asr(dict,   D, From, Asr) :- asr_head_prop(Asr, _, _, _, _, D, From).
curr_prop_asr(comm,   C, From, Asr) :- asr_comm(Asr,    C, From).
curr_prop_asr(comp, M:P, From, Asr) :- asr_comp(Asr, M, P, From).
curr_prop_asr(call, M:P, From, Asr) :- asr_call(Asr, M, P, From).
curr_prop_asr(succ, M:P, From, Asr) :- asr_succ(Asr, M, P, From).
curr_prop_asr(glob, M:P, From, Asr) :- asr_glob(Asr, M, P, From).

%!  asr_aprop(+Asr, +Key, ?Prop, -From)
%
%   Extensible accessor to assertion properties, ideal to have different views
%   of assertions, to extend the assertions or to create ancillary assertions
%   (see module assrt_meta.pl for an example). The first argument is wrapped to
%   facilitate indexing.  Note that it is recommended that multiple clauses of
%   this predicate be mutually exclusive.

:- multifile asr_aprop/4.
asr_aprop(rtcheck(Asr), Key, Prop, From) :-
    curr_prop_asr(Key, Prop, From, Asr).

prop_asr(H, M, Stat, Type, Dict, From, Asr) :-
    implementation_module(M:H, IM),
    asr_head_prop(Asr, C, H, Stat, Type, Dict, From),
    implementation_module(C:H, IM).

:- meta_predicate prop_asr(?, 0, +, +).

prop_asr(Key, M:P, From, Asr) :-
    implementation_module(M:P, IM),
    curr_prop_asr(Key, C:P, From, Asr),
    implementation_module(C:P, IM).

add_arg(_, G1, G2, _, _) :-
    var(G1),
    var(G2), !,
    assertion(fail),
    fail.
add_arg(H, G1, G2, PPos1, PPos2) :-
    \+ ( var(PPos1),
         var(PPos2)
       ),
    PPos1 = parentheses_term_position(From, To, Pos1),
    PPos2 = parentheses_term_position(From, To, Pos2),
    !,
    add_arg(H, G1, G2, Pos1, Pos2).
add_arg(H, M:G1, M:G2,
        term_position(From, To, FFrom, FTo, [MPos, Pos1]),
        term_position(From, To, FFrom, FTo, [MPos, Pos2])) :- !,
    add_arg(H, G1, G2, Pos1, Pos2).
add_arg(H, G1, G2, Pos,
        term_position(From, To, FFrom, FTo, PosL)) :-
    ( Pos = term_position(From, To, FFrom, FTo, PosL1)
    ->( nonvar(PosL1)
      ->append(PosL1, [0-0], PosL)
      ; true
      )  
    ; Pos = From-To
    ->FFrom = From,
      FTo = To,
      PosL = [0-0]
    ),
    extend_args(G1, [H], G2).

filepos_line(File, CharPos, Line, LinePos) :-
    setup_call_cleanup('$push_input_context'(filepos),
                       prolog_codewalk:filepos_line(File, CharPos, Line, LinePos),
                       '$pop_input_context').

var_location(Term, Pos, Var, Loc) :-
    ( var(Var),
      subterm_location_eq(L, Var, Term)
    ->subpos_location(L, Pos, Loc)
    ; true
    ).

term_expansion((decompose_assertion_body(Body, Format, A, B, C, D, E, F) :- valid_cp(C)),
               (decompose_assertion_body(Body, Format, Pos,
                                         A,  B,  C,  D,  E,  F,
                                         PDP, DPP, CPP, APP, GPP, COP) :- valid_cp(C))) :- !,
    maplist(var_location(Body, Pos),
            [A,  B,  C,  D,  E,  F],
            [PDP, DPP, CPP, APP, GPP, COP]).

term_expansion((decompose_assertion_body(Body, Format, A, B, C, D, E, F)),
               (decompose_assertion_body(Body, Format, Pos,
                                         A,  B,  C,  D,  E,  F,
                                         PDP, DPP, CPP, APP, GPP, COP))) :-
    maplist(var_location(Body, Pos),
            [A,  B,  C,  D,  E,  F],
            [PDP, DPP, CPP, APP, GPP, COP]).
term_expansion((decompose_assertion_body(Body, Format, A, B, C, D, E, F) :-
                    decompose_assertion_body(BO, Format, A, B, C, D, E, "")),
               (decompose_assertion_body(Body, Format, Pos,
                                         A,  B,  C,  D,  E,  F,
                                         PDP, DPP, CPP, APP, GPP, COP) :-
                    decompose_assertion_body(BO, Format, BOP,
                                             A,  B,  C,  D,  E,  "",
                                             PDP, DPP, CPP, APP, GPP, _))) :-
    Body = BO#F,
    maplist(var_location(Body, Pos), [BO, F], [BOP, COP]).

%!  decompose_assertion_body(+Body, +Format, +Pos, -Head, -Compat, -Call, -Succ, -Glob, Comment, -HPos, -CmpPos, -CPos, -SPos, -GPos, -ComPos)
%
%   Extract the different sections from the Body of an assertion. Note that this
%   is expanded during compilation to contain extra arguments with the term
%   locations for each section of the assertion from:
%
%   ```
%     decompose_assertion_body(+Body, +Format, +Pos, -Head, -Compat, -Call, -Succ, -Glob, Comment)
%   ```
%
%   SWI-Extensions with respect to the Ciao Assertion Language:
%   - Usage of is/2 in addition to +/2.
%   - Solved ambiguity of :/2 and module qualification (valid_cp/1)

% ----------------------- A  B C  D    E F-- - -A--B-----C-----D-----E-----F--- %ABCDEF
decompose_assertion_body((A::B:C=>D  + E#F), p, A, B,    C,    D,    E,    F ). %111111
decompose_assertion_body((A::B:C=>D is E#F), p, A, B,    C,    D,    E,    F ). %111111
decompose_assertion_body((A::B:C=>D  + E  ), p, A, B,    C,    D,    E,    ""). %111100
decompose_assertion_body((A::B:C=>D is E  ), p, A, B,    C,    D,    E,    ""). %111100
decompose_assertion_body((A::B:C=>D     #F), p, A, B,    C,    D,    true, F ). %111011
decompose_assertion_body((A::B:C=>D       ), p, A, B,    C,    D,    true, ""). %111000
decompose_assertion_body((A::B:C     + E#F), p, A, B,    C,    true, E,    F ). %110111
decompose_assertion_body((A::B:C    is E#F), p, A, B,    C,    true, E,    F ). %110111
decompose_assertion_body((A::B:C     + E  ), p, A, B,    C,    true, E,    ""). %110100
decompose_assertion_body((A::B:C    is E  ), p, A, B,    C,    true, E,    ""). %110100
decompose_assertion_body((A::B:C        #F), p, A, B,    C,    true, true, F ). %110011
decompose_assertion_body((A::B:C          ), p, A, B,    C,    true, true, ""). %110000
decompose_assertion_body((A::B  =>D  + E#F), p, A, B,    true, D,    E,    F ). %101111
decompose_assertion_body((A::B  =>D is E#F), p, A, B,    true, D,    E,    F ). %101111
decompose_assertion_body((A::B  =>D  + E  ), p, A, B,    true, D,    E,    ""). %101100
decompose_assertion_body((A::B  =>D is E  ), p, A, B,    true, D,    E,    ""). %101100
decompose_assertion_body((A::B  =>D     #F), p, A, B,    true, D,    true, F ). %101011
decompose_assertion_body((A::B  =>D       ), p, A, B,    true, D,    true, ""). %101000
decompose_assertion_body((A::B       + E#F), p, A, B,    true, true, E,    F ). %100111
decompose_assertion_body((A::B      is E#F), p, A, B,    true, true, E,    F ). %100111
decompose_assertion_body((A::B       + E  ), p, A, B,    true, true, E,    ""). %100100
decompose_assertion_body((A::B      is E  ), p, A, B,    true, true, E,    ""). %100100
decompose_assertion_body((A::B          #F), d, A, B,    true, true, true, F ). %100011
decompose_assertion_body((A::B            ), d, A, B,    true, true, true, ""). %100000
decompose_assertion_body((A   :C=>D  + E#F), p, A, true, C,    D,    E,    F ) :- valid_cp(C). %011111
decompose_assertion_body((A   :C=>D is E#F), p, A, true, C,    D,    E,    F ) :- valid_cp(C). %011111
decompose_assertion_body((A   :C=>D  + E  ), p, A, true, C,    D,    E,    "") :- valid_cp(C). %011100
decompose_assertion_body((A   :C=>D is E  ), p, A, true, C,    D,    E,    "") :- valid_cp(C). %011100
decompose_assertion_body((A   :C=>D     #F), s, A, true, C,    D,    true, F ) :- valid_cp(C). %011011
decompose_assertion_body((A   :C=>D       ), s, A, true, C,    D,    true, "") :- valid_cp(C). %011000
decompose_assertion_body((A   :C     + E#F), g, A, true, C,    true, E,    F ) :- valid_cp(C). %010111
decompose_assertion_body((A   :C    is E#F), g, A, true, C,    true, E,    F ) :- valid_cp(C). %010111
decompose_assertion_body((A   :C     + E  ), g, A, true, C,    true, E,    "") :- valid_cp(C). %010100
decompose_assertion_body((A   :C    is E  ), g, A, true, C,    true, E,    "") :- valid_cp(C). %010100
decompose_assertion_body((A   :C        #F), c, A, true, C,    true, true, F ) :- valid_cp(C). %010011
decompose_assertion_body((A   :C          ), c, A, true, C,    true, true, "") :- valid_cp(C). %010000
decompose_assertion_body((A     =>D  + E#F), p, A, true, true, D,    E,    F ). %001111
decompose_assertion_body((A     =>D is E#F), p, A, true, true, D,    E,    F ). %001111
decompose_assertion_body((A     =>D  + E  ), p, A, true, true, D,    E,    ""). %001100
decompose_assertion_body((A     =>D is E  ), p, A, true, true, D,    E,    ""). %001100
decompose_assertion_body((A     =>D     #F), s, A, true, true, D,    true, F ). %001011
decompose_assertion_body((A     =>D       ), s, A, true, true, D,    true, ""). %001000
decompose_assertion_body((A          + E#F), g, A, true, true, true, E,    F ). %000111
decompose_assertion_body((A         is E#F), g, A, true, true, true, E,    F ). %000111
decompose_assertion_body((A          + E  ), g, A, true, true, true, E,    ""). %000100
decompose_assertion_body((A         is E  ), g, A, true, true, true, E,    ""). %000100
decompose_assertion_body((BO            #F), P, A, B,    C,    D,    E,    F ) :-
    decompose_assertion_body(BO, P, A, B, C, D, E, "").                         %000011
decompose_assertion_body((A             #F), p, A, true, true, true, true, F ). %000011
decompose_assertion_body((A               ), t, A, true, true, true, true, ""). %000000

fix_format_global(p, p).
fix_format_global(d, p).
fix_format_global(s, p).
fix_format_global(g, g).
fix_format_global(c, g).
fix_format_global(t, g).

valid_cp(C) :- \+ invalid_cp(C).

invalid_cp(_/_).

%!  assrt_type(Type)
%
%  The type of assertion

assrt_type(pred).
assrt_type(prop).
% assrt_type(decl). % Not used, instead we have declaration
% assrt_type(func).
assrt_type(calls).
assrt_type(success).
assrt_type(comp).
assrt_type(entry).
assrt_type(exit).
% assrt_type(modedef).

%!  assrt_status(Status)
%
%   The status of an assertion

assrt_status(true).
assrt_status(false).
assrt_status(check).
assrt_status(checked).
assrt_status(trust).
assrt_status(trace).
assrt_status(debug).

%!  default_assrt_status(+Type:assrt_type,-Status:assrt_status)
%
%   Defines the status to be used for a given assertion type, if an
%   assertion status is not specified explicitly.

default_assrt_status(entry,   true) :- !. % ???
default_assrt_status(X,       check) :-
    assrt_type(X),
    !.

normalize_status_and_type(Assertions, APos, AssrtStatus, AssrtType, UBody, BPos) :-
    normalize_status_and_type_1(Assertions, APos, AssrtStatus, AssrtType, UBody, BPos),
    status_and_type(AssrtStatus, AssrtType).

normalize_status_and_type_1(Assertions, PPos, AssrtStatus, AssrtType, UBody, BPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    normalize_status_and_type_1(Assertions, Pos, AssrtStatus, AssrtType, UBody, BPos).
normalize_status_and_type_1(Assertions, term_position(_, _, _, _, [BPos]),
                          _, AssrtType, UBody, BPos) :-
    Assertions =.. [AssrtType, UBody].
normalize_status_and_type_1(Assertions, term_position(_, _, _, _, [_, BPos]),
                          AssrtStatus, AssrtType, UBody, BPos) :-
    Assertions =.. [AssrtType, AssrtStatus, UBody].

status_and_type(AssrtStatus, AssrtType) :-
    assrt_type(AssrtType),
    ( var(AssrtStatus)
    ->default_assrt_status(AssrtType, AssrtStatus)
    ; assrt_status(AssrtStatus)
    ).

term_expansion(generate_nodirective_error, Clauses) :-
    expand_nodirective_error(Clauses).

expand_nodirective_error(Clauses) :-
    findall((:- export(Type/Arity)),
            ( assrt_type(Type),
              member(Arity, [1, 2])
            ), Clauses, ClauseT),
    findall((Assr :- Body),
            ( assrt_type(Type),
              normalize_status_and_type_1(Assr, _, Status, Type, _, _),
              functor(Assr, Type, Arity),
              Body0 = ignore(assrt_lib:nodirective_error_hook(Assr)),
              ( Arity = 1
              ->Body = Body0
              ; Body = (assrt_lib:assrt_status(Status), Body0 )
              )
            ),
            ClauseT).

%!  exit(+Status, +AssertionBody)
%
%   Assertion type exit, specifies the properties on success, but only for
%   external calls.

%!  exit(+AssertionBody)
%
%   Same as exit/2, but with default status check

%!  entry(+Status, +AssertionBody)
%
%   Assertion type entry, specifies the properties at call time, only for
%   external entry.

%!  entry(+AssertionBody)
%
%   Same as entry/2, but with default status check

%!  calls(+Status, +AssertionBody)
%
%   Assertion type calls, specifies the properties at call time.

%!  calls(+AssertionBody)
%
%   Same as calls/2, but with default status check

%!  success(+Status, +AssertionBody)
%
%   Assertion type success, specifies the properties on success, but only for
%   external calls.

%!  success(+AssertionBody)
%
%   Same as success/2, but with default status check

%!  comp(+Status, +AssertionBody)
%
%   Assertion type comp, specifies computational or global properties.

%!  comp(+AssertionBody)
%
%   Same as comp/2, but with default status check

%!  prop(+Status, +AssertionBody)
%
%   States that the predicate is a property

%!  prop(+AssertionBody)
%
%   Same as prop/2, but with default status check

%!  pred(+Status, +AssertionBody)
%
%   Union of calls, success and comp assertion types

%!  pred(+AssertionBody)
%
%   Same as pred/2, but with default status check

% To Avoid attempts to execute asertions (must be declarations):
generate_nodirective_error.

%!  assertion_format(AssrtType:assrt_type, Code:assrt_format_code)
%
%   Code describes an admissible format in which assertions of the class
%   AssrtType can be written.

assertion_format(pred, X) :- assrt_format_code(X).
assertion_format(decl, X) :- assrt_format_code(X). % ?
assertion_format(prop, X) :- assrt_format_code(X).
assertion_format(calls,   c).
assertion_format(success, s).
% DTM: New assertion type
assertion_format(exit, s).
assertion_format(comp, g).
% These to become obsolete?
assertion_format(entry, c).
assertion_format(entry, t).

%!  assrt_format_code(X)
%
%   X is a designator for an assertion format.

assrt_format_code(p).
assrt_format_code(d).
assrt_format_code(c).
assrt_format_code(s).
assrt_format_code(g).
assrt_format_code(t).

% EMM: Support for grouped global properties

current_body(MBodyS, M, PPos, Body, BPos, Gl1, Gl) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    current_body(MBodyS, M, Pos, Body, BPos, Gl1, Gl).
current_body(M:BodyS, _, term_position(_, _, _, _, [_, PosS]), Body, BPos, Gl1, Gl) :-
    atom(M), !,
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).
current_body(BodyS + BGl, M, term_position(_, _, _, _, [PosS, PGl]),
             Body, BPos, Gl0, Gl) :- !,
    propdef(BGl, M, PGl, Gl0, Gl1),
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).
current_body(BodyS is BGl#Co, M,
             term_position(From, To, _, _, [PosS, A2TermPos]),
             Body, BPos, Gl0, Gl) :- !,
    f2_pos(A2TermPos, FFrom, FTo, PGl, PosCo),
    propdef(BGl, M, PGl, Gl0, Gl1),
    current_body(BodyS#Co, M, term_position(From, To, FFrom, FTo, [PosS, PosCo]),
                 Body, BPos, Gl1, Gl).
current_body(BodyS is BGl, M, term_position(_, _, _, _, [PosS, PGl]),
             Body, BPos, Gl0, Gl) :- !,
    propdef(BGl, M, PGl, Gl0, Gl1),
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).

/*
  NOTE: Next syntax is ambiguous, but shorter:
    ```
      :- det callable.
    ```
  is equivalent to:
    ```
      :- true prop callable/1 is det
    ```
  
  but can be confused with:
  ```
  :- true prop det(callable)
  :- true prop det(X) :: callable(X).
  ```
  in any case this is syntax sugar so we can always write the long version of
  the assertion to avoid ambiguities
*/

current_body(BodyS, M, PosS, Body, BPos, Gl0, Gl) :-
    is_decl_global(BodyS, M),
    BodyS =.. [F, Head|GArgL],
    nonvar(Head), !,
    PosS = term_position(_, _, FF, FT, [HPos|PArgL]),
    BGl =.. [F|GArgL],
    current_body(Head + BGl, M,
                 term_position(_, _, _, _,
                               [HPos, term_position(_, _, FF, FT, [PArgL])]),
                 Body, BPos, Gl0, Gl).
current_body(BodyS, M, PosS, Body, BPos, Gl0, Gl) :-
    ( body_member(BodyS, PosS, Lit, LPos)
    *->
      current_body(Lit, M, LPos, Body, BPos, Gl0, Gl)
    ; Body = M:BodyS,
      Gl = Gl0,
      BPos = PosS
    ).

f2_pos(PPos, FFrom, FTo, PGl, PosCo) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    f2_pos(Pos, FFrom, FTo, PGl, PosCo).
f2_pos(term_position(_, _, FFrom, FTo, [PGl, PosCo]), FFrom, FTo, PGl, PosCo).

body_member(Body, _, _, _) :-
    var(Body), !, fail.
body_member([], _, _, _) :- !, fail.
body_member(Body, PPos, Lit, Pos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, BPos), !,
    body_member(Body, BPos, Lit, Pos).
body_member([A|B], list_position(From, To, [APos|EPos], TPos), Lit, LPos) :- !,
    ( Lit=A, LPos=APos
    ; Lit=B, LPos=list_position(From, To, EPos, TPos)
    ).
body_member((A, B), term_position(_, _, _, _, [APos, BPos]), Lit, LPos) :-
    ( Lit=A, LPos=APos
    ; Lit=B, LPos=BPos
    ).

is_decl_global(Head, M) :-
    is_decl_global(Head, _, _, M).

is_decl_global(Head, Status, Type, M) :-
    prop_asr(head, M:Head, _, Asr),
    ( ( prop_asr(glob, metaprops:declaration(Status, _), _, Asr)
      ; Status = true,
        prop_asr(glob, metaprops:declaration(_), _, Asr)
      )
    ->( prop_asr(glob, metaprops:global(Type, _), _, Asr)
      ; Type = (pred)
      )
    ; ( prop_asr(glob, metaprops:global(Type, _), _, Asr)
      ; Type = (pred),
        prop_asr(glob, metaprops:global(_), _, Asr)
      ),
      Status = true
    ), !.

current_normalized_assertion(Assertions, M, PPos, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, APos), !,
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos).
current_normalized_assertion(Assertions  + BGl, M, term_position(_, _, _, _, [APos, PGl]),
                             Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :- !,
    propdef(BGl, M, PGl, Gl, Gl0),
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, CoPos, RPos).
current_normalized_assertion(Assertions is BGl, M, term_position(_, _, _, _, [APos, PGl]),
                             Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :- !,
    propdef(BGl, M, PGl, Gl, Gl0),
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, CoPos, RPos).
current_normalized_assertion(Assertions # Co2, M, term_position(_, _, _, _, [APos, CoPos2]),
                             Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :- !,
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl, Co1, CoPos1, RPos),
    once(merge_comments(Co1, CoPos1, Co2, CoPos2, Co, CoPos)).
current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    ( is_decl_global(Assertions, DStatus, DType, M)
    ->Term =.. [DType, DStatus, Assertions],
      current_normalized_assertion(Term, M, term_position(_, _, _, _, [0-0, APos]),
                                   Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos)
    ; normalize_status_and_type(Assertions, APos, Status, Type, BodyS, PosS),
      current_body(BodyS, M, PosS, BM:Body, BPos, Gl, Gl0),
      normalize_assertion_head_body(Body, BM, BPos, Pred, Format, Cp, Ca, Su, Gl0, Co, CoPos, RPos),
      (Gl \= [] -> fix_format_global(Format, GFormat) ; GFormat = Format),
      assertion_format(Type, GFormat)
    ).

merge_comments("",  _, C, P, C, P).
merge_comments(C, P, "",  _, C, P).
merge_comments(C1, P1, C2, P2, [C1, C2], list_position(_, _, [P1, P2])).

normalize_assertion_head_body(Body, M, BPos, Pred, Format, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    once(decompose_assertion_body(Body, Format, BPos, Head, BCp, BCa, BSu,
                                  BGl, Co, HPos, PCp, PCa, PSu, PGl, CoPos)),
    normalize_assertion_head(Head, M, HPos, Pred, Cp0, Ca0, Su0, Gl0, RPos),
    apropdef(Pred, M, BCp, PCp, Cp, Cp0),
    apropdef(Pred, M, BCa, PCa, Ca, Ca0),
    apropdef(Pred, M, BSu, PSu, Su, Su0),
    propdef(BGl, M, PGl, Gl, Gl0).

normalize_assertion_head(Head, M, PPos, Pred, Cp, Ca, Su, Gl, HPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    normalize_assertion_head(Head, M, Pos, Pred, Cp, Ca, Su, Gl, HPos).
normalize_assertion_head((H1,H2), M, term_position(_, _, _, _, [P1, P2]),
                         P, Cp, Ca, Su, Gl, RP) :- !,
    ( normalize_assertion_head(H1, M, P1, P, Cp, Ca, Su, Gl, RP)
    ; normalize_assertion_head(H2, M, P2, P, Cp, Ca, Su, Gl, RP)
    ).
normalize_assertion_head([H1|H2], M, list_position(From, To, [P1|E], TP),
                         P, Cp, Ca, Su, Gl, RP) :- !,
    ( normalize_assertion_head(H1, M, P1, P, Cp, Ca, Su, Gl, RP)
    ; normalize_assertion_head(H2, M, list_position(From, To, E, TP),
                               P, Cp, Ca, Su, Gl, RP)
    ).
normalize_assertion_head(M:H, _, term_position(_, _, _, _, [_, HP]),
                         P, Cp, Ca, Su, Gl, RP) :-
    atom(M), !,
    normalize_assertion_head(H, M, HP, P, Cp, Ca, Su, Gl, RP).
normalize_assertion_head(F/A, M, Pos, M:Pred, [], [], [], [], Pos) :- !,
    functor(Pred, F, A).
normalize_assertion_head(Head, M, Pos, M:Pred, Cp, Ca, Su, Gl, Pos) :-
    compound(Head),
    !,
    functor(Head, F, A),
    functor(Pred, F, A),
    Pos = term_position(_, _, _, _, PosL),
    normalize_args(PosL, 1, Head, M, Pred, Cp, Ca, Su, Gl).
normalize_assertion_head(Head, M, Pos, M:Head, [], [], [], [], Pos) :-
    atom(Head).

normalize_args([Pos|PosL], N0, Head, M, Pred, Cp0, Ca0, Su0, Gl0) :-
    arg(N0, Head, HArg), !,
    resolve_types_modes(HArg, M, PArg, Pos, Cp0, Ca0, Su0, Gl0, Cp1, Ca1, Su1, Gl1),
    arg(N0, Pred, PArg),
    succ(N0, N),
    normalize_args(PosL, N, Head, M, Pred, Cp1, Ca1, Su1, Gl1).
normalize_args([], _, _, _, _, [], [], [], []).

resolve_types_modes(A,    _, A, _, Cp,  Ca,  Su,  Gl,  Cp, Ca, Su, Gl) :- var(A), !.
resolve_types_modes(A1, M, A, PPos, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    resolve_types_modes(A1, M, A, Pos, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl).
resolve_types_modes(A0:T, M, A, term_position(_, _, _, _, [PA0, PT]), Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl) :-
    do_propdef(T, M, A, PT, Pr0, Pr1),
    do_modedef(A0, M, A1, A, PA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !,
    do_propdef(A1, M, A, PA1, Pr1, Pr).
resolve_types_modes(A0, M, A, PA0, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl) :-
    do_modedef(A0, M, A1, A, PA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    do_propdef(A1, M, A, PA1, Pr0, Pr).

do_propdef(A,  _, A, _,   Cp,  Cp) :- var(A), !.
do_propdef(A1, M, A, PA1, Cp1, Cp) :-
    hpropdef(A1, M, A, PA1, Cp1, Cp).

do_modedef(A0, M, A1, A, PA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    nonvar(A0),
    modedef(A0, M, A1, A, PA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr), !.
do_modedef(A0, M, A1, A, PPA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    nonvar(PPA0),
    PPA0 = parentheses_term_position(_, _, PA0), !,
    modedef(A0, M, A1, A, PA0, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr).
do_modedef(A0, M, A1, A, APos, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    atom(A0),
    A2 =.. [A0, A],
    ( var(APos) -> true ; APos = From-To, Pos = term_position(From, To, From, To, [To-To]) ),
    modedef(A2, M, A1, A, Pos, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr), !.
do_modedef(A0, M, A1, A, From-To, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    integer(A0),
    modedef(is_pred(A0, A), M, A1, A, term_position(From, To, From, From, [From-From, From-To]), PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !.
do_modedef(A0, _, A0, _, PA0, PA0, Cp0, Ca, Su, Gl, Cp, Ca, Su, Gl, Cp0, Cp).

% Support for modes are hard-wired here:
% ISO Modes
modedef(+(A),         M, A, B, Pos, PA, Cp,                       Ca0,               Su,                          Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- Pos = term_position(_, _, _, _, [PA]),
    (var(A), var(Ca1) -> Ca0 = [(M:nonvar(B))-Pos|Ca1] ; Ca0 = Ca1). % A bit confuse hack, Ca1 come instantiated to optimize the expression
modedef(-(A),         M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],               Su0,                         Gl,  Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]).
modedef(?(A),         _, A, _, Pos, PA, Cp0,                      Ca,                Su,                          Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
modedef(@(A),         _, A, B, Pos, PA, Cp0,                      Ca,                Su,  [(globprops:nfi(B))-Pos|Gl], Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
% PlDoc (SWI) Modes
modedef(:(A0),        _, A, B, Pos, PA, Cp,                       Ca0,               Su,                          Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- Pos = term_position(From, To, FFrom, FTo, [PA0]),
     % The first part of this check is not redundant if we forgot the meta_predicate declaration
    (var(A0 ), var(Ca1) -> Ca0 = [(typeprops:mod_qual(B))-Pos|Ca1], A0 = A ; Ca0 = Ca1, A = typeprops:mod_qual(A0, B), PA = term_position(From, To, FFrom, FTo, [PA0, From-From])).
modedef(is_pred(N,A), _, A, B, Pos, PA, Cp,  [(typeprops:is_pred(N,B))-Pos|Ca0],     Su,                          Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [_,PA]).
modedef('!'(A),       M, A, B, Pos, PA, Cp0, [(M:compound(B))-Pos|Ca],               Su,                          Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]). % May be modified using setarg/3 or nb_setarg/3 (mutable)
% Ciao Modes:
modedef(in(A),        M, A, B, Pos, PA, Cp,    [(M:ground(B))-Pos|Ca0],                 Su,                       Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [PA]).
modedef(out(A),       M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],  [(M:gnd(B))-Pos|Su0],                     Gl,  Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]).
modedef(go(A),        M, A, B, Pos, PA, Cp0,                      Ca,   [(M:gnd(B))-Pos|Su],                      Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
% Additional Modes (See Coding Guidelines for Prolog, Michael A. Covington, 2009):
modedef('*'(A),       M, A, B, Pos, PA, Cp,    [(M:ground(B))-Pos|Ca0],              Su,                          Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [PA]).
modedef('='(A),       _, A, B, Pos, PA, Cp0,                      Ca,                Su,  [(globprops:nfi(B))-Pos|Gl], Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]). % The state of A is preserved
modedef('/'(A),       M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],               Su0, [(globprops:nsh(B))-Pos|Gl], Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]). % Like '-' but also A don't share with any other argument
modedef('>'(A),       _, A, _, term_position(_, _, _, _, [PA]), PA, Cp, Ca,          Su0,                         Gl,  Cp, Ca, Su, Gl, Su0, Su). % Like output but A might be nonvar on entry

% nfi == not_further_inst
% nsh == not_shared

:- multifile prolog:error_message/3.

prolog:error_message(assertion(il_formed_assertion, Term)) -->
    [ 'Il formed assertion, check term ~w'-[Term]].

hpropdef(A0, M, A, PA0, Cp0, Cp) :-
    term_variables(A0, V),
    ( member(X, V), X==A ->
      Cp0 = [(M:A0)-PA0|Cp]
    ; aprops_arg(A0, M, A, PA0, Cp0, Cp)
    ).

apropdef_2(0, _, _, _, _) --> !, {fail}.
apropdef_2(N, Head, M, A, PPos) -->
    { nonvar(PPos),
      PPos = parentheses_term_position(_, _, Pos)
    }, !,
    apropdef_2(N, Head, M, A, Pos).
apropdef_2(1, Head, M, A, APos) -->
    {arg(1, Head, V)}, !,
    hpropdef(A, M, V, APos).
apropdef_2(N0, Head, M, (P * A), term_position(_, _, _, _, [PPos, APos])) -->
    {arg(N0, Head, V)}, !,
    {N is N0 - 1},
    apropdef_2(N, Head, M, P, PPos),
    hpropdef(A, M, V, APos).

apropdef(Var, _, _, _) --> {var(Var), !, fail}.
apropdef(_:Head, M, A, APos) -->
    {functor(Head, _, N)},
    apropdef_2(N, Head, M, A, APos), !.
apropdef(_, M, A, APos) --> propdef(A, M, APos).

propdef(A, M, APos) --> props_args(A, M, push, APos).

push(A, M, Pos) --> [(M:A)-Pos].

aprops_arg(A, M, V, PPos) -->
    { nonvar(PPos),
      PPos = parentheses_term_position(_, _, Pos)
    }, !,
    aprops_arg(A, M, V, Pos).
aprops_arg({A}, M, V, brace_term_position(_, _, Pos)) --> !, aprops_args(A, M, V, Pos).
aprops_arg(A,   M, V, Pos) --> aprops_args(A, M, V, Pos).

aprops_args(A, M, V, Pos) --> props_args(A, M, prop_arg(V), Pos).

:- meta_predicate props_args(?, ?, 5, ?, ?, ?).

props_args(true,   _, _, _) --> !, [].
props_args(A, M, V, PPos) -->
    { nonvar(PPos),
      PPos = parentheses_term_position(_, _, Pos)
    }, !,
    props_args(A, M, V, Pos).
props_args((A, B), M, V, term_position(_, _, _, _, [PA, PB])) --> !,
    props_args(A, M, V, PA),
    props_args(B, M, V, PB).
props_args((A; B), M, V, Pos) --> !,
    { Pos = term_position(_, _, _, _, [PA, PB]),
      props_args(A, M, V, PA, P1, []),
      list_sequence(P1, C1),
      props_args(B, M, V, PB, P2, []),
      list_sequence(P2, C2)
    },
    [(C1;C2)-Pos].
props_args(M:A, _, V, term_position(_, _, _, _, [_, PA])) -->
    {atom(M)}, !,
    props_args(A, M, V, PA).
props_args(A, M, V, Pos) --> call(V, A, M, Pos).

prop_arg(V, A, M, Pos) -->
    {add_arg(V, A, P, Pos, PPos)},
    [(M:P)-PPos].

assertion_records_helper(Match, a(Match, Record, Pos), Record, Pos).

assertion_records(M, Dict, Decl, PPos, Records, RPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos),
    !,
    assertion_records(M, Dict, Decl, Pos, Records, RPos).
assertion_records(_, Dict, M:Decl, term_position(_, _, _, _, [_, DPos]),
                  Records, RPos) :-
    atom(M), !,
    assertion_records(M, Dict, Decl, DPos, Records, RPos).
assertion_records(M, Dict, doc(Key, Doc),
                  term_position(From, To, FFrom, FTo, [KPos, DPos]),
                  assrt_lib:doc_db(Key, M, Doc, Dict),
                  term_position(0, 0, 0, 0,
                                [0-0,
                                 term_position(From, To, FFrom, FTo,
                                               [KPos, 0-0, DPos, 0-0 ])])) :- !.
% Note: We MUST save the full location (File, HPos), because later we will not
% have access to source_location/2, and this will fails for further created
% clauses --EMM
assertion_records(CM, Dict, Assertions, APos, Records, RPos) :-
    Match=(Assertions-Dict),
    findall(a(Match, Clause, HPos),
            assertion_record_each(CM, Dict, Assertions, APos, Clause, HPos),
            ARecords),
    ARecords \= [],
    maplist(assertion_records_helper(Match), ARecords, Records, RPos).

assertion_record_each(CM, Dict, Assertions, APos, Clause, TermPos) :-
    ignore(source_location(File, Line0)),
    ( nonvar(File)
    ->Loc = file(File, Line, Pos, _),
      ( var(APos)
      ->Line = Line0,
        Pos = -1
      ; true
      )
    ; true
    ),
    current_normalized_assertion(Assertions, CM, APos, M:Head, Status,
                                 Type, CpL, CaL, SuL, GlL, Co, CoPos, HPos),
    get_sequence_and_inc(Count),
    term_variables(t(Co, CpL, CaL, SuL, GlL), ShareL),
    atom_number(AIdx, Count),
    Asr =.. [AIdx, M, Head|ShareL], % Asr also contains variable bindings. By
                                    % convention, M is in the 1st position and
                                    % Head in the 2nd, to facilitate work
    ( Clause = assrt_lib:asr_head_prop(Asr, M, Head, Status, Type, Dict, Loc),
      SubPos = HPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
        arg(2, SubPos, To),
        TermPos = term_position(From, To, From, To,
                                [SubPos, 0-0, 0-0, 0-0, _, _, _])
      ; true
      )
    ; Co \= "",
      Clause = assrt_lib:asr_comm(Asr, Co, Loc),
      SubPos = CoPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
        arg(2, SubPos, To),
        TermPos = term_position(From, To, From, To, [_, SubPos, _])
      ; true
      )
    ; ( Clause = assrt_lib:AClause,
        member(AClause-PrL,
               [asr_comp(Asr, PM, Pr, Loc)-CpL,
                asr_call(Asr, PM, Pr, Loc)-CaL,
                asr_succ(Asr, PM, Pr, Loc)-SuL
               ]),
        member(MPr-SubPos, PrL),
        strip_module(MPr, PM, Pr)
      ; Clause = assrt_lib:asr_glob(Asr, PM, Pr, Loc),
        member(MGl-GPos, GlL),
        strip_module(MGl, PM, Gl),
        add_arg(_, Gl, Pr, GPos, SubPos)
      ;
        once(( member(MGl-GPos, GlL),
               member(Gl, [declaration, declaration(_)]),
               strip_module(MGl, PM, Gl),
               add_arg(_, Gl, Pr, GPos, _),
               implementation_module(PM:Pr, metaprops),
               functor(Head, Op, 1)
             )),
        Clause = (:- '$export_ops'([op(1125, fy, Op)], PM, File))
      ; member(MGl-_, GlL),
        member(Gl, [declaration,
                    declaration(_),
                    global,
                    global(_)]),
        strip_module(MGl, PM, Gl),
        add_arg(_, Gl, Pr, _, _),
        implementation_module(PM:Pr, metaprops)
      ->functor(Head, Fn, N),
        ( \+ predicate_property(M:Head, meta_predicate(_)),
          functor(Meta, Fn, N),
          Meta =.. [_|ArgL],
          once(append(ArgL1, [0], ArgL)),
          maplist(=(?), ArgL1),
          Clause = (:- meta_predicate Meta)
        )
      ),
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
        arg(2, SubPos, To),
        TermPos = term_position(From, To, From, To, [_, 0-0, SubPos, _])
      ; true
      )
    ),
    ( var(Pos),
      nonvar(File)
    ->( nonvar(SubPos),
        integer(From)
      ->filepos_line(File, From, Line, Pos)
      ; Line = Line0,
        Pos = -1
      )
    ; true
    ).

%!  assertion_records(+Decl, DPos, -Records, RPos) is semidet.
%
%   Process a Declaration as an assertion.  This is called in a term_expansion/2
%   of the assertion module. Fails if Decl is not a valid assertion.

assertion_records(Decl, DPos, Records, RPos) :-
    '$current_source_module'(M),
    assertion_records(M, Dict, Decl, DPos, Records, RPos),
    % Dict Must be assigned after assertion_records/6 to avoid performance
    % issues --EMM
    ( nb_current('$variable_names', Dict)
    ->true
    ; Dict = []
    ).

:- dynamic sequence/1.
sequence(1).

get_sequence_and_inc(S) :-
    retract(sequence(S)),
    succ(S, S2),
    assertz(sequence(S2)).

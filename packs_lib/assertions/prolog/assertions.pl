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

:- module(assertions,
          [asr_head/2,
           assrt_type/1,
           assrt_status/1,
           expand_assertion/4,
           asr_head_prop/7,
           curr_prop_asr/4,
           asr_aprop/4,
           aprop_asr/4,
           prop_asr/4,
           prop_asr/7]).

:- discontiguous '$exported_op'/3.
:- reexport(library(compound_expand)).
:- reexport(library(assertions_op)).

:- use_module(library(extend_args)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(extra_messages), []).
:- use_module(library(filepos_line)).
:- use_module(library(lists)).
:- use_module(library(list_sequence)).
:- use_module(library(neck)).
:- use_module(library(termpos)).
:- use_module(library(subpos_utils)).
:- use_module(library(prolog_codewalk), []).

/** <module> Assertion reader for SWI-Prolog

@note:
  asr_* declared multifile to allow extensibility. At this point you
  extend concrete assertions (not abstractions or fake ones, since they will be
  collected by the run-time checker, for instance)


@note: Next syntax is ambiguous, but shorter:
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

%!  asr_head(?Asr, ?Head) is det
%
%   Extract the Head for a given assertion identifier.  Note that the first and
%   second arguments of the Assertion identifier should contain the module and
%   the head respectively.

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

prop_asr(H, M, Stat, Type, Dict, From, Asr) :-
    asr_head_prop(Asr, C, H, Stat, Type, Dict, From),
    predicate_property(C:H, implementation_module(IM)),
    match_modules(H, M, IM).

match_modules(_, M, M) :- !.
match_modules(H, M, IM) :- predicate_property(M:H, implementation_module(IM)).

:- meta_predicate
       prop_asr(?, 0, +, +),
       aprop_asr(?, 0, +, +).

prop_asr(Key, M:P, From, Asr) :-
    curr_prop_asr(Key, C:P, From, Asr),
    predicate_property(C:P, implementation_module(IM)),
    match_modules(P, M, IM).

aprop_asr(Key, M:P, From, Asr) :-
    asr_aprop(Asr, Key, C:P, From),
    predicate_property(C:P, implementation_module(IM)),
    match_modules(P, M, IM).

add_arg(_, G1, G2, _, _) :-
    var(G1),
    var(G2),
    !,
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
        term_position(From, To, FFrom, FTo, [MPos, Pos2])) :-
    !,
    add_arg(H, G1, G2, Pos1, Pos2).
add_arg(H, G1, G2, Pos1, Pos2) :-
    ( var(Pos1)
    ->true
    ; ( Pos1 = term_position(From, To, FFrom, FTo, PosL1)
      ->( nonvar(PosL1)
        ->append(PosL1, [0-0 ], PosL)
        ; true
        )
      ; Pos1 = From-To
      ->FFrom = From,
        FTo = To,
        PosL = [0-0 ]
      ),
      Pos2 = term_position(From, To, FFrom, FTo, PosL)
    ),
    extend_args(G1, [H], G2).

%!  decompose_assertion_body(+Body, +Pos, -Head, -Compat, -Call, -Succ, -Glob, Comment, -HPos, -CmpPos, -CPos, -SPos, -GPos, -ComPos).
%
%   Extract the different sections from the Body of an assertion. Note that this
%   is expanded during compilation to contain extra arguments with the term
%   locations for each section of the assertion from:
%
%   ```
%     decompose_assertion_body(+Body, +Pos, -Head, -Compat, -Call, -Succ, -Glob, Comment)
%   ```
%
%   SWI-Extensions with respect to the Ciao Assertion Language:
%   - Usage of is/2 in addition to +/2.
%   - Solved ambiguity of :/2 and module qualification (valid_cp/1)

:- add_termpos(decompose_assertion_body(+, -, -, -, -)).
:- add_termpos(decompose_assertion_body(+, -, -, -, -, -)).
:- add_termpos(decompose_assertion_body(+, -, -, -, -, -, -)).

% ----------------------- AB C  D    E- -AB--C-----D-----E----- %CDE
decompose_assertion_body((AB:C=>D  + E), AB, C,    D,    E   ) :- valid_cp(C). %111
decompose_assertion_body((AB:C=>D is E), AB, C,    D,    E   ) :- valid_cp(C). %111
decompose_assertion_body((AB:C=>D     ), AB, C,    D,    true) :- valid_cp(C). %110
decompose_assertion_body((AB:C     + E), AB, C,    true, E   ) :- valid_cp(C). %101
decompose_assertion_body((AB:C    is E), AB, C,    true, E   ) :- valid_cp(C). %101
decompose_assertion_body((AB:C        ), AB, C,    true, true) :- valid_cp(C). %100
decompose_assertion_body((AB  =>D  + E), AB, true, D,    E   ). %011
decompose_assertion_body((AB  =>D is E), AB, true, D,    E   ). %011
decompose_assertion_body((AB  =>D     ), AB, true, D,    true). %010
decompose_assertion_body((AB       + E), AB, true, true, E   ). %001
decompose_assertion_body((AB      is E), AB, true, true, E   ). %001
decompose_assertion_body((AB          ), AB, true, true, true). %000

decompose_assertion_body((BO#F), A, B, C, D, E, F ) :- decompose_assertion_body(BO, A, B, C, D, E).
decompose_assertion_body(BO,     A, B, C, D, E, "") :- decompose_assertion_body(BO, A, B, C, D, E).

decompose_assertion_body((A::BO), A, B,    C, D, E) :- decompose_assertion_body(BO, B, C, D, E).
decompose_assertion_body(BO,      A, true, C, D, E) :- decompose_assertion_body(BO, A, C, D, E).

valid_cp(C) :- \+ invalid_cp(C).

invalid_cp(_/_).

%!  validate_body_sections(+Type:assrt_type, -Compat:list(pair), -Calls:list(pair), -Success:list(pair), -Global:list(pair), -MustBeEmpty:list(pair), -MustNotBeEmpty:list(pair)) is det.
%   
%   Unifies MustBeEmpty with a list of sections that must be empty, and
%   MustNotBeEmpty with a list of sections that must not be empty.  The elements
%   of both lists are pairs like Section-List, where section could be
%   compatibility, preconditions, postconditions or global.

validate_body_sections(pred,     _,  _,     _,  _, [], []).
validate_body_sections(prop,     _,  _,     _,  _, [], []).
validate_body_sections(calls,   Cp, Ca,    Su, Gl,
                       [compatibility-Cp, postconditions-Su, globalprops-Gl],
                       [preconditions-Ca]).
validate_body_sections(success, Cp,  _,    Su, Gl,
                       [compatibility-Cp, globalprops-Gl],
                       [postconditions-Su]).
validate_body_sections(comp,    Cp,  _,    Su, Gl,
                       [compatibiltiy-Cp, postconditions-Su],
                       [globalprops-Gl]).

%!  assrt_type(Type)
%
%  The type of assertion, could have the following values:
%
%  calls - Specifies the properties at call time.
%
%  success - Specifies the properties on success, but only for external calls.
%
%  comp - Assertion type comp, specifies computational or global properties.
%
%  prop - States that the predicate is a property
%
%  pred - Union of calls, success and comp assertion types

assrt_type(Type) :-
    validate_body_sections(Type, _, _, _, _, _, _),
    neck.

%!  assrt_status(Status)
%
%   The status of an assertion. Be careful, since they are not compatible with
%   those found in Ciao-Prolog. Could have the following values:
%
%   check - Assertion should be checked statically or with the rtcheck tracer (default)
%
%   true  - Assertion is true, provided by the user
%
%   false - Assertion is false, provided by the user (not implemented yet)
%
%   debug - Assertion should be checked only at development time
%
%   static - Assertion is always instrumented in the code via a wrapper, in
%            other words, it is considered part of the implementation.
%
%
%   @note: For static, such instrumentation can be removed only if a static
%   analysis prove it is always true (not implemented yet).
%
%   @tbd: The next are intended to be used internally, once the system be able
%   to infer new assertions:
%
%   right: inferred by the static analysis
%   trust: Ciao-Prolog like, provided by the user
%   fail: false, inferred by the static analyss.

assrt_status(check).
assrt_status(true).
assrt_status(false).
assrt_status(debug).
assrt_status(static).

:- add_termpos(decompose_status_and_type_1(+,?,?,-)).
:- add_termpos(decompose_status_and_type(+,?,?,-)).

decompose_status_and_type_1(Assertions, check, AssrtType, UBody) :-
    assrt_type(AssrtType),
    Assertions =.. [AssrtType, UBody],
    neck.
decompose_status_and_type_1(Assertions, AssrtStatus, AssrtType, UBody) :-
    assrt_type(AssrtType),
    Assertions =.. [AssrtType, AssrtStatus, UBody],
    neck.

decompose_status_and_type(Assertions, APos, AssrtStatus, AssrtType, UBody, BPos) :-
    cleanup_parentheses(APos, Pos),
    decompose_status_and_type_1(Assertions, Pos, AssrtStatus, AssrtType, UBody, BPos),
    assrt_status(AssrtStatus).

cleanup_parentheses(Pos1, Pos) :-
    nonvar(Pos1),
    Pos1 = parentheses_term_position(_, _, Pos2),
    !,
    cleanup_parentheses(Pos2, Pos).
cleanup_parentheses(Pos, Pos).

% To Avoid attempts to execute asertions (must be declarations):

:- assrt_type(Type),
   member(Arity, [1, 2]),
   neck,
   export(Type/Arity).

Assr :-
    decompose_status_and_type_1(Assr, _, Status, Type, _, _),
    functor(Assr, Type, Arity),
    Body1 = ignore(nodirective_error_hook(Assr)),
    ( Arity = 1
    ->Body = Body1
    ; Body = (assrt_status(Status), Body1)
    ),
    neck,
    Body.

is_decl_global(Head, M) :-
    is_decl_global(Head, _, _, M).

is_decl_global(Head, Status, Type, M) :-
    forall(Head = HM:_, (var(HM);atom(HM))),
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
    ),
    !.

:- add_termpos(current_decomposed_assertion(+,?,?,?,?,?,?,?,?,-,?)).
:- add_termpos(current_decomposed_assertion_(+,?,?,?,?,?,?,?,?,-,?)).
:- add_termpos(propdef(+,?,?,?)).
:- add_termpos(merge_comments(-,-,+)).
:- add_termpos(decompose_assertion_head_body(+,?,?,+,?,?,?,?,-,?)).
:- add_termpos(decompose_assertion_head_body_(+,?,?,+,?,?,?,?,-,?)).
:- add_termpos(decompose_assertion_head(+,?,?,+,-,?,?,?,?,?)).
:- add_termpos(decompose_assertion_head_(+,?,?,+,-,?,?,?,?,?)).

merge_comments("",  C,       C).
merge_comments(C,  "",       C).
merge_comments(C1, C2, [C1, C2]).

current_decomposed_assertion(Assertions, PPos, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    cleanup_parentheses(PPos, APos),
    current_decomposed_assertion_(Assertions, APos, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos).

current_decomposed_assertion_(AssertionsBGl, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :-
    member(AssertionsBGl, [Assertions  + BGl,
                           Assertions is BGl]),
    necki,
    propdef(BGl, M, Gl, Gl1),
    current_decomposed_assertion(Assertions, M, Pred, Status, Type, Cp, Ca, Su, Gl1, Co, RPos).
current_decomposed_assertion_(Assertions # Co2, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :-
    !,
    current_decomposed_assertion(Assertions, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co1, RPos),
    once(merge_comments(Co1, Co2, Co)).
current_decomposed_assertion_(Assertions, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :-
    ( is_decl_global(Assertions, DStatus, DType, M)
    ->once(decompose_status_and_type_1(Term, DStatus, DType, Assertions)),
      current_decomposed_assertion_(Term, M, Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos)
    ; decompose_status_and_type(Assertions, Status, Type, BodyS),
      decompose_assertion_head_body(BodyS, M, Pred, true, Cp, Ca, Su, Gl, Co, RPos),
      validate_body_sections(Type, Cp, Ca, Su, Gl, MustBeEmpty, MustNotBeEmpty),
      maplist(report_must_be_empty(Type), MustBeEmpty),
      maplist(report_must_not_be_empty(Type, RPos), MustNotBeEmpty)
    ).

report_must_be_empty(Type, Section-Props) :-
    maplist(report_must_be_empty(Type, Section), Props).

termpos_location(Pos, Loc) :-
    ignore(source_location(File, Line)),
    ( nonvar(File)
    ->( nonvar(Pos)
      ->Loc = file_term_position(File, Pos)
      ; nonvar(Line)
      ->Loc = file(File, Line, -1, _)
      ; true
      )
    ; true
    ).

report_must_be_empty(Type, Section, Prop-Pos) :-
    termpos_location(Pos, Loc),
    print_message(
        warning,
        at_location(
            Loc,
            format("In '~w' assertion, '~w' section, '~w' will be ignored",
                   [Type, Section, Prop]))).

report_must_not_be_empty(Type, Pos, Section-Prop) :-
    ( Prop = []
    ->termpos_location(Pos, Loc),
      print_message(
          warning,
          at_location(
              Loc,
              format("In '~w' assertion, missing properties in '~w' section",
                     [Type, Section])))
    ; true
    ).

combine_pi_comp(N, Head, PosL1, PosL, BCp, PCp) :-
    cleanup_parentheses(PCp, Pos),
    combine_pi_comp_(N, Head, PosL1, PosL, BCp, Pos).

combine_pi_comp_(N1, Head, PosL1, PosL, (H * P), term_position(_, _, _, _, [TPos, Pos])) :-
    arg(N1, Head, P),
    !,
    succ(N, N1),
    combine_pi_comp(N, Head, [Pos|PosL1], PosL, H, TPos).
combine_pi_comp_(N, Head, PosL, [Pos|PosL], P, Pos) :-
    arg(N, Head, P).

% EMM: Support for grouped properties

merge_props(BCp1, _,    BCp,  PCp, BCp, PCp) :- strip_module(BCp1, _, true).
merge_props(BCp,  PCp,  BCp2, _,   BCp, PCp) :- strip_module(BCp2, _, true).
merge_props(BCp1, PCp1, BCp2, PCp2, (BCp1, BCp2), term_position(_, _, _, _, [PCp1, PCp2])).

decompose_assertion_head_body(B, P1, M, Pred, BCp, PCp, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    cleanup_parentheses(P1, P),
    decompose_assertion_head_body_(B, P, M, Pred, BCp, PCp, Cp, Ca, Su, Gl, Co, CoPos, RPos).

decompose_assertion_head_body_((B1, B2), M, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos) :-
    !,
    ( decompose_assertion_head_body(B1, M, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos)
    ; decompose_assertion_head_body(B2, M, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos)
    ).
decompose_assertion_head_body_([B1|B2], M,
                              Pred, BCp, Cp, Ca, Su, Gl, Co, RPos) :-
    !,
    ( decompose_assertion_head_body(B1, M, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos)
    ; decompose_assertion_head_body(B2, M, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos)
    ).
decompose_assertion_head_body_(M:Body, CM, Pred, BCp, Cp, Ca, Su, Gl, Co, RPos) :-
    atom(M),
    callable(Body),
    !,
    % Switching the body context does not implies switching the context of the
    % compatibility properties, that is why CM should be preserved in the
    % compatibility section:
    decompose_assertion_head_body(Body, M, Pred, CM:BCp, Cp, Ca, Su, Gl, Co, RPos).
decompose_assertion_head_body_([], _, _, _, _, _, _, _, _, _) :-
    !,
    fail.
decompose_assertion_head_body_(Body, BPos, M, Pred, BCp, PCp, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    is_decl_global(Body, M),
    Body =.. [F, Head|GArgL],
    nonvar(Head),
    !,
    ( nonvar(BPos)
    ->BPos = term_position(_, _, FF, FT, [HPos|PArgL]),
      NPos = term_position(_, _, _, _, [HPos, term_position(_, _, FF, FT, [PArgL])])
    ; true
    ),
    BGl =.. [F|GArgL],
    decompose_assertion_head_body(Head + BGl, NPos, M, Pred, BCp, PCp, Cp, Ca, Su, Gl, Co, CoPos, RPos).
decompose_assertion_head_body_(Body, BPos, M, Pred, BCp2, PCp2, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    ( once(decompose_assertion_body(Body, BPos, BH1, PH1, BCp1, PCp1,
                                    BCa, PCa, BSu, PSu, BGl, PGl, BCo, PCo)),
      Body \= BH1
    ->propdef(BGl, PGl, M, Gl, Gl1),
      once(merge_props(BCp1, PCp1, BCp2, PCp2, BCp, PCp)),
      decompose_assertion_head_body(BH1, PH1, M, Pred, BCp, PCp, Cp, Ca1, Su1, Gl1, BCo1, PCo1, RPos),
      apropdef(Pred, M, BCa, PCa, Ca, Ca1),
      apropdef(Pred, M, BSu, PSu, Su, Su1),
      once(merge_comments(BCo1, PCo1, BCo, PCo, Co, CoPos))
    ; decompose_assertion_head(Body, BPos, M, Pred, BCp2, PCp2, BCp, PCp, Cp1, Ca, Su, Gl, RPos),
      apropdef(Pred, M, BCp, PCp, Cp, Cp1),
      Co = ""
    ).

decompose_assertion_head(Head, PPos, M, Pred, BCp1, PCp1, BCp, PCp, Cp, Ca, Su, Gl, HPos) :-
    cleanup_parentheses(PPos, Pos),
    decompose_assertion_head_(Head, Pos, M, Pred, BCp1, PCp1, BCp, PCp, Cp, Ca, Su, Gl, HPos).

decompose_assertion_head_(M:H, _, P, BCp1, BCp, Cp, Ca, Su, Gl, RP) :-
    atom(M),
    !,
    decompose_assertion_head(H, M, P, BCp1, BCp, Cp, Ca, Su, Gl, RP).
decompose_assertion_head_(F/A, HPos, M, M:Pred, BCp1, PCp1, BCp, PCp, Cp, Ca, Su, Gl, Pos) :-
    !,
    functor(Head, F, A),
    ( once(( BCp1 = CM:BCp2,
             PCp1 = term_position(_, _, _, _, [_, PCp2])
           ; BCp1 = BCp2,
             PCp1 = PCp2,
             CM = M
           )),
      BCp2 \= true,
      combine_pi_comp(A, Head, [], PosL, BCp2, PCp2)
    ->( nonvar(HPos)
      ->HPos = term_position(From, To, _, _, [FPos, APos]),
        ( nonvar(FPos)
        ->arg(1, FPos, FFrom),
          arg(2, FPos, FTo)
        ; true
        )
      ; true
      ),
      decompose_assertion_head_(Head, term_position(From, To, FFrom, FTo, PosL), CM,
                                CM:Pred, true, APos, BCp, PCp, Cp, Ca, Su, Gl, Pos)
    ; Pred = Head,
      Cp = [],
      Ca = [],
      Su = [],
      Gl = [],
      HPos = Pos,
      BCp = BCp1,
      PCp = PCp1
    ).
decompose_assertion_head_(Head, Pos, M, M:Pred, BCp, PCp, BCp, PCp, Cp, Ca, Su, Gl, Pos) :-
    compound(Head),
    !,
    functor(Head, F, A),
    functor(Pred, F, A),
    Pos = term_position(_, _, _, _, PosL),
    decompose_args(PosL, 1, Head, M, Pred, Cp, Ca, Su, Gl).
decompose_assertion_head_(Head, Pos, M, M:Head, BCp, PCp, BCp, PCp, [], [], [], [], Pos) :-
    atom(Head).

decompose_args([Pos|PosL], N1, Head, M, Pred, Cp1, Ca1, Su1, Gl1) :-
    arg(N1, Head, HArg),
    !,
    resolve_types_modes(HArg, Pos, M, PArg, Cp1, Ca1, Su1, Gl1, Cp2, Ca2, Su2, Gl2),
    arg(N1, Pred, PArg),
    succ(N1, N),
    decompose_args(PosL, N, Head, M, Pred, Cp2, Ca2, Su2, Gl2).
decompose_args([], _, _, _, _, [], [], [], []).

:- add_termpos(resolve_types_modes_(+,?,?,?,?,?,?,?,?,?,?)).
:- add_termpos(do_modedef(+,?,?,-,?,?,?,?,?,?,?,?,?,?)).
:- add_termpos(modedef(+,?,?,-,?,?,?,?,?,?,?,?,?,?)).
:- add_termpos(do_propdef(+,?,?,?,?)).
:- add_termpos(hpropdef(+,?,?,?,?)).

resolve_types_modes(A,     _, _, A, Cp,  Ca,  Su,  Gl,  Cp, Ca, Su, Gl) :- var(A), !.
resolve_types_modes(A1, PPos, M, A, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl) :-
    cleanup_parentheses(PPos, Pos),
    resolve_types_modes_(A1, Pos, M, A, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl).

resolve_types_modes_(A1:T, term_position(_, _, _, _, [PA1, PT]), M, A, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl) :-
    do_propdef(T, PT, M, A, Pr1, Pr2),
    cleanup_parentheses(PA1, PA11),
    do_modedef(A1, PA11, M, A, A2, PA2, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr),
    !,
    do_propdef(A2, PA2, M, A, Pr2, Pr).
resolve_types_modes_(A1, M, A, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl) :-
    do_modedef(A1, M, A, A2, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr),
    do_propdef(A2, M, A, Pr1, Pr).

do_propdef(A,  _, A, Cp,  Cp) :- var(A), !.
do_propdef(A1, M, A, Cp1, Cp) :-
    hpropdef(A1, M, A, Cp1, Cp).

do_modedef(A1, M, A, A2, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr) :-
    nonvar(A1),
    modedef(A1, M, A, A2, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr),
    !.
do_modedef(A1, APos, M, A, A2, PA1, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr) :-
    atom(A1),
    A3 =.. [A1, A],
    ( var(APos) -> true ; APos = From-To, Pos = term_position(From, To, From, To, [To-To]) ),
    modedef(A3, Pos, M, A, A2, PA1, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr),
    !.
do_modedef(A1, From-To, M, A, A2, PA1, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr) :-
    integer(A1),
    ( A1 >= 0
    ->A3 = goal_in(A1, A)
    ; A4 is -A1,
      A3 = goal_go(A4, A)
    ),
    modedef(A3, term_position(From, To, From, From, [From-From, From-To]), M, A, A2,
            PA1, Cp1, Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Pr1, Pr),
    !.
do_modedef(A1, PA1, _, _, A1, PA1, Cp1, Ca, Su, Gl, Cp, Ca, Su, Gl, Cp1, Cp).

:- add_termpos(put_prop(+,?,?,?)).

put_prop(_, Pos, Prop) --> [Prop-Pos].

% Support for modes are hard-wired here:
% ISO Modes
modedef(+A, M, B, A, Cp, Ca1, Su, Gl, Cp, Ca, Su, Gl, Ca2, Ca) :-
    ( var(A), var(Ca2)
    ->put_prop(A, M:nonvar(B), Ca1, Ca2)
    ; Ca1 = Ca2
    ). % A bit confuse hack, Ca1 come instantiated to optimize the expression
modedef(-A,   M, B, A, Cp,  Ca2, Su1, Gl,  Cp, Ca, Su, Gl, Su1, Su) :- put_prop(A, M:var(B), Ca2, Ca).
% Less restrictive - uses further instantiated:
% modedef(-(A),         _, A, B, Pos, PA, Cp,                       Ca,                Su1,  [(globprops:fi(B))-Pos|Gl], Cp, Ca, Su, Gl, Su1, Su) :- Pos = term_position(_, _, _, _, [PA]).
modedef(?A,   _, _, A, Cp1, Ca,  Su,  Gl,  Cp, Ca, Su, Gl, Cp1, Cp).
modedef(@(A), _, B, A, Cp1, Ca,  Su,  Gl1, Cp, Ca, Su, Gl, Cp1, Cp) :- put_prop(A, globprops:nfi(B), Gl1, Gl).
% PlDoc (SWI) Modes
modedef(:A1, Pos, _, B, A, PA, Cp, Ca1, Su, Gl, Cp, Ca, Su, Gl, Ca2, Ca) :-
    Pos = term_position(From, To, FFrom, FTo, [PA1]),
    % The first part of this check is not redundant if we forgot the meta_predicate declaration
    ( var(A1),
      var(Ca2)
    ->put_prop(A, Pos, typeprops:mod_qual(B), Ca1, Ca2),
      A1 = A,
      PA = PA1
    ; Ca1 = Ca2,
      A = typeprops:mod_qual(A1, B),
      PA = term_position(From, To, FFrom, FTo, [PA1, From-From])
    ).
modedef(goal_in(N,A), _, B, A, Cp,  Ca2,  Su,  Gl, Cp, Ca, Su, Gl, Ca1, Ca) :- put_prop(A, typeprops:goal(N,B), Ca2, Ca1).
modedef(goal_go(N,A), _, B, A, Cp,  Ca,   Su2, Gl, Cp, Ca, Su, Gl, Su1, Su) :- put_prop(A, typeprops:goal(N,B), Su2, Su1).
modedef(!A,           M, B, A, Cp1, Ca2,  Su,  Gl, Cp, Ca, Su, Gl, Cp1, Cp) :- put_prop(A, M:compound(B),       Ca2, Ca). % May be modified using setarg/3 or nb_setarg/3 (mutable)
% Ciao Modes:
modedef(in(A),  M, B, A, Cp,  Ca2, Su,  Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- put_prop(A, M:ground(B), Ca2, Ca1).
modedef(out(A), M, B, A, Cp,  Ca2, Su2, Gl,  Cp, Ca, Su, Gl, Su1, Su) :- put_prop(A, M:var(B), Ca2, Ca), put_prop(A, M:gnd(B), Su2, Su1).
modedef(go(A),  M, B, A, Cp1, Ca,  Su2, Gl,  Cp, Ca, Su, Gl, Cp1, Cp) :- put_prop(A, M:gnd(B), Su2, Su).
% Additional Modes (See Coding Guidelines for Prolog, Michael A. Covington, 2009):
modedef(*A,     M, B, A, Cp,  Ca2, Su,  Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- put_prop(A, M:ground(B), Ca2, Ca1).
modedef(=A,     _, B, A, Cp1, Ca,  Su,  Gl1, Cp, Ca, Su, Gl, Cp1, Cp) :- put_prop(A, globprops:nfi(B), Gl1, Gl). % The state of A is preserved
modedef(/A,     M, B, A, Cp,  Ca1, Su1, Gl1, Cp, Ca, Su, Gl, Su1, Su) :- put_prop(A, M:var(B), Ca1, Ca), put_prop(A, globprops:nsh(B), Gl1, Gl). % Like '-' but also A don't share with any other argument
modedef(>A,     _, _, A, Cp,  Ca,  Su1, Gl,  Cp, Ca, Su, Gl, Su1, Su). % Like output but A might be nonvar on entry

% nfi == not_further_inst
% nsh == not_shared

:- multifile prolog:error_message/3.

prolog:error_message(assertion(il_formed_assertion, Term)) -->
    [ 'Il formed assertion, check term ~w'-[Term]].

hpropdef(A1, PA1, M, A, Cp1, Cp) :-
    term_variables(A1, V),
    ( member(X, V), X==A ->
      Cp1 = [(M:A1)-PA1|Cp]
    ; aprops_arg(A1, M, A, PA1, Cp1, Cp)
    ).

apropdef_2(0, _, _, _, _) --> !, {fail}.
apropdef_2(N, Head, M, A, PPos) -->
    {cleanup_parentheses(PPos, Pos)},
    !,
    apropdef_2_(N, Head, M, A, Pos).

apropdef_2_(1, Head, M, A, APos) -->
    {arg(1, Head, V)},
    !,
    hpropdef(A, APos, M, V).
apropdef_2_(N1, Head, M, (P * A), term_position(_, _, _, _, [PPos, APos])) -->
    {arg(N1, Head, V)},
    !,
    {succ(N, N1)},
    apropdef_2(N, Head, M, P, PPos),
    hpropdef(A, APos, M, V).

apropdef(Var, _, _, _) --> {var(Var), !, fail}.
apropdef(_:Head, M, A, APos) -->
    {functor(Head, _, N)},
    apropdef_2(N, Head, M, A, APos),
    !.
apropdef(_, M, A, APos) --> propdef(A, APos, M).

propdef(A, APos, M) --> props_args(A, M, push, APos).

push(A, M, Pos) --> [(M:A)-Pos].

aprops_arg(A, M, V, PPos) -->
    {cleanup_parentheses(PPos, Pos)},
    aprops_arg_(A, M, V, Pos).

aprops_arg_({A}, M, V, brace_term_position(_, _, Pos)) --> !, aprops_args(A, M, V, Pos).
aprops_arg_(A,   M, V, Pos) --> aprops_args(A, M, V, Pos).

aprops_args(A, M, V, Pos) --> props_args(A, M, prop_arg(V), Pos).

:- meta_predicate props_args(?, ?, 5, ?, ?, ?).

props_args(true,   _, _, _) --> !, [].
props_args(A, M, V, PPos) -->
    {cleanup_parentheses(PPos, Pos)},
    !,
    props_args_(A, M, V, Pos).

props_args_((A, B), M, V, term_position(_, _, _, _, [PA, PB])) -->
    !,
    props_args(A, M, V, PA),
    props_args(B, M, V, PB).
props_args_((A; B), M, V, Pos) -->
    !,
    { Pos = term_position(_, _, _, _, [PA, PB]),
      props_args(A, M, V, PA, P1, []),
      pairs_keys(P1, ML1),
      maplist(cleanup_mod(M), ML1, L1),
      list_sequence(L1, C1),
      props_args(B, M, V, PB, P2, []),
      pairs_keys(P2, ML2),
      maplist(cleanup_mod(M), ML2, L2),
      list_sequence(L2, C2)
    },
    [(M:(C1;C2))-Pos].
props_args_(M:A, _, V, term_position(_, _, _, _, [_, PA])) -->
    {atom(M)},
    !,
    props_args(A, M, V, PA).
props_args_(A, M, V, Pos) --> call(V, A, M, Pos).

cleanup_mod(M, M:C, C) :- !.
cleanup_mod(_, MC, MC).

prop_arg(V, A, M, Pos) -->
    {add_arg(V, A, P, Pos, PPos)},
    [(M:P)-PPos].

expand_assertion_helper(Match, a(Match, Record, Pos), Record, Pos).

expand_assertion(M, Dict, Decl, PPos, Records, RPos) :-
    cleanup_parentheses(PPos, Pos),
    !,
    expand_assertion_(M, Dict, Decl, Pos, Records, RPos).

expand_assertion_(_, Dict, M:Decl, term_position(_, _, _, _, [_, DPos]),
                  Records, RPos) :-
    atom(M),
    !,
    expand_assertion(M, Dict, Decl, DPos, Records, RPos).
expand_assertion_(M, Dict, doc(Key, Doc),
                  term_position(From, To, FFrom, FTo, [KPos, DPos]),
                  assertions:doc_db(Key, M, Doc, Dict),
                  term_position(0, 0, 0, 0,
                                [0-0,
                                 term_position(From, To, FFrom, FTo,
                                               [KPos, 0-0, DPos, 0-0 ])])) :- !.
% Note: We MUST save the full location (File, HPos), because later we will not
% have access to source_location/2, and this will fails for further created
% clauses --EMM
expand_assertion_(CM, Dict, Assertions, APos, Records, RPos) :-
    Match=(Assertions-Dict),
    findall(a(Match, Clause, HPos),
            assertion_record_each(CM, Dict, Assertions, APos, Clause, HPos),
            ARecords),
    ARecords \= [],
    maplist(expand_assertion_helper(Match), ARecords, Records, RPos).

%! assertion_record_each(CM, Dict, Assertions, APos, Clause, TermPos) is multi.
%
%  Unifies clause with each one of the records that defines the assertion in the
%  assertion database.  Note that Clause contains in the last argument the
%  locator, this is needed to get more precision, since the location is defined
%  as file(File, Line, Pos, _), instead of the term
%  '$source_location'(File,Line):Clause

assertion_record_each(CM, Dict, Assertions, APos, Clause, TermPos) :-
    ignore(source_location(File, Line1)),
    ( nonvar(File)
    ->Loc = file(File, Line, Pos, _),
      ( var(APos)
      ->Line = Line1,
        Pos = -1
      ; true
      )
    ; true
    ),
    current_decomposed_assertion(Assertions, APos, CM, M:Head, Status,
                                 Type, CpL, CaL, SuL, GlL, Co, CoPos, HPos),
    with_mutex('get_sequence_and_inc/1', get_sequence_and_inc(Count)),
    term_variables(t(Co, CpL, CaL, SuL, GlL), ShareL),
    atom_number(AIdx, Count),
    Asr =.. [AIdx, M, Head|ShareL], % Asr also contains variable bindings. By
                                    % convention, M is in the 1st position and
                                    % Head in the 2nd, to facilitate work
    ( Clause = assertions:asr_head_prop(Asr, M, Head, Status, Type, Dict, Loc),
      SubPos = HPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
        arg(2, SubPos, To),
        TermPos = term_position(From, To, From, To,
                                [SubPos, 0-0, 0-0, 0-0, _, _, _])
      ; true
      )
    ; Co \= "",
      Clause = assertions:asr_comm(Asr, Co, Loc),
      SubPos = CoPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
        arg(2, SubPos, To),
        TermPos = term_position(From, To, From, To, [_, SubPos, _])
      ; true
      )
    ; ( Clause = assertions:AClause,
        member(AClause-PrL,
               [asr_comp(Asr, PM, Pr, Loc)-CpL,
                asr_call(Asr, PM, Pr, Loc)-CaL,
                asr_succ(Asr, PM, Pr, Loc)-SuL
               ]),
        member(MPr-SubPos, PrL),
        strip_module(MPr, PM, Pr)
      ; Clause = assertions:asr_glob(Asr, PM, Pr, Loc),
        member(MGl-GPos, GlL),
        strip_module(MGl, PM, Gl),
        add_arg(_, Gl, Pr, GPos, SubPos)
      ;
        once(( member(MGl-GPos, GlL),
               member(Gl, [declaration, declaration(_)]),
               strip_module(MGl, PM, Gl),
               add_arg(_, Gl, Pr, GPos, _),
               predicate_property(PM:Pr, implementation_module(metaprops)),
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
        predicate_property(PM:Pr, implementation_module(metaprops))
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
      ; Line = Line1,
        Pos = -1
      )
    ; true
    ).

%!  expand_assertion(+Decl, DPos, -Records, RPos) is semidet.
%
%   Process a Declaration as an assertion.  This is called in a term_expansion/2
%   of the assertion module. Fails if Decl is not a valid assertion.

expand_assertion(Decl, DPos, Records, RPos) :-
    '$current_source_module'(M),
    expand_assertion(M, Dict, Decl, DPos, Records, RPos),
    % Dict Must be assigned after expand_assertion/6 to avoid performance
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

% ----------------------------------------------------------------------------

term_expansion_decl(Decl, PPos, Records, RPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    term_expansion_decl(Decl, Pos, Records, RPos).
term_expansion_decl(Decl, term_position(_, _, _, _, [DPos]), Records, RPos) :-
    expand_assertion(Decl, DPos, Records, RPos).

term_expansion((:- Decl), DPos, Records, RPos) :-
    term_expansion_decl(Decl, DPos, Records, RPos).

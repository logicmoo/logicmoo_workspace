:- module(assrt_lib, [assertion_read/9,
		      assertion_body/7,
		      assertion_head_body/10,
		      assertion_head_body_loc/9,
		      comps_to_goal/3,
		      comps_to_goal/4,
		      assertion_records/4,
		      a_fake_body/5,
		      assertion_db/12,
		      normalize_assertion_head/9,
		      add_arg/3,
		      qualify_with/3,
		      assrt_lib_tr/4]).

:- use_module(assertions(assertions_op)).
:- use_module(library(apply)).
:- use_module(library(extra_messages), []).
:- use_module(library(lists)).
:- use_module(xlibrary(implementation_module)).

:- expects_dialect(swi).

% :- ml(160).

% Assertion reader for SWI-Prolog

:- multifile
    assrt_lib:assertion_head/7,
    assrt_lib:doc_db/4,
    assrt_lib:nodirective_error_hook/1.

:- dynamic
    assrt_lib:assertion_head/7,
    assrt_lib:doc_db/4,
    assrt_lib:nodirective_error_hook/1.

% :- volatile
%     assrt_lib:assertion_head/7,
%     assrt_lib:doc_db/4.

add_arg(_, G1, G2) :-
    var(G1),
    var(G2),
    !,
    assertion(fail),
    fail.
add_arg(H, M:G0, M:G) :- !,
    add_arg(H, G0, G).
add_arg(H, G0, G) :-
    ( nonvar(G0)
    ->G0 =.. [F|L],
      G  =.. [F,H|L]
    ; nonvar(G)
    ->G  =.. [F,H|L],
      G0 =.. [F|L]
    ).

list_conj([],     true).
list_conj([E|EL], CL) :-
    list_conj_2(EL, E, CL).

list_conj_2([E|L], E0, (E0, S)) :- list_conj_2(L, E, S).
list_conj_2([], E, E).

% conj_list(V) --> {var(V)}, !, [V].
% conj_list((A, B)) --> !,
%     conj_list(A),
%     conj_list(B).
% conj_list(A) --> [A].

a_fake_body(CompL, CallL, SuccL, GlobL0, (call(Comp), call(Call), call(Succ), call(Glob))) :-
    list_conj(CompL, Comp),
    list_conj(CallL, Call),
    list_conj(SuccL, Succ),
    ( nonvar(Glob)
    ->list_conj(GlobL, Glob),
      maplist(add_arg(_), GlobL0, GlobL)
    ; maplist(add_arg(_), GlobL0, GlobL),
      list_conj(GlobL, Glob)
    ).

% Note: assertion_db/12 encapsulates the nasty call to clause/2 and can be
% extended to fetch assertions from extra places.
%
:- multifile assertion_db/12.
assertion_db(Head, M, CM, Status, Type, Comp, Call, Succ, Glob, Comm, Dict, Loc) :-
    assertion_head_body_loc(Head, M, Status, Type, Comm, Dict, FBody, CM, Loc),
    once(a_fake_body(Comp, Call, Succ, Glob, FBody)).

assertion_head_body(Head, M, Status, Type, Comm, Dict, Pos, FBody, CM, Ref) :-
    clause(assertion_head(Head, M, Status, Type, Comm, Dict, Pos), CM:FBody, Ref).

assertion_head_body_loc(Head, M, Status, Type, Comm, Dict, FBody, CM, Loc) :-
    assertion_head_body(Head, M, Status, Type, Comm, Dict, Pos, FBody, CM, Ref),
    clause_pos_location(Ref, Pos, Loc).

clause_pos_location(Ref, Pos, Loc) :-
    ( clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line))
    ->( var(Pos)
      ->Loc = file(File, Line, -1, _)
      ; Loc = file(File, Line, Pos, _)
      )
    ; Loc = clause(Ref)
    ).

filepos_line(File, CharPos, Line, LinePos) :-
    setup_call_cleanup('$push_input_context'(filepos),
		       prolog_codewalk:filepos_line(File, CharPos, Line, LinePos),
		       '$pop_input_context').

qualify_with(CM, MProp, M:Prop) :-
    strip_module(CM:MProp, M, Prop).

% For compatibility with Ciao Libraries
assertion_read(Head, M, Status, Type, Body, Dict, File, Line0, Line1) :-
    assertion_db(Head, M, CM, Status, Type, Comp0, Call0, Succ0, Glob0,
		 Comm, Dict, Loc),
    maplist(add_arg(Head), Glob0, Glob1),
    ( M \= CM
    ->maplist(maplist(qualify_with(CM)),
	      [Comp0, Call0, Succ0, Glob1],
	      [Comp,  Call,  Succ,  Glob ])
    ; [Comp0, Call0, Succ0, Glob1] = [Comp, Call, Succ, Glob]
    ),
    assertion_body(Head, Comp, Call, Succ, Glob, Comm, Body),
    ( Loc = file_term_position(File, Pos),
      nonvar(Pos),
      arg(1, Pos, From),
      arg(2, Pos, To),
      integer(From),
      integer(To)
    ->filepos_line(File, From, Line0, _),
      filepos_line(File, To,   Line1, _)
    ; Loc = file(File, Line0, _, _)
    ->Line1 = Line0
    ; Loc = clause(Ref),
      clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line0 ))
    ->Line1 = Line0
    ; true
    ).

% ---------------------------------------------------------------------------
% :- pred normalize_assertion_body(B,F,NB)
%    # "@var{NB} is a normalized assertion body corresponding to the
%      unnomalized assertion body @var{B}.".
% ---------------------------------------------------------------------------

% MH: Added new assertions for transition. Marked as %N
% MH: No comments allowed now in basic assertions (difficult to document).
% EM: SWI-Like notation, usage of is/2 instead of +/2.
% EM: valid_cp/1 solves ambiguity of :/2 with module qualification
% --------------------------- A  B   C     D  E -- FormatId ------------------------------- %ABCDE
normalize_assertion_body((PD::DP:CP=>AP  + GP#CO), p, PD, DP,   CP,   AP,   GP,   CO) :- !. %11111%N
normalize_assertion_body((PD::DP:CP=>AP is GP#CO), p, PD, DP,   CP,   AP,   GP,   CO) :- !. %11111%N
normalize_assertion_body((PD::DP:CP=>AP  + GP   ), p, PD, DP,   CP,   AP,   GP,   "") :- !. %11110
normalize_assertion_body((PD::DP:CP=>AP is GP   ), p, PD, DP,   CP,   AP,   GP,   "") :- !. %11110
normalize_assertion_body((PD::DP:CP=>AP      #CO), p, PD, DP,   CP,   AP,   true, CO) :- !. %11101%N%N
normalize_assertion_body((PD::DP:CP=>AP         ), p, PD, DP,   CP,   AP,   true, "") :- !. %11100
normalize_assertion_body((PD::DP:CP      + GP#CO), p, PD, DP,   CP,   true, GP,   CO) :- !. %11011%N
normalize_assertion_body((PD::DP:CP     is GP#CO), p, PD, DP,   CP,   true, GP,   CO) :- !. %11011%N
normalize_assertion_body((PD::DP:CP      + GP   ), p, PD, DP,   CP,   true, GP,   "") :- !. %11010
normalize_assertion_body((PD::DP:CP     is GP   ), p, PD, DP,   CP,   true, GP,   "") :- !. %11010
normalize_assertion_body((PD::DP:CP          #CO), p, PD, DP,   CP,   true, true, CO) :- !. %11001%N
normalize_assertion_body((PD::DP:CP             ), p, PD, DP,   CP,   true, true, "") :- !. %11000
normalize_assertion_body((PD::DP   =>AP  + GP#CO), p, PD, DP,   true, AP,   GP,   CO) :- !. %10111%N
normalize_assertion_body((PD::DP   =>AP is GP#CO), p, PD, DP,   true, AP,   GP,   CO) :- !. %10111%N
normalize_assertion_body((PD::DP   =>AP  + GP   ), p, PD, DP,   true, AP,   GP,   "") :- !. %10110
normalize_assertion_body((PD::DP   =>AP is GP   ), p, PD, DP,   true, AP,   GP,   "") :- !. %10110
normalize_assertion_body((PD::DP   =>AP      #CO), p, PD, DP,   true, AP,   true, CO) :- !. %10101%N
normalize_assertion_body((PD::DP   =>AP         ), p, PD, DP,   true, AP,   true, "") :- !. %10100
normalize_assertion_body((PD::DP         + GP#CO), p, PD, DP,   true, true, GP,   CO) :- !. %10011%N
normalize_assertion_body((PD::DP        is GP#CO), p, PD, DP,   true, true, GP,   CO) :- !. %10011%N
normalize_assertion_body((PD::DP         + GP   ), p, PD, DP,   true, true, GP,   "") :- !. %10010
normalize_assertion_body((PD::DP        is GP   ), p, PD, DP,   true, true, GP,   "") :- !. %10010
normalize_assertion_body((PD::DP             #CO), d, PD, DP,   true, true, true, CO) :- !. %10001%N
normalize_assertion_body((PD::DP                ), d, PD, DP,   true, true, true, "") :- !. %10000
normalize_assertion_body((PD    :CP=>AP  + GP#CO), p, PD, true, CP,   AP,   GP,   CO) :- valid_cp(CP), !. %01111%N
normalize_assertion_body((PD    :CP=>AP is GP#CO), p, PD, true, CP,   AP,   GP,   CO) :- valid_cp(CP), !. %01111%N
normalize_assertion_body((PD    :CP=>AP  + GP   ), p, PD, true, CP,   AP,   GP,   "") :- valid_cp(CP), !. %01110
normalize_assertion_body((PD    :CP=>AP is GP   ), p, PD, true, CP,   AP,   GP,   "") :- valid_cp(CP), !. %01110
normalize_assertion_body((PD    :CP=>AP      #CO), s, PD, true, CP,   AP,   true, CO) :- valid_cp(CP), !. %01101%N
normalize_assertion_body((PD    :CP=>AP         ), s, PD, true, CP,   AP,   true, "") :- valid_cp(CP), !. %01100
normalize_assertion_body((PD    :CP      + GP#CO), g, PD, true, CP,   true, GP,   CO) :- valid_cp(CP), !. %01011%N
normalize_assertion_body((PD    :CP     is GP#CO), g, PD, true, CP,   true, GP,   CO) :- valid_cp(CP), !. %01011%N
normalize_assertion_body((PD    :CP      + GP   ), g, PD, true, CP,   true, GP,   "") :- valid_cp(CP), !. %01010
normalize_assertion_body((PD    :CP     is GP   ), g, PD, true, CP,   true, GP,   "") :- valid_cp(CP), !. %01010
normalize_assertion_body((PD    :CP          #CO), c, PD, true, CP,   true, true, CO) :- valid_cp(CP), !. %01001%N
normalize_assertion_body((PD    :CP             ), c, PD, true, CP,   true, true, "") :- valid_cp(CP), !. %01000
normalize_assertion_body((PD       =>AP  + GP#CO), p, PD, true, true, AP,   GP,   CO) :- !. %00111%N
normalize_assertion_body((PD       =>AP is GP#CO), p, PD, true, true, AP,   GP,   CO) :- !. %00111%N
normalize_assertion_body((PD       =>AP  + GP   ), p, PD, true, true, AP,   GP,   "") :- !. %00110
normalize_assertion_body((PD       =>AP is GP   ), p, PD, true, true, AP,   GP,   "") :- !. %00110
normalize_assertion_body((PD       =>AP      #CO), s, PD, true, true, AP,   true, CO) :- !. %00101%N
normalize_assertion_body((PD       =>AP         ), s, PD, true, true, AP,   true, "") :- !. %00100
normalize_assertion_body((PD             + GP#CO), g, PD, true, true, true, GP,   CO) :- !. %00011%N
normalize_assertion_body((PD            is GP#CO), g, PD, true, true, true, GP,   CO) :- !. %00011%N
normalize_assertion_body((PD             + GP   ), g, PD, true, true, true, GP,   "") :- !. %00010
normalize_assertion_body((PD            is GP   ), g, PD, true, true, true, GP,   "") :- !. %00010
normalize_assertion_body((BO                 #CO), P, PD, DP,   CP,   AP,   GP,   CO) :-
    normalize_assertion_body(BO, P, PD, DP, CP, AP, GP, ""), !. %00001%N
normalize_assertion_body((PD                 #CO), p, PD, true, true, true, true, CO) :- !. %00001%N
normalize_assertion_body((PD                    ), t, (PD                    ), true, true, true, true, ""). %00000
%% ---------------------------------------------------------------------------------------- %% ----

fix_format_global(p, p).
fix_format_global(d, p).
fix_format_global(s, p).
fix_format_global(g, g).
fix_format_global(c, g).
fix_format_global(t, g).

valid_cp(CP) :- \+ invalid_cp(CP).

invalid_cp(_/_).

assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
	       (Pred::Compat:Call=>Succ+Comp#Comm)).

assrt_type(pred).
assrt_type(prop).
assrt_type(decl).
assrt_type(func).
assrt_type(calls).
assrt_type(success).
assrt_type(comp).
assrt_type(entry).
assrt_type(exit).
assrt_type(test).
assrt_type(texec).
assrt_type(modedef).

assrt_status(true).
assrt_status(false).
assrt_status(check).
assrt_status(checked).
assrt_status(trust).
assrt_status(trace).
assrt_status(debug).

% ---------------------------------------------------------------------------
% :- pred default_assrt_status(+assrt_type,-assrt_status)
% # "Defines the status to be used for a given assertion type, if an
%    assertion status is not specified explicitly.".
% ---------------------------------------------------------------------------

default_assrt_status(entry,   true) :- !. % ???
default_assrt_status(modedef, true) :- !. % ???
default_assrt_status(X,       check) :-
    assrt_type(X),
    !.

normalize_status_and_type(Assertions, APos, AssrtStatus, AssrtType, UBody, BPos) :-
    normalize_status_and_type_1(Assertions, APos, AssrtStatus, AssrtType, UBody, BPos),
    status_and_type(AssrtStatus, AssrtType).

normalize_status_and_type_1(Assertions, term_position(_, _, _, _, [BPos]),
			  _, AssrtType, UBody, BPos) :-
    Assertions =.. [AssrtType, UBody].
normalize_status_and_type_1(Assertions, term_position(_, _, _, _, [_, BPos]),
			  AssrtStatus, AssrtType, UBody, BPos) :-
    Assertions =.. [AssrtType, AssrtStatus, UBody].

status_and_type(AssrtStatus, AssrtType) :-
    assrt_type(AssrtType),
    ( default_assrt_status(AssrtType, AssrtStatus)
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

% To Avoid attempts to execute asertions (must be declarations):
generate_nodirective_error.

%% ---------------------------------------------------------------------------
% :- pred assertion_format(AssrtType, Code) :: assrt_type * assrt_format_code
% # "@var{Code} describes an admissible format in which assertions of
%    the class @var{AssrtType} can be written.".
%% ---------------------------------------------------------------------------

% Admissible assertion formats:
assertion_format(pred, X) :- assrt_format_code(X).
assertion_format(decl, X) :- assrt_format_code(X). % ?
assertion_format(prop, X) :- assrt_format_code(X).
assertion_format(test, X) :- assrt_format_code(X). % For unit-test -- EMM
% Obsolete: delete eventually...
% assertion_format(type,    t).
% Not needed any more...
% assertion_format(type,    g). %% Added for now to put typedef there...
% assertion_format(compat,  d). %% Not using these as basic any more?!
assertion_format(calls,   c).
assertion_format(success, s).
% Entry for unit-test -- EMM
assertion_format(texec, g).
assertion_format(texec, c).
% DTM: New assertion type
assertion_format(exit, s).
assertion_format(comp, g).
% These to become obsolete?
assertion_format(entry, c).
assertion_format(entry, t).

% Not an assertion any more, but a status instead
% assertion_format(trust,   X) :- assrt_format_code(X).
assertion_format(modedef, X) :- assrt_format_code(X).

% :- prop assrt_format_code(X) + regtype
%    # "@var{X} is a designator for an assertion format.".

assrt_format_code(p).
assrt_format_code(d).
assrt_format_code(c).
assrt_format_code(s).
assrt_format_code(g).
assrt_format_code(t).

% EMM: Support for grouped global properties

current_body(M:BodyS, _, term_position(_, _, _, _, [_, PosS]), Body, BPos, Gl0, Gl) :-
    atom(M), !,
    current_body(BodyS, M, PosS, Body, BPos, Gl0, Gl).
current_body(BodyS + PGl, M, term_position(_, _, _, _, [PosS, _]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(PGl, M, Gl0, Gl1),
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).
current_body(BodyS is PGl#Co, M,
	     term_position(From, To, _, _,
			   [PosS, term_position(_, _, FFrom, FTo, [_, PosCo])]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(PGl, M, Gl0, Gl1),
    current_body(BodyS#Co, M, term_position(From, To, FFrom, FTo, [PosS, PosCo]),
		 Body, BPos, Gl1, Gl).
current_body(BodyS is PGl, M, term_position(_, _, _, _, [PosS, _]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(PGl, M, Gl0, Gl1),
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).
current_body(BodyS, M, PosS, Body, BPos, Gl0, Gl) :-
    ( body_member(BodyS, PosS, Lit, LPos)
    *->
      current_body(Lit, M, LPos, Body, BPos, Gl0, Gl)
    ; Body = M:BodyS,
      Gl = Gl0,
      BPos = PosS
    ).

body_member(Body, _, _, _) :-
    var(Body), !, fail.
body_member([], _, _, _) :- !, fail.
body_member([A|B], list_position(From, To, [APos|EPos], TPos), Lit, LPos) :- !,
    ( Lit=A, LPos=APos
    ; Lit=B, LPos=list_position(From, To, EPos, TPos)
    ).
body_member((A, B), term_position(_, _, _, _, [APos, BPos]), Lit, LPos) :- !,
    ( Lit=A, LPos=APos
    ; Lit=B, LPos=BPos
    ).

current_normalized_assertion(Assertions, CM, APos, M:Head, Status, Type,
			     Cp, Ca, Su, Gl, Co, HPos) :-
    current_unfold_assertion(Assertions, CM, APos, M:Head, Status, Type,
			     Cp0, Ca0, Su0, Gl0, Co, HPos),
    once(maplist(maplist(compact_module_call(CM)),
    		 [Cp0, Ca0, Su0, Gl0 ],
    		 [Cp,  Ca,  Su,  Gl])).

%% compact_module_call(+, +, -) is det.
%
% Reduce redundant modules in nested lists and sequences:
%
compact_module_call(M, M:C, C) :- !.
compact_module_call(M, (A0;B0), (A;B)) :- !,
    compact_module_call(M, A0, A),
    compact_module_call(M, B0, B).
compact_module_call(M, [A0|B0], [A|B]) :- !,
    compact_module_call(M, A0, A),
    compact_module_call(M, B0, B).
compact_module_call(_, C, C).

current_unfold_assertion(Assertions  + PGl, M, term_position(_, _, _, _, [APos, _]),
		     Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :- !,
    propdef(PGl, M, Gl, Gl0),
    current_unfold_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, RPos).
current_unfold_assertion(Assertions is PGl, M, term_position(_, _, _, _, [APos, _]),
		     Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :- !,
    propdef(PGl, M, Gl, Gl0),
    current_unfold_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, RPos).
current_unfold_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl, Co, RPos) :-
    once(normalize_status_and_type(Assertions, APos, Status, Type, BodyS, PosS)),
    current_body(BodyS, M, PosS, BM:Body, BPos, Gl, Gl0),
    normalize_assertion_head_body(Body, BM, BPos, Pred, Format, Cp, Ca, Su, Gl0, Co, RPos),
    (Gl \= [] -> fix_format_global(Format, GFormat) ; GFormat = Format),
    assertion_format(Type, GFormat).

:- use_module(library(prolog_codewalk), []).

normalize_assertion_head_body(Body, M, BPos, Pred, Format, Cp, Ca, Su, Gl, Co, RPos) :-
    normalize_assertion_body(Body, Format, Head, PCp, PCa, PSu, PGl, Co),
    ignore(prolog_codewalk:subterm_pos(Head, Body, BPos, HPos)),
    normalize_assertion_head(Head, M, HPos, Pred, Cp0, Ca0, Su0, Gl0, RPos),
    apropdef(Pred, M, PCp, Cp, Cp0),
    apropdef(Pred, M, PCa, Ca, Ca0),
    apropdef(Pred, M, PSu, Su, Su0),
    propdef(PGl, M, Gl, Gl0).

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
    normalize_args(1, Head, M, Pred, Cp, Ca, Su, Gl).
normalize_assertion_head(Head, M, Pos, M:Head, [], [], [], [], Pos) :-
    atom(Head).

normalize_args(N0, Head, M, Pred, Cp0, Ca0, Su0, Gl0) :-
    arg(N0, Head, HArg),
    !,
    resolve_types_modes(HArg, M, PArg, Cp0, Ca0, Su0, Gl0, Cp1, Ca1, Su1, Gl1),
    arg(N0, Pred, PArg),
    succ(N0, N),
    normalize_args(N, Head, M, Pred, Cp1, Ca1, Su1, Gl1).
normalize_args(_, _, _, _, [], [], [], []).

resolve_types_modes(A,    _, A, Cp,  Ca,  Su,  Gl,  Cp, Ca, Su, Gl) :- var(A), !.
resolve_types_modes(A0:T, M, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl) :-
    do_propdef(T, M, A, Pr0, Pr1),
    do_modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !,
    do_propdef(A1, M, A, Pr1, Pr).
resolve_types_modes(A0, M, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl) :-
    do_modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    do_propdef(A1, M, A, Pr0, Pr).

do_propdef(A, _, A, Cp,  Cp) :- var(A), !.
do_propdef(A1, M, A, Cp1, Cp) :-
    hpropdef(A1, M, A, Cp1, Cp).

do_modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    nonvar(A0),
    modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !.
do_modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    atom(A0),
    A2 =.. [A0, A],
    modedef(A2, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !.
do_modedef(A0, M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    integer(A0),
    modedef(is_pred(A, A0), M, A1, A, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !.
do_modedef(A0, _, A0, _, Cp0, Ca, Su, Gl, Cp, Ca, Su, Gl, Cp0, Cp).

% Support for modes are hard-wired here:
% ISO Modes
modedef(+(A),         M, A, B, Cp,                 Ca0,               Su,         Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :-
    (var(A), var(Ca1) -> Ca0 = [M:nonvar(B)|Ca1] ; Ca0 = Ca1). % A bit confuse hack, Ca1 come instantiated to optimize the expression
modedef(-(A),         M, A, B, Cp,       [M:var(B)|Ca],               Su0,        Gl,  Cp, Ca, Su, Gl, Su0, Su).
modedef(?(A),         _, A, _, Cp0,                Ca,                Su,         Gl,  Cp, Ca, Su, Gl, Cp0, Cp).
modedef(@(A),         _, A, B, Cp0,                Ca,                Su, [nativeprops:nfi(B)|Gl], Cp, Ca, Su, Gl, Cp0, Cp).
% PlDoc (SWI) Modes
modedef(:(A0 ),       _, A, B, Cp,                 Ca0,               Su,          Gl, Cp, Ca, Su, Gl, Ca1, Ca) :-
     % The first part of this check is not redundant if we forgot the meta_predicate declaration
    (var(A0 ), var(Ca1) -> Ca0 = [nativeprops:mod_qual(B)|Ca1], A0 = A ; Ca0 = Ca1, A = nativeprops:mod_qual(B, A0 )).
modedef(is_pred(A,N), _, A, B, Cp,  [nativeprops:is_pred(B,N)|Ca0],  Su,          Gl,  Cp, Ca, Su, Gl, Ca0, Ca).
modedef('!'(A),       M, A, B, Cp0, [M:compound(B)|Ca],               Su,          Gl,  Cp, Ca, Su, Gl, Cp0, Cp). % May be modified using setarg/3 or nb_setarg/3 (mutable)
% Ciao Modes:
modedef(in(A),        M, A, B, Cp,    [M:ground(B)|Ca0],              Su,          Gl,  Cp, Ca, Su, Gl, Ca0, Ca).
modedef(out(A),       M, A, B, Cp,       [M:var(B)|Ca],  [M:ground(B)|Su0],        Gl,  Cp, Ca, Su, Gl, Su0, Su).
modedef(go(A),        M, A, B, Cp0,                Ca,   [M:ground(B)|Su],         Gl,  Cp, Ca, Su, Gl, Cp0, Cp).
% Additional Modes (See Coding Guidelines for Prolog, Michael A. Covington, 2009):
modedef('*'(A),       M, A, B, Cp,    [M:ground(B)|Ca0],              Su,          Gl,  Cp, Ca, Su, Gl, Ca0, Ca).
modedef('='(A),       _, A, B, Cp0,                Ca,                Su,  [nativeprops:nfi(B)|Gl], Cp, Ca, Su, Gl, Cp0, Cp). % The state of A is preserved
modedef('/'(A),       M, A, B, Cp,       [M:var(B)|Ca],               Su0, [nativeprops:nsh(B)|Gl], Cp, Ca, Su, Gl, Su0, Su). % Like '-' but also A don't share with any other argument
modedef('>'(A),       _, A, _, Cp, Ca,                                Su0,         Gl,  Cp, Ca, Su, Gl, Su0, Su). % Like output but A might be nonvar on entry

				% nfi == not_further_inst
				% nsh == not_shared

:- multifile prolog:error_message/3.

prolog:error_message(assertion(il_formed_assertion, Term)) -->
    [ 'Il formed assertion, check term ~w'-[Term]].

hpropdef(A0, M, A, Cp0, Cp) :-
    term_variables(A0, V),
    ( member(X, V), X==A ->
      Cp0 = [M:A0|Cp]
    ; aprops_arg(A0, M, A, Cp0, Cp)
    ).

apropdef_2(0, _, _, _) --> !, {fail}.
apropdef_2(1, Head, M, A) -->
    {arg(1, Head, V)},
    !,
    hpropdef(A, M, V).
apropdef_2(N0, Head, M, (P * A)) -->
    {arg(N0, Head, V)},
    !,
    {N is N0 - 1},
    apropdef_2(N, Head, M, P),
    hpropdef(A, M, V).

apropdef(Var, _, _) --> {var(Var), !, fail}.
apropdef(_:Head, M, A) -->
    {functor(Head, _, N)},
    apropdef_2(N, Head, M, A), !.
apropdef(_, M, A) --> propdef(A, M).

propdef(A, M) --> props_args(A, M, push).

push(A, M) --> [M:A].

aprops_arg('{}'(A), M, V) --> !, aprops_args(A, M, V).
aprops_arg(A,       M, V) -->    aprops_args(A, M, V).

aprops_args(A, M, V) --> props_args(A, M, prop_arg(V)).

:- meta_predicate props_args(?, ?, 4, ?, ?).

props_args(true,   _, _) --> !, [].
props_args((A, B), M, V) --> !,
    props_args(A, M, V),
    props_args(B, M, V).
props_args((A; B), M, V) --> !,
    {props_args(A, M, V, P1, []),
     list_conj(P1, C1)},
    {props_args(B, M, V, P2, []),
     list_conj(P2, C2)},
    [(C1;C2)].
props_args(M:A, _, V) -->
    {atom(M)}, !,
    props_args(A, M, V).
props_args(A, M, V) --> call(V, A, M).

% add_module(M, P, M:P).

prop_arg(V, A, M) -->
    {add_arg(V, M:A, P)},
    [P].

comp_to_goal_assrt(M:Comp, M:Body0, Body) :- !,
	comp_to_goal_assrt(Comp, Body0, Body).
comp_to_goal_assrt(Comp, Body0, Body) :-
	Comp  =.. [PropName, _|Args],
	Body0 =.. [PropName, Body|Args].

:- pred comps_to_goal/3 #
	"This predicate allows to compound a list of global properties in to
	sucessive meta-calls".

comps_to_goal(Comp) -->
	comps_to_goal(Comp, comp_to_goal_assrt).

:- pred comps_to_goal/3 # "This predicate allows to compound a list of
	global properties in to successive meta-calls, but in the
	third argument you can use your own selector.".

:- test comps_to_goal(Comp, Goal, Pred) :
	(
	    Comp = [not_fails(p(A)), is_det(p(A)), exception(p(A), exc)],
	    Pred = p(A)
	) =>
	(
	    Goal = not_fails(is_det(exception(p(A),exc)))
	).

:- meta_predicate comps_to_goal(?, 3, ?, ?).
comps_to_goal([],             _) --> [].
comps_to_goal([Check|Checks], Goal) -->
    comps_to_goal2(Checks, Check, Goal).

:- meta_predicate comps_to_goal2(?, ?, 3, ?, ?).
comps_to_goal2([], Check, Goal) -->
    call(Goal, Check).
comps_to_goal2([Check|Checks], Check0, Goal) -->
    call(Goal, Check0),
    comps_to_goal2(Checks, Check, Goal).

assrt_lib_tr((:- Decl), Records, M, Dict) :-
    assertion_records(M, Dict, Decl, _, Records, _).

assertion_records_helper(Match, a(Match, Record, Pos), Record, Pos).

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
% have access to source_location/2, and it will fails for further created
% clauses --EMM
assertion_records(CM, Dict, Assertions, APos, Records, RPos) :-
    Match=(Assertions-Dict),
    Clause0 = (assrt_lib:assertion_head(Head, M, Status, Type, Co, Dict, Pos) :- FBody),
    ( source_location(File, Line0 )
    ->Clause = ('$source_location'(File, Line):Clause0 )
    ; Clause = Clause0
    ),
    findall(a(Match, Clause, HPos),
	    ( ( nonvar(APos) ->NonVarAPos=true ; NonVarAPos = fail ),
	      current_normalized_assertion(Assertions, CM, APos, HM:Head, Status,
					   Type, Cp, Ca, Su, Gl, Co, HPos),
	      implementation_module(HM:Head, M),
	      ( nonvar(File)
	      ->( nonvar(HPos),
		  arg(1, HPos, HFrom),
		  integer(HFrom)
		->filepos_line(File, HFrom, Line, Pos)
		; ( NonVarAPos=true,
		    arg(1, APos, AFrom),
		    integer(AFrom)
		  ->filepos_line(File, AFrom, Line, Pos)
		  ; Line = Line0
		  )
		)
	      ; true		% Loc will be instantiated later
	      ),
	      a_fake_body(Cp, Ca, Su, Gl, FBody)
	    ),
	    ARecords),
    ARecords \= [], % Is a valid assertion if it defines at least one Record
    maplist(assertion_records_helper(Match), ARecords, Records, RPos).

assertion_records(Decl, DPos, Records, RPos) :-
    '$set_source_module'(M, M),
    assertion_records(M, Dict, Decl, DPos, Records, RPos),
    %% Dict Must be assigned after assertion_records/6 to avoid performance
    %% issues --EMM
    b_getval('$variable_names', Dict).

/*
:- use_module(library(dialect/ciao), []).

ciao:declaration_hook(Decl, Records) :-
    assertion_records(Decl, _, Records, _).
*/

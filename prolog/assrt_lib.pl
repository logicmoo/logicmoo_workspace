:- module(assrt_lib,
	  [assertion_read/9,
	   assertion_body/7,
	   comps_to_goal/3,
	   comps_to_goal/4,
	   assertion_records/4,
	   assertion_db/13,
	   asr_head_prop/8,
	   asr_head_prop/7,
	   asr_glob/4,
	   asr_comp/4,
	   asr_call/4,
	   asr_succ/4,
	   asr_comm/3,
	   prop_asr/4,
	   asr_aprop/4,
	   prop_asr/5,
	   collect_prop/3,
	   normalize_assertion_head/7,
	   qualify_with/3,
	   assrt_lib_tr/4]).

:- use_module(library(assertions_op)).
:- use_module(library(apply)).
:- use_module(library(extra_messages), []).
:- use_module(library(lists)).
:- use_module(library(implementation_module)).
:- use_module(library(subpos_utils)).
:- use_module(library(prolog_codewalk), []).

:- expects_dialect(swi).

% :- ml(160).

% Assertion reader for SWI-Prolog

% asr_* declared multifile to allow extensibility. At this point you extend
% concrete assertions (not abstractions nor fake ones, since they will be
% collected by the run-time checker, for instance)

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

asr_head_prop(Asr, CM, Head, Status, Type, Comm, Dict, Loc) :-
    asr_head_prop(Asr, CM, Head, Status, Type, Dict, Loc),
    asr_comm(Asr, Comm, _).

prop_asr(head, M:P, From, Asr) :- asr_head_prop(Asr, M, P, _, _, _, From).
prop_asr(stat,   P, From, Asr) :- asr_head_prop(Asr, _, _, P, _, _, From).
prop_asr(type,   P, From, Asr) :- asr_head_prop(Asr, _, _, _, P, _, From).
prop_asr(dict,   D, From, Asr) :- asr_head_prop(Asr, _, _, _, _, D, From).
prop_asr(comm,   C, From, Asr) :- asr_comm(Asr,    C, From).
prop_asr(comp, M:P, From, Asr) :- asr_comp(Asr, M, P, From).
prop_asr(call, M:P, From, Asr) :- asr_call(Asr, M, P, From).
prop_asr(succ, M:P, From, Asr) :- asr_succ(Asr, M, P, From).
prop_asr(glob, M:P, From, Asr) :- asr_glob(Asr, M, P, From).

% Extensible accessor to assertion properties, ideal to have different views of
% assertions, to extend the assertions or to create ancillary assertions (see
% module assrt_meta.pl for an example). The first argument is wrapped to
% facilitate indexing.  Note that it is recommended that multiple clauses of
% this predicate be mutually exclusive.
:- multifile asr_aprop/4.
asr_aprop(rtcheck(Asr), Key, Prop, From) :-
    prop_asr(Key, Prop, From, Asr).

:- meta_predicate prop_asr(?, 0, -, +, +).

prop_asr(Key, M:P, IM, From, Asr) :-
    implementation_module(M:P, IM),
    prop_asr(Key, C:P, From, Asr),
    implementation_module(C:P, IM).

% :- volatile
%     assrt_lib:assertion_head/7,
%     assrt_lib:doc_db/4.

add_arg(_, G1, G2, _, _) :-
    var(G1),
    var(G2), !,
    assertion(fail),
    fail.
add_arg(H, M:G0, M:G,
	term_position(From, To, FFrom, FTo, [MPos, Pos0 ]),
	term_position(From, To, FFrom, FTo, [MPos, Pos  ])) :- !,
    add_arg(H, G0, G, Pos0, Pos).
add_arg(H, G0, G,
	Pos,
	term_position(From, To, FFrom, FTo, [0-0|PosL])) :-
    ( Pos = term_position(From, To, FFrom, FTo, PosL)
    ->true
    ; Pos = From-To
    ->FFrom = From,
      FTo = To,
      PosL = []
    ),
    ( nonvar(G0)
    ->G0 =.. [F|L],
      G  =.. [F,H|L]
    ; nonvar(G)
    ->G  =.. [F,H|L],
      G0 =.. [F|L]
    ).

:- meta_predicate collect(?,^,-).
collect(Tmpl, Goal, List) :-
    (bagof(Tmpl, Goal, List) *-> true ; List = []).

collect_props(Asr, CM, CompL, CallL, SuccL, GlobL) :-
    collect_prop(asr_comp(Asr), CM, CompL),
    collect_prop(asr_call(Asr), CM, CallL),
    collect_prop(asr_succ(Asr), CM, SuccL),
    collect_prop(asr_glob(Asr), CM, GlobL).

:- meta_predicate collect_prop(3,+,-).
collect_prop(GenProp, CM, PropL) :-
    collect(MProp,
	    (M,Prop,From)^( call(GenProp, M, Prop, From),
			    ( M \= CM
			    ->MProp = M:Prop
			    ; MProp = Prop
			    )
			  ), PropL).

assertion_db(Asr, Head, M, CM, Status, Type, Comp, Call, Succ, Glob, Comm, Dict, Loc) :-
    asr_head_prop(Asr, CM, Head, Status, Type, Dict, Loc),
    ( asr_comm(Asr, Comm, _)
    ->true
    ; Comm = ""
    ),
    implementation_module(CM:Head, M),
    collect_props(Asr, CM, Comp, Call, Succ, Glob).

filepos_line(File, CharPos, Line, LinePos) :-
    setup_call_cleanup('$push_input_context'(filepos),
		       prolog_codewalk:filepos_line(File, CharPos, Line, LinePos),
		       '$pop_input_context').

qualify_with(CM, MProp, M:Prop) :-
    strip_module(CM:MProp, M, Prop).


set_arg_1(Head, Glob) :- arg(1, Glob, Head).

% For compatibility with Ciao Libraries
assertion_read(Head, M, Status, Type, Body, Dict, File, Line0, Line1) :-
    assertion_db(_, Head, M, CM, Status, Type, Comp0, Call0, Succ0, Glob0,
		 Comm, Dict, Loc),
    maplist(set_arg_1(Head), Glob0),
    ( M \= CM
    ->maplist(maplist(qualify_with(CM)),
	      [Comp0, Call0, Succ0, Glob0],
	      [Comp,  Call,  Succ,  Glob ])
    ; [Comp0, Call0, Succ0, Glob0] = [Comp, Call, Succ, Glob]
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

var_location(Term, Pos, Var, Loc) :-
    ( var(Var),
      subterm_location_eq(L, Var, Term)
    ->subpos_location(L, Pos, Loc)
    ; true
    ).

term_expansion((normalize_assertion_body(Assertion, Format, PD, DP, CP, AP, GP, CO) :- Body),
	       (normalize_assertion_body(Assertion, Format, Pos,
					 PD,  DP,  CP,  AP,  GP,  CO,
					 PDP, DPP, CPP, APP, GPP, COP) :- Body)) :-
    memberchk(Body, [!, (valid_cp(CP), !)]), !,
    maplist(var_location(Assertion, Pos),
	    [PD,  DP,  CP,  AP,  GP,  CO],
	    [PDP, DPP, CPP, APP, GPP, COP]).

term_expansion((normalize_assertion_body(Assertion, Format, PD, DP, CP, AP, GP, CO)),
	       (normalize_assertion_body(Assertion, Format, Pos,
					 PD,  DP,  CP,  AP,  GP,  CO,
					 PDP, DPP, CPP, APP, GPP, COP))) :-
    maplist(var_location(Assertion, Pos),
	    [PD,  DP,  CP,  AP,  GP,  CO],
	    [PDP, DPP, CPP, APP, GPP, COP]).
term_expansion((normalize_assertion_body(Assertion, Format, PD, DP, CP, AP, GP, CO) :-
	            normalize_assertion_body(BO, Format, PD, DP, CP, AP, GP, ""), !),
	       (normalize_assertion_body(Assertion, Format, Pos,
					 PD,  DP,  CP,  AP,  GP,  CO,
					 PDP, DPP, CPP, APP, GPP, COP) :-
	            normalize_assertion_body(BO, Format, BOP,
					     PD,  DP,  CP,  AP,  GP,  "",
					     PDP, DPP, CPP, APP, GPP, _), !)) :-
    Assertion = BO#CO,
    maplist(var_location(Assertion, Pos), [BO, CO], [BOP, COP]).

% Without Pos information:
% normalize_assertion_body(Assertion, Format, PD, DP, CP, AP, GP, CO) :-
%     normalize_assertion_body(Assertion, Format, _, PD, DP, CP, AP, GP, CO, _, _, _, _, _, _).

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
normalize_assertion_body((PD                    ), t, PD, true, true, true, true, ""). %00000
%% ---------------------------------------------------------------------------------------- %% ----

fix_format_global(p, p).
fix_format_global(d, p).
fix_format_global(s, p).
fix_format_global(g, g).
fix_format_global(c, g).
fix_format_global(t, g).

valid_cp(CP) :- \+ invalid_cp(CP).

invalid_cp(_/_).

assertion_body( Pred, Comp,Call, Succ,Glob,Comm,
	       (Pred::Comp:Call=>Succ+Glob#Comm)).

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
current_body(BodyS + BGl, M, term_position(_, _, _, _, [PosS, PGl]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(BGl, M, PGl, Gl0, Gl1),
    current_body(BodyS, M, PosS, Body, BPos, Gl1, Gl).
current_body(BodyS is BGl#Co, M,
	     term_position(From, To, _, _,
			   [PosS, term_position(_, _, FFrom, FTo, [PGl, PosCo])]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(BGl, M, PGl, Gl0, Gl1),
    current_body(BodyS#Co, M, term_position(From, To, FFrom, FTo, [PosS, PosCo]),
		 Body, BPos, Gl1, Gl).
current_body(BodyS is BGl, M, term_position(_, _, _, _, [PosS, PGl]),
	     Body, BPos, Gl0, Gl) :- !,
    propdef(BGl, M, PGl, Gl0, Gl1),
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

current_normalized_assertion(Assertions  + BGl, M, term_position(_, _, _, _, [APos, PGl]),
		     Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :- !,
    propdef(BGl, M, PGl, Gl, Gl0),
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, CoPos, RPos).
current_normalized_assertion(Assertions is BGl, M, term_position(_, _, _, _, [APos, PGl]),
		     Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :- !,
    propdef(BGl, M, PGl, Gl, Gl0),
    current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl0, Co, CoPos, RPos).
current_normalized_assertion(Assertions, M, APos, Pred, Status, Type, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    normalize_status_and_type(Assertions, APos, Status, Type, BodyS, PosS),
    current_body(BodyS, M, PosS, BM:Body, BPos, Gl, Gl0),
    normalize_assertion_head_body(Body, BM, BPos, Pred, Format, Cp, Ca, Su, Gl0, Co, CoPos, RPos),
    (Gl \= [] -> fix_format_global(Format, GFormat) ; GFormat = Format),
    assertion_format(Type, GFormat).

normalize_assertion_head_body(Body, M, BPos, Pred, Format, Cp, Ca, Su, Gl, Co, CoPos, RPos) :-
    normalize_assertion_body(Body, Format, BPos,
			     Head, BCp, BCa, BSu, BGl, Co,
			     HPos, PCp, PCa, PSu, PGl, CoPos),
    normalize_assertion_head(Head, M, HPos, Pred, Cp0, Ca0, Su0, Gl0, RPos),
    apropdef(Pred, M, BCp, PCp, Cp, Cp0),
    apropdef(Pred, M, BCa, PCa, Ca, Ca0),
    apropdef(Pred, M, BSu, PSu, Su, Su0),
    propdef(BGl, M, PGl, Gl, Gl0).

normalize_assertion_head(Spec, M, Pred, Comp, Call, Succ, Glob) :-
    normalize_assertion_head(Spec, M, _, Pred, CompP, CallP, SuccP, GlobP, _),
    maplist(pairs_keys, [CompP, CallP, SuccP, GlobP], [Comp, Call, Succ, Glob]).

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
do_modedef(A0, M, A1, A, APos, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    atom(A0),
    A2 =.. [A0, A],
    ( var(APos) -> true ; APos = From-To, Pos = term_position(From, To, From, To, [To-To]) ),
    modedef(A2, M, A1, A, Pos, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr), !.
do_modedef(A0, M, A1, A, From-To, PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr) :-
    integer(A0),
    modedef(is_pred(A, A0), M, A1, A, term_position(From, To, From, From, [From-From, From-To]), PA1, Cp0, Ca0, Su0, Gl0, Cp, Ca, Su, Gl, Pr0, Pr),
    !.
do_modedef(A0, _, A0, _, PA0, PA0, Cp0, Ca, Su, Gl, Cp, Ca, Su, Gl, Cp0, Cp).

% Support for modes are hard-wired here:
% ISO Modes
modedef(+(A),         M, A, B, Pos, PA, Cp,                       Ca0,               Su,                           Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- Pos = term_position(_, _, _, _, [PA]),
    (var(A), var(Ca1) -> Ca0 = [(M:nonvar(B))-Pos|Ca1] ; Ca0 = Ca1). % A bit confuse hack, Ca1 come instantiated to optimize the expression
modedef(-(A),         M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],               Su0,                          Gl,  Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]).
modedef(?(A),         _, A, _, Pos, PA, Cp0,                      Ca,                Su,                           Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
modedef(@(A),         _, A, B, Pos, PA, Cp0,                      Ca,                Su, [(nativeprops:nfi(B))-Pos|Gl], Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
% PlDoc (SWI) Modes
modedef(:(A0 ),       _, A, B, Pos, PA, Cp,                       Ca0,               Su,                           Gl,  Cp, Ca, Su, Gl, Ca1, Ca) :- Pos = term_position(From, To, FFrom, FTo, [PA0 ]),
     % The first part of this check is not redundant if we forgot the meta_predicate declaration
    (var(A0 ), var(Ca1) -> Ca0 = [(nativeprops:mod_qual(B))-Pos|Ca1], A0 = A ; Ca0 = Ca1, A = nativeprops:mod_qual(B, A0 ), PA = term_position(From, To, FFrom, FTo, [From-From, PA0 ])).
modedef(is_pred(A,N), _, A, B, Pos, PA, Cp,  [(nativeprops:is_pred(B,N))-Pos|Ca0],   Su,         Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [PA, _]).
modedef('!'(A),       M, A, B, Pos, PA, Cp0, [(M:compound(B))-Pos|Ca],               Su,         Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]). % May be modified using setarg/3 or nb_setarg/3 (mutable)
% Ciao Modes:
modedef(in(A),        M, A, B, Pos, PA, Cp,    [(M:ground(B))-Pos|Ca0],                 Su,      Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [PA]).
modedef(out(A),       M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],  [(M:gnd(B))-Pos|Su0],    Gl,  Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]).
modedef(go(A),        M, A, B, Pos, PA, Cp0,                      Ca,   [(M:gnd(B))-Pos|Su],     Gl,  Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]).
% Additional Modes (See Coding Guidelines for Prolog, Michael A. Covington, 2009):
modedef('*'(A),       M, A, B, Pos, PA, Cp,    [(M:ground(B))-Pos|Ca0],              Su,                            Gl,  Cp, Ca, Su, Gl, Ca0, Ca) :- Pos = term_position(_, _, _, _, [PA]).
modedef('='(A),       _, A, B, Pos, PA, Cp0,                      Ca,                Su,  [(nativeprops:nfi(B))-Pos|Gl], Cp, Ca, Su, Gl, Cp0, Cp) :- Pos = term_position(_, _, _, _, [PA]). % The state of A is preserved
modedef('/'(A),       M, A, B, Pos, PA, Cp,       [(M:var(B))-Pos|Ca],               Su0, [(nativeprops:nsh(B))-Pos|Gl], Cp, Ca, Su, Gl, Su0, Su) :- Pos = term_position(_, _, _, _, [PA]). % Like '-' but also A don't share with any other argument
modedef('>'(A),       _, A, _, term_position(_, _, _, _, [PA]), PA, Cp, Ca,          Su0,        Gl,  Cp, Ca, Su, Gl, Su0, Su). % Like output but A might be nonvar on entry

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

aprops_arg({A}, M, V, brace_term_position(_, _, Pos)) --> !, aprops_args(A, M, V, Pos).
aprops_arg(A,   M, V, Pos) --> aprops_args(A, M, V, Pos).

aprops_args(A, M, V, Pos) --> props_args(A, M, prop_arg(V), Pos).

:- meta_predicate props_args(?, ?, 5, ?, ?, ?).

props_args(true,   _, _, _) --> !, [].
props_args((A, B), M, V, term_position(_, _, _, _, [PA, PB])) --> !,
    props_args(A, M, V, PA),
    props_args(B, M, V, PB).
props_args((A; B), M, V, Pos) --> !,
    { Pos = term_position(_, _, _, _, [PA, PB]),
      props_args(A, M, V, PA, P1, []),
      list_conj(P1, C1),
      props_args(B, M, V, PB, P2, []),
      list_conj(P2, C2)
    },
    [(C1;C2)-Pos].
props_args(M:A, _, V, term_position(_, _, _, _, [_, PA])) -->
    {atom(M)}, !,
    props_args(A, M, V, PA).
props_args(A, M, V, Pos) --> call(V, A, M, Pos).

list_conj([],     true).
list_conj([E|EL], CL) :-
    list_conj_2(EL, E, CL).

list_conj_2([E|L], E0-_, (E0, S)) :- list_conj_2(L, E, S).
list_conj_2([], E-_, E).

% conj_list(V) --> {var(V)}, !, [V].
% conj_list((A, B)) --> !,
%     conj_list(A),
%     conj_list(B).
% conj_list(A) --> [A].

% add_module(M, P, M:P).

prop_arg(V, A, M, Pos) -->
    {add_arg(V, A, P, Pos, PPos)},
    [(M:P)-PPos].

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
    Asr =.. [AIdx|ShareL], % Idx also contains variable bindings
    Clause = assrt_lib:AClause,
    ( AClause = asr_head_prop(Asr, M, Head, Status, Type, Dict, Loc),
      SubPos = HPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
	arg(2, SubPos, To),
	TermPos = term_position(From, To, From, To,
				[SubPos, 0-0, 0-0, 0-0, _, _, _])
      ; true
      )
    ; Co \= "",
      AClause = asr_comm(Asr, Co, Loc),
      SubPos = CoPos,
      ( nonvar(SubPos)
      ->arg(1, SubPos, From),
	arg(2, SubPos, To),
	TermPos = term_position(From, To, From, To, [_, SubPos, _])
      ; true
      )
    ; ( member(AClause-PrL,
	       [asr_comp(Asr, PM, Pr, Loc)-CpL,
		asr_call(Asr, PM, Pr, Loc)-CaL,
		asr_succ(Asr, PM, Pr, Loc)-SuL
	       ]),
	member(MPr-SubPos, PrL),
	strip_module(MPr, PM, Pr)
      ; AClause = asr_glob(Asr, PM, Pr, Loc),
	member(MGl-GPos, GlL),
	strip_module(MGl, PM, Gl),
	add_arg(_, Gl, Pr, GPos, SubPos)
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

assertion_records(Decl, DPos, Records, RPos) :-
    '$set_source_module'(M, M),
    assertion_records(M, Dict, Decl, DPos, Records, RPos),
    %% Dict Must be assigned after assertion_records/6 to avoid performance
    %% issues --EMM
    b_getval('$variable_names', Dict).

:- dynamic sequence/1.
sequence(1).

get_sequence_and_inc(S) :-
    retract(sequence(S)),
    succ(S, S2),
    assertz(sequence(S2)).

/*
:- use_module(library(dialect/ciao), []).

ciao:declaration_hook(Decl, Records) :-
    assertion_records(Decl, _, Records, _).
*/

:- module(location_utils,
	[property_location/3, predicate_location/2, property_from/3,
	 record_location_dynamic/3, predicate_from/2, cleanup_locations/4,
	 from_location/2, from_to_file/2, in_set/2, in_dir/2, r_true/1,
	 conj_chks/2, record_location_meta/5, record_location/4,
	 option_allchk/3, option_filechk/3, option_dirchk/3, compound_chks/2]).

:- use_module(library(lists)).
:- use_module(library(database_fact)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(implemented_in)).
:- use_module(library(implementation_module)).
:- use_module(library(record_locations)).

from_location(From, Location) :-
	prolog:message_location(From, Location, []),
	!.
from_location(From, From).

from_to_file(clause_term_position(ClauseRef, _), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(clause(ClauseRef), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(file_term_position(File, _), File).
from_to_file(file(File, _, _, _), File).

in_set(FileL, File) :-
    memberchk(File, FileL).

in_dir(DirL, File) :-
    member(Dir, DirL),
    directory_file_path(Dir, _, File),
    !.

r_true(_).

option_files([],     r_true)  :- !.
option_files(AliasL, FileChk) :-
    findall(File,
	    ( member(Alias, AliasL),
	      absolute_file_name(Alias, Pattern, [file_type(prolog),
						  solutions(all)]),
	      expand_file_name(Pattern, FileL0 ),
	      member(File, FileL0 )),
	    FileU),
    sort(FileU, FileL),
    FileChk = in_set(FileL).

option_dirs([],     r_true)  :- !.
option_dirs(AliasL, FileChk) :-
    findall(Dir,
	    ( member(Alias, AliasL),
	      absolute_file_name(Alias, Pattern, [file_type(directory),
						  solutions(all)]),
	      expand_file_name(Pattern, DirL),
	      member(Dir, DirL)),
	    DirU),
    sort(DirU, DirL),
    FileChk = in_dir(DirL).

option_dirchk(OptionL0, OptionL, DirChk) :-
    select_option(dirs(AliasL0), OptionL0, OptionL1, []),
    select_option(dir( Alias),   OptionL1, OptionL,  []),
    ( Alias = []
    ->AliasL = AliasL0
    ; AliasL = [Alias|AliasL0]
    ),
    option_dirs(AliasL, DirChk).

option_pred([],   r_true)  :- !.
option_pred(Head, PredChk) :-
    findall(File,
	    ( implemented_in(Head, From, _),
	      from_to_file(From, File)
	    ), FileU),
    sort(FileU, FileL),
    PredChk = in_set(FileL).

option_filechk(OptionL0, OptionL, FileChk) :-
    select_option(files(AliasL0), OptionL0, OptionL1, []),
    select_option(file( Alias),   OptionL1, OptionL,  Alias),
    ( var(Alias) % ignore
    ->AliasL = AliasL0
    ; AliasL = [Alias|AliasL0]
    ),
    option_files(AliasL, FileChk).

option_predchk(OptionL0, OptionL, PredChk) :-
    select_option(pred(Head), OptionL0, OptionL, []),
    option_pred(Head, PredChk).

simplify_chks([], []).
simplify_chks([Elem|L0 ], L) :-
    simplify_chk(Elem, L0, L1, L, L2),
    simplify_chks(L1, L2).

simplify_chk(in_set(A), L0, [in_set(I)|L1], L, L) :-
    select(in_set(B), L0, L1),
    !,
    intersection(A, B, I).
simplify_chk(r_true, L0, L0, L, L) :- !.
simplify_chk(Elem, L0, L0, [Elem|L], L).

conj_chks(AllChkL, File) :-
    forall(member(AllChk, AllChkL), call(AllChk, File)).

compound_chks_rec([],       r_true) :- !.
compound_chks_rec([AllChk], AllChk) :- !.
compound_chks_rec(AllChkL, conj_chks(AllChkL)).

rm_conj_chks([],     []).
rm_conj_chks([E|L1], L2) :-
    rm_conj_chk(E, L2, L),
    rm_conj_chks(L1, L).

rm_conj_chk(conj_chks(L0 ), L1, L) :- !,
    append(L1, L0, L).
rm_conj_chk(E, [E|L], L).

compound_chks -->
    rm_conj_chks,
    compound_chks_0.

compound_chks_0 -->
    sort,
    simplify_chks,
    compound_chks_rec.

option_allchk(OptionL0, OptionL, AllChk) :-
    option_dirchk(OptionL0,  OptionL1, DirChk),
    option_filechk(OptionL1, OptionL2, FileChk),
    option_predchk(OptionL2, OptionL,  PredChk),
    compound_chks_0([DirChk,FileChk,PredChk], AllChk).

% For preds + decls
property_location(Prop, Declaration, Location) :-
    property_from(Prop, Declaration, From),
    from_location(From, Location).

% non det
property_from(Head, Declaration, From) :-
    ( ( dec_location(Head, Declaration, From)
      ; def_location(Head, Declaration, From)
      ) *-> true
    ; From = []
    ).

dec_location(Head0/0, Declaration, From) :-
    normalize_head(Head0, M:Head),
    declaration_location(Head, M, Declaration, From).
dec_location(M:Head0, Declaration, From) :-
    normalize_head(M:Head0, M:Head),
    declaration_location(Head, M, Declaration, From).

clause_from(Ref, clause(Ref)).

def_location(Head/I, clause(I), From) :-
    normalize_head(Head, P),
    nth_clause(P, I, Ref),
    clause_from(Ref, From).
def_location(M:Head, Declaration, From) :-
    normalize_head(M:Head, P),
    predicate_properties(P, List),
    ( List = []
    ->Declaration = predicate
    ; Declaration = predicate(List)
    ),
    predicate_from(P, From).

:- meta_predicate predicate_location(:,-).

predicate_location(P, Loc) :-
    predicate_from(P, From),
    from_location(From, Loc).

:- meta_predicate predicate_properties(:,-).
predicate_properties(P, List) :-
	findall(Prop,
		( predicate_property(P, Prop),
		  \+ memberchk(Prop, [interpreted,
				      visible,
				      number_of_rules(_),
				      number_of_clauses(_),
				      imported_from(_),
				      file(_),
				      indexed(_),
				      line_count(_)])
		), List).

:- meta_predicate predicate_from(:,-).

predicate_from(P, file(File, Line, -1, 0)) :-
	predicate_property(P, file(File)),
	predicate_property(P, line_count(Line)).

record_location_goal(Goal, Type, From) :-
    normalize_head(Goal, M:Head),
    ground(M),
    callable(Head),
    record_location(Head, M, Type, From).

record_location(Head, M, Type, From) :-
    ( declaration_location(Head, M, Type, From)
    ->true
    ; assertz(declaration_location(Head, M, Type, From))
    ).

record_location_meta_each(MCall, IM, From, FactBuilder, Recorder) :-
    MCall = CM:Call,
    call(FactBuilder, Def, IM:Call, Fact),
    ( (var(Fact) ; Fact = _:_) ->
      MFact = Fact
    ; implementation_module(CM:Fact, M),
      MFact = M:Fact
    ),
    call(Recorder, MFact, dynamic(Def, CM, IM:Call), From).

:- meta_predicate record_location_meta(+,?,+,3,3).
record_location_meta(MCall, IM, From, FactBuilder, Recorder) :-
    implementation_module(MCall, IM),
    ( record_location_meta_each(MCall, IM, From, FactBuilder, Recorder),
      fail
    ; true
    ).

record_location_dynamic(MCall, IM, From) :-
    record_location_meta(MCall, IM, From, database_fact_ort, record_location_goal).

cleanup_locations(Head, M, Type, From) :-
    retractall(declaration_location(Head, M, Type, From)).

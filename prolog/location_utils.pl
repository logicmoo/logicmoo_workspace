:- module(location_utils,
	[property_location/3, predicate_location/2, property_from/3,
	 record_location_dynamic/3, predicate_from/2, cleanup_locations/4,
	 from_location/2, from_to_file/2, in_set/2, in_dir/2, r_true/1,
	 record_location_meta/5, record_location/4]).

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
    extra_location(Head, M, Declaration, From).
dec_location(M:Head0, Declaration, From) :-
    normalize_head(M:Head0, M:Head),
    extra_location(Head, M, Declaration, From).

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
    ( extra_location(Head, M, Type, From)
    ->true
    ; assertz(extra_location(Head, M, Type, From))
    ).

record_location_meta_each(MCall, M, From, FactBuilder, Recorder) :-
    implementation_module(MCall, IM),
    MCall = CM:Call,
    call(FactBuilder, Def, IM:Call, Fact),
    ( var(Fact)
    ->MFact = Fact,
      FM = CM
    ; ( Fact = FM:FH
      ->( atom(FM),
	  callable(FH)
	->implementation_module(FM:FH, M)
	; M = FM
	)
      ; FM = CM,
	FH = Fact,
	implementation_module(FM:FH, M)
      ),
      MFact = M:FH
    ),
    call(Recorder, MFact, dynamic(Def, FM, IM:Call), From).

:- meta_predicate record_location_meta(+,?,+,3,3).
record_location_meta(MCall, M, From, FactBuilder, Recorder) :-
    ( record_location_meta_each(MCall, M, From, FactBuilder, Recorder),
      fail
    ; true
    ).

record_location_dynamic(MCall, M, From) :-
    record_location_meta(MCall, M, From, database_fact_ort, record_location_goal).

cleanup_locations(Head, M, Type, From) :-
    retractall(extra_location(Head, M, Type, From)).

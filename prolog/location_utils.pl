:- module(location_utils,
	[property_location/3, predicate_location/2, property_from/3,
	 record_location_dynamic/3, predicate_from/2, cleanup_locations/4,
	 from_location/2, in_set/2, in_dir/2, all_call_refs/5,
	 record_location_meta/5, record_location/4]).

:- use_module(library(lists)).
:- use_module(library(prolog_codewalk), []).
:- use_module(library(clambda)).
:- use_module(library(database_fact)).
:- use_module(library(extra_location)).
:- use_module(library(implementation_module)).
:- use_module(library(normalize_head)).
:- use_module(library(static_strip_module)).

from_location(From, Location) :-
    prolog:message_location(From, Location, []),
    !.
from_location(From, From).

in_set(FileL, File) :-
    memberchk(File, FileL).

in_dir(DirL, File) :-
    member(Dir, DirL),
    directory_file_path(Dir, _, File),
    !.

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

prop_t(use). % In some cases is already tracked by prolog:called_by/4@database_fact
prop_t(def).

all_call_refs(lit,  Goal,  _, CM, CM:Goal).
all_call_refs(Prop, Goal, IM, CM, CM:Fact) :-
    prop_t(Prop),
    database_fact(Prop, IM:Goal, Fact).

record_location_goal(MGoal, CM, Type, Call, _, From) :-
    normalize_head(MGoal, M:Head),
    ground(M),
    callable(Head),
    record_location(Head, M, dynamic(Type, CM, Call), From).

record_location(Head, M, Type, From) :-
    ( loc_dynamic(Head, M, Type, From)
    ->true
    ; assertz(loc_dynamic(Head, M, Type, From))
    ).

record_location_meta_each(MCall, M, From, FactBuilder, Recorder) :-
    static_strip_module(MCall, Call, CM, M),
    implementation_module(MCall, IM),
    call(FactBuilder, Type, Call, IM, CM, MFact),
    static_strip_module(MFact, Fact, FM, CM),
    implementation_module(FM:Fact, M),
    call(Recorder, M:Fact, FM, Type, IM:Call, CM, From).

:- meta_predicate record_location_meta(+,?,+,5,6).
record_location_meta(MCall, M, From, FactBuilder, Recorder) :-
    ( record_location_meta_each(MCall, M, From, FactBuilder, Recorder),
      fail
    ; true
    ).

record_location_dynamic(MCall, M, From) :-
    record_location_meta(MCall, M, From, \T^G^M^_^F^database_fact_ort(T,G,M,F),
			 record_location_goal).

cleanup_locations(Head, M, Type, From) :-
    retractall(loc_declaration(Head, M, Type, From)).

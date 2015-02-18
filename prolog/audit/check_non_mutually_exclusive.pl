:- module(check_non_mutually_exclusive, []).

:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(referenced_by)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1,
    mutually_exclusive_predicate/1.

:- dynamic mutually_exclusive_db/1.

audit:check(non_mutually_exclusive, Result, OptionL0 ) :-
    option_allchk(OptionL0, OptionL1, FileChk),
    select_option(module(M), OptionL1, _, M),
    findall(Pairs, check_non_mutually_exclusive(from_chk(FileChk), M, Pairs), Result).

check_non_mutually_exclusive(FromChk, M, warning-(Ref-LocIdxs)) :-
    Ref0 = M:_,
    mutually_exclusive_predicate(Ref0),
    normalize_head(Ref0, Ref),
    collect_non_mutually_exclusive(FromChk, Ref),
    retract(mutually_exclusive_db(LocIdxs0)),
    sort(LocIdxs0, LocIdxs).

collect_non_mutually_exclusive(FromChk, Ref) :-
    nth_clause(Ref, Index, ClauseRef),
    From = clause(ClauseRef),
    call(FromChk, From),
    from_location(From, Loc),
    clause(Ref, _, ClauseRef),
    findall(LocIdx, mutually_exclusive(Ref, Index, LocIdx), LocIdxs0),
    LocIdxs0 \= [],
    ( mutually_exclusive_db(LocIdxs1),
      intersection(LocIdxs0, LocIdxs1, LocIdxs2),
      LocIdxs2 \= []
    ->
      retract(mutually_exclusive_db(LocIdxs1)),
      union(LocIdxs0, LocIdxs1, LocIdxs3)
    ; LocIdxs3 = LocIdxs0
    ),
    assertz(mutually_exclusive_db([Loc/Index|LocIdxs3])),
    fail.
collect_non_mutually_exclusive(_, _).

mutually_exclusive(Ref, Index, Loc/MutExcl) :-
    clause(Ref, _, ClauseRef),
    nth_clause(Ref, MutExcl, ClauseRef),
    MutExcl < Index,
    From = clause(ClauseRef),
    from_location(From, Loc).

prolog:message(acheck(non_mutually_exclusive)) -->
    ['---------------------------------',nl,
     'Non Mutually Exclusive Predicates',nl,
     '---------------------------------',nl,
     'The predicates below are marked as mutually_exclusive, but they have', nl,
     'non mutually exclusive clauses. You can resolve the ambiguity unifying', nl,
     'the non mutual exclusive clauses or changing the specification of such', nl,
     'predicates.', nl, nl].
prolog:message(acheck(non_mutually_exclusive, PI-LocCIs)) -->
    check:predicate(PI),
    [' have non mutually exclusive clauses:', nl],
    maplist_dcg(group_non_mut_ex, LocCIs).

locindex_index(_/I, I).

locindex_loccl(Loc/I, Loc/[' clause ~w'-[I]]).

group_non_mut_ex(LocIdxs) -->
    {maplist(locindex_index, LocIdxs, Idxs)},
    {maplist(locindex_loccl, LocIdxs, LCIs)},
    ['    ', 'Match between clauses ~w at'-[Idxs], nl],
    referenced_by(LCIs).

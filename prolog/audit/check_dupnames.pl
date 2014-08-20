:- module(check_dupnames, []).

:- use_module(library(check), []).
:- use_module(library(location_utils)).
:- use_module(library(normalize_head)).

:- multifile
    prolog:message//1,
    ignore_dupnames/3,
    audit:audit/4.

ignore_dupnames(_, _, refactor).
ignore_dupnames(_, _, i18n_refactor).
ignore_dupnames(term_expansion, 2, _).
ignore_dupnames(term_expansion, 4, _).
ignore_dupnames(goal_expansion, 2, _).
ignore_dupnames(goal_expansion, 4, _).

audit:check(dupnames, Ref, Result, OptionL0 ) :-
    option_allchk(OptionL0, _OptionL, FileChk),
    check_dupnames(Ref, FileChk, Result).

check_dupnames(Ref0, FileChk, Result) :-
    normalize_head(Ref0, Ref),
    Ref = M:H,
    findall((F/A)-M,
	    ( current_predicate(M:F/A),
	      \+ ignore_dupnames(F, A, M),
	      functor(H,F,A),
	      predicate_property(M:H, file(File)),
	      call(FileChk, File),
	      \+predicate_property(M:H, imported_from(_))
	    ), PU),
    sort(PU, PL),
    group_pairs_by_key(PL, GL),
    findall(G, ( member(G, GL),
		 G = (F/A)-ML,
		 G \= _-[_],
		 functor(H, F, A),
		 once(( member(M, ML),
			predicate_property(M:H, exported)
		      ))
	       ), Groups),
    group_pairs_by_key(Pairs, Groups),
    maplist(add_location, Pairs, Result).

add_location(PI-M, warning-(PI-(LocDL/M))) :-
    findall(Loc/D, property_location(M:PI, D, Loc), LocDU),
    sort(LocDU, LocDL).

prolog:message(acheck(dupnames)) -->
    ['-----------------',nl,
     'Duplicated predicate names',nl,
     '-----------------',nl,
     'The predicates below has been implemented in different modules,', nl,
     'but has the same names and at least one of them has been exported,', nl,
     'making difficult its import in other modules.  Would be a symptom', nl,
     'of duplicated functionality.  This can be fixed by merging the', nl,
     'duplicated code, or if this is not the case, by renaming one of', nl,
     'the predicates and/or unexporting the unused ones.', nl, nl].
prolog:message(acheck(dupnames, PI-PIL)) -->
    ['~w is duplicated:'-[PI], nl],
    maplist_dcg(message_duplicated(PI), PIL).

message_duplicated(PI, LocDL/M) -->
    maplist_dcg(message_duplicated_each(M:PI), LocDL).

message_duplicated_each(PI, Loc/D) -->
    ['  '], Loc, ['duplicated ~w: ~w'-[D, PI], nl].

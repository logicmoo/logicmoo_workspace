:- module(check_dupcode, []).

:- use_module(library(check), []).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(normalize_head)).

:- multifile
    prolog:message//1,
    ignore_dupcode/4,
    ignore_dupgroup/3,
    audit:audit/4.

dupcode_type(name).
% dupcode_type(clause).
% dupcode_type(predicate).

ignore_dupcode(_, _, refactor,       name).
ignore_dupcode(_, _, i18n_refactor,  name).
ignore_dupcode(term_expansion, 2, _, name).
ignore_dupcode(term_expansion, 4, _, name).
ignore_dupcode(goal_expansion, 2, _, name).
ignore_dupcode(goal_expansion, 4, _, name).

audit:check(dupcode, Ref, Result, OptionL0 ) :-
    option_allchk(OptionL0, _OptionL, FileChk),
    check_dupcode(Ref, FileChk, Result).

duptype_elem(name,      H, M, F/A,         M) :- functor(H, F, A).
duptype_elem(clause,    H, M, (H :- Body), M:F/A-Idx)
:-
    nth_clause(M:H, Idx, Ref),
    clause(M:H, Body, Ref),
    functor(H, F, A).
duptype_elem(predicate, H, M, ClauseL, M) :-
    findall((H :- B), clause(M:H, B), ClauseL).

ignore_dupgroup(_-[_]) :- !.	% no duplicates
ignore_dupgroup((DupType-Elem)-DupIdL) :-
    ignore_dupgroup(DupType, Elem, DupIdL).

ignore_dupgroup(name, F/A, ML) :-
    ignore_dupname(F, A, ML).

ignore_dupname(F, A, ML) :-
    functor(H, F, A),
    \+ ( member(M, ML),
	 predicate_property(M:H, exported)
       ), !.

check_dupcode(Ref0, FileChk, Result) :-
    normalize_head(Ref0, Ref),
    Ref = M:H,
    findall((DupType-Elem)-DupId,
	    ( current_predicate(M:F/A),
	      functor(H, F, A),
	      \+predicate_property(M:H, imported_from(_)),
	      dupcode_type(DupType),
	      \+ ignore_dupcode(F, A, M, DupType),
	      predicate_property(M:H, file(File)),
	      call(FileChk, File),
	      duptype_elem(DupType, H, M, Elem, DupId)
	    ), PU),
    sort(PU, PL),
    group_pairs_by_key(PL, GL),
    findall(G, ( member(G, GL),
		 \+ ignore_dupgroup(G)
	       ), Groups),
    group_pairs_by_key(Pairs, Groups),
    maplist(add_location, Pairs, Result).

elem_location(name,   PI, M, Loc/D) :- property_location(M:PI, D, Loc).
elem_location(clause, (H :- _), M:F/A-Idx, Loc/D)
:-
    functor(H, F, A),
    property_location((M:H)/Idx, D, Loc).
elem_location(predicate, [(H :- _)|_], M, Loc/D)
:-
    property_location(M:H, D, Loc).

add_location((DupType-Elem)-DupId, warning-((DupType/Elem)-(LocDL/DupId))) :-
    findall(LocD, elem_location(DupType, Elem, DupId, LocD), LocDU),
    sort(LocDU, LocDL).

element_name(name, PI, PI).
element_name(clause,     (H :- _),    F/A) :- functor(H, F, A).
element_name(predicate, [(H :- _)|_], F/A) :- functor(H, F, A).

dupid_name(name,      M,       M).
dupid_name(clause,    M:_/_-_, M).
dupid_name(predicate, M,       M).

prolog:message(acheck(dupcode)) -->
    ['---------------',nl,
     'Duplicated Code',nl,
     '---------------',nl,
     'The elements below has been implemented in different modules,', nl,
     'but are duplicates.  Would be a symptom of duplicated functionality.', nl,
     'In the case of predicate names, at least one has been exported,', nl,
     'making difficult to import it in other modules without clash risk.', nl,
     'This can be fixed by merging the duplicated code, or by refactoring', nl,
     'one of the duplicated to aovid this warning.', nl, nl].
prolog:message(acheck(dupcode, (DupType/Elem)-LocDL)) -->
    {element_name(DupType, Elem, Name)},
    ['~w ~w is duplicated:'-[DupType, Name], nl],
    maplist_dcg(message_duplicated(DupType, Name), LocDL).

message_duplicated(DupType, EName, LocDL/DupId) -->
    {dupid_name(DupType, DupId, Name)},
    maplist_dcg(message_duplicated_each(EName, Name), LocDL).

message_duplicated_each(EName, Name, Loc/D) -->
    ['  '], Loc, ['duplicated ~w: ~w in ~w'-[D, EName, Name], nl].

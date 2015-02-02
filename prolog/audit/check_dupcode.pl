:- module(check_dupcode, []).

:- use_module(library(check), []).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).
:- use_module(library(record_locations)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(option_utils)).
:- use_module(library(ungroup_keys_values)).

:- multifile
    prolog:message//1,
    ignore_dupcode/4,
    ignore_dupgroup/3,
    audit:audit/4.

:- dynamic duptype/1.

% Note: the order of clauses is important, to allow remove redundant information,
% that is, 'predicate' implies 'clause' implies 'name' duplication.
%
duptype(meta_predicate).
duptype(predicate).
duptype(clause).
duptype(name).

% Use the same group key to allow filtering of redundant messages.
%
element_group(meta_predicate, M:H,     M:F/A) :- functor(H, F, A).
element_group(predicate,      _:F/A,   F/A).
element_group(clause,         _:F/A-_, F/A).
element_group(name,           _:F/A,   F/A).

ignore_dupcode(Name, _, _, _) :- atom_concat('__aux_wrapper_', _, Name).
ignore_dupcode(_, _, refactor,       name).
ignore_dupcode(_, _, i18n_refactor,  name).
ignore_dupcode(term_expansion, 2, _, name).
ignore_dupcode(term_expansion, 4, _, name).
ignore_dupcode(goal_expansion, 2, _, name).
ignore_dupcode(goal_expansion, 4, _, name).
ignore_dupcode('$exported_op', 3, _, _).
ignore_dupcode('$mode', 2, _, _).
ignore_dupcode('$pred_option', 4, system, _).
ignore_dupcode('$included', 4, system, _).
ignore_dupcode('$load_context_module', 3, system, _).

audit:check(dupcode, Ref, Result, OptionL0 ) :-
    option_allchk(OptionL0, _OptionL, FileChk),
    check_dupcode(Ref, FileChk, Result).

%% duptype_elem(+DupType, +Head, +Module, -Elem, -DupId) is multi
%
% For a given Element of the language, returns a duplication key and an
% associated value
%
duptype_elem(name,   H, M, F/A,   M:F/A) :- functor(H, F, A).
duptype_elem(clause, H, M, DupId, M:F/A-Idx) :-
    nth_clause(M:H, Idx, Ref),
    clause(M:H, Body, Ref),
    functor(H, F, A),
    variant_sha1((H :- Body), DupId).
duptype_elem(predicate, H, M, DupId, M:F/A) :-
    findall((H :- B), clause(M:H, B), ClauseL),
    variant_sha1(ClauseL, DupId),
    functor(H, F, A).
duptype_elem(meta_predicate, H, M, M:F/A, M:H) :-
    extra_location(H, M, meta_predicate, _),
    functor(H, F, A).

ignore_dupgroup(_-[_]) :- !.	% no duplicates
ignore_dupgroup((DupType-_)-ElemL) :-
    ignore_dupgroup(DupType, ElemL).

ignore_dupgroup(name, PIL) :-
    ignore_dupname(PIL).

ignore_dupname(PIL) :-
    \+ ( append(_, [M:F/A|PIL2], PIL),
	 functor(H, F, A),
	 predicate_property(M:H, exported),
	 member(M2:F2/A2, PIL2),
	 functor(H2, F2, A2),
	 predicate_property(M2:H2, exported)
       ).

check_dupcode(Ref0, FileChk, Result) :-
    normalize_head(Ref0, Ref),
    Ref = M:H,
    findall((DupType-DupId)-Elem,
	    ( current_predicate(M:F/A),
	      functor(H, F, A),
	      \+predicate_property(M:H, imported_from(_)),
	      duptype(DupType),
	      \+ ignore_dupcode(F, A, M, DupType),
	      predicate_property(M:H, file(File)),
	      call(FileChk, File),
	      duptype_elem(DupType, H, M, DupId, Elem)
	    ), PU),
    sort(PU, PL),
    group_pairs_by_key(PL, GL),
    findall(G, ( member(G, GL),
		 \+ ignore_dupgroup(G)
	       ), Groups),
    ungroup_keys_values(Groups, Pairs),
    clean_redundants(Pairs, CPairs),
    maplist(add_location, CPairs, Result).

pair_group(Pair, GKey-(DupType-(DupId/Elem))) :-
    Pair = (DupType-DupId)-Elem,
    element_group(DupType, Elem, GKey).

clean_redundants(Pairs, CPairs) :-
    maplist(pair_group, Pairs, GPairs),
    sort(GPairs, GSorted),
    group_pairs_or_sort(GSorted, Groups),
    maplist(clean_redundant_group, Groups, CGroups),
    ungroup_keys_values(CGroups, CPairs).

clean_redundant_group(GKey-Group, (DupType/GKey)-List) :-
    duptype(DupType),
    memberchk(DupType-List, Group), !.

elem_property(name,           PI,        PI,        _).
elem_property(clause,         M:F/A-Idx, (M:H)/Idx, _) :- functor(H, F, A).
elem_property(predicate,      M:F/A,     M:H,       _) :- functor(H, F, A).
elem_property(meta_predicate, M:H,       (M:H)/0,   meta_predicate).

elem_location(DupType, Elem, D, Loc) :-
    elem_property(DupType, Elem, Prop, D),
    property_location(Prop, D, Loc).

add_location(DupType/GKey-DupId/Elem,
	     warning-(DupType/GKey-(DupId-(LocDL/Elem)))) :-
    findall(Loc/D, (elem_location(DupType, Elem, D, Loc), D \= goal), LocDU),
    sort(LocDU, LocDL).

prolog:message(acheck(dupcode)) -->
    ['---------------',nl,
     'Duplicated Code',nl,
     '---------------',nl,
     'The elements below would has been implemented in different modules,', nl,
     'but are duplicates.  Would be a symptom of duplicated functionality.', nl,
     'In the case of predicate names, at least two has been exported,', nl,
     'making difficult to import it in other modules without clash risk.', nl,
     'This can be fixed by merging the duplicated code, or by refactoring', nl,
     'one of the duplicated to avoid this warning.', nl, nl].
prolog:message(acheck(dupcode, (DupType/GKey)-LocDL)) -->
    ['~w ~w is duplicated:'-[DupType, GKey], nl],
    maplist_dcg(message_duplicated, LocDL).

message_duplicated(_-[LocD|LocDL]) -->
    message_duplicated('* ', LocD),
    maplist_dcg(message_duplicated('  '), LocDL).

message_duplicated(Pre, LocDL/Elem) -->
    maplist_dcg(message_duplicated(Pre, Elem), LocDL).

message_duplicated(Pre, Elem, Loc/D) -->
    [Pre], Loc, ['duplicated ~w: ~w'-[D, Elem], nl].

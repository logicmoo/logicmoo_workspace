/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(check_dupcode, []).

:- use_module(library(check), []).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(extra_location)).
:- use_module(library(maplist_dcg)).
:- use_module(library(option_utils)).
:- use_module(library(ungroup_keys_values)).
:- use_module(library(clambda)).

:- multifile
    prolog:message//1,
    ignore_dupcode/3.

:- dynamic duptype/1.

% Note: the order of clauses is important, to allow remove redundant information,
% that is, 'predicate' implies 'clause' implies 'name' duplication.
%
% duptype(meta_predicate).
duptype(declaration).
duptype(predicate).
duptype(clause).
duptype(name).

% Use the same group key to allow filtering of redundant messages.
%
element_group(declaration,    T-M:H, T-M:G) :-
    (T = (meta_predicate) -> functor(H, F, A), G=F/A ; G = H).
element_group(predicate,      _:F/A,   F/A).
element_group(clause,         _:F/A-_, F/A).
element_group(name,           _:F/A,   F/A).

ignore_dupcode(H, _, _) :-
    functor(H, Name, _),
    atom_concat('__aux_wrapper_', _, Name).
ignore_dupcode(_,                             refactor, name).
ignore_dupcode(_,                        i18n_refactor, name).
ignore_dupcode(term_expansion(_, _),            _,      name).
ignore_dupcode(term_expansion(_, _, _, _),      _,      name).
ignore_dupcode(goal_expansion(_, _),            _,      name).
ignore_dupcode(goal_expansion(_, _, _, _),      _,      name).
ignore_dupcode('$exported_op'(_, _, _),         _,      _).
ignore_dupcode('$mode'(_, _),                   _,      _).
ignore_dupcode('$pred_option'(_, _, _, _),      system, _).
ignore_dupcode('$included'(_, _, _, _),         system, _).
ignore_dupcode('$load_context_module'(_, _, _), system, _).
ignore_dupcode(_,                               prolog, declaration(_)).
ignore_dupcode(_,                               user,   declaration(use_module)).
ignore_dupcode(_,                               _,      declaration(dynamic(_,_,_))).

audit:check(dupcode, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_dupcode(OptionL, FileChk, Result).

%% duptype_elem(+DupType, +Head, +Module, :FileChk, -DupId, -Elem) is multi
%
% For a given Element of the language, returns a duplication key and an
% associated value
%
duptype_elem(name,   H, M, FileChk, F/A, M:F/A) :-
    predicate_property(M:H, file(File)),
    call(FileChk, File),
    functor(H, F, A).
duptype_elem(clause, H, M, FileChk, DupId, M:F/A-Idx) :-
    nth_clause(M:H, Idx, Ref),
    clause(M:H, Body, Ref),
    clause_property(Ref, file(File)),
    call(FileChk, File),
    functor(H, F, A),
    variant_sha1((H :- Body), DupId).
duptype_elem(predicate, H, M, FileChk, DupId, M:F/A) :-
    predicate_property(M:H, file(File)),
    call(FileChk, File),
    findall((H :- B), clause(M:H, B), ClauseL),
    variant_sha1(ClauseL, DupId),
    functor(H, F, A).

duptype_elem_declaration(H, M, FileChk, DupId, Elem) :-
    loc_declaration(H, M, T, From),
    \+ ignore_dupcode(H, M, declaration(T)),
    from_chk(FileChk, From),
    \+ memberchk(T, [goal, assertion(_,_)]),
    once(dtype_dupid_elem(T, T, From, H, M, DupId, Elem)).

dtype_dupid_elem(meta_predicate, T, _, H, M, T-M:F/A,    T-M:H) :- functor(H, F, A).
dtype_dupid_elem(use_module,     T, F, H, M, T-File:M:H, T-File:M:H) :-
    from_to_file(F, File). % Ignore duplicated use_module's from different files
dtype_dupid_elem(consult,        T, F, H, M, T-File:M:H, T-File:M:H) :-
    from_to_file(F, File). % Ignore duplicated consult's    from different files
% dtype_dupid_elem(use_module_2,   T, H, M, T-M:H,  T-M:H).
dtype_dupid_elem(T,              T, _, H, M, T-M:PI,     T-M:G) :-
    ( H =.. [_|Vars1],
      term_variables(H, Vars2),
      Vars1==Vars2
    ->functor(H, F, A),
      PI=F/A,
      G =F/A
    ; PI=H,
      G =H
    ).

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

curr_duptype_elem(M, FileChk, DupType, DupId, Elem) :-
    current_predicate(M:F/A),
    functor(H, F, A),
    \+predicate_property(M:H, imported_from(_)),
    duptype(DupType),
    \+ ignore_dupcode(H, M, DupType),
    duptype_elem(DupType, H, M, FileChk, DupId, Elem).
curr_duptype_elem(M, FileChk, declaration, DupId, Elem) :-
    duptype_elem_declaration(_, M, FileChk, DupId, Elem).

check_dupcode(OptionL, FileChk, Result) :-
    select_option(module(M), OptionL, _, M),
    findall((DupType-DupId)-Elem,
	    curr_duptype_elem(M, FileChk, DupType, DupId, Elem), PU),
    keysort(PU, PL),
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

elem_property(name,           PI,        PI,        T, T).
elem_property(clause,         M:F/A-Idx, (M:H)/Idx, T, T) :- functor(H, F, A).
elem_property(predicate,      M:F/A,     M:H,       T, T) :- functor(H, F, A).
elem_property(declaration,    T-M:PI,    (M:H)/0,   T, declaration) :-
    (PI = F/A->functor(H, F, A) ; PI = H).

elem_location(DupType, Elem, D, Loc) :-
    elem_property(DupType, Elem, Prop, T, D),
    property_location(Prop, T, Loc).

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
    [Pre], Loc, ['duplicated '-[D]],
    message_elem(D, Elem),
    [nl].

message_elem(declaration, T-M:PI) --> !, [':- ~w ~w.'-[T, M:PI]].
message_elem(Type, Elem) --> ['~w ~w'-[Type, Elem]].

% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(utils, [
		remove_sentence_nr/2,  % +ListIn, -ListOut
		extract_vars/2,        % +Term, -VarList
		write_terms/2,         % +Stream, +TermList
		print_term/2,          % +Stream, +Term
		write_rules/2,         % +Stream, +RuleList
		write_list_items/2,    % +Stream, +List
		negate/2,              % +Term, -NegTerm
		clean_factset/2,       % +FactsetIn, -FactsetOut
		get_factset/2,         % +FactsetIn, -FactsetOut
		concat_tokens/2,       % +TokenList, -Atom
		get_setof/3            % +Template, +Goal, -Set
	]).

:- use_module(ape('utils/drs_to_ascii')).
:- use_module(op_defs).
:- use_module(list_utils).

/** <module> General utilities

Some utility predicates.

@author Tobias Kuhn
@version 2008-11-24

@see list_utils.pl
*/


%% remove_sentence_nr(+CondsIn, -CondsOut) is det
%
% Removes the sentence numbers from the flat condition list.

remove_sentence_nr([], []).

remove_sentence_nr([Cond-_|RestIn], [Cond|RestOut]) :-
    remove_sentence_nr(RestIn, RestOut).


%% extract_vars(+Term, -VarList) is det
%
% Extracts all variables of Term and stores them in VarList. VarList contains no
% dublicates.

extract_vars(Term, VarList) :-
    extract_vars(Term, [], VarList).


extract_vars(Var, VarList, VarList) :-
    var(Var),
    is_exact_member(Var, VarList),
    !.

extract_vars(Var, VarList, [Var|VarList]) :-
    var(Var),
    !.

extract_vars([], VarList, VarList) :-
	!.

extract_vars([Head|Tail], VarListIn, VarListOut) :-
	!,
	extract_vars(Head, VarListIn, VarListTemp),
	extract_vars(Tail, VarListTemp, VarListOut).

extract_vars(Term, VarList, VarList) :-
	Term =.. [Term],
	!.

extract_vars(Term, VarListIn, VarListOut) :-
	!,
	Term =.. List,
	extract_vars(List, VarListIn, VarListOut).


%% write_terms(+OutStream, +Terms) is det
%
% Writes each of the terms of the list Terms into the stream OutStream.

write_terms(_, []).

write_terms(Out, [Term|Rest]) :-
	print_term(Out, Term),
	write(Out, '.\n'),
	write_terms(Out, Rest).


%% print_term(+OutStream, +Term) is det
%
% Writes the term onto the stream. Variables are pretty-printed.

print_term(Out, Term) :-
	copy_term(Term, Copy),
	numbervars(Copy, 0, _),
	write_term(Out, Copy, [quoted(true), module(op_defs), numbervars(true)]).


%% write_rules(+OutStream, +Rules) is det
%
% Prints the rules on the stream.

write_rules(_, []).

write_rules(Out, [('', Fact, [])|Rest]) :-
    !,
	print_term(Out, Fact),
    write(Out, '.\n'),
	write_rules(Out, Rest).

write_rules(Out, [(Label, Fact, [])|Rest]) :-
    !,
    write(Out, '\''),
    write(Out, Label),
    write(Out, '\':: '),
	print_term(Out, Fact),
    write(Out, '.\n'),
	write_rules(Out, Rest).

write_rules(Out, [('', Head, Body)|Rest]) :-
    !,
    copy_term((Head, Body), (HeadC, BodyC)),
    numbervars((HeadC, BodyC), 0, _),
    print_term(Out, HeadC),
    write(Out, ' <- ['),
    write_body(Out, BodyC),
    write(Out, '].\n'),
	write_rules(Out, Rest).

write_rules(Out, [(Label, Head, Body)|Rest]) :-
    !,
    copy_term((Head, Body), (HeadC, BodyC)),
    numbervars((HeadC, BodyC), 0, _),
    write(Out, '\''),
    write(Out, Label),
    write(Out, '\':: '),
    print_term(Out, HeadC),
    write(Out, ' <- ['),
    write_body(Out, BodyC),
    write(Out, '].\n'),
	write_rules(Out, Rest).

write_rules(Out, [Term|Rest]) :-
    print_term(Out, Term),
    write(Out, '.\n'),
    write_rules(Out, Rest).


%% write_body(+OutStream, +RuleBody) is det
%
% Writes the rule-body onto the stream.

write_body(Out, [Term]) :-
    print_term(Out, Term).

write_body(Out, [Term|Rest]) :-
    print_term(Out, Term),
    write(Out, ', '),
    write_body(Out, Rest).


%% write_list_items(+OutStream, +List) is det
%
% Writes each element of List into the stream OutStream.

write_list_items(_, []).

write_list_items(Out, [Item|Rest]) :-
	print_term(Out, Item),
	write(Out, '.\n'),
	write_list_items(Out, Rest).


%% negate(+X, -NotX) is det
%
% Negates the predicate with classical negation. Positive -> negative and the 
% other way round.

negate(-X, X) :-
    !.

negate(X, -X).


%% clean_factset(+FactsetIn, -FactsetOut) is det
%
% Cleans the factset: auxiliary predicates are removed; label and body of the rule structure are
% removed.

clean_factset(FactsetIn, FactsetOut) :-
    findall(A,
            ( member((_,A,_), FactsetIn), A \= aux_prop(_, _), A \= (- aux_prop(_, _)), A \= aux(_), A \= (- aux(_)) ),
            Factset1),
    sort(Factset1, Factset2),
    copy_term(Factset2, FactsetOut).


%% get_factset(+FactsetIn, -FactsetOut)
%
% Label and body of the rule structure are removed (they do not make much sense after the answer
% set is calculated). The facts are sorted.

get_factset(FactsetIn, FactsetOut) :-
    findall(A,
            member((_,A,_), FactsetIn),
            Factset1),
    sort(Factset1, Factset2),
    copy_term(Factset2, FactsetOut).


%% concat_tokens(+TokenList, -Atom)
%
% A token list representing an ACE text is transformed into a single atom.

concat_tokens([], '').

concat_tokens([a, '-one'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('someone ', RestAtom, Atom).

concat_tokens([a, '-body'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('somebody ', RestAtom, Atom).

concat_tokens([a, '-thing'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('something ', RestAtom, Atom).

concat_tokens([every, '-one'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('everyone ', RestAtom, Atom).

concat_tokens([every, '-body'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('everybody ', RestAtom, Atom).

concat_tokens([every, '-thing'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('everything ', RestAtom, Atom).

concat_tokens([no, '-one'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('no one ', RestAtom, Atom).

concat_tokens([no, '-body'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('nobody ', RestAtom, Atom).

concat_tokens([no, '-thing'|Rest], Atom) :-
	!,
	concat_tokens(Rest, RestAtom),
	atom_concat('nothing ', RestAtom, Atom).

concat_tokens(['"', Token, '"', '.'|Rest], Atom) :-
	!,
	concat_list(['"', Token, '".\n'], AtomC),
	concat_tokens(Rest, RestAtom),
	atom_concat(AtomC, RestAtom, Atom).

concat_tokens(['"', Token, '"'|Rest], Atom) :-
	!,
	concat_list(['"', Token, '" '], AtomC),
	concat_tokens(Rest, RestAtom),
	atom_concat(AtomC, RestAtom, Atom).

concat_tokens([Token, '.'|Rest], Atom) :-
	!,
	atom_concat(Token, '.\n', AtomC),
	concat_tokens(Rest, RestAtom),
	atom_concat(AtomC, RestAtom, Atom).

concat_tokens([Token|Rest], Atom) :-
    atom_concat(Token, ' ', AtomC),
    concat_tokens(Rest, RestAtom),
    atom_concat(AtomC, RestAtom, Atom).


%% get_setof(+Template, +Goal, -Set)
%
% Returns the set of terms that fulfill the goal. In contrast to the built-in predicate
% setof/3, this predicate always succeeds. An empty list is returned if no solution is found.

get_setof(Template, Goal, Set) :-
    setof(Template, Goal, Set),
    !.

get_setof(_Template, _Goal, []).

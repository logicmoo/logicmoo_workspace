/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2019, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(plweb_changes,
          [ load_changes/2,                     % +GitDir, +Revisions
            changes/2                           % +About, -Changes
          ]).
:- use_module(library(git)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

/** <module> Examine changelog entries relevant for a predicate

This is some demo code  to  extract   changelog  entries  that mention a
particular predicate. The aim is to  use   this  in the online manual to
inform people about relevant changes.
*/

:- dynamic changelog/5.   % Commit, Version, About, Level, Subject

:- debug(changelog).

load_changes :-
    load_changes('..', 'V5.6.0..').

load_changes(Dir, Revisions) :-
    retractall(changelog(_,_,_,_,_)),
    git_shortlog(Dir, Logs, [revisions(Revisions)]),
    length(Logs, Len),
    debug(changelog, 'Processing ~D commits', [Len]),
    process_logs(Logs, 'HEAD'),
    predicate_property(changelog(_,_,_,_,_), number_of_clauses(N)),
    debug(changelog, 'Extracted ~D records', [N]).

process_logs([], _).
process_logs([H|T], Version0) :-
    update_version(H, Version0, Version),
    parse_commit(H, Level, Abouts),
    (   Abouts \== []
    ->  git_log_data(commit_hash, H, Commit),
        git_log_data(subject, H, Subject),
        forall(member(About, Abouts),
               assertz(changelog(Commit, Version, About, Level, Subject)))
    ;   true
    ),
    process_logs(T, Version).

update_version(Commit, _Version0, Version) :-
    git_log_data(ref_names, Commit, RefNames),
    member(RefName, RefNames),
    string_concat("tag: V", Nr, RefName),
    split_string(Nr, ".", "", NumS),
    maplist(number_string, [Major, Minor, Patch], NumS),
    !,
    Version is 10000*Major+100*Minor+Patch.
update_version(_, Version, Version).

parse_commit(Commit, Level, Abouts) :-
    git_log_data(subject, Commit, Subject),
    string_codes(Subject, Codes),
    phrase(subject(Level, Abouts), Codes).

subject(Level, Abouts) -->
    level(Level),
    abouts(Abouts0),
    { sort(Abouts0, Abouts)
    }.

level(Level) -->
    capitals(List), ":",
    { List \== [] },
    !,
    { string_codes(S, List),
      downcase_atom(S, Level)
    }.
level(unclassified) -->
    [].

abouts([H|T]) --> about(H), !, abouts(T).
abouts([]) --> remainder(_).

about(Name/Arity) -->
    string(_),
    blank,
    prolog_identifier(Name),
    arity(Arity),
    !.

prolog_identifier(Name) -->
	[C0], { code_type(C0, prolog_atom_start) }, !,
	prolog_id_cont(CL),
	{ atom_codes(Name, [C0|CL]) }.

prolog_id_cont([H|T]) -->
	[H], { code_type(H, prolog_identifier_continue) }, !,
	prolog_id_cont(T).
prolog_id_cont([]) --> "".

arity(Arity) --> "//", nonneg(A0), !, { Arity is A0+2 }.
arity(Arity) --> "/", nonneg(Arity).


capitals([H|T]) --> capital(H), !, capitals(T).
capitals([]) --> [].

capital(H) --> [H], { between(0'A, 0'Z, H) }.

nonneg(I) --> int_codes(Codes), { number_codes(I, Codes) }.

int_codes([D0|D]) --> digit(D0), digits(D).


		 /*******************************
		 *             QUERY		*
		 *******************************/

changes(About, Commits) :-
    findall(commit(Commit, Version, Level, Subject),
            changelog(Commit, Version, About, Level, Subject),
            Commits).


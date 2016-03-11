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

:- module(implemented_in, [implemented_in/1,
			   implemented_in/3]).

:- use_module(library(lists)).
:- use_module(library(extra_location)).
:- use_module(library(prolog_codewalk), []). % for message_location//1
:- use_module(library(normalize_head)).
:- use_module(library(extra_codewalk)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(implemented_in(From, Args))) -->
    prolog:message_location(From),
    ['Implements ~w'-Args].

implemented_in(MGoal0, From, Args) :-
    normalize_head(MGoal0, MGoal),
    M:Goal = MGoal,
    functor(Goal, F, A),
    findall(MI, ( current_module(M),
		  \+ predicate_property(MGoal, imported_from(_)),
		  MI = M
		; predicate_property(MGoal, imported_from(MI))
		), UML),
    sort(UML, ML),
    member(M, ML),
    ( ( loc_declaration(Goal, M, Declaration, From),
	Declaration \= goal
      ; loc_dynamic(Goal, M, Declaration, From),
	Declaration \= dynamic(query, _, _)
      ),
      Args = [M:F/A-Declaration]
    ; From = clause(ClauseRef),
      catch(( clause(M:Goal, _, ClauseRef),
	      nth_clause(M:Goal, N, ClauseRef)
	    ), _, fail),
      Args = [M:F/A-N]
    ).

:- dynamic prepared/0.

:- public prepare/0.

prepare :-
    extra_walk_code([source(false),
		     infer_meta_predicates(false),
		     autoload(false),
		     evaluate(false),
		     trace_reference(_),
		     module_class([user, system, library]),
		     on_etrace(collect_dynamic_locations(M))], M, _),
    retractall(prepared),
    assertz(prepared).

implemented_in(MGoal) :-
    ( prepared -> true ; prepare ),
    forall(implemented_in(MGoal, From, Args),
	   print_message(information, acheck(implemented_in(From, Args)))).

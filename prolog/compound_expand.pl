/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

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

:- module(compound_expand, []).

/* This module allows to define compositional term and goal expansions,
   using this module in a module that already defines the predicates
   term_expansion/2/4 and goal_expansion/2/4 but don't export them.
   
   The composition of expansions is instrumental to grammar and syntax
   extensions, which is the key point of Ciao Prolog, but not supported in SWI
   Prolog. We do not need to deal with all the complexity that the Ciao package
   system have, so with this helper the port of Ciao Packages to SWI Prolog can
   be achieved smoothly and such modules can be used in SWI Programs that do not
   requires the Ciao dialect.
*/

:- use_module(xlibrary(expansion_module)).
:- use_module(xlibrary(remove_dups)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4.

:- public implemented_pi/1.
:- meta_predicate implemented_pi(:).
implemented_pi(M:F/A) :-
    functor(H, F, A),
    % Can not use current_module/1 at this stage: --EMM
    once(predicate_property(M:H, visible)),
    \+ predicate_property(M:H, imported_from(_)).

system:goal_expansion(Goal0, Pos0, Goal, Pos) :-
    '$set_source_module'(M, M),
    expansion_module(M, EM),
    ( implemented_pi(EM:goal_expansion/4) ->
      EM:goal_expansion(Goal0, Pos0, Goal, Pos)
    ; EM:goal_expansion(Goal0, Goal),
      Pos = Pos0
    ),
    Goal0 \== Goal,
    !.

:- dynamic
    lock_expansion/0.

call_lock(Goal) :-
    setup_call_cleanup(( \+ lock_expansion,
			 assertz(lock_expansion)),
		       Goal,
		       retract(lock_expansion)).

system:term_expansion(Term0, Pos0, Term, Pos) :-
    '$set_source_module'(M, M),
    M \= user, % Compound expansions not supported in user module
    findall(EM-PI, ( expansion_module(M, EM),
		     ( implemented_pi(EM:term_expansion/4)
		     ->PI=[term_expansion/4]
		     ; PI=[term_expansion/2]
		     )), MD),
    MD \= [],
    remove_dups(MD, ML),
    call_lock('$expand':call_term_expansion(ML, Term0, Pos0, Term, Pos)),
    Term0 \== Term,
    [Term0] \== Term.		% Fail to try other expansions

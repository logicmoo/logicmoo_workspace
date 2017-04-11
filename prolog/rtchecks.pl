/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.

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

:- module(rtchecks,
          [(rtchecked)/1,
           
           op(1150, fx, rtchecked)
          ]).

:- use_module(system:library(rtchecks_rt)).

rtchecked(PlList) :-
    throw(error(context_error(nodirective, rtcheck(PlList)), _)).

:- multifile
    system:term_expansion/2,
    prolog:rename_predicate/2.

:- dynamic
    system:term_expansion/2.

wrappers(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
wrappers((A,B)) -->
    !,
    wrappers(A),
    wrappers(B).
wrappers(Name//Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1).
wrappers(Name/Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      functor(Head, Name, Arity),
      atom_concat(Name, ' rtchecked', WrapName),
      Head =.. [Name|Args],
      WrappedHead =.. [WrapName|Args],
      prolog_load_context(module, Module)
    },
    [ :- public '$rtchecked'/1,
      '$rtchecked'(Head),
      (:- module_transparent Name/Arity),
      (   Head :-
             start_rtcheck(Module:Head, WrappedHead)
      )
    ].

%!  prolog:rename_predicate(:Head0, :Head) is semidet.
%
%   Hook into term_expansion for  post   processing  renaming of the
%   generated predicate.

prolog:rename_predicate(M:Head0, M:Head) :-
    '$flushed_predicate'(M:'$rtchecked'(_)),
    call(M:'$rtchecked'(Head0)),
    !,
    rename_term(Head0, Head).

rename_term(Compound0, Compound) :-
    compound(Compound0),
    !,
    compound_name_arguments(Compound0, Name, Args),
    atom_concat(Name, ' rtchecked', WrapName),
    compound_name_arguments(Compound, WrapName, Args).
rename_term(Name, WrapName) :-
    atom_concat(Name, ' rtchecked', WrapName).


system:term_expansion((:- rtchecked(Preds)),
                      [ (:- discontiguous('$rtchecked'/1))
                      | Clauses
                      ]) :-
    phrase(wrappers(Preds), Clauses).

:- multifile
    sandbox:safe_directive/1.

%!  sandbox:safe_directive(+Directive) is semidet.
%
%   Allow rtchecks directives that affect locally defined predicates.

sandbox:safe_directive(Dir) :-
    ground(Dir),
    local_rtchecks_dir(Dir).

local_rtchecks_dir(rtchecked(Preds)) :-
    local_preds(Preds).

local_preds((A,B)) :-
    !,
    local_preds(A),
    local_preds(B).

local_preds(Name/Arity) :-
    atom(Name), integer(Arity).
local_preds(Name//Arity) :-
    atom(Name), integer(Arity).

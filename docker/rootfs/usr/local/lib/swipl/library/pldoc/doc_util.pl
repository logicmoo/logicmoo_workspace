/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2018, University of Amsterdam
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(doc_util,
          [ insert_alias/2,             % +Path, -Aliased
            expand_alias/2,             % +Aliased, -Path
            ensure_slash_end/2,         % +Dir, -DirSlash
            atom_pi/2,                  % +Atom, -PI
            atom_to_object/2,           % +Atom, -Object
            normalise_white_space//1    % -Text
          ]).
:- use_module(library(dcg/basics)).

/** <module> PlDoc utilities

@author Jan Wielemaker
*/

                 /*******************************
                 *     PATH ALIAS PROCESSING    *
                 *******************************/

%!  insert_alias(+Path0, -Path) is det.
%
%   Translate a native path to  an   aliased  path. Path aliases are
%   defined by path_alias/2. Aliased paths   are  re-translated into
%   native form using expand_alias/2.

insert_alias(Path0, Path) :-
    path_alias(Alias, Prefix),
    atom_concat(Prefix, PostFix, Path0),
    !,
    atom_concat(Alias, PostFix, Path).
insert_alias(Path, Path).


%!  expand_alias(+Path0, -Path) is det.
%
%   Translate an aliased path to a native path.

expand_alias(Path0, Path) :-
    path_alias(Alias, Prefix),
    atom_concat(Alias, Postfix, Path0),
    !,
    atom_concat(Prefix, Postfix, Path).
expand_alias(Path, Path).


%!  path_alias(?Alias, ?Path) is nondet.
%
%   True if Alias: is an alias  for   Path.  This is used to rewrite
%   paths below the SWI-Prolog home to   give them shorter and fixed
%   names.

path_alias('/_SWI_/', Dir) :-
    current_prolog_flag(home, Dir0),
    ensure_slash_end(Dir0, Dir).
path_alias('/_CWD_/', Dir) :-
    working_directory(Dir0, Dir0),
    ensure_slash_end(Dir0, Dir).


                 /*******************************
                 *      MISC PATH OPERATIONS    *
                 *******************************/

%!  ensure_slash_end(+Dir, -DirSlash) is det.
%
%   Ensure Dir ends with a /.

ensure_slash_end(Dir, Dir) :-
    sub_atom(Dir, _, _, 0, /),
    !.
ensure_slash_end(Dir0, Dir) :-
    atom_concat(Dir0, /, Dir).

                 /*******************************
                 *          PREDICATES          *
                 *******************************/

%!  atom_pi(+Atom, -PI) is det.
%
%   Translate an external predicate indicator   representated  as an
%   atom  into  a  predicate  indicator    term.  If  Atom  contains
%   <module>:, PI is qialified. If no arity is provided it is a term
%   Name/_, i.e., with unbound arity.


atom_pi(Atom, Module:PI) :-
    atomic_list_concat([Module, PIAtom], :, Atom),
    Module \== '',
    forall(sub_atom(Module, _,1,_,C), char_type(C,alnum)),
    !,
    atom_pi2(PIAtom, PI).
atom_pi(Atom, PI) :-
    atom_pi2(Atom, PI).

atom_pi2(Atom, Name//Arity) :-
    sub_atom(Atom, B, _, A, //),
    sub_atom(Atom, _, A, 0, ArityA),
    atom_number(ArityA, Arity),
    sub_atom(Atom, 0, B, _, Name),
    Name \== '',
    !.
atom_pi2(Atom, Name/Arity) :-
    sub_atom(Atom, B, _, A, /),
    sub_atom(Atom, _, A, 0, ArityA),
    atom_number(ArityA, Arity),
    !,
    sub_atom(Atom, 0, B, _, Name).
atom_pi2(Name, Name/_).

%!  atom_to_object(+Atom, -PredicateIndicator) is semidet.
%
%   If Atom is `Name/Arity', decompose to Name and Arity. No errors.

atom_to_object(Atom, Object) :-
    atom(Atom),
    atom_concat('f-', Atom0, Atom),
    !,
    PI = _/_,                           % avoid qualified and DCG
    atom_pi(Atom0, PI),
    Object = f(PI).
atom_to_object(Atom, Object) :-
    atom(Atom),
    atom_pi(Atom, PI),
    ground(PI),
    !,
    (   PI = Name/Arity,
        integer(Arity),
        atom_concat('f-', FuncName, Name)
    ->  Object = f(FuncName/Arity)
    ;   Object = PI
    ).
atom_to_object(Atom, c(Function)) :-
    atom(Atom),
    (   sub_atom(Atom, 0, _, _, 'PL_')
    ->  true
    ;   sub_atom(Atom, 0, _, _, 'S')
    ),
    sub_atom(Atom, B, _, _, '('),
    !,
    sub_atom(Atom, 0, B, _, Function).

%!  normalise_white_space(-Text)// is det.
%
%   Text is input after deleting leading   and  trailing white space
%   and mapping all internal white space to a single space.

normalise_white_space(Text) -->
    blanks,
    normalise_white_space2(Text).

normalise_white_space2(Text) -->
    non_ws(Text, Tail),
    blanks,
    (   eos
    ->  { Tail = [] }
    ;   { Tail = [0'\s|T2] },
        normalise_white_space2(T2)
    ).

%!  non_ws(-Text, ?Tail) is det.
%
%   True if the  difference  list  Text-Tail   is  the  sequence  of
%   non-white-space characters.

non_ws([H|T0], T) -->
    [H],
    { \+ code_type(H, space) },
    !,
    non_ws(T0, T).
non_ws(T, T) -->
    [].

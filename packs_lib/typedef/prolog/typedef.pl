/* Part of typedef
	Copyright 2014-2015 Samer Abdallah (UCL)

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(typedef,
    [ op(1150,fx,type)
    , op(1130,xfx,--->)
    , (type)/1
    , current_type/1
    , current_type_constructor/2
    ]).

/** <module> Type definition framework

   This module provides a way to declare new types that hook into the
   must_be/2 framework of library(error).

   The type definition language is designed to be close to the one in
   Tom Schrijver's type_check package, supporting type synonyms, such as
   ==
   :- type natnum == nonneg.
   ==
   and algebraic type definitions like
   ==
   :- type nat ---> zero ; succ(nat).
   ==

   A built in type =|partial(T)|= is defined, which is satisfied by variables
   as well as terms that satisfy type T. This should be probably be extended to
   include all partial instantiations of type T, eg =|s(s(_))|= should satisfy
   =|partial(nat)|=, which it _does not_ at the moment.

   The result types can be used with must_be/2 and therefore in the record
   declarations provided by library(record) and the peristency declarations
   provided by library(persistency).

   TODO
   - Consider allowing duplicate type definitions if the definitions are the same.
   - Consider extending partial(T) type.
*/

:- multifile user_type_syn/2, user_type_def/1, user_type_constructor/2.
:- multifile error:has_type/2.
:- op(1150,fx,type).
:- op(1130,xfx, --->).

% true if the module whose terms are being read has specifically
% imported library(typedef).
wants_typedef :-
   prolog_load_context(module, Module),
   Module \== typedef,  % we don't want typedef sugar ourselves
   predicate_property(Module:type(_),imported_from(typedef)).

check_not_defined(Type) :-
   (  (user_type_syn(Type,_); user_type_def(Type))
   -> throw(error(duplicate_type(Type),(type)/1))
   ;  true
   ).

%% type(Spec).
%  Declares a new type. Spec can be one of two forms:
%  ==
%  NewType ---> Constructor1 ; Constructor2 ; ... .
%  NewType == OldType
%  ==
%  NewType can included type variables, which can be used in the constructor
%  terms. The arguments of constructor terms are the types of the required
%  arguments for that constructor. The second form declares a type synonym,
%  so NewType is equivalent to OldType.
%
%  It is an error to declare the same type more than once, even if the definition
%  is the same. Type name space is flat, not module scoped.
%  This is directive. It cannot be called.
type(Spec) :- throw(error(context_error(nodirective, type(Spec)), _)).

%% current_type(+Type) is semidet.
%% current_type(-Type) is multi.
%
%  True if Type is a currently defined type.  For example,
%
%      :- type foo ---> one ; two ; three.
%      :- type bar ---> hello ; goodbye.
%      ?- current_type(Type).
%      Type = foo ;
%      Type = bar .
current_type(Type) :-
    user_type_def(Type).

%% current_type_constructor(?T, ?Constructor)
%
%  True if a type T allows the Constructor.  For example,
%
%      :- type foo ---> one ; two ; three.
%      :- type bar ---> hello ; goodbye.
%      ?- current_type_constructor(bar, C).
%      C = hello ;
%      C = goodbye .
current_type_constructor(Type, Constructor) :-
    user_type_constructor(Type, Constructor).

system:term_expansion(:- type(Decl), Clauses) :-
   wants_typedef,
   (  expand_type_declaration(Decl, Clauses) -> true
   ;  throw(error(bad_type_declaration(Decl), (type)/1))
   ).

expand_type_declaration(Type == Syn, [C1,C2]) :-
   % check_not_defined(Type),
   C1 = typedef:user_type_syn(Type,Syn),
   C2 = (error:has_type(Type, Value) :- error:has_type(Syn, Value)).
expand_type_declaration((Type ---> Defs), Clauses) :-
   % check_not_defined(Type),
   type_def(Type,Defs,Clauses,[]).

type_def(Type,Defs) -->
   [ typedef:user_type_def(Type) ],
   [ error:has_type(Type, Value) :- typedef:has_type(Type,Value) ],
   constructors(Type,Defs).

constructors(Type,C1;CX) --> !, constructor(Type,C1), constructors(Type,CX).
constructors(Type,CZ) --> constructor(Type,CZ).
constructor(Type,C) --> [ typedef:user_type_constructor(Type,C) ].

error:has_type(partial(Type),Term) :- !,
   var(Term) -> true; error:has_type(Type,Term).

has_type(Type,Term) :-
   user_type_def(Type), !, nonvar(Term),
   user_type_constructor(Type,Cons),
   (  atomic(Cons) -> Cons=Term
   ;  functor(Cons,F,A),
      functor(Term,F,A),
      forall( arg(N,Cons,ArgType), (arg(N,Term,ArgVal), error:has_type(ArgType,ArgVal)))
   ).

prolog:message(error(duplicate_type(Type),_)) -->
   {numbervars(Type,0,_)},
   [ 'Redefinition of type ~w.'-[Type], nl].

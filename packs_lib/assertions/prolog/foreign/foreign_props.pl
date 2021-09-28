/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(foreign_props,
          [foreign/1,
           foreign/2,
           foreign_spec/1,
           (native)/1,
           (native)/2,
           normalize_ftype/2,
           normalize_ftgen/2,
           fimport/1,
           fimport/2,
           nimport/1,
           nimport/2,
           int64/1,
           lang/1,
           long/1,
           returns/2,
           parent/2,
           returns_state/1,
           memory_root/1,
           ptr/1,
           ptr/2,
           array/3,
           setof/2,
           float_t/1,
           size_t/1,
           sgen/1,
           tgen/1,
           tgen/2,
           dict_t/2,
           dict_t/3,
           dict_join_t/4,
           dict_extend_t/4,
           join_dict_types/6,
           join_type_desc/4]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(plprops)).
:- use_module(library(extend_args)).
:- use_module(library(mapargs)).
:- use_module(library(neck)).

:- type foreign_spec/1.

foreign_spec(name(  Name  )) :- atm(Name).
foreign_spec(prefix(Prefix)) :- atm(Prefix).
foreign_spec(suffix(Suffix)) :- atm(Suffix).
foreign_spec(lang(Lang)) :- lang(Lang).

:- type lang/1.
lang(prolog).
lang(native).

normalize_ftype(native( O, G), native( O, G)).
normalize_ftype(foreign(O, G), foreign(O, G)).
normalize_ftype(fimport(O, G), foreign([lang(prolog), O], G)).
normalize_ftype(native(    G), native( [prefix(pl_)], G)).
normalize_ftype(foreign(   G), foreign([prefix('')], G)).
normalize_ftype(fimport(   G), foreign([lang(prolog), prefix('')], G)).
normalize_ftype(nimport(O, G), foreign([lang(native), O], G)).
normalize_ftype(nimport(   G), foreign([lang(native), prefix('')], G)).

:- type ftype_spec/1.

ftype_spec(tdef). % Use typedef to implement the type
ftype_spec(decl). % Generate the equivalent struct/enum declaration for the given type
ftype_spec(gett). % Generate the getter of the given type
ftype_spec(unif). % Generate the unifier of the given type

normalize_ftgen(tgen(   G), tgen([tdef, decl, gett, unif], G)).
normalize_ftgen(sgen(   G), tgen([decl, gett, unif], G)).
normalize_ftgen(tgen(O, G), tgen(O, G)).

%!  native(+ForeignSpec, :Predicate)
%
%   Predicate is implemented in C as specified by ForeignSpec.

%!  native(:Predicate)
%
%   Predicate is implemented in C with a pl_ prefix.

%!  tgen(:FTypeSpec, :Predicate)
%
%   Type is implemented in C as specified by FTypeSpec.

:- global native( nlist(foreign_spec), callable).
:- global foreign(nlist(foreign_spec), callable).
:- global fimport(nlist(foreign_spec), callable).
:- global nimport(nlist(foreign_spec), callable).
:- global native( callable).
:- global foreign(callable).
:- global fimport(callable).
:- global nimport(callable).
:- global sgen(callable).
:- global tgen(callable).
:- global tgen(nlist(ftype_spec), callable).

H :-
    ( normalize_ftype(H, N)
    ; normalize_ftgen(H, N)
    ),
    ( H == N
    ->functor(H, _, A),
      arg(A, H, G),
      B = call(G)
    ; B = N
    ),
    necki,
    B.

:- global returns/2.
returns(_, G) :- call(G).

:- global parent/2.
parent(_, G) :- call(G).

:- global returns_state/1.
returns_state(G) :- call(G).

:- global memory_root/1.
memory_root(G) :- call(G).

:- type float_t/1 # "Defines a float".
float_t(Num) :- num(Num).

:- type ptr/1 # "Defines a void pointer".
ptr(Ptr) :- int(Ptr).

:- type long/1 # "Defines a long integer".
long(Long) :- int(Long).

:- type size_t/1 # "Defines a size".
size_t(Size) :- nnegint(Size).

:- type int64/1 # "Defines a 64 bits integer".
int64(I) :- int(I).

%!  array(:Type, Dimensions:list(nnegint), Array)
%
%   Defines an array of dimensions Dimentions. In Prolog an array is implemented
%   as nested terms, with a functor arity equal to the dimension at each
%   level. In the foreign language is the typical array structure.  Note that we
%   use functor since they are equivalent to arrays in Prolog.

:- type array(1, list(size_t), term).
:- meta_predicate array(1, +, ?).

array(Type, DimL, Array) :-
    array_(DimL, Type, Array).

array_([], T, V) :- type(T, V).
array_([Dim|DimL], T, V) :-
    size_t(Dim),
    functor(V, v, Dim),
    mapargs(array_(DimL, T), V).

%!  setof(:Type, ?Set)
%
%   Set is a set of Type.  The actual implementation would be a bit tricky,
%   but for now we simple use list/2.

:- type setof/2 # "Defines a set of elements".

:- meta_predicate setof(1, ?).

setof(Type, List) :-
    list(Type, List).

%!  ptr(:Type, ?Ptr)
%
%   Defines a typed pointer. Note that if the value was allocated dynamically by
%   foreign_interface, it allows its usage as parent in FI_new_child_value/array
%   in the C side to perform semi-automatic memory management

:- type ptr/2.

:- meta_predicate ptr(1, ?).

ptr(Type, Ptr) :-
    call(Type, Ptr).

prolog:called_by(dict_t(Desc, _), foreign_props, M, L) :-
    called_by_dict_t(Desc, M, L).
prolog:called_by(dict_t(_, Desc, _), foreign_props, M, L) :-
    called_by_dict_t(Desc, M, L).

called_by_dict_t(Desc, CM, L) :-
    nonvar(Desc),
    dict_create(Dict, _Tag, Desc),
    findall(M:P,
            ( MType=Dict._Key,
              strip_module(CM:MType, M, T),
              nonvar(T),
              extend_args(T, [_], P)
            ), L).

:- type dict_t/2.
:- meta_predicate dict_t(:, ?).
dict_t(Desc, Term) :-
    dict_t(_, Desc, Term).

:- type dict_t/3.
:- meta_predicate dict_t(?, :, ?).
dict_t(Tag, M:Desc, Term) :-
    dict_mq(Desc, M, Tag, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

:- type dict_join_t/4.
:- meta_predicate dict_join_t(?, ?, 1, 1).
dict_join_t(Term, Tag, M1:Type1, M2:Type2) :-
    join_dict_types(Type1, M1, Type2, M2, Tag, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

:- type dict_extend_t/4.
:- meta_predicate dict_extend_t(1, ?, +, ?).
dict_extend_t(Type, Tag, Desc, Term) :-
    join_type_desc(Type, Tag, Desc, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

:- meta_predicate join_type_desc(1, ?, +, -).
join_type_desc(M:Type, Tag, Desc2, Dict) :-
    type_desc(M:Type, Desc1),
    join_dict_descs(M:Desc1, M:Desc2, Tag, Dict).

dict_mq(M:Desc, _, Tag, Dict) :- !,
    dict_mq(Desc, M, Tag, Dict).
dict_mq(Desc, M, Tag, Dict) :-
    dict_create(Dict, Tag, Desc),
    forall(Value=Dict.Key, nb_set_dict(Key, Dict, M:Value)).

dict_kv(Dict, Key-Value) :-
    Type=Dict.Key,
    call(Type, Value).

:- pred extend_one_arg(1, -goal) is det.

extend_one_arg(Call1, Call) :- extend_args(Call1, [_], Call).

type_desc(MType, Desc) :-
    extend_one_arg(MType, MCall),
    clause(MCall, dict_t(_, Desc, _)).

join_dict_types(Type1, M1, Type2, M2, Tag, Dict) :-
    type_desc(M1:Type1, Desc1),
    type_desc(M2:Type2, Desc2),
    join_dict_descs(M1:Desc1, M2:Desc2, Tag, Dict).

join_dict_descs(M1:Desc1, M2:Desc2, Tag, Dict) :-
    dict_mq(Desc1, M1, Tag, Dict1),
    dict_mq(Desc2, M2, Tag, Dict2),
    Dict=Dict1.put(Dict2),
    assertion(Dict=Dict2.put(Dict1)).

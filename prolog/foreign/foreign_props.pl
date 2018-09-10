/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
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
           namespec/1,
           (native)/1,
           (native)/2,
           fimport/1,
           fimport/2,
           long/1,
           returns/2,
           parent/2,
           returns_state/1,
           memory_root/1,
           ptr/1,
           ptr/2,
           float_t/1,
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

:- global foreign/1.
foreign(G) :- call(G).

:- global foreign/2.
foreign(_, G) :- call(G).

:- type namespec/1.

namespec(name(  Name  )) :- atm(Name).
namespec(prefix(Prefix)) :- atm(Prefix).
namespec(suffix(Suffix)) :- atm(Suffix).

%!  native(+NameSpec, :Predicate)
%
%   Predicate is implemented in C as specified by NameSpec.

:- global native/2 :: namespec * callable.
native(_, G) :- call(G).

%!  native(:Predicate)
%
%   Predicate is implemented in C with a pl_ prefix.

:- global declaration (native)/1.

native(X) :- native(prefix(pl_), X).

:- global fimport/1.
fimport(G) :- call(G).

:- global fimport/2.
fimport(_, G) :- call(G).

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

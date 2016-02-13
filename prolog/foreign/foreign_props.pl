:- module(foreign_props,
	  [foreign/1,
	   foreign/2,
	   fimport/1,
	   fimport/2,
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
	   join_type_desc/5]).

:- use_module(assertions(assertions)).
:- use_module(assertions(basicprops)).
:- use_module(assertions(plprops)).

:- prop foreign/1 + no_rtcheck.
:- meta_predicate foreign(0).
foreign(G) :- call(G).

:- prop foreign/2 + no_rtcheck.
:- meta_predicate foreign(0,?).
foreign(G, _) :- call(G).

:- prop fimport/1 + no_rtcheck.
:- meta_predicate fimport(0).
fimport(G) :- call(G).

:- prop fimport/2 + no_rtcheck.
:- meta_predicate fimport(0,?).
fimport(G, _) :- call(G).

:- prop returns/2 + no_rtcheck.
:- meta_predicate returns(0,?).
returns(G,_) :- call(G).

:- prop parent/2 + no_rtcheck.
:- meta_predicate parent(0,?).
parent(G,_) :- call(G).

:- prop returns_state/1 + no_rtcheck.
:- meta_predicate returns_state(0).
returns_state(G) :- call(G).

:- prop memory_root/1 + no_rtcheck.
:- meta_predicate memory_root(0).
memory_root(G) :- call(G).

:- prop float_t/1 + type # "Defines a float".
float_t(Num) :- num(Num).

:- prop ptr/1 + type # "Defines a void pointer".
ptr(Ptr) :- int(Ptr).

:- prop ptr/2 + type # "Defines a typed pointer. Note that if the value was
    allocated dinamically by foreign_interface, it allows its usage as parent in
    FI_new_child_value/array in the C side to perform semi-automatic memory
    management".

:- meta_predicate ptr(?,1).
ptr(Ptr, Type) :-
    call(Type, Ptr).

:- prop dict_t/2 + type.
:- meta_predicate dict_t(?, :).
dict_t(Term, Desc) :-
    dict_t(Term, _, Desc).
    
:- prop dict_t/3 + type.
:- meta_predicate dict_t(?, ?, :).
dict_t(Term, Tag, M:Desc) :-
    dict_mq(Desc, M, Tag, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

:- prop dict_join_t/4 + type.
:- meta_predicate dict_join_t(?, ?, 1, 1).
dict_join_t(Term, Tag, M1:Type1, M2:Type2) :-
    join_dict_types(Type1, M1, Type2, M2, Tag, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

:- prop dict_extend_t/4 + type.
:- meta_predicate dict_extend_t(?, 1, ?, +).
dict_extend_t(Term, M:Type, Tag, Desc) :-
    join_type_desc(Type, M, Tag, Desc, Dict),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict), Pairs).

join_type_desc(Type, M, Tag, Desc2, Dict) :-
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

type_desc(M:Type, Desc) :-
    extend_term(Type, [_], Call),
    clause(M:Call, dict_t(_, _, Desc)).

join_dict_types(Type1, M1, Type2, M2, Tag, Dict) :-
    type_desc(M1:Type1, Desc1),
    type_desc(M2:Type2, Desc2),
    join_dict_descs(M1:Desc1, M2:Desc2, Tag, Dict).

join_dict_descs(M1:Desc1, M2:Desc2, Tag, Dict) :-
    dict_mq(Desc1, M1, Tag, Dict1),
    dict_mq(Desc2, M2, Tag, Dict2),
    Dict=Dict1.put(Dict2),
    assertion(Dict=Dict2.put(Dict1)).

extend_term(Term, _, Term) :-
    var(Term), !.
extend_term(M:Term0, Extra, M:Term) :- !,
    extend_term(Term0, Extra, Term).
extend_term(Atom, Extra, Term) :-
    atom(Atom), !,
    Term =.. [Atom|Extra].
extend_term(Term0, Extra, Term) :-
    compound_name_arguments(Term0, Name, Args0),
    '$append'(Args0, Extra, Args),
    compound_name_arguments(Term, Name, Args).

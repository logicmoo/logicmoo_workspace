:- module(rbutils, [rb_fold/4, rb_get/4, rb_upd/5, rb_app/4, rb_add/4, rb_upd_or_ins/4, rb_app_or_new/5]).
:- use_module(library(rbtrees)).
:- reexport(library(rbtrees)).

:- meta_predicate rb_app(+,2,+,-), rb_app_or_new(+,2,1,+,-).

rb_upd(K,V1,V2,T1,T2) :- rb_update(T1,K,V1,V2,T2).
rb_app(K,P,T1,T2) :- rb_apply(T1,K,P,T2).
rb_add(K,V,T1,T2) :- rb_insert_new(T1,K,V,T2).
rb_get(K,V,T,T) :- rb_lookup(K,V,T).
% rb_app_or_new(K,P,Q,T1,T2) :- rb_apply_or_create(T1,K,P,Q,T2).
rb_app_or_new(K,P,Q) --> (rb_add(K,V) -> {call(Q,V)}; rb_app(K,P)).
rb_upd_or_ins(K,update(V1,V2)) --> rb_upd(K,V1,V2), !.
rb_upd_or_ins(K,insert(V)) --> rb_add(K,V).

user:goal_expansion(rb_add(K,V,T1,T2),rb_insert_new(T1,K,V,T2)).
user:goal_expansion(rb_upd(K,V1,V2,T1,T2),rb_update(T1,K,V1,V2,T2)).
user:goal_expansion(rb_app(K,P,T1,T2),rb_apply(T1,K,P,T2)).
user:goal_expansion(rb_get(K,V,T1,T2),(T2=T1, rb_lookup(K,V,T1))).
% user:goal_expansion(rb_app_or_new(K,P,Q,T1,T2),rb_apply_or_create(T1,K,P,Q,T2)).

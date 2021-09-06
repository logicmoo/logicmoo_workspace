

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_boxlog.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(common_logic_utils,
          [ ]).

:- use_module(library(logicmoo_utils)).

:- include(library('logicmoo/common_logic/common_header.pi')).



:- thread_local(t_l:kif_option/2).
:- thread_local(t_l:kif_option_list/1).

means_false_kif(Value):- Value == none ; Value == fail; Value == []; Value == false; Value == no ; Value=todo.


use_kif_option(_Jiggler,never):-!,fail.
use_kif_option(_Jiggler,always):-!.
use_kif_option(Jiggler,_Default):- foption_to_name(Jiggler,Name), kif_option_value(Name,Value),!, \+ means_false_kif(Value),
     (compound(Jiggler) -> arg(1,Jiggler,Value) ; true).
use_kif_option(_Jiggler,Default):- \+ means_false_kif(Default).

as_local_kv(-Key,Key,false):-!.
as_local_kv(+Key,Key,true):-!.
as_local_kv(Key,Key,true):- atom(Key),!.
as_local_kv(KeyValue,_Key,_):- \+ compound(KeyValue),!,fail.
as_local_kv(KeyValue,Key,Value):- KeyValue=..[Key,Value].
as_local_kv(KeyValue,Key,Value):- KeyValue=..[_,Key,Value].

set_kif_option(Name+Name2):- !,set_kif_option(Name),set_kif_option(+Name2).
set_kif_option(Name-Name2):- !,set_kif_option(Name),set_kif_option(-Name2).
set_kif_option(List):- is_list(List),!,maplist(set_kif_option,List).
set_kif_option(+Name):- !, set_kif_option(Name,true).
set_kif_option(-Name):- !, set_kif_option(Name,false).
set_kif_option(N=V):- !, set_kif_option(N,V).
set_kif_option(N:V):- !, set_kif_option(N,V).
set_kif_option(Name):- !, set_kif_option(Name,true).

:- dynamic(kif_option_default/3).

set_kif_option(Name,Value):- assert_setting01(t_l:kif_option(Name,Value)),!.
set_kif_option_default(Name,Value):- ignore(source_location(S,_)),assert_if_new(kif_option_default(Name,Value,S)),!.

kif_option_value(Name,Value):- zotrace(kif_option_value0(Name,Value)).

kif_option_value0(Name,Value):- Value==none,!,(kif_option_value(Name,ValueReally)->ValueReally==Value;true).
kif_option_value0(Name,Value):- t_l:kif_option_list(Dict),atom(Name),
   (is_dict(Dict) -> (get_dict(Key,Dict,Value),atom(Name),atom(Key),atom_concat(Key,_,Name));
    true -> ((member(KeyValue,Dict),as_local_kv(KeyValue,Key,Value),(atom_concat(Key,_,Name);atom_concat(_,Key,Name))))),!.
kif_option_value0(Name,Value):- t_l:kif_option(Name,Value),!.
kif_option_value0(Name,Value):- atom(Name),current_prolog_flag(Name,Value),!.
kif_option_value0(Name,Value):- clause_b(feature_setting(Name,Value)),!.

foption_to_name(Name,Name):- \+ compound(Name),!.
foption_to_name(_:Jiggler,Name):- !,foption_to_name(Jiggler,Name).
foption_to_name(adapt_to_arity_2(Jiggler),Name):-!,foption_to_name(Jiggler,Name).
foption_to_name(Jiggler,Name):-functor(Jiggler,Name,_).

:- meta_predicate adapt_to_arity_2(1,?,?).
adapt_to_arity_2(Pred1,A,O):- call(Pred1,A),A=O.

:- meta_predicate kif_optionally(+,1,?).
kif_optionally(YN,Pred1,Arg):- kif_optionally(YN,adapt_to_arity_2(Pred1),Arg,_).

:- meta_predicate kif_optionally(+,2,?,?).
%kif_optionally(_,_,JIGGLED,JIGGLED):- JIGGLED==[],!. 
kif_optionally(Default,Jiggler,KIF,JIGGLED):- \+ is_list(KIF),!,kif_optionally_e(Default,Jiggler,KIF,JIGGLED).
%kif_optionally(never,_,JIGGLED,JIGGLED):-!.
%kif_optionally(always,Jiggler,KIF,JIGGLED):-  must_maplist_det(Jiggler,KIF,JIGGLED),!.
kif_optionally(Default,Jiggler,KIF,JIGGLED):- must_maplist_det(kif_optionally_e(Default,Jiggler),KIF,JIGGLED),!.

:- meta_predicate kif_optionally_e(+,1,?).
kif_optionally_e(YN,Pred1,Arg):- kif_optionally_e(YN,adapt_to_arity_2(Pred1),Arg,_).

:- meta_predicate kif_optionally_e(+,2,?,?).
kif_optionally_e(Default,Jiggler,KIF,JIGGLED):- 
   foption_to_name(Jiggler,Name), !,
  must(kif_optionally_ee(Default,Name,Jiggler,KIF,JIGGLED)),!.

kif_optionally_ee(Default,Name,Jiggler,KIF,JIGGLO):- 
  kif_optionally_eee(Default,Name,Jiggler,KIF,JIGGLED), 
  (JIGGLED\=nesc(nesc(_))->JIGGLO=JIGGLED
   ; (dumpST,
      wdmsg(kif_optionally_ee(Default,Name,Jiggler,KIF,JIGGLO)),
      break,rtrace(kif_optionally_eee(Default,Name,Jiggler,KIF,JIGGLO)),break)).

kif_optionally_eee( never ,_  ,_,JIGGLED,JIGGLED):-!.
kif_optionally_eee(Default,Name,Jiggler,KIF,JIGGLED):-
     (kif_option_value(Name,ShouldDo)-> true ; ShouldDo = Default),
     ((means_false_kif(ShouldDo), \+ Default==always) 
       -> KIF=JIGGLED 
       ; ((locally_tl(kif_option(Name,ShouldDo), must(w_o_c(call(Jiggler,KIF,JIGGLED)))),
          if_debugging2(Name, (KIF \=@= JIGGLED -> sdmsg(Name=JIGGLED); true))))),!.
          %if_debugging2(Name, (KIF \=@= JIGGLED -> sdmsg(Name=(in(KIF)==>out(Name,JIGGLED))); true))))),!.

visit_name_value_option(N,V):-nonvar(N),nonvar(V),V\==always,foption_to_name(N,Opt),set_kif_option_default(Opt,V).

grovel_kif_option(V):- \+ compound(V),!,fail.
grovel_kif_option(clause_b(G)):- !, grovel_kif_option(G).
grovel_kif_option(clause_u(G)):- !, grovel_kif_option(G).
grovel_kif_option(must(G)):- !, grovel_kif_option(G).
grovel_kif_option(==>(G)):- !,grovel_kif_option(G).
grovel_kif_option(==>(G,_)):- !,grovel_kif_option(G).
grovel_kif_option(_:G):- !,grovel_kif_option(G).


grovel_kif_option(kif_optionally(V,N,_,_)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally_e(V,N,_,_)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally(V,_,N,_)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally_e(V,_,N,_)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally(V,N,_)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally_e(V,N,_)):- visit_name_value_option(N,V).
%grovel_kif_option(current_prolog_flag(N,V)):- visit_name_value_option(N,V).
%grovel_kif_option(set_prolog_flag(N,V)):- visit_name_value_option(N,V).
grovel_kif_option(set_kif_option(N,V)):- visit_name_value_option(N,V).
grovel_kif_option(kif_option(N,V)):- visit_name_value_option(N,V).
grovel_kif_option(kif_option_value(N,V)):- visit_name_value_option(N,V).
grovel_kif_option(use_kif_option(N,V)):- visit_name_value_option(N,V).
grovel_kif_option(kif_optionally_e(V,N,_)):- visit_name_value_option(N,V).
grovel_kif_option(feature_setting(N,V)):- visit_name_value_option(N,V).

:- fixup_exports.

:- multifile(system:goal_expansion/4).
:- module_transparent(system:goal_expansion/4).
:- multifile(system:term_expansion/4).
:- module_transparent(system:term_expansion/4).
system:goal_expansion(G,P,_,_):- nonvar(P), once(grovel_kif_option(G)),fail.
system:term_expansion(G,P,_,_):- nonvar(P), once(grovel_kif_option(G)),fail.


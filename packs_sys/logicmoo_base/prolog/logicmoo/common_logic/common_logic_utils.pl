

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_boxlog.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(common_logic_utils,
          [ ]).

:- include(library('logicmoo/common_logic/common_header.pi')).



%:- endif.
%% delistify_last_arg( ?Arg, :PredMiddleArgs, ?Last) is det.
%
% Delistify Last Argument.
%
:- module_transparent(delistify_last_arg/3).

delistify_last_arg(Arg,Pred,Last):- no_repeats(Last,must(delistify_last_arg0(Arg,Pred,Last))).

delistify_last_arg0(Arg,Pred,Last):- is_list(Arg),!,member(E,Arg),must(delistify_last_arg0(E,Pred,Last)).
delistify_last_arg0(Arg,Pred,Last):- call(Pred,Arg,NEW),each_ele(NEW,Last).
/*delistify_last_arg0(Arg,M:Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],
  quietly(M:NEWCALL),member_ele(NEW,Last).*/

each_ele(NEW,Last):- member_ele(NEW,Last).

%% is_kif_clause( ?Var) is det.
%
% If Is A Knowledge Interchange Format Rule.
%
is_kif_clause(Var):- is_ftVar(Var),!,fail.
is_kif_clause(R):- kif_hook(R),!.
is_kif_clause(R):- is_clif(R),!.




%% kif_hook(+TermC) is det.
%
% Knowledge Interchange Format Hook.
%
kif_hook(C):- not_ftCompound(C),!,fail.
kif_hook(_H :- _):-  !,fail.
kif_hook(_H <- _):-  !,fail.
kif_hook(_H --> _):- !,fail.
kif_hook(_ ==> _):-  !,fail.
kif_hook(_ <==> _):- !,fail.
% uncommenting these next 3 lines may break sanity_birdt test

 kif_hook(  ~(H)):- !,nonvar(H),kif_hook(H).
 kif_hook(  \+ H):- !,nonvar(H),kif_hook(H).
 kif_hook(not(H)):- !,nonvar(H),kif_hook(H).

kif_hook( naf(H)):- !,nonvar(H),kif_hook(H).
kif_hook(In):- kif_hook_skel(In).
kif_hook(C):- callable(C),functor(C,F,A),kif_hook(C,F,A).

kif_hook(_,F,_):- atom_concat('sk',_,F),atom_concat(_,'Fn',F),!.
kif_hook(C,_,_):- leave_as_is(C),!,fail.
kif_hook(C,F,_):- is_sentence_functor(F),!,arg(_,C,E),kif_hook(E).

:- fixup_exports.

%% kif_hook_skel(+TermC) is det.
%
% Knowledge Interchange Format Hook Skelecton.
%

kif_hook_skel(forAll(_,_)).
kif_hook_skel(=>(_,_)).
kif_hook_skel(<=(_,_)).
kif_hook_skel(<=>(_,_)).
kif_hook_skel(&(_,_)).
kif_hook_skel((_ /\ _)).
kif_hook_skel((_ \/ _)).
kif_hook_skel(v(_ , _)).
kif_hook_skel(nesc(_)).
kif_hook_skel(poss(_)).
kif_hook_skel(cir(_)).
kif_hook_skel(all(_,_)).
kif_hook_skel(exactly(_,_,_)).
kif_hook_skel(atmost(_,_,_)).
kif_hook_skel(atleast(_,_,_)).
kif_hook_skel(quant(_,_,_)).
kif_hook_skel(exists(_,_)).
kif_hook_skel(if(_,_)).
kif_hook_skel(iff(_,_)).
kif_hook_skel(equiv(_,_)).
kif_hook_skel(implies(_,_)).
kif_hook_skel(CLIF):- is_clif(CLIF).
kif_hook_skel( ~(H)):- loop_check(kif_hook(H)).
kif_hook_skel( not(H)):- loop_check(kif_hook(H)).
kif_hook_skel( Compound):- arg(_,v(poss,nesc,until,always,release,cir),F),between(2,3,A),functor( Compound,F,A).
kif_hook_skel( Compound):- compound( Compound),!,functor(Compound,F,_),arg(_,v(and,or,xor),F).
kif_hook_skel( Compound):- var(Compound),!,arg(_,v(and,or,xor),F),between(1,12,A),functor( Compound,F,A).





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


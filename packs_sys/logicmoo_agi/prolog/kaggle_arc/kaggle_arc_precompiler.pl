/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).


:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll1(0)).
:- meta_predicate(must_det_ll_failed(0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).

%:- no_xdbg_flags.
:- meta_predicate(wno_must(0)).

wno_must(G):- locally(nb_setval(no_must_det_ll,t),locally(nb_setval(cant_rrtrace,t),call(G))).

must_det_ll_maplist(_,[]):-!.
must_det_ll_maplist(P1,[H|T]):- must_det_ll(call(P1,H)), must_det_ll_maplist(P1,T).

must_det_ll_maplist(_,[],[]):-!.
must_det_ll_maplist(P2,[HA|TA],[HB|TB]):- must_det_ll(call(P2,HA,HB)), must_det_ll_maplist(P2,TA,TB).

must_det_ll_maplist(_,[],[],[]):-!.
must_det_ll_maplist(P3,[HA|TA],[HB|TB],[HC|TC]):- must_det_ll(call(P3,HA,HB,HC)), must_det_ll_maplist(P3,TA,TB,TC).

must_det_ll(G):- !, once((notrace(G)*->true;must_det_ll_failed(G))).
must_det_ll(G):- notrace(arc_html),!, ignore(notrace(G)),!.
must_det_ll(G):- tracing,!, once((call(G)*->true;must_det_ll_failed(G))).
%must_det_ll(X):- !,must_not_error(X).
must_det_ll((X,Goal)):- is_trace_call(X),!,call((itrace,Goal)).
must_det_ll(X):- is_trace_call(X),!,itrace.
must_det_ll(X):- nb_current(no_must_det_ll,t),!,call(X),!.
must_det_ll(X):- \+ callable(X), !, throw(must_det_ll_not_callable(X)).
must_det_ll((A*->X;Y)):- !,(must_not_error(A)*->must_det_ll(X);must_det_ll(Y)).
must_det_ll((A->X;Y)):- !,(must_not_error(A)->must_det_ll(X);must_det_ll(Y)).
must_det_ll((X,Cut)):- (Cut==(!)), !, (must_det_ll(X),!).
must_det_ll(my_maplist(P1,List)):- !, must_det_ll_maplist(P1,List).
must_det_ll(my_maplist(P2,ListA,ListB)):- !, must_det_ll_maplist(P2,ListA,ListB).
must_det_ll(my_maplist(P3,ListA,ListB,ListC)):- !, must_det_ll_maplist(P3,ListA,ListB,ListC).
must_det_ll((X,Cut,Y)):- (Cut==(!)), !, (must_det_ll(X),!,must_det_ll(Y)).
must_det_ll((X,Y)):- !, (must_det_ll(X),!,must_det_ll(Y)).
%must_det_ll(X):- notrace(catch(X,_,fail)),!.
must_det_ll(X):- conjuncts_to_list(X,List),List\=[_],!,my_maplist(must_det_ll,List).
must_det_ll(must_det_ll(X)):- !, must_det_ll(X).
must_det_ll(grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
must_det_ll(call(P2,I,O)):- !, must_grid_call(P2,I,O).
%must_det_ll((X,Y,Z)):- !, (must_det_ll(X)->must_det_ll(Y)->must_det_ll(Z)).
%must_det_ll((X,Y)):- !, (must_det_ll(X)->must_det_ll(Y)).
must_det_ll(if_t(X,Y)):- !, if_t(must_not_error(X),must_det_ll(Y)).
must_det_ll(forall(X,Y)):- !, must_det_ll(forall(must_not_error(X),must_not_error(Y))).
must_det_ll(\+ (X, \+ Y)):- !, must_det_ll(forall(must_not_error(X),must_not_error(Y))).

must_det_ll((X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;must_det_ll_failed(X;Y)).
must_det_ll(\+ (X)):- !, (\+ must_not_error(X) -> true ; must_det_ll_failed(\+ X)).
%must_det_ll((M:Y)):- nonvar(M), !, M:must_det_ll(Y).
must_det_ll(X):- must_det_ll1(X),!.

must_det_ll1(X):- tracing,!,must_not_error(X),!.
must_det_ll1(once(A)):- !, once(must_det_ll(A)).
must_det_ll1(X):- 
  strip_module(X,M,P),functor(P,F,A),setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(X)*->true;must_det_ll_failed(X)),
    nop(trace(M:F/A,-fail))),!.

%must_not_error(G):- must(once(G)).

must_not_error(G):- tracing,!,call(G).
must_not_error(G):- is_cgi,!, catch(notrace(G),E,((u_dmsg(E=G)))).
must_not_error(X):- \+ nb_current(cant_rrtrace,t),is_guitracer,!, call(X).
must_not_error(X):- catch(X,E,(always_rethrow(E)-> throw(E);(/*arcST,*/writeq(E=X),pp(etrace=X),
  rrtrace(visible_rtrace([-all,+exception]),X)))).

always_rethrow(_):- nb_current(cant_rrtrace,t),!.
always_rethrow('$aborted').
always_rethrow(must_det_ll_failed(_)).


main_debug:- main_thread,current_prolog_flag(debug,true).

%must_det_ll_failed(X):- predicate_property(X,number_of_clauses(1)),clause(X,(A,B,C,Body)), (B\==!),!,must_det_ll(A),must_det_ll((B,C,Body)).
must_det_ll_failed(G):- tracing,notrace(u_dmsg(must_det_ll_failed(G))),!,throw(must_det_ll_failed(G)).
must_det_ll_failed(G):- main_debug,notrace(u_dmsg(must_det_ll_failed(G))),!,trace,call(G).
must_det_ll_failed(G):- is_cgi,!, u_dmsg(arc_html(must_det_ll_failed(G))).
must_det_ll_failed(X):- notrace,is_guitracer,u_dmsg(failed(X))/*,arcST*/,nortrace,atrace, call(X).
must_det_ll_failed(X):-  u_dmsg(failed(X))/*,arcST*/,nortrace,atrace,visible_rtrace([-all,+fail,+call,+exception],X).
% must_det_ll(X):- must_det_ll(X),!.

:- meta_predicate(rrtrace(0)).
rrtrace(X):- rrtrace(etrace,X).

is_guitracer:- getenv('DISPLAY',_), current_prolog_flag(gui_tracer,true).
:- meta_predicate(rrtrace(1,0)).
rrtrace(P1,X):- nb_current(cant_rrtrace,t),!,nop((u_dmsg(cant_rrtrace(P1,X)))),!,fail.
rrtrace(P1,G):- is_cgi,!, u_dmsg(arc_html(rrtrace(P1,G))),call(P1,G).
rrtrace(P1,X):- notrace, \+ is_guitracer,!,nortrace, /*arcST, sleep(0.5), trace,*/
   (notrace(\+ current_prolog_flag(gui_tracer,true)) -> call(P1,X); (itrace,call(P1,X))).
%rrtrace(_,X):- is_guitracer,!,notrace,nortrace,catch(call(call,gtrace),_,true),atrace,call(X).
rrtrace(P1,X):- itrace,!, call(P1,X).

:- meta_predicate(arc_wote(0)).
arc_wote(G):- with_pp(ansi,wote(G)).
arcST:- itrace,arc_wote(bts),itrace.
atrace:- arc_wote(bts).
%atrace:- ignore((stream_property(X,file_no(2)), with_output_to(X,dumpST))),!.

:- meta_predicate(odd_failure(0)).
odd_failure(G):- nb_current(cant_rrtrace,t),!,call(G).
odd_failure(G):- wno_must(G)*->true;fail_odd_failure(G).

:- meta_predicate(fail_odd_failure(0)).
fail_odd_failure(G):- u_dmsg(odd_failure(G)),rtrace(G), fail.
%fail_odd_failure(G):- call(G)*->true;(u_dmsg(odd_failure(G)),fail,rrtrace(G)).


bts:- 
 ensure_loaded(library(prolog_stack)),
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
  notrace(prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)])),
  stream_property(S,file_no(1)), prolog_stack:print_prolog_backtrace(S, Stack),
  ignore((fail, current_output(Out), \+ stream_property(Out,file_no(1)), print_prolog_backtrace(Out, Stack))),!.

my_assertion(G):- call(G),!.
my_assertion(G):- u_dmsg(my_assertion(G)),writeq(goal(G)),nl,!,ibreak.

must_be_free(Free):- plain_var(Free),!.
must_be_free(Free):- \+ nonvar_or_ci(Free),!.
must_be_free(Nonfree):- arcST,u_dmsg(must_be_free(Nonfree)),
  ignore((attvar(Nonfree),get_attrs(Nonfree,ATTS),pp(ATTS))),ibreak,fail.

must_be_nonvar(Nonvar):- nonvar_or_ci(Nonvar),!.
must_be_nonvar(IsVar):- arcST,u_dmsg(must_be_nonvar(IsVar)),ibreak,fail.

%itrace:- !.
itrace:- \+ current_prolog_flag(debug,true),!.
itrace:- if_thread_main(trace),!.
ibreak:- if_thread_main(((trace,break))).
%recolor(_,_):- ibreak.


remove_must_dets(G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
remove_must_dets(G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.


% goal_expansion(must_det_l(G),I,must_det_ll(G),O):- nonvar(I),source_location(_,_), nonvar(G),I=O.

%goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), remove_must_dets(G,GG),I=O.

%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
/*
goal_expansion(Goal,Out):- compound(Goal), arg(N1,Goal,E), 
   compound(E), E = set(Obj,Member), setarg(N1,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_setarg_p2(P3,E,Cmpd,SA).
get_setarg_p2(P3,E,Cmpd,SA):- arg(N1,Cmpd,E), SA=call(P3,N1,Cmpd).
get_setarg_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(P3,E,Arg,SA).

my_b_set_dict(Member,Obj,Var):- set_omemberh(b,Member,Obj,Var).
%nb_set_dict(Member,Obj,Var),
set_omemberh(_,Member,Obj,Var):- !, arc_setval(Obj,Member,Var).
%nb_link_dict(Member,Obj,Var),
%set_omemberh(nb,Member,Obj,Var):- !, nb_set_dict(Member,Obj,Var).
%set_omemberh(link,Member,Obj,Var):- !, nb_link_dict(Member,Obj,Var).
%set_omemberh(How,Member,Obj,Var):- call(call,How,Member,Obj,Var),!.

set_omember(Member,Obj,Var):-  set_omember(b,Member,Obj,Var).

set_omember(How,Member,Obj,Var):- 
  must_be_nonvar(Member), must_be_nonvar(Obj),  must_be_nonvar(How),  !,
  set_omemberh(How,Member,Obj,Var),!.



get_kov(K,O,V):- dictoo:is_dot_hook(user,O,K,V),!,o_m_v(O,K,V).
get_kov(K,O,V):- ((get_kov1(K,O,V)*->true;(get_kov1(props,O,VV),get_kov1(K,VV,V)))).

get_kov1(K,O,V):- (is_hooked_obj(O),o_m_v(O,K,V))*->true;get_kov2(K,O,V).
% (get_kov(Prop,VM,Value) -> true ; (get_kov(props,VM,Hashmap),nonvar(Hashmap),must_not_error(nb_get_value(Hashmap,Prop,ValueOOV)),get_oov_value(ValueOOV,Value))).
get_kov2(K,O,V):- is_dict(O),!,get_dict(K,O,OOV),get_oov_value(OOV,V).
get_kov2(K,O,V):- nonvar(K),is_rbtree(O),!,rb_lookup(K,V,O).
get_kov2(K,O,V):- is_rbtree(O),!,rb_in(K,V,OOV),get_oov_value(OOV,V).
%get_kov(K,O,V):- is_rbtree(O),!,nb_rb_get_node(K,O,Node),nb_rb_node_value(Node,V).

get_oov_value(ValueOOV,Value):- compound(ValueOOV),ValueOOV=oov(Value),!.
get_oov_value(Value,Value).




term_expansion_setter(I,O):- compound(I), expand_must_det(I,O).

term_expansion_setter((Head:-Body),Out):- 
   get_setarg_p1(setarg,I,Head,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),
   BodyCode = (Body, set_omember(How,Member,Obj,Var)),
   % goal_expansion_setter(BodyCode,Goal),
   expand_term((Head:- BodyCode),Out),!.

%term_expansion_setter((Head:-Body),(Head:-GBody)):- goal_expansion_setter(Body,GBody),!.

:- export(term_expansion_setter/2).
:- system:import(term_expansion_setter/2).

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

is_setter_syntax(I,_Obj,_Member,_Var,_):- \+ compound(I),!,fail.
is_setter_syntax(set(Obj,Member),Obj,Member,_Var,b).
is_setter_syntax(gset(Obj,Member),Obj,Member,_Var,nb).
is_setter_syntax(hset(How,Obj,Member),Obj,Member,_Var,How).
is_setter_syntax(set(ObjMember),Obj,Member,_Var,b):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(gset(ObjMember),Obj,Member,_Var,nb):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(hset(How,ObjMember),Obj,Member,_Var,How):- obj_member_syntax(ObjMember,Obj,Member).

obj_member_syntax(ObjMember,Obj,Member):-compound(ObjMember), compound_name_arguments(ObjMember,'.',[Obj,Member]),!.

expand_must_det(I,_):- \+ compound(I),!,fail.
%expand_must_det(I,_):- compound(I),!,fail. % THIS DISABLES
% THIS DISABLES
%expand_must_det(must_det_ll(GoalL),GoalL):-!.
expand_must_det(must_det_ll(GoalL),GoalLO):- !, expand_must_det0(GoalL,GoalLO).
expand_must_det(my_maplist(P1,GoalL),GoalLO):- P1 ==must_det_ll,!,
  expand_must_det0(GoalL,GoalLO).

expand_must_det0(Nil,true):- Nil==[],!.
expand_must_det0(Var,Var):- \+ callable(Var),!.
expand_must_det0([A|B],(AA,BB)):- assertion(callable(A)), assertion(is_list(B)), !, 
  expand_must_det1(A,AA), expand_must_det0(B,BB).
expand_must_det0(A,AA):- !, expand_must_det1(A,AA).

prevents_expansion(A):- is_trace_call(A).
is_trace_call(A):- A == trace.
is_trace_call(A):- A == itrace.
skip_expansion(A):- A == !.
expand_must_det1(Var,Var):- \+ callable(Var),!.
expand_must_det1(my_maplist(P1,A),must_det_ll_maplist(P1,A)):-!.
expand_must_det1(my_maplist(P2,A,B),must_det_ll_maplist(P2,A,B)):-!.
expand_must_det1(my_maplist(P3,A,B,C),must_det_ll_maplist(P3,A,B,C)):-!.
expand_must_det1(Cut,Cut):-  skip_expansion(Cut).
expand_must_det1(Goal,O):- \+ compound(Goal), !,O = must_det_ll(Goal).
expand_must_det1((A,B),must_det_ll((A,B))):- prevents_expansion(A),!.
expand_must_det1((A,B),(AA,BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1((C*->A;B),(must_not_error(C)*->AA;BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1((C->A;B),(must_not_error(C)->AA;BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1((C;B),(must_not_error(C)->true;BB)):- !, expand_must_det1(B,BB).

expand_must_det1(locally(C,A),locally(C,AA)):- !, expand_must_det1(A,AA).

expand_must_det1(call_cleanup(A,B),call_cleanup(AA,BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1(setup_call_cleanup(C,A,B),setup_call_cleanup(CC,AA,BB)):- !, 
  expand_must_det1(C,CC),expand_must_det1(A,AA), expand_must_det1(B,BB).

expand_must_det1(M:P, M:AABB):-!,expand_must_det1(P, AABB).
expand_must_det1(P, AABB) :- predicate_property(P,(meta_predicate( MP ))),
   strip_module(P,_,SP),strip_module(MP,_,SMP), kaggle_arc_1_pred(_,SP),
   \+ skippable_built_in(P),
   SP=..[F|Args],SMP=..[F|Margs],!,
   my_maplist(expand_meta_predicate_arg,Margs,Args,EArgs),
   AABB=..[F|EArgs].  
expand_must_det1(must_det_ll(AB), AABB):-!, expand_must_det1(AB,AABB).
expand_must_det1( A,must_det_ll(AA)):- expand_goal(A,AA).

kaggle_arc_1_pred(M,P):- 
  predicate_property(M:P,file(F)),
  \+ predicate_property(M:P,imported_from(_)),
  \+ \+ atom_contains(F,'arc_'),  
  \+ atom_contains(F,'_pfc'),
  \+ atom_contains(F,'_afc'),
  % \+ atom_contains(F,'_ui_'),
  true.

%meta_builtin(P):- var(P),meta_builtin(P).
%meta_builtin(P):- predicate_property(P,interpreted),predicate_property(P,static).
skippable_built_in(MP):- strip_module(MP,_,P), predicate_property(system:P,built_in),once(predicate_property(system:P,iso);predicate_property(system:P,notrace)).
%meta_builtin(P):- predicate_property(P,notrace), \+ predicate_property(P,nodebug). 

expand_meta_predicate_arg(':',A,AA):- !,expand_must_det1(A,AA).
expand_meta_predicate_arg(0,A,AA):- !,expand_must_det1(A,AA).
expand_meta_predicate_arg(*,A,AA):- !,expand_must_det1(A,AA).
expand_meta_predicate_arg(_,A,A).

goal_expansion_getter(Goal,O):- \+ compound(Goal), !,O = Goal.
goal_expansion_getter(I,O):- expand_must_det(I,O).
goal_expansion_getter(Goal,get_kov(Func,Self,Value)):-
  compound_name_arguments(Goal,'.', [Self, Func, Value]),!.
goal_expansion_getter(Goal,Out):- 
 compound_name_arguments(Goal,F,Args),
 my_maplist(goal_expansion_getter,Args,ArgsOut),
 compound_name_arguments(Out,F,ArgsOut).

:- export(goal_expansion_getter/2).
:- system:import(goal_expansion_getter/2).


goal_expansion_setter(Goal,_):- \+ compound(Goal), !, fail.
%goal_expansion_setter((G1,G2),(O1,O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.
goal_expansion_setter(set_omember(A,B,C,D),set_omember(A,B,C,D)):-!.
goal_expansion_setter(set_omember(A,B,C),set_omember(b,A,B,C)):-!.
goal_expansion_setter(Goal,get_kov(Func,Self,Value)):- compound(Goal), compound_name_arguments(Goal,'.',[ Self, Func, Value]).

goal_expansion_setter(I,O):- expand_must_det(I,O).

goal_expansion_setter(Goal,Out):- 
   predicate_property(Goal,meta_predicate(_)),!,fail,
   arg(N1,Goal,P), goal_expansion_setter(P,MOut),
   setarg(N1,Goal,MOut), !, expand_goal(Goal, Out).

goal_expansion_setter(Goal,Out):-
   arg(N1,Goal,P),  is_setter_syntax(P,Obj,Member,Var,How),
   setarg(N1,Goal,Var), !, expand_goal((Goal,set_omember(How,Member,Obj,Var)), Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(setarg,I,Goal,P1), compound(I), compound_name_arguments(I,'.',[ Self, Func, Value]),
   call(P1,get_kov(Func,Self,Value)),!,
   expand_goal(Goal,Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(setarg,I,Goal,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),!,
   expand_goal((Goal,set_omember(How,Member,Obj,Var)),Out).

:- export(goal_expansion_setter/2).
:- system:import(goal_expansion_setter/2).


/*
system:term_expansion((Head:-Goal),I,(Head:-Out),O):- nonvar(I),  compound(Goal), 
 goal_expansion_setter(Goal,Out),Goal\=@=Out,I=O,!,
 nop((print(goal_expansion_getter(Goal-->Out)),nl)).
*/
arc_term_expansion1((system:term_expansion((Head:-Body),I,Out,O):- 
   nonvar(I),  compound(Head),      
     term_expansion_setter((Head:-Body),Out),(Head:-Body)=In,In\==Out,I=O,!,
     nop((print(term_expansion_setter(In-->Out)),nl)))).


%system:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!, 
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).

%user:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!, 
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).

:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).
arc_term_expansion1((goal_expansion(Goal,I,Out,O):-  
   goal_expansion_setter(Goal,Out),Goal\==Out,I=O,!, 
  nop((print(goal_expansion_setter(Goal-->Out)),nl)))).

:- export(arc_term_expansions/1).
arc_term_expansions(H:- (current_prolog_flag(arc_term_expansion, true), B)):-
  arc_term_expansion1(H:-B).

:- export(enable_arc_expansion/0).
enable_arc_expansion:-
 forall(arc_term_expansions(Rule),
   (strip_module(Rule,M,Rule0), 
     nop(u_dmsg(asserta_if_new(Rule,M,Rule0))),
     asserta_if_new(Rule))),
 set_prolog_flag(arc_term_expansion, true).

:- export(disable_arc_expansion/0).
disable_arc_expansion:-
 forall(arc_term_expansions(Rule),forall(retract(Rule),true)),
 set_prolog_flag(arc_term_expansion, false).


/*
 tests for term expander


*/
:- if(debugging(term_expansion)).
:- enable_arc_expansion.
:- style_check(-singleton).
dte:- set(_X.local) = val.
dte:- gset(_X.global) = gval.
dte:- must_det_ll((set(_X.a) = b)).
dte:- must_det_ll(locally(nb_setval(e,X.locally),dte([foo|set(X.tail)]))).
dte:- member(set(V.element),set(V.list)).
dte(set(E.v)):- set(E.that)=v.
:- style_check(+singleton).
:- disable_arc_expansion.
:- listing(dte).
:- endif.

/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).

:- meta_predicate(if_t(0,0)).
if_t(IF, THEN) :- call(call,ignore((((IF,THEN))))).
:- meta_predicate(must_ll(0)).
must_ll(G):- md(call,G)*->true;throw(not_at_least_once(G)).
:- meta_predicate(at_least_once(0)).
at_least_once(G):- call(G)*->true;throw(not_at_least_once(G)).

md_like(must_det_ll).
%remove_must_det(MD):- !.
remove_must_det(_):- !,fail.
%remove_must_det(MD):- nb_current(remove_must_det(MD),TF),!,TF==true.
%remove_must_det(MD):- \+ false.

%remove_mds(MD,G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
%remove_mds(MD,G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.
remove_mds(MD,GG,GO):- sub_term(G,GG),compound(G),compound_name_arg(G,MD,GGGG),subst001(GG,G,GGGG,GGG),remove_mds(MD,GGG,GO).
remove_mds(_,GG,GG).
%remove_mds(MD,G,GG):- compound(G), G = ..[MD,AA], compound(G),removed_term(G,GO),expand_goal(GO,GG).

compound_name_arg(G,MD,Goal):- var(G),!,atom(MD),G=..[MD,Goal].
compound_name_arg(G,MD,Goal):- compound(G),!, compound_name_arguments(G,MD,[Goal]).


:- multifile(user:message_hook/3). 
:- dynamic(user:message_hook/3).
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,wdmsg(user:message_hook(Term, Kind, Lines)),trace,fail.
user:message_hook(Term, Kind, Lines):- error==Kind,  wdmsg(user:message_hook(Term, Kind, Lines)),fail.

:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll1(1,0)).
:- meta_predicate(md_failed(1,0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).

%:- no_xdbg_flags.
:- meta_predicate(wno_must(0)).

wno_must(G):- locally(nb_setval(no_must_det_ll,t),locally(nb_setval(cant_rrtrace,t),call(G))).

md_maplist(_MD,_,[]):-!.
md_maplist(MD,P1,[H|T]):- call(MD,call(P1,H)), md_maplist(MD,P1,T).

md_maplist(_MD,_,[],[]):-!.
md_maplist(MD,P2,[HA|TA],[HB|TB]):- call(MD,call(P2,HA,HB)), md_maplist(MD,P2,TA,TB).

md_maplist(_MD,_,[],[],[]):-!.
md_maplist(MD,P3,[HA|TA],[HB|TB],[HC|TC]):- call(MD,call(P3,HA,HB,HC)), md_maplist(MD,P3,TA,TB,TC).

%must_det_ll(G):- !, once((/*notrace*/(G)*->true;md_failed(P1,G))).

must_det_ll(X):- md(once,X).

wraps_each(must_det_ll,once).

md(P1,G):- remove_must_det(MD), wraps_each(MD,P1),!,call(G).
md(P1,G):- never_rrtrace,!, call(P1,G).
md(P1,G):- /*notrace*/(arc_html),!, ignore(/*notrace*/(call(P1,G))),!.
md(P1,G):- tracing,!, call(P1,G). % once((call(G)*->true;md_failed(P1,G))).
%md(P1,X):- !,must_not_error(X).
md(P1,(X,Goal)):- is_trace_call(X),!,call((itrace,call(P1,Goal))).
md(_, X):- is_trace_call(X),!,itrace.
md(P1, X):- nb_current(no_must_det_ll,t),!,call(P1,X).
md(P1,X):- \+ callable(X), !, throw(md_not_callable(P1,X)).
md(P1,(A*->X;Y)):- !,(must_not_error(A)*->md(P1,X);md(P1,Y)).
md(P1,(A->X;Y)):- !,(must_not_error(A)->md(P1,X);md(P1,Y)).
md(P1,(X,Cut)):- (Cut==(!)),md(P1,X),!.
md(MD,maplist(P1,List)):- !, call(MD,md_maplist(MD,P1,List)).
md(MD,maplist(P2,ListA,ListB)):- !, call(MD,md_maplist(MD,P2,ListA,ListB)).
md(MD,maplist(P3,ListA,ListB,ListC)):- !, call(MD,md_maplist(MD,P3,ListA,ListB,ListC)).
md(P1,(X,Cut,Y)):- (Cut==(!)), !, (md(P1,X),!,md(P1,Y)).
md(P1,(X,Y)):- !, (md(P1,X),md(P1,Y)).
%md(P1,X):- /*notrace*/(catch(X,_,fail)),!.
%md(P1,X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
md(_,must_det_ll(X)):- !, must_det_ll(X).
md(_,grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,(X,Y,Z)):- !, (md(P1,X)->md(P1,Y)->md(P1,Z)).
%md(P1,(X,Y)):- !, (md(P1,X)->md(P1,Y)).
%md(P1,if_t(X,Y)):- !, if_t(must_not_error(X),md(P1,Y)).
md(P1,forall(X,Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).
md(P1,\+ (X, \+ Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).

md(P1,(X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;md_failed(P1,X;Y)).
md(P1,\+ (X)):- !, (\+ must_not_error(X) -> true ; md_failed(P1,\+ X)).
%md(P1,(M:Y)):- nonvar(M), !, M:md(P1,Y).
md(P1,X):- 
 catch(must_det_ll1(P1,X),
  md_failed(P1,G,N), % <- ExceptionTerm
   % bubble up and start running 
  ((M is N -1, M>0)->throw(md_failed(P1,G,M));(ftrace(X),throw('$aborted')))),!.
%must_det_ll(X):- must_det_ll1(P1,X),!.

must_det_ll1(P1,X):- tracing,!,must_not_error(call(P1,X)),!.
must_det_ll1(P1,once(A)):- !, once(md(P1,A)).
must_det_ll1(P1,X):- 
  strip_module(X,M,P),functor(P,F,A),setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(call(P1,X))*->true;md_failed(P1,X)),
    nop(trace(M:F/A,-fail))),!.

%must_not_error(G):- must(once(G)).

must_not_error(G):- (tracing;never_rrtrace),!,call(G).
must_not_error(G):- notrace(is_cgi),!, catch((G),E,((u_dmsg(E=G)))).
%must_not_error(X):- is_guitracer,!, call(X).
%must_not_error(G):- !, call(G).
must_not_error(X):- !,catch(X,E,(wdmsg(E=X),trace,ftrace(X))).
must_not_error(X):- catch(X,E,(rethrow_abort(E);(/*arcST,*/writeq(E=X),pp(etrace=X),
  trace,
  rrtrace(visible_rtrace([-all,+exception]),X)))).

always_rethrow(E):- never_rrtrace,!,throw(E).
always_rethrow('$aborted').
always_rethrow(md_failed(_,_,_)).

%catch_non_abort(Goal):- cant_rrtrace(Goal).
catch_non_abort(Goal):- catch(cant_rrtrace(Goal),E,rethrow_abort(E)),!.
rethrow_abort(E):- format(user_error,'~N~q~n',[catch_non_abort_or_abort(E)]),fail.
rethrow_abort(time_limit_exceeded):-!.
rethrow_abort('$aborted'):- !, throw('$aborted'),!,forall(between(1,700,_),sleep(0.01)),writeln(timeout),!,fail.
rethrow_abort(E):- ds,!,format(user_error,'~N~q~n',[catch_non_abort(E)]),!.
never_rrtrace:- nb_current(cant_rrtrace,t),!,notrace.
never_rrtrace:- is_cgi,notrace.
cant_rrtrace(Goal):- never_rrtrace,!,call(Goal). 
cant_rrtrace(Goal):- setup_call_cleanup(cant_rrtrace,Goal,can_rrtrace).

main_debug:- main_thread,current_prolog_flag(debug,true).
cant_rrtrace:- nb_setval(cant_rrtrace,t).
can_rrtrace:- nb_setval(cant_rrtrace,f).
%md_failed(P1,X):- predicate_property(X,number_of_clauses(1)),clause(X,(A,B,C,Body)), (B\==!),!,must_det_ll(A),must_det_ll((B,C,Body)).
md_failed(P1,G):- never_rrtrace,!,notrace,/*notrace*/(u_dmsg(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
md_failed(_,_):- never_rrtrace,!,fail.
md_failed(P1,G):- tracing,/*notrace*/(u_dmsg(md_failed(P1,G))),!,fail. 
md_failed(P1,G):- main_debug,/*notrace*/(u_dmsg(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
md_failed(P1,G):- is_cgi,!, u_dmsg(arc_html(md_failed(P1,G))).
md_failed(P1,X):- notrace,is_guitracer,u_dmsg(failed(X))/*,arcST*/,nortrace,atrace, call(P1,X).
md_failed(P1,X):-  u_dmsg(failed(P1,X))/*,arcST*/,nortrace,atrace,
 trace,visible_rtrace([-all,+fail,+call,+exception],X).
% must_det_ll(X):- must_det_ll(X),!.

:- meta_predicate(rrtrace(0)).
rrtrace(X):- rrtrace(etrace,X).

is_guitracer:- getenv('DISPLAY',_), current_prolog_flag(gui_tracer,true).
:- meta_predicate(rrtrace(1,0)).
rrtrace(P1,X):- never_rrtrace,!,nop((u_dmsg(cant_rrtrace(P1,X)))),!,fail.
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
odd_failure(G):- never_rrtrace,!,call(G).
odd_failure(G):- wno_must(G)*->true;fail_odd_failure(G).

:- meta_predicate(fail_odd_failure(0)).
fail_odd_failure(G):- u_dmsg(odd_failure(G)),rtrace(G), fail.
%fail_odd_failure(G):- call(G)*->true;(u_dmsg(odd_failure(G)),fail,rrtrace(G)).


bts:- 
 ensure_loaded(library(prolog_stack)),
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
  /*notrace*/(prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)])),
  stream_property(S,file_no(1)), prolog_stack:print_prolog_backtrace(S, Stack),
  ignore((fail, current_output(Out), \+ stream_property(Out,file_no(1)), print_prolog_backtrace(Out, Stack))),!.

my_assertion(G):- my_assertion(call(G),G).

my_assertion(_,G):- call(G),!.
my_assertion(Why,G):- u_dmsg(my_assertion(Why,G)),writeq(Why=goal(G)),nl,!,ibreak.

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



% goal_expansion(must_det_l(G),I,must_det_ll(G),O):- nonvar(I),source_location(_,_), nonvar(G),I=O.

%goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), remove_mds(MD,G,GG),I=O.

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


term_expansion_setter(I,O):- maybe_expand_md(must_det_ll,I,O),I\=@=O,!.
term_expansion_setter(I,O):- maybe_expand_md(must_det_ll,I,M),I\=@=M,!,term_expansion_setter(M,O).
term_expansion_setter(Goal,get_kov(Func,Self,Value)):- compound(Goal), 
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).

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

maybe_expand_md(_MD,I,_):- \+ compound(I),!,fail.
%maybe_expand_md(MD,I,_):- compound(I),!,fail. % THIS DISABLES
% THIS DISABLES
%maybe_expand_md(MD,must_det_ll(GoalL),GoalL):-!.
maybe_expand_md(MD,MDGoal,GoalLO):- compound_name_arg(MDGoal,MD,Goal),!, expand_md(MD,Goal,GoalLO).
maybe_expand_md(MD,maplist(P1,GoalL),GoalLO):- P1 ==MD,!,
  expand_md(MD,GoalL,GoalLO).
maybe_expand_md(MD,maplist(P1,GoalL),GoalLO):- P1 ==MD,!,
  expand_md(MD,GoalL,GoalLO).
maybe_expand_md(MD,I,O):- sub_term(C,I),compound(C), compound_name_arg(C,MD,Goal),
   compound(Goal),Goal=(_,_),
   once((expand_md(MD,Goal,GoalO),subst(I,C,GoalO,O))),I\=@=O.

  
%maybe_expand_md(MD,I,O):- sub_term(S,I),compound(S),S=must_det_ll(G),
%  once(expand_md(MD,S,M)),M\=S,
  


expand_md(_MD,Nil,true):- Nil==[],!.
expand_md(_MD,Var,Var):- \+ callable(Var),!.
expand_md(MD,[A|B],(AA,BB)):- assertion(callable(A)), assertion(is_list(B)), !, 
  expand_md1(MD,A,AA), expand_md(MD,B,BB).
expand_md(MD,A,AA):- !, expand_md1(MD,A,AA).

prevents_expansion(A):- is_trace_call(A).
is_trace_call(A):- A == trace.
is_trace_call(A):- A == itrace.

skip_expansion(A):- var(A),!,fail.
skip_expansion(!).
skip_expansion(false).
skip_expansion(true).
skip_expansion(C):- compound(C),functor(C,F,A),skip_fa_expansion(F,A).
skip_fa_expansion(once,1).
skip_fa_expansion(call,_).
skip_fa_expansion(if_t,2).

expand_md1(_MD,Var,Var):- \+ callable(Var),!.
expand_md1(_MD,Cut,Cut):-  skip_expansion(Cut),!.
expand_md1(MD,MDAB, AABB):- compound(MDAB), compound_name_arg(MDAB,MD,AB),!, expand_md(MD,AB,AABB).
expand_md1(MD,maplist(P1,A),md_maplist(MD,P1,A)):-!.
expand_md1(MD,maplist(P2,A,B),md_maplist(MD,P2,A,B)):-!.
expand_md1(MD,maplist(P3,A,B,C),md_maplist(MD,P3,A,B,C)):-!.
expand_md1(MD,my_maplist(P1,A),md_maplist(MD,P1,A)):-!.
expand_md1(MD,my_maplist(P2,A,B),md_maplist(MD,P2,A,B)):-!.
expand_md1(MD,my_maplist(P3,A,B,C),md_maplist(MD,P3,A,B,C)):-!.
%expand_md1(MD,Goal,O):- \+ compound(Goal), !,O = must_det_ll(Goal).
%expand_md1(MD,(A,B),((A,B))):- remove_must_det(MD), prevents_expansion(A),!.
%expand_md1(MD,(A,B),must_det_ll((A,B))):- prevents_expansion(A),!.
expand_md1(MD,(A,B),(AA,BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB).
expand_md1(MD,(C*->A;B),(CC*->AA;BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB), expand_must_not_error(C,CC).
expand_md1(MD,(C->A;B),(CC->AA;BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB), expand_must_not_error(C,CC).
expand_md1(MD,(C;B),(CC;BB)):- !, expand_md(MD,B,BB), expand_must_not_error(C,CC).

expand_md1(MD,locally(C,A),locally(C,AA)):- !, expand_md(MD,A,AA).

expand_md1(MD,call_cleanup(A,B),call_cleanup(AA,BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB).
expand_md1(MD,setup_call_cleanup(C,A,B),setup_call_cleanup(CC,AA,BB)):- !, 
  expand_md(MD,C,CC),expand_md(MD,A,AA), expand_md(MD,B,BB).

expand_md1(MD,M:P, M:AABB):-!,expand_md(MD,P, AABB).

expand_md1(MD,P, AABB) :- predicate_property(P,(meta_predicate( MP ))),
   strip_module(P,_,SP),strip_module(MP,_,SMP), kaggle_arc_1_pred(_,SP),
   \+ skippable_built_in(P),
   SP=..[F|Args],SMP=..[F|Margs],!,
   maplist(expand_meta_predicate_arg(MD),Margs,Args,EArgs),
   AABB=..[F|EArgs].  

expand_md1(MD, A, MDAA):- \+ remove_must_det(MD), !, expand_goal(A,AA),!,compound_name_arg(MDAA,MD,AA).
expand_md1(_MD, A, AA):- expand_goal(A,AA),!.

expand_must_not_error(C,C):- remove_must_det(must_not_error),!.
expand_must_not_error(C,CC):- \+ predicate_property(C,meta_predicate(_)),!, CC = must_not_error(C),!.
expand_must_not_error(C,CC):- expand_md(must_not_error, C, CC). 

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
skippable_built_in(MP):- strip_module(MP,_,P), predicate_property(system:P,built_in),
  once(predicate_property(system:P,iso);predicate_property(system:P,notrace)).
%meta_builtin(P):- predicate_property(P,/*notrace*/), \+ predicate_property(P,nodebug). 

expand_meta_predicate_arg(_MD,'?',A,A):-!.
expand_meta_predicate_arg(_MD,'+',A,A):-!.
expand_meta_predicate_arg(_MD,'-',A,A):-!.
expand_meta_predicate_arg(MD, ':',A,AA):- !,expand_md1(MD,A,AA).
expand_meta_predicate_arg(MD,  0,A,AA):- !,expand_md1(MD,A,AA).
%expand_meta_predicate_arg(MD,*,A,AA):- !,expand_md1(MD,A,AA).
expand_meta_predicate_arg(_MD,_,A,A).

goal_expansion_getter(Goal,O):- \+ compound(Goal), !,O = Goal.
goal_expansion_getter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.
goal_expansion_getter(I,O):- md_like(MD),maybe_expand_md(MD,I,M),I\=@=M,!,goal_expansion_getter(M,O).
goal_expansion_getter(Goal,get_kov(Func,Self,Value)):- compound(Goal), 
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).
goal_expansion_getter(Goal,Out):- 
 compound_name_arguments(Goal,F,Args),
 maplist(goal_expansion_getter,Args,ArgsOut),
 compound_name_arguments(Out,F,ArgsOut).

:- export(goal_expansion_getter/2).
:- system:import(goal_expansion_getter/2).


goal_expansion_setter(Goal,_):- \+ compound(Goal), !, fail.


goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.
goal_expansion_setter(G,GO):- remove_must_det(MD), !,remove_mds(MD,G,GG),goal_expansion_setter(GG,GO).
%goal_expansion_setter(GG,GO):- remove_must_det(MD), sub_term(G,GG),compound(G),G = must_det_ll(GGGG),subst001(GG,G,GGGG,GGG),!,goal_expansion_setter(GGG,GO).
%goal_expansion_setter((G1,G2),(O1,O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.
goal_expansion_setter(set_omember(A,B,C,D),set_omember(A,B,C,D)):-!.
goal_expansion_setter(set_omember(A,B,C),set_omember(b,A,B,C)):-!.
goal_expansion_setter(Goal,get_kov(Func,Self,Value)):- compound(Goal), 
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).
goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,M),I\=@=M,!,goal_expansion_setter(M,O).


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

:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).

goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), 
    compound(G), 
     (remove_must_det(MD)->remove_mds(MD,G,GG);(md_like(MD),maybe_expand_md(MD,G,GG))),I=O.


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



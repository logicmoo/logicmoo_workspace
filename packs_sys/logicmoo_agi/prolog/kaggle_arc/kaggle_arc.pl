/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- encoding(iso_latin_1).

:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(stream_type_check,false).
:- current_prolog_flag(argv,C),(member('--',C)->set_prolog_flag(load_arc_webui,true);true).
:- current_prolog_flag(argv,C),(member('--',C)->set_prolog_flag(use_arc_webui,true);set_prolog_flag(use_arc_webui,false)).
:- set_prolog_flag(arc_term_expansion,false).

:- dynamic('$messages':to_list/2).
:- multifile('$messages':to_list/2).
:- asserta(('$messages':to_list(In, List) :- ((is_list(In)-> List = In ; List = [In])),!)).
%my_time(Goal):- !,call(Goal).

:- use_module(library(statistics)).
:- import(prolog_statistics:time/1).
my_time(Goal):- time(Goal),flush_tee.

catch_log(G):- ignore(catch(notrace(G),E,writeln(E=G))).
catch_nolog(G):- ignore(catch(notrace(G),E,nop(wdmsg(E=G)))).

%:- pack_install('https://github.com/logicmoo/logicmoo_utils.git').
:- catch_log(pack_install(logicmoo_utils,[
  %url('https://github.com/logicmoo/logicmoo_utils.git'),
  interactive(false),
  upgrade(true),git(true)])).
:- pack_upgrade(logicmoo_utils),!.
% :- pack_install(dictoo).
% :- pack_upgrade(dictoo).



%:- module(system).

:- set_prolog_flag(arc_term_expansion, false).

:- include(kaggle_arc_header).

arc_history(_).
arc_history1(_).
% :- dynamic(grid_hint_pred/1). :- discontiguous(grid_hint_pred/1). :- multifile(grid_hint_pred/1).


my_is_clause(H,B):- clause(H,B,Ref),clause(HH,BB,Ref), H+B=@=HH+BB,!.
my_asserta_if_new((H:-B)):- !, (my_is_clause(H,B) -> nop(wdmsg(my_is_clause(H,B))) ; arc_assert(H:-B)).
my_asserta_if_new(HB):- my_asserta_if_new(HB:-true).

my_assertz_if_new((H:-B)):- !, (my_is_clause(H,B) -> true ; assertz(H:-B)).
my_assertz_if_new(HB):- my_assertz_if_new(HB:-true).

%:- multifile(decl_sf/1).
%:- discontiguous(decl_sf/1).
%:- dynamic(decl_sf/1).
:- dynamic(is_decl_sf/1).
decl_sf(G):- must_det_ll((nonvar(G), !, my_assertz_if_new(is_decl_sf(G)))).
%:- multifile(decl_pt/2).
%:- discontiguous(decl_pt/2).
%:- dynamic(decl_pt/2).
%:- multifile(decl_pt/1).
%:- discontiguous(decl_pt/1).
%:- dynamic(decl_pt/1).
:- dynamic(is_decl_pt/2).
:- discontiguous(is_decl_pt/2).
:- multifile(is_decl_pt/2).

decl_pt(G):- must_det_ll((nonvar(G), !, my_assertz_if_new(is_decl_pt(plain,G)))).
decl_pt(How,G):- must_det_ll((nonvar(How),nonvar(G), !, my_assertz_if_new(is_decl_pt(How,G)))).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- set_stream(user_output, tty(true)).
:- set_stream(user_error, tty(true)).
%:- set_stream(user_output, newline(unix)).

:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).

:- meta_predicate(if_t(0,0)).
if_t(IF, THEN) :- (   call(IF) ->  call(THEN) ;   true ).
:- meta_predicate(quietlyd(0)).
:- export(quietlyd/1).
quietlyd(G):- quietly(G),!.

:- strip_module(_,M,_),abolish(system:muarc_mod/1),asserta(system:muarc_mod(M)).

/*
:- discontiguous '$exported_op'/3. 
*/
:- multifile system:'$exported_op'/3. 
:- dynamic system:'$exported_op'/3. 
:- catch((assert(system:('$exported_op'(_,_,_):- fail))),_,true).

'$pldoc'(_,_,_,_):- fail.
:- multifile '$pldoc'/4. 
:- dynamic '$pldoc'/4. 
%:- discontiguous '$pldoc'/4. 

'$autoload'(_,_,_):- fail.
:- multifile '$autoload'/3. 
%:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.

update_changes:- notrace((ignore(update_changed_files))).
cls_z_make:- notrace((ignore(cls_z),ignore(update_and_fail))).
clsmake:- notrace(ignore((\+ is_detatched_thread, cls_z_make))),!.
update_and_fail:- once(update_changes),fail.
update_and_fail_cls:- once(cls_z),update_and_fail.

% COMMAND LINE ARC
:- if(\+ current_module(logicmoo_arc)).

  :- set_prolog_flag(access_level,system).
  
  :- SL  is 2_147_483_648*8*4, set_prolog_flag(stack_limit, SL ).
  :- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
  %:- unsetenv('DISPLAY').
  %  :- (getenv('DISPLAY',_) -> guitracer ; true).
  %  :- noguitracer.
  :- set_prolog_flag(toplevel_print_anon,false).
  :- set_prolog_flag(toplevel_print_factorized,true).
  
  :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(5), attributes(dots)]).
  :- set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(5), attributes(dots)]).
  :- set_prolog_flag(print_write_options, [quoted(true), portray(true), max_depth(50), attributes(dots)]).
  
  :- set_prolog_flag(debug_on_error,true).
  :- set_prolog_flag(report_error,true).
  :- set_prolog_flag(on_error,status).
  :- set_prolog_flag(debugger_show_context,true).
  
  :- set_prolog_flag(last_call_optimisation,false).
  %:- set_prolog_flag(trace_gc,false).
  :- set_prolog_flag(write_attributes,dots).
  :- set_prolog_flag(backtrace_depth,1000).
  :- catch(noguitracer,_,true).

%arc_assert(P):- pfcAddF(P).

:- else.  % SWISH ARC

  :- catch(noguitracer,_,true).

  :- if(current_module(trill)).
    :- set_prolog_flag_until_eof(trill_term_expansion,false).
    :- dynamic(muarc:ns4query/1).
  :- endif.

  pfcUnique(_,P):- mpred_unique_u(P).
  pfcAdd(P):- mpred_ain(P).
  pfcFwd(P):- mpred_fwc(P).
  arc_assert(P):- pfcAdd(P).

:- endif.

pfcAddF(P):-  
  forall(retract(P),true),
  ignore(mpred_info(P)),
  pfcUnique(post, P)-> pfcAdd(P) ; pfcFwd(P).


%:- set_prolog_flag(verbose_load,true).  
%:- set_prolog_flag(verbose_autoload,true).



:- current_prolog_flag(argv,C),wdmsg(current_prolog_flag(argv,C)),!.

:- set_prolog_flag(no_sandbox,true).


with_webui(_Goal):- \+ current_prolog_flag(use_arc_webui,true),!.
with_webui(Goal):- ignore(when_arc_webui(with_http(Goal))).
%:- initialization arc_http_server.

:- exists_source(library(xlisting/xlisting_web)) -> system:use_module(library(xlisting/xlisting_web)) ; true.

ld_logicmoo_webui:-
   exists_source(library(logicmoo_webui)), use_module(library(logicmoo_webui)), 
  system:use_module(library(xlisting/xlisting_web)),
  system:use_module(library(xlisting/xlisting_web_server)),
  catch_log(dmsg((?-webui_start_swish_and_clio))).
ld_logicmoo_webui.

logicmoo_webui:- ld_logicmoo_webui,catch_log(call(call,webui_start_swish_and_clio)).

:- ld_logicmoo_webui.
:- (current_prolog_flag(load_arc_webui,true)->catch_log(ld_logicmoo_webui) ; true).

    


%:- autoload_all.


%:- listing((.)/3).
%:- autoload_all.
:- set_prolog_flag(verbose_load,false).  
:- set_prolog_flag(verbose_autoload,false).


% we alias these so we can catch out of control list growth
my_append(A,B):- append(A,B).
my_append(A,B,C):- append(A,B,C). % ,check_len(A),check_len(C),check_len(C).
check_len(_).

:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll_failed(0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).

:- no_xdbg_flags.

%must_det_ll(G):- !, call(G).
%must_det_ll(X):- !,must_not_error(X).
must_det_ll(X):- \+ callable(X), !, throw(must_det_ll_not_callable(X)).
must_det_ll((X,!)):- !, (must_det_ll(X),!).
must_det_ll((X,!,Y)):- !, (must_det_ll(X),!,must_det_ll(Y)).
must_det_ll((X,Y)):- !, (must_det_ll(X),must_det_ll(Y)).
%must_det_ll(X):- notrace(catch(X,_,fail)),!.
must_det_ll(X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
must_det_ll(must_det_ll(X)):- !, must_det_ll(X).
must_det_ll(grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
must_det_ll(call(P2,I,O)):- !, must_grid_call(P2,I,O).
%must_det_ll((X,Y,Z)):- !, (must_det_ll(X)->must_det_ll(Y)->must_det_ll(Z)).
%must_det_ll((X,Y)):- !, (must_det_ll(X)->must_det_ll(Y)).
must_det_ll(if_t(X,Y)):- !, if_t(must_not_error(X),must_det_ll(Y)).
must_det_ll((A*->X;Y)):- !,(must_not_error(A)*->must_det_ll(X);must_det_ll(Y)).
must_det_ll((A->X;Y)):- !,(must_not_error(A)->must_det_ll(X);must_det_ll(Y)).
must_det_ll((X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;must_det_ll_failed(X;Y)).
must_det_ll(\+ (X)):- !, (\+ must_not_error(X) -> true ; must_det_ll_failed(\+ X)).
%must_det_ll((M:Y)):- nonvar(M), !, M:must_det_ll(Y).
must_det_ll(X):- tracing,!,must_not_error(X).
must_det_ll(once(A)):- !, once(must_det_ll(A)).
must_det_ll(X):- 
  strip_module(X,M,P),functor(P,F,A),setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(X)*->true;must_det_ll_failed(X)),
    nop(trace(M:F/A,-fail))).
  
must_not_error(X):- catch(X,E,((E=='$aborted';nb_current(cant_rrtrace,t))-> throw(E);(/*arcST,*/writeq(E=X),pp(etrace=X),
  rrtrace(visible_rtrace([-all,+exception]),X)))).


odd_failure(G):- call(G)*->true;(wdmsg(odd_failure(G)),fail,rrtrace(G)).


%must_det_ll_failed(X):- predicate_property(X,number_of_clauses(1)),clause(X,(A,B,C,Body)), (B\==!),!,must_det_ll(A),must_det_ll((B,C,Body)).
must_det_ll_failed(X):- notrace,wdmsg(failed(X))/*,arcST*/,nortrace,trace,visible_rtrace([-all,+fail,+exception],X).
% must_det_ll(X):- must_det_ll(X),!.

rrtrace(X):- rrtrace(etrace,X).

rrtrace(P1,X):- nb_current(cant_rrtrace,t),!,nop((wdmsg(cant_rrtrace(P1,X)))),!,fail.
rrtrace(P1,X):- !, call(P1,X).
rrtrace(P1,X):- notrace,nortrace, arcST, sleep(0.5), trace, (notrace(\+ current_prolog_flag(gui_tracer,true)) -> call(P1,X); (trace,call(P1,X))).

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

/*

set(_355218._355220)=_355272)
*/

is_setter_syntax(I,_Obj,_Member,_Var,_):- \+ compound(I),!,fail.
is_setter_syntax(set(Obj,Member),Obj,Member,_Var,b).
is_setter_syntax(gset(Obj,Member),Obj,Member,_Var,nb).
is_setter_syntax(hset(How,Obj,Member),Obj,Member,_Var,How).
is_setter_syntax(set(ObjMember),Obj,Member,_Var,b):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(gset(ObjMember),Obj,Member,_Var,nb):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(hset(How,ObjMember),Obj,Member,_Var,How):- obj_member_syntax(ObjMember,Obj,Member).

obj_member_syntax(ObjMember,Obj,Member):-compound(ObjMember), compound_name_arguments(ObjMember,'.',[Obj,Member]),!.

expand_must_det(I,_):- \+ compound(I),!,fail.
expand_must_det(must_det_ll(GoalL),GoalLO):- !, expand_must_det1(GoalL,GoalLO).
expand_must_det(maplist(P1,GoalL),GoalLO):- P1 ==must_det_ll,!,
  expand_must_det1(GoalL,GoalLO).

expand_must_det1(Nil,true):- Nil==[],!.
expand_must_det1(Var,Var):- \+ compound(Var),!.
expand_must_det1((A,B),(AA,BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1([A|B],(AA,BB)):- !, expand_must_det1(A,AA), expand_must_det1(B,BB).
expand_must_det1(must_det_ll(AB), AABB):-!, expand_must_det1(AB,AABB).
expand_must_det1( A,must_det_ll(AA)):- expand_goal(A,AA).

goal_expansion_getter(Goal,O):- \+ compound(Goal), !,O = Goal.
goal_expansion_getter(I,O):- expand_must_det(I,O).
goal_expansion_getter(Goal,get_kov(Func,Self,Value)):-
  compound_name_arguments(Goal,'.', [Self, Func, Value]),!.
goal_expansion_getter(Goal,Out):- 
 compound_name_arguments(Goal,F,Args),
 maplist(goal_expansion_getter,Args,ArgsOut),
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

get_map_pairs(Map,is_assoc,Pairs):- is_assoc(Map), assoc_to_list(Map, Pairs).
get_map_pairs(Map,is_rbtree,Pairs):- is_rbtree(Map), rb_visit(Map, Pairs).
get_map_pairs(Map,is_dict(T),Pairs):- is_dict(Map), dict_pairs(Map,T,Pairs).

is_vm(Tree):- is_map(Tree), once(get_kov(program,Tree,_);get_kov(program_i,Tree,_)).

is_map(Tree):- is_rbtree(Tree),!.
is_map(Dict):- is_dict(Dict),!.



arc_setval(TT,List):- is_list(List),!,maplist(arc_setval(TT),List).
arc_setval(TT,Map):- get_map_pairs(Map,_Type,Pairs),!,maplist(arc_setval(TT),Pairs).
arc_setval(TT,N=V):- !, arc_setval(TT,N,V).
arc_setval(TT,N-V):- !, arc_setval(TT,N,V).
arc_setval(TT,NV):- arc_setval(TT,NV,t).
arc_setval(TT,N,V):- is_dict(TT),!, nb_set_dict(N,TT,V).
arc_setval(TT,N,V):- (nb_rb_get_node(TT,N,Node)->nb_rb_set_node_value(Node,V);nb_rb_insert(TT,N,V)).


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
   (strip_module(Rule,M,Rule0), wdmsg(asserta_if_new(Rule,M,Rule0)),asserta_if_new(Rule))),
 set_prolog_flag(arc_term_expansion, true).

:- export(disable_arc_expansion/0).
disable_arc_expansion:-
 forall(arc_term_expansions(Rule),forall(retract(Rule),true)),
 set_prolog_flag(arc_term_expansion, false).


/*
 tests for term expander

:- style_check(-singleton).

d:- set(X.Y) = V.
d:- must_det_ll((set(X.a) = b)).
d:- must_det_ll(didit([foo|set(X.Y)])).
d:- member(set(X.Y),V).
doit(set(E.v)):- that.
:- style_check(+singleton).
*/

%arc_user(main):- !.
arc_user(ID):- thread_self(TID),arc_user(TID, ID).

suggest_arc_user(ID):- catch((if_arc_webui(xlisting_web:find_http_session(ID))),_,fail),!.
suggest_arc_user(ID):- catch((pengine:pengine_user(ID)),_,fail),!.
suggest_arc_user(ID):- catch((http_session:session_data(_,username(ID))),_,fail),!.

arc_webui:-  notrace(arc_webui0).

arc_webui0:- current_prolog_flag(use_arc_webui,false),!,fail.
arc_webui0:- toplevel_pp(http),!.
arc_webui0:- in_pp(http),!.
arc_webui0:- toplevel_pp(swish),!.
arc_webui0:- in_pp(swish),!,fail.
arc_webui0:- in_pp(bfly),!.
arc_webui0:- is_webui,!.

arc_user(TID, ID):- \+ arc_webui,!,TID=ID,!.
arc_user(TID, ID):- catch((http_session:session_data(TID,username(ID))),_,fail),!.
arc_user(TID, ID):- suggest_arc_user(ID), TID\=ID,!.


:- dynamic(arc_user_prop/3).

%luser_setval(N,V):- nb_setval(N,V),!.
luser_setval(N,V):- arc_user(ID),luser_setval(ID,N,V),!.
luser_setval(ID,N,V):- nb_setval(N,V),retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).

luser_default(N,V):- luser_setval(global,N,V).

luser_linkval(N,V):- arc_user(ID),luser_linkval(ID,N,V),!.
luser_linkval(ID,N,V):- nb_linkval(N,V),retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).

:- meta_predicate(if_arc_webui(-)).
%if_arc_webui(Goal):- !, fail,Goal.
if_arc_webui(_):- \+ arc_webui,!,fail.
if_arc_webui(Goal):- arc_webui,!,g_out(call(Goal)).


:- meta_predicate(when_arc_webui(-)).
when_arc_webui(G):- toplevel_pp(http),call(G),!.
when_arc_webui(G):- toplevel_pp(swish),call(G),!.
when_arc_webui(G):- ignore(if_arc_webui(G)).

%arc_option(grid_size_only):- !,fail.
arc_option(P):- luser_getval(P,t).

with_luser(N,V,Goal):-
  luser_getval(N,OV),
  setup_call_cleanup(
    luser_setval(N,V),
    Goal,
    luser_getval(N,OV)).

luser_getval(N,V):- nb_current(N,VV),VV\==[],!,V=VV.
% caches the valuetemp on this thread
luser_getval(N,V):-  luser_getval_0(N,VV),nb_setval(N,VV),b_setval(N,VV),!,VV=V.

luser_getval_0(N,V):- if_arc_webui((((get_param_req_or_session/2),get_param_req_or_session(N,V), V\=='',V\==""))).
luser_getval_0(N,V):- arc_user(ID),luser_getval_id(ID,N,V),!.
%luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.
%luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).
luser_getval_id(ID,N,V):-
 ((nb_current(N,Val),Val\==[])*->Val=V;
  (arc_user_prop(ID,N,V)*->true;arc_user_prop(global,N,V))).

/*
luser_getval(ID,N,V):- 
 (arc_user_prop(ID,N,V)*->true;
  (nb_current(N,V))*->true;arc_user_prop(global,N,V)).
*/

%c:- forall(clause(fav(A,B),true),arc_history1((fav(A,B)))).
:- arc_history1(fav2).
:- arc_history1(arc2).
:- arc_history1(arc).
:- arc_history1(arc1).
:- arc_history1(fav).
:- arc_history1(fav1).
:- arc_history1(fav3).

:- multifile(mregression_test/0).
:- dynamic(mregression_test/0).

:- fixup_exports.

%:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

arc:- forall(arc11,true).
arc1:- clsmake, test_names_by_hard(X), whole_test(X).
arc2:- clsmake, test_names_by_hard_rev(X), whole_test(X).
arc11:- clsmake, test_names_by_hard(X), arc1(X).
arc22:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- clsmake, arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav3:- clsmake, arc1(t('3631a71a')>(_+_)),!.
fav:- clsmake,forall(fav11,true).
favr:- clsmake,forall(fav22,true).
fav1:- clsmake, test_names_by_hard_rev(X), whole_test(X).
fav2:- clsmake, test_names_by_fav_rev(X), whole_test(X).
fav11:- clsmake, test_names_by_fav(X), arc1(X).
fav22:- clsmake, test_names_by_fav_rev(X), arc1(X).
favL:- clsmake, get_current_test(X),!,whole_test(X).
favC:- clsmake, set_current_test(Y), UT=until_test(Y),!,
  test_names_by_hard(X),until_test(X)=UT,nb_setarg(1,UT,_),whole_test(X).

whole_test(X):- cls1, with_tty_raw(interactive_test(X)).
%whole_test(X):- cls1, noninteractive_test(X).

fav(X):- nonvar(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- time(forall(arc1(true,TestID),true)).

arc1(TName):- arc1(true,TName).
%arc1(G,TName):- arc2(G,TName,(_+0)).


arc1(G,TName):- 
 fix_test_name(TName,TestID,_UExampleNum), 
 locally(set_prolog_flag(gc,true),
  (clear_shape_lib(TestID),
   nb_delete('$training_vm'),
   % choice point created here purposely
  forall(kaggle_arc(TestID,ExampleNum,_In,_Out),
  ignore((catch((call(G),
    run_arc_io(TestID,ExampleNum)),'$aborted',true)))))).


is_detatched_thread:- arc_webui,!.
is_detatched_thread:- \+ (thread_self(Main) -> Main == main ; main==0),!.

cls_z:- is_detatched_thread,!,flush_tee.
cls_z:- tracing,!.
cls_z:- catch(cls,_,true), flush_tee, nop((clear_tee,clear_test_html)).
cls1:- nop(catch(cls_z,_,true)).

list_to_rbtree_safe(I,O):- must_be_free(O), list_to_rbtree(I,M),!,M=O.
:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)>(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')>(tst+0),"segv").
%is_buggy_pair(t('27a28665')>(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum):- Pair = (TestID>ExampleNum), is_buggy_pair(Pair,Why),!,format("~N1 % Skipping ~q because: ~w ~n~n",[Pair,Why]).
run_arc_io(TestID,ExampleNum):- 
  time(train_test(TestID)),
  time(solve_test(TestID,ExampleNum)).

get_training(Training):- luser_getval('$training_vm',Training),compound(Training),!.
get_training(Tree):- list_to_rbtree([p-q],T),!,ignore(Tree=T),!.
get_training(Training):- must_det_ll(((
  get_current_test(TestID), make_training(TestID,Training), !,
  luser_linkval('$training_vm',Training)))),!.
set_training(Training):- luser_linkval('$training_vm',Training).
set_training(Prop,Value):- get_training(Training), gset(Training.Prop)=Value.
get_training(Prop,Value):- get_training(Training), get_kov(Prop,Training,Value).
set_vm(VM):- luser_linkval('$grid_vm',VM).
get_vm(VM):- luser_getval('$grid_vm',VM),!.
get_vm(VM):- ndividuator,!,luser_getval('$grid_vm',VM),!.

peek_vm(VM):- luser_getval('$grid_vm',VM),!.
peek_vm(Key,Value):- luser_getval('$grid_vm',VM)->get_kov(Key,VM,Value);luser_getval(Key,Value).

set_vm(Prop,Value):- ignore(luser_getval('$grid_vm',VM)),
 luser_set_vm(VM,Prop,Value).

luser_set_vm(VM,Prop,Value):- var(VM), !, print_grid(luser_set_vm(Prop),Value).
luser_set_vm(VM,Prop,Value):-
 (get_kov1(Prop,VM,_) -> gset(VM.Prop) = Value ; 
  (get_kov1(props,VM,Hashmap) -> 
    (var(Hashmap)->(list_to_rbtree([Prop-Value],Hashmap),gset(VM.props)=Hashmap); must_not_error( gset(Hashmap.Prop)=Value));
      (list_to_rbtree([Prop-Value],Hashmap),gset(VM.props)=Hashmap))).

set_vm_obj(Prop,Or,Value):- set_vm(Prop,Value),ignore(set_vm_obj1(Prop,Or,Value)),!.

set_vm_obj1(Prop,Or,Value):- Value==[],!, set_vm_obj_nil(Prop,Or).
set_vm_obj1(Prop,Or,Value):- is_grid(Value),!,
  localpoints_include_bg(Value,IndvPoints),
  grid_size(Value,H,V),
  if_t(IndvPoints\==[],
    (get_vm(VM),
          make_indiv_object(VM,[iz(Prop),vis2D(H,V),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          make_bg_visible(Value,VValue),
          print_grid(H,V,Prop,VValue))),!.

set_vm_obj1(Prop,Or,IndvPoints):- is_points_list(IndvPoints),!,
  if_t(IndvPoints\==[],
    (get_vm(VM),          
      make_indiv_object(VM,[iz(Prop),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
      %addObjects(VM,Obj),
      print_grid(VM.h,VM.v,Prop,IndvPoints))),!.


set_vm_obj1(Prop,Or,Value):- is_object(Value),!,
  get_vm(VM),
  remObjects(VM,Value),
  override_object([iz(Prop),birth(set_vm(Prop))|Or],Value,NewObj),
  addObjects(VM,NewObj),
  object_grid(NewObj,Grid),
  print_grid(Prop,Grid),!.

set_vm_obj_nil(Prop,Or):- wdmsg(set_vm_obj_nil(Prop,Or)).
/*
 get_vm(VM), Value==[], H = VM.h, V = VM.v,

  %points_to_grid(H,V,Points,Grid),
  %Grid=[[wbg]],

  make_indiv_object(VM,[iz(Prop),vis2D(1,1),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          make_bg_visible(Value,VValue),
          print_grid(H,V,Prop,VValue))),
  % set_vm_obj(Prop,Or,Grid),

*/


get_vm(Key,Value):-  get_vm(VM), get_kov(Key,VM,Value).
  
get_kov(K,O,V):- get_kov1(K,O,V),!.
get_kov(K,O,V):- get_kov1(props,O,VV),!,get_kov1(K,VV,V).
% (get_kov(Prop,VM,Value) -> true ; (get_kov(props,VM,Hashmap),nonvar(Hashmap),must_not_error(nb_get_value(Hashmap,Prop,ValueOOV)),get_oov_value(ValueOOV,Value))).
get_kov1(K,O,V):- is_dict(O),!,get_dict(K,O,OOV),get_oov_value(OOV,V).

get_kov1(K,O,V):- nonvar(K),is_rbtree(O),!,rb_lookup(K,V,O).
get_kov1(K,O,V):- is_rbtree(O),!,rb_in(K,V,OOV),get_oov_value(OOV,V).
%get_kov(K,O,V):- is_rbtree(O),!,nb_rb_get_node(K,O,Node),nb_rb_node_value(Node,V).

get_oov_value(ValueOOV,Value):- compound(ValueOOV),ValueOOV=oov(Value),!.
get_oov_value(Value,Value).

:- ensure_loaded(kaggle_arc_two).
test_regressions:- make, forall((clause(mregression_test,Body),ppt(Body)),must_det_ll(Body)).
:- arc_history1(test_regressions).

:- dynamic(muarc_2_mods/2).
:- strip_module(_,M,_), prolog_load_context(module,MM), retractall(muarc_2_mods(_,_)), asserta(muarc_2_mods(M,MM)).

%:- forall(ping_indiv_grid(X),atom_concat(X,Y
:- include(kaggle_arc_footer).
%:- initialization(demo,program).
%:- initialization(demo,restore_state).
%:- initialization(demo,main).
%:- initialization(demo,after_load).
:- muarc_mod(M), arc_history1((module(M))).

%:- muarc_mod(M), M:show_tests.
:- load_last_test_name.

%:- muarc_mod(M), M:listing((addOptions)/2).
%:- xlisting((.)/3).
%:- xlisting(user:'.'(_, _, _)).

:- ensure_loaded(kaggle_arc_simple).

:- dynamic(saved_training/1).
saved_training(TestID):- call_u('~'(saved_training(TestID))), !, fail. % explictly always assume unsaved?
saved_training(TestID):- test_name_output_file(TestID,File),exists_file(File).



:- set_prolog_flag(arc_term_expansion, true).

%:- ensure_loaded('kaggle_arc_fwd.pfc').

%:- set_prolog_flag(arc_term_expansion, false).

%:- if(prolog_load_context(reload,false)).
%:- fixup_module_exports_into_from(system,muarc).
%:- endif.

%:- fixup_module_exports_now.  
user:portray(Grid):- fail, 
   current_prolog_flag(debug,false),
    \+ tracing,
   \+ nb_current(arc_can_portray,nil),
   current_predicate(bfly_startup/0), \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!, flush_output.


%:- ignore(check_dot_spacing).

   
:- add_history((print_test)).
:- add_history((webui_tests)).
:- add_history((bfly_test(a1))).
:- add_history((bfly_tests)).
:- add_history((test_pp)).
:- add_history((bfly_startup)).
%:- add_history1((cls_z,make,demo)).
:- add_history1((demo)).


:- nb_setval(arc_can_portray,nil).
:- nb_setval(arc_can_portray,t).
:- nb_setval(arc_can_expand_query,nil).
:- nb_setval(arc_can_expand_query,t).
%:- \+ nb_current(arc_can_portray,nil).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).

:- catch_log(set_stream(current_output,encoding(utf8))).

:- (current_prolog_flag(load_arc_webui,true)->catch_log(logicmoo_webui) ; true).
:- current_prolog_flag(load_arc_webui,true) -> catch_log(start_arc_server) ; true.

:- catch_log(set_long_message_server('https://logicmoo.org:17771')).

bfly_startup:-
   set_toplevel_pp(bfly),
   asserta(was_inline_to_bfly),inline_to_bfly_html,
   bfly,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!,
   ansi.


ansi_startup:- 
   ansi,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!.

:- luser_default(example,trn+0).
:- luser_default(no_diags,false).
:- luser_default(no_individuator, f).
:- luser_default(grid_size_only,true).
:- luser_default(extreme_caching,true).
:- luser_default(cmd,test_easy_solve_by).
:- luser_default(cmd2,print_all_info_for_test).
:- luser_default(individuated_cache,true).
:- luser_default(extreme_caching,true).
:- scan_uses_test_id.
:- store_grid_size_predictions.
:- make_grid_cache.
:- gen_gids.
:- test_show_colors.
:- fmt('% Type ?- demo. % or press up arrow').

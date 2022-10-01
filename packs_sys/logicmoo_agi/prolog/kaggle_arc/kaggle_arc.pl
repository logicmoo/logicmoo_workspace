/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- encoding(iso_latin_1).

:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(stream_type_check,false).
:- current_prolog_flag(argv,C),(member('--',C)->set_prolog_flag(load_arc_webui,true);true).

:- dynamic('$messages':to_list/2).
:- multifile('$messages':to_list/2).
:- asserta(('$messages':to_list(In, List) :- ((is_list(In)-> List = In ; List = [In])),!)).

catch_log(G):- format('~N'),ignore(catch(notrace(G),E,wdmsg(E=G))),format('~N').

%:- pack_install('https://github.com/logicmoo/logicmoo_utils.git').
%:- pack_upgrade(logicmoo_utils).
% :- pack_install(dictoo).
% :- pack_upgrade(dictoo).

%:- module(system).

:- set_prolog_flag(arc_term_expansion, false).
:- dynamic((ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(cmemo/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).

:- multifile(fav/2).
:- discontiguous(fav/2).
:- dynamic(fav/2).
:- export(fav/2).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).

:- dynamic(kaggle_arc/4).
:- discontiguous(kaggle_arc/4).
:- multifile(kaggle_arc/4).
:- export(kaggle_arc/4).

:- dynamic(arc_test_property/3).
:- discontiguous(arc_test_property/3).
:- multifile(arc_test_property/3).

:- dynamic(individuation_macros/2).
:- discontiguous(individuation_macros/2).
:- multifile(individuation_macros/2).


arc_history(_).
arc_history1(_).
% :- dynamic(grid_hint_pred/1). :- discontiguous(grid_hint_pred/1). :- multifile(grid_hint_pred/1).


my_is_clause(H,B):- clause(H,B,Ref),clause(HH,BB,Ref), H+B=@=HH+BB,!.
my_asserta_if_new((H:-B)):- !, (my_is_clause(H,B) -> nop(wdmsg(my_is_clause(H,B))) ; arc_assert(H:-B)).
my_asserta_if_new(HB):- my_asserta_if_new(HB:-true).

my_assertz_if_new((H:-B)):- !, (my_is_clause(H,B) -> true ; assertz(H:-B)).
my_assertz_if_new(HB):- my_assertz_if_new(HB:-true).

:- multifile(decl_sf/1).
:- discontiguous(decl_sf/1).
:- dynamic(decl_sf/1).
decl_sf(G):- ground(G), !, my_assertz_if_new(decl_sf(G)).
:- multifile(decl_pt/2).
:- discontiguous(decl_pt/2).
:- dynamic(decl_pt/2).
:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).
decl_pt(G):- ground(G), !, my_assertz_if_new(decl_pt(plain,G)).
decl_pt(How,G):- nonvar(How),ground(G), !, my_assertz_if_new(decl_pt(How,G)).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).
:- multifile is_fti_step/1.
:- multifile is_fti_stepr/1.
:- discontiguous is_fti_step/1.
:- discontiguous is_fti_stepr/1.


:- discontiguous is_changeable_param/1.
:- multifile is_changeable_param/1.
:- dynamic is_changeable_param/1.

:- discontiguous ping_indiv_grid/1.
:- multifile ping_indiv_grid/1.

:- meta_predicate(fif(0,0)).
fif(IF, THEN) :- (   call(IF) ->  call(THEN) ;   true ).
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

clsmake:- is_detatched_thread,!.
clsmake:- notrace((cls_z,!,update_changed_files,make)),!.

%arc_assert(P):- pfcAddF(P).

:- else.  % SWISH ARC
:- catch(noguitracer,_,true).

clsmake:- update_changed_files,!.

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

:- system:use_module(library(quasi_quotations)).
:- system:use_module(library(hashtable)).
:- system:use_module(library(gensym)).
:- system:use_module(library(sort)).
:- system:use_module(library(writef)).
:- system:use_module(library(rbtrees)).
:- system:use_module(library(dicts)).
:- system:use_module(library(edinburgh)).
%:- system:use_module(library(lists)).
:- system:use_module(library(statistics)).
:- system:use_module(library(nb_set)).
:- system:use_module(library(assoc)).
:- system:use_module(library(pairs)).
:- system:use_module(library(logicmoo_common)).
:- system:use_module(library(prolog_trace)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(prolog_source)).
 %library(trace/clause) 
%:- autoload_all.
:- system:use_module(library(gvar_globals_api)).
:- system:use_module(library(dictoo_lib)).

:- current_prolog_flag(argv,C),wdmsg(current_prolog_flag(argv,C)),!.

:- set_prolog_flag(no_sandbox,true).


:- if(current_prolog_flag(load_arc_webui,true)).
with_webui(Goal):- ignore(call(Goal)),!.
:- endif.
with_webui(Goal):- ignore(when_arc_webui(with_http(Goal))).
%:- initialization arc_http_server.

logicmoo_webui:-
  system:use_module(library(xlisting/xlisting_web)),
  system:use_module(library(xlisting/xlisting_web_server)),
   exists_source(library(logicmoo_webui)), use_module(library(logicmoo_webui)), 
   catch_log(dmsg((?-webui_start_swish_and_clio))),
   nop(catch_log(call(call,webui_start_swish_and_clio))).
logicmoo_webui.

:- (current_prolog_flag(load_arc_webui,true)->catch_log(logicmoo_webui) ; true).


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
must_det_ll(X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
must_det_ll(must_det_ll(X)):- !, must_det_ll(X).
must_det_ll((X,Y,Z)):- !, (must_det_ll(X)->must_det_ll(Y)->must_det_ll(Z)).
must_det_ll((X,Y)):- !, (must_det_ll(X)->must_det_ll(Y)).
must_det_ll(fif(X,Y)):- !, fif(must_not_error(X),must_det_ll(Y)).
must_det_ll((A->X;Y)):- !,(must_not_error(A)->must_det_ll(X);must_det_ll(Y)).
must_det_ll((A*->X;Y)):- !,(must_not_error(A)*->must_det_ll(X);must_det_ll(Y)).
must_det_ll((X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;must_det_ll_failed(X;Y)).
must_det_ll(\+ (X)):- !, (\+ must_not_error(X) -> true ; must_det_ll_failed(\+ X)).
%must_det_ll((M:Y)):- nonvar(M), !, M:must_det_ll(Y).
must_det_ll(X):- tracing,!,must_not_error(X).
must_det_ll(once(A)):- !, once(must_det_ll(A)).
must_det_ll(X):- 
  strip_module(X,M,P),functor(P,F,A),setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(X)*->true;must_det_ll_failed(X)),
    nop(trace(M:F/A,-fail))).
  
must_not_error(X):- catch(X,E,((E=='$aborted';nb_current(cant_rrtrace,t))-> throw(E);(/*arcST,*/writeq(E=X),pp(rrtrace=X),rrtrace(X)))).

must_det_ll_failed(X):- notrace,wdmsg(failed(X))/*,arcST*/,nortrace,trace,rrtrace(X),!.
% must_det_ll(X):- must_det_ll(X),!.

rrtrace(X):- nb_current(cant_rrtrace,t),!,nop((wdmsg(cant_rrtrace(X)))),!,fail.
rrtrace(X):- !, rtrace(X).
rrtrace(X):- notrace,nortrace, arcST, sleep(0.5), trace, (notrace(\+ current_prolog_flag(gui_tracer,true)) -> rtrace(X); (trace,call(X))).

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

arc_user(main):-!.
%arc_user(ID):- thread_self(TID),arc_user(TID, ID).

suggest_arc_user(ID):- catch((if_arc_webui(xlisting_web:find_http_session(ID))),_,fail),!.
suggest_arc_user(ID):- catch((pengine:pengine_user(ID)),_,fail),!.
suggest_arc_user(ID):- catch((http_session:session_data(_,username(ID))),_,fail),!.

arc_webui:-  notrace(arc_webui0).
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

luser_defval(N,V):- luser_setval(global,N,V).

luser_linkval(N,V):- arc_user(ID),luser_linkval(ID,N,V),!.
luser_linkval(ID,N,V):- nb_linkval(N,V),retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).

:- meta_predicate(if_arc_webui(-)).
if_arc_webui(Goal):- arc_webui,!,g_out(call(Goal)).
if_arc_webui(_):- \+ arc_webui,!,fail.

:- meta_predicate(when_arc_webui(-)).
when_arc_webui(G):- toplevel_pp(http),call(G),!.
when_arc_webui(G):- toplevel_pp(swish),call(G),!.
when_arc_webui(G):- ignore(if_arc_webui(G)).


luser_getval(N,V):- if_arc_webui(((get_param_req_or_session(N,V), V\=='',V\==""))).
luser_getval(N,V):- arc_user(ID),luser_getval(ID,N,V),!.
%luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.
%luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).
luser_getval(ID,N,V):- !,
 (nb_current(N,Val)*->Val=V;
  (arc_user_prop(ID,N,V)*->true;arc_user_prop(global,N,V))).

/*
luser_getval(ID,N,V):- 
 (arc_user_prop(ID,N,V)*->true;
  (nb_current(N,V))*->true;arc_user_prop(global,N,V)).
*/
:- luser_defval(example,trn+0).

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

:- enable_arc_expansion.

%:- set_prolog_flag(verbose_load,true).  
%:- set_prolog_flag(verbose_autoload,true).

%:- learn_shapes.
:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_interpreter).
:- ensure_loaded(kaggle_arc_test_loader).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_physics).
:- ensure_loaded(kaggle_arc_db).
:- ensure_loaded(kaggle_arc_heuristics).
:- ensure_loaded(kaggle_arc_intruder).
:- ensure_loaded(kaggle_arc_test_cache).
:- ensure_loaded(kaggle_arc_individuation).

:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_boards).
:- ensure_loaded(kaggle_arc_learning).
:- ensure_loaded(kaggle_arc_imagens).
:- ensure_loaded(kaggle_arc_recognise).
:- ensure_loaded(kaggle_arc_uniqueness).
:- ensure_loaded(kaggle_arc_ui_html).
:- ensure_loaded(kaggle_arc_test_easy).
:- ensure_loaded(kaggle_arc_test_old).
:- set_prolog_flag(verbose_load,false).
:- set_prolog_flag(verbose_autoload,false).


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
 set_current_test(TName),
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

cls_z:- is_detatched_thread,!.
cls_z:- catch(cls,_,true).
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

set_vm_obj1(Prop,Or,Value):- is_grid(Value),!,
  localpoints_include_bg(Value,IndvPoints),
  grid_size(Value,H,V),
  fif(IndvPoints\==[],
    (get_vm(VM),
          make_indiv_object(VM,[iz(Prop),v_hv(H,V),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          print_grid(H,V,Prop,Value))),!.

set_vm_obj1(Prop,Or,IndvPoints):- is_points_list(IndvPoints),!,
  fif(IndvPoints\==[],
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

make_training(TestID,VMO):- 
 %make_fti(_GH,_GV,TestID,_Grid,_Sofar,_Reserved,_Options,_Points,ArgVM),
 must_det_ll((
    WAZ = _{
      %program:[],
      %pairs:_, %datatree:_, 
      %current:_,
      test_id:TestID},
    make_training_hints(TestID,WAZ,VMO))).
    /*
     test:ID,mappings:_,
     pre_in:_, pre_out:_,
     inC:_InC,outC:_OutC,
     removed:_,added:_, kept:_,   
     grid_in:_,grid_target:_,
   set(VM.mappings) =[map])), !. % pp(VM),nl.
  */


  

%show_arc_pair_progress(TestID,ExampleNum,In,Out):- show_arc_pair_progress_sol(TestID,ExampleNum,In,Out),!.
train_test:- notrace(get_current_test(TestID)), once(train_test(TestID)).
train_test(TestID):- clear_training(TestID),
  compile_and_save_test(TestID),
  train_test(TestID,train_using_oo_ii_io).
train_test(TestID,P2):-   
  print_testinfo(TestID),
  flag(indiv,_,0),
  %get_training(PrevPairEnv),
  %luser_setval(prev_pairEnv,PrevPairEnv),
  %nb_delete('$training_vm'),
  %get_training(Training),
  %my_time(make_training_hints(TestID,Training,_HIDE_Dictation)),
  %Dictation = Training,
  %set_training(Dictation),
  rb_new(Dictation),
  my_time(call(P2,TestID,Dictation,DictOut)),
  set_training(DictOut),!.

my_time(Goal):- !,call(Goal).
my_time(Goal):- statistics:time(Goal).

train_using_oo_ii_io(TestID,DictIn,DictOut):- 
  train_using_oo_ii_io(TestID,trn,0,DictIn,DictOut).

train_using_oo_ii_io(TestID,Trn,N1,DictIn,DictOut):-
 (kaggle_arc(TestID,(Trn+N1),In1,Out1), N2 is N1 + 1),

 (kaggle_arc(TestID,(Trn+N2),_In2,Out2)
   -> 
    (train_for_objects_from_pair_with_mono(DictIn,TestID,[Trn,'o',N1,'o',N2],Out1,Out2,Dict1),
     %nop((train_for_objects_from_pair_with_mono(Dict0,TestID,[Trn,'i',N1,'i',N2],In1,In2,Dict1))),
     train_using_oo_ii_io(TestID,Trn,N2,Dict1,DictM))
     ; (DictM = DictIn)),
  !,
  train_for_objects_from_pair_with_mono(DictM,TestID,[Trn,'i',N1,'o',N1],In1,Out1,DictOut),!.

train_using_oo_ii_io(_TestID,_Trn,_N1,DictInOut,DictInOut).

train_only_from_pairs:- notrace(get_current_test(TestID)), train_only_from_pairs(TestID).

train_only_from_pairs(TestID):- clear_training(TestID), train_test(TestID,train_using_io).

train_using_io(TestID,DictIn,DictOut):- train_using_io(TestID,trn,0,DictIn,DictOut).
train_using_io(TestID,Trn,N1,DictIn,DictOut):- 
  kaggle_arc(TestID,(Trn+N1),In,Out),!,
  detect_pair_hints(TestID,(Trn+N1),In,Out),
  train_for_objects_from_1pair(DictIn,TestID,[Trn,'i',N1,'o',N1],In,Out,DictMid),
  N2 is N1 + 1,
  train_using_io(TestID,Trn,N2,DictMid,DictOut).
train_using_io(_TestID,_Trn,_,DictInOut,DictInOut).

%:- thread_local(keep_going/0).

which_io0(i,in). which_io0(o,out).
which_io(I,In):- which_io0(I,In),!.
which_io(In,In):- which_io0(_,In),!.

train_for_objects_from_pair_with_mono(Dict0,TestID,Desc,In,Out,Dict9):-
 must_det_ll((
  into_monochrome(In,MonoIn0), into_monochrome(Out,MonoOut0),
  copy_term(MonoIn0,MonoIn),copy_term(MonoOut0,MonoOut),
 Desc = [_Trn,IsIO1,N1,IsIO2,N2], 
 MonoDesc = ['train_mono',IsIO1,N1,IsIO2,N2], 
  train_for_objects_from_1pair(Dict0,TestID,Desc,In,Out,Dict1),!,
  nop(train_for_objects_from_1pair(Dict1,TestID,MonoDesc,MonoIn,MonoOut,Dict9)),!,
   ignore(Dict1=Dict9))),!.

train_for_objects_from_1pair(Dict0,TestID,Desc,InA,OutA,Dict1):-
  locally(set_prolog_flag(gc,true),train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1)).

train_for_objects_from_1pair1(Dict0,_TestID,Desc,_InA,_OutA,Dict0):- Desc = [_Trn,'o',_N1,'o',_N2], !.

train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1):-
 collapsible_section(debug,train_for_objects_from_1pair1,true,
(maplist(must_det_ll,[
 Desc = [Trn,IsIO1,N1,IsIO2,N2], 
 which_io(IsIO1,IO1),
 which_io(IsIO2,IO2),
 atomic_list_concat([IO1,IO2],'_',ModeIn),
 atomic_list_concat([IO2,IO1],'_',ModeOut),
 atom_concat(IO1,N1,ION1),
 atom_concat(IO2,N2,ION2),
 atomic_list_concat([ION1,ION2],'_',ExampleNum),
 pp([train_for_objects_from_1pair1=ExampleNum,left=ION1,right=ION2]),
 garbage_collect,
  Dict0=Dict1,
   format('~N dict= '), pp(Dict0),

   %get_map_pairs(Dict0,_Type,Pairs),
   %list_to_rbtree_safe(Pairs,InVM),
   into_grid(InA,In), into_grid(OutA,Out),!,
   name_the_pair(TestID,ExampleNum,In,Out,PairName),
 	 grid_size(In,IH,IV), grid_size(Out,OH,OV),
	 ignore((IH+IV \== OH+OV , writeln(io(size(IH,IV)->size(OH,OV))))),
   
   into_fti(TestID>(Trn+N1)*IO1,ModeIn,In,InVM),!,
   into_fti(TestID>(Trn+N2)*IO2,ModeOut,Out,OutVM)]),!,

   %InVM.compare=OutVM, 
   set(InVM.grid_target)=Out,
   %OutVM.compare=InVM, 
   set(OutVM.grid_target)=In,
  maplist(must_det_ll,[
   show_pair_grid(yellow,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,  
  individuate_c(InVM),!,
  individuate_c(OutVM)]),!,

  InC = InVM.objs,
  OutC = OutVM.objs,
  %print_info(InC),
  %print_info(OutC),
  %wdmsg(InC=OutC),
  maplist(must_det_ll,[
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  /*add_shape_lib(pair,RetainedIn),
  % add_shape_lib(pair,RetainedOut),
  add_shape_lib(removed(PairName),Removed),
  add_shape_lib(added(PairName),Added),*/
  
  dash_chars,dash_chars,dash_chars,dash_chars,
  show_pair_grid(cyan,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,
  max_min(IH,OH,IOH,_), max_min(IV,OV,IOV,_),
  luser_setval(no_rdot,true),
  ((Removed==Added, Removed==[]) -> pp(yellow,nothing_removed_added(PairName)) ;
    show_pair_diff_code(IOH,IOV,IOH,IOV,removed(PairName),added(PairName),PairName,Removed,Added)),
  ((RetainedIn==RetainedOut, RetainedIn==[]) -> pp(yellow,nothing_retained(PairName)) ;
    show_pair_diff_code(IH,IV,   OH, OV,retained(ION1),retained(ION2),PairName,RetainedIn,RetainedOut)),
  ((InC==OutC, InC==[]) -> pp(yellow,nothing_individuated(PairName)) ;
    show_pair_diff_code(IH,IV,   OH, OV,individuated1(ION1),individuated1(ION2),PairName,InC,OutC)),!, 
  luser_setval(no_rdot,false),
   % pp(OutC=InC),

   ignore(( learn_rule_o(ModeIn,InVM,OutVM))),

   ignore(( ModeIn == in_out, Trn == trn,  
            train_io_from_hint(TestID,Trn+N1,InVM))),

  dash_chars,dash_chars,dash_chars,dash_chars,
  print_testinfo(TestID)]))).

show_pair_diff_code(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  dash_chars,dash_chars,
  nop(show_pair_code(In,Out)),!.

show_pair_code(In,Out):- 
  pp(purple,show_objs_as_code),
  dash_chars,
  show_objs_as_code(In),
  dash_chars,
  show_objs_as_code(Out),
  dash_chars,dash_chars.

print_testinfo(TestID):-
  ignore(((test_info(TestID,F),forall(member(I,F),pp(test_info=I))))).

% trials(learn). trials(clue).   
trials(human). trials(sol).
trials(dsl). trials(runDSL).
trial_non_human(sol).

sols_for(TestID,Trial,TrialSol):- trials(Trial),once((compound_name_arguments(Entry,Trial,[Sol]), test_info(TestID,Sols),member(Entry,Sols))),
  append_trial(Trial,Sol,TrialSol).

append_trial(Trial,Sol,TrialSol):- listify(Sol,SolL),
  ((appended_trial(Trial,TrialAppend), \+ append(_,TrialAppend,SolL)) -> append(SolL,TrialAppend,TrialSol) ;
    TrialSol = SolL).

appended_trial(human,[learn_rule]).



solve_test:- forall(trial_non_human(Trial),solve_test_trial(Trial)).

solve_test_trial(Trial):- mmake,
 my_menu_call((get_current_test(TestID), catch(solve_test_trial(Trial,TestID,(tst+_)),E,wdmsg(E=solve_test_trial(Trial,TestID,(tst+_)))))),!.

solve_test_training_too:- 
 solve_test,
 my_menu_call((get_current_test(TestID), catch(solve_test_trial(Trial,TestID,(trn+A)),E,wdmsg(E=solve_test_trial(Trial,TestID,(trn+A)))))),!.


solve_test(Name):- forall(trial_non_human(Trial),solve_test_trial(Trial,Name)).

solve_test_trial(Trial,Name):- 
  fix_test_name(Name,TestID,ExampleNum),!, 
  solve_test_trial(Trial,TestID,ExampleNum).

solve_test(TestID,ExampleNum):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum)).

solve_test_trial(Trial,TestID,ExampleNum):-
 forall(kaggle_arc(TestID,ExampleNum,TestIn,ExpectedOut),
   ignore(solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut))).

solve_test(TestID,ExampleNum,TestIn,ExpectedOut):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut)).

  
solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut):-
   must_det_ll((    
    name_the_pair(TestID,ExampleNum,TestIn,ExpectedOut,PairName))),
   must_det_ll((       
    grid_size(TestIn,IH,IV), grid_size(ExpectedOut,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(io(size(IH,IV)->size(OH,OV))))),
    print_testinfo(TestID))), 
   must_det_ll((
   try_easy_io(TestID>ExampleNum,TestIn,ExpectedOut),
    dash_chars, dash_chars,
    show_pair_grid(green,IH,IV,OH,OV,'Test TestIn','Solution ExpectedOut (Not computed by us)',PairName,TestIn,ExpectedOut),!,  
    get_training(Training))),
    flag(indiv,_,0),    
    into_fti(TestID>ExampleNum*in,in,TestIn,InVM),!,
    set(InVM.objs) = [],
    %set(InVM.points) = [],
    %set(InVM.training) = Training,
    set_training(Training),
    maybe_set_vm(InVM),    
    gset(InVM.grid_target) = _,
    must_det_ll((
    %print(training(Training)),nl,
    %ppt(InVM),
    dash_chars, dash_chars,    
    %print_testinfo(TestID),
    do_sols_for(Trial,"Taking Test",InVM,TestID,ExampleNum))).

    % find indiviuation one each side that creates the equal number of changes
    
do_sols_for(Trial,Why,InVM,TestID,ExampleNum) :-
 must_det_ll(( ppt("BEGIN!!!"+Why+TestID>ExampleNum), 
    kaggle_arc_io(TestID,ExampleNum,out,ExpectedOut),
    forall(sols_for(TestID,Trial,SolutionProgram),
     ignore(((
      once((pp(cyan,trial=Trial),
       ppt(cyan,run_dsl(TestID>ExampleNum,Trial,SolutionProgram)),!,
       (time((
              maybe_set_vm(InVM),
              kaggle_arc_io(TestID,ExampleNum,in,TestIn),
              gset(InVM.grid) = TestIn,
              maybe_set_vm(InVM),
              run_dsl(InVM,SolutionProgram,InVM,GridOut)))*->!;GridOut=InVM.grid),
       into_pipe(GridOut,Solution)))
       *->    
       ignore((count_difs(ExpectedOut,Solution,Errors),
        print_side_by_side(blue,Solution,"Our Ran Solution",_,ExpectedOut,"Expected Solution"),
           (Errors==0 -> 
              (banner_lines(green),arcdbg(pass(Why,TestID,ExampleNum,SolutionProgram)),banner_lines(green))
              ; (banner_lines(red), arcdbg(fail(Why,Errors,TestID,ExampleNum,SolutionProgram)),
               test_info(TestID,InfoF),wqnl(fav(TestID>ExampleNum,InfoF)),
               banner_lines(red)))))
       ;arcdbg(warn(unrunable(TestID,ExampleNum,SolutionProgram))))))),
    print_grid("our grid", InVM.grid),!,
    print_list_of("our objs",InVM.objs),
    ppt("END!!!"+Why+TestID+ExampleNum))),!.
   


:- luser_linkval(test_rules,[rules]).
:- luser_linkval(pair_rules,[rules]).
  

reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  my_append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  obj_to_oid(B,BOID),
  obj_to_oid(A,_AOID),
  setq(A,oid(BOID),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pp(shared_object(GlyphB->GlyphA))); 
    (pp(same_object(GlyphA,GlyphB,How))))).

test_regressions:- make, forall((clause(mregression_test,Body),ppt(Body)),must_det_ll(Body)).
:- arc_history1(test_regressions).

:- dynamic(muarc_2_mods/2).
:- strip_module(_,M,_), prolog_load_context(module,MM), retractall(muarc_2_mods(_,_)), asserta(muarc_2_mods(M,MM)).

%:- forall(ping_indiv_grid(X),atom_concat(X,Y
:- fixup_exports.
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
user:portray(Grid):-
   \+ nb_current(arc_can_portray,nil),
   current_predicate(bfly_startup/0), \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!.


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
%:- \+ nb_current(arc_can_portray,nil).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).

:- set_stream(current_output,encoding(utf8)).

:- current_prolog_flag(load_arc_webui,true) -> catch_log(start_arc_server) ; true.

bfly_startup:-    
   asserta(was_inline_to_bfly),inline_to_bfly_html,
 %  bfly,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!.


ansi_startup:- 
   ansi,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!.




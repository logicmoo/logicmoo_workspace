/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- if((prolog_load_context(module, M),assert(tmp:loading_arc_from(M)))).
:- module(kaggle_arc,[]).
:- '$set_source_module'(user).
:- endif.
:- multifile(swi_option:option/2).
:- dynamic(swi_option:option/2).
:- use_module(library(option)).

:- set_prolog_flag(stream_type_check,false).
:- set_prolog_flag(never_pp_hook, true).
%:- set_prolog_flag(xpce,false).
%:- set_prolog_flag(pce,false).f
%:- set_prolog_flag(windows,false).
%:- set_prolog_flag(gui_tracer,false).

%:- current_prolog_flag(argv,C),(member('--',C)->set_prolog_flag(use_arc_swish,true);true).
:- set_prolog_flag(arc_term_expansion,false).
:- multifile prolog_edit:load/0.
:- dynamic prolog_edit:load/0.

:- dynamic('$messages':to_list/2).
:- multifile('$messages':to_list/2).
:- asserta(('$messages':to_list(In, List) :- ((is_list(In)-> List = In ; List = [In])),!)).
%my_time(Goal):- !,call(Goal).

:- use_module(library(statistics)).
:- import(prolog_statistics:time/1).
my_time(Goal):- time(Goal),flush_tee.
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).
catch_nolog(G):- ignore(catch(notrace(G),E,once(true;nop(u_dmsg(E=G))))).
catch_log(G):- ignore(catch(notrace(G),E,((writeln(E=G),catch_nolog(ds))))).

get_user_error(UE):- stream_property(UE,file_no(2)),!.
get_user_error(UE):- stream_property(UE,alias(user_error)),!.

ufmt(G):- fmt(G)->true;writeln(G).
u_dmsg(G):- is_list(G),!,my_maplist(u_dmsg,G).
u_dmsg(M):- get_user_error(UE), \+ current_predicate(with_toplevel_pp/2),!, with_output_to(UE,ufmt(M)).
u_dmsg(M):- get_user_error(UE),!, with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))).
u_dmsg(M):- get_user_error(UE),  stream_property(UO,file_no(1)), current_output(CO),!,
  (UO==CO ->  dmsg(M) ; 
   (with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))), with_output_to(CO,pp(M)))).
u_dmsg(G):-ufmt(G),!.

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
my_asserta_if_new((H:-B)):- !, (my_is_clause(H,B) -> nop(u_dmsg(my_is_clause(H,B))) ; arc_assert(H:-B)).
my_asserta_if_new(HB):- my_asserta_if_new(HB:-true).

my_assertz_if_new((H:-B)):- !, (my_is_clause(H,B) -> true ; assertz(H:-B)).
my_assertz_if_new(HB):- my_assertz_if_new(HB:-true).

%:- multifile(decl_sf/1).
%:- discontiguous(decl_sf/1).
%:- dynamic(decl_sf/1).
:- dynamic(is_decl_sf/1).
decl_sf(G):- must_det_ll((nonvar(G), !, my_assertz_if_new(is_decl_sf(G)))).

:- dynamic(is_decl_gf/1).
decl_gf(G):- must_det_ll((nonvar(G), !, my_assertz_if_new(is_decl_gf(G)))).
%:- multifile(decl_pt/2).
%:- discontiguous(decl_pt/2).
%:- dynamic(decl_pt/2).
%:- multifile(decl_pt/1).
%:- discontiguous(decl_pt/1).
%:- dynamic(decl_pt/1).
:- dynamic(is_decl_pt/2).
:- discontiguous(is_decl_pt/2).
:- multifile(is_decl_pt/2).

decl_pt(G):- must_det_ll((nonvar(G), !, my_assertz_if_new(is_decl_pt(plain,G)))),!.
decl_pt(How,G):- must_det_ll((nonvar(How),nonvar(G), !, my_assertz_if_new(is_decl_pt(How,G)))),!.
:- set_prolog_flag(color_term,true).
%:- arc_set_stream(current_output, tty(true)).
%:- arc_set_stream(user_output, tty(true)).
%:- arc_set_stream(user_error, tty(true)).
%:- arc_set_stream(user_output, newline(unix)).

arc_set_stream(S,P):- ignore((nonvar(S),nonvar(P),catch(set_stream(S,P),_,fail))).
%arc_set_stream(S,P):- ignore((print(user_error,set_stream(S,P)),nl(user_error),set_stream(S,P))).

%:- stream_property(S,file_no(2)), arc_set_stream(S,tty(true)).
%:- stream_property(S,file_no(1)), arc_set_stream(S,tty(true)).

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

:- multifile '$pldoc'/4. 
:- dynamic '$pldoc'/4. 
:- catch((assert(('$exported_op'(_,_,_,_):- fail))),_,true).
%'$pldoc'(_,_,_,_):- fail.
%:- discontiguous '$pldoc'/4. 

:- multifile '$autoload'/3. 
%:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
:- catch((assert(('$autoload'(_,_,_):- fail))),_,true).
%'$autoload'(_,_,_):- fail.


:- dynamic(em_html/0).


%is_cgi:- !.
is_cgi:- em_html,!.
is_cgi:- xlisting_web:is_cgi_stream,!,current_predicate( wants_html/0), wants_html.
is_cgi:- arc_html,!.
%arc_html:- in_pp(ansi),!,fail.
arc_html:- em_html,!.
arc_html:- current_predicate( wants_html/0), wants_html.

ansi_main:- thread_self(main),nop(is_cgi),!.
main_thread:- thread_self(main),!.
if_thread_main(G):- main_thread->call(G);true.

update_changes:- \+ thread_self(main),!.
update_changes:- 
    '$update_library_index',
    findall(File, make:modified_file(File), Reload0),
    list_to_set(Reload0, Reload),
    forall(prolog:make_hook(before, Reload),true),
    notrace((ignore(update_changed_files1))),
    print_message(silent, make(reload(Reload))),
    make:maplist(reload_file, Reload),
    print_message(silent, make(done(Reload))),
    forall(prolog:make_hook(after, Reload),true).


cls_z_make:- if_thread_main(notrace((ignore(cls_z),ignore(update_and_fail)))).
clsmake:- if_thread_main(notrace(ignore((\+ is_detatched_thread, cls_z_make)))),!.
update_and_fail:- once(update_changes),fail.
update_and_fail_cls:- once(cls_z),update_and_fail.

% COMMAND LINE ARC
:- if(\+ current_module(logicmoo_arc)).

  :- set_prolog_flag(access_level,system).
  
  :- SL  is 2_147_483_648*8*4, set_prolog_flag(stack_limit, SL ).
  set_display:-!.
  set_display:- ((getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0'))).
  :- if(current_prolog_flag(xpce,true)).
  :- set_display.
  :- endif.
  set_guitracer:- set_display,checkgui_tracer.
  unset_guitracer:- (unsetenv('DISPLAY')),checkgui_tracer.
  checkgui_tracer:- (getenv('DISPLAY',_) -> catch(call(call,guitracer),_,true) ; catch(call(call,noguitracer),_,true)).

  :- if( \+ current_prolog_flag(xpce,true)).
  %:- unsetenv('DISPLAY').
  :- set_prolog_flag(gui_tracer,false).
  :- endif.
  %:- checkgui_tracer.
  %:- catch(noguitracer,_,true).

  :- if(current_prolog_flag(xpce,true)).
  :- set_guitracer.
  :- endif.

  :- set_prolog_flag(toplevel_print_anon,true).
  :- set_prolog_flag(toplevel_print_factorized,true).
  
  :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(6), attributes(portray)]).
  :- set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(5), attributes(portray)]).
  :- set_prolog_flag(print_write_options, [quoted(true), portray(true), max_depth(50), attributes(portray)]).
  
  :- set_prolog_flag(debug_on_error,true).
  :- set_prolog_flag(report_error,true).
  :- set_prolog_flag(on_error,status).
  :- set_prolog_flag(debugger_show_context,true).
  
  :- set_prolog_flag(last_call_optimisation,false).
  %:- set_prolog_flag(trace_gc,false).
  %:- set_prolog_flag(write_attributes,dots).
  :- set_prolog_flag(backtrace_depth,1000).
 

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


:- current_prolog_flag(argv,C),u_dmsg(current_prolog_flag(argv,C)),!.

:- set_prolog_flag(no_sandbox,true).


:- set_prolog_flag(use_arc_www,true).
:- set_prolog_flag(use_arc_swish,false).
:- set_prolog_flag(use_arc_bfly,false).
:- set_prolog_flag(http_port,1766).

% false = command line (no butterfly)
% butterfly (arc_html.sh)
% true = no butterfly (SWISH only)
%
:- nb_setval(arc_can_portray,nil).

% --name=bool, --name, --no-name or --no-name=false.
process_not_option(P2,E,false):- process_cmdln_option(P2,E,true).
process_not_option(P2,E,_):- process_cmdln_option(P2,E,false).

process_cmdln_option(P2,E,TF):- atom_concat('no-',O,E),process_not_option(P2,O,TF),!.
process_cmdln_option(P2,E,TF):- atom_concat('--no-',M,E),atom_concat('--',M,O),process_not_option(P2,O,TF),!.
%process_cmdln_option(P2,E,TF):- atom_concat('no',O,E),process_not_option(P2,O,TF),!.
process_cmdln_option(P2,E,TF):- atom_concat(O,'=false',E),process_not_option(P2,O,TF).
process_cmdln_option(_,'--',_):-!.
process_cmdln_option(P2,E,TF):- atom_concat(O,'=true',E),process_cmdln_option(P2,O,TF).
process_cmdln_option(P2,E,TF):- atom_concat('--',O,E),!,process_cmdln_option(P2,O,TF).
process_cmdln_option(P2,E,TF):- atom_concat('use-',O,E),!,process_cmdln_option(P2,O,TF).
process_cmdln_option(P2,E,true):- atomic_list_concat([N,V],'=',E),!,process_cmdln_option(P2,N,V).
%process_cmdln_option(P2,E,true):- atom_contains(E,'='),!,notrace(catch((atom_to_term(E,N=V,Vs),my_maplist(ignore,Vs)),_,fail)),process_cmdln_option(P2,N,V).
process_cmdln_option(_P2,E,V):- forall((current_prolog_flag(O,_),atom_concat(_,E,O)),(echo_option(O,V),set_prolog_flag(O,V))),fail.
process_cmdln_option(P2,E,V):- call(P2,E,V).

echo_option(N,V):- u_dmsg(echo_option(N,V)).

:- current_prolog_flag(argv,C),forall(member(E,C),process_cmdln_option(echo_option,E,true)).
:- current_prolog_flag(argv,C),forall(member(E,C),process_cmdln_option(set_prolog_flag,E,true)).


:- if(current_prolog_flag(xpce,false)).
:- ignore((retract((prolog_edit:load:- pce:ensure_loaded(library(swi_edit)))))).
:- listing(prolog_edit:load).
%:- use_module(libary(swi_edit)).
:- use_module(library(edit)).
:- endif.

%:- autoload_all.

%:- listing((.)/3).
%:- autoload_all.
:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose_load,false).
%:- autoload_all.


% we alias these so we can catch out of control list growth

my_append(A,B):- append(A,B).
my_append(A,B,C):- append(A,B,C). % ,check_len(A),check_len(C),check_len(C).
gappend(A,B):- append(A,B).
gappend(A,B,C):- append(A,B,C). % ,check_len(A),check_len(C),check_len(C).
check_len(_).

:- set_prolog_flag(arc_term_expansion, false).
:- ensure_loaded(kaggle_arc_precompiler).
/*

set(_355218._355220)=_355272)
*/
arc_sensical_term(O):- nonvar(O), O\==[], O\=='', O \= (_ - _), O\==end_of_file.
arc_sensical_term(V,O):- arc_sensical_term(V), !, O=V.



get_map_pairs(Map,is_assoc,Pairs):- is_assoc(Map), assoc_to_list(Map, Pairs).
get_map_pairs(Map,is_rbtree,Pairs):- is_rbtree(Map), rb_visit(Map, Pairs).
get_map_pairs(Map,is_dict(T),Pairs):- is_dict(Map), dict_pairs(Map,T,Pairs).

is_vm(Tree):- is_vm_map(Tree), once(get_kov(lo_program,Tree,_)).

is_vm_map(Tree):- is_rbtree(Tree),!, rb_in(izmap,true,Tree).
is_vm_map(Dict):- is_dict(Dict),!, get_dict(izmap,Dict,true).



arc_setval(O,List):- is_list(List),!,maplist(arc_setval(O),List).
arc_setval(O,Map):- get_map_pairs(Map,_Type,Pairs),!,my_maplist(arc_setval(O),Pairs).
arc_setval(O,N=V):- !, arc_setval(O,N,V).
arc_setval(O,N-V):- !, arc_setval(O,N,V).
arc_setval(O,NV):- arc_setval(O,NV,t).


arc_setval(O,N,V):- is_dict(O),!, nb_set_dict(N,O,V).
arc_setval(O,N,V):- is_rbtree(O),!, (nb_rb_get_node(O,N,Node)->nb_rb_set_node_value(Node,V);nb_rb_insert(O,N,V)).
arc_setval(O,N,V):- set_o_m_v(O,N,V).



:- meta_predicate(when_in_html(0)).
%when_in_html(Goal):- !, fail,Goal.
%when_in_html(_):- never_webui,!.
when_in_html(Goal):- ignore((arc_html,!,current_output(Out),with_set_stream(Out,tty(false),Goal))).

:- meta_predicate(with_set_stream(+,+,0)).
with_set_stream(Stream,Set,Goal):-
  make_unifiable(Set,Unset),compound(Unset),stream_property(Stream,Unset), ground(Unset),!,
  redo_call_cleanup(arc_set_stream(Stream,Set),Goal,arc_set_stream(Stream,Unset)).
with_set_stream(Stream,Set,Goal):- arc_set_stream(Stream,Set),!,call(Goal).
   
:- meta_predicate(when_arc_webui_enabled(0)).
when_arc_webui_enabled(G):- is_cgi,!, call(G).
when_arc_webui_enabled(_):- never_webui,!.
when_arc_webui_enabled(G):- call(G).

:- meta_predicate(when_using_swish(0)).
when_using_swish(G):- (current_prolog_flag(use_arc_swish,true)-> catch_log(G) ; true).

:- meta_predicate(as_if_webui(0)).
%as_if_webui(_Goal):- never_webui,!.
as_if_webui(Goal):- in_pp(bfly),!,call(Goal).
as_if_webui(Goal):- in_pp(swish),!,call(Goal).
as_if_webui(Goal):- is_cgi,!,call(Goal).
as_if_webui(Goal):- with_toplevel_pp(http,Goal). 
/*
as_if_webui(Goal):- arc_html,!,call(Goal).
as_if_webui(Goal):- in_pp(bfly),!,call(Goal).
as_if_webui(Goal):- ignore(when_arc_webui_enabled(catch_log(as_if_http(Goal)))).
%:- initialization arc_http_server.
as_if_http(Goal):- in_pp(bfly),!,call(Goal).
as_if_http(Goal):- in_pp(swish),!,call(Goal).
as_if_http(Goal):- with_toplevel_pp(http,Goal).
*/
%arc_html:- in_pp(http),!.
%arc_html:- \+ current_output(user_output).

:- exists_source(library(xlisting/xlisting_web)) -> system:use_module(library(xlisting/xlisting_web)) ; true.

%ld_logicmoo_webui:- !.
ld_logicmoo_webui:-
   set_prolog_flag(use_arc_www,true),
   ignore((exists_source(library(logicmoo_webui)), 
         use_module(library(logicmoo_webui)),
         catch_log(dmsg(('?-'('webui_start_swish_and_clio')))),
         catch_log(set_long_message_server('https://logicmoo.org:11766')))), 
  system:use_module(library(xlisting/xlisting_web)),
  system:use_module(library(xlisting/xlisting_web_server)),
  ensure_loaded(kaggle_arc_ui_html_wss),
  ensure_loaded(kaggle_arc_ui_html),!.


logicmoo_use_swish:-
  set_prolog_flag(use_arc_swish,true),
  ld_logicmoo_webui,webui_start_swish_and_clio,
  http_handler('/swish', http_redirect(moved, '/swish/'), []).

arc_user(Nonvar):- nonvar(Nonvar),!,arc_user(Var),!,Nonvar=Var.
arc_user(main):- main_thread, !. %\+ if_thread_main(fail),!.
arc_user(ID):- catch((pengine:pengine_user(ID)),_,fail),!.
arc_user(ID):- catch((xlisting_web:is_cgi_stream,xlisting_web:find_http_session(User),http_session:session_data(User,username(ID))),_,fail),!.
arc_user(ID):- catch((is_cgi, (xlisting_web:find_http_session(ID))),_,fail),!.
arc_user(ID):- is_cgi,!,ID=web_user.
arc_user(ID):- thread_self(ID).

:- dynamic(arc_user_prop/3).

%luser_setval(N,V):- nb_setval(N,V),!.
luser_setval(N,V):- arc_user(ID),luser_setval(ID,N,V),!.
luser_setval(ID,N,V):- \+ (arc_sensical_term(N),arc_sensical_term(V)),  
  warn_skip(not_arc_sensical_term(luser_setval(ID,N,V))).
luser_setval(ID,N,V):- 
  (atom(N)->nb_setval(N,V);true),
  retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).


luser_unsetval(N):- ignore(nb_delete(N)), arc_user(ID),luser_unsetval(ID,N),!.
luser_unsetval(ID,N):- retractall(arc_user_prop(ID,N,_)).

luser_default(N,V):- var(V),!,luser_getval(N,V).
luser_default(N,V):- luser_setval(global,N,V).

luser_linkval(N,V):- arc_user(ID),luser_linkval(ID,N,V),!.
luser_linkval(ID,N,V):- \+ var(V), \+ (arc_sensical_term(N),arc_sensical_term(V)),  
 trace,
 warn_skip(not_arc_sensical_term(luser_linkval(ID,N,V))).
luser_linkval(ID,N,V):- 
  (atom(N)->nb_linkval(N,V);true), 
  retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).


%arc_option(grid_size_only):- !,fail.
arc_option(O):- luser_getval(O,t).
if_arc_option(O,G):- (arc_option(O)->must_det_ll(G); true).

with_luser(N,V,Goal):-
  (luser_getval(N,OV);OV=[]),
  setup_call_cleanup(
    luser_setval(N,V),
    once(Goal),
    luser_setval(N,OV)).

%luser_getval(N,V):- nb_current(N,VVV),arc_sensical_term(VVV,VV),!,V=VV.
% caches the valuetemp on this thread
luser_getval(N,V):-  luser_getval_0(N,VV),VV=V,arc_sensical_term(V),!.


luser_getval_0(arc_user,V):- arc_user(V).
luser_getval_0(N,V):- luser_getval_1(N,V).

luser_getval_1(N,V):- luser_getval_2(N,V).
luser_getval_1(N,V):- luser_getval_3(N,V), \+ (luser_getval_2(N,VV), nop(VV\=V)).
luser_getval_1(N,V):- luser_getval_4(N,V), \+ (luser_getval_3(N,VV), nop(VV\=V)), \+ (luser_getval_2(N,VV), nop(VV\=V)).

%luser_getval_0(N,V):- luser_getval_2(N,V), \+ luser_getval_1(N,_).
%luser_getval_0(N,V):- luser_getval_3(N,V), \+ luser_getval_2(N,_), \+ luser_getval_1(N,_).
%luser_getval_3(N,V):- is_cgi, current_predicate(get_param_req/2),get_param_req(N,M),url_decode_term(M,V).
luser_getval_2(N,V):- \+ main_thread, atom(N), httpd_wrapper:http_current_request(Request), member(search(List),Request),member(N=VV,List),url_decode_term(VV,V),arc_sensical_term(V),!.
luser_getval_2(N,V):- atom(N), nb_current(N,ValV),arc_sensical_term(ValV,Val),Val=V.

luser_getval_3(N,V):- arc_user(ID), arc_user_prop(ID,N,V).
luser_getval_3(_,_):- \+ is_cgi, !, fail.
luser_getval_3(N,V):-  \+ main_thread, atom(N), current_predicate(get_param_sess/2),get_param_sess(N,M),url_decode_term(M,V),arc_sensical_term(V).
%luser_getval_3(N,V):- atom(N), nb_current(N,ValV),arc_sensical_term(ValV,Val),Val=V.


luser_getval_4(N,V):- arc_user_prop(global,N,V).
luser_getval_4(N,V):- atom(N), current_prolog_flag(N,V).
%luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.
%luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).


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



is_detatched_thread:- is_cgi,!.
is_detatched_thread:- \+ (thread_self(Main) -> Main == main ; main==0),!.

cls_z:- tracing,!.
cls_z:- is_detatched_thread,!,flush_tee.
cls_z:- if_thread_main((catch(really_cls,_,true), flush_tee, nop((clear_tee,clear_test_html)))).

really_cls:- write('\ec\33c\033[2J\033[H\033[3J'),!.
really_cls:- catch(cls,_,true),!.

cls1:- nop(catch(cls_z,_,true)).


list_to_rbtree_safe(I,O):- must_be_free(O), list_to_rbtree(I,M),!,M=O.

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
          make_indiv_object(VM,[iz(Prop),vis2D(H,V),/*b*/iz(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          make_bg_visible(Value,VValue),
          print_grid(H,V,Prop,VValue))),!.

set_vm_obj1(Prop,Or,IndvPoints):- is_points_list(IndvPoints),!,
  if_t(IndvPoints\==[],
    (get_vm(VM),          
      make_indiv_object(VM,[iz(Prop),/*b*/iz(set_vm(Prop))|Or],IndvPoints,_Obj),
      %addObjects(VM,Obj),
      print_grid(VM.h,VM.v,Prop,IndvPoints))),!.


set_vm_obj1(Prop,Or,OValue):- is_object(OValue),!,
  get_vm(VM),
  remObjects(VM,OValue),
  override_object([iz(Prop),/*b*/iz(set_vm(Prop))|Or],OValue,NewObj),
  addObjects(VM,NewObj),
  object_grid(NewObj,Grid),
  print_grid(Prop,Grid),!.

set_vm_obj_nil(Prop,Or):- u_dmsg(set_vm_obj_nil(Prop,Or)).
/*
 get_vm(VM), Value==[], H = VM.h, V = VM.v,

  %points_to_grid(H,V,Points,Grid),
  %Grid=[[wbg]],

  make_indiv_object(VM,[iz(Prop),vis2D(1,1),/*b*/iz(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          make_bg_visible(Value,VValue),
          print_grid(H,V,Prop,VValue))),
  % set_vm_obj(Prop,Or,Grid),

*/


get_vm(Key,Value):-  get_vm(VM), get_kov(Key,VM,Value).

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
saved_training(TestID):- test_name_output_file(TestID,'.pl',File),exists_file(File).



:- set_prolog_flag(arc_term_expansion, true).

:- ensure_loaded('kaggle_arc_fwd.pfc').

%:- set_prolog_flag(arc_term_expansion, false).

%:- if(prolog_load_context(reload,false)).
%:- fixup_module_exports_into_from(system,muarc).
%:- endif.

%:- fixup_module_exports_now.  

%:- ignore(check_dot_spacing).

   
:- add_history((print_test)).
:- add_history((webui_tests)).
:- add_history((bfly_test(a1))).
:- add_history((bfly_tests)).
:- add_history((test_pp)).
:- add_history((bfly_startup)).
%:- add_history1((cls_z,make,demo)).
:- add_history1((noguitracer,demo)).
:- add_history1((demo)).



:- nb_setval(arc_can_expand_query,nil).
:- nb_setval(arc_can_expand_query,t).
%:- \+ nb_current(arc_can_portray,nil).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).



bfly_startup:-
   set_toplevel_pp(bfly),
   asserta(was_inline_to_bfly),inline_to_bfly_html,
   bfly,
   catch_log(webui_tests),
   ansi,
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,prev_test)),!,
   ansi.


ansi_startup:- 
   ansi,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,prev_test)),!.

:- luser_default(example,(trn+0)).
:- luser_default(no_diags,false).
:- luser_default(no_individuator, f).
:- luser_default(grid_size_only,true).
%:- luser_default(cmd,test_easy).
%:- luser_default(cmd,learn_ilp).
:- luser_default(cmd,solve_via_scene_change).
:- luser_default(cmd2,print_all_info_for_test).
%:- luser_default(cmd2,test_show_grid_objs).
:- luser_default(use_individuated_cache,true).

:- current_prolog_flag(argv,C),forall(member(E,C),process_cmdln_option(luser_default,E,true)).


load_task_states:- exists_directory('/data/evaluation/'),catch_log(load_json_files(evaluation,v,'/data/evaluation/*.json')),!.
load_task_states:- exists_directory('./secret_data/evaluation/'),catch_log(load_json_files(evaluation,v,'./secret_data/evaluation/*.json')),!.
load_task_states:- exists_directory('../../secret_data/evaluation/'),catch_log(load_json_files(evaluation,v,'../../secret_data/evaluation/*.json')),!.
test_load_task_states:- load_task_states.



run_arcathon:-
  load_task_states,
  %catch_log(prolog),
  catch_log(demo).
test_run_arcathon:- run_arcathon.

test_secret_data:- u_dmsg(todo_test_secret_data).


save_arcathon_runner:- qsave_program('bin/logicmoo_arcathon_runner',[stand_alone(true),verbose(true),toplevel(run_arcathon),goal(run_arcathon),% class(runtime),
                                             class(runtime),obfuscate(true)]).
save_arcathon_runner_dbg:- qsave_program('bin/logicmoo_arcathon_runner_dbg',[stand_alone(true),verbose(true),toplevel(run_arcathon),goal(run_arcathon),% class(runtime),
                                             class(development),obfuscate(false)]).
save_arcathon_runner_devel:- qsave_program('bin/logicmoo_arcathon_runner_devel',[stand_alone(true),verbose(true),goal(demo),class(development),obfuscate(false)]),
                             save_arcathon_runner_dbg, save_arcathon_runner.
test_compile_arcathon:- save_arcathon_runner_devel.

devaluation:- catch_log(load_json_files(eval400,v,'./data/devaluation/*.json')).

:- ensure_loaded(kaggle_arc_two).

:- arc_sub_path('./muarc_cache/test_state/',Test_State),make_directory_path(Test_State),!.
load_from_main:- 
  catch_log(load_json_files),
  catch_log(load_task_states),
  catch_log(devaluation),
  !.

:- initialization(load_from_main).
:- initialization(scan_uses_test_id(main_file)).
%:- initialization(store_grid_size_predictions).
%:- initialization(test_grid_size_predictions).
:- initialization(make_grid_cache).
:- initialization(gen_gids).

:- use_module(library(xlisting/xlisting_web)).
:- use_module(library(xlisting/xlisting_console)).
:- use_module(library(xlisting)).

:- no_web_dbg.

:- initialization(test_show_colors,after_load).
:- nb_setval(arc_can_portray,t).
:- nb_setval(arc_can_portray,nil).
%:- load_arc_db_temp_cache.
demo_msg:- nl,writeln('% Type ?- demo. % or press up arrow').
:- initialization(demo_msg,after_load).
:- luser_default(extreme_caching,false).
:- nb_setval(arc_can_portray,nil).
%:- (getenv('DISPLAY',_) -> ensure_guitracer_x ; true).


:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose_load,false).
%:- autoload_all.

gui_flag(GUI):- (current_prolog_flag(gui, GUI)-> true ;
   (((getenv('DISPLAY', Display), Display \== '') -> GUI = true ; GUI=false),
      set_prolog_flag(gui,GUI))).

:- gui_flag(GUI),set_prolog_flag(gui,GUI).

:- current_prolog_flag(gui,false)->set_prolog_flag(xpce,false);true.

:- if(current_prolog_flag(xpce,true)).
use_gui_debugger:- 
 locally(set_prolog_flag(xpce,true),
   locally(set_prolog_flag(gui,true),
     locally(set_prolog_flag(autoload,true),
  must_det_ll((
    absolute_file_name(swi(xpce/prolog/lib),X), 
    asserta(user:library_directory(X),CLRef), 
    user:use_module(library(pce_prolog_xref)),
    user:use_module(library(emacs_extend)),
    user:use_module(library(trace/gui)),
    user:use_module(library(pce)),
    user:use_module(library(gui_tracer)),
    %reload_library_index,
    erase(CLRef),
    gtrace,notrace,guitracer,notrace,
    call(autoload_all)))))).

%:- use_gui_debugger.
:- locally(set_prolog_flag(autoload,true), 
           prolog_ide(thread_monitor)).

:- endif.
:- remove_undef_search.
%:- prolog_ide(debug_monitor).
:- when_arc_webui_enabled(((ld_logicmoo_webui))).
%kill_xpce:- user:file_search_path(image, '/usr/lib/swi-prolog
%:- redefine_system_predicate(prolog_trace:trace/0),abolish(prolog_trace:trace/0),asserta(prolog_trace:trace).
%:- redefine_system_predicate(system:break/0),abolish(system:break/0),asserta(system:break).
%:- redefine_system_predicate(system:trace/0),abolish(system:trace/0),asserta(system:trace).
%:- redefine_system_predicate(prolog_trace:trace/2),abolish(prolog_trace:trace/2),asserta(prolog_trace:trace(_,_)).
:- when_arc_webui_enabled(((start_arc_http_server))).
:- when_using_swish(logicmoo_use_swish).
:- set_prolog_flag(never_pp_hook, false).
:- if(current_prolog_flag(bfly,true)).
:- bfly_startup.
:- endif.
:- set_prolog_flag(autoload,true).
:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)>(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')>(tst+0),"segv").
%is_buggy_pair(t('27a28665')>(tst+2), "BUG: Re-Searcher gets stuck!").
/*


:- set_current_test(v('1d398264')). 
:- luser_default(task,v('1d398264')). 
:- luser_default(task,v('37d3e8b2')). 

*/
create_group_dmiles:- 
   must_det_ll((create_group(dmiles_nsn,[
    'e41c6fd3','ea32f347','37d3e8b2','0a2355a6', 'b230c067','a61f2674','d2abd087','08ed6ac7']))),
  %must_det_ll((create_group(dmiles_nsn2,[
  % 'a61f2674','0a2355a6', 'a61ba2ce', 'ea32f347', 'a79310a0', '37d3e8b2', 'e41c6fd3', 'b230c067', '0d3d703e', '08ed6ac7']))),
   must_det_ll((create_group(dmiles,[
              'makesboxsq','a61ba2ce',   
              'e41c6fd3','ea32f347',
              '37d3e8b2','0a2355a6',
              'a79310a0','103eff5b',
              '33b52de3','d2abd087','08ed6ac7',
              '1b60fb0c','1d398264',
              '0d3d703e','626c0bcc',
              '5582e5ca','25d487eb',
              '32e9702f','f8b3ba0a',
              'b230c067',
              '29c11459','a61f2674']))),
  set_current_test('0a2355a6'),
  set_pair_mode(whole_test),
  %set_current_test('makesboxsq'),
  %set_current_test('08ed6ac7'),
  !.
:- initialization(create_group_dmiles).
%:- noguitracer.
% :- set_current_test(t('0d3d703e')).  % :- set_current_test(t('5582e5ca')).

%:- luser_default(task,v('1b60fb0c')). %626c0bcc
:- set_prolog_flag(gui_tracer, false), visible(-cut_call).

%:- demo.
:- current_prolog_flag(argv,C),(member('-l',C)->initialize;true).
:- initialization(scan_uses_test_id(main_file_complete)).
%:- use_module('./induction/h_muarc_alephlib').
%:- consult('./induction/h_muarc_aleph').
%:- tmp:loading_arc_from(M),'$set_source_module'(M).

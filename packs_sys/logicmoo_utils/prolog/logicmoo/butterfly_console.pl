/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================

%:- use_module(library(logicmoo/butterfly_console)).

*/

% We save the name of the module loading this module
:- module(butterfly,[bformat/1,bformat/2,bformat/3,
  is_butterfly_console/0,
  set_is_butterfly_console/1,
  bfly_test/1,
  bfly_tests/0,
  pre_style/0,mouse_over_span/0]).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/pretty_clauses)).

:- thread_local(t_l:in_block_format/0).
:- dynamic(lmcache:is_butterfly_thread/2).

%:- use_module(library(pengines)).
:- pengine_sandbox:use_module(library(pengines)).
:- use_module(library(http/html_write)).
:- autoload(library(http/html_write),[html/3,print_html/1]).
:- autoload(library(lynx/html_text),[html_text/2]).

set_is_butterfly_console(TF):- thread_self(X), retractall(lmcache:is_butterfly_thread(X,_)),
  asserta(lmcache:is_butterfly_thread(X,TF)),!, (TF==t->pre_style;true).

:- meta_predicate(wbfc(0)).
wbfc(G):-G=true,!,set_is_butterfly_console(t).
wbfc(G):-G=false,!,set_is_butterfly_console(f).
wbfc(Goal):-with_butterfly_console(t,Goal).

:- meta_predicate(with_butterfly_console(+,0)).
with_butterfly_console(TF,Goal):- in_bfly(TF,Goal). 
%with_butterfly_console(TF,Goal):- thread_self(X), %retractall(lmcache:is_butterfly_thread(X,_)),
%  setup_call_cleanup(asserta(lmcache:is_butterfly_thread(X,TF),Ref),Goal,erase(Ref)).

is_butterfly_console:- toplevel_pp(bfly),!.
%is_butterfly_console:- thread_self(X), lmcache:is_butterfly_thread(X,TF),!,TF==t.
%is_butterfly_console:- getenv('COLORTERM',butterfly),!.
%is_butterfly_console:- thread_self(X),atom(X),(atom_concat(_,'23',X);atom_concat(_,'01',X);atom_concat(_,'00',X)),!.


block_format(G):- t_l:in_block_format,!,call(G).
block_format(G):- wots((S),locally(t_l:in_block_format,G)),bformat(S),!.


%bfly_write_html(S):- !, format("(HTML ~w)",[S]),!.
%bfly_write_html(P):- format("\x90;HTML|~w\x93",[P]).
%bfly_write_html(P):- format("P;HTML|~wP",[P]),!. %'
%bfly_write_html(S):- format("\x1bP;HTML|~w\x1bP",[S]),end_escape.

%bfly_write_html(S):- rich_output(Out),!,with_output_to(Out,bfly_write_html(S)).

%bformat(P):- is_visible_output,is_butterfly_console,format(string(S),'~w',[P]),atom_contains(S,'<'),!,bformat(S).
%


%:- /*system:*/use_module(library(http/term_html)).
:- /*system:*/use_module(butterfly_term_html,[bfly_term//2]).

:- /*system:*/use_module(library(http/thread_httpd)).
:- /*system:*/use_module(thread_httpd:library(http/http_dispatch)).
%:- use_module(library(http/http_dispatch))
:- /*system:*/use_module(swi(library/http/html_head)).
:- /*system:*/use_module(library(http/http_dispatch)).
:- /*system:*/use_module(library(http/http_path)).
:- /*system:*/use_module(library(http/http_log)).
:- /*system:*/use_module(library(http/http_client)).
:- /*system:*/use_module(library(http/http_server_files)).
:- /*system:*/use_module(library(http/http_parameters)).

:- /*system:*/use_module(library(uri)).
:- /*system:*/use_module(library(http/http_openid)).
:- /*system:*/use_module(library(http/http_host)).
% :- use_module(library(http/html_write)).
:- /*system:*/use_module(library(http/http_error)).

%:- abolish(bfly_dyn:bfly_style_type/6).
:- dynamic(bfly_dyn:bfly_style_type/6).
:- volatile(bfly_dyn:bfly_style_type/6).

%:- abolish(bfly_dyn:bfly_style_answered/0).
:- dynamic(bfly_dyn:bfly_style_answered/0).
:- volatile(bfly_dyn:bfly_style_answered/0).


%:- abolish(bfly_dyn:bfly_style_asked/1).
:- dynamic(bfly_dyn:bfly_style_asked/1).
:- volatile(bfly_dyn:bfly_style_asked/1).

maybe_into_number(A,Num):- number(A),!,Num=A.
maybe_into_number(A,Num):- \+ string(A), sformat(S,'~w',[A]), string(S),!, maybe_into_number(S,Num),!.
maybe_into_number(A,Num):- atomic_list_concat([_|Es],'/',A), Es\==[], last(Es,E),!,maybe_into_number(E,Num).
maybe_into_number(A,Num):- atom_number(A,Num),!.
maybe_into_number(_,Num):- Num is -1.

use_pts_files:- fail.

bfly_reoffer:- \+ use_pts_files,!.
bfly_reoffer:- 
  bfly_info,
  retractall(bfly_dyn:bfly_style_type(_,_,_,_,_,_)),
  retractall(bfly_dyn:bfly_style_asked(_)),
  retractall(bfly_dyn:bfly_style_answered),
  bfly_offer(60),
  bfly_info.

bfly_offer:- bfly_offer(15).

bfly_offer(_Duration):- \+ use_pts_files,!.
bfly_offer( Duration):-
  expand_file_name('/dev/pts/*',[_,_|X]),  
  retractall(bfly_dyn:bfly_style_asked(_)),
  retractall(bfly_dyn:bfly_style_answered),
  forall(member(E,X),bfly_ask_style(E)),
  get_time(Time), Until is Time + Duration, 
  (repeat,
    ((get_time(TimeNow), TimeNow > Until)
       -> true ; 
       wait_for_input_or_web)), !.

bfly_start:- do_each_main_interval(bfly_offer(15), 60).
%bfly_start:- initialization(do_each_main_interval(bfly_offer(15), 60), program).
%:- add_history(bfly_start).
:- export(bfly_start/0).

wait_for_input_or_web:- \+ bfly_dyn:bfly_style_asked(_),!.
wait_for_input_or_web:- bfly_dyn:bfly_style_answered,!.
wait_for_input_or_web:- with_tty_raw((
  wait_for_input([user_input], In, 0.3),
  (In==[]-> (!,fail) ; 
   (get_single_char(H),!, asserta(bfly_dyn:bfly_style_answered), bfly_key_accept(H))))),!.

bfly_key_accept(H):- H = 32,  retractall(bfly_dyn:bfly_style_asked(_)),!.
bfly_key_accept(H):- H> 96, PTS is H-96, bfly_decl_style_key(PTS,ansi),!.
bfly_key_accept(H):- H> 64, PTS is H-64, bfly_decl_style_key(PTS,ansi),!.

bfly_decl_style_key(Num,_Style):- \+ bfly_dyn:bfly_style_asked(Num),!.
bfly_decl_style_key(Num, Style):- thread_self(TID),bfly_decl_1_style(TID,Num,Style).

:- export(bfly_decl_style_http/1).
bfly_decl_style_http(Request) :-
  member(search(List),Request),
  member(tid=TID,List), member(pts=PTS,List), member(style=Style,List),  
  bfly_decl_1_style(TID,PTS,Style),!,
  print_term_to_html_page(Request).

  
print_term_to_html_page(Tree):- 
  wots((S),
    in_pp_html(print_tree(Tree))),
    phrase(html([
     html([head(''),body(pre( \ html_raw(S)))])]), Tokens),
     print_html(Tokens),!.


%:-  http_handler(swish(logicmoo), xlisting_web:handler_logicmoo_cyclone, [id(handler_logicmoo_cyclone)]). % chunked
%:-  http_handler(swish(nc_logicmoo), xlisting_web:handler_logicmoo_cyclone1, [chunked,id(handler_logicmoo_cyclone1)]).
%:- http_handler('/swish/bfly_decl_1_style',butterfly:bfly_decl_1_style,[prefix]).
:- http_handler(root(swish/bfly_decl_style),bfly_decl_style_http,[chunked,methods([get,post,put])]).


:- export(bfly_decl_1_style/3).
bfly_decl_1_style(TID,PTSA,Style):- \+ number(PTSA), maybe_into_number(PTSA,Num), number(Num), !, bfly_decl_1_style(TID,Num,Style).
%bfly_decl_1_style(_TID,Num,_Style):- \+ bfly_dyn:bfly_style_asked(Num),!.
bfly_decl_1_style(TID,Num,Style):-
  %id_to_href(ID,HREF),
  ignore(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_Was)),
  forall(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_),
          retractall(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_))),
  asserta(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,Style)),
  asserta(bfly_dyn:bfly_style_answered),
  retractall(bfly_dyn:bfly_style_asked(_)),!.


print_html_term_tree(Term):- current_print_write_options(Options), print_html_term_tree(Term, Options).
print_html_term_tree(Term, Options):- in_pp_html(print_tree(Term,Options)).


print_html_term(Term):- current_print_write_options(Options), print_html_term(Term, Options).
print_html_term(Term, Options):- 
 must_or_rtrace(phrase(bfly_term(Term,Options),Tokens)),!,
 must_or_rtrace(print_html(Tokens)),!.

bfly_portray(X):- 
  \+ tracing, ground(X),
  \+ ( nb_current('$inprint_message', Messages), Messages\==[] ),
  bfly_get(butterfly,t),
  display_length(X,L), L>120,
  print_html_term_tree(X).

:- meta_predicate(in_bfly(+,0)).
in_bfly(TF,Goal):- 
  bfly_get(butterfly,Was),
  setup_call_cleanup(
    bfly_set(butterfly,TF),
    Goal,    
    bfly_set(butterfly,Was)),!.

:- meta_predicate(in_pp_html(0)).
in_pp_html(Goal):- with_pp(bfly,Goal).

bfly_ask_style(E):- maybe_into_number(E,Num), bfly_ask_style(E, Num).
bfly_ask_style(_, Num):- bfly_dyn:bfly_style_asked(Num),!.
bfly_ask_style(E,   _):- E=='/dev/pts/ptmx',!.
%bfly_ask_style(E,   _):- bfly_dyn:bfly_style_type(_TID,E,  _,_,_,UK), UK\==unknown, !.
bfly_ask_style(_, Num):- bfly_dyn:bfly_style_type(_TID,_,Num,_,_,UK), UK\==unknown, !.
bfly_ask_style(_, Num):- number(Num), Num is -1, !.
bfly_ask_style(E, Num):- 
 ignore((
  atom(E),
  thread_self(TID),
  current_output(Out), current_input(In),  
  retractall(bfly_dyn:bfly_style_type(_,_,Num,_,_,_)),  
  asserta(bfly_dyn:bfly_style_asked(Num)),
  sformat(S1,'<font color="gold"><a target="_new" href="/swish/bfly_decl_style?tid=~w&pts=~w&style=html_esc">Click This GOLD text at ~w for an HTMLy Interface.</font></a><p>',
   [TID,Num,E]),
  bfly_to_pts(E,html_esc,S1),
  Key is Num + 64,  
  sformat(S2,'~nOr Press: <SHIFT+~s>=ansi, <~s>=ansi, <SPACE>=cancel',[[Key],[Key]]),
  bfly_to_pts(E,ansi,S2),
  nop(asserta(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,ansi))) )).


open_for_output(E,_Style,Out,close(Out)):- atom(E), exists_file(E), open(E,append,Out),!.
open_for_output(E,_Style,Out,true):- atomic(E), is_stream(E),!,Out = E.
open_for_output(N, Style,Out,OnExit):- number(N),atom_concat('/dev/pts/',N,E), open_for_output(E,Style,Out,OnExit).
open_for_output(_,_Style,Out,true):- current_output(Out).

tty_to_output_style(E,     Style):- \+ number(E),maybe_into_number(E,Num),number(Num),!,tty_to_output_style(Num, Style).
tty_to_output_style(Num,   Style):- bfly_dyn:bfly_style_type(_,_,Num,_,_, Style), !.
tty_to_output_style(Num, unknown):- bfly_dyn:bfly_style_asked(Num),!.
tty_to_output_style(_,   html_esc):- bfly_dyn:bfly_style_type(_,_,_,_,_,ansi),!.
tty_to_output_style(_,   ansi).



:- meta_predicate(bfly_html_goal(0)).
bfly_html_goal(Goal):- inside_bfly_html_esc,!,call(Goal).
bfly_html_goal(Goal):- 
 setup_call_cleanup(set_bfly_style('html_esc',t), 
  wots(S,(Goal->PF=t;PF=f)),
  set_bfly_style('html_esc',f)), 
  bfly_write_h(S),!,PF==t.

bfly_write_h(S0):- prepend_trim_for_html(S0,SM), prepend_trim(SM,S), ignore((\+ empty_str(S), 
 setup_call_cleanup(bfly_in, write(S),(bfly_out,flush_output)))).

%prepend_trim_for_html(S,S):-!.
prepend_trim_for_html(S,SS):- correct_html_len(S,SS).
%prepend_trim_for_html(S,SS):- prepend_trim(S,SM),correct_html_len(SM,SS).

correct_html_len(S,S):- atom_contains(S,'<pre>'),!.
correct_html_len(S,O):- atomic_list_concat(L,'\n',S),maplist(correct_html_len1,L,LL),!,atomic_list_concat(LL,'\n',O).

% correct_html_len1(S,S):- atom_contains(S,'<pre>'),!.
correct_html_len1(S,S):- atom_length(S,L),L < 120, !.
correct_html_len1(S,S):- atom_contains(S,'<pre>'),!.
correct_html_len1(S,O):- %fail, 
  member(Split,['/>','>','</','<',')','  ']), sub_atom(S,Before,_,_,Split), Before>120,Before < 150, !,
  sub_atom(S,0,Before,_,Left), % BeforeP1 is Before + 1,
  sub_atom(S,Before,_,0,Right),
  correct_html_len(Right,Mid),!,
  atomic_list_concat([Left,'\n ',Mid],'',O),!.

correct_html_len1(S,O):- atomic_list_concat(L,'<',S),atomic_list_concat(L,'\n<',O),!.
correct_html_len1(S,S).


:- meta_predicate(bfly_out_in(0)).
bfly_out_in(Goal):- inside_bfly_html_esc -> setup_call_cleanup(bfly_out, wotso(Goal), bfly_in) ; call(Goal).

bfly_in:- inside_bfly_html_esc,!,flag('$inside_bfly_html_esc_level',X,X+1).
bfly_in:- set_bfly_style('html_esc',t),bfly_write(_,escape_from_screen([esc(80),';HTML|'])).
%bfly_in:- set_bfly_style('html_esc',t),!,bfly_write(_,[escape_from_screen(7),';HTML|']).
bfly_in1:- set_bfly_style('html_esc',t),bfly_write(_,[escape_from_screen('$start'),esc(80),';HTML|']).
%bfly_in:- set_bfly_style('html_esc',t),bfly_write(_,[escape_from_screen('$start'),7,';HTML|']).

bfly_out:- \+ inside_bfly_html_esc,!,flag('$inside_bfly_html_esc_level',X,X-1).
bfly_out:- bfly_write(_,escape_from_screen(esc(80))), set_bfly_style('html_esc',f).
bfly_out1:- bfly_write(_,[7,escape_from_screen('$end')]), set_bfly_style('html_esc',f).

%bfly_out:- set_bfly_style('html_esc',f),bfly_write(_,escape_from_screen(7)).
%bfly_out:- bfly_write(_,[esc(80),escape_from_screen('$end')]), set_bfly_style('html_esc',f).


inside_bfly_html_esc:- in_bfly_style('html_esc',t).



%bfly_html_goal(Goal):- throw(unknown_stream(bfly_html_goal(Goal))).


/*
 Assume will be printed to..

Stream Type               Starts_in            Will SwitchTo
============            ===============       ==============
httpd stream              html_esc             pre_tag,html_esc
pengines output           html_esc             pre_tag,html_esc
ansi terminal             ansi             ansi
butterfly terminal        ansi             html_esc,ansi


html_esc = unformated body elements
ansi = text with color info
ansi = text with color info
pre_tag = preformat text with HTML embedded


*/


%bfly_write_html(S):- (nb_current('$in_swish',t);pengines:pengine_self(_Self)),!, pengines:pengine_output(S),!.
% bfly_write_html(S):- bfly_to_all_pts(S),!.
%bformat(P):- compound(P),wots((S),post_html(P)),bfly_write_html(S),!.

%write_direct(S):- in_swish,!, pengines:pengine_output(S).
write_direct(S):- pformat(S).

%bformat(P):- atom(P),sformat(S,P,[]),!,bformat(S).
%bformat(S):- string(S),atom_concat(PL,'\n',S),!,bformat(PL).
%bformat(S):- t_l:in_block_format,!,format("~w",[S]),!.
bformat(Stream,Fmt,Args):- atomic(Stream),is_stream(Stream),!, with_output_to(Stream,bformat(Fmt,Args)).
bformat(Stream,Fmt,Args):- format(Stream,Fmt,Args).
bformat(Fmt,Args):- sformat(P,Fmt,Args),bformat(P).
bformat(S):- use_pts_files,!,bfly_to_all_pts(S).
bformat(S):- bfly_write(S).

bfly_write(Write):- bfly_write(current,Write).
bfly_write_plain(Stuff):- bfly_out_in(bfly_write(ansi,Stuff)).
bfly_write_html(Stuff):- bfly_html_goal(bfly_write(http,Stuff)).
bfly_write_pre(Stuff):- bfly_write_html(pre(Stuff)).


only_bfly(Goal):- ignore((toplevel_pp(bfly), Goal)).

guess_is_pp(Guess):- in_pp(Guess).
% guess_is_pp(Guess):- toplevel_pp(Guess).

bfly_write(Style,S):- var(S),!, write(var_in_style(Style,S)),!.

bfly_write(_,  '$html'):- !, only_bfly(bfly_in).
bfly_write(_,'$nohtml'):- !, only_bfly(bfly_out).



bfly_write(_,esc(Char)):- !, only_bfly(( put(27),!, put_code(Char))).

bfly_write(Style,IsList):- is_list(IsList), !, bfly_at_once(must_maplist_det(bfly_write(Style),IsList)),!.

bfly_write(Style,escape_from_screen('$start')):- !, only_bfly(bfly_write(Style,[when_in_screen(esc(80))])).
bfly_write(Style,escape_from_screen('$end')):- !, only_bfly(bfly_write(Style,[when_in_screen(esc(92))])).
bfly_write(Style,escape_from_screen(X)):-!, bfly_write(Style,[when_in_screen(esc(80)),X,when_in_screen(esc(92))]).
bfly_write(Style,when_in_screen(X)):- !, only_bfly(ignore((getenv('TERM',screen),bfly_write(Style,X)))).

bfly_write(current,S):- guess_is_pp(What),!,bfly_write(What,S).
bfly_write(Style,S):- (string(S);is_codelist(S);is_charlist(S)), format(atom(T),'~s',[S]), !, bfly_write(Style,T).
bfly_write(_Styl,S):- atom(S),(atom_contains(S,'<'),atom_contains(S,'>')),!,write_direct(S).
bfly_write(_Styl,ansi(X)):-!, bfly_write_plain(X).


bfly_write(_Styl,call(X)):-!, call(X).
bfly_write(ansi,pre(X)):- !,bfly_write(ansi,X).
bfly_write(_Styl,pre(X)):- !, bfly_write_html([html(' <pre>'),X,html('</pre>')]),!.
bfly_write(_Styl,html(X)):- !, bfly_write_html(X),!.
bfly_write(ansi,term(X)):- !, bfly_out_in(print_tree(X)).
bfly_write(_Styl,term(X)):- !, bfly_html_goal(print_html_term(X)).
bfly_write(ansi,style(_,X)):- !, bfly_out_in(bfly_write(ansi,X)).
bfly_write(Style,style(C,X)):- !,bfly_write(Style,[html('<font style="~w">',[C]),X,html('</font>')]),!.
bfly_write(ansi,color(C,X)):- !,color_format(fg(C),'~@',[bfly_write(ansi,X)]).
bfly_write(_Styl,color(C,X)):- !,sformat(S,'<font color="~w">',[C]),bfly_write_html([html(S),X,html('</font>')]),!.
bfly_write(_Styl,w(Text)):- !, write(Text). % needed in order to write an integer or special atoms
bfly_write(_Styl,hwt(0)):- !, bfly_write_html('<pre>hello world</pre>').
bfly_write(_Styl,hwt(a)):- !, write("\e]8;;https://example.com\aThis is a link\e]8;;\a\c").
bfly_write(Style,hwt(1)):- !, bfly_write(Style,ht('https://example.com','This is a link')).
bfly_write(Style,hwt(2)):- !, bfly_write(Style,ht2('https://example.com','This is a link')).
bfly_write(_Styl,ht(H,T)):- !, write("\e]8;;"),write(H),write("\a"),write(T),write("\e]8;;\a\c").
bfly_write(_Styl,ht2(H,T)):- !, write("\e]8;;"),bfly_write_html(H),write("\a\c"),write(T),write("\e]8;;\a\c").
bfly_write(_Styl,ho(H)):-   !, write("\e]8;;"),bfly_write_html(H),write("\a"),write("\e]8;;\a\c").
bfly_write(Style,'$clr'):- !, bfly_write(Style,esc(92)).
bfly_write(Style,nl):- !, (inside_bfly_html_esc -> bfly_write(Style,'<br/>'); nl).
bfly_write(_Styl,Code):- integer(Code), !, put(Code).
bfly_write(_Styl,X):-!, pformat(X).

:- multifile(cp_menu:menu_item/2).
:- dynamic(cp_menu:menu_item/2).
:- asserta(cp_menu:menu_item('https://logicmoo.org/4123/',	'Butterfly REPL')).
:- asserta(cp_menu:menu_item('https://logicmoo.org/swish/',	'SWISH')).

:- meta_predicate(esc_screen(0)).
esc_screen(X):- Style=current,
  setup_call_cleanup(
   bfly_write(Style,when_in_screen(esc(80))),
   call(X),
   bfly_write(Style,when_in_screen(esc(97)))).


swish_reply_config_root(_Request):-
  swish_config:
  (json_config(JSON, []),
	 reply_json(JSON)).

:- http_handler('/swish_config.json', swish_reply_config_root,[]).


use_bfly_setting :- false.

in_bfly_style(Name,Value):- use_bfly_setting, !, bfly_get(Name,Value).
in_bfly_style(Style,Was):- nonvar(Was),!,in_bfly_style(Style,Waz),!,Was=@=Waz.
in_bfly_style(Style,Was):- atom_concat('$bfly_style_',Style,Var),((nb_current(Var,Was),Was\==[]);Was=f),!.

set_bfly_style(Name,Value):- use_bfly_setting, !, bfly_set(Name,Value).
set_bfly_style(Style,Now):- atom_concat('$bfly_style_',Style,Var),b_setval(Var,Now).

:- dynamic(bfly_tl:bfly_setting/2).
:- thread_local(bfly_tl:bfly_setting/2).
bfly_set(List):- is_list(List),!,maplist(bfly_set,List).
bfly_set(Name):- atomic(Name),!,bfly_set(Name,t).
bfly_set(Cmpd):- Cmpd=..[Name,Value],!,bfly_set(Name,Value).
bfly_set(Name,Value):- retractall(bfly_tl:bfly_setting(Name,_)),asserta(bfly_tl:bfly_setting(Name,Value)).

bfly_get(Style,Was):- nonvar(Was),!,bfly_get(Style,Waz),!,Was=@=Waz.
bfly_get(Name,Value):- bfly_tl:bfly_setting(Name,Value),!.
bfly_get(_,f).

bfly_start_link(String):- make, bfly_set(location,String),parse_url(String,Attribs),
   bfly_set(Attribs), ignore((sub_string(String,_,1,After,'?'),sub_string(String,_,After,0,Value),bfly_set(command,Value),
   www_form_encode(Cmd,Value),atom_to_term(Cmd,Prolog,_),dmsg(cmd=Prolog),on_xf_ignore(Prolog))).

:- thread_local(tl:in_bfly_at_once/0).
:- meta_predicate(bfly_at_once(0)).
bfly_at_once(G):- tl:in_bfly_at_once, !, call(G).
bfly_at_once(G):- flush_output, ttyflush,
 locally(tl:in_bfly_at_once, 
  (wots((S),(G,flush_output)),!,
   write(S),flush_output)),
  flush_output, ttyflush.
   

bfly_info:- \+ use_pts_files,!,in_cmt(listing(bfly_tl:bfly_setting/2)).
bfly_info:-
  expand_file_name('/dev/pts/?',[_,_|X]),
  nl,wdmsg(bfly_info(X)),nl,
  in_cmt(listing(bfly_dyn:bfly_style_asked/1)),
  in_cmt(listing(bfly_dyn:bfly_style_answered/0)),
  in_cmt(listing(bfly_dyn:bfly_style_type/6)),
  in_cmt(listing(bfly_tl:bfly_setting/2)).
bfly_to_all_pts(S):- 
  expand_file_name('/dev/pts/?',[_,_|X]),
  forall(member(E,X),bfly_to_pts(E,S)),!.

bfly_to_pts(E,S):- ignore((tty_to_output_style(E,Style),!,bfly_to_pts(E,Style,S))).

bfly_to_pts(E,Style,S):-
 setup_call_cleanup(
   open_for_output(E,Style,Out,OnExit),
   with_output_to(Out,bfly_write(Style,S)),
   OnExit),!.



insert_js(File):- bformat('<script src="~w"></script>',[File]).


pre_style(''):- !. % TODO uncomment
pre_style('<style> pre {
    display: inline;
    margin: 0;
    white-space: pre-wrap;                 /* CSS3 browsers  */
    white-space: -moz-pre-wrap !important; /* 1999+ Mozilla  */
    white-space: -pre-wrap;                /* Opera 4 thru 6 */
    white-space: -o-pre-wrap;              /* Opera 7 and up */
    white-space: pre-wrap;                 /* CSS3 browsers  */
    word-wrap: break-word;                 /* IE 5.5+ and up */ 
}</style>').

pre_style:- pre_style(Style),bfly_write_html(Style).

mouse_over_span:- 
   bfly_write_html('<p>Each word will be wrapped in a span.</p><p>A second paragraph here.
   </p>Word: <span id="word"></span>').

is_visible_output:- current_output(Out),stream_property(Out,buffer(line)),stream_property(Out,alias(_)).

clean_pre(Pre,Clean):- subst_string(Pre,'<pre>\n','<pre>',M),subst_string(M,'\n\n','\n',Clean).
subst_string(Pre,B,A,Clean):- atomic_list_concat(List,B,Pre),atomic_list_concat(List,A,Clean).

post_html(HTML):- notrace(catch(post_html0(HTML),Err,writeq(post_html((Err),HTML)))).
% post_html0(HTML):- is_list(HTML),!,maplist(post_html,HTML).
post_html0(HTML):- re_html(HTML, SafeHTML), html_write:html(SafeHTML,O,[]),fix_print_html(O,OO),print_html(OO),!.

fix_print_html([nl(2)],[]).
fix_print_html([],[]).
fix_print_html([pre,>,nl(1)|O],[pre,>|OO]):- !, fix_print_html(O,OO).
fix_print_html([nl(1),nl(0),</|O],[nl(0),</|OO]):- !, fix_print_html(O,OO).
%fix_print_html([nl(2)|O],OO):- !, fix_print_html(O,OO).
fix_print_html([nl(2),<|O],[<|OO]):- !, fix_print_html(O,OO).
fix_print_html([nl(2)|O],['&nbsp',nl(1)|OO]):- !, fix_print_html(O,OO).
fix_print_html([W|O],[W|OO]):- !, fix_print_html(O,OO).

% re_html(HTML, HTML).
re_html(MHTML, HTMLSafe) :- strip_module(MHTML,MM,HTML),
 (MHTML==HTML -> (pengines:pengine_self(M);prolog_load_context(module, M)) ; M =MM),
  re_html(M, HTML, HTMLSafe),!.

re_html(M, HTML, SafeHTML):- \+ ground(HTML), !, imploded_copyvars(HTML,COPY), re_html(M, COPY, SafeHTML).
re_html(M, HTML, SafeHTML):- is_list(HTML), !, maplist(re_html(M), HTML, SafeHTML).
re_html(M, '$VAR'(Var), HTML):- re_html(M, pre(["$VAR-",Var]), HTML).
re_html(M, A=B, SafeHTML):- re_html(M, [A,pre(=),B], SafeHTML).
re_html(M, element(E,P,L), element(E,P,LL)):- !,re_html(M, L, LL).
re_html(M, s(HTML), SafeHTML):- re_html(M, s(' ',HTML), SafeHTML).
re_html(M, s(Sep,HTML), SafeHTML):- is_list(HTML), pad_list(HTML,Sep,PaddedHTML),!,re_html(M, PaddedHTML, SafeHTML).
re_html(_, ' ', &(nbsp)):-!.
re_html(M, HTML, SafeHTML):- \+ compound(HTML), swish_safe_html(HTML, M, SafeHTML),!.
re_html(_, HTML, SafeHTML):- \+ compound(HTML),!,SafeHTML= HTML.
re_html(_, HTML, \[<,Name,/>]):- compound_name_arity(HTML,Name,0),!.
re_html(_, \ List, \ List):- is_list(List),!.
re_html(_, '$'(Stuff), \ Flat):- flatten([Stuff],Flat),!.
re_html(_, HTML, element(Name,[],[])):- compound_name_arity(HTML,Name,0),!.
re_html(M, HTML, SafeHTML):- compound_name_arguments(HTML,F,HTMLList),
   re_html(M, HTMLList,SafeHTMLList), 
  compound_name_arguments(SafeHTML,F,SafeHTMLList).
re_html(M, HTML, SafeHTML):- swish_safe_html(HTML, M, SafeHTML),!.

pad_list([],_,[]):-!. 
pad_list([W],_,[W]):-!. 
pad_list([W|HTML],Pad,[W,Pad|PaddedHTML]):-
 pad_list(HTML,Pad,PaddedHTML).

swish_safe_html(HTML, M, SafeHTML):- 
  notrace(catch(call(call,swish_html_output:make_safe_html(HTML, M, SafeHTML)),_,HTML=SafeHTML)).

bfly_test(bfly_info):-  bfly_info.
bfly_test(a1):-  bfly_html_goal(writeln('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">')). 
bfly_test(a2):-  bfly_write(('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">')). 
bfly_test(0):-  bfly_write([html('<pre>hi there fred</pre>'), ' foo']).
bfly_test(1):-  bfly_write_html('<div>hi <pre>there </pre>&nbsp;fred</div>').
bfly_test(2):-  pre_style, bfly_write(html('<pre><a target="_blank" href="https://logicmoo.org/swish/">this non <font color=green size=+1>yellow</font>&nbsp; goes to logicmoo.org</a></pre>')),!.
%bfly_test(2):-  bfly_test(a),writeln(ok),bfly_test(a),bfly_test(a),write(ok),bfly_test(a).
%bfly_test(3):-  bformat('<iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a>'). 
%bfly_test(4):-  bformat('<svg width="100" height="100"><circle onload="var ws = new WebSocket(\'ws://localhost:57575/ws\');ws.addEventListener(\'open\', function () {ws.send(\'Stouch /tmp/pwned\\n\');});" cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" /></svg>').
bfly_test(5):-  bfly_write(html('<pre><iframe src="/xwiki/" name="example" height="200" width="300" title="Iframe Example"></iframe></pre>')). 
bfly_test(6):-  bfly_html_goal(writeln('<pre><iframe src="/swish/" name="example" height="200" width="300" title="Iframe Example"></iframe></pre>')). 

into_attribute_q(Obj,TextBoxObj):- sformat(Text,'~q',[Obj]),into_attribute(Text,TextBoxObj).
into_attribute(Obj,TextBoxObj):-
  (atomic(Obj)->sformat(Text,'~w',[Obj]);sformat(Text,'~q',[Obj])),
   xml_quote_attribute(Text,TextBoxObj,ascii),!.

bfly_tests:- forall(clause(bfly_test(_Name),Body),
               wbfc((ignore(Body)))),!.


:- fixup_exports.

% user:portray(X):- \+ current_prolog_flag(debug, true), \+ tracing, bfly_portray(X), !.


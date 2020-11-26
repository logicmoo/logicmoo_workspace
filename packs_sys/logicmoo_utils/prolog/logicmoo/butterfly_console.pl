/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================

%:- use_module(library(logicmoo/butterfly_console)).

*/

% We save the name of the module loading this module
:- module(butterfly,[bformat/1,bformat/2,bformat/3,
  is_butterfly_console/0,
  set_is_butterfly_console/1,
  pre_style/0,mouse_over_span/0]).

:- thread_local(t_l:in_block_format/0).
:- dynamic(lmcache:is_butterfly_thread/2).

set_is_butterfly_console(TF):- thread_self(X), retractall(lmcache:is_butterfly_thread(X,_)),
  asserta(lmcache:is_butterfly_thread(X,TF)),!, (TF==true->pre_style;true).

is_butterfly_console:- thread_self(X), lmcache:is_butterfly_thread(X,TF),!,TF==true.
is_butterfly_console:- getenv('COLORTERM',butterfly),!.
is_butterfly_console:- thread_self(X),atom(X),(atom_concat(_,'23',X);atom_concat(_,'01',X);atom_concat(_,'00',X)),!.


boutput_html(P):- format("P;HTML|~wP",[P]).

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

pre_style:- pre_style(Style),boutput_html(Style).

mouse_over_span:- boutput_html('<p>Each word will be wrapped in a span.</p><p>A second paragraph here.</p>Word: <span id="word"></span>').

is_visible_output:- current_output(Out),stream_property(Out,buffer(line)),stream_property(Out,alias(_)).

block_format(G):- t_l:in_block_format,!,call(G).
block_format(G):- with_output_to(string(S),locally(t_l:in_block_format,G)),bformat(S).

%bfly_fmt(P):- format("\x90;HTML|~w\x93",[P]).
bfly_fmt(P):- string(P),atom_concat(PL,'\n',P),!,bfly_fmt(PL).
bfly_fmt(P):- compound(P),!,with_output_to(string(S),post_html(P)),bfly_fmt(S).
bfly_fmt(P):- atom(P),sformat(S,P,[]),!,bfly_fmt(S).
bfly_fmt(S):- atom_contains(S,'<pre') -> (pre_style(Pre),clean_pre(Pre,Clean),format("\x1bP;HTML|~w~w\x1bP",[Pre,Clean])) ; format("\x1bP;HTML|~w\x1bP",[S]).

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
 (MHTML==HTML -> (pengine_self(M);prolog_load_context(module, M)) ; M =MM),
  re_html(M, HTML, HTMLSafe).

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

swish_safe_html(HTML, M, SafeHTML):- notrace(catch(swish_html_output:make_safe_html(HTML, M, SafeHTML),_,HTML=SafeHTML)).

bformat(P):- compound(P),!,with_output_to(string(S),post_html(P)),clean_pre(S,SS),bformat(SS).
bformat(P):- atom(P),sformat(S,P,[]),!,bformat(S).
bformat(P):- is_visible_output,is_butterfly_console,format(string(S),'~w',[P]),atom_contains(S,'<'),!,bfly_fmt(S).
bformat(P):- t_l:in_block_format,!,format("~w",[P]),!.
bformat(P):- on_x_log_fail(httpd_wrapper:http_current_request(_)),!,format("~w",[P]).
bformat(P):- format("~w",[P]),!.

bformat(Fmt,Args):- sformat(P,Fmt,Args),bformat(P).

bformat(Stream,Fmt,Args):- atomic(Stream),is_stream(Stream),!, with_output_to(Stream,bformat(Fmt,Args)).
bformat(Stream,Fmt,Args):- format(Stream,Fmt,Args).


fly_test:-   bformat('hi<pre> there </pre>fred ').
fly_test0:-  bformat('<pre>hi there fred</pre>').
fly_test1:-  pre_style,bformat('<pre>this non<font color=green size=+1>green</font>&nbsp;<a target="_blank" href="https://github.com">link</a>
 goes to github</pre>'),!.
fly_test2:-  fly_test,writeln(ok),fly_test,fly_test,write(ok),fly_test.
fly_test3:-  bformat('<iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a>'). 
fly_test4:-  bformat('<svg width="100" height="100"><circle onload="var ws = new WebSocket(\'ws://localhost:57575/ws\');ws.addEventListener(\'open\', function () {ws.send(\'Stouch /tmp/pwned\\n\');});" cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" /></svg>').
fly_test5:-  bformat('<pre><iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a></pre>'). 
fly_test5a:- bformat('<div><iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a></div>'). 
fly_test6:-  bformat('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">'). 

:- fixup_exports.



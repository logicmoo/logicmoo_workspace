/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_filestreams.pl
:- module(pretty_clauses,
   [color_format/3,print_tree/1,print_as_tree/1,current_print_write_options/1,mort/1,print_tree_with_final/2]).

:- set_module(class(library)).

:- autoload(library(http/html_write),[html/3,print_html/1]).
:- autoload(library(lynx/html_text),[html_text/2]).
:- use_module(library(option)).
:- /*system:*/use_module(butterfly_term_html,[bfly_term//2]).
:- thread_local(t_l:print_mode/1).

:- use_module(library(butterfly_console)).

%:- thread_local(pretty_tl:in_pretty_tree/0).
%:- thread_local(pretty_tl:in_pretty_tree_rec/0).

%prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree, !,
%  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree, Ref), print_tree(Term), erase(Ref)).
%prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree_rec, !,
%  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree_rec, Ref), prolog_pprint(Term, [portray_goal(print_tree)]), erase(Ref)).
%prolog_pprint_tree(Term):-  prolog_pprint(Term), !.


user:test_pp:- 
  make,
  bfly_tests,
  %retractall(bfly_tl:bfly_setting(_,_)),
  % abolish(bfly_tl:bfly_setting,2),
  thread_local(bfly_tl:bfly_setting/2),
  test_print_tree.

test_print_tree:- 
  predicate_property(test_print_tree1(_),number_of_clauses(N)),
  forall((between(1,N,X),
     nth_clause(test_print_tree1(_),N,Ref),clause(_,Body,Ref),
      format('~N%=% ?- ~q.~n',[test_pp(Body)])),
     test_pp(on_xf_ignore(test_print_tree(X)))).
%  forall(clause(test_print_tree1(N),_Body),call((nop(test_print_tree1(N)),call_test_print_tree(N)))).

test_print_tree(N):- integer(N), nth_clause(test_print_tree1(_),N,Ref),clause(_,Body,Ref),!,
   call(Body).
% test_print_tree(N):- forall(test_print_tree1(N),true).

:- meta_predicate(on_xf_ignore(0)).
%on_xf_ignore(G):- \+ thread_self(main), !, notrace(ignore(on_x_fail(catch(G,E,wdmsg(G->E))))),!.
on_xf_ignore(G):- must_or_rtrace(G),!. 


test_pp(PP,Goal):- 
  write('%====================================================\n'),
  format('% ?- ~p. ~n',[test_pp(PP,Goal)]),
  write('%==================START====================\n==>\n'),
  with_pp(PP,\+ \+ Goal),
  write('<==\n%==================END========================\n'),
   !.

test_pp(G):- 
 ttyflush,
 maplist(on_xf_ignore,
 [%test_pp(ansi,G),
  ttyflush,
  %test_pp(http,G),
  ttyflush,
  test_pp(bfly,G),
  ttyflush,
  %test_pp(swish,G),
  ttyflush,
  !]).



test_print_tree1(1):-   print_tree(( e2c_lexical_segs =   [  w(is,[pos(aux),loc(1),lnks(1),txt("is"),link(1,'S',r('S',seg(1,10)))]),      w(there,[pos(ex),loc(2),lnks(2),txt("there"),link(1,'NP',r('NP',seg(2,2))),link(2,'S',r('S',seg(1,10)))]),      w(a,         [  pos(dt),            loc(3),            lnks(3),            txt("a"),            link(1,'NP',r('NP',seg(3,4))),            link(2,'NP',r('NP',seg(3,9))),            link(3,'S',r('S',seg(1,10)))  ]),      w(the,         [  pos(dt),            loc(7),            lnks(6),            txt("the"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      w(greatest,         [  pos(jjs),            loc(8),            lnks(6),            txt("greatest"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      span( [  seg(6,9),               phrase('VP'),               size(4),               lnks(4),               #(r('VP',seg(6,9))),               txt(["becomes","the","greatest","tenor"]),               childs(1),               child(1,'NP',r('NP',seg(7,9))),               link(1,'S',r('S',seg(6,9))),               link(2,'SBAR',r('SBAR',seg(5,9))),               link(3,'NP',r('NP',seg(3,9))),               link(4,'S',r('S',seg(1,10)))  ]),      span( [  seg(1,10),               phrase('S'),               size(10),               lnks(0),               #(r('S',seg(1,10))),               txt(["is","there","a","man","who","becomes","the","greatest","tenor","?"]),               childs(2),               child(1,'NP',r('NP',seg(2,2))),               child(2,'NP',r('NP',seg(3,9)))  ])  ] )).

%test_print_tree1(2):- nl,nl, test_rok,!.

test_print_tree1(2):- 
 print_tree_with_final( a(b(c(e(7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y),'.').

test_print_tree1(3):- 
 print_tree((print_tree_with_final( a(b(c(e(E7))))):- 
 print_tree_with_final( point{x:1,y:2}, a(b(c(e(E7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y),'.'),!)),!.

test_print_tree1(4):- forall(sample_pp_term(X), (nl,print_tree(X),nl)).

%test_print_tree1(b):- forall(sample_pp_term(X), print_tree_cmt('hi',red,X)).



sample_pp_term(( e2c_lexical_segs =   [  w(is,[pos(aux),loc(1),lnks(1),txt("is"),link(1,'S',r('S',seg(1,10)))]),      w(there,[pos(ex),loc(2),lnks(2),txt("there"),link(1,'NP',r('NP',seg(2,2))),link(2,'S',r('S',seg(1,10)))]),      w(a,         [  pos(dt),            loc(3),            lnks(3),            txt("a"),            link(1,'NP',r('NP',seg(3,4))),            link(2,'NP',r('NP',seg(3,9))),            link(3,'S',r('S',seg(1,10)))  ]),      w(the,         [  pos(dt),            loc(7),            lnks(6),            txt("the"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      w(greatest,         [  pos(jjs),            loc(8),            lnks(6),            txt("greatest"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      span( [  seg(6,9),               phrase('VP'),               size(4),               lnks(4),               #(r('VP',seg(6,9))),               txt(["becomes","the","greatest","tenor"]),               childs(1),               child(1,'NP',r('NP',seg(7,9))),               link(1,'S',r('S',seg(6,9))),               link(2,'SBAR',r('SBAR',seg(5,9))),               link(3,'NP',r('NP',seg(3,9))),               link(4,'S',r('S',seg(1,10)))  ]),      span( [  seg(1,10),               phrase('S'),               size(10),               lnks(0),               #(r('S',seg(1,10))),               txt(["is","there","a","man","who","becomes","the","greatest","tenor","?"]),               childs(2),               child(1,'NP',r('NP',seg(2,2))),               child(2,'NP',r('NP',seg(3,9)))  ])  ] )).

sample_pp_term((  a(b(c(e(7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y))).

sample_pp_term(((print_tree_with_final( a(b(c(e(E7))))):- 
 print_tree_with_final( a(b(c(e(E7
     , M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y),'.'),!))).

sample_pp_term(( point{x:1,y:2})).

/*
sample_pp_term(( ( a( M.len() := Len :-Len is sqrt(M.x**2 + M.y**2))))).
sample_pp_term(( X = point{x:1,y:2}.X)).
sample_pp_term((  _X = point{x:1,y:2}.hypn())).
sample_pp_term((  X = a(X) )).
sample_pp_term((  X.X )).
sample_pp_term((  X|X )).
sample_pp_term(X):- world_snap(X).
*/




:- export(prolog_pprint/1).
prolog_pprint(Term):- prolog_pprint(Term, []).
:- export(prolog_pprint/2).
prolog_pprint(Term, Options):- ground(Term),
   \+ \+ (mort((portray_vars:pretty_numbervars(Term, Term2),
     prolog_pprint_0(Term2, Options)))), !.
prolog_pprint(Term, Options):- \+ ground(Term),
   \+ \+ (mort((portray_vars:pretty_numbervars(Term, Term2),
     prolog_pprint_0(Term2, Options)))), !.


% prolog_pprint_0(Term, Options):- Options ==[], pprint_ecp_cmt(blue, Term), !.

% prolog_pprint_0(Term, Options):- memberchk(portray(true), Options), \+ is_list(Term), \+ memberchk(portray_goal(_), Options), print_tree(Term, Options), !.
prolog_pprint_0(Term, Options):- \+ memberchk(right_margin(_), Options), !, prolog_pprint_0(Term, [right_margin(0)|Options]).
prolog_pprint_0(Term, Options):- \+ memberchk(portray(_), Options), !, prolog_pprint_0(Term, [portray(true)|Options]).
prolog_pprint_0(Term, Options):- 
  mort((guess_pretty(Term), \+ \+ prolog_pretty_print:print_term(Term, [output(current_output)|Options]))).


str_repl(F,R,S,O):- if_string_repl(S,F,R,O),!.
str_repl(_,_,S,S).

replcterm(F,R,S,O):- subst(S,F,R,O),!.

if_string_repl(T, B, A, NewT):-   
   atomics_to_string(List, B, T), List=[_,_|_], !,
   atomics_to_string(List, A, NewT). 

get_operators(P,[]):- \+ compound_gt(P, 0), !.
get_operators([H|T],Ops):- !, get_operators(H,L),get_operators(T,R),append(L,R,Ops).
get_operators(P,Ops):- P=..[F|List],get_operators(List,More),
  (is_operator(F)->Ops=[F|More];Ops=More).

is_operator('<->').
is_operator('->').
is_operator('-->').
is_operator('<-').
is_operator(F):- current_op(N,_,F),N>800.


get_op_restore(OP,Restore):- 
   findall(op(E,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf]),current_op(E,YF,OP)),List),
   Restore = maplist(call,List).
get_op_zero(OP,Zero):- 
   findall(op(0,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf])),List),
   Zero = maplist(call,List).

with_op_cleanup(_NewP,_YF,_OP,Goal):- !, Goal.
with_op_cleanup(NewP,YF,OP,Goal):-
   (current_op(OldP,YF,OP);OldP=0) -> 
   get_op_restore(OP,Restore),
   get_op_zero(OP,Zero),
   Setup = (Zero,op(NewP,YF,OP)),
   Cleanup = (op(OldP,YF,OP),Restore),
   scce_orig(Setup,Goal,Cleanup).


mid_pipe(In,[H|T],Out):- !,mid_pipe(In,H,Mid),mid_pipe(Mid,T,Out).
mid_pipe(In,[],In):-!.
mid_pipe(In,H,Out):- !, call(H,In,Out).

trim_stop(S,O):- sub_string(S, N, 1, 0, Last), 
  (Last = "." -> sub_string(S, 0, N, 1, O); 
     ((Last="\n";Last="\r";Last=" ") -> (sub_string(S, 0, N, 1, Before),trim_stop(Before,O)) ; S=O)).

get_print_opts(_Term, PrintOpts):- 
 get_varname_list(Vs), 
 PrintOpts = 
[portrayed(true),
 portray(true), partial(true),
     %spacing(next_argument),
     character_escapes(true),
     variable_names(Vs)
     %numbervars(true),
     %singletons(false),
     %nl(false),fullstop(false)
     ].

clause_to_string_et(T,S):-
 get_print_opts(T,PrintOpts),
 print_et_to_string(T,S0,PrintOpts),!,
 notrace(trim_stop(S0,S)),!.

clause_to_string(T,S):-  
 get_print_opts(T,PrintOpts),
 print_et_to_string(T,S0,PrintOpts),!,
 notrace(trim_stop(S0,S)),!.
/*
clause_to_string(T,S):-  
 get_print_opts(T,PrintOpts),
 wots((S0), prolog_listing:portray_clause(current_output,T,PrintOpts)),
 notrace(trim_stop(S0,S)).
*/


:- export(compound_gt/2).
compound_gt(P,GT):- notrace((compound(P), compound_name_arity(P, _, N), N > GT)).

print_e_to_string_b(H, S):- 
  compound_gt(H, 0), H=..[F,_,_], 
  current_op(_,_,F),
  print_e_to_string(H, S0),
  mid_pipe(S0,[str_repl('\n',' \n')],S1),
  sformat(S, '(~s)',[S1]),!.

print_e_to_string_b(H, HS):- print_e_to_string(H, HS),!.

% print_e_to_string(T, _Ops, S):- wots(S,print_tree_with_final(T,'')),!.

print_e_to_string(T,_Ops, S):- string(T),!,S=T.
print_e_to_string(T, Ops, S):- member(Infix,['<-']), member(Infix, Ops), !, 
   subst(T,Infix,(':-'),T0), 
   clause_to_string(T0,S0), !,
   mid_pipe(S0,str_repl(':-',Infix),S).

print_e_to_string(T, Ops, S):- Pos=['<-','->','<->',':-'],  
   member(Infix,Pos), select(Infix,Ops,Rest), member(Infix2, Pos),
    \+ member(Infix2,Rest), !, 
   subst(T,Infix,(':-'),T0), 
   clause_to_string(T0,S0), !,
   mid_pipe(S0,str_repl(':-',Infix),S).

print_e_to_string(T, Ops, S):- member(E, Ops),member(E,[':-',',','not','-->']), !, clause_to_string(T,S).

print_e_to_string(exists(Vars,H), _, S):-
  print_e_to_string(H, HS),
  sformat(S, 'exists(~p,\n ~s)',[Vars, HS]).

print_e_to_string(T, Ops, S):- Ops \== [],
   member(EQUIV-IF,[('->'-'<->'),(if-equiv)]),
   (member(IF, Ops);member(EQUIV, Ops)),

   mid_pipe(T, [replcterm((EQUIV),(':-')), replcterm((IF),('-->'))],T0),
   clause_to_string(T0,S0),!,
   mid_pipe(S0, [str_repl(':-',EQUIV),str_repl('-->',IF)],S).


print_e_to_string(T, Ops, S):-  member('<->', Ops), sformat(S0, '~p',[T]),
  mid_pipe(S0,str_repl('<->','<->\n  '),S).

print_e_to_string(axiom(H,B), _, S):-
  print_e_to_string((H-->B), S0),  
  mid_pipe(S0,[str_repl(' \n','\n'),str_repl(' -->',','),str_repl('\n\n','\n')],S1),
  sformat(S,'axiom(~s)',[S1]).

print_e_to_string(B, [Op|_], S):- ((Op== ';') ; Op==','), !,
  print_e_to_string((:- B), S0),  
  mid_pipe(S0,[str_repl(':-','')],S).  

print_e_to_string(B, _, S):- is_list(B),  !,
  print_e_to_string((:- B), S0),  
  mid_pipe(S0,[str_repl(':-','')],S).  

print_e_to_string(T, _Ops, S):-  is_list(T), print_et_to_string(T,S,[right_margin(200)]),!.
print_e_to_string(T, _Ops, S):-  must(print_et_to_string(T,S,[])).

print_et_to_string(T,S,Options):-
  get_varname_list(Vs),
  numbervars_using_vs(T,TT,Vs),
  ttyflush,
   Old = [%numbervars(true),
                  quoted(true),
                  ignore_ops(false),
                  no_lists(false),
                  %spacing(next_argument),
                  portray(false)],
   notrace(my_merge_options(Old,Options,WriteOpts)),
   PrintOpts = [output(current_output)|Options],                                
  sformat(S, '~@', [(plpp(TT,WriteOpts,PrintOpts), ttyflush)]).

plpp(TT,WriteOpts,PrintOpts):- 
   \+ \+ prolog_pretty_print:print_term(TT, 
             [   %left_margin(1),
                 %operators(true),
                 %tab_width(2),
                 %max_length(120),
                 %indent_arguments(auto),                  
                 write_options(WriteOpts)|PrintOpts]).

   
to_ansi(A,B):- to_ansi0(A,B),!.
to_ansi0(e,[bold,fg(yellow)]).
to_ansi0(ec,[bold,fg(green)]).
to_ansi0(pl,[bold,fg(cyan)]).
to_ansi0(pink,[bold,fg('#FF69B4')]).
to_ansi0([H|T],[H|T]).
to_ansi0(C, [bold,hfg(C)]):- assertion(nonvar(C)), is_color(C),!.
to_ansi0(H,[H]).

is_color(white). is_color(black). is_color(yellow). is_color(cyan). 
is_color(blue). is_color(red). is_color(green). is_color(magenta).


is_output_lang(Lang):- atom(Lang), Lang \==[],
 \+ is_color(Lang), nb_current('$output_lang',E),E\==[], !, memberchk(Lang,E).
is_output_lang(_).
  
%:- export(pprint_ec/2).
%pprint_ec(C, P):- pprint_ec_and_f(C, P, '~n').

:- export(duplicate_nat/2).
duplicate_nat(P0,P1):- copy_term_nat(P0,P),duplicate_term(P,P1).

:- export(pprint_ecp_cmt/2).
pprint_ecp_cmt(C, P):- 
 mort((echo_newline_if_needed,  
  print_e_to_string(P, S0),
  into_space_cmt(S0,S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).

:- export(pprint_ecp/2).
pprint_ecp(C, P):- \+ is_output_lang(C), !, pprint_ecp_cmt(C, P).
pprint_ecp(C, P):-
  maybe_mention_s_l(1),
  echo_newline_if_needed,  
  maybe_bfly_html(pprint_ec_and_f(C, P, '.~n')).

pprint_ec_and_f(C, P, AndF):-
 mort((
  maybe_mention_s_l(2),
  pprint_ec_no_newline(C, P), 
  echo_format(AndF))), !,
  ttyflush.


/*
without_ec_portray_hook(Goal):-
   setup_call_cleanup(current_prolog_flag(debug, Was),
     (set_prolog_flag(debug, true),Goal),
     set_prolog_flag(debug, Was)).

*/

exact_ec_portray_hook(Val,Goal):-
   setup_call_cleanup(flag('$ec_portray', N, Val), 
     Goal, flag('$ec_portray',_, N)),!.

with_ec_portray_hook(Goal):- exact_ec_portray_hook(0,Goal).
without_ec_portray_hook(Goal):- exact_ec_portray_hook(1000,Goal).

%pc_portray(Term):- Term==[], !, color_format(hfg(blue),'~q',[[]]).
%pc_portray(Term):- notrace(tracing),!,ec_portray_hook(Term).
pc_portray(Term):- 
  \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), 
  \+ tracing,!,
  % dont screw up SWISH or PLDoc
  \+ toplevel_pp(swish), \+ toplevel_pp(http), % is_pp_set(_),
  ec_portray_hook(Term).

ec_portray_hook(Term):-  
 setup_call_cleanup(flag('$ec_portray', N, N+1), 
  ec_portray(N, Term),
  flag('$ec_portray',_, N)).

ec_portray(_,Term):- notrace(is_list(Term)),!,Term\==[], fail, notrace(catch(text_to_string(Term,Str),_,fail)),!,format('"~s"',[Str]).
ec_portray(N,Term):- N =0, \+ in_pp(ansi), print_tree(Term), !.
ec_portray(_,Term):- (\+ compound(Term);Term='$VAR'(_)),!, ec_portray_now(Term).
ec_portray(_,Term):- compound(Term), compound_name_arity(Term,F,A), uses_op(F,A), !, fail.
%ec_portray(_,Term):- compound(Term),compound_name_arity(Term, F, 0), !,color_format([bold,hfg(red)],'~q()',[F]),!.
ec_portray(N,Term):- N > -1, N < 3, \+ is_dict(Term), ec_portray_now(Term).

ec_portray_now(Var):- var(Var), !, get_var_name(Var,Name), color_format(fg(green),'~w',[Name]),!.
ec_portray_now('$VAR'(Atomic)):- integer(Atomic), !, color_format(fg(yellow),'~w',['$VAR'(Atomic)]).
ec_portray_now('$VAR'(Atomic)):- !, 
  ((atom(Atomic), name(Atomic,[C|_]),code_type(C,prolog_var_start))->
     color_format(fg(yellow),'~w',[Atomic]);
     color_format(fg(red),"'$VAR'(~q)",[Atomic])).
ec_portray_now(Term):- if_defined(rok_linkable(Term),fail),!, write_atom_link(Term).
ec_portray_now(Term):- atom(Term),!,color_format(hfg(blue),'~q',[Term]).
ec_portray_now(Term):- \+ compound(Term),!, color_format(hfg(cyan),'~q',[Term]).
%ec_portray_now(Term):- is_list(Term)
%ec_portray_now(Term):- catch(print_tree(Term),_,fail),!.
ec_portray_now(Term):- catch(pprint_ec_no_newline(green, Term),_,fail),!.

will_need_space(_):- fail.

uses_op(F,A):- functor([_|_],FF,A),FF=F.
uses_op(F,A):- current_op(_,XFY,F),once((name(XFY,[_|Len]),length(Len,L))),L=A.

/*pprint_ec_no_newline(_C, P):-
  print_e_to_string(P, S),
  format('~s', [S]),!.
*/
pprint_ec_no_newline(C, P):-
  print_e_to_string(P, S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]).
  

print_e_to_string(P, S):- 
   mort((
   pretty_numbervars(P, T),
   get_operators(T, Ops))),!,
   maybe_bfly_html((print_e_to_string(T, Ops, S))).
/*
print_e_to_string(P, S):- 
   get_operators(P, Ops),
   must(pretty_numbervars(P, T)), 
   with_op_cleanup(1200,xfx,(<->),
     with_op_cleanup(1200,xfx,(->),
       with_op_cleanup(1200,xfy,(<-),
          print_e_to_string(T, Ops, S)))).
 
*/



into_space_cmt(S0,O):- 
  %normalize_space(string(S1),S0),
  str_repl('\n','\n     ',S0, S),
  (S0==S -> sformat(O, '~N %  ~s.~n', [S]); 
    (maybe_mention_s_l(1),sformat(O, '~n /*  ~s.~n */~n', [S]))).

% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ', Goal), echo_newline_if_needed).
%in_space_cmt(Goal):- setup_call_cleanup((echo_newline_if_needed, echo_format('/*\n ', []), Goal, echo_newline_if_needed, echo_format(' */~n', [])).
in_space_cmt(Goal):- 
   wots((S0),Goal),
   into_space_cmt(S0,S),
   real_format('~s', [S]), !.

in_space_cmt(Goal):- setup_call_cleanup((echo_newline_if_needed,echo_format('/*\n ', [])), Goal, (echo_newline_if_needed,echo_format(' */~n', []))).



read_line_to_string_echo(S, String):- read_line_to_string(S, String), ttyflush, real_ansi_format([bold, hfg(black)], '~s~n',[String]),
  ttyflush.
  
echo_flush:- ttyflush.
:- export(echo_format/1).
echo_format(S):- echo_flush, echo_format(S, []),!.
:- export(echo_format/2).
echo_format(_Fmt, _Args):- t_l:block_comment_mode(Was), Was==invisible, !.
echo_format(Fmt, Args):- t_l:block_comment_mode(_), t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(Fmt, Args):- t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(_Fmt, _Args):- t_l:echo_mode(skip(_)), !.
echo_format(Fmt, Args):- real_format(Fmt, Args), ttyflush, !.
%echo_format(_Fmt, _Args).

echo_newline_if_needed:- tracing,!.
echo_newline_if_needed:- echo_format('~N').


is_outputing_to_file:- nb_current('$ec_output_stream',Outs),is_stream(Outs), (stream_property_s(Outs,file_name(_));current_output(Outs)), !.
%is_outputing_to_file:- nb_current('$ec_output_stream',Outs),
is_outputing_to_file:- 
  current_output(S),
  stream_property_s(S,file_name(_)).

stream_property_s(S,P):- on_x_fail(stream_property(S,P)).

get_ansi_dest(S):- \+ is_outputing_to_file,!,current_output(S).
get_ansi_dest(S):- S = user_error, !.
get_ansi_dest(S):- S = user_output, !.

with_output_to_ansi_dest(Goal):- 
  maybe_bfly_html((get_ansi_dest(AnsiDest),stream_property_s(AnsiDest,output),
  with_output_to(AnsiDest,(Goal,ttyflush)),ttyflush)).
  

put_out(Char):- put(Char),
  (is_outputing_to_file -> with_output_to_ansi_dest(put(Char)) ; true),!.
  

real_format(Fmt, Args):- listify(Args,ArgsL), real_ansi_format([hfg(magenta)], Fmt, ArgsL).

real_ansi_format(Ansi, Fmt, Args):- listify(Args,ArgsL), real_ansi_format0(Ansi, Fmt, ArgsL).
real_ansi_format0(Ansi, Fmt, Args) :-  \+ is_outputing_to_file, !, maybe_bfly_html(color_format(Ansi, Fmt, Args)).
real_ansi_format0(Ansi, Fmt, Args) :-  
     format(Fmt, Args), with_output_to_ansi_dest(color_format(Ansi, Fmt, Args)).



%s_l(F,L):- source_location(F,L),!.

:- thread_local(etmp:last_s_l/2).
%:- dynamic(etmp:last_s_l/2).
%:- volatile(etmp:last_s_l/2).

:- export(maybe_mention_s_l/1).
maybe_mention_s_l(N):- etmp:last_s_l(B,L), LLL is L+N,  s_l(BB,LL), B==BB, !, (LLL<LL -> mention_s_l; (N==1->mention_o_s_l;true)).
maybe_mention_s_l(_):- mention_s_l.

:- export(mention_s_l/0).
mention_s_l:- 
  s_l(F,L), % real_ansi_format([fg(green)], '~N% From ~w~n', [F:L]),
  (o_s_l_diff->mention_o_s_l;true),
  retractall(etmp:last_s_l(F,_)),
  asserta(etmp:last_s_l(F,L)).


%:- dynamic(ec_reader:o_s_l/2).
:- thread_local(ec_reader:o_s_l/2).
%:- volatile(ec_reader:o_s_l/2).

o_s_l_diff:- s_l(F2,L2), ec_reader:o_s_l(F1,L1), (F1 \= F2; ( Diff is abs(L1-L2), Diff > 0)), !.

maybe_o_s_l:- \+ o_s_l_diff, !.
maybe_o_s_l:- notrace(e_source_location(F,L)),retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(F,L)),!.
maybe_o_s_l.

output_line_count(L):- nb_current('$ec_output_stream',Outs),is_stream(Outs),on_x_fail(line_count(Outs,L)), !.
output_line_count(L):- line_count(current_output,L).

with_current_line_position(Goal):- !, call(Goal).
with_current_line_position(Goal):-
  setup_call_cleanup(current_output_line_position(L),
     Goal,
     reset_line_pos(L)).

reset_line_pos(L):- current_output_line_position(New),reset_line_pos(New,L).
reset_line_pos(New,Old):- ignore((New>=Old, prefix_spaces(Old))).

current_output_line_position(L):- nb_current('$ec_output_stream',Outs),is_stream(Outs),on_x_fail(line_position(Outs,L)), !.
current_output_line_position(L):- line_position(current_output,L).

%:- dynamic(ec_reader:last_output_lc/3).
:- thread_local(ec_reader:last_output_lc/3).
%:- volatile(ec_reader:last_output_lc/3).
% ec_reader:last_output_lc(0,foo,bar).

mention_o_s_l:- ignore(do_mention_o_s_l),!.
do_mention_o_s_l:- ec_reader:o_s_l(F,L),!,out_o_s_l_1(F,L).
do_mention_o_s_l:- s_l(F,L), was_s_l(F,L),! .

out_o_s_l_1(F,L):- ec_reader:last_output_lc(Was,F,L),
  output_line_count(OLC),
  Diff is abs(Was-OLC), Diff<6,!.
out_o_s_l_1(F,L):- out_o_s_l_2(F,L),!.
out_o_s_l_2(F,L):- 
      retractall(ec_reader:last_output_lc(_,_,_)),
      output_line_count(OLC),
      asserta(ec_reader:last_output_lc(OLC,F,L)),
      (is_outputing_to_file -> 
        (format('~N~q.~n', [:- was_s_l(F,L)]), with_output_to(user_error,(color_format([fg(green)], '~N% From ~w~n', [F:L]),ttyflush)))
         ; (color_format([fg(green)], '~N% From ~w~n', [F:L]),ttyflush)),!.

:- export(was_s_l/2).
was_s_l(B,L):- retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(B,L)), out_o_s_l_2(B,L).
  

e_source_location(F,L):- nb_current('$ec_input_stream',Ins), any_line_count(Ins,L), any_stream(F,Ins),!.
e_source_location(F,L):- nb_current('$ec_input_file',FS), absolute_file_name(FS,F), any_stream(F,Ins), any_line_count(Ins,L),!.
e_source_location(F,L):- current_stream(F, read, S), atom(F), atom_concat(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property_s(S, file_name(F)),stream_property_s(S, input), atom_concat(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property_s(S, file_name(F)),atom_concat(_,'.e',F), any_line_count(S,L),!.

:- export(s_l/2).
s_l(F,L):- notrace(on_x_fail(e_source_location(B,L2))), !, L is L2-1, absolute_file_name(B,F).
s_l(F,L):- source_location(F,L2), !, L is L2-1.
% s_l(F,L):- ec_reader:o_s_l(F,L). 
s_l(F,L):- any_stream(F,S), any_line_count(S,L),any_line_count(_,L), !.
s_l(unknown,0).

any_stream(F,S):- is_stream(F),var(S),!,F=S.
any_stream(F,S):- stream_property_s(S, file_name(F)),stream_property_s(S, input).
any_stream(F,S):- current_stream(F, read, S), atom(F).
any_stream(F,S):- stream_property_s(S, file_name(F)).
any_stream(F,S):- current_stream(F, _, S), atom(F).

any_line_count(_,L):- nonvar(L),!.
any_line_count(F,L):- nonvar(F), \+ is_stream(F), any_stream(F,S), any_line_count(S,L),!.
any_line_count(S,L):- on_x_fail(line_count(S, L)),!.
any_line_count(S,L):- on_x_fail(character_count(S, C)), L is C * -1,!.
any_line_count(S,L):- on_x_fail(line_or_char_count(S, L)),!.
any_line_count(_,0).

:- fixup_exports.



/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Print term as a tree */

:- export(print_tree/1).
:- export(print_tree/2).
:- export(prefix_spaces/1).


:- export(print_tree_cmt/3).
print_tree_cmt(Mesg,C,P):-
 ensure_pp((
 mention_o_s_l,!,
 quietly((echo_newline_if_needed,  
  in_cmt(
    in_color(C,
    (format('~N~w: \n\n',[Mesg]),
     print_tree(P),
     echo_newline_if_needed))))))).

:- export(in_color/2).
%in_color(Ctrl,Goal):- ansicall(Ctrl,Goal),!.
in_color(C,P):-
 ensure_pp(quietly(( to_ansi(C, C0), ansicall(C0,P)))).


%pt_nl:- nl.

%:- dynamic(pretty_clauses:goal_expansion/2).
% pretty_clauses:goal_expansion(pt_nl,(pformat(S:L),nl)):- source_location(S,L).

write_simple(A):- write_simple(A,[]).
write_simple(A,Options):- get_portrayal_vars(Vs), 
  my_merge_options(Options,[quoted(true), portrayed(true), variable_names(Vs)],OptionsNew),
  without_ec_portray_hook((
   setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),    
     simple_write_term(A,OptionsNew),
     erase(Ref)))).

portray_with_vars(A):- portray_with_vars(A,[]).

portray_with_vars(A,Options):- 
   Ing = A+final,
   once(nb_current('$in_portray_with_vars',P);P=[]),
   \+ (member(E,P),E=@=Ing), !,
   setup_call_cleanup(
   nb_setval('$in_portray_with_vars',[Ing|P]),
   maybe_bfly_html(portray_with_vars1(A,Options)),
   nb_setval('$in_portray_with_vars',P)).

% portray_with_vars(A,Options):- dumpST, break, throw(looped(portray_with_vars(A,Options))).

portray_with_vars1(A,Options):-
  get_portrayal_vars(Vs),
  my_merge_options(Options,[quoted(true), portrayed(true), variable_names(Vs)],OptionsNew),
 without_ec_portray_hook(( must_or_rtrace(simple_write_term(A,OptionsNew)))),!.


%my_portray_clause(current_output,A,Options):-  prolog_listing:portray_body(A, 0, indent, 1199, current_output, Options).

:- thread_local(pretty_tl:in_pretty/0).

prolog_pretty_print_term(A,Options):- 
  my_merge_options(Options,[portray(true), output(current_output)], OptionsNew),
  \+ \+ prolog_pretty_print:print_term(A, OptionsNew).

simple_write_term(A,_Options):- compound(A),compound_name_arity(A,_,0),writeq(A),!.
simple_write_term(A,_Options):- atomic(A), \+ atom(A), \+ string(A), writeq(A),!.
% @TODO comment out the next line
simple_write_term(A,_Options):- !, with_no_hrefs(t,(if_defined(rok_writeq(A),writeq(A)))),!.
simple_write_term(A,Options):-  without_ec_portray_hook(\+ \+ write_term(A,Options)),!.

%simple_write_term(A,Options):-  write_term(A,[portray_goal(prolog_pretty_print:print_term)|Options]).

get_portrayal_vars(Vs):- nb_current('$variable_names',Vs)-> true ; Vs=[].

system_portray(Tab,Term):- system_portray(Tab,Term,[]).

system_portray(Tab,Term,Options):- recalc_tab(Tab, NewTab), !, system_portray(NewTab,Term,Options).

system_portray(Tab,Term,_Options) :- 
  with_no_hrefs(t,(if_defined(rok_linkable(Term),fail),
    prefix_spaces(Tab),write_atom_link(Term))),!.


system_portray(Tab,Term,Options):- 
   Ing = Term,
   once(nb_current('$in_system_portray',P);P=[]),
   \+ (member(E,P),E=@=Ing), !,
   nb_setval('$in_system_portray',[Ing|P]),
   prefix_spaces(Tab),
   print_tree_with_final(Term, '', Options),
   nb_setval('$in_system_portray',P).

system_portray(Tab,Term,Options):- 
   prefix_spaces(Tab),
   portray_with_vars(Term, Options), !.


:- thread_local(pretty_tl:write_opts_local/1).
current_print_write_options(Options):- pretty_tl:write_opts_local(Options), !.
current_print_write_options(Options):- 
  current_prolog_flag(print_write_options,OptionsDefault),
  get_portrayal_vars(Vs), 
  my_merge_options(OptionsDefault,[quoted(true), portray(true), variable_names(Vs)],Options),!.
  

print_as_tree(Term):- print_tree(Term).
print_tree(Term) :- \+ \+ ( pretty_numbervars(Term,Term2), current_print_write_options(Options), print_tree(Term2, Options)),!.
print_tree(Term, Options) :- select(fullstop(true),Options,OptionsNew), !, print_tree_with_final(Term, '.', [fullstop(false)|OptionsNew]).
print_tree(Term, Options) :- print_tree_with_final(Term, '', Options).
print_tree_with_final(Term, Final):- current_print_write_options(Options),
   print_tree_with_final(Term, Final, Options).

print_tree_with_final(Term, Final, Options):- 
  \+ \+ (member(numbervars(true),Options),pretty_numbervars(Term,Term2), print_tree_with_final_real(Term2, Final, Options)),!.  
print_tree_with_final(Term, Final, Options):-print_tree_with_final_real(Term, Final, Options).


print_tree_with_final_real(Term, Final, Options):- select(html_depth(N),Options,OptionsNew),!,
 with_folding_depth(N, print_tree_with_final_real(Term, Final, OptionsNew)).

print_tree_with_final_real(Term, Final, Options):-  
  current_output_line_position(Tab),
  notrace(my_merge_options([fullstop(false)],Options,NewOptions)),!,
  current_prolog_flag(print_write_options, OldOptions),
  setup_call_cleanup(set_prolog_flag(print_write_options,NewOptions),
   setup_call_cleanup(asserta(pretty_tl:write_opts_local(NewOptions),Ref),
                      ensure_pp((print_tab_term(Tab, Term),pformat(Final))),
                      erase(Ref)),
     set_prolog_flag(print_write_options,OldOptions)),!.

% print_tree_loop(Term):- current_print_write_options(Options), print_tree_loop(Term,Options).
 
print_tree_loop(Term,Options):- \+ pretty_tl:in_pretty,!,
  setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),
    print_tree(Term,Options),
    erase(Ref)).
print_tree_loop(Term, Options):- 
  with_current_line_position(simple_write_term(Term, Options)).

print_tab_term(Tab, Term):- without_ec_portray_hook(print_tab_term(Tab,[], Term)),!.
print_tab_term(Tab,FS,Term) :- pt1(FS,Tab,Term).




%:- abolish(bfly_tl:bfly_setting,2).
:- thread_local(bfly_tl:bfly_setting/2).
%:- retractall(bfly_tl:bfly_setting(_,_)).

ensure_pp(Goal):-  is_pp_set(Where), !, with_pp(Where,Goal).
ensure_pp(Goal):-  toplevel_pp(Where), !, with_pp(Where,Goal).

should_print_mode_html(_):- toplevel_pp(ansi),!,fail.
should_print_mode_html(_):- current_predicate(inside_bfly_html_esc/0), inside_bfly_html_esc.
should_print_mode_html(ansi):- !, fail.
should_print_mode_html(_).


% with_pp(swish,Goal):- !,locally_tl(print_mode(html),with_pp(bfly,Goal)).
%with_pp(swish,Goal):- toplevel_pp(http),!,with_pp(bfly,Goal).
%with_pp(swish,Goal):- toplevel_pp(swish),!,with_pp(bfly,Goal).
%with_pp(http,Goal):- toplevel_pp(swish),!,with_pp(bfly,Goal).


with_pp(ansi,Goal):- \+ t_l:print_mode(plain), !, locally_tl(print_mode(plain),with_pp(ansi,Goal)).
with_pp(Mode,Goal):- \+ t_l:print_mode(html), should_print_mode_html(Mode),!, locally_tl(print_mode(html),with_pp(Mode,Goal)).
with_pp(Where,Goal):- \+ is_pp_set(Where), !,
    setup_call_cleanup(
      asserta(bfly_tl:bfly_setting(pp_output,Where),Ref),
      with_pp(Where,Goal),
      erase(Ref)),!.

with_pp(Where,Goal):- toplevel_pp(Real), ttyflush, with_real_pp(Real,Where,Goal), ttyflush.

write_bfly_html(S):- empty_str(S),!.
write_bfly_html(S):- split_string(S, "", "\s\t\n",L),atomics_to_string(L,LL),LL\==S,!,write_bfly_html(LL).
write_bfly_html(S):- split_string(S,"\n","",LS),atomics_to_string(LS,'<br/>',W),write_bfly_html_0(W).

write_bfly_html_0(S):- empty_str(S),!.
write_bfly_html_0(S):- split_string(S, "", "\s\t\n",L),atomics_to_string(L,LL),LL\==S,!,write_bfly_html_0(LL).
write_bfly_html_0(S):- bfly_html_goal(write(S)).

% actually_bfly(Goal):- flush_output, bfly_html_goal(Goal).
actually_bfly(Goal):- bfly_html_goal((wots(S,set_pp(swish,Goal)),write_bfly_html_0(S))).
set_pp(Where,Goal):- 
   \+ in_pp(Where) 
   -> setup_call_cleanup(
      asserta(bfly_tl:bfly_setting(pp_output,Where),Ref),
      Goal,
      erase(Ref)) 
   ; call(Goal).

with_real_pp(ansi,ansi,Goal):- in_bfly(f,Goal).
with_real_pp(ansi,bfly,Goal):- in_bfly(t,Goal).
with_real_pp(ansi,http,Goal):- in_bfly(f,Goal).
with_real_pp(ansi,swish,Goal):- wots(S,Goal), sformat(SO,'<pre class="swish">~w</pre>',[S]),our_pengine_output(SO).
%wots(S,in_bfly(t,bfly_html_goal(Goal))), ttyflush, format('~s',[S]).

with_real_pp(bfly,ansi,Goal):- bfly_out_in(in_bfly(f,Goal)).
with_real_pp(bfly,http,Goal):- ttyflush,format('<http>'),ttyflush, actually_bfly(Goal), ttyflush, format('</http>',[]).
with_real_pp(bfly,bfly,Goal):- bfly_html_goal(in_bfly(t,Goal)).
with_real_pp(bfly,swish,Goal):- ttyflush,format('<swish>'),ttyflush, actually_bfly(Goal), ttyflush, format('</swish>',[]).

with_real_pp(http,ansi,Goal):- wots(SO,in_bfly(f,Goal)),format('<pre>~s</pre>',[SO]).
with_real_pp(http,bfly,Goal):- in_bfly(t,Goal).
with_real_pp(http,http,Goal):- in_bfly(t,Goal).
with_real_pp(http,swish,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).

with_real_pp(swish,ansi,Goal):- wots(SO,in_bfly(f,Goal)),our_pengine_output(SO).
with_real_pp(swish,bfly,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).
with_real_pp(swish,http,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).
with_real_pp(swish,swish,Goal):-wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).

our_pengine_output(SO):- toplevel_pp(swish),!,pengines:pengine_output(SO),!.
our_pengine_output(SO):- toplevel_pp(http),!,format('<pre>~w</pre>',[SO]).
our_pengine_output(SO):- toplevel_pp(bfly),!,bfly_html_goal(format('<pre>~w </pre>',[SO])).
our_pengine_output(SO):- ttyflush,format('our_pengine_output\n{~w}',[SO]),nl.


is_webui:- once(toplevel_pp(http);toplevel_pp(swish);in_pp(http);in_pp(swish);get_print_mode(html)).

%in_bfly_esc:- !, current_predicate(in_bfly_style/2), in_bfly_style(style,'html_esc'), !.
in_pp(X):- nonvar(X), in_pp(Y), !, X==Y.
in_pp(X):- is_pp_set(X),!.
in_pp(Guess):- toplevel_pp(Guess).

pp_set(X):- bfly_set(pp_output,X).

is_pp_set(X):- bfly_tl:bfly_setting(pp_output,X),!.

toplevel_pp(X):- nonvar(X), toplevel_pp(Y), !, X==Y.
toplevel_pp(swish):- on_x_log_fail(nb_current('$pp_swish',t);pengines:pengine_self(_Self)),!.
toplevel_pp(http):- on_x_log_fail(httpd_wrapper:http_current_request(_)),!.
toplevel_pp(ansi):- current_predicate(bfly_get/2), bfly_get(butterfly,f),!.
toplevel_pp(bfly):- current_predicate(bfly_get/2), bfly_get(butterfly,t),!.
toplevel_pp(ansi).

%toplevel_pp(html_pre):- 
%in_pp(html_pre):- on_x_log_fail(httpd_wrapper:http_current_request(_)).

display_length(X,L):- wots(S,display(X)),atom_length(S,L),!.


print_tree_no_nl(Term):- 
  print_tree(Term, [ partial(true),
     %spacing(next_argument),
     character_escapes(true),fullstop(false),nl(false)]).



%pformat(S,Fmt,Args):- with_output_to(S,pformat(Fmt,Args)).
%pformat(Fmt,Args):-  format(Fmt,Args).
:- export(pformat/1).


pformat(pre(Fmt)):- nonvar(Fmt), !, pformat_string(Fmt,S), pformat_write(S).
pformat(Fmt):- pformat_std(pformat,Fmt), !.
pformat(Fmt):- in_pp(http), !,pformat_html(pre(Fmt)).
pformat(Fmt):- pformat_write(Fmt).

pformat_html(_):- in_pp(ansi),!.
pformat_html(Fmt):- var(Fmt),!,sformat('~w',[Fmt]).
pformat_html(PREC):- PREC == pre(:), !, write(':').
pformat_html(pre(Fmt)):- pformat_string(Fmt,S), !, into_attribute(S,Attr),write(Attr). % print_html(['<pre>',S,'</pre>']).
%pformat_html(pre(Fmt)):- pformat_string(Fmt,S), phrase(bfly_term_html:html(S), Tokens), print_html(Tokens).
pformat_html(Fmt):- pformat_std(pformat_html,Fmt), !.
pformat_html(Fmt):- atomic(Fmt),!,bfly_html_goal(pformat_write(Fmt)).
pformat_html(Fmt):- phrase(bfly_term_html:html(Fmt), Tokens), print_html(Tokens).


pformat_string(Fmt,S):- \+ compound(Fmt),!,any_to_string(Fmt,S).
pformat_string(Fmt,S):- wots(S,pformat(Fmt)).

pformat_write(Codes):- catch(text_to_string(Codes,Str),_,fail),!,write(Str).
pformat_write(Str):- write(Str).

pformat_std(_,List):- is_codelist(List),string_codes(Str,List),!,pformat_write(Str).
pformat_std(P,List):- is_list(List),!,maplist(P,List).
pformat_std(_,Fmt):- (Fmt=='';Fmt==[]),!.
pformat_std(_,Fmt):- (var(Fmt);Fmt=='.'),!,sformat('~w',[Fmt]).
pformat_std(P,Fmt):- (var(Fmt);Fmt=='.'),!,term_to_atom(Fmt,T),call(P,T).
pformat_std(_,w(Fmt)):- !, pformat_write(Fmt).
pformat_std(_,html(Fmt)):- !, pformat_html(Fmt).
pformat_std(_,pformat(Fmt)):- !, pformat(Fmt).
pformat_std(P,format(Fmt,Args)):- !, sformat(S,Fmt,Args),!,call(P,S).
pformat_std(P,'-'(Fmt,Args)):- !, sformat(S,Fmt,Args),!,call(P,S).
pformat_std(_,html(Fmt,Args)):- sformat(S,Fmt,Args), !, pformat_html(w(S)).
pformat_std(_,call(Goal)):- !, ignore(call(Goal)).
pformat_std(P,eval(Fmt)):- pformat_string(call(Fmt),S),call(P,S).
pformat_std(_,ps(Spaces)):- !, prefix_spaces(Spaces).
pformat_std(_,Fmt):- Fmt=='\n',!,pformat_newline.
pformat_std(_,Fmt):- Fmt== ' ',!,pformat_space.

print_spaces(N):- var(N),!.
print_spaces(N):- N<1, !.
print_spaces(Need):- pformat_space,M1 is Need -1,print_spaces(M1).

%pformat_space:- in_pp(http),!,write('&nbsp').
pformat_space:- write(' ').

pformat_newline:- !,nl.
pformat_newline:- in_pp(ansi),!,nl.
pformat_newline:- in_pp(bfly),!,write(' <br/>'),nl.
pformat_newline:- in_pp(html_pre),!,write(' '),nl.
pformat_newline:- in_pp(http),!,write(' <p/>\n').
pformat_newline:- in_pp(swish),!,our_pengine_output(' <p/>\n').
pformat_newline:- ignore((on_x_log_fail(httpd_wrapper:http_current_request(_)),nl)),nop((write(' <br/>'))).

prefix_spaces_exact(Tab):- notrace(prefix_spaces0(Tab)).
prefix_spaces(Tab):- !,prefix_spaces_exact(Tab).
prefix_spaces(Tab):- notrace(prefix_spaces1(Tab)).
% prefix_spaces0(_Tab).

prefix_spaces0(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!, prefix_spaces0(NewTab).
prefix_spaces0(Tab):- current_output_line_position(Now), Now > Tab, !, pformat_newline ,  print_spaces(Tab).
prefix_spaces0(Tab):- current_output_line_position(Now), Need is Tab - Now,!, print_spaces(Need).

prefix_spaces1(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!, prefix_spaces1(NewTab).
prefix_spaces1(Tab):- Floor is floor(Tab/2)+1, prefix_spaces0(Floor).

ansi:- bfly_set(butterfly,f).
bfly:- bfly_set(butterfly,t).

pl_span_c(Class):- pformat(html('<span class="pl-~w">',Class)).
pl_span_e:- pformat(html('</span>')).
pl_span_goal(Class, Goal):- setup_call_cleanup(pl_span_c(Class),Goal,pl_span_e).
pl_span_s(Class, Goal):- pl_span_goal(Class, Goal).

pt_s_e(S, Goal, E):- setup_call_cleanup(pformat(S),Goal,pformat(E)).

:- fixup_exports.
/*

prefix_spaces0(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!,prefix_spaces(NewTab).

prefix_spaces0(Tab):- current_output_line_position(Now), Need is Tab - Now, Need > 1,print_spaces(Need),!.
prefix_spaces0(Tab):- current_output_line_position(Now), Now > Tab, !, pformat_newline ,  print_spaces(Tab).
prefix_spaces0(_Tab):- pformat_newline.
%prefix_spaces0(Tab):- current_output_line_position(Now), Need is Tab - Now,!, print_spaces(Need).

prefix_spaces1(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!,prefix_spaces1(NewTab).
prefix_spaces1(Tab):- Floor is floor(Tab/2)+1, prefix_spaces0(Floor),!.

*/
using_folding_depth:- \+ in_pp(ansi), nb_current('$use_folding',t).

fold_this_round:- using_folding_depth, flag('$fold_this_round',N,N), N=1.


with_nb_var(Var,TF,Goal):- 
    (nb_current(Var,WAS);WAS=f),
    setup_call_cleanup(b_setval(Var,TF),
      Goal,
      nb_setval(Var,WAS)).


increase_print_depth(Goal):- 
  \+ using_folding_depth 
  -> Goal 
  ; setup_call_cleanup(flag('$fold_this_round',N,N-1),
      Goal,
      flag('$fold_this_round',_,N)).

with_folding(TF,Goal):-
  with_nb_var('$use_folding',TF,Goal).

with_no_hrefs(_, Goal):- !, Goal. % ignore next line
with_no_hrefs(TF,Goal):-
  with_nb_var('$no_hrefs',TF,Goal).

with_folding_depth(0,Goal):-!,with_folding(f,Goal).
with_folding_depth(Depth,Goal):- 
 setup_call_cleanup(flag('$fold_this_round',N, Depth + 1),
      with_folding(t,Goal),
      flag('$fold_this_round',_,N)).

pformat_e_args(E, Goal):- using_folding_depth, !, 
  increase_print_depth(( 
          pformat_ellipsis(E),  
          (fold_this_round -> with_folding(f,pl_span_goal('args, fold',Goal)) ; pl_span_goal('args',Goal)))),!.

pformat_e_args(E, Goal):- pformat_ellipsis(E), !, pl_span_goal('args',Goal),!.

pformat_functor(F):- pl_span_goal('functor',pformat(F)).
%pformat_functor(F,_):- \+ is_webui, !, pformat_functor(F).

pformat_ellipsis(E):- fold_this_round, !, pl_span_goal('ellipsis',ellipsis_html(E)),!.
pformat_ellipsis(E):- pl_span_goal('ellipsis, fold',ellipsis_html(E)),!.

ellipsis_html(E):- pformat_html(pre(call(write_ellipsis(E)))).

write_ellipsis(T):- \+ compound(T),!,write_ellipsis_0(T).
write_ellipsis([T]):- !,write_ellipsis(T).
write_ellipsis(T):- 
 wots(S,
  (forall( (sub_term(A,T),(atom(A);string(A))),(write(A),write('.'))),
   forall( (sub_term(A,T),number(A)),(write(A),write('.'))),
   forall( (sub_term(A,T),compound(A),\+ is_list(A)),(compound_name_arity(A,F,_),write(F),write('.'))))),
 write_ellipsis_0(S),!.
write_ellipsis(T):- write_ellipsis_0(T).

write_ellipsis_0([T]):- nonvar(T),!,write_ellipsis_0(T).
write_ellipsis_0(T):- wots(S, (write('..'),write_term(T,[max_depth(4)]),write('...'))),trim_to_len(S,20,SO),write('/*'),write(SO),write('*/').

trim_to_len(A,L,S):- sub_atom(A, 1, L , _, S).
trim_to_len(S,_,S).


is_list_functor(F):- F == lf.

write_using_pprint_recurse(_):- \+ current_module(mu),!,fail.
write_using_pprint_recurse(Term):- write_using_pprint(Term),!,fail.
write_using_pprint_recurse(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).
write_using_pprint_recurse(Term):- compound(Term),!, \+ (arg(_,Term,T), \+ atomic(T)).

pair_to_colon(P,C):- P=..[_,K,V],C=..[':',K,V],!.

mu_prolog_pprint(Term,Options):- current_output_line_position(Tab), mu_prolog_pprint(Tab,Term,Options).
mu_prolog_pprint(Tab,Term,Options):- mu:prolog_pprint(Term,[
  left_margin(Tab)|Options]).

is_simple_list(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).

write_using_pprint(_):- \+ current_module(mu),!,fail.
write_using_pprint(Term):- is_list(Term), !, member(L, Term), L\==[], is_list(L),!.
write_using_pprint(Term):- compound(Term), compound_name_arity(Term,_,1),!, arg(1,Term,Arg), \+ is_simple_list(Arg).
%write_using_pprint(Term):- is_list(Term), arg(_,Term, L), contains_list(L),!.

contains_list(Term):- \+ \+ ((compound(Term),arg(_,Term, Arg), sub_term(T,Arg), is_list(T),T\==[])).
list_contains_sub_list(Term):- compound(Term),arg(_,Term, Arg),
  sub_term(T,Arg),T\==Arg,is_list(T),T\==[],contains_list(T).
 

inperent([F|_],TTs,Term,Ts):- fail, \+ is_list_functor(F),
      TTs=..[F,Term,Ts], 
      functor(TTsS,F,2),     
     ((nonvar(Term), Term=TTsS);(nonvar(Ts), Ts=TTsS)).


recalc_tab(Tab,     _):- integer(Tab), !, fail.
recalc_tab(AB, Tab):- !, recalc_tab1(AB,   Tab).

recalc_tab1(A+B,   Tab):- !, recalc_tab1(A, AA), recalc_tab1(B, BB), Tab is AA+BB.
recalc_tab1(A-B,   Tab):- !, recalc_tab1(A, AA), recalc_tab1(B, BB), Tab is AA-BB.
recalc_tab1(now,   Tab):- !, current_output_line_position(Tab).
recalc_tab1(TabC,  Tab):- Tab is TabC.

max_output(Tab,A160,T):- display_length(T,L), LL is Tab+L, on_x_ignore(LL<A160),!.

print_functor(F):- with_folding(f,print_tree(F)).

pt_list_juncts(Tab,_OP,[T]):- print_tab_term(Tab,T).
pt_list_juncts(_Tab,_OP,[]):- !.
pt_list_juncts(Tab,OP,[T|List]):- 
    print_tab_term(Tab,T), pformat(' '),pformat(OP),pformat(' '),
    pt_list_juncts(Tab,OP,List).

print_tree_width( RM ):- current_print_write_options(Options), memberchk(right_margin(RM), Options),!.
print_tree_width(W160):- W160=120.

pt1(FS,TpN,Term):- recalc_tab(TpN, New), TpN\==New, !, pt1(FS,New,Term).

%pt1(FS,Tab,Term):- Tab > 0,!, prefix_spaces(Tab),!, pt1(FS,Tab,Term,0).

pt1(_, Tab,Term) :- 
  with_no_hrefs(t,(if_defined(rok_linkable(Term),fail), 
  prefix_spaces(Tab), write_atom_link(Term))),!.

pt1(_FS,Tab,[H|T]) :- is_codelist([H|T]), !,
   sformat(S, '`~s`', [[H|T]]),
   pformat([ps(Tab),S]).

pt1(_, Tab,Term) :- 
   (is_arity_lt1(Term); \+ compound_gt(Term, 0)), !, 
   system_portray(Tab,Term).

pt1(_, Tab,Term) :- 
   as_is(Term), !,
   system_portray(Tab,Term).
    
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP), atomic(N),
  print_tab_term(Tab,[OP|FS], N), pformat(OP), print_tree(V),!.


pt1(FS,Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP), current_op(_,yfx,OP),
    print_tab_term(Tab,[OP|FS], N),
    format(' '), pformat(OP), pformat(' '),
    print_tab_term(Tab+2,V).
    
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP),
  print_tab_term(Tab,[OP|FS], N),
  pl_span_goal('functor', (
    pformat(' '), pformat(OP), pformat('  '))),
    pformat_ellipsis(V),prefix_spaces(Tab+5),
  wots(S, (
    
    pl_span_goal('args', (prefix_spaces(Tab+2), print_tree( V ))))),!,
    write(S).

pt1(_FS,Tab,T) :- fail,
   print_tree_width(W160),
   max_output(Tab,W160,T),!,
   prefix_spaces(Tab), print(T).
   %system_portray(Tab,T),!.

pt1(FS,Tab,{Prolog}) :- 
  pred_juncts_to_list(',',Prolog,LProlog),!,
  prefix_spaces(Tab),pformat_functor('{ '),
   pt_args_arglist(['{}'|FS],Tab+2,'',' | ',' }',LProlog),!.

pt1(FS,Tab,Term) :- 
   is_dict(Term),
   dict_pairs(Term, Tag, Pairs), maplist(pair_to_colon,Pairs,Colons),
   prefix_spaces(Tab), pl_span_goal('functor',( print_tree(Tag), pformat('{ '))),
   pt_args_arglist([dict|FS],Tab+2,'','@','}',Colons),!.

pt1(FS,Tab,List) :- List=[_|_], !,
  prefix_spaces(Tab),pformat_functor('[ '),
  pt_args_arglist([lf|FS],Tab+2,'',' | ',']',List),!.

% pt1(FS,Tab,q(E,V,G)):- atom(E), !, T=..[E,V,G],!, print_tab_term(Tab,FS,T).

% xf/yf
pt1(_FS,Tab,T1) :-
  %max_output(Tab,300,T),
  compound_name_arguments(T1,OP, [T]),  
  (current_op(Pri,yf,OP);current_op(Pri,xf,OP)),
  Pri >= 400,
  prefix_spaces(Tab),pformat_functor('( ( '),
  pformat_e_args(T,
   system_portray(Tab+3,T,[right_margin(40)])),
  pformat([') ',OP,' )']),!.

% fx/fy
pt1(_FS,Tab,T1) :-
  %max_output(Tab,300,T),
  compound_name_arguments(T1,OP, [T]),  
  (current_op(Pri,fy,OP);current_op(Pri,fx,OP)),
  Pri >= 400,
  prefix_spaces(Tab), pformat('( '), print_functor(OP), pformat_functor(' ( '),
  pformat_e_args(T,
   system_portray(Tab+3,T,[right_margin(40)])),
  pformat(') )'), !.

pt1(_FS,Tab,T) :- % fail,
   print_tree_width(W160), \+ using_folding_depth,
   max_output(Tab,W160,T),!,
   system_portray(Tab,T),!.

% xfy/yfx/xfx
pt1(_FS,Tab,T) :-
  compound_name_arity(T,OP, 2),  
  (current_op(Pri,xfy,OP);current_op(Pri,yfx,OP);current_op(Pri,xfx,OP)),
  Pri >= 400,
  pred_juncts_to_list(OP,T,List),!,
  prefix_spaces(Tab), pformat_functor('( '),  
    pformat_e_args(T, 
       pt_list_juncts(Tab+2,OP,List)), 
   pformat(')'),!.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Arg]), nonvar(Arg), Arg = [A|Args],
   is_arity_lt1(A), !,
   prefix_spaces(Tab), print_functor(F), pformat_functor('([ '),
   pt_args_arglist([F|FS],Tab+3,'','|','])',[A|Args]), !.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Arg]), nonvar(Arg), Arg = [A|Args],
   is_arity_lt1(A),
   prefix_spaces(Tab), print_functor(F), pformat_functor(format('([ ~p, ',[A])),
   pt_args_arglist([F|FS],Tab+3,'','|','])',Args), !.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Args]), nonvar(Args), Args = [_|_],
   prefix_spaces(Tab), print_functor(F), pformat_functor('([ '),
   pt_args_arglist([F|FS],Tab+3,'','|','])',Args), !.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,Args),
   prefix_spaces(Tab), print_functor(F), pformat_functor('( '),
   pt_args_arglist([F|FS],Tab+3,'','@',')',Args), !.



is_colon_mark('=').
is_colon_mark(':').
is_colon_mark(':-').
is_colon_mark('-->').
is_colon_mark('->').
is_colon_mark('::').
is_colon_mark('::::').


major_conj(F):-  (F == ',';F == ';' /*;F=='&'*/),!.

splice_off([A0,A|As],[A0|Left],[R|Rest]):- 
   is_arity_lt1(A0), append(Left,[R|Rest],[A|As]), 
    Rest\==[] , %  is_list(Rest),
   ( (\+ is_arity_lt1(R)) ; (length(Left,Len),Len>=6)),!.



pt_args_arglist( _, _, S,_,E,[]):- pt_s_e(S, (pl_span_goal('ellipsis, fold',true),pl_span_goal('args',true)),E).
pt_args_arglist(FS,Tab,S,M,E,[H|T]):-
 pt_s_e(S,  
  pformat_e_args([H|T], 
    (prefix_spaces(Tab), print_tree(H), pt_cont_args(Tab,', ', M, FS,T))),E).


pt_cont_args(_Ab,_Sep,_Mid,_In, Nil) :- Nil==[], !.
pt_cont_args(Tab, Sep,_Mid, FS,[A|R]) :- R==[], pformat(Sep), print_tab_term(Tab,FS,A), !.
pt_cont_args(Tab,_Sep, Mid, FS, A) :- A \= [_|_], !, pformat(Mid), print_tab_term(Tab,FS,A), !.

pt_cont_args(Tab, Sep,_Mid,_FS,List) :- ground(List),is_list(List),length(List,Len),Len>1, Len<6, maplist(is_arity_lt1,List), !,
   pformat(Sep), notrace(prefix_spaces(Tab)),pformat(' '), List=[A|R], write_simple(A), write_simple_each(Sep,R),!.

pt_cont_args(Tab,Sep, Mid, FS, RL) :- append(List,Right,RL), ground(List),is_list(List),length(List,Len),Len>1, Len<6, maplist(is_arity_lt1,List), !,
   pformat(Sep), notrace(prefix_spaces(Tab)),pformat(' '), List=[A|R], write_simple(A), write_simple_each(Sep,R),
   pt_cont_args(Tab,Sep, Mid, FS, Right),!.

pt_cont_args(Tab,Sep, Mid, FS,[A|As]) :- !,  
   pformat(Sep),
   print_tab_term(Tab,[lf|FS],A),
   pt_cont_args(Tab,Sep, Mid,[lf|FS],As).


:- export(print_tab_term/2).
:- export(print_tab_term/3).

is_arity_lt1(A) :- \+ compound(A),!.
is_arity_lt1(A) :- compound_name_arity(A,_,0),!.
is_arity_lt1(A) :- functor(A,'$VAR',_),!.
is_arity_lt1(V) :- is_dict(V), !, fail.
is_arity_lt1(S) :- is_charlist(S),!.
is_arity_lt1(S) :- is_codelist(S),!.

on_x_ignore(G):- catch(G,E,(dumpST,writeq(E=on_x_ignore(G)))).

as_is(V):- var(V).
as_is(V) :- is_dict(V), !, fail.
as_is(A) :- is_arity_lt1(A), !.
as_is(A) :- functor(A,F,_), simple_f(F).
as_is(A):- is_list(A), maplist(is_arity_lt1,A).
as_is([A]) :- is_list(A),length(A,L),on_x_ignore(L<2),!.
as_is(A) :- functor(A,F,2), simple_fs(F),arg(2,A,One),atomic(One),!.
as_is('_'(_)) :- !.
as_is(Q) :- is_quoted_pt(Q).

as_is(not(A)) :- !,as_is(A).
as_is(A) :- A=..[_|S], maplist(is_arity_lt1,S),length(S,SL),SL<5, !.
as_is(A) :- compound_name_arguments(A,PlusMinus,List),member(PlusMinus,[(+),(-)]),maplist(as_is,List).
as_is(A) :- A=..[_,B|S], fail, as_is(B), maplist(is_arity_lt1,S), !.
% as_is(F):- simple_arg(F), !.

is_quoted_pt(Q):- nonvar(Q), fail, catch(call(call,quote80(Q)),_,fail),!.

simple_fs(:).

simple_f(denotableBy).
simple_f(iza).
simple_f(c).
simple_f(ip).
simple_f(p).
simple_f(h).
simple_f(sub__examine).
simple_f(isa).
simple_f(has_rel).
simple_f(HasSpace):- atom_contains(HasSpace,' ').

simple_arg(S):- (nvar(S) ; \+ compound(S)),!.
%simple_arg(S):- S=[_,A], simple_arg(A), !.
simple_arg(S):- \+ (arg(_,S,Var), compound(Var), \+ nvar(Var)).

nvar(S):- \+ is_arity_lt1(S)-> functor(S,'$VAR',_); var(S).


write_simple_each(_Sep,[]).
write_simple_each(Sep,[A0|Left]):-  pformat(Sep), write_simple(A0), write_simple_each(Sep,Left).


:- export(canonicalise_defaults/2).
canonicalise_defaults(Dict, Out) :- is_dict(Dict), !, dict_pairs(Dict, _, Pairs), canonicalise_defaults2(Pairs, Out).
canonicalise_defaults(In, Out) :- canonicalise_defaults2(In, Out).

canonicalise_defaults2([], []).
canonicalise_defaults2([H0|T0], [H|T]) :- canonicalise_default(H0, H), canonicalise_defaults2(T0, T).
canonicalise_defaults2(H,[O]):- canonicalise_default(H,O).

canonicalise_default(Name=Value, Name=Value) :- !.
canonicalise_default(Name-Value, Name=Value) :- !.
canonicalise_default(NameValue, Name=Value) :- compound(NameValue), compound_name_arguments(NameValue,Name,[Value]),!.
canonicalise_default(Name, Name=_).

:- export(my_merge_options/3).
my_merge_options(N,O,MO):-
  merge_defaults(N,O,M),!,
  swi_option:merge_options([],M,MO).

wots_test(S,G):-freeze(S,(dumpST,break)),wots((SS),G),!,
  set_prolog_flag(access_level,system),trace,ignore((get_attrs(S,Atts))),ignore((get_attrs(SS,Atts))),display(SS=S),ignore(SS=S).

:- export(merge_defaults/3).
merge_defaults([], Old, Merged) :- !, canonicalise_defaults(Old, Merged).
merge_defaults(New, [], Merged) :- !, canonicalise_defaults(New, Merged).
merge_defaults(New, Old, Merged) :- 
    canonicalise_defaults(New, NCanonical),
    canonicalise_defaults(Old, OCanonical),
    merge_canonical_defaults(NCanonical,OCanonical,Merged).

merge_canonical_defaults([],O,O):-!.
merge_canonical_defaults([N=V|T],Old,O):- select(N=_,T,NewT),!,
  merge_canonical_defaults([N=V|NewT],Old,O).
merge_canonical_defaults([N=V|T],Old,O):- select(N=_,Old,NewOld),!,
  merge_canonical_defaults([N=V|T],NewOld,O).
merge_canonical_defaults([N=V|T],Old,[N=V|O]):- 
  merge_canonical_defaults(T,Old,O).
merge_canonical_defaults(O,[],O):-!.


:- system:use_module(library(logicmoo_startup)).

% user:portray(Term):- in_pp(swish), print_tree(Term).

user:portray(Term):- pc_portray(Term),!.




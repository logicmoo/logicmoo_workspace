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
   [print_tree/1]).

:- set_module(class(library)).

:- meta_predicate with_op_cleanup(*,*,*,0).


str_repl(F,R,Tab,O):- if_string_repl(Tab,F,R,O),!.
str_repl(_,_,Tab,Tab).

replcterm(F,R,Tab,O):- subst(Tab,F,R,O),!.

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

clause_to_string_et(T,S):-
 guess_varnames(T),
 get_varname_list(Vs), 
 print_et_to_string(T,S0,
    [portrayed(true),portray(true),partial(false),% spacing(next_argument),
     variable_names(Vs),
     nl(false),fullstop(false),singletons(false)]),!,
 trim_stop(S0,S),!.

clause_to_string(T,S):- 
 get_varname_list(Vs),
 with_output_to(string(S0), 
  prolog_listing:portray_clause(current_output,T,
    [portrayed(false),variable_names(Vs),partial(true),nl(false),
     % spacing(next_argument),
     fullstop(false),singletons(false)])),!,
 trim_stop(S0,S).

:- export(compound_gt/2).
compound_gt(P,GT):- notrace((compound(P), compound_name_arity(P, _, N), N > GT)).

print_e_to_string_b(H, S):- 
  compound_gt(H, 0), H=..[F,_,_], 
  current_op(_,_,F),
  print_e_to_string(H, S0),
  mid_pipe(S0,[str_repl('\n',' \n')],S1),
  sformat(S, '(~s)',[S1]),!.

print_e_to_string_b(H, HS):- print_e_to_string(H, HS),!.

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

/*.
ec_portray(','):-write(',').
user:portray(Nonvar):- nonvar(Nonvar), ec_portray(Nonvar).   */
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
  ttyflush,
   Old = [numbervars(true),
                  quoted(true),
                  ignore_ops(false),
                  no_lists(false),
                  %spacing(next_argument),
                  portray(false)],
   swi_option:merge_options(Options,Old,WriteOpts),
   PrintOpts = [output(current_output)|Options],
                                 
  sformat(S, '~@',
    [(prolog_pretty_print:print_term(T, 
             [   %left_margin(1),
                 %operators(true),
                 %tab_width(2),
                 %max_length(120),
                 %indent_arguments(auto),                  
                 write_options(WriteOpts)|PrintOpts]),
      ttyflush)]).

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

:- export(pprint_ecp_cmt/2).
pprint_ecp_cmt(C, P):-
 notrace((echo_format('~N'),  
  print_e_to_string(P, S0),
  into_space_cmt(S0,S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).

:- export(pprint_ecp/2).
pprint_ecp(C, P):- \+ is_output_lang(C), !, pprint_ecp_cmt(C, P).
pprint_ecp(C, P):-
  maybe_mention_s_l(1),
  notrace((echo_format('~N'),
  pprint_ec_and_f(C, P, '.~n'))).

pprint_ec_and_f(C, P, AndF):-
  maybe_mention_s_l(2),
  pprint_ec_no_newline(C, P), 
  echo_format(AndF), !,
  ttyflush.

user:portray(Term):- \+ current_prolog_flag(debug,true), \+ tracing, ec_portray_hook(Term).

ec_portray_hook(Term):- 
 setup_call_cleanup(flag('$ec_portray', N, N+1), 
  ec_portray(N, Term),
  flag(ec_portray,_, N)).

ec_portray(_,Var):- var(Var),!,fail. % format('~p',[Var]),!.
ec_portray(_,'$VAR'(Atomic)):-  atom(Atomic), name(Atomic,[C|_]), !,
   (code_type(C,prolog_var_start)->write(Atomic);writeq('$VAR'(Atomic))).
ec_portray(_,Term):- notrace(is_list(Term)),!,Term\==[], fail, notrace(catch(text_to_string(Term,Str),_,fail)),!,format('"~s"',[Str]).
ec_portray(_,Term):- compound(Term),compound_name_arity(Term, F, 0), !,ansi_format([bold,hfg(red)],'~q()',[F]),!.
ec_portray(N,Term):- N < 2, 
  % ttyflush,
  ttyflush,
  catch(pprint_ec_no_newline(white, Term),_,fail),!.


pprint_ec_no_newline(C, P):-
  print_e_to_string(P, S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]).
  

print_e_to_string(P, S):- 
   get_operators(P, Ops),
   pretty_numbervars(P, T),
   print_e_to_string(T, Ops, S).
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
  str_repl('\n','\n   ',S0, S),
  (S0==S -> sformat(O, '~N %  ~s.~n', [S]); 
    (maybe_mention_s_l(1),sformat(O, '~n /*  ~s.~n */~n', [S]))).

% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ', Goal), echo_format('~N', [])).
%in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /*~n', []), Goal, echo_format('~N*/~n', [])).
in_space_cmt(Goal):- 
   with_output_to(string(S0),Goal),
   into_space_cmt(S0,S),
   real_format('~s', [S]).

in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /* ', []), Goal, echo_format('~N */~n', [])).


read_line_to_string_echo(S, String):- read_line_to_string(S, String), ttyflush, real_ansi_format([bold, hfg(black)], '~s~N',[String]),
  ttyflush.
  
echo_flush:- ttyflush.
:- export(echo_format/1).
echo_format(S):- echo_flush, echo_format(S, []).
:- export(echo_format/2).
echo_format(_Fmt, _Args):- t_l:block_comment_mode(Was), Was==invisible, !.
echo_format(Fmt, Args):- t_l:block_comment_mode(_), t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(Fmt, Args):- t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(_Fmt, _Args):- t_l:echo_mode(skip(_)), !.
echo_format(Fmt, Args):- real_format(Fmt, Args), ttyflush, !.
%echo_format(_Fmt, _Args).

is_outputing_to_file:- 
  current_output(S),
  stream_property(S,file_name(_)).

get_ansi_dest(S):- \+ is_outputing_to_file,!,current_output(S).
get_ansi_dest(S):- S = user_error, !.
get_ansi_dest(S):- S = user_output, !.

with_output_to_ansi_dest(Goal):- 
  get_ansi_dest(AnsiDest),with_output_to(AnsiDest,(Goal,ttyflush)),ttyflush.
  

put_out(Char):- put(Char),
  (is_outputing_to_file -> with_output_to_ansi_dest(put(Char)) ; true),!.
  

real_format(Fmt, Args):- listify(Args,ArgsL), real_ansi_format([hfg(magenta)], Fmt, ArgsL).

real_ansi_format(Ansi, Fmt, Args):- listify(Args,ArgsL), real_ansi_format0(Ansi, Fmt, ArgsL).
real_ansi_format0(Ansi, Fmt, Args) :-  \+ is_outputing_to_file, !, ansi_format(Ansi, Fmt, Args).
real_ansi_format0(Ansi, Fmt, Args) :-  
     format(Fmt, Args), with_output_to_ansi_dest(ansi_format(Ansi, Fmt, Args)).



%s_l(F,L):- source_location(F,L),!.

:- dynamic(etmp:last_s_l/2).

:- export(maybe_mention_s_l/1).
maybe_mention_s_l(N):- etmp:last_s_l(B,L), LLL is L+N,  s_l(BB,LL), B==BB, !, (LLL<LL -> mention_s_l; (N==1->mention_o_s_l;true)).
maybe_mention_s_l(_):- mention_s_l.

:- export(mention_s_l/0).
mention_s_l:- 
  s_l(F,L), % real_ansi_format([fg(green)], '~N% From ~w~n', [F:L]),
  (o_s_l_diff->mention_o_s_l;true),
  retractall(etmp:last_s_l(F,_)),
  asserta(etmp:last_s_l(F,L)).


o_s_l_diff:- s_l(F2,L2), ec_reader:o_s_l(F1,L1), (F1 \= F2; ( Diff is abs(L1-L2), Diff > 0)), !.

maybe_o_s_l:- \+ o_s_l_diff, !.
maybe_o_s_l:- e_source_location(F,L),retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(F,L)),!.
maybe_o_s_l.

output_line_count(L):- nb_current('$ec_output_stream',Outs),is_stream(Outs),line_count(Outs,L).
output_line_count(L):- line_count(current_output,L).

output_line_position(L):- line_position(current_output,L).

:- dynamic(ec_reader:last_output_lc/3).
ec_reader:last_output_lc(0,foo,bar).

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
        (format('~N~q.~n', [:- was_s_l(F,L)]), with_output_to(user_error,(ansi_format([fg(green)], '~N% From ~w~n', [F:L]),ttyflush)))
         ; (ansi_format([fg(green)], '~N% From ~w~n', [F:L]),ttyflush)),!.

:- dynamic(ec_reader:o_s_l/2).
:- export(was_s_l/2).
was_s_l(B,L):- retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(B,L)), out_o_s_l_2(B,L).
  

e_source_location(F,L):- nb_current('$ec_input_stream',Ins), any_line_count(Ins,L), any_stream(F,Ins),!.
e_source_location(F,L):- nb_current('$ec_input_file',FS), absolute_file_name(FS,F), any_stream(F,Ins), any_line_count(Ins,L),!.
e_source_location(F,L):- current_stream(F, read, S), atom(F), atom_concat(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property(S, file_name(F)),stream_property(S, input), atom_concat(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property(S, file_name(F)),atom_concat(_,'.e',F), any_line_count(S,L),!.

:- export(s_l/2).
s_l(F,L):- e_source_location(B,L2), !, L is L2-1, absolute_file_name(B,F).
s_l(F,L):- source_location(F,L2), !, L is L2-1.
% s_l(F,L):- ec_reader:o_s_l(F,L). 
s_l(F,L):- any_stream(F,S), any_line_count(S,L),any_line_count(_,L), !.
s_l(unknown,0).

any_stream(F,S):- is_stream(F),var(S),!,F=S.
any_stream(F,S):- stream_property(S, file_name(F)),stream_property(S, input).
any_stream(F,S):- current_stream(F, read, S), atom(F).
any_stream(F,S):- stream_property(S, file_name(F)).
any_stream(F,S):- current_stream(F, _, S), atom(F).
any_line_count(_,L):- nonvar(L),!.
any_line_count(F,L):- nonvar(F), \+ is_stream(F), any_stream(F,S), any_line_count(S,L),!.
any_line_count(S,L):- line_count(S, L),!.
any_line_count(S,L):- character_count(S, C), L is C * -1,!.
any_line_count(S,L):- line_or_char_count(S, L),!.
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
print_tree_cmt(Info,C,P):-
 mention_o_s_l,
 notrace((echo_format('~N'),  
  with_output_to(string(S), in_cmt((
    format('~N~w: \n\n',[Info]),
    print_tree(P)))),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).

:- export(in_color/2).
in_color(C,P):-
 notrace((
  with_output_to(string(S), ((    
    call(P)))), to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).


pt_nl:- nl.

%:- dynamic(pretty_clauses:goal_expansion/2).
% pretty_clauses:goal_expansion(pt_nl,(write(S:L),nl)):- source_location(S,L).

write_simple(A):- get_portrayal_vars(Vs), 
  setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),
    write_term(A,[quoted(true),partial(true), portrayed(true), variable_names(Vs)]),
    erase(Ref)).

portray_with_vars(A):- get_portrayal_vars(Vs),  
  simple_write_term(A,[quoted(true),partial(true), portrayed(true), variable_names(Vs)]),!.

:- thread_local(pretty_tl:in_pretty/0).

prolog_pretty_print(A,Options):- 
  prolog_pretty_print:print_term(A, [portray(true), output(current_output)|Options]).

simple_write_term(A,Options):- fail, is_list(A), \+ pretty_tl:in_pretty, !,
  setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),
    prolog_pretty_print(A,Options),
    erase(Ref)).
simple_write_term(A,Options):-  write_term(A,Options),!.
%simple_write_term(A,Options):-  write_term(A,[portray_goal(prolog_pretty_print:print_term)|Options]).

get_portrayal_vars(Vs):- nb_current('$variable_names',Vs)-> true ; Vs=[].

print_tree(Term) :- print_tree_with_final(Term,'.').
print_tree(Term, Options) :- select(fullstop(true),Options,OptionsNew),!,print_tree_final_options(Term, '.', OptionsNew).
print_tree(Term, Options) :- print_tree_final_options(Term, '', Options).


print_tree_loop(Term,Options):- \+ pretty_tl:in_pretty,
  setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),
    print_tree(Term,Options),
    erase(Ref)).
print_tree_loop(Term, Options):- write_term(Term, Options).


print_tree_with_final(Term, Final):- print_tree_final_options(Term, Final, []).

print_tree_final_options(Term, Final, Options) :-
   (\+ memberchk(variable_names(_),Options) -> guess_pretty(Term) ; true),
   output_line_position(Tab), 
   print_final_tree_options_tab(Final,Term,Options,Tab),   
   nop(((output_line_position(Now), Now==Tab -> true) ; (nl,format('~t~*|', [Tab])))),
   !.

:- thread_local(pretty_tl:write_opts_local/1).

% print_tree0(Final,Term) :- as_is(Term),line_position(current_output,0),prefix_spaces(1),format('~N~p',[Term]),!.
print_final_tree_options_tab(Final,Term, Options,Tab):- 
  setup_call_cleanup(asserta(pretty_tl:write_opts_local(Options),Ref),
    print_final_term_tab(Final,Term,Tab),erase(Ref)).

% print_final_term_tab(Final,Term,Tab):-  list_contains_sub_list(Term), prefix_spaces(Tab), mu_prolog_pprint(Tab,Term,[]), write(Final).
print_final_term_tab(Final,Term,Tab):- \+ as_is(Term), pt0([],Final, Term, Tab),!.
print_final_term_tab(Final,Term,Tab):-  prefix_spaces(Tab), format('~@~w',[portray_with_vars(Term),Final]),!.


prefix_spaces(Tab):- output_line_position(Now), Now > Tab, !,nl,  format('~t~*|', [Tab]).
prefix_spaces(Tab):- output_line_position(Now), Need is Tab - Now, format('~t~*|', [Need]).

format_functor(F):- upcase_atom(F,U), ((F==U,current_op(_,_,F)) -> format("'~w'",[F]) ; format("~q",[F])).

is_list_functor(F):- F == lf.

write_using_pprint_recurse(_):- \+ current_module(mu),!,fail.
write_using_pprint_recurse(Term):- write_using_pprint(Term),!,fail.
write_using_pprint_recurse(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).
write_using_pprint_recurse(Term):- compound(Term),!, \+ (arg(_,Term,T), \+ atomic(T)).


mu_prolog_pprint(Term,Options):- output_line_position(Tab), mu_prolog_pprint(Tab,Term,Options).
mu_prolog_pprint(Tab,Term,Options):- mu:prolog_pprint(Term,[left_margin(Tab)|Options]).

is_simple_list(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).

write_using_pprint(_):- \+ current_module(mu),!,fail.
write_using_pprint(Term):- is_list(Term), !, member(L, Term), L\==[], is_list(L),!.
write_using_pprint(Term):- compound(Term), functor(Term,_,1),!, arg(1,Term,Arg), \+ is_simple_list(Arg).
%write_using_pprint(Term):- is_list(Term), arg(_,Term, L), contains_list(L),!.

contains_list(Term):- \+ \+ ((compound(Term),arg(_,Term, Arg), sub_term(T,Arg), is_list(T),T\==[])).
list_contains_sub_list(Term):- compound(Term),arg(_,Term, Arg),
  sub_term(T,Arg),T\==Arg,is_list(T),T\==[],contains_list(T).
 

inperent([F|_],TTs,Term,Ts):- fail, \+ is_list_functor(F),
      TTs=..[F,Term,Ts], 
      functor(TTsS,F,2),     
     ((nonvar(Term), Term=TTsS);(nonvar(Ts), Ts=TTsS)).

pt0(_,Final,Term,Tab) :- 
   is_arity_lt1(Term), !,
   prefix_spaces(Tab), portray_with_vars(Term),write(Final), nop(pt_nl).

pt0(_,Final,Term,Tab) :- 
   as_is(Term), !,
   prefix_spaces(Tab), portray_with_vars(Term),write(Final), nop(pt_nl).

pt0(FS,Final,[T|Ts],Tab):- !,
   pt0_list(FS,Final,[T|Ts],Tab).

/*
pt0(FS,Final,TTs,Tab) :- 
   inperent(FS,TTs,T,Ts),
   I0 is Tab-3,
   pt0(FS,Final,T,I0),   
   pt0(FS,Final,Ts,Tab).
*/

pt0(FS, Final,T,Tab) :- 
   T=..[F,A0,A|As], \+ major_conj(F), is_arity_lt1(A0), append(Left,[R|Rest],[A|As]), 
    (\+ is_arity_lt1(R) ; Rest==[]), !,   
   must_or_rtrace(pt0_functor(FS,Final,F,Tab,I0,LC2)),
   write_simple(A0), write_simple_each(Left),
   pt_args([F|FS],LC2,[R|Rest],I0).

pt0(FS, Final,T,Tab) :- 
   T=..[F,A0], !,   
   must_or_rtrace(pt0_functor(FS,Final,F,Tab,_I0,LC2)),
   print_tree_with_final(A0,LC2).
   %pt_args([F|FS],LC2,[],I0).

/*
pt0(_, Final,Term,Tab) :- is_simple_list(Term),
  prefix_spaces(Tab), mu_prolog_pprint(Tab,Term,[right_margin(6)]), write(Final).

pt0(_, Final,Term,Tab) :- write_using_pprint(Term),!,
  prefix_spaces(Tab), mu_prolog_pprint(Tab,Term,[right_margin(30)]), write(Final).

pt0(_, Final,Term,Tab) :- fail, write_using_pprint_recurse(Term),
  prefix_spaces(Tab), 
   mu_prolog_pprint(Tab,Term,[portray_goal(print_tree_loop)]),
   write(Final).

*/


pt0(FS,Final,q(E,V,G),Tab):- atom(E), !, T=..[E,V,G],!, pt0(FS,Final,T,Tab).

pt0([Fs|FS],Final,T,Tab0) :- 
   T=..[F,A,As], 
   Fs == F,   
   major_conj(F),
   Tab is Tab0,
   prefix_spaces(Tab0),write(' '),
   sformat(FinA, " ~w ",[F]), print_tree_with_final(A,FinA),
   format(atom(LC2),'~w',[Final]),
   pt0([F|FS],LC2,As,Tab).


pt0(FS,Final,T,Tab0) :- 
   T=..[F,A,As], 
   major_conj(F),
   Tab is Tab0+1,
   prefix_spaces(Tab0),write('('),
   sformat(FinA, " ~w ",[F]), print_tree_with_final(A,FinA),
   format(atom(LC2),')~w',[Final]),
   pt0([F|FS],LC2,As,Tab).

pt0(FS,Final,T,Tab) :- !,
   T=..[F,A|As],   
   pt0_functor(FS,Final,F,Tab,I0,LC2),
   pt0([F|FS],'',A,I0),
   pt_args([F|FS],LC2,As,I0).

pt0_functor(FS,Final,F,Tab,I0,LC2):-
   (((FS==F, major_conj(F) )
       -> (I0 is Tab+1,LCO='~w' )
       ; (prefix_spaces(Tab), format_functor(F),format('(',[]), I0 is Tab+3, /*pt_nl,*/ LCO=')~w'))),
     format(atom(LC2),LCO,[Final]).


pt0_list(FS,Final,[T|Ts],Tab) :- !,
  prefix_spaces(Tab),write('[ '),
   I2 is Tab+2,
   pt0(FS,'',T,I2),
   format(atom(NLC),' ]~w',[Final]),   
   pt_args([lf|FS],NLC,Ts,I2),!.

/*

pt0(FS,Final,T,Tab) :- fail,  T=..[F,A], !,
   prefix_spaces(Tab), format_functor(F),format('(',[]),
   I0 is Tab+1, format(atom(LC2),')~w',[Final]),   
   pt_args([F|FS],LC2,[A],I0).



pt0(FS,Final,T,Tab) :- fail,  T.=.[F,A,B|As], is_arity_lt1(A), !, 
   prefix_spaces(Tab), format_functor(F), format('( ~@,',[portray_with_vars(A)]), pt_nl,
   I0 is Tab+2, format(atom(LC2),')~w',[Final]),
   pt_args([F|FS],LC2,[B|As],I0).
*/


major_conj(F):-  (F == ',';F == ';';F=='&'),!.

splice_off([A0,A|As],[A0|Left],[R|Rest]):- 
   is_arity_lt1(A0), append(Left,[R|Rest],[A|As]), 
    Rest\==[] , %  is_list(Rest),
   ( (\+ is_arity_lt1(R)) ; (length(Left,Len),Len>=3)),!.

%pt_args( [F|_], Final,[A|Args],_Tab) :- simple_f(F),!,write_simple(A), write_simple_each(Args), write(Final),!.
pt_args( _, Final,[A],_Tab) :- number(A),  write(', '), write(A), write(Final),!.
pt_args( In, Final,Var,Tab):- Var\==[],  \+ is_list(Var), !, /* is_arity_lt1(Var), */ write(' | '), pt0(In,Final,Var,Tab).
pt_args(_In, Final,[],_) :- !, write(Final).
pt_args( FS, Final,[A|R],Tab) :- R==[], write(', '), prefix_spaces(Tab), pt0(FS,Final,A,Tab), !.
pt_args( FS, Final,[A0,A|As],Tab) :- 
   splice_off([A0,A|As],[_,L1|Left],Rest), !, 
   write(', '), write_simple(A0), write_simple_each([L1|Left]), 
   output_line_position(New), write(', '), nl, 
   Avr is round(((New - Tab)/2 + Tab)) + 4, !,
   prefix_spaces(Avr), 
   pt_args([lf|FS],Final,Rest,Avr).  
pt_args( FS, Final,[A|As],Tab) :- !,  write(', '), prefix_spaces(Tab), 
   pt0([lf|FS],'',A,Tab),
   pt_args([lf|FS],Final,As,Tab).



is_arity_lt1(A) :- \+ compound(A),!.
is_arity_lt1(A) :- compound_name_arity(A,_,0),!.
is_arity_lt1(A) :- functor(A,'$VAR',_),!.
is_arity_lt1(A) :- functor(A,'-',_),!.
is_arity_lt1(A) :- functor(A,'+',_),!.
is_arity_lt1(S) :- is_charlist(S),!.
is_arity_lt1(S) :- is_codelist(S),!.

as_is(V):- var(V).
as_is(A) :- is_arity_lt1(A), !.
as_is(A) :- functor(A,F,_), simple_f(F).
as_is(A):- is_list(A), maplist(is_arity_lt1,A).
as_is([A]) :- is_list(A),length(A,L),L<2,!.
as_is(A) :- functor(A,F,2), simple_fs(F),arg(2,A,One),atomic(One),!.
as_is('_'(_)) :- !.
as_is(Q) :- is_quoted_pt(Q).
   
as_is(not(A)) :- !,as_is(A).
as_is(A) :- A=..[_|S], maplist(is_arity_lt1,S),length(S,SL),SL<4, !.
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


write_simple_each([]).
write_simple_each([A0|Left]):-  format(', '), write_simple(A0), write_simple_each(Left).



:- system:use_module(library(logicmoo_startup)).


end_of_file.



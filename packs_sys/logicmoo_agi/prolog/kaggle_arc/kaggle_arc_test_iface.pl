/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

:- use_module(library(pengines)).

:- dynamic(muarc_tmp:cached_tests/2).
:- dynamic(muarc_tmp:cached_tests_hard/2).
:- multifile(muarc_tmp:cached_tests/2).
:- multifile(muarc_tmp:cached_tests_hard/2).

menu :- write_menu('i').

write_menu(Mode):-
  print_single_pair,!,
  write_menu_opts(Mode).

write_menu_opts(Mode):-
  get_current_test(TestID),some_current_example_num(Example),!,
  format('~N\n    With selected test: ~q ~q ~n~n',[TestID,example(Example)]),
  menu_options(Mode).
 
menu_options(Mode):- 
  forall(menu_cmd1(Mode,Key,Info,Goal),print_menu_cmd(Key,Info,Goal)),
  forall(menu_cmd9(Mode,Key,Info,Goal),print_menu_cmd9(Key,Info,Goal)),
  % show_pair_mode,
  !.
print_menu_cmd(Key):- ignore((menu_cmd1(_,Key,Info,Goal),print_menu_cmd(Key,Info,Goal))).
print_menu_cmd(Key,Info,_Goal):- arc_html, print_menu_cmd1(write(Info),(Key)),nl,!.
%print_menu_cmd(Key,Info,Goal):- atom(Goal), print_menu_cmd1(write(Info),(Goal)),nl,!.
print_menu_cmd(_Key,Info,Goal):- nl_if_needed, print_menu_cmd1(write(Info),Goal).

print_menu_cmd9(_Key,Info,Goal):- write_nbsp,print_menu_cmd1(Info,Goal).

print_menu_cmd1(Goal):- shorten_text(Goal,Str),  print_menu_cmd1(Str,Goal),!.
print_menu_cmd1(Info,Goal):- arc_html,!,must_det_ll((shorten_text(Info,Info1),write_nav_cmd(Info1,Goal))),!.
print_menu_cmd1(Info,_Goal):- shorten_text(Info,Str), format('~w',[Str]).

write_cmd_link2(Info,Goal):- nonvar(Goal),
  term_to_www_encoding(Goal,A),  toplevel_pp(PP), %in_pp(PP),
   sformat(SO,'<a href="/swish/lm_xref/?mouse_iframer_div=~w&cmd=~w" target="lm_xref">~w</a>~n ',[PP,A,Info]),!,
   our_pengine_output(SO).

shorten_text(Info,Info1):- \+ string(Info),!,into_title_str(Info,S),!,shorten_text(S,Info1).
%shorten_text(Info,Info1):- \+ atom_contains(Info,'  '),!,Info1=Info.
shorten_text(Info,Info1):- string_concat(' or ',L,Info),!,shorten_text(L,Info1).
shorten_text(Info,Info1):- string_concat(' ',L,Info),!,shorten_text(L,Info1).
shorten_text(Info,Info).



test_http_on:-
  current_output(Out),
  arc_set_stream(Out,tty(false)),
  arc_set_stream(Out,encoding(utf8)),
  arc_set_stream(Out,representation_errors(error)),
  arc_set_stream(Out,locale(default)),
  retractall(em_html),
  assert(em_html),
  set_http_debug_error(true),
  set_toplevel_pp(http).

test_http_off:-
  current_output(Out),
  arc_set_stream(Out,tty(true)),
  arc_set_stream(Out,encoding(text)),
  arc_set_stream(Out,representation_errors(unicode)),
  arc_set_stream(Out,locale(default)),
  retractall(em_html),
  set_toplevel_pp(ansi).


:- multifile(menu_cmd1/4).
:- multifile(menu_cmd9/4).
menu_cmd1(_,'t','       You may fully (t)rain from examples',(cls_z_make,fully_train)).
menu_cmd1(_,'T',S,(switch_pair_mode)):- get_pair_mode(Mode),  \+ arc_html, 
  sformat(S,"                  or (T)rain Mode switches between: 'entire_suite','whole_test','single_pair' (currently: ~q)",[Mode]).
menu_cmd1(i,'i','             See the (i)ndividuation correspondences in the input/outputs',(clear_tee,cls_z_make,!,locally(nb_setval(show_indiv,f),each_ndividuator))).
menu_cmd1(_,'I','                  or (I)ndividuate all',(whole_ndividuator)).
menu_cmd1(i,'o','                  or (o)bjects found in the input/outputs',                (clear_tee,cls_z_make,!,locally(nb_setval(show_indiv,t),ndividuator))).
menu_cmd1(_,'u','                  or (u)niqueness between objects in the input/outputs',   (cls_z_make,!,ignore(what_unique),ndividuator)).
menu_cmd1(_,'y','                  or Wh(y) between objects in the input/outputs',   ((cls_z_make,!,ndividuator))).
menu_cmd1(_,'a','                  or (a)ll between objects',   (cls_z_make,!,ndividuator)).
menu_cmd1(_,'j','                  or (j)unctions between objects',   (cls_z_make,!,ndividuator)).
menu_cmd1(_,'k','                  or (k)ill/clear all test data.',(update_changes,clear_test)).
menu_cmd1(_,'B','                  or (B)oxes test.',(update_changes,pbox_indivs)).
menu_cmd1(_,'R','                  or (R)epairs test.',(update_changes,repair_symmetry)).
menu_cmd1(_,'g','                  or (g)ridcells between objects in the input/outputs',(cls_z_make,!,compute_and_show_test_hints,compile_and_save_hints)).
menu_cmd1(_,'p','                  or (p)rint the test (textured grid)',(update_changes,maybe_set_suite,print_testinfo,print_test)).
menu_cmd1(_,'w','                  or (w)rite the test info',(update_changes,switch_pair_mode)).
menu_cmd1(_,'X','                  or  E(X)amine the program leared by training',(cls_z_make,print_test,!,learned_test,solve_easy)).
menu_cmd1(_,'L','                  or (L)earned Data',(learned_test)).
menu_cmd1(_,'e',S,(Cmd)):- get_test_cmd(Cmd),
      sformat(S,"                  or (e)xecute .................. '~@'",[bold_print(color_print(cyan,Cmd))]).
menu_cmd1(_,'x',S,(Cmd)):- get_test_cmd2(Cmd),
      sformat(S,"                  or e(x)ecute .................. '~@'",[bold_print(color_print(blue,Cmd))]).
menu_cmd1(_,'s','              Try to (s)olve based on training',(cls_z_make,print_test,!,solve_test)).
menu_cmd1(_,'S','                  or (S)olve confirming it works on training pairs',(cls_z_make,print_test,!,solve_test_training_too)).
menu_cmd1(_,'h','                  or (h)uman proposed solution',(human_test)).
menu_cmd1(_,'r','               Maybe (r)un some of the above: (p)rint, (t)rain, (e)xamine and (s)olve !',(cls_z_make,fully_test)).
menu_cmd1(_,'A','                  or (A)dvance to the next test and (r)un it',(cls_z_make,!,run_next_test)).
menu_cmd1(_,'N','                  or (N)ext random test',(randomize_suite,forward_test)).
menu_cmd1(_,'n','                  or (n)ext test (skipping this one)',(forward_test)).
menu_cmd1(_,'b','                  or (b)ack to previous test',(back_test)).
menu_cmd1(_,'f','                  or (f)orce a favorite test.',(force_full_tee,enter_test)):- \+ arc_html.
menu_cmd1(_,'~','                  or (PageUp) to begining of suite',(prev_suite)).
menu_cmd1(_,'~','                  or (PageDown) Next suite',(prev_suite)).
menu_cmd1(i,'R','             Menu to (R)un all tests noninteractively',(run_all_tests)).
menu_cmd1(_,'l','                  or (l)ist special tests to run,',(show_tests)).
menu_cmd1(r,'i','             Re-enter(i)nteractve mode.',(interact)).

% How I got into fostering was I had spent about 10k dollars at Dove Lewis Animal Hospital for a cat in heart failure.  When he passed, about 3 years ago, I decided (this time) to go to a "kill" shelter to adopt another.   While was waiting for an appointment with an adoption screener I saw a flyer that said 'all medical expenses' would be paid if i became a foster volunteer. Holly shit, that sounded good.  So I started fostering 'moms with kittens' and other cats in all stages and needs.  Sometimes cats that are under protection by court orders.  T

menu_cmd9(_,'m','recomple this progra(m),',(clear_tee,update_changes,threads)).
menu_cmd9(_,'c','(c)lear the scrollback buffer,',(force_full_tee,really_cls)).
menu_cmd9(_,'C','(C)lear cached test info,',(clear_training,clear_test)).
menu_cmd9(_,'r','(r)un DSL code,',(call_dsl)).
menu_cmd9(_,'Q','(Q)uit Menu,',true).
menu_cmd9(_,'^q','(^q)uit to shell,',halt(4)). 
menu_cmd9(_,'D','or (D)ebug/break to interpreter.',(ibreak)).


menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd1(Mode,Key,Mesg,Goal).
menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd9(Mode,Key,Mesg,Goal).

menu_or_upper(_IOKey):- is_cgi,!.
menu_or_upper(IOKey):- nb_current(menu_key,Key),(Key==IOKey;(upcase_atom(Key,Key),\+ upper_already_bound(Key))),!.

upper_already_bound(Key):- menu_cmds(_Mode,Key,_Mesg,_Goal).




find_tests(F):-
   current_predicate(N),N=F/0, (atom_concat(test_,_,F); atom_concat(_,'_test',F)),
    \+ ( atom_codes(F,Codes),member(C,Codes),char_type(C,digit) ).

find_f_tests(F):- quick_test_menu(F).
find_g_tests(F):- ping_indiv_grid(F).
%find_g_tests(F):- is_fti_stepr(F).
find_g_tests(F):- uses_test_id(F).
find_g_tests(F):- find_tests(F).

list_of_tests(S):- findall(F,find_f_tests(F),L1),findall(F,find_g_tests(F),L),sort_safe(L,L2),append(L1,L2,L12),list_to_set(L12,S).

show_tests:- update_changes, list_of_tests(L), print_menu_list(L), show_indivizers.

print_menu_list(L):- forall(nth_above(100,N,L,E),format('~N~@',[print_menu_cmd1(write(N:E),E)])),nl.








% ignore((read_line_to_string(user_input,Sel),atom_number(Sel,Num))),

ui_menu_call(G):- ignore(catch(must_not_error(G),E,u_dmsg(E))).
%ui_menu_call(G):- when_in_html(catch(ignore((G)),E,u_dmsg(E))) ->true ; catch(ignore((G)),E,u_dmsg(E)).
  
my_menu_call(E):- locally(set_prolog_flag(gc,true),ui_menu_call(E)).

my_submenu_call(G):- current_predicate(_,G), \+ is_list(G),!, locally(set_prolog_flag(nogc,false),ui_menu_call(G)),!.
my_submenu_call0(E):- peek_vm(VM),!, ui_menu_call(run_dsl(VM,E,VM.grid,Out)), set(VM.grid) = Out.

key_read_borked(PP):- fail, in_pp(PP), PP\==ansi,PP\==bfly.

:- dynamic(mu_tmp:asserted_queued_cmd/2).
read_queued_cmd(Out):- retract(mu_tmp:asserted_queued_cmd(Out,_Why)),!.
read_queued_cmd(Out):- mu_tmp:asserted_queued_cmd(Out,Why),!,ignore(retract(mu_tmp:asserted_queued_cmd(Out,Why))),!.

read_menu_chars(Start,_SelMax,Out):- is_input_null_stream,!,Out=Start.
read_menu_chars(_Start,_SelMax,Out):- pengine_self(_Id),!,read(Out).
read_menu_chars(Start,SelMax,Out):- repeat, read_menu_chars0(Start,SelMax,Out),!.

has_pending_input:- is_input_null_stream,!,fail.
has_pending_input:- catch_nolog(wait_for_input([user_input], In, 0.01)), In\==[].

%is_input_null_stream:- arc_html,!.
is_input_null_stream:- is_cgi_stream,!. % stream_property(user_input,output),!.

read_menu_chars0(_Start,_SelMax, Out):- read_queued_cmd(Out),!.
read_menu_chars0(_Start,_SelMax, Out):- key_read_borked(PP),!, u_dmsg(read_menu_chars(PP)),once((\+ toplevel_pp(http),read(Out))).
read_menu_chars0(_Start,_SelMax,_Out):- \+ has_pending_input,show_pair_mode,format('~N Your menu(?) selection: '),fail.
read_menu_chars0( Start, SelMax, Out):- read_menu_chars1( Start, SelMax,Out),!.
%read_menu_chars0( Start, SelMax, Out):- wait_for_input([user_input], In, 0.3), In \== [],!,read_menu_chars1( Start, SelMax,Out).

read_menu_chars1( Start, SelMax,Out):- fail,
  read_pending_codes(user_input, Codes, []),
  atom_codes(Key,Codes), append_num_code(Start,SelMax,Key,Out).

read_menu_chars1(Start,SelMax,Out):-
  get_single_key_code(Codes), atom_codes(Key,Codes),
  append_num_code(Start,SelMax,Key,Out).

append_num_code(Start,_SelMax,Key,Start):- atom_codes(Key,[C|_]), char_type(C,end_of_line),!.
append_num_code(Start,SelMax,Digit,Out):- atom_codes(Digit,[C|_]),char_type(C,digit),write(Digit),atom_concat(Start,Digit,NStart), 
 ((atom_number(NStart,Num),Num>99) -> Out = NStart ; read_menu_chars1(NStart,SelMax,Out)).
append_num_code(Start,_SelMax,Key,Sel):- atom_concat(Start,Key,Sel).

get_single_key_code(CCodes):- is_input_null_stream,!,CCodes = -1.
get_single_key_code(CCodes):- key_read_borked(PP),!, u_dmsg(get_single_key_code(PP)),sleep(5),once((\+ toplevel_pp(http),read(CCodes))).
get_single_key_code(CCodes):- get_single_char(C), 
  (  C== -1 -> (CCodes=`Q`) ; (read_pending_codes(user_input,Codes, []), [C|Codes]=CCodes)).


clsR:- flush_tee, !. % once(cls_z).

enter_test:- is_input_null_stream,!.
enter_test:- repeat, write("\nYour favorite: "), read_line_to_string(user_input,Sel),enter_test(Sel),!.

enter_test(""):- ppnl("resuming menu"), menu,!.
enter_test(Sel):- \+ is_valid_testname(Sel), fix_test_name(Sel,TestID),is_valid_testname(TestID),switch_test(TestID),!.
enter_test(Sel):- 
   sformat(SSel,'~q',[Sel]),
   catch(read_term_from_atom(SSel,Name,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),_,
        (ppnl(['failed to read: ',Sel]),fail)),
        my_maplist(ignore,Vs),my_maplist(ignore,Singles),
        Name\==Sel,
       (fix_test_name(Name,TestID) -> true ; (ppnl(['could not read a test from: ',Sel,nl,'try again']),!,fail)),
       enter_test(TestID).


switch_test(TestID):- ppnl(['Switching to test: ',TestID]),set_current_test(TestID),print_test.



:- dynamic(wants_exit_menu/0).
/*interact:- 
 ((
  repeat, write_menu('i'), 
   catch((interact),'$aborted',fail))),!.
*/
interact:-
  list_of_tests(L), length(L,SelMax),!,write_menu('i'),interact(SelMax).
/*interact:- list_of_tests(L), length(L,SelMax),!,
  repeat, 
    i_key(SelMax,Key),
    writeq(Key),
   once((do_menu_key(Key))), 
   retract(wants_exit_menu),!.
*/

interact(SelMax):- catch(interact0(SelMax),'$aborted',interact(SelMax)).
interact0(_SelMax):- retract(wants_exit_menu),!.
interact0(SelMax):- i_key(SelMax,Key),
    writeq(Key),%flush_tee,
    invoke_arc_cmd(Key),flush_tee,
    interact0(SelMax).

i_key(SelMax,Key):-
  %get_single_char(Code), u_dmsg(code=Code), char_code(Key,Code),  put_char(Key), 
   (once(read_menu_chars('',SelMax,Key))),!.

clear_pending_input:- is_input_null_stream,!.
clear_pending_input:- read_pending_codes(user_input,_Ignored1,[]).
menu_goal(Goal):-  
  clear_pending_input,
  pp(calling(Goal)),!, 
  ignore(once((time(((catch(my_menu_call(Goal),'$aborted',fail))))*->!;(!,fail,atrace,arcST,rrtrace(Goal))))),!,
  clear_pending_input,!.

:- public(do_web_menu_key/1).
:- export(do_web_menu_key/1).

invoke_arc_cmd(Key):- \+ arc_sensical_term(Key),!.
invoke_arc_cmd(Key):- arc_atom_to_term(Key,Prolog,Vs),Vs==[], Prolog\=@=Key,!,invoke_arc_cmd(Prolog).
  
invoke_arc_cmd(Goal):- \+ missing_arity(Goal,0),!, 
  write('<pre style="color: white">'),
  weto(ignore(Goal)),
  write('</pre>'),!.
invoke_arc_cmd(Key):- do_web_menu_key(Key).


do_web_menu_key(Key):-
  locally(nb_setval(menu_key,Key),ignore((do_menu_key(Key)))).

do_menu_key(-1):- !, arc_assert(wants_exit_menu). 
do_menu_key('Q'):-!,format('~N returning to prolog.. to restart type ?- demo. '), asserta_if_new(wants_exit_menu).
do_menu_key('?'):- !, write_menu_opts('i').
do_menu_key('M'):- !, clear_tee, make, u_dmsg('Recompiled'),real_list_undefined,menu.
do_menu_key('W'):- !, report_suites.
do_menu_key('P'):- !, switch_grid_mode,print_test.
do_menu_key( ''):- !, fail.

do_menu_key('d'):- !, dump_from_pairmode.

do_menu_key(Numerals):- atom(Numerals), atom_number(Numerals,Num), number(Num), do_menu_number(Num),!.
do_menu_key(Num):- number(Num), do_menu_number(Num),!.

do_menu_key(Key):- atom(Key), atom_codes(Key,Codes), clause(do_menu_codes(Codes),Body), !, menu_goal(Body).
do_menu_key(Key):- atom(Key), menu_cmds(_,Key,_,Body), !, menu_goal(Body).
do_menu_key(Key):- atom(Key), atom_codes(Key,[Code]), Code<27, CCode is Code + 96, atom_codes(CKey,[94,CCode]),!,do_menu_key(CKey).

do_menu_key(Key):- atom(Key), atom_length(Key,1), \+ menu_cmd1(_,Key,_,_),
   char_type(Key,to_upper(LowerKey)),LowerKey\==Key, \+ \+ menu_cmd1(_,LowerKey,_,_),
   format('~N~n'), get_pair_mode(Mode), alt_pair_mode(Mode,Alt), !,
     with_pair_mode(Alt,do_menu_key(LowerKey)).

do_menu_key(Name):- atom(Name), atom_length(Name,N), N>=3,do_menu_name(Name).

%do_menu_key(Atom):- atom(Atom), atom_codes(Atom,Codes), once(Codes=[27|_];Codes=[_]),format("~N % Menu: '~w' ~q ~n",[Atom,Codes]),fail.
do_menu_key(Text):- atom(Text),atom_concat(' ',Text,Text),Text\=='', !,do_menu_key(Text).
do_menu_key(Text):- atom(Text),atom_concat(Text,' ',Text),Text\=='', !,do_menu_key(Text).
do_menu_key(Text):- atom(Text),atom_concat(Text,'\r',Text),Text\=='', !,do_menu_key(Text).


do_menu_key(OID):- atom(OID),oid_to_obj(OID,Obj),!,show_indiv(Obj).


% refering to a Test Suite or Object
do_menu_key(Key):- ground(Key), Key = (TestID>ExampleNum*_IO),!,set_example_num(ExampleNum),set_current_test(TestID),
  set_pair_mode(single_pair), click_grid(Key).
do_menu_key(Key):- ground(Key), Key = (TestID>ExampleNum),!,set_example_num(ExampleNum),set_current_test(TestID),
  set_pair_mode(single_pair),show_selected_object.
do_menu_key(Key):- is_valid_testname(Key), set_current_test(Key),!,
  set_pair_mode(whole_test),skip_if_ansi(show_selected_object).
do_menu_key(Key):- ground(Key), fix_test_name(Key,TestID),!,do_menu_key(TestID).

% a Text object
do_menu_key(Key):- \+ atom(Key), catch(text_to_string(Key,Str),_,fail),Key\==Str,catch(atom_string(Atom,Str),_,fail),
  Atom\=@=Key, 
    do_menu_key(Atom),!.

% Atom masking a Code object
do_menu_key(Key):- atom(Key), arc_atom_to_term(Key,Term,Vs), nonvar(Term),
  Term\=@=Key, 
    locally(nb_setval('$variable_names',Vs), do_menu_key(Term)),!.

% Any Code object
do_menu_key(Key):- % ls   foo
 % nl,writeq(do_menu_key(Key)),nl,
  maybe_call_code(Key),!.

do_menu_key(Key):- atom(Key),atom_codes(Key,Codes),!,debuffer_atom_codes(Key,Codes),!.

do_menu_name(E):- list_of_indivizers(L),member(E,L),!, set_indivs_mode(E).
do_menu_name(E):- list_of_pair_modes(L),member(E,L),!, set_pair_mode(E).
do_menu_name(E):- test_suite_list(L),member(E,L),!, set_test_suite(E).

do_menu_number(N):- N>=300,N=<800,set_pair_mode(entire_suite),fail.
do_menu_number(N):- N<800,do_test_number(N),!.
do_menu_number(N):- N>=800,N=<999,do_indivizer_number(N),!.

debuffer_atom_codes(_Key,[27|Codes]):- append(Left,[27|More],Codes),
  atom_codes(Key1,[27|Left]),atom_codes(Key2,[27|More]),!,
  (do_menu_key(Key1)->true;do_menu_key(Key2)).
debuffer_atom_codes(_Key,[C|Codes]):- C\==27, Codes\==[],
  atom_codes(Key1,[C]),atom_codes(Key2,Codes),
  (do_menu_key(Key1)->true;do_menu_key(Key2)). 
debuffer_atom_codes(Key,Codes):- format("~N % Menu did understand '~w' ~q ~n",[Key,Codes]).

arc_atom_to_term(Key,Prolog,Vs):- atom(Key),notrace(catch(atom_to_term(Key,Prolog,Vs),_,fail)), arc_sensical_term(Prolog).

maybe_call_code(Key):- \+ atom(Key),
 notrace(catch(text_to_string(Key,Str),_,fail)),Key\==Str,catch(atom_string(Atom,Str),_,fail),!,maybe_call_code(Atom).

maybe_call_code(Key):- atom(Key), 
  arc_atom_to_term(Key,Term,Vs), nonvar(Term), Term\=@=Key,
  locally(nb_setval('$variable_names',Vs),
  maybe_call_code(Term)),!.

maybe_call_code(Term):- 
%nonvar(Term),
 % current_predicate(_,Term),
   \+ missing_arity(Term,0),
   asserta_new(xlisting_whook:offer_testcase(Term)),!,
   locally(nb_setval('$term',Term),
     locally(nb_setval('$user_term',Term), 
       weto(my_submenu_call(Term)))).

call_dsl:- repeat, write("\nYour DSL Goal: "), read_line_to_string(user_input,Sel),ignore(do_menu_key(Sel)),!.



% nth that starts counting at three

nth_above(M,X,Y,Z):- var(X),!,nth0(N,Y,Z), X is N + M .
nth_above(M,X,Y,Z):- N is X -M, nth0(N,Y,Z).

set_test_suite_silently(N):- 
   luser_getval(test_suite_name,X),
   set_test_suite_silently(X,N).

%set_test_suite_silently(X,_N):- arc_html, X == end_of_file,!.
%set_test_suite_silently(_X,N):- arc_html, N == end_of_file,!.
set_test_suite_silently(X,N):-
   if_t(X\==N,
   (luser_setval(test_suite_name,N),!,
    u_dmsg(switched(X-->N)))).

set_test_suite(N):- 
   luser_getval(test_suite_name,X),
   set_test_suite(X,N).

%set_test_suite(X,_N):- arc_html, X == end_of_file,!.
%set_test_suite(_X,N):- arc_html, N == end_of_file,!.
set_test_suite(X,N):-
   if_t(X\==N,
   (set_test_suite_silently(N),
    notrace((restart_suite)),
    was_set_pair_mode(entire_suite))).

preview_suite:- luser_getval(test_suite_name,X),preview_suite(X).

do_suite_number(Num):- integer(Num),test_suite_list(L), nth_above(300,Num,L,SuiteX),!,set_test_suite(SuiteX),
  preview_suite.

select_suite(N):- string(N),atom_string(A,N),select_suite(A),!.
select_suite(N):- preview_suite(N).

preview_suite(Num):- integer(Num),!,do_suite_number(Num).
preview_suite(Name):- \+ is_list(Name),set_test_suite(Name),
 w_section(["Suite:",Name],
   (get_current_suite_testnames(Set),!,preview_suite(Set))).
% suites with only one test
 
%preview_suite([TestID]):- invoke_arc_cmd(TestID),!.
preview_suite([TestID]):- set_current_test(TestID),!,set_pair_mode(whole_test),show_selected_object.
preview_suite(Set):- length(Set,L),L=<10,web_reverse(Set,Rev),!, with_pair_mode(whole_test,my_maplist(preview_test,Rev)).
preview_suite(Set):- length(Set,L),L=<20,web_reverse(Set,Rev),!, with_pair_mode(single_pair,my_maplist(preview_test,Rev)).
preview_suite(Set):- length(Set,L),L>100, first_ten(Set,100,Rev),with_pair_mode(single_pair,preview_test_per_page(Rev)).
preview_suite(Set):- reverse(Set,Rev), with_pair_mode(single_pair,preview_test_per_page(Rev)).

first_n_of_list(Max,List,LeftMax,Rest):-length(List,Len),Len>Max,!,length(LeftMax,Max),append(LeftMax,Rest,List).
first_n_of_list(_Max,List,List,[]).

preview_test_per_page(List):- preview_test_per_page(1,20,List).
preview_test_per_page(Strt,Max,List):- length(List,Len),Len>Max,first_n_of_list(Max,List,LeftMax,Rest),
   NStrt is Strt+Max,Thru is NStrt-1,
   w_section(title(['Suite Tasks',Strt,"thru",Thru]),my_maplist(preview_test,LeftMax)),preview_test_per_page(NStrt,Max,Rest).
preview_test_per_page(Strt,_,List):- length(List,Len), 
  Thru is Strt+Len-1,
  w_section(title(['Suite Tasks',Strt,"thru",Thru]),my_maplist(preview_test,List)).

first_ten(Set,Ten,Rev):- length(LL,Ten),append(LL,_,Set),web_reverse(LL,Rev).

web_reverse(E,E):- arc_html,!.
web_reverse(L,R):- reverse(L,R).


do_test_number(Num):- do_suite_number(Num),!.
do_test_number(Num):- integer(Num),list_of_tests(L), 
  u_dmsg(do_test_number(Num)),nth_above(100,Num,L,E),!,set_test_cmd(E),do_test_pred(E).

do_test_pred(E):- 
  get_current_grid(G),
  set_flag(indiv,0), 
  u_dmsg(do_test_pred(E)),
  show_time_gt_duration(3.0,my_submenu_call(no_bfly(maybe_test(E,G)))),!.

maybe_test(E,_):- callable_arity(E,0), !, call(E).
maybe_test(E,G):- callable_arity(E,1), call(E,G),!.
maybe_test(E,G):- igo(E,G).



get_current_grid(G):- get_current_test(T),kaggle_arc_io(T,_,_,G).

% home
do_menu_codes([27,91,49,126]):- !, sort_suite, restart_suite, print_qtest.
% end
do_menu_codes([27,91,52,126]):- !, reverse_suite, restart_suite, print_qtest.
% insert
do_menu_codes([27,91,50,126]):- !, report_suites, print_qtest.
% delete
do_menu_codes([27,91,51,126]):- !, randomize_suite, print_qtest.


% crl left arrow
do_menu_codes([27,79,68]):- !, prev_test, print_test.
% ctrl right arrow
do_menu_codes([27,79,67]):- !, next_test, print_test.


% alt left arrow
do_menu_codes([27,27,91,68]):- !, prev_test, print_test.
% alt right arrow
do_menu_codes([27,27,91,67]):- !, next_test, print_test.


% page up
do_menu_codes([27,91,53,126]):- !, prev_suite,prev_test, was_set_pair_mode(entire_suite).
% page down
do_menu_codes([27,91,54,126]):- !, was_set_pair_mode(entire_suite), next_suite,print_single_pair.
% up arrow
do_menu_codes([27,91,65]):- !, was_set_pair_mode(single_pair),prev_pair.
% down arrow
do_menu_codes([27,91,66]):- !, was_set_pair_mode(single_pair),next_pair.
% left arrow
do_menu_codes([27,91,68]):- !, was_set_pair_mode(whole_test), maybe_cls, prev_test, report_suite_test_count, print_qtest.
% right arrow
do_menu_codes([27,91,67]):- !, was_set_pair_mode(whole_test), maybe_cls, next_test, report_suite_test_count, print_qtest.

%back_test:- do_menu_codes([27,91,68]),!.
back_test:- format('~N'),prev_test,print_single_pair,!.
%forward_test:- do_menu_codes([27,91,67]).
forward_test:- next_test, print_single_pair,!.
%forward_test:- next_random_test,print_single_pair.

maybe_cls:- nop(cls_z).

interactive_test(X):- set_current_test(X), print_test(X), interact.

run_all_tests:- 
  repeat,
   run_next_test,
   write_menu('r'),
   catch_nolog(wait_for_input([user_input],F,2)),
   F \== [], !,
   interact,!.

rtty:- with_tty_raw(rtty1).
rtty1:- repeat,get_single_char(C),dmsg(c=C),fail.


whole_ndividuator(TestID):- ensure_test(TestID),
  check_for_refreshness,
  nop(show_test_grids), set_flag(indiv,0),
  compile_and_save_hints,
  with_individuated_cache(true,
   with_pair_mode(whole_test,
    findall(PairGroups,
    (kaggle_arc(TestID,ExampleNum,In,Out),%nonvar(Out),
     once(each_ndividuator(TestID,ExampleNum,In,Out,PairGroups)),
     ignore(call_list(PairGroups))),
   _AllPairGroups))),
  show_groups(TestID).
  


each_ndividuator(TestID):- ensure_test(TestID),
  check_for_refreshness,
  nop(show_test_grids), set_flag(indiv,0),
  compile_and_save_hints,
  findall(PairGroups,
  with_individuated_cache(true,
    (with_task_pairs(TestID,ExampleNum,In,Out,
         once(each_ndividuator(TestID,ExampleNum,In,Out,PairGroups))))),
   AllPairGroups),
  show_groups(TestID),
  call_list(AllPairGroups).

call_list(List):-is_list(List),!,my_maplist(call_list,List).
call_list(Goal):-ignore(Goal).


get_each_ndividuator(Complete):-!,get_indivs_mode(Complete).
get_each_ndividuator(Complete):-
  findall(Complete,((toplevel_individuation(TL),Complete=[TL,do_ending]);get_indivs_mode(Complete)),List),
  list_to_set(List,Set),!,member(Complete,Set).

each_ndividuator(TestID,ExampleNum,In,Out, OUTPUT):- 
 name_the_pair(TestID,ExampleNum,In,Out,PairName), 
 findall(show_pair_result(PairName,Complete,InC,OutC),
  (get_each_ndividuator(Complete),
   with_indivs_mode(Complete,((
    with_task_pairs(TestID,ExampleNum,In,Out, 
     ((  w_section(individuate_pair_debug(Complete),
          must_det_ll((
           once(individuate_pair(Complete,In,Out,InC,OutC)))))))))))),PairGroups),

  OUTPUT= [dash_chars,dash_chars,dash_chars,dash_chars,
        print_side_by_side(each_ndividuator(PairName),In,Out),dash_chars,dash_chars|PairGroups].


show_pair_result(PairName,Complete,InC,OutC):-   
   print_side_by_side(show_pair_result(PairName,Complete),InC,OutC),
   nop((w_section(pair_result_objects(PairName,Complete),
     (my_maplist(show_i(in),InC), dash_chars, my_maplist(show_i(out),OutC),dash_chars)))),
   !.

show_i(Y,O):- 
  global_grid(O,GG),
  object_grid(O,OG),
  tersify(O,OT),
  wots(S,(write(Y),write(' '),write(OT))),
  print_ss(S,GG,OG).

ndividuator(TestID):- ensure_test(TestID),
  never_entire_suite,nop(show_test_grids), set_flag(indiv,0),
  %each_ndividuator(TestID),
  compute_and_show_test_hints(TestID),
  forall(with_task_pairs(TestID,ExampleNum,In,Out,ndividuator(TestID,ExampleNum,In,Out)),true).

ndividuator(TestID,ExampleNum,In,Out):- 
 get_indivs_mode(Complete), ndividuator(TestID,ExampleNum,Complete,In,Out).
ndividuator(TestID,ExampleNum,Complete,In,Out):-  
 with_indivs_mode(Complete,((name_the_pair(TestID,ExampleNum,In,Out,_PairName),
   with_task_pairs(TestID,ExampleNum,In,Out, i_pair(Complete,In,Out))))).

show_task_pairs(TestID):- ensure_test(TestID), set_flag(indiv,0),
 forall( with_task_pairs(TestID,ExampleNum,In,Out,
   print_side_by_side(green,In,in(show_task_pairs(TestID>ExampleNum)),_,Out,out(show_task_pairs(TestID>ExampleNum)))), true).
%show_test_grids:- get_current_test(TestID),set_flag(indiv,0),with_test_grids(TestID,Grid,print_grid(show_test_grids(TestID),Grid)).


first_indivs_modes([complete,pbox_vm,i_repair_pattern]).

next_indivs_mode(M1,M2):- first_indivs_modes(List),next_in_list(M1,List,M2).

next_indivs_mode:- get_indivs_mode(M1),next_indivs_mode(M1,M2),set_indivs_mode(M2).

set_indivs_mode(Mode):- luser_setval('$indivs_mode',Mode).
get_indivs_mode(Mode):- nonvar(Mode),get_indivs_mode(TMode),!,TMode==Mode.

get_indivs_mode(Mode):- test_config(indiv(Mode)).
%get_indivs_mode(IndivMode):- get_current_test(TestID),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).
%
get_indivs_mode(Mode):- once(luser_getval('$indivs_mode',Mode);next_indivs_mode(Mode,_)).
with_indivs_mode(Mode,Goal):- 
  get_indivs_mode(WasMode),
  setup_call_cleanup(set_indivs_mode(Mode),
          call(Goal),set_indivs_mode(WasMode)).

:- first_indivs_modes([M1|_]),set_indivs_mode(M1).


% Training modes
list_of_pair_modes([single_pair,whole_test,entire_suite]).


next_pair_mode(M1,M2):- list_of_pair_modes(List),next_in_list(M1,List,M2).

alt_pair_mode(M1,M2):- list_of_pair_modes([M1,M2|_]),!.
alt_pair_mode(_,M1):- list_of_pair_modes([M1|_]).

was_set_pair_mode(_).
set_pair_mode(Mode):- luser_setval('$pair_mode',Mode).
get_pair_mode(Mode):- nonvar(Mode),get_pair_mode(TMode),!,TMode==Mode.
get_pair_mode(Mode):- once(luser_getval('$pair_mode',Mode);next_pair_mode(Mode,_)).
with_pair_mode(Mode,Goal):- get_pair_mode(OldMode), trusted_redo_call_cleanup( set_pair_mode(Mode), Goal, set_pair_mode(OldMode)). 
switch_pair_mode:- get_pair_mode(Mode),next_pair_mode(Mode,NextMode),!,set_pair_mode(NextMode).
show_pair_mode:- get_pair_mode(Mode),get_test_cmd(Cmd), luser_getval(test_suite_name,Suite), get_indivs_mode(IndivMode),
  get_current_test(TestID),some_current_example_num(ExampleNum),!,
  (nonvar(ExampleNum)-> (SelTest=(TestID>ExampleNum)) ; SelTest=TestID),  
  ((muarc_tmp:cached_tests(Suite,Set),length(Set,Len)) -> true ; Len = ?),
  wots_vs(SS,color_print(yellow,call(format("'~w' (~w)",[Suite,Len])))),
  ppnl([format("~N ~w: ",[Mode]), format('~w',[SS])," indiv:",b(q(IndivMode)), " selected test: ",b(q(SelTest)), ".......... (e)xecute: ", b(q(Cmd))
    % "with pair mode set to: ",b(q(Mode)),
  %b(q(example(ExampleNum)))
  ]),flush_tee_maybe.
skip_entire_suite:- never_entire_suite,!,fail.
never_entire_suite:- ignore((get_pair_mode(entire_suite),set_pair_mode(whole_test))).

:- list_of_pair_modes([M1|_]),set_pair_mode(M1).

dont_set_test_cmd(Var):- var(Var),!.
dont_set_test_cmd(print_test).
dont_set_test_cmd(print_qtest).
dont_set_test_cmd(print_single_pair).
dont_set_test_cmd(next_test).
dont_set_test_cmd(next_random_test).
dont_set_test_cmd(randomize_suite).
dont_set_test_cmd(back_test).
dont_set_test_cmd(forward_test).
dont_set_test_cmd(prev_test).
dont_set_test_cmd(do_web_menu_key).
dont_set_test_cmd(do_menu_key).

dont_set_test_cmd(P):- compound(P), in_iface_file(P),!.
dont_set_test_cmd(A):- atom(A),current_predicate(A/_,P), in_iface_file(P).

dont_set_test_cmd(A):- atom(A),!, \+ current_predicate(A/0), \+ current_predicate(A/1).
dont_set_test_cmd(C):- \+ compound(C),!,fail.
dont_set_test_cmd((C1,C2)):- dont_set_test_cmd(C1),dont_set_test_cmd(C2).
dont_set_test_cmd(_:C):- !, dont_set_test_cmd(C).
dont_set_test_cmd(C):- compound_name_arity(C,F,_),!,dont_set_test_cmd(F).

in_iface_file(P):-predicate_property(P,file(File)),atom_contains(File,iface).

set_test_cmd(Mode):- \+ \+ dont_set_test_cmd(Mode),!.
set_test_cmd(Mode):- into_test_cmd(Mode,Cmd),luser_setval('cmd',Cmd).
get_test_cmd(Mode):- luser_getval('cmd',Mode).

set_test_cmd2(Mode):- \+ \+ dont_set_test_cmd(Mode),!.
set_test_cmd2(Mode):- into_test_cmd(Mode,Cmd),luser_setval('cmd2',Cmd).
get_test_cmd2(Mode):- luser_getval('cmd2',Mode).

into_test_cmd((Cmd1,Cmd2),Cmd):- dont_set_test_cmd(Cmd1),!,into_test_cmd(Cmd2,Cmd).
into_test_cmd((Cmd2,Cmd1),Cmd):- dont_set_test_cmd(Cmd1),!,into_test_cmd(Cmd2,Cmd).
into_test_cmd(Cmd,Cmd).

%set_pair_cmd(Mode):- luser_setval('cmd',Mode).
%get_pair_cmd(Mode):- luser_getval('cmd',Mode).

% Hides solution grid from code
kaggle_arc_io_safe(TestID,ExampleNum,IO,G):- kaggle_arc_io(TestID,ExampleNum,IO,G), 
  (((((ExampleNum*IO) \= ((tst+_)*out))))).


test_grids(TestID,G):- get_pair_mode(entire_suite), !, kaggle_arc_io_safe(TestID,_ExampleNum,_IO,G).
test_grids(TestID,G):- get_pair_mode(whole_test), !, ensure_test(TestID), kaggle_arc_io_safe(TestID,_ExampleNum,_IO,G).
test_grids(TestID,G):- ensure_test(TestID), some_current_example_num(ExampleNum), kaggle_arc_io(TestID,ExampleNum,_IO,G).
with_test_grids(TestID,G,P):- forall_count(test_grids(TestID,G),my_menu_call((ensure_test(TestID),P))).


% Hides solution grid from code
if_no_peeking(tst+_,_,_):-!.  if_no_peeking(_,O,O).
kaggle_arc_safe(TestID,ExampleNum,I,O):- kaggle_arc(TestID,ExampleNum,I,OO),if_no_peeking(ExampleNum,OO,O). 

test_pairs(TestID,I,O):- test_pairs(TestID,_ExampleNum,I,O).

test_pairs(TestID,ExampleNum,I,O):- get_pair_mode(entire_suite), !,ensure_test(TestID), kaggle_arc_safe(TestID,ExampleNum,I,O).
test_pairs(_TestID,ExampleNum,I,_O):- nonvar(ExampleNum),nonvar(I),!.

test_pairs(TestID_IN,ExampleNum,I,O):- get_pair_mode(whole_test), ensure_test(TestID_IN,TestID),!, %ignore(ExampleNum=trn+_), 
  kaggle_arc_safe(TestID,ExampleNum,I,O).
%test_pairs(TestSpec,ExampleNum,I,O):- compound(TestSpec),(TestSpec = (TestID>ExampleNum)),testspec_to_pairs(TestSpec,TestID,ExampleNum,I,O).

%test_pairs(TestID_IN,ExampleNum,I,O):- get_pair_mode(whole_test), ensure_test(TestID_IN,TestID),!,kaggle_arc_safe(TestID,ExampleNum,I,O).
%test_pairs(TestID_IN,ExampleNum,I,O):- get_pair_mode(single_pair), ensure_test(TestID_IN,TestID),!,kaggle_arc_safe(TestID,ExampleNum,I,O).

test_pairs(TestID,ExampleNum,I,O):- ignore(ensure_test(TestID)), some_current_example_num(ExampleNum),
  kaggle_arc(TestID,ExampleNum,I,O).

%with_task_pairs(TestID,I,O,P):- forall(test_pairs(TestID,I,O),my_menu_call((ensure_test(TestID),P))).
testspec_to_pairs(Var,TestID,ExampleNum,I,O):- var(Var),!,test_pairs(TestID,ExampleNum,I,O).
testspec_to_pairs(TestSpec,TestID,ExampleNum,I,O):- 
  testid_name_num_io(TestSpec,TestID,Example,Num,_IO), ExampleNum = Example+Num,!,
  ensure_test(TestID),kaggle_arc_safe(TestID,ExampleNum,I,O).
  %test_pairs(TestID,ExampleNum,I,O).

for_each(Gen,Goal):-
  Gen,(Goal*->true;true).

with_trn_pairs(TestID,ExampleNum,In,Out,Goal):- 
  ignore(ExampleNum=trn+_),
  with_task_pairs(TestID,ExampleNum,In,Out,Goal).

with_test_pairs(TestID,ExampleNum,In,Out,Goal):- 
  ignore(ExampleNum=tst+_),
  with_task_pairs(TestID,ExampleNum,In,Out,Goal).

with_task_pairs(TestID,ExampleNum,I,O,P):- 
 for_each((test_pairs(TestID,ExampleNum,I,O)),
  my_menu_call((
    ensure_test(TestID),
    set_example_num(ExampleNum),     
     set_current_pair(I,O),
     call_cleanup(with_current_pair(I,O,P),flush_tee)))).

bad:- igo([complete],v(aa4ec2a5)>(trn+0)*in).


report_suite_test_count:- luser_getval(test_suite_name,SuiteX), write('\n--->>>>'),report_suite_test_count(SuiteX).

report_suite_test_count(SuiteX):-  
  ((muarc_tmp:cached_tests(SuiteX,Set),length(Set,Len)) -> true ; Len = ?),
  print_suite_link(SuiteX,Len).

print_suite_link(passed(SuiteX),Len):-  bold_print(underline_print(color_print(green,SuiteX=passed(Len)))).
print_suite_link(failed(SuiteX),Len):-  bold_print(underline_print(color_print(red,SuiteX=failed(Len)))).
print_suite_link(SuiteX,Len):-  bold_print(underline_print(color_print(yellow,SuiteX=Len))).

assert_test_suite(Suite,TestID):- append_term(Suite,TestID,Term),
  assert_if_new(test_suite_name(Suite)),
  assert_if_new(Term).

:- system:export(ls/0).
:- import(system:ls/0).
%:- import(shell:ls/0).
/*
print_ctest(S):- 
  with_luser(alt_grid_dot,169,
    locally(nb_setval(number_grid_colors,t),print_qtest(S))),!.*/
print_ctest(S):- print_qtest(S).

dump_suite1:-   
   get_current_suite_testnames(Set),
   dump_suite1(Set).

%dump_suite1(Set):- arc_html,!,with_pair_mode(single_pair, forall_count(member(S,Set),print_ctest(S))).
dump_suite1(Set):- is_cgi,!,
   with_luser(alt_grid_dot,'_',with_pair_mode(single_pair, forall_count(member(S,Set),print_ctest(S)))).
dump_suite1(Set):-
   with_luser(alt_grid_dot,color_number,with_pair_mode(single_pair, forall_count(member(S,Set),print_ctest(S)))).

dump_suite:-   
   get_current_suite_testnames(Set),
   with_pair_mode(whole_test, forall_count(member(S,Set),print_ctest(S))).

dump_suite_sorted:-   
   get_current_suite_testnames(Set),
   sort_by_hard(Set,Sorted),
   with_pair_mode(whole_test, forall_count(member(S,Sorted),print_ctest(S))).

dump_not_suite:-   
   get_current_suite_testnames(Set),
   forall_count(((kaggle_arc_safe(TestID,_ExampleNum,_I,_O), \+ member(TestID,Set))),
    print_ctest(TestID)).

dump_from_pairmode:- get_pair_mode(single_pair),!,dump_suite1.
dump_from_pairmode:- get_pair_mode(entire_suite),!,dump_suite.
%dump_from_pairmode:- get_pair_mode(whole_test),!,dump_suite.
dump_from_pairmode:- get_pair_mode(whole_test),!,
     dump_suite_sorted,nop(dump_not_suite).
     %forall(kaggle_arc_safe(TestID,ExampleNum,I,O),
      %  print_ss(blue,in(TestID,ExampleNum)=I,out=O)).

sort_suite:-   
   luser_getval(test_suite_name,SuiteX), get_by_hard(SuiteX,ByHard),
   retractall(muarc_tmp:cached_tests(SuiteX,_)),
   asserta_new(muarc_tmp:cached_tests(SuiteX,ByHard)).

reverse_suite:-
   luser_getval(test_suite_name,SuiteX), 
   retract(muarc_tmp:cached_tests(SuiteX,ByHard)),
   reverse(ByHard,NewSet), asserta_new(muarc_tmp:cached_tests(SuiteX,NewSet)),!.
reverse_suite:-
   luser_getval(test_suite_name,SuiteX), get_by_hard(SuiteX,ByHard), reverse(ByHard,NewSet),
   retractall(muarc_tmp:cached_tests(SuiteX,_)),
   asserta_new(muarc_tmp:cached_tests(SuiteX,NewSet)).


restart_suite:- 
   ignore((get_current_suite_testnames(Set),
   [NewFirst|_]=Set,
   set_current_test(NewFirst))).

randomize_suite:-
 must_det_ll((
  luser_getval(test_suite_name,SuiteX), test_suite_testIDs(SuiteX,Set),!,
  get_by_hard(SuiteX,ByHard), reverse(ByHard,RevByHard),
  ignore((
  ((Set == RevByHard ; Set == ByHard), random_permutation(Set,NewSet),   
   retractall(muarc_tmp:cached_tests(SuiteX,_)),
   asserta_new(muarc_tmp:cached_tests(SuiteX,NewSet))))))).
 
%prev_suite:- once((get_current_test(TestID),get_current_suite_testnames([First|_]))), report_suite_test_count,TestID\==First,!,restart_suite.

prev_suite:- luser_getval(test_suite_name,X), 
   test_suite_list(List),prev_in_list(X,List,N), set_test_suite(N).

next_suite:- 
   luser_getval(test_suite_name,X),
   test_suite_list(List),next_in_list(X,List,N),set_test_suite(N).


%test_suite_name(arc_easy_test).
:- multifile(dir_test_suite_name/1).
:- dynamic(dir_test_suite_name/1).

:- dynamic(dont_sort_by_hard/1).
dont_sort_by_hard(S):- S \== test_names_by_hard,!.
dont_sort_by_hard(test_names_by_fav). dont_sort_by_hard(all_arc_test_name). dont_sort_by_hard(all_arc_test_name_unordered).
dont_sort_by_hard(_).
%dont_sort_by_hard(P):- atom(P), \+ atom_concat(_,'_hard',P).


create_group(Name,Tests):- arc_assert(test_suite_name(Name)),arc_assert(dont_sort_by_hard(Name)),
  assert(muarc_tmp:skip_calc_suite(Name)),
  my_maplist(fix_test_name,Tests,TestID),list_to_set(TestID,Set),arc_assert(muarc_tmp:cached_tests(Name,Set)),
  set_test_suite(Name).


:- multifile(test_suite_name/1).
:- dynamic(test_suite_name/1).
test_suite_name(evaluation).
test_suite_name(easy_solve_suite).
test_suite_name(test_names_by_fav). 
test_suite_name(human_t).
test_suite_name(michod_solved_ordered).
test_suite_name(is_symgrid).
%test_suite_name(sol_t).
%test_suite_name(hard_t).
%test_suite_name(key_pad_tests). % test_suite_name(alphabetical_v). test_suite_name(alphabetical_t).
%test_suite_name(test_names_by_fav_rev). 
%test_suite_name(test_names_by_hard_rev).
%test_suite_name(all_arc_test_name).
test_suite_name(dbigham_train_core).
test_suite_name(dbigham_eval_pass).
test_suite_name(dbigham_train_pass).
test_suite_name(dbigham_personal).
test_suite_name(dbigham_fail).
test_suite_name(test_names_by_hard). 
test_suite_name(TS):- dir_test_suite_name(TS).
test_suite_name(icecuber_pass).
test_suite_name(icecuber_fail).
test_suite_name(P1):- return_sorted(PS,test_suite_name_by_call(PS)),PS=P1.
test_suite_name(Prop):- test_suite_marker(Prop).

return_sorted(P1,Goal):- findall(P1,Goal,List),sort_safe(List,Set),member(P1,Set).

:- dynamic(muarc_tmp:cached_tests/2).
%:- retractall(muarc_tmp:cached_tests(_,_)).
:- test_suite_name(Name)->luser_default(test_suite_name,Name).

get_current_suite_testnames(Set):-
  luser_getval(test_suite_name,X),
  current_suite_testnames(X,Set).

:- dynamic(muarc_tmp:skip_calc_suite/1).

current_suite_testnames(X,Set):- nonvar(Set),current_suite_testnames(X,SetV),!,Set=SetV.
current_suite_testnames(X,Set):- muarc_tmp:cached_tests(X,Set),acceptable_test_set(Set),!.
current_suite_testnames(X,_Set):- muarc_tmp:skip_calc_suite(X),%bt,
  pp(skip_calc_suite(current_suite_testnames(X))),fail.
current_suite_testnames(X,Set):-  
 setup_call_cleanup(
  assert(muarc_tmp:skip_calc_suite(X),Ref),
 (once((%pp(creating(current_suite_testnames(X))), 
 must_det_ll((
 findall(ID,test_suite_info(X,ID),List),
  %length(List,L), pp(current_suite_testnames(X)=L),  
  my_list_to_set_variant(List,Set),
     !,(ignore((acceptable_test_set(Set),asserta(muarc_tmp:cached_tests(X,Set)))))))))),
  erase(Ref)).

acceptable_test_set(Set):- Set\==[],length(Set,Len),Len\==833.

get_by_hard(X,Set):- nonvar(Set),get_by_hard(X,SetV),!,Set=SetV.
get_by_hard(X,ByHard):- muarc_tmp:cached_tests_hard(X,ByHard),!.
get_by_hard(X,ByHard):- 
 show_time_gt_duration(3.0, ((
  must_det_ll((
  pp(creating(get_by_hard(X))),    
  current_suite_testnames(X,Set),
  length(Set,L),
  pp(get_by_hard(X)=L),  
  likely_sort(X,Set,ByHard),
  !,(ignore((ByHard\==[],asserta(muarc_tmp:cached_tests_hard(X,ByHard)))))))))).


likely_sort(X,Set,Set):- dont_sort_by_hard(X),!,pp(dont_sort_by_hard(X)).
likely_sort(_X,Set,Set):- \+ arc_option(always_sort),!.
likely_sort(X,Set,ByHard):-  pp(sorting_suite(X)), !, sort_by_hard(Set,ByHard), !.

sort_by_hard(List,NamesByHardUR):- 
  sort_safe(List,Sorted),
  findall(Hard-Name,(member(Name,Sorted),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),  my_maplist(arg(2),AllK,NamesByHardU),!,
  reverse(NamesByHardU,NamesByHardUR).

:- dynamic(test_results/4).

some_test_suite_name(SuiteX):- some_test_suite_name_good(Good),some_test_pass_fail(Good,SuiteX).

some_test_pass_fail(SuiteX,SuiteX).
some_test_pass_fail(SuiteX,_):- muarc_tmp:cached_tests(SuiteX,Set),
  forall((member(TestID,Set),test_results(PassFail2,SuiteX2,TestID,Elapsed2),SuiteX2\==SuiteX,
        \+ test_results(_PassFail,SuiteX,TestID,_Elapsed)),
           assert_if_new(test_results(PassFail2,SuiteX,TestID,Elapsed2))),fail.
some_test_pass_fail(SuiteX,passed(SuiteX)):- test_results(pass,SuiteX,_TestID,_Elapsed).
some_test_pass_fail(SuiteX,failed(SuiteX)):- test_results(fail,SuiteX,_TestID,_Elapsed).

some_test_suite_name_good(SuiteX):- test_suite_name(SuiteX),
  SuiteX\==test_names_ord_hard,
  %SuiteX\==test_names_by_hard,
  SuiteX\==all_arc_test_name_unordered,
  SuiteX\==test_names_by_hard_rev.


test_suite_info(SuiteX,TestID):- var(SuiteX),!,forall(some_test_suite_name(SuiteX),test_suite_info(SuiteX,TestID)),
  !,some_test_suite_name(SuiteX),test_suite_info(SuiteX,TestID).
test_suite_info(SuiteX,TestID):- test_suite_testIDs(SuiteX,Set),!,member(TestID,Set).

:- dynamic(muarc_tmp:skip_calc_suite_test/1).
test_suite_testIDs(SuiteX,Set):- muarc_tmp:cached_tests_hard(SuiteX,Set).
test_suite_testIDs(SuiteX,Set):- muarc_tmp:cached_tests(SuiteX,Set).
test_suite_testIDs(SuiteX,Set):- SuiteX==all_arc_test_name_unordered,!,findall(TestID,all_arc_test_name_unordered(TestID),Set).
test_suite_testIDs(SuiteX,_Set):- muarc_tmp:skip_calc_suite_test(SuiteX),
  pp(looped(muarc_tmp:skip_calc_suite_test(SuiteX))),fail.
test_suite_testIDs(SuiteX,Set):- 
  setup_call_cleanup(assert(muarc_tmp:skip_calc_suite_test(SuiteX),Ref),
    (findall(TestID_C,test_suite_info_1(SuiteX,TestID_C),List),
     list_to_set(List,Set),ignore((acceptable_test_set(Set),some_test_suite_name_good(SuiteX),
       retractall(muarc_tmp:cached_tests(SuiteX,_)),
       asserta(muarc_tmp:cached_tests(SuiteX,Set))))),
    erase(Ref)).

:- meta_predicate(test_suite_info_1(+,?)).



%test_suite_info_1(SuiteX,TestID):- var(SuiteX),!,some_test_suite_name(SuiteX),test_suite_info_1(SuiteX,TestID).
test_suite_info_1(SuiteX,TestID):- muarc_tmp:cached_tests(SuiteX,Set),member(TestID,Set).
test_suite_info_1(SuiteX,TestID):- muarc_tmp:cached_tests_hard(SuiteX,Set),member(TestID,Set).
test_suite_info_1(passed(SuiteX),TestID):- test_results(pass,SuiteX,TestID,_Info). 
test_suite_info_1(failed(SuiteX),TestID):- test_results(fail,SuiteX,TestID,_Info). 
%test_suite_info_1(SuiteX,TestID):- var(TestID), all_arc_test_name_unordered(TestID),test_suite_info(SuiteX,TestID).
test_suite_info_1(icecuber_fail,TestID):- icu(Name,PF),PF == -1,atom_id_e(Name,TestID).
test_suite_info_1(icecuber_pass,TestID):- icu(Name,PF),PF \== -1,atom_id_e(Name,TestID).
test_suite_info_1(dbigham_fail,TestID):- all_arc_test_name_unordered(TestID),
   \+ test_suite_info(dbigham_train_core,TestID),
   \+ test_suite_info(dbigham_train_pass,TestID),
   \+ test_suite_info(dbigham_eval_pass,TestID).
test_suite_info_1(SuiteX,TestID):- suite_tag(SuiteX,List),tasks_split(TestID,List).
test_suite_info_1(SuiteX,TestID):- test_info_no_loop(TestID,Sol), suite_mark(SuiteX,Sol).
test_suite_info_1(SuiteX,TestID):- nonvar(SuiteX), callable_arity(SuiteX,1),!,call(SuiteX,TestID).


clauses_predicate_cmpd_goal(F/N,Into):- !,
  clauses_predicate(M:F/N,P1),clause(M:P1,Body),first_cmpd_goal(Body,Goal),
  compound(Goal),functor(Goal,Into,_),arg(1,P1,Var1), \+ \+ (sub_term(Cmpd,Goal),compound(Cmpd),arg(1,Cmpd,Var2),Var1==Var2).
clauses_predicate_cmpd_goal(M:F/N,Into):- 
   clauses_predicate(M:F/N,P1),clause(M:P1,Body),first_cmpd_goal(Body,Goal),
   compound(Goal),functor(Goal,Into,_),arg(1,P1,Var1), \+ \+ (sub_term(Cmpd,Goal),compound(Cmpd),arg(1,Cmpd,Var2),Var1==Var2).



:- dynamic(muarc_tmp:p1_memo_caches/2).

:- multifile(muarc:clear_all_caches/0).
:- dynamic(muarc:clear_all_caches/0).
muarc:clear_all_caches:-  retractall(muarc_tmp:p1_memo_caches(_,_)), fail.

memoized_p1(P1,F):- muarc_tmp:p1_memo_caches(P1,L),!,member(F,L).
memoized_p1(P1,F):- findall(E,call(P1,E),S), list_to_set(S,L), asserta(muarc_tmp:p1_memo_caches(P1,L)),!,member(F,L).


test_suite_name_by_call(F):- memoized_p1(test_suite_name_by_call_1,F).

test_suite_name_by_call_1(F):- clauses_predicate_cmpd_goal(F/1,F),member(ensure_arc_test_properties,foreach_test).
%test_suite_name_by_call_1(F):- clauses_predicate_cmpd_goal(F/1,every_grid).
test_suite_name_by_call_1(no_pair(P1)):-every_pair(P1). 
test_suite_name_by_call_1(every_pair(P1)):-every_pair(P1). 

test_suite_name_by_call_1(no_grid(P1)):-is_monadic_grid_predicate(P1). 
test_suite_name_by_call_1(every_grid(P1)):-is_monadic_grid_predicate(P1). 
test_suite_name_by_call_1(no_input_grid(P1)):-is_monadic_grid_predicate(P1). 
test_suite_name_by_call_1(every_input_grid(P1)):-is_monadic_grid_predicate(P1). 
test_suite_name_by_call_1(no_output_grid(P1)):-is_monadic_grid_predicate(P1). 
test_suite_name_by_call_1(every_output_grid(P1)):-is_monadic_grid_predicate(P1). 


is_monadic_grid_predicate(F):-  clauses_predicate_cmpd_goal(F/1,Into_Grid),member(Into_Grid,[ensure_grid,into_grid]).
is_monadic_grid_predicate(F):-  luser_getval(generate_gids,true), clauses_predicate_cmpd_goal(F/1,Into_Grid),member(Into_Grid,[ensure_gid]).

io_side_effects.

every_grid(P1,TestID):-   every_grid(TestID,_ExampleNum,_IO,P1).
every_input_grid(P1,TestID):- every_grid(TestID,_,in,P1).
every_output_grid(P1,TestID):- every_grid(TestID,trn+_,out,P1).

no_grid(P1,TestID):- no_input_grid(P1,TestID),no_output_grid(P1,TestID).
no_input_grid(P1,TestID):- every_input_grid(not_p1(P1),TestID).
no_output_grid(P1,TestID):- every_output_grid(not_p1(P1),TestID).


every_grid(TestID,ExampleNum,IO,P1):-
  all_arc_test_name_unordered(TestID),
  forall((kaggle_arc_io(TestID,ExampleNum,IO,G),(IO=in->true;ExampleNum=(trn+_))),call(P1,G)).

%is_monochromish(I,O):- unique_colors(I,A),A=[_,_],unique_colors(O,B),sort_safe(A,AB),sort_safe(B,AB).
%is_colorchanging(I,O):- unique_fg_colors(I,A),unique_fg_colors(O,B),sort_safe(A,AB),\+ sort_safe(B,AB).
%is_monochromish(Grid):- ensure_grid(Grid), unique_color_count(Grid,[_,_]).
is_size_1x1(Grid):- ensure_grid(Grid), mgrid_size(Grid,1,1).
is_size_3x3(Grid):- ensure_grid(Grid), mgrid_size(Grid,3,3).
is_size_lte_3x3(Grid):- ensure_grid(Grid), mgrid_size(Grid,H,V),H=<3,V=<3.
%is_size_lte_10x10(Grid):- ensure_grid(Grid), mgrid_size(Grid,H,V),H=<10,V=<10.
is_size_gte_20x20(Grid):- ensure_grid(Grid), mgrid_size(Grid,H,V),H>=20,V>=20.
is_mass_lte_25(Grid):- ensure_grid(Grid), mmass(Grid,Mass),Mass=<25.
%is_mass_lte_81(Grid):- ensure_grid(Grid), mmass(Grid,Mass),Mass=<81.
%is_mass_gte_600(Grid):- ensure_grid(Grid), mmass(Grid,Mass),Mass>=600.

is_colors_adjacent(Grid):- ensure_gid(Grid,GID), ensure_cmem(GID),
  cmem(GID,HV1,C1),is_adj_point_es_d(HV1,HV2),cmem(GID,HV2,C2),
  C1\==C2,is_fg_color(C1),is_fg_color(C2),!.

is_colors_adjacent_no_d(Grid):- ensure_gid(Grid,GID), ensure_cmem(GID),
  cmem(GID,HV1,C1),is_adj_point_es(HV1,HV2),cmem(GID,HV2,C2),
  C1\==C2,is_fg_color(C1),is_fg_color(C2),!.



%mgrid_size(A,B,C):- arc_memoized(grid_size(A,B,C)),!.
%mmass(A,B):- arc_memoized(mass(A,B)),!.

not_p2(P2,I,O):- \+ call(P2,I,O).
not_p1(P1,I):- \+ call(P1,I).

%all_pairs_change_size(TestID):- every_pair(TestID,trn+_,op_op(grid_size_term,(\==))).
%every_pair(is_colorchanging).
%every_pair(is_monochromish).
every_pair(op_op(grid_size_term,(\==))).

no_pair(P2,TestID):- every_pair(TestID,trn+_,not_p2(P2)).
every_pair(P2,TestID):- every_pair(TestID,trn+_,P2).

every_pair(TestID,ExampleNum,P2):-
  all_arc_test_name_unordered(TestID),
  forall(kaggle_arc(TestID,ExampleNum,I,O),call(P2,I,O)).


%into_numbers(N,List):- findall(Num,(sub_term(Num,N),number(Num)),List),List\==[].
%num_op(OP,V1,V2):- number(V1),number(V2),!,call(OP,V1,V2).
%num_op(OP,V1,V2):- into_numbers(V1,Ns1),into_numbers(V2,Ns2),my_maplist(num_op(OP),Ns1,Ns2).







suite_mark(SuiteX,Sol):- sub_term(E,Sol),suite_mark1(SuiteX,E),!.
suite_mark1(_SuiteX,E):- var(E),!,fail.
suite_mark1( SuiteX,E):- SuiteX=@=E,!.
suite_mark1('*'(SuiteX),E):-nonvar(SuiteX),E=SuiteX.
suite_mark1( SuiteX,E):- compound(E),compound_name_arity(E,F,_),!,suite_mark1(SuiteX,F).
    
full_test_suite_list(Set):-   findall(SN,some_test_suite_name(SN),List),list_to_set(List,Set).
test_suite_list(AtLeastTwo):- full_test_suite_list(Set),include(at_least_two_tests,Set,AtLeastTwo).
at_least_two_tests(SuiteX):- 
  nop((current_suite_testnames(SuiteX,Tests),
  length(Tests,L),L>=2)).

ensure_level_1_test_info:-!.
ensure_level_1_test_info:- precache_all_grid_objs.

:- meta_predicate(when_html(0)).
when_html(G):- arc_html->call(G);true.

grid_to_task_pair(Grid,TestIDExample):- ground(Grid),was_grid_gid(Grid,TestIDExample),!.
grid_to_task_pair(Grid,TestIDExample):- ground(Grid),is_grid_tid(Grid,TestIDExample),!.
grid_to_task_pair(Grid,OID):- grid_to_image_oid(Grid,OID),!.
%grid_to_task_pair(_,TestID>Example):- get_current_test(TestID),some_current_example_num(Example),!.
grid_to_task_pair(_,Unknown):- gensym(grid_table_,Unknown).

grid_to_image_oid(Grid,OID):- ground(Grid), !, oid_to_global_grid(OID,Grid),!.
grid_to_image_oid(Grid,OID):- copy_term(Grid,GridC), oid_to_global_grid(OID,GridC),
  oid_to_global_grid(OID,GridCC),GridCC=@=Grid,!.


full_test_suite_list:-
  ensure_level_1_test_info,
 (luser_getval(test_suite_name,SuiteXC); SuiteXC=[]),
 full_test_suite_list(L),
 %when_html(write('<style type="text/css">a {color: cyan;} body {background-color: black; color: white; background-blend-mode: difference; mix-blend-mode: difference}</style>')),
 when_html(write('<style type="text/css">a {color: cyan;} body { background-blend-mode: difference; mix-blend-mode: difference}</style>')),
 ((forall(nth_above(300,N,L,SuiteX),
  (%nl,%current_suite_testnames(SuiteX,_),
   print_menu_cmd1((format(' ~w:  ',[N]),
   report_suite_test_count(SuiteX)),N),ignore((SuiteX==SuiteXC,write('   <<<<-------'))))))).

report_suites:-  
 ensure_level_1_test_info,
 (luser_getval(test_suite_name,SuiteXC); SuiteXC=[]),
 test_suite_list(L),
 %when_html(write('<style type="text/css">a {color: cyan;} body {background-color: black; color: white; background-blend-mode: difference; mix-blend-mode: difference}</style>')),
 when_html(write('<style type="text/css">a {color: cyan;} body { background-blend-mode: difference; mix-blend-mode: difference}</style>')),
 ((forall(nth_above(300,N,L,SuiteX),
  (nl,current_suite_testnames(SuiteX,_),
   wots(SS,(format(' ~w:  ',[N]),report_suite_test_count(SuiteX))),
   print_menu_cmd1(SS,N),ignore((SuiteX==SuiteXC,write('   <<<<-------'))))))).



test_prop_example(TPE):-   less_fav(_,PropList),member(Prop,PropList),as_test_prop(Prop,TPE).
test_prop_example(TPE):-   fav(_,PropList),member(Prop,PropList),as_test_prop(Prop,TPE).

test_suite_marker(Prop):- return_sorted(Prop,test_prop_example(Prop)).
%as_test_prop(Prop,Prop):- atom(Prop), \+ atom_contains(Prop,'/'), \+ atom_contains(Prop,' ').
as_test_prop(+Prop,+Prop):- atom(Prop).
as_test_prop(-Prop,-Prop):- atom(Prop).
%as_test_prop(Prop,'?'(Prop)):- atom(Prop), \+ atom_contains(Prop,'/'), nop((\+ atom_contains(Prop,' '))).
as_test_prop(Prop,Prop):- atom(Prop), use_atom_test(Prop).
as_test_prop(Prop,F):- compound(Prop),compound_name_arity(Prop,F,_), use_atom_test(F).


prev_test:-  must_det_ll((get_current_test(TestID), get_prev_test(TestID,NextID), set_current_test(NextID))).
next_test:- get_current_test(TestID), notrace((get_next_test(TestID,NextID), set_current_test(NextID))),!.
next_random_test:-  randomize_suite, next_test.
is_valid_testname(TestID):- nonvar(TestID), kaggle_arc(TestID,_,_,_).
is_valid_atom_testname(TestID):- nonvar(TestID), once(kaggle_arc(t(TestID),_,_,_);kaggle_arc(v(TestID),_,_,_)).

report_test:- report_suite_test_count, print_qtest.

get_current_test(TestID):- luser_getval(task,TestID),is_valid_testname(TestID),!.
get_current_test(TestID):- get_next_test(TestID,_),!.
get_current_test(TestID):- get_current_test_fb(TestID),kaggle_arc(TestID,_,_,_),!.
get_current_test(TestID):- kaggle_arc(TestID,_,_,_),!.
get_current_test_fb(t('00d62c1b')).
get_current_test_fb(v(fe9372f3)).

get_current_test_atom(UUID):- get_current_test(TestID),test_id_atom(TestID,UUID).

test_id_atom(TestID,UUID):- ((compound(TestID),arg(1,TestID,UUID));UUID=TestID),atom(UUID),!.
test_id_atom(TestID,OID):- sub_atom_value(TestID,OID),is_valid_atom_testname(OID),!.
test_id_atom(TestID,OID):- sub_atom_value(TestID,OID),!.

get_random_test(ID):-  
 get_current_test(TestID),get_next_test(TestID,NextID),
 get_current_suite_testnames(List),random_member(ID,List),ID\==TestID, ID\==NextID,!.

get_next_test(TestID,NextID):- get_current_suite_testnames(List), next_in_list(TestID,List,NextID).
get_prev_test(TestID,PrevID):-  get_current_suite_testnames(List), prev_in_list(TestID,List,PrevID).
next_in_list(TestID,List,Next):- append(_,[TestID,Next|_],List)-> true; List=[Next|_].
prev_in_list(TestID,List,PrevID):-  once(append(_,[PrevID,TestID|_],List); last(List,PrevID)).

%v(f9d67f8b)
:- export(load_last_test_name/0).
system:load_last_test_name:- 
  muarc:arc_settings_filename(Filename),
  notrace((exists_file(Filename),setup_call_cleanup(open(Filename,read,O),ignore((read_term(O,TestID,[]),luser_setval(task,TestID))),close(O)))),!.
system:load_last_test_name:- set_current_test(v(fe9372f3)).

system:save_last_test_name:- notrace(catch(save_last_test_name_now,_,true)),!.
system:save_last_test_name_now:- muarc:arc_settings_filename(Filename),
  ignore(notrace((luser_getval(task,TestID), tell(Filename),format('~n~q.~n',[TestID]),told))).

muarc:arc_settings_filename(Filename):- muarc:arc_settings_filename1(File), 
  (exists_file(File) -> (Filename=File) ; absolute_file_name(File,Filename,[access(append),file_errors(fail),expand(true)])).
muarc:arc_settings_filename1('muarc_tmp/current_test').
muarc:arc_settings_filename1('~/.arc_current_test').
muarc:arc_settings_filename1('/tmp/.arc_current_test').


set_current_test(Name):-  
  ignore((testid_name_num_io_0(Name,TestID,Example,NumE,_IO),
    ignore((is_valid_testname(TestID),really_set_current_test(TestID))),
    ignore((nonvar(Example),set_example_num(Example+NumE))))).

really_set_current_test(TestID):-
  once(luser_getval(task,WTestID);WTestID=[]),
  ignore((WTestID\==TestID,luser_setval(task,TestID), test_id_atom(TestID,Atom),set_html_component(task,Atom))),
  once(luser_getval(last_test_name,WasTestID);WasTestID=[]),
  ignore((WasTestID\==TestID, new_current_test_info(WasTestID,TestID))).



some_current_example_num(_):- get_pair_mode(whole_test), !.
some_current_example_num(_):- get_pair_mode(entire_suite), !.
some_current_example_num(ExampleNum):- foc_current_example_num(ExampleNum).

foc_current_example_num(ExampleNum):- get_example_num(ExampleNum),!.
foc_current_example_num(ExampleNum):- ExampleNum = trn+0, set_example_num(ExampleNum),!.
foc_current_example_num(ExampleNum):- ignore(get_example_num(ExampleNum)),set_example_num(ExampleNum),!.


current_test_example(TestID,ExampleNum):- get_current_test(TestID),
  (get_example_num(ExampleNum) -> true ; must_det_ll(first_current_example_num(ExampleNum))).

get_example_num(ExampleNum):- \+ arc_html, nb_current(example,ExampleNum),ground(ExampleNum),ExampleNum\==[],!.
get_example_num(ExampleNum):- luser_getval(example,ExampleNum),ground(ExampleNum),ExampleNum\==[],!.

set_example_num(_ > ExampleNum):- nonvar(ExampleNum), !, set_example_num(ExampleNum).
set_example_num(ExampleNum * _):- nonvar(ExampleNum), !, set_example_num(ExampleNum).
set_example_num(Trn+N):- nonvar(Trn),!,luser_setval(example,Trn+N).
set_example_num(Atom):- must_det_ll((testid_name_num_io(Atom,_TID,Trn,N,_IO),luser_setval(example,Trn+N))),!.

tid_to_id((TID>(Trn+Num)*IO),(TID>(Trn+Num)*IO)).
tid_to_id(Atom,(TID>(Trn+Num)*IO)):- testid_name_num_io(Atom,TID,Trn,Num,IO),!.

first_current_example_num(TrnN):- some_current_example_num(TrnN),ground(TrnN),TrnN\==[],
  get_current_test(TestID),kaggle_arc(TestID,TrnN,_,_),!.
first_current_example_num(TrnN):- TrnN = trn+0.

next_pair:- with_pair_mode(single_pair,next_pair0).
next_pair0:- 
  %get_current_test(TestID),
  first_current_example_num(TrnN),  
  next_pair(TrnN,ExampleNum),
  set_example_num(ExampleNum),
  set_pair_mode(single_pair), %(ExampleNum=(tst+_)->set_pair_mode(whole_test);set_pair_mode(single_pair)),  
  ((ExampleNum==trn+0)->next_test;true),
  print_single_pair,!.

next_pair(Tst+N,Trn+0):-  pair_count(Tst,N2), N>=N2,!,trn_tst(Tst,Trn).
next_pair(Trn+N,Trn+N2):- N2 is N+1.
pair_count(Trn,C):- get_current_test(TestID),findall(N2,kaggle_arc(TestID,Trn+N2,_,_),List),sort_safe(List,Sort),last(Sort,C).
prev_pair(Tst+0,Trn+N2):- trn_tst(Tst,Trn),!,pair_count(Trn,N2).
prev_pair(Trn+N,Trn+N2):- N2 is N-1.

prev_pair:- with_pair_mode(single_pair,prev_pair0).
prev_pair0:- 
  %get_current_test(TestID),
  first_current_example_num(TrnN),  
  prev_pair(TrnN,ExampleNum),
  set_example_num(ExampleNum),
  set_pair_mode(single_pair),%(ExampleNum=(tst+_)->set_pair_mode(whole_test);set_pair_mode(single_pair)),  
  ((TrnN==trn+0)->prev_test;true),
  print_single_pair.

trn_tst(trn,tst).
trn_tst(tst,trn).


new_current_test_info(WasTestID,TestID):-   
  luser_setval(next_test_name,TestID),  
  forall(on_leaving_test(WasTestID),true),
  (\+ get_current_test(TestID)->set_current_test(TestID);true),
  ignore((  
  %luser_getval(task,TestID),
  %pp(fav(TestID,[])),
  %set_example_num(tst+0),
  luser_setval(last_test_name,TestID))),
  save_last_test_name,
  luser_setval(prev_test_name,WasTestID),
  forall(on_entering_test(TestID),true),
  maybe_set_suite(TestID).

maybe_set_suite(TestID):-   luser_getval(test_suite_name,Suite),test_suite_info_1(Suite,TestID),!.
maybe_set_suite(TestID):-   some_test_suite_name(Suite), test_suite_info_1(Suite,TestID),!,set_test_suite_silently(Suite).
%maybe_set_suite(TestID):-   some_test_suite_name_good(Suite), muarc_tmp:cached_tests(Suite,Set),member(TestID,Set),!,set_test_suite(Suite).
maybe_set_suite(_TestID).
  
  %clear_training(TestID).

test_name_ansi_output_file(TestID,File):- is_valid_atom_testname(TestID),!,
  atomic_list_concat(['muarc_cache/test_state/',TestID,'.ansi'],File1),
  arc_sub_path(File1,File),!.
test_name_ansi_output_file(TestID,File):- \+ atom(TestID),
  test_id_atom(TestID,OID), test_name_ansi_output_file(OID,File).
test_name_ansi_output_file(TestID,File):- arc_sub_path(TestID,File),!.


test_name_output_file(TestID,Ext,ExtFile):- 
  test_name_ansi_output_file(TestID,File),
  ensure_file_extension(File,Ext,ExtFile),!.


maybe_append_file_extension(File,DotExt,NewName):- \+ atom_concat(_,DotExt,File), atom_concat(File,DotExt,NewName).

ensure_file_extension(File,DotExt,NewName):- var(File),!,atom_concat(File,DotExt,NewName).
ensure_file_extension(File,DotExt,NewName):- maybe_append_file_extension(File,DotExt,NewName),!.
ensure_file_extension(File,  _Ext,File):-!.

call_file_goal(S,encoding(Enc)):- arc_set_stream(S,encoding(Enc)),!.
call_file_goal(_, discontiguous(_)):- !.
call_file_goal(_,Goal):- call(Goal),!.

load_file_dyn(File):- maybe_append_file_extension(File,'.pl',NewName),!,load_file_dyn(NewName).
load_file_dyn(File):- \+ exists_file(File), !.
load_file_dyn(File):- load_file_dyn_pfc(File),!.
%load_file_dyn(File):- consult(File),!.
load_file_dyn_pfc(File):- 
 open(File,read,I),
 repeat,read_term(I,Term,[]),
 (Term = end_of_file -> ! ; (must_det_ll(load_file_term(I,Term)),fail)).
load_file_term(S,(:- Goal)):- !, call_file_goal(S,Goal).
load_file_term(_,Term):- assertz_new(Term),!.
%load_file_term(Term):- arc_assert(Term),!.
%load_file_term(Term):- pfcAdd(Term).
:- luser_default(prev_test_name,'.').
:- luser_default(next_test_name,'.').

test_html_file('.','.'):-!.
test_html_file(Nil,'.'):- Nil == [], !.
test_html_file(TestID,This):- string(TestID),This=TestID,!.
test_html_file(TestID,This):- sub_atom_value(TestID,HashID),sformat(This,'~w.html',[HashID]).
write_tee_link(W,TestID):- 
 test_html_file(TestID,This),
 format('<p/> <a href="~w">~w ~w</a> ',[This,W,This]).



on_entering_test(TestID):- is_list(TestID),!,my_maplist(on_entering_test,TestID).
on_entering_test(TestID):- 
 must_det_ll((
  set_current_test(TestID),
  write_test_links_file,
  clear_tee,  
  %force_flush_tee,
  clear_test(TestID),
  test_name_output_file(TestID,'.pl',PLFile),
  nop((load_file_dyn(PLFile))))).


on_leaving_test(TestID):- is_list(TestID),!,my_maplist(on_leaving_test,TestID).
on_leaving_test(TestID):-     
 must_det_ll((
  save_test_hints(TestID),
  clear_test(TestID),  
  flush_tee,
  clear_tee)).

test_html_file_name(FN):- 
  get_current_test(TestID),
  test_html_file(TestID,This),
  (This == [] -> FN = [] ; format(atom(FN),'muarc_output/~w',[This])).

%worth_saving(FN):- \+ exists_file(FN),!.
worth_saving:- test_html_file_name(FN), \+ exists_file(FN), !.
worth_saving:- test_html_file_name(FN), size_file(FN,Size), Size < 10_000,!.
worth_saving:- size_file('muarc_tmp/tee.ansi',Size), Size > 20_000.

:- set_prolog_flag(nogc,false).

no_tee_file:- \+ (getenv('TEE_FILE',File), exists_file(File)).

install_ansi2html:- shell('pip install ansi2html').

begin_tee:- no_tee_file,!.
begin_tee:- install_ansi2html,get_current_test(TestID),on_entering_test(TestID),tee_op((at_halt(exit_tee))).
flush_tee_maybe:- force_full_tee.

flush_tee:- tee_op(ignore((worth_saving -> force_flush_tee ; true))).
force_flush_tee:- tee_op((
   my_shell_format('tail -20000 muarc_tmp/tee.ansi > muarc_tmp/tee1000.ansi',[]),
   tee_to_html('muarc_tmp/tee1000.ansi'))).

force_full_tee:-   
  tee_op((
   my_shell_format('cat muarc_tmp/tee.ansi > muarc_tmp/tee1000.ansi',[]),
   tee_to_html('muarc_tmp/tee1000.ansi'))).


tee_to_html(Tee1000):-
 tee_op((
  test_html_file_name(FN),% -W -a
  ignore((FN \== [], 
   my_shell_format('cat kaggle_arc_header.html > ~w',[FN]),
   my_shell_format('cat muarc_tmp/test_links ~w muarc_tmp/test_links | ansi2html -u -a >> ~w',[Tee1000,FN]),
   my_shell_format('cat kaggle_arc_footer.html >> ~w',[FN]))))).

clear_test_html :- tee_op((tee_to_html('muarc_tmp/null'))). % was /dev/null
clear_tee:- force_full_tee, tee_op((shell('cat muarc_tmp/null > muarc_tmp/tee.ansi'))).
exit_tee:-  get_current_test(TestID),on_leaving_test(TestID).

write_test_links_file:- tee_op((notrace((setup_call_cleanup(tell('muarc_tmp/test_links'), write_test_links, told))))).
write_test_links:-  
 format('~N'), ensure_test(TestID), 
 tee_op((
  ignore((get_prev_test(TestID,PrevID),write_tee_link('Prev',PrevID))),
  ignore((((luser_getval(prev_test_name,AltPrevID),AltPrevID\==PrevID,AltPrevID\==TestID,AltPrevID\=='.'),write_tee_link('AltPrevID',AltPrevID)))),
  ignore(write_tee_link('This',TestID)),
  ignore((get_next_test(TestID,NextID),write_tee_link('Next',NextID))),
  ignore((((luser_getval(next_test_name,AltNextID),NextID\==AltNextID,AltNextID\==TestID,AltNextID\=='.'),write_tee_link('AltNextID',AltNextID)))),  
  format('~N<pre>'))).

tee_op(_):- no_tee_file,!.
tee_op(G):- ignore(notrace(catch(call(G),E,u_dmsg(G=E)))).
shell_op(G):- tee_op(G).

my_shell_format(F,A):- shell_op((sformat(S,F,A), shell(S))).

warn_skip(Goal):- u_dmsg(warn_skip(Goal)).

save_test_hints(TestID):- is_list(TestID),!,my_maplist(save_test_hints,TestID).
save_test_hints(TestID_IN):- ensure_test(TestID_IN,TestID), save_test_hints(TestID,_File).

save_test_hints(TestID,File):- var(TestID),!, forall(ensure_test(TestID), save_test_hints(TestID,File)).
save_test_hints(TestID,File):- var(File),!,test_name_output_file(TestID,'.pl',File), save_test_hints(TestID,File).
save_test_hints(TestID,File):- maybe_append_file_extension(File,'.pl',NewName),!,save_test_hints(TestID,NewName).

save_test_hints(TestID,File):- !, warn_skip(save_test_hints(TestID,File)).
save_test_hints(TestID,File):-
 saveable_test_info(TestID,Info),
   setup_call_cleanup(open(File,write,O,[create([default]),encoding(text)]), 
       with_output_to(O,(
         write_intermediatre_header,
         my_maplist(print_ref,Info))),
      close(O)), 
   nop(statistics).



clear_test(TestID):- is_list(TestID),!,my_maplist(clear_test,TestID).
clear_test(TestID):- ensure_test(TestID),
   clear_training(TestID),
   %warn_skip
   (clear_saveable_test_info(TestID)),
   unload_test_file(TestID).

clear_saveable_test_info(TestID):- 
   saveable_test_info(TestID,Info),
   erase_refs(Info).
   

erase_refs(Info):- my_maplist(erase,Info).

unload_test_file(TestID):- unload_file_dyn(TestID).

unload_file_dyn(File):- test_name_output_file(File,'.pl',NewName),NewName\=@=File,!,unload_file_dyn(NewName).
unload_file_dyn(File):- \+ exists_file(File), !.
unload_file_dyn(File):- unload_file_dyn_pfc(File), unload_file(File),!.

unload_file_dyn_pfc(File):-  
 open(File,read,I),
 repeat,read_term(I,Term,[]),
 (Term = end_of_file -> ! ; (must_det_ll(unload_file_term(I,Term)),fail)).

unload_file_term(S,(:- Goal)):- !, call_file_goal(S,Goal).
unload_file_term(_,Term):- forall(retract(Term),true),!.
%unload_file_term(Term):- pfcRemove(Term).

/*
clear_test_training(TestID):- 
 must_det_ll((
     ignore(( 
      \+ arc_option(extreme_caching),
      
      test_name_output_file(TestID,'.pl',NewName),
      unload_file(File),
      (exists_file(File)->delete_file(File);true))),
*/
clear_training(TestID):- ensure_test(TestID),
  %retractall(arc_cache:individuated_cache(_,_,_)),
  set_bgc(_),
  set_flag(indiv,0),
  retractall(arc_test_property(TestID,_,_,_)),
  training_info(TestID,InfoSet), erase_refs(InfoSet),
  forall(test_local_dyn(F),
   forall((current_predicate(F/A),A\==0),
    ((functor(X,F,A),
      forall((clause(X,_,Ref),arg(1,X,E),E==TestID),
       erase(Ref)))))),
  nb_delete(grid_bgc),
  luser_linkval(test_rules, [rules]),
  wno((clear_shape_lib(test), clear_shape_lib(noise), 
   retractall(grid_nums(_,_)), retractall(grid_nums(_)))),
  nop(retractall(g_2_o(_,_))),!,
  retractall(arc_test_property(TestID,_,_,_)).






new_test_pair(PairName):-
  luser_getval(new_test_pair,Was), Was == PairName,!,
  luser_setval(new_test_pair,waz).
new_test_pair(PairName):-
  luser_setval(new_test_pair,PairName),
  %nb_delete(grid_bgc),
  clear_shape_lib(pair),clear_shape_lib(in),clear_shape_lib(out),
  luser_setval(test_pairname,PairName),
  luser_linkval(pair_rules, [rules]),
  retractall(is_shared_saved(PairName*_,_)),
  retractall(is_shared_saved(PairName>_,_)),
  retractall(is_shared_saved(PairName,_)),
  retractall(is_unshared_saved(PairName*_,_)),
  retractall(is_unshared_saved(PairName,_)),
  retractall(is_grid_tid(PairName*_,_)),
  retractall(is_grid_tid(PairName>_,_)),
  retractall(is_grid_tid(PairName,_)),!.

human_test:- solve_test_trial(human).

fully_train:- print_test,train_test.
fully_test:- fully_train, !, solve_test, !.
run_next_test:- notrace(next_test), fully_test.

www_demo:- as_if_webui(demo).

info(Info):- nonvar(Info),u_dmsg(Info).
system:demo:- 
  catch_log(reverse_suite),
  update_changes,!,clear_tee,
  begin_tee,interact.

:- export(demo/0).
rat:- info("Run all tests"), run_all_tests.
noninteractive_test(X):- my_time(ignore(forall(arc1(true,X),true))).


cmt_border:- format('~N% '), dash_chars(120,"="), !, nl.

test_id_border(TestID):-
    get_current_test(WasTestID),
    ignore((WasTestID\==TestID,set_current_test(TestID), cmt_border)).

print_whole_test(Name):- fix_test_name(Name,TestID), with_pair_mode(whole_test,print_test(TestID)).

maybe_set_suite:- get_current_test(TestID),maybe_set_suite(TestID).

print_single_pair(TName):- is_cgi,!,preview_test(TName).
print_single_pair(TName):-
 ignore((
  fix_test_name(TName,TestID,ExampleNum),
  ensure_test(TestID),
  ignore(first_current_example_num(ExampleNum)),
  forall(once(kaggle_arc(TestID,ExampleNum,In,Out)),
       print_single_pair(TestID,ExampleNum,In,Out)),
     write('%= '), parcCmt(TestID))),!.

print_test(TName):- (is_cgi ; arc_html),!,preview_test(TName).
print_test(TName):-    
  fix_test_name(TName,TestID,ExampleNum1),  
  arc_user(USER),  
  %set_example_num(ExampleNum1),
   cmt_border,format('%~w % ?- ~q. ~n',[USER,print_test(TName)]),cmt_border,
   (arc_html->true;ignore(print_test_hints(TestID))),
   format('~N% '),dash_chars,
    forall(arg(_,v((trn+_),(tst+_)),ExampleNum1),
     forall(kaggle_arc(TestID,ExampleNum1,In,Out),
          print_single_pair(TestID,ExampleNum1,In,Out))),
  write('%= '), parcCmt(TestID),!,force_full_tee.


preview_test(TName):- \+ (is_cgi ; arc_html), !,print_single_pair(TName),!.
preview_test(TName):- (is_cgi ; arc_html),!,
 fix_test_name(TName,TestID,ExampleNum1),
 test_id_atom(TestID,TestAtom),
 when_in_html((write('<hr>'),write_nav_cmd(pp(TestID),navCmd(TestAtom)))),
 write('<span class="flex_row">'),
  %set_example_num(ExampleNum1),
  forall(arg(_,v((trn+_),(tst+_)),ExampleNum1),
   forall(kaggle_arc(TestID,ExampleNum1,In,Out),
     print_single_pair(TestID,ExampleNum1,In,Out))),
 write('</span>'),
 when_in_html(write('<hr>')),!.



print_single_pair(TestID,ExampleNum,In,Out):- 
  as_d_grid(In,In1),as_d_grid(Out,Out1), xfer_zeros(In1,Out1),!,
   print_single_pair_pt2(TestID,ExampleNum,In1,Out1),!.

print_single_pair_pt2(TestID,ExampleNum,In,Out):- is_cgi,!, 
 must_det_ll((test_id_atom(TestID,TestAtom),
 in_out_name(ExampleNum,NameIn0,RightTitle),
              sformat(NameIn,'~w  "~w" ',[NameIn0,TestAtom]),
 (ID1 = (TestID>ExampleNum*in)),
 (ID2 = (TestID>ExampleNum*out)),
 print_ss_html_pair(cyan, 
   NameIn,navCmd((TestID>ExampleNum)),ID1,In,wqs('Input'),
   TestAtom,navCmd((TestAtom)),ID2,Out,RightTitle))),!.
print_single_pair_pt2(TestID,ExampleNum,In1,Out1):- 
   test_id_atom(TestID,TestAtom),
   in_out_name(ExampleNum,NameIn0,NameOut),!,%easy_diff_idea(TestID,ExampleNum,In1,Out1,LIST),!,
  sformat(NameIn,'~w  "~w" ',[NameIn0,TestAtom]),
   format('~Ntestcase(~q,"\n',[TestID>ExampleNum]),
   call_cleanup(ignore(print_side_by_side(cyan,In1,NameIn,_,Out1,NameOut)),write('").\n\n')),
   format('~N'),
   %ignore((grid_hint_swap(i-o,In,Out))),
   format('~N'),
   ignore(show_reduced_io_rarely(In1^Out1)),!.



next_grid_mode(dots,dashes):-!.
next_grid_mode(_,dots).
switch_grid_mode:- (luser_getval('$grid_mode',Dots);Dots=dots),next_grid_mode(Dots,Dashes),luser_setval('$grid_mode',Dashes).

with_next_grid_mode(Goal):- 
  (luser_getval('$grid_mode',Dots);Dots=dots),next_grid_mode(Dots,Dashes), with_luser('$grid_mode',Dashes,Goal).

as_d_grid(In,Out):- as_d_grid0(In,Out),!.
%as_d_grid(In,Out):- apply_grid_color_order(_,In,Mid),as_d_grid0(Mid,Out),!.


as_d_grid0(In,In):- \+ luser_getval('$grid_mode',dashes),!.
as_d_grid0(In,In1):- as_ngrid(In,In1),!.
as_ngrid(In,In1):- must_det_ll((change_bg_fg(In, _BG, _FG,In0), most_d_colors(In0,_CI,In1))),!.
as_ngrid(In,In):-!.

%change_bg_fg(In,_BG,_FG,In):-!.
change_bg_fg(In,BG,FG,Mid):- 
   get_black(Black),Black=BG,
   must_det_ll((available_fg_colors(Avails),
   unique_colors(In,Colors),subtract(Avails,Colors,CanUse),
   ((fail,last(CanUse,FG))->true;FG=wbg),subst001(In,BG,FG,Mid))),!.

available_fg_colors(Avails):- findall(Color,enum_fg_colors(Color),Avails).

%print_test(TName):- !, parcCmt(TName).
print_qtest:- get_current_test(TestID),print_qtest(TestID).

:- luser_default('$grid_mode',dots).
%print_qtest(TestID):- \+ luser_getval('$grid_mode',dots),!,print_test(TestID).
%print_qtest(TestID):- \+ luser_getval('$grid_mode',dashes),!,print_test(TestID).
print_qtest(TestID):- ensure_test(TestID), \+ get_pair_mode(single_pair), !, print_test(TestID),!.
print_qtest(TestID):- print_single_pair(TestID),!.

print_single_pair:-
   with_pair_mode(single_pair, print_qtest).

get_thingy(In,Out,Gets):- 
  findall(W=GetO,(get_thingy1(In,Out,W,Get),nop(mass(Get,M)->M>0),most_d_colors(Get,_,GetO)),Gets).

non_intersectiond(Black,Target,Target):- get_black(Black),!.
non_intersectiond(Target,Black,Target):- get_black(Black),!.
non_intersectiond(This,Target,Black):- This =@= Target, get_black(Black), !.
non_intersectiond(This,_,That):- copy_term(This,That),!.

get_thingy1(In,Out,"I minus O",NewGrid):-  maplist_ignore(fg_subtractiond,In,Out,NewGrid).
get_thingy1(Out,In,"O shared I",NewGrid):-  maplist_ignore(fg_intersectiond,In,Out,NewGrid).
get_thingy1(Out,In,"O minus I",NewGrid):-  maplist_ignore(fg_subtractiond,In,Out,NewGrid).
get_thingy1(Out,In,"O non_intersectiond I",NewGrid):-  maplist_ignore(non_intersectiond,In,Out,NewGrid).
get_thingy1(In,Out,"I non_intersectiond O",NewGrid):- maplist_ignore(non_intersectiond,In,Out,NewGrid).
get_thingy1(In,_,"IN-NGRID",In).
get_thingy1(_,Out,"OUT-NGRID",Out).
%easy_diff_idea(In1,Out1,MInfo):- 
easy_diff_idea(TestID,ExampleNum,In,Out,[NameIn=In,(NameOut+TestID)=Out|Get]):- fail, is_grid(Out),
  ignore(in_out_name(ExampleNum,NameIn,NameOut)),
  get_thingy(In,Out,Get).

easy_diff_idea(TestID,ExampleNum,In,Out,[NameIn=In,(NameOut+TestID)=Out]):-
  ignore(in_out_name(ExampleNum,NameIn,NameOut)).

other_grid_mode(I^O,II^OO):- with_next_grid_mode((as_d_grid(I,II),as_d_grid(O,OO))).

:- meta_predicate in_out_name(-,+,+).
%in_out_name(trn+NN,SI,SO):- arc_html,N is NN+1, format(atom(SI),'Training Pair #~w Input',[N]),format(atom(SO),'Training Pair #~w Output',[N]),!.
in_out_name(trn+N,SI,SO):- into_title_str(trn+N,SI), format(atom(SO),'Output',[]),!.
in_out_name(tst+N,SI,SO):- into_title_str(tst+N,SI), format(atom(SO),'Output<(REVEALED)>',[]),!.
in_out_name(X,'Input'(X),'Output'(X)):-!.


arc_pair_id(TestID,ExampleNum):- 
  ensure_test(TestID),
 % ignore((luser_getval(example,Example+NumE), Example\==tst , ExampleNum=Example+NumE)),
  kaggle_arc_io(TestID,ExampleNum,in,_).

arc_grid_pair(In,Out):- 
 ((var(In),var(Out))-> arc_pair_id(TestID,ExampleNum); true),
  kaggle_arc(TestID,ExampleNum,In,Out).

arc_grid(Grid):- arc_grid(_In,Grid).
arc_grid(IO,Grid):-
  arc_pair_id(TestID,ExampleNum),
  kaggle_arc_io(TestID,ExampleNum,IO,Grid).

ensure_test(TestID,RealTestID):- fix_test_name(TestID,RealTestID),!,ensure_test(RealTestID).

var_ensure_test(TestID):- ground(TestID), !, is_valid_testname(TestID).
var_ensure_test(TestID):- get_pair_mode(enire_suite),!, all_arc_test_name(TestID).
var_ensure_test(TestID):- \+ get_pair_mode(enire_suite),!,get_current_test(TestID).
%var_ensure_test(TestID):- var(TestID), !, ensure_test(TestID).
%var_ensure_test(TestID):- \+ ground(TestID), !, all_arc_test_name(TestID).

var_ensure_test(TestID,_):- compound(TestID), TestID=(_^_),!,fail.
var_ensure_test(TestID,OUT):- var_ensure_test(TestID),OUT=TestID,is_valid_testname(OUT).

var_ensure_test(TestSpec,I,O,Goal):- 
  var_ensure_test(TestSpec,TestID),
  wots(Title,\+ \+ (I=(+(in)),O=(-(out)),writeln(Goal))),
  forall(with_task_pairs(TestID,ExampleNum,I,O,
    (print_side_by_side(green,I,orig_in(TestID,Title,ExampleNum),_,O,orig_out(TestID,Title,ExampleNum)),
     forall(Goal,true))),true).

ensure_test(TestID):- nonvar(TestID),!, ignore(( is_valid_testname(TestID), really_set_current_test(TestID))).
ensure_test(TestID):- var(TestID), !, var_ensure_test(TestID).
%ensure_test(TestID):- all_arc_test_name(TestID).

all_arc_test_name(TestID):- get_current_test(Test),!,
 (((TestID=Test);(all_suite_test_name(TestID),TestID\=Test);(set_current_test(Test),!,fail))).

all_arc_test_name_unordered(TestID):- kaggle_arc(TestID,trn+0,_,_).

all_suite_test_name(TestID):- get_current_suite_testnames(Set),!,member(TestID,Set).

matches(InfoS,InfoS):-!.
matches(InfoS,InfoM):- member(InfoS,InfoM).

:- abolish(muarc_tmp:test_info_cache,2).
:- dynamic(muarc_tmp:test_info_cache/2).

print_testinfo(TestID):- ensure_test(TestID), forall(test_info_recache(TestID,F),pp(fav(TestID,F))),
  nop(test_show_grid_objs(TestID)),
  findall(III,runtime_test_info(TestID,III),LL),pp(LL).

test_info_no_loop(TestID,Sol):- var(TestID),!,all_arc_test_name_unordered(TestID),test_info_no_loop(TestID,Sol).
test_info_no_loop(TestID,Sol):- muarc_tmp:test_info_cache(TestID,Sol),!. % test_info
test_info_no_loop(TestID,Sol):- some_test_info(TestID,Sol).

ensure_test_info:- muarc_tmp:test_info_cache(_,_)-> true ; ( pp(recreating(test_info)),
  forall_count(all_arc_test_name_unordered(TestID),test_info_recache(TestID,_))).


test_info(TestID,InfoS):- var(TestID),   var(InfoS), !, ensure_test_info,!, 
    all_arc_test_name_unordered(TestID), test_info_no_loop(TestID,InfoS).
test_info(TestID,InfoS):- var(TestID),   nonvar(InfoS),!,all_arc_test_name_unordered(TestID),term_variables(InfoS,Vs),no_repeats(Vs,(test_info(TestID,InfoM),matches(InfoS,InfoM))).
test_info(TestID,InfoS):- nonvar(TestID),once(fix_test_name(TestID,FTestID,_)),TestID\=@=FTestID,!,test_info(FTestID,InfoS).
test_info(TestID,InfoS):- nonvar(TestID),nonvar(InfoS),!,test_info(TestID,InfoM),matches(InfoS,InfoM).
test_info(TestID,InfoS):- nonvar(TestID),var(InfoS),!,test_info_no_loop(TestID,InfoS)*->true;test_info_recache(TestID,InfoS).

test_info1(TestID,Info1):- test_info(TestID,InfoS),member(Info1,InfoS).

test_info_recache(TestID,InfoSS):-  %once((fix_test_name(CTestID,CFTestID,_),CFTestID=TestID)),
  findall(Inf,(all_arc_test_name_unordered(TestID),all_test_info(TestID,Inf)),Info),
  flatten([Info],InfoFF),repair_info(InfoFF,InfoF),list_to_set(InfoF,InfoS),!,
  forall(retract(muarc_tmp:test_info_cache(TestID,_)),true),
  asserta(muarc_tmp:test_info_cache(TestID,InfoS)),!,InfoS=InfoSS.

some_test_info(TestID,III):- some_test_info_1(TestID,Inf0),repair_info(Inf0,III).
some_test_info(TestID,III):- muarc_tmp:test_info_cache(TestID,Inf0),repair_info(Inf0,III).

:- multifile(muarc_tmp:some_test_info_prop/2).
:- dynamic(muarc_tmp:some_test_info_prop/2).

%some_test_info_1(TestID,Sol):- nonvar(TestID),test_info_recache(TestID,Sol),!.
some_test_info_1(TestID,III):- more_test_info(TestID,III).
%some_test_info(X,[keypad]):- key_pad_tests(X). 
some_test_info_1(TestID,III):- fav(TestID,III).
some_test_info_1(TestID,III):- fav_less(TestID,III).
some_test_info_1(TestID,III):- muarc_tmp:some_test_info_prop(TestID,III).

all_test_info(TestID,III):- some_test_info(TestID,III).
all_test_info(TestID,SuiteX):- nonvar(SuiteX),some_test_suite_name(SuiteX),test_suite_info(SuiteX,TestID).

runtime_test_info(TestID,[test_suite(SuiteSet)]):- 
  findall(SuiteX,((some_test_suite_name(SuiteX),test_suite_info_1(SuiteX,TestID))),SuiteS),
  sort_safe(SuiteS,SuiteSet).
runtime_test_info(T,S):- findall(I,test_info(T,I),F),flatten([F],L),list_to_set(L,S),
  retractall(muarc_tmp:test_info_cache(T,_)),
  asserta(muarc_tmp:test_info_cache(T,S)),!.

repair_info(Inf,InfO):- listify(Inf,Inf1),my_maplist(repair_info0,Inf1,InfO).

is_plus_minus_or_sym(+). is_plus_minus_or_sym(-).
is_plus_minus_or_sym(F):- upcase_atom(F,UC),downcase_atom(F,UC).
listify_args(F):- \+ is_plus_minus_or_sym(F).

no_atom_test(print_grid).
no_atom_test(test_suite).
no_atom_test(out_grid).
use_atom_test(F):- no_atom_test(F),!,fail.
use_atom_test(F):- atom_contains(F,' '),!.
use_atom_test(F):- \+ is_plus_minus_or_sym(F), \+ atom_contains(F,'/'), nop((\+ atom_contains(F,' '))).

repair_info0(Inf0,Inf):- is_list(Inf0),!,my_maplist(repair_info0,Inf0,Inf).
repair_info0(Inf,InfO):- compound(Inf),functor(Inf,F,1),listify_args(F),!,arg(1,Inf,A),listify(A,ArgsL),InfO=..[F,ArgsL].
repair_info0(Inf,InfO):- compound(Inf),compound_name_arguments(Inf,F,ArgsL),listify_args(F),!,InfO=..[F,ArgsL].
repair_info0(Inf,Inf).% listify(Inf,InfM),my_maplist(repair_info,InfM,Info).

was_fav(X):- nonvar_or_ci(X), clause(fav(XX,_),true),nonvar_or_ci(XX),X==XX.


alphabetical_v(Set):- findall(v(Name),all_arc_test_name_unordered(v(Name)),List),sort_safe(List,Set).
alphabetical_t(Set):- findall(t(Name),all_arc_test_name_unordered(t(Name)),List),sort_safe(List,Set).


human_t(T):- human_t_set(Set),member(T,Set).

human_t_set(NamesByHardUR):- muarc_tmp:cached_tests(human_t,NamesByHardUR),!.
human_t_set(NamesByHardUR):- % Name=t(_),
  findall(Name,(all_arc_test_name_unordered(Name),some_test_info(Name,Sol),member(human(_),Sol)),All),
  list_to_set(All,NamesByHardUR),
  asserta(muarc_tmp:cached_tests(human_t,NamesByHardUR)).


/*
sol_t(T):- sol_t_set(Set),member(T,Set).
%sol_t(T):- human_t_set(Set),member(T,Set).

sol_t_set(NamesByHardUR):- muarc_tmp:cached_tests(sol_t,NamesByHardUR),!.
sol_t_set(NamesByHardUR):- % Name=t(_),
  findall(Name,
   (some_test_info(Name,Sol),member(C,Sol),compound(C),functor(C,F,1),atom_contains(F,sol)),All),
  list_to_set(All,NamesByHardUR),
  asserta(muarc_tmp:cached_tests(sol_t,NamesByHardUR)).
*/




alphabetical_t:- clsmake, write_ansi_file(alphabetical_t).
alphabetical_v:- clsmake, write_ansi_file(alphabetical_v).

write_ansi_file(F):- call(F,Set),
  ensure_file_extension(F,'.vt100',FN),
  setup_call_cleanup(open(FN,write,O,[create([default]),encoding(iso_latin_1)]),
  forall(member(T,Set), 
    (wots(S,print_test(T)), write(O,S),write(S))),close(O)).


test_names_by_hard(Name):- 
 test_names_ord_hard(NamesByHard),
 test_names_ord_favs(FavList),
 my_append(NamesByHard,FavList,All),list_to_set(All,AllS),!,member(Name,AllS).

test_names_by_hard_rev(Name):- 
 test_names_ord_hard(NamesByHard),
 reverse(NamesByHard,NamesByHardR),
 test_names_ord_favs(FavList),
 my_append(NamesByHardR,FavList,All),list_to_set(All,AllS),!,member(Name,AllS).

test_names_by_fav(Name):- test_names_ord_favs(All),member(Name,All).
test_names_by_fav_rev(Name):- test_names_ord_favs(AllS),reverse(AllS,AllR),member(Name,AllR).


:- dynamic(ord_hard/1).
test_names_ord_hard(NamesByHard):- ord_hard(NamesByHard),!.
test_names_ord_hard(NamesByHard):- 
  pp(recreating(test_names_ord_hard)),
  findall(Hard-Name,(all_arc_test_name_unordered(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),  my_maplist(arg(2),AllK,NamesByHardU),!,
  list_to_set(NamesByHardU,NamesByHard), 
  asserta(ord_hard(NamesByHard)).

:- dynamic(ord_favs/1).
test_names_ord_favs(FavListS):- ord_favs(FavListS),!.
test_names_ord_favs(FavListS):- 
 my_time((
  pp(recreating(test_names_ord_favs)), 
  findall(Name,fav(Name),FavList),list_to_set(FavList,FavListS),
  pp(done_recreating(test_names_ord_favs)),  
  asserta(ord_favs(FavListS)))).


%:- use_module(library(pfc_lib)).
%:- retractall(ord_favs(_)),retractall(ord_hard(_)).

ascending_hard:-
  pp(recreating(ascending_hard)),
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(TestID),
    forall(kaggle_arc(TestID,ExampleNum,In,Out),format('~q.~n',[kaggle_arc_ord(TestID,ExampleNum,In,Out)]))),
  told,
  reconsult(arc_ascending).

:- style_check(-singleton).
negate_number(N,NN):- NN is - N.


pair_cost(TestID,Cost):- kaggle_arc(TestID,(trn+_),I,O),
 unique_colors(I,IC),grid_size(I,IH,IV),
 unique_colors(I,OC),grid_size(O,OH,OV),
 intersection(IO,OC,S,LO,RO),
 my_maplist(length,[S,LO,RO],[SN,LON,RON]),
 Cost is (IH+OH)*(IV+OV)*(LON+1)*(RON+1).

hardness_of_name(TestID,TMass):-!,
 kaggle_arc(TestID,(trn+1),I,O),
 mass(I,IMass),mass(O,OMass),TMass is -(IMass+OMass).

hardness_of_name(TestID,TMass+TArea+Sum+Dif):-
 kaggle_arc(TestID,(trn+0),I,O),
 area(I,IArea),area(O,OArea),TArea is -IArea*OArea,
 mass(I,IMass),mass(O,OMass),TMass is -(IMass+OMass),
 unique_colors(I,II),unique_colors(O,OO),length(II,IL), length(OO,OL), Sum is -(IL+OL),
 intersection(II,OO,Same,IU,OU),length(IU,IUL), length(OU,OUL), Dif is -(IUL+OUL),
 
 
 !.
 %findall(Cost,pair_cost(TestID,Cost),List),sumlist(List,Sum),!.
/*
hardness_of_name(TestID,Sum):-
  kaggle_arc(TestID,(trn+0),_,_),
 findall(Cost,pair_cost(TestID,Cost),List),sumlist(List,Sum),!.
*/
/*
hardness_of_name(TestID,Hard):-
 %ExampleNum=tst+_,
 ExampleNum=_,
 findall(_,kaggle_arc(TestID,(trn+_),_,_),Trns),
 length(Trns,TrnsL),
 %extra_tio_name(TestID,TIO),
  findall(PHard,
  (kaggle_arc(TestID,ExampleNum,In,Out),
   pair_dictation(TestID,ExampleNum,In,Out,T),
   my_maplist(negate_number,[T.in_specific_colors_len,T.out_specific_colors_len],[InOnlyC,OutOnlyC]),
   PHard = (TrnsL+ T.shared_colors_len + OutOnlyC + InOnlyC + T.ratio_area+ T.delta_density)),
    %(catch(Code,_,rrtrace(Code)))),
  All),
 sort_safe(All,AllK),last(AllK,Hard).
*/
:- style_check(-singleton).


arc_index_pairs([ncg,'o',I,'o',O]):- between(0,7,I),between(0,7,O),I<O.
arc_index_pairs([trn,'o',I,'o',O]):- between(0,7,I),between(0,7,O),I<O.
arc_index_pairs([ncg,'i',I,'i',O]):- between(0,7,I),between(0,7,O),I<O.
arc_index_pairs([trn,'i',I,'i',O]):- between(0,7,I),between(0,7,O),I<O.
arc_index_pairs([trn,'i',O,'o',O]):- between(0,7,O).
arc_index_pairs([tst,'i',O,'o',O]):- between(0,2,O).


arc_indexed_pairs(TestID,S,Prefix,G1,G2):- kaggle_arc_io_m(TestID,tst+0,_,_),  arc_index_pairs(Prefix),
  Prefix = [Type,IO1,D1,IO2,D2], 
  sformat(S,'~w__~w~w_~w~w__',[Type,IO1,D1,IO2,D2]),
  kaggle_arc_io_m(TestID,Type+D1,IO1,G1),
  kaggle_arc_io_m(TestID,Type+D2,IO2,G2).

kaggle_arc_io_m(TestID,Type+D2,IO2,G2):- IO2==i,!,kaggle_arc_io_m(TestID,Type+D2,in,G2).
kaggle_arc_io_m(TestID,Type+D2,IO2,G2):- IO2==o,!,kaggle_arc_io_m(TestID,Type+D2,out,G2).
kaggle_arc_io_m(TestID,ncg+D2,IO2,G2):- !,kaggle_arc_io(TestID,trn+D2,IO2,G1),into_monochrome(G1,G2).
kaggle_arc_io_m(TestID,Type+D2,IO2,G2):- kaggle_arc_io(TestID,Type+D2,IO2,G2).
  


extra_tio_name(TestID,TIO):-
  kaggle_arc(TestID,(trn+0),In0,Out0),
  kaggle_arc(TestID,(trn+1),In1,Out1),
  do_pair_dication(In0,In1,TI),
  do_pair_dication(Out0,Out1,TO),
  my_maplist(precat_name('o0_o1_'),TO,TOM),
  my_maplist(precat_name('i0_i1_'),TI,TIM),
  append(TIM,TOM,TIO),!.




make_comparison(DictIn,TestID,Prefix,In,Out,DictOut):-
  do_pair_dication(In,Out,Vs),!,
 % append(Vs,[shared=[], refused=[], patterns=[], added=[], removed=[]],Vs0),
  Vs=Vs0,
  atomic_list_concat(Prefix,PrefixA),
  my_maplist(precat_name(PrefixA),Vs0,VsT),
  vars_to_dictation(VsT,DictIn,DictOut).
  
  
make_training_hints(TestID,DictIn,DictOut):- test_hints_5(TestID,trn,0,DictIn,DictOut).
test_hints_5(TestID,Trn,N,DictIn,DictOut):-
  (kaggle_arc(TestID,(Trn+N),In,Out),
  make_comparison(DictIn,TestID,[Trn,'_i',N,'_o',N,'_'],In,Out,DictM),
  NN is N + 1),
 (kaggle_arc(TestID,(Trn+NN),In2,Out2) -> 
    (make_comparison(DictM,TestID,[Trn,'_i',N,'_i',NN,'_'],In,In2,Dict0),
     make_comparison(Dict0,TestID,[Trn,'_o',N,'_o',NN,'_'],Out,Out2,Dict1),
     test_hints_5(TestID,Trn,NN,Dict1,DictOut));
  (DictM = DictOut)),!.
  

print_test_hints(TestID):- ensure_test(TestID),
  hardness_of_name(TestID,Hard),!,
  write('/*'),
  pp(hard=Hard),
  %make_training_hints(TestID,print_test{},DictOut),
  %pp(all=DictOut),
  writeln('*/').


precat_name(P,N=V,NN=V):- atom_concat(P,N,NN).
  

:- dynamic(cached_dictation/2).
:- retractall(cached_dictation(_,_)).
pair_dictation(TestID,ExampleNum,In,Out,DictOut):- cached_dictation(pair_dictation(TestID,ExampleNum,In,Out),DictOut),!.
pair_dictation(TestID,ExampleNum,In,Out,DictOut):-
  do_pair_dication(In,Out,Vs),!,
  vars_to_dictation(Vs,_{},DictOut),
  retractall(cached_dictation(pair_dictation(TestID,ExampleNum,_,_),_)),
  arc_assert(cached_dictation(pair_dictation(TestID,ExampleNum,In,Out),DictOut)).


:- quasi_quotation_syntax(dictate_sourcecode).
:- export(dictate_sourcecode/4).
dictate_sourcecode(Content, _Vars, OutterVars, TP):-     
    phrase_from_quasi_quotation(muarc:copy_qq(Chars), Content),    
    atom_to_term(Chars,Sourcecode0,Vs0),
    parse_expansions([],Vs0, Vs, Sourcecode0, Sourcecode),!,
    my_maplist(share_vars(Vs),OutterVars),
    \+ \+ ((  my_maplist(ignore_numvars,Vs),
              numbervars(TP,0,_,[attvar(bind),singletons(true)]),
              print(program=Sourcecode),nl,
              my_maplist(print_prop_val,Vs))),
    !, TP = source_buffer(Sourcecode, Vs).


do_pair_dication(In,Out,_Vs):-   
 run_source_code(['In'=In, 'Out'=Out], _Vs,
{|dictate_sourcecode||
 [
  

  grid_size(In,InH,InV),
  grid_size(Out,OutH,OutV),
  InArea is InH * InV,
  OutArea is OutH * OutV,
  
  ratio_for(RatioArea,OutArea,InArea),
  max_min(OutArea,InArea,BothMaxArea,BothMinArea),

  mass(In,InMass),
  mass(Out,OutMass),
  ratio_for(DeltaMass,OutMass,InMass),

  unique_color_count(In,InColorLen),
  unique_color_count(Out,OutColorLen),
  ratio_for(RatioColorLen,OutColorLen,InColorLen),
  unique_colors(In,InColors),
  unique_colors(Out,OutColors),
  intersection(InColors,OutColors,SharedColors,InSpecificColors,OutSpecificColors),
  append([InSpecificColors,SharedColors,OutSpecificColors],AllUnsharedColors),
  dont_include(AllUnsharedColors),
  sort_safe(AllUnsharedColors,AllColors),
  my_maplist(length,[InColors,OutColors,SharedColors,InSpecificColors,OutSpecificColors,AllColors],
              [InColorsLen,OutColorsLen,SharedColorsLen,InSpecificColorsLen,OutSpecificColorsLen,AllColorsLen]),
  %my_maplist(negate_number,[InColorsLen,OutColorsLen,SharedColorsLen,InSpecificColorsLen,OutSpecificColorsLen,AllColorsLen],
  %            [InColorsLenNeg,OutColorsLenNeg,SharedColorsLenNeg,InSpecificColorsLenNeg,OutSpecificColorsLenNeg,AllColorsLenNeg]),

  ratio_for(RescaleH,OutH,InH), ratio_for(RescaleV,OutV,InV),
  ratio_for(InDensity,InMass,InArea),
  ratio_for(OutDensity,OutMass,OutArea),
  
  ratio_for(DeltaDensity,OutDensity,InDensity),

  max_min(OutH,OutV,OutMaxHV,_),
  max_min(InH,InV,InMaxHV,_),
  OutMaxHVArea is OutMaxHV*OutMaxHV, 
  InMaxHVArea is InMaxHV*InMaxHV,

  ratio_for(RatioMaxHVArea,OutMaxHVArea,InMaxHVArea),

  max_min(InMaxHVArea,OutMaxHVArea,BothMaxHVAreaMax,BothMaxHVAreaMin),
  ratio_for(RatioBothMaxHVArea,BothMaxHVAreaMax,BothMaxHVAreaMin)]|}).



sort_univ(=,A,B):- var(A),var(B), A==B,!.
sort_univ(=,A,B):- compound(A),compound(B), \+ A\=B,!.
sort_univ(R,A,B):- compare(R,A,B).

:- style_check(+singleton).


macro(one_obj, must_det_ll(len(objs)=1)).


test_p2(_):- update_and_fail,fail.
test_p2(P2):- (test_p2a(P2)*->flush_tee;(flush_tee,fail)).
test_p2a(P2):- 
  (get_pair_mode(single_pair);get_pair_mode(whole_test)),!,
  append_termlist(P2,[N1,'$VAR'('Result')],N2), 
  put_attr(G2,expect_p2,Expected),
  forall_count(test_pairs(_,G1,Expected),     
     forall((set_current_test(G1),call(P2,G1,G2)),
       ((grid_to_gid(G1,N1),
       once(ignore((grid_arg(G2,GR,Rest),print_side_by_side(red,G1,N1-Rest,_LW,GR,(?-(N2))),
         show_sf_if_lame(test_p2(P2),G2,Expected),dash_chars))))))),!.

test_p2a(P2):-
  append_termlist(P2,[N1,'$VAR'('Result')],N2), 
  forall_count(into_grids(N1,G1),     
     forall((set_current_test(G1),call(P2,G1,G2)),
       once(ignore((grid_arg(G2,GR,Rest),print_side_by_side(red,G1,N1-Rest,_LW,GR,(?-(N2))),dash_chars))))).

grid_arg(G2,G2,[]):- is_grid(G2),!.
grid_arg(GRest,GR,GRest):- compound(GRest),arg(N,GRest,GR), is_grid(GR),!,setarg(N,GRest,grid),!.
grid_arg(GRest,GR,expect_p2):- get_attr(GRest,expect_p2,GR).

%:- style_check(-singleton).
%whole(I,O):- is_group(I),length(I,1),I=O,!.
%whole(I,O):- print_grid(I),pp(I),into_obj(I,O).
one_obj(I,I):- is_group(I),length(I,1),!.
one_obj(I,I):- is_group(I),!.
one_obj(I,I).

is_fti_step(uncolorize).
uncolorize(VM):- decl_many_fg_colors(FG),
  set_all_fg_colors(FG,VM.grid,UCGRID),
  set_vm_grid(VM,UCGRID),
  set_all_fg_colors(FG,VM.objs,UCOBJS),
  set(VM.objs)=UCOBJS.
uncolorize(I,O):- decl_many_fg_colors(FG),set_all_fg_colors(FG,I,O).
%resize_grid(_H,_V,List,_,List):- is_list(List).
%resize_grid(H,V,Color,_,NewGrid):- make_grid(H,V,Grid),replace_grid_point(1,1,Color,_,Grid,NewGrid),nop(set_bgc(Color)).

resize_grid(H,V,Grid,NewGrid):- crop(H,V,Grid,NewGrid).
%resize_grid(H,V,_,NewGrid):- make_grid(H,V,NewGrid).

h_symmetric(Group,TF):- call_bool(h_symmetric(Group),TF).

call_bool(G,TF):- (call(G)*->TF=true;TF=false).
freeze_on([_NV],Goal):- !, call(Goal).
freeze_on([],Goal):- !, call(Goal).
freeze_on([NV|Vars],Goal):- nonvar(NV),!,freeze_on(Vars,Goal).
freeze_on([_NV|Vars],Goal):- my_maplist(nonvar,Vars),!,call(Goal).
freeze_on(Vars,Goal):- my_maplist(freeze_until(Goal,Vars),Vars).
freeze_until(Goal,Vars,Var):- freeze(Var,freeze_on(Vars,Goal)).

i(A,B,C):- individuate(A,B,C),!.

:- dynamic(is_db/2).
db_value(value:Color,_,Color).
db_value(largest:P1,[Obj|_],P1:TF):- nonvar(P1),!,freeze(Obj,call_bool(call(P1,Obj),TF)),!.
db_value(Color,_,Color).


is_eval(P1,Prev,P1A):- nop(is_eval(P1,Prev,P1A)),fail.
db_u(P1L,P1,P2L,P2,In,Out):- is_eval(P1,Prev,P1A),!,db_u([Prev|P1L],P1A,P2L,P2,In,Out).
db_u(P1L,P1,P2L,P2,In,Out):- is_eval(P2,Prev,P2A),!,db_u(P1L,P1,[Prev|P2L],P2A,In,Out).

%db(P1,P2,In,In):- t_or_t(freeze_for([P2],arc_assert(is_db(TF,P2))),is_db(TF,P2)).
%db(P2,P1,In,In):- nonvar(Color), db_value(P1,In,TF),!,t_or_t(freeze_for([Color],arc_assert(is_db(TF,Color))),is_db(TF,Color)).
db(P1,P2,In,Out):- db_u([],P1,[],P2,In,Out).
db(X,Y,I,I):- pp(db(X,Y)),pp(I).



copy_grid(In,G,G):- In == in,!.
copy_grid(Name,_,G):- get_training(VM),get_kov(Name,VM,G).

% ExampleNum is tst or trn
/*
kaggle_arc(t(Name), TypeI, In, Out):- 
 nth_fact(kaggle_arc_train(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_train(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum-*I)).
kaggle_arc(v(Name), TypeI, In, Out):- 
 member(ExampleNum, [trn, tst]), nth_fact(kaggle_arc_eval(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_eval(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum-*I)).
*/
fix_test_name(Try, TestID):- is_valid_testname(Try),!,TestID=Try.
fix_test_name(Try, TestID):- fix_id_1(Try,   TestID),!.
fix_test_name(Try, TestID):- testid_name_num_io(Try,TestID,_Example,_Num,_IO).

gfix_test_name(G,T,E):- is_grid(G),!, kaggle_arc_io(T,E,_,GO),GO=@=G.
fix_test_name(V,VV,_):- var(V),!,VV=V.
fix_test_name(ID,Fixed,Example+Num):- testid_name_num_io(ID,Tried,Example,Num,_), fix_test_name(Tried,Fixed).

testid_name_num_io(ID,TestID,Example,Num,IO):- 
  track_modes(testid_name_num_io(ID,TestID,Example,Num,IO),Modes),
  testid_name_num_io_0(ID,TestID,Example,Num,IO),
  ignore((fail, Modes=[+,-|_], nonvar(TestID), kaggle_arc(TestID,_,_,_), really_set_current_test(TestID))).


track_modes(I,M):- I=..[_|L],my_maplist(plus_minus_modes,L,M).
plus_minus_modes(Var,-):- var(Var),!. 
plus_minus_modes(_,+).

kaggle_arc_io_current_first(TestID,E,IO,G):- 
  get_current_test(TestID),kaggle_arc_io(TestID,E,IO,G).
kaggle_arc_io_current_first(TestID,E,IO,G):- 
  get_current_test(TestID2),kaggle_arc_io(TestID,E,IO,G), TestID\==TestID2.

testid_name_num_io_0(ID,_Name,_Example,_Num,_IO):- var(ID),!, fail.
testid_name_num_io_0(X,TestID,E,N,IO):- is_grid(X),!,kaggle_arc_io_current_first(TestID,E+N,IO,G),G=@=X.
testid_name_num_io_0(ID,_Name,_Example,_Num,_IO):- is_grid(ID),!, fail.
testid_name_num_io_0(ID,_Name,_Example,_Num,_IO):- is_list(ID), \+ my_maplist(nonvar,ID),!,fail.

testid_name_num_io_0([V,Name,Example,ANum|IOL],TestID,Example,Num,IO):- !, atom(V),VName=..[V,Name],
  atom_number(ANum,Num),!,
  fix_id_1(VName,TestID), freeze(IO,member(IO,[in,out])),member(IO,IOL),!.
testid_name_num_io_0(TestID>Example+Num*IO,Name,Example,Num,IO):- !,fix_id_1(TestID,Name).
testid_name_num_io_0(TestID>(Example+Num)*IO,Name,Example,Num,IO):- !,fix_id_1(TestID,Name).
testid_name_num_io_0(TestID>Example+Num,Name,Example,Num,_IO):- !,fix_id_1(TestID,Name).
testid_name_num_io_0(TestID>(Example+Num),Name,Example,Num,_IO):- !,fix_id_1(TestID,Name).
testid_name_num_io_0(ID,Name,Example,Num,IO):- ID = (TestID>((Example+Num)*IO)),!,fix_id_1(TestID,Name),!.
testid_name_num_io_0(ID,Name,Example,Num,IO):- ID = ((TestID>(Example+Num))*IO),!,fix_id_1(TestID,Name),!.
testid_name_num_io_0(ID,Name,Example,Num,IO):- ID = (TestID>(Example+Num)*IO),!,fix_id_1(TestID,Name),!.
testid_name_num_io_0(ID,Name,Example,Num,_IO):- ID = ((TestID>Example)+Num),!,fix_id_1(TestID,Name),!.
testid_name_num_io_0(ID,Name,Example,Num,_IO):- ID = (TestID>Example+Num),!,fix_id_1(TestID,Name),!.

testid_name_num_io_0(V,TestID,Example,Num,IO):- atom(V), atom_concat(VV,'.json',V),!,testid_name_num_io_0(VV,TestID,Example,Num,IO).
testid_name_num_io_0(ID,Name,Example,Num,IO):- atom(ID),atomic_list_concat(Term,'_',ID), Term\==[ID], 
  testid_name_num_io_0(Term,Name,Example,Num,IO),!.
testid_name_num_io_0(ID,Name,Example,Num,IO):- arc_atom_to_term(ID,Term,_), Term\==ID, nonvar(Term), arc_sensical_term(Term), 
  testid_name_num_io_0(Term,Name,Example,Num,IO),!.
%testid_name_num_io_0(ID,Name,_Example,_Num,_IO):- atom(ID),!,fix_id_1(ID,   Name),!.
testid_name_num_io_0(ID,Name,_Example,_Num,_IO):- fix_id_1(ID,   Name),!. %, kaggle_arc_io(Name,Example+Num,IO,_).

testid_name_num_io_gid(TestID,Example,Num,IO,GIDR):-
   TestID=..[V,ID],
   LIST=[V,ID,Example,Num,IO],
   maplist(must_be_nonvar,LIST),
   atomic_list_concat(LIST,'_',GIDR).




fix_id_1(Tried,   Tried):- var(Tried),!.
fix_id_1(X,_):- is_cpoint(X),!,fail.
fix_id_1(X,_):- is_cpoints_list(X),!,fail.
fix_id_1(X,TestID):- is_grid(X),kaggle_arc_io(TestID,_,_,G),G=@=X.
fix_id_1(obj_to_oid(_,X),Fixed):-  !, fix_test_name(X,Fixed).
fix_id_1(Tried,   Tried):- kaggle_arc(Tried,_,_,_),!.
fix_id_1(x(Tried),   TriedV):- !, atom_id(Tried,TriedV),!.
fix_id_1(v(Tried),   TriedV):- !, atom_id(Tried,TriedV),!.
fix_id_1(t(Tried),   TriedV):- !, atom_id(Tried,TriedV),!.
fix_id_1(Tried,   TriedV):- atom_id(Tried,TriedV),!.

%DD2401ED
atom_id(NonAtom,TriedV):- \+ atom(NonAtom),string(NonAtom),!,atom_string(Tried,NonAtom),atom_id(Tried,TriedV).
%0934a4d8 %07654321
atom_id(NonAtom,_TriedV):- \+ atom(NonAtom),!,number(NonAtom), fail. %, todo.
atom_id(Tried,TriedV):- atom_concat(Atom,'.json',Tried),!,atom_id(Atom,TriedV),!.
atom_id(Atom,TriedV):- atom_id_e(Atom,TriedV),!.
atom_id(Atom,TriedV):- downcase_atom(Atom,Tried),Atom\==Tried,atom_id(Tried,TriedV).
%fix_test_name(Tried,Fixed):- !, fail,compound(Tried),!,arg(_,Tried,E),nonvar_or_ci(E),fix_test_name(E,Fixed),!.
atom_id_e(Tried,t(Tried)):- kaggle_arc(t(Tried),_,_,_),!.
atom_id_e(Tried,v(Tried)):- kaggle_arc(v(Tried),_,_,_),!.
atom_id_e(Tried,x(Tried)):- kaggle_arc(x(Tried),_,_,_),!.
atom_id_e(Sel, TestID):- sformat(SSel,'~q',[Sel]),
   catch(read_term_from_atom(SSel,Name,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),_,
        (ppnl(['failed to read: ',Sel]),fail)),
        my_maplist(ignore,Vs),my_maplist(ignore,Singles),
        Name\==Sel,
       (fix_test_name(Name,TestID,_) -> true ; (ppnl(['could not read a test from: ',Sel,nl,'try again']),fail)).





print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).


parcCmt(TName):- 
  fix_test_name(TName,TestID),
  %color_print(magenta,call(((grid_hint(TestID))))),
  parcCmt1(TestID),flush_tee.
parcCmt1(TName):- 
  ignore((
  fix_test_name(TName,TestID),
  kaggle_arc(TestID,(trn+0),In,Out),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  IHV = IH*IV, OHV = OH*OV,
  BGColor = '$VAR'('Color'),
  (IHV\==OHV -> CG = resize_grid(OH,OV,BGColor); CG = copy_grid(in)),
  findall(III,test_info(TestID,III),InfoUF),
  flatten(InfoUF,InfoF),
  DSL = no_sol(i(complete),CG,incomplete),
  predsort(sort_univ,[DSL|InfoF],InfoFS), %44f52bb0
  reverse(InfoFS,InfoSR),
  P = fav(TestID,InfoSR),
  format('~q.~n',[P]))).


parc:- parc1((trn+1),6300*3).
parc(ExampleNum,OS):- clsmake,   luser_setval(task,[]),
 locally(set_prolog_flag(color_term,true),
   setup_call_cleanup(tell(user),
    ((set_prolog_flag(color_term,true),
      arc_set_stream(current_output, tty(true)),
      parc1(ExampleNum,OS))),
      told)).

parcf:- parcf((tst+_),_).
parcf(ExampleNum,OS):- make,   luser_setval(task,[]),
 locally(set_prolog_flag(color_term,false),
 setup_call_cleanup( open('test_cache.vt100',write,O,[encoding(text)]), 
   with_output_to(O,
    ((set_prolog_flag(color_term,true),
      arc_set_stream(O, tty(true)),
      parc1(ExampleNum,OS)))), close(O))).

parctt:- parctt((tst+_),6300*3).
parctt(ExampleNum,OS):- make,   luser_setval(task,[]),
  locally(set_prolog_flag(color_term,false),
   setup_call_cleanup( open('kaggle_arc_test_cache.new',write,O,[encoding(utf8)]), 
   with_output_to(O,
    ((arc_set_stream(O, tty(false)),
      parc1(ExampleNum,OS)))), close(O))).

parc1(ExampleNum,OS):-
 locally(set_prolog_flag(gc,true),forall(parc11(ExampleNum,OS,_),true)).

parc11(ExampleNum,OS,TName):-     
 fix_test_name(TName,TestID,ExampleNum),   
 forall(kaggle_arc(TestID,ExampleNum,In,Out),
  must_det_ll((
  mapgrid(color_sym(OS),In,I),
  mapgrid(color_sym(OS),Out,O),
  format('~Ntestcase(~q,"\n',[TestID>ExampleNum]),
    print_side_by_side(I,O),!, format('").\n'),
  ignore((write('%= '), parcCmt(TestID),nl,nl))))).

%color_sym(OS,[(black='�'),(blue='�'),(red='�'),(green=''),(yellow),(silver='O'),(purple),(orange='o'),(cyan= 248	� ),(brown)]).
color_sym(OS,C,C):- var(OS),!.
color_sym(OS,C,Sym):- is_list(C),my_maplist(color_sym(OS),C,Sym),!.
color_sym(_,Black,' '):- get_black(B),Black=B.
color_sym(OS,C,Sym):- color_sym(OS,4,C,Sym).
color_sym(_,_,C,Sym):- enum_colors(C),color_int(C,I),nth1(I,`ose=xt~+*zk>`,S),name(Sym,[S]).
%color_sym(P*T,_,C,Sym):- enum_colors(C),color_int(C,I),S is P+I*T,name(Sym,[S]).

with_current_test(P1):- current_predicate_human(P1/0),!,call(P1).
with_current_test(P1):- get_pair_mode(entire_suite),!,
  forall_count(all_arc_test_name(TestID), 
           catch_non_abort(with_current_test(P1,TestID))).
with_current_test(P1):- current_predicate(P1/1),!,call(P1,_).
with_current_test(P1):- doall(with_current_test(P1,_TestID)).

%with_current_test(P1,TestID_IN):- ensure_test(TestID_IN,TestID), with_current_test(P1,TestID).


with_current_test(P1,TestID):- get_pair_mode(single_pair),!, with_pair_mode(whole_test,with_current_test(P1,TestID)).  
with_current_test(P1,TestID):- get_pair_mode(entire_suite),!,var(TestID),
  all_arc_test_name(TestID), catch_non_abort(with_current_test(P1,TestID)).
with_current_test(P1,TestID):- current_predicate_human(P1/1),!,ensure_test(TestID),call(P1,TestID).
with_current_test(P1,TestID):- doall(with_current_test(P1,TestID,_)).

with_current_test(P1,TestID,ExampleNum):- \+ ground(ExampleNum),get_pair_mode(single_pair),!,
  with_pair_mode(whole_test,with_current_test(P1,TestID,ExampleNum)).  
with_current_test(P1,TestID,ExampleNum):-  Call = with_current_test(P1,TestID),
  \+ ground(Call), \+ \+ ((test_pairs(TestID,ExampleNum,_,_), ground(Call))),!,
  test_pairs(TestID,ExampleNum,_,_),
  call(Call). 
with_current_test(P1,TestID,ExampleNum):- var(TestID),!,ensure_test(TestID),with_current_test(P1,TestID,ExampleNum).
with_current_test(P1,TestID,ExampleNum):- var(ExampleNum),!,with_task_pairs(TestID,ExampleNum,_I,_O, with_current_test(P1,TestID,ExampleNum)).


with_current_test(P1,TestID,ExampleNum):- current_predicate_human(P1/2),!,call(P1,TestID,ExampleNum).
with_current_test(P1,TestID,ExampleNum):- with_current_test(P1,TestID,ExampleNum,_,_).


with_current_test(P1,TestID,ExampleNum,I,O):- \+ ground(ExampleNum),get_pair_mode(single_pair),!,
  with_pair_mode(whole_test,with_current_test(P1,TestID,ExampleNum,I,O)).  

with_current_test(P1,TestID,ExampleNum,I,O):- 
  Call = with_current_test(P1,TestID,ExampleNum,I,O),
  \+ ground(Call), test_pairs(TestID,ExampleNum,I,O), ground(Call),call(Call). 

with_current_test(P1,TestID,ExampleNum,I,O):- var(TestID),!,ensure_test(TestID),with_current_test(P1,TestID,ExampleNum,I,O).
%with_current_test(P1,TestID,ExampleNum,I,O):- var(ExampleNum),!,with_task_pairs(TestID,ExampleNum,_I,_O, with_current_test(P1,TestID,ExampleNum,I,O)).
with_current_test(P1,TestID,ExampleNum,I,O):- current_predicate_human(P1/4),!,with_task_pairs(TestID,ExampleNum,I,O,call(P1,TestID,ExampleNum,I,O)).
with_current_test(P1,TestID,ExampleNum,I,O):- current_predicate_human(P1/2),!,with_task_pairs(TestID,ExampleNum,I,O,call(P1,TestID,ExampleNum)).

with_current_test.

current_predicate_human(F/A):- current_predicate(F/A,P), 
 \+ (clause(P,Body),
     first_cmpd_goal(Body,G),
     functor(G,with_current_test,_)).



/*
with_current_test(P1,TestID,ExampleNum,I,O):- var(I),var(O),!,
  forall(kaggle_arc(TestID,ExampleNum,I,O),
    with_current_test(P1,TestID,ExampleNum,I,O)).

with_current_test(P1,TestID,ExampleNum,I,O):- 
    with_task_pairs(TestID,ExampleNum,I,O,
     with_current_test(P1,TestID,ExampleNum)).
*/

first_cmpd_goal(GG,_):- \+ compound(GG),!,fail.
first_cmpd_goal(forall(G,_),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(time(G),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(must_det_ll(G),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(my_time(G),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(':'(G,_),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(forall_count(G,_),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal((G,_),GG):- !, first_cmpd_goal(G,GG).
first_cmpd_goal(G,G).

clauses_predicate(M:F/N,P):-
                 current_predicate(M:F/N),functor(P,F,N),
                  \+ \+ predicate_property(M:P,number_of_clauses(_)), 
                   \+ predicate_property(M:P,imported_from(_)).

indicates_arg1_testid(var_ensure_test).
indicates_arg1_testid(ensure_test).
indicates_arg1_testid(testid_name_num_io).
indicates_arg1_testid(fix_test_name).
indicates_arg1_testid(is_valid_testname).
indicates_arg1_testid(with_test_grids).
indicates_arg1_testid(with_task_grids).
indicates_arg1_testid(with_trn_grids).
%indicates_arg1_testid(into_test_id_io1).
indicates_arg1_testid(test_grids).

skip_uses_test_id(into_test_id_io1).
skip_uses_test_id(get_prev_test).
skip_uses_test_id(do_menu_key).
skip_uses_test_id(set_example_num).
skip_uses_test_id(F):- indicates_arg1_testid(F).

uses_test_id(P):- clauses_predicate(M:F/N,P), 
                   once((
                   \+ skip_uses_test_id(F),
                   member(N,[0,1,4]),
                   \+ \+ ((clause(M:P,GG,_Ref),
                          first_cmpd_goal(GG,G),compound(G),functor(G,GF,_),
                          \+ \+ indicates_arg1_testid(GF),
                          if_t(compound(P),(arg(1,P,Var1),arg(1,G,Var2),Var1==Var2)),
                          ignore((fail,N>=0,listing((M:P))))

                   )),
                   ((N1 is N-1, functor(P1,F,N1),
                     nop((\+ predicate_property(M:P1,static))))))).

assert_missing_skel_mfa(M,F,A):- 
 ignore((
  length(Args,A),
   Head=..[F|Args],
   \+ predicate_property(M:Head,static),
   \+ predicate_property(M:Head,static),
   \+ clause(M:Head,_),
   Body=..[with_current_test,F|Args],
  asserta(M:(Head:- (with_current_test,Body))),
  writeln(synthesized(M:Head)))).


make_use_test_id(MP):- strip_module(MP,M,P),functor(P,F,_),make_use_test_id(M,F).
make_use_test_id(M,P):- compound(P),!,functor(P,F,_),make_use_test_id(M,F).
make_use_test_id(M,(F/A)):- !, assert_missing_skel_mfa(M,F,A).
make_use_test_id(M,F):-
 must_det_ll((
  assert_missing_skel_mfa(M,F,0),
  assert_missing_skel_mfa(M,F,1),   
  if_t(current_predicate_human(F/4),
   assert_missing_skel_mfa(M,F,2)))).



scan_uses_test_id:- scan_uses_test_id(uses_test_id).
scan_uses_test_id(When):- 
 dmsg(When),
 forall(uses_test_id(P),make_use_test_id(P)).



%:- all_source_file_predicates_are_exported.

%:- initialization(scan_uses_test_id(now),now).
:- initialization(scan_uses_test_id(default)).
:- initialization(scan_uses_test_id(restore),restore).


end_of_file.


psql -h chado.flybase.org -U flybase flybase -c "
SELECT f.uniquename as gene_name, f.organism_id as species_id, o.genus || ' ' || 
 o.species as organism, f.residues as sequence
FROM feature f
JOIN organism o ON f.organism_id = o.organism_id
WHERE f.type_id = (SELECT cvterm_id FROM cvterm WHERE 
cv_id = (SELECT cv_id FROM cv WHERE name = 'sequence')) AND 
f.uniquename ILIKE '%wingless%'
LIMIT 10;"




psql -h chado.flybase.org -U flybase flybase -c "
SELECT f.uniquename as gene_name, f.organism_id as species_id, o.genus || ' ' || o.species as organism, f.residues as sequence
FROM feature f
JOIN organism o ON f.organism_id = o.organism_id
WHERE 
f.type_id = (SELECT cvterm_id FROM cvterm WHERE name = 'gene' AND cv_id = (SELECT cv_id FROM cv WHERE name = 'sequence')) AND 
o.genus = 'Drosophila' AND o.species = 'melanogaster' AND f.uniquename ILIKE '%wingless%'
LIMIT 10;
"


psql -h chado.flybase.org -U flybase flybase -c "
SELECT f.uniquename AS gene_name, f.organism_id AS species_id, o.genus || ' ' || o.species AS organism, f.residues AS sequence
FROM feature f
JOIN organism o ON f.organism_id = o.organism_id
WHERE f.type_id = (SELECT cvterm_id FROM cvterm WHERE name = 'gene' AND cv_id = (SELECT cv_id FROM cv WHERE name = 'sequence'))
AND o.genus = 'Drosophila' AND o.species = 'melanogaster' AND f.uniquename ILIKE '%wingless%'
LIMIT 10;"

#!/bin/bash

# Replace these variables with your own values
your_host="your_host"
your_user="your_user"
your_database="your_database"
your_schema="your_schema"

# Connect to PostgreSQL server and retrieve table names within the specified schema
tables=$(psql -h $your_host -U $your_user -d $your_database -t -c "


SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_catalog = 
  'flybase' AND table_schema = 'public';


SELECT table_name FROM information_schema.tables WHERE table_catalog = 'flybase' AND table_schema = 'public';


# Create a directory to store the CSV files
mkdir -p csv_exports/$your_schema

# Iterate over each table
for table in $tables; do
    # Print the table name
    echo "Exporting table: $your_schema.$table"

    # Export the table data to a CSV file
    psql -h $your_host -U $your_user -d $your_database -c "
     COPY $your_schema.$table TO STDOUT WITH (FORMAT csv, HEADER true)" > "csv_exports/$your_schema/${table}.csv"
done


psql -h chado.flybase.org -U flybase flybase -c "
SELECT f.uniquename as gene_name, f.organism_id as species_id, o.genus || ' ' || o.species as organism, 
f.residues as sequence FROM feature f 
JOIN organism o ON f.organism_id = o.organism_id WHERE 
f.uniquename ILIKE '%wingless%'
LIMIT 10;
"

o.genus = 'Drosophila' AND o.species = 'melanogaster' AND 

#!/bin/bash

# Replace these variables with your own values
your_host="your_host"
your_user="your_user"
your_database="your_database"
your_schema="your_schema"

# Connect to PostgreSQL server and retrieve table names within the specified schema
tables=$(psql -h $your_host -U $your_user -d $your_database -t -c "SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_catalog = '$your_database' AND table_schema = '$your_schema';")

# Create a directory to store the CSV files
mkdir -p csv_exports/$your_schema

# Iterate over each table
for table in $tables; do
    # Print the table name
    echo "Exporting table: $your_schema.$table"

    # Export the table data to a CSV file
    psql -h $your_host -U $your_user -d $your_database -c "COPY $your_schema.$table TO STDOUT WITH (FORMAT csv, HEADER true)" > "csv_exports/$your_schema/${table}.csv"
done


/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.
:- use_module(library(pengines)).

test_menu :- with_webui(menu).
menu :- bfly_html_goal(with_pp(bfly,write_menu('i'))).

write_menu(Mode):-
  get_current_test(TestID),!,
  print_single_test(TestID),!,
  write_menu_opts(Mode).

write_menu_opts(Mode):-
  get_current_test(TestID),(luser_getval(example,Example);Example=_),!,
  format('~N\n    With selected test: ~q ~q ~n~n',[TestID,example(Example)]),
  menu_options(Mode).

menu_options(Mode):- 
  forall(menu_cmd1(Mode,Key,Info,Goal),print_menu_cmd(Key,Info,Goal)),
  forall(menu_cmd9(Mode,Key,Info,Goal),print_menu_cmd9(Key,Info,Goal)),
  !.
print_menu_cmd(Key):- ignore((menu_cmd1(_,Key,Info,Goal),print_menu_cmd(Key,Info,Goal))).
print_menu_cmd(_Key,Info,Goal):- format('~N '),print_menu_cmd1(Info,Goal).
print_menu_cmd9(_Key,Info,Goal):- format(' '),print_menu_cmd1(Info,Goal).

print_menu_cmd1(Goal):-  if_arc_webui(write_cmd_link(Goal)),!.
print_menu_cmd1(Info,Goal):- if_arc_webui(write_cmd_link(Info,Goal)),!.
print_menu_cmd1(Info,_Goal):- format('~w',[Info]).


:- multifile(menu_cmd1/4).
:- multifile(menu_cmd9/4).
menu_cmd1(_,'t','       You may fully (t)rain from examples considering all the test pairs (this is the default)',(cls_z,!,print_test,train_test)).
menu_cmd1(_,'T','                  or (T)rain from only understanding single pairs (not considering the test as a whole)',(cls_z,train_only_from_pairs)).
menu_cmd1(i,'i','             See the (i)ndividuation of the input/outputs',(cls_z,!,ndividuator1)).
menu_cmd1(_,'u','                  or (u)niqueness between objects in the input/outputs',(cls_z,!,what_unique)).
menu_cmd1(_,'g','                  or (g)ridcells between objects in the input/outputs',(cls_z,!,compile_and_save_test)).
menu_cmd1(_,'p','                  or (p)rint the test (textured grid)',(update_changed_files,print_test)).
menu_cmd1(_,'e','                  or (e)xamine the program leared by training',(cls_z,print_test,!,learned_test,solve_easy)).
menu_cmd1(_,'L','                  or (L)earned program',(learned_test)).
menu_cmd1(_,'s','              Try to (s)olve based on training',(cls_z,print_test,!,solve_test)).
menu_cmd1(_,'S','                  or (S)olve confirming it works on training pairs',(cls_z,print_test,!,solve_test_training_too)).
menu_cmd1(_,'h','                  or (h)uman proposed solution',(human_test)).
menu_cmd1(_,'r','               Maybe (r)un some of the above: (p)rint, (t)rain, (e)xamine and (s)olve !',(cls_z,fully_test)).
menu_cmd1(_,'a','                  or (a)dvance to the next test and (r)un it',(cls_z,!,run_next_test)).
menu_cmd1(_,'n','               Go to (n)ext test (skipping this one)',(next_test,print_qtest)).
menu_cmd1(_,'N','                  or (N)ext suite',(next_suite)).
menu_cmd1(_,'b','                  or (b)ack to previous test.',(previous_test,print_qtest)).
menu_cmd1(_,'f','                  or (f)orce a favorite test.',(enter_test)).
menu_cmd1(_,'~','                  or (PageUp) to begining of suite',(prev_suite)).
menu_cmd1(i,'R','             Menu to (R)un all tests noninteractively',(run_all_tests,menu)).
menu_cmd1(_,'l','                  or (l)ist special tests to run,',(show_tests)).
menu_cmd1(r,'i','             Re-enter(i)nteractve mode.',(interactive_test_menu)).

menu_cmd9(_,'m','recomple this progra(m),',(make,menu)).
menu_cmd9(_,'c','(c)lear the scrollback buffer,',(cls_z)).
menu_cmd9(_,'C','(C)all DSL,',(call_dsl)).
menu_cmd9(_,'Q','(Q)uit Menu,',true).
menu_cmd9(_,'X','e(X)it to shell,',halt(4)). 
menu_cmd9(_,'B','or (B)reak to interpreter.',(break)).

menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd1(Mode,Key,Mesg,Goal).
menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd9(Mode,Key,Mesg,Goal).

find_tests(F):-
   current_predicate(N),N=F/0, (atom_concat(test_,_,F); atom_concat(_,'_test',F)),
    \+ ( atom_codes(F,Codes),member(C,Codes),char_type(C,digit) ).

find_g_tests(F):- ping_indiv_grid(F).
find_g_tests(F):- is_fti_stepr(F).
find_g_tests(F):- is_fti_step(F).
find_g_tests(F):- find_tests(F).

list_of_tests(S):- findall(F,find_g_tests(F),L),sort(L,S).

show_tests:- make, list_of_tests(L),forall(nth10(N,L,E),format('~N~w: ~@  ',[N,print_menu_cmd1(E,E)])),nl.

  % ignore((read_line_to_string(user_input,Sel),atom_number(Sel,Num))),

ui_menu_call(G):- if_arc_webui(catch(ignore(G),E,wdmsg(E)))->true;catch(ignore(G),E,wdmsg(E)).
  
my_menu_call(E):- locally(set_prolog_flag(gc,true),ui_menu_call(E)).

my_submenu_call(G):- current_predicate(_,G), \+ is_list(G),!,
  locally(set_prolog_flag(gc,true),ui_menu_call(G)).
my_submenu_call0(E):- peek_vm(VM),!, ui_menu_call(run_dsl(VM,E,VM.grid,Out)),
  set(VM.grid) = Out.

key_read_borked(PP):- in_pp(PP), PP\==ansi,PP\==bfly.

read_menu_chars(_Start,_SelMax,Out):- key_read_borked(PP),!, wdmsg(read_menu_chars(PP)),once((\+ toplevel_pp(http),read(Out))).
read_menu_chars(_Start,_SelMax,Out):- pengine_self(_Id),!,read(Out).
read_menu_chars(Start,SelMax,Out):-
  get_single_key_code(Codes), atom_codes(Key,Codes),
  append_num_code(Start,SelMax,Key,Out).

get_single_key_code(CCodes):- key_read_borked(PP),!, wdmsg(get_single_key_code(PP)),sleep(5),once((\+ toplevel_pp(http),read(CCodes))).
get_single_key_code(CCodes):- get_single_char(C), 
  (  C== -1 -> (CCodes=`Q`) ; (read_pending_codes(user_input,Codes, []), [C|Codes]=CCodes)).

/*
get_single_key_code(Code):- get_single_char(C), into_key_codes([C],Code).


into_key_codes([27],Codes):- get_single_char(C1),into_key_codes([27,C1],Codes).
into_key_codes([27,27],[27|Codes]):- get_single_char(C1),into_key_codes([27,C1],Codes).

into_key_codes([27,91],Codes):- get_single_char(C1),into_key_codes([27,91,C1],Codes).
into_key_codes([27,79],Codes):- get_single_char(C1),into_key_codes([27,79,C1],Codes).
into_key_codes([27,N1,54],Codes):- get_single_char(C1),into_key_codes([27,N1,54,C1],Codes).
into_key_codes([27,N1,53],Codes):- get_single_char(C1),into_key_codes([27,N1,53,C1],Codes).
into_key_codes(C,C).
*/

append_num_code(Start,_SelMax,Key,Start):- atom_codes(Key,[C|_]), char_type(C,end_of_line),!.
append_num_code(Start,SelMax,Digit,Out):- atom_codes(Digit,[C|_]),char_type(C,digit),write(Digit),atom_concat(Start,Digit,NStart), 
 ((atom_number(NStart,Num),Num>9) -> Out = NStart ; read_menu_chars(NStart,SelMax,Out)).
append_num_code(Start,_SelMax,Key,Sel):- atom_concat(Start,Key,Sel).


clsR:- !. % once(cls_z).


enter_test:- repeat, write("\nYour favorite: "), read_line_to_string(user_input,Sel),enter_test(Sel),!.

enter_test(""):- wqnl("resuming menu"), menu,!.
enter_test(Sel):- atom_string(Name,Sel), fix_test_name(Name,TestID,_),switch_test(TestID),!.
enter_test(Sel):- 
   catch(read_term_from_atom(Sel,Name,[module(user),double_quotes(string),variable_names(Vs),singletons(Singles)]),_,
        (wqnl(['failed to read: ',Sel]),fail)),
        maplist(ignore,Vs),maplist(ignore,Singles),
       (fix_test_name(Name,TestID,_) -> true ; (wqnl(['could not read a test from: ',Sel,nl,'try again']),fail)),
       switch_test(TestID).
       
switch_test(TestID):- wqnl(['Swithing to test: ',TestID]),set_current_test(TestID),print_test.



:- dynamic(wants_exit_menu/0).
interact:- list_of_tests(L), length(L,SelMax),!,interact(SelMax).
/*interact:- list_of_tests(L), length(L,SelMax),!,
  repeat, 
    i_key(SelMax,Key),
    writeq(Key),
   once((do_menu_key(Key))), 
   retract(wants_exit_menu),!.
*/

interact(SelMax):- i_key(SelMax,Key),
    writeq(Key),
    ignore((do_menu_key(Key))),interact(SelMax).

i_key(SelMax,Key):-
  format('~N Your menu(?) selection: '), 
  %get_single_char(Code), wdmsg(code=Code), char_code(Key,Code),  put_char(Key), 
   with_tty_raw(once(read_menu_chars('',SelMax,Key))),!.


do_menu_key('Q'):-!,format('~N returning to prolog.. to restart type ?- demo. '), arc_assert(wants_exit_menu).
do_menu_key('?'):- !, write_menu_opts('i').
do_menu_key('P'):- !, switch_grid_mode,print_test.
do_menu_key('I'):- !, cls_z,!,ndividuator.
do_menu_key('o'):- !, cls_z,!,ndividuatorO1.
do_menu_key('O'):- !, cls_z,!,ndividuatorO.
do_menu_key('G'):- !, cls_z,!,detect_test_hints1.
do_menu_key(-1):- !, arc_assert(wants_exit_menu).
do_menu_key(Key):- atom_codes(Key,Codes),  do_menu_codes(Codes), !.
do_menu_key(Key):- atom_string(Name,Key), fix_id(Name,TestID),set_current_test(TestID),!,print_test.
do_menu_key(Sel):- atom_number(Sel,Num), number(Num), do_test_number(Num),!.
do_menu_key(Key):- print_menu_cmd(Key),menu_cmds(_Mode,Key,_Info,Goal),!, format('~N~n'),
  dmsg(calling(Goal)),!, ignore(once((catch(my_menu_call(Goal),'$aborted',fail)*->true;(fail,trace,arcST,rrtrace(Goal))))),!,
   read_pending_codes(user_input,_Ignored,[]),!.
do_menu_key(Key):- maybe_call_code(Key),!.
do_menu_key(Key):- atom(Key),atom_length(Key,1),atom_codes(Key,Codes), format("~N % Menu: didn't understand: '~w' ~q ~n",[Key,Codes]),once(mmake).
do_menu_key(_).

maybe_call_code(Key):- atom_length(Key,Len),Len>2,
 catch(atom_to_term(Key,Term,Vs),_,fail),!, 
 locally(nb_setval('$variable_names',Vs),
   locally(nb_setval('$term',Term),
     locally(nb_setval('$user_term',Term), 
       my_submenu_call(Term)))).

call_dsl:- repeat, write("\nYour DSL Goal: "), read_line_to_string(user_input,Sel),ignore(do_menu_key(Sel)),!.



% nth that starts counting at three
nth10(X,Y,Z):- var(X),!,nth0(N,Y,Z), X is N + 10 .
nth10(X,Y,Z):- N is X -10, nth0(N,Y,Z).

do_test_number(Num):- list_of_tests(L), nth10(Num,L,E),!, cls_z, get_current_grid(G), set_flag(indiv,0), my_submenu_call(ig(E,G)),!.

get_current_grid(G):- get_current_test(T),kaggle_arc_io(T,_,_,G).

% crl left arrow
do_menu_codes([27,79,68]):- !, previous_test, print_test.
% ctrl right arrow
do_menu_codes([27,79,67]):- !, next_test, print_test.
% alt left arrow
do_menu_codes([27,27,91,68]):- !, previous_test, print_test.
% alt right arrow
do_menu_codes([27,27,91,67]):- !, next_test, print_test.
% left arrow
do_menu_codes([27,91,68]):- !, cls_z, previous_test, print_test.
% right arrow
do_menu_codes([27,91,67]):- !, cls_z,  next_test, print_test.
% page up
do_menu_codes([27,91,53,126]):- !, prev_suite.
% page down
do_menu_codes([27,91,54,126]):- !, next_suite.
% up arrow
do_menu_codes([27,91,65]):- !, prev_pair.
% down arrow
do_menu_codes([27,91,66]):- !, next_pair.

interactive_test(X):- set_current_test(X), print_test(X), interactive_test_menu.
interactive_test_menu:- 
 my_menu_call((
  repeat, write_menu('i'), 
   catch((interact),'$aborted',fail))),!.
run_all_tests:- 
  repeat,
   run_next_test,
   write_menu('r'),
   wait_for_input([user_input],F,2),
   F \== [], !,
   interact,!.

rtty:- with_tty_raw(rtty1).
rtty1:- repeat,get_single_char(C),dmsg(c=C),fail.


ip(I,O):- ip(complete,I,O).

ndividuator1:- get_current_test(TestID),set_flag(indiv,0),with_test_pairs1(TestID,In,Out,ip(In,Out)).
ndividuator:- get_current_test(TestID),set_flag(indiv,0),with_test_pairs(TestID,In,Out,ip(In,Out)).
ndividuatorO1:- get_current_test(TestID),set_flag(indiv,0),with_test_grids1(TestID,In,igo(In)).
ndividuatorO:- get_current_test(TestID),set_flag(indiv,0),with_test_grids(TestID,In,igo(In)).

test_grids(TestID,G):- ignore(get_current_test(TestID)), kaggle_arc_io(TestID,ExampleNum,IO,G), ((ExampleNum*IO) \= ((tst+_)*out)).
with_test_grids(TestID,G,P):- forall(test_grids(TestID,G),my_menu_call(P)).
with_test_grids1(TestID,G,P):- ignore(luser_getval(example,ExampleNum)),
  forall((kaggle_arc_io(TestID,ExampleNum,IO,G),((ExampleNum*IO) \= ((tst+_)*out))),
  my_menu_call(P)).

test_pairs(TestID,In,Out):- ignore(get_current_test(TestID)), kaggle_arc(TestID,_,In,Out).

with_test_pairs(TestID,In,Out,P):- forall(test_pairs(TestID,In,Out),my_menu_call(P)).
with_test_pairs1(TestID,In,Out,P):- 
  ignore(luser_getval(example,ExampleNum)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out), my_menu_call(P)).

bad:- ig([complete],v(aa4ec2a5)>(trn+0)*in).


restart_suite:- 
   get_current_suite_testnames([First|_]),
   set_current_test(First),!.

prev_suite:- once((get_current_test(TestID),get_current_suite_testnames([First|_]))),TestID\==First,!,restart_suite.
prev_suite:- 
   findall(SN,test_suite_name(SN),List),
   luser_getval(test_suite_name,X),
   prev_in_list(X,List,N),
   luser_setval(test_suite_name,N),!,
   wdmsg(switched(X-->N)),
   restart_suite.
next_suite:- 
   findall(SN,test_suite_name(SN),List),
   luser_getval(test_suite_name,X),
   next_in_list(X,List,N),
   luser_setval(test_suite_name,N),!,
   wdmsg(switched(X-->N)),
   restart_suite.

%test_suite_name(arc_easy_test).
test_suite_name(human_t).
test_suite_name(sol_t).
test_suite_name(hard_t). test_suite_name(test_names_by_fav). 
test_suite_name(key_pad_tests). test_suite_name(alphabetical_v). test_suite_name(alphabetical_t).
test_suite_name(test_names_by_hard). 
test_suite_name(test_names_by_fav_rev). test_suite_name(test_names_by_hard_rev).
test_suite_name(all_arc_test_name).

:- dynamic(muarc_tmp:cached_tests/2).
%:- retractall(muarc_tmp:cached_tests(_,_)).
:- test_suite_name(Name)->luser_defval(test_suite_name,Name).
get_current_suite_testnames(Set):-
  luser_getval(test_suite_name,X),
  current_suite_testnames(X,Set).

current_suite_testnames(X,Set):- muarc_tmp:cached_tests(X,Set),!.  
current_suite_testnames(X,Set):-  pp(recreating(X)),
  findall(ID,call(X,ID),List), my_list_to_set_variant(List,Set),!,asserta(muarc_tmp:cached_tests(X,Set)).

previous_test:-  get_current_test(TestID), get_previous_test(TestID,NextID), set_current_test(NextID).
next_test:- get_current_test(TestID), notrace((get_next_test(TestID,NextID), set_current_test(NextID))),!.
is_valid_testname(TestID):- kaggle_arc(TestID,_,_,_).

get_current_test(TestID):- luser_getval(task,TestID),is_valid_testname(TestID),!.
get_current_test(TestID):- get_next_test(TestID,_),!.
get_current_test(v(fe9372f3)).

get_next_test(TestID,NextID):- get_current_suite_testnames(List), next_in_list(TestID,List,NextID).
get_previous_test(TestID,PrevID):-  get_current_suite_testnames(List), prev_in_list(TestID,List,PrevID).
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
muarc:arc_settings_filename1('current_test').
muarc:arc_settings_filename1('~/.arc_current_test').
muarc:arc_settings_filename1('/tmp/.arc_current_test').


set_current_test(Name):-  
  ignore((fix_id(Name,TestID),is_valid_testname(TestID),really_set_current_test(TestID))).

really_set_current_test(TestID):-
   luser_setval(task,TestID),
  (luser_getval(last_test_name,WasTestID);WasTestID=[]),
  (WasTestID==TestID-> true ; new_current_test_info(WasTestID,TestID)).

some_current_example_num(TrnN):- nb_current(example,TrnN),ground(TrnN),TrnN\==[],!.
some_current_example_num(TrnN):- luser_getval(example,TrnN),ground(TrnN),TrnN\==[],!.
some_current_example_num(trn+0).

next_pair:- 
  get_current_test(TestID),
  some_current_example_num(Trn+N),
  N2 is N+1,
  trn_tst(Trn,Tst),
  (kaggle_arc(TestID,Trn+N2,_,_)-> ExampleNum=Trn+N2 ; ExampleNum=Tst+0),
  nb_setval(example,ExampleNum),
  print_single_test(TestID>ExampleNum),!.

prev_pair:- 
  get_current_test(TestID),
  some_current_example_num(Trn+N),
  N2 is N-1,
  trn_tst(Trn,Tst),
  (kaggle_arc(TestID,Trn+N2,_,_)-> ExampleNum=Trn+N2 ; ExampleNum=Tst+0),
  nb_setval(example,ExampleNum),
  print_single_test(TestID>ExampleNum),!.


trn_tst(trn,tst).
trn_tst(tst,trn).

clear_test(TestID):- is_list(TestID),!,maplist(clear_test,TestID).
clear_test(TestID):- clear_training(TestID),
 saveable_test_info(TestID,Info),
 maplist(erase,Info).

new_current_test_info(WasTestID,TestID):- 
  clear_test(WasTestID),
  ignore((
  %luser_getval(task,TestID),
  get_current_test(TestID),
  dmsg(fav(TestID,[])),
  %luser_setval(example,tst+0),
  luser_setval(last_test_name,TestID))),
  save_last_test_name,
  load_test_id_dyn(TestID).
  %clear_training(TestID).

load_test_id_dyn(TestID):- 
  test_name_output_file(TestID,File),
  load_file_dyn(File).


unload_file_dyn(File):- \+ exists_file(File), !.
unload_file_dyn(File):- unload_file(File),!.
unload_file_dyn_pfc(File):- 
 open(File,read,I),
 repeat,read_term(I,Term,[]),
 (Term = end_of_file -> true ; pfcRemove(Term),fail).

load_file_dyn(File):- \+ exists_file(File), !.
load_file_dyn(File):- consult(File),!.
load_file_dyn(File):- load_file_dyn_pfc(File).
load_file_dyn_pfc(File):- 
 open(File,read,I),
 repeat,read_term(I,Term,[]),
 (Term = end_of_file -> true ; pfcAdd(Term),fail).

new_test_pair(PairName):-
  %nb_delete(grid_bgc),
  clear_shape_lib(pair),clear_shape_lib(in),clear_shape_lib(out),
  luser_setval(test_pairname,PairName),
  luser_linkval(pair_rules, [rules]),
  retractall(is_shared_saved(PairName*_,_)),
  retractall(is_shared_saved(PairName,_)),
  retractall(is_unshared_saved(PairName*_,_)),
  retractall(is_unshared_saved(PairName,_)),
  retractall(is_grid_tid(PairName*_,_)),
  retractall(is_grid_tid(PairName,_)),!.

human_test:- solve_test_trial(human).
fully_test:- print_test, !, train_test, !, solve_test, !.
run_next_test:- notrace(next_test), fully_test.

info(Info):- nonvar(Info),wdmsg(Info).
system:demo:- update_changed_files,!, interactive_test_menu.
:- export(demo/0).
rat:- info("Run all tests"), run_all_tests.
noninteractive_test(X):- time(ignore(forall(arc1(true,X),true))).


cmt_border:- format('~N% '), dash_chars(120,"="), !, nl.

test_id_border(TestID):-
    get_current_test(WasTestID),
    ignore((WasTestID\==TestID,set_current_test(TestID), cmt_border)).


print_test:- notrace((get_current_test(TestID),print_test(TestID))).
print_test(TName):- 
  arc_user(USER),
  fix_test_name(TName,TestID,ExampleNum1),
  luser_setval(example,ExampleNum1),
   cmt_border,format('%~w % ?- ~q. ~n',[USER,print_test(TName)]),cmt_border,
   ignore(print_test_hints(TestID)),
   format('~N% '),dash_chars,
    forall(arg(_,v((trn+_),(tst+_)),ExampleNum1),
     forall(kaggle_arc(TestID,ExampleNum1,In,Out),
      ignore((
       once(in_out_name(ExampleNum1,NameIn,NameOut)),
         as_d_grid(In,In1),as_d_grid(Out,Out1),
       format('~Ngridcase(~q,"\n~@").~n~n~n',[TestID>ExampleNum1,
            ((print_side_by_side(cyan,In1,NameIn,_,Out1,NameOut),
                           nl,
                           ignore(show_reduced_io(In1+Out1))))
           ]),
       nop((grid_hint_swap(i-o,In1,Out1))))))),format('~N'),
       write('%= '), parcCmt(TestID),!.

next_grid_mode(dots,dashes):-!.
next_grid_mode(_,dots).
switch_grid_mode:- (luser_getval('$grid_mode',Dots);Dots=dots),next_grid_mode(Dots,Dashes),luser_setval('$grid_mode',Dashes).
as_d_grid(In,In):- \+ luser_getval('$grid_mode',dashes),!.
as_d_grid(In,In1):- as_ngrid(In,In1).
as_ngrid(In,In1):- must_det_ll((change_bg_fg(In, _BG, _FG,In0), most_d_colors(In0,_CI,In1))),!.

%change_bg_fg(In,_BG,_FG,In):-!.
change_bg_fg(In,BG,FG,Mid):- 
   black=BG,
   must_det_ll((available_fg_colors(Avails),
   unique_colors(In,Colors),subtract(Avails,Colors,CanUse),
   ((fail,last(CanUse,FG))->true;FG=wbg),subst001(In,BG,FG,Mid))),!.

available_fg_colors(Avails):- findall(Color,enum_fg_colors(Color),Avails).

%print_test(TName):- !, parcCmt(TName).
print_qtest:- get_current_test(TestID),print_qtest(TestID).

print_single_test:- get_current_test(TestID),print_single_test(TestID).

print_qtest(TestID):- \+ luser_getval('$grid_mode',dots),!,print_test(TestID).
print_qtest(TestID):- !, print_single_test(TestID),!.
print_qtest(TestID):-
    dash_chars,nl,nl,nl,dash_chars,
     ignore(luser_getval(example,ExampleNum)),
     forall(kaggle_arc(TestID,ExampleNum,In,Out),
      ignore((
       as_d_grid(In,In1),as_d_grid(Out,Out1),
       once(in_out_name(ExampleNum,NameIn,NameOut)),
       format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID>ExampleNum,print_side_by_side(cyan,In1,NameIn,_LW,Out1,NameOut+TestID)])))),
       write('%= '), parcCmt(TestID).

print_single_test(TName):-
  fix_test_name(TName,TestID,ExampleNum),
  ignore(luser_getval(example,ExampleNum)),
  kaggle_arc(TestID,ExampleNum,In,Out),
  once(in_out_name(ExampleNum,NameIn,NameOut)),
  as_d_grid(In,In1),as_d_grid(Out,Out1),
  print_side_by_side(green,In1,NameIn,_LW,Out1,NameOut),!,
  parcCmt(TestID).

in_out_name(trn+NN,SI,SO):- N is NN+1, format(atom(SI),'Training Pair #~w Input',[N]),format(atom(SO),'Output',[]).
in_out_name(tst+NN,SI,SO):- N is NN+1, format(atom(SI),'EVALUATION TEST #~w',[N]),format(atom(SO),'Output<(REVEALED)>',[]).
in_out_name(X,'Input'(X),'Output'(X)).


all_arc_test_name(TestID):- kaggle_arc(TestID,trn+0,_,_).

all_suite_test_name(TestID):- get_current_suite_testnames(Set),!,member(TestID,Set).

arc_pair_id(TestID,ExampleNum):- 
  arc_test_name(TestID),
  ignore((luser_getval(example,Example+NumE), Example\==tst , ExampleNum=Example+NumE)),
  kaggle_arc_io(TestID,ExampleNum,in,_).

arc_grid_pair(In,Out):- 
 ((var(In),var(Out))-> arc_pair_id(TestID,ExampleNum); true),
  kaggle_arc(TestID,ExampleNum,In,Out).

arc_grid(Grid):- arc_grid(in,Grid).
arc_grid(IO,Grid):-
  arc_pair_id(TestID,ExampleNum),
  kaggle_arc_io(TestID,ExampleNum,IO,Grid).

arc_test_name(TestID):- get_current_test(TestID).
%arc_test_name(TestID):- get_current_test(WasTestID), (TestID=WasTestID;(get_current_suite_testnames(List),member(TestID,List),WasTestID\== TestID, set_current_test(TestID))).

some_test_info(TestID,III):- more_test_info(TestID,III).
some_test_info(X,[keypad]):- key_pad_tests(X). 
some_test_info(TestID,III):- fav(TestID,III).

matches(InfoS,InfoM):- member(InfoS,InfoM).


:- dynamic(muarc_tmp:test_info_cache/2).
:- retractall(muarc_tmp:test_info_cache(_,_)).
test_info(TestID,InfoS):- var(TestID),var(InfoS),!, pp(recreating(test_info)),all_arc_test_name(TestID),test_info(TestID,InfoS).
test_info(TestID,InfoS):- var(TestID),nonvar(InfoS),!,term_variables(InfoS,Vs),no_repeats(Vs,(test_info(TestID,InfoM),matches(InfoS,InfoM))).
%test_info(TestID,InfoS):- \+ \+ muarc_tmp:test_info_cache(TestID,_),!,muarc_tmp:test_info_cache(TestID,InfoS).
test_info(TestID,InfoS):- nonvar(TestID),once(fix_test_name(TestID,FTestID,_)),TestID\=@=FTestID,!,test_info(FTestID,InfoS).
test_info(TestID,InfoS):- muarc_tmp:test_info_cache(TestID,InfoS),!.
test_info(TestID,InfoS):- 
 findall(Inf,
  (some_test_info(CTestID,Inf0),once((fix_test_name(CTestID,CFTestID,_),CFTestID=TestID)),
   repair_info(Inf0,Inf)),Info),
  flatten([Info],InfoFF),repair_info(InfoFF,InfoF),list_to_set(InfoF,InfoS),!,
  asserta(muarc_tmp:test_info_cache(TestID,InfoS)),!.

repair_info(Inf,InfO):- listify(Inf,Inf1),maplist(repair_info0,Inf1,InfO).
repair_info0(Inf0,Inf):- is_list(Inf0),!,maplist(repair_info0,Inf0,Inf).
repair_info0(Inf,InfO):- compound(Inf),functor(Inf,F,1),!,arg(1,Inf,A),listify(A,ArgsL),InfO=..[F,ArgsL].
repair_info0(Inf,InfO):- compound(Inf),!,compound_name_arguments(Inf,F,ArgsL),InfO=..[F,ArgsL].
repair_info0(Inf,Inf).% listify(Inf,InfM),maplist(repair_info,InfM,Info).

was_fav(X):- nonvar_or_ci(X), clause(fav(XX,_),true),nonvar_or_ci(XX),X==XX.

test_names_by_hard(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),
 my_append(NamesByHard,FavList,All),list_to_set(All,AllS),!,member(Name,AllS).

test_names_by_hard_rev(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),
 reverse(NamesByHard,NamesByHardR),
 my_append(NamesByHardR,FavList,All),list_to_set(All,AllS),!,member(Name,AllS).

test_names_by_fav(Name):- test_names_ord_favs(All),member(Name,All).
test_names_by_fav_rev(Name):- test_names_ord_favs(AllS),reverse(AllS,AllR),member(Name,AllR).

:- dynamic(ord_favs/1).
test_names_ord_favs(FavListS):- ord_favs(FavListS),!.
test_names_ord_favs(FavListS):- 
  pp(recreating(test_names_ord_favs)), 
  findall(Name,fav(Name),FavList),list_to_set(FavList,FavListS),
  pp(done_recreating(ascending_hard)),  
  asserta(ord_favs(FavListS)).

alphabetical_v(Set):- findall(v(Name),all_arc_test_name(v(Name)),List),sort(List,Set).
alphabetical_t(Set):- findall(t(Name),all_arc_test_name(t(Name)),List),sort(List,Set).


human_t(T):- human_t_set(Set),member(T,Set).

%human_t_set(NamesByHardUR):- muarc_tmp:cached_tests(human_t_set,NamesByHardUR),!.
human_t_set(NamesByHardUR):- % Name=t(_),
  findall(Name,(test_info(Name,Sol),member(human(_),Sol)),All),
  list_to_set(All,NamesByHardUR).

sol_t(T):- sol_t_set(Set),member(T,Set).
sol_t(T):- human_t_set(Set),member(T,Set).
sol_t_set(NamesByHardUR):- % Name=t(_),
  findall(Name,
   (test_info(Name,Sol),member(C,Sol),compound(C),functor(C,F,1),atom_contains(F,sol)),All),
  list_to_set(All,NamesByHardUR).



hard_t(T):- hard_t_set(Set),member(T,Set).

hard_t_set(NamesByHardUR):- Name=t(_),
  findall(Name,all_arc_test_name(Name),List),sort(List,Sorted),
  findall(Hard-Name,(member(Name,Sorted),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),  maplist(arg(2),AllK,NamesByHardU),!,
  reverse(NamesByHardU,NamesByHardUR).

hard_t:- cls_z, hard_t(NamesByHardUR),
  forall(member(Name,NamesByHardUR),print_test(Name)).


alphabetical_t:- clsmake, write_ansi_file(alphabetical_t).
alphabetical_v:- clsmake, write_ansi_file(alphabetical_v).

write_ansi_file(F):- call(F,Set),
  atom_concat(F,'.vt100',FN),
  setup_call_cleanup(open(FN,write,O,[create([default]),encoding(utf8)]),
  forall(member(T,Set), 
    (wots(S,print_test(T)), write(O,S),write(S))),close(O)).


:- dynamic(ord_hard/1).
test_names_ord_hard(NamesByHard):- ord_hard(NamesByHard),!.
test_names_ord_hard(NamesByHard):- 
  pp(recreating(test_names_ord_hard)),
  findall(Hard-Name,(all_arc_test_name(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),  maplist(arg(2),AllK,NamesByHardU),!,
  list_to_set(NamesByHardU,NamesByHard), 
  asserta(ord_hard(NamesByHard)).

%:- use_module(library(pfc_lib)).
:- retractall(ord_favs(_)),retractall(ord_hard(_)).

ascending_hard:-
  pp(recreating(ascending_hard)),
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(TestID),
    forall(kaggle_arc(TestID,ExampleNum,In,Out),format('~q.~n',[kaggle_arc_ord(TestID,ExampleNum,In,Out)]))),
  told,
  reconsult(arc_ascending).

:- style_check(-singleton).
negate_number(N,NN):- NN is - N.
hardness_of_name(TestID,Hard):-
 %ExampleNum=tst+_,
 ExampleNum=_,
 findall(_,kaggle_arc(TestID,(trn+_),_,_),Trns),
 length(Trns,TrnsL),
 %extra_tio_name(TestID,TIO),
  findall(PHard,
  (kaggle_arc(TestID,ExampleNum,In,Out),
   pair_dictation(TestID,ExampleNum,In,Out,T),
   maplist(negate_number,[T.in_specific_colors_len,T.out_specific_colors_len],[InOnlyC,OutOnlyC]),
   PHard = (TrnsL+ T.shared_colors_len + OutOnlyC + InOnlyC + T.ratio_area+ T.delta_density)),
    %(catch(Code,_,rrtrace(Code)))),
  All),
 sort(All,AllK),last(AllK,Hard).

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
  maplist(precat_name('o0_o1_'),TO,TOM),
  maplist(precat_name('i0_i1_'),TI,TIM),
  append(TIM,TOM,TIO),!.




make_comparison(DictIn,TestID,Prefix,In,Out,DictOut):-
  do_pair_dication(In,Out,Vs),!,
 % append(Vs,[shared=[], refused=[], patterns=[], added=[], removed=[]],Vs0),
  Vs=Vs0,
  atomic_list_concat(Prefix,PrefixA),
  maplist(precat_name(PrefixA),Vs0,VsT),
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
  

print_test_hints(TestID):- 
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
    maplist(share_vars(Vs),OutterVars),
    \+ \+ ((  maplist(ignore_numvars,Vs),
              numbervars(TP,0,_,[attvar(bind),singletons(true)]),
              print(program=Sourcecode),nl,
              maplist(print_prop_val,Vs))),
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

  amass(In,InMass),
  amass(Out,OutMass),
  ratio_for(DeltaMass,OutMass,InMass),

  unique_color_count(In,InColorLen),
  unique_color_count(Out,OutColorLen),
  ratio_for(RatioColorLen,OutColorLen,InColorLen),
  unique_colors(In,InColors),
  unique_colors(Out,OutColors),
  intersection(InColors,OutColors,SharedColors,InSpecificColors,OutSpecificColors),
  append([InSpecificColors,SharedColors,OutSpecificColors],AllUnsharedColors),
  dont_include(AllUnsharedColors),
  sort(AllUnsharedColors,AllColors),
  maplist(length,[InColors,OutColors,SharedColors,InSpecificColors,OutSpecificColors,AllColors],
              [InColorsLen,OutColorsLen,SharedColorsLen,InSpecificColorsLen,OutSpecificColorsLen,AllColorsLen]),
  %maplist(negate_number,[InColorsLen,OutColorsLen,SharedColorsLen,InSpecificColorsLen,OutSpecificColorsLen,AllColorsLen],
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

test_p2(P2):- clsmake,
  append_termlist(P2,[N1,'$VAR'('Result')],N2), 
  time(forall(into_grids(N1,G1),     
     forall((set_current_test(G1),call(P2,G1,G2)),
       once(ignore((grid_arg(G2,GR,Rest),print_side_by_side(red,G1,N1-Rest,_LW,GR,(?-(N2))),dash_chars)))))).

grid_arg(G2,G2,[]):- is_grid(G2),!.
grid_arg(GRest,GR,GRest):- arg(N,GRest,GR), is_grid(GR),!,setarg(N,GRest,grid),!.

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
freeze_on([_NV|Vars],Goal):- maplist(nonvar,Vars),!,call(Goal).
freeze_on(Vars,Goal):- maplist(freeze_until(Goal,Vars),Vars).
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

fix_test_name(V,VV,_):- var(V),!,VV=V.
fix_test_name(G,T,E):- is_grid(G),!, kaggle_arc_io(T,E,_,GO),GO=@=G.
fix_test_name(ID,Fixed,Example+Num):- testid_name_num_io(ID,Tried,Example,Num,_), fix_id(Tried,Fixed).


testid_name_num_io(ID,_Name,_Example,_Num,_IO):- var(ID),!, fail.
testid_name_num_io(ID,_Name,_Example,_Num,_IO):- is_grid(ID),!, fail.
testid_name_num_io(ID,_Name,_Example,_Num,_IO):- is_list(ID), \+ maplist(nonvar,ID),!,fail.

testid_name_num_io([V,Name,Example,ANum,IO|_],TestID,Example,Num,IO):- !, atom(V),VName=..[V,Name],atom_number(ANum,Num),!,fix_id(VName,TestID).
testid_name_num_io(TestID>Example+Num*IO,Name,Example,Num,IO):- !,fix_id(TestID,Name).
testid_name_num_io(TestID>(Example+Num)*IO,Name,Example,Num,IO):- !,fix_id(TestID,Name).
testid_name_num_io(TestID>Example+Num,Name,Example,Num,_IO):- !,fix_id(TestID,Name).
testid_name_num_io(TestID>(Example+Num),Name,Example,Num,_IO):- !,fix_id(TestID,Name).
testid_name_num_io(ID,Name,Example,Num,IO):- ID = (TestID>((Example+Num)*IO)),!,fix_id(TestID,Name),!.
testid_name_num_io(ID,Name,Example,Num,IO):- ID = ((TestID>(Example+Num))*IO),!,fix_id(TestID,Name),!.
testid_name_num_io(ID,Name,Example,Num,IO):- ID = (TestID>(Example+Num)*IO),!,fix_id(TestID,Name),!.
testid_name_num_io(ID,Name,Example,Num,_IO):- ID = ((TestID>Example)+Num),!,fix_id(TestID,Name),!.
testid_name_num_io(ID,Name,Example,Num,_IO):- ID = (TestID>Example+Num),!,fix_id(TestID,Name),!.

testid_name_num_io(V,TestID,Example,Num,IO):- atom(V), atom_concat(VV,'.json',V),!,testid_name_num_io(VV,TestID,Example,Num,IO).
testid_name_num_io(ID,Name,Example,Num,IO):- atom(ID),atomic_list_concat(Term,'_',ID), Term\==[ID], 
  testid_name_num_io(Term,Name,Example,Num,IO),!.
testid_name_num_io(ID,Name,Example,Num,IO):- atom(ID),catch(atom_to_term(ID,Term,_),_,fail), Term\==ID, nonvar(Term), 
  testid_name_num_io(Term,Name,Example,Num,IO),!.
%testid_name_num_io(ID,Name,_Example,_Num,_IO):- atom(ID),!,fix_id(ID,   Name),!.
testid_name_num_io(ID,Name,_Example,_Num,_IO):- fix_id(ID,   Name),!. %, kaggle_arc_io(Name,Example+Num,IO,_).





fix_id(Tried,   Tried):- var(Tried),!.
fix_id(X,_):- is_cpoint(X),!,fail.
fix_id(X,_):- is_cpoints_list(X),!,fail.
fix_id(obj_to_oid(_,X),Fixed):-  !, fix_id(X,Fixed).
fix_id(Tried,   Tried):- kaggle_arc(Tried,_,_,_),!.
fix_id(v(Tried),   TriedV):- !, atom_id(Tried,TriedV),!.
fix_id(t(Tried),   TriedV):- !, atom_id(Tried,TriedV),!.
fix_id(Tried,   TriedV):- atom_id(Tried,TriedV),!.

%DD2401ED
atom_id(NonAtom,TriedV):- \+ atom(NonAtom),!,string(NonAtom),atom_string(Tried,NonAtom),atom_id(Tried,TriedV).
atom_id(Tried,TriedV):- atom_concat(Atom,'.json',Tried),atom_id(Atom,TriedV),!.
atom_id(Tried,t(Tried)):- kaggle_arc(t(Tried),_,_,_),!.
atom_id(Tried,v(Tried)):- kaggle_arc(v(Tried),_,_,_),!.
atom_id(Atom,TriedV):- downcase_atom(Atom,Tried),Atom\==Tried,atom_id(Tried,TriedV).
%fix_id(Tried,Fixed):- !, fail,compound(Tried),!,arg(_,Tried,E),nonvar_or_ci(E),fix_id(E,Fixed),!.




print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).


parcCmt(TName):- 
  fix_test_name(TName,TestID,_),
  %color_print(magenta,call(((grid_hint(TestID))))),
  parcCmt1(TestID).
parcCmt1(TName):-
  ignore((
  fix_test_name(TName,TestID,_),
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
      set_stream(current_output, tty(true)),
      parc1(ExampleNum,OS))),
      told)).

parcf:- parcf((tst+_),_).
parcf(ExampleNum,OS):- make,   luser_setval(task,[]),
 locally(set_prolog_flag(color_term,false),
 setup_call_cleanup( open('test_cache.vt100',write,O,[encoding(text)]), 
   with_output_to(O,
    ((set_prolog_flag(color_term,true),
      set_stream(O, tty(true)),
      parc1(ExampleNum,OS)))), close(O))).

parctt:- parctt((tst+_),6300*3).
parctt(ExampleNum,OS):- make,   luser_setval(task,[]),
  locally(set_prolog_flag(color_term,false),
   setup_call_cleanup( open('kaggle_arc_test_cache.new',write,O,[encoding(utf8)]), 
   with_output_to(O,
    ((set_stream(O, tty(false)),
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
    print_side_by_side(I,O), format('").\n'),
  ignore((write('%= '), parcCmt(TestID),nl,nl))))).

%color_sym(OS,[(black='°'),(blue='©'),(red='®'),(green=''),(yellow),(silver='O'),(purple),(orange='o'),(cyan= 248	ø ),(brown)]).
color_sym(OS,C,C):- var(OS),!.
color_sym(OS,C,Sym):- is_list(C),maplist(color_sym(OS),C,Sym),!.
color_sym(_,black,' ').
color_sym(OS,C,Sym):- color_sym(OS,4,C,Sym).
color_sym(_,_,C,Sym):- enum_colors(C),color_int(C,I),nth1(I,`ose=xt~+*zk>`,S),name(Sym,[S]).
%color_sym(P*T,_,C,Sym):- enum_colors(C),color_int(C,I),S is P+I*T,name(Sym,[S]).

:- all_source_file_predicates_are_exported.

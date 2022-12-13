/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

make_keypad([[_,_,_],[_,_,_],[_,_,_]]).

key_pad_tests(TestID):-  kaggle_arc(TestID,tst+0,In,Out), once((make_keypad(In), make_keypad(Out))).
key_pad_tests(TestID):-  kaggle_arc(TestID,tst+0,In,Out), once((make_keypad(Out), \+ make_keypad(In))).
key_pad_tests(TestID):-  kaggle_arc(TestID,tst+0,In,Out), once((make_keypad(In), \+ make_keypad(Out))).

forall_count(P,Q):- flag('$fac_t',W,W), W>0,!,time(forall(P,Q)),report_forall_count(progress,so_far).
forall_count(P,Q):-
  setup_call_cleanup(flag('$fac_t',W,0),
    setup_call_cleanup(flag('$fac_p',W2,0),
      time(forall((P,flag('$fac_t',X,X+1)),
        ignore(once((Q,flag('$fac_p',Y,Y+1)))))),     
      (report_forall_count(P,Q),flag('$fac_p',_,W2))),
    flag('$fac_t',_,W)).

report_forall_count(P,Q):- flag('$fac_t',ET,ET),flag('$fac_p',EP,EP),
  (ET<2 -> true ;
   (Percent is round(EP/ET*10_000)/100,
    fmt('~N % Success ~p% (~q) for ~p ~n',[Percent,EP/ET,forall_count(P,Q)]))).
  

/*
solves_all_pairs(TestID,P,P2S):- kaggle_arc(TestID,tst+0,_,_), 
 findall(P2,(easy_solve_by(TestID,P2),
   findall(try_p2(P2,In,Out),
     kaggle_arc(TestID,_,In,Out),AllTrue),maplist(call,AllTrue)),[P|P2S]).
*/
/*
test_easy:- clsmake, forall_count(ensure_test(TestID),test_easy(TestID)).
test_easy(TestID):- 
  solves_all_pairs(TestID,P2), dash_chars, dmsg(P2), print_test(TestID), dmsg(P2), dash_chars.

solves_all_pairs(TestID,P2):- !,
   easy_solve_by(TestID,P2),
   kaggle_arc(TestID,tst+0,EI,EO), 
   grid_call(P2,EI,EM),EM=@=EO,!.

solves_all_pairs(TestID,P2):-
   easy_solve_by(TestID,P2),
   findall(grid_call(P2,In,Out),kaggle_arc(TestID,_,In,Out),AllTrue),   
   nop(maplist(grid_call,AllTrue)),
   kaggle_arc(TestID,trn+0,TI,TO),
   try_p2(P2,TI,TO), 
   kaggle_arc(TestID,tst+0,EI,EO), 
   try_p2(P2,EI,EO).
*/

        
%test_easy:- clsmake, fail.
/*
%test_easy:- test_p2(test_easy_solve_pair).
%test_easy:- test_p2(simple_todolist(_)).
*/
test_easy:- once(update_and_fail_cls),fail.
test_easy:- get_pair_mode(entire_suite),!,forall_count(all_arc_test_name(TestID),test_easy(TestID)).
test_easy:- get_pair_mode(whole_test), get_current_test(TestID),!,must_det_ll(ignore(test_easy(TestID))).
test_easy:- get_pair_mode(single_pair),
  get_current_test(TestID),some_current_example_num(ExampleNum),!,
  forall_count(kaggle_arc(TestID,ExampleNum,I,O),
    (set_current_pair(I,O),
     print_ss((?-test_easy_solve_test_pair(TestID,ExampleNum,_,_))=I,single_pair=O),
     test_easy_solve_test_pair(TestID,ExampleNum,I,O))).
test_easy:- test_p2(test_easy_solve_pair).

:- luser_default(cmd,test_easy). 

test_easy_solve_pair(I,O):- %set_current_test(I),
 (kaggle_arc(TestID,ExampleNum,I,O) *-> test_easy_solve_test_pair(TestID,ExampleNum,I,O) ; 
   (test_example_grid(I),test_example_grid(O))).

test_easy_solve_test_pair(TestID,ExampleNum,I,O):- var(TestID),get_current_test(TestID),nonvar(TestID),!,
  test_easy_solve_test_pair(TestID,ExampleNum,I,O).
test_easy_solve_test_pair(TestID,ExampleNum,I,O):- var(ExampleNum),some_current_example_num(ExampleNum),!,
  test_easy_solve_test_pair(TestID,ExampleNum,I,O).
test_easy_solve_test_pair(TestID,ExampleNum,I,O):- 
  test_easy_solve_test_pair_now(_,TestID,ExampleNum,I,O,_P2S).

p2_from_p2_possibles(_TestID,List,P2):- is_list(List),!,member(P2,List).
p2_from_p2_possibles(TestID,P2Possibles,P2):- easy_solve_by(TestID,P2), ignore(member(P2, P2Possibles)).

test_easy_solve_test_pair_now(P2Possibles,TestID,ExampleNum,I,O,P2S):-   
   ensure_test(TestID),
   ignore(kaggle_arc(TestID,ExampleNum,I,O)),      
   (CALL=  ?-(test_easy_solve_test_pair(TestID,ExampleNum,'$VAR'('I'),'$VAR'('O')))),   !,
   set_current_pair(I,O),
   findall(P2,    
    ( p2_from_p2_possibles(TestID,P2Possibles,P2),
      once((((EM=O;put_attr(EM,expect_p2,O)),grid_call(P2,I,EM),I\=@=EM))),nonvar(EM), %EM=@=O,
     (EM=@=O->print_side_by_side([grid_call_1(P2)=I,result=EM,needed=O]);fail)),
     P2SI),!,
   (P2SI\==[]->P2S=P2SI;fail),
/*
   (P2SI\==[]->P2S=P2SI;findall(grid_call_unify(P2),(easy_solve_by(TestID,P2),grid_call(P2,I,EM),nonvar(EM),
      \+ \+ EM=O, !,
     print_side_by_side([grid_call_2(P2)=I,result=EM,needed=O])),P2S)),
*/
   nl_if_needed,
   (P2S\==[] -> wqs(["Passed:\n ",CALL," using\n ",call(maplist(show_passing(TestID),P2S))]) ; ( \+ get_pair_mode(entire_suite),wqs(["failed: ",b(q(CALL))]),!,fail)).

show_passing(TestID,P2):- write(' '),pp(green, ?- easy_solve_suite(TestID,P2)).
grid_call_unify(P2,I,O):- grid_call(P2,I,O).

test_example_grid(I):- var(I),!.
test_example_grid(I):- kaggle_arc(TestID,ExampleNum,I,O),!,test_easy_solve_test_pair(TestID,ExampleNum,I,O).
test_example_grid(O):- kaggle_arc(TestID,ExampleNum,_,O),!,test_easy_solve_test_pair(TestID,ExampleNum,O,_).
test_example_grid(T):- is_valid_testname(T),set_current_test(T),!,kaggle_arc(T,ExampleNum,I,O),test_easy_solve_test_pair(T,ExampleNum,I,O).
test_example_grid(G):- set_current_test(G),!,get_current_test(TestID),test_easy(TestID).

%easy_solve_by(_TestID,P2):- ground(P2),!.
%easy_solve_by(_TestID,grow_from_shape).
/*
easy_solve_by( TestID,P2):- nonvar(P2),!, copy_term(P2,P2T), findall(P2T,(easy_solve_by(TestID,P2T),P2\=@=P2T),List), member(P2,[P2|List]).
easy_solve_by( TestID,flip_Once(_)):- get_black(Black),user:arc_test_property(TestID,common,comp(cbg(Black),i-o,grav_rot),_).
*/
%easy_solve_by(_TestID,use_simplified_recall(_)).
easy_solve_by(_TestID,P2):- easy_p2(P2). % easy_p2(P2).




easy_p2_0(blur_or_not_least_rot90_x4):- test_hint(mass_and_area(grow_less_than_times(4),'=')).
easy_p2_0(blur_rot90):- test_hint(mass_and_area(grow_less_than_times(1),'=')).
easy_p2_0(unbind_and_fill_in_blanks(_Code)).
easy_p2_0(repair_and_select(_How,_M)):- test_hint(input_gt,unique_color_count),
                                      test_hint(input_plus(1),unique_color_count).

%easy_p2(use_simplified_recall(_)).
easy_p2(do_easy1(_)). %:- easy0(_,GFS).
easy_p2(do_easy2(_,_)).

color_getter_p2(unique_colors).
color_getter_p2(colors).

io_colors(I,O,IOColors):- 
  color_getter_p2(P2),
  once((call(P2,I,IColors),
  call(P2,O,OColors), 
  ignore(IColors=OColors),
  append(IColors,OColors,IIOOColors))),
  findall(C,(sub_term(C,IIOOColors),is_color(C)),OIColors),
  list_to_set(OIColors,IOColors).

subst_colors_with_vars(Colors,Vars,I,O):-
 % io_colors(I,O,Colors),
  length(Colors,CL),length(Vars,CL),
  subst_2L(Colors,Vars,I,O).

%apply_equiv_xforms(subst_colors_with_vars(Colors,Vars),II,III):- subst_colors_with_vars(Colors,Vars,II,III).

use_simplified_recall(Where,II,OO):-  get_simplified_recall(Where,II,OO).

get_simplified_recall(W,I,O):- get_simplified_recall_exact(W,I,O)*->true;get_simplified_recall_close(W,I,O).

get_simplified_recall_exact(d(Where+ExampleNum),I,O):- kaggle_arc(Where,ExampleNum,I,O),get_current_test(Current),Where\==Current.

t_v2(t,v). t_v2(v,t).
get_simplified_recall_close(Where+ExampleNum,II,OO):-    
   get_current_test(Current),
   t_v2(T,V), functor(Current,T,1), !, 
   (functor(Where,V,1);functor(Where,T,1)),
   abstracted_recall(Where,ExampleNum,I,O),
   kaggle_arc(Where,ExampleNum,OI,OO),
   Where\==Current,
   I+O = II+OO,!,
   print_ss([simplified_recall(Current->Where)=OI,orig=OO,match=I,for=O]).

:- dynamic(in_abstracted_recall_cache/4).
test_abstracted_recall(TestID):- 
  ensure_test(TestID), 
  forall(abstracted_recall(TestID,ExampleNum,I,O),
  (number_gridvars(I+O),%writeq(I),writeq(O),
  print_single_pair(TestID,ExampleNum,I,O))).

number_gridvars(Grid):-
  grid_variables(Grid,Vars),
  numbervars(Vars).

grid_variables(Grid,Vars):- term_variables(Grid,Vars).

:- abolish(in_abstracted_recall_cache/4).
:- dynamic(in_abstracted_recall_cache/4).

grid_copy_size(G0,G9):- grid_size(G0,H,V),make_grid(H,V,G9).
abstracted_recall(TestID,ExampleNum,I,O):- \+ ground(ExampleNum+TestID),!,kaggle_arc(TestID,ExampleNum,_,_),abstracted_recall(TestID,ExampleNum,I,O).
abstracted_recall(TestID,ExampleNum,I,O):- in_abstracted_recall_cache(TestID,ExampleNum,I,O),!.
abstracted_recall(TestID,ExampleNum,II,OO):-
   grid_copy_size(II,I),
   ignore((nonvar(OO),grid_copy_size(OO,O))),
   kaggle_arc(TestID,ExampleNum,I,O),
   PairIn = I+O, 
   get_black(Black),
   once((io_colors(I,O,Colors), length(Colors,CL),length(Vars,CL))),
   subst_2L([Black|Colors],[bg|Vars],PairIn,PairOut), PairOut = II+OO,
   asserta(in_abstracted_recall_cache(TestID,ExampleNum,II,OO)).


do_easy1(C1,I,O):- easy0(_N,C1),once(grid_call(C1,I,O)),I\=@=O.
do_easy2(C1,C2,I,O):- 
  easy0(N,C1),N=<3,
  C1\==(=),
  once(grid_call(C1,I,M)),I\=@=M,
  easy0(M,C2),M>3,
  C1\==C2, C2\== (=),
  grid_call(C2,M,O),
  M\=@=O.

/*
%easy_solve_by( TestID,repair_and_select(_How,_M)):- is_symgrid(TestID),!.
easy_p2(_):- trace.

%easy_p2(repair_and_select_property([unbind_color(_),now_fill_in_blanks_good],repaired)).
easy_p2(blur_or_not(rot90_blur_flipD)):- test_hint(mass_and_area(grow_less_than_times(1),'=')).
easy_p2(repair_and_select(_How,_M)):- test_hint(input_lt,unique_color_count).
easy_p2(blur_or_not_least_2(_,_)):- test_hint(mass_and_area(grow_less_than_times(2),'=')).
%easy_p2(unbind_and_fill_in_blanks(_Code)).
easy_p2(simple_todolist([trim_blank_lines,grow_2])):- test_hint(mass_and_area(input_gt,(\=))).
%easy_p2(simple_todolist(_)).
easy_p2(P2):- easy0(_,P2).*/
/*
easy_p2(do_simple_todolist(List)):- List = [C0,C1,C2,C3,C4,C5],
  easy0(0,C0),easy0(1,C1),easy0(2,C2),easy0(3,C3),easy0(4,C4),easy0(5,C5),
  once((nth1(M,List,'='),nth1(N,List,'='),nth1(O,List,'='),nth1(P,List,'='),M<N,O>N,P>O)).
*/
  
  
%easy_p2(two_ops(repair_in_vm(repair_repeats(black)),get(repaired))).

rot90_blur_flipD(I,O):- h_as_v(blur_or_not(flipD),I,O).

expect_p2:attr_unify_hook(_,_).

do_simple_todolist(C,I,O):- var(C),!,throw(var_do_simple_todolist(C,I,O)).
do_simple_todolist(Nil,I,O):- Nil==[], !,I=O.
do_simple_todolist([H|T],I,O):- !, do_simple_todolist(H,I,M),do_simple_todolist(T,M,O).
do_simple_todolist(C,I,O):- \+ callable(C),!,throw(not_callable_do_simple_todolist(C,I,O)).
do_simple_todolist(P2,I,O):- grid_call(P2,I,O).

repair_and_select(How,M,I,O):- induce_from_training(repair_and_select_property(How,M),I,O).
repair_and_select_property(How,get_scene_object(M),I,O):- 
 %\+ is_grid_symmetricD(I), 
    %How=[_|_],
    unbind_and_repair(How,I,Mid), %is_grid_symmetricD(Mid), !, 
    which_member(I,Mid,M,EO), EO=O.

unbind_and_repair(Code,Grid,RepairedResultO):-
 unbind_and_fill_in_blanks(Code,Grid,RepairedResultO).

unbind_and_repair(Code,Grid,RepairedResultO):-
 Code = [blur_rot90], blur_rot90(Grid,RepairedResultO).

which_member(I,Mid,M,EO):- which_member(I,Mid,NVs), member(M-EO,NVs).

which_member(Grid,RepairedResultG,Results):-
  once((localpoints_include_bg(Grid,OriginalPoints),
  grid_size(Grid,H,V),
  localpoints_include_bg(RepairedResultG,RepairedPoints),
  intersection(OriginalPoints,RepairedPoints,Unchanged,NeededChanged,Changed),
  points_to_grid(H,V,Changed,ChangedG),
  trim_to_rect(Changed,TrimChangedG),
  points_to_grid(H,V,Unchanged,UnchangedG),
  points_to_grid(H,V,NeededChanged,NeededChangedG))),
  Results = [repaired-RepairedResultG,
             changed-TrimChangedG,            
             changedUntrimmed+ChangedG,
             neededChanged+NeededChangedG,
             unchanged+UnchangedG].
  


%print_side_by_side_io(P2,I,O):- !, % \+ is_gridoid(P2),
%  print_side_by_side(green,I,in(P2),_,O,out(P2)),!.
print_side_by_side_io(P2,I,O):-
  training_pp_msg_color(P2,Color), !, 
  print_side_by_side(Color,I,[P2|in],_,O,out(P2)),!.

training_pp_msg_color(P2,Color):- sub_term(E,P2),compound(P2),arg(1,P2,E),atomic(E),pp_msg_color(E,Color).
training_pp_msg_color(P2,Color):- pp_msg_color(P2,Color).

induce_from_training(P2,I,O):- \+ is_grid(I),!,into_grid(I,G),induce_from_training(P2,G,O).
%induce_from_training(P2,I,O):- nonvar(O),!,
induce_from_training(P2,I,O):- ground_enough(P2),!,wdmsg(ground_enough(P2)),grid_call(P2,I,O).
induce_from_training(P2,I,O):- get_current_test(TestID), !, induce_from_training(TestID, P2,I,O).

induce_from_training(TestID, P2,I,O):-  (nonvar(O) ; (kaggle_arc(TestID,Ex1,I,O),(Ex1 = trn+_))), !, induce_from_training_pair(P2,Ex1,I,O).
induce_from_training(TestID, P2,I,O):- !,
  (ExampleNum = trn+_),
  findall(sample(ExampleNum,II0,OO0), 
     (kaggle_arc(TestID,ExampleNum,II0,OO0),I\==II0),TrainingPairs),
  TrainingPairs\==[],!,
  induce_from_training_pairs(P2,TrainingPairs,TrainingPairs,I,O),!.
induce_from_training(TestID, P2,I,O):- %copy_term(P2,P22),!,
  (ExampleNum = trn+_ ),
  findall(sample(ExampleNum,II0,OO0), (kaggle_arc(TestID,ExampleNum,II0,OO0),I\==II0),TrainingPairs),
  select(sample(Ex1,II1,OO1),TrainingPairs,More), induce_from_training_pair(P2,Ex1,II1,OO1),
  member(sample(Ex2,II2,OO2),  More),             induce_from_training_pair(P2,Ex2,II2,OO2),
  induce_from_testing_pair(P2,Ex1,I,O).



induce_from_testing_pair(P2,Ex1,I,O):- 
   set_current_pair(I,O),
   with_io_training_context(I,O,
     ((grid_call(P2,I,O), print_side_by_side_io(
        induce_from_testing_pair_pass2(P2,Ex1),I,O)))),!.

induce_from_training_pairs(P2,[],_TP,I,O):- !, induce_from_testing_pair(P2,tst+0,I,O).
induce_from_training_pairs(P2,TrainingPairs,TP,I,O):-  
  select(sample(Ex1,II1,OO1),TrainingPairs,More), induce_from_training_pair(P2,Ex1,II1,OO1),!, 
  induce_from_training_pairs(P2,More,TP,I,O).

induce_from_training_pair(P2,Ex1,II1,OO1):-   
  with_io_training_context(II1,OO1, 
   (((grid_call(P2,II1,OO1) *-> pp(induce_from_training_continue(P2,Ex1)) ;
       ((fail, nonvar(OO1), grid_call(P2,II1,_)) -> true ; (pp(induce_from_training_fail_cont(P2,Ex1)),fail))),
      ignore(( warn_and_fail_on_bad_p2(cyan,orange,checking_training(P2,Ex1),P2,II1,OO1)))))).

warn_and_fail_on_bad_p2(Cyan,Orange,Ex1,P2,I,Expect):- 
 collapsible_section((
 \+ \+ with_io_training_context(I,Expect,   
 ((put_attr(M,expect_p2,Expect),
   (grid_call(P2,I,M)->OurOut=M;OurOut=I),
   count_difs(OurOut,Expect,Errors),
   (Errors = 0 
     -> banner_grids(Cyan,I,pass_p2(P2,Ex1),OurOut,"MATCH") 
     ; (banner_grids(Orange,OurOut,fail(Errors,P2,Ex1),Expect,"MISMATCH"),
        banner_grids(red,I,fail(Errors,P2,Ex1),OurOut,"WRONG"),
        nop(show_sameness_or_lameness(Cyan,Orange,warn_and_fail_on_bad_p2(P2,Ex1),OurOut,Expect,Errors)),!,fail))))))).
     


  /*  
induce_from_training_pair(P2,Ex1,II1,OO1):- 
  print_side_by_side_io(induce_from_training(P2,Ex1),II1,OO1), 
   with_io_training_context(II1,OO1,((grid_call(P2,II1,OO1),
      grid_call(P2,II1,OOO1),print_side_by_side_io(checking_training(P2,Ex1),II1,OOO1)))),!.
*/
:- dynamic(test_results/4).


try_p2_verbose(P2,TI1,TO1):-grid_call(P2,TI1,EM),print_side_by_side(grid_call(P2),TI1,EM),try_p2(=,EM,TO1).

p2_in_to_p2s(P2In,P2S):- (var(P2In);is_list(P2In);is_cons(P2In)),!,P2S=P2In.
p2_in_to_p2s(P2In,P2S):-  P2S = [P2In].

test_easy(TestID):- var(TestID),!,
  forall_count(ensure_test(TestID),test_easy(TestID)).
test_easy(TestID):- 
  get_time(Now), luser_setval(test_start_time,Now),
  % once(print_all_info_for_test),
  arcdbg_info(blue,"BEGIN_TEST"=TestID),
  print_test(TestID),   
  (easy_solve_training(TestID,P2)*-> 
    (easy_solve_testing(TestID,P2)*-> (nl,nl,arcdbg_info(green,success(TestID,P2)),assert_test_result(pass,test_easy,P2),nl,nl) ; arcdbg_info(yellow,didnt_work(TestID,P2)))
     ; (nop(arcdbg_info(red,failed_finding_plan_to_solve_training(TestID))),fail)),!.
test_easy(TestID):- arcdbg_info(red,failed_test(TestID)),assert_test_result(fail,test_easy,dunno),!,fail.

assert_test_result(PassFail,Prover,How):- get_current_test(TestID),luser_getval(test_suite_name,SuiteX),
  get_time(Now),luser_getval(test_start_time,Was), Elapsed is Now - Was,
  W=test_results(PassFail,SuiteX,TestID,pew(Prover,Elapsed,How)), pp(yellow,W), assert_if_new(W).


easy_solve_training(TestID,P2In):- 
  p2_in_to_p2s(P2In,P2S),  
  ExampleNum = trn+_Some,
  once(((kaggle_arc(TestID,ExampleNum,TI1,TO1),
     test_easy_solve_test_pair_now(P2In,TestID,ExampleNum,TI1,TO1,P2S)))),
  pp(?-easy_solve_training(TestID,P2In)),
  P2In=P2S,
   forall((kaggle_arc(TestID,Other,TI2,TO2),ExampleNum\==Other),
     test_easy_solve_test_pair_now(P2In,TestID,Other,TI2,TO2,P2S)),
   
  nop((
   collapsible_section((
      once((member(P2,P2S),try_p2_verbose(P2,TI2,TO2),
       warn_and_fail_on_bad_p2(cyan,orange,generalness,P2,TI2,TO2))))))).

easy_solve_testing(TestID,P2In):- p2_in_to_p2s(P2In,P2S),
   pp(?-easy_solve_testing(TestID,P2In)),
   nop((
   member(P2,P2S),
   forall(kaggle_arc(TestID,tst+Tst,EI,EO),
     warn_and_fail_on_bad_p2(green,red,final_test(TestID,Tst),P2,EI,EO)))),!.

:- meta_predicate(with_io_training_context(+,+,0)).
with_io_training_context(I,O,G):- (peek_vm(PrevVM), PrevVM.grid_o  =@=I),!, set(PrevVM.grid_target)=O, with_current_pair(I,O,G).
with_io_training_context(I,O,G):- (peek_vm(PrevVM), PrevVM.grid_o \=@=I),!,
 call_cleanup(with_io_training_context1(I,O,G),set_vm(PrevVM)),!.
with_io_training_context(I,O,G):- with_io_training_context1(I,O,G).
:- meta_predicate(with_io_training_context1(+,+,0)).
with_io_training_context1(I,O,G):- 
 set_current_pair(I,O),
  %get_current_test(TestID),
  %kaggle_arc(TestID,ExampleNum,I,O),
 with_current_pair(I,O,
  (grid_to_tid(I,ID), into_fti(ID,in_out,I,VM),
   set(VM.grid)=I,
   set(VM.grid_target)=O,
   set_vm(VM),
   call(G))).


gravity_s_1(I,O):- gravity(1,s,I,O),!.
ground_enough(P2):- ground(P2),!.
ground_enough(P2):- compound(P2),arg(1,P2,E),ground_enough(E),!.

test_hint_easy(A,B):- nop(test_hint(A,B)).
test_hint_easy(A):- nop(test_hint(A)).

%easy0(X):- easy_sol(X).
%easy0(_,=).
easy0(0,trim_hv_repeats):- test_hint(mass_and_area(input_lt,input_lt)).
easy0(0,trim_to_rect):- test_hint(mass_and_area(ignore_equal,input_lt)).
easy0(1,trim_blank_lines):- test_hint(mass_and_area(ignore_equal,input_lt)).
easy0(1,gravity_s_1):- test_hint(mass_and_area('=','=')).

easy0(2,flip_Once(_)):- test_hint(mass_and_area('=','=')).
%easy0(3,maybe_subst_fg_color(_,_)).
easy0(2,remove_color(green)):- test_hint(input_lt,mass), test_hint(input_lt,unique_color_count).
easy0(2,maybe_subst_fg_color(_,_)):-  test_hint(mass_and_area('=','=')).

%easy0(2,fill_odd_even(_,_)):-  test_hint(mass_and_area((\=),'=')). % 00d62c1b
%easy_solve_suite(t('00d62c1b')).

%unique_colors( test_hint('=',unique_color_count),test_hint('\=',unique_colors).

easy0(4,blur_flipV):- test_hint(mass_and_area(grow_less_than_times(1),'=')).
easy0(4,blur_or_not_least_2(flipV,flipH)):- test_hint(mass_and_area(grow_less_than_times(2),'=')).

easy0(5,grow_from_shape):-
   %test_hint(ratio_between(area,square(area))),
   %test_hint(grow_greater_than_times(3),mass),
   test_hint('=',unique_color_count),
   ignore(test_hint(is_squared,mass)), %;test_hint(is_squared,mass)
   %test_hint(input_lt,area),
   test_hint(mass_and_area(input_lt,input_lt)),
   %test_hint(ratio_between(mass,square(mass))), %t(007bbfb7)
   %test_hint(ratio_between(square(mass),mass)),
   %test_hint(mass_and_area(input_gt,input_gt)),
   true.
easy0(5,increase_size(IS)):- test_hint(mass_and_area_times(N)), 
    (nonvar(N)-> IS is floor(sqrt(N)) ; is_squared(N,IS)).
   
easy0(5,increase_size_by_grid_mass):- test_hint(ratio_between(mass,square(area))). %ac0a08a4
easy0(5,increase_size_by_color_count):- test_hint(ratio_between(unique_color_count,and(square(mass),square(area)))). %ac0a08a4
easy0(5,grow_4_p2(rot90)):- test_hint(mass_and_area_times(4)).% 7fe24cdd
easy0(5,grow_4_p2(sameR)):- test_hint(mass_and_area_times(4)).% t('3af2c5a8')
easy0(5,grow_4_flipHV):- test_hint(mass_and_area_times(4)).% t('3af2c5a8')
easy0(5,grow_2):- test_hint(mass_and_area_times(2)). % t('963e52fc').
easy0(5,grow_flip_2):- test_hint(mass_and_area_times(2)). % 963e52fc
easy0(5,double_size):- test_hint(n_times(2),mass).
%easy0(5,crop_by(_)).

%easy_solve_suite(TestID):- nonvar(TestID),test_easy(TestID).
easy_solve_suite(TestID):- var(TestID),easy_solve_suite(TestID,_Uses).

easy_solve_suite(t('963e52fc')).
easy_solve_suite(t('7fe24cdd')).
easy_solve_suite(t('3af2c5a8')).
easy_solve_suite(t('ac0a08a4')).

easy_solve_suite(TestID,P2):- (nonvar(TestID),nonvar(P2)),!, 
  easy_solve_training(TestID,P2),
  easy_solve_testing(TestID,P2).
easy_solve_suite(t('67a3c6ac'),flip_Once(flipH)).
easy_solve_suite(t('007bbfb7'),grow_from_shape).
easy_solve_suite(t('c3e719e8'),grow_from_shape).

max_fg_color(Grid,Color):- colors(Grid,Colors),member(cc(Color,N),Colors),N>0,is_fg_color(Color),is_real_color(Color).

% comp(cbg(black),i-o,ogs)=ogs([ogs(notrim,whole,loose,loc2D(1,1)),ogs(notrim,whole,call_ogs(rot90,loose),loc2D(4,1)),ogs(notrim,whole,call_ogs(rot180,loose),loc2D(4,4)),ogs(notrim,whole,call_ogs(rot270,loose),loc2D(1,4))])
grow_each_p2(Rot90,In,[A,B,C,D]):- into_grid(In,A),call(Rot90,A,B),call(Rot90,B,C),call(Rot90,C,D).
grow_4_p2(P2,In,Out):- grow_each_p2(P2,In,[A,B,C,D]),p1_grow([[=(A),=(B)],[=(D),=(C)]],[],Out).

%grow_4_rot(In,Out):- !, p2_grow([[sameR,rot90],[rot270,rot180]],In,Out).
learn_about_p2(grow_4_rot).

grow_4_rot(In,Out):- grow_4_p2(rot90,In,Out).

%grow_4(In,Out):- !, p2_grow([[sameR,sameR],[sameR,sameR]],In,Out).
grow_4_flipHV(In,Out):- !, flipV(In,FlipV),append(In,FlipV,Left),flipH(Left,Right),append_left(Left,Right,Out).
%grow_4(In,Out):- grow_each_p2(=,In,[A,B,C,D]), grow([[A,B],[D,C]],[],Out).
grow_4(In,Out):- grow_4_p2(sameR,In,Out).


maybe_subst_fg_color(X,Y,I,O):- 
   guess_unbound(swap_colors(X,Y,I,O)),
   swap_colors(X,Y,I,O).

guess_unbound(swap_colors(X,Y,I,O)):- 
   unique_fg_colors_pos(I,IC),unique_fg_colors_pos(O,OC), 
   member(X,IC), once(var(O);\+ member(X,OC)),
   member(Y,OC), X\==Y, \+ member(Y,IC).

%unique_colors_of(In,Blue):- unique_colors(In,Colors),member(Blue,Colors),is_real_color(Blue).
unique_fg_colors_pos(I,IC):- var(I),!,available_fg_colors(IC).
unique_fg_colors_pos(I,IC):- unique_colors(I,ICB),delete(ICB,black,IC).

simple_todolist(List,I,OO):- nonvar(List),!, do_simple_todolist(List,I,OO).
simple_todolist(List,I,OO):- ignore(get_attr(OO,expect_p2,O)),nonvar(O),!,simple_todolist(List,I,O).
/*
simple_todolist( [],I,O):- I=O,!.
simple_todolist([P2|List],I,O):-
  easy0(_,P2),grid_call(P2,I,M),
  fits_grid(O,M),
  simple_todolist(List,M,O).
*/
simple_todolist(List,I,OO):-
  ignore(get_attr(OO,expect_p2,O)),
  guess_simple_todolist(0,[],[],List,I,O,OO),!.

/*
guess_simple_todolist(N,SolSoFar,DoneSoFar,Plan,I,O,OO):- !,
 ((fits_grid(O,I),N>0) 
-> (Plan=DoneSoFar,OO=I) 
;(findall(h_g(H1,I1),(easy0(N,H1),grid_call(H1, I,I1), (H1=='=' -> true ; (is_a_change(I,I1), \+ member(I1,SolSoFar)))), H1G),
  predsort_using_only(arg(2),H1G,H1GSS),sort(H1GSS,H1GS),
  maplist(arg(2),H1GS,SOFAR),append(SolSoFar,SOFAR,NewSOFAR),
  member(h_g(H1,I1),H1GS),
  Next is N+1,
  guess_simple_todolist(Next,NewSOFAR,[H1|DoneSoFar],Plan,I1,O,OO))).
*/
guess_simple_todolist(N,_,_,_,_,_,_):- N> 10,!,fail.
guess_simple_todolist(N,_Failed,IPlan,Plan,I,O,OO):- N\==0, fits_grid(O,I),!,OO=I,reverse(IPlan,Plan).
/*
guess_simple_todolist(N,Failed,Planned,Plan,I,O,OO):- Next is N+1,
  findall(h_g(H1,I1),(easy0(N,H1),grid_call(H1, I,I1), (H1=='='-> true ; nop((is_a_change(I,I1), \+ member(h_g(_,I1),Failed))))), H1G),
  predsort_using_only(arg(2),H1G,H1GS),
  select(h_g(H1,I1),H1GS,Rest),
  %append(Rest,Failed,NewFailed), 
  Rest=NewFailed, 
  guess_simple_todolist(Next,NewFailed,[H1|Planned],Plan,I1,O,OO).
*/
guess_simple_todolist(N,Failed,Planned,Plan,I,O,OO):-
  Next is N+1, guess_simple_todolist(Next,Failed,Planned,Plan,I,O,OO).

guess_simple_todolist(N,Failed,Planned,Plan,I,O,OO):- Next is N+1,
  easy0(N,H1), grid_call(H1, I,I1), is_a_change(I,I1), guess_simple_todolist(Next,Failed,[H1|Planned],Plan,I1,O,OO).

/*
simple_todolist(SolSoFar,List,I,O,OO):-
  findall(h_g(H1,I1),(easy0(H1),grid_call(H1, I,I1),\+ member(I1,SolSoFar)), H1G),
  predsort_using_only(arg(2),H1G,H1GSS),sort(H1GSS,H1GS),
  maplist(arg(2),H1GS,SOFAR),
  member(h_g(H1,I1),H1GS),
  ((findall(h_g(H2,I2),(easy2(H2),H1\==H2,grid_call(H2,I1,I2),\+ member(I2,SOFAR)),H2G),
    predsort_using_only(arg(2),H2G,H2GSS),sort(H2GSS,H2GS),
    member(h_g(H2,I2),H2GS),
  ((fits_grid(O,I2),[H1,H2]=List,I2=OO))) 
  ;((fits_grid(O,I1),[H1]=List,I1=OO)))
  ,!,
  nop(print_side_by_side(green,I,simple(List),_,OO,simple(List))),!.
*/

fits_grid(O,_):- var(O),!.
fits_grid(O,I1):-O=@=I1.


blur_rot90(I,O):- duplicate_term(I,II), unbind_color(red,II,M), blur_or_not(rot90,M,O).
blur_flipV(I,O):- duplicate_term(I,II), blur_or_not(flipV,II,O).

blur_or_not_least_2(FlipV,Rot180,I,O):- 
  %blur_or_not_least_pair(FlipV,Rot180),
  blur_or_not_least_2a(FlipV),
  blur_or_not_least_2b(Rot180),
  Rot180\==FlipV,
  do_simple_todolist([
    into_grid,
    duplicate_term,
    blur_or_not_least(FlipV),
    blur_or_not_least(Rot180)],I,O).

blur_or_not_least_2a(flipV). blur_or_not_least_2a(rot90). blur_or_not_least_2a(flipD). blur_or_not_least_2a(blur_or_not_least_rot90_x2).
blur_or_not_least_2b(flipH). blur_or_not_least_2b(rot90). blur_or_not_least_2b(rot180). 

blur_or_not_least_rot90_x2(I,O):- 
  do_simple_todolist([
    into_grid,
    duplicate_term,
    blur_or_not_least(rot90),
    blur_or_not_least(rot90)],I,O).

blur_or_not_least_rot90_x4(I,O):- 
  do_simple_todolist([
    into_grid,
    duplicate_term,
    blur_or_not_least(rot90),
    blur_or_not_least(rot90),
    blur_or_not_least(rot90)],I,O).

% HOUGH Transform

%unbind_and_fill_in_blanks([guess_unbind_color(black),P2],Grid,RepairedResultO):-
%  now_fill_in_blanks(P2,Grid,RepairedResultO).

crop_by(HH/H,In,Out):- grid_size(In,H,V),between(1,H,HH),HH<H,clip(1,1,HH,V,In,Out).
grow_2(In,Out):- append_left(In,In,Out).
grow_flip_2(In,Out):- flipH(In,FlipH),append_left(In,FlipH,Out).
shrink_grid(I,O):- grid_to_norm(I,_,O),!.



:- retractall(muarc_tmp:test_info_cache/2).
:- abolish(muarc_tmp:test_info_cache,2).
:- dynamic(muarc_tmp:test_info_cache/2).

is_fti_step(last_indiv).
last_indiv(VM):- show_vm_changes(VM,last_indiv, last_indiv(VM.objs,set(VM.objs))).
last_indiv(I,R):- into_group(I,M),I\=@=M,!,predsort_on(loc_term,M,O),reverse(O,R).



% =====================================================================
is_fti_step(overlay_original).
% =====================================================================

overlay_original(VM):-
  mapgrid(overlay_onto,VM.grid_o,VM.grid,set(VM.grid)).


overlay_onto(FG,_,FG):- is_fg_color(FG),!.
overlay_onto(_,Else,Else).

% =====================================================================
is_fti_step(add_object).
% =====================================================================
add_object(Spec,VM):- 
  ensure_objects(VM),
  UParentVM = VM.parent_vm,
  (var(UParentVM) -> ParentVM = VM ; ParentVM = UParentVM),
  include(has_prop(Spec),VM.objs,Matches),
  Unsullied = Matches,%maplist(remove_giz,Matches,Unsullied),
  maplist(addNonVMObject(ParentVM),Unsullied).


addNonVMObject(VM,Obj):-
  localpoints_include_bg(Obj,Points),
  make_indiv_object(VM,[],Points,NewObj),
  global_grid(Obj,GGrid),mapgrid(overlay_onto,GGrid,VM.grid,set(VM.grid)),
  indv_props(Obj,PrevProps),
  indv_props(NewObj,BetterProps),
  override_object(PrevProps,NewObj,NewObj2),
  override_object(BetterProps,NewObj2,NewObj3),
  maybe_replace_object(VM,NewObj,NewObj3).


% =====================================================================
is_fti_step(with_objects).
% =====================================================================
with_objects(Spec,Code,VM):-
  ensure_objects(VM),
  include(has_prop(Spec),VM.objs,Matches),
  maplist(run_code_on_object(VM,Code),Matches).

run_code_on_object(VM,Code,Obj):- 
  ensure_objects(VM),
  (object_call(Code,Obj,NewObj)->maybe_replace_object(VM,Obj,NewObj);true).
/*
  object_grid(Obj,Grid),
  into_fti(_ID,Code,Grid,NewVM),
  set(NewVM.parent_vm) = VM,
  run_fti(NewVM).
  */


test_tag_info(X):- 
 (var(X)->test_tag(X);true),
 findall(TestID,(test_info(TestID,Info),sub_var(X,Info)),Tests),
  list_to_set(Tests,Set),
  length(Set,LS),
  dash_chars,
  wdmsg(?- test_tag_info(X)=LS),
  maplist(test_tag_info(X),Set),
  wdmsg(test_tag_info(X)=LS),
  dash_chars.

test_tag_info(_X,TestID):- print_qtest(TestID),!.

test_tag:- forall(test_tag(X),test_tag_info(X)).

 test_tag(adapt_image_to_grid).
 test_tag(algebra).
 test_tag(associate_color_to_bools).
 test_tag(associate_colors_to_bools).
 test_tag(associate_colors_to_colors).
 test_tag(associate_colors_to_images).
 test_tag(associate_colors_to_numbers).
 test_tag(associate_colors_to_patterns).
 test_tag(associate_colors_to_ranks).
 test_tag(associate_colors_to_shapes).
 test_tag(associate_images_to_bools).
 test_tag(associate_images_to_colors).
 test_tag(associate_images_to_images).
 test_tag(associate_images_to_numbers).
 test_tag(associate_images_to_patterns).

 test_tag(associate_patterns_to_colors).
 test_tag(associate_patterns_to_patterns).

 test_tag(background_filling).
 test_tag(bouncing).
 test_tag(bouncing).
 test_tag(bridges).
 test_tag(bring_patterns_close).
 test_tag(color_guessing).
 test_tag(color_matching).
 test_tag(color_palette).
 test_tag(color_permutation).
 test_tag(compare_image).
 test_tag(concentric).
 test_tag(connect_the_dots).
 test_tag(contouring).
 test_tag(count_different_colors).
 test_tag(count_hor_lines).
 test_tag(count_patterns).
 test_tag(count_shapes).
 test_tag(count_tiles).
 test_tag(count_ver_lines).
 test_tag(create_grid).
 test_tag(create_image_from_info).
 test_tag(crop).
 test_tag(cylindrical).
 test_tag(detect_background_color).
 test_tag(detect_closed_curves).
 test_tag(detect_connectedness).
 test_tag(detect_enclosure).
 test_tag(detect_grid).
 test_tag(detect_hor_lines).
 test_tag(detect_repetition).
 test_tag(detect_symmetry).
 test_tag(detect_wall).
 test_tag(diagonal_symmetry).
 test_tag(diagonals).
 test_tag(direction_guessing).
 test_tag(direction_guessing).
 test_tag(divide_by_n).
 test_tag(dominant_color).
 test_tag(draw_line_from_border).
 test_tag(draw_line_from_point).
 test_tag(draw_parallel_line).
 test_tag(draw_pattern_from_point).
 test_tag(draw_rectangle).
 test_tag(enlarge_image).
 test_tag(even_or_odd).
 test_tag(ex_nihilo).
 test_tag(extrapolate_image_from_grid).
 test_tag(find_the_intruder).
 test_tag(fractal_repetition).
 test_tag(gravity).
 test_tag(grid_coloring).
 test_tag(holes).
 test_tag(homeomorphism).
 test_tag(image_expansion).
 test_tag(image_expansion).
 test_tag(image_filling).
 test_tag(image_juxtaposition).
 test_tag(image_reflection).
 test_tag(image_repetition).
 test_tag(image_resizing).
 test_tag(image_rotation).
 test_tag(image_within_image).
 test_tag(inside_out).
 test_tag(jigsaw).
 test_tag(loop_filling).
 test_tag(maze).
 test_tag(measure_area).
 test_tag(measure_distance_from_side).
 test_tag(measure_length).
 test_tag(mimic_pattern).
 test_tag(obstacles).
 test_tag(one_yes_one_no).
 test_tag(only_one).
 test_tag(order_numbers).
 test_tag(out_of_boundary).
 test_tag(pairwise_analogy).
 test_tag(pattern_alignment).
 test_tag(pattern_coloring).
 test_tag(pattern_completion).
 test_tag(pattern_deconstruction).
 test_tag(pattern_differences).
 test_tag(pattern_expansion).
 test_tag(pattern_intersection).
 test_tag(pattern_juxtaposition).
 test_tag(pattern_modification).
 test_tag(pattern_moving).
 test_tag(pattern_reflection).
 test_tag(pattern_repetition).
 test_tag(pattern_resizing).
 test_tag(pattern_rotation).
 test_tag(portals).
 test_tag(projection_unto_rectangle).
 test_tag(proximity_guessing).
 %test_tag(recolor).
 test_tag(recoloring).
 test_tag(rectangle_guessing).
 %test_tag(remove_intruders).
 test_tag(remove_intruders).
 test_tag(remove_noise).
 test_tag(replace_pattern).
 test_tag(rectangle_guessing).
 test_tag(separate_image).
 test_tag(separate_images).
 test_tag(separate_shapes).
 test_tag(shape_guessing).
 test_tag(size_guessing).
 test_tag(spacing).
 test_tag(summarize).
 test_tag(take_complement).
 test_tag(take_half).
 test_tag(take_intersection).
 test_tag(take_maximum).
 test_tag(take_minimum).
 test_tag(take_negative).
 test_tag(x_marks_the_spot).


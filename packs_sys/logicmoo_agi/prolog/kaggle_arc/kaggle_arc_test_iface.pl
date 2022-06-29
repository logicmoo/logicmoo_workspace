/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


menu :- write_menu('i').
write_menu(Mode):-
  format('~N'),
  get_current_test(TestID),
  format(' Current Test: ~q ~n~n',[TestID]),
  forall(menu_cmd(Mode,Key,Info,Goal),print_menu_cmd(Key,Info,Goal)),format('~N  '),
  forall(menu_cmd1(Mode,Key,Info,Goal),print_menu_cmd1(Key,Info,Goal)),format('~N').
print_menu_cmd(Key):- ignore((menu_cmd(_,Key,Info,Goal),print_menu_cmd(Key,Info,Goal))).
print_menu_cmd(Key,Info,_Goal):- format('~N   ~w: ~w \t\t ~n',[Key,Info]).
print_menu_cmd1(_Key,Info,_Goal):- format(' ~w',[Info]).

:- multifile(menu_cmd/4).
:- multifile(menu_cmd1/4).
menu_cmd(i,'i','Examine (i)ndividuator,',(cls,!,ndividuator)).
menu_cmd(_,'p','     or (p)rint training pairs (captial to reveal Solutions)',(print_test)).
menu_cmd(_,'t','See the (t)raining happen on this (t)est,',(cls,!,train_test)).
menu_cmd(_,'s','     or (s)olve the problem as learned.',(cls,print_test,!,solve_test)).
menu_cmd(_,'h','     or (h)uman proposed solution.',human_test).
menu_cmd(_,'r','  Maybe (r)un All of the above: Print, Train, and Solve.',(fully_test)).
menu_cmd(_,'a','     or (a)dvance to the next test and run',(cls,!,run_next_test)).
menu_cmd(_,'n','  Go to (n)ext test',(next_test)).
menu_cmd(_,'b','     or (b)ack to previous.',(previous_test)).
menu_cmd(_,'S','     or  back to begining of (S)uite',(restart_suite)).
menu_cmd(_,'N','     or (N)ext Suite',(next_suite)).
menu_cmd(_,'L','     or (L)ist feature tests and run',(run_a_test)).
 
menu_cmd(i,'R','(R)un the Suite noninteractively',(run_all_tests,menu)).
menu_cmd(r,'i','Re-enter(i)nteractve mode.',(interactive_test_menu)).

menu_cmd1(_,'m','Recomple this progra(m),',(make,menu)).
menu_cmd1(_,'c','(c)lear the scrollback buffer,',(cls)).
menu_cmd1(_,'T','(T)est regressions,\n     ',(make,menu)).
menu_cmd1(_,'Q','(Q)uit Menu,',true).
menu_cmd1(_,'X','e(X)it to shell,',halt(4)). 
menu_cmd1(_,'B','or (B)reak to interpreter.',(break)).

menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd(Mode,Key,Mesg,Goal).
menu_cmds(Mode,Key,Mesg,Goal):-menu_cmd1(Mode,Key,Mesg,Goal).

find_tests(F):-
   current_predicate(N),N=F/0, atom_concat(test_,_,F),\+ ( atom_codes(F,Codes),member(C,Codes),char_type(C,digit)).
list_of_tests(L):- findall(F,find_tests(F),L).

show_tests:- make, list_of_tests(L),forall(nth0(N,L,E),format('~N~w: ~w  ',[N,E])),nl.

run_a_test:- 
   show_tests,
   write("\nYour selection: "), read_line_to_string(user_input,Sel),
   list_of_tests(L),
   ignore((atom_number(Sel,Num),nth0(Num,L,E),!,call(E))).

clsR:- !. % once(cls).

interact:- 
  repeat, format('~N Your selection: '), get_single_char(Code), 
  char_code(Key,Code),  put_char(Key), nb_setval(last_menu_key,Key), do_menu_key(Key),!.
do_menu_key('Q'):-!,format('~N returning to prolog.. to restart type ?- demo. ').
do_menu_key('P'):- !, do_menu_key('p').
do_menu_key(Key):- print_menu_cmd(Key),menu_cmds(_Mode,Key,_Info,Goal),!, format('~N~n'),
  dmsg(calling(Goal)),!, ignore(once((Goal*->true;(fail,trace,dumpST,rrtrace(Goal))))),!,read_pending_codes(user_input,_,[]),!,fail.
do_menu_key(Key):- format("~N % Menu: didn't understand: '~w'~n",[Key]),once(mmake),menu,fail.
interactive_test(X):- set_current_test(X), print_test(X), interactive_test_menu.
interactive_test_menu:- 
  repeat, 
   write_menu('i'), 
   interact,!.
run_all_tests:- 
  repeat,
   run_next_test,
   write_menu('r'),
   wait_for_input([user_input],F,2),
   F \== [], !,
   interact,!.

ndividuator:- get_current_test(TestID),with_test_grids(TestID,G,ig(complete,G)).
test_grids(TestID,G):- kaggle_arc_io(TestID,ExampleNum,IO,G), ((ExampleNum*IO) \= ((tst+_)*out)).
with_test_grids(TestID,G,P):- forall(test_grids(TestID,G),call(P)).

bad:- ig([complete],v(aa4ec2a5)*(trn+0)*in).


restart_suite:- 
   get_current_suite_testnames([First|_]),
   set_current_test(First),!.
next_suite:- 
   findall(SN,test_suite_name(SN),List),
   nb_current(test_order,X),
   next_in_list(X,List,N),
   nb_setval(test_order,N),!,
   wdmsg(switched(X-->N)),
   restart_suite.

test_suite_name(key_pad_tests).
test_suite_name(test_names_by_fav). test_suite_name(test_names_by_hard). 
test_suite_name(test_names_by_fav_rev). test_suite_name(test_names_by_hard_rev).

:- nb_setval(test_order,test_names_by_hard).
get_current_suite_testnames(Set):-
  nb_current(test_order,X),
  findall(ID,call(X,ID),List),
  my_list_to_set_variant(List,Set),!.

previous_test:-  get_current_test(TestID), get_previous_test(TestID,NextID), set_current_test(NextID),print_qtest(NextID).
next_test:- get_current_test(TestID), notrace((get_next_test(TestID,NextID), set_current_test(NextID),print_qtest(NextID))),!.
is_valid_testname(TestID):- kaggle_arc(TestID,_,_,_).
get_current_test(TestID):- nb_current(test_name,TestID),is_valid_testname(TestID),!.
get_current_test(TestID):- get_next_test(TestID,_),!.
%get_current_test(v(fe9372f3)).
get_next_test(TestID,NextID):- get_current_suite_testnames(List), next_in_list(TestID,List,NextID).
get_previous_test(TestID,PrevID):-  get_current_suite_testnames(List), prev_in_list(TestID,List,PrevID).
next_in_list(TestID,List,Next):- append(_,[TestID,Next|_],List)-> true; List=[Next|_].
prev_in_list(TestID,List,PrevID):-  once(append(_,[PrevID,TestID|_],List); last(List,PrevID)).

%v(f9d67f8b)
load_last_test_name:- 
  ignore(notrace((setup_call_cleanup(open(current_test,read,O),ignore((read_term(O,TestID,[]),nb_setval(test_name,TestID))),close(O))))).
save_last_test_name:- 
  ignore(notrace((nb_current(test_name,TestID), tell(current_test),format('~n~q.~n',[TestID]),told))).

set_current_test(Name):-  
  ignore((fix_test_name(Name,TestID,_),is_valid_testname(TestID),really_set_current_test(TestID))).

really_set_current_test(TestID):-
   nb_setval(test_name,TestID),
  (nb_current(last_test_name,WasTestID);WasTestID=[]),
  (WasTestID==TestID-> true ; new_current_test_info).

new_current_test_info:- 
  ignore((
  nb_current(test_name,TestID),
  dmsg(fav(TestID,[])),
  nb_setval(last_test_name,TestID))),
  save_last_test_name,
  set_bgc(_),
  set_flag(indiv,0),
  nb_delete(grid_bgc),
  nb_linkval(test_rules, [rules]),
  wots(_,(clear_shape_lib(test), clear_shape_lib(noise), retractall(grid_nums(_,_)), retractall(grid_nums(_)))),
  nop(retractall(g2o(_,_))),!.

new_test_pair(PairName):-
  %nb_delete(grid_bgc),
  clear_shape_lib(pair),clear_shape_lib(in),clear_shape_lib(out),
  nb_setval(test_pairname,PairName),
  nb_linkval(pair_rules, [rules]),
  retractall(is_shared_saved(PairName*_,_)),
  retractall(is_shared_saved(PairName,_)),
  retractall(is_unshared_saved(PairName*_,_)),
  retractall(is_unshared_saved(PairName,_)),
  retractall(is_grid_id(PairName*_,_)),
  retractall(is_grid_id(PairName,_)),!.

human_test:- solve_test.
fully_test:- print_test, !, train_test, !, solve_test, !.
run_next_test:- notrace(next_test), fully_test.

info(_).
demo:- make, interactive_test_menu.
rat:- info("Run all tests"), run_all_tests.
noninteractive_test(X):- time(ignore(forall(arc1(true,X),true))).



cmt_border:- format('~N% '), dash_chars(120,"="), !, nl.

test_id_border(TestID):-
    get_current_test(WasTestID),
    ignore((WasTestID\==TestID,set_current_test(TestID), cmt_border)).
print_test:- get_current_test(TestID),print_test(TestID).
print_test(TName):- 
  notrace((once(fix_test_name(TName,TestID,_)),
  cmt_border,format('% ?- ~q. ~n',[print_test(TName)]),cmt_border,
  ignore(print_test_hints(TName)),
  parcCmt(TName),nl,
  dash_chars,
  print_test4(TestID))),!.

print_test4(TestID):-
    forall(arg(_,v((trn+_)),ExampleNum1),
     forall(kaggle_arc(TestID,ExampleNum1,In,Out),
      ignore((
       once(in_out_name(ExampleNum1,NameIn,_NameOut)),
       format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum1,print_side_by_side4(red,In,NameIn,_,Out,' ')]))))),
       write('%= '), parcCmt(TestID),
  dash_chars,
    forall(arg(_,v((tst+_)),ExampleNum2),
     forall(kaggle_arc(TestID,ExampleNum2,In,Out),
      ignore((
       once(in_out_name(ExampleNum2,NameIn,NameOut)),
       grid_size(Out,OH,OV),make_grid(OH,OV,Blank),
       (nb_current(last_menu_key,'P')
         -> format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum2,print_side_by_side4(red,In,NameIn,_,Out,NameOut)])
         ; format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum2,print_side_by_side4(red,In,NameIn,_,Blank,"Hidden Output")])))))),!.

%print_test(TName):- !, parcCmt(TName).
print_qtest:- get_current_test(TestID),print_qtest(TestID).
print_qtest(TestID):-
    dash_chars,nl,nl,nl,dash_chars,
    forall(arg(_,v((trn+_)),ExampleNum),
     forall(kaggle_arc(TestID,ExampleNum,In,Out),
      ignore((
       once(in_out_name(ExampleNum,NameIn,NameOut)),
       format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum,print_side_by_side4(red,In,NameIn,_LW,Out,NameOut+TestID)]))))),
       write('%= '), parcCmt(TestID).

print_single_test(TName):-
  fix_test_name(TName,TestID,ExampleNum),
  kaggle_arc(TestID,ExampleNum,In,Out),
  nb_current(test_name,WasTestID),
  once(in_out_name(ExampleNum,NameIn,NameOut)),
  format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum,print_side_by_side4(red,In,NameIn,_LW,Out,NameOut)]),
  ignore((WasTestID\==TestID, write('%= '), parcCmt(TName), nl)).

in_out_name(trn+NN,SI,SO):- N is NN+1, format(atom(SI),'Training Pair #~w Input',[N]),format(atom(SO),'Output',[]).
in_out_name(tst+NN,SI,SO):- N is NN+1, format(atom(SI),'EVALUATION TEST #~w',[N]),format(atom(SO),'Output<(REVEALED)>',[]).
in_out_name(X,'Input'(X),'Output'(X)).



arc_test_name(TestID):- 
  findall(TestID,kaggle_arc(TestID,_,_,_),All),
  list_to_set(All,AllS),member(TestID,AllS).
test_info(TestID,InfoS):- fix_test_name(TestID,CTestID,_),
 findall([Inf],(user:fav(CTestID,Inf0),repair_info(Inf0,Inf)),Info),
  flatten(Info,InfoF),list_to_set(InfoF,InfoS).

repair_info(Inf0,Inf):- is_list(Inf0),!,maplist(repair_info,Inf0,Inf).
repair_info(Inf,InfO):- compound(Inf),functor(Inf,F,1),!,arg(1,Inf,A),listify(A,ArgsL),InfO=..[F,ArgsL].
repair_info(Inf,InfO):- compound(Inf),!,compound_name_arguments(Inf,F,ArgsL),InfO=..[F,ArgsL].
repair_info(Inf,Inf).

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
test_names_ord_favs(FavListS):- pt(recreating(test_names_ord_favs)), findall(Name,fav(Name),FavList),list_to_set(FavList,FavListS),  asserta(ord_favs(FavListS)).

:- dynamic(ord_hard/1).
test_names_ord_hard(NamesByHard):- ord_hard(NamesByHard),!.
test_names_ord_hard(NamesByHard):- pt(recreating(test_names_ord_hard)),findall(Hard-Name,(arc_test_name(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),  maplist(arg(2),AllK,NamesByHardU),!,
  list_to_set(NamesByHardU,NamesByHard), 
  asserta(ord_hard(NamesByHard)).

%:- use_module(library(pfc_lib)).
:- retractall(ord_favs(_)),retractall(ord_hard(_)).

ascending_hard:-
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
  append(Vs,[shared=[], refused=[], patterns=[], added=[], removed=[]],Vs0),
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
  pt(hard=Hard),
  %make_training_hints(TestID,print_test{},DictOut),
  %pt(all=DictOut),
  writeln('*/').


precat_name(P,N=V,NN=V):- atom_concat(P,N,NN).
  

:- dynamic(cached_dictation/2).
:- retractall(cached_dictation(_,_)).
pair_dictation(TestID,ExampleNum,In,Out,DictOut):- cached_dictation(pair_dictation(TestID,ExampleNum,In,Out),DictOut),!.
pair_dictation(TestID,ExampleNum,In,Out,DictOut):-
  do_pair_dication(In,Out,Vs),!,
  vars_to_dictation(Vs,_{},DictOut),
  assert(cached_dictation(pair_dictation(TestID,ExampleNum,In,Out),DictOut)).
/*
The IEEE floating-point standard, supported by almost all modern floating-point units, specifies that every floating 
 point arithmetic operation, including division by zero, has a well-defined result. 
  The standard supports signed zero, as well as infinity and NaN (not a number). 
   There are two zeroes: +0 (positive zero) and -0 (negative zero) and this removes any ambiguity when dividing. 
   In IEEE 754 arithmetic, a ÷ +0 is positive infinity when a is positive, negative infinity when a is negative, 
   and NaN when a = ±0. The infinity signs change when dividing by -0 instead.
*/
ratio_for(Ratio,_/_=Out,In):- nonvar(Out), !, ratio_for(Ratio,Out,In).
ratio_for(Ratio,Out,_/_=In):- nonvar(In), !, ratio_for(Ratio,Out,In).
ratio_for(Out/In=Ratio,Out,In):- ratio_for0(Ratio,Out,In).
ratio_for0(1.0,Out,In):- 0 is In, 0 is Out,!.
ratio_for0(Ratio,Out,In):- 0 is In, !, Ratio is -0.0.
ratio_for0(1,Out,In):- Out =:= In.
ratio_for0(Ratio,Out,In):- 0 is Out, !, Ratio is +0.0.
ratio_for0(Ratio,Out,In):- catch(Ratio is rationalize(Out/In),error(evaluation_error(_zero_divisor),_),fail),!.
ratio_for0(Ratio,Out,In):- catch(NRatio is rationalize(In/Out),error(evaluation_error(_zero_divisor),_),fail),!, Ratio is -NRatio.

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

:- dynamic(fav/2).

macro(one_obj, must(len(objs)=1)).

test_p2(P2):- clsmake,
  append_termlist(P2,[N1,'$VAR'('Result')],N2), 
  time(forall(into_gridoid(N1,G1),     
     forall((set_current_test(G1),call(P2,G1,G2)),
       once(ignore((print_side_by_side4(red,G1,N1,_LW,G2,?-N2),dash_chars)))))).

%:- style_check(-singleton).
whole(I,O):- is_group(I),length(I,1),I=O,!.
whole(I,O):- print_grid(I),pt(I),into_obj(I,O).
one_obj(I,I):- is_group(I),length(I,1),!.
one_obj(I,I):- is_group(I),!.
one_obj(I,I).
uncolorize(I,O):- set_all_fg_colors(grey,I,O).
resize_grid(_H,_V,List,_,List):- is_list(List).
resize_grid(H,V,Color,_,NewGrid):- make_grid(H,V,Grid),replace_grid_point(1,1,Color,_,Grid,NewGrid),nop(set_bgc(Color)).

resize_grid(H,V,_,NewGrid):- make_grid(H,V,NewGrid).

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

%db(P1,P2,In,In):- t_or_t(freeze_for([P2],assert(is_db(TF,P2))),is_db(TF,P2)).
%db(P2,P1,In,In):- nonvar(Color), db_value(P1,In,TF),!,t_or_t(freeze_for([Color],assert(is_db(TF,Color))),is_db(TF,Color)).
db(P1,P2,In,Out):- db_u([],P1,[],P2,In,Out).
db(X,Y,I,I):- pt(db(X,Y)),pt(I).

copy_grid(In,G,G):- In == in,!.
copy_grid(Name,_,G):- get_training(VM), G=VM.Name .


/*  

%fav(v('20818e16'),[guess_bg,indiv(min(2))]).

%(fav(_,P)/(flatten([P],Flat),member(E,Flat))) ==> fav_trait(E).

*/
:- dynamic(muarc:kaggle_arc_json/4).


load_json_files(F,Mask):- 
  arc_sub_path('.',ARC_DIR),
  absolute_file_name(Mask,AbsMask,[relative_to(ARC_DIR)]),
  expand_file_name(AbsMask,FullNames),
  maplist(file_base_name,FullNames,BaseNames),
  maplist(file_name_extension,Names,_,BaseNames),
  maplist(load_json_file(F),Names,FullNames),!.

load_json_file(F, BaseName, FullName):- Testname=..[F,BaseName], 
  % dmsg(load_json_file=FullName),
  setup_call_cleanup(open(FullName,read,In),
   json_read(In,Term,[]),
   close(In)),
  load_json_of_file(Testname,file,Term),!.

  load_json_of_file(Name,Type,json(Term)):-! , load_json_of_file(Name,Type,Term).
  load_json_of_file(Name,_,Type=Value):-!, load_json_of_file(Name,Type,Value).
  load_json_of_file(Name,train,T):-!,load_json_of_file(Name,trn,T).
  load_json_of_file(Name,test,T):-!,load_json_of_file(Name,tst,T).
    load_json_of_file(Testname,ExampleNum,[input=In,output=Out]):-
       json_to_colors(In,InColor),
       json_to_colors(Out,OutColor),
       assert_if_new(muarc:kaggle_arc_json(Testname,ExampleNum,InColor,OutColor)),!.
  load_json_of_file(Name,Type,[input=In,output=Out]):-assert_if_new(muarc:kaggle_arc_json(Name,Type,In,Out)),!.
  load_json_of_file(Name,Type,[H|T]):- !, forall(nth0(N,[H|T],E), load_json_of_file(Name,Type+N,E)).
  load_json_of_file(N,T,V):- wdmsg(load_json_of_file(N,T,V)),!.


json_to_colors(Out,Color):- is_grid_color(Out),!,Out=Color.
json_to_colors(Out,Color):- is_list(Out),!,maplist(json_to_colors,Out,Color).
json_to_colors(Out,Color):- grid_color_code(Out,Color).


:- dynamic(muarc_tmp:arc_directory/1).
muarc_tmp:arc_directory(ARC_DIR):- getenv('ARC_DIR',ARC_DIR), exists_directory(ARC_DIR),!.

:- multifile (user:file_search_path/2).
user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

:- prolog_load_context(directory,ARC_DIR), asserta(muarc_tmp:arc_directory(ARC_DIR)).

absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  catch(absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(directory),solutions(first),file_errors(error),access(read)]),_,fail),!.
absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(regular),solutions(first),file_errors(error),access(read)]).

arc_sub_path(Subdir,AbsolutePath):- muarc_tmp:arc_directory(ARC_DIR),absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath),!.

:- export(arc_sub_path/2).

:- load_json_files(t,'./data/training/*.json').
:- load_json_files(v,'./data/evaluation/*.json').
%:- load_json_files(v,'./data/test/*.json').
kaggle_arc(TName,ExampleNum,In,Out):- muarc:kaggle_arc_json(TName,ExampleNum,In,Out).
% ExampleNum is tst or trn
/*
kaggle_arc(t(Name), TypeI, In, Out):- 
 nth_fact(kaggle_arc_train(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_train(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum->I)).
kaggle_arc(v(Name), TypeI, In, Out):- 
 member(ExampleNum, [trn, tst]), nth_fact(kaggle_arc_eval(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_eval(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum->I)).
*/

fix_test_name(V,VV,_):- var(V),!,VV=V.
fix_test_name(G,T,E):- is_grid(G),!, kaggle_arc_io(T,E,_,GO),GO=E.

fix_test_name(Tried*ExampleNum*_,Fixed,ExampleNum):- !, fix_id(Tried,Fixed).
fix_test_name(Tried*ExampleNum,  Fixed,ExampleNum):- !, fix_id(Tried,Fixed).
fix_test_name(Tried           ,  Fixed,         _):-    fix_id(Tried,Fixed).


fix_id(Tried,   Tried):- var(Tried),!.
fix_id(Tried,   Tried):- kaggle_arc(Tried,_,_,_),!.
fix_id(Tried,t(Tried)):- kaggle_arc(t(Tried),_,_,_),!.
fix_id(Tried,v(Tried)):- kaggle_arc(v(Tried),_,_,_),!.
fix_id(t(Tried),v(Tried)):- kaggle_arc(v(Tried),_,_,_),!.
fix_id(v(Tried),t(Tried)):- kaggle_arc(t(Tried),_,_,_),!.
fix_id(Tried,Fixed):- compound(Tried),!,arg(_,Tried,E),nonvar_or_ci(E),fix_id(E,Fixed),!.




print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).



parc1:- parc1(6300*3). 
parc1(OS):- clsmake,   nb_setval(test_name,[]),
   open(tt,write,O,[encoding(text)]), parc0(OS), with_output_to(O,parc0(OS)), close(O).
parc0(OS):-
 locally(set_prolog_flag(gc,true),forall(parc11(OS,_),true)).

parcCmt(TName):-
  ignore((
  fix_test_name(TName,TestID,_),
  kaggle_arc(TestID,(trn+0),In,Out),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  IHV = IH*IV, OHV = OH*OV,
  BGColor = '$VAR'('Color'),
  (IHV\==OHV -> CG = resize_grid(OH,OV,BGColor); CG = copy_grid(in)),
  findall(III,gather_more_task_info(TestID,III),InfoUF),
  flatten(InfoUF,InfoF),
  DSL = sol(i(complete),CG,incomplete),
  predsort(sort_univ,[DSL|InfoF],InfoFS), %44f52bb0
  reverse(InfoFS,InfoSR),
  P = fav(TestID,InfoSR),
  format('~q.~n',[P]))).




parc11(_OS,_):-
  forall(test_names_by_hard(TName),    print_test(TName)).


parc11(OS,TName):- fail,    
  fix_test_name(TName,TestID,ExampleNum),   
  kaggle_arc(TestID,ExampleNum,In,Out),
  maplist(color_sym(OS),In,I),
  grid_size(In,IH,IV),
  grid_size(Out,_OH,OV),
  %V is OV-IV,
  H is IH,
  maplist(color_sym(OS),Out,O),
  format('~Ntestcase(~q,"\n~@").~n',[TestID*ExampleNum,
    print_side_by_side(call((print_grid(IH,IV,I),
      write(' '),forall(between(IV,OV,_),(write('\n '),dash_chars(H,'  '))))),call(print_grid(O)))]).

%color_sym(OS,[(black='°'),(blue='©'),(red='®'),(green=''),(yellow),(silver='O'),(purple),(orange='o'),(cyan= 248	ø ),(brown)]).
color_sym(OS,C,Sym):- is_list(C),maplist(color_sym(OS),C,Sym),!.
color_sym(_,black,' ').
color_sym(OS,C,Sym):- color_sym(OS,4,C,Sym).
color_sym(_,_,C,Sym):- enum_colors(C),color_int(C,I),nth1(I,`ose=xt~+*zk>`,S),name(Sym,[S]).
%color_sym(P*T,_,C,Sym):- enum_colors(C),color_int(C,I),S is P+I*T,name(Sym,[S]).

:- fixup_exports.

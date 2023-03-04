/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
%+++++++++++   95% grid size prediction accuracy

:- include(kaggle_arc_header).

:- dynamic(muarc_tmp:learned_grid_size/2).
:- dynamic(muarc_tmp:grid_size_prediction/4).


test_grid_size_prediction:- forall_count(all_arc_test_name(TestID), test_grid_sizes(TestID)). 
store_grid_size_predictions:- forall_count(all_arc_test_name(TestID), test_grid_sizes(TestID)). 

test_grid_sizes(TestID):- 
   retractall(muarc_tmp:learned_grid_size(TestID,_)),
   retractall(muarc_tmp:grid_size_prediction(TestID,_,_,_)),
   findall(R,(kaggle_arc(TestID,(trn+_),In,Out),learn_grid_size(In,Out,R),nop((writeq(R),write('.\n')))),L), 
   asserta_if_new(muarc_tmp:learned_grid_size(TestID,L)),
   forall(kaggle_arc(TestID,tst+_,In,Out),predict_grid_size(TestID,In,Out)).

learn_grid_size(In,Out,R):- 
  grid_size(In,IH,IV),grid_size(Out,OH,OV),
  locally(nb_setval(allow_unused_proportion,t),
    proportional(size2D(IH,IV),size2D(OH,OV),R)).
  %proportional_size2D(IH,IV,OH,OV,R).
   
predict_grid_size(TestID,In,Out):-    
   grid_size(In,IH,IV),grid_size(Out,OH,OV),
   muarc_tmp:learned_grid_size(TestID,List),
 %  predsort_on(better_grid_size_prop,List,SList), 
   wots(SS,((             
   dash_chars, dash_chars, write(test_grid_sizes(TestID)), write('\n'),   
   predict_grid_size(List,IH,IV,PH,PV),
   ((PH=OH,PV=OV) -> C = green ; C = red),
   color_print(C,predict_grid_size(TestID,in(size2D(IH,IV)),predicted(size2D(PH,PV)),actual(size2D(OH,OV))))))),!,
   (C==green 
     -> asserta(muarc_tmp:grid_size_prediction(TestID,In,PH,PV))
     ;(nop(print_test(TestID)),  write(SS),assert_test_suite(failed_predict_grid_size,TestID),!,fail)).


add_akeys(A,A-A).
alphabetize(List,ListA):- my_maplist(add_akeys,List,AKeys),keysort(AKeys,AKeysSorted),my_maplist(arg(2),AKeysSorted,ListA).


predict_grid_size(List,IH,IV,PH,PV):-
  alphabetize(List,ListA),
  predsort_on(better_grid_size_prop,ListA,SList),
  add_info(SList,[],NewInfo),
   %my_maplist(ppnl,List),dash_chars,
   my_maplist(ppnl,ListA),dash_chars,
   my_maplist(ppnl,NewInfo),dash_chars,
  predict_grid_size1(ListA,NewInfo,IH,IV,PH,PV).
predict_grid_size(_List,IH,IV,IH,IV).

better_grid_size_prop(_,1).

add_info([],List,List):-!.
add_info([E|More],List,NewInfo):-
  \+ \+ ((member(T,List), E=@=T)), !, 
  add_info(More,List,NewInfo).
add_info([E|More],List,NewInfo):-
  \+ ((member(T,List), min_unifier(E,T,Var),nonvar(Var))),!,
  add_info(More,[E|List],NewInfo).
add_info([E|More],List,NewInfo):-
  select(T,List,Rest), min_unifier(E,T,Var),nonvar(Var),!,
  add_info(More,[Var|Rest],NewInfo).
  
predict_grid_size1(_OList, List,IH,IV,PH,PV):- member(size2D(MH,MV),List), apply_proportional(IH,MH,PH,IV,MV,PV),!.
predict_grid_size1( ListA, _List, _, _,PH,PV):- findall(size2D(MH,MV),
  (member(size2D(o_i_swap(ratio_of(MV,G27)),o_i_swap(ratio_of(MH,G27))),ListA),number(MH)),L),
  L\=[],L\=[_],my_maplist(=(_),L),last(L,size2D(PH,PV)),!.


predict_grid_size1( ListA, _List, _, _,PH,PV):- 
  findall(size2D(MH,MV),
  (member((_->size2D(MH,MV)),ListA),number(MH)),L),
  L\=[],L\=[_],my_maplist(=(_),L),last(L,size2D(PH,PV)),!.

predict_grid_size1( ListA, _List, _, _,PH,PV):- findall(size2D(MH,MV),
  (member(size2D(MH,MV),ListA),number(MH)),L),
  L\=[],L\=[_],my_maplist(=(_),L),last(L,size2D(PH,PV)),!.

predict_grid_size1( ListA, _List, IH, IV,PH,PV):-  
 findall(size2D(PH,PV),
  (member((size_inv(num(_,+_,_),num(_,+_,ratio(Two)))
  ),ListA),
  integer(Two),PH = IH, PV is IV / Two, PV is floor(PV)),L),
  L\=[],L\=[_],my_maplist(=(_),L),last(L,size2D(PH,PV)),!.

apply_proportional(IH,MH,PH,IV,MV,PV):- 
  %functor(MH,F,A), functor(MV,F,A),
  apply_proportional(IH,MH,PH), apply_proportional(IV,MV,PV).
%apply_proportional(IH,o_i_swap(MH),PH,IV,o_i_swap(MV),PV):- apply_proportional(IH,MV,PH,IV,MH,PV).

apply_proportional(IH,ratio_of(IH,OH),OH).
apply_proportional(IH,o_i_swap(ratio_of(OH,IH)),OH).
%apply_proportional(IH,num(_,_,ratio(Rat)),OH):- nonvar(Rat), OH is IH * Rat, OH is floor(OH).


% 2-D Proportionals

proportional_width(W1,W2,How):- W2=@=W1,!, How = equal(W1).
proportional_width(W1,W2,How):- W1>W2, !, proportional_width(W2,W1,IHow), How = o_i_swap(IHow).
proportional_width(W1,W2,How):- pw5(W1,W2,0,0,N1,N2),How=ratio_of(N1,N2).
proportional_width(W1,W2,How):- pw5(W1,W2,2,0,N1,N2),How=bordered_inner(N1,N2).
proportional_width(W1,W2,How):- pw5(W1,W2,-1,1,N1,N2),How=ttt_outter(N1,N2).
proportional_width(W1,W2,How):- pw5(W1,W2,1,1,N1,N2),How=gridded_outter(N1,N2).
proportional_width(W1,W2,How):- pw5(W1,W2,2,0,N1,N2),How=bordered_outter(N1,N2).
pw5(W1,W2,Upper,Lower,N1,N2):- N2 is (W2-Upper), N1 is (W1+Lower), 0 is N2 rem N1.


proportional_size2D(H1,V1,H2,V2,WAS):- WAS = ((size2D(H1,V1) -> size2D(H2,V2))).
proportional_size2D(H1,V1,H2,V2,size2D(How1,How2)):- proportional_width(H1,H2,How1),compound(How1), functor(How1,F,_),
                                                     proportional_width(V1,V2,How2),functor(How2,F,_),!.
proportional_size2D(H1,V1,H2,V2,size(H,V)):- proportional_size(H1,H2,H),proportional_size(V1,V2,V).
proportional_size2D(V1,H1,H2,V2,size_inv(H,V)):- proportional_size(H1,H2,H),proportional_size(V1,V2,V).

proportional_size2D(H1,V1,H2,V2,area(HV)):- !, HV1 is H1*V1, HV2 is H2*V2, proportional_size(HV1,HV2,HV).


proportional_size(M1,N2,P):- maybe_number(M1,N1),M1\==N1, !, proportional_size(N1,N2,P).
proportional_size(N1,M2,P):- maybe_number(M2,N2),M2\==N2, !, proportional_size(N1,N2,P).
proportional_size(N1,N2,P):- unused_proportion(N1,N2,P),!.

proportional_size(N1,N2,num(vals(Vals),+N,ratio(R))):- number(N1),number(N2),!,
  into_vals(N1,N2,Vals), N is N2-N1,
 (calc_ratio(R,N1,N2)-> true ; catch(R is rationalize(N1/N2),_,true)).









:- meta_predicate(with_other_grid(+,0)).
with_other_grid(OtherGrid,Goal):- 
  locally(nb_setval(other_grid,OtherGrid),
    (set_target_grid(OtherGrid),Goal)).

other_grid(_,OtherGrid):- luser_getval(other_grid,OtherGrid),is_grid(OtherGrid),!.
other_grid(_,OtherGrid):- peek_vm(VM), OtherGrid = VM.target_grid, is_grid(OtherGrid),!.
other_grid(Grid,OtherGrid):- is_other_grid(Grid,OtherGrid),!.
other_grid(Grid,OtherGrid):- \+ is_grid(Grid),!, into_grid(Grid,ThisGrid),  Grid\==ThisGrid,!,other_grid(ThisGrid,OtherGrid).
other_grid(In,OtherGrid):- get_current_test(TestID), muarc_tmp:grid_size_prediction(TestID,In,PH,PV), make_grid(PH,PV,OtherGrid).

:- dynamic(is_decl_other_grid/2).
ensure_other_grid(ThisGrid,OtherGrid):- is_other_grid(ThisGrid,OtherGrid),!.
ensure_other_grid(ThisGrid,OtherGrid):- asserta_if_new(is_decl_other_grid(ThisGrid,OtherGrid)).

is_other_grid(ThisGrid,OtherGrid):- is_decl_other_grid(ThisGrid,OtherGrid),!.
is_other_grid(ThisGrid,OtherGrid):- is_decl_other_grid(OtherGrid,ThisGrid),!.
is_other_grid(ThisGrid,OtherGrid):-
  once((kaggle_arc_io(TestID,ExampleNum,IO,ThisGrid), 
  in_to_out(IO,OI), ignore(ExampleNum \= tst+_), 
  kaggle_arc_io(TestID,ExampleNum,OI,OtherGrid))).

other_grid_size(_Grid,PH,PV):- luser_getval(other_grid_size,size2D(PH,PV)),!.
other_grid_size( Grid,PH,PV):- must_det_ll((other_grid(Grid,OtherGrid),grid_size(OtherGrid,PH,PV))).
other_grid_size(   In,PH,PV):- get_current_test(TestID), muarc_tmp:grid_size_prediction(TestID,In,PH,PV).


set_target_grid(ExpectedOut):-
    luser_setval(other_grid,ExpectedOut),
    grid_size(ExpectedOut,GOH,GOV),
    luser_setval(other_grid_size,size2D(GOH,GOV)).

with_current_pair(I,O,Goal):-
  (luser_getval(input_grid,CI)->current_pair(CI,CO);true),
  ((I=@=CI,O=@=CO) -> call(Goal) ;
  setup_call_cleanup(set_current_pair(I,O),Goal,
   set_current_pair(CI,CO))).

  

set_current_pair(I,O):- 
  luser_setval(input_grid,I),
  luser_setval(output_grid,O),ensure_other_grid(I,O),set_target_grid(O),
  must_det_ll((other_grid(I,OO),OO==O)).

current_pair(I,O):- current_pair0(II,OO),II=I,OO=O.
current_pair0(I,O):- luser_getval(input_grid,I), is_gridoid(I),!, must_det_ll((other_grid(I,O))).
current_pair0(I,O):- current_test_example(TestID,ExampleNum),
   setup_call_cleanup(true,
     ((kaggle_arc(TestID,ExampleNum,I,O);fail),set_current_pair(I,O)), luser_setval(input_grid,[])).

% test_hint(ratio_between(mass,mass)). %ac0a08a4
increase_size_by_grid_mass(In,Out):- mass(In,Mass),increase_size(Mass,In,Out).
% test_hint(ratio_between(unique_color_count,mass)). %ac0a08a4
increase_size_by_color_count(In,Out):- fg_color_count(In,Size),increase_size(Size,In,Out).



ratio_between(Unique_color_count,and(Mass,Area)):- !, current_pair(I,O),
  call(Unique_color_count,I,UCC), ratio_about(Mass,UCC,I,O), ratio_about(Area,UCC,I,O).
ratio_between(Mass1,Mass2):- current_pair(I,O), call(Mass1,I,UCC), ratio_about(Mass2,UCC,I,O).

ratio_about(square(Area), UCC,I,O):- !, call(Area,I,IA), call(Area,O,OA), n_times(UCC^2,IA,OA).
ratio_about(Mass, UCC,I,O):-  call(Mass,I,IM), call(Mass,O,OM), n_times(UCC,IM,OM).

test_hint(How,P2):- must_det_ll((current_pair(I,O),call(P2,I,II),call(P2,O,OO))),call(How,II,OO).
test_hint(G):- current_predicate(_,G),!,call(G).

mass_and_area(P2Mass,P2Area):- test_hint(P2Mass,mass),test_hint(P2Area,area).
mass_and_area_times(N):- mass_and_area(n_times(N),n_times(N)).

:- use_module(library(clpfd)).
% each blur effect.. 
% 6 -> 7 - 12
%   -> 8 - 24
%   -> 9 - 48
%   -> 10 - 96

input_plus(N,X,Y):- Y #= X + N.
input_lt(X,Y):- Y #> X.
input_gt(X,Y):- Y #< X.

is_squared(X,Y):- Y #= X * X.
grow_less_than_times(N,A,B):- N #>= 1, N #=< 4, MaxB #= A*2^(N-1), MinB #= A+N,  MaxB #> B, B #> MinB.
grow_greater_than_times(N,A,B):- N #>= 1, N #=< 10, MaxB #= A*2^(N-1), MinB #= A+N,  MaxB #< B, B #< MinB.
n_times(N,A,B):- \+ compound(N),!, B #= N * A.
n_times(N^2,A,B):- !, B #= N * N * A.
%n_times(N,A,B):- B #= N * A.


:- include(kaggle_arc_footer).




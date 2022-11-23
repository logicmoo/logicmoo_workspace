/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
%+++++++++++   95% grid size prediction accuracy

:- include(kaggle_arc_header).

:- dynamic(learned_grid_size/2).

test_grid_size_prediction:- forall_count(kaggle_arc(TestID,trn+0,_,_), test_grid_sizes(TestID)). 
store_grid_size_predictions:- forall_count(kaggle_arc(TestID,trn+0,_,_), test_grid_sizes(TestID)). 

test_grid_sizes(TestID):- 
   retractall(learned_grid_size(TestID,_)),
   retractall(grid_size_prediction(TestID,_,_,_)),
   findall(R,(kaggle_arc(TestID,trn+_,In,Out),learn_grid_size(In,Out,R),nop((writeq(R),write('.\n')))),L), 
   asserta_if_new(learned_grid_size(TestID,L)),
   forall(kaggle_arc(TestID,tst+_,In,Out),predict_grid_size(TestID,In,Out)).

learn_grid_size(In,Out,R):- 
  grid_size(In,IH,IV),grid_size(Out,OH,OV),
  locally(nb_setval(allow_unused_proportion,t),
    proportional(size2D(IH,IV),size2D(OH,OV),R)).
  %proportional_size2D(IH,IV,OH,OV,R).
   
predict_grid_size(TestID,In,Out):-    
   grid_size(In,IH,IV),grid_size(Out,OH,OV),
   learned_grid_size(TestID,List),
 %  predsort(sort_on(better_grid_size_prop),List,SList), 
   wots(SS,((             
   dash_chars, dash_chars, write(test_grid_sizes(TestID)), write('\n'),   
   predict_grid_size(List,IH,IV,PH,PV),
   ((PH=OH,PV=OV) -> C = green ; C = red),
   color_print(C,predict_grid_size(TestID,in(size2D(IH,IV)),predicted(size2D(PH,PV)),actual(size2D(OH,OV))))))),!,
   (C==green 
     -> asserta(grid_size_prediction(TestID,In,PH,PV))
     ;(nop(print_test(TestID)),write(SS),!,fail)).

add_akeys(A,A-A).
alphabetize(List,ListA):- maplist(add_akeys,List,AKeys),keysort(AKeys,AKeysSorted),maplist(arg(2),AKeysSorted,ListA).


predict_grid_size(List,IH,IV,PH,PV):-
  alphabetize(List,ListA),
  predsort(sort_on(better_grid_size_prop),ListA,SList),
  add_info(SList,[],NewInfo),
   %maplist(wqnl,List),dash_chars,
   maplist(wqnl,ListA),dash_chars,
   maplist(wqnl,NewInfo),dash_chars,
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
  L\=[],L\=[_],maplist(=(_),L),last(L,size2D(PH,PV)),!.


predict_grid_size1( ListA, _List, _, _,PH,PV):- 
  findall(size2D(MH,MV),
  (member((_->size2D(MH,MV)),ListA),number(MH)),L),
  L\=[],L\=[_],maplist(=(_),L),last(L,size2D(PH,PV)),!.

predict_grid_size1( ListA, _List, _, _,PH,PV):- findall(size2D(MH,MV),
  (member(size2D(MH,MV),ListA),number(MH)),L),
  L\=[],L\=[_],maplist(=(_),L),last(L,size2D(PH,PV)),!.

predict_grid_size1( ListA, _List, IH, IV,PH,PV):-  
 findall(size2D(PH,PV),
  (member((
     size_inv(num(_,+_,_),num(_,+_,ratio(Two)))
  ),ListA),integer(Two),PH = IH, PV is IV / Two, PV is floor(PV)),L),
  L\=[],L\=[_],maplist(=(_),L),last(L,size2D(PH,PV)),!.

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

:- include(kaggle_arc_footer).


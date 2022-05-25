

learn_arc:-
  forall(learn_arc(_),true).

test_arc:-
  forall(test_arc(_),true).

learn_arc(TestID):- with_arc(learn,TestID).

test_arc(TestID):- with_arc(solve,TestID).

with_arc(Action,TestID):- var(TestID), !, with_arc(Action,fav).

with_arc(Action,fav):- !, findall(Name,fav(Name),L),
  list_to_set(L,S), member(TestID,S),with_arc(Action,TestID).

with_arc(Action,arc):- !, findall(Name,kaggle_arc_db(Name,_,_,_,_),L),
  list_to_set(L,S), member(TestID,S),with_arc(Action,TestID).

with_arc(Action,TestName):-
  into_gridname(TestName,TestID*_Test*_IO),TestName\==TestID,!,
  with_arc(Action,TestID).

with_arc(solve,TestID):- !, 
  with_arc(learn,TestID),
  with_arc(preview,TestID),
  forall(between(0,6,Num),with_pair(solve,TestID,tst,Num)).

with_arc(Action,TestID):-
  forall(between(0,6,Num),with_pair(Action,TestID,lrn,Num)).

with_pair(Action,TestID,Type,Num):-
  kaggle_arc_db(TestID,Type,Num,in,In),
  kaggle_arc_db(TestID,Type,Num,out,Out),
  with_pair(Action,TestID,Type,Num,In,Out),!.

with_pair(Action,TestID,Type,Num,In,Out):- !,
  name_the_pair(TestID,Type,Num,In,Out,PairName),  
  with_named_pair(Action,TestID,PairName,In,Out).

with_named_pair(solve,TestID,PairName,In,Out):- !,
  with_named_pair(cheat,TestID,PairName,In,Out).

with_named_pair(cheat,TestID,PairName,In,Out):- !,
  catch(maybe_confirm_sol(TestID,PairName,In,Out),E,(wdmsg(E),fail)),!.

with_named_pair(preview,TestID,PairName,In,Out):- !,
  dash_char(60,"|"),nl,nl,nop((wqnl(arc1(TestID)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  show_pair(IH,IV,OH,OV,test,PairName,In,Out).

with_named_pair(learn,TestID,PairName,In,Out):- !,
  nop((wqnl(learning(TestID=PairName)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  %color_counts(In,InCC),
  %color_counts(Out,OutCC),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut),!,
  compute_diff(SharedOut,SharedIn,Diff),
  pt(Diff).


name_the_pair(TestID,Type,Num,In,Out,PairName):- 
  ExampleNum = Type+Num,  
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,TestID),
  nb_setval(test_name_w_type,TestID*ExampleNum),
  must_det_l((
   ignore((CName\==TestID, flag(indiv,_,0),    
   dash_char(60,"A"),nl,dash_char(60,"|"),dash_char(6,"\n"),nl,
    dash_char(60,"|"),nl,dash_char(60,"V"),nl,
    nl,wqnl(arc1(TestID)),nl,nl,dash_char(60,"A"),nl)),   
  PairName= TestID*(ExampleNum),
  GridNameIn= TestID*(ExampleNum)*in,
  GridNameOut= TestID*(ExampleNum)*out,
  set_gridname(In,GridNameIn),
  set_gridname(Out,GridNameOut),  
  test_info(TestID,Info), wqnl(fav(TestID,Info)),nl)).
  





/*

! 
_
/
\

*/
learn_shape(Name,Ascii):- replace_in_string([ 
   '\r'='\n','\n\n'='\n','! '='!','!\n'='\n','!'=''],Ascii,Ascii0),
   atomics_to_string(Rows1,'\n',Ascii0),Rows1=[_|Rows],maplist(atom_chars,Rows,GrowthChart),
   subst_each(GrowthChart,[
   ' '=_,
   ','=Fill,
   '.'=Fill,
   '/'=Color,
   '|'=Color,
   '='=Color,
  '\\'=Color,
   'o'=Color], Grid),
   assertz_if_new(learned_color_inner_shape(Name,Color,Fill,Grid,GrowthChart)),
   Color = green, Fill = red,
   grid_size(Grid,H,V),
   print_grid(H,V,Grid),
   wqnl(learned(Name)).

learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).

:- learn_shapes.




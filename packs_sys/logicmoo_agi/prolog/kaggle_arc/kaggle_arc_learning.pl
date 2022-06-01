
rot_by_90([A,B,C,D]):- rot_by_90_0([A,B,C,D,A,B,C]).

rot_by_90_0([A,B]):- rot90(A,B),!.
rot_by_90_0([A,B|List]):- rot90(A,B),rot_by_90_0([B|List]).
  
subtractGrid(Out,In,Alien):- plain_var(Alien), remove_global_points(In,Out,Alien),!.
subtractGrid(Out,In,Alien):- plain_var(Out),!,add_global_points(Alien,In,Out).
subtractGrid(Out,In,Alien):- plain_var(In),!,remove_global_points(Alien,Out,In).

find_by_shape(Grid,Find,Founds):- 
 makeup_gridname(ID),
 vis_hv(Find,GH,GV),
 decolorize(Find,F), 
 Prog = (all_rotations(F,F1),
   %print_grid(F1),!,
   ogs(H,V,F1,Grid),% trace,
   all_points(F1,GH,GV,Points),
  pt(Points),
   make_indiv_object(ID,GH,GV,Points,[F1,loc_xy(H,V)],F2)),
 findall(F2,Prog,Matches),
 align_founds(Matches,Founds).

align_founds(Founds,Founds).

in_out(In,Out):-
  nb_current(test_pairname,PairName),
  into_gridnameA(In,PairName*in),
  into_gridnameA(Out,PairName*out).

lrn0:-    
   in_out(In,Out),
   subtractGrid(Out,In,Alien),
   rot_by_90([Alien,A,B,C]),
   find_by_shape(In,Alien,[A,B,C]),
   find_by_shape(Out,Alien,[A,B,C,Alien]).

lrn:- forall(lrn1, true).
lrn1:- learn_arc(_).

tst:- forall(tst1, true).
tst1:- test_arc(_).

learn_arc(TestID):- with_arc(learn,TestID).

test_arc(TestID):- with_arc(solve,TestID).

with_arc(Action,TestID):- plain_var(TestID),!, findall(Name,fav(Name),L),
  list_to_set(L,S), member(TestID,S), with_arc(Action,TestID).

with_arc(Action,arc):- !, findall(Name,kaggle_arc_db(Name,_,_,_,_),L),
  list_to_set(L,S), member(TestID,S), with_arc(Action,TestID).

with_arc(Action,TestName):-
  fix_test_name(TestName,TestID,_Type),TestName\==TestID,!,
  with_arc(Action,TestID).

with_arc(solve,TestID):- !, 
  with_arc(learn,TestID),
  forall(between(0,6,Num),with_pair(preview,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(solve,TestID,tst,Num)).

with_arc(test,TestID):- !, 
  with_arc(learn,TestID),
  forall(between(0,6,Num),with_pair(solve,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(solve,TestID,tst,Num)).

with_arc(preview,TestID):- !, 
  forall(between(0,6,Num),with_pair(preview,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(preview,TestID,tst,Num)).

with_arc(Action,TestID):-
  forall(between(0,6,Num),with_pair(Action,TestID,trn,Num)).

with_pair(Action,TestID,Type,Num):-
  kaggle_arc_db(TestID,Type,Num,in,In),
  kaggle_arc_db(TestID,Type,Num,out,Out),
  with_pair(Action,TestID,Type,Num,In,Out),!.

with_pair(Action,TestID,Type,Num,In,Out):- !,
  name_the_pair(TestID,Type,Num,In,Out,PairName),  
  with_named_pair(Action,TestID,PairName,In,Out).

with_named_pair(preview,TestID,PairName,In,Out):- !,
  dash_char(60,"|"),nl,nl,nop((wqnl(arc1(TestID)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  show_pair(IH,IV,OH,OV,test,PairName,In,Out).

with_named_pair(solve,TestID,PairName,In,Out):- !,
  with_named_pair(cheat,TestID,PairName,In,Out).

with_named_pair(cheat,TestID,PairName,In,Out):- !,
  ignore(catch(maybe_confirm_sol(TestID,PairName,In,Out),E,(wdmsg(E),fail))),!.

with_named_pair(learn,TestID,PairName,In,Out):- !,
  nop((wqnl(learning(TestID=PairName)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  %ccs(In,InCC),
  %ccs(Out,OutCC),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut),!,
  ((wqnl(learning_diff(TestID=PairName)),nl)),
  showdiff(SharedOut,SharedIn),
  ((wqnl(learned(TestID=PairName)),nl)).

new_test_id(TestID):-
  nb_setval(test_name,TestID),
  set_flag(indiv,0),
  nb_delete(grid_bgc),
  retractall(grid_nums(_,_)),
  retractall(grid_nums(_)),
  retractall(g2o(_,_)),!.

new_test_pair(PairName):-
  %nb_delete(grid_bgc),
  nb_setval(test_pairname,PairName),
  retractall(is_shared_saved(PairName*_,_)),
  retractall(is_shared_saved(PairName,_)),
  retractall(is_unshared_saved(PairName*_,_)),
  retractall(is_unshared_saved(PairName,_)),
  retractall(is_gridname(PairName*_,_)),
  retractall(is_gridname(PairName,_)),!.

name_the_pair(TestID,Type,Num,In,Out,PairName):- 
  name_the_pair(TestID,Type+Num,In,Out,PairName).

name_the_pair(TestID,ExampleNum,In,Out,PairName):- 
  PairName= TestID*ExampleNum,
  current_test_name(CName),
  new_test_pair(PairName),
  must_det_l((
   ignore((CName\==TestID, 
        new_test_id(TestID),
        dash_char(60,"A"),nl,dash_char(60,"|"),dash_char(6,"\n"),nl,
        dash_char(60,"|"),nl,dash_char(60,"V"),nl,
        nl,wqnl(arc1(TestID)),nl,nl,dash_char(60,"A"),nl)),   
  GridNameIn= PairName*in,
  GridNameOut= PairName*out,
  set_gridname(In,GridNameIn),
  set_gridname(Out,GridNameOut),  
  test_info(TestID,Info), pt(fav(TestID,Info)),nl)).
  



/*

! 
_
/
\

*/
growthchart_to_grid(GrowthChart,Color,Fill,Grid):-
   bg_sym(BG), 
  subst_each(GrowthChart,[
   ' '=BG, ','=Fill, '.'=Fill, '/'=Color, '|'=Color, '-'=Color,
   '_'=Color, '='=Color, '\\'=Color, 'o'=Color], BGrid),
   bg_to_fresh_vars(BGrid,Grid).

learn_shape(Name,Ascii):-
   ascii_to_growthchart(Ascii,GrowthChart),
   growthchart_to_grid(GrowthChart,Color,Fill,Grid),
   assertz_if_new(learned_color_inner_shape(Name,Color,Fill,Grid,GrowthChart)),
   nop((
     Color = green, Fill = red,        
     grid_size(Grid,H,V),
     print_grid(H,V,Grid),     
     wqnl(learned(Name)))).

learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).





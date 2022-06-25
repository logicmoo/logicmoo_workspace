/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

rot_in_incr_90(X,Y):- freeze(X,rot90(X,Y)).
rot_in_incr_90(X,Y):- freeze(X,rot180(X,Y)).
rot_in_incr_90(X,Y):- freeze(X,rot270(X,Y)).

rot_by_90_v1(List):- between_each(dif,List),between_each(rot_in_incr_90,List).

between_each(_P2,[]):- !.
between_each(_P2,[_]):- !.
between_each(P2,[X, Y]):- !, call(P2, X, Y).
between_each(P2,[X, Y, Z]):- !, call(P2, X, Y), call(P2, X, Z), call(P2, Z, Y).
between_each(P2,[X|Text]):- mapgroup(dif(X), Text), between_each(P2,Text).


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
 Prog = 
  (all_rotations(F,F1),
   %print_grid(F1),!,
   ogs(H,V,F1,Grid),% trace,

   grid_to_points(F1,GH,GV,Points),
   pt(Points),
   make_indiv_object(ID,GH,GV,Points,[iz(find_by_shape),F1,loc_xy(H,V)],F2)),
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
  dash_chars(60,"|"),nl,nl,nop((wqnl(arc1(TestID)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  show_pair_grid(red,IH,IV,OH,OV,in,out,PairName,In,Out).

with_named_pair(solve,TestID,PairName,In,Out):- !,
  with_named_pair(cheat,TestID,PairName,In,Out).

with_named_pair(cheat,TestID,PairName,In,Out):- !,
  ignore(catch(maybe_confirm_sol(_VM,TestID,PairName,In,Out),E,(wdmsg(E),fail))),!.

with_named_pair(learn,TestID,PairName,In,Out):- !,
  nop((wqnl(learning(TestID=PairName)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  %ccs(In,InCC),
  %ccs(Out,OutCC),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair_diff(IH,IV,OH,OV,in(unshared),out(unshared),PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair_i(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair_i(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair_diff(IH,IV,OH,OV,shared_in,shared_out,PairName,SharedIn,SharedOut),!,
  ((wqnl(learning_diff(TestID=PairName)),nl)),
  showdiff(SharedOut,SharedIn),
  ((wqnl(learned(TestID=PairName)),nl)).

name_the_pair(TestID,Type,Num,In,Out,PairName):- 
  name_the_pair(TestID,Type+Num,In,Out,PairName).

name_the_pair(TestID,ExampleNum,In,Out,PairName):- 
  PairName= TestID*ExampleNum,
  get_current_test(CName),
  new_test_pair(PairName),
  must_det_ll((
   ignore((CName\==TestID, 
        set_current_test(TestID),
        dash_chars(60,"A"),nl,dash_chars(60,"|"),dash_chars(6,"\n"),nl,
        dash_chars(60,"|"),nl,dash_chars(60,"V"),nl,
        nl,wqnl(arc1(TestID)),nl,nl,dash_chars(60,"A"),nl)),   
  GridNameIn= PairName*in,
  GridNameOut= PairName*out,
  set_grid_id(In,GridNameIn),
  set_grid_id(Out,GridNameOut),  
  test_info(TestID,Info), pt(fav(TestID,Info)),nl)).
  



/*

! 
_
/
\

*/
growthchart_to_grid(GrowthChart,Color,Fill,BGrid):-
  bg_sym(BG), 
  subst_each(GrowthChart,[
   ' '=BG, ','=Fill, '.'=Fill, '/'=Color, '|'=Color, '-'=Color,
   '_'=Color, '='=Color, '\\'=Color, 'o'=Color], BGrid).

learned_color_inner_shape(Name,Color,Fill,Grid,GrowthChart):-
   l_shape(Name,Ascii),
   ascii_to_growthchart(Ascii,GrowthChart),
   growthchart_to_grid(GrowthChart,Color,Fill,GridIn),
   to_real_grid(GridIn,Grid),
   \+ \+ ((nop((
     Color = green, Fill = red,        
     grid_size(Grid,H,V),
     print_grid(H,V,Grid),     
     wqnl(learned(Name)))))).

%learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).



:- fixup_exports.


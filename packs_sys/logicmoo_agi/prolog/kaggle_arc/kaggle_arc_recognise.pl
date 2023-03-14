/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).
:- discontiguous is_exit_hook/1.
:- discontiguous h666/2. 
:- discontiguous f666/2. 
:- discontiguous n_grid/2.
:- discontiguous in_shape_lib/2.
:- multifile in_shape_lib/2.
:- dynamic in_shape_lib/2.

test_ogs:- clsmake, mmake, my_time(forall(test_ogs(_,_,_),true)).
test_ogs_l:- clsmake, mmake, locally(b_setval(find_rule,loose),my_time(forall(test_ogs(_,_,_),true))).
test_ogs_s:- clsmake, mmake, locally(b_setval(find_rule,strict),my_time(forall(test_ogs(_,_,_),true))).

:- arc_history1(test_ogs).
:- arc_history1(test_ogs0).

test_ogs0:- clsmake, my_time(forall(test_ogs0(_,_,_),true)).
test_ogs1:- clsmake, my_time(forall(test_ogs1(_,_,_),true)).
%test_ogs2:- clsmake, my_time(forall(test_ogs2(_,_,_),true)).

:- arc_history1(test_ogs_m).
:- arc_history1(test_ogs_m0).

test_ogs_m:- clsmake, my_time(forall(test_ogs(_,_,true),true)).
test_ogs_m0:- clsmake, my_time(forall(test_ogs0(_,_,true),true)).

test_ogs_for_ans(PF,TestID,In,Out):- maybe_into_grid(In,InG),!,test_ogs_for_ans(PF,TestID,InG,Out).
test_ogs_for_ans(PF,TestID,In,Out):- maybe_into_grid(Out,OutG),!,test_ogs_for_ans(PF,TestID,In,OutG).
%test_ogs_for_ans(PF,TestID,In,Out):- % print_side_by_side(Test,In,Out), test_ogs_for_ans(PF,TestID,In,Out).
test_ogs_for_ans(PF,TestID,In,Out):-   
  trace_all_ogs([], Answers,In,Out), 
  Answers\==[],(PF\==fail->!;true),show_ogs_ans_success(TestID,Answers,In,Out),!.

test_ogs_for_ans(PF,TestID,In,Out):- % options: call,step,redo,exit,fail
  PF\==pass,
  trace_all_ogs([call,step,redo,exit,fail], Answers,In,Out), show_ogs_ans_success(TestID,Answers,In,Out), Answers\==[].

show_ogs_ans_success(TestID,Answers,In,Out):- 
  print_ss(TestID,In,Out),!,
  must_det_ll((treeify_props(show_ogs_ans_success,Answers,R),
  pp(R))),
  forall(member(Ans,Answers),show_answer(Ans,In,Out)),!.

show_answer(Ans,_In,Out):- !,
 must_det_ll((
  member(grid(Grid),Ans), 
  member(loc2D(H,V),Ans),
  include(p1_not(p1_arg(1,is_gridoid)),Ans,Props),
  grid_size(Out,GH,GV),
  H2 is H-2, V2 is V-2,
  offset_grid(H2,V2,Grid,OF),
  print_grid(GH,GV,Props,OF))),!.

show_answer(Ans,In,Out):-  
 %must_det_ll((
  include(p1_not(p1_arg(1,is_gridoid)),Ans,Props),
  member(loc2D(H,V),Ans),
  H2 is H-2, V2 is V-2,
 (once(member(grid(Grid),Ans);member(f_grid(Grid),Ans);object_l(grid_f(Grid),Ans);object_l(grid(Grid),Ans))->
  offset_grid(H2,V2,Grid,OF);OF=Out),
  grid_size(Out,GH,GV),
  (like_object(Ans,Out,ObjO)->true;ObjO=In),
  print_side_by_side(Props,print_grid(GH,GV,OF),print_grid(GH,GV,[ObjO,ObjO])),!.


test_ogs_a:- 
  red_only(In),
  test_ogs_for_ans(both,test_ogs_a,In,t_1b60fb0c_trn_0_out).
test_ogs_b:-
  red_only(In),
  into_grid(t_1b60fb0c_trn_0_in,Out),!,
  test_ogs_for_ans(both,test_ogs_b,In,Out).
test_ogs_c:- 
   test_ogs_for_ans(both,test_ogs_c,v_2697da3f_trn_0_in,v_2697da3f_trn_0_out).

test_ogs_d:- test_ogs_mode(pass).
test_ogs_e:- test_ogs_mode(both).
test_ogs_f:- test_ogs_mode(fail).

test_ogs_mode(Pass):-
 with_pair_mode(entire_suite,test_ogs_mode(Pass,_)).
test_ogs_mode(Pass,TestSpec):-
 forall_count(testspec_to_pairs(TestSpec,TestID,ExampleNum,In,Out),
  test_ogs_for_ans(Pass,test_ogs_mode(Pass,TestID>ExampleNum),In,Out)).



with_ogs_trace(Ports,Goal):-
 locally(t_l:pss_trace(Ports),Goal).
trace_all_ogs(Ports,R,In,Out):-
 with_ogs_trace(Ports, all_ogs(R,In,Out)).
all_ogs(R,In,Out):-
  findall(E,maybe_ogs(E,In,Out),R).


align_ints([VV1,VV2|VVs]):- pluz(VV1,1,VV2),!,align_ints([VV2|VVs]).
align_ints([_]). align_ints([]).

xfer_point(C-PXY,X,Y,HH,VV,C-HHVV):- lazy_p3(hv_point,X,Y,PXY),lazy_p3(hv_point,HH,VV,HHVV).
xfer_point(PXY,X,Y,HH,VV,HHVV):- lazy_p3(hv_point,X,Y,PXY),lazy_p3(hv_point,HH,VV,HHVV).

reassign_points([CXY|Points],HHs,VVs,[CHHVV|FreePoints]):-!,
  xfer_point(CXY,X,Y,HH,VV,CHHVV),  
  nth0(X,HHs,HH),nth0(Y,VVs,VV),
  reassign_points(Points,HHs,VVs,FreePoints).
reassign_points([],_,_,[]).
points_to_free_points_hv(Points,FreePoints):-
   sort(Points,FPs),
   length(HHs,32),align_ints(HHs),
   length(VVs,32),align_ints(VVs),
   reassign_points(FPs,HHs,VVs,FreePoints).

relate_points(P1,P2,F1,F2):- point_minus(P2,P1,Dist),lazy_p3(point_plus,F1,Dist,F2),!.
relate_points(P2,P1,F2,F1):- point_minus(P2,P1,Dist),lazy_p3(point_plus,F1,Dist,F2),!.
relate_points(P1,P2,F1,F2):- hv_point(H1,V1,P1),hv_point(H2,V2,P2),
   OH is H2-H1,OV is V2-V1, 
   lazy_relate(F1,OH,OV,F2),!.

lazy_relate(F1,OH,OV,F2):- freeze(F1,offset_hv(F1,OH,OV,F2)), freeze(F2,offset_hv(F1,OH,OV,F2)).

offset_hv(F1,OH,OV,F2):- nonvar(F1),!,hv_point(H1,V1,F1),H2 is H1+ OH, V2 is V1+OV, hv_point(H2,V2,F2).
offset_hv(F1,OH,OV,F2):- nonvar(F2),!,hv_point(H2,V2,F2),H1 is H2- OH, V1 is V2-OV, hv_point(H1,V1,F1).

fg_or_bg_points(Points,FGPoints):- fg_points(Points,FGPoints),FGPoints\==[],!.
fg_or_bg_points(Points,BGPoints):- my_maplist(bg_as(black),Points,BGPoints).

bg_as(Black,I,O):- I==bg,!,O = Black.
bg_as(_Black,I,I).

into_free_points(Grid,FreePointsO):-
 must_det_ll((
   localpoints(Grid,Points),
   fg_or_bg_points(Points,FGPoints),
   points_to_free_points(FGPoints,FreePoints),
   FreePoints=FreePointsO)),!.

points_to_free_points([C1-P1|Points],OUT):-
  must_det_ll(( points_to_free_points_pa(P1,F1,Points,FreePoints),
  OUT = [C1-F1|FreePoints])).

points_to_free_points_pa(P1,F1,[C2-P2|Points],OUT):- !,
 must_det_ll((
  relate_points(P1,P2,F1,F2),
  points_to_free_points_pa(P1,F1,Points,FreePoints),
  OUT=[C2-F2|FreePoints])).
points_to_free_points_pa(_,_,A,A).

create_shape_clones(Need,FreePoints,ExtraSet,Len,Min):-
  length(FreePoints,Len),
  Min is Need div Len,
  findall(Copy,(between(1,Min,_),copy_term(FreePoints,Copy)),ExtraSet).
  

random_ammount(Low,High,List,RndAmnt):-
   Range is High - Low + 1, Ammt is Low + random(Range),
   get_n_top(Ammt,List,RndAmnt).

get_n_top(N,List,TopN):-
   length(TopN,N),
   append(TopN,_,List).

calc_clones_needed(MinMaxPerObject,MinMaxLeftOver,[ShapeSize|More],[MaxNeeded|MMaxx],[Clones|TMore],Total):- !,
  Clones #=< MaxNeeded, %(Clones #>= 1 #\/ Clones#=0),
  Clones #>= 0,
  Clones #=< MaxPO, 
  Clones #>= MinPO,
   MinMaxPerObject = range(MinPO-MaxPO),
   Total  #= (ShapeSize * Clones) + Next,
   calc_clones_needed(MinMaxPerObject,MinMaxLeftOver,More,MMaxx,TMore,Next).
calc_clones_needed(_MinMaxPerObject,MinMaxLeftOver,[],[],[],Total):-
  Total #>= 0,
  Total #=< 900,
 ignore((
  MinMaxLeftOver = range(MinLeftOver-MaxLeftOver),
  Total #>= MinLeftOver,
  Total #=< MaxLeftOver)).

all_different_grid_locs(All):- all_different_bindings(All).

bestfit_test_grid([[green,green],[_,green]]). bestfit_test_grid([[blue,blue],[blue,blue]]).
bestfit_test_grid([[_,red],[red,red]]). bestfit_test_grid([[yellow,_],[yellow,yellow]]).
bestfit_test:- mmake, 
  findall(S,bestfit_test_grid(S),Set),
  into_grid(v('626c0bcc')>ExampleNum*in,Grid),
  grid_size(Grid,H,V),
  print_grid(H,V,ExampleNum,Grid),
  MinMaxPerObject = range(1-3),
  MinMaxLeftOver = range(_-0),
  bestfit_hybrid_shapes_on(mono_colorhack,Set,Grid,MinMaxPerObject,MinMaxLeftOver,GoodFit),
  print_list_of(print_grid(H,V),bestfit_test(ExampleNum),GoodFit),
  print_grid(H,V,allInUse,GoodFit).

mono_colorhack(_C1,_C2,P1,P1).
bestfit_hybrid_shapes_on(ColorHacks,Set,Grid,MinMaxPerObject,MinMaxLeftOver,OE):-
 %unique_fg_colors(Grid,[FG]),
 length(Set,Len),
  globalpoints(Grid,AvailablePoints),
  fg_points(AvailablePoints,FGPoints),
  length(FGPoints,Need),
  %Need<100,
 must_det_ll((
  %my_maplist(mapgrid(bestfit_shaped(FG,bg)),OSet,MSet),
  %list_to_set(MSet,Set),
  print_grid(bestfit_hybrid_shapes_on(Len),Grid),
  my_maplist(into_free_points,Set,FreePoints),

  my_maplist(create_shape_clones(Need),FreePoints,ExtraSet,Sizes,Maxes),
  flatten(ExtraSet,ES),all_different_grid_locs(ES),
  calc_clones_needed(MinMaxPerObject,MinMaxLeftOver,Sizes,Maxes,Wants,Need))),!,
  label(Wants),
  writeln(Wants),
  my_maplist(get_n_top,Wants,ExtraSet,SomeClones),
  append_2(SomeClones,SomePoints),
  equify_points_list(ColorHacks,SomePoints,FGPoints),
  append(SomeClones,GoodFit),
  findall(obj_loc(O,E),
    (nth1(N,Set,O),nth1(N,SomeClones,ListClones),member(C,ListClones),member(E,GoodFit),E==C),OE).

append_2(ListOfListOFLists,SomePoints):- 
  append(ListOfListOFLists,ListOfLists),append(ListOfLists,SomePoints),!.

equify_points_list(_ColorHacks,[],[]):-!.
equify_points_list(ColorHacks,SomePoints,[C1-P1|Points]):-   
   call(ColorHacks,C1,C2,P1,P2),
   select(C2-P2,SomePoints,Rest),
   %call(ColorHacks,C1,C2,P1,P2),
   equify_points_list(ColorHacks,Rest,Points).

best_fit_combos(OgsList,GridPs,ComboGroups):-
  globalpoints(GridPs,GridPoints), 
  =(OgsList,SmallToBig),reverse(SmallToBig,BigToSmall),
  max_group_filling(SmallToBig,GridPoints,Smallest,LeftOverA,LeftOutA),
  max_group_filling(BigToSmall,GridPoints,Largest,LeftOverB,LeftOutB),
  Fits = [result(LeftOverA,LeftOutA)-Smallest,result(LeftOverB,LeftOutB)-Largest],
  with_pre(pp(Fits)),
  sort(Fits,ComboGroups).


max_group_filling(SmallToBig,GridPoints,Smallest,SizeLeftOver,SizeLeftOut):-
  remove_objs(SmallToBig,GridPoints,Smallest,LeftOverA,LeftOutA),
  length(LeftOutA,SizeLeftOut),
  fg_points(LeftOverA,FGs),
  length(FGs,SizeLeftOver).

remove_objs([],GridPoints,[],GridPoints,[]):-!.
remove_objs(SmallToBig,[],[],[],SmallToBig):-!.
remove_objs([Obj|Smallest],GridPoints,[Obj|Used],LeftOverA,LeftOutA):-
  globalpoints(Obj,Pts), intersection(GridPoints,Pts,_,LeftOver1,[]),
  remove_objs(Smallest,LeftOver1,Used,LeftOverA,LeftOutA),!.
%remove_objs(LeftOutA,GridPoints,[],GridPoints,LeftOutA):- !.
remove_objs([Obj|Smallest],GridPoints,Used,LeftOverA,[Obj|LeftOutA]):-
  remove_objs(Smallest,GridPoints,Used,LeftOverA,LeftOutA).


maybe_ogs(ROut,In,Out):- 
   def_ogs_prog(Constr),
    maybe_ogs(Constr,RRR,In,Out),
    simpl_ogs(RRR,ROut).
    
%maybe_ogs(ROut,In,Out):- try_ogs_pass_9([plain],ROut,In,Out).

maybe_ogs(Constr,R,In,Out):- var(In),!,maybe_ogs_test_i(In),maybe_ogs(Constr,R,In,Out).
maybe_ogs(Constr,R,In,Out):- var(Out),!,maybe_ogs_test_o(Out),maybe_ogs(Constr,R,In,Out).

maybe_ogs(Constr,R,In,Out):- \+ is_grid(Out),!,into_grid(Out,OOut),maybe_ogs(Constr,R,In,OOut).
maybe_ogs(Constr,R,In,Out):- is_grid(In),!, maybe_ogs_pass0([],Constr,R,In,Out).
maybe_ogs(Constr,R,[In|List],Out):- !,
 maybe_cvt_to_grid(In,Grid),!, (maybe_ogs_pass0([maybe_cvt_to_grid],Constr,R,Grid,Out);maybe_ogs(Constr,R,List,Out)).
maybe_ogs(Constr,R,In,Out):- In\==[],maybe_cvt_to_grid(In,Grid),!, maybe_ogs_pass0([],Constr,R,Grid,Out).

%maybe_ogs_pass0(Constr,R,Grid,Out):- maybe_if_changed(fpad_grid(s),Out,OOut),!, maybe_ogs_pass_1(Prf,Constr,R,Grid,OOut).

maybe_ogs_pass0(Prf,Constr,ROut,In,Out):- 
  prepare_output_hooks(CheckTypes,List1,Out,OutC),!,
  prepare_input_hooks(Constr,CheckTypes,List2,In,InC),!,
  append([Prf,List1,List2],Strt),
  maybe_ogs_pass0a(CheckTypes,Strt,Constr,ROut,InC,OutC).

maybe_ogs_pass0a(CheckTypes,Prf,Constr,ROut,Grid,Out):- 
  \+ \+ trace_ogs(call,begin_search(Constr),Grid,Out),
  (maybe_ogs_pass_cs(Prf,Constr,RR,Grid,Out),
   CheckTypes=run,
   must((do_exit_hooks(RR))),
   reverse(RR,ROut)).


maybe_ogs_pass_cs(Prf,Constr,R,In,Out):- maybe_ogs_pass_1(Prf,Constr,R,In,Out).
%maybe_ogs_pass_cs(Prf,Constr,[auto(Cs)|R],In,Out):- do_suggested_color_swaps(Cs,In,Out,InCs,OutCs), maybe_ogs_pass_1(Prf,Constr,R,InCs,OutCs).


do_exit_hooks(R):- \+ compound(R),!.
do_exit_hooks([H|T]):-!,do_exit_hooks(H),do_exit_hooks(T).
do_exit_hooks(C):- clause(is_exit_hook(C),Hook),!,show_failure(Hook).
do_exit_hooks(_-C):- clause(is_exit_hook(C),Hook),!,show_failure(Hook).
do_exit_hooks(_).

select_allow_or_must(Idea,Constr,Next):-
  select(Idea,Constr,Next),functor(Idea,F,_),member(F,[allow,must]).
/*
maybe_ogs_pass_1(Prf,Constr,R,In,Out):- select(after(Before,Idea),Constr,Next),member(Before,Prf),!, 
  idea_io_goal(Idea,IO,Call), maybe_ogs_pass_2([Idea|Prf],Next,Idea,IO,Call,R,In,Out).  

maybe_ogs_pass_1(Prf,Constr,R,In,Out):- 
  select(must(Idea),Constr,Next),  idea_io_goal(Idea,IO,Call),!,
  maybe_ogs_pass_2([Idea|Prf],[disallow(Idea)|Next],must(IO),Call,R,In,Out).

maybe_ogs_pass_1(Prf,Constr,R,In,Out):- (member(allow(Idea),Constr); idea_for(In,Out,Idea)), 
  allowed_in(Idea,Constr), idea_io_goal(Idea,IO,Call),  
  maybe_ogs_pass_2([Idea|Prf],[disallow(Idea)|Constr],IO,Call,R,In,Out).  
*/
maybe_ogs_pass_1(Prf,Constr,ROut,In,Out):- 
  try_ogs_pass_7(Constr,Prf,ROut,In,Out),
  ignore(learn_hybrid_shape_board(ogs(ROut),In)).

meets_constraints(-C,R):- !, \+ member(C,R).
meets_constraints(+C,R):- !, member(C,R).
meets_constraints([C|Constr],ROut):- !,meets_constraints(C,ROut),meets_constraints(Constr,ROut).
%meets_constraints([],_ROut):-!.
meets_constraints(_C,_R).

%(member(rul(loose),ROut) -> was_loose_ok(ROut) ; true),


maybe_ogs_pass_2(Prf,Constr,must(IO),Call,R,In,Out):- 
  must_det_ll(use_idea(IO,grid_call(Call),In,Out,NewIn,NewOut)),
   maybe_ogs_pass_1(Prf,Constr,R,NewIn,NewOut).

/*
maybe_ogs_pass_2(Prf,Constr,IO,Call,R,In,Out):- 
   use_idea(IO,maybe_if_changed_grid_call(Call),In,Out,NewIn,NewOut),
   maybe_ogs_pass_1(Prf,Constr,R,NewIn,NewOut).
*/

maybe_ogs_pass_2(Prf,Constr,IO,Call,R,In,Out):- 
   use_idea(IO,grid_call(Call),In,Out,NewIn,NewOut),
   maybe_ogs_pass_1(Prf,Constr,R,NewIn,NewOut).


use_idea(inp,Call,In,Out,NewIn,Out):- copy_term(In,InC),call(Call,In,NewIn),trace_ogs(step,before_and_after(inp,Call),InC,NewIn).
use_idea(out,Call,In,Out,In,NewOut):- call(Call,Out,NewOut),trace_ogs(step,before_and_after(out,Call),Out,NewOut).


release_some_c(Grid,Grid):-!.

release_some_c(Grid,NonBG):- is_group(Grid),!,mapgroup(release_some_c,Grid,NonBG).
release_some_c(Grid,Out):- sub_var(black,Grid),!,Out=Grid.
release_some_c(Grid,Out):- release_non_fg(Grid,Out).

release_non_mc(Grid,Grid):-!.

release_non_mc(Grid,NonBG):- is_group(Grid),!,mapgroup(release_non_mc,Grid,NonBG).
release_non_mc(Grid,NonBG):- 
 ((sub_var(black,Grid),(sub_var(bg,Grid);sub_var(wbg,Grid);sub_var(fg,Grid);sub_var(zero,Grid);sub_var(wfg,Grid)))->
  release_non_bg(Grid,NonBG);release_non_fg(Grid,NonBG)).


release_non_fg(Point,Point):- is_fg_color(Point),!.
release_non_fg(Point,_):- is_color(Point),!.
release_non_fg(List,FGList):- is_list(List),!,my_maplist(release_non_fg,List,FGList).
release_non_fg(Keep,NonFG):- is_grid(Keep),!,mapgrid(release_non_fg,Keep,NonFG).
release_non_fg(Keep,NonFG):- is_cpoints_list(Keep),!,my_maplist(release_non_fg,Keep,NonFG).
release_non_fg(Keep,NonFG):- map_pred1(release_non_fg,Keep,NonFG),!.
release_non_fg(Keep,Keep).

release_non_bg(Point,Point):- is_bg_color(Point),!.
release_non_bg(Point,_):- is_color(Point),!.
release_non_bg(Keep,NonFG):- is_grid(Keep),!,mapgrid(release_non_bg,Keep,NonFG).
release_non_bg(Keep,NonFG):- is_cpoints_list(Keep),!,my_maplist(release_non_bg,Keep,NonFG).
release_non_bg(List,FGList):- is_list(List),!,my_maplist(release_non_bg,List,FGList).
release_non_bg(Keep,NonBG):- map_pred1(release_non_bg,Keep,NonBG),!.
release_non_bg(Keep,Keep).

subst_all_fg_colors_with_vars(Cs,Vs,In,Mid):- 
  copy_safe(In,InC),unique_fg_colors(InC,Cs),
  Cs\==[], % at least some colors
  subst_colors_with_vars(Cs,Vs,InC,Mid),    
  ground(Cs), % fully grounded test
  my_maplist(cfg,Vs), % constrain to foreground
  my_maplist(dif(zero),Vs), % constrain to not zero
  !.




is_exit_hook(subst_all_fg_colors_with_vars(Cs,Vs,_In,_Out)):- 
  Cs\=@=Vs, % slightly differnt 
  list_to_set(Vs,Set), Vs=@=Set, % All differnt colors
  ground(Vs), % fully grounded results
  my_maplist(same_color_class,Cs,Vs), % FG == FG ,.. BG == BG etc    
  %\+ member(black,Vs),
  !.


maybe_if_changed_grid_call(Call,Out,NewOut):-
   maybe_if_changed(grid_call(Call),Out,NewOut).

   %maybe_ogs(pass_1,[unbind_black|R],In,Out):- sub_var(black,In),maybe_if_changed(unbind_black,In,IIn), mass(IIn,Mass),Mass>0, maybe_ogs(pass_2,R,IIn,Out).
%maybe_ogs(pass_2,[into_monogrid(find)|R],In,Out):- maybe_if_changed(into_monogrid,Out,OOut), maybe_ogs(pass_3,R,In,OOut).
%maybe_ogs(pass_3,[into_monogrid(srcharea)|R],In,Out):- maybe_if_changed(into_monogrid,In,IIn), maybe_ogs(pass_4,R,IIn,Out).
%maybe_ogs(pass_4,[trim_to_rect|R],In,Out):- maybe_if_changed(trim_to_rect,In,IIn),maybe_ogs(pass_5,R,IIn,Out).
%maybe_ogs(pass_5,R,In,Out):- subst(In,red,blue,InC),rot180(InC,In180), maybe_ogs(pass_7,R,In180,Out).
%maybe_ogs_pass_cs(Prf,Constr,R,In,Out):- maybe_ogs_pass_1(Prf,Constr,R,In,Out).

try_ogs_pass_7(Constr,Prf,RROut,In,Out):-   
  select(+rot2D(sameR),Constr,RestConstr),!,
  NextPrf = [rot2D(sameR)|Prf],
  try_ogs_pass_8(NextPrf,ROut,In,Out),
  meets_constraints(RestConstr,ROut),
  append(ROut,NextPrf,RROut).

try_ogs_pass_7(Constr,Prf,RROut,In,Out):-   
  findall_rotations(RotG,In,InR),
  NextPrf = [rot2D(RotG)|Prf],
  try_ogs_pass_8(NextPrf,ROut,InR,Out),
  meets_constraints(Constr,ROut),
  append(ROut,NextPrf,RROut).

findall_rotations(sameR,In,In).
findall_rotations(RotG,In,InR):-
  Rot = (RotG-InR),
  findall(Rot,((enum_orientation(RotG),RotG\==rollD,once(call(RotG,In,InR)),InR\=@=In)),RotL),
  predsort(using_compare(arg(2)),RotL,RotS),member(Rot,RotS).

enum_simple_xforms(RotG):- enum_orientation(RotG).
enum_simple_xforms(Blur_FlipV):- easy0(4,Blur_FlipV).
enum_simple_xforms(Grow2):- easy0(5,Grow2).
enum_simple_xforms(Flip_Once):- easy0(2,Flip_Once).
enum_simple_xforms(invert_one_color(_)).

invert_one_color(Cs,In,Out):- var(Cs),!,unique_fg_colors(In,[Cs]),
  choose_bg_color(Out,Black,black),
  swap_colors(Cs,Black,In,Out).

try_ogs_pass_8(Prf,R,In,Out):- try_ogs_pass_9(Prf,R,In,Out).
try_ogs_pass_8(Prf,[Cs|R],In,Out):- 
  do_suggested_color_swaps(Cs,In,Out,InCs,OutCs), InCs\=@=In,
  try_ogs_pass_9(Prf,R,InCs,OutCs).
%try_ogs_pass_8a(Prf,R,In,Out):- try_ogs_pass_9(Prf,R,In,Out).
/*
try_ogs_pass_8b(Prf,R,In,Out):- try_ogs_pass_8a(Prf,R,In,Out).
try_ogs_pass_8b(Prf,ROut,In,Out):- 
  subst_all_fg_colors_with_vars(Cs,Vs,In,IIn),
  Cs\==[], % at least some colors
  ground(Cs), % fully grounded test
  try_ogs_pass_9(Prf,R,IIn,Out),  % do our search!
  Cs\=@=Vs, % slightly differnt 
  my_maplist(cfg,Vs),
  my_maplist(same_color_class,Cs,Vs), % FG == FG ,.. BG == BG etc    
  ground(Vs), % fully grounded results
  list_to_set(Vs,Set), Vs=@=Set, % All differnt colors
  [subst(Cs,Vs)|R]=ROut.
*/
% 007bbfb7 needs loose %,!,R\==loose.
%try_ogs_pass_9(Prf,[rul(R),loc2D(X,Y)/*f_grid(In)*/],In,Out):- nonvar(R),!,(R==strict->find_ogs(X,Y,In,Out);ogs_11(X,Y,In,Out)).
%try_ogs_pass_9(Prf,[loc2D(X,Y),rul(ogs_11)/*f_grid(In)*/|Prf],In,Out):- 
%  trace_ogs(redo,ogs_11(Prf),In,Out),
try_ogs_pass_9(Prf,ROut,In,Out):- !,
  (try_ogs_pass_9a(Prf,ROut,In,Out)
   *-> trace_ogs(exit,ROut,In,Out)
    ; (trace_ogs(fail,Prf,In,Out),fail)).

try_ogs_pass_9a(Prf,ROut,In,Out):-
  trim_rect_by_1(In,InRect),
  trim_rect_by_1(Out,OutRect),!,
   XYR = xyr(X,Y,R),
   findall(XYR,  (ogs_r_find(X,Y,R,In,Out);ogs_r_find(X,Y,R,InRect,OutRect)),XYRL),!,
   XYRL\==[],
  must_det_ll((
   list_to_set(XYRL,XYRS),
   %(XYRS == [] -> trace_ogs(fail,ogs_r_find(Prf),In,Out); trace_ogs(fail,ogs_r_find(Prf),In,Out)),
   member(XYR,XYRS),
   [loc2D(X,Y),rul(R),grid(InRect)|Prf]=ROut)).

trim_rect_by_1(In,Rect):- 
  append([_|Mid],[_],In),
  rot90(Mid,Mid90),
  append([_|Mid270],[_],Mid90),
  rot270(Mid270,Rect).

ogs_r_find(X,Y,R,In,Out):- 
  mass(In,N),N>0,
  copy_term(In+Out,CIn+COut),
  copy_term(In+Out,DIn+DOut), 
  ogs_11(X,Y,DIn,DOut),(find_ogs(X,Y,CIn,COut)->R=strict;R=loose),  
  %R\==loose,
  In=CIn,DIn=CIn,
  Out=COut,DOut=COut.

red_only(X):-
  into_grid(t_1b60fb0c_trn_0_in,In),
  into_grid(t_1b60fb0c_trn_0_out,Y),
  grid_minus_grid(Y,In,X),!.



:- thread_local(t_l:pss_trace/1).

maybe_ogs_test:- 
  set_current_test('1b60fb0c'),
  forall(maybe_ogs_test_i(In),
    forall(maybe_ogs_test_o(Out),
      ( findall(R,maybe_ogs(R,In,Out),RL),
        print_table([[call(pp(RL))],[call(print_side_by_side(In,Out))]])))).

maybe_ogs_test2:- 
 maybe_ogs(R,X,Y),print_grid(X),pp(R),print_grid(Y).

maybe_ogs_test_i(In):- maybe_ogs_test_j(I),(maybe_if_changed(trim_to_rect,I,In);In=I),mass(In,Mass),Mass>0.
maybe_ogs_test_j(In):- enum_object(O),global_grid(O,In).
maybe_ogs_test_j(In):- maybe_ogs_test_p(In).
maybe_ogs_test_p(In):- current_pair(I,O),grid_minus_grid(I,O,In).
maybe_ogs_test_p(In):- current_pair(I,O),mapgrid(cell_minus_cell,O,I,In).
maybe_ogs_test_p(Out):- current_test_example(X,Y),into_grid(X>Y*_,Out).
maybe_ogs_test_o(Out):- get_current_test(TestID), kaggle_arc(TestID,_,_,Out), \+ maybe_ogs_test_p(Out).
maybe_ogs_test_o(Out):- maybe_ogs_test_p(Out).

bg_to_not_fg(G,GG):- is_grid(G),!,mapgrid(bg_to_not_fg,G,GG).
bg_to_not_fg(G,GG):- is_group(G),!,mapgroup(bg_to_not_fg,G,GG).
bg_to_not_fg(G,GG):- is_bg_color(G),!,decl_cbg(GG).
%bg_to_not_fg(G,GG):- plain_var(G),!,nop(cbg(GG)).
bg_to_not_fg(G,GG):- is_grid_cell(G),!,G=GG.
bg_to_not_fg(G,GG):- is_gridoid(G),maptree(bg_to_not_fg,G,GG).
bg_to_not_fg(G,G). 
/*
bg_to_not_fg(G,GG):- attvar(G), \+ get_attr(G,ci,_),!, GG = G, put_attr(GG,ci,bg(_)).
bg_to_not_fg(G,GG):- attvar(G), !, GG=G.
bg_to_not_fg(G,GG):- fail, var(G),freeze(G,if_t(is_color(G),cbg(GG))).
bg_to_not_fg(G,GG):- is_bg_color(G),!,cbg(GG).*/

decl_cbg(GG):- is_fg_color(GG),!.
decl_cbg(GG):- cbg(GG).

cbg(Trig,Var):- freeze(Trig, cbg(Var)).
cbg(GG):- plain_var(GG),!,decl_bg_color(GG).
cbg(GG):- is_fg_color(GG),!,fail.
cbg(GG):- \+ is_fg_color(GG),!, decl_bg_color(GG).
cbg(GG):- freeze(GG, \+ is_fg_color(GG)).
%cbg(Var):- freeze(Var, \+ is_fg_color(Var)).

/*
yg_to_not_xg(G,GG):- is_grid(G),!,mapgrid(yg_to_not_xg,G,GG).
yg_to_not_xg(G,GG):- is_group(G),!,mapgroup(yg_to_not_xg,G,GG).
yg_to_not_xg(G,GG):- var(G),freeze(G,if_t(is_color(G), \+ is_xg_color(GG))).
yg_to_not_xg(G,GG):- is_list(G),!,my_maplist(yg_to_not_xg,G,GG).
yg_to_not_xg(G,GG):- is_yg_color(G),!,freeze(GG, \+ is_xg_color(GG)).
yg_to_not_xg(G,G). 
*/
fg_to_not_bg(G,GG):- is_grid(G),!,mapgrid(fg_to_not_bg,G,GG).
fg_to_not_bg(G,GG):- is_group(G),!,mapgroup(fg_to_not_bg,G,GG).
fg_to_not_bg(G,GG):- plain_var(G),!,cfg(GG).
fg_to_not_bg(G,GG):- is_fg_color(G),!,cfg(GG).
fg_to_not_bg(G,GG):- is_grid_cell(G),!,G=GG.
fg_to_not_bg(G,GG):- is_gridoid(G),maptree(fg_to_not_bg,G,GG).
fg_to_not_bg(G,G). 
/*
fg_to_not_bg(G,GG):- attvar(G), \+ get_attr(G,ci,_),!, GG = G, put_attr(GG,ci,fg(_)).
fg_to_not_bg(G,GG):- attvar(G), !, GG=G.
fg_to_not_bg(G,GG):- fail, var(G),freeze(G,if_t(is_color(G),cfg(GG))).
fg_to_not_bg(G,GG):- is_fg_color(G),!,cfg(GG).*/


cfg(Trig,Var):- freeze(Trig, cfg(Var)).
cfg(GG):- plain_var(GG),!,put_attr(GG,ci,fg(_)),!. 
cfg(GG):- \+ is_bg_color(GG).

unbind_bg(BRect0,BRect):- sub_var(bg,BRect0),!,maptree(bg_to_plain_var,BRect0,BRect).
unbind_bg(BRect0,BRect):- unbind_black(BRect0,BRect).
unbind_black(BRect0,BRect):- maptree(black_to_plain_var,BRect0,BRect).
bg_to_plain_var(BG,_):- BG==bg,!.  black_to_plain_var(G,_):- G==black,!.

is_trimmable_cell(BG,Cell):- \+ \+ BG = Cell, !, BG=Cell.
%is_trimmable_cell(_BG,Cell):- is_bg_color(Cell),!.
is_trimmable_cell(_BG,Cell):- \+ is_fg_color(Cell),!.
%is_trimmable_cell(_BG,Cell):- attvar(Cell),!,fail.

is_exit_hook(prepare_output_hooks(CheckTypes,_)):- CheckTypes=run.
is_exit_hook(prepare_input_hooks(CheckTypes,_)):- CheckTypes=run.
choose_bg_color(Out,Black,Else):-
  (sub_var(bg,Out)->Black=bg;
    sub_var(wbg,Out)->Black=wbg;
     sub_var(black,Out)->Black=black;
       Black = Else).

%maybe_constrain_grid_now(CntDwn,f,CheckTypes,Out2,OutZ):- constrain_grid_now(CntDwn,f,CheckTypes,Out2,OutZ),!.
maybe_constrain_grid_now(_CntDwn,f,_CheckTypes,OutZ,OutZ):-!.

prepare_output_hooks(CheckTypes,[canvas(BlackS,ZeroS),ogs_trig(CheckTypes)],Out,OutZ):-
  Zero = black, Else = Zero,
  choose_bg_color(Out,Black,Else),
  subst_color_auto(Black,Zero,Out,Out1),
  fpad_grid(f,var,Out1,Out2),
  maybe_constrain_grid_now(_CntDwn,f,CheckTypes,Out2,OutZ),
  sformat(BlackS,'~w',[Black]),
  sformat(ZeroS,'~w',[Zero]).

%if_be_fast(G):- can_be_slow,!.
if_be_fast(G):-once(G).

prepare_input_hooks(Constr,CheckTypes,Props,In,O):-
 member(+rul(loose),Constr),!, 
 grid_size(In,GH,GV), %if_be_fast(GH>=3;GV>=3),!,
 must_det_ll(( 
   choose_bg_color(In,Black,dont),
   subst_color_auto(Black,bg,In,InBG),
   trim_to_rect4_always(L,T,R,B,InBG,InBGTrimmed))),
 grid_size(InBGTrimmed,H,V),%if_be_fast(H>=3;V>=3),
 must_det_ll(( fpad_grid(f,var,InBGTrimmed,MO),constrain_grid_now(sloose,f,CheckTypes,MO,O),  
   Props = [offset2D(L,T),shrinkSE(R,B),size2D(H,V),vis2D_Z(GH,GV)])).

prepare_input_hooks(Constr,CheckTypes,Props,In,O):- % R = [],
  member(+rul(strict),Constr),!, 
  CntDwn = 0,
 grid_size(In,GH,GV), %if_be_fast(GH>=3;GV>=3),!,
 must_det_ll(( 
   choose_bg_color(In,Black,dont),
   subst_color_auto(Black,bg,In,InBG),
   trim_to_rect4_always(L,T,R,B,InBG,InBGTrimmed))),
 grid_size(InBGTrimmed,H,V),%if_be_fast(H>=3;V>=3),
 must_det_ll(( fpad_grid(f,var,InBGTrimmed,MO),constrain_grid_now(CntDwn,f,CheckTypes,MO,O),  
   Props = [offset2D(L,T),shrinkSE(R,B),size2D(H,V),vis2D_Z(GH,GV)])).

prepare_input_hooks(_Constr,CheckTypes,Props,In,O):- % R = [],
  gensym(count_down_,CntDwn),
 grid_size(In,GH,GV), %if_be_fast(GH>=3;GV>=3),!,
 must_det_ll(( 
   choose_bg_color(In,Black,dont),
   subst_color_auto(Black,bg,In,InBG),
   trim_to_rect4_always(L,T,R,B,InBG,InBGTrimmed))),
 grid_size(InBGTrimmed,H,V),%if_be_fast(H>=3;V>=3),
 must_det_ll(( fpad_grid(f,var,InBGTrimmed,MO),constrain_grid_now(CntDwn,f,CheckTypes,MO,O),  
   Props = [offset2D(L,T),shrinkSE(R,B),size2D(H,V),vis2D_Z(GH,GV)])).
/*
prepare_input_hooks(CheckTypes,Props,I,O):- % R = [],
 must_det_ll(( grid_size(I,GH,GV), I=M, ([L,T,R,B] = [0,0,0,0]),
  grid_size(M,H,V),fpad_grid(f,var,M,MO),constrain_grid_now(CntDwn,f,CheckTypes,MO,O),
  Props = [offset2D(L,T),shrinkSE(R,B),size2D(H,V),vis2D_Z(GH,GV)])).
*/
trim_to_rect4_always(L,T,R,B,I,M):- trim_to_rect4(L,T,R,B,I,M),!.
trim_to_rect4_always(0,0,0,0,I,M):- I=M.

trim_to_rect4(Grid,MGrid):-  trim_to_rect4(_T,_R,_B,_L,Grid,MGrid).
%trim_to_rect4(L,T,R,B,Grid,MGrid):- get_bgc(BG), trim_then_90(BG,[L,T,R,B],Grid,MGrid),!.
trim_to_rect4(L,T,R,B,Grid,IM360):-
  trim_unused_n_vert(0,BG,Grid,IM,T), 
  rot90(IM,IM90),
  trim_unused_n_vert(0,BG,IM90,I90,L), 
  rot90(I90,IM180),
  trim_unused_n_vert(0,BG,IM180,I180,B), 
  rot90(I180,IM270),
  trim_unused_n_vert(0,BG,IM270,I270,R),
  rot90(I270,IM360).

%trim_to_rect4(_T,_R,_B,_L,Color,MGrid):- trim_to_rect(Color,MGrid). 
trim_then_90(_,[],Grid,Grid):- !.
trim_then_90(BG,[R90|More],Grid,GridO):- trim_unused_n_vert(0,BG,Grid,GridR,R90),
  rot90(GridR,Grid90),trim_then_90(BG,More,Grid90,GridO).

is_trimmable_row(Row,BG):- my_maplist(is_trimmable_cell(BG),Row).

trim_unused_n_vert(N,BG,[Row|Grid],GridO,N2):- 
  is_trimmable_row(Row,BG),!,
  trim_unused_n_vert(N,BG,Grid,GridO,N1),plus(N1,1,N2).
trim_unused_n_vert(N,_,GridO,GridO,N):-!.

allowed_in(IO-Idea,Constr):- member(allow(all(IO)),Constr),!, \+ member(disallow(IO-Idea),Constr).
allowed_in(Idea,Constr):- member(allow(all),Constr),!, \+ member(disallow(Idea),Constr).
allowed_in(IO-Idea,Constr):- member(disallow(all(IO)),Constr),!, member(allow(IO-Idea),Constr).
allowed_in(Idea,Constr):- member(disallow(all),Constr),!, member(allow(Idea),Constr).
allowed_in(Idea,Constr):- member(allow(Idea),Constr).

idea_io_goal(IO-Call,IO,Call).
expand_io(IO,IOD):- IO == both,!,member(IOD,[inp,out]).
expand_io(IO,IO).


copy_safe(In,InC):- duplicate_term(In,InC). % ,copy_term(In,InC),In=InC.

suggested_fg_color_swaps(In,Out,CIn,COut):- 
  copy_safe(In,InC),copy_safe(Out,OutC),
  unique_fg_colors(InC,InCs),InCs=[_],
  unique_fg_colors(OutC,CsOut),
  InC \== OutC,  
  member(CIn,InCs), % \+ member(CIn,CsOut),
  member(COut,CsOut), 
  COut \== zero,
  \+ member(COut,InCs).

subst_color_auto(F,_,I,O):- must_be_free(O),F== dont,!,I=O.
subst_color_auto(F,R,I,O):- subst0011(F,R,I,O).
%subst_color_auto(F,R,I,O):- subst(I,F,R,O).
%subst_color_auto(F,R,I,O):- subst0011a(F,R,I,O).

do_suggested_color_swaps(recolor(CIn,COut),In,Out,InCs,Out):- 
 suggested_fg_color_swaps(In,Out,CIn,COut),
 subst_color_auto(CIn,COut,In,InCs).
do_suggested_color_swaps(recolor(CIn,CIn),In,Out,In,Out):- 
 suggested_fg_color_swaps(In,Out,CIn,_),!.
/*
idea_for(In,Out,IODCall):-  fail, 
  suggested_fg_color_swaps(In,Out,CIn,COut),
  idea_io_goal(IODCall,inp,subst_color_auto(CIn,COut)).

%idea_for(_,_,IOD-Call):- fail, idea_for_data(IO-Call),expand_io(IO,IOD).
%idea_for_data(inp-trim_to_rect4(_T,_Rgt,_B,_L)).
%idea_for_data(out-trim_to_rect4).
%idea_for_data(inp-fpad_grid(f)).
%idea_for_data(out-fpad_grid(s)).
%idea_for_data(inp-subst_color(red,blue)).
%idea_for_data(both-bg_to_not_fg).
%idea_for_data(subst_all_fg_colors_with_vars(_,_)).
%idea_for_data(both-unbind_bg).
%idea_for_data(both-unbind_black).
%idea_for_data(both-constrain_grid(CntDwn,f,_CheckType)).
%idea_for_data(out-constrain_grid(CntDwn,s,_CheckType)).
%idea_for_data(inp-ogs_rotate(_,[rot2D(_P2)])).
%idea_for_data(both-into_monogrid).
*/
was_loose_ok(R):- \+ member(subst(_,_),R), \+ member(rotate(_),R), \+ member(into_monogrid(_),R).

ogs_rotate(_Trig,[rot2D(P2)],I,O):- rotP2(P2),P2\==rollD,grid_call(P2,I,O),I\=@=O.

simpl_ogs(In,Out):- my_maplist(prop_lists,In,Props), !, append(Props,Out).
simpl_ogs(IO,IO).

prop_lists(_-P,Props):-!,prop_lists(P,Props).
prop_lists(P,P):- is_list(P),!.
prop_lists(P,Props):- arg(2,P,Props),is_list(Props),!.
prop_lists(P,[P]).



maybe_if_changed(P2,I,O):- grid_call(P2,I,O),notrace(((I\=@=O,mass(O,Mass),Mass>0))).

skip_pps_port(Port):- t_l:pss_trace(List),!, \+ member(Port,List).
%skip_pps_port(Port):- Port \== exit,!.
skip_pps_port(_All).

trace_ogs(Port,P2,I,O):- \+ \+ ignore(pss1(Port,P2,I,O)).
pss1(Port,_,_,_):- skip_pps_port(Port),!.
pss1(Port,P2,I,O):-   
 (is_list(P2)->reverse(P2,P2R);P2=P2R),
 once(( nl_if_needed, dash_chars,dash_chars,
   pp(Port),pp(P2R), nl_if_needed,
   writeg(I),nl_if_needed, 
   print_side_by_side(wqs(P2R),I,O), writeg(O),nl_if_needed,
   dash_chars)).   
  
def_ogs_prog(
 [
 %must(inp-subst_all_fg_colors_with_vars(_,_)),
 %must(out-prepare_output_hooks(CheckTypes,_)),
 %must(inp-prepare_input_hooks(CheckTypes,_)),
 %must(out-bg_to_not_fg),
 %must(out-subst_color_auto(black,zero)),
 %must(out-fpad_grid(f,var)),
 %must(out-constrain_grid_now(CntDwn,f,Trig)),
 %must(out-fpad_grid(f,get_black)),
 %allow(inp-subst_color(red,blue)),

  disallow(inp-bg_to_not_fg),disallow(out-bg_to_not_fg),
 % allow(inp-trim_to_rect4),
  
  allow(all)]).


same_color_class(FG,NotBG):- is_fg_color(FG),!, \+ is_bg_color(NotBG).
same_color_class(BG,NotFG):- is_bg_color(BG),!, \+ is_fg_color(NotFG).

% unify allowing some failures
uaf(Fm,G1,G2):- uaf(Fm,0,_Fo,G1,G2).

uaf_split(Cmpd,H,T):- compound(Cmpd),uaf_split_2(Cmpd,H,T).
uaf_split_2([H|T],H,T):-!.
uaf_split_2(Cmpd,Name,Args):-compound_name_arguments(Cmpd,Name,Args).

% unify allowing some failures (cells only)
uaf(_Fm,Fn,Fn,Cell1,Cell2):- Cell1=Cell2,!.
uaf(Fm,Fi,Fo,Cmpd1,Cmpd2):- uaf_split(Cmpd1,H1,T1),uaf_split(Cmpd2,H2,T2),
  uaf(Fm,Fi,Fn,H1,H2), uaf(Fm,Fn,Fo,T1,T2).   
uaf(Fm,Fi,Fo,_,_):- plus(Fi,1,Fo),Fm>Fo.

% unify penalizing a resource each failure
upref(Resource,O1,O2):-
  resource_value(Resource,Value),
  uaf(Value,0,Used,O1,O2),
  ((Used>0) ->  
    (plus(Value,-Used,NewValue),b_set_resource(Resource,NewValue)) 
   ; true).

uaf_append(Fm,Fi,Fo,[], L1, L2):- uaf(Fm,Fi,Fo,L1,L2).
uaf_append(Fm,Fi,Fo,[H1|T], L, [H2|R]) :- uaf(Fm,Fi,Fn,H1,H2),
    uaf_append(Fm,Fn,Fo,T, L, R).

% append allowing some failures (cells only)

  



maybe_cvt_to_grid(In,Grid):- is_object(In),
   global_grid(In,Grid),!.


%pre_ogs_alter(maybe_unbind_bg).
pre_ogs_alter(P2):- rot_ogs(P2).

fg_to_bg(In,NewIn):- get_black(Black), \+ sub_var(Black,In), unique_colors(In,UCs),
 include(is_real_fg_color,UCs,FGC),!,FGC=[FG],subst(In,FG,Black,NewIn),!.


rot_ogs_step2(maybe_if_changed(subst_color(red,blue))).
rot_ogs_step2(maybe_if_changed(unbind_bg)).
%rot_ogs_step2(maybe_if_changed(colors_to_vars)).
rot_ogs_step1(maybe_if_changed(trim_to_rect)).
rot_ogs_step1([maybe_if_changed(trim_to_rect),Step2]):- rot_ogs_step2(Step2).

rot_ogs0(maybe_if_changed(unbind_bg)).

rot_ogs1(Step1):- rot_ogs_step1(Step1).
rot_ogs1(maybe_if_changed(P2)):- rotP0(P2).
rot_ogs1([Step1,maybe_if_changed(P2)]):- rot_ogs_step1(Step1),rotP2(P2).

rot_ogs(Step1):-rot_ogs1(Step1).
rot_ogs([Step0,Step1]):-rot_ogs0(Step0),rot_ogs1(Step1).


:- dynamic(tr:existing_result/3).

save_tr:- cls_z, 
 forall(tr:existing_result(X,Y,Z),
  format('~N~@.',[write_term(perfect_result(X,Y,Z),
    [quoted(true),quote_non_ascii(true),numbervars(false)])])).

got_result(SG,FG,Match):-
  copy_term(FG,CFG),copy_term(SG,CSG),
  numbervars(CSG+CFG,999,_,[attvar(bind)]),
  ignore((perfect_result(CSG,CFG,WMatch), 
    ((Match\==WMatch) -> (pp(red,'ChAnGED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n\n\n\n\n\n\n\n\n'),sleep(0.1)); pp(green,sameR)))),
  retractall(tr:existing_result(CSG,CFG,_)),
  arc_assert(tr:existing_result(CSG,CFG,Match)),!.
  
was_result(SG,FG,WMatch):-  
  copy_term(FG,CFG),copy_term(SG,CSG),
  numbervars(CSG+CFG,999,_,[attvar(bind)]),
  ignore((perfect_result(CSG,CFG,WMatch))).

:- asserta((prolog_stack:option(_,_):- fail)).

test_ogs2:- 
 with_tag_class('table','grid-to-grid', 
  (findall(T-SG,ss666(T,SG),TargetsR),
   reverse(TargetsR,Targets),
   findall(th(SG),member(_-SG,Targets),THs),
   html_table_row([th("test_ogs2&nbsp;"),th("&nbsp;find&nbsp;")|THs]),
   forall(ff666(W,F), once(print_ff_row(W,F,Targets))))).

print_ff_row(W,FG,Targets):-
  findall(R,(member(T-SG,Targets),once(test_ogs2_fs(W=T,FG,SG,R))),Results),
  html_table_row([th(FG),td(" ")|Results]).
test_ogs2_fs(_W=_T,FG,SG,TDO):-
  copy_term(SG,CSG),copy_term(FG,CFG),
  findall(at(H,V),find_ogs(H,V,CFG,CSG),R),
  TDO = td(pp(R)).

  
% hacking on this one
test_ogs2(H,V,Match):-
  ppnl("searching..."),
  ss666(T,SG),
  ff666(T,FG),
  copy_term(SG,CSG),copy_term(FG,CFG),
  %copy_term(SG,XSG),copy_term(FG,XFG),

  once((constrain_grid(CntDwn,f,CheckTypes,FG,XFG), constrain_grid(CntDwn,s,CheckTypes,SG,XSG))),

  ((ogs_1(H,V,XFG,XSG),CheckTypes=run) *-> 
   (show_match(H,V,FG,XSG),Match=true);
   (show_mismatch(XFG,XSG),Match=fail)),
  got_result(CSG,CFG,Match),
  Match==true.

% should still be the sameR
test_ogs1(H,V,Match):-
  Run = once(( print_side_by_side(FG,SG),nop(ppt(tf=T)))),
  ppnl("searching..."),
    ff666(T,UFG),      ss666(T,USG),
  copy_term(UFG,FG), copy_term(USG,SG),

  XSG = SG,
  get_black(Black),
  once((constrain_grid(cntDwn,f,CheckTypes,FG,XFG))),
  
  once((grid_detect_bg(SG,Background), my_maplist(cbg,Background))),

  ((ogs_11(H,V,XFG,XSG),CheckTypes=Black) *-> Match=true; Match=false),

  (Match==true->show_match(H,V,Run,XFG,FG);show_mismatch(XFG,Run,FG)),
  ignore(got_result(SG,FG,Match)),
  Match==true.



% should still be the sameR
test_ogs0(H,V,Match):-
  Run = once(( print_side_by_side(FG,SG),print(test_ogs0(H,V,TMatch)),nop(ppt(tf=T)))),
  ppnl("searching..."),
    ff666(T,UFG),      ss666(T,USG),
  copy_term(UFG,FG), copy_term(USG,SG),

  ground(UFG),
  get_black(Black),
  once((constrain_grid(CntDwn,f,CheckTypes,FG,XFG), nop(constrain_grid(CntDwn,s,CheckTypes,SG,XSG)))),
  once((grid_detect_bg(XSG,Background), my_maplist(cbg,Background))),
  
  ((ogs_11(H,V,XFG,XSG),CheckTypes=Black) *-> TMatch=true; TMatch=false),

  was_result(SG,FG,WMatch), 
  (WMatch\==TMatch ; TMatch == Match),
  dash_chars,
  ignore(got_result(SG,FG,TMatch)),
  (TMatch==true->show_match(H,V,Run,XFG,FG);show_mismatch(XFG,Run,FG)),
  ignore(got_result(SG,FG,TMatch)),
  dash_chars,
  Match==true.


test_ogs(H,V,Match):- 
  Run = once(( print_side_by_side(FG,XSG),ppa(FG),ppa(XSG),nop(ppt(tf=T)))),
  ppnl("searching..."),
    ff666(_,FG),
    ss666(T,SG),
    
  copy_term(FG,XFG), copy_term(SG,XSG),
 % ground(FG),
  
  ((find_ogs(H,V,XFG,XSG)) *-> TMatch=true; TMatch=false),
  was_result(SG,FG,WMatch), (WMatch\==TMatch ; TMatch == Match),
  dash_chars,
  ignore(got_result(SG,FG,TMatch)),
   if_t(SG\=@=XSG,print_side_by_side(SG,XSG)),
  (TMatch==true->show_match(H,V,Run,XFG,FG);show_mismatch(XFG,Run,FG)),
  ignore(got_result(SG,FG,TMatch)),
  dash_chars,
  nop((fail, Match==true)).


% get_prolog_backtrace(3,StackFrames,[]), nth0(2, StackFrames, SFrame) , prolog_stack_frame_property(SFrame, location(File:Line)),

grid_minus_grid(I,O,In):- is_grid(I),is_grid(O),!,mapgrid(cell_minus_cell,I,O,In).
grid_minus_grid(B,A,OI):- vis2D(B,BH,BV),vis2D(A,AH,AV),(BH\==AH;BV\==AV),!,OI=B.
grid_minus_grid(B,A,OI):- remove_global_points(A,B,OI),!.
%grid_minus_grid(B,A,OI):- is_list(B),my_maplist(grid_minus_grid,B,A,OI).
%grid_minus_grid(B,A,C):- ignore(grid_minus_grid0(B,A,C)).
%grid_minus_grid(B,_,B):- !.
%grid_minus_grid0(B,A,OI):- B==A,!, OI=black.
%grid_minus_grid0(B,A,OI):- nonvar(B),nonvar(A),IO=black.
%grid_minus_grid0(B,A,OI):- nonvar(B),IO=B.
%grid_minus_grid0(B,A,B):- !.
/*

grid_minus_grid(B,A,OI):- 
  pred_global_points(point_minus_point(A,B),A,B,OI).

point_minus_point(A,B,H,V,C,Old,Grid,Grid):- nonvar(Old),nonvar(C),Old\==C,nop(point_minus_point(A,B,H,V,C,Old,Grid,Grid)).
point_minus_point(A,B,H,V,C,Old,Grid,Grid):-  nth1(V,Grid,Row),nb_set_nth1(H,Row,black),!, nop(point_minus_point(A,B,H,V,C,Old,Grid,Grid)).
*/

h666(_,G):- fail,ff666(_,G0),
  flipV(G0,GV),
  flipH(G0,GH),
  append([GH,GV],G1),
  fpad_grid(f,var,G1,G). 

%f666(Ham,G0):-  clause(f666(Ham,F),true),into_g666(F,G),must_det_ll(all_rotations(G,G0)).

show_mismatch(F,G):-  %fail, 
  nl,dash_chars,
  show_m_pair(false,color_print(yellow,"Mismatched"),1,1,F,G),
  nl,dash_chars,!.

show_mismatch(F,C,G):- %fail, 
  nl,dash_chars,
  show_m_pair(false,(color_print(yellow,"Mismatched"),C),1,1,F,G),
  nl,dash_chars,!.

show_match(H,V,F,G):- 
  nl,dash_chars,
  show_m_pair(true,color_print(green,'Matched'(H,V)),H,V,F,G),
  nl,dash_chars,!.

show_match(H,V,C,F,G):- 
  nl,dash_chars,
  show_m_pair(true,(color_print(green,'Matched'(H,V)),C),H,V,F,G),
  nl,dash_chars,!.

show_m_pair(_TF,S,H,V,F,G):- 
 my_maplist(must_det_ll,
 [H2 is H-3, V2 is V-3,
  offset_grid(H2,V2,F,OF),
  constrain_grid(CntDwn,f,_TrigF,OF,FF),!, 
  print_grid("find",FF),
  writeg(FF),
  dash_chars(60,' '),ignore(call(S)),nl,
  constrain_grid(CntDwn,s,_TrigG,G,CG),!,
  print_grid("find on",CG)]),!,
  writeg(CG).


print_fgrid(GH,GV,F):- ((\+ \+ ((constrain_grid(cntDwn,f,_Trig,F,_FG),print_grid(GH,GV,F),nl)))),!.
print_sgrid(F):- ((\+ \+ ((constrain_grid(cntDwn,s,_Trig,F,_FG),print_grid(F),nl)))),!.



%constrain_type(Var,Cond):- nonvar(Var),!,call(Cond).
%constrain_type(Var,Cond):- frozen(Var,Goals),sub_term(E,Goals),E=@=Cond,!. % u_dmsg(skipping(Cond)),atrace.
constrain_type(Var,Cond):- freeze(Var,Cond).

find_ogs(H,V,FG,SG):- luser_getval(find_rule,Rul),find_ogs_c(Rul,H,V,FG,SG).

find_ogs_c(_,_,_,_FG,SG):- SG==[],!,fail.
find_ogs_c(Rul,H,V,FG,SG):- 
  %constrain_grid(CntDwn,f,CheckTypes,FG,_XFG2),
  locally(nb_setval(find_rule,Rul),
    once((constrain_grid(CntDwn,f,CheckTypes,FG,XFG), constrain_grid(CntDwn,s,CheckTypes,SG,XSG)))),
  get_black(Black),
  ogs_1(H,V,XFG,XSG),CheckTypes=Black.
/*

find_ogs(H,V,FG,SG):-
  %constrain_type(CheckTypes,FG,FG,XFG),!,
  ogs_0(CheckTypes,H,V,FG,SG),
  CheckTypes=run.
*/

ogs_0(CheckTypes,H,V,FG,SG):-
  %constrain_type(CheckTypes,Grid,FG,XFG),
  once((constrain_grid(CntDwn,f,CheckTypes,FG,XFG),
        constrain_grid(CntDwn,s,CheckTypes,SG,XSG))),
  ogs_1(H,V,XFG,XSG).
  %CheckTypes=run,
  %print_grid(XFG),nl,
  %ppnl(found_at(H,V)),
  %print_grid(XSG),nl,
  %true.


ogs_1(H,V,FindI,Search):- \+ is_grid(FindI),!,maybe_cvt_to_grid(FindI,Grid),ogs_1(H,V,Grid,Search).
ogs_1(H,V,FindI,Search):- (\+ number(H); \+ number(V)), !, ogs_11(H,V,FindI,Search).
ogs_1(Hi,Vi,Find,Search):-
  H is Hi - 1, V is Vi - 1,  
  length(LPad,H),
  length(VPad,V),!,
  append(VPad,[LPadAndRow|Next],Search),
  Find = [R1|FGrid],
  append(R1,_,Rho),
  append(LPad,Rho,LPadAndRow),
  ogs_pt2(H,FGrid,Next),!.


ogs_11(H,V,FindI,Search):- \+ is_grid(FindI),!,maybe_cvt_to_grid(FindI,Grid),ogs_11(H,V,Grid,Search).
ogs_11(H,V,FindI,Search):-
  vis2D(Search,SH,SV),
  vis2D(FindI,FH,FV),
  MH is SH - FH,MV is SV - FV,
  ogs_2(Hi,Vi,MH,MV,FindI,Search),
  H is Hi + 1,
  V is Vi + 1.

ogs_2(H,V,MH,MV,[R1|FGrid],Search):-  
  append(R1,_,Rho),!,
  append(VPad,[LPadAndRow|Next],Search),
  length(VPad,V),
  %between(0,MV,V),
  ((V>MV) -> (!, fail) ; true),
  between(0,MH,H),
  once((length(LPad,H),
  append(LPad,Rho,LPadAndRow),
  once((ogs_pt2(H,FGrid,Next),
        length(VPad,V))))).

ogs_pt2(_,[],_):-!.
ogs_pt2(H,[Row|FindRows],[S|Search]):-
  length(LPad2,H),append(LPad2,Row,LPadRow2),
  append(LPadRow2,_,S),!,
  ogs_pt2(H,FindRows,Search),!.


grid_detect_bg(Grid1,[bg|Background]):- 
  term_singletons(Grid1,Background).

  
grid_label_bg(CT,GridIn,GridO):- CT=f,!,
  copy_term(GridIn,Grid1),
  grid_detect_bg(Grid1,Background),
  my_maplist(to_grid_bg(CT,Grid1),Background),
  get_dc_label(BGL),
  get_bgc(BG),subst001(Grid1,BG,BGL,GridO),!.
grid_label_bg(CT,GridIn,GridO):- CT=s,!,
  copy_term(GridIn,Grid1),
  grid_detect_bg(Grid1,Background),
  my_maplist(to_grid_bg(CT,Grid1),Background),
  get_dc_label(BGL),
  get_bgc(BG),subst001(Grid1,BG,BGL,GridO),!.
grid_label_bg(CT,GridIn,GridO):- 
  copy_term(GridIn,Grid1),
  grid_detect_bg(Grid1,Background),
  my_maplist(to_grid_bg(CT,Grid1),Background),
  get_dc_label(BGL),
  get_bgc(BG),subst001(Grid1,BG,BGL,GridO),!.
grid_label_bg(_,GridO,GridO):-!.

to_grid_bg(_CT,_,E):- cant_be_color(E),!.
to_grid_bg(_CT,_,BG):- get_bgc(BG),!.
to_grid_bg(_CT,_,BG):- bg_sym(BG),!.
to_grid_bg(_CT,_,_).

grid_detect_fg_vars(GridIn,Foreground1):- 
  grid_detect_bg(GridIn,Background),!,
  term_variables(GridIn,Foreground),!,
  include(not_in(Background),Foreground,Foreground1).
  

grid_label_fg(CT,GridIn):- 
  my_assertion(is_grid(GridIn)),
  grid_detect_fg_vars(GridIn,Foreground1),
  grid_label_fg(CT,GridIn,Foreground1),!.

grid_label_fg(_CT,_,[]):-!.
grid_label_fg(CT,GridIn,Foreground1):- 
  copy_term(Foreground1,ForegroundCopy),
  numbervars(ForegroundCopy,2021,_,[attvar(skip)]),
  my_maplist(to_grid_fg(CT,GridIn),Foreground1,ForegroundCopy),!.

%maybe_grid_numbervars(GridIn,GridIn):-!.
%maybe_grid_numbervars(O,I):- is_vm_map(O),append(O.objs,[O.grid],I),!.
maybe_grid_numbervars(GridIn,GridIn):- \+ is_grid(GridIn),!.
maybe_grid_numbervars(GridI,GridIn):- grid_numbervars(GridI,GridIn),!.
maybe_grid_numbervars(GridIn,GridIn):-!.

not_in(Background,Foreground):-
  \+ (member(E,Background), E == Foreground). 
not_sub_var(Background,Foreground):-
  \+ sub_var(Foreground,Background).

to_grid_fg(_CT,_,E,_):- cant_be_color(E),!.
to_grid_fg(_CT,_,N,'$VAR'(N)):-!.
to_grid_fg(_CT,_,B,B).

grid_numbervars(GridIn,GridO):- 
 must_det_ll((grid_label_bg(p,GridIn,GridO),grid_label_fg(p,GridO))).





offset_grid(H2,V2,FF,OF):-
  offset_v_grid(V2,FF,FF0),
  c_r(offset_v_grid(H2),FF0,OF).

offset_v_grid(V2,FF,OF):-  
  vis2D(FF,GW,_), 
  offset_v_grid_row(GW,V2,FF,OF).

offset_v_grid_row(_,V2,FF,FF):- V2<0,!.
offset_v_grid_row(GW,V2,FF,[Row|OF]):- V1 is V2-1,
   length(Row,GW), offset_v_grid_row(GW,V1,FF,OF).
   
  
get_dc_label(bg).

%pad_with_contraints_3(GridO,TODO):-
%  vis2D(GridO,HH,VV),
%  pad_with_contraints_3(GridO,HH,VV,TODO),!.
fpad_grid(CT,Before,After):-
  get_dc_label(BGL),sub_var(BGL,Before),!,
  fpad_grid(CT,=(BGL),Before,After).

fpad_grid(CT,Before,After):-  
  get_dc_label(BGL),
  fpad_grid(CT,=(BGL),Before,After).
fpad_grid(CT,P1,O,GridO):- is_object(O),!,object_grid(O,GridIn),!,fpad_grid(CT,P1,GridIn,GridO).
fpad_grid(_CT,P1,Grid,Grid2):-
  vis2D(Grid,H,_), H2 is H +2,
  length(T,H2),my_maplist(P1,T),
  length(B,H2),my_maplist(P1,B),
  my_maplist(pad_sides(P1),Grid,FillRows),
  append([T|FillRows],[B],Grid2).


maybe_into_grid(I,O):- \+ is_grid(I), into_grid(I,O), I \=@=O,!.


%constrain_grid_f(Grid2,GridO):- Grid2=GridO.
%constrain_grid_f(Grid2,Trig,GridO):- constrain_grid(CntDwn,f,Trig,Grid2,GridO),!.
%constrain_grid_s(Grid2,Trig,GridO):- constrain_grid(CntDwn,s,Trig,Grid2,GridO),!.
constrain_grid(CntDwn,CT,Trig,Obj,GridO):- maybe_into_grid(Obj,Grid),!,constrain_grid(CntDwn,CT,Trig,Grid,GridO),!.
constrain_grid(_CntDwn,CT,_Trig,Grid,GridO):- CT==sameR,!,Grid=GridO.
constrain_grid(_CntDwn,CT,_Trig,Grid,GridO):- CT==copy,!,copy_term(Grid,GridO).


constrain_grid(CntDwn,CT,_Trig,[[FC]], Grid1):- CT==f, !,   
  Grid1=[[NW,N0,NE],
         [W0,FC,E0],
         [SW,S0,SE]],
 my_maplist(dif_violate(CntDwn,FC),[NE,NW,SW,SE,N0,S0,E0,W0]).
/*OLD

constrain_grid(CntDwn,CT,Trig,[[FC]], GridO):- CT==f(Rul), !,   
  Grid1=[[NW,N0,NE],
         [N1,FG,N3],
         [SW,N2,SE]],
 Grid2=[[NWL, _, _, _, NEL],
         [_ , NW,N0,NE ,_ ],
         [_ , N1,FG,N3 ,_ ],
         [_ , SW,N2,SE ,_ ],
        [SWL, _, _, _, SEL]],
 my_maplist(dif_violate(CntDwn,FG),[NE,NW,SW,SE]),
 nop((freeze(Trig,\+ (NE==SW, dif_violate(CntDwn,FG,NE))),
 freeze(Trig,\+ (NW==SE, dif_violate(CntDwn,FG,SE))),
 freeze(Trig,\+ (NW==NWL,dif_violate(CntDwn,FG,NW))),
 freeze(Trig,\+ (SW==SWL,dif_violate(CntDwn,FG,SW))),
 freeze(Trig,\+ (NE==NEL,dif_violate(CntDwn,FG,NE))),
 freeze(Trig,\+ (SE==SEL,dif_violate(CntDwn,FG,SE))),
 freeze(GridO,(Grid2=GridO;GridO=Grid1)))),
 GridO = Grid2,
 my_maplist(dif_violate(CntDwn,FG),[N0,N1,N2,N3]),
 freeze(Trig,FC=FG).
*/

constrain_grid(CntDwn,CT,Trig,Grid1,GridO):- 
  fpad_grid(CT,Grid1,Grid2),
  grid_label_bg(CT,Grid2,Grid3),
  must_det_ll(constrain_grid_now(CntDwn,CT,Trig,Grid3,GridO)),!.
  


release_bg(CT,Trig,Grid2,GridO):- must_det_ll((release_bg0(CT,Trig,Grid2,GridO))),!.
release_bg0(CT,Trig,GridIn,GridO):- is_list(GridIn), !, my_maplist(release_bg0(CT,Trig),GridIn,GridO).
release_bg0(_CT,_Trig,GridIn,GridIn):- attvar(GridIn),!.
release_bg0(_CT,_Trig,GridIn,GridIn):- plain_var(GridIn),!.
%release_bg0(CT,Trig,GridIn-P,GridO-P):- !, release_bg0(CT,Trig,GridIn,GridO).
release_bg0(_CT,_Trig,BG,C):- is_bg_color(BG),!,decl_bg_color(C).
release_bg0(_CT,_Trig,C1I,C):- is_spec_fg_color(C1I,C),!.
release_bg0(_CT,_Trig,C,C).

%constrain_grid_now(CntDwn,CT,Trig,GridIn, GridO):- GridIn= GridO,!.

constrain_grid_now(CntDwn,CT,Trig,GridIn, GridO):-
  vis2D(GridIn,H,V), make_grid(H,V, GridO),
  maybe_constrain_fg(Trig,GridIn),
  constrain_grid_now(CntDwn,CT,Trig,GridIn,H,V,H,V,GridO),!.

maybe_constrain_fg(_Trig,GridIn):- 
  grid_detect_fg_vars(GridIn,FGUnits),
  ignore((FGUnits\==[],
          set_fg_vars(FGUnits))),!.

constrain_grid_now(_CntDwn,_CT,_Trig,_GridIn,_Hi,0,_GH,_GV,_GridO):-!.
constrain_grid_now(CntDwn,CT,Trig,GridIn,Hi,Vi,GH,GV,GridO):-
  get_color_at(Hi,Vi,GridIn,C1I),
  get_color_at(Hi,Vi,GridO,C1O),
  constrain_ele(CntDwn,CT,GH,GV,Trig,GridIn,Hi,Vi,C1I,C1O,GridO),
  (Hi==1 -> (H = GH, V is Vi-1) ; (H is Hi -1, V=Vi)),!,
  constrain_grid_now(CntDwn,CT,Trig,GridIn,H,V,GH,GV,GridO).

% Out of bounds on Source Canvas
constrain_ele(_CntDwn,s,GH,GV,_Trig,_GridIn,H,V,_C1I,_C1O,_GridO):- (H==1;V==1;V==GV;H==GH),!.
% FG Source Canvas
constrain_ele(_CntDwn,s,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- nonvar(C1I), nonvar(C1O),!, C1O\==C1I,fail.
constrain_ele(_CntDwn,s,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- is_spec_fg_color(C1I,_),!, 
  %constrain_type(C1O, \+ is_bg_color(C1O)),
  C1I=C1O.
% BG Source Canvas
constrain_ele(_CntDwn,s,_GH,_GV,Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- is_bg_color(C1I),!,
  %constrain_type(C1O, \+ is_fg_color(C1O)),
  constrain_type(Trig, (\+ is_fg_color(C1O))),
  !.
constrain_ele(_CntDwn,s,_GH,_GV,Trig,_GridIn,_H,_V,_C1I,C1O,_GridO):- % unspecified
  %constrain_type(C1O, \+ is_fg_color(C1O)),
  constrain_type(Trig, (\+ is_fg_color(C1O))),
  !.

% BG Find On Canvas
%constrain_ele(CntDwn,f,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,_C1O,_GridO):- is_bg_color(C1I),!.
% Find FG On Canvas
constrain_ele(_CntDwn,f,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- nonvar(C1I), nonvar(C1O),!, C1O\==C1I,fail.
constrain_ele(_CntDwn,f,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- fail, is_spec_fg_color(C1I,_),nonvar(C1I),!, 
  (C1O==C1I -> true ; C1O=C1I).


constrain_ele(CntDwn,f,_GH,_GV,Trig,GridIn,H,V,C1I,C1O,GridO):- is_spec_fg_color(C1I,_),!, 
  C1I=C1O,
  ((C1O==C1I) -> true ; must_det_ll((constrain_type(Trig,C1I=C1O), attach_fg_ci(C1O,C1I)))), 
  constrain_dir_ele(CntDwn,f,Trig,[n,s,e,w],GridIn,H,V,C1I,C1O,GridO).

% BG Find On Canvas
constrain_ele(_CntDwn,f,GH,GV,_Trig,_GridIn,H,V,C1I,_C1O,_GridO):- 
   H\==1, V\==1, V\==GV, H\==GH,
   is_bg_color(C1I),!.
   

  % UNKNOWN
constrain_ele(_CntDwn,_CT,_GH,_GV,_Trig,_GridIn,_H,_V,_C1I,_C1O,_GridO).

%same_colors(C1I,C1O):- is_spec_fg_color(C1O,_),!,C1I=C1O.




count_o_neighbors(C,H,V,Dirs,GridIn):- 
  muarc_mod(M),
  findall(Dir,
    (M:is_adjacent_hv(H,V,Dir,H2,V2),
     get_color_at(H2,V2,GridIn,C1O),
       is_spec_fg_color(C1O,_),C1O\=@=C), 
    Dirs).

count_c_neighbors(C,H,V,Dirs,GridIn):- 
  muarc_mod(M),
  findall(Dir,
    (M:is_adjacent_hv(H,V,Dir,H2,V2),
     get_color_at(H2,V2,GridIn,C1O),
       C1O=@=C), 
    Dirs).


%constrain_dir_ele(CntDwn,CT,Trig,[_|SEW],GridIn,H,V,GridO):- constrain_dir_ele(CntDwn,CT,Trig,SEW,GridIn,H,V,GridO).

mfreeze(Trig,_CDE):- nonvar(Trig),!.
mfreeze(Trig,CDE):- constrain_type(Trig,CDE).

constrain_dir_ele(_CntDwn,_CT,_Trig,_,_GridIn,_H,_V,_C1I,_C1O,_GridO):- luser_getval(find_rule,loose),!.
constrain_dir_ele(_CntDwn,_CT,_Trig,[],_GridIn,_H,_V,_C1I,_C1O,_GridO).

/*
constrain_dir_ele(CntDwn,CT, Trig,[Dir|SEW],GridIn,H,V,C1I,C1O,GridO):- luser_getval(find_rule,strict),!,
  muarc_mod(M),
  ignore((
  M:is_adjacent_hv(H,V,Dir,H2,V2),
  get_color_at(H2,V2,GridIn,C2I),
  get_color_at(H2,V2,GridO,C2O),
     \+ is_spec_fg_color(C2I,_),
     dif_violate(CntDwn,C2O,C1O))),!,
  constrain_dir_ele(CntDwn,CT, Trig,SEW,GridIn,H,V,C1I,C1O,GridO).
*/

constrain_dir_ele(CntDwn,CT, Trig,[Dir|SEW],GridIn,H,V,C1I,C1O,GridO):-
  muarc_mod(M),
  ignore((
  M:is_adjacent_hv(H,V,Dir,H2,V2),
  get_color_at(H2,V2,GridIn,C2I),
  get_color_at(H2,V2,GridO,C2O),
     \+ is_spec_fg_color(C2I,_),
     count_o_neighbors(C1I,H2,V2,DCN,GridIn),
     count_c_neighbors(C1I,H2,V2,SCN,GridIn),
     sort_safe(SCN,SCNS),
     o_c_n(CT,Dir,DCN,SCNS),     
     dif_violate(CntDwn,C2O,C1O))),!,
  constrain_dir_ele(CntDwn,CT, Trig,SEW,GridIn,H,V,C1I,C1O,GridO).

:- use_module(kaggle_arc_ndif).
dif_violate(CntDwn,_X,_Y) :- CntDwn==sloose,!.
dif_violate(CntDwn,X, Y) :- CntDwn==0,!,ndif(X,Y).
dif_violate(CntDwn,X, Y) :- 
  freeze(X,check_dif_violate(CntDwn,X, Y)),
  freeze(Y,check_dif_violate(CntDwn,X, Y)).

check_dif_violate(_CntDwn, X, Y):- (var(X) ; var(Y)),!.
check_dif_violate(_CntDwn, X, Y):- X\==Y,!.
check_dif_violate(CntDwn, _, _):- nb_current(CntDwn,V),number(V),V>0,VV is V-1,!, b_setval(CntDwn,VV).
check_dif_violate(CntDwn, _, _):- b_setval(CntDwn,7).


% o_c_n(CT,Dir,DCN,SCN),     
o_c_n(f,_,[],[_]):-!,fail. % no neighbours and just self
o_c_n(f,_,[_,_|_],[]):-!,fail. % no neighbours and just self
%o_c_n(f,_,[_,_|_],_):-!,fail.
o_c_n(f,_,[_],_):-!,fail.
o_c_n(f,_,_,_).
o_c_n(s,_,_,_).

g666(_,Y):- in_shape_lib(l_shape,X),
  once((object_grid(X,G),fpad_grid(f,G,Y))).


make_row(Rows,FV):- functor(P,v,FV), P=..[v|Rows].

% _______________
h666(g1,"|     Y Y Y     |
  | G G Y v Y G G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | . ® Y v Y ® . |
  | . . Y v Y . . |
  | . ® Y v Y ® . |
  | G ® Y v Y ® G |
  | G G Y v Y G G |
  |     Y Y Y     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
").
h666(g2,"
   _______________
  |     Y Y Y     |
  | G G Y v Y G G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G ® Y v Y ® G |
  | G G Y v Y G G |
  |     Y Y Y     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ ").

h666(g3,"
   _______________
  |     . . .     |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | . . . . . . . |
  | G . . . . . G |
  | G ® . . . ® G |
  | G . . . . . G |
  | . . . . . . . |
  | . . . . . . . |
  |     . . .     |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
").

h666(g4,"
   _______________
  | ½           ½ |
  | ½ ½       ½ ½ |
  | ½           ½ |
   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
").

test_make_predicate:-
  test_make_predicate([g1,g2,g3]),
  test_make_predicate([g3,g4]),
  test_make_predicate([g1,g2,g4]).

test_make_predicate(List):-
  my_maplist(ss666,List,Grids),
  gensym('newpred_',NewPred),
  Pred =.. [NewPred|Grids],
  print_ss(Grids),
  assert_if_new(pred_impl(Pred)).


f666(_Color,
[[4,5,1],
 [4,5,1],
 [4,5,1]]).

f666(_Ham,G):- the_hammer1(G).
f666(_Ham,G):- in_shape_lib(hammer,Ham),object_grid(Ham,G0),all_rotations(G0,G).

ham_h_s("_________________________________________________
      B B B                           B B B       
        B B                             B B      
        B       B                       B       B
        B B B B B                 R R   B B B B B
        B B   B B                 R R R B B   B B
          B                       R       B      
        B B                             B B      
        B B B                           B B B     
__________________________________________________").

ham_h_s2(
"_________________________________________________
 0 0 B B B 0 0 0 0 0 0 0 0 0 0 0 0 0 B B B 0 0 0 0
 0 0 0 B B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 B B 0 0 0 0
 0 0 0 B 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 B 0 0 0 B 0
 0 0 0 B B B B B 0 0 0 0 0 0 0 0 R R 0 B B B B B 0
 0 0 0 B B 0 B B 0 0 0 0 0 0 0 0 R R R B B 0 B B 0
 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 R 0 0 0 B 0 0 0 0
 0 0 0 B B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 B B 0 0 0 0
 0 0 0 B B B 0 0 0 0 0 0 0 0 0 0 0 0 0 B B B 0 0 0
 _________________________________________________").

s666(_Ham,G):-ham_h_s2(TG),into_grid(TG,G).
%s666(_Ham,G):-ham_h_s(TG),into_grid(TG,G).

f666(_Ham,G):-clause(hamstring(G),Body),notrace(Body).

hamstring(G):-
S="
 _________
 B B B B B 
 B B B B B 
 0 0 B 0 0
 0 0 B 0 0
 _________",
 must_det_ll((text_to_grid(S,SG),print_grid(SG),into_g666(SG,G1),Color=blue,once(subst001(G1,blue,Color,G)))).

hamstring2(G):- 
S="
 _________
 B B B B B 
 B B B B B 
 0 0 B 0 0
 0 0 B 0 0
 _________",
 must_det_ll((text_to_grid(S,SG),into_g666(SG,G1),Same=_,once(subst001(G1,blue,Same,G)))).

hamstring3(G):- hamstring(G1),subst001(G1,'blue',red,G).
%hamstring4(G):- hamstring(G1),subst001(G1,'#6666FF',_Same,G).


h666(_Ham,G):- into_grid(v(aa4ec2a5)>(tst+0)*in,G).


ham_h_p(
"_________________________________________________
         B B B B B               B B B B B
         B B B B B               B B B B B
             B     B B       R R     B     B B
             B     B B       R R     B     B B
             B B B B B       R R R R B B B B B
             B     B B       R R     B     B B
             B     B B       R R     B     B B
         B B B B B               B B B B B
         B B B B B               B B B B B
 _________________________________________________").

h666(_Ham,G):-ham_h_p(TG),into_grid(TG,G).

trans_to_color('B','blue').
trans_to_color('R','red').
trans_to_color('G','green').
trans_to_color('O','orange').
trans_to_color('®','red').
trans_to_color('Y','yellow').
trans_to_color('b','brown').
trans_to_color('f','fg').
trans_to_color('v','purple').
trans_to_color('½','cyan').
trans_to_color(S,Name):- c2s(CN,S),color_name(CN,Name).
trans_to_color(' ',Black):- get_black(Black).


into_grid_color(L,O):- is_list(L),!,mapgroup(into_grid_color,L,O).
into_grid_color(L,O):- plain_var(L),!,L=O.
into_grid_color(L,O):- color_code(L,O),!.
into_grid_color(O,O).

into_g666(Nil,_):- Nil==[],!,fail.
into_g666(Text,G):- string(Text),maybe_fix_ascii(Text,Ascii0),!,into_g666(Ascii0,G).
into_g666([Nil|Grid],G):- Nil==[], is_grid(Grid),!,mapgrid(into_grid_color,Grid,G),!.
into_g666(String,G):- string(String),!,text_to_grid(String,TG),!,into_g666(TG,G),!.
into_g666(Grid,G):- is_grid_of(integer,Grid),!,mapgrid(into_grid_color,Grid,G),!.
into_g666(Grid,G):- is_grid(Grid),!,mapgrid(into_grid_color,Grid,G),!.
into_g666(Obj,G):- is_object(Obj),!,must_det_ll(object_grid(Obj,OG)),!,into_g666(OG,G).

%into_g666(Text,G):- atomic(Text),maybe_fix_ascii(Text,Ascii0),!,into_g666(Ascii0,G).
into_g666(Text,G):-  notrace(catch(text_to_string(Text,String),_,fail)),!,into_g666(String,G).
into_g666(Other,G):- u_dmsg(failed(into_g666(Other,G))),!,fail.

ss666(T,G):- h666(T,S),must_det_ll(into_g666(S,G)).
sp666(T,Y):- ss666(T,X), fpad_grid(s,X,Y).

ff666(T,G0):- no_repeats(G0,((f666(T,F),into_g666(F,G),all_rotations(G,G0)))).
fp666(T,Y):- ff666(T,X), fpad_grid(f,X,Y).

%maybe_fix_ascii(Text,Ascii0):- trim_leading_trailing_whitespace(Text,Ascii0),Text\==Ascii0,!.

maybe_fix_ascii(Text,Chars):- is_charlist(Text),!,notrace(catch(text_to_string(Text,String),_,fail)),!,
   maybe_fix_ascii(String,Ascii0),
   format(chars(Chars),'~w',[Ascii0]).

maybe_fix_ascii(Text,Ascii0):- Text\==[],
 replace_in_string([
  % windows /     mac
   '\r\n'='\n','\r'='\n',
   '$ '='$','$\n'='\n','$'=''],Text,Ascii0),!,
 Text\==Ascii0,!.

maybe_fix_ascii(Text,Ascii0):- fail,
 replace_in_string([ 
   '\r'='\n','\n\n'='\n','$ '='$','$\n'='\n','$'='','\n '='\n','_\n'='_',' _'='_'],Text,Ascii0),!,
 Text\==Ascii0,!.

ascii_to_growthchart(Text,G):- maybe_fix_ascii(Text,Ascii0), !, ascii_to_growthchart(Ascii0,G).
ascii_to_growthchart(Text,GrowthChart):-  
  atomics_to_string(Rows1,'\n',Text),Rows1=[_|Rows],mapgroup(atom_chars,Rows,GrowthChart),!.

:- luser_default(find_rule,regular).
% ?- h666(X),text_to_grid(X,G).


text_to_grid(Text,GO):- ascii_to_grid(Text,GO),!.
text_to_grid(Text,G):- ensure_charlist(Text,Chars),!,text_to_grid(Chars,G).

ascii_to_grid(Text,G):- maybe_fix_ascii(Text,Ascii0), !, ascii_to_grid(Ascii0,G).
ascii_to_grid(Text,G):- %atom_contains(Text,'____'),!, 
   ascii_append_grid(Text,[],G),!.
ascii_to_grid(Text,G):- 
 ascii_to_growthchart(Text,GrowthChart),
 growthchart_to_grid(GrowthChart,6,5,G).

text_to_points(Text,GPs):- 
  ascii_to_grid(Text,G),
  print_grid(ag,G),
  % grid_to_tid(G,ID),
  globalpoints_include_bg(G,GPs),!.

text_to_grid(Text,HH,VV,ObjPoints,GO):-  
 must_det_ll((
  text_to_points(Text,GPs),!,
  points_range(GPs,_LoH_,_LoV_,HiH,HiV,_,_),
  LoH=1,LoV=1,
  deoffset_points(LoH,LoV,GPs,ObjPoints),  
  %print_grid(ObjPoints),
  HH is HiH - LoH + 1,
  VV is HiV - LoV + 1,  
  points_to_grid_safe(HH,VV,ObjPoints,GO))),!.

points_to_grid_safe(HH,VV,ObjPoints,GO):- nl,
  %writeq(points_to_grid_safe(HH,VV,ObjPoints,GO)),nl,
  points_to_grid(HH,VV,ObjPoints,GO),!.

suggest_row_cells_seps(['\n','|',' '],[' ']).
suggest_row_cells_seps(['|',' '],[' ']).
suggest_row_cells_seps(['|'],[]).
suggest_row_cells_seps(['\n'],[]).


detect_ascii_grid_style(Text,Style):- ensure_charlist(Text,Chars),!,detect_ascii_grid_style(Chars,Style).
detect_ascii_grid_style(Text,Style):- append([_,['¯'],Len,['¯'],NoTopper],Text),\+ member('¯',NoTopper),my_maplist(==('¯'),Len),
  length([_,Len],LL),Width is floor(LL/2), Style=pttgS(Width).
detect_ascii_grid_style(AllText,Style):-
  suggest_row_cells_seps(RowSep,CellSep),
  Style = g_style(RowSep,CellSep,BlackStyle,VarStyle),
  next_row(strict,Style,AllText,RowText,MoreText), RowText \==[], MoreText \==[],
  notrace(read_cell(Style,RowText,Cell,_)),
  Cell \= unreadable(_),
  ignore((member(BlackStyle,AllText),member(BlackStyle,['0','.',' ']),
  ignore((member(VarStyle,AllText),VarStyle\==BlackStyle,member(VarStyle,['?',' ']))))),!.
detect_ascii_grid_style(Text,Style):- \+ member('|',Text),!, Style=pttgS(_).
detect_ascii_grid_style(Text,g_style(['|'],[' '],' ','?')):- member('?',Text),!.
%detect_ascii_grid_style(Text,g_style(['|'],[' '],' ','.')):- member('.',Text),!.
detect_ascii_grid_style(_Text,g_style(['|'],[' '],'.',' ')).
detect_ascii_grid_style(_Text,g_style(['|'],[],'0',' ')).

/*ensure_charlist(Text,Chars):- member('\n',Text),member('|',Text), 
  split_string(Text, ", ", "|\s\t\n",List), 
  atomics_to_string(List,'|',Out).
*/
ensure_charlist(Text,Chars):- notrace((Text\==[], \+ is_charlist(Text),format(chars(Chars),'~w',[Text]))).

ascii_append_grid(Text,Start,Grid):- detect_ascii_grid_style(Text,Style), ascii_append_grid(Style,Text,Start,Grid),!.
ascii_append_grid(Text,Start,Grid):-  must_det_ll((detect_ascii_grid_style(Text,Style), ascii_append_grid(Style,Text,Start,Grid))).

pttgS(Width,CCs,[Row|G0],G):- var(Width),length(Row,Width),!,pttgS(Width,CCs,[Row|G0],G).
pttgS(Width,['\n'|CCs],G0,G):- append([Row,['\n'],More],CCs), \+ member('|',More),!,
  pttgSR(Width,Row,CRow),  !, append(G0,[CRow],GG), pttgS(Width,['\n'|More],GG,G).
pttgS(Width,CCs,G0,G):- append([_,['|',' '],Row,['|'],More],CCs), \+ member('\n',Row),!,
  pttgSR(Width,Row,CRow),  !,
  append(G0,[CRow],GG),
  pttgS(Width,More,GG,G).
pttgS(Width,CCs,G0,G):- append([_,['\n',' '],Row,['\n'],More],CCs), \+ member('|',Row),!,
  pttgSR(Width,Row,CRow),  !,
  append(G0,[CRow],GG),
  pttgS(Width,More,GG,G).
pttgS(_,_,G,G):-!.
pttgSR(Width,[' ',C|Row],Out):- C\==' ',!,pttgSR(Width,[C|Row],Out).
pttgSR(Width,[C,' '|Row],[CC|CRow]):- trans_to_color1(C,CC),pttgSR(Width,Row,CRow).
pttgSR(_,_,[]).

ascii_append_grid(Style,Text,Start,Grid):- ensure_charlist(Text,Chars),!,ascii_append_grid(Style,Chars,Start,Grid).
ascii_append_grid(pttgS(Width),Text,Start,Grid):- !,pttgS(Width,Text,Start,Grid),!.
ascii_append_grid(Style,Text,Start,Grid):- 
  next_row(_,Style,Text,Chars,MoreText),read_row_cells(Style,Chars,Row), !, 
  append(Start,[Row],MidG),
  ascii_append_grid(Style,MoreText,MidG,Grid).
ascii_append_grid(_,_,PassThruG,PassThruG).

read_row_cells(_,[],Row):-!, Row = [].
read_row_cells(g_style(RowSep,_,_,_),Chars,[]):- append(RowSep,_,Chars),!.
read_row_cells(Style,Chars,[Cell|MoreCells]):- read_cell(Style,Chars,Cell,MoreChars),read_row_cells(Style,MoreChars,MoreCells).
read_row_cells(_,_,[]).


list_contains(AllText,RowSep):- \+ \+ append([_|RowSep],_,AllText).

% ""
next_row(strict,_,AllText,RowText,MoreText):- AllText==[],!,fail,RowText = AllText, MoreText = [].

% a
next_row(strict,g_style(RowSep,CellSep,_,_),AllText,RowText,MoreText):- fail,
 RowSep\==[], \+ list_contains(AllText,RowSep), 
 list_contains(RowText,CellSep), RowText = AllText, MoreText = [].

% everything below
next_row(Strict,Style,AllText,RowText,MoreText):- !,
   next_row1(Strict,Style,AllText,RowText,MoreText),!,
   ((Strict = strict) -> ( RowText\==[], Style = g_style(_,CellSep,_,_), list_contains(RowText,CellSep)); true).

% |a|b..
next_row1(strict,g_style(RowSep,CellSep,_,_),AllText,RowText,MoreText):- 
 append(RowSep,CellSep,RowsApart),
 append([RowsApart,RowText,RowsApart,More],AllText),  
 RowText\==[],!, append(RowsApart,More,MoreText),!.

% a|b..
next_row1(strict,g_style(RowSep,CellSep,_,_),AllText,RowText,MoreText):- 
 append(RowSep,CellSep,RowsApart),
 append([RowText,RowsApart,More],AllText),  
 RowText\==[],!, append(RowsApart,More,MoreText),!.

% |a
next_row1(strict,g_style(RowSep,CellSep,_,_),AllText,RowText,[]):- 
 append(RowSep,CellSep,RowsApart),
 append([RowsApart,RowText],AllText).

% a|
next_row1(strict,g_style(RowSep,CellSep,_,_),AllText,RowText,[]):- 
 append(RowSep,CellSep,RowsApart),
 append([RowText,RowsApart],AllText).



read_cell(g_style(_RowSep,CellSep,BlackStyle,VarStyle),[C|Rest],Cell,More):- 
  read_one_color(BlackStyle,VarStyle,C,Cell), append(CellSep,Rest,More).

read_one_color(BlackStyle,VarStyle,C,Cell):- is_black_color(BlackStyle,VarStyle,C),!,get_black(Cell).
read_one_color(BlackStyle,VarStyle,C,Cell):- C\==BlackStyle,(C==VarStyle;(var(VarStyle), var(C))),!,Cell=_.
read_one_color(_BlackStyle,_VarStyle,C,Cell):- trans_to_color1(C,Cell),!.
read_one_color(_BlackStyle,_VarStyle,C,unreadable(C)):- atrace, !.

trans_to_color1(Num,Color):- atom_number(Num,CC),color_name(CC,Color),!.
trans_to_color1(' ',_):-!.
trans_to_color1(C,NewC):- char_code(C,Code),Code>31,trans_to_color(C,NewC).
trans_to_color1('.',black):-!.
trans_to_color1(C1O,NewC):-trans_to_color(C1O,NewC).

is_black_color(BlackStyle,VarStyle,C):- BlackStyle==C,VarStyle\==C.

pttgo(H,V,X,G,GO):- append(LEft,[' ','|'|Right],X), append(LEft,['|'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,['\r'|Right],X), append(LEft,['\n'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,['\n','\n'|Right],X), append(LEft,['\n'|Right],Y),!, pttgo(H,V,Y,G,GO).
% pttgo(H,V,X,G,GO):- append(LEft,[' ','\n'|Right],X), append(LEft,['\n'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,['|','\n'|Right],X), append(LEft,['\n'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,['\n','|'|Right],X), append(LEft,['\n'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,['¯'|Right],X), append(LEft,['_'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(H,V,X,G,GO):- append(LEft,[' ','_'|Right],X), append(LEft,['_'|Right],Y),!, pttgo(H,V,Y,G,GO).
pttgo(0,0,X,G,GO):-
  append(_,['_','\n'|Right],X),
  pttgo(1,1,Right,G,GO).
pttgo(0,0,X,G,GO):-
  append(_,['\n'|Right],X),
  pttgo(1,1,Right,G,GO).

pttgo(_,V,['|'|X],G,GO):- 
  pttgo(1,V,X,G,GO).
pttgo(H,V,[' ',C1O|X],G,GO):- C1O \== '\n',
  trans_to_color(C1O,NewC),
  replace_global_hvc_point(H,V,NewC,_,G,GM),
  H2 is H+1,
  pttgo(H2,V,X,GM,GO).
pttgo(H,V,[' '|X],G,GO):-
  H2 is H+1,
  pttgo(H2,V,X,G,GO).
pttgo(H,V,[C1O|X],G,GO):-
  trans_to_color(C1O,NewC),
  replace_global_hvc_point(H,V,NewC,_,G,GM),
  H2 is H+1,
  pttgo(H2,V,X,GM,GO).
pttgo(_,V,['\n'|X],G,GO):-
  V2 is V+1,
  pttgo(1,V2,X,G,GO).
pttgo(_,_,[],G,G):-!.
pttgo(_,_,['_'|_],G,G):-!.

insert_col_row_pad_open(H0,V0,G,GUU):- 
   insert_col_pad_open(H0,G,GU),
   insert_row_pad_open(V0,GU,GUU).

insert_col_pad_open(V0,GU,GUU):-  c_r(insert_row_pad_open(V0),GU,GUU).
insert_row_pad_open(V0,GU,GridU):- functor(P,v,V0),P=..[v|L],append(L,GU,LGU), append(LGU,_,GridU).


h666(colors_cc,
[[4,5,1,7,8,9,4,5,2,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9],
 [1,5,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,7,8,9,4,5,1,7,8,9],
 [1,1,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,1,8,9,1,1,1,1,1,9],
 [1,1,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,7,8,9,4,5,1,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9]]).

h666(colors_cc,
[[1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9]]).

h666(colors_cc,
[[4,5,1,7,8,9],
 [4,5,1,7,8,9],
 [4,5,1,7,8,9],
 [4,5,1,1,8,9],
 [4,1,1,7,8,9],
 [4,1,1,1,8,9],
 [4,1,1,7,8,9],
 [4,5,1,7,8,9]]).



h666(colors_cc,
[[1,2,3,4,5,2,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,1,1,1,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,2,7,8,9]]).

n_shape(LibObj):- 
  n_grid(Shapes0,Grid),
  into_lib_object(Shapes0,Grid,LibObj).


n_grid(plus_3x3,
[[_,1,_],
 [1,1,1],
 [_,1,_]]).

n_grid(plus_3x3_colorless,
[[_,X,_],
 [X,X,X],
 [_,X,_]]):- decl_one_fg_color(X).

n_grid(plus_5x5,
[[_,_,1,_,_],
 [_,_,1,_,_],
 [1,1,1,1,1],
 [_,_,1,_,_],
 [_,_,1,_,_]]).


f666(_,
[[_,_,1,_,_],
 [_,_,1,_,_],
 [2,2,1,1,1],
 [_,_,1,_,_],
 [_,_,1,_,_]]).


n_grid(plus_5x5_colorless,
[[_,_,X,_,_],
 [_,_,X,_,_],
 [X,X,X,X,X],
 [_,_,X,_,_],
 [_,_,X,_,_]]):- decl_one_fg_color(X).


f666(_Ncolors,
[[5,X,7],
 [5,1,7],
 [5,1,7]]):- decl_one_fg_color(X).


n_grid(vbar_6_colorless,
[[X],
 [X],
 [X],
 [X],
 [X],
 [X]]):- decl_one_fg_color(X).

f666(T,
[[X,_],
 [X,_],
 [X,_],
 [X,X],
 [X,_]]):- decl_one_fg_color(X), T = _.

f666(T,
[[X,_],
 [X,X],
 [X,_],
 [X,_]]):- decl_one_fg_color(X), T = _.



h666(1,
[[1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,1,1,1,8,9],
 [1,1,1,1,1,1,1,1,1],
 [1,2,3,4,1,1,1,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9]]).

h666(1,
[[1,1,3,4,5,1,7,8,9],
 [1,2,1,4,5,1,7,8,1],
 [1,2,3,1,5,1,7,1,9],
 [1,2,3,4,1,1,1,8,9],
 [1,1,1,1,1,1,1,1,1],
 [1,2,3,4,1,1,1,8,9],
 [1,2,3,1,5,1,7,8,9],
 [1,2,1,4,5,1,7,8,9],
 [1,1,3,4,5,1,7,8,9]]).

f666(1,S):- n_grid(_,S).

f666(1,
[[1,1,1],
 [1,3,1],
 [1,1,1]]).

n_grid(hollow_3x3,
[[1,1,1],
 [1,_,1],
 [1,1,1]]).

n_grid(solid_3x3,
[[1,1,1],
 [1,1,1],
 [1,1,1]]).

n_grid(solid_5x5,
[[1,1,1,1,1],
 [1,1,1,1,1],
 [1,1,1,1,1],
 [1,1,1,1,1],
 [1,1,1,1,1]]).

h666(1,
[[2,2,2,2,2],
 [2,1,1,1,2],
 [2,1,2,1,2],
 [2,1,1,1,2],
 [2,2,2,2,2]]).

h666(1,
[[2,2,2,2,2],
 [2,1,1,1,2],
 [2,1,3,1,2],
 [2,1,1,1,1],
 [2,2,2,2,2]]).

f666(1,[[1]]).
f666(1,[[FG]]):- decl_one_fg_color(FG).
f666(1,[[3]]).

%f666(1,[[1]]).

%f666(1,[[_]]).

create_padding(GridIn,LowH,LowV,HiH,HiV,H,V,HH,VV,GridO):- 
   fix_v_range(LowV,HiV,H,V,VV,GridIn,Grid1),
   c_r(fix_v_range(LowH,HiH,VV,H,HH),Grid1,GridO).
   


fix_v_range(1,HiV,H,V,VV,GridIn,GridO):-
  make_row(Row,H), 
  fix_v_range(2,HiV,H,V,V2,[Row|GridIn],GridO), VV is V2+1.

fix_v_range(LowV,HiV,H,V,VV,GridIn,GridO):- HiV==V,!, 
  make_row(Row,H),
  append(GridIn,[Row],Grid2),
  HiV2 is HiV+1,
  fix_v_range(LowV,HiV2,H,V,V2,Grid2,GridO),
  VV is V2+1.
fix_v_range(_LowV,_HiV,_H,V,V,GridIn,GridIn).


perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,'$VAR'(1149)],[blue,blue,blue],[blue,'$VAR'(1150),'$VAR'(1151)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,'$VAR'(1120)],[blue,blue,blue],[blue,'$VAR'(1121),'$VAR'(1122)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,'$VAR'(999)],[blue,blue,blue],[blue,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue],['$VAR'(1149),blue,blue],['$VAR'(1150),blue,'$VAR'(1151)]],true).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue],['$VAR'(1120),blue,blue],['$VAR'(1121),blue,'$VAR'(1122)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue],['$VAR'(999),blue,blue],['$VAR'(1000),blue,'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue],[blue,blue,blue],['$VAR'(1151),blue,blue]],true).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue],[blue,blue,blue],['$VAR'(1122),blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue],[blue,blue,blue],['$VAR'(1001),blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),blue,'$VAR'(1150)],[blue,blue,'$VAR'(1151)],[blue,blue,blue]],true).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),blue,'$VAR'(1121)],[blue,blue,'$VAR'(1122)],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,'$VAR'(1001)],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,blue],['$VAR'(1151),'$VAR'(1152),blue,blue],[blue,blue,blue,blue],['$VAR'(1153),'$VAR'(1154),blue,blue],['$VAR'(1155),'$VAR'(1156),blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,blue],['$VAR'(1122),'$VAR'(1123),blue,blue],[blue,blue,blue,blue],['$VAR'(1124),'$VAR'(1125),blue,blue],['$VAR'(1126),'$VAR'(1127),blue,blue]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,blue],['$VAR'(1001),'$VAR'(1002),blue,blue],[blue,blue,blue,blue],['$VAR'(1003),'$VAR'(1004),blue,blue],['$VAR'(1005),'$VAR'(1006),blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,'$VAR'(1149),'$VAR'(1150)],[blue,blue,'$VAR'(1151),'$VAR'(1152)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1153),'$VAR'(1154)],[blue,blue,'$VAR'(1155),'$VAR'(1156)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,'$VAR'(1120),'$VAR'(1121)],[blue,blue,'$VAR'(1122),'$VAR'(1123)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1124),'$VAR'(1125)],[blue,blue,'$VAR'(1126),'$VAR'(1127)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,'$VAR'(999),'$VAR'(1000)],[blue,blue,'$VAR'(1001),'$VAR'(1002)],[blue,blue,blue,blue],[blue,blue,'$VAR'(1003),'$VAR'(1004)],[blue,blue,'$VAR'(1005),'$VAR'(1006)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[red,red,'$VAR'(1149)],[red,red,red],[red,'$VAR'(1150),'$VAR'(1151)]],true).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[red,red,'$VAR'(1120)],[red,red,red],[red,'$VAR'(1121),'$VAR'(1122)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[red,red,'$VAR'(999)],[red,red,red],[red,'$VAR'(1000),'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[red,red,red],['$VAR'(1149),red,red],['$VAR'(1150),red,'$VAR'(1151)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[red,red,red],['$VAR'(1120),red,red],['$VAR'(1121),red,'$VAR'(1122)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[red,red,red],['$VAR'(999),red,red],['$VAR'(1000),red,'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),red],[red,red,red],['$VAR'(1151),red,red]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),red],[red,red,red],['$VAR'(1122),red,red]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),red],[red,red,red],['$VAR'(1001),red,red]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),red,'$VAR'(1150)],[red,red,'$VAR'(1151)],[red,red,red]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),red,'$VAR'(1121)],[red,red,'$VAR'(1122)],[red,red,red]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),red,'$VAR'(1000)],[red,red,'$VAR'(1001)],[red,red,red]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)],['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)],['$VAR'(1150),'$VAR'(1151),'$VAR'(1149),'$VAR'(1152),'$VAR'(1153)],['$VAR'(1154),'$VAR'(1155),'$VAR'(1149),'$VAR'(1156),'$VAR'(1157)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)],['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)],['$VAR'(1121),'$VAR'(1122),'$VAR'(1120),'$VAR'(1123),'$VAR'(1124)],['$VAR'(1125),'$VAR'(1126),'$VAR'(1120),'$VAR'(1127),'$VAR'(1128)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1152),'$VAR'(1153),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1154),'$VAR'(1155),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1156),'$VAR'(1157),'$VAR'(1151),'$VAR'(1151)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1123),'$VAR'(1124),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1125),'$VAR'(1126),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1127),'$VAR'(1128),'$VAR'(1122),'$VAR'(1122)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1002),'$VAR'(1003),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1006),'$VAR'(1007),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),'$VAR'(1151),'$VAR'(1152),'$VAR'(1153)],['$VAR'(1154),'$VAR'(1155),'$VAR'(1151),'$VAR'(1156),'$VAR'(1157)],['$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124)],['$VAR'(1125),'$VAR'(1126),'$VAR'(1122),'$VAR'(1127),'$VAR'(1128)],['$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1149),'$VAR'(1150),'$VAR'(1151)],['$VAR'(1149),'$VAR'(1149),'$VAR'(1152),'$VAR'(1153)],['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)],['$VAR'(1149),'$VAR'(1149),'$VAR'(1154),'$VAR'(1155)],['$VAR'(1149),'$VAR'(1149),'$VAR'(1156),'$VAR'(1157)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122)],['$VAR'(1120),'$VAR'(1120),'$VAR'(1123),'$VAR'(1124)],['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)],['$VAR'(1120),'$VAR'(1120),'$VAR'(1125),'$VAR'(1126)],['$VAR'(1120),'$VAR'(1120),'$VAR'(1127),'$VAR'(1128)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(1000),'$VAR'(1001)],['$VAR'(999),'$VAR'(999),'$VAR'(1002),'$VAR'(1003)],['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(999),'$VAR'(1004),'$VAR'(1005)],['$VAR'(999),'$VAR'(999),'$VAR'(1006),'$VAR'(1007)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],true).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[yellow,silver,blue],[yellow,silver,blue],[yellow,silver,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[yellow,yellow,yellow],[silver,silver,silver],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,silver,yellow],[blue,silver,yellow],[blue,silver,yellow]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue],[silver,silver,silver],[yellow,yellow,yellow]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),blue,'$VAR'(1150)],[blue,blue,blue],['$VAR'(1151),blue,'$VAR'(1152)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),blue,'$VAR'(1121)],[blue,blue,blue],['$VAR'(1122),blue,'$VAR'(1123)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),blue,'$VAR'(1000)],[blue,blue,blue],['$VAR'(1001),blue,'$VAR'(1002)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)],[blue,blue,blue,blue,blue],['$VAR'(1157),'$VAR'(1158),blue,'$VAR'(1159),'$VAR'(1160)],['$VAR'(1161),'$VAR'(1162),blue,'$VAR'(1163),'$VAR'(1164)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)],[blue,blue,blue,blue,blue],['$VAR'(1128),'$VAR'(1129),blue,'$VAR'(1130),'$VAR'(1131)],['$VAR'(1132),'$VAR'(1133),blue,'$VAR'(1134),'$VAR'(1135)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)],[red,red,blue,blue,blue],['$VAR'(1157),'$VAR'(1158),blue,'$VAR'(1159),'$VAR'(1160)],['$VAR'(1161),'$VAR'(1162),blue,'$VAR'(1163),'$VAR'(1164)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)],[red,red,blue,blue,blue],['$VAR'(1128),'$VAR'(1129),blue,'$VAR'(1130),'$VAR'(1131)],['$VAR'(1132),'$VAR'(1133),blue,'$VAR'(1134),'$VAR'(1135)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[red,red,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),red,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),red,'$VAR'(1155),'$VAR'(1156)],[blue,blue,blue,blue,blue],['$VAR'(1157),'$VAR'(1158),blue,'$VAR'(1159),'$VAR'(1160)],['$VAR'(1161),'$VAR'(1162),blue,'$VAR'(1163),'$VAR'(1164)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),red,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),red,'$VAR'(1126),'$VAR'(1127)],[blue,blue,blue,blue,blue],['$VAR'(1128),'$VAR'(1129),blue,'$VAR'(1130),'$VAR'(1131)],['$VAR'(1132),'$VAR'(1133),blue,'$VAR'(1134),'$VAR'(1135)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),red,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),red,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)],[blue,blue,blue,red,red],['$VAR'(1157),'$VAR'(1158),blue,'$VAR'(1159),'$VAR'(1160)],['$VAR'(1161),'$VAR'(1162),blue,'$VAR'(1163),'$VAR'(1164)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)],[blue,blue,blue,red,red],['$VAR'(1128),'$VAR'(1129),blue,'$VAR'(1130),'$VAR'(1131)],['$VAR'(1132),'$VAR'(1133),blue,'$VAR'(1134),'$VAR'(1135)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,red,red],['$VAR'(1007),'$VAR'(1008),blue,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),blue,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),blue,'$VAR'(1151),'$VAR'(1152)],['$VAR'(1153),'$VAR'(1154),blue,'$VAR'(1155),'$VAR'(1156)],[blue,blue,blue,blue,blue],['$VAR'(1157),'$VAR'(1158),red,'$VAR'(1159),'$VAR'(1160)],['$VAR'(1161),'$VAR'(1162),red,'$VAR'(1163),'$VAR'(1164)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),blue,'$VAR'(1122),'$VAR'(1123)],['$VAR'(1124),'$VAR'(1125),blue,'$VAR'(1126),'$VAR'(1127)],[blue,blue,blue,blue,blue],['$VAR'(1128),'$VAR'(1129),red,'$VAR'(1130),'$VAR'(1131)],['$VAR'(1132),'$VAR'(1133),red,'$VAR'(1134),'$VAR'(1135)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),blue,'$VAR'(1001),'$VAR'(1002)],['$VAR'(1003),'$VAR'(1004),blue,'$VAR'(1005),'$VAR'(1006)],[blue,blue,blue,blue,blue],['$VAR'(1007),'$VAR'(1008),red,'$VAR'(1009),'$VAR'(1010)],['$VAR'(1011),'$VAR'(1012),red,'$VAR'(1013),'$VAR'(1014)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),'$VAR'(1151),'$VAR'(1152),'$VAR'(1153)],['$VAR'(1154),'$VAR'(1155),'$VAR'(1151),'$VAR'(1156),'$VAR'(1157)],['$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151),'$VAR'(1151)],['$VAR'(1158),'$VAR'(1159),'$VAR'(1151),'$VAR'(1160),'$VAR'(1161)],['$VAR'(1162),'$VAR'(1163),'$VAR'(1151),'$VAR'(1164),'$VAR'(1165)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124)],['$VAR'(1125),'$VAR'(1126),'$VAR'(1122),'$VAR'(1127),'$VAR'(1128)],['$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122),'$VAR'(1122)],['$VAR'(1129),'$VAR'(1130),'$VAR'(1122),'$VAR'(1131),'$VAR'(1132)],['$VAR'(1133),'$VAR'(1134),'$VAR'(1122),'$VAR'(1135),'$VAR'(1136)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1004),'$VAR'(1005),'$VAR'(1001),'$VAR'(1006),'$VAR'(1007)],['$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001),'$VAR'(1001)],['$VAR'(1008),'$VAR'(1009),'$VAR'(1001),'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1001),'$VAR'(1014),'$VAR'(1015)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[silver,'$VAR'(1149),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[silver,'$VAR'(1120),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[silver,'$VAR'(999),orange],[silver,blue,orange],[silver,blue,orange]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[silver,silver,silver],[blue,blue,'$VAR'(1149)],[orange,orange,orange]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[silver,silver,silver],[blue,blue,'$VAR'(1120)],[orange,orange,orange]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[silver,silver,silver],[blue,blue,'$VAR'(999)],[orange,orange,orange]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(1149),silver]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(1120),silver]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[orange,blue,silver],[orange,blue,silver],[orange,'$VAR'(999),silver]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[orange,orange,orange],['$VAR'(1149),blue,blue],[silver,silver,silver]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[orange,orange,orange],['$VAR'(1120),blue,blue],[silver,silver,silver]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[orange,orange,orange],['$VAR'(999),blue,blue],[silver,silver,silver]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149)],['$VAR'(1149)],['$VAR'(1149)],['$VAR'(1149)],['$VAR'(1149)],['$VAR'(1149)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120)],['$VAR'(1120)],['$VAR'(1120)],['$VAR'(1120)],['$VAR'(1120)],['$VAR'(1120)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)],['$VAR'(999)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150)],['$VAR'(1149),'$VAR'(1151)],['$VAR'(1149),'$VAR'(1152)],['$VAR'(1149),'$VAR'(1149)],['$VAR'(1149),'$VAR'(1153)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121)],['$VAR'(1120),'$VAR'(1122)],['$VAR'(1120),'$VAR'(1123)],['$VAR'(1120),'$VAR'(1120)],['$VAR'(1120),'$VAR'(1124)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],true).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],true).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1003)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)],['$VAR'(1150),'$VAR'(1149),'$VAR'(1151),'$VAR'(1152),'$VAR'(1153)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)],['$VAR'(1121),'$VAR'(1120),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(999),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150)],['$VAR'(1150),'$VAR'(1150)],['$VAR'(1151),'$VAR'(1150)],['$VAR'(1152),'$VAR'(1150)],['$VAR'(1153),'$VAR'(1150)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121)],['$VAR'(1121),'$VAR'(1121)],['$VAR'(1122),'$VAR'(1121)],['$VAR'(1123),'$VAR'(1121)],['$VAR'(1124),'$VAR'(1121)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)],['$VAR'(1003),'$VAR'(1000)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),'$VAR'(1151),'$VAR'(1152),'$VAR'(1153)],['$VAR'(1152),'$VAR'(1152),'$VAR'(1152),'$VAR'(1152),'$VAR'(1152)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124)],['$VAR'(1123),'$VAR'(1123),'$VAR'(1123),'$VAR'(1123),'$VAR'(1123)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),'$VAR'(1003)],['$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002),'$VAR'(1002)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150)],['$VAR'(1149),'$VAR'(1149)],['$VAR'(1149),'$VAR'(1151)],['$VAR'(1149),'$VAR'(1152)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121)],['$VAR'(1120),'$VAR'(1120)],['$VAR'(1120),'$VAR'(1122)],['$VAR'(1120),'$VAR'(1123)]],true).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],true).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],true).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(999),'$VAR'(999)],['$VAR'(999),'$VAR'(1001)],['$VAR'(999),'$VAR'(1002)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1149),'$VAR'(1149),'$VAR'(1149)],['$VAR'(1150),'$VAR'(1151),'$VAR'(1149),'$VAR'(1152)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1120),'$VAR'(1120),'$VAR'(1120)],['$VAR'(1121),'$VAR'(1122),'$VAR'(1120),'$VAR'(1123)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(999),'$VAR'(999),'$VAR'(999)],['$VAR'(1000),'$VAR'(1001),'$VAR'(999),'$VAR'(1002)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150)],['$VAR'(1151),'$VAR'(1150)],['$VAR'(1150),'$VAR'(1150)],['$VAR'(1152),'$VAR'(1150)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121)],['$VAR'(1122),'$VAR'(1121)],['$VAR'(1121),'$VAR'(1121)],['$VAR'(1123),'$VAR'(1121)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000)],['$VAR'(1001),'$VAR'(1000)],['$VAR'(1000),'$VAR'(1000)],['$VAR'(1002),'$VAR'(1000)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149),'$VAR'(1150),'$VAR'(1151),'$VAR'(1152)],['$VAR'(1150),'$VAR'(1150),'$VAR'(1150),'$VAR'(1150)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123)],['$VAR'(1121),'$VAR'(1121),'$VAR'(1121),'$VAR'(1121)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002)],['$VAR'(1000),'$VAR'(1000),'$VAR'(1000),'$VAR'(1000)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue],[blue,'$VAR'(1149),blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue],[blue,'$VAR'(1120),blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],true).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue],[blue,'$VAR'(999),blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue],[blue,green,blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
%perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
%perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue,blue,blue],[blue,blue,blue],[blue,blue,blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[blue]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[blue]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[blue]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[blue]],false).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[blue]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[blue]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[['$VAR'(1149)]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[['$VAR'(1120)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[['$VAR'(999)]],true).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[['$VAR'(999)]],true).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[['$VAR'(999)]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[['$VAR'(999)]],true).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[['$VAR'(999)]],true).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),blue,blue,blue,'$VAR'(1002),'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),'$VAR'(1010),'$VAR'(1011),'$VAR'(1012),'$VAR'(1013),'$VAR'(1014),blue,blue,blue,'$VAR'(1015),'$VAR'(1016),'$VAR'(1017)],['$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),blue,blue,'$VAR'(1022),'$VAR'(1023),'$VAR'(1024),'$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),'$VAR'(1031),'$VAR'(1032),'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),blue,blue,'$VAR'(1036),'$VAR'(1037),'$VAR'(1038)],['$VAR'(1039),'$VAR'(1040),'$VAR'(1041),'$VAR'(1042),blue,'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),'$VAR'(1051),'$VAR'(1052),'$VAR'(1053),'$VAR'(1054),'$VAR'(1055),'$VAR'(1056),blue,'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),blue],['$VAR'(1060),'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),blue,blue,blue,blue,blue,'$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),'$VAR'(1070),'$VAR'(1071),red,red,'$VAR'(1072),blue,blue,blue,blue,blue],['$VAR'(1073),'$VAR'(1074),'$VAR'(1075),'$VAR'(1076),blue,blue,'$VAR'(1077),blue,blue,'$VAR'(1078),'$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),'$VAR'(1085),red,red,red,blue,blue,'$VAR'(1086),blue,blue],['$VAR'(1087),'$VAR'(1088),'$VAR'(1089),'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),'$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),red,'$VAR'(1103),'$VAR'(1104),'$VAR'(1105),blue,'$VAR'(1106),'$VAR'(1107),'$VAR'(1108)],['$VAR'(1109),'$VAR'(1110),'$VAR'(1111),'$VAR'(1112),blue,blue,'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),'$VAR'(1118),'$VAR'(1119),'$VAR'(1120),'$VAR'(1121),'$VAR'(1122),'$VAR'(1123),'$VAR'(1124),'$VAR'(1125),'$VAR'(1126),blue,blue,'$VAR'(1127),'$VAR'(1128),'$VAR'(1129)],['$VAR'(1130),'$VAR'(1131),'$VAR'(1132),'$VAR'(1133),blue,blue,blue,'$VAR'(1134),'$VAR'(1135),'$VAR'(1136),'$VAR'(1137),'$VAR'(1138),'$VAR'(1139),'$VAR'(1140),'$VAR'(1141),'$VAR'(1142),'$VAR'(1143),'$VAR'(1144),'$VAR'(1145),'$VAR'(1146),blue,blue,blue,'$VAR'(1147),'$VAR'(1148)]],[[green]],false).
perfect_result([['$VAR'(999),'$VAR'(1000),'$VAR'(1001),'$VAR'(1002),blue,blue,blue,blue,blue,'$VAR'(1003),'$VAR'(1004),'$VAR'(1005),'$VAR'(1006),'$VAR'(1007),'$VAR'(1008),'$VAR'(1009),blue,blue,blue,blue,blue,'$VAR'(1010),'$VAR'(1011)],['$VAR'(1012),'$VAR'(1013),'$VAR'(1014),'$VAR'(1015),blue,blue,blue,blue,blue,'$VAR'(1016),'$VAR'(1017),'$VAR'(1018),'$VAR'(1019),'$VAR'(1020),'$VAR'(1021),'$VAR'(1022),blue,blue,blue,blue,blue,'$VAR'(1023),'$VAR'(1024)],['$VAR'(1025),'$VAR'(1026),'$VAR'(1027),'$VAR'(1028),'$VAR'(1029),'$VAR'(1030),blue,'$VAR'(1031),'$VAR'(1032),blue,blue,'$VAR'(1033),'$VAR'(1034),'$VAR'(1035),red,red,'$VAR'(1036),'$VAR'(1037),blue,'$VAR'(1038),'$VAR'(1039),blue,blue],['$VAR'(1040),'$VAR'(1041),'$VAR'(1042),'$VAR'(1043),'$VAR'(1044),'$VAR'(1045),blue,'$VAR'(1046),'$VAR'(1047),blue,blue,'$VAR'(1048),'$VAR'(1049),'$VAR'(1050),red,red,'$VAR'(1051),'$VAR'(1052),blue,'$VAR'(1053),'$VAR'(1054),blue,blue],['$VAR'(1055),'$VAR'(1056),'$VAR'(1057),'$VAR'(1058),'$VAR'(1059),'$VAR'(1060),blue,blue,blue,blue,blue,'$VAR'(1061),'$VAR'(1062),'$VAR'(1063),red,red,red,red,blue,blue,blue,blue,blue],['$VAR'(1064),'$VAR'(1065),'$VAR'(1066),'$VAR'(1067),'$VAR'(1068),'$VAR'(1069),blue,'$VAR'(1070),'$VAR'(1071),blue,blue,'$VAR'(1072),'$VAR'(1073),'$VAR'(1074),red,red,'$VAR'(1075),'$VAR'(1076),blue,'$VAR'(1077),'$VAR'(1078),blue,blue],['$VAR'(1079),'$VAR'(1080),'$VAR'(1081),'$VAR'(1082),'$VAR'(1083),'$VAR'(1084),blue,'$VAR'(1085),'$VAR'(1086),blue,blue,'$VAR'(1087),'$VAR'(1088),'$VAR'(1089),red,red,'$VAR'(1090),'$VAR'(1091),blue,'$VAR'(1092),'$VAR'(1093),blue,blue],['$VAR'(1094),'$VAR'(1095),'$VAR'(1096),'$VAR'(1097),blue,blue,blue,blue,blue,'$VAR'(1098),'$VAR'(1099),'$VAR'(1100),'$VAR'(1101),'$VAR'(1102),'$VAR'(1103),'$VAR'(1104),blue,blue,blue,blue,blue,'$VAR'(1105),'$VAR'(1106)],['$VAR'(1107),'$VAR'(1108),'$VAR'(1109),'$VAR'(1110),blue,blue,blue,blue,blue,'$VAR'(1111),'$VAR'(1112),'$VAR'(1113),'$VAR'(1114),'$VAR'(1115),'$VAR'(1116),'$VAR'(1117),blue,blue,blue,blue,blue,'$VAR'(1118),'$VAR'(1119)]],[[green]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown,yellow,silver,red,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,silver,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,blue,cyan,brown,blue,blue,blue,blue,blue,brown],[blue,blue,blue,blue,cyan,brown,yellow,silver,blue,orange,cyan,brown],[blue,blue,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown,yellow,silver,blue,orange,cyan,brown]],[[green]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[green]],false).
perfect_result([[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown],[yellow,silver,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,blue,blue,blue,cyan,brown],[yellow,blue,blue,orange,cyan,brown],[yellow,silver,blue,orange,cyan,brown]],[[green]],false).
perfect_result([[blue,red,green,yellow,silver,red,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,red,orange,cyan,brown]],[[green]],false).
perfect_result([[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown],[blue,red,green,yellow,silver,blue,orange,cyan,brown]],[[green]],false).
perfect_result([[blue,blue,green,yellow,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,blue],[blue,red,green,blue,silver,blue,orange,blue,brown],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,blue,blue,blue,blue,blue,blue,blue,blue],[blue,red,green,yellow,blue,blue,blue,cyan,brown],[blue,red,green,blue,silver,blue,orange,cyan,brown],[blue,red,blue,yellow,silver,blue,orange,cyan,brown],[blue,blue,green,yellow,silver,blue,orange,cyan,brown]],[[green]],true).
perfect_result([[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue],[blue,blue,blue,blue,blue]],[[green]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,red,blue,red],[red,blue,blue,blue,red],[red,red,red,red,red]],[[green]],false).
perfect_result([[red,red,red,red,red],[red,blue,blue,blue,red],[red,blue,green,blue,red],[red,blue,blue,blue,blue],[red,red,red,red,red]],[[green]],true).
  
:- include(kaggle_arc_footer).



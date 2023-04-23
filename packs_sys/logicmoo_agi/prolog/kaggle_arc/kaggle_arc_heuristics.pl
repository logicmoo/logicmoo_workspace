/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

:- multifile learn_shapelib/7.
:- multifile individuals_from_pair/9.

:- discontiguous learn_shapelib/7.
:- discontiguous individuals_from_pair/9.


i_pair(WasROptions,GridIn,GridOut):- fail, WasROptions==complete, % TODO unfail this
 catch((must_det_ll((
 check_for_refreshness,
 ((var(GridIn);var(GridOut))-> current_pair_io(GridIn,GridOut) ; true),
 maybe_name_the_pair(GridIn,GridOut,PairName))),
 notrace(((((guess_how(HOW,GridIn,GridOut,Stuff1,Stuff2), 
             guess_how_else(HOW_ELSE,Stuff1,Stuff2,InC,OutC)))))),
 INDIV = [HOW|HOW_ELSE],
 show_individuated_pair(PairName,INDIV,GridIn,GridOut,InC,OutC)),_,fail),!.

i_pair(ROptions,GridIn,GridOut):-
 must_det_ll((    
  check_for_refreshness,
  individuate_pair(ROptions,GridIn,GridOut,InC,OutC),
  maybe_name_the_pair(GridIn,GridOut,PairName),
  show_individuated_pair(PairName,ROptions,GridIn,GridOut,InC,OutC))).



maybe_optimize_objects(InC00,OutC00,InCR,OutCR):-
 wots(S,
 w_section(optimize_objects, ((
 once((must_det_ll((
  remove_background_only_object(InC00,InC000),
  remove_background_only_object(OutC00,OutC000),
 extend_grp_proplist(InC000,InC0),
 extend_grp_proplist(OutC000,OutC0),
  maybe_fix_group(InC0,InCRFGBG),
  maybe_fix_group(OutC0,OutCRFGBG),
  mostly_fg_objs(InCRFGBG,InCR),
  mostly_fg_objs(OutCRFGBG,OutCR),
  nop((show_changes(InC0,InCR),
  show_changes(OutC0,OutCR))))))))),ran_collapsed)),
   ((InC00\=@=InCR;OutC00\=@=OutCR),(InCR\==[],OutCR\==[])),!,
  write(S).

optimize_objects(InC00,OutC00,InC,OutC):-
  (maybe_optimize_objects(InC00,OutC00,InC,OutC)->true;(InC00=InC,OutC00=OutC)).

pairname_to_examplenum(PairName,Example+Num):-
  sub_term(E,PairName),compound(E),E=(Example+Num),!.

show_individuated_pair(PairName,ROptions,GridIn,GridOfIn,InC,OutC):-  
  GridIn=@=GridOfIn,!,
 must_det_ll((
  into_iog(InC,OutC,IndvS),
  show_individuated_nonpair(PairName,ROptions,GridIn,GridOfIn,IndvS))).

show_individuated_pair(PairName,ROptions,GridIn,GridOut,InC00,OutC00):- 
  maybe_optimize_objects(InC00,OutC00,InCR,OutCR),!,
  show_individuated_pair(PairName,ROptions,GridIn,GridOut,InCR,OutCR),!.


show_individuated_pair(PairName,ROptions,GridIn,GridOut,InCB,OutCB):-
 ensure_test(TestID),
 (var(PairName)->maybe_name_the_pair(GridIn,GridOut,PairName);true),
 ignore((pairname_to_examplenum(PairName,ExampleNum)->set_example_num(ExampleNum);ensure_example_num(GridIn,GridOut))),
 must_det_ll((
  grid_to_tid(GridIn,ID1),  grid_to_tid(GridOut,ID2),   
 w_section(show_individuated_sections,((                          
  print_side_by_side(red,GridIn,gridIn(ROptions,ID1,PairName),_,GridOut,gridOut(ID2)),
  as_ngrid(GridIn,GridIn1),as_ngrid(GridOut,GridOut1), xfer_zeros(GridIn1,GridOut1), 
  print_side_by_side(yellow,GridIn1,ngridIn(ROptions,ID1,PairName),_,GridOut1,ngridOut(ID2)),
  %trace,
  grid_size(GridIn,IX,IY),grid_size(GridOut,OX,OY),
  print_side_by_side(green,print_grid(IX,IY,InCB),before_rm_bg(ROptions,ID1,PairName),_,print_grid(OX,OY,OutCB),before_rm_bg(ID2)),
  remove_background_only_object(InCB,InC),remove_background_only_object(OutCB,OutC),
  %pp(InC),
  print_side_by_side(green,InC,after_rm_bg(ROptions,ID1),_,OutC,objs(ID2)),

  if_t( \+ nb_current(menu_key,'i'),
((((((

   show_indivs_side_by_side(inputs,InC),
   show_indivs_side_by_side(outputs,OutC),
    
  %w_section(show_io_groups,show_io_groups(green,ROptions,ID1,InC,ID2,OutC)),
  %show_io_groups(green,ROptions,ID1,InC,ID2,OutC),
  =(InCR,InC), =(OutCR,OutC),
    print_side_by_side(green,InC,lhs(ROptions,ID1),_,OutC,rhs(ROptions,ID2)),
 true))),

 (((
  banner_lines(orange), %visible_order(InC,InCR),
 if_t( once(true; \+ nb_current(menu_key,'i')),

 w_section(show_individuated_learning,must_det_ll((
   %when_in_html(if_wants_output_for(guess_some_relations,guess_some_relations(InC,OutC))),
   %when_in_html(if_wants_output_for(sort_some_relations,sort_some_relations(InC,OutC))),

  (sub_var(trn,ID1)->show_object_dependancy(TestID,ExampleNum,InC,OutC);show_object_dependancy(TestID)),
 if_t( fail,((


   w_section(learn_group_mapping,        if_t(sub_var(trn,ID1), learn_group_mapping(InCR,OutCR))),
   ignore((w_section(learn_group_mapping_of_tst, if_t(sub_var(tst,ID1), learn_group_mapping(InCR,OutCR))))), 
   if_wants_output_for(show_safe_assumed_mapped, show_safe_assumed_mapped),
   if_wants_output_for(show_assumed_mapped, show_assumed_mapped),

   if_wants_output_for(show_test_associatable_groups, 
       forall(member(In1,InC),show_test_associatable_groups(ROptions,ID1,In1,GridOut)))), 

   if_wants_output_for(try_each_using_training,
     forall(try_each_using_training(InC,GridOut,RulesUsed,OurOut),
      ignore((
       print_grid(try_each_using_training,OurOut),
       nop(pp(RulesUsed)),
       banner_lines(orange,2))))))))))),

  banner_lines(orange,4))))))))))))),!,
  if_wants_output_for(show_interesting_props,  show_interesting_props(PairName,InC,OutC)),!.
  



show_indivs_side_by_side(W,InC):- \+ wants_html,!,
  print_list_of(show_indiv(W),W,InC).
show_indivs_side_by_side(W,InC):- show_indivs_side_by_side_html(W,InC).

show_indivs_side_by_side_html(W,InC):-
  my_maplist(show_indiv_vert(W),InC,Vert),
  print_table([Vert]).

show_indiv_vert(W,Obj,TD):- wots(TD,show_indiv(W,Obj)).
  


arc_spyable_keyboard_key(detect_pair_hints,'g').
arc_spyable_keyboard_key(show_interesting_props,'o').
arc_spyable_keyboard_key(show_safe_assumed_mapped,'o').
arc_spyable_keyboard_key(learn_group_mapping,'o').
arc_spyable_keyboard_key(learn_group_mapping_of_tst,'o').
arc_spyable_keyboard_key(show_test_associatable_groups,'o').
arc_spyable_keyboard_key(try_each_using_training,'u').


show_test_associatable_groups(ROptions,ID1,InC,GridOut):- 
  print_grid(wqs(show_test_assocs(ROptions,ID1)),InC),
  nop(nop((forall(
    must_det_ll1((use_test_associatable_group(InC,Sols)
      *-> show_result("Our Learned Sols", Sols,GridOut,_)
        ; arcdbg_info(red,warn("No Learned Sols")))),
    true)))).


show_group(ID1,InC):-
 must_det_ll((
  length(InC,IL),g_display(InC,InCSS),print_side_by_side(len(ID1,IL)=InCSS),print_grid(combined(ID1)=InC))).

show_io_groups(Color,ROptions,ID1,InC0,ID2,OutC0):- 
 must_det_ll((    
    banner_lines(Color,3),  
    visible_order(InC0,InC), 
    visible_order(OutC0,OutC),    
    print_side_by_side(Color,InC,lhs(ROptions,ID1),_,OutC,rhs(ROptions,ID2)),
    banner_lines(Color,2),
    show_group(lhs(ID1),InC),
    banner_lines(Color),
    show_group(rhs(ID2),OutC),   
    banner_lines(Color,3))),
 nop((if_t(should_not_be_io_groups(InC,OutC), ( print(InC),print(OutC),ibreak)))).

should_not_be_io_groups(I,O):- I\==[],O\==[],I=@=O.

visible_order_fg(InC0,InC):- is_grid(InC0),!,InC=InC0.
visible_order_fg(InC00,InC):- is_list(InC00),!,include(is_used_fg_object,InC00,InC0),visible_order(InC0,InC).
visible_order_fg(InC,InC).
visible_order(InC0,InC):- is_grid(InC0),!,InC=InC0.
visible_order(InC0,InC):- is_list(InC0),!, predsort(sort_on(most_visible),InC0,InC).
visible_order(InC,InC).

most_visible(Obj,LV):- has_prop(pixel2C(_,_,_),Obj),!, LV= (-1)^1000^1000.
most_visible(Obj,LV):- area(Obj,1), grid_size(Obj,H,V), Area is (H-1)*(V-1), !, LV=Area^1000^1000.
most_visible(Obj,LV):- area(Obj,Area),cmass(bg,Obj,BGMass), % cmass(fg,Obj,FGMass),
  findall(_,doing_map_list(_,_,[Obj|_]),L),length(L,Cnt),NCnt is -Cnt, !, %, NCMass is -CMass,
  LV = Area^BGMass^NCnt.
most_visible(_Obj,LV):- LV= (-1)^1000^1000.


recalc_sizes(VM,[After|TODO]):-
   recalc_sizes(VM),
   nop((set(VM.lo_program) = [After,recalc_sizes|TODO])).
/*
   � mass(3) cc(blue,3.0) vis2D(1,3) loc2D(2,1) pen([]) /*b*/iz((nsew)) iz(symmetry_type(sym_hv)) center2G(2,2) layer(in) nth(21)
%mass(3) cc(cyan,3.0) vis2D(1,3) loc2D(1,1) pen([]) /*b*/iz((nsew)) iz(symmetry_type(sym_hv)) center2G(1,2) layer(in) nth(22)
%  Iz(Non Diag):         � mass(3) cc(green,3.0) vis2D(1,3) loc2D(3,1) pen([]) /*b*/iz((nsew)) iz(nsew) iz(rectangulator) iz(symmetry_type(sym_hv)) center2G(3,2) layer(in) nth(20)

*/

recalc_sizes(VM):- is_vm_map(VM),
   length(VM.objs,Count), Count>3,
   computeMassIndex(VM,Sizes),
   computeMinMass(VM,Sizes,Count,Min),
   computeMaxMass(VM,Sizes,Count,Max),
   if_t(
     (VM.objs_min_mass \== Min ; VM.objs_max_mass \== Max),
      pp(yellow,decide_min_max_size(Sizes,Max,Min))),

   set(VM.objs_min_mass) = Min,
   set(VM.objs_max_mass) = Max,
   show_vm_changes(VM,cullObjectsOutsideOf(Min,Max),cullObjectsOutsideOf(VM,Min,Max)).

cullObjectsOutsideOfRanges(VM):- is_vm_map(VM), length(VM.objs,N),N< floor(VM.objs_max_len/2),!.
cullObjectsOutsideOfRanges(VM):- is_vm_map(VM), cullObjectsOutsideOf(VM,VM.objs_min_mass,VM.objs_max_mass). 
cullObjectsOutsideOf(VM,Min,Max):- 
  (var(Min)->computeMinMass(VM,Min);true),
  (var(Max)->computeMaxMass(VM,Max);true),
  Objs = VM.objs,
  my_partition(within_mass(Min,Max),Objs,Keep,Cull),
  length(Keep,Len),ignore((Len>20,
     print_side_by_side(Keep,Cull),
     my_partition(within_mass(1,1),Cull,_Ones,NonOnes),
     remObjects(VM,NonOnes))).

within_mass(Min,Max,Obj):- mass(Obj,Mass),between(Min,Max,Mass).

size_to_keys(N,N-N).
computeMassIndex(VM,Sizes):-  my_maplist(mass,VM.objs,UKeySizes), my_maplist(size_to_keys,UKeySizes,KSizes),keysort(KSizes,SKSizes),
  my_maplist(arg(2),SKSizes,SKSizesR),reverse(SKSizesR,Sizes).

computeMinMass(VM,Min):- computeMassIndex(VM,List),length(List,Count),computeMinMass(VM,List,Count,Min).
computeMinMass(VM,List,Count,Min):- Count>VM.objs_max_len,length(Left,VM.objs_max_len),append(Left,_,List),
   computeMinMass(VM,Left,VM.objs_max_len,Min).
computeMinMass(VM,List,Count,Min):-  Count>20, length(Left,20), append(Left,_,List), computeMinMass(VM,Left,20,Min).
computeMinMass(VM,List,_Count,Min):-  last(List,SMin),max_min(VM.objs_min_mass,SMin,Min,_). 
computeMinMass(VM,List,_Count,Min):-  append(_,[A,B|_],List),A>B,max_min(VM.objs_max_mass,A,_Max,_),!,SMin is floor(B/2),
  max_min(VM.objs_min_mass,SMin,Min,_).

computeMaxMass(VM,Max):- computeMassIndex(VM,List),length(List,Count),computeMaxMass(VM,List,Count,Max).
computeMaxMass(VM,List,Count,Min):- Count>VM.objs_max_len,length(Left,VM.objs_max_len),append(Left,_,List),
   computeMaxMass(VM,Left,VM.objs_max_len,Min).
computeMaxMass(VM,List,Count,Min):-  Count>20, length(Left,20), append(Left,_,List), computeMaxMass(VM,Left,20,Min).
computeMaxMass(VM,List,_Count,Max):- append(_,[A,B|_],List),A>B,max_min(VM.objs_max_mass,A,Max,_),!.
computeMaxMass(VM,_List,_Count,Max):- max_min(VM.objs_max_mass,700,_,Max).


/*
proportional_objs(Obj1,Obj2,vis_hv_term(N)):- once((vis_hv_term(Obj1,N1),vis_hv_term(Obj2,N2))),proportional(N1,N2,N).
proportional_objs(Obj1,Obj2,loc_term(N)):- once((loc_term(Obj1,N1),loc_term(Obj2,N2))),proportional(N1,N2,N).
proportional_objs(Obj1,Obj2,center_term(N)):- center_term(Obj1,N1),center_term(Obj2,N2),proportional(N1,N2,N).
proportional_objs(Obj1,Obj2,color_diff(N)):- colors_cc(Obj1,N1),colors_cc(Obj2,N2),proportional(N1,N2,N).
proportional_objs(Obj1,Obj2,mass(N)):- once((mass(Obj1,N1),mass(Obj2,N2))),proportional_size(N1,N2,N).
*/


:- discontiguous learn_color_individuals_lib_one_way/17.

:- ensure_loaded(kaggle_arc_intruder).

    

individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV):- 
  forall(clause(learn_shapelib(PairName,In,Out,IH,IV,OH,OV),Body),rtrace_on_error(once(Body))).

individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV):- 
  forall(clause(individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO),Body),
    (rtrace_on_error(once(Body)),rtrace_on_error(show_pair_no_i(IH,IV,OH,OV,shared,PairName,ShapesI,ShapesO)))).

/*
individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):-
  forall(clause(learn_shapelib(PairName,In,Out,IH,IV,OH,OV),Body),rtrace_on_error(once(Body))),
  ShapesI=_,
  ShapesO=_,
  fail.
*/

% trying unique_colors per grid
individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- 
  add_note("trying unique_colors per grid"),
  unique_colors(In,ICs), unique_colors(Out,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  my_maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  individuals_from_pair_colors(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,ShapesI,ShapesO).

individuals_from_pair_colors(PairName,In,Out,IH,IV,OH,OV,
    _ICs,_IPCs,CommonCs,_OPCs,_OCs,
    _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL, ShapesI,ShapesO):- % fail,
    %add_cond(hasCommonColors(pair,CommonCs)), add_cond(hasPrivateColor(out,OPCs)), add_cond(hasPrivateColor(in,IPCs)),
    
    do_action(remove_colors(CommonCs,Out,OmI)), do_action(remove_colors(CommonCs,In,ImO)),
    
    show_pair_no_i(IH,IV,OH,OV,individuals_from_pair_colors,PairName,ImO,OmI),
    
    % input has its own noise
    CommonCsL>0,IPCsL>0,OPCsL==0,  
    %ignore((
    mass(ImO,IMass),mass(OmI,OMass),IMass>0, OMass==0,
    % individu ate([],[options([full])],ImO,NoiseObject), 
    grid_to_individual(ImO,NoiseObject),!,
    add_shape_lib(noise,NoiseObject),
    add_shape_lib(in,NoiseObject),
    remove_global_points(NoiseObject,In,NoiseFreeIn),
    show_pair_no_i(IH,IV,IH,IV,noise_objects_removed,PairName,NoiseObject,NoiseFreeIn),
    individuate(defaults,NoiseFreeIn,RestOfInObjs),
    add_shape_lib(pair,RestOfInObjs),
    individuate(defaults,Out,RestOfOutObjs),
    ShapesI = [NoiseObject|RestOfInObjs], 
    RestOfOutObjs = ShapesO.
/*

individuals_from_pair_colors(PairName,In,Out,IH,IV,OH,OV,
     _ICs,IPCs,CommonCs,OPCs,_OCs,
     _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL,
    RestOfInObjs,RestOfOutObjs):-
  CommonCsL>0,
  once((
  one_is_zero(IPCsL,OPCsL),
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(remove_colors(OPCs,Out,OmI)),
  remove_colors(IPCs,In,ImO),
  % mass(In,InMass),mass(Out,OutMass),
  mass(ImO,IMass),mass(OmI,OMass),
  IMass>0, OMass>0, OPCsL == 0,
  individu ate([],options([filltype(solid)(rectangle),defaults]),ImO,NewImO), 
  add_shape_lib(pair,NewImO),
  show_pair_no_i(IH,IV,OH,OV,'Filter noise',PairName,ImO,OmI),
  add_comparitor(-size2D),
  show_pair_no_i(IH,IV,OH,OV,'Filter noise',PairName,RestOfInObjs,OmI),
  add_shape_lib(in,RestOfInObjs),
  add_action(show_pair_no_i),
  individualizer_from_grid(PairName,out,Out,In,RestOfOutObjs))).

*/

/*
individualize_into_lib(in(_),Options,Grid):- !, individualize_into_lib(pair,Options,Grid).
individualize_into_lib(TargetLib,Options,Grid):-
  individu ate([],Options,Grid,Result),
  do_action(add_shape_lib(TargetLib,Result)).

switch_grid_lib(out(N),in(N)):- !, N<3.
switch_grid_lib(in(N),out(N2)):- N2 is N +1,!.
switch_grid_lib(X,X).
*/
                                                            
% trying unique_colors per grid
learn_shapelib(PairName,In,Out,IH,IV,OH,OV):- !,
  add_note("trying unique_colors per grid"),
  unique_colors(In,ICs), unique_colors(Out,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  my_maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  learn_color_individuals_lib(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL).

learn_color_individuals_lib(PairName,In,Out,IH,IV,OH,OV, ICs,IPCs,CommonCs,OPCs,OCs, ICsL,IPCsL,CommonCsL,OPCsL,OCsL):- 
  learn_color_individuals_lib_one_way(PairName,In,Out,IH,IV,OH,OV, ICs,IPCs,CommonCs,OPCs,OCs, ICsL,IPCsL,CommonCsL,OPCsL,OCsL).

learn_color_individuals_lib(PairName,In,Out,IH,IV,OH,OV, ICs,IPCs,CommonCs,OPCs,OCs, ICsL,IPCsL,CommonCsL,OPCsL,OCsL):- 
  learn_color_individuals_lib_one_way(PairName,Out,In,OH,OV,IH,IV, OCs,OPCs,CommonCs,IPCs,ICs, OCsL,OPCsL,CommonCsL,IPCsL,ICsL).

learn_color_individuals_lib_one_way(PairName,In,Out,IH,IV,OH,OV,
    _ICs,IPCs,CommonCs,OPCs,_OCs,
    _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL):- % fail,
  CommonCsL>0,
  IPCsL>0, OPCsL==0,
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(remove_colors(CommonCs,Out,OmI)),
  do_action(remove_colors(CommonCs,In,ImO)),
  show_pair_no_i(IH,IV,OH,OV,learn_color_individuals_lib_one_way,PairName,ImO,OmI),
  mass(ImO,IMass),mass(OmI,OMass),
  %one_is_zero(IMass,OMass),
  ((
  ignore((IMass>0, OMass==0, 
     IV<OV, IH<OH,
     grid_to_individual(ImO,NoiseObject),
     do_action(add_shape_lib(pair,NoiseObject)))),

  ignore((IMass>0, OMass==0, 
    individuate([options([by_color(IPCs)])],ImO,NewImO),    
    add_shape_lib(pair,NewImO))),

   true)).
  %tie_break_individualizer(PairName,ImO,OmI,IMass,OMass))).


one_is_zero(IMass,OMass):- 
  once(IMass>0;OMass>0),once(IMass=:=0;OMass=:=0).

remove_colors([],Out,Out):-!.
remove_colors([C|IPLs],In,Out):- 
 get_black(Black),
 subst001(In,C,Black,Mid),
 remove_colors(IPLs,Mid,Out).


:- include(kaggle_arc_footer).


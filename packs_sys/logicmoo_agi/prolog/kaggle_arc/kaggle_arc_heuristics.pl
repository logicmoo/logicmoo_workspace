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

recalc_sizes(VM,[After|TODO]):-
   recalc_sizes(VM),
   nop((set(VM.program_i) = [After,recalc_sizes|TODO])).
/*
   � amass(3) cc(blue,3.0) vis2D(1,3) loc2D(2,1) pen([]) birth(ifti3(nsew)) iz(symmetry(sym_hv)) center2D(2,2) layer(in) nth(21)
%amass(3) cc(cyan,3.0) vis2D(1,3) loc2D(1,1) pen([]) birth(ifti3(nsew)) iz(symmetry(sym_hv)) center2D(1,2) layer(in) nth(22)
%  Iz(Non Diag):         � amass(3) cc(green,3.0) vis2D(1,3) loc2D(3,1) pen([]) birth(ifti3(nsew)) iz(nsew) iz(rectangulator) iz(symmetry(sym_hv)) center2D(3,2) layer(in) nth(20)

*/

recalc_sizes(VM):- is_map(VM),
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

cullObjectsOutsideOfRanges(VM):- is_map(VM), length(VM.objs,N),N< floor(VM.objs_max_len/2),!.
cullObjectsOutsideOfRanges(VM):- is_map(VM), cullObjectsOutsideOf(VM,VM.objs_min_mass,VM.objs_max_mass). 
cullObjectsOutsideOf(VM,Min,Max):- 
  (var(Min)->computeMinMass(VM,Min);true),
  (var(Max)->computeMaxMass(VM,Max);true),
  Objs = VM.objs,
  my_partition(within_mass(Min,Max),Objs,Keep,Cull),
  length(Keep,Len),ignore((Len>20,
     print_side_by_side(Keep,Cull),
     my_partition(within_mass(1,1),Cull,_Ones,NonOnes),
     remObjects(VM,NonOnes))).

within_mass(Min,Max,Obj):- amass(Obj,Mass),between(Min,Max,Mass).

size_to_keys(N,N-N).
computeMassIndex(VM,Sizes):-  maplist(amass,VM.objs,UKeySizes), maplist(size_to_keys,UKeySizes,KSizes),keysort(KSizes,SKSizes),
  maplist(arg(2),SKSizes,SKSizesR),reverse(SKSizesR,Sizes).

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
proportional_objs(Obj1,Obj2,color_diff(N)):- colors(Obj1,N1),colors(Obj2,N2),proportional(N1,N2,N).
proportional_objs(Obj1,Obj2,amass(N)):- once((amass(Obj1,N1),amass(Obj2,N2))),proportional_size(N1,N2,N).
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
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
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
    amass(ImO,IMass),amass(OmI,OMass),IMass>0, OMass==0,
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
  % amass(In,InMass),amass(Out,OutMass),
  amass(ImO,IMass),amass(OmI,OMass),
  IMass>0, OMass>0, OPCsL == 0,
  individu ate([],options([solid(rectangle),defaults]),ImO,NewImO), 
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
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
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
  amass(ImO,IMass),amass(OmI,OMass),
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


:- fixup_exports.


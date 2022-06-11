/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- multifile learn_shapelib/7.
:- multifile individuals_from_pair/9.

:- discontiguous learn_shapelib/7.
:- discontiguous individuals_from_pair/9.


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
    mass(ImO,IMass),mass(OmI,OMass),IMass>0, OMass==0,
    % individuate([],[options([full])],ImO,NoiseObject), 
    grid_to_individual(ImO,NoiseObject),!,
    add_shape_lib(noise,NoiseObject),
    add_shape_lib(in,NoiseObject),
    remove_global_points(NoiseObject,In,NoiseFreeIn),
    show_pair_no_i(IH,IV,IH,IV,noise_objects_removed,PairName,NoiseObject,NoiseFreeIn),
    individuate_default(NoiseFreeIn,RestOfInObjs),
    add_shape_lib(pair,RestOfInObjs),
    individuate_default(Out,RestOfOutObjs),
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
  individuate([],options([solid(rectangle),defaults]),ImO,NewImO), 
  add_shape_lib(pair,NewImO),
  show_pair_no_i(IH,IV,OH,OV,'Filter noise',PairName,ImO,OmI),
  add_comparitor(-size),
  show_pair_no_i(IH,IV,OH,OV,'Filter noise',PairName,RestOfInObjs,OmI),
  add_shape_lib(in,RestOfInObjs),
  add_action(show_pair_no_i),
  individualizer_from_grid(PairName,out,Out,In,RestOfOutObjs))).

*/

/*
individualize_into_lib(in(_),Options,Grid):- !, individualize_into_lib(pair,Options,Grid).
individualize_into_lib(TargetLib,Options,Grid):-
  individuate([],Options,Grid,Result),
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
 subst_w_attv(In,C,black,Mid),
 remove_colors(IPLs,Mid,Out).


:- fixup_exports.


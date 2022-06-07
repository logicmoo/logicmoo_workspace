/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- discontiguous individualizers_from_pair/9.
:- discontiguous grid_color_individualizer/19.

:- ensure_loaded(kaggle_arc_intruder).


  %  *->true;individualizer_fallback(PairName,In,Out,IH,IV,OH,OV, RestOfInObjs,RestOfOutObjs)).


% trying unique_colors per grid
individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs):- 
  add_note("trying unique_colors per grid"),
  unique_colors(In,ICs), unique_colors(Out,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(my_len,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  grid_color_individualizer(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    RestOfInObjs,RestOfOutObjs).


grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
    _ICs,IPCs,CommonCs,OPCs,_OCs,
    _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL,
    I,O):- % fail,
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),

  do_action(delete_colors(CommonCs,Out,OmI)),
  do_action(delete_colors(CommonCs,In,ImO)),

  show_pair_indivs(IH,IV,OH,OV,grid_color_individualizer,PairName,ImO,OmI),

  % input has its own noise
  CommonCsL>0,IPCsL>0,OPCsL==0,  
  ignore((mass(ImO,IMass),mass(OmI,OMass),IMass>0, OMass==0,
   % individuate([],[options([full])],ImO,NoiseObject), 
    grid_to_individual(ImO,NoiseObject),!,
    add_shape_lib(noise,NoiseObject),
    %do_action(add_shape_lib(in,NoiseObject)),
    remove_global_points(NoiseObject,In,NoiseFreeIn),
          show_pair_indivs(IH,IV,IH,IV,noise_objects_removed,PairName,NoiseObject,NoiseFreeIn),
    individuate([],[options([default])],NoiseFreeIn,RestOfInObjs),
    add_shape_lib(pair,RestOfInObjs),
    individuate([],[options([default])],Out,RestOfOutObjs))),
  I = [NoiseObject|RestOfInObjs],
  RestOfOutObjs = O.

/*
individualizer_fallback(_PairName,In,Out,_IH,_IV,_OH,_OV,RestOfInObjs,RestOfOutObjs):- 
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(my_len,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,RestOfInObjs);
   (IMass>OMass) -> individuate(UnsharedOut,In,RestOfOutObjs)),
  add_rule(less_shapes(IMass,OMass)).
*/
/*
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(my_len,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,RestOfInObjs);
   (IMass>OMass) -> individuate(UnsharedOut,In,RestOfOutObjs)),
  add_rule(less_shapes(IMass,OMass)).
*/
/*
individualizer_from_grid(PairName,InOrOut,In,Out,RestOfOutObjs):-
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  individualizer_from_grid(PairName,InOrOut,In,IH,IV,Out,OH,OV,RestOfOutObjs).

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,RestOfOutObjs):-
   (IH<6;IV<6),!,make_indivs_options([retain_grid(full),dots,done],RestOfOutObjs).

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,RestOfOutObjs):-
  (IH>15,IV>15),!,make_indivs_options([-(=(dots)),fourway,defaults],RestOfOutObjs).
  
individualizer_from_grid(_PairName,_InOrOut,_In,_IH,_IV,_Out,_OH,_OV,[]):-!.
*/  
/*
grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
  ICs,IPCs,CommonCs,OPCs,OCs,
  ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
  RestOfInObjs,RestOfOutObjs):- 
  grid_color_individualizer0(PairName,In,Out,IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    RestOfInObjs,RestOfOutObjs).

grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
  ICs,IPCs,CommonCs,OPCs,OCs,
  ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
  RestOfInObjs,RestOfOutObjs):- 
  grid_color_individualizer0(PairName,Out,In,OH,OV,IH,IV,
    OCs,OPCs,CommonCs,IPCs,ICs,
    OCsL,OPCsL,CommonCsL,IPCsL,ICsL,
    RestOfOutObjs,RestOfInObjs).
  */                                                          

individualize_into_lib(in(_),Options,Grid):- !, individualize_into_lib(pair,Options,Grid).
individualize_into_lib(TargetLib,Options,Grid):-
  individuate([],Options,Grid,Result),
  do_action(add_shape_lib(TargetLib,Result)).

switch_grid_lib(out(N),in(N)):- !, N<3.
switch_grid_lib(in(N),out(N2)):- N2 is N +1,!.
switch_grid_lib(X,X).
individualizers_from_pair(TargetLib,_PairName,In,_Out,IH,IV,_OH,_OV):-
   (IH<6;IV<6),individualize_into_lib(TargetLib,[dots,release_points,full,done],In).


/*


          
          IPCsL>0, OPCsL==0,

    )),
  
  ignore((OMass==0, IMass>0
    individuate([options([full])],ImO,NoiseObject), do_action(add_shape_lib(pair,NoiseObject)))),
  individuate([options([full])],ImO,NoiseObject), do_action(add_shape_lib(pair,NoiseObject)),
  %individuate([options([by_color(IPCs)])],ImO,NewImO), do_action(add_shape_lib(noise,NewImO)),
  %individuate([options([by_color(IPCs)])],OmO,NewImO), do_action(add_shape_lib(noise,NewImO)),
  RestOfOutObjs=RestOfInObjs,
%  tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesIO,ShapesIO):-
    grid_size(ImO,IH,IV), grid_size(OmI,OH,OV),
    show_pair_indivs(IH,IV,OH,OV,tie_break,PairName,ImO,OmI),  
    ((IMass==0, OMass>0) -> (individuate([],OmI,RestOfOutObjs)) ;
      ((OMass==0, IMass>0) -> (individuate([full],ImO,RestOfInObjs)))))).

   
    %nb_current(rules,Rules),
    %pt("USING-RULE"=Info+Rules),
 %   nop(print_grid(USE)).
  %tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,RestOfInObjs,RestOfOutObjs))).
*/

/*

grid_color_individualizer0(PairName,In,Out,IH,IV,OH,OV,
     _ICs,IPCs,CommonCs,OPCs,_OCs,
     _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL,
    RestOfInObjs,RestOfOutObjs):- fail,
  CommonCsL>0,
  once((
  one_is_zero(IPCsL,OPCsL),
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(delete_colors(OPCs,Out,OmI)),
  delete_colors(IPCs,In,ImO),
  % mass(In,InMass),mass(Out,OutMass),
  mass(ImO,IMass),mass(OmI,OMass),
  IMass>0,OMass>0, OPCsL == 0,
  RestOfInObjs = options([solid(squares),defaults]),
  individuate([options([solid(squares)])],ImO,NewImO), do_action(add_shape_lib(pair,NewImO)),
  add_indiv(in,RestOfInObjs),
  add_comparitor(-size),
  show_pair_indivs(IH,IV,OH,OV,'Filter noise',PairName,ImO,OmI),
  add_action(show_pair_indivs),
  individualizer_from_grid(PairName,out,Out,In,RestOfOutObjs))).

*/

one_is_zero(IMass,OMass):- 
  once(IMass>0;OMass>0),once(IMass=:=0;OMass=:=0).

delete_colors([],Out,Out):-!.
delete_colors([C|IPLs],In,Out):- 
 subst_w_attv(In,C,black,Mid),
 delete_colors(IPLs,Mid,Out).


:- fixup_exports.


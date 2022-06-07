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


individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):-
  (individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO)
    *->true;individualizer_fallback(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO)).

individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):-
  individualizers_from_pair_intruder(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO).

tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesIO,ShapesIO):-
  grid_size(ImO,IH,IV), grid_size(OmI,OH,OV),
  show_pair_indivs(IH,IV,OH,OV,tie_break,PairName,ImO,OmI),  
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO;
    (( OMass > IMass) -> USE = ImO;  USE = OmI))),
  individuate([],USE,ShapesIO),
  %nb_current(rules,Rules),
  %pt("USING-RULE"=Info+Rules),
  nop(print_grid(USE)).

% trying unique_colors per grid
individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- 
  add_note("trying unique_colors per grid"),
  unique_colors(In,ICs), unique_colors(Out,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  grid_color_individualizer(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    ShapesI,ShapesO).

individualizer_fallback(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- 
	individualizer_from_grid(PairName,in,In,IH,IV,Out,OH,OV,ShapesI),
	individualizer_from_grid(PairName,out,Out,OH,OV,In,IH,IV,ShapesO),!.
/*
individualizer_fallback(_PairName,In,Out,_IH,_IV,_OH,_OV,ShapesI,ShapesO):- 
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(length,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,ShapesI);
   (IMass>OMass) -> individuate(UnsharedOut,In,ShapesO)),
  add_rule(less_shapes(IMass,OMass)).
*/
/*
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(length,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,ShapesI);
   (IMass>OMass) -> individuate(UnsharedOut,In,ShapesO)),
  add_rule(less_shapes(IMass,OMass)).
*/
individualizer_from_grid(PairName,InOrOut,In,Out,ShapesO):-
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  individualizer_from_grid(PairName,InOrOut,In,IH,IV,Out,OH,OV,ShapesO).

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,ShapesO):-
   (IH<6;IV<6),!,make_indivs_options([retain_grid(full),dots,done],ShapesO).

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,ShapesO):-
  (IH>15,IV>15),!,make_indivs_options([-(=(dots)),fourway,defaults],ShapesO).
  
individualizer_from_grid(_PairName,_InOrOut,_In,_IH,_IV,_Out,_OH,_OV,[]):-!.
  

grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
  ICs,IPCs,CommonCs,OPCs,OCs,
  ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
  ShapesI,ShapesO):- 
  grid_color_individualizer0(PairName,In,Out,IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    ShapesI,ShapesO).

grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
  ICs,IPCs,CommonCs,OPCs,OCs,
  ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
  ShapesI,ShapesO):- 
  grid_color_individualizer0(PairName,Out,In,OH,OV,IH,IV,
    OCs,OPCs,CommonCs,IPCs,ICs,
    OCsL,OPCsL,CommonCsL,IPCsL,ICsL,
    ShapesO,ShapesI).
                                                            

grid_color_individualizer0(PairName,In,Out,IH,IV,OH,OV,
    _ICs,IPCs,CommonCs,OPCs,_OCs,
    _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL,
    ShapesI,ShapesO):- % fail,
  CommonCsL>0,
  IPCsL>0, OPCsL==0,
    once((
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(delete_colors(CommonCs,Out,OmI)),
  do_action(delete_colors(CommonCs,In,ImO)),
  show_pair_indivs(IH,IV,OH,OV,grid_color_individualizer,PairName,ImO,OmI),
  mass(ImO,IMass),mass(OmI,OMass),
  %one_is_zero(IMass,OMass),
  individuate([options([full])],ImO,NoiseObject), do_action(add_shape_lib(pair,NoiseObject)),
  individuate([options([by_color(IPCs)])],ImO,NewImO), do_action(add_shape_lib(noise,NewImO)),
  tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesI,ShapesO))).



grid_color_individualizer0(PairName,In,Out,IH,IV,OH,OV,
     _ICs,IPCs,CommonCs,OPCs,_OCs,
     _ICsL,IPCsL,CommonCsL,OPCsL,_OCsL,
    ShapesI,ShapesO):-% fail,
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
  ShapesI = options([solid(squares),defaults]),
  individuate([options([solid(squares)])],ImO,NewImO), do_action(add_shape_lib(pair,NewImO)),
  add_indiv(in,ShapesI),
  add_comparitor(-size),
  show_pair_indivs(IH,IV,OH,OV,'Filter noise',PairName,ImO,OmI),
  add_action(show_pair_indivs),
  individualizer_from_grid(PairName,out,Out,In,ShapesO))).



one_is_zero(IMass,OMass):- 
  once(IMass>0;OMass>0),once(IMass=:=0;OMass=:=0).

delete_colors([],Out,Out):-!.
delete_colors([C|IPLs],In,Out):- 
 subst_w_attv(In,C,black,Mid),
 delete_colors(IPLs,Mid,Out).


:- fixup_exports.


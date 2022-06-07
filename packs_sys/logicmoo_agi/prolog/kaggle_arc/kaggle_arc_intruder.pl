/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


test( " Fill the smallest square hole?",
"_________________________     _________________________
|                         |   |                         |
|   l l l l l             |   |   N N N N N             |
|   l       l             |   |   N       N             |
|   l       l   n n n n   |   |   N       N   n n n n   |
|   l       l   n     n   |   |   N       N   n x x n   |
|   l       l   n     n   |   |   N       N   n x x n   |
|   l l l l l   n n n n   |   |   N N N N N   n n n n   |
|                         |   |                         |
|       N N N N N N       |   |       ? ? ? ? ? ?       |
|       N         N       |   |       ?         ?       |
|       N         N       |   |       ?         ?       |
|       N N N N N N       |   |       ? ? ? ? ? ?       |
 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯     ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯").


% Grid subtraction
individualizers_from_pair_intruder(PairName,In,Out,H,V,H,V,RestOfInObjs,RestOfOutObjs):- fail,
 once((
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),mass(OmI,OMass),
  show_pair_indivs(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO)),
   individuate([options(defaults)],USE,Intruder),
   add_shape_lib(pair,Intruder),
   individuate([],[options([default])],In,RestOfInObjs),
   add_shape_lib(pair,RestOfInObjs),
   individuate([],[options([default])],Out,RestOfOutObjs))).


% intruder to Out
individualizers_from_pair_intruder(_PairName,In,Out,IH,IV,OH,OV,NoiseObject,Intruder):-
  (IV > OV; IH> OH) , ogs(_,_,Out,In), 
  individuate([],[options([full])],Out,Intruder), 
  add_shape_lib(pair,Intruder),
  individuate([],[options([default])],In,NoiseObject),
  add_shape_lib(noise,NoiseObject).

% intruder was in ./. now in a scene in out
individualizers_from_pair_intruder(_PairName,Out,In,OH,OV,IH,IV,Intruder,NoiseObject):-
  (IV > OV; IH> OH) , ogs(_,_,Out,In), 
  individuate([],[options([full])],Out,Intruder), 
  add_shape_lib(pair,Intruder),
  individuate([],[options([default])],In,NoiseObject),
  add_shape_lib(noise,NoiseObject).


% Grid subtraction
individualizers_from_pair_intruder(PairName,In,Out,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs):- fail,
 once((
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),%mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),%mass(OmI,OMass),
  show_pair_indivs(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  unique_colors(ImO,ICs), unique_colors(OmI,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(my_len,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  grid_color_individualizer(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    RestOfInObjs,RestOfOutObjs))).



:- fixup_exports.


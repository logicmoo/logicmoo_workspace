/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


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
individualizers_from_pair_intruder(PairName,In,Out,H,V,H,V,ShapesI,ShapesO):-
 once((
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),mass(OmI,OMass),
  show_pair_indivs(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  %one_is_zero(IMass,OMass),
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO)),
  individuate([options(defaults)],USE,Intruder),
  ShapesI=ShapesO,
  ShapesI=Intruder,
  do_action(add_shape_lib(pair,Intruder)))),
  fail.

% intruder map
individualizers_from_pair_intruder(_PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):-
  ((IV > OV; IH> OH) -> find_intruder(In,Out,Intruder) ; find_intruder(Out,In,Intruder)),
  ShapesI=ShapesO,
  ShapesI=Intruder,
  do_action(add_shape_lib(pair,Intruder)),
  fail.

% find the smallest object on a map is also the interuder game map
individualizers_from_pair_intruder(_PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- fail, %covered above
  ((IV > OV; IH> OH) -> find_intruder(In,Out,Intruder) ; find_intruder(Out,In,Intruder)),
  ShapesI=ShapesO,
  ShapesI=Intruder,
  do_action(add_shape_lib(pair,Intruder)).

%intruder is the output image that was found in the input image
find_intruder(In,Out,Intruder):-
   ogs(_,_,Out,In),
   individuate([options([full])],In,Intruder),
   do_action(add_shape_lib(sol,Intruder)),!.



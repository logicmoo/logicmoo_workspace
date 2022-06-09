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
:- multifile learn_shapelib/7.
:- discontiguous learn_shapelib/7.
learn_shapelib(PairName,In,Out,IH,IV,OH,OV):-
  forall(clause(learn_intruders(PairName,In,Out,IH,IV,OH,OV),Body), rtrace_on_error(once(Body))).

learn_intruders(PairName,In,Out,IH,IV,OH,OV):- nop(learn_intruders(PairName,In,Out,IH,IV,OH,OV)).

rtrace_on_error(Goal):- !, call(Goal),!.
rtrace_on_error(Goal):- catch(quietly(Goal),E,(notrace,dmsg(E=Goal),break,1==1,rtrace(Goal))).

:- multifile individuals_from_pair/9.
:- discontiguous individuals_from_pair/9.
% Grid subtraction
individuals_from_pair(_PairName,In,Out,H,V,H,V,RestOfInObjs,RestOfOutObjs):- 
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),mass(OmI,OMass),
  %show_pair_no_i(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO)),
   individuate([options(defaults)],USE,Intruder),
   add_shape_lib(pair,Intruder),
   individuate_default(In,RestOfInObjs),
   add_shape_lib(pair,RestOfInObjs),
   individuate_default(Out,RestOfOutObjs).


% intruder to Out
individuals_from_pair(_PairName,In,Out,IH,IV,OH,OV,[Intruder|NoiseObjects],[Intruder]):-
  (IV > OV; IH> OH) , ogs(_,_,Out,In), 
  grid_to_individual(Out,Intruder),
  add_shape_lib(pair,Intruder),
  individuate_default(In,NoiseObjects),
  nop(add_shape_lib(noise,NoiseObjects)).

% intruder was in ./. now in a scene in out
individuals_from_pair(_PairName,Out,In,OH,OV,IH,IV,[Intruder],[Intruder|NoiseObjects]):-
  (IV > OV; IH> OH) , ogs(_,_,Out,In), 
  grid_to_individual(Out,Intruder),
  add_shape_lib(pair,Intruder),
  individuate_default(In,NoiseObjects),
  nop(add_shape_lib(noise,NoiseObjects)).


% Grid subtraction
individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs):- 
 ((
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),%mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),%mass(OmI,OMass),
  %show_pair_no_i(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  unique_colors(ImO,ICs), unique_colors(OmI,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  individuals_from_pair_colors(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    RestOfInObjs,RestOfOutObjs))).



:- fixup_exports.


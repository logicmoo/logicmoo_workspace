/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.
:- discontiguous stuff_options1/2.
:- discontiguous stuff_options1/3.
:- discontiguous stuff_options/2.
:- discontiguous stuff_options/3.

test( " Fill the smallest square hole?",
"_____________     _____________
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

:- discontiguous learn_intruders/7.

learn_intruders(PairName,In,Out,IH,IV,OH,OV):- nop(learn_intruders(PairName,In,Out,IH,IV,OH,OV)).

rtrace_on_error(Goal):- !, call(Goal),!.
rtrace_on_error(Goal):- catch(quietly(Goal),E,(notrace,dmsg(E=Goal),break,1==1,rtrace(Goal))).

:- multifile individuals_from_pair/9.
:- discontiguous individuals_from_pair/9.
% Grid subtraction
individuals_from_pair(PairName,In,Out,H,V,H,V,RestOfInObjs,RestOfOutObjs):- 
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),amass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),amass(OmI,OMass),
   show_pair_no_i(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO)),
   print_grid(USE),
   individuate(complete,USE,Intruder),
   add_shape_lib(intruder,Intruder),
   individuate(defaults,In,RestOfInObjs),
   add_shape_lib(pair,RestOfInObjs),
   individuate(complete,Out,RestOfOutObjs).


% intruder to Out
individuals_from_pair(_PairName,In,Out,IH,IV,OH,OV,[Intruder|NoiseObjects],[Intruder]):-
  (IV > OV; IH> OH) , find_ogs(_,_,Out,In), 
  grid_to_individual(Out,Intruder),
  add_shape_lib(intruder,Intruder),
  individuate(defaults,In,NoiseObjects),
  nop(add_shape_lib(noise,NoiseObjects)).

% intruder was in ./. now in a scene in out
individuals_from_pair(_PairName,Out,In,OH,OV,IH,IV,[Intruder],[Intruder|NoiseObjects]):-
  (IV > OV; IH> OH) , find_ogs(_,_,Out,In), 
  grid_to_individual(Out,Intruder),
  add_shape_lib(intruder,Intruder),
  individuate(defaults,In,NoiseObjects),
  nop(add_shape_lib(noise,NoiseObjects)).

set_prop_of(_NeuralVM,In,Prop,Val):- root_key(Prop,RHT),ht_update(RHT,In,_,Val).
get_prop_of(_NeuralVM,In,Prop,Val):- root_key(Prop,RHT),ht_get(RHT,In,Val).

root_key(Key,RHT):- current_neurons(NeuralVM),(ht_get(NeuralVM,Key,RHT)->true;(ht_new(RHT),ht_put_new(NeuralVM,Key,RHT))).


current_neurons(NeuralVM):- luser_getval(system_props,NeuralVM).
:- ht_new(NeuralVM), luser_linkval(system_props,NeuralVM).

% Grid subtraction
learn_intruders(PairName,In,Out,IH,IV,OH,OV):- %trace, 
  current_neurons(NeuralVM),
  set_prop_of(NeuralVM,NeuralVM,in,In),
  set_prop_of(NeuralVM,NeuralVM,out,Out),
  in_out_xform(NeuralVM,PairName,[In,Out],[ImO,OmI],[PLAN]),
  wdmsg(learn_intruders=PLAN),
  individualizer_heuristics(PairName,ImO,OmI,IH,IV,OH,OV).

individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs):-
  current_neurons(NeuralVM),
  in_out_xform(NeuralVM,PairName,[In,Out],[ImO,OmI],[PLAN]),
  (In\=@=ImO;Out\=@=OmI),
  wdmsg(individuals_from_pair=PLAN),
  individuals_from_pair(PairName,ImO,OmI,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs).
  

/*% Grid subtraction
individuals_from_pair(PairName,In,Out,IH,IV,OH,OV,RestOfInObjs,RestOfOutObjs):- 
 ((
  add_note("trying grid minus grid"),
  in_out_xform(NeuralVM,PairName,In,Out,IH,IV,OH,OV,NameOf),
  %show_pair_no_i(H,V,H,V,grid_subtraction,PairName,ImO,OmI),
  unique_colors(ImO,ICs), unique_colors(OmI,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  individuals_from_pair_colors(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    RestOfInObjs,RestOfOutObjs))).
*/

use_common_conj(A,B,C):- intersection(A,B,C,_,_).
use_disjunction(A,B,C):- intersection(A,B,_,C,_).
use_missing_stuff(A,B,C):- intersection(A,B,_,_,C).

stuff_options(overlapType,use_common_conj).
stuff_options1(overlapType,use_disjunction).
stuff_options(overlapType,use_missing_stuff).

get_in_out(NeuralVM,_,IIn,OOut):-  set_prop_of(NeuralVM,NeuralVM,in,IIn), set_prop_of(NeuralVM,NeuralVM,out,OOut).
%get_in_out(NeuralVM,TestID*ExampleNum,IIn,OOut):- kaggle_arc(TestID,ExampleNum,IIn,OOut).

get_self_xform(_NeuralVM,_PairName,In,In):- nonvar(In).
get_self_xform(NeuralVM,_PairName,_In,In):- get_prop_of(NeuralVM,NeuralVM,in,In).
get_self_temp_xform(NeuralVM,_PairName,In,Buffer):- get_prop_of(NeuralVM,In,buffer,Buffer).
get_self_shared_xform(NeuralVM,_PairName,In,Buffer):- get_prop_of(NeuralVM,In,shared_xform,Buffer),!.
get_other_xform(NeuralVM,TestID*ExampleNum,In,Out):- get_in_out(NeuralVM,TestID*ExampleNum,IIn,OOut),(IIn=@=In->OOut=Out;OOut=@=Out->In=IIn;fail).
get_other_temp_xform(NeuralVM,PairName,In,Buffer):- get_other_xform(NeuralVM,PairName,In,Out),get_self_temp_xform(NeuralVM,PairName,Out,Buffer).
get_other_shared_xform(NeuralVM,PairName,In,Buffer):- get_other_xform(NeuralVM,PairName,In,Out),get_self_shared_xform(NeuralVM,PairName,Out,Buffer).

set_self_xform(_NeuralVM,_PairName,In,In).
set_self_temp_xform(NeuralVM,_PairName,In,Buffer):- get_prop_of(NeuralVM,In,buffer,Buffer).
set_self_shared_xform(NeuralVM,_PairName,In,Buffer):- get_prop_of(NeuralVM,In,shared_xform,Buffer),!.
set_other_xform(NeuralVM,PairName,In,Buffer):- get_other_xform(NeuralVM,PairName,In,Out),set_self_xform(NeuralVM,PairName,Out,Buffer).
set_other_temp_xform(NeuralVM,PairName,In,Buffer):- get_other_xform(NeuralVM,PairName,In,Out),set_self_temp_xform(NeuralVM,PairName,Out,Buffer).
set_other_shared_xform(NeuralVM,PairName,In,Buffer):- get_other_xform(NeuralVM,PairName,In,Out),set_self_shared_xform(NeuralVM,PairName,Out,Buffer).

stuff_options1(fromType,get_self_xform).
stuff_options(fromType,get_self_temp_xform).
stuff_options(fromType,get_self_shared_xform).
stuff_options(fromType,get_other_xform).
stuff_options(fromType,get_other_temp_xform).
stuff_options(fromType,get_self_shared_xform).

stuff_options(targetType,set_self_xform).
stuff_options(targetType,set_self_temp_xform).
stuff_options(targetType,set_self_shared_xform).
stuff_options1(targetType,set_other_xform).
stuff_options(targetType,set_other_temp_xform).
stuff_options(targetType,set_self_shared_xform).


in_out_xform(_NeuralVM,_PairName,ResultInOut,ResultInOut,[]).
in_out_xform(NeuralVM,PairName,StartInOut,ResultInOut,[DONE|MORETODO]):-
 DONE=withStuff(SourceType,StuffType,TargetType,OverlapType,WhatWithType),
 findall(DONE,
   (stuff_options1(fromType,SourceType),
    stuff_options1(targetType,TargetType),
    dif(SourceType,TargetType),
    stuff_options1(stuffType,_,StuffType),
    stuff_options1(overlapType,OverlapType),
    stuff_options1(whatWithType,WhatWithType)),
  Options),
  random_permutation(Options,ROptions),
  writeq(in_out_xform=ROptions),nl,
  member(DONE,ROptions),
  writeq(dONE=DONE),nl,
  maybe_in_out_xform(NeuralVM,PairName,StartInOut,NextStartInOut,DONE),
  in_out_xform(NeuralVM,PairName,NextStartInOut,ResultInOut,MORETODO).


maybe_in_out_xform(NeuralVM,PairName,StartInOut,NextStartInOut,DONE):-
 arcST,trace,
 DONE=withStuff(SourceType,StuffType,TargetType,OverlapType,WhatWithType),
  must_det_ll((
    maplist(call(SourceType,NeuralVM,PairName),StartInOut,FromInOut),
    maplist(call(TargetType,NeuralVM,PairName),StartInOut,TargetInOut),
    maplist(StuffType,FromInOut,StuffInOut),
    maplist(call(OverlapType,TargetInOut),StuffInOut,OverlapInOut),  
    maplist(call(WhatWithType,StuffType,OverlapInOut),TargetInOut,FromInOut,NextStartInOut)
  )).


stuff_options1(stuffType,is_cpoints,globalpoints). %:- globalpoints(Grid,Stuff).
stuff_options1(stuffType,is_colors,unique_colors). %:- unique_colors(Grid,Stuff).
stuff_options(stuffType,is_nc_points,shape). %:-  globalpoints(Grid,Stuff),decolorize(StuffM,Stuff).
stuff_options(stuffType,is_group,default_individuals). %:- individuals_default(Grid,Stuff).


remove_stuff_matching(_,_,[],Result,Result):-!.
remove_stuff_matching(A,B,[H|T],Target,Result):-!,remove_stuff_matching(A,B,H,Target,MResult),remove_stuff_matching(A,B,T,MResult,Result).
remove_stuff_matching(_,is_color,Each,Target,Result):- remove_colors(Each,Target,Result),!.
remove_stuff_matching(is_cpoints,_,Each,Target,Result):- remove_global_points(Each,Target,Result),!.
remove_stuff_matching(_,is_cpoints,Each,Target,Result):- remove_global_points(_-Each,Target,Result),!.
remove_stuff_matching(_,_,Each,Target,Result):- remove_global_points(Each,Target,Result),!.

change_stuff_not_matching(_,_,[],Result,Result):-!.
change_stuff_not_matching(A,B,[H|T],Target,Result):-!,add_stuff_missing(A,B,H,Target,MResult),add_stuff_missing(A,B,T,MResult,Result).
change_stuff_not_matching(A,B,C,Target,Result):- B= is_color, add_stuff_missing(A,B,C,Target,Result),!.
change_stuff_not_matching(is_cpoints,_,Each,Target,Result):- add_global_points(Each,Target,Result),!.
change_stuff_not_matching(_,is_cpoints,Each,Target,Result):- add_global_points(Each,Target,Result),!.
change_stuff_not_matching(_,_,Each,Target,Result):- add_global_points(Each,Target,Result),!.

add_stuff_missing(_,_,[],Result,Result):-!.
add_stuff_missing(A,B,[H|T],Target,Result):-!,add_stuff_missing(A,B,H,Target,MResult),add_stuff_missing(A,B,T,MResult,Result).
add_stuff_missing(A,B,C,Target,Result):- B= is_color,
   globalpoints(Target,CPoints),my_partition(=(C-_),CPoints,Includes,_),add_stuff_missing(A,B,Includes,Target,Result).
add_stuff_missing(is_cpoints,_,Each,Target,Result):- add_global_points(Each,Target,Result),!.
add_stuff_missing(_,is_cpoints,Each,Target,Result):- add_global_points(Each,Target,Result),!.
add_stuff_missing(_,_,Each,Target,Result):- add_global_points(Each,Target,Result),!.

stuff_options1(whatWithType,remove_stuff_matching).
stuff_options(whatWithType,change_stuff_not_matching).
stuff_options(whatWithType,add_stuff_missing).



:- fixup_exports.


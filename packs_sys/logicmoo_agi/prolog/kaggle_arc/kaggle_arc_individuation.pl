/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
/*

Rule types

i1 to o1
i2 to o1
o1 to o1



*/
:- include(kaggle_arc_header).
:- multifile is_fti_step/1.
:- discontiguous is_fti_step/1.
:- discontiguous is_fti_stepr/1.
:- discontiguous toplevel_individuation/1. 




individuation_macros(do_ending, [
  maybe_lo_dots,
  %find_edges,
  % find_contained_points, % mark any "completely contained points"
 %combine_same_globalpoints, % make sure any objects are perfectly the equal part of the media(image) are iz(flag(combined))
 %keep_only_shown(1),
 %remove_if_prop(and(cc(bg,1))),
 remove_illegals,
 %combine_if_prop(and(cc(bg,1),)),
 %remove_if_prop(and(iz(stype(dot))])),
 %combine_same_globalpoints,
 %reset_points, 
 %gather_cached,
 remove_used_points,
 named_grid_props(post_indiv),
 remove_background_only_object,
 find_relations,
 remove_dead_links,
 %find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
 %find_subsumes,
 %find_overlaps,
 %find_touches,
 %find_sees,
 %remove_if_prop(and(link(contained_by,_),cc(fg,0))),
 %remove_if_prop(and(giz(g(out)),cc(fg,0))),
 %remove_dead_links,
 %combine_same_globalpoints,  
 extend_obj_proplists,
 really_group_vm_priors,
 %whole,
 %combine_objects, 
 end_of_macro]).

no_fg_mass(Zero):- mass(Zero,Mass),!,Mass=0.
i_minus_o_equals_none:- i_minus_o(Zero), no_fg_mass(Zero).
o_minus_i_equals_some:- o_minus_i(Zero), \+ no_fg_mass(Zero).
mono_i_minus_o_equals_none:- mono_i_minus_o(Zero), no_fg_mass(Zero).
mono_o_minus_i_equals_some:- mono_o_minus_i(Zero), \+ no_fg_mass(Zero).

i_minus_o(Zero):- current_pair_io(I,O),mapgrid(cell_minus_cell,I,O,Zero).
o_minus_i(Zero):- current_pair_io(I,O),mapgrid(cell_minus_cell,O,I,Zero).

i_intersect_o(Zero):- current_pair_io(I,O),mapgrid(fg_intersectiond,O,I,Zero).
mono_i_intersect_o(Zero):- current_pair_io(I,O),mapgrid(fg_intersectiond_mono,O,I,Zero).

mono_i_minus_o(Zero):- current_pair_io(I,O),mapgrid(mono_cell_minus_cell,I,O,Zero).
mono_o_minus_i(Zero):- current_pair_io(I,O),mapgrid(mono_cell_minus_cell,O,I,Zero).

show_minuses:-
  maplist(show_minuses,[i_minus_o,mono_i_minus_o,mono_o_minus_i,o_minus_i],SS),
  print_ss(SS).

show_minuses(P1,P1=G):- call(P1,G).




individuation_macros(complete, [i_complete]).
individuation_macros(i_complete, ListO):- 
 must_det_ll((
   im_complete(ListC),
   flatten([ListC,do_ending],ListM),
   list_to_set(ListM,ListS))),!,
 ListO=ListS.

pair_test(mono_i_to_o_is_none_some_none).
pair_test(i_to_o_is_none_some_some).
pair_test(mono_i_to_o_is_none_some_some_not_color).


mono_i_to_o_is_none_some_some_not_color:-
   \+ i_to_o_is_none_some_some,
   mono_i_minus_o_equals_none,mono_o_minus_i_equals_some.


i_to_o_is_none_some_some:- i_minus_o_equals_none,o_minus_i_equals_some.

mono_i_to_o_is_none_some_none:- 
  mono_i_minus_o(Zero1),no_fg_mass(Zero1),
  mono_o_minus_i(Zero2),no_fg_mass(Zero2),
  o_minus_i_equals_some.



im_complete(do_im_complete).

is_fti_step(do_im_complete).

do_im_complete(VM):- is_im_complete(Info), run_fti(VM,Info).

%is_im_complete(colormass):- 
is_im_complete(i_to_o_is_none_some_some):- i_to_o_is_none_some_some,!.
is_im_complete(mono_i_to_o_is_none_some_none):- mono_i_to_o_is_none_some_none,!.
is_im_complete(ListO):- test_config(indiv(ListO)), [i_repair_patterns]\=@= ListO,[i_repair_patterns_f]\=@= ListO,[any]\= ListO,['']\= ListO,''\= ListO,!.
is_im_complete(i_complete_generic).
%im_complete(ListO):- ListO=[nsew,all_lines,diamonds,do_ending].
%im_complete([i_repair_patterns]):- get_current_test(TestID),is_symgrid(TestID),!.
%im_complete(i_repair_patterns):-!.

is_output_vm(VM):- VM.id=(_>(trn+N)*out),!,integer(N).
is_input_vm(VM):- VM.id=(_>(trn+N)*in),!,integer(N).


fg_mass_geq(Two,O):- globalpoints(O,GP),include(is_fg_point,GP,FG), length(FG,Len),Len>=Two.
% =====================================================================
is_fti_step(i_to_o_is_none_some_some).
% =====================================================================

i_to_o_is_none_some_some(VM):- \+ is_output_vm(VM),!,i_to_o_is_none_some_some_in(VM),!.
i_to_o_is_none_some_some(VM):-    is_output_vm(VM),!,
  set_example_num_vm(VM),
  o_minus_i(NewGrid), 
  gset(VM.objs)=[],
  show_minuses,
  set_vm_grid_now(VM,NewGrid),
  i_intersect_o(Intersect),
  individuate([keypads],Intersect,AddObjs),
  my_maplist(mergeObject(VM),AddObjs),
  run_fti(VM,generic_nsew_colormass),
  current_pair_io(I,_O),
  set_vm_grid_now(VM,I),
  i_to_o_is_none_some_some_in(VM).

i_to_o_is_none_some_some_in(VM):-
 set_current_pair(VM.start_grid,VM.target_grid),
 ignore(keypads(VM)),



 length(VM.lo_points,LenMass),
 if_t(LenMass>0,
  must_det_ll((
  OObjs = VM.objs,
  set_example_num_vm(VM),
  gset(VM.objs)=[],
  i_intersect_o(Intersect),
  individuate([keypads],Intersect,AddObjs),
  my_maplist(mergeObject(VM),AddObjs),
  o_minus_i(Zero), \+ no_fg_mass(Zero),
  grid_to_points(Zero,Points),
  gset(VM.grid)=Zero,
  % individuate(i_colormass,Zero,Library),
  gset(VM.lo_points)=Points,
  one_fti(VM,colormass),
  Library1 = VM.objs,
  gset(VM.objs)=[],
  filter_library(Library1,Library),
  print_grid(library2(i_to_o_is_none_some_some_in),Library),
  nop(Library=[_|_]->my_maplist(add_shape_lib(pairs),Library);true),
  OGrid = VM.start_grid,
  OPoints = VM.start_points,
  gset(VM.grid)=OGrid,
  gset(VM.lo_points)=OPoints,
  print_ss(find_hybrid_shapes2,Library,OGrid),
  maybe_find_hybrid_shapes_on(Library,OGrid,RGroup),
  pp(rgroup2=RGroup),
  gset(VM.objs)=OObjs,
  my_maplist(mergeObject(VM),RGroup),
  %addOGSObjects(VM,RGroup),
  globalpoints(RGroup,UsedPoints),
  intersection(OPoints,UsedPoints,_,LeftOver,_),
  gset(VM.lo_points) = LeftOver,
  %pp(objs = VM.objs),
  %print_grid(objs, VM.objs),
  %LeftOver = VM.lo_points,
  points_to_grid(VM.h,VM.v,LeftOver,Grid),
  gset(VM.grid)=Grid,
  print_ss(afterHybrid2,Grid,VM.objs),
  run_fti(VM,generic_nsew_colormass)))),!.

set_example_num_vm(VM):-
 must_det_ll((
  ID = VM.id,
  get_example_num(ExampleNum),
  set_example_num(ID),
  get_example_num(ExampleNum2),
  ignore((o_minus_i(NewGrid))),
  %dmsg(ExampleNum->ID->ExampleNum2),
  print_ss(wqs(o_minus_i(ExampleNum->ID->ExampleNum2)),NewGrid,VM.objs))),!.

% =====================================================================
is_fti_step(mono_i_to_o_is_none_some_none).
% =====================================================================

mono_i_to_o_is_none_some_none(VM):-  set_example_num_vm(VM),
  \+ is_output_vm(VM),!,mono_i_to_o_is_none_some_none_in(VM),!.
mono_i_to_o_is_none_some_none(VM):-
  set_example_num_vm(VM),
  is_output_vm(VM),!,
  must_det_ll((
  show_minuses,
  gset(VM.objs)=[],
  gset(VM.grid)=I,
  current_pair_io(I,_O),
  o_minus_i(NewGrid), 
  set_vm_grid_now(VM,NewGrid),
  run_fti(VM,generic_nsew_colormass),
  set_vm_grid_now(VM,I),
  gset(VM.grid)=I,
  gset(VM.start_grid)=I,
  mono_i_to_o_is_none_some_none_in(VM))).

filter_library(Library1,Library):- 
  must_det_ll((remove_background_only_object(Library1,Library2),
  include(fg_mass_geq(3),Library2,Library))),
%  include(is_fg_object,Library3,Library).
  !.

mono_i_to_o_is_none_some_none_in(VM):-
 set_example_num_vm(VM),
 ignore(keypads(VM)),
 length(VM.lo_points,LenMass),
 OObjs = VM.objs,
 if_t(LenMass>0,
  must_det_ll((
  o_minus_i(Zero), \+ no_fg_mass(Zero),
  grid_to_points(Zero,Points),
  gset(VM.grid)=Zero,% individuate(i_colormass,Zero,Library),
  gset(VM.lo_points)=Points,
  gset(VM.objs)=[],
  one_fti(VM,colormass),
  Library1 = VM.objs,
  gset(VM.objs)=[],
  filter_library(Library1,Library),
  %pp(library=Library),
  print_grid(library(mono_i_to_o_is_none_some_none_in),Library),
  nop((Library=[_|_]->my_maplist(add_shape_lib(pairs),Library);true)),
  OGrid = VM.start_grid,
  OPoints = VM.start_points,
  gset(VM.grid)=OGrid,
  gset(VM.lo_points)=OPoints,
  print_ss(find_hybrid_shapes,Library,OGrid),
  maybe_find_hybrid_shapes_on(Library,OGrid,RGroup),
  %i_colormass,
  %trace,
  pp(rgroup=RGroup),
  gset(VM.objs)=OObjs,
  %trace,
  my_maplist(mergeObject(VM),RGroup),
  %addOGSObjects(VM,RGroup),
  globalpoints(RGroup,UsedPoints),
  intersection(OPoints,UsedPoints,_,LeftOver,_),
  gset(VM.lo_points) = LeftOver,
  %print_grid(objs, VM.objs),
  %LeftOver = VM.lo_points,
  points_to_grid(VM.h,VM.v,LeftOver,Grid),
  gset(VM.grid)=Grid,
  print_ss(afterHybrid,Grid,VM.objs),
  run_fti(VM,generic_nsew_colormass)))),!.

mono_i_to_o_is_none_some_none_in(VM):- 
  run_fti(VM,generic_nsew_colormass).
  


individuation_macros(simple_grids,[ 
    keypads,
    bigger_grid_contains_other,
    maybe_glyphic,
    save_as_hybrid_shapes([indv_omem_points]),    
    find_hybrid_shapes]).

toplevel_individuation(subtractions):- \+ cant_use_intersections.
individuation_macros(subtractions, 
  [keypads,
   bigger_grid_contains_other,
   fg_intersections([generic_nsew_colormass]),
   fg_subtractions([i_subtract_objs]),
   i_subtract_objs]).

%individuation_macros(i_complete_generic,[i_complete_generic(subtractions)]):-  use_subtractions,!.
%individuation_macros(i_complete_generic,[i_complete_generic(generic_nsew_colormass)]):- mass_same_io,!,cant_use_intersections.
individuation_macros(i_complete_generic,[generic_nsew_colormass]).

toplevel_individuation(i_colormass):- cant_use_intersections.
%individuation_macros(i_colormass,[subshape_both(v,colormass), maybe_lo_dots]).
%individuation_macros(i_colormass,[colormass,alone_dots(lte(25))]).
individuation_macros(i_colormass,[colormass]).


toplevel_individuation(generic_nsew_colormass):- cant_use_intersections.
individuation_macros(generic_nsew_colormass, 
[ keypads,
  %bigger_grid_contains_other,
  %maybe_glyphic,
  nsew,
  black_to_zero,
  nsew,nsew_2,colormass_3,
  zero_to_black,
  colormass,
  black_to_zero,alone_dots(lte(25)),zero_to_black,
  print_vm_info(post_generic_nsew_colormass),
  lo_dots,
  print_vm_info(post_lo_dots)]).


mass_same_io:- once(arc_common_property(mass(_));((current_pair_io(I,O),is_grid(O),  mass(I,IM),mass(O,OM), IM=OM))).
use_subtractions:-  \+ cant_use_intersections.
  %once(arc_common_property(containsAll(o-i));arc_common_property(containsAll(o-i))).


find_indivizers(F,R):-clause(individuation_macros(F,R),Body),catch(Body,_,fail),R\==complete,nonvar(F).
%find_indivizers(F,is_fti_stepr):- is_fti_stepr(F).
find_indivizers(F,is_fti_step):- is_fti_step(F), atom(F), current_predicate(F/1).
find_indivizers(F,is_item):-clause(individuation_macros(F,R),Body),catch(Body,_,fail),member(F,R).
list_of_indivizers(S):- findall(F+R,find_indivizers(F,R),LS1),predsort(sort_on(arg(2)),LS1,S).
show_indivizers:- update_changes, list_of_indivizers(L),
 forall(nth_above(800,N,L,E+_),
  (find_indivizers(E,R),format('~N'),sformat(S,'~t~d~t  ~w:  ~t~w',[N,E,R]),print_menu_cmd1(write(S),E),nl)).
do_indivizer_number(N):- 
 list_of_indivizers(L),nth_above(800,N,L,E+_), set_indivs_mode(E),
 with_indivs_mode(E,ndividuator).

% vm_grid_call(subst_color(black,brown)),

individuation_macros(nsew_bg,[
  with_mapgrid([fgc_as_color(plain_var),bgc_as_color(zero),plain_var_as(black)],'_nsew_bg',[nsew,alone_dots,lo_dots])]).

fill_in_bg(_Black,G2,GG2):- is_fg_color(G2),!,ignore(G2=GG2).
fill_in_bg(Black,G2,GG2):- \+ G2\=Black,!,ignore(GG2=Black).
fill_in_bg(Alt,In,Out):- only_color_data_or(Alt,In,Out),!.
fill_in_bg(_Alt,In,In):-!.
into_solid_grid(I,GG1):- into_grid(I,G1),mapgrid(fill_in_bg(black),G1,GG1),!.





completely_represents(I,O):-
   into_solid_grid(I,I1),into_solid_grid(O,O1),
   (I1=@=O1-> true ;  (print_ss(not_completely_represents,I1,O1),fail)).

current_how(HOW,Stuff1,Stuff2):-
  current_pair_io(I,O), guess_how(HOW,I,O,Stuff1,Stuff2).

current_how_else(HOW,Stuff1,Stuff2):-
  current_how(HOW,I,O), guess_how_else(HOW,I,O,Stuff1,Stuff2).


guess_how_else([HOW|HOW_ELSE],I,O,Stuff1,Stuff2):-
  guess_how(HOW,I,O,MID1,MID2),
  maplist(guess_how_else,HOW_ELSE,MID1,MID2,Stuff1,Stuff2),!.
guess_how_else([],I,O,I,O).

guess_how(HOW,I1,O1,Stuff1,Stuff2):-
  must_det_ll((into_solid_grid(I1,I2),into_solid_grid(O1,O2))),!,
  indiv_how1(HOW),
  duplicate_term(I2+O2,I+O),
  must_det_ll((
    
    once((individuate(HOW,I,Stuff1))),
    once((individuate(HOW,O,Stuff2))))),

    length(Stuff1,L),L>0,length(Stuff2,L),
    must_det_ll((
    print_ss(orig,I,O),
    ignore(completely_represents(I,Stuff1)),
    ignore(completely_represents(O,Stuff2)),
    print_ss(objs(HOW,L),Stuff1,Stuff2),
    save_how_io(HOW,HOW))).




individuation_macros(completely_do(This),[This,do_ending]).

is_grid_io(Grid,RealIO):- get_current_test(TestID),kaggle_arc_io(TestID,_,RealIO,G),Grid=@=G,!.
indiv_how(IO,How):- is_grid(IO),is_grid_io(IO,RealIO),!,indiv_how(RealIO,How).
indiv_how(IO,How):- arc_test_property(common,indiv_how(IO),How),!.
indiv_how(_,How):- indiv_how1(How).

indiv_how1(completely_do(i_colormass)).
indiv_how1(completely_do(i_complete)).
%indiv_how1(completely_do(i_mono_colormass)).
%indiv_how(completely_do(maybe_lo_dots)).
indiv_how1( indv_opt(Flags)):- fail,
      toggle_values([bground,diags,mono,shape,boxes,parts],Flags).
toggle_values([N|Names],[V|Vars]):- toggle_values(Names,Vars), toggle_val(N,V).
toggle_values([],[]).
toggle_val(Lst,V):- is_list(Lst),!,member(V,Lst).
toggle_val(N,V):- member(TF,[false,true]), V =..[N,TF].

:- dynamic(arc_cache:indv_flag/2).
is_fti_step(set_indv_flags).
set_indv_flags(X,_VM):- set_indv_flags(X),!.
set_indv_flags(X):- is_list(X),!,maplist(set_indv_flags,X).
set_indv_flags(NV):- NV=..[N,V],retractall(arc_cache:indv_flag(N,_)),assert(arc_cache:indv_flag(N,V)). 

is_fti_step(which_tf).
which_tf(Name,True,False,VM):- (arc_cache:indv_flag(Name,true)->run_fti(VM,True);run_fti(VM,False)).

is_fti_step(nop).

individuation_macros(indv_opt(Flags), 
 [set_indv_flags(Flags),which_tf(mono,fg_shapes(TODO),TODO)]):- 
 TODO = [ %bg_shapes(nsew),
           which_tf(shape,do_im_complete,nop),
           which_tf(diags,colormass,nsew),
           call(print(Flags)),
           which_tf(boxes,pbox_vm_special_sizes,nop),
           which_tf(bground,remove_if_prop(cc(fg,0)),nop)].


individuation_macros(i_complete_generic333, 
  [save_as_obj_group([indv_omem_points]),
   save_as_obj_group([consider_other_grid]),
   save_as_obj_group([colormass]),
   save_as_obj_group([i_mono_colormass]),    
   save_as_obj_group([mono_shapes(nsew)]),
   save_as_obj_group([by_color]),
   fg_intersections([i_intersect]),
   fg_subtractions([i_subtract_objs]),
   
    each_ogs_object([i_ogs_subobj]),
   %remove_used_points,
   %save_as_obj_group([pbox_vm_special_sizes([special_sizes_v_h_sorted_s_l])]),
   %pbox_vm_special_sizes([special_sizes_v_h_sorted_l_s]),
   %print_vm_info(post_in_intersection),
   %i_subtract_objs,
   %fg_subtractions([i_subtract_objs]),
   i_subtract_objs,
   gather_cached,
   %remove_used_points,
   %i_complete_generic1,
   print_vm_info(post_i_complete_generic)]).

individuation_macros(i_complete_generic2, 
  [%fg_intersections2([i_intersect]),
   fg_intersections([i_intersect]),
   gather_cached,
   %remove_used_points,
   %save_as_obj_group([pbox_vm_special_sizes([special_sizes_v_h_sorted_s_l])]),
   %pbox_vm_special_sizes([special_sizes_v_h_sorted_l_s]),
   %print_vm_info(post_in_intersection),
   %fg_subtractions([i_subtract_objs]),
   remove_used_points,
   fg_subtractions(i_subtract_objs),
   print_vm_info(post_i_complete_generic)]).




individuation_macros(i_complete_generic1, [

    call(retractall(special_sizes(_,_))),
    ([fg_subtractions(colormass),fg_intersections(colormass),colormass]),

    save_as_obj_group([fg_subtractions(nsew),fg_intersections(nsew),nsew]),


    save_as_obj_group([consider_other_grid]),
    save_as_obj_group([i_mono_colormass]),    
    save_as_obj_group([by_color]),


    %reset_objs, reset_points,
    %reset_points,
    find_hybrid_shapes,
                     pbox_vm_special_sizes([special_sizes_v_h_sorted_l_s]),
                     reset_points,
                     pbox_vm_special_sizes([special_sizes_v_h_sorted_s_l]),
    remove_used_points,
    interlink_overlapping_black_lines]).

individuation_macros(i_complete_generic3, [
                     consider_other_grid, reset_points,
                     indv_omem_points, reset_points,
                     i_mono_colormass, reset_points,
                     reset_objs,
    pbox_vm_special_sizes,
 %  ([fg_subtractions(nsew),fg_intersections(nsew),nsew]),reset_points,
 %  ([fg_subtractions(colormass),fg_intersections(colormass),colormass]),reset_points,
   ([i_mono_colormass]), reset_points,   
   ([i_mono_nsew]), reset_points,
   gather_cached,reset_points,
   alone_dots(lte(5)),
   print_vm_info(pre_boxes),
   pbox_vm_special_sizes,
   print_vm_info(post_boxes),
   keep_if_prop(or(iz(type()),iz(type(sa_dots)))),                     
   remove_used_points,
   pbox_vm_special_sizes,
   remove_used_points,
   fg_subtractions(nsew),nsew,
   fg_subtractions(colormass),colormass,
   fg_intersections(nsew),nsew,
   fg_intersections(colormass),colormass,
  %remove_omem_trumped_by_boxes,  
  interlink_overlapping_black_lines,
  reset_points,remove_used_points,
%  print_vm_info,
  whole]).

special_sizes_v_h(H,V):-  special_sizes(H,V).
special_sizes_v_h(H,V):-  special_sizes(V,H).
special_sizes_v_h(H,V):-  special_sizes(HH,VV),H is HH-1,V is VV-1.
special_sizes_v_h(H,V):-  special_sizes(HH,VV),H is HH+1,V is VV+1.

special_sizes_vm(_VM,H,V):- special_sizes_v_h(H,V).
special_sizes_vm(VM,H,V):- h_v_from(VM.h,VM.v,H,V).
special_sizes_vm(VM,H,V):- this_grid_is_multiple_of_other(VM), h_v_from(VM.ogx,VM.ogy,H,V).

h_v_from(HH,VV,HH,VV).
h_v_from(HH,VV,HH,HH):- 0 is VV rem HH.
h_v_from(HH,VV,VV,VV):- 0 is HH rem VV.
h_v_from(HH,VV,FH,FV):- FH is floor(HH/2),FV is floor(VV/2).
h_v_from(HH,VV,NH,NV):- between(2,5,IH), between(2,5,IV), is_h_v_from(HH,VV, IH, IV, NH, NV).
h_v_from(HH,VV,IH,IV):- between(3,7,IH), between(3,7,IV), is_h_v_from(HH,VV, IH, IV, _NH, _NV).

is_h_v_from(HH,VV,IH,IV,NH,NV):- 0 is HH rem IH, 0 is VV rem IV,  NV is VV rem IV,  NH is HH rem IH.

special_sizes_v_h_sorted_l_s(H,V):- peek_vm(VM),special_sizes_v_h_sorted_l_s(VM,H,V).

gather_special_sizes_v_h_sorted_s_to_l(VM,SizesR):- findall(size2D(H,V),special_sizes_vm(VM,H,V),Sizes1), 
  predsort(sort_on(neg_h_v_area),Sizes1,SizesR).


special_sizes_v_h_sorted_l_s(VM,H,V):- 
  gather_special_sizes_v_h_sorted_s_to_l(VM,SizesR),reverse(SizesR,Sizes),member(size2D(H,V),Sizes),H>0,V>0.

special_sizes_v_h_sorted_s_l(VM,H,V):- 
  gather_special_sizes_v_h_sorted_s_to_l(VM,SizesR),member(size2D(H,V),SizesR),H>0,V>0.



  


individuation_macros(i_complete_generic2, [
   named_grid_props(pre_objs),
   whole,
   maybe_glyphic,
   identify_subgrids,
   pixelate_swatches,
   maybe_repair_in_vm(find_symmetry_code),
  
                     %%find_subsumes,
                     %%find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
                     %%find_overlaps,
                     %%find_touches,
                     %%find_sees,
  %remove_if_prop(and(link(contained_by,_),cc(fg,0))),
  %remove_if_prop(and(giz(g(out)),cc(fg,0))),
  %remove_dead_links,
  %combine_same_globalpoints,
  %really_group_vm_priors,
  %combine_objects,
   find_relations,
  end_of_macro  ]). 

/*
show_individuated_pair(PairName,ROptions,GridIn,GridOfIn,InC,OutC):- 
  InC=@=OutC,!,
  must_det_ll((into_iog(InC,OutC,IndvS),
  show_individuated_nonpair(PairName,ROptions,GridIn,GridOfIn,IndvS))).
*/




% =========================================================
% TESTING FOR INDIVIDUATIONS
% =========================================================
 %i:- fav_i(X),i(X).   %i(GridIn):- i2([complete],GridIn).
igo:- fav_i(X),igo(X),!. igo(GridIn):- i2(complete,GridIn).
iq:- fav_i(X),igo(X).    iq(GridIn):-  iq(complete,GridIn).
iL:- fav_i(X),iL(X).     iL(GridIn):-  i2([shape_lib(as_is),complete],GridIn).


:- arc_history1(igo).

fav_i(X):- clsmake, luser_getval(task,X),X\==[].
fav_i(t('00d62c1b')).
fav_i(X):- fav(X).
fav_i(_).

check_for_refreshness:- 
  if_t(nb_current(menu_key,'i'),
    (forall(muarc:clear_arc_caches,true),
     retractall(arc_cache:individuated_cache(_,_,_,_,_)),
     %retractall(arc_cache:is_group(_,_,_,_,_)),
     clear_arc_learning,
     abolish(arc_cache:object_to_object/6),dynamic(arc_cache:object_to_object/6))),
  if_t(nb_current(menu_key,'o'),(clear_arc_learning,abolish(arc_cache:object_to_object/6),dynamic(arc_cache:object_to_object/6))),!.


i2(ROptions,GridSpec):- clsmake,
  clear_shape_lib(as_is),
  into_grids(GridSpec,GridIn),
  once((into_grid(GridIn,Grid),igo(ROptions,Grid))).

/*
*/

%worker_output(G):- \+ menu_or_upper('B'),!, time(wots(_,arc_weto(G))).
worker_output(G):- call(G).

igo(ROptions,Grid):-
  check_for_refreshness,
  w_section(do_ig(ROptions,Grid,IndvS)),
  into_grid(Grid,GridIn),
  w_section(show_individuated_nonpair(igo,ROptions,Grid,GridIn,IndvS)).

maybe_name_the_pair(In,Out,PairName):-
  kaggle_arc(TestID,ExampleNum,In,Out),
    name_the_pair(TestID,ExampleNum,In,Out,PairName),!.
maybe_name_the_pair(_In,_Out,PairName):-current_test_example(TestID,ExampleNum),
  ignore((PairName = (TestID>ExampleNum))),!.


do_ig(ROptions,Grid,IndvS):-
 must_det_ll((
  into_grid(Grid, GridIn), 
  dash_chars('='),
  print_grid(GridIn), 
  %indiv_grid_pings(GridIn),
  grid_to_tid(GridIn,ID),
  dash_chars('*'),
  format("~N~n% ?- ~q.~n~n",[igo(ROptions,ID)]),
  testid_name_num_io(ID,TestID,Example,Num,IO),
  ig_test_id_num_io(ROptions,GridIn,ID,TestID,Example,Num,IO,IndvS))).

ig_test_id_num_io(ROptions,GridIn,_ID,TestID,trn,Num,in,IndvS):- 
 must_det_ll((
  In = GridIn,
  kaggle_arc_io(TestID,(trn+Num),out,Out),!,
  my_time((maybe_name_the_pair(In,Out,_PairName),
  individuate_pair(ROptions,In,Out,IndvSI,IndvSO),
  into_iog(IndvSI,IndvSO,IndvS))))).

ig_test_id_num_io(ROptions,GridIn,_ID,TestID,_Example,_Num,_IO,IndvS):- 
  set_current_test(TestID),
  worker_output((individuate_nonpair(ROptions,GridIn,IndvS))),!.
  %my_maplist(add_shape_lib(as_is),IndvS),  

into_iog(InC,OutC,IndvS):- append(InC,OutC,IndvC),!, must_det_ll(list_to_set(IndvC,IndvS)).

into_gio(IndvS,InSO,OutSO):- 
  include(has_prop(giz(g(out))),IndvS,OutC),
  include(has_prop(giz(g(in))),IndvS,InC),
  include(not_io,IndvS,IOC),
  append_sets([IOC,OutC],OutS),
  append_sets([IOC,InC],InS),
  must_det_ll((OutSO=OutS,InSO=InS)).

not_io(O):- \+ has_prop(giz(g(out)),O), \+ has_prop(giz(g(in)),O).

% comp(cbg(black),i-o,size2D)=size2D(num(vals([_846946,_846952]),+ -3,ratio(2)),num(vals([_846978,_846984]),+ -3,ratio(2)))


xfer_1zero(In,Out):-
 ignore((
 color_texture_point_data(In,C1,T1,_),
 color_texture_point_data(Out,C2,T2,_), 
 C1==C2, merge_texture(T1,T2,TO), 
 nb_setarg(1,In,TO),nb_setarg(1,Out,TO))).

xfer_1zero_ss(In,Out):-
 ignore((
 color_texture_point_data(In,C1,T1,_),
 color_texture_point_data(Out,C2,T2,_), 
 C1==C2,
 merge_texture(T1,T2,TO), 
 nb_setarg(1,In,TO),nb_setarg(1,Out,TO))).

merge_texture(X,Y,O):- merge_texture1(X,Y,O),Y\=='~',!.
merge_texture(X,Y,O):- merge_texture1(Y,X,O),X\=='~',!.
%merge_texture(X,_,X).
merge_texture1(0,_,0).
merge_texture1('<',_,'<').
merge_texture1('>',_,'>').
merge_texture1('V',_,'V').
merge_texture1('A',_,'A').
%merge_texture1('-',_,'-').

progress(P):- nop(pp(progress(P))).
progress(C,P):- nop(pp(C,progress(P))).

xfer_zeros(_,_):-!.
xfer_zeros(In,Out):- 
  is_grid(In),is_grid(Out),grid_size(In,H,V),  grid_size(Out,H,V),!,
  forall(between(1,H,Hi),forall(between(1,V,Vi),
     (hv_c_value(In,ZV1,Hi,Vi),hv_c_value(Out,ZV2,Hi,Vi),xfer_1zero_ss(ZV1,ZV2)))).

xfer_zeros(In,Out):- 
 ignore((
 is_grid(In),is_grid(Out),
 ignore((
  grid_size(In,H1,V1),  grid_size(Out,H2,V2),
  max_min(H1,H2,_,H), max_min(V1,V2,_,V),
  forall(between(1,H,Hi),forall(between(1,V,Vi),
     (hv_c_value(In,ZV1,Hi,Vi),hv_c_value(Out,ZV2,Hi,Vi),xfer_1zero(ZV1,ZV2)))))))),!.





show_individuated_nonpair(PairName,ROptions,GridIn,Grid,InC):-
 must_det_ll((
  print_list_of(show_indiv,PairName,InC),!,
  length(InC,Len),
  pp(show_individuated_nonpair=Len),
  if_t(GridIn\==Grid,
    print_ss(cyan,GridIn,into_grid_changed(PairName),_,Grid,into_grid_changed(result))),
  print_ss(green,GridIn,show_individuated_nonpair(PairName),_,InC,indvs(ROptions,PairName)))).
  
 
flatten_set(F,S):- flatten(F,L),list_to_set(L,BF),!,BF=S.


 

do_pair_filtering(ID1,GridIn,InC,InShownO,ID2,GridOut,OutC,OutShownO):- 
  grid_size(GridIn,IH,IV),filter_shown(IH,IV,InC,InShown,InHidden), filter_shown(IH,IV,InHidden,InHiddenLayer1,InHiddenLayer2),
  grid_size(GridOut,OH,OV),filter_shown(OH,OV,OutC,OutShown,OutHidden), filter_shown(OH,OV,OutHidden,OutHiddenLayer1,OutHiddenLayer2),
  print_ss(green,InHiddenLayer2,hiddens2(ID1),_,OutHiddenLayer2,hiddens2(ID2)),
  print_ss(green,InHiddenLayer1,hiddens1(ID1),_,OutHiddenLayer1,hiddens1(ID2)),
  print_ss(green,InShown,shown(ID1),_,OutShown,shown(ID2)),!,
  OutShownO = OutC,InShownO = InC.
do_pair_filtering(_ID1,_GridIn,InC,InC,_ID2,_GridOut,OutC,OutC).


really_show_touches(Title,InShown,Obj):- 
  show_indiv(really_show_touches(Title),Obj),
  dbg_show_touches(InShown,Obj).
   

% =========================================================

iq(ROptions,Grid):-igo(ROptions,Grid).

% =========================================================

:- use_module(library(multivar)).

maybe_multivar(C):- nonvar(C),!.
%maybe_multivar(C):- multivar(C).
maybe_multivar(_).

:- dynamic(reuse_grid_nums/1).

%:- discontiguous(fsi/14).
:- discontiguous(fti/2).
:- discontiguous(one_fti/2).
   
:- dynamic(is_unshared_saved/2).
:- dynamic(is_shared_saved/2).

the_big_three_oh(90).


% ?- print_grid(gridFn(X)).
preserve_vm(VM,Goal):- 
  get_map_pairs(VM,_,Pairs),
  set_vm(VM),
  setup_call_cleanup(
     duplicate_term(Pairs,DPairs),
      Goal,
    my_maplist(arc_setval(VM),DPairs)),
  set_vm(VM).


remove_texture(Cell,C-Point):- color_texture_point_data(Cell,C,_Texure,Point).
is_texture(List,Cell):- color_texture_point_data(Cell,_C,Texture,_Point),member(T,List),Texture==T,!.
is_fti_step(gather_texture).

gather_texture(VM):- nonvar(VM.ngrid),!.
gather_texture(VM):- Grid = VM.grid, into_ngrid(Grid,NGrid), gset(VM.ngrid)=NGrid,!.
gather_texture(VM):-
 must_det_ll((
    Grid = VM.grid,
    as_ngrid(Grid,GMap),!,
    grid_size(Grid,H,V),
    grid_to_points(GMap,H,V,TPoints),
    my_partition(is_fgp,TPoints,FGPoints,BGPoints),
    
    my_partition(is_texture([0]),FGPoints,Zeros,Rest),
    my_partition(is_texture([/*'V','<','^','v','>'*/]),Rest,Shooters,NFGPoints),
    append(BGPoints,NFGPoints,NPoints),
    my_maplist(remove_texture,NPoints,LOPoints),!,
    points_to_grid(H,V,LOPoints,NewGrid0),!,
    mapgrid(assign_plain_var_with(wbg),NewGrid0,NewGrid),
    set(VM.start_points)=LOPoints,
    set(VM.grid)=NewGrid,
    as_ngrid(NewGrid,NewGMap),!,
    print_ss(green,GMap,nGrid(1),_,NewGMap,nGrid(2)),
    my_maplist(make_textured_point_object(VM,[birth(texture)]),Zeros,_),
    my_maplist(make_textured_point_object(VM,[birth(texture)]),Shooters,_))),!.


make_textured_point_object(VM,Overrides,Cell,Indv):-
   color_texture_point_data(Cell,C,Texture,Point),
   make_point_object(VM,[iz(stype(dot)),iz(media(shaped)),texture(Texture)|Overrides],(C-Point),Indv).


/*
find_grids(VM):-
 
  Grid=VM.grid,
  two_rows(Grid,S1,R1,R2).

two_rows(Grid,S1,R1,R2):-
  nth1(R1,Grid,[S1|Row1]),nonvar(S1),
  nth1(R2,Grid,Row2),
  R2>R1+1,
  [S1|Row1]==Row2,
  my_maplist(==(S1),Row1).
*/  

% =====================================================================
is_fti_step(print_vm_info).
% =====================================================================
print_vm_info(_Why,_VM):-!.
print_vm_info(Why,VM):-
  \+ \+ print_grid(VM.h,VM.v,ii(print_vm_info(Why,points)),VM.lo_points),  
  \+ \+ writeg(i(print_vm_info(Why,grid))=VM.grid),
  visible_order(VM.objs,Objs),
  \+ \+ print_grid(VM.h,VM.v,ii(print_vm_info(Why,objs)),Objs),
  banner_lines(red).
 
% =====================================================================
is_fti_step(remove_illegals).
% =====================================================================
remove_illegals(VM):-
 my_partition(illegal_object(VM),VM.objs,_Bad,Good),
 gset(VM.objs)=Good.

illegal_object(VM,Obj):-
  loc2D(Obj,X,Y),
  once(X>VM.h;Y>VM.v),
  nop(show_indiv('illegal_object',Obj)).
 

% =====================================================================
is_fti_step(remove_omem_trumped_by_boxes).
% =====================================================================
remove_omem_trumped_by_boxes(VM):-
  Objs = VM.objs,
  remove_omem_trumped_by_boxes(VM,Objs,New),
  gset(VM.objs) = New.
remove_omem_trumped_by_boxes(VM,I,O):-
  member(O1,I),
  has_prop(iz(type()),O1),
  select(O2,I,II),
  has_prop(iz(flag(omem)),O2),
  \+ has_prop(iz(type()),O2),
  O1\==O2, 
  globalpoints(O1,Ps1),globalpoints(O2,Ps2),
  Ps1\==Ps2,
  area(O1,A1),area(O2,A2), (A1/2>A2),
  area_contained(O1,O2),!,
  obj_to_oid(O1,GOID),
  erase_obj(GOID),
  remove_omem_trumped_by_boxes(VM,II,O).
remove_omem_trumped_by_boxes(VM,I,O):-
  member(O1,I),
  has_prop(iz(type()),O1),
  select(O2,I,II),
  has_prop(iz(type()),O2),
  O1\==O2, 
  globalpoints(O1,Ps1),globalpoints(O2,Ps2),
  Ps1\==Ps2,
  area(O1,A1),area(O2,A2), ((A1/4)>A2),
  findall(C-P1,(member(C-P1,Ps1),member(C-P1,Ps2)),Overlaps),
  length(Overlaps,Len),Len>=2,
  area_contained(O1,O2),
  obj_to_oid(O2,GOID),
  erase_obj(GOID),
  remove_omem_trumped_by_boxes(VM,II,O).
remove_omem_trumped_by_boxes(_,O,O).

area_contained(Outer,Inner):-
  loc2D(Outer,LowHOuter,LowVOuter),loc2D(Inner,LowHInner,LowVInner),   
  LowHInner >= LowHOuter, LowVInner >= LowVOuter,
  vis2D(Outer,HOuter,VOuter),vis2D(Inner,HInner,VInner), 
  HOuter>= HInner, VOuter>= VInner,
  HighHOuter is LowHOuter+HOuter, HighVOuter is LowVOuter+VOuter,
  HighHInner is LowHInner+HInner, HighVInner is LowVInner+VInner,
  HighHOuter >= HighHInner,
  HighVOuter >= HighVInner.



% =====================================================================
is_fti_step(identify_subgrids).
% =====================================================================
identify_subgrids(VM):-
  Objs = VM.objs,
  identify_subgrids(VM,Objs,New),
  gset(VM.objs) = New.
identify_subgrids(_,IO,IO).

% =====================================================================
is_fti_step(pixelate_swatches).
% =====================================================================
pixelate_swatches(VM):-
  Objs = VM.objs,
  pixelate_swatches(VM,Objs,New),
  gset(VM.objs) = New.


fill_in_missing(_C,LPoints,_X,Y,_VX,_VY,LPoints):- Y<1,!.
fill_in_missing(C,LPoints0,X,Y,VX,VY,LPoints):- X<1,!, Ym1 is Y-1, fill_in_missing(C,LPoints0,VX,Ym1,VX,VY,LPoints).
fill_in_missing(C,LPoints0,X,Y,VX,VY,LPoints):-
  hv_point(X,Y,P1), \+ member(_-P1,LPoints0),!,
  Xm1 is X-1, fill_in_missing(C,[C-P1|LPoints0],Xm1,Y,VX,VY,LPoints).
fill_in_missing(C,LPoints0,X,Y,VX,VY,LPoints):- Xm1 is X-1, 
  fill_in_missing(C,LPoints0,Xm1,Y,VX,VY,LPoints).

pixelate_swatches(VM,[Obj|Objs],New):-
   pixelate_swatches(VM,Objs,ObjsNew),!,
   pixelate_swatches(VM,Obj,ObjsNew,New).
pixelate_swatches(VM,[Obj|Objs],[Obj|New]):-!, pixelate_swatches(VM,Objs,New).
pixelate_swatches(_,[],[]).

pixelate_swatches(VM,Obj,ObjsNew,New):-
   has_prop(iz(type()),Obj),
   \+ has_prop(iz(type(subgrid(_,_,_,_))),Obj),
   has_prop(unique_colors_count(UCC),Obj),UCC>=3,
   area(Obj,Area), Area<30, Area>3,
   globalpoints(Obj,GPoints), % length(GPoints,GLen), GLen==Area,
   loc2D(Obj,OX,OY),
   deoffset_points(OX,OY,GPoints,LPoints0),
   vis2D(Obj,VX,VY),
   fill_in_missing(black,LPoints0,VX,VY,VX,VY,LPoints),
   offset_points(OX,OY,LPoints,AllGpoints),   
   add_pixel_swatches(VM,OX,OY,VX,VY,AllGpoints,NewGs),
   append(ObjsNew,NewGs,New).

pixelate_swatches(_VM,Obj,ObjsNew,[Obj|ObjsNew]).

add_pixel_swatches(_VM,_OX,_OY,_VX,_VY,[],[]).
add_pixel_swatches(VM,OX,OY,VX,VY,[C-P1|GPoints],[New|Gs]):-   
  add_pixel_swatches(VM,OX,OY,VX,VY,GPoints,Gs),
  hv_point(PX,PY,P1),
  X is PX-OX+1,Y is PY-OY+1,
  PosX = rational(X/VX),
  PosY = rational(Y/VY),

   make_point_object(VM,[birth(pixelate_swatches),pixel2D(X,Y),pixel2C(X,Y,C), 
     % iz(flag(hidden)),
     pixel2G(PosX,PosY),iz(media(shaped))],C-P1,New).
 
% =====================================================================
is_fti_step(interlink_overlapping_black_lines).
% =====================================================================
interlink_overlapping_black_lines(VM):- 
  Objs = VM.objs,
  interlink_overlapping_black_lines(VM,Objs,New),
  gset(VM.objs) = New.



interlink_overlapping_black_lines(VM,Objs,[Obj1|New]):-
  BlackL = [_],
  select(Obj1,Objs,Rest0),
  ( \+ has_prop(iz(type((border_frame(_,_,BlackL),_))),Obj1) ;
    \+ has_prop(unique_colors(BlackL),Obj1)),!,
  interlink_overlapping_black_lines(VM,Rest0,New).

interlink_overlapping_black_lines(VM,Objs,New):-
  BlackL = [_],
  select(Obj1,Objs,Rest0),
  has_prop(iz(type((border_frame(H,V,BlackL),PassNum))),Obj1),
  has_prop(unique_colors(BlackL),Obj1),
  globalpoints(Obj1,P1s),

  select(Obj2,Rest0,Rest),
  has_prop(iz(type((border_frame(H,V,BlackL),_))),Obj2),
  has_prop(unique_colors(BlackL),Obj2),
  globalpoints(Obj2,P2s),

  intersection(P1s,P2s,Overlap,_,_),
  Overlap=[_,_|_],!,
  append(P1s,P2s,P12s),!,

  sort_safe(P12s,Points),
  make_indiv_object(VM,[iz(info(combined)),iz(type((border_frame(H,V,BlackL),PassNum))),
    iz(type(frame_group)),birth(merge_frame_group)],Points,NewObj),
  assumeAdded(VM,NewObj),
  interlink_overlapping_black_lines(VM,[NewObj|Rest],New).
interlink_overlapping_black_lines(_,Objs,Objs).

% =====================================================================
is_fti_step(remove_dead_links).
% =====================================================================
remove_dead_links(VM):- 
  Objs = VM.objs,
  remove_dead_links(Objs,Objs,New),
  gset(VM.objs) = New.
remove_dead_links([],_Obj,[]):-!.
remove_dead_links([Obj|Objs],LiveObjs,[obj(NewProps)|New]):-
  indv_props_list(Obj,Props),
  remove_dead_props(Props,LiveObjs,NewProps),
  remove_dead_links(Objs,LiveObjs,New).

remove_dead_props([Prop|Props],LiveObjs,OUT):-
  select(Prop,Props,RestProps),compound(Prop),
  functor(Prop,link,_), arg(_,Prop,Ref),atom(Ref),is_oid(Ref),
  (sub_var(oid(Ref),LiveObjs)->OUT=[Prop|NewProps];OUT=NewProps),
  remove_dead_props(RestProps,LiveObjs,NewProps).
remove_dead_props(Props,_LiveObjs,Props).


%most_d_colors

/*
% =====================================================================
is_fti_step(sub_individuate).
% =====================================================================
sub_individuate(From,SubProgram,VM):-
  OldObjs = VM.objs,
  ignore(fti(VM,From)),
  NewObjs = VM.objs,
  intersection(NewObjs,OldObjs,_,OnlyNew,_),
  if_t(OnlyNew\==[],
   (globalpoints_include_bg(OnlyNew,Points),
    H = VM.h, V = VM.v,
    points_to_grid(H,V,Points,Grid),
    gensym('ID_',ID2),
    print_grid(SubProgram,Grid),
    preserve_vm(VM,individuate7(_,ID2,SubProgram,Grid,WasInside)),
    assumeAdded(VM,WasInside))),!.
*/
pointless(Res,VM):-
  set(VM.lo_points) = [],
  run_fti(VM,Res).

is_fti_step(sub_indiv).
sub_indiv(SubProgram,VM):-
  OnlyNew = VM.objs,  
  my_maplist(individuate_object(VM,VM.gid,SubProgram),OnlyNew,WasInside),
  my_maplist(addObjects(VM),WasInside).

individuate_object(VM,GID,SubProgram,OnlyNew,WasInside):-
 must_det_ll((
   object_grid(OnlyNew,OGrid),
   get_black(Black),mapgrid(assign_plain_var_with(Black),OGrid,Grid),
   object_glyph(OnlyNew,Glyph),
   loc2D(OnlyNew,X,Y),
   atomic_list_concat([GID,'_',Glyph,'_sub'],NewGID),
   assert_grid_gid(Grid,NewGID),
   set_vm(VM),!,
   with_global_offset(X,Y,
    individuate7(_NewVM,NewGID,SubProgram,Grid,WasInside)),!,
   set_vm(VM))),
   addObjects(VM,WasInside).

% =====================================================================
is_fti_step(consider_other_grid).
% =====================================================================
consider_other_grid(VM):- 
 ignore((
  StartGrid = VM.start_grid,
  other_grid_size(StartGrid,OGX,OGY),
  gset(VM.ogx)=OGX,
  gset(VM.ogy)=OGY,
  % let  take over objectification
  if_t(this_grid_is_multiple_of_other(VM),
    gset(VM.objs)=[]),

  other_grid(StartGrid,Other),
  is_grid(Other),
  Grid = VM.grid,
  Other = In,
  forall( maybe_ogs(ROHOV,Other,Grid),
  %u_dmsg(maybe_ogs(R,OH,OV,In,Grid)),       
     (member(loc2D(OH,OV),ROHOV),
      globalpoints_include_bg(In,LPoints),
      include(p1_arg(1,is_real_color),LPoints,ROPoints),
      offset_points(OH,OV,ROPoints,GOPoints),
      intersection(VM.lo_points,GOPoints,UsedByObj,LeftOver,Missing),
      Missing\==[],
      UsedByObj\==[],
      count_adjacent_same_colored_points(LeftOver,GOPoints,HVCount,DiagCount),
      DiagCount=_, HVCount<4,
      !,
      must_det_ll((
      %indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
      make_indiv_object(VM,[iz(stype(R))],GOPoints,Obj),
      
      %offset_grid(OH,OV,In,OffsetGrid),!, is_grid(OffsetGrid),
      %OffsetGrid = In,
      add_grid_label(Grid,Info,A), add_grid_label([Obj],Info,B),
      as_debug(9,((dash_chars,Info=maybe_ogs(R,OH,OV), print_ss(A,B)))), % atrace,
      %print_ss([Obj|Grid]-wqs(maybe_ogs(R,OH,OV))), %  atrace,  
      %print_grid(maybe_ogs(R,OH,OV),[Obj|Grid]), %  atrace,  
      remLOPoints(VM,GOPoints),
      remLOPoints(VM,UsedByObj))))))).


% =====================================================================
is_fti_step(indv_omem_points).
% =====================================================================
indv_omem_points(_):- \+ cant_use_intersections,!.
indv_omem_points(VM):- 
 luser_setval(generate_gids,true),

 ignore((
 VMID = VM.id, 
 \+ (sub_var(tst,VMID), sub_var(out,VMID)),
 mass_same_io,

 must_det_ll((

  Grid = VM.start_grid,
  with_luser(generate_gids,true,
   ((
    grid_to_gid(Grid,GID),    
    ensure_gid(Grid,GID),
    erase_objects(GID),
    retractall(cmem(GID,_,_)),
    ensure_cmem(GID),
    remake_texture(GID),
    show_grid_texture(GID),
    erase_objects(GID),
    cache_grid_objs(GID),
    show_gid_objs(GID),
    findall(OID,omem(GID,_,OID),OIDS),
    list_to_set(OIDS,SET),
    length(SET,Len),
    pp(omem(GID)=Len),
    forall(member(OID,SET),
       ((
         findall(C1-HV1,(omem(GID,HV1,OID),cmem(GID,HV1,C1)),Points),
         oid_to_texture_points(OID,TPoints),
         nop((TP = [tpoints(TPoints)])),
         TP=[],
         make_indiv_object(VM,[birth(omem),iz(flag(omem)),omem_oid(OID),oid(OID)|TP],Points,Obj),
         assumeAdded(VM,Obj),
         %print_grid(OID,TPoints), print_info(Obj),
         nop(show_indiv(Obj)))))))))))).
 
  

% =====================================================================
is_fti_step(remove_if_prop).
% =====================================================================
remove_if_prop(Prop,VM):- my_partition(has_prop(Prop),VM.objs,_With,gset(VM.objs)).

% =====================================================================
is_fti_step(keep_if_prop).
% =====================================================================
keep_if_prop(Prop,VM):- my_partition(has_prop(Prop),VM.objs,gset(VM.objs),_With).

% =====================================================================
is_fti_step(combine_if_prop).
% =====================================================================
combine_if_prop(Prop,VM):- include(has_prop(Prop),VM.objs,With),
  combine_vm_objects(combine_if_prop(Prop),With,VM).

combine_vm_objects(_Why,[],_).
combine_vm_objects(_Why,[_],_).
combine_vm_objects(Why,[I,O],VM):- !, combine_objects(Why,I,O,_New,VM).
combine_vm_objects(Why,[I,O|More],VM):- !, 
  combine_objects(Why,I,O,New,VM),
  combine_vm_objects(Why,[New|More],VM).
  
combine_objects(Why,I,O,New,VM):- 
 must_det_ll((
   merge_2objs(VM,I,O,[iz(info(combined(Why)))],New),
   Objs = VM.objs,
   select(I,Objs,Objs1),
   select(O,Objs1,Objs2),
   gset(VM.objs)=Objs2)).



% =====================================================================
is_fti_step(objects_as_grid).
% =====================================================================
objects_as_grid(VM):-
 must_det_ll((
  ensure_objects(VM),    
  Objs = VM.objs, % group_to_grid(Objs,Grid), DebugObjs = Objs, confirm_reproduction(Objs,DebugObjs,VM.start_grid),   
  if_t(is_group(Objs),
   (group_to_grid(Objs,Grid),
    set(VM.start_grid)= Grid,
    set(VM.grid)=_ )),
  print_grid(VM))),!.


% =====================================================================
is_fti_step(ensure_objects).
% =====================================================================
ensure_objects(VM):-
 must_det_ll((
  if_t(\+ is_group(VM.objs),
  (individuate(complete,VM))))).

% =====================================================================
is_fti_step(objects_into_grid).
% =====================================================================
objects_into_grid(VM):-
 must_det_ll((
  ensure_objects(VM),
  Objs = VM.objs,
  group_to_grid(Objs,Grid),
  %DebugObjs = Objs, confirm_reproduction(Objs,DebugObjs,VM.start_grid),
  set(VM.grid)=Grid,
  print_grid(VM))),!.

% =====================================================================
is_fti_step(nsew_2).
% =====================================================================
nsew_2(VM):- 
  Points = VM.lo_points,
  select(C-P1,Points,Points1),
  member(Nsew,[s,e]),
  is_adjacent_point(P1,Nsew,P2),
  select(C-P2,Points1,PointsRest),
  \+ (is_adjacent_point(P1,_,P1E),member(C-P1E,PointsRest)),
  \+ (is_adjacent_point(P2,_,P2E),member(C-P2E,PointsRest)),
  make_indiv_object(VM,[iz(type(nsew)),iz(media(shaped)),birth(nsew_2)],[C-P1,C-P2],NewObj),
  set(VM.lo_points)=PointsRest,
  assumeAdded(VM,NewObj),
  nsew_2(VM),!.
nsew_2(VM):- 
  Points = VM.lo_points,
  select(C-P1,Points,PointsRest),
  \+ (is_adjacent_point(P1,_,P1E),member(C-P1E,PointsRest)),
  make_indiv_object(VM,[iz(type(nsew)),iz(media(shaped)),birth(nsew_1)],[C-P1],NewObj),
  set(VM.lo_points)=PointsRest,
  assumeAdded(VM,NewObj),
  nsew_2(VM),!.
nsew_2(_).

overlapping_points(PointsRest,CantHave):- member(P1,PointsRest),member(P1,CantHave).

% =====================================================================
is_fti_step(colormass_3).
% =====================================================================
%colormass_3(_VM):- !.
colormass_3(VM):- 
  globalpoints(VM.objs,CantHave),
  Points = VM.lo_points,
  intersection(CantHave,Points,_RemoveThese,_,KeepThese),
  select(C-P1,KeepThese,Points1),
  member(Nsew,[s,e]),
  is_adjacent_point(P1,Nsew,P2),
  select(C-P2,Points1,PointsRest),
  FirstTwo = [C-P1,C-P2],
  %\+ overlapping_points(FirstTwo,CantHave),
  all_individuals_near(_SkipVM,Nsew,nsew,C,FirstTwo,PointsRest,LeftOver,GPoints),
  %\+ overlapping_points(GPoints,CantHave),!,
  append(FirstTwo,GPoints,IndivPoints),
  sort(IndivPoints,IndivPointSet),
  print_ss(cm3,VM.objs,IndivPointSet),
  length(IndivPointSet,Len),Len>=3,
  \+ (member(C-P3,IndivPointSet),member(C-P4,LeftOver),is_adjacent_point(P3,Card,P4),n_s_e_w(Card)),
  make_indiv_object(VM,[iz(type(colormass)),iz(media(shaped)),birth(colormass_3)],IndivPointSet,NewObj),
  set(VM.lo_points)=LeftOver,
  assumeAdded(VM,NewObj),
  colormass_3(VM),!.
colormass_3(_).


% =====================================================================
is_fti_step(sub_individuate).
% =====================================================================
sub_individuate(From,SubProgram,VM):-
  OldObjs = VM.objs,
  ignore(run_fti(VM,From)),
  NewObjs = VM.objs,
  intersection(NewObjs,OldObjs,_,OnlyNew,_),
    preserve_vm(VM,my_maplist(individuate_object(VM, VM.gid,SubProgram),OnlyNew,WasInside)),
  my_maplist(addObjects(VM),WasInside),
  set_vm(VM).


% =====================================================================
is_fti_step(rule).
% =====================================================================
rule(Pre,Post,VM):- 
  ensure_objects(VM),
  OldObjs = VM.objs,
  my_maplist(maybe_rule(VM,Pre,Post),OldObjs,NewObjs),
  gset(VM.objs)=NewObjs,
  print_side_by_side(did_rule(Pre,Post),OldObjs,NewObjs),!.

is_fti_stepr(maybe_rule).
maybe_rule(VM,Pre,Post,Obj,NewObj):- 
   pp(rule(Pre,Post,Obj)),
   sub_term(E,Obj),shall_count_as_same(Pre,E),ignore(Pre=E),do_rule(VM,Pre,Post,Obj,NewObj),!.
maybe_rule(_,_,_,Obj,Obj).

do_rule(VM,Pre,Post,Obj,NewObj):-
   object_grid(Obj,GPs),
   obj_call(Post,GPs,GPsO),!,
   globalpoints_include_bg(GPsO,LPoints),
   loc2D(Obj,OX,OY),
   offset_points(OX,OY,LPoints,NewGPoints),
   must_det_ll(rebuild_from_globalpoints(VM,Obj,NewGPoints,NewObj)),
   print_side_by_side(rule(Pre,Post),[Obj],[NewObj]),!,
   Obj\=@=NewObj,!.
do_rule(_VM,Pre,Post,Obj,NewObj):-
   obj_call(Post,Obj,NewObj),!,
   print_side_by_side(rule(Pre,Post),[Obj],[NewObj]),!,
   Obj\=@=NewObj,!.

obj_call(subst_color(fg,Color),O1,O2):- !, map_pred1(replace_term(is_fg_color,=(Color)),O1,O2).
obj_call(copy,Obj,ObjO):- !, ObjO=Obj.
obj_call(P2,I,O):- object_call(P2,I,O),!.

replace_term(P1C,P1A,I,O):- p1_call(P1C,I), p1_call(P1A,O).

% =====================================================================
is_fti_step(grid_to_obj_other).
% =====================================================================

grid_to_obj_other(VM):- 
  Grid= VM.start_grid,
  forall(grid_to_obj_other(Grid,VM,_O),true).

% =====================================================================
:- ensure_loaded(kaggle_arc_individuation_pbox).
:- ensure_loaded(kaggle_arc_prior_groups).
% =====================================================================

% =====================================================================
is_fti_step(only_proportional_mass).
% =====================================================================

only_proportional_mass(VM):- 
  Grid= VM.start_grid,
  mass(Grid,GMass),
  Objs = VM.objs,
  findall(O,(member(O,Objs),mass(O,OMass),(kept_ideal_obj(VM,GMass,Objs,OMass,O)->true;(print_grid(removing,O),fail))),Keep),
  gset(VM.objs) = Keep.

:- style_check(-singleton).
kept_ideal_obj(VM,GMass,Objs,OMass,O):- OMass=:=GMass,!,fail.
kept_ideal_obj(VM,GMass,Objs,0,O):- !,fail.
kept_ideal_obj(VM,GMass,Objs,OMass,O):- 0 is GMass rem OMass,!.
kept_ideal_obj(VM,GMass,Objs,OMass,O):- has_prop(iz(media(image)),O), \+ has_prop(iz(media(shaped)),O),!,fail.
kept_ideal_obj(VM,GMass,Objs,OMass,O).
:- style_check(+singleton).

% =====================================================================
is_fti_step(find_hybrid_shapes).
% =====================================================================
find_hybrid_shapes(VM):-
 ignore((
  enum_hybrid_shapes(Set,VM),
  %find_hybrid_shapes_on(VM.gid,Set,VM.o_grid,GroupO),
  maybe_find_hybrid_shapes_on(Set,VM.grid,Group),
  ((var(Group);Group==[])->true;addOGSObjects(VM,Group)))).
  
enum_hybrid_shapes(Set,_VM):-
 must_det_ll((
  current_test_example(TestID,ExampleNum),
  Pair = pair,
  findall(Grid,hybrid_shape(TestID,ExampleNum,Pair,Grid),List),
  remove_shapes_redundant(List,Better),
  my_maplist(release_non_mc,Better,FGList),
  predsort_on(hybrid_order,FGList,Set))).

remove_shapes_redundant([S1,S2|List],Better):- maybe_ogs(Found,S1,S2),Found\==[],!,
  remove_shapes_redundant([S1|List],Better).
remove_shapes_redundant([S1|List],[S1|Better]):- !, remove_shapes_redundant(List,Better).
remove_shapes_redundant([],[]).

count_adjacent_same_colored_points(O1,O2,HVCount,DiagCount):-
  flag(is_diag,WasD,0),flag(is_hv,WasH,0),
  forall((member(C-P1,O1),member(C-P2,O2), once(is_adjacent_point(P1,Dir,P2))),
    (is_diag(Dir)->flag(is_diag,D,D+1);flag(is_hv,D,D+1))),
  flag(is_diag,DiagCount,WasD),
  flag(is_diag,HVCount,WasH),!.

ogs_size_ok(SX,SY,_Mass):- SY>=3,SX>=3,!.
ogs_size_ok(SX,SY,_Mass):- SY=1,SX>=3,!.
ogs_size_ok(SX,SY,_Mass):- SX=1,SY>=3,!.
ogs_size_ok(SX,SY,_Mass):- SY=2,SX=2,!.
ogs_size_ok(_SX,_SY,Mass):- Mass>3,!.

maybe_find_hybrid_shapes_on(_,_,[]):-!.
maybe_find_hybrid_shapes_on( Set,Grid,AllInUse):- find_hybrid_shapes_on( Set,Grid,AllInUse),!.
find_hybrid_shapes_on([],_,[]):-!.
find_hybrid_shapes_on(_Set,Grid,[]):- mass(Grid,GMass), GMass<1,  !.
find_hybrid_shapes_on( Set,Grid,AllInUse):-
  once((length(Set,Len), Len<10);
  (globalpoints(Grid,AvailablePoints),
    fg_points(AvailablePoints,FGPoints),length(FGPoints,Need),Need<100)),
 multi_hybrid_shapes_on( Set,Grid,AllInUse),!.
find_hybrid_shapes_on(_Set,_Grid,[]).


multi_hybrid_shapes_on(Set,Grid,GoodFit):-
  length(Set,Len),
  print_grid(find_hybrid_shapes_on(Len),Grid),!,Len=<5,
  once((
       if_t(var(ROHOVInS),
        ignore((with_ogs_trace([],
          findall(ROHOV,
             ((member(In,Set),maybe_ogs([+rul(strict),+rot2D(sameR)],ROHOV,In,Grid))),ROHOVInS)),ROHOVInS\==[]))),
       if_t(var(ROHOVInS),
        ignore((with_ogs_trace([],
          findall(ROHOV,
             ((member(In,Set),maybe_ogs([+rul(strict)],ROHOV,In,Grid))),ROHOVInS)),ROHOVInS\==[]))),
       if_t(var(ROHOVInS),
        ignore((with_ogs_trace([],
          findall(ROHOV,
             ((member(In,Set),maybe_ogs([+rul(loose),+rot2D(sameR)],ROHOV,In,Grid))),ROHOVInS)),ROHOVInS\==[]))))),

  ROHOVInS\==[], % unless debugging 
  ((
  print_grouped_props(find_hybrid_shapes_on,ROHOVInS),
  if_t(ROHOVInS==[],
    forall(member(In,Set),
      ((ignore((test_ogs_for_ans(fail,find_hybrid_shapes_on,In,Grid)))),itrace))),
  my_maplist(ogs_with_location(Grid),ROHOVInS,GPointsList,_PropsList),
  %globalpoints(OgsList,OgsPoints),   
  (no_overlap(GPointsList)->GoodFit=ROHOVInS;
   ( 
     print_ss(skipOgsList=OgsList),!,fail,
     ((MinMaxPerObject = range(0-1))),
      
     nop((mass(Grid,GMass),MinMaxLeftOver = range(0-GMass))),
     bestfit_hybrid_shapes_on(mono_colorhack,OgsList,Grid,MinMaxPerObject,MinMaxLeftOver,GoodFit))),
  print_grid(ogsPoints,GoodFit))).

no_overlap(OgsList):- \+ yes_overlap(OgsList).
yes_overlap(OgsList):- 
  my_maplist(globalpoints,OgsList,CPoints),my_maplist(arg(2),CPoints,Points),!,
  two_elements(Points,Obj1,Obj2),
  member(Point,Obj1),member(Point,Obj2),!.

two_elements(Points,Obj1,Obj2):- append(_,[Obj1|RPoints],Points),member(Obj2,RPoints).
/*
make_fit(OgsList,Grid,RGroup):- 
  best_fit_combos(OgsList,Grid,ComboGroups),
  last(ComboGroups,Group),
  remove_sort_tag(Group,RGroup).
*/
/*
find_hybrid_shapes_on( Set,Grid,GoodFit):-
 unique_fg_colors(Grid,[_]),
  MinMaxPerObject = range(_-_),
  once((length(Set,Len), Len<10);
  (globalpoints(Grid,AvailablePoints),
    fg_points(AvailablePoints,FGPoints),length(FGPoints,Need),Need<100)),
  MinMaxLeftOver = range(0-0),
  length(Set,Len), Len<5,
 bestfit_hybrid_shapes_on(mono_colorhack,Set,Grid,MinMaxPerObject,MinMaxLeftOver,GoodFit),!.

*/


/*
localpoints(I,LPoints):- object_grid(I,Grid),localpoints(Grid,LPoints).
object_grid(I,LPoints):- localpoints(I,LPoints),vis2D(H,V),points_to_grid(H,V,LPoints,Grid).
*/
%ogs_into_obj_props(_Obj,AnsProps,Obj):-
%ogs_with_location(_OutGrid,ObjL,Obj):- \+ is_list(ObjL), indv_props_list(ObjL,PropStart),ogs_into_obj_props(PropStart,Obj),!.
ogs_with_location(_OutterGrid,ObjL,GPoints,Props):- get_gpoints_and_props(ObjL,GPoints,Props),!.
  
get_gpoints_and_props(ReallyAdd,GPoints,Props):- var(ReallyAdd),!,throw(instaniation_error(get_gpoints_and_props(ReallyAdd,GPoints,Props))).
get_gpoints_and_props(obj_loc(PropsM,GPointsM),GPoints,Props):- !, 
  get_gpoints_and_props(PropsM,_,Props), get_gpoints_and_props(GPointsM,GPoints,_).
get_gpoints_and_props(ReallyAdd,GPoints,Props):- is_grid(ReallyAdd),!, globalpoints(ReallyAdd,GPoints),Props=[].
get_gpoints_and_props(ReallyAdd,GPoints,Props):- is_cpoints_list(ReallyAdd),!, globalpoints(ReallyAdd,GPoints),Props=[].
%get_gpoints_and_props(ReallyAdd,GPoints,Props):- \+ is_obj_props(ReallyAdd),!,indv_props_list(ReallyAdd,PropsList),!,get_gpoints_and_props(PropsList,GPoints,Props).
get_gpoints_and_props(ReallyAdd,GPoints,Props):- \+ is_list(ReallyAdd),!,indv_props_list(ReallyAdd,PropsList),!,
  get_gpoints_and_props(PropsList,GPoints,Props).
%get_gpoints_and_props(ReallyAdd,GPoints,Props):- compound(ReallyAdd),ReallyAdd=obj(LProps),!,get_gpoints_and_props(LProps,GPoints,Props).
get_gpoints_and_props(ReallyAdd,GPoints,Props):- 
  select(globalpoints(GPoints),ReallyAdd,Props),!.

get_gpoints_and_props(Props,RGOPoints,Props):-
  member(loc2D(OH,OV),Props),
  member(localpoints(LPoints),Props),!,
  offset_points(OH,OV,LPoints,GOPoints),
  include(p1_arg(1,is_real_color),GOPoints,RGOPoints).

get_gpoints_and_props(Props,RGOPoints,PropsO):-
  member(grid(In),Props),localpoints(In,LPoints),!,
  get_gpoints_and_props([localpoints(LPoints)|Props],RGOPoints,PropsO).


/*

*/
to_props_and_globalpoints(ObjL,_Ans,_GOPoints):- is_grid(ObjL),!,fail.
to_props_and_globalpoints(ObjL,Ans,GOPoints):- get_gpoints_and_props(ObjL,GOPoints,Ans).
ogs_into_obj_props( OutGrid,AnsProps,Obj):- like_object(AnsProps,OutGrid,Obj),!.

like_object(Ans,Out,ObjO):- 
  get_gpoints_and_props(Ans,GOPoints,Props),
  grid_to_gid(Out,GID),grid_size(Out,GH,GV), 
  make_indiv_object_s(GID,GH,GV,Props,GOPoints,ObjO).
like_object(Ans,Out,obj([f_grid(Grid),grid(Grid)|ObjO])):- 
  to_props_and_globalpoints(Ans,Props,GOPoints),
  once(member(grid(Grid),Ans);member(f_grid(Grid),Ans);object_l(grid(Grid),Ans);object_l(grid_f(Grid),Ans)),
  grid_size(Out,GH,GV),
  grid_to_gid(Out,GID),
  make_indiv_object_s(GID,GH,GV,Props,GOPoints,obj(ObjO)).

%ogs_into_obj_props(I,GPoints):- localpoints(I,LPoints),loc2D(I,OH,OV),offset_points(OH,OV,LPoints,GPoints).


hybrid_shape_from_search(Group,VM):-
   addObjects(VM,Group).

hybrid_order(Grid,DCT+NArea):-
  dont_care_terms(Grid,DCT),area(Grid,Area),NArea is -Area.

dont_care_terms(Grid,Len):- term_variables_len(Grid,Len),Len>0,!.
dont_care_terms(Grid,Len):- colors_match_count(is_bg_color,Len,Grid).

colors_match_count(Match,Count,Grid):- colors_match_count(Match,0,Count,Grid).
colors_match_count(Match,N,Count,CD):- \+ is_list(CD),!,(cmatch(Match,CD)->plus(N,1,Count);N=Count).
colors_match_count(Match,N,Count,[H|T]):- !, colors_match_count(Match,N,NCount,H), colors_match_count(Match,NCount,Count,T).
colors_match_count(_,N,N,[]).

%shape_size(G,H+V+VsC+Cs):- grid_size(G,H,V),term_variables_len(G,VsC),colors_cc(G,Cs).
term_variables_len(G,VsC):- term_variables(G,Vs),length(Vs,VsC).

:- dynamic(is_hybrid_shape/4).
hybrid_shape(TestID,ExampleNum,Name,Shape):- is_hybrid_shape(TestID,ExampleNum,Name,Shape).
%hybrid_shape(_TestID,_ExampleNum,Name,Shape):- shape_lib_direct(Name,GalleryS), member(Shape,GalleryS). 


learn_hybrid_shape(ReColored):- 
  learn_hybrid_shape(pair,ReColored).
learn_hybrid_shape(_,_).

learn_hybrid_shape_real(ReColored):-
  learn_hybrid_shape_real(pair,ReColored).
learn_hybrid_shape_real(Type,Obj):- is_list(Type),!,my_maplist(lambda_rev(learn_hybrid_shape_real(Obj)),Type).
%learn_hybrid_shape(Type,Obj):- is_group(Obj),!,mapgroup(learn_hybrid_shape(Type),Obj).
learn_hybrid_shape_real(Name,ReColored):-
 current_test_example(TestID,ExampleNum),
 learn_hybrid_shape(TestID,ExampleNum,Name,ReColored).

%learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- !, nop(learn_hybrid_shape(TestID,ExampleNum,Name,ReColored)).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_grid(ReColored),!,learn_hybrid_shape_grid(ReColored,TestID,ExampleNum,Name,ReColored).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_group(ReColored),!,mapgroup(learn_hybrid_shape(TestID,ExampleNum,Name),ReColored).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_list(ReColored),!,my_maplist(learn_hybrid_shape(TestID,ExampleNum,Name),ReColored).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_object(ReColored),!,object_grid(ReColored,Grid), 
  learn_hybrid_shape_grid(ReColored,TestID,ExampleNum,Name,Grid).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- into_grid(ReColored,Grid),!,learn_hybrid_shape(TestID,ExampleNum,Name,Grid).

use_hybrid_grid(In):- grid_size(In,H,V),(H<3;V<3),mass(In,Mass),Mass\==0,Mass<5,!,fail.
use_hybrid_grid(In):- In\=[[_]], mass(In,Mass),Mass>2, grid_size(In,H,V),H>2,V>2. % nop((area(In,AMass),AMass < Mass*2)).
use_hybrid_grid(In):- In\=[[_]], mass(In,Mass),Mass>2, nop((area(In,AMass),AMass < Mass*2)).

% learn_hybrid_shape_grid(_ReColored,_TestID,_ExampleNum,_Name,_Grid):-!.
learn_hybrid_shape_grid(ReColored,TestID,ExampleNum,Name,Grid):- is_grid(ReColored), \+ use_hybrid_grid(Grid),!,ignore((Grid\=[[_]], nop(print_grid(unused(learn_hybrid_shape_grid(TestID,ExampleNum,Name)),ReColored)))),!.
learn_hybrid_shape_grid(Object,TestID,ExampleNum,Name,Grid):- \+ is_always_kept(Object),
  \+ use_hybrid_grid(Grid),!,ignore((Grid\=[[_]], nop(print_grid(unused(learn_hybrid_shape_grid(TestID,ExampleNum,Name)),Grid)))),!.
learn_hybrid_shape_grid(Object,TestID,ExampleNum,Name,ReColored):- % print_grid(learn_hybrid_shape(TestID,ExampleNum,Name),ReColored),
  desc_title(Object,Title),
  if_t( \+ is_hybrid_shape(TestID,ExampleNum,Name,ReColored),
    (print_grid(learn_hybrid_shape_grid(TestID,ExampleNum,Name,Title),ReColored),
     assert_if_new(is_hybrid_shape(TestID,ExampleNum,Name,ReColored)))).

desc_title(Object,Title):- is_object(Object),!,tersify(Object,Title).
desc_title(Grid,  Title):- is_grid(Grid),colors_cc(Grid,CC),!,(known_grid(ID,Grid)->Title=(ID/CC);Title=CC).
desc_title(Object,Title):- data_type(Object,Title),!.

title_objs(Obj,Title=Grid):- is_object(Obj),object_glyph(Obj,Glyph),loc2D(Obj,X,Y),vis2D(Obj,H,V),!,
  Title=objFn(Glyph,[loc2D(X,Y),size2D(H,V)]),
  object_grid(Obj,Grid),!.
title_objs(Obj,Title=Grid):- desc_title(Obj,Title),object_grid(Obj,Grid),!.

print_obj_short_props(Set):- is_list(Set),!,my_maplist(print_obj_short_props,Set).
print_obj_short_props(Obj):- short_indv_props(Obj,Props,_),global_grid(Obj,Grid),!,print_grid(Grid),nl_if_needed,ppnl(Props).

obj_short_props(Obj,Title=Grid):- short_indv_props(Obj,Title,_),global_grid(Obj,Grid),!.

r_props(Short,SortR):- % obj_to_oid(Obj,MyOID),
  remove_too_verbose(0,Short,TV0),include(not_too_verbose,TV0,TooV),
  sort_safe(TooV,Sort),reverse(Sort,SortR).

short_indv_props(Props,ShortR,LongR):- is_list(Props),!,my_partition(is_short_prop,Props,Short,Long),r_props(Short,ShortR),r_props(Long,LongR).
short_indv_props(Obj,ShortR,LongR):- indv_props_list(Obj,Props),!,short_indv_props(Props,ShortR,LongR).

is_long_prop(vis_hv_term(_)).
is_long_prop(link(_,_,_)). is_long_prop(link(_,_)).
is_long_prop(pg(_,_,_,_)). is_long_prop(giz(_)).
is_long_prop(edge(_,_)). is_long_prop(on_edge(_,_)). is_long_prop(on_edge(_)). 
is_long_prop(pen(_)).
is_long_prop(merged(_)). is_long_prop(rot2D(sameR)).
is_long_prop(gid(_)). is_long_prop(cc(_,0)). is_long_prop(symmetry_type(_)).
is_long_prop(chromatic(_,_)). is_long_prop(sizeY(_)). is_long_prop(sizeX(_)). 
is_long_prop(overlap(_,_)).  is_long_prop(dir_touching(_,_)). is_long_prop(dir_seeing(_,_)). is_long_prop(contained_by(_,_)).
is_long_prop(iz(P)):- compound(P),arg(1,P,A),number(A),!.
is_long_prop(iz(S)):-!,is_long_prop(S).
is_long_prop(P):- compound(P),arg(1,P,A),is_long_arg(A).
is_long_prop(P):- compound(P),functor(P,_,A),arg(A,P,E),is_list(E),!.
is_long_arg(A):- A==[],!.  
is_long_arg(A):- is_points_list(A),!.
is_long_arg(A):- is_gridoid(A),!.

is_short_prop(birth(_)). 
is_short_prop(sid(_)). 
is_short_prop(giz(g(_))). is_short_prop(giz(glyph(_))).
is_short_prop(P):- is_long_prop(P),!,fail.
is_short_prop(_).

get_hybrid_set(Set):-
  findall(O,(current_test_example(TestID,ExampleNum),
             hybrid_shape(TestID,ExampleNum,_Name,O)),List),
  sort_safe(List,SList),
  h_all_rots(SList,SetR),
  predsort_on(hybrid_order,SetR,Set).

h_all_rots(Set,SetR):- findall(E,(member(G,Set),each_rot(G,E)),L),list_to_set(L,SetR).

each_rot(G,G).
each_rot(G,R):- rot90(G,R).
each_rot(G,R):- rot180(G,R).
each_rot(G,R):- rot270(G,R).
each_rot(G,R):- flipH(G,R).
each_rot(G,R):- flipV(G,R).

% =====================================================================
is_fti_step(use_member).
% =====================================================================
use_member(Member,VM):- 
  into_grid(VM.Member,Grid),
  adopt_grid(Grid,VM).
% =====================================================================
is_fti_step(adopt_grid).
% =====================================================================
adopt_grid(Grid,VM):- 
  gset(VM.grid)= Grid,
  globalpoints(Grid,Points),
  gset(VM.lo_points)= Points.
% =====================================================================
is_fti_step(vm_grid_call).
% =====================================================================
vm_grid_call(Call,VM):- 
  call(Call,VM.grid,NewGrid),
  adopt_grid(NewGrid,VM).

% =====================================================================
is_fti_step(reset_points).
% =====================================================================
reset_points(VM):- 
  gset(VM.lo_points)= VM.start_points.

% =====================================================================
is_fti_step(reset_grid).
% =====================================================================
reset_grid(VM):- 
  gset(VM.grid)= VM.start_grid.

% =====================================================================
is_fti_step(reset_objs).
% =====================================================================
reset_objs(VM):- 
  gset(VM.objs)= [].

fix_indivs_options(O,L):-is_list(O),my_maplist(fix_indivs_options,O,OL),my_append(OL,L).
fix_indivs_options(G,[G]):- var(G),!.
fix_indivs_options(I,O):- atom(I),individuation_macros(I,M),!,fix_indivs_options(M,O),!.
fix_indivs_options(macro(I),[macro(O)]):- fix_indivs_options(I,O).
%fix_indivs_options(save_as_obj_group(I),[save_as_obj_group(O)]):- fix_indivs_options(I,O).
fix_indivs_options(fg_subtractions(I),[fg_subtractions(O)]):- fix_indivs_options(I,O).
fix_indivs_options(fg_shapes(I),[fg_shapes(O)]):- fix_indivs_options(I,O).
fix_indivs_options(subshape_both(V,I),[subshape_both(V,O)]):- fix_indivs_options(I,O).
fix_indivs_options(detect(O),[detect(O)]):-!.
fix_indivs_options(O,[detect(O)]):- is_gridoid(O),!.
fix_indivs_options(I,O):-listify(I,O),!.

% =====================================================================
is_fti_step(keep_only_shown).
% =====================================================================
keep_only_shown(_Layers,_VM):-!.
keep_only_shown(Layers,VM):-
  filter_shown(VM.h,VM.v,VM.objs,Layers,OutShown0,OutHidden),
  if_t(OutHidden\==[],
    print_ss(blue,OutHidden,deleting,OutShown0,keeping)),
  combine_same_globalpoints(OutShown0,OutShown),
  gset(VM.objs)=OutShown,!.

filter_shown(OH,OV,OutC,N,OutShown,OutHidden):-  N=<1,!, filter_shown(OH,OV,OutC,OutShown,OutHidden),!.
filter_shown(OH,OV,OutC,N,OutShown,OutHiddenO):-  NN is N-1,
   include_always_keep(OutC,Kept),
   filter_shown(OH,OV,OutC,NN,OutShown0,OutHidden0),
   filter_shown(OH,OV,OutHidden0,_Hidden,OutHiddenKeep),
   append_sets([OutShown0,OutHiddenKeep,Kept],OutShown),
   my_partition(is_not_in(OutShown),OutC,OutHiddenO,_).
   

is_always_kept(Obj):- sub_term(E,Obj),(E==diamonds;E==flag(always_keep)),!.
%is_always_kept(Obj):- has_prop(birth(indiv(i_colormass)),Obj),!.
include_always_keep(OutC,Kept):- include(is_always_kept,OutC,Kept).

filter_shown(OH,OV,OutC,OutShown,OutHidden):-
 must_det_ll((
  object_printables(OutC,GroupVis,GroupPP),
  append_sets([GroupVis,GroupPP],OutObjs),
  findall(Obj,(between(1,OH,H),between(1,OV,V),object_at(H,V,OutObjs,Obj)),Shown),
  include_always_keep(OutC,Kept),
  append_sets([Shown,Kept],OutShown0),
  combine_same_globalpoints(OutShown0,OutShown),
  my_partition(is_not_in(OutShown),OutC,OutHidden,_Show))).

is_not_in(Set,Obj):- \+ (member(O,Set),O=@=Obj).
object_at(H,V,OutC,Obj):- hv_point(H,V,Point), member(Obj,OutC),once(globalpoints(Obj,Points)),member(_-Point,Points),!.

% =====================================================================
is_fti_step(keep_points).
% =====================================================================
keep_points(While,VM):- 
  assertion(is_vm_map(VM)),
  set_vm(VM),
  Points  = VM.lo_points,
%  get_vm
  Program = VM.lo_program,
  %OldObjs = VM.objs,
  %set(VM.grid) = VM.start_grid,
  gset(VM.lo_points) = VM.start_points,
  %set(VM.objs) = [],  
  ignore(fti(VM,While)),
  %NewObjs = VM.objs,
  %append(NewObjs,OldObjs,Objs),
  
  %must_det_ll(combine_duplicates(Objs,NewObjs)),
  %list_to_set(Objs, NewObjs),
  %set_vm(VM,objs,NewObjs),
  set_vm(points,Points),
  set_vm(lo_program,Program).
  

% =====================================================================
is_fti_step(maybe_1_3rd_mass).
% =====================================================================
maybe_1_3rd_mass(VM):-
  mass(VM.grid,Mass),
  colors_cc(VM.grid,[cc(C,Size)|_Colors]),
  Area is VM.h * VM.v,
  Size * 3 > Area,
  Size * 3 > Mass,
  Min is Size/2,
  my_partition(has_color(C),VM.lo_points,ThisGroup,LeftOver),
  ignore(((
   length(ThisGroup,Len),  Len >= Min,
   set(VM.lo_points)=LeftOver,
   meets_indiv_criteria(VM,birth(maybe_1_3rd_mass),ThisGroup),
   make_indiv_object(VM,[birth(maybe_1_3rd_mass),iz(media(image))],ThisGroup,ColorObj),
   assumeAdded(VM,ColorObj)))).


:- decl_pt(detect_indvs(group,group,-)).
detect_indvs(In,Out,Grid):- individuate(In,Grid,Out).

individuation_reserved_options(ROptions,Reserved,Options):- 
   fix_indivs_options(ROptions,ReservedOptions),
   %my_partition(is_object_or_grid,ReservedOptions,Reserved,Options),
   Options = ReservedOptions,
   Reserved = [],
   %select_default_i_options(Grid,H,V,Points,DefaultOptions),
   %default_i_options(DefaultOptions),
   %subst(Options0,defaults,DefaultOptions,Options),
   %(Options0==Options -> my_append(Options,DefaultOptions,NewOptions) ; (Options=NewOptions)),!,
   ignore((ROptions \= Options,is_list(ROptions), sub_var(complete,ROptions),
      (progress(blue,fix_indivs_options(ro=ROptions,r=Reserved,o=Options))))).


%individuate_second_pass(Grid,IndvS):- individuate([second_pass],Grid,IndvS).
%?- i(v(e41c6fd3)>(trn+0)*in).

:- dynamic(arc_cache:individuated_cache/5).
:- retractall(arc_cache:individuated_cache(_,_,_,_,_)).
muarc:clear_arc_caches:-  \+ luser_getval(extreme_caching,true), retractall_in_testid(arc_cache:individuated_cache(_,_,_,_)).

:- luser_default(extreme_caching,false).

:- multifile(muarc:clear_all_caches/0).
:- dynamic(muarc:clear_all_caches/0).
muarc:clear_all_caches:-  forall(muarc:clear_arc_caches,true), fail.
:- luser_default(use_individuated_cache,true).
:- luser_setval(use_individuated_cache,true).

get_individuated_cache(TID,ROptions,GOID,IndvS):- nonvar(ROptions),
  ground(GOID), \+ luser_getval(use_individuated_cache,false), call_in_testid(arc_cache:individuated_cache(TID,GOID,ROptions,IndvS)),!.
get_individuated_cache(_TID,ROptions,GOID,IndvS):- nonvar(ROptions),
  ground(GOID), \+ luser_getval(use_individuated_cache,false), saved_group(individuate(ROptions,GOID),IndvS),!.

individuate(VM):- individuate1(VM,VM.start_options,VM.grid,InC),set(VM.objs)=InC,!.
individuate_c(VM):- individuate1(VM,complete,VM.grid,InC),set(VM.objs)=InC,!.
%individuate(VM):-  individuate(VM.start_options, VM),!.

individuate(ROptions,VM):-  individuate(ROptions,VM,_),!.
individuate(_ROptions,Grid,IndvS):- Grid==[],!,IndvS=[].
individuate([ROption],GridIn,IndvS):- !,nonvar(ROption), individuate1(_,ROption,GridIn,IndvS).
%individuate(complete,GridIn,List):- !,  findall(IndvS,(individuator(Some),individuate1(Some,GridIn,IndvS)),List),
individuate(ROptions,GridIn,IndvS):- individuate1(_,ROptions,GridIn,IndvS).

%individuate(ROptions,GridIn,IndvS):- individuation_macros(ROptions,R), atom(R),R\==ROptions,!,individuate1(_,R,GridIn,IndvS).

individuate1(_, ROptions,VM,LF):- is_vm_map(VM),!, individuate1(VM, ROptions, VM.grid, LF).
individuate1(VM,ROptions,GridIn,IndvS):- \+ is_grid(GridIn), into_grid(GridIn,Grid),!,individuate1(VM,ROptions,Grid,IndvS).
individuate1(VM,ROptions,GridIn,IndvS):- 
  must_grid_to_gid(GridIn,GOID), !,
  individuate2(VM,ROptions,GOID,GridIn,IndvS),
  once((delistify_single_element(ROptions,NamedOpts),
        maybe_save_grouped(individuate(NamedOpts,GOID),IndvS))).

must_grid_to_gid(GridIn,GOID):- must_det_ll(grid_to_gid(GridIn,GOID)).

maybe_save_grouped(A,B):- save_grouped(A,B).

allow_out_in :- true.

first_grid_same_areas(In,Out,IO):-
  unique_color_count(In,ISize), unique_color_count(Out,OSize), 
  ((OSize> ISize) -> (IO=out_in);(IO=in_out)).
first_grid_same_areas(In,Out,IO):-
  mass(In,ISize), mass(Out,OSize), 
  ((OSize<ISize) -> (IO=out_in);(IO=in_out)).

first_grid(_In,_Out,in_out):- \+ allow_out_in,!.
%first_grid(In,Out,IO):- trim_to_rect(In,InT),trim_to_rect(Out,OutT),first_grid1(InT,OutT,IO),!.
first_grid(_In,_Out,in_out).

first_grid1(In,Out,in_out):- find_ogs(_,_,In,Out),!.
first_grid1(In,Out,out_in):- find_ogs(_,_,Out,In),allow_out_in,!.
first_grid1(In,Out,out_in):- 
  area(In,IArea), area(Out,OArea), OArea<IArea, allow_out_in, !.
first_grid1(In,Out,IO):-
  area(In,IArea), area(Out,OArea), IArea==OArea, first_grid_same_areas(In,Out,IO), allow_out_in,!.
%first_grid(In,Out,IO):- first_grid_same_areas(In,Out,IO).
first_grid1(_In,_Out,in_out).

compile_and_save_current_test(_):-!.
compile_and_save_current_test(Why):-
 get_current_test(TestID),
 time(compile_and_save_test(TestID)),!,
 detect_all_training_hints,!,
 forall(compile_and_save_current_test_pt_2(TestID,Why),true).

arc_test_property(A,B,C):- 
  get_current_test(TestID), compile_and_save_test(TestID), !,
  arc_test_property(TestID,A,B,C).

arc_common_property(Prop):- arc_test_property(common,_,Prop).
arc_common_property(Prop):- arc_test_property(common,Prop,O),O\==[].

%compile_and_save_current_test_pt_2(TestID,Why):- is_list(Why),!,my_maplist(compile_and_save_current_test_pt_2(TestID),Why).


compile_and_save_current_test_pt_2(_TestID,_):- cant_use_intersections,!.

compile_and_save_current_test_pt_2(_TestID,_):-
   current_pair_io(I,O),
   arc_common_property(containsAll(i-o)),
   mapgrid(cell_minus_cell,I,O,IMinusO),
   print_grid(early_i_minus_o,IMinusO).

compile_and_save_current_test_pt_2(_TestID,_):-
   current_pair_io(I,O), 
   arc_common_property(containsAll(o-i)),
   mapgrid(cell_minus_cell,O,I,OMinusI),
   print_grid(early_o_minus_i,OMinusI),!.

cell_minus_cell(I,O,M):- is_grid(I),!,mapgrid(cell_minus_cell,I,O,M),!.
cell_minus_cell(I,O,M):- I=@=O, M=bg.
cell_minus_cell(I,_,I).

mono_cell_minus_cell(I,O,M):- is_grid(I),!,mapgrid(mono_cell_minus_cell,I,O,M),!.
mono_cell_minus_cell(I,O,M):- is_fg_color(I),is_fg_color(O), M=bg.
mono_cell_minus_cell(I,_,I).

lessThan(How,A,B):- call(How,A,AA),call(How,B,BB), AA < BB.

individuate_nonpair(ROptions,In,IndvSI):- 
  into_grid(In,InG), 
  compile_and_save_current_test([for(InG)]),!,
  individuate(ROptions,InG,IndvSI).

individuate_pair(ROptions,In,Out,InC,OutC):-
 into_grid(In,InG), into_grid(Out,OutG),
 [for(out,OutG),for(in,InG)] = Why,
 compile_and_save_current_test(Why),!,
  (individuate_pair1(ROptions,In,Out,IndvSI,IndvSO)*->
   true;individuate_pair2(ROptions,In,Out,IndvSI,IndvSO)),!,
 optimize_objects(IndvSI,IndvSO,InC,OutC).

individuate_pair1(ROptions,In,Out,IndvSI,IndvSO):-
 once((
  check_for_refreshness,
  into_grid(In,InG), into_grid(Out,OutG),
  current_test_example(TestID,ExampleNum),
 w_section(["individuate pair: ",TestID,ExampleNum,g(ROptions)],
  must_det_ll(((   
   first_grid(InG,OutG,IO),
   (IO==in_out 
       -> individuate_two_grids(ROptions,InG,OutG,IndvSI,IndvSO)
              ; individuate_two_grids(IO,OutG,InG,IndvSO,IndvSI)))))))),
 (IndvSO\==[],IndvSI\==[]).

individuate_pair2(ROptions,In,Out,IndvSI,IndvSO):-
 must_be_free(IndvSI),must_be_free(IndvSO),
  check_for_refreshness,
  into_grid(In,InG), into_grid(Out,OutG),
  with_current_pair(InG,OutG,individuate_two_grids(ROptions,InG,OutG,IndvSII,IndvSOO)),
  IndvSOO=IndvSO,IndvSII=IndvSI,!.



doing_pair:- nb_current(doing_pair,t).

individuate_two_grids(ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
  delistify_single_element(ROptions,NamedOpts),
  must_grid_to_gid(GridIn,OIDIn), must_grid_to_gid(GridOut,OIDOut),
  locally(nb_setval(doing_pair,t),
    with_current_pair(GridIn,GridOut,
      individuate_two_grids_once(two(OIDIn,OIDOut),NamedOpts,GridIn,GridOut,IndvSI,IndvSO))).

/*
individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- saved_group(individuate(ROptions,OID_In_Out),IndvS),!,into_gio(IndvS,IndvSI,IndvSO),!.
individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
    individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO),
    ignore((into_iog(IndvSI,IndvSO,IndvS),maybe_save_grouped(individuate(ROptions,OID_In_Out),IndvS))).
*/

individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
  %u_dmsg(individuate(ROptions,OID_In_Out,IndvSI,IndvSO)),
  ((saved_group(individuate(ROptions,OID_In_Out),IndvS),IndvS\==[])
    -> into_gio(IndvS,IndvSI,IndvSO)
    ; ((individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO),
         into_iog(IndvSI,IndvSO,IndvS),maybe_save_grouped(individuate(ROptions,OID_In_Out),IndvS)))).


:- retractall(is_why_grouped_g(_,_,_,_)).
muarc:clear_all_caches:-  retractall(is_why_grouped_g(_,_,_,_)), fail.

individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
 must_det_ll((
  grid_to_tid(GridIn,IDIn), grid_to_tid(GridOut,IDOut),
  localpoints_include_bg(GridIn,PIn),
  localpoints_include_bg(GridOut,POut),
  individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IDIn,IDOut,PIn,POut,IndvSI,IndvSO))),!.


individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IDIn,IDOut,PIn,POut,IndvSI,IndvSO):- fail,
  grid_size(GridIn,HIn,VIn), grid_size(GridOut,HOut,VOut), HIn==HOut,VIn==VOut,
  include(is_bgp,PIn,PInXBG), 
  include(is_bgp,POut,POutXBG),
  intersection(PInXBG,POutXBG,SInOutXBG,UInXBG,UOutXBG),
  SInOutXBG\==[],
  mostly_fgp(PIn,PInX),
  mostly_fgp(POut,POutX),
  append(PInX,UInXBG,PUIn),
  append(POutX,UOutXBG,PUOut),
  (PUIn\==PIn;PUOut\==POut),!,
  must_det_ll((
  %points_to_grid(HIn,VIn,PUIn,GridInX),
  %points_to_grid(HOut,VOut,PUOut,GridOutX),
  u_dmsg(individuate_two_grids_now),
  individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IDIn,IDOut,PUIn,PUOut,IndvSI,IndvSO))).


individuate_two_grids_now(OID_In_Out,ROptions,Grid_In,Grid_Out,ID_In,ID_Out,P_In,P_Out,IndvSI,IndvSO):-
  
  grid_size(Grid_In,H_In,V_In),  
  grid_size(Grid_Out,H_Out,V_Out), 
  must_det_ll((
  maybe_into_fti(ID_In,ROptions,Grid_In,VM_In), set(VM_In.target_grid) = Grid_Out,
  maybe_into_fti(ID_Out,ROptions,Grid_Out,VM_Out), set(VM_Out.target_grid) = Grid_In,

  if_t(fail,
  if_t((H_In==H_Out,V_In==V_Out),((
      mostly_fgp(P_In,P_InX),
      mostly_fgp(P_Out,P_OutX),
      intersection(P_InX,P_OutX,Same,U_In,U_Out),
      u_dmsg(intersection(P_InX,P_OutX,Same,U_In,U_Out)),

      if_t((U_In==[],U_Out\==[]),(set(VM_Out.lo_points)=U_Out,set(VM_Out.start_points)=U_Out,
                            points_to_grid(H_Out,V_Out,U_Out,Grid_OutX),
                            set(VM_Out.grid)=Grid_OutX,set(VM_Out.start_grid)=Grid_OutX)),

      if_t((U_In\==[],U_Out==[]),(set(VM_In.lo_points)=U_In,set(VM_In.start_points)=U_In,
                            points_to_grid(H_In,V_In,U_In,Grid_InX),
                            set(VM_In.grid)=Grid_InX,set(VM_In.start_grid)=Grid_InX)),

      if_t((U_In\==[],U_Out\==[]),(subtract(P_Out,Same,NewP_Out),
                             set(VM_Out.lo_points)=NewP_Out,set(VM_Out.start_points)=NewP_Out,
                             points_to_grid(H_Out,V_Out,NewP_Out,Grid_OutX),
                             set(VM_Out.grid)=Grid_OutX,set(VM_Out.start_grid)=Grid_OutX)),

      !
    )))),

  (var(Grid_InX)->Grid_InX=Grid_In;true),
  (var(Grid_OutX)->Grid_OutX=Grid_Out;true),
  individuate_two_grids_now_X(OID_In_Out,ROptions,Grid_In,Grid_Out,VM_In,VM_Out,Grid_InX,Grid_OutX,IndvSI,IndvSO))).


individuate_two_grids_now_X(OID_In_Out,ROptions,Grid_In,Grid_Out,VM_In,VM_Out,Grid_In,Grid_Out,Objs_In,Objs_Out):- 
 must_det_ll((   
   grid_to_tid(Grid_In,ID_In), grid_to_tid(Grid_Out,ID_Out),
   individuate2(VM_In,ROptions,ID_In,Grid_In,Objs_In),
   individuate2(VM_Out,ROptions,ID_Out,Grid_Out,Objs_Out),
   into_iog(Objs_In,Objs_Out,IndvS),
  maybe_save_grouped(individuate(ROptions,OID_In_Out),IndvS))),!.
/*

individuate_two_grids_now_X(OID_In_Out,ROptions,Grid_In,Grid_Out,VM_In,VM_Out,Grid_InX,Grid_OutX,Objs_InX,Objs_OutX):- 
 must_det_ll((

  grid_to_tid(Grid_In,ID_In), grid_to_tid(Grid_Out,ID_Out),

  print_ss(yellow,Grid_InX,OID_In_Out=gridInX(ROptions,ID_In),_,Grid_OutX,OID_In_Out=gridOutX(ROptions,ID_Out)),

  worker_output((
   with_other_grid(Grid_OutX,do_individuate(VM_In,ROptions,Grid_InX,Objs_In)),!, 

     gset(VM_Out.robjs) = Objs_In,
   with_other_grid(Grid_InX,do_individuate(VM_Out,ROptions,Grid_OutX,Objs_Out)),!,

  must_grid_to_gid(Grid_In,OID_In),must_grid_to_gid(Grid_Out,OID_Out),

  maybe_into_fti(ID_In,ROptions,Grid_InX,VM_InX), set(VM_InX.target_grid) = Grid_Out, set(VM_InX.robjs) = Objs_Out,    
  with_other_grid(Grid_Out,individuate2(VM_InX,ROptions,OID_In,Grid_InX,Objs_InX)),!,

  maybe_into_fti(ID_Out,ROptions,Grid_OutX,VM_OutX), set(VM_OutX.target_grid) = Grid_In, set(VM_OutX.robjs) = Objs_InX, 
  with_other_grid(Grid_In,individuate2(VM_OutX,ROptions,OID_Out,Grid_OutX,Objs_OutX)))),!,

  into_iog(Objs_InX,Objs_OutX,IndvS),
  maybe_save_grouped(individuate(ROptions,OID_In_Out),IndvS))).
*/

individuate2(VM,[ROptions],GOID,Grid,IndvS):- nonvar(ROptions), !, individuate2(VM,ROptions,GOID,Grid,IndvS).


individuate2(_VM,ROptions,GOID,_GridIn,IndvS):- nonvar(GOID), 
  get_individuated_cache(_TID,ROptions,GOID,IndvS),!,
  length(IndvS,Len),ignore((Len>=0,progress(yellow,oid_cached(ROptions,GOID,len(Len),'$VAR'(Len))))),
  !.
individuate2(VM,ROptions,GOID,GridIn,IndvS):-
  do_individuate(VM,ROptions,GridIn,LF),!,
  remove_background_only_object(LF,IndvS,VM),
  if_t(nonvar(GOID),
   (retractall_in_testid(arc_cache:individuated_cache(_,GOID,ROptions,_)), 
    length(IndvS,Len),ignore((Len>0,atom(ROptions),progress(yellow,oid_created(ROptions,GOID,len(Len),'$VAR'(Len))))),
   % if_t(nonvar(VM), TID = VM.id),
    %if_t(var(VM), gid_to_tid(GridIn,TID)),
    %if_t(var(TID), grid_to_tid(GridIn,TID)),
    TID = VM.id,
    asserta_in_testid(arc_cache:individuated_cache(TID,GOID,ROptions,IndvS)))),!.

oid_created(ROptions,GOID,Len,IndvS):- oid_cached(ROptions,GOID,Len,IndvS).

oid_cached(ROptions,G,len(Len),IndvS):- must_grid_to_gid(G,GOID), saved_group(individuate(ROptions,GOID),IndvS),check_len(Len,IndvS).
oid_cached(ROptions,G,len(Len),IndvS):- into_grid(G,GOID), individuate(ROptions,GOID,IndvS),check_len(Len,IndvS).
check_len(Len,IndvS):- \+ \+ ((is_list(IndvS),number(Len),length(IndvS,LenS),LenS>=Len)),!.



into_points_grid(GridIn,Points,Grid):-
  (var(GridIn) -> atrace ; true),
   globalpoints_include_bg(GridIn,Points),
   into_grid(GridIn,Grid),!.

do_individuate(VM, ROptions, GridIn,LFO):- must_be_free(LF), 
 locally(set_prolog_flag(nogc,false),
   (into_grid(GridIn,Grid),  grid_to_tid(Grid,ID), %my_assertion(\+ is_grid(ID)),
    individuate7(VM,ID,ROptions,Grid,LF))),!,
    guard_whole(LF,LFO),
    gset(VM.objs) = LFO,!.
   %nop((tid_to_gid(ID,GID), my_maplist(assert_object_oid(GID),LF,_Glyphs,_OIDs))).
  %smallest_first(mass,IndvS,SF),
  %largest_first(mass,SF,LF),
  
   %!.
   %VM.target_grid

guard_whole([obj(LF)],[obj([iz(stype(part))|LF])]):- !.
guard_whole(LF,LF).
% tiny grid becomes a series of points
%individuate(GH,GV,ID,ROptions,_Grid,Points,IndvS):- \+ is_list(ROptions), is_glyphic(Points,GH,GV), individuate_glyphic(GH,GV,ID,Points,IndvS),!.
%individuate(GH,GV,ID,whole,_Grid,Points,IndvS):-  individuate_whole(GH,GV,ID,Points,IndvS),!.

individuate7(VM,ID,ROptions,GridIn,IndvS):-
  ignore((fix_indivs_options(ROptions,List),list_upto(4,List,Some),append(Some,['...'],Options))),
  w_section(title(individuate(Options,ID)),individuate8(VM,ID,ROptions,GridIn,IndvS)).
individuate8(VM,ID,ROptions,GridIn,IndvS):-
 must_det_ll((
      (var(VM) -> maybe_into_fti(ID,ROptions,GridIn,VM) ; true),
      %VM.lo_points = Points,
      %individuation_reserved_options(ROptions,Reserved,NewOptions),
      %atrace,
      %ensure_fti(GH,GV,ID,Grid,[],Reserved,NewOptions,Points,VM),   
      set_vm(VM),
      %individuals_raw(VM,GH,GV,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
      run_fti(VM,ROptions),
      ObjsB = VM.objs,
      remove_background_only_object(VM),
      find_relations(VM),      
      remove_dead_links(VM),
      extend_obj_proplists(VM),
      really_group_vm_priors(VM),
      %remove_if_prop(cc(fg,0),VM),
      IndvSRaw = VM.objs,
  %as_debug(9,ppt((individuate=IndvSRaw))),
      make_indiv_object_list(VM,IndvSRaw,IndvS1),
      %combine_objects(IndvS1,IndvS2),
      combine_same_globalpoints(IndvS1,IndvS),
      print_ss(indvS,ObjsB,IndvS),
      %list_to_set(IndvS1,IndvS),
      nop(print_info(IndvS)))).  


maybe_into_fti(ID_In,[ROptions],Grid_In,VM_In):- nonvar(ROptions),!,maybe_into_fti(ID_In,ROptions,Grid_In,VM_In). 
%maybe_into_fti(ID_In,ROptions,Grid_In,VM_In):- ROptions==complete,grid_vm(Grid_In,VM_In),!,gset(VM_In.id) = ID_In.
maybe_into_fti(ID_In,ROptions,Grid_In,VM_In):- into_fti(ID_In,ROptions,Grid_In,VM_In).


into_fti(TID,ROptions,GridIn0,VM):-
  fix_indivs_options(ROptions,Options),
  must_det_ll((
   ignore((ROptions \= Options,
     is_list(ROptions), sub_var(complete,ROptions), 
     (progress(blue,fix_indivs_options(ro=ROptions,r=Reserved,o=Options))))),
  statistics(cputime,X),Timeleft is X+30,
  %rtrace,
   %rtrace,
  (is_dict(GridIn0)-> (VM = GridIn0, GridIn = GridIn0.grid) ; GridIn0 = GridIn),
  into_grid(GridIn,Grid),
  globalpoints_include_bg(GridIn,Points),
  (var(TID)->grid_to_tid(Grid,TID);true),
  tid_to_id(TID,ID),
  grid_size(Grid,H,V),
 % rb_new(HM),duplicate_term(HM,Hashmap),
  max_min(H,V,MaxM,_),
  max_min(320,MaxM,Max,_),
  % term_to_oid(ID,GOID),

  must_grid_to_gid(Grid,GOID),
  progress(blue,fix_indivs_options(tid_gid,ID,GOID,ROptions->Options)),
  TID_GID=tid_gid(ID,GOID),
  check_tid_gid(TID_GID,Grid),

  set_example_num(ID),

  
  ignore(other_grid_size(Grid,OGX,OGY)),

  listify(ROptions,OOptions),
  Area is H*V,
 % progress(yellow,igo(ROptions,ID)=into_fti(H,V)),
  ArgVM = vm{
    ogx:OGX,ogy:OGY,
   % parent VM
   %training:_,
     %compare:_, 
   target_grid:_,  last_key:_,  
   % Options and TODO List (are actually equal things)
   lo_program:Options, options:OOptions, start_options:ROptions, %todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, objs_max_len:Max, objs_min_mass:_, objs_max_mass:_,
   % Grid and point representations
   grid:Grid, gid:GOID,
   area:Area,
   added_points:[],
   lo_points:Points,
   start_points:Points, % repaired:[],
   props:_{},
   option_repair_grid:  _, % Automatic,  
   option_pboxes:  _, % Automatic,  
     option_Background:  _, % Automatic,                         /* The background color of scenes. */
     option_CheckForGridsAndDividers: true    ,                  /* If we see things that look like grids/dividers, should we treat the specially, such as segmenting them into their own objects? */
     option_SubdivideInput: false,                               /* If {rowCount, columnCount} is passed in, we subdivide the input into a grid of objects and then try to find rules. e.g. 2dee498d */
     option_Segmentation:  none           ,                      /* Can be Columns or Rows to give a hint that objects seem to be segmented the same in input/output scene pairs wrt being in single-pixel columns or rows. e.g. 1e0a9b12 */
   %  option_ExampleIndex:  Missing[option_NotSpecifiedoption_], /* Given a list of example inputs/outputs, what example number is this scene? */
     option_FindOcclusions:  _, % Automatic,                     /* Whether we should consider possible occlusions when interpreting the scene. */
     option_FollowDiagonals:  _, % Automatic,                    /* Should diagonally adjacent pixels form a single object? */
     option_FormMultiColorCompositeObjects:  _, % Automatic,     /* Whether connected single-color objects should be iz combined to form multi-color composite objects. If set to Automatic, the OtherScene option will be used to help make more informed decisions. */
     option_InferPropertiesThatRequireFullObjectList:  true,     /* Rank and RankInverse properties require that we have the full object list. If False, we won't infer those properties. */
     %option_InputOrOutput:                                      /* Is this an input scene or an output scene? */
     option_NoMappings:  false,                                  /* If True, we will treat the inputObject as not mapping to any of the components of the output object. */
     option_NotableSubImages: _, % Automatic,                    /* The list of images which are considered notable sub-images. If we find objects that contain these as sub-images, we should consider splitting that object up so that the sub-media(image) is its own object. */
     option_OtherScene:  _,                                      /* A parse of the scene this scene corresponds to. For example, if `scene` is an input scene, then OtherScene would be the output scene, and vice versa. If provided, we can use OtherScene to resolve some ambiguities about whether to chunk objects into composite objects. An association of the form <|option_WithoutMultiColorCompositeObjects:  ..., option_WithMultiColorCompositeObjects:  ...|> should be passed. */
     option_SingleColorObjects: _, % Automatic,                  /* If the single color objects have already been determined, they can be passed in to save time. */
     option_SingleObject: _, % Automatic,                        /* Should all non-background pixels be treated as part of a single object, even if they are non-contiguous? */
     option_IncludeImageShapes: false,   /* Whether to include shapes of stype <Type:  Image, ...|>. */

  option_NoLinks:_,

%=%   
   changed:_,% solution:_,neededChanged
   neededChanged:_, repaired:_,
   full_grid:_,
   parent_vm:_,
   izmap:true,
   % Original copies of Grid and point representations
   start_grid:Grid, 
   ngrid:_,
   rule_dir: ROptions,
   
   % objects found in grid and object that are reserved to not be found
   objs:[],  robjs:Reserved, % objs_prev:[],
   % Notes and debug info
   type:grid, % notes:_, debug:_,
   % height width and lookup key for media(image)
   h:H, v:V, id:ID},
   %ignore(VM>:<ArgVM),
   (var(VM) -> ArgVM=VM ; transfer_missing(ArgVM,VM)),
   %set_vm(VM),
   %(var(VM) -> (fix_test_name(ID,TestID,_), make_training_hints(TestID,ArgVM,HintedVM), HintedVM = VM) ; true),
   %(luser_getval('$vm_pair',Shared)-> transfer_missing(Shared,VM) ; true),
    true)),
   %b_set_dict(objs,VM,[]),
   %set(VM.current_i) = VM
   progress(yellow,igo(ROptions,ID)=(H,V)),
   !.


into_grid_d(Grid,Grid_D):- most_d_colors(Grid,_CI,Grid_D),!.
%must_det_ll((subst001(Grid,black,wbg,TexturedGrid), most_d_colors(TexturedGrid,_CI,Grid_D))),

transfer_missing(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     my_maplist(nb_link_pairs_if_missing(VM),Pairs).
transfer_onto_dict(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     my_maplist(nb_link_pairs(VM),Pairs).
force_onto_dict(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     my_maplist(nb_link_pairs(VM),Pairs).

b_set_pairs(VM,K-V):- (nonvar(V),V\==[])->my_b_set_dict(K,VM,V);true.
nb_set_pairs(VM,K-V):- (nonvar(V),V\==[])->nb_set_dict(K,VM,V);true.
nb_link_pairs(VM,K-V):- (nonvar(V),V\==[])->nb_link_dict(K,VM,V);true.
nb_link_pairs_if_missing(VM,K-NewV):- V = VM.K, ((var(V),\+ attvar(V))->nb_link_dict(K,VM,NewV);true).



% =====================================================================
is_fti_step(complete_occluded).
% =====================================================================
complete_occluded(_VM):- dmsg(todo(complete_occluded)).


% =====================================================================
is_fti_step(complete_broken_lines).
% =====================================================================
complete_broken_lines(VM):- complete_broken_lines(VM.lo_points,set(VM.lo_points)).
complete_broken_lines(Ps,Done):-    
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
  \+ member(C-P3,G2), 
  select(M-P3,G2,G3), M\==C, 
   select(C-P4,G3,G4), 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P3|Ps],Done).
complete_broken_lines(Ps,Done):-    
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
   select(C-P3,G2,G3),
  \+ member(C-P4,G2), select(N-P4,G3,G4), N\==C, 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P4|Ps],Done).

complete_broken_lines(Ps,Done):-
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
  \+ member(C-P3,G2), select(M-P3,G2,G3), M\==C, 
  \+ member(C-P4,G2), select(N-P4,G3,G4), N\==C, 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P3,C-P4|Ps],Done).

complete_broken_lines(Done,Done).
row_of_5(P1,P2,P3,P4,P5):- 
  n_s_e_w(Dir),
  is_adjacent_point(P1,Dir,P2),
  is_adjacent_point(P2,Dir,P3),
  is_adjacent_point(P3,Dir,P4),
  is_adjacent_point(P4,Dir,P5).



%unraw_inds(_VM,I,O):- I=O,!.
unraw_inds(_VM,I,O):- I=O,!.

unraw_inds(VM,IndvS,IndvOO):-   
  largest_first(mass,IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(VM,_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).


run_fti(VM):- 
  run_fti(VM,VM.lo_program).

run_fti(_,[]):- !.
run_fti(_,_):- arc_option(no_individuator), !.
run_fti(VM,NotAList):- \+ is_list( NotAList),!, must_det_ll(run_fti(VM,[NotAList])).
run_fti(VM,[Step|Program]):- is_list(Step),flatten([Step|Program],StepProgram),!,must_det_ll(run_fti(VM,StepProgram)).
run_fti(_,[Done|TODO]):- my_assertion(nonvar(Done)), ( \+ done \= Done ), !, u_dmsg(done_run_fti([Done|TODO])),set_vm(lo_program,[done]),!.
run_fti(VM,[if_done|TODO]):- !, (VM.lo_points==[] -> (u_dmsg(if_done),set_vm(lo_program,[if_done])) ; run_fti(VM,TODO)).
run_fti(VM,[end_of_macro|TODO]):- !, must_det_ll(run_fti(VM,TODO)).
run_fti(VM,[recalc_sizes|TODO]):- !, must_det_ll(run_fti(VM,TODO)).
%run_fti(VM,X):- !, fti(VM,X),!.
%run_fti(VM,[H|T]):- !, must_det_ll((fti(VM,H),run_fti(VM,T))),!.
run_fti(VM,[F|TODO]):- 
  %must_det_ll(fti(VM,[F|TODO])),!, 
  show_vm_changes(VM,F, must_det_ll(fti(VM,[F|TODO]))),!,
  must_det_ll(get_kov(lo_program,VM,Code)),!,    
  must_det_ll(([F|TODO]=@=Code -> 
    (% progress(blue,fti=[F|TODO]), progress(blue,code=Code), 
     set(VM.lo_program) = TODO, 
       run_fti(VM,TODO)) ; run_fti(VM))),!.


print_vm_debug_objs(_VM):- !.
print_vm_debug_objs(VM):- 
  Objs = VM.objs,  
  length(Objs,Count),
  as_debug(8,(mass(Objs,Mass), length(VM.lo_points,PC),
      maybe_four_terse(VM.lo_program,Four),
      progress(t([obj/obj_mass=(Count/Mass),unprocessed_points=PC,start_options=VM.start_options,fsi=Four])))).


maybe_four_terse(L,F=..N):- length(L,N),N>4,!,length(F,4),append(F,_,L),!.
maybe_four_terse(L,L):-!.
%fti(VM,_):- VM.lo_points=[], !.
fti(_,[]):- !.
fti(VM,NotAList):- \+ is_list( NotAList),!, fti(VM,[NotAList]).
fti(VM,[Step|Program]):- is_list(Step),append(Step,Program,StepProgram),!,fti(VM,StepProgram).
fti(VM,[end_of_macro|TODO]):- !, fti(VM,TODO).

fti(_,[Done|TODO]):-  ( \+ done \= Done ), !, u_dmsg(done_fti([Done|TODO])),!.
%fti(VM,_):- VM.lo_points==[], !.
fti(VM,_):-
  Objs = VM.objs,  
  length(Objs,Count),
  ((member(progress,VM.options); catch_log(Count > VM.objs_max_len); (statistics(cputime,X), catch_log(X > (VM.timeleft))))) -> 
   print_vm_debug_objs(VM),fail.

fti(VM,[+(AddOptions)|TODO]):- !,
  must_det_ll((
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.lo_program)))).


fti(VM,Prog):- fail, length(Prog,Len),Len>1, exceeded_objs_max_len(VM),!,
  set(VM.objs_max_len) is VM.objs_max_len + 2000,
  set(VM.lo_program)= [do_ending],!.

fti(VM,[IsToBeRewritten|Rest]):-
    %atom(IsToBeRewritten), 
    nonvar(IsToBeRewritten),
    individuation_macros(IsToBeRewritten,Expansion),!,
    listify(Expansion,ExpansionL),
    my_append(ExpansionL,Rest,set(VM.lo_program)).

fti(VM,_):- fail, member(recalc_sizes,VM.options), once(recalc_sizes(VM)), fail.
fti(VM,[recalc_sizes|TODO]):- nop(recalc_sizes(VM)),!, fti(VM,TODO).
fti(VM,[recalc_sizes,After|TODO]):- After == recalc_sizes,!, fti(VM,[recalc_sizes|TODO]).
fti(VM,[recalc_sizes,After|TODO]):- 
  (recalc_sizes(VM,[After|TODO])
     -> true; (set(VM.lo_program)= [After,recalc_sizes|TODO])),!.

fti(VM,[max_learn_objects(Routine,Max)|set(VM.lo_program)]):- fail,
   set(VM.objs_max_len) = Max,
   set(VM.options)= [max_learn_objects(Routine,Max)|VM.options].

fti(VM,[Routine|set(VM.lo_program)]):-  fail,
   member(max_learn_objects(Routine,Max),VM.options),
   length(VM.objs,Count),
   Count>Max,!,fail.

fti(VM,[-(DelOptions)|TODO]):-
  listify(DelOptions,OList),
  my_partition(option_matches(OList),VM.options,_,set(VM.options)),
  my_partition(option_matches(OList),TODO,_,set(VM.lo_program)).

option_matches(List,Arg):- member(E,List),E==Arg.

fti(VM,[(OptionsL)|TODO]):- fail,
  is_list(OptionsL), \+ is_group(OptionsL), \+ is_grid(OptionsL),!,
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.lo_program)).

fti(VM,[options(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.lo_program)).

fti(VM,[macrof(AddTodo)|set(VM.lo_program)]):-
  listify(AddTodo,TodoLst),
  fti(VM,TodoLst).

fti(VM,[macro(AddTodo)|TODO]):-
  listify(AddTodo,TodoL),
  my_append([progress|TodoL],TODO,set(VM.lo_program)).

/*
%fti(VM,Prog):- length(Prog,Len),Len>2, exceeded_objs_max_len(VM),!,set(VM.lo_program)= [do_ending],!,length(VM.objs,Count),set(VM.objs_max_len) is Count+3.
fti(VM,[Step|Program]):- functor(Step,F,_), ping_indiv_grid(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, !, my_submenu_call(call(Step, VM.grid)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, !, my_submenu_call(call(Step,VM)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, Step=..[F|ARGS], !, my_submenu_call(apply(F,[VM|ARGS])).
fti(VM,[Step|Program]):- set(VM.lo_program) = Program, one_fti_step(Step), !, my_submenu_call(one_fti(VM,Step)),!.
*/

fti(VM,[when(G,D)|TODO]):- ((call_expanded(VM,G),!,progress(using_when(G,D)))->R=D;R=call(nop(progress(skipped(G,D))))),
  set(VM.lo_program) = [R|TODO].

fti(VM,[call(G)|TODO]):-   set(VM.lo_program) = TODO, !, my_subfti_call(call(G),VM,call_expanded(VM,G)).

%fti(VM,Prog):- length(Prog,Len),Len>2, exceeded_objs_max_len(VM),!,set(VM.lo_program)= [do_ending],!,length(VM.objs,Count),set(VM.objs_max_len) is Count+3.
fti(VM,[Step|Program]):- functor(Step,F,_), ping_indiv_grid(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, !, my_subfti_call(Step,VM,call(Step, VM.grid)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step_f(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, !, my_subfti_call(Step,VM,call(Step,VM)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), \+ warn_missing_arity(Step,1), set(VM.lo_program) = Program, Step=..[F|ARGS], !, my_subfti_call(Step,VM,apply(F,[VM|ARGS])).
fti(VM,[Step|Program]):- set(VM.lo_program) = Program, one_fti_step(Step), !, ignore(one_fti(VM,Step)).
   %my_subfti_call(Step,VM,one_fti(VM,Step)),!.

%my_subfti_call(_Step,VM,Goal):- !, _OldObjs = VM.objs, !, my_submenu_call(Goal).



is_fti_step_f(F):- is_fti_step(Step),functor(Step,F,_).

my_subfti_call(Step,VM,Goal):-
  my_subfti_vm_call(VM,my_submenu_call(Goal),GoodObjs,Added),
  add_birth_if_missing(Step,Added,FixAdded),
  append(GoodObjs,FixAdded,NewObjs),
  set(VM.objs)=NewObjs.


my_subfti_vm_call(VM,Goal,GoodObjs,Added):-  
  OldObjs = VM.objs, 
  my_submenu_call(Goal),
  NewObjs = VM.objs,!,
  pred_intersection(is_same_oids,
      OldObjs,NewObjs,
     _OldUpdated,GoodObjs,
     _Deleted,Added).


% =====================================================================
is_fti_step(larger_than).
% =====================================================================
larger_than(Mass,TODO,VM):- 
  my_subfti_vm_call(VM,run_fti(VM,TODO),GoodObjs,Added),
  include(not_less_mass(Mass),Added,FixAdded),
  append(GoodObjs,FixAdded,NewObjs),
  set(VM.objs)=NewObjs.
  


is_same_oids(O1,O2):- obj_to_oid(O1,OID1),obj_to_oid(O2,OID2),OID1=@=OID2.


%fti(VM,[Step|Program]):- callable_arity(Step,0), !, set(VM.lo_program) = Program, my_submenu_call(call(Step)).

fti(VM,_):- 
  Objs = VM.objs,
  length(Objs,Count),
  (fail;(member(progress,VM.options); Count > VM.objs_max_len; (statistics(cputime,X), X > (VM.timeleft)))) -> 
   print_vm_debug_objs(VM),fail.


exceeded_objs_max_len(VM):- 
  Objs = VM.objs,
  length(Objs,Count),
  Count > VM.objs_max_len,
  u_dmsg(warn(Count > VM.objs_max_len)),
  %dumpST(2),
  u_dmsg(throw(exeeded_object_limit(Count > VM.objs_max_len))),!.
  

fti(VM,[F|TODO]):- once(fix_indivs_options([F|TODO],NEWTODO)),[F|TODO]\==NEWTODO,!,fti(VM,NEWTODO).
  

fti(VM,[F|TODO]):- F=='',!,
   u_dmsg(fti_miss(F,TODO)),
   bt,
   u_dmsg(fti_miss(F,TODO)),
   
   set(VM.lo_program)= TODO.

%:- listing(fti/2).
% fti(_VM,[F|_]):- u_dmsg(F),!. % atrace,fail.


one_fti_step(Name):- is_thing_or_connection(Name).
one_fti_step(whole).
one_fti_step(Name):- clause(one_fti(_VM,Step),_),nonvar(Step),Name=Step.

i_step(Name):- no_repeats(Name,(i_step0(FName),functor(FName,Name,_))).
i_step0(Name):- clause(fti(_VM,[Step|_]),_),nonvar(Step),Name=Step.
i_step0(Name):- one_fti_step(Name).
i_step0(Name):- is_fti_step(Name).
i_step0(Name):- is_fti_stepr(Name).
i_step0(Name):- ping_indiv_grid(Name).
i_step0(Name):- individuation_macros(Name,_).


warn_missing_arity(Step,N):- missing_arity(Step,N) -> u_dmsg(warning(missing_arity(Step,N))).

is_fti_stepr(_):-fail.

% =====================================================================
is_fti_step(indiv_grid_pings).
% =====================================================================
indiv_grid_pings(Grid):- is_grid(Grid),!,forall(ping_indiv_grid(P1),ignore(catch(call(P1,Grid),E, ((E == '$aborted')->throw(E);fail)))),!.
indiv_grid_pings(VM):- get_kov(grid,VM,Grid),!, indiv_grid_pings(Grid).
indiv_grid_pings(VM):- io_side_effects, into_grid(VM,G),!,indiv_grid_pings(G).



% =====================================================================
is_fti_step(release_objs_lighter).
% =====================================================================
release_objs_lighter(Two,VM):-
 ignore(( fail,
  my_partition(less_mass(Two),VM.objs,Smaller,set(VM.objs)),
  my_maplist(globalpoints,Smaller,Points),
  append_sets([VM.lo_points|Points],set(VM.lo_points)))).

less_mass(Mass,Obj):- mass(Obj,M),M<Mass.
not_less_mass(Mass,Obj):- mass(Obj,M), \+ M<Mass.


mergable_objects(VM,O1,O2):- 
  O1\==O2,
  globalpoints(O1,Ps1),
  globalpoints(O2,Ps2),!,
  mergable_objects_direct(O1,O2,Ps1,Ps2),
  select(O2,VM.objs,Rest),
  \+ (member(O3,Rest), O3\==O1, 
      globalpoints(O3,Ps3),
      mergable_objects_direct(O1,O3,Ps1,Ps3)),!.

mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,dg_line(_)), iz(O2,dg_line(_)), dir_mergeable_list(PsA,PsB,[ne,se,sw,nw,n,s,e,w],[]),!.
mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,hv_line(_)), iz(O2,dg_line(_)), dir_mergeable_list(PsA,PsB,[n,s,e,w],[ne,se,sw,nw]),!.
mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,dg_line(_)), iz(O2,hv_line(_)), dir_mergeable_list(PsA,PsB,[ne,se,sw,nw],[n,s,e,w]),!.
%mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,hv_line(U)), iz(O2,hv_line(D)), u_d(U,D), adjacent_objs(PsA,PsB).
mergable_objects_direct(O1,O2,PsA,PsB):- \+ iz(O1,dot), mass(O2,N),N==1,dir_mergeable_list(PsA,PsB,[n,s,e,w,ne,se,sw,nw],[]),!.
mergable_objects_direct(_O1,_O2,PsA,PsB):- double_touch(PsA,PsB).
mergable_objects_direct(_O1,_O2,PsA,PsB):- double_touch(PsB,PsA).
double_touch(PsA,PsB):- 
  member(C-PB1,PsB),
  is_adjacent_point(PB1,_,PA1),select(C-PA1,PsA,BestA1),
  is_adjacent_point(PB1,_,PA2),member(C-PA2,BestA1),!.

adjacent_objs(PsA,PsB):- 
  member(C-PB1,PsB),
  is_adjacent_point(PB1,_,PA2),member(C-PA2,PsA),!.

dir_mergeable_list(Ps1,Ps2,DirsOK,DirsNotOK):- 
  \+ \+ ((mergable_dir(Ps1,Ps2,OkDir),member(OkDir,DirsOK))),
  \+ ((member(BadDir,DirsNotOK),mergable_dir(Ps1,Ps2,BadDir))).

mergable_dir(Ps1,Ps2,Dir):- member(C-P1,Ps1), member(C-P2,Ps2), is_adjacent_point(P1,Dir,P2),!.

% one_fti(VM,List):- is_list(List),!,fti(VM,List).
one_fti(VM,'all_rows'):-
 Grid = VM.start_grid,
 (\+ ground(Grid) -> true ;
  maplist_n(1,row_to_indiv(VM,'all_rows'), Grid)).

one_fti(VM,'some_rows'):-
 Grid = VM.start_grid,
 (\+ ground(Grid) -> true ;
  forall( solid_row_number(Grid,_Color,N,C2), row_to_indiv(VM,'some_rows',N,C2))).

solid_row_number(Grid,C,N,Row):- 
    grid_size(Grid,_H,V),
    %make_list(black,H,BlankRow),
    nth1(N,Grid,Row),    
    my_maplist(=(C),Row),
    \+ is_bg_color(C),
    \+ \+ (
    ( N == 1 -> \+ (nth1(2,Grid,Row2),my_maplist(=(C),Row2)) ; 
    ( N == V -> ( N1 is N - 1, \+ (nth1(N1,Grid,Row1),my_maplist(=(C),Row1))) ;
    ( N1 is N - 1, \+ (nth1(N1,Grid,Row1),my_maplist(=(C),Row1)) , N2 is N + 1, \+ (nth1(N2,Grid,Row2),my_maplist(=(C),Row2)))))),    
    \+ (delete(Grid,Row,Rest), member(Row2, Rest), append(_,[C,C|_],Row2)).

row_to_indiv(VM,Birth,N,Row):-
  sameR([Row],Rot90),
  localpoints_include_bg(Rot90,LPoints),
  offset_points(1,N,LPoints,GPoints),
  %grid_to_individual([Row],Obj0),  
  % a column is a row that was prematurely rotated 270 degrees
  make_indiv_object(VM,[iz(media(image)), iz(stype(Birth)), iz(grouped(Birth)),
     loc2D(1,N),
     vis2D(VM.h,1),
     giz(grid_sz(VM.h,VM.v))],GPoints,Obj),
  assumeAdded(VM,Obj).

one_fti(VM,'all_columns'):-
 Grid = VM.start_grid,
 (\+ ground(Grid) -> true ;
  rot90(Grid,Grid90),
  maplist_n(1,column_to_indiv(VM,'all_columns'), Grid90)).

%Emani 501 N grahm %200  Feb 3rd 3pm


one_fti(VM,'some_columns'):-
 Grid = VM.start_grid,
 (\+ ground(Grid) -> true ;
  rot90(Grid,Grid90),
  forall( solid_row_number(Grid90,_Color,N,C2), column_to_indiv(VM,'some_columns',N,C2))).

column_to_indiv(VM,Birth,N,Row):-  
  rot270([Row],Rot270),
  localpoints_include_bg(Rot270,LPoints),
  offset_points(N,1,LPoints,GPoints),
  %grid_to_individual([Row],Obj0),  
  % a column is a row that was prematurely rotated 270 degrees
  make_indiv_object(VM,[/*iz(hv_line(v)),rotated(sameR),*/
    iz(stype(Birth)),iz(media(image)),iz(grouped(Birth)),loc2D(N,1),rot2D(rot180),vis2D(1,VM.v),giz(grid_sz(VM.h,VM.v))],GPoints,Obj),
  assumeAdded(VM,Obj).


% =====================================================================
is_fti_step(mono_shapes).
% =====================================================================
mono_shapes(Shape,VM):-
 with_mapgrid(mono_shaped,'_mono_shaped',Shape,VM).

into_monogrid(Orig,NewGrid):-mapgrid(mono_shaped,Orig,NewGrid).

mono_shaped(G,O):- is_grid(G),!,mapgrid(mono_shaped,G,O),!.
mono_shaped(Cell,NewCell):- mono_shaped(fg,wbg,Cell,NewCell).
mono_shaped(_FG,_BG,Cell,bg):- plain_var(Cell),!.
mono_shaped(_FG,_BG,Cell,bg):- Cell==bg,!.
mono_shaped(_FG, BG,Cell,NewCell):- is_bg_color(Cell),!,nop(decl_many_bg_colors(NewCell)),NewCell=BG.
mono_shaped( FG,_BG,Cell,NewCell):- is_fg_color(Cell),!,nop(decl_many_fg_colors(_NewCell)),NewCell=FG.
mono_shaped(_,_,Cell,Cell).

recolor_object(VM,Recolors,Old,New):- 
  %globalpoints_include_bg(Grid,Recolors),
  globalpoints_include_bg(Old,GPoints),
  my_maplist(recolor_point(Recolors),GPoints,NewGPoints),  
  must_det_ll(rebuild_from_globalpoints(VM,Old,NewGPoints,New)),!.

recolor_point(Recolors,_-Point,C-Point):- 
  must_det_l((hv_point(H,V,Point), hv_c_value(Recolors,C,H,V))).

% =====================================================================
is_fti_step(fg_shapes).
% =====================================================================
fg_shaped( BGCs,Cell,wbg):- member(C,BGCs),Cell==C,!.
fg_shaped(_BGCs,Cell,fg):- is_fg_color(Cell),!.
fg_shaped(_BGCs,Cell,Cell):- attvar(Cell),!.
fg_shaped(_BGCs,Cell,bg):- plain_var(Cell),!.
fg_shaped(_BGCs,_Cell,black).
%fg_shaped(Cell,NewCell):- is_fg_color(Cell),!,decl_many_fg_colors(NewCell),NewCell=Cell.

fg_shapes(Shape,VM):-
  Grid = VM.grid,
  colors_across(Grid,Silvers),
  get_black(Black),
  BGCs = [Black,wbg,bg|Silvers],
  with_mapgrid(fg_shaped(BGCs),'_fg_shaped',Shape,VM).

% =====================================================================
is_fti_step(with_mapgrid).
% =====================================================================

plain_var_as(C,Var,C):- plain_var(Var),!.
plain_var_as(_,W,W).

color_as_plain_var(_,W,W):- plain_var(W),!.
color_as_plain_var(C,W,_):- \+ C\=W, !.
color_as_plain_var(_,W,W).

bgc_as_color(_,W,W):- plain_var(W),!.
bgc_as_color(C,BG,W):- is_bg_color(BG),!,cvt_to_color(C,W).
bgc_as_color(_,W,W).

fgc_as_color(_,W,W):- plain_var(W),!.
fgc_as_color(C,BG,W):- is_fg_color(BG),!,cvt_to_color(C,W).
fgc_as_color(_,W,W).

only_keep(_,W,W):- plain_var(W),!.
only_keep(C,W,W):- \+ C\=W.
only_keep(_,_,_).

cvt_to_color(PV,_):- PV == plain_var,!.
cvt_to_color(C,Cpy):- attvar(C),copy_term(C,Cpy).
cvt_to_color(PV,PV).

with_mapgrid(MapGrids,Sub_fg_shaped,Shape,VM):-
 ignore((
 VMGID = VM.gid,
   \+ atom_contains(VMGID,Sub_fg_shaped),
  Grid = VM.grid,
  mapgrid(MapGrids,Grid,NewGrid),
  other_grid(VM.start_grid,Other),
  atomic_list_concat([VM.gid,Sub_fg_shaped],GOID), assert_grid_gid(NewGrid,GOID),
  get_vm(VMS), 
  with_other_grid(Other,individuate2(_,Shape,GOID,NewGrid,FoundObjs)),
  set_vm(VMS),
  globalpoints_include_bg(VM.start_grid,Recolors),
  my_maplist(recolor_object(VM,Recolors),FoundObjs,ReColored),
  learn_hybrid_shape(ReColored),
  remLOPoints(VM,ReColored),
  addObjects(VM,ReColored))),!.



colors_across(Grid,Silvers):-
  findall(C,color2_across(Grid,C),Silvers).
color2_across(Grid,C):- rot90(Grid,Grid90), color1_across(Grid90,C).
color2_across(Grid,C):- color1_across(Grid,C).
color1_across(Grid,C):- enum_fg_colors(C),two_rows(Grid,C,_,_).

two_rows(Grid,S1,R1,R2):-
  nth1(R1,Grid,[S1|Row1]),nonvar(S1),
  my_maplist(==(S1),Row1),
  nth1(R2,Grid,Row2),
  R2>R1+1,
  [S1|Row1]==Row2.
  

/* Metadata about properties.
   RuleConditionQuality:
       A quality score of how much we should favor rule conditions that use a given property.
       By default, we check how far down the property is in the $properties list, with the
       top of the list being equivalent to a score of 1, and the bottom of the list being
       a score of 0, but sometimes we want to customize this score, such as to compensate
       for the ARCExpressionComplexity score of a property's values unfairly penalyzing
       conditions with that property. */
objectProperties((
    /* As of August 3 2022 we'll order "Colors" above "Image" so that when we find object
       references, we'll list Colors references above Image references, as they feel more
       general / better for forming rules. e.g. 05f2a901 */
    "Colors" = data((
        "SyntacticType" = 'Repeated'("Color"),
        "SemanticType" = "Color"
    )),
    "Color" = data((
        "SyntacticType" = "Color",
        "SemanticType" = "Color"
    )),
    "Image" = data((
        "SyntacticType" = "Image",
        "SemanticType" = "Image",
        /* We'll fudge this higher than we otherwise would since the ARCExpressionComplexity
           of values for this property tend to penalize these conditions too much. */
        "RuleConditionQuality" = 1.2
    )),
    "MonochromeImage" = data((
        "SyntacticType" = "MonochromeImage",
        "SemanticType" = "Image",
        /* We'll fudge this higher than we otherwise would since the ARCExpressionComplexity
           of values for this property tend to penalize these conditions too much. */
        "RuleConditionQuality" = 1.2
    )),
    /* Will exclude for now. */
    /*"PixelPositions"*/
    "Shape" = data((
        "SyntacticType" = "Shape",
        "SemanticType" = "Shape",
        /* We'll fudge this higher than we otherwise would since the ARCExpressionComplexity
           of values for this property tend to penalize these conditions too much. */
        "RuleConditionQuality" = 1.2
    )),
    "Shapes" = data((
        "SyntacticType" = 'Repeated'("Shape"),
        "SemanticType" = "Shape",
        /* We'll fudge this higher than we otherwise would since the ARCExpressionComplexity
           of values for this property tend to penalize these conditions too much. */
        "RuleConditionQuality" = 1.195,
        /* In ARCFindPropertyToInferValues, don't try to break the list down into positional
           sub-elements. */
        "ClassList" = true
    )),
    "Angle" = data((
        "SyntacticType" = "Angle",
        "SemanticType" = "Angle"
    )),
    /* We'll put properties like Height above properties like Y, since properties like
       Height and HeightRank seem better than properties like Y for producing rules. */
    "Width" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "SizeDimensionValue"
    )),
    "Height" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "SizeDimensionValue"
    )),
    "Length" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "SizeDimensionValue"
    )),
    "Position" = data((
        "SyntacticType" = "Position",
        "SemanticType" = "Position"
    )),
    "VerticalLineSymmetry" = data((
        "SyntacticType" = "Boolean",
        "SemanticType" = "Symmetry",
        "RuleConditionQuality" = 0.8
    )),
    "HorizontalLineSymmetry" = data((
        "SyntacticType" = "Boolean",
        "SemanticType" = "Symmetry",
        "RuleConditionQuality" = 0.8
    )),
    "VerticalAndHorizontalLineSymmetry" = data((
        "SyntacticType" = "Boolean",
        "SemanticType" = "Symmetry",
        "RuleConditionQuality" = 0.8
    )),
    "Y" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "X" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "YInverse" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "XInverse" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "Y2" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "X2" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "Y2Inverse" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "X2Inverse" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "YMiddle" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "XMiddle" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "XRelative" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "YRelative" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    /*"YHalf" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),
    "XHalf" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue",
        "RuleConditionQuality" = 0.5
    )),*/
    "ZOrder" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "PositionDimensionValue"
    )),
    "PrimarySizeDimension" = data((
        "SyntacticType" = "SizeDimension",
        "SemanticType" = "SizeDimension"
    )),
    "AspectRatio" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "SizeDimensionRatio"
    )),
    "HollowCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count"
    )),
    "Area" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Area"
    )),
    "FilledArea" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Area"
    )),
    "FilledProportion" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "AreaProportion"
    )),
    "SurfacePixelCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count"
    )),
    /* The number of pixels in the scene with this color. */
    "ColorUseCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count"
    )),
    "PixelPositions" = data((
        "SyntacticType" = "Region",
        "SemanticType" = "Region"
    )),
    /* Set in a rule condition when some or all of the matched objects need to first
       be formed from a group of non-contiguous objects in the scene. Not set on input
       objects. */
    "Group" = data((
        "RuleConditionQuality" = 0.5
    )),
    "ColorCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count",
        /* Otherwise it will lose out to "Colors" for "6e02f1e3". */
        "RuleConditionQuality" = 0.9
    )),
    "MostUsedColor" = data((
        "SyntacticType" = "Color",
        "SemanticType" = "Color",
        /* Adopting this from "ColorCount", not sure if it's needing to be high like this. */
        "RuleConditionQuality" = 0.9
    )),
    "SecondMostUsedColor" = data((
        "SyntacticType" = "Color",
        "SemanticType" = "Color",
        /* Adopting this from "ColorCount", not sure if it's needing to be high like this. */
        "RuleConditionQuality" = 0.9
    )),
    "ImageUseCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count",
        "RuleConditionQuality" = 0.5
    )),
    "ShapeUseCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count",
        "RuleConditionQuality" = 0.5
    )),
    "GeneralShapeUseCount" = data((
        "SyntacticType" = "Integer",
        "SemanticType" = "Count",
        "RuleConditionQuality" = 0.5
    )),
    "GridPosition" = data((
        "SyntacticType" = "GridPosition",
        "SemanticType" = "Position"
    ))
)).

sub_self(NewGrid,NewOther,TODO,VM,FoundObjs):-  
 print_grid(pp(doing(TODO)),NewGrid),
 must_det_ll((
 globalpoints(NewGrid,Points),
 duplicate_term(VM,SavedVM),
 set(VM.lo_points) = Points,
 set(VM.start_points) = Points,
 set(VM.grid)= NewGrid, 
 set(VM.target_grid)= NewOther,
 set(VM.start_grid)= NewGrid,
 %itrace,
 run_fti(VM,TODO),
 %itrace,
 FoundObjs = VM.objs,
 LOPoints = VM.lo_points,
 force_onto_dict(SavedVM,VM),
 set(VM.lo_points)= LOPoints, 
 set(VM.objs)= FoundObjs,
 %set(VM.grid)= PrevGrid,
 %set(VM.target_grid)= Other,
 remLOPoints(VM,FoundObjs),
 addObjects(VM,FoundObjs))),!.

/*
sub_self(NewGrid,NewOther,TODO,VM,FoundObjs):- !,
 ttyflush,
 must_det_ll((
 % if_thread_main(cls),
 print_ss(sub_self(TODO),NewGrid,NewOther),
% print_grid(sub_self(TODO),NewGrid),
 with_other_grid(NewOther,
   (/*itrace,*/do_individuate(_,TODO,NewGrid,FoundObjs))),

 print_grid(sub_self(foundObjs),FoundObjs),
 addObjects(VM,FoundObjs),
 %itrace,
 remLOPoints(VM,FoundObjs),
 %remGridPoints(VM,FoundObjs),
 addObjects(VM,FoundObjs))),!.

*/
% =====================================================================
is_fti_step(remove_background_only_object).
% =====================================================================
remove_background_only_object(VM):-
  Objs = VM.objs,
  ignore((
  is_group(Objs),length(Objs,Len),Len>1,
  must_det_ll((
  remove_background_only_object(Objs,NewObjs,VM),
  setObjects(NewObjs,VM))))),!.

setObjects(NewObjs,VM):-
    VM.objs == NewObjs -> true ; gset(VM.objs)=NewObjs.

remove_background_only_object(Objs,NewObjs):-
  must_det_ll(grid_size(Objs,H,V)),remove_background_only_object(H,V,Objs,NewObjs).
remove_background_only_object(Objs,NewObjs,VM):-
  remove_background_only_object(VM.h,VM.v,Objs,NewObjs).

remove_background_only_object(H,V,Objs,NewObjs):- fail,
   select(O,Objs,Mid),
   contains_numberedvars(O),
   pp(O),
   fail,
   !,remove_background_only_object(H,V,Mid,NewObjs).

remove_background_only_object(H,V,Objs,NewObjs):-
    select(O,Objs,Mid),
    area(O,Area),Area>3,
    vis2D(O,OH,OV), abs(H - OH)<3, abs(V - OV)<3,
    has_prop(cc(fg,0),O),
    %has_prop(cc(plain_var,0),O),!,
    print_ss(remove_background_only_object,[O],NewObjs),
    !,remove_background_only_object(H,V,Mid,NewObjs).
remove_background_only_object(_,_,Objs,Objs):-!.

% =====================================================================
is_fti_step(i_subtract_objs).
% =====================================================================
i_subtract_objs(VM):- fail,
  Grid = VM.grid,
  localpoints(Grid,Ps),include(is_fgp,Ps,FGPs),
  FGPs\==[],
  length(FGPs,PsL),PsL<25,!,
  %zero_to_black(VM),
  run_fti(VM,[pbox_vm_special_sizes,lo_dots]).

i_subtract_objs(VM):- VM.lo_points==[],!,
  writeg(VM.grid).
i_subtract_objs(VM):-
  %zero_to_black(VM),
  points_to_grid(VM.h,VM.v,VM.lo_points,Grid),
  print_grid(i_subtract_objs(points),Grid),
  print_grid(i_subtract_objs(grid),VM.grid),
  run_fti(VM,[find_hybrid_shapes,colormass,pbox_vm_special_sizes,nsew,lo_dots]).
  
% =====================================================================
is_fti_step(fg_subtractions).
% =====================================================================
fg_subtractiond(G,M,O):- is_grid(G),!,mapgrid(fg_subtractiond,G,M,O),!.
fg_subtractiond(This,Minus,_):- This =@= Minus,!.  %fg_subtractiond(This,Minus,_):- \+ This \= Minus,!.
fg_subtractiond(This,_,This).

fg_subtractions(_TODO,_VM):- cant_use_intersections,!.
fg_subtractions(TODO,VM):-
 copy_term(VM.start_grid,Grid),
 other_grid(Grid,Other), is_grid(Other),
 grid_size(Other,H,V),VM.h==H,VM.v==V,
 mapgrid(fg_subtractiond,Grid,Other,NewGrid),
 mapgrid(plain_var_to(black),NewGrid),
 ignore((mass(NewGrid,NM), NM > 0, % mass(Grid,GM), NM\==GM,% ,!, mass(Other,OM),
 sub_self(NewGrid,Other,TODO,VM,FoundObjs),
 addObjects(VM,FoundObjs))).

% =====================================================================
is_fti_step(i_intersect).
% =====================================================================
i_intersect(VM):- fail,
  %zero_to_black(VM),
  Grid = VM.grid,
  localpoints(Grid,Ps),include(is_fgp,Ps,FGPs),
  FGPs\==[],
  length(FGPs,PsL),PsL=<15,!,
  set(VM.lo_points)=FGPs,
  run_fti(VM,[lo_dots]).
  %black_to_zero(VM).
  %my_maplist(make_point_object(VM,[birth(lo_dots),iz(stype(dot)),iz(media(shaped))]),FGPs,_IndvList),
  %remGridPoints(VM,FGPs).

i_intersect(VM):- %generic_nsew_colormass find_hybrid_shapes,nsew,colormass,lo_dots
  fti(VM,[generic_nsew_colormass]).

% =====================================================================
is_fti_step(black_to_zero).
is_fti_step(zero_to_black).
% =====================================================================
black_to_zero(VM):- may_use_zero -> vm_subst(zero,black,VM) ; true.
zero_to_black(VM):- may_use_zero -> vm_subst(black,zero,VM) ; true.


vm_subst(Black,Zero,VM):-
  subst001(VM,Black,Zero,NewVM),
  set(VM.grid)=NewVM.grid,
  set(VM.start_grid)=NewVM.start_grid,
  set(VM.objs)=NewVM.objs,
  set(VM.start_points)=NewVM.start_points,
  set(VM.lo_points)=NewVM.lo_points,
  !.



% =====================================================================
is_fti_step(fg_intersections).
% =====================================================================
fg_intersectiond(This,That,Inter):- is_grid(This),!,mapgrid(fg_intersectiond,This,That,Inter),!.
fg_intersectiond(This,That,This):- This =@= That,!.
fg_intersectiond(Cell,_,bg):- \+ is_grid(Cell).
%fg_intersectiond(_,_,Black):-  get_black(Black).

fg_intersectiond_mono(This,That,Inter):- is_grid(This),!,mapgrid(fg_intersectiond_mono,This,That,Inter),!.
fg_intersectiond_mono(This,That,This):- is_fg_color(This),is_fg_color(That).
fg_intersectiond_mono(Cell,_,bg):- \+ is_grid(Cell).


cant_use_intersections:- 
   current_test_example(TestID,ExampleNum),
   \+ has_blank_alt_grid(TestID,ExampleNum),!,
   dmsg(warn(cant_use_intersections(TestID,ExampleNum))).

fg_intersections(_TODO,_VM):- cant_use_intersections,!.
fg_intersections(TODO,VM):-
 copy_term(VM.start_grid,Grid),
 other_grid(Grid,Other), is_grid(Other),
 grid_size(Other,H,V),VM.h==H,VM.v==V,
 mapgrid(fg_intersectiond,Grid,Other,NewGrid),
 mapgrid(plain_var_to(black),NewGrid),
 mass(NewGrid,NM), NM > 0, % mass(Grid,GM), NM\==GM,% ,!, mass(Other,OM),
 %itrace,
 sub_self(NewGrid,Other,TODO,VM,FoundObjs),
 addObjects(VM,FoundObjs).


% =====================================================================
is_fti_step(bigger_grid_contains_other).
% =====================================================================


bigger_grid_contains_other(_VM):- !.
bigger_grid_contains_other(_VM):- cant_use_intersections,!.
bigger_grid_contains_other(VM):- 
  ignore((w_section(bigger_grid_contains_other,
    once(bigger_grid_contains_other_i_o(VM.id,VM))))).

bigger_grid_contains_other_i_o(ID,VM):-
  ID = ((_TestID)>(_Trn+_N)*IO),
  if_t((IO\==out;IO\==in), pp(id=ID)),
  if_t((IO==out;IO==in),
    bigger_grid_contains_other_i_o_now(IO,VM)).

bigger_grid_contains_other_i_o_now(IO,VM):-
  flag(bigger_grid_contains_other_i_o,X,X), X==0,
  setup_call_cleanup(flag(bigger_grid_contains_other_i_o,_,X+1),
     (ignore(bigger_grid_contains_other_i_o_1(IO,VM)),
      ignore(bigger_grid_contains_other_i_o_2(IO,VM))),
     flag(bigger_grid_contains_other_i_o,_,X)).


bigger_grid_contains_other_i_o_1(out,VM):- 
 copy_term(VM.start_grid,Grid),
 other_grid(Grid,Other), is_grid(Other),
 current_pair_io(I,O), O=@=Grid,
  arc_common_property(containsAll(o-i)),
  individuate(generic_nsew_colormass,I,IObjs),
  mapgrid(cell_minus_cell,O,I,R),
  
  print_ss(wqs(containsAll(o-i)),R,IObjs),
  set(VM.grid)=R,
  grid_to_points(R,Points),
  set(VM.lo_points)=Points,
  addObjects(VM,IObjs),!.

bigger_grid_contains_other_i_o_2(in,VM):- 
 copy_term(VM.start_grid,Grid),
 other_grid(Grid,Other), is_grid(Other),
  current_pair_io(I,O), I=@=Grid,
  %arc_common_property(containsAll(i-o)),
  mapgrid(cell_minus_cell,I,O,R),
  individuate(generic_nsew_colormass,O,OObjs),
  print_ss(wqs(containsAll(i-o)),R,OObjs),
  set(VM.grid)=R,
  grid_to_points(R,Points),
  set(VM.lo_points)=Points,
  addObjects(VM,OObjs),!.
/*
bigger_grid_contains_other_i_o_2(IO,VM):- 
 copy_term(VM.start_grid,Grid),
 other_grid(Grid,Other), is_grid(Other),
 current_pair_io(I,O),
 I=@=Grid,  
   arc_common_property(containsAll(i-o)),
  must_det_ll((
   mapgrid(cell_minus_cell,I,O,R),
   print_grid(i_minus_o,R),%print_grid(o_minus_r,NewO),
   mapgrid(cell_minus_cell,I,R,NewI),
   print_grid(replace_in_vm,VM.grid,NewI),
   set(VM.grid)=NewI,
   %print_grid(i_minus_o,R),print_grid(i_minus_r,NewI),
   itrace,
   individuate(generic_nsew_colormass,R,Objs),
   print_ss(containsAll(o-i),R,Objs),
   my_maplist(learn_hybrid_shape,Objs))),!.

 */


/*

glean_shape_lib(VM):- 
 current_pair_io(I,O),
 ignore(glean_shape_lib_i(I,O,VM)),
 ignore(glean_shape_lib_o(I,O,VM)).

glean_shape_lib_o(I,O,VM):-
  arc_common_property(containsAll(o-i)),
  mapgrid(cell_minus_cell,O,I,R),
  mapgrid(cell_minus_cell,O,R,NewO),
  %print_grid(o_minus_i,R),print_grid(o_minus_r,NewO),
  individuate(generic_nsew_colormass,R,Objs),
  print_grid(containsAll(o-i),Objs),
  my_maplist(add_shape_lib(as_is),Objs).
glean_shape_lib_i(I,O,VM):-
 arc_common_property(containsAll(i-o)),
 mapgrid(cell_minus_cell,I,O,R),
 mapgrid(cell_minus_cell,I,R,NewI),
 %print_grid(i_minus_o,R),print_grid(i_minus_r,NewI),
 individuate(generic_nsew_colormass,R,Objs),
 print_grid(containsAll(i-o),Objs),
 my_maplist(add_shape_lib(as_is),Objs).




  forall(
 suggest_r1(R1),
(grid_call(R1,R,BRect),
 writeg(BRect),
 nl,nl,writeq(?-all_ogs(HintO,BRect,NewO)),writeln('.'),nl,
 all_ogs(R1,BRect,NewO,HintO),
 all_ogs(R1,BRect,I,HintI), 
 %(HintI\==[];HintO\==[]),
 pp(o_r(R1)=HintO),
 pp(i_r(R1)=HintI))),
 ttyflush,!,fail,
 ((sleep(30))))),!.
suggest_r1([subst_color(red,_)]).
suggest_r1([my_release_bg]).
suggest_r1([my_release_bg,subst_color(red,_)]).
*/

/*
 pp(all_ogs(i,R2)=HintI),!.


 individuate(generic_nsew_colormass,R,Objs),
 print_grid(o_minus_i_objs,Objs),
 %decolorize(Objs,MObjs),
 =(Objs,MObjs),
 
 pp(all_ogs(o,R1)=HintO),
 pp(all_ogs(i,R2)=HintI),!.
*/ 


% =====================================================================
is_fti_step(i_ogs_subobj).
% =====================================================================
i_ogs_subobj(VM):-
  Grid = VM.grid,
  localpoints(Grid,Ps),include(is_fgp,Ps,FGPs),length(FGPs,PsL),PsL<25,!.

i_ogs_subobj(VM):-
  run_fti(VM,[larger_than(3,nsew),colormass,lo_dots]).


% =====================================================================
is_fti_step(each_ogs_object).
% =====================================================================
each_ogs_object(TODO,VM):-
 must_det_ll((
  copy_term(VM.start_grid,StartGrid), 
  other_grid(StartGrid,Other), 
  is_grid(Other),
  all_ogs_123(StartGrid,Other,Hints))),
  %copy_term(VM.grid,This),  
  must_det_ll_maplist(use_each_ogs(VM,TODO,StartGrid,Other),Hints).

all_ogs_123(StartGrid,Other,Hints):- 
  all_ogs(StartGrid,Other,Hints1),
  all_ogs(Other,StartGrid,Hints2),
  append(Hints1,Hints2,Hints).



about_i_or_o(_-OorT,OorT):-!. about_i_or_o(TrimIO,OorT):- arg(1,TrimIO,IO),!,about_i_or_o(IO,OorT). about_i_or_o(_,t).

use_each_ogs(VM,TODO,This,Other,Hint):- ignore(use_each_ogs_i(VM,TODO,This,Other,Hint)).
use_each_ogs_i(VM,TODO,This,Other,Hint):- 
 nl_if_needed,pp(cyan,Hint),
 Hint = ogs(TrimIO,Named,Special,[loc2D(OH,OV),vis2D(SH,SV)]),
 about_i_or_o(TrimIO,OorT), ( (OorT == o) -> From = This ; From = Other ),
 obj_gpoints(From,OBJGRID,OH,OV,SH,SV,GOPoints),
 intersection(GOPoints, VM.start_points,Shared,MissingGoPoints,_ExtraPoints),
((MissingGoPoints==[], Shared\==[]) -> true;
 must_det_ll((
  print_grid(Hint,OBJGRID),
  make_indiv_object(VM,[iz(stype(ogs)),
     iz(tio(TrimIO)),iz(todo(TODO)),grid(OBJGRID),iz(named(Named)),iz(spec(Special))],GOPoints,Obj),    
  % remLOPoints(VM,Obj), remGridPoints(VM,Obj),
  assumeAdded(VM,Obj),
  grid_size(From,GH,GV),
  points_to_grid(GH,GV,GOPoints,NewGrid),
  sub_self(NewGrid,Other,TODO,VM,FoundObjs),
  addObjects(VM,FoundObjs)))).

% =====================================================================
is_fti_step(fg_abtractions).
% =====================================================================
%fg_abtractiond(Cell,Cell):- is_bg_color(Cell),!.
fg_abtractiond(This,Target,This):- get_black(Black), \+ Target \= Black,!.
fg_abtractiond(_,_,Black):- get_black(Black).
%fg_abtractiond(Cell,NewCell):- is_fg_color(Cell),!,decl_many_fg_colors(NewCell),NewCell=Cell.

fg_abtractions(Subtraction,VM):-
 ignore((
 VMGID = VM.gid,
 \+ atom_contains(VMGID,'_fg_abtractiond'),
 Grid = VM.start_grid,
 other_grid(Grid,Target), 
 is_grid(Target),!,
 grid_size(Target,H,V),!,VM.h==H,VM.v==V,
 mapgrid(fg_abtractiond,Grid,Target,NewGrid),mass(NewGrid,M),mass(Grid,M2),!,
 M>4,M2\==M,Grid\==Target,NewGrid\==Grid,

 %(M==0->maplist_ignore(fg_abtractiond,Target,Grid,NewGrid); MNewGrid=NewGrid),
 must_det_ll((
  %var(GOID),
  atomic_list_concat([VMGID,'_fg_abtractiond'],GOID), assert_grid_gid(NewGrid,GOID),
  get_vm(VMS), 
  %individuate2(_,Subtraction,GOID,NewGrid,FoundObjs),
  individuate(Subtraction,NewGrid,FoundObjs),
  set_vm(VMS),
  ReColored = FoundObjs,
  %globalpoints_include_bg(VM.start_grid,Recolors), my_maplist(recolor_object(VM,Recolors),FoundObjs,ReColored),
  print_grid(fg_abtractions(GOID),NewGrid),
  print_ss(ReColored),
  remLOPoints(VM,ReColored),
  addObjects(VM,ReColored))))),!.



% =====================================================================
is_fti_step(after_subtract).
% =====================================================================
after_subtract(Shape,VM):-
  OldPoints = VM.lo_points,
  OldGrid = VM.grid,
  must_det_ll((
  mapgrid(into_monochrome,OldGrid,MonoGrid),
  localpoints_include_bg(MonoGrid,Keep),
  points_to_grid(VM.h,VM.v,Keep,NewGrid),
  set(VM.lo_points)=Keep,!,
  set(VM.grid)=NewGrid,
  run_fti(VM,Shape),
  set(VM.lo_points)=OldPoints,
  set(VM.grid)=OldGrid)),
  !.

% =====================================================================
is_fti_step(drops_as_objects).
% =====================================================================
drops_as_objects(Name,VM):-
  %Objs = VM.objs,!,
 must_det_ll((
  Grid = VM.grid,
  %notrace(catch(call_with_depth_limit(individuate1(_,Name,VM.start_grid,IndvS0),20_000,R),E,(u_dmsg(E)))),
  individuate1(_,Name,Grid,_IndvS0)
  %my_maplist(override_object(iz(bp(Name))),IndvS0,IndvS1),
  )),!.

% =====================================================================
% check_tid_gid/2
% =====================================================================
check_tid_gid(TID_GID,_Grid):- 
  %print_grid(TID_GID,Grid), 
  TID_GID=tid_gid(ID,GID),
  nop(check_tid_gid2(ID,GID)).

check_tid_gid2(GOID,GID):- 
 must_det_ll((
  testid_name_num_io(GOID,TestID1,Example1,Num1,IO1),
  testid_name_num_io(GID,TestID2,Example2,Num2,IO2),
  check_tid_gid3([TestID1,Example1,Num1,IO1],[TestID2,Example2,Num2,IO2]))).

check_tid_gid3(GOID,GID):- my_maplist(check_tid_gid4,GOID,GID).
check_tid_gid4(GOID,GID):- GOID=GID,!.
%check_tid_gid4(out,in):- dumpST,!.
%check_tid_gid4(in,out):- dumpST,!.
check_tid_gid4(GOID,GID):- pp(check_tid_gid4(GOID,GID)).


%objs_with_feat(Objs,Name,Matches):-
%  include(is_prior_prop(Name),Objs,Matches).


% =====================================================================
is_fti_step(save_as_obj_group).
% =====================================================================
save_as_obj_group(ROptions,VM):-
  %Objs = VM.objs,!,
 must_det_ll((
  GridIn = VM.grid,
  %notrace(catch(call_with_depth_limit(individuate1(_,Name,VM.start_grid,IndvS0),20_000,R),E,(u_dmsg(E)))),
    must_grid_to_gid(GridIn,GOID),
    individuate2(_,ROptions,GOID,GridIn,IndvS0),
    my_maplist(add_birth_if_missing(indiv(ROptions)),IndvS0,IndvSL),
    once((delistify_single_element(ROptions,NamedOpts), 
      (maybe_save_grouped(individuate(NamedOpts,GOID),IndvSL)))),!,
    TID = VM.id,
  (asserta_in_testid(arc_cache:individuated_cache(TID,GOID,ROptions,IndvSL))),
  nop((addObjectOverlap(VM,IndvSL))))),!.


% =====================================================================
is_fti_step(save_as_hybrid_shapes).
% =====================================================================
save_as_hybrid_shapes(ROptions,VM):-
 must_det_ll((
    GridIn = VM.grid,
    save_as_hybrid_shapes(ROptions,GridIn,VM),
    other_grid(VM.grid,Other),
    if_t(is_grid(Other),
     save_as_hybrid_shapes(ROptions,Other,VM)))).


save_as_hybrid_shapes(ROptions,GridIn,VM):-
 must_det_ll((
    VM.objs = Objs,  
  %notrace(catch(call_with_depth_limit(individuate1(_,Name,VM.start_grid,IndvS0),20_000,R),E,(u_dmsg(E)))),
    must_grid_to_gid(GridIn,GOID),
    individuate2(_,ROptions,GOID,GridIn,IndvS0),  my_maplist(add_birth_if_missing(indiv(ROptions)),IndvS0,IndvSL),
    set(VM.grid) = VM.start_grid,
    set(VM.lo_points) = VM.start_points,
    set(VM.objs) = Objs,
    learn_hybrid_shape(IndvSL))).


add_birth_if_missing(Birth,I,O):- is_list(I),my_maplist(add_birth_if_missing(Birth),I,O).
%add_birth_if_missing(_,I,I):-!.
%add_birth_if_missing(_,O,O):- has_prop(iz(birth(_)),O),!.
add_birth_if_missing(Birth,I,O):- override_object(iz(birth(Birth)),I,O).


addObjectOverlap(VM,IndvS2):- 
   append(IndvS2,VM.objs,IndvS3),
   list_to_set(IndvS3,IndvS4),
   combine_same_globalpoints(IndvS4,IndvS5),
   length(IndvS5,Count),
   set(VM.objs)= IndvS5,
   set(VM.objs_max_len) is Count+3.


% =====================================================================
is_fti_step(gather_cached).
% =====================================================================
gather_cached(VM):-
  TID = VM.id,
  findall(IndvS,call_in_testid(arc_cache:individuated_cache(TID,_,_,IndvS)),IndvSL),
  append(IndvSL,IndvSS),
  addObjectOverlap(VM,IndvSS).
   

/*
  atrace,Obj = obj(Props),
  globalpoints_include_bg(Obj,GPoints),
  my_partition(props_not_for_merge,Props,_Exclude,Overrides),
  u_dmsg(oberrides=Overrides),
  make_indiv_object(VM,Overrides,GPoints,Obj),
  assumeAdded(VM,Obj).
*/

% =====================================================================
is_fti_step(point_corners).
% =====================================================================
point_corners(VM):-
  each_obj(VM.objs,Obj,
   (point_corners(Obj,Dir,OC-P1),
     is_adjacent_point(P1,Dir,P2),
     select(OC-P1,VM.start_points,Rest),     
     select_always(C-P2,Rest,Points0),C\==OC,
     sa_point(C-P2,Points0),
     remLOPoints(VM,[C-P2]),
     make_point_object(VM,[iz(important)],C-P2,Obj),
     assumeAdded(VM,Obj))).

select_always(E,B,A):- select(E,B,A)*->true;B=A.

sa_point(C-P2,Points):- select_always(C-P2,Points,Points0), 
  is_sa(Points,C-P2),
  \+ (is_adjacent_point(P2,Dir,P3), member(C-P3,Points0), \+ is_diag(Dir)),!.


% =====================================================================
is_fti_step(alone_dots).
is_fti_step(maybe_alone_dots).
is_fti_step(maybe_alone_dots_by_color).
% =====================================================================
alone_dots(N,VM):- using_alone_dots(VM,maybe_alone_dots(N,VM)),!.
alone_dots(_,_):-!.

maybe_alone_dots(lte(LTE),VM):- maybe_sa_dots(VM.lo_points,lte(LTE),VM).
maybe_alone_dots(_,_).

% alone_dots(lte(5)) that have no adjacent points of the equal color (could be gathered first)

is_sa(Points,C-P2):-
    allowed_color(sa_dots,C),
    \+ (is_adjacent_point(P2,Dir,P3), Dir\==c, member(C-P3,Points), \+ is_diag(Dir)),!.

contains_alone_dots(Grid):- nonvar(Grid), globalpoints_maybe_bg(Grid,Points),include(is_sa(Points),Points,SAs),SAs\==[].

using_alone_dots(VM, _):-  fail, \+ contains_alone_dots(VM.start_grid), \+ contains_alone_dots(VM.target_grid),!.
using_alone_dots(_,Goal):- fail,  when_arc_expanding(once(Goal)).
using_alone_dots(_,Goal):- call(Goal).

maybe_alone_dots_by_color(lte(LTE),VM):-  
   available_fg_colors(TodoByColors),
   do_maybe_alone_dots_by_color(TodoByColors,lte(LTE),VM).

do_maybe_alone_dots_by_color(Color,lte(LTE),VM):- is_color(Color),!,
    my_partition(has_color(Color),VM.lo_points,ThisGroup,_WrongColor),
    maybe_sa_dots(ThisGroup,lte(LTE),VM).
do_maybe_alone_dots_by_color([Color|TodoByColors],lte(LTE),VM):- !,
  do_maybe_alone_dots_by_color(Color,lte(LTE),VM),
  do_maybe_alone_dots_by_color(TodoByColors,lte(LTE),VM).
do_maybe_alone_dots_by_color(_,_,_).

maybe_sa_dots(Points,lte(LTE),VM):-
  \+ exceeded_objs_max_len(VM),
  if_t(Points \== [],
   (my_partition(is_sa(Points),Points,SAPs,_NonSAPoints),
    length(SAPs,Len),
    if_t((LTE>=Len),
    ( remLOPoints(VM,SAPs),
     using_alone_dots(VM,(my_maplist(make_point_object(VM,[birth(alone_dots(lte(LTE))),iz(type(sa_dots)),iz(media(shaped))]),SAPs,IndvList),
     nop(assumeAdded(VM,IndvList)))))))),!.

% =====================================================================
is_fti_step(maybe_lo_dots).
% =====================================================================
maybe_lo_dots(VM):-
  current_as_one(VM),
  %length(VM.lo_points,Len), (VM.h>=Len ; VM.v>=Len), 
  lo_dots(VM).
maybe_lo_dots(_):-!.

% =====================================================================
is_fti_step(lo_dots).
% =====================================================================
% lo_dots may have adjacent points of the equal color (because we are in 'lo_dots' mode)
mostly_fgp(Points,LO_POINTS):- length(Points,Len), Len =< 49,!, Points=LO_POINTS.
mostly_fgp(Points,FG_POINTS):- my_partition(is_fgp,Points,FG_POINTS,_),!.

lo_dots(VM):-  
  mostly_fgp(VM.lo_points,LO_POINTS),
  (true; VM.h=<5 ; VM.v=<5 ; (LO_POINTS \=[], length(LO_POINTS,Len), Len<12, (Len =< VM.h ; Len =< VM.v  ))),!,  
  using_alone_dots(VM,(my_maplist(make_point_object(VM,[birth(lo_dots),iz(stype(dot)),iz(media(shaped))]),LO_POINTS,IndvList),
  assumeAdded(VM,IndvList))),
  ((set(VM.lo_points) =[])),!.
lo_dots(VM):-  
  mostly_fgp(VM.lo_points,LO_POINTS),
   length(LO_POINTS,Len), Len<12,
  using_alone_dots(VM,(my_maplist(make_point_object(VM,[birth(lo_dots),iz(stype(dot))]),LO_POINTS,IndvList),
  assumeAdded(VM,IndvList))),
  ((set(VM.lo_points) =[])),!.


/*
fti(VM,[colormass_merger(Size)|TODO]):-
  colormass_merger(Size,VM),!,
  colormass_merger(Size,VM),!,
  %colormass_merger(3,VM),
  set(VM.lo_program) = TODO.
*/

% =====================================================================
is_fti_step(colormass_merger).
% =====================================================================
%colormass_merger(_Size,_VM):-!.
colormass_merger(Size,VM):-
  Objs = VM.objs,
  my_partition(less_mass(Size),Objs,Smaller,Bigger),
  my_maplist(colormass_merger(VM,Smaller),Bigger).

colormass_merger(VM,Smaller,Bigger):- 
   find_mergeable(VM,Bigger,Smaller,Touches),
   ignore((Touches\==[],
   print_grid(VM.h,VM.v,"Removing...",Touches),
   merge_objs(VM,Bigger,Touches,[iz(info(combined))],Combined),
   intersection(VM.objs,[Bigger|Touches],_Remove,Kept,_),   
   print_grid(VM.h,VM.v,"Adding...",Combined),
   set(VM.objs) = [Combined|Kept])) .

find_mergeable(_VM,_,[],[]).
find_mergeable(VM,Found,[SubProgram|ScanPoints],[SubProgram|Engulfed]):-    
 mergable_objects(VM,Found,SubProgram),
 find_mergeable(VM,Found,ScanPoints,Engulfed),!.
find_mergeable(VM,Found,[_|ScanPoints],Engulfed):-
 find_mergeable(VM,Found,ScanPoints,Engulfed),!.


%individuate_points(Grid,How,Results):- globalpoints(Grid,Points), grid_size(Grid,H,V),individuate_points0(H,V,Grid,Points,How,Results).
%individuate_points0(H,V,Grid,Points,How,Results):- How = by_row,Results = Grid,member(
%individuate_points0(H,V,Grid,Points,How,Results):- How = by_col, rot90(Grid,Grid90), Results = Grid.
%individuateR(Grid,complete,Objs):- individuate(complete,Grid,Objs).
%individuateR(Grid,Name,Objs):- var(Grid),!,arc_grid(Grid),individuateR(Grid,Name,Objs).
%individuateR(Grid,Name,Objs):- 
%  no_repeats(Name+DirS,(allow_dir_list(Name,Dirs),sort_safe(Dirs,DirS))),
%  individuate(Name,Grid,Objs),Objs\==[].

%one_fti_step(Name)


% =====================================================================
is_fti_step(rectangles).
% =====================================================================
rectangles(_VM):- !.
rectangles(VM):- 
  Grid = VM.grid,
  Objs = VM.objs,
  rectangles_from_grid(Grid,VM),
  NewObjs = VM.objs,!,
  if_t(Objs==NewObjs,
   (texture_grid(Grid,TexturedGrid),
    mapgrid(maybe_subst_grid_type,TexturedGrid,Retextured),
    print_ss(red,TexturedGrid,texture,_,Retextured,no_squares))).

maybe_subst_grid_type(V,V):- var(V),!.
maybe_subst_grid_type(E-C,'+'-C):- nonvar(E), arg(_,t('A','<','y','>','/','\\','=','!'),E). %,ignore(C=wbg).
maybe_subst_grid_type(E-C,'*'-C):- arg(_,t('v','A'),E). %,ignore(C=wbg).
%maybe_subst_grid_type(E-C,'.'-C):- arg(_,t('|','-'),E),ignore(C=wbg).
maybe_subst_grid_type(A,A).
maybe_wbg(E-V,E-wbg):- var(V),!.
maybe_wbg(V,wbg):- var(V),!.
maybe_wbg(X,X).

unbind_list(L,O):- length(L,N),length(O,N),!.
texture_grid(In,In2):- must_det_ll((make_bg_visible(In,Retextured), most_d_colors(Retextured,_CI,In2))).

row_not_isnt(LeftN,RightN,C,Row,NewSubRow,NewRow):-
  length(Left,LeftN), length(Right,RightN),
  append([Left,SubRow,Right],Row),  
  append([[_-C1],_List,[_-C1]],SubRow),!,
  C==C1,
  %my_maplist(was_color(C),List),
  my_maplist(divide_colors(C),SubRow,NewSubRow,NewSubRowReplace),  
  append([Left,NewSubRowReplace,Right],NewRow),!.

divide_colors(C,Cell,Cell,_):- was_color(C,Cell),!.
divide_colors(_,Cell,_,Cell).

mustlist_until_failed(P3,[A1|L1],[A2|L2],[A3|L3],Rest):-
  call(P3,A1,A2,A3),!,
  mustlist_until_failed(P3,L1,L2,L3,Rest).
mustlist_until_failed(_P3,Rest,[],[],Rest):-!.

row_in_grid(C,L,LeftN,SecondRow,RightN,TexturedGrid):- nonvar(LeftN),
  length(LeftS,LeftN), length(RightS,RightN),
  (var(SecondRow)->member(SecondRow,TexturedGrid);true),
  append([LeftS,[La-Ca],_,[Lb-Cb],RightS],SecondRow),  
  (nonvar(L) -> (La==L, Lb==L) ; (La = Lb, Lb=L)),
  (nonvar(C) -> (Ca==C, Cb==C) ; (Ca == Cb, Cb=C, C \== wbg, C \== fg)).

row_in_grid(C,L,LeftN,SecondRow,RightN,TexturedGrid):-
  (var(SecondRow)->member(SecondRow,TexturedGrid);true),
  append([LeftS,[La-Ca],_,[Lb-Cb],RightS],SecondRow),  
  (nonvar(L) -> (La==L, Lb==L) ; (La = Lb, Lb=L)),
  (nonvar(C) -> (Ca==C, Cb==C) ; (Ca == Cb, Cb=C, C \== wbg, C \== fg)),  
  length(LeftS,LeftN), length(RightS,RightN).


rectangles_from_grid(Grid,_VM):- fail,
  append(_Top,[M1,M2|_Bottem],Grid),
  dif(L1,L2),
  dif(R1,L2),R2=L2,
  append([LL,[L1,L2],T,[R2,R1],_],M1),  
  my_maplist(=(L2),T),
  length(LL,NL),
  length(LL2,NL),
  %L1\==L2,R1\==R2,
  append([LL2,[_,L2],T,[R2,_],_],M2).
  

rectangles_from_grid(Grid,VM):-
  texture_grid(Grid,TexturedGrid),
  mapgrid(maybe_subst_grid_type,TexturedGrid,Retextured),!,
  grid_size(Retextured,H,_),%make_list('X'-black,H,DeadRow),

  member(L2,['|','+']),
  row_in_grid(C,L2,LeftN,SecondRow,RightN,TexturedGrid),
  nop(nth1(N2,TexturedGrid,SecondRow)),

  member(L1,['+','|']),
  row_in_grid(C,L1,LeftN,FirstRow,RightN,TexturedGrid),
  nop((nth1(N1,TexturedGrid,FirstRow), N2>N1)),

  append(BeforeFirstRow,[FirstRow|Rest],Retextured),  
  %my_maplist(was_color(C),FirstRow),

  mustlist_until_failed(row_not_isnt(LeftN,RightN,C),[FirstRow|Rest],RowsInvolvedClipped,Replacements,Below),
  length(RowsInvolvedClipped,N),N>1,!,
  append([BeforeFirstRow,Replacements,Below],NewGrid),
  nth1(1,RowsInvolvedClipped,First),
  last(RowsInvolvedClipped,Last),
  writeq(Last),nl,
  nop(my_maplist(was_color_or_unbound(C),First)),
  my_maplist(was_color_or_unbound(C),Last),!,
  Width is H - RightN - LeftN,
  print_ss(C,TexturedGrid,texture,_,Retextured,retextured(Width,N)),
  print_ss(C,NewGrid,'grid',_,RowsInvolvedClipped,clipped(Width,N)),
  mapgrid(only_color_data,RowsInvolvedClipped,Textureless),
  localpoints(Textureless,NewObjPoints),!,
  ignore((NewObjPoints\==[],make_indiv_object(VM,[birth(rectangles),iz(stype(rectangle))],NewObjPoints,_Obj))),
  ignore((NewGrid\==[], print_grid(newGrid, NewGrid),!, rectangles_from_grid(NewGrid,VM))),
  !.
rectangles_from_grid(_,_).


% @TODO
rects_of(_Obj,[]).

mass_gt(N,Obj):- mass(Obj,Mass),Mass>N.


% ======================================
is_fti_step(keypads).
% ======================================
keypads(VM):- 
  Grid = VM.grid,
  trim_to_rect4(OX,OY,EX,EY,Grid,Keypad),
  grid_size(Keypad,H,V),
  print_ss(keypad(OX,OY,H,V,EX,EY),Grid,Keypad),
  H==3,V==3,
  mass(Keypad,Mass),
  Mass>=9,Mass=<9,
  must_det_ll((
  grid_to_points(Keypad,LPoints),
  writeg(keypad=Keypad),
  length(LPoints,Len),
  writeg(points(Len)=LPoints),
  offset_points0(OX,OY,LPoints,AllGpoints),
  %itrace,
  my_maplist(make_point_object(VM,[birth(keypads),iz(media(shaped))]),AllGpoints,IndvList),
  print_grid(allGpoints,IndvList),
 % remLOPoints(VM,IndvList),
  remLOPoints(VM,IndvList))),!.
  %addObjects(VM,IndvList)

keypads(VM):- 
 ignore((
  Grid = VM.grid,
  trim_to_rect4(OX,OY,EX,EY,Grid,Keypad),
  grid_size(Keypad,H,V),
  print_ss(keypad(OX,OY,H,V,EX,EY),Grid,Keypad),
  Area is H*V,
  once( H>1 ;  V>1 ),
  mass(Keypad,Mass),
  Mass==Area,Mass=<25,
  grid_to_points(Keypad,LPoints),
  unique_colors(LPoints,CCs),CCs=[_,_|_],
  writeg(keypad=Keypad),
  length(LPoints,Len),
  writeg(points(Len)=LPoints),
  must_det_ll((
  offset_points0(OX,OY,LPoints,AllGpoints),
  %itrace,
  my_maplist(make_point_object(VM,[birth(keypads),iz(media(shaped))]),AllGpoints,IndvList),
  print_grid(allGpoints,IndvList),
 % remLOPoints(VM,IndvList),
  remLOPoints(VM,IndvList),
  %addObjects(VM,IndvList)
  !)))),!.

% ======================================
% tiny grid becomes a series of points
% ======================================
is_fti_step(maybe_glyphic).
maybe_glyphic(VM):- 
  Points = VM.lo_points,
  if_t(is_glyphic(VM,Points,VM.h,VM.v),one_fti(VM,glyphic)).

%is_glyphic(Points,_GH,_GV):- length(Points,Len), Len < 5.
%is_glyphic(Points,_GH,_GV):- mass(Points,Len), Len =< 25,!.
is_glyphic(_VM,_Points,GH,GV):- ( GH=<4 , GV=<4 ).
%is_glyphic(_VM, Points,GH,GV):- ( GH=<3 , GV=<3 ), nop((length(Points,Len), Len is GH * GV)).
is_glyphic( VM,_Points,GH,GV):- ( GH=<5 , GV=<5 ), other_grid_size(VM.start_grid,OH,OV),!,
  (OV > GV,OH > GH),
  is_purp(_How,OH,OV,GH,GV).

is_purp(0,OH,OV,GH,GV):- is_purp0(OH,OV,GH,GV).
is_purp(-1,OH,OV,GH,GV):- is_purpM(-1,OH,OV,GH,GV),!.
is_purp(-2,OH,OV,GH,GV):- is_purpM(-2,OH,OV,GH,GV),!.
is_purpM(O,OH+O,OV+O,GH,GV):- is_purp0(OH,OV,GH,GV).
is_purpM(O,OH,OV,GH+O,GV+O):- is_purp0(OH,OV,GH,GV).
is_purpM(O,OH+O,OV+O,GH+O,GV+O):- is_purp0(OH,OV,GH,GV).
is_purp0(OH,OV,GH,GV):- 0 is OH rem GH, 0 is OV rem GV.

%is_glyphic(Points,_GH,_GV):- mass(Points,Len), Len =< 16,!.

one_fti(VM,glyphic):-
 must_det_ll((
  one_fti(VM,whole),
  localpoints_include_bg(VM.start_grid,Points),
  %length(Points,LenBG),
  %(LenBG=<15->UPoints=Points;mostly_fgp(Points,UPoints)),
  %length(UPoints,ULen),!,
  ignore(( %ULen=<15,
  UPoints = Points,
  VM.lo_program=Code,
  %run_fti(VM,[i_by_color]),
  set(VM.lo_program)=Code,
  using_alone_dots(VM,(my_maplist(make_point_object(VM,[birth(glyphic),iz(media(shaped))]),UPoints,IndvList), assumeAdded(VM,IndvList),
  maybe_save_grouped(individuate(glyphic,VM.gid),IndvList))))))).

%make_point_object(VM,_Opts,Point,Indv):-
%    member(Point=Indv, VM.added_points),!.


named_grid_props(Name,VM):- one_fti(VM,named_grid_props(Name)),!.

is_fti_step(named_grid_props(name)).
one_fti(VM,named_grid_props(Name)):- nop(named_grid_props(Name,VM)).
/*
  H=VM.h,V=VM.v,
  Grid= VM.start_grid,
  hv_point_value(1,1,Grid,PointNW),
  hv_point_value(1,V,Grid,PointSW),
  hv_point_value(H,1,Grid,PointNE),
  hv_point_value(H,V,Grid,PointSE),
  grid_props(Grid,Props),
  append(Props,[mass(0),vis2D(H,V),birth(named_grid_props),loc2D(1,1),iz(flag(always_keep)),iz(media(image)),iz(flag(hidden))],AllProps),
  make_indiv_object(VM,AllProps,[PointNW,PointSW,PointNE,PointSE],_),!.
*/
hv_point_value(H,V,Grid,C-Point):- hv_point(H,V,Point),point_c_value(Point,C,Grid).

is_fti_step(whole).
whole(VM):- one_fti(VM,whole),!.

one_fti(VM,whole):-
  %localpoints_include_bg(VM.grid,Points),
  Grid = VM.grid,
  Objs = VM.objs,
  include(has_prop(iz(stype(whole))),Objs,IsWhole),
  (IsWhole \== [] -> true;
   (whole_into_obj(VM,Grid,Whole), gset(VM.objs)=[Whole|Objs])).

whole_into_obj(VM,Grid,Whole):- 
 must_det_ll((
  grid_size(Grid,H,V),
  localpoints_include_bg(Grid,Points),
  %length(Points,Len),
  grid_props(Grid,Props0),
  Area is H * V,
  delete(Props0,sometimes_grid_edges(_),Props),
  make_indiv_object(VM,[iz(stype(whole)),iz(media(image)),% iz(flag(hidden)),
    mass(Area),loc2D(1,1),globalpoints(Points),localpoints(Points)|Props],
    Points,Whole),assumeAdded(VM,Whole),
  maybe_save_grouped(individuate(whole,VM.gid),[Whole]),learn_hybrid_shape(pair,Whole))).
  /*
  if_t(Len>=0,
    (make_indiv_object(VM,[mass(Len),vis2D(H,V),iz(stype(whole)),iz(flag(always_keep)),loc2D(1,1),iz(media(image))|Props],Points,Whole),
      assumeAdded(VM,Whole),
       maybe_save_grouped(individuate(whole,VM.gid),[Whole]),learn_hybrid_shape(pair,Whole))),
  localpoints(Grid,LPoints),
  length(LPoints,CLen),if_t((CLen=<144,CLen>=0),    
    (make_indiv_object(VM,[iz(stype(whole)),iz(media(shaped)),mass(Area),loc2D(1,1)],LPoints,Whole2),assumeAdded(VM,Whole2))).
*/


% =====================================================================
is_fti_step(remove_used_points).
% =====================================================================
remove_used_points(VM):-  
  remLOPoints(VM,VM.objs),
  points_to_grid(VM.h,VM.v,VM.lo_points,Grid),
  mapgrid(plain_var_to(black),Grid),
  gset(VM.grid)=[[black]].
  %gset(VM.grid) = Grid.

plain_var_to(Black,Var):- plain_var(Var),!,Var=Black.
plain_var_to(_,_).
% =====================================================================
is_fti_step(colormass_subshapes).
% =====================================================================
colormass_subshapes(VM):- colormass_subshapes(VM,VM.objs).
colormass_subshapes(_VM,[]):-!.
colormass_subshapes(VM,VMObjs):- % fail,
  select(Obj,VMObjs,SubProgram),
  object_grid(Obj,Grid),
  individuate7(_,VM.id,[subshape_in_object],Grid,WasInside),
  ignore((WasInside =[_,_|_], % two or more
        print_grid("colormass_subshapes",WasInside),
        assumeAdded(VM,WasInside))),
  colormass_subshapes(VM,SubProgram).
colormass_subshapes(_,_):-!.



one_fti(VM,by_color(Min,C)):- 
  my_partition(has_color(C),VM.lo_points,ThisGroup,LeftOver),
  ignore(((
   ThisGroup\==[],
   length(ThisGroup,Len),  Len >= Min,
   set(VM.lo_points)=LeftOver,
   meets_indiv_criteria(VM,birth(by_color),ThisGroup),
   make_indiv_object(VM,[birth(by_color),iz(media(image)),iz(media(shaped))],ThisGroup,ColorObj),
   assumeAdded(VM,ColorObj)))).

one_fti(VM,by_color(Min)):- 
  findall(by_color(Min,Color),enum_fg_colors(Color),TodoByColors),
  run_fti(VM,TodoByColors).


one_fti(VM,shape_lib(LibName)):-
  one_fti(VM,shape_lib(regular,LibName)).
one_fti(VM,shape_lib(Method,LibName)):-
  locally(b_setval(find_rule,Method),
    ((shape_lib_expanded(LibName,Reserved),
      try_shapelib(VM,Method,LibName,Reserved)))).

try_shapelib(VM,Method,LibName,Reserved):-   
  length(Reserved,RL),
  ignore((RL>30,progress(shape_lib_direct(LibName)=RL))),
  %smallest_first
  %largest_first(mass,Reserved,ReservedSL),
  %print_info(Reserved),
  use_shapelib(VM,Method,LibName,Reserved),
  %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,'shape_lib'+VM.id,Found)),
  !.

use_shapelib(_VM,_Method,_Hammer,[]):-!.
use_shapelib(VM,Method,LibName,[Shape|ReservedSL]):- !,
  (try_shape(VM,Method,LibName,Shape)-> use_shapelib(VM,Method,LibName,[Shape|ReservedSL]) ; use_shapelib(VM,Method,LibName,ReservedSL)).

try_shape(VM,Method,LibName,Shape):-     
   %ignore((length(RestReserved,RL),1 is RL mod 7, progress(searchLib(LibName)=RL))),
   % Points\==[],
   %\+ color(Shape,black),
   object_grid(Shape,OGrid),
   vis2D(Shape,SH,SV),
   Grid = VM.grid, % GH = VM.h, GV = VM.v,
   vis2D(Grid,GH,GV),!,
   GH>=SH, GV>=SV,   
 % dmsg((GH>=SH, GV>=SV)), !,  
   Key = loc_shape(OH,OV,LibName),
   find_ogs_c(Method,OH,OV,OGrid,Grid),
   \+ member(Key,VM.robjs),
  must_det_ll((
   addRObjects(VM,Key),
   show_match(OH,OV,OGrid,Grid),
 %  Grid = VM.grid,
   %print_ss(,OGrid),
   localpoints_include_bg(Shape,LPoints),
   offset_points(OH,OV,LPoints,ObjPoints),
   %Points = VM.lo_points,
   %intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
   %Sofar = VM.robjs,
   %do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),   
   %my_append(Intersected,Use,All),
   %list_to_set(All,AllS), AllS \== [],  
   %set(VM.lo_points) = RestOfPoints,
   %set(VM.objs)= Sofar2,
   %nl,writeq(((points_to_grid(RestOfPoints,GH,GV,NewGrid)))),nl,
   %set(VM.grid) = NewGrid,

   indv_props_list(Shape,ShapeProps), 
   my_partition(props_not_for_merge,ShapeProps,_Exclude,Include),
   make_indiv_object(VM,[birth(shape_lib(Method,LibName)),vis2D(SH,SV),loc2D(OH,OV)|Include],ObjPoints,Indiv),  %obj_to_oid(Shape,_,Iv), %override_object(obj_to_oid(VM.id,Iv),Indiv0,Indiv),  %make_indiv_object(VM,Use,Indiv),
   %nop(points_to_grid(RestOfPoints,set(VM.grid))),  %print_grid(Indiv),
   %assumeAdded(VM,Indiv),
   nop(print_info(Indiv)))).
   

/*  
use_shapelib(VM,Name,[Obj|Reserved]):-
   once((globalpoints(Obj,ObjPoints), 
   \+ color(Obj,black),
   Points = VM.lo_points,
    Sofar = VM.objs,
   intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
         do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),
         my_append(Intersected,Use,All),
         list_to_set(All,AllS))), AllS \== [],
         make_indiv_object(VM,[iz(override(Name))|AllS],Indiv0), 
         obj_to_oid(Obj,_,Iv), 
         override_object(obj_to_oid(VM.id,Iv),Indiv0,Indiv), 
         assumeAdded(VM,Indiv),
         %make_indiv_object(VM,Use,Indiv),
         use_shapelib(VM,Name,Reserved).
*/
do_leftover(Sofar,[],Intersected,Intersected,Sofar):- !.
%do_leftover([],_,_,_,_):- !,fail.
do_leftover(Sofar,LeftOverA,Intersected,Use,Removed):- select(S,Sofar,Removed), globalpoints(S,SPoints),
    intersection(SPoints,LeftOverA,Usable,[],[]),my_append(Usable,Intersected,Use).

one_fi(VM,retain(Option)):-
    Grid = VM.grid,
    ID = VM.id,
    globalpoints(Grid,NewGPoints), %  H> 14, V> 14,
    freeze(W,W>5),filter_indivs(VM.objs,[mass(W)];[iz(Option)],Retained1),
    filter_indivs(Retained1, \+ iz(background),Retained),
    as_debug(9,print_grid(H,V,'retained'+ID,Retained)),    
    remove_global_points(Retained,NewGPoints,set(VM.lo_points)),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    set(VM.objs)= Retained,
    set(VM.grid)= NNewGrid,
    set(VM.lo_points)= NextScanPoints,

    %as_debug(9,print_grid(H,V,'newgrid'+ID,NNewGrid)),
    !.

% =====================================================================
is_fti_step(release_points).
% =====================================================================
release_points(VM):- 
    globalpoints(VM.grid,NextScanPoints1),
    addLOPoints(VM,NextScanPoints1),
    globalpoints(VM.objs,NextScanPoints2),
    addLOPoints(VM,NextScanPoints2).

objs_into_single_hidden(VM):- objs_into_single_now(VM,[iz(info(combined)),iz(flag(hidden)),iz(into_single)]).
objs_into_single(VM):- objs_into_single_now(VM,[iz(info(combined)),iz(into_single)]).

objs_into_single_now(Opts,VM):-
    my_maplist(globalpoints,VM.objs,IndvPoints),
    meets_indiv_criteria(VM,into_single,IndvPoints),!,
    make_indiv_object(VM,Opts,IndvPoints,Indv),    
    assumeAdded(VM,Indv).


% =====================================================================
is_fti_step(recompute_points).
% =====================================================================
recompute_points(VM):- 
    ignore((
    VM.objs\==[],
    globalpoints(VM.start_points,GridPoints),
    globalpoints(VM.objs,ObjsPoints),
    subtract(GridPoints,ObjsPoints,LeftOver),!,
    LeftOver\==[],
    points_to_grid(VM.h,VM.v,LeftOver,NewGrid),
    mapgrid(add_missing,NewGrid,NNewGrid),
    nop((as_ngrid(NNewGrid,NGrid),
    print_ss(green,NGrid,'recompute_points',_,VM.objs,'objects'))),
    
    gset(VM.lo_points)= LeftOver,
    gset(VM.grid)= NewGrid)).

add_missing(Plain,wbg):- plain_var(Plain).
add_missing(X,X).
% =====================================================================
is_fti_step(leftover_as_one).
% =====================================================================
leftover_as_one(VM):-
   Points = VM.lo_points,
   ignore((Points\==[],
   u_dmsg(leftover_as_one=Points),
   make_indiv_object(VM,[iz(info(combined)),iz(info(leftover_as_one))],Points,LeftOverObj), verify_object(LeftOverObj),
   assumeAdded(VM,LeftOverObj))),
   VM.lo_points=[].

% =====================================================================
is_fti_step(current_as_one).
% =====================================================================
current_as_one(VM):-
 Points = VM.lo_points,
   ignore((
   Points\==[],
   %set_html_stream_encoding, 
   u_dmsg(current_as_one=Points),
   make_indiv_object(VM,[iz(info(combined)),birth(current_as_one)],Points,LeftOverObj), verify_object(LeftOverObj),
   assumeAdded(VM,LeftOverObj),
   set(VM.lo_points) = Points)).
   

ignore_rest(VM):- VM.lo_points=[].


same_lcolor(LargestColor,Obj):- color(Obj,Color),nop(print_grid(Obj)),!,Color==LargestColor.




% @TODO will get sub objects later
not_list(G):- \+ is_list(G).


is_sort_tag(L):- nonvar(L),(L=sort(_);L=result(_,_);L=sort(_,_)).

remove_sort_tag(L,G):- is_list(L),!,my_maplist(remove_sort_tag,L,G).
remove_sort_tag(L,G):- maybe_remove_sort_tag(L,G),!.
remove_sort_tag(G,G).

maybe_remove_sort_tag(L-G,G):- is_sort_tag(L).

into_list(G,L):- maybe_remove_sort_tag(G,LL),!,into_list(LL,L).
into_list(G,L):- is_grid(G),!,L=[G].
into_list(G,L):- is_list(G),!,L=G.
into_list(G,L):- is_vm_map(G),!,L = G.objs,my_assertion(is_list(L)).
into_list(I,O):- listify(I,O),!.


assume_vm(_).
:- style_check(-singleton).

addPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  my_maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.Prop,ReallyAdd,set(VM.Prop)).

remPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  my_maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,ReallyRemove,Keep,PretendToRemove),
  gset(VM.Prop) = Keep.

addRObjects(_VM,Obj):- Obj==[],!.
addRObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.robjs,List,PretendToAdd,Prev,ReallyAdd),
  %addGridPoints(VM, Obj),
  %addGridPoints(VM, Obj),  
  my_append(VM.robjs,ReallyAdd,set(VM.robjs)).

remObjects(_VM,Obj):- Obj==[],!.
remObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,ReallyRemove,Keep,PretendToRemove),
  %remGridPoints(VM, ReallyRemove),
  set(VM.objs) = Keep.

addLOPoints(_VM,Obj):- Obj==[],!.
addLOPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,LOPoints), 
  intersection(VM.lo_points,LOPoints,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.lo_points,ReallyAdd,set(VM.lo_points)).

remLOPoints(_VM,Obj):- Obj==[],!.
%remLOPoints(VM,Obj):- is_group(Obj),!,mapgroup(remLOPoints(VM),Obj).
remLOPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.lo_points,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.lo_points) = Keep,
  remGridPoints(VM,Obj).

remGridPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  Grid = VM.grid,
  remove_global_points(List,Grid,StartGrid),
  set(VM.grid) = StartGrid.

addGridPoints(_VM,Obj):- Obj==[],!.
addGridPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,GPoints), 
  intersection(VM.added_points,GPoints,PretendToAdd,_Prev,ReallyAdd),
  my_append(VM.added_points,ReallyAdd,set(VM.added_points)).
  

addObjects(_VM,Obj):- Obj==[],!.
addObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.objs,ReallyAdd,set(VM.objs)),
  assumeAdded(VM,Obj).
assumeAdded(VM,Obj):- addGridPoints(VM, Obj),remLOPoints(VM, Obj),!. % nop(assertion(member(Obj,VM.objs))).
/*
addObjects(_VM,Obj):- Obj==[],!.
addObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,_PretendToAdd,_Prev,ReallyAdd),
  my_append(VM.objs,ReallyAdd,set(VM.objs)).
  %my_maplist(mergeObject(VM),ReallyAdd).
*/

mergeObject(VM,ReallyAdd):- ReallyAdd==[],!.
mergeObject(VM,ReallyAdd):- is_group_or_objects_list(ReallyAdd),!,my_maplist(mergeObject(VM),ReallyAdd).
mergeObject(VM,ReallyAdd):- 
  get_gpoints_and_props(ReallyAdd,GPoints,Props),!,
  (GPoints==[] -> true ;
   (GPoints\==[] -> (make_indiv_object(VM,Props,GPoints,Obj),assumeAdded(VM,Obj),
     nop(addObjects(VM,Obj))) ; true)).

is_list_of_prop_lists(List):- List\==[], my_maplist(is_obj_props,List).
is_group_or_objects_list(ReallyAdd):- (is_list_of_prop_lists(ReallyAdd); is_group(ReallyAdd)).

  
  

%  my_append(VM.objs,ReallyAdd,set(VM.objs)).

addOGSObjects(_VM,Obj):- Obj==[],!.
addOGSObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List),
  intersection(VM.objs,List,_PretendToAdd,_Prev,ReallyAdd),
  my_maplist(mergeObject(VM),ReallyAdd).


addRepairedPoints(_VM,Obj):- Obj==[],!.
addRepairedPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,LOPoints),
   intersection(VM.repaired,LOPoints,PretendToAdd,Prev,ReallyAdd),
   my_append(VM.repaired,ReallyAdd,set(VM.repaired)).

remRepairedPoints(_VM,Obj):- Obj==[],!.
remRepairedPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.repaired,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.repaired) = Keep.


addOptions(VM, Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.options,ReallyAdd,set(VM.options)),
  intersection(VM.lo_program,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.lo_program,ReallyAdd,set(VM.lo_program)).

remOptions(VM,Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.options) = Keep,
  intersection(VM.lo_program,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.lo_program) = Keep.


:- style_check(+singleton).

%in_set(Set,I):- member(E,Set), E=@=I,!.

is_fti_stepr(remove_from_image(_Data)).
remove_from_image(VM,Data):-    
    must_det_ll((remove_global_points(Data,VM.lo_points,Points),
    progress(Points),
    set(VM.lo_points) = Points)),!.
   

overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_ll((remove_global_points(FourWay1s,Sofar,Sofar1),add_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).


one_fti(VM,merge_shapes(ShapeType1)):-one_fti(VM,merge_shapes(ShapeType1,ShapeType1)),!.
one_fti(VM,merge_shapes(ShapeType1,ShapeType2)):-
  Option = merge_shapes(ShapeType1,ShapeType2), copy_term(Option,OptionC),!, 
      Sofar = VM.objs,
      selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess), % atrace,
      \+ has_prop(iz(media(image)),HV1),
      \+ has_prop(iz(media(image)),HV2),
      any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
      connection_direction(Option,Dir),
  %rot_left_45(Dir1,DirL),rot_left_45(DirL,Dir90),
  % \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),  
  combine_2objs(VM,HV1,HV2,[],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  assumeAdded(VM,Combined),
  cycle_back_in(VM,OptionC).


cycle_back_in(VM,OptionC):- 
  TODO = VM.lo_program,
  length(TODO,N),
  cycle_back_in(VM,OptionC,N,TODO),!.
cycle_back_in(VM,OptionC,TODO):- 
  length(TODO,N), N2 is floor(N/2),length(LL,N2),my_append(LL,RR,TODO),my_append(LL,[OptionC|RR],OptionsOut),
  set(VM.lo_program)= OptionsOut.

%cycle_back_in(VM,OptionC,0,TODO):- set(VM.lo_program) = [OptionC].
cycle_back_in(_,OptionC,_,[T,A|_]):- (OptionC==T ; OptionC==A),!.
cycle_back_in(VM,OptionC,_,[T|ODO]):- !, set(VM.lo_program)= [T,OptionC|ODO].
cycle_back_in(VM,OptionC,_,TODO):- set(VM.lo_program)= [OptionC|TODO].





selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess):- 
  into_group(Sofar,Sofar1,Closure),
  select(HV1,Sofar1,Found1), isz(HV1,ShapeType1),
  select(HV2,Found1,SofarLess1),isz(HV2,ShapeType2),
  call(Closure,SofarLess1,SofarLess).

one_fti(VM,connects(ShapeType1)):-one_fti(VM,connects(ShapeType1,ShapeType1)),!.
one_fti(VM,connects(ShapeType1,ShapeType2)):-
  Option = connects(ShapeType1,ShapeType2), copy_term(Option,OptionC),!,
  Sofar = VM.objs,
  selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
  connection_direction(Option,Dir),    
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),  
  combine_2objs(VM,HV1,HV2,[],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  assumeAdded(VM,Combined),
  cycle_back_in(VM,OptionC).

connection_direction(Connected,Dir):-
   arg(_,Connected,ShapeType),
     shape_type_dirs(ShapeType,[Dir|_]). 
    %shape_type_dir(ShapeType2,Dirs2),

one_fti(VM,jumps(ShapeType1)):-
  Option = jumps(ShapeType1), copy_term(Option,OptionC),!,
  Sofar = VM.objs,
  Grid = VM.grid,
  
  selected_from(Sofar,ShapeType1,ShapeType1,HV1,HV2,SofarLess),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), any_gpoint(HV2,C-P2),
  % skip over MP
  is_adjacent_point(P0,Dir,P1), is_adjacent_point(P1,Dir,MP), is_adjacent_point(MP,Dir,P2),is_adjacent_point(P2,Dir,P3),
  any_gpoint(HV1,_-P0), any_gpoint(HV2,_-P3),
  %Grid = VM.grid,
  %get_color_at(MP,Grid,_MC),
  ignore(once((get_color_at_point(Grid,MP,MC),is_color(MC));MC=C)),
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  % TODO: HACK WE MIGHT NOT WANT TO STEAL THE POINT?   
  %once(select(MC-MP,Points,RestPoints);Points=RestPoints),  
  combine_2objs(VM,HV1,HV2,[MC-MP],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  assumeAdded(VM,Combined),
  cycle_back_in(VM,OptionC).
  

% =====================================================================
is_fti_step(extends).
% =====================================================================
extends(ShapeType1,VM):-
  Option = extends(ShapeType1), copy_term(Option,OptionC),!,
  select(HV1,VM.objs,SofarLess),isz(HV1,ShapeType1),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,MP),
  Points = VM.lo_points,
  select(MC-MP,Points,ScanPoints),
  \+ (is_adjacent_point(P1,_,P2), is_adjacent_point(P2,_,MP),any_gpoint(HV1,_-P2)),
  all_individuals_near(VM,Dir,Option,C,[MC-MP],ScanPoints,NextScanPoints,IndvPoints),  
  combine_2objs(VM,HV1,[],IndvPoints,[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  set(VM.lo_points)=NextScanPoints,
  assumeAdded(VM,Combined),
  cycle_back_in(VM,OptionC).

    combine_2objs(VM,HV1,HV2,NewPoints,IPROPS,Combined):-
      globalpoints(HV1,GP1), globalpoints(HV2,GP2),    
      % indv_props_list(HV1,Props1),indv_props_list(HV2,Props2),
      
      append_sets([GP1,GP2,NewPoints],GPoints),      
      Props1=[],Props2=[],flatten_sets([Props1,Props2,IPROPS],Info),
      meets_indiv_criteria(VM,Info,GPoints),
      make_indiv_object(VM,Info,GPoints,Combined).


one_fti(VM,Option):- 
  ( Option \== lo_dots), 
  ignore((exceeded_objs_max_len(VM), !, set(VM.objs_max_len) is VM.objs_max_len + 2)),  
  one_ifti(VM,Option),!.

one_ifti(VM,Shape):- 
   \+ exceeded_objs_max_len(VM),!,
   ( Shape \== lo_dots),
   is_thing_or_connection(Shape),
   find_one_individual(Shape,Indv,VM),
   globalpoints(Indv,IndvPoints),
   meets_indiv_criteria(VM,Shape,IndvPoints),
   assumeAdded(VM,Indv),!,
   ignore(one_ifti(VM,Shape)).

one_ifti(_VM,Option):- is_thing_or_connection(Option),!.

is_thing_or_connection(S):- no_repeats(S,is_thing_or_connection1(S)).
is_thing_or_connection1(Option):-allowed_dir(Option,_Dir).
is_thing_or_connection1(connects(_,_)).
is_thing_or_connection1(merge_shapes(_,_)).
is_thing_or_connection1(jumps(_,_)).


find_one_individual(Option,Obj,VM):- find_one_ifti3(Option,Obj,VM),!.
%find_one_individual(Option,Obj,VM):- find_one_ifti2(Option,Obj,VM),!.

include_only_between(HS,HE,VS,VE,IndvPoints0,IndvPoints):-
   include(w_in_box(HS,HE,VS,VE),IndvPoints0,IndvPoints).

w_in_box(HS,HE,VS,VE,CPoint):- hv_c_value(CPoint,_,H,V),between(HS,HE,H),between(VS,VE,V),!.
  

find_one_ifti3(Option,Obj,VM):- 
    Points = VM.lo_points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), allowed_color(Option,C1), % non_free_fg(C2), % \+ is_black(C2),
    ok_color_with(C1,C2), %ScanPoints = Rest1, %maybe_multivar(C2), 
    is_adjacent_point(HV1,Dir,HV2),
    allowed_dir(Option,Dir), adjacent_point_allowed(C2,HV1,Dir,HV2), select(C2-HV2,Rest0,Rest1),
    FirstTwo = [C1-HV1,C2-HV2], points_allowed(VM,Option,FirstTwo),
    %i3(Option,Dir,Rest1,FirstTwo,ScanPoints,PointsFrom),
    FirstTwo=PointsFrom,
    Rest1 = ScanPoints,
    points_allowed(VM,Option,PointsFrom),
    all_individuals_near(_SkipVM,Dir,Option,C1,PointsFrom,ScanPoints,NextScanPoints,IndvPoints0),!,

    include_only_between(1,VM.h,1,VM.v,IndvPoints0,IndvPoints),

    length(IndvPoints,Len),Len>=2,!,
    make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth((ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.lo_points) = NextScanPoints,
  assumeAdded(VM,Obj),
  cycle_back_in(VM,OptionC),!.

find_one_ifti2(Option,Obj,VM):- 
    Points = VM.lo_points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
  select(C-HV,Points,Rest0), allowed_color(Option,C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(VM,Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints),!,
  make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType)),birth(i2(ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.lo_points) = NextScanPoints,
  assumeAdded(VM,Obj),
  cycle_back_in(VM,OptionC).

/*
find_one_ifti3(Option,Obj,VM):- 
    Points = VM.lo_points,
    %shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), allowed_color(Option,C1), % non_free_fg(C2), % \+ is_black(C2),
    ok_color_with(C1,C2),ok_color_with(C1,C3),
    is_adjacent_point(HV1,Dir,HV2),
    \+ \+ allowed_dir(Option,Dir),
    adjacent_point_allowed(C2,HV1,Dir,HV2),
    select(C2-HV2,Rest0,Rest1),
    points_allowed(VM,Option,[C1-HV1,C2-HV2]),!,
    %ScanPoints = Rest1,
    ((adjacent_point_allowed(C3,HV2,Dir,HV3),select(C3-HV3,Rest1,ScanPoints));
     (allowed_dir(Option,Dir2),Dir2\=Dir, adjacent_point_allowed(C3,HV2,Dir2,HV3),select(C3-HV3,Rest1,ScanPoints))),    
    %maybe_multivar(C2), 
    point_allowed(VM,Option,C3-HV3),!,
    all_individuals_near(VM,Dir,Option,C1,[C1-HV1,C2-HV2,C3-HV3],ScanPoints,NextScanPoints,IndvPoints),
    !, length(IndvPoints,Len),Len>=2,
    make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.lo_points) = NextScanPoints,
  assumeAdded(VM,Obj),
  cycle_back_in(VM,OptionC),!.
*/
%meets_indiv_criteria(_VM,_Info,[C-P1,C-P2]):- is_adjacent_point(P1,_Dir,P2),!,fail.
meets_indiv_criteria(VM,ShapeL,PointL):- is_list(ShapeL),!,forall(member(Shape,ShapeL),meets_indiv_criteria(VM,Shape,PointL)).
meets_indiv_criteria(_VM,Shape,PointL):- nop(( points_allowed(PointL,Shape,PointL))).

points_allowed(Nil,_Shape,_Point):- \+ Nil\=[], !.
points_allowed(VM,Shape,PointL):- is_vm(VM),!,points_allowed(VM.start_points,Shape,PointL).
points_allowed(VM,ShapeL,PointL):- is_list(ShapeL),member(Shape,ShapeL),points_allowed(VM,Shape,PointL),!.
points_allowed(VM,Shape,PointL):- my_maplist(point_allowed0(VM,Shape),PointL).


point_allowed(Nil,_Shape,_Point):- var(Nil),!.
point_allowed(Nil,_Shape,_Point):- \+ Nil\=[], !.
point_allowed(VM,Shape,Point):- is_vm(VM),!,point_allowed(VM.start_points,Shape,Point).
point_allowed(VM,ShapeL,Point):- is_list(ShapeL),member(Shape,ShapeL),point_allowed(VM,Shape,Point),!.
point_allowed(VM,Shape,Point):- point_allowed0(VM,Shape,Point).

point_allowed0(From,Shape,C-HV):- 
  findall(Dir, (is_adjacent_point(HV,Dir, HV2),\+ is_diag(Dir), member(C-HV2,From)),NonDiags),
  findall(Dir, (is_adjacent_point(HV,Dir, HV2), is_diag(Dir), member(C-HV2,From)),Diags),
  point_dirs_allowed(Shape,NonDiags,Diags).

point_dirs_allowed(nsew,[],_):-!,fail. % dont allow stragglers
point_dirs_allowed(diamonds,NonDiags,_):- !, NonDiags==[].
point_dirs_allowed(nsew,[_],[_]):-!,fail. % dont allow stragglers
point_dirs_allowed(_,_,_).

/*
find_one_ifti3(Option,Obj,VM):- 
    Points = VM.lo_points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), allowed_color(Option,C1), % non_free_fg(C2), % \+ is_black(C2),
    ok_color_with(C1,C2), %ScanPoints = Rest1, %maybe_multivar(C2), 
    is_adjacent_point(HV1,Dir,HV2),
    allowed_dir(Option,Dir), adjacent_point_allowed(C2,HV1,Dir,HV2), select(C2-HV2,Rest0,Rest1),
    FirstTwo = [C1-HV1,C2-HV2], points_allowed(VM,Option,FirstTwo),
    i3(Option,Dir,Rest1,FirstTwo,ScanPoints,PointsFrom),
    points_allowed(VM,Option,PointsFrom),
    all_individuals_near(VM,Dir,Option,C1,PointsFrom,ScanPoints,NextScanPoints,IndvPoints),
    make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth((ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.lo_points) = NextScanPoints,
  assumeAdded(VM,Obj),
  cycle_back_in(VM,OptionC),!.


i3(Option,Dir,Rest1,[C1HV1,C2-HV2],ScanPoints,[C1HV1,C2-HV2,C3-HV3]):-  
  ok_color_with(C2,C3), 
  ((adjacent_point_allowed(C3,HV2,Dir, HV3),select(C3-HV3,Rest1,ScanPoints));
   (allowed_dir(Option,Dir2),Dir2\=Dir,adjacent_point_allowed(C3,HV2,Dir2,HV3),select(C3-HV3,Rest1,ScanPoints))).

i3(Option,_Dir,ScanPointsRest1,PointsFromFirstTwo,ScanPointsRest1,PointsFromFirstTwo):- Option\==diamonds.
*/
/*
*/

allowed_color(_Option,C):- nonvar(C),!.
allowed_color(_Option,C):- \+ free_cell(C),!.
allowed_color(_Option,C):- C==black,!.
allowed_color(_Option,C):- C==zero,!.

point_groups_by_color(Option,[IndvPoints|Groups],Points,Rest):-    
    select(C-HV,Points,Rest0), allowed_color(Option,C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
    all_individuals_near(_,Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), !,
    point_groups_by_color(Option,Groups,NextScanPoints,Rest).
point_groups_by_color(_Option,[],Points,Points).

shape_min_points(VM,Shape,MinShapeO):- MS = VM.objs_min_mass, number(MS), length(MinShape,MS),
  !,append(MinShape,_,MinShapeO),!,shape_min_points0(Shape,MinShapeO).
shape_min_points(_VM,Shape,MinShapeO):-shape_min_points0(Shape,MinShapeO).


%shape_min_points0(Kind,{_,_]):-!,fail.
%shape_min_points0(Kind,Points):- nonvar(Points),shape_min_points0(Kind,PointsO),!,
%  freeze(Points,PointsO = Points).
% shape_min_points0(colormass,[_,_,_,_,_|_]):-!.
%shape_min_points0(nsew,[_,_,_]):-!,fail.
%shape_min_points0(nsew,[_,_|_]):-!.
%Rshape_min_points0(diamonds,[_,_|_]):-!.
%shape_min_points0(_,[_,_|_]).
shape_min_points0(_,_).
%  shape_min_points(VM,_,_).

:- luser_default(color_mode,monochrome).

ok_color_with(C1,C1).
/*
ok_color_with(C1,C2):- my_assertion(is_color(C1)), 
  (plain_var(C2)->freeze(C2,ok_color_with(C1,C2));
    (luser_getval(color_mode,monochrome) -> 
     (is_fg_color(C1)-> is_fg_color(C2);is_bg_color(C2)) 
     ; (\+ (C1 \= C2)))).
*/

sameglobalpoints(Points,IndvC,NextScanPoints,IndvC):-
  globalpoints(IndvC,GPoints),
  remove_gpoints(GPoints,Points,NextScanPoints).

remove_gpoints([],Rest,Rest).
remove_gpoints([GPoint|GPoints],Points,Rest):- select(GPoint,Points,Mid),remove_gpoints(GPoints,Mid,Rest).

get_neighbors(From,P1,Found,HV):- 
  findall(f(Dir,C-HV2), (is_adjacent_point(HV,Dir, HV2),member(C-HV2,From),call(P1,C)),Found).

has_2neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),member(C-HV3,Rest2),!.

has_3neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),rot_left_45(NW,W),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),member(C-HV4,Rest3),!.

has_4neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),rot_left_45(NW,W),rot_left_45(W,SW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),select(C-HV4,Rest3,Rest4),
    is_adjacent_point(HV,SW, HV5),member(C-HV5,Rest4),!.

min_neighbours(0,_,_,_).
min_neighbours(Count,Dir,C,Rest,HV):- maybe_multivar(C),     
    is_adjacent_point(HV,Dir, HV2),member(C-HV2,Rest),
    rot_left_45(Dir,NW), CountDown is Count-1, min_neighbours(CountDown,NW,C,Rest,HV).



color_of(HV,TL,TLC):- t_l:id_cells(_ID,Points), is_adjacent_point(HV,TL,TLHV),member(TLC-TLHV,Points).

colors_block_diag(C,TLC,TRC,_C2):- get_bgc(Zero), TLC==TRC, TRC\==Zero, C \== TRC, non_free_fg(TRC).

unused_filtered_point(C,HV):- maybe_multivar(C), t_l:id_cells(_ID,Points),% select(_-HV,Points,Rest), 
  findall(Dir-HV2,(adjacent_point_allowed(C,HV,Dir,HV2),member(C-HV2,Points)),Used),
  findall(Dir-HV2,(adjacent_disallowed(C,HV,Dir,HV2),member(C-HV2,Points)),Unused),
  shape_has_filtered_use(C,Used,Unused),
  %u_dmsg(shape_has_filtered_use(C,HV,Used,Unused)),
  !.

shape_has_filtered_use(_,[],_Unused).
shape_has_filtered_use(C,[_],_):- shape_filter(C,nsew),!.


adjacent_groups(C1,Grp1,Dir,Grp2):- member(_-P1,Grp1),ok_color_with(C1,C2),member(C2-P2,Grp2),is_adjacent_point(P1,Dir,P2).
adjacent_point(C,HV,HV2):- adjacent_point_allowed(C,HV,_Dir,HV2).

adjacent_point_allowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), 
   (is_diag(Dir) -> freeze(C, \+ is_bg_color(C)) ; true).
  %shape_filter(C,Shape),allow_dir_list(Shape,DirS),member(Dir,DirS).
%adjacent_point_allowed(_,C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS),member(Dir,DirS).
adjacent_disallowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS), \+ member(Dir,DirS).


all_individuals_near(_VM,_Dir,_Options,_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(VM,Dir,Options,C1,Indv,ScanPoints,NewScanPoints,NewSet):-
   ok_color_with(C1,C2),
   individuals_near(VM,Dir,Options,C2,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (my_append(Indv,New,IndvNew),
        all_individuals_near(VM,Dir,Options,C1,IndvNew,NextScanPoints,NewScanPoints,NewSet))),!.

individuals_near(_VM,_Dir,_Options,_C,_From,[],[],[]):-!.
individuals_near(VM,Dir,Options,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- 
  nearby_one(Dir,Options,C,E,From)->point_allowed(VM,Options,E),!,
  individuals_near(VM,Dir,Options,C,[E|From],ScanPoints,Nears,NextScanPoints),!.

individuals_near(VM,Dir,Options,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(VM,Dir,Options,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(_Dir_,Options,C1,C2-E,List):- allowed_dir(Options,Dir), adjacent_point_allowed(C1,E2,Dir,E), 
  member(C2-E2,List),ok_color_with(C1,C2).

check_minsize(_,I,I):-!.
check_minsize(_,[],[]):-!.
check_minsize(Sz,[I|IndvS],[A,B|IndvSO]):- mass(I,2),globalpoints(I,[A,B]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[A,B,C|IndvSO]):- mass(I,3),globalpoints(I,[A,B,C]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[I|IndvSO]):- check_minsize(Sz,IndvS,IndvSO).

/*
meets_size(_,Points):- mass(Points,1).
meets_size(_,Points):- mass(Points,2),!,fail.
meets_size(_,Points):- mass(Points,3),!,fail.
meets_size(_,Points):- mass(Points,4).
*/
meets_size(Len,Points):- mass(Points,L),!,L>=Len.

remove_bgs(IndvS,IndvL,BGIndvS):- partition(is_bg_indiv,IndvS,BGIndvS,IndvL).

finish_grp(C,Grp,Point2,Dir,Rest,NewGroup,RRest):- 
   \+ (is_diag(Dir),is_bg_color(C)),
   is_adjacent_point(Point2,Dir,Point3),
   single_point(C-Point3,Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).


single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  single_point0(C-Point,IndvS,Rest1).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
 fail, select(obj(I),IndvS,Rest1), fail, % round 2
  globalpoints(obj(I),[C-Point]),
  nonvar_or_ci(C).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select([C-Point],IndvS,Rest1),
  nonvar_or_ci(C).

single_point0(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(C-Point,IndvS,Rest1),
  nonvar_or_ci(C).


/*
unraw_inds2(VM,Options,IndvS,IndvO):- fail,
   largest_first(mass,IndvS,Indv),
   reverse(Indv,IndvR), IndvR\=@=IndvS,
   unraw_inds2(VM,Options,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(VM,Options,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,iz(diagonal),C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  % minimum 4 findall(C-CP,member(C-CP,NewGroup),LL),LL=[_,_,_,_|_],
  unraw_inds2(VM,Options,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(VM,Options,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),fail,
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,iz(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(VM,Options,[NewGroup|RRestO],IndvO).



unraw_inds2(VM,Options,IndvS,IndvO):-  %fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),
  single_point(C2-Point2,Rest1,Rest),fail,
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(VM,Options,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(VM,Options,IndvS,IndvO):- fail,
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(VM,Options,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(VM,Options,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Dir,Options,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(VM,Options,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_VM,_,IndvS,IndvS).




merge_indivs(IndvA,IndvB,BetterA,BetterB,BetterC):-
  my_append(IndvA,IndvB,IndvSU),list_to_set(IndvSU,IndvS),
  smallest_first(mass,IndvS,IndvC),
  merge_indivs_cleanup(IndvA,IndvB,IndvC,BetterA,BetterB,BetterC),!.

merge_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  my_maplist(length,[IndvA,IndvB,IndvC],Rest),
  u_dmsg(len=Rest),fail.
merge_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  merge_a_b(A,B,AA),
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
  merge_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
merge_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
merge_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  obj_to_oid(B,OID),
  setq(A,oid(OID),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(progress(shared_object(GlyphB->GlyphA))); 
    (progress(same_object(GlyphA,GlyphB,How))))).






individuation_macros(subshape_in_object, complete).


individuation_macros(out, current).
individuation_macros(in, current).

individuation_macros(train_mono_in_in, current).
individuation_macros(train_mono_in_out, current).
individuation_macros(train_mono_out_out, current).

individuation_macros(in_in, current).
individuation_macros(out_in, current).
individuation_macros(out_out, current).
individuation_macros(in_out, current).

individuation_macros(current,Mode):- get_indivs_mode(Mode), \+ (Mode==current;individuation_macros(Mode, current)),!.
individuation_macros(current,complete).

% if there are 10 or less of a color dont group the whole color (It/they must be special points)
individuation_macros(by_color, X):-
   findall(by_color(10,Color),enum_fg_colors(Color),X).

individuation_macros(force_by_color, X):-
   findall(by_color(1,Color),enum_fg_colors(Color),X).

individuation_macros(subshape_in_object, [
   subshape_both(h,colormass),   
   %progress,
   nsew, % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   by_color, % any after this wont find individuals unless this is commented out
   end_of_macro]).

individuation_macros(subshape_main, [
   maybe_glyphic,
   subshape_both(v,nsew),   
   by_color,
   %alone_dots(lte(5)),
   %progress,
   %nsew % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   %by_color % any after this wont find individuals unless this is commented out
   end_of_macro]).

% never add done to macros
individuation_macros(subshape_both(HV,CM), 
 [
  % glean_grid_patterns,
   %shape_lib(hammer), % is a sanity test/hack
   %squire,
   CM,
   alone_dots(lte(5)),
   hv_line(HV), dg_line(D), dg_line(U), hv_line(VH),
   diamonds, nsew, colormass, 
   %show_neighbor_map,
   %indiv_grid_pings,
   %+recalc_sizes,
   connects(dg_line(_),dg_line(_)),
   connects(hv_line(_),dg_line(_)),
   connects(hv_line(_),hv_line(_)),
   jumps,% run the "jumps" macro
   %merge_shapes(Z,Z), % merge lines into square
   merge_shapes(hv_line(_),hv_line(_)),
   merge_shapes(dg_line(_),dg_line(_)),
   %point_corners,
   %alone_dots(lte(5)),
   %connects(X,X)
   end_of_macro]):- hv_vh(HV,VH),du_vh(U,VH),du_ud(U,D).



individuation_macros(subshape_main2, [
   maybe_glyphic,
   subshape_both(v,nsew),   
   by_color,
   %alone_dots(lte(5)),
   %progress,
   %nsew % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   %by_color % any after this wont find individuals unless this is commented out
   end_of_macro]).

% never add done to macros
individuation_macros(all_lines,
 [
  % glean_grid_patterns,
   %shape_lib(hammer), % is a sanity test/hack
   %squire,
   %alone_dots(lte(5)),
   hv_line(HV), dg_line(D), dg_line(U), hv_line(VH),
   %connects(X,X)
   end_of_macro]):- hv_vh(HV,VH),du_vh(U,VH),du_ud(U,D),!.

hv_vh(h,v).
hv_vh(v,h).
du_vh(d,h).
du_vh(u,v).
du_ud(d,u).
du_ud(u,d).

individuation_macros(jumps,
  [ %progress, 
    jumps(hv_line(h)), % multicolored lines
    jumps(hv_line(v)),
    jumps(dg_line(d)), % multicolored diagonals
    jumps(dg_line(u)), 
    jumps(jumps(hv_line(v))),
    jumps(jumps(hv_line(h))) % joined jumps
    ]).  

individuation_macros(common_shape_lib, [                     
                     shape_lib(strict,rect_squares),
                     shape_lib(regular,rect_squares)
                     %shape_lib(loose,squares)
                     %shape_lib(removed),
                     %shape_lib(in), 
                      %shape_lib(out), % random objects learned from other passes
                      %shape_lib(pair) % random objects learned from this pass
]).

individuation_macros(std_shape_lib_lean, [                     
                     shape_lib(as_is),
                     shape_lib(hammer),
                     shape_lib(removed),
                     shape_lib(in), 
                      shape_lib(out), % random objects learned from other passes
                      shape_lib(pair) % random objects learned from this pass
]).
individuation_macros(std_shape_lib, [
    shape_lib(out), 
    shape_lib(as_is),
    shape_lib(noise), % data found we needed to delay analysis on
    shape_lib(intruder), % anything that stood out in the past 
   %shape_lib(added), 
    shape_lib(in), 
    shape_lib(out), % random objects learned from other passes
    shape_lib(pair), % random objects learned from this pass
    shape_lib(hammer), % cheater objects for hammer sanity test/hack
    shape_lib(l_shape), % random objects shown by douglas to look for
    shape_lib(human)]). % random objects shown by humans to look for


% noit doing press-pass presently
individuation_macros(pre_pass, [  standard,complete]).

% Bring the "cognitive load" down by culling tiny objects
individuation_macros(reduce_population, [
                     when(len(objs)>40,colormass_merger(3)),
                     when(len(objs)>40,colormass_merger(4)),
                     when(len(objs)>40,colormass_merger(5)),
                     when(len(objs)>40,release_objs_lighter(3)),
                     when(len(objs)>40,release_objs_lighter(4)),
                     when(len(objs)>40,release_objs_lighter(5)),
                     when(len(objs)>40,colormass_merger(6)),
                     when(len(objs)>40,release_objs_lighter(6)),

                     when(len(objs)>20,colormass_merger(7)),

                     when(len(objs)>20,colormass_merger(10)),

                     when(len(objs)>20,colormass_merger(15)),
                     call(true)]).

% about to finish make sure we grab what we can
individuation_macros(altro, [
    reduce_population,
    remove_used_points,
    when((len(points)=<ThreeO),alone_dots(lte(5))),    
    when((len(points)>ThreeO),by_color)]):- the_big_three_oh(ThreeO).

individuation_macros(some_leftovers, [
     recompute_points,
     diamonds,
     colormass,
     by_color(1),by_color(0)]).


individuation_macros(S,Some):- sub_individuation_macro(S,Some).

sub_individuation_macro(S,Some):-
  individuator(S,From),
  flatten([
    % [reset_points],
    From],Some).


include_black(_VM):- set_bgc(wbg).

% 1204
%individuation_macros(complete, [parallel,done]).





individuation_macros(i_complete_generic, SetO):- fail,
%individuation_macros(complete, ListO):-  \+ test_config(indiv(_)),!, %reset_points, %sub_individuate(force_by_color,subshape_both), %TODO %
  %findall(drops_as_objects(From),(individuator(From,_)),ListM),
  findall(save_as_obj_group(From),individuator(From,_),ListS),
  flatten([  
   whole,
   %save_as_obj_group
   ListS,
   %find_grids,   
   %[i_columns,i_rows],
   %[gather_texture],
   %ListM,ListS,
   %save_as_obj_group(i_hybrid_shapes),
   %find_hybrid_shapes,
   %save_as_obj_group(diamonds),
   gather_cached,
   
   %[pointless([sub_indiv([save_as_obj_group(force_by_color),save_as_obj_group(i_colormass),save_as_obj_group(i_nsew)])])],
   %do_ending,

   %only_proportional_mass,
   []],ListO),list_to_set(ListO,SetO).
%use_individuator(Some):- individuator(Some,_).

individuation_macros(i_columns,[when(get(h)=<5,all_columns),when(get(h)>5,some_columns)]). %:- \+ doing_pair.
individuation_macros(i_rows,[when(get(h)=<5,all_rows),when(get(v)>5,some_rows)]). %:- \+ doing_pair.
individuation_macros(i_maybe_glypic,[maybe_glyphic]).

fast_simple :- true.

%individuator(i_hammer,[shape_lib(hammer),do_ending]).
%

individuation_macros(i_pbox,[pbox_vm_special_sizes]).
individuation_macros(i_mono_colormass,[fg_shapes(colormass),generic_nsew_colormass,do_ending]).
%individuator(i_colormass,[subshape_both(v,colormass), maybe_lo_dots]).
individuation_macros(i_mono_nsew,[fg_shapes(nsew)]).


%individuator(i_omem_points,[indv_omem_points]).
individuator(i_mono,[save_as_obj_group([i_mono_colormass])]).
individuator(i_omem,[indv_omem_points]).
%individuator(i_pbox,[whole,pbox_vm]).
individuator(i_maybe_glypic,[maybe_glyphic]). %:- \+ doing_pair.
individuator(i_repair_patterns,[maybe_repair_in_vm(find_symmetry_code)]).
%individuator(i_nsew,[gather_cached]). %,nop((maybe_alone_dots_by_color(lte(20))))]).
individuator(i_pbox_vm_special_sizes,[gather_cached,pbox_vm_special_sizes]).
%% OMEM SUPER SIMPLE individuator(i_subtractions,[fg_subtractions([i_nsew])]).
%% OMEM SUPER SIMPLE individuator(i_intersections,[fg_intersections([i_nsew])]).
/*
individuator(i_nsew,[nsew]).
% NEW SYSTEM individuator(i_subtractions,  [fg_subtractions([whole,save_as_obj_group(i_nsew),save_as_obj_group(i_mono_nsew)])]).
% NEW SYSTEM individuator(i_intersections,[fg_intersections([whole,save_as_obj_group(i_nsew),save_as_obj_group(i_mono_nsew)])]).
% NEW SYSTEM individuator(i_colormass,[colormass]).
individuator(i_alone_dots,[maybe_alone_dots_by_color(lte(40)),leftover_as_one]).
%individuator(i_diags,[do_diags]).
%individuator(i_by_color,[by_color(0), by_color(0,wbg), by_color(0,fg),  reset_points, by_color(1,black),by_color(1,bg), by_color(1,fg),/* ,*/[]]).
individuator(i_by_color,[by_color(0,wbg), by_color(1,black),by_color(1,bg), by_color(1,fg)]).
individuator(i_hybrid_shapes,[find_hybrid_shapes]).
individuator(i_repair_patterns,[maybe_repair_in_vm(find_symmetry_code)]).
*/

individuation_macros(i_repair_patterns_f,[repair_in_vm(find_symmetry_code)]).
individuation_macros(do_diags,[ /*dg_line(d), dg_line(u), */ diamonds]).


%individuator(i_abtractions,[fg_abtractions([save_as_obj_group(i_mono_nsew),save_as_obj_group(i_nsew)])]).

/*

individuator(i_omem_points,[indv_omem_points]).
individuator(i_maybe_glypic,[maybe_glyphic]). %:- \+ doing_pair.
individuator(i_subtractions,  [fg_subtractions([whole,save_as_obj_group(i_nsew),save_as_obj_group(i_mono_nsew)])]).
individuator(i_intersections,[fg_intersections([whole,save_as_obj_group(i_nsew),save_as_obj_group(i_mono_nsew)])]).
individuator(i_colormass,[colormass]).
individuator(i_alone_dots,[maybe_alone_dots_by_color(lte(40)),leftover_as_one]).
individuator(i_nsew,[pbox_vm,maybe_alone_dots_by_color(lte(20)),nsew,diamonds,colormass]).
individuator(i_diag,[diamonds,maybe_alone_dots_by_color(lte(20)),colormass]).
individuator(i_pbox,[i_nsew,leftover_as_one]).
%individuator(i_diags,[do_diags]).
individuator(i_by_color,[by_color(0), by_color(0,wbg), by_color(0,fg), 
  reset_points, by_color(1,black),by_color(1,bg), by_color(1,fg),/* ,*/[]]).
%individuator(i_sub_pbox,[sub_individuate(pbox_vm)]).
%individuator(i_pbox,[maybe_pbox_vm,i_colormass]).
individuator(i_mono_colormass,[fg_shapes(i_colormass)]).
individuator(i_mono_nsew,[fg_shapes(i_nsew)]).
individuator(i_hybrid_shapes,[find_hybrid_shapes]).
individuator(i_repair_patterns,[maybe_repair_in_vm(find_symmetry_code)]).
individuation_macros(i_repair_patterns_f,[repair_in_vm(find_symmetry_code)]).
*/

/*

*/
%individuation_macros(i_repair_repeats,[repair_in_vm(repair_repeats(Black))]):- get_black(Black).
/*
individuator(i_nsew,[subshape_both(h,nsew), maybe_lo_dots]).
%individuator(i_maybe_glypic,[whole]):- doing_pair.
individuator(i_mono,[save_as_obj_group(mono_shapes([subshape_both(h,nsew)])),
                          save_as_obj_group(mono_shapes([subshape_both(v,colormass)]))]).

*/
/*
individuator(i_mono_nsew,
 [sub_individuate(
    mono_shapes([subshape_both(h,nsew)]),
   ([save_as_obj_group(force_by_color),save_as_obj_group(i_colormass)])),do_ending]).
*/
%individuator(i_subobjs,[sub_indiv([save_as_obj_group(force_by_color),save_as_obj_group(i_colormass)])]).

%individuator(i_bg_nsew,[mono_shapes(subshape_both(h,nsew))]).
%individuator(i_mono_colormass,[fg_shapes([subshape_both(v,colormass)])]).
%individuator(i_fgbg,[by_color(1,bg), by_color(1,fg),do_ending]).
%individuator(i_diamonds,[subshape_both(h,diamonds), alone_dots(lte(5)), maybe_lo_dots]).
%individuator(i_decolorize,[subshape_both(v,decolorize), maybe_lo_dots]).
%individuator(i_monochrome,[subshape_both(h,into_monochrome), maybe_lo_dots]).
%individuator(i_mono_nsew,[decolorize,subshape_both(v,nsew), maybe_lo_dots]).
%individuator(i_mono_mass,[into_monocnhrome,subshape_both(v,colormass), maybe_lo_dots]).
% % %%  
%individuator(i_shapes,[subshape_both(h,std_shape_lib_lean),do_ending]).
% % %%  
%
%individuator(i_shapelib,[subshape_both(h,shape_lib(pairs)), alone_dots(lte(5)), maybe_lo_dots]).
% % %%  
%individuator(i_repair_patterns,[fourway]).
% % %%    individuator(i_as_is,[shape_lib(as_is)]).
% % %%    individuator(i_common,[common_shape_lib,do_ending]).
  %   when(len(objs)>=70,keep_points(whole)),
  %TODO when(len(objs)<70,when(len(points)<50,glyphic)),
  %do_ending,
  %complete_broken_lines,
  %complete_occluded,
find_symmetry_code(VM,Grid,RepairedResult,Code):- 
  if_deepen_arc(find_symmetry_code1(VM,Grid,RepairedResult,Code)),!.

%if_deepen_arc(_):- !, fail.
if_deepen_arc(Goal):- once(Goal).
%never_repair_grid(Grid):- is_grid_symmetricD(Grid),!.
never_repair_grid(Grid):- get_current_test(TestID),kaggle_arc_io(TestID,_,out,G),G==Grid.

find_symmetry_code1(_VM,Grid,RepairedResult,Code):-  never_repair_grid(Grid),!,fail,RepairedResult=Grid,Code=[sameR].
find_symmetry_code1(VM,Grid,RepairedResult,Code):- 
   % \+ is_grid(VM.target_grid),
    copy_term(Grid,Orig),
    ignore((kaggle_arc_io(TestID,ExampleNum,in,Grid),
            kaggle_arc_io(TestID,ExampleNum,out,Out))),
    ignore((Out  = VM.target_grid)),

   ID = VM.id,
   %atrace,
   ((test_symmetry_code(Grid,GridS,RepairedResult,Code)
      *-> 
       (if_t(GridS\==[],print_grid(test_RepairedResult,GridS)),
        if_t(Orig\==Grid,print_ss(green,Orig,orig(ID),_,Grid,altered(ID))),
        print_ss(green,Orig,gridIn(ID),_,RepairedResult,repaired(ID)),        
        if_t(is_grid(Out),
          if_t(RepairedResult\==Out,
            (print_ss(yellow,RepairedResult,unexpected_repairedResult(ID),_,Out,expected(ID)),
            arcdbg_info(yellow,mismatched(symmetry_code(ID,Code)))))),
        arcdbg_info(green,success(symmetry_code(ID,Code))))
    ;
    ((var(Out)->Out=[[_]];true),
      print_ss(red,Orig,gridIn(ID),_,Out,out(ID)),
      arcdbg_info(red,none_found(symmetry_code(ID))),!,fail))).

find_symmetry_code1(VM,Grid,RepairedResult,Steps):-
   \+ is_grid(VM.target_grid),!, repair_fourway(VM,Grid,RepairedResult,Steps).

%individuation_macros(complete, [parallel]).
%individuation_macros(complete, [complete2]).
% the typical toplevel indivduator
individuation_macros(complete2, [
    %maybe_repair_in_vm(repair_repeats),
    shape_lib(as_is),
    fourway,
    find_colorfull_idioms,
    maybe_glyphic,
    if_done,
    complete_broken_lines,
    complete_occluded,
    maybe_1_3rd_mass,
    %shape_lib(as_is),    
    %nsew,
    %colormass,    
    standard,%colormass_merger(3), % call the standard things done in most indiviguators    
    point_corners,
    reduce_population, % @TODO DISABLED FOR TESTS    %altro,
    colormass_subshapes, % find subshapes of the altro
    %when((colors_cc(i.lo_points,Cs),len(Cs)<2),alone_dots(lte(5))), % any after this wont find individuals unless this is commented out
    colormass_merger(2),
    when((len(points)=<ThreeO),alone_dots(lte(5))),
    alone_dots(lte(5)),
    %leftover_as_one, % any after this wont find individuals unless this is commented out    
   done % stop processing
 ]):- the_big_three_oh(ThreeO).

% the standard things done in most indiviguators
individuation_macros(standard, [
    %fourway, % find fold patterns 
    %recalc_sizes,
    std_shape_lib, % stuff that was learned/shown previously
   +max_learn_objects(colormass,ThreeO),
   +max_learn_objects(nsew,ThreeO),
   +max_learn_objects(hv_line(_),ThreeO),
   +max_learn_objects(dg_line(_),ThreeO),
    %nsew,
    %+recalc_sizes, % blobs of any colorlesspoints that are the equal color  
    % @TODO DISABLED FOR TESTS   colormass_subshapes, % subdivide the color masses .. for example a square with a dot on it
    subshape_main, % macro for sharing code with "subshape_in_object"
    connects(jumps(X),jumps(X)), % connected jumps    
    % merge_shapes(Z,Z), % merge objects of identical types (horizontal lines become filltype(solid) squares)   
    %do_ending,    
    end_of_macro]):- the_big_three_oh(ThreeO).

individuation_macros(defaults, [ complete ]).

individuation_macros(unused, [
  detect(_VM_), % makes an media(image) detectable
  detect(_Group_), % makes several objects and images availble 
  done, %terminates object detection
  ls, % shows current director contents
  progress, % show maroexpansion process
  shape_lib(cheat), % library of all shapes (for debugging)
  shape_lib(in), % objects that got removed in this pair
  use_reserved, % objects that were already found dont find again
  - progress, % turn off a detector option
  + progress, % turn on a detector option
  stype(solid(nsew)), % chat that looks for solid rectanglez
  
  %polygons,%shape_lib(nsew), %shape_lib(all), %shape_lib(hammer),
  
  % colormass, %hv_line(v), hv_line(h), %dg_line(u),dg_line(d), %CS,
  all
  % line(_),dg_line(_), % release_points, all, %into_single_hidden,oldway %retain(filltype(solid)(nsew)), % shapes, %into_single_hidden,
  ]). 


:- include(kaggle_arc_footer).



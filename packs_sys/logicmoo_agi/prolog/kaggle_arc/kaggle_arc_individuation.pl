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

individuation_macros(complete, ListO):- im_complete(ListC),
   flatten([ListC,do_ending],ListM),
   list_to_set(ListM,ListO),!.

individuation_macros(do_ending, [
  %find_edges,
  % find_contained_points, % mark any "completely contained points"
 %combine_same_globalpoints, % make sure any objects are perfectly the equal part of the media(image) are iz(flag(combined))
 %keep_only_shown(1),
 %remove_if_prop(and(cc(bg,1))),
 %combine_if_prop(and(cc(bg,1),)),
 %remove_if_prop(and(iz(stype(dot))])),
 %combine_same_globalpoints,
 reset_points,
 %gather_cached,
 remove_used_points,
 named_grid_props(post_indiv),
 find_relations,
 remove_dead_links,
 %find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
 %find_subsumes,
 %find_overlaps,
 %find_touches,
 %find_sees,
 %remove_if_prop(and(link(contains,_),cc(fg,0))),
 %remove_if_prop(and(giz(g(out)),cc(fg,0))),
 %remove_dead_links,
 %combine_same_globalpoints,  
 extend_obj_proplists,
 really_group_vm_priors,
 %whole,
 %combine_objects,
 end_of_macro]).

individuation_macros(i_complete_generic, 
  [fg_intersections([i_intersect]),
    each_ogs_object([i_ogs_subobj]),
   %remove_used_points,
   %save_as_obj_group([pbox_vm_special_sizes([special_sizes_v_h_sorted_s_l])]),
   %pbox_vm_special_sizes([special_sizes_v_h_sorted_l_s]),
   %print_vm_info(post_in_intersection),
   %i_subtract_objs,
   %fg_subtractions([i_subtract_objs]),
   i_subtract_objs,
   %remove_used_points,
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
    save_as_obj_group([indv_omem_points]),
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
   keep_if_prop(or(iz(type(dot)),iz(type(sa_dots)))),                     
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
  %remove_if_prop(and(link(contains,_),cc(fg,0))),
  %remove_if_prop(and(giz(g(out)),cc(fg,0))),
  %remove_dead_links,
  %combine_same_globalpoints,
  %really_group_vm_priors,
  %combine_objects,
   find_relations,
  end_of_macro  ]). 



show_individuated_pair(PairName,ROptions,GridIn,GridOfIn,InC,OutC):- 
  GridIn=@=GridOfIn,!,
 must_det_ll((
  into_iog(InC,OutC,IndvS),
  show_individuated_nonpair(PairName,ROptions,GridIn,GridOfIn,IndvS))).

/*
show_individuated_pair(PairName,ROptions,GridIn,GridOfIn,InC,OutC):- 
  InC=@=OutC,!,
  must_det_ll((into_iog(InC,OutC,IndvS),
  show_individuated_nonpair(PairName,ROptions,GridIn,GridOfIn,IndvS))).
*/

show_individuated_pair(PairName,ROptions,GridIn,GridOut,InC00,OutC00):- 
 once((must_det_ll((
 extend_obj_proplist(InC00,InC0),
 extend_obj_proplist(OutC00,OutC0),
  maybe_fix_group(InC0,InCRFGBG),
  maybe_fix_group(OutC0,OutCRFGBG),
  include(is_fg_object,InCRFGBG,InCR),
  include(is_fg_object,OutCRFGBG,OutCR),
  show_changes(InC0,InCR),
  show_changes(OutC0,OutCR))))),
  (InC00\=@=InCR;OutC00\=@=OutCR),!,
  show_individuated_pair(PairName,ROptions,GridIn,GridOut,InCR,OutCR),!.

show_individuated_pair(PairName,ROptions,GridIn,GridOut,InC,OutC):-
 must_det_ll((
  grid_to_tid(GridIn,ID1),  grid_to_tid(GridOut,ID2),   
  print_side_by_side(green,GridIn,gridIn(ID1),_,GridOut,gridOut(ID2)),
  as_ngrid(GridIn,GridIn1),as_ngrid(GridOut,GridOut1), xfer_zeros(GridIn1,GridOut1), 
  print_side_by_side(red,GridIn1,ngridIn(ID1,PairName),_,GridOut1,ngridOut(ID2)),

  %grid_size(GridIn,IH,IV),grid_size(GridOut,OH,OV),

  % do_pair_filtering(ID1,GridIn,InC,InShown,ID2,GridOut,OutC,OutShown),
  %IDIn1 = in(ID1), nop(print_list_of(really_show_touches(IDIn1,InShown),IDIn1,InShown)),

  %print_list_of(show_touches(OutShown),out(ID2),OutShown))),
  ((InC==OutC, InC==[]) -> progress(yellow,nothing_individuated(PairName)) ;
   with_luser(no_rdot,true,
     (w_section(show_individuated_sections(ID1,ID2,ROptions,GridIn,GridOut,InC,OutC)),
      w_section(show_individuated_learning(ID1,ID2,ROptions,GridIn,GridOut,InC,OutC))))))).

show_individuated_sections(ID1,ID2,ROptions,_GridIn,_GridOut,InC,OutC):- 
  must_det_ll((((
   %show_io_groups(green,ROptions,ID1,InC,ID2,OutC),
   %show_io_groups(yellow,ROptions,ID1,InC,ID2,OutC),
   print_side_by_side(yellow,InC,objs(ID1),_,OutC,objs(ID2)),
   print_list_of(show_indiv(inputs),inputs,InC),     
   print_list_of(show_indiv(outputs),outputs,OutC),
   show_io_groups(green,ROptions,ID1,InC,ID2,OutC),
   if_wants_output_for(show_interesting_props, show_interesting_props(ID1,InC,OutC)))))).

show_individuated_learning(ID1,_ID2,ROptions,_GridIn,GridOut,InC,OutC):- 
 must_det_ll((
  =(InCR,InC), =(OutCR,OutC),
  banner_lines(orange), %visible_order(InC,InCR),
   if_wants_output_for(learn_group_mapping,(sub_var(trn,ID1), learn_group_mapping(InCR,OutCR))),
   if_wants_output_for(learn_group_mapping_of_tst, (sub_var(tst,ID1),learn_group_mapping(InCR,OutCR))), 
   if_wants_output_for(show_safe_assumed_mapped, show_safe_assumed_mapped),

   if_wants_output_for(show_test_associatable_groups, 
       forall(member(In1,InC),show_test_associatable_groups(ROptions,ID1,In1,GridOut))), 

   if_wants_output_for(try_each_using_training,
     forall(try_each_using_training(InC,GridOut,Rules,OurOut),
      ignore((
       print_grid(try_each_using_training,OurOut),
       nop(pp(Rules)),
       banner_lines(orange,2))))),

   banner_lines(orange,4))).
  

arc_spyable_keyboard_key(detect_pair_hints,'g').
arc_spyable_keyboard_key(show_interesting_props,'y').
arc_spyable_keyboard_key(show_safe_assumed_mapped,'j').
arc_spyable_keyboard_key(learn_group_mapping,'o').
arc_spyable_keyboard_key(learn_group_mapping_of_tst,'o').
arc_spyable_keyboard_key(show_test_associatable_groups,'a').
arc_spyable_keyboard_key(try_each_using_training,'u').


show_test_associatable_groups(ROptions,ID1,InC,GridOut):- 
  print_grid(show_test_assocs(ROptions,ID1),InC),
  forall(
    ((use_test_associatable_group(InC,Sols)*-> show_result("Our Learned Sols", Sols,GridOut,_); arcdbg_info(red,warn("No Learned Sols")))),
    true).


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
 nop((if_t(should_not_be_io_groups(InC,OutC), ( print(InC),print(OutC),break)))).

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


% =========================================================
% TESTING FOR INDIVIDUATIONS
% =========================================================
 %i:- fav_i(X),i(X).   %i(GridIn):- i2([complete],GridIn).
igo:- fav_i(X),igo(X),!. igo(GridIn):- i2(complete,GridIn).
iq:- fav_i(X),igo(X).    iq(GridIn):-  iq(complete,GridIn).
iL:- fav_i(X),iL(X).     iL(GridIn):-  i2([shape_lib(as_is),complete],GridIn).

:- multifile is_fti_step/1.
:- discontiguous is_fti_step/1.
:- discontiguous is_fti_stepr/1.

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

i_pair(ROptions,GridIn,GridOut):-
  check_for_refreshness,
  my_time((maybe_name_the_pair(GridIn,GridOut,PairName),
    individuate_pair(ROptions,GridIn,GridOut,InC,OutC),
    w_section(show_individuated_pair(PairName,ROptions,GridIn,GridOut,InC,OutC)))).

%worker_output(G):- \+ menu_or_upper('B'),!, time(wots(_,arc_weto(G))).
worker_output(G):- time(G).

igo(ROptions,Grid):-
  check_for_refreshness,
  w_section(do_ig(ROptions,Grid,IndvS)),
  into_grid(Grid,GridIn),
  w_section(show_individuated_nonpair(igo,ROptions,Grid,GridIn,IndvS)).

maybe_name_the_pair(In,Out,PairName):-
  kaggle_arc(TestID,ExampleNum,In,Out),
    name_the_pair(TestID,ExampleNum,In,Out,PairName),!.
maybe_name_the_pair(_In,_Out,PairName):-current_test_example(TestID,ExampleNum),
  PairName = (TestID>ExampleNum),!.


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
  kaggle_arc_io(TestID,trn+Num,out,Out),!,
  my_time((maybe_name_the_pair(In,Out,_PairName),
  individuate_pair(ROptions,In,Out,IndvSI,IndvSO),
  into_iog(IndvSI,IndvSO,IndvS))))).

ig_test_id_num_io(ROptions,GridIn,_ID,TestID,_Example,_Num,_IO,IndvS):- 
  set_current_test(TestID),
  worker_output(my_time(individuate_nonpair(ROptions,GridIn,IndvS))),!.
  %maplist(add_shape_lib(as_is),IndvS),  

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
  print_list_of(debug_as_grid,PairName,InC),!,
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
  debug_as_grid(really_show_touches(Title),Obj),
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
    maplist(arc_setval(VM),DPairs)),
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
    %TT = [ /*'V','<','^','v','>'*/ ],
    TT = [],
    my_partition(is_texture(TT),Rest,Shooters,NFGPoints),
    append(BGPoints,NFGPoints,NPoints),
    maplist(remove_texture,NPoints,CPoints),!,
    points_to_grid(H,V,CPoints,NewGrid0),!,
    mapgrid(assign_plain_var_with(wbg),NewGrid0,NewGrid),
    set(VM.points_o)=CPoints,
    set(VM.grid)=NewGrid,
    as_ngrid(NewGrid,NewGMap),!,
    print_ss(green,GMap,nGrid(1),_,NewGMap,nGrid(2)),
    maplist(make_textured_point_object(VM,[birth(texture)]),Zeros,_),
    maplist(make_textured_point_object(VM,[birth(texture)]),Shooters,_))),!.


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
  maplist(==(S1),Row1).
*/  

% =====================================================================
is_fti_step(print_vm_info).
% =====================================================================
print_vm_info(Why,VM):-
  print_grid(VM.h,VM.v,print_vm_info(Why,points),VM.points),  
  writeg(print_vm_info(Why,grid)=VM.grid),
  visible_order(VM.objs,Objs),
  print_grid(VM.h,VM.v,print_vm_info(Why,objs),Objs),
  banner_lines(red),!.
  
% =====================================================================
is_fti_step(remove_omem_trumped_by_boxes).
% =====================================================================
remove_omem_trumped_by_boxes(VM):-
  Objs = VM.objs,
  remove_omem_trumped_by_boxes(VM,Objs,New),
  gset(VM.objs) = New.
remove_omem_trumped_by_boxes(VM,I,O):-
  member(O1,I),
  has_prop(iz(type(pomem)),O1),
  select(O2,I,II),
  has_prop(iz(flag(omem)),O2),
  \+ has_prop(iz(type(pomem)),O2),
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
  has_prop(iz(type(pomem)),O1),
  select(O2,I,II),
  has_prop(iz(type(pomem)),O2),
  O1\==O2, 
  globalpoints(O1,Ps1),globalpoints(O2,Ps2),
  Ps1\==Ps2,
  area(O1,A1),area(O2,A2), (A1/4>A2),
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
   has_prop(iz(type(pomem)),Obj),
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
    raddObjects(VM,WasInside))),!.
*/
pointless(Res,VM):-
  set(VM.points) = [],
  run_fti(VM,Res).

is_fti_step(sub_indiv).
sub_indiv(SubProgram,VM):-
  OnlyNew = VM.objs,  
  maplist(individuate_object(VM,VM.gid,SubProgram),OnlyNew,WasInside),
  maplist(addObjects(VM),WasInside).

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
  StartGrid = VM.grid_o,
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
  forall( maybe_ogs_color(R,OH,OV,Other,Grid),
 %u_dmsg(maybe_ogs_color(R,OH,OV,In,Grid)),
  (globalpoints_include_bg(In,OPoints),
  offset_points(OH,OV,OPoints,GOPoints),
  intersection(VM.points,GOPoints,TODO,LeftOver,Missing),
  Missing\==[],
  TODO\==[],
  count_adjacent_same_colored_points(LeftOver,GOPoints,HVCount,DiagCount),
  DiagCount=_, HVCount<4,
  !,
  must_det_ll((
  %indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
  make_indiv_object(VM,[iz(stype(R))],GOPoints,Obj),
  
  %offset_grid(OH,OV,In,OffsetGrid),!, is_grid(OffsetGrid),
  %OffsetGrid = In,
  add_grid_label(Grid,Info,A), add_grid_label([Obj],Info,B),
  as_debug(9,((dash_chars,Info=maybe_ogs_color(R,OH,OV), print_ss(A,B)))), % atrace,
  %print_ss([Obj|Grid]-wqs(maybe_ogs_color(R,OH,OV))), %  atrace,  
  %print_grid(maybe_ogs_color(R,OH,OV),[Obj|Grid]), %  atrace,  
  remCPoints(VM,GOPoints),
  remGPoints(VM,TODO))))))).


% =====================================================================
is_fti_step(indv_omem_points).
% =====================================================================
indv_omem_points(VM):- 
 luser_setval(generate_gids,true),

 ignore((
 VMID = VM.id, \+ (sub_var(tst,VMID), sub_var(out,VMID)),

 must_det_ll((

  Grid = VM.grid_o,
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
         %print_grid(OID,TPoints), debug_indiv(Obj),
         nop(debug_as_grid(Obj)))))))))))).
 
  

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
  Objs = VM.objs, % group_to_grid(Objs,Grid), DebugObjs = Objs, confirm_reproduction(Objs,DebugObjs,VM.grid_o),   
  if_t(is_group(Objs),
   (group_to_grid(Objs,Grid),
    set(VM.grid_o)= Grid,
    set(VM.grid)=_ )),
  print_grid(VM))),!.


% =====================================================================
is_fti_step(ensure_objects).
% =====================================================================
ensure_objects(VM):-
 must_det_ll((
  if_t(\+ is_group(VM.objs),
   individuate(complete,VM)))).

% =====================================================================
is_fti_step(objects_into_grid).
% =====================================================================
objects_into_grid(VM):-
 must_det_ll((
  ensure_objects(VM),
  Objs = VM.objs,
  group_to_grid(Objs,Grid),
  %DebugObjs = Objs, confirm_reproduction(Objs,DebugObjs,VM.grid_o),
  set(VM.grid)=Grid,
  print_grid(VM))),!.

% =====================================================================
is_fti_step(sub_individuate).
% =====================================================================
sub_individuate(From,SubProgram,VM):-
  OldObjs = VM.objs,
  ignore(run_fti(VM,From)),
  NewObjs = VM.objs,
  intersection(NewObjs,OldObjs,_,OnlyNew,_),
    preserve_vm(VM,maplist(individuate_object(VM, VM.gid,SubProgram),OnlyNew,WasInside)),
  maplist(addObjects(VM),WasInside),
  set_vm(VM).

% =====================================================================
is_fti_step(grid_to_obj_other).
% =====================================================================

grid_to_obj_other(VM):- 
  Grid= VM.grid_o,
  forall(grid_to_obj_other(Grid,VM,_O),true).

% =====================================================================
:- ensure_loaded(kaggle_arc_individuation_pbox).
:- ensure_loaded(kaggle_arc_prior_groups).
% =====================================================================

% =====================================================================
is_fti_step(only_proportional_mass).
% =====================================================================

only_proportional_mass(VM):- 
  Grid= VM.grid_o,
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
hybrid_shape(VM):-
 ignore((
  set(VM.grid)= VM.grid_o,
  Grid = VM.grid,
  mass(Grid,GMass),
  current_test_example(TestID,ExampleNum),
  findall(In,
    ( hybrid_shape(TestID,ExampleNum,pair,In),
      In\=[[_]],In\=Grid,
      mass(In,Mass),  
      Mass<GMass,
      Mass>2,
      mass(In,AMass),
      nop(AMass==9)),List),
  List\==[],
  length(List,HL),!,
  as_debug(9,(print_grid(hybrid_shape(HL,TestID,pair,VM.gid),VM.grid))),!,
  maplist(release_bg,List,FGList),
  % maplist(=,List,FGList),
  predsort_on(hybrid_order,FGList,Set),
  as_debug(1,(print_ss(Set))),!,
  call(ignore((hybrid_shape_from(Set,VM)))))).

release_bg(List,FGList):- is_list(List),!,maplist(release_bg,List,FGList).
release_bg(Point,_):- is_bg_color(Point),!.
release_bg(Point,Point).

count_adjacent_same_colored_points(O1,O2,HVCount,DiagCount):-
  flag(is_diag,WasD,0),flag(is_hv,WasH,0),
  forall((member(C-P1,O1),member(C-P2,O2), once(is_adjacent_point(P1,Dir,P2))),
    (is_diag(Dir)->flag(is_diag,D,D+1);flag(is_hv,D,D+1))),
  flag(is_diag,DiagCount,WasD),
  flag(is_diag,HVCount,WasH),!.
  

hybrid_shape_from(Set,VM):-
  Grid = VM.grid,
  member(In,Set),
  maybe_ogs_color(R,OH,OV,In,Grid),
 %u_dmsg(maybe_ogs_color(R,OH,OV,In,Grid)),
  globalpoints_include_bg(In,OPoints),
  offset_points(OH,OV,OPoints,GOPoints),
  intersection(VM.points,GOPoints,TODO,LeftOver,Missing),
  Missing\==[],
  TODO\==[],
  count_adjacent_same_colored_points(LeftOver,GOPoints,HVCount,DiagCount),
  DiagCount=_, HVCount<4,
  !,
  must_det_ll((
  %indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
  make_indiv_object(VM,[iz(stype(R))],GOPoints,Obj),
  
  %offset_grid(OH,OV,In,OffsetGrid),!, is_grid(OffsetGrid),
  %OffsetGrid = In,
  add_grid_label(Grid,Info,A), add_grid_label([Obj],Info,B),
  as_debug(9,((dash_chars,Info=maybe_ogs_color(R,OH,OV), print_ss(A,B)))), % atrace,
  %print_ss([Obj|Grid]-wqs(maybe_ogs_color(R,OH,OV))), %  atrace,  
  %print_grid(maybe_ogs_color(R,OH,OV),[Obj|Grid]), %  atrace,  
  remCPoints(VM,GOPoints),
  remGPoints(VM,TODO),
  ignore(hybrid_shape_from(Set,VM)))).


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

learn_hybrid_shape(Type,Obj):- is_list(Type),!,maplist(lambda_rev(learn_hybrid_shape(Obj)),Type).
learn_hybrid_shape(Type,Obj):- is_group(Obj),!,mapgroup(learn_hybrid_shape(Type),Obj).
learn_hybrid_shape(Name,ReColored):-
 current_test_example(TestID,ExampleNum),
 learn_hybrid_shape(TestID,ExampleNum,Name,ReColored).

learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_grid(ReColored),!,learn_hybrid_shape_grid(ReColored,TestID,ExampleNum,Name,ReColored).
learn_hybrid_shape(TestID,ExampleNum,Name,ReColored):- is_list(ReColored),!,maplist(learn_hybrid_shape(TestID,ExampleNum,Name),ReColored).
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

print_obj_short_props(Set):- is_list(Set),!,maplist(print_obj_short_props,Set).
print_obj_short_props(Obj):- short_indv_props(Obj,Props,_),global_grid(Obj,Grid),!,print_grid(Grid),nl_if_needed,ppnl(Props).

obj_short_props(Obj,Title=Grid):- short_indv_props(Obj,Title,_),global_grid(Obj,Grid),!.

r_props(Short,SortR):- % obj_to_oid(Obj,MyOID),
  remove_too_verbose(0,Short,TV0),include(not_too_verbose,TV0,TooV),sort_safe(TooV,Sort),reverse(Sort,SortR).
short_indv_props(Props,ShortR,LongR):- is_list(Props),!,my_partition(is_short_prop,Props,Short,Long),r_props(Short,ShortR),r_props(Long,LongR).
short_indv_props(Obj,ShortR,LongR):- indv_props_list(Obj,Props),!,short_indv_props(Props,ShortR,LongR).

is_long_prop(vis_hv_term(_)).
is_long_prop(link(_,_,_)). is_long_prop(link(_,_)).
is_long_prop(pg(_,_,_,_)). is_long_prop(giz(_)).
is_long_prop(edge(_,_)). is_long_prop(on_edge(_,_)). is_long_prop(on_edge(_)). 
is_long_prop(pen(_)).
is_long_prop(merged(_)). is_long_prop(rot2L(sameR)).
is_long_prop(gid(_)). is_long_prop(cc(_,0)). is_long_prop(symmetry_type(_)).
is_long_prop(chromatic(_,_)). is_long_prop(sizeY(_)). is_long_prop(sizeX(_)). 
is_long_prop(overlap(_,_)).  is_long_prop(dir_touching(_,_)). is_long_prop(dir_seeing(_,_)). is_long_prop(contains(_,_)).
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
is_fti_step(reset_points).
% =====================================================================
reset_points(VM):- 
  gset(VM.points)= VM.points_o.

% =====================================================================
is_fti_step(reset_grid).
% =====================================================================
reset_grid(VM):- 
  gset(VM.grid)= VM.grid_o.

% =====================================================================
is_fti_step(reset_objs).
% =====================================================================
reset_objs(VM):- 
  gset(VM.objs)= [].

fix_indivs_options(O,L):-is_list(O),maplist(fix_indivs_options,O,OL),my_append(OL,L).
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
  Points  = VM.points,
%  get_vm
  Program = VM.program_i,
  %OldObjs = VM.objs,
  %set(VM.grid) = VM.grid_o,
  gset(VM.points) = VM.points_o,
  %set(VM.objs) = [],  
  ignore(fti(VM,While)),
  %NewObjs = VM.objs,
  %append(NewObjs,OldObjs,Objs),
  
  %must_det_ll(combine_duplicates(Objs,NewObjs)),
  %list_to_set(Objs, NewObjs),
  %set_vm(VM,objs,NewObjs),
  set_vm(points,Points),
  set_vm(program_i,Program).
  

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
  my_partition(has_color(C),VM.points,ThisGroup,LeftOver),
  ignore(((
   length(ThisGroup,Len),  Len >= Min,
   set(VM.points)=LeftOver,
   meets_indiv_criteria(VM,birth(maybe_1_3rd_mass),ThisGroup),
   make_indiv_object(VM,[birth(maybe_1_3rd_mass),iz(media(image))],ThisGroup,ColorObj),
   raddObjects(VM,ColorObj)))).


:- decl_pt(detect_indvs(group,group,'-')).
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

individuate(VM):- individuate1(VM,VM.roptions,VM.grid,InC),set(VM.objs)=InC,!.
individuate_c(VM):- individuate1(VM,complete,VM.grid,InC),set(VM.objs)=InC,!.
%individuate(VM):-  individuate(VM.roptions, VM),!.

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
        save_grouped(individuate(NamedOpts,GOID),IndvS))).

must_grid_to_gid(GridIn,GOID):- must_det_ll(grid_to_gid(GridIn,GOID)).

allow_out_in :- true.

first_grid_same_areas(In,Out,IO):-
  unique_color_count(In,ISize), unique_color_count(Out,OSize), 
  ((OSize> ISize) -> (IO=out_in);(IO=in_out)).
first_grid_same_areas(In,Out,IO):-
  mass(In,ISize), mass(Out,OSize), 
  ((OSize<ISize) -> (IO=out_in);(IO=in_out)).

first_grid(_In,_Out,in_out):- \+ allow_out_in,!.
first_grid(In,Out,IO):- trim_to_rect(In,InT),trim_to_rect(Out,OutT),first_grid1(InT,OutT,IO),!.
first_grid(_In,_Out,in_out).

first_grid1(In,Out,in_out):- find_ogs(_,_,In,Out),!.
first_grid1(In,Out,out_in):- find_ogs(_,_,Out,In),allow_out_in,!.
first_grid1(In,Out,out_in):- 
  area(In,IArea), area(Out,OArea), OArea<IArea, allow_out_in, !.
first_grid1(In,Out,IO):-
  area(In,IArea), area(Out,OArea), IArea==OArea, first_grid_same_areas(In,Out,IO), allow_out_in,!.
%first_grid(In,Out,IO):- first_grid_same_areas(In,Out,IO).
first_grid1(_In,_Out,in_out).


compile_and_save_current_test(Why):-
  compile_and_save_test(TestID),
  forall(compile_and_save_current_test_pt_2(TestID,Why),true).

arc_test_property(A,B,C):- get_current_test(TestID), compile_and_save_test(TestID), arc_test_property(TestID,A,B,C).
arc_common_property(Prop):- arc_test_property(common,_,Prop).
arc_common_property(Prop):- arc_test_property(common,Prop,O),O\==[].

%compile_and_save_current_test_pt_2(TestID,Why):- is_list(Why),!,maplist(compile_and_save_current_test_pt_2(TestID),Why).
compile_and_save_current_test_pt_2(_TestID,_):-
   current_pair(I,O),
   arc_common_property(containsAll(i-o)),
   mapgrid(cell_minus_cell,I,O,IMinusO),
   print_grid(i_minus_o,IMinusO).

compile_and_save_current_test_pt_2(_TestID,_):-
   current_pair(I,O), 
   arc_common_property(containsAll(i-o)),
   mapgrid(cell_minus_cell,O,I,R),
   print_grid(o_minus_i,R).

cell_minus_cell(I,O,M):- I=@=O, M=bg.
%cell_minus_cell(I,_,M):- dont_care_color(I),M=I.
%cell_minus_cell(I,O,M):- dont_care_color(O),M=I.
cell_minus_cell(I,_,I).




individuate_pair(ROptions,In,Out,IndvSI,IndvSO):-
  check_for_refreshness,
  into_grid(In,InG), into_grid(Out,OutG),
  current_test_example(TestID,ExampleNum),
 [for(out,OutG),for(in,InG)] = Why,
 compile_and_save_current_test(Why),
  sformat(Title,"individuate pair: ~w ~w",[TestID,ExampleNum]),
  w_section(title(Title),
  must_det_ll(((   
   first_grid(InG,OutG,IO),
   ((IO==in_out; true)
       -> individuate_two_grids(ROptions,InG,OutG,IndvSI,IndvSO)
              ; individuate_two_grids(IO,OutG,InG,IndvSO,IndvSI)))))),!.

individuate_nonpair(ROptions,In,IndvSI):- 
  into_grid(In,InG), 
  compile_and_save_current_test([for(InG)]),
  individuate(ROptions,InG,IndvSI).




doing_pair:- nb_current(doing_pair,t).

individuate_two_grids(ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
  delistify_single_element(ROptions,NamedOpts),
  must_grid_to_gid(GridIn,OIDIn), must_grid_to_gid(GridOut,OIDOut),
  locally(nb_setval(doing_pair,t),
    individuate_two_grids_once(two(OIDIn,OIDOut),NamedOpts,GridIn,GridOut,IndvSI,IndvSO)).

/*
individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- saved_group(individuate(ROptions,OID_In_Out),IndvS),!,into_gio(IndvS,IndvSI,IndvSO),!.
individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
    individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO),
    ignore((into_iog(IndvSI,IndvSO,IndvS),save_grouped(individuate(ROptions,OID_In_Out),IndvS))).
*/

individuate_two_grids_once(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO):- 
  %u_dmsg(individuate(ROptions,OID_In_Out,IndvSI,IndvSO)),
  (saved_group(individuate(ROptions,OID_In_Out),IndvS)
    -> into_gio(IndvS,IndvSI,IndvSO)
    ; ((individuate_two_grids_now(OID_In_Out,ROptions,GridIn,GridOut,IndvSI,IndvSO),
         into_iog(IndvSI,IndvSO,IndvS),save_grouped(individuate(ROptions,OID_In_Out),IndvS)))).


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
  maybe_into_fti(ID_In,ROptions,Grid_In,VM_In), set(VM_In.grid_target) = Grid_Out,
  maybe_into_fti(ID_Out,ROptions,Grid_Out,VM_Out), set(VM_Out.grid_target) = Grid_In,

  if_t(fail,
  if_t((H_In==H_Out,V_In==V_Out),((
      mostly_fgp(P_In,P_InX),
      mostly_fgp(P_Out,P_OutX),
      intersection(P_InX,P_OutX,Same,U_In,U_Out),
      u_dmsg(intersection(P_InX,P_OutX,Same,U_In,U_Out)),

      if_t((U_In==[],U_Out\==[]),(set(VM_Out.points)=U_Out,set(VM_Out.points_o)=U_Out,
                            points_to_grid(H_Out,V_Out,U_Out,Grid_OutX),
                            set(VM_Out.grid)=Grid_OutX,set(VM_Out.grid_o)=Grid_OutX)),

      if_t((U_In\==[],U_Out==[]),(set(VM_In.points)=U_In,set(VM_In.points_o)=U_In,
                            points_to_grid(H_In,V_In,U_In,Grid_InX),set(VM_In.grid)=Grid_InX,set(VM_In.grid_o)=Grid_InX)),

      if_t((U_In\==[],U_Out\==[]),(subtract(P_Out,Same,NewP_Out),
                             set(VM_Out.points)=NewP_Out,set(VM_Out.points_o)=NewP_Out,
                             points_to_grid(H_Out,V_Out,NewP_Out,Grid_OutX),
                             set(VM_Out.grid)=Grid_OutX,set(VM_Out.grid_o)=Grid_OutX)),

      !
    )))),

  (var(Grid_InX)->Grid_InX=Grid_In;true),
  (var(Grid_OutX)->Grid_OutX=Grid_Out;true),
  individuate_two_grids_now_X(OID_In_Out,ROptions,Grid_In,Grid_Out,VM_In,VM_Out,Grid_InX,Grid_OutX,IndvSI,IndvSO))).

individuate_two_grids_now_X(OID_In_Out,ROptions,Grid_In,Grid_Out,VM_In,VM_Out,Grid_InX,Grid_OutX,Objs_InX,Objs_OutX):- 
 must_det_ll((

  grid_to_tid(Grid_In,ID_In), grid_to_tid(Grid_Out,ID_Out),

  print_ss(yellow,Grid_InX,OID_In_Out=gridInX(ID_In),_,Grid_OutX,OID_In_Out=gridOutX(ID_Out)),

  worker_output((
  with_other_grid(Grid_OutX,do_individuate(VM_In,ROptions,Grid_InX,Objs_In)),!, 
  gset(VM_Out.robjs) = Objs_In,
  with_other_grid(Grid_InX,do_individuate(VM_Out,ROptions,Grid_OutX,Objs_Out)),!,

  must_grid_to_gid(Grid_In,OID_In),must_grid_to_gid(Grid_Out,OID_Out),

  maybe_into_fti(ID_In,ROptions,Grid_InX,VM_InX), set(VM_InX.grid_target) = Grid_Out, set(VM_InX.robjs) = Objs_Out,    
  with_other_grid(Grid_Out,individuate2(VM_InX,ROptions,OID_In,Grid_InX,Objs_InX)),!,

  maybe_into_fti(ID_Out,ROptions,Grid_OutX,VM_OutX), set(VM_OutX.grid_target) = Grid_In, set(VM_OutX.robjs) = Objs_InX, 
  with_other_grid(Grid_In,individuate2(VM_OutX,ROptions,OID_Out,Grid_OutX,Objs_OutX)))),!,

  into_iog(Objs_InX,Objs_OutX,IndvS),
  save_grouped(individuate(ROptions,OID_In_Out),IndvS))).

individuate2(VM,[ROptions],GOID,Grid,IndvS):- nonvar(ROptions), !, individuate2(VM,ROptions,GOID,Grid,IndvS).


individuate2(_VM,ROptions,GOID,_GridIn,IndvS):- nonvar(GOID), 
  get_individuated_cache(_TID,ROptions,GOID,IndvS),!,
  length(IndvS,Len),ignore((Len>=0,progress(yellow,oid_cached(ROptions,GOID,len(Len),'$VAR'(Len))))),
  !.
individuate2(VM,ROptions,GOID,GridIn,IndvS):-
  do_individuate(VM,ROptions,GridIn,LF),!,
  =(LF,IndvS),
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
   %nop((tid_to_gid(ID,GID), maplist(assert_object_oid(GID),LF,_Glyphs,_OIDs))).
  %smallest_first(mass,IndvS,SF),
  %largest_first(mass,SF,LF),
  
   %!.
   %VM.grid_target

guard_whole([obj(LF)],[obj([iz(stype(part))|LF])]):- !.
guard_whole(LF,LF).
% tiny grid becomes a series of points
%individuate(GH,GV,ID,ROptions,_Grid,Points,IndvS):- \+ is_list(ROptions), is_glyphic(Points,GH,GV), individuate_glyphic(GH,GV,ID,Points,IndvS),!.
%individuate(GH,GV,ID,whole,_Grid,Points,IndvS):-  individuate_whole(GH,GV,ID,Points,IndvS),!.

individuate7(VM,ID,ROptions,GridIn,IndvS):-
  individuate8(VM,ID,ROptions,GridIn,IndvS).
individuate8(VM,ID,ROptions,GridIn,IndvS):-
 must_det_ll((
      (var(VM) -> maybe_into_fti(ID,ROptions,GridIn,VM) ; true),
      %VM.points = Points,
      %individuation_reserved_options(ROptions,Reserved,NewOptions),
      %atrace,
      %ensure_fti(GH,GV,ID,Grid,[],Reserved,NewOptions,Points,VM),   
      set_vm(VM),
      %individuals_raw(VM,GH,GV,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
      run_fti(VM,ROptions), 
      IndvSRaw = VM.objs,
  %as_debug(9,ppt((individuate=IndvSRaw))),
      make_indiv_object_list(VM,IndvSRaw,IndvS1),
      %combine_objects(IndvS1,IndvS2),
      combine_same_globalpoints(IndvS1,IndvS),
      %list_to_set(IndvS1,IndvS),
      nop(print_info(IndvS)))).  


maybe_into_fti(ID_In,[ROptions],Grid_In,VM_In):- nonvar(ROptions),!,maybe_into_fti(ID_In,ROptions,Grid_In,VM_In). 
maybe_into_fti(ID_In,ROptions,Grid_In,VM_In):- ROptions==complete,grid_vm(Grid_In,VM_In),!,gset(VM_In.id) = ID_In.
maybe_into_fti(ID_In,ROptions,Grid_In,VM_In):- into_fti(ID_In,ROptions,Grid_In,VM_In).

into_fti(ID,ROptions,GridIn0,VM):-
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
  (var(ID)->grid_to_tid(Grid,ID);true),
  grid_size(Grid,H,V),
 % rb_new(HM),duplicate_term(HM,Hashmap),
  max_min(H,V,MaxM,_),
  max_min(320,MaxM,Max,_),
  % term_to_oid(ID,GOID),

  must_grid_to_gid(Grid,GOID),
  progress(blue,fix_indivs_options(tid_gid,ID,GOID,ROptions->Options)),
  TID_GID=tid_gid(ID,GOID),
  check_tid_gid(TID_GID,Grid),

  
  ignore(other_grid_size(Grid,OGX,OGY)),

  listify(ROptions,OOptions),
  Area is H*V,
 % progress(yellow,igo(ROptions,ID)=into_fti(H,V)),
  ArgVM = vm{
    ogx:OGX,ogy:OGY,
   % parent VM
   %training:_,
     %compare:_, 
   grid_target:_,  last_key:_,  
   % Options and TODO List (are actually equal things)
   program_i:Options, options:OOptions, roptions:ROptions, %todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, objs_max_len:Max, objs_min_mass:_, objs_max_mass:_,
   % Grid and point representations
   grid:Grid, gid:GOID,
   area:Area,
   allocated_points:[],
   points:Points,
   props:_,
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

   changed:_,% solution:_,neededChanged
   neededChanged:_, repaired:_,
   full_grid:_,
   parent_vm:_,
   izmap:true,
   % Original copies of Grid and point representations
   grid_o:Grid, 
   ngrid:_,
   rule_dir: ROptions,
   points_o:Points, % repaired:[],
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
     maplist(nb_link_pairs_if_missing(VM),Pairs).
transfer_onto_dict(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     maplist(nb_link_pairs(VM),Pairs).
force_onto_dict(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     maplist(nb_link_pairs(VM),Pairs).

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
complete_broken_lines(VM):- complete_broken_lines(VM.points,set(VM.points)).
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
  %is_rbtree(VM),
  %guitracer,!,
  must_det_ll(get_kov(program_i,VM,Code)),!,
  must_det_ll(run_fti(VM,Code)).

run_fti(_,[]):- !.
run_fti(_,_):- arc_option(no_individuator), !.
run_fti(VM,NotAList):- \+ is_list( NotAList),!, must_det_ll(run_fti(VM,[NotAList])).
run_fti(VM,[Step|Program]):- is_list(Step),flatten([Step|Program],StepProgram),!,must_det_ll(run_fti(VM,StepProgram)).
run_fti(_,[Done|TODO]):- my_assertion(nonvar(Done)), ( \+ done \= Done ), !, u_dmsg(done_run_fti([Done|TODO])),set_vm(program_i,[done]),!.
run_fti(VM,[if_done|TODO]):- !, (VM.points==[] -> (u_dmsg(if_done),set_vm(program_i,[if_done])) ; run_fti(VM,TODO)).
run_fti(VM,[end_of_macro|TODO]):- !, must_det_ll(run_fti(VM,TODO)).
run_fti(VM,[recalc_sizes|TODO]):- !, must_det_ll(run_fti(VM,TODO)).

run_fti(VM,[F|TODO]):- 
  %must_det_ll(fti(VM,[F|TODO])),!, 
  show_vm_changes(VM,F, must_det_ll(fti(VM,[F|TODO]))),!,
  must_det_ll(get_kov(program_i,VM,Code)),!,    
  must_det_ll(([F|TODO]=@=Code -> 
    (% progress(blue,fti=[F|TODO]), progress(blue,code=Code), 
     set(VM.program_i) = TODO, 
       run_fti(VM,TODO)) ; run_fti(VM))),!.


print_vm_debug_objs(_VM):- !.
print_vm_debug_objs(VM):- 
  Objs = VM.objs,  
  length(Objs,Count),
  as_debug(8,(mass(Objs,Mass), length(VM.points,PC),
      maybe_four_terse(VM.program_i,Four),
      progress(t([obj/obj_mass=(Count/Mass),unprocessed_points=PC,roptions=VM.roptions,fsi=Four])))).


maybe_four_terse(L,F=..N):- length(L,N),N>4,!,length(F,4),append(F,_,L),!.
maybe_four_terse(L,L):-!.
%fti(VM,_):- VM.points=[], !.
fti(_,[]):- !.
fti(VM,NotAList):- \+ is_list( NotAList),!, fti(VM,[NotAList]).
fti(VM,[Step|Program]):- is_list(Step),append(Step,Program,StepProgram),!,fti(VM,StepProgram).
fti(VM,[end_of_macro|TODO]):- !, fti(VM,TODO).
fti(_,[Done|TODO]):-  ( \+ done \= Done ), !, u_dmsg(done_fti([Done|TODO])),!.
%fti(VM,_):- VM.points==[], !.
fti(VM,_):-
  Objs = VM.objs,  
  length(Objs,Count),
  ((member(progress,VM.options); Count > VM.objs_max_len; (statistics(cputime,X), X > (VM.timeleft)))) -> 
   print_vm_debug_objs(VM),fail.

fti(VM,Prog):- fail, length(Prog,Len),Len>1, exceeded_objs_max_len(VM),!,
  set(VM.objs_max_len) is VM.objs_max_len + 2000,
  set(VM.program_i)= [do_ending],!.

fti(VM,[IsToBeRewritten|Rest]):-
    %atom(IsToBeRewritten), 
    nonvar(IsToBeRewritten),
    individuation_macros(IsToBeRewritten,Expansion),!,
    listify(Expansion,ExpansionL),
    my_append(ExpansionL,Rest,set(VM.program_i)).

fti(VM,_):- fail, member(recalc_sizes,VM.options), once(recalc_sizes(VM)), fail.
fti(VM,[recalc_sizes|TODO]):- nop(recalc_sizes(VM)),!, fti(VM,TODO).
fti(VM,[recalc_sizes,After|TODO]):- After == recalc_sizes,!, fti(VM,[recalc_sizes|TODO]).
fti(VM,[recalc_sizes,After|TODO]):- 
  (recalc_sizes(VM,[After|TODO])
     -> true; (set(VM.program_i)= [After,recalc_sizes|TODO])),!.

fti(VM,[max_learn_objects(Routine,Max)|set(VM.program_i)]):- fail,
   set(VM.objs_max_len) = Max,
   set(VM.options)= [max_learn_objects(Routine,Max)|VM.options].

fti(VM,[Routine|set(VM.program_i)]):-  fail,
   member(max_learn_objects(Routine,Max),VM.options),
   length(VM.objs,Count),
   Count>Max,!,fail.

fti(VM,[-(DelOptions)|TODO]):-
  listify(DelOptions,OList),
  my_partition(option_matches(OList),VM.options,_,set(VM.options)),
  my_partition(option_matches(OList),TODO,_,set(VM.program_i)).

option_matches(List,Arg):- member(E,List),E==Arg.

fti(VM,[+(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[(OptionsL)|TODO]):- fail,
  is_list(OptionsL), \+ is_group(OptionsL), \+ is_grid(OptionsL),!,
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[options(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[macrof(AddTodo)|set(VM.program_i)]):-
  listify(AddTodo,TodoLst),
  fti(VM,TodoLst).

fti(VM,[macro(AddTodo)|TODO]):-
  listify(AddTodo,TodoL),
  my_append([progress|TodoL],TODO,set(VM.program_i)).

/*
%fti(VM,Prog):- length(Prog,Len),Len>2, exceeded_objs_max_len(VM),!,set(VM.program_i)= [do_ending],!,length(VM.objs,Count),set(VM.objs_max_len) is Count+3.
fti(VM,[Step|Program]):- functor(Step,F,_), ping_indiv_grid(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_submenu_call(call(Step, VM.grid)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_submenu_call(call(Step,VM)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, Step=..[F|ARGS], !, my_submenu_call(apply(F,[VM|ARGS])).
fti(VM,[Step|Program]):- set(VM.program_i) = Program, one_fti_step(Step), !, my_submenu_call(one_fti(VM,Step)),!.
*/

fti(VM,[when(G,D)|TODO]):- ((call_expanded(VM,G),!,progress(using_when(G,D)))->R=D;R=call(nop(progress(skipped(G,D))))),
  set(VM.program_i) = [R|TODO].

fti(VM,[call(G)|TODO]):-   set(VM.program_i) = TODO, !, my_subfti_call(call(G),VM,call_expanded(VM,G)).

%fti(VM,Prog):- length(Prog,Len),Len>2, exceeded_objs_max_len(VM),!,set(VM.program_i)= [do_ending],!,length(VM.objs,Count),set(VM.objs_max_len) is Count+3.
fti(VM,[Step|Program]):- functor(Step,F,_), ping_indiv_grid(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_subfti_call(Step,VM,call(Step, VM.grid)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_subfti_call(Step,VM,call(Step,VM)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, Step=..[F|ARGS], !, my_subfti_call(Step,VM,apply(F,[VM|ARGS])).
fti(VM,[Step|Program]):- set(VM.program_i) = Program, one_fti_step(Step), !, ignore(one_fti(VM,Step)).
   %my_subfti_call(Step,VM,one_fti(VM,Step)),!.

%my_subfti_call(_Step,VM,Goal):- !, _OldObjs = VM.objs, !, my_submenu_call(Goal).

my_subfti_call(Step,VM,Goal):-
  my_subfti_vm_call(VM,my_submenu_call(Goal),GoodObjs,Added),
  add_birth_if_missing(Step,Added,FixAdded),
  append(GoodObjs,FixAdded,NewObjs),
  set(VM.objs)=NewObjs.


my_subfti_vm_call(VM,Goal,GoodObjs,Added):-  
  OldObjs = VM.objs, 
  my_submenu_call(Goal),
  Objs = VM.objs,!,
  pred_intersection(is_same_oids,OldObjs,Objs,_OldUpdated,GoodObjs,_Deleted,Added).


% =====================================================================
is_fti_step(larger_than).
% =====================================================================
larger_than(Mass,TODO,VM):- 
  my_subfti_vm_call(VM,run_fti(VM,TODO),GoodObjs,Added),
  include(not_less_mass(Mass),Added,FixAdded),
  append(GoodObjs,FixAdded,NewObjs),
  set(VM.objs)=NewObjs.
  


is_same_oids(O1,O2):- obj_to_oid(O1,OID1),obj_to_oid(O2,OID2),OID1=@=OID2.


%fti(VM,[Step|Program]):- callable_arity(Step,0), !, set(VM.program_i) = Program, my_submenu_call(call(Step)).

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
  

fti(VM,[F|TODO]):-
   u_dmsg(fti_miss(F)),
   set(VM.program_i)= TODO.

%:- listing(fti/2).
% fti(_VM,[F|_]):- u_dmsg(F),!. % atrace,fail.


one_fti_step(Name):- is_thing_or_connection(Name).
one_fti_step(whole).
one_fti_step(Name):- clause(one_fti(_VM,Step),_),nonvar(Step),Name=Step.

i_step(Name):- no_repeats(Name,i_step0(Name)).
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
  maplist(globalpoints,Smaller,Points),
  append_sets([VM.points|Points],set(VM.points)))).

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
 Grid = VM.grid_o,
 (\+ ground(Grid) -> true ;
  maplist_n(1,row_to_indiv(VM,'all_rows'), Grid)).

one_fti(VM,'some_rows'):-
 Grid = VM.grid_o,
 (\+ ground(Grid) -> true ;
  forall( solid_row_number(Grid,_Color,N,C2), row_to_indiv(VM,'some_rows',N,C2))).

solid_row_number(Grid,C,N,Row):- 
    grid_size(Grid,_H,V),
    %make_list(black,H,BlankRow),
    nth1(N,Grid,Row),    
    maplist(=(C),Row),
    \+ is_bg_color(C),
    \+ \+ (
    ( N == 1 -> \+ (nth1(2,Grid,Row2),maplist(=(C),Row2)) ; 
    ( N == V -> ( N1 is N - 1, \+ (nth1(N1,Grid,Row1),maplist(=(C),Row1))) ;
    ( N1 is N - 1, \+ (nth1(N1,Grid,Row1),maplist(=(C),Row1)) , N2 is N + 1, \+ (nth1(N2,Grid,Row2),maplist(=(C),Row2)))))),    
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
  raddObjects(VM,Obj).

one_fti(VM,'all_columns'):-
 Grid = VM.grid_o,
 (\+ ground(Grid) -> true ;
  rot90(Grid,Grid90),
  maplist_n(1,column_to_indiv(VM,'all_columns'), Grid90)).

%Emani 501 N grahm %200  Feb 3rd 3pm


one_fti(VM,'some_columns'):-
 Grid = VM.grid_o,
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
    iz(stype(Birth)),iz(media(image)),iz(grouped(Birth)),loc2D(N,1),rot2L(rot180),vis2D(1,VM.v),giz(grid_sz(VM.h,VM.v))],GPoints,Obj),
  raddObjects(VM,Obj).
  
% =====================================================================
is_fti_step(bg_shapes).
% =====================================================================
bg_shapes(Shape,VM):-
 must_det_ll((
  GridIn = VM.grid_o,
  other_grid(GridIn,Other),
  Orig = VM.grid,
  into_monogrid(Orig,NewGrid),
  must_be_free(GOID),
  atomic_list_concat([VM.gid,'_bg_shaped'],OID2), assert_grid_gid(NewGrid,OID2),  
  get_vm(VMS),
  %map_pred(bg_shaped,FoundObjs,ReColored),
  with_other_grid(Other,individuate2(_,Shape,GOID,NewGrid,FoundObjs)),
  set_vm(VMS),
  %globalpoints_include_bg(VM.grid_o,Recolors),
  maplist(recolor_object(Orig),FoundObjs,ReColored),
  remCPoints(VM,ReColored),
  remGPoints(VM,ReColored),
  addInvObjects(VM,ReColored))),!.

into_monogrid(Orig,NewGrid):-mapgrid(bg_shaped,Orig,NewGrid).
bg_shaped(Cell,NewCell):- is_bg_color(Cell),!,nop(decl_many_bg_colors(NewCell)),NewCell=wbg.
bg_shaped(Cell,NewCell):- is_fg_color(Cell),!,nop(decl_many_fg_colors(_NewCell)),NewCell=fg.
bg_shaped(Cell,bg):- plain_var(Cell),!.
bg_shaped(Cell,Cell).

recolor_object(Recolors,Old,New):- 
  %globalpoints_include_bg(Grid,Recolors),
  globalpoints_include_bg(Old,GPoints),
  maplist(recolor_point(Recolors),GPoints,NewGPoints),  
  must_det_ll(rebuild_from_globalpoints(Old,NewGPoints,New)),!.

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
 ignore((
 VMGID = VM.gid,
 \+ atom_contains(VMGID,'_fg_shaped'),

 %must_det_ll
 ((
  Grid = VM.grid,

  colors_across(Grid,Silvers),
  get_black(Black),
  BGCs = [Black,wbg,bg|Silvers],
  mapgrid(fg_shaped(BGCs),Grid,NewGrid),
  %var(GOID),
  other_grid(VM.grid_o,Other),
  atomic_list_concat([VM.gid,'_fg_shaped'],GOID), assert_grid_gid(NewGrid,GOID),
  get_vm(VMS), 
  with_other_grid(Other,individuate2(_,Shape,GOID,NewGrid,FoundObjs)),
  set_vm(VMS),
  globalpoints_include_bg(VM.grid_o,Recolors),
  maplist(recolor_object(Recolors),FoundObjs,ReColored),
  learn_hybrid_shape(ReColored),
  remCPoints(VM,ReColored),
  remGPoints(VM,ReColored),
  addInvObjects(VM,ReColored))))),!.



colors_across(Grid,Silvers):-
  findall(C,color2_across(Grid,C),Silvers).
color2_across(Grid,C):- rot90(Grid,Grid90), color1_across(Grid90,C).
color2_across(Grid,C):- color1_across(Grid,C).
color1_across(Grid,C):- enum_fg_colors(C),two_rows(Grid,C,_,_).

two_rows(Grid,S1,R1,R2):-
  nth1(R1,Grid,[S1|Row1]),nonvar(S1),
  maplist(==(S1),Row1),
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

sub_self(NewGrid,NewOther,TODO,VM):- 
 print_grid(NewGrid),
 pp(doing(TODO)),
 must_det_ll((
 globalpoints(NewGrid,Points),
 duplicate_term(VM,SavedVM),
 set(VM.points) = Points,
 set(VM.points_o) = Points,
 set(VM.grid)= NewGrid,
 set(VM.grid_o)= NewGrid,
 set(VM.grid_target)= NewOther,
 run_fti(VM,TODO),
 FoundObjs = VM.objs,
 LOPoints = VM.points,
 force_onto_dict(SavedVM,VM),
 set(VM.points)= LOPoints, 
 set(VM.objs)= FoundObjs,
 %set(VM.grid)= PrevGrid,
 %set(VM.grid_target)= Other,
 remCPoints(VM,FoundObjs),
 remGPoints(VM,FoundObjs),
 addInvObjects(VM,FoundObjs))),!.



% =====================================================================
is_fti_step(i_subtract_objs).
% =====================================================================
i_subtract_objs(VM):- fail,
  Grid = VM.grid,
  localpoints(Grid,Ps),include(is_fgp,Ps,FGPs),length(FGPs,PsL),PsL<25, fail,!,
  run_fti(VM,[pbox_vm_special_sizes,lo_dots]).

i_subtract_objs(VM):-   
  points_to_grid(VM.h,VM.v,VM.points,Grid),
  print_grid(i_subtract_objs(points),Grid),
  print_grid(i_subtract_objs(grid),VM.grid),
  run_fti(VM,[colormass,pbox_vm_special_sizes,nsew,lo_dots]).
  
% =====================================================================
is_fti_step(fg_subtractions).
% =====================================================================
fg_subtractiond(This,Minus,_):- This =@= Minus,!.  %fg_subtractiond(This,Minus,_):- \+ This \= Minus,!.
fg_subtractiond(This,_,This).

fg_subtractions(TODO,VM):-
 copy_term(VM.grid_o,Grid),
 other_grid(Grid,Other), is_grid(Other),
 grid_size(Other,H,V),VM.h==H,VM.v==V,
 mapgrid(fg_subtractiond,Grid,Other,NewGrid),
 mapgrid(plain_var_to(black),NewGrid),
 mass(NewGrid,NM), NM > 0, % mass(Grid,GM), NM\==GM,% ,!, mass(Other,OM),
 sub_self(NewGrid,Other,TODO,VM).

% =====================================================================
is_fti_step(i_intersect).
% =====================================================================
i_intersect(VM):-
  Grid = VM.grid,
  localpoints(Grid,Ps),include(is_fgp,Ps,FGPs),length(FGPs,PsL),PsL<25,!,
  run_fti(VM,[lo_dots]),!.

i_intersect(VM):-
  run_fti(VM,[nsew,colormass,lo_dots]).

% =====================================================================
is_fti_step(fg_intersections).
% =====================================================================
fg_intersectiond(This,That,This):- This =@= That,!.
fg_intersectiond(_,_,_).
%fg_intersectiond(_,_,Black):-  get_black(Black).

fg_intersections(TODO,VM):-
 copy_term(VM.grid_o,Grid),
 other_grid(Grid,Other), is_grid(Other),
 grid_size(Other,H,V),VM.h==H,VM.v==V,
 mapgrid(fg_intersectiond,Grid,Other,NewGrid),
 mapgrid(plain_var_to(black),NewGrid),
 mass(NewGrid,NM), NM > 0, % mass(Grid,GM), NM\==GM,% ,!, mass(Other,OM),
 sub_self(NewGrid,Other,TODO,VM).


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
  copy_term(VM.grid_o,StartGrid), 
  other_grid(StartGrid,Other), 
  is_grid(Other),
  all_ogs_123(StartGrid,Other,Hints))),
  %copy_term(VM.grid,This),  
  must_det_ll_maplist(use_each_ogs(VM,TODO,StartGrid,Other),Hints).

all_ogs_123(StartGrid,Other,Hints):- 
  all_ogs(t-o,StartGrid,Other,Hints1),
  all_ogs(o-t,Other,StartGrid,Hints2),
  append(Hints1,Hints2,Hints).


about_i_or_o(_-OorT,OorT):-!. about_i_or_o(TrimIO,OorT):- arg(1,TrimIO,IO),!,about_i_or_o(IO,OorT). about_i_or_o(_,t).

use_each_ogs(VM,TODO,This,Other,Hint):- 
 nl_if_needed,pp(cyan,Hint),
 Hint = ogs(TrimIO,Named,Special,[loc2D(OH,OV),vis2D(SH,SV)]),
 about_i_or_o(TrimIO,OorT), ( (OorT == o) -> From = This ; From = Other ),
 obj_gpoints(From,OBJGRID,OH,OV,SH,SV,GOPoints),
 intersection(GOPoints, VM.points_o,Shared,MissingGoPoints,_ExtraPoints),
((MissingGoPoints==[], Shared\==[]) -> true;
 must_det_ll((
  print_grid(Hint,OBJGRID),
  make_indiv_object(VM,[iz(stype(ogs)),
     iz(tio(TrimIO)),iz(todo(TODO)),grid(OBJGRID),iz(named(Named)),iz(spec(Special))],GOPoints,Obj),    
  % remCPoints(VM,Obj), remGPoints(VM,Obj),
  addInvObjects(VM,Obj),
  addCPoints(VM,GOPoints),
  grid_size(From,GH,GV),
  points_to_grid(GH,GV,GOPoints,NewGrid),
  sub_self(NewGrid,Other,TODO,VM)))).

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
 Grid = VM.grid_o,
 other_grid(Grid,Target), 
 is_grid(Target),!,
 grid_size(Target,H,V),!,VM.h==H,VM.v==V,
 mapgrid(fg_abtractiond,Grid,Target,NewGrid),mass(NewGrid,M),mass(Grid,M2),!,
 M>4,M2\==M,Grid\==Target,NewGrid\==Grid,

 %(M==0->maplist_ignore(fg_abtractiond,Target,Grid,NewGrid); MNewGrid=NewGrid),
 must_det_ll((
  var(GOID),atomic_list_concat([VMGID,'_fg_abtractiond'],GOID), assert_grid_gid(NewGrid,GOID),
  get_vm(VMS), 
  %individuate2(_,Subtraction,GOID,NewGrid,FoundObjs),
  individuate(Subtraction,NewGrid,FoundObjs),
  set_vm(VMS),
  ReColored = FoundObjs,
  %globalpoints_include_bg(VM.grid_o,Recolors), maplist(recolor_object(Recolors),FoundObjs,ReColored),
  print_grid(fg_abtractions(GOID),NewGrid),
  print_ss(ReColored),
  remCPoints(VM,ReColored),
  remGPoints(VM,ReColored),
  addInvObjects(VM,ReColored))))),!.



% =====================================================================
is_fti_step(after_subtract).
% =====================================================================
after_subtract(Shape,VM):-
  OldPoints = VM.points,
  OldGrid = VM.grid,
  must_det_ll((
  mapgrid(into_monochrome,OldGrid,MonoGrid),
  localpoints_include_bg(MonoGrid,Keep),
  points_to_grid(VM.h,VM.v,Keep,NewGrid),
  set(VM.points)=Keep,!,
  set(VM.grid)=NewGrid,
  run_fti(VM,Shape),
  set(VM.points)=OldPoints,
  set(VM.grid)=OldGrid)),
  !.

% =====================================================================
is_fti_step(drops_as_objects).
% =====================================================================
drops_as_objects(Name,VM):-
  %Objs = VM.objs,!,
 must_det_ll((
  Grid = VM.grid,
  %notrace(catch(call_with_depth_limit(individuate1(_,Name,VM.grid_o,IndvS0),20_000,R),E,(u_dmsg(E)))),
  individuate1(_,Name,Grid,_IndvS0)
  %maplist(override_object(iz(bp(Name))),IndvS0,IndvS1),
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

check_tid_gid3(GOID,GID):- maplist(check_tid_gid4,GOID,GID).
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
  %notrace(catch(call_with_depth_limit(individuate1(_,Name,VM.grid_o,IndvS0),20_000,R),E,(u_dmsg(E)))),
    must_grid_to_gid(GridIn,GOID),
    individuate2(_,ROptions,GOID,GridIn,IndvS0),
    maplist(add_birth_if_missing(indiv(ROptions)),IndvS0,IndvSL),
    once((delistify_single_element(ROptions,NamedOpts), save_grouped(individuate(NamedOpts,GOID),IndvSL))),!,
    TID = VM.id,
  asserta_in_testid(arc_cache:individuated_cache(TID,GOID,ROptions,IndvSL)),
  nop((addObjectOverlap(VM,IndvSL))))),!.

add_birth_if_missing(Birth,I,O):- is_list(I),maplist(add_birth_if_missing(Birth),I,O).
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
  raddObjects(VM,Obj).
*/

% =====================================================================
is_fti_step(point_corners).
% =====================================================================
point_corners(VM):-
  each_obj(VM.objs,Obj,
   (point_corners(Obj,Dir,OC-P1),
     is_adjacent_point(P1,Dir,P2),
     select(OC-P1,VM.points_o,Rest),     
     select_always(C-P2,Rest,Points0),C\==OC,
     sa_point(C-P2,Points0),
     remCPoints(VM,[C-P2]),
     make_point_object(VM,[iz(important)],C-P2,Obj),
     raddObjects(VM,Obj))).

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

maybe_alone_dots(lte(LTE),VM):- maybe_sa_dots(VM.points,lte(LTE),VM).
maybe_alone_dots(_,_).

% alone_dots(lte(5)) that have no adjacent points of the equal color (could be gathered first)

is_sa(Points,C-P2):-
    \+ free_cell(C),
    \+ (is_adjacent_point(P2,Dir,P3), Dir\==c, member(C-P3,Points), \+ is_diag(Dir)),!.

contains_alone_dots(Grid):- nonvar(Grid), globalpoints_maybe_bg(Grid,Points),include(is_sa(Points),Points,SAs),SAs\==[].

using_alone_dots(VM, _):-  fail, \+ contains_alone_dots(VM.grid_o), \+ contains_alone_dots(VM.grid_target),!.
using_alone_dots(_,Goal):- fail,  when_arc_expanding(once(Goal)).
using_alone_dots(_,Goal):- call(Goal).

maybe_alone_dots_by_color(lte(LTE),VM):-  
   available_fg_colors(TodoByColors),
   do_maybe_alone_dots_by_color(TodoByColors,lte(LTE),VM).

do_maybe_alone_dots_by_color(Color,lte(LTE),VM):- is_color(Color),!,
    my_partition(has_color(Color),VM.points,ThisGroup,_WrongColor),
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
    ( remCPoints(VM,SAPs),
     using_alone_dots(VM,(maplist(make_point_object(VM,[birth(alone_dots(lte(LTE))),iz(type(sa_dots)),iz(media(shaped))]),SAPs,IndvList),
     nop(raddObjects(VM,IndvList)))))))),!.

% =====================================================================
is_fti_step(maybe_lo_dots).
% =====================================================================
maybe_lo_dots(VM):-
  current_as_one(VM),
  %length(VM.points,Len), (VM.h>=Len ; VM.v>=Len), 
  lo_dots(VM).
maybe_lo_dots(_):-!.

% =====================================================================
is_fti_step(lo_dots).
% =====================================================================
% lo_dots may have adjacent points of the equal color (because we are in 'lo_dots' mode)
mostly_fgp(Points,LO_POINTS):- length(Points,Len), Len =< 49,!, Points=LO_POINTS.
mostly_fgp(Points,FG_POINTS):- my_partition(is_fgp,Points,FG_POINTS,_),!.

lo_dots(VM):-  
  mostly_fgp(VM.points,LO_POINTS),
  (true; VM.h=<5 ; VM.v=<5 ; (LO_POINTS \=[], length(LO_POINTS,Len), Len<12, (Len =< VM.h ; Len =< VM.v  ))),!,  
  using_alone_dots(VM,(maplist(make_point_object(VM,[birth(lo_dots),iz(stype(dot)),iz(media(shaped))]),LO_POINTS,IndvList),
  raddObjects(VM,IndvList))),
  set(VM.points) =[],!.
lo_dots(VM):-  
  mostly_fgp(VM.points,LO_POINTS),
   length(LO_POINTS,Len), Len<12,
  using_alone_dots(VM,(maplist(make_point_object(VM,[birth(lo_dots),iz(stype(dot))]),LO_POINTS,IndvList),
  raddObjects(VM,IndvList))),
  set(VM.points) =[],!.


/*
fti(VM,[colormass_merger(Size)|TODO]):-
  colormass_merger(Size,VM),!,
  colormass_merger(Size,VM),!,
  %colormass_merger(3,VM),
  set(VM.program_i) = TODO.
*/

% =====================================================================
is_fti_step(colormass_merger).
% =====================================================================
%colormass_merger(_Size,_VM):-!.
colormass_merger(Size,VM):-
  Objs = VM.objs,
  my_partition(less_mass(Size),Objs,Smaller,Bigger),
  maplist(colormass_merger(VM,Smaller),Bigger).

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
  %maplist(was_color(C),List),
  maplist(divide_colors(C),SubRow,NewSubRow,NewSubRowReplace),  
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
  maplist(=(L2),T),
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
  %maplist(was_color(C),FirstRow),

  mustlist_until_failed(row_not_isnt(LeftN,RightN,C),[FirstRow|Rest],RowsInvolvedClipped,Replacements,Below),
  length(RowsInvolvedClipped,N),N>1,!,
  append([BeforeFirstRow,Replacements,Below],NewGrid),
  nth1(1,RowsInvolvedClipped,First),
  last(RowsInvolvedClipped,Last),
  writeq(Last),nl,
  nop(maplist(was_color_or_unbound(C),First)),
  maplist(was_color_or_unbound(C),Last),!,
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

% tiny grid becomes a series of points
is_fti_step(maybe_glyphic).
maybe_glyphic(VM):- 
  Points = VM.points,
  if_t(is_glyphic(VM,Points,VM.h,VM.v),one_fti(VM,glyphic)).

%is_glyphic(Points,_GH,_GV):- length(Points,Len), Len < 5.
%is_glyphic(Points,_GH,_GV):- mass(Points,Len), Len =< 25,!.
is_glyphic(_VM,_Points,GH,GV):- ( GH=<4 , GV=<4 ).
%is_glyphic(_VM, Points,GH,GV):- ( GH=<3 , GV=<3 ), nop((length(Points,Len), Len is GH * GV)).
is_glyphic( VM,_Points,GH,GV):- ( GH=<5 , GV=<5 ), other_grid_size(VM.grid_o,OH,OV),!,
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
  localpoints_include_bg(VM.grid_o,Points),
  %length(Points,LenBG),
  %(LenBG=<15->UPoints=Points;mostly_fgp(Points,UPoints)),
  %length(UPoints,ULen),!,
  ignore(( %ULen=<15,
  UPoints = Points,
  VM.program_i=Code,
  %run_fti(VM,[i_by_color]),
  set(VM.program_i)=Code,
  using_alone_dots(VM,(maplist(make_point_object(VM,[birth(glyphic),iz(media(shaped))]),UPoints,IndvList), raddObjects(VM,IndvList),
  save_grouped(individuate(glyphic,VM.gid),IndvList))))))).

%make_point_object(VM,_Opts,Point,Indv):-
%    member(Point=Indv, VM.allocated_points),!.


named_grid_props(Name,VM):- one_fti(VM,named_grid_props(Name)),!.

is_fti_step(named_grid_props(name)).
one_fti(VM,named_grid_props(Name)):- nop(named_grid_props(Name,VM)).
/*
  H=VM.h,V=VM.v,
  Grid= VM.grid_o,
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
    Points,Whole),raddObjects(VM,Whole),
  save_grouped(individuate(whole,VM.gid),[Whole]),learn_hybrid_shape(pair,Whole))).
  /*
  if_t(Len>=0,
    (make_indiv_object(VM,[mass(Len),vis2D(H,V),iz(stype(whole)),iz(flag(always_keep)),loc2D(1,1),iz(media(image))|Props],Points,Whole),
      raddObjects(VM,Whole),
       save_grouped(individuate(whole,VM.gid),[Whole]),learn_hybrid_shape(pair,Whole))),
  localpoints(Grid,LPoints),
  length(LPoints,CLen),if_t((CLen=<144,CLen>=0),    
    (make_indiv_object(VM,[iz(stype(whole)),iz(media(shaped)),mass(Area),loc2D(1,1)],LPoints,Whole2),raddObjects(VM,Whole2))).
*/

% =====================================================================
is_fti_step(remove_used_points).
% =====================================================================
remove_used_points(VM):-  
  remCPoints(VM,VM.objs),
  points_to_grid(VM.h,VM.v,VM.points,Grid),
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
        raddObjects(VM,WasInside))),
  colormass_subshapes(VM,SubProgram).
colormass_subshapes(_,_):-!.



one_fti(VM,by_color(Min,C)):- 
  my_partition(has_color(C),VM.points,ThisGroup,LeftOver),
  ignore(((
   ThisGroup\==[],
   length(ThisGroup,Len),  Len >= Min,
   set(VM.points)=LeftOver,
   meets_indiv_criteria(VM,birth(by_color),ThisGroup),
   make_indiv_object(VM,[birth(by_color),iz(media(image)),iz(media(shaped))],ThisGroup,ColorObj),
   raddObjects(VM,ColorObj)))).

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
  %debug_indiv(Reserved),
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
   localpoints_include_bg(Shape,OPoints),
   offset_points(OH,OV,OPoints,ObjPoints),
   %Points = VM.points,
   %intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
   %Sofar = VM.robjs,
   %do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),   
   %my_append(Intersected,Use,All),
   %list_to_set(All,AllS), AllS \== [],  
   %set(VM.points) = RestOfPoints,
   %set(VM.objs)= Sofar2,
   %nl,writeq(((points_to_grid(RestOfPoints,GH,GV,NewGrid)))),nl,
   %set(VM.grid) = NewGrid,

   indv_props_list(Shape,ShapeProps), 
   my_partition(props_not_for_merge,ShapeProps,_Exclude,Include),
   make_indiv_object(VM,[birth(shape_lib(Method,LibName)),vis2D(SH,SV),loc2D(OH,OV)|Include],ObjPoints,Indiv),  %obj_to_oid(Shape,_,Iv), %override_object(obj_to_oid(VM.id,Iv),Indiv0,Indiv),  %make_indiv_object(VM,Use,Indiv),
   %nop(points_to_grid(RestOfPoints,set(VM.grid))),  %print_grid(Indiv),
   %raddObjects(VM,Indiv),
   nop(debug_indiv(Indiv)))).
   

/*  
use_shapelib(VM,Name,[Obj|Reserved]):-
   once((globalpoints(Obj,ObjPoints), 
   \+ color(Obj,black),
   Points = VM.points,
    Sofar = VM.objs,
   intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
         do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),
         my_append(Intersected,Use,All),
         list_to_set(All,AllS))), AllS \== [],
         make_indiv_object(VM,[iz(override(Name))|AllS],Indiv0), 
         obj_to_oid(Obj,_,Iv), 
         override_object(obj_to_oid(VM.id,Iv),Indiv0,Indiv), 
         raddObjects(VM,Indiv),
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
    remove_global_points(Retained,NewGPoints,set(VM.points)),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    set(VM.objs)= Retained,
    set(VM.grid)= NNewGrid,
    set(VM.points)= NextScanPoints,

    %as_debug(9,print_grid(H,V,'newgrid'+ID,NNewGrid)),
    !.

% =====================================================================
is_fti_step(release_points).
% =====================================================================
release_points(VM):- 
    globalpoints(VM.grid,NextScanPoints1),
    addCPoints(VM,NextScanPoints1),
    globalpoints(VM.objs,NextScanPoints2),
    addCPoints(VM,NextScanPoints2).

objs_into_single_hidden(VM):- objs_into_single_now(VM,[iz(info(combined)),iz(flag(hidden)),iz(into_single)]).
objs_into_single(VM):- objs_into_single_now(VM,[iz(info(combined)),iz(into_single)]).

objs_into_single_now(Opts,VM):-
    maplist(globalpoints,VM.objs,IndvPoints),
    meets_indiv_criteria(VM,into_single,IndvPoints),!,
    make_indiv_object(VM,Opts,IndvPoints,Indv),    
    raddObjects(VM,Indv).


% =====================================================================
is_fti_step(recompute_points).
% =====================================================================
recompute_points(VM):- 
    ignore((
    VM.objs\==[],
    globalpoints(VM.points_o,GridPoints),
    globalpoints(VM.objs,ObjsPoints),
    subtract(GridPoints,ObjsPoints,LeftOver),!,
    LeftOver\==[],
    points_to_grid(VM.h,VM.v,LeftOver,NewGrid),
    mapgrid(add_missing,NewGrid,NNewGrid),
    nop((as_ngrid(NNewGrid,NGrid),
    print_ss(green,NGrid,'recompute_points',_,VM.objs,'objects'))),
    
    gset(VM.points)= LeftOver,
    gset(VM.grid)= NewGrid)).

add_missing(Plain,wbg):- plain_var(Plain).
add_missing(X,X).
% =====================================================================
is_fti_step(leftover_as_one).
% =====================================================================
leftover_as_one(VM):-
   Points = VM.points,
   ignore((Points\==[],
   u_dmsg(leftover_as_one=Points),
   make_indiv_object(VM,[iz(info(combined)),iz(info(leftover_as_one))],Points,LeftOverObj), verify_object(LeftOverObj),
   raddObjects(VM,LeftOverObj))),
   VM.points=[].

% =====================================================================
is_fti_step(current_as_one).
% =====================================================================
current_as_one(VM):-
 Points = VM.points,
   ignore((
   Points\==[],
   %set_html_stream_encoding, 
   u_dmsg(current_as_one=Points),
   make_indiv_object(VM,[iz(info(combined)),birth(current_as_one)],Points,LeftOverObj), verify_object(LeftOverObj),
   raddObjects(VM,LeftOverObj),
   set(VM.points) = Points)).
   

ignore_rest(VM):- VM.points=[].


same_lcolor(LargestColor,Obj):- color(Obj,Color),nop(print_grid(Obj)),!,Color==LargestColor.




% @TODO will get sub objects later
not_list(G):- \+ is_list(G).

mapgroup(P2,G1,L2):- into_list(G1,L1),!, maplist(P2,L1,L2).
mapgroup(P1,G1):- into_list(G1,L1), !, maplist(P1,L1).

into_list(G,L):- is_list(G),!,L=G.
into_list(G,L):- is_vm_map(G),!,L = G.objs,my_assertion(is_list(L)).
into_list(I,O):- listify(I,O),!.

assume_vm(_).
:- style_check(-singleton).

addPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.Prop,ReallyAdd,set(VM.Prop)).

remPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,ReallyRemove,Keep,PretendToRemove),
  gset(VM.Prop) = Keep.

raddObjects(VM,Obj):- nop(assertion(member(Obj,VM.objs))).
addObjects(_VM,Obj):- Obj==[],!.
addObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  addGPoints(VM, Obj),
  addAPoint(VM, Obj),  
  my_append(VM.objs,ReallyAdd,set(VM.objs)).

addRObjects(_VM,Obj):- Obj==[],!.
addRObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.robjs,List,PretendToAdd,Prev,ReallyAdd),
  %addGPoints(VM, Obj),
  %addAPoint(VM, Obj),  
  my_append(VM.robjs,ReallyAdd,set(VM.robjs)).

addInvObjects(_VM,Obj):- Obj==[],!.
addInvObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.objs,ReallyAdd,set(VM.objs)).

remObjects(_VM,Obj):- Obj==[],!.
remObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,ReallyRemove,Keep,PretendToRemove),
  remGPoints(VM, ReallyRemove),
  set(VM.objs) = Keep.

addCPoints(_VM,Obj):- Obj==[],!.
addCPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints), 
  intersection(VM.points,CPoints,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.points,ReallyAdd,set(VM.points)).

remCPoints(_VM,Obj):- Obj==[],!.
remCPoints(VM,Obj):- is_group(Obj),!,mapgroup(remCPoints(VM),Obj).
remCPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.points,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.points) = Keep.

addPoints(_VM,Obj):- Obj==[],!.
addRPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints),
   intersection(VM.repaired,CPoints,PretendToAdd,Prev,ReallyAdd),
   my_append(VM.repaired,ReallyAdd,set(VM.repaired)).

remRPoints(_VM,Obj):- Obj==[],!.
remRPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.repaired,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.repaired) = Keep.


addAPoint(_VM,Obj):- Obj==[],!.
addAPoint(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints),
   ignore(((CPoints=[Point], member(Point=Other,VM.allocated_points), throw(Point=Other)))),
   ignore(((CPoints=[Point], append(VM.allocated_points,[Point=Obj],set(VM.allocated_points))))).



addOptions(VM, Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.options,ReallyAdd,set(VM.options)),
  intersection(VM.program_i,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.program_i,ReallyAdd,set(VM.program_i)).

remOptions(VM,Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.options) = Keep,
  intersection(VM.program_i,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.program_i) = Keep.


remGPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  Grid = VM.grid,
  remove_global_points(List,Grid,StartGrid),
  set(VM.grid) = StartGrid.
  
addGPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  Grid = VM.grid,
  replace_grid_points(List,_,Grid,StartGrid),
  set(VM.grid) = StartGrid.
  

:- style_check(+singleton).

%in_set(Set,I):- member(E,Set), E=@=I,!.

is_fti_stepr(remove_from_image(_Data)).
remove_from_image(VM,Data):-    
    must_det_ll((remove_global_points(Data,VM.points,Points),
    progress(Points),
    set(VM.points) = Points)),!.
   

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
  raddObjects(VM,Combined),
  cycle_back_in(VM,OptionC).


cycle_back_in(VM,OptionC):- 
  TODO = VM.program_i,
  length(TODO,N),
  cycle_back_in(VM,OptionC,N,TODO),!.
cycle_back_in(VM,OptionC,TODO):- 
  length(TODO,N), N2 is floor(N/2),length(LL,N2),my_append(LL,RR,TODO),my_append(LL,[OptionC|RR],OptionsOut),
  set(VM.program_i)= OptionsOut.

%cycle_back_in(VM,OptionC,0,TODO):- set(VM.program_i) = [OptionC].
cycle_back_in(_,OptionC,_,[T,A|_]):- (OptionC==T ; OptionC==A),!.
cycle_back_in(VM,OptionC,_,[T|ODO]):- !, set(VM.program_i)= [T,OptionC|ODO].
cycle_back_in(VM,OptionC,_,TODO):- set(VM.program_i)= [OptionC|TODO].





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
  raddObjects(VM,Combined),
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
  raddObjects(VM,Combined),
  cycle_back_in(VM,OptionC).
  

% =====================================================================
is_fti_step(extends).
% =====================================================================
extends(ShapeType1,VM):-
  Option = extends(ShapeType1), copy_term(Option,OptionC),!,
  select(HV1,VM.objs,SofarLess),isz(HV1,ShapeType1),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,MP),
  Points = VM.points,
  select(MC-MP,Points,ScanPoints),
  \+ (is_adjacent_point(P1,_,P2), is_adjacent_point(P2,_,MP),any_gpoint(HV1,_-P2)),
  all_individuals_near(VM,Dir,Option,C,[MC-MP],ScanPoints,NextScanPoints,IndvPoints),  
  combine_2objs(VM,HV1,[],IndvPoints,[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  set(VM.points)=NextScanPoints,
  raddObjects(VM,Combined),
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
   raddObjects(VM,Indv),!,
   ignore(one_ifti(VM,Shape)).

one_ifti(_VM,Option):- is_thing_or_connection(Option),!.

is_thing_or_connection(S):- no_repeats(S,is_thing_or_connection1(S)).
is_thing_or_connection1(Option):-allowed_dir(Option,_Dir).
is_thing_or_connection1(connects(_,_)).
is_thing_or_connection1(merge_shapes(_,_)).
is_thing_or_connection1(jumps(_,_)).


find_one_individual(Option,Obj,VM):- find_one_ifti3(Option,Obj,VM),!.
%find_one_individual(Option,Obj,VM):- find_one_ifti2(Option,Obj,VM),!.


find_one_ifti3(Option,Obj,VM):- 
    Points = VM.points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), \+ free_cell(C1), % non_free_fg(C2), % \+ is_black(C2),
    ok_color_with(C1,C2), %ScanPoints = Rest1, %maybe_multivar(C2), 
    is_adjacent_point(HV1,Dir,HV2),
    allowed_dir(Option,Dir), adjacent_point_allowed(C2,HV1,Dir,HV2), select(C2-HV2,Rest0,Rest1),
    FirstTwo = [C1-HV1,C2-HV2], points_allowed(VM,Option,FirstTwo),
    %i3(Option,Dir,Rest1,FirstTwo,ScanPoints,PointsFrom),
    FirstTwo=PointsFrom,
    Rest1 = ScanPoints,
    points_allowed(VM,Option,PointsFrom),
    all_individuals_near(_SkipVM,Dir,Option,C1,PointsFrom,ScanPoints,NextScanPoints,IndvPoints),!,
    length(IndvPoints,Len),Len>=2,!,
    make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth((ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.points) = NextScanPoints,
  raddObjects(VM,Obj),
  cycle_back_in(VM,OptionC),!.

/*
find_one_ifti3(Option,Obj,VM):- 
    Points = VM.points,
    %shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), \+ free_cell(C1), % non_free_fg(C2), % \+ is_black(C2),
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
  set(VM.points) = NextScanPoints,
  raddObjects(VM,Obj),
  cycle_back_in(VM,OptionC),!.
*/
%meets_indiv_criteria(_VM,_Info,[C-P1,C-P2]):- is_adjacent_point(P1,_Dir,P2),!,fail.
meets_indiv_criteria(VM,ShapeL,PointL):- is_list(ShapeL),!,forall(member(Shape,ShapeL),points_allowed(VM,Shape,PointL)).
meets_indiv_criteria(_VM,Shape,PointL):- points_allowed(PointL,Shape,PointL).

points_allowed(Nil,_Shape,_Point):- \+ Nil\=[], !.
points_allowed(VM,Shape,PointL):- is_vm(VM),!,points_allowed(VM.points_o,Shape,PointL).
points_allowed(VM,ShapeL,PointL):- is_list(ShapeL),member(Shape,ShapeL),points_allowed(VM,Shape,PointL),!.
points_allowed(VM,Shape,PointL):- maplist(point_allowed0(VM,Shape),PointL).


point_allowed(Nil,_Shape,_Point):- var(Nil),!.
point_allowed(Nil,_Shape,_Point):- \+ Nil\=[], !.
point_allowed(VM,Shape,Point):- is_vm(VM),!,point_allowed(VM.points_o,Shape,Point).
point_allowed(VM,ShapeL,Point):- is_list(ShapeL),member(Shape,ShapeL),point_allowed(VM,Shape,Point),!.
point_allowed(VM,Shape,Point):- point_allowed0(VM,Shape,Point).

point_allowed0(From,Shape,C-HV):- 
  findall(Dir, (is_adjacent_point(HV,Dir, HV2),\+ is_diag(Dir), member(C-HV2,From)),NonDiags),
  findall(Dir, (is_adjacent_point(HV,Dir, HV2), is_diag(Dir), member(C-HV2,From)),Diags),
  point_dirs_allowed(Shape,NonDiags,Diags).

point_dirs_allowed(nsew,[_],[_]):-!,fail. % dont allow stragglers
point_dirs_allowed(nsew,[],_):-!,fail. % dont allow stragglers
point_dirs_allowed(diamonds,NonDiags,_):- !, NonDiags==[].
point_dirs_allowed(_,_,_).

/*
find_one_ifti3(Option,Obj,VM):- 
    Points = VM.points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C1-HV1,Points,Rest0), \+ free_cell(C1), % non_free_fg(C2), % \+ is_black(C2),
    ok_color_with(C1,C2), %ScanPoints = Rest1, %maybe_multivar(C2), 
    is_adjacent_point(HV1,Dir,HV2),
    allowed_dir(Option,Dir), adjacent_point_allowed(C2,HV1,Dir,HV2), select(C2-HV2,Rest0,Rest1),
    FirstTwo = [C1-HV1,C2-HV2], points_allowed(VM,Option,FirstTwo),
    i3(Option,Dir,Rest1,FirstTwo,ScanPoints,PointsFrom),
    points_allowed(VM,Option,PointsFrom),
    all_individuals_near(VM,Dir,Option,C1,PointsFrom,ScanPoints,NextScanPoints,IndvPoints),
    make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth((ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.points) = NextScanPoints,
  raddObjects(VM,Obj),
  cycle_back_in(VM,OptionC),!.


i3(Option,Dir,Rest1,[C1HV1,C2-HV2],ScanPoints,[C1HV1,C2-HV2,C3-HV3]):-  
  ok_color_with(C2,C3), 
  ((adjacent_point_allowed(C3,HV2,Dir, HV3),select(C3-HV3,Rest1,ScanPoints));
   (allowed_dir(Option,Dir2),Dir2\=Dir,adjacent_point_allowed(C3,HV2,Dir2,HV3),select(C3-HV3,Rest1,ScanPoints))).

i3(Option,_Dir,ScanPointsRest1,PointsFromFirstTwo,ScanPointsRest1,PointsFromFirstTwo):- Option\==diamonds.
*/
/*
find_one_ifti2(Option,Obj,VM):- 
    Points = VM.points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
  select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(VM,Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints),!,
  make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType)),birth(i2(ShapeType))],IndvPoints,Obj),
    meets_indiv_criteria(VM,Option,IndvPoints),
  set(VM.points) = NextScanPoints,
  raddObjects(VM,Obj),
  cycle_back_in(VM,OptionC).
*/

point_groups_by_color(Option,[IndvPoints|Groups],Points,Rest):-    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
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

adjacent_point_allowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS),member(Dir,DirS).
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

meets_size(_,Points):- mass(Points,1).
meets_size(_,Points):- mass(Points,2),!,fail.
meets_size(_,Points):- mass(Points,3),!,fail.
meets_size(_,Points):- mass(Points,4).
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
  maplist(length,[IndvA,IndvB,IndvC],Rest),
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







individuation_macros(out, complete).
individuation_macros(in, complete).

individuation_macros(subshape_in_object, complete).

individuation_macros(train_mono_in_in, complete).
individuation_macros(train_mono_in_out, complete).
individuation_macros(train_mono_out_out, complete).

individuation_macros(in_in, complete).
individuation_macros(out_in, complete).
individuation_macros(out_out, complete).
individuation_macros(in_out, complete).


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




im_complete(ListO):- test_config(indiv(ListO)), [i_repair_patterns]\=@= ListO,[i_repair_patterns_f]\=@= ListO,!.
im_complete(i_complete_generic).
%im_complete(ListO):- ListO=[nsew,all_lines,diamonds,do_ending].
%im_complete([i_repair_patterns]):- get_current_test(TestID),is_symgrid(TestID),!.

%im_complete(i_repair_patterns):-!.

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
individuation_macros(i_mono_colormass,[fg_shapes(colormass)]).
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
%individuator(i_by_color,[by_color(0), by_color(0,wbg), by_color(0,fg),  reset_points, by_color(1,black),by_color(1,bg), by_color(1,fg),
 ,
 []]).
individuator(i_by_color,[by_color(0,wbg), by_color(1,black),by_color(1,bg), by_color(1,fg)]).
individuator(i_hybrid_shapes,[find_hybrid_shapes]).
individuator(i_repair_patterns,[maybe_repair_in_vm(find_symmetry_code)]).
*/

individuation_macros(i_repair_patterns_f,[repair_in_vm(find_symmetry_code)]).
individuation_macros(do_diags,[ /*dg_line(d), dg_line(u), */ diamonds]).

%individuator(i_colormass,[subshape_both(v,colormass), maybe_lo_dots]).

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
  reset_points, by_color(1,black),by_color(1,bg), by_color(1,fg),
  % ,
  []]).
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
individuator(i_mono,[save_as_obj_group(bg_shapes([subshape_both(h,nsew)])),
                          save_as_obj_group(bg_shapes([subshape_both(v,colormass)]))]).

*/
/*
%individuator(i_subobjs,[sub_indiv([save_as_obj_group(force_by_color),save_as_obj_group(i_colormass)])]).

%individuator(i_bg_nsew,[bg_shapes(subshape_both(h,nsew))]).
%individuator(i_mono_colormass,[fg_shapes([subshape_both(v,colormass)])]).
%individuator(i_fgbg,[by_color(1,bg), by_color(1,fg),do_ending]).
%individuator(i_diamonds,[subshape_both(h,diamonds), alone_dots(lte(5)), maybe_lo_dots]).
%individuator(i_decolorize,[subshape_both(v,decolorize), maybe_lo_dots]).
%individuator(i_monochrome,[subshape_both(h,into_monochrome), maybe_lo_dots]).
%individuator(i_mono_nsew,[decolorize,subshape_both(v,nsew), maybe_lo_dots]).
%individuator(i_mono_mass,[into_monocnhrome,subshape_both(v,colormass), maybe_lo_dots]).
% % %%  
%individuator(i_shapes,[subshape_both(h,std_shape_lib_lean),do_ending]).
% % %%  individuator(i_colormass,[subshape_both(v,colormass), maybe_lo_dots]).
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
  */
% eeee
/*
individuator(i_mono_nsew,
 [sub_individuate(
    bg_shapes([subshape_both(h,nsew)]),
   ([save_as_obj_group(force_by_color),save_as_obj_group(i_colormass)])),do_ending]).
*/

% 
find_symmetry_code(VM,Grid,RepairedResult,Code) :- 
  if_deepen_arc(find_symmetry_code1(VM,Grid,RepairedResult,Code)),!.

%if_deepen_arc(_):- !, fail.
if_deepen_arc(Goal):- once(Goal).
%never_repair_grid(Grid):- is_grid_symmetricD(Grid),!.
never_repair_grid(Grid):- get_current_test(TestID),kaggle_arc_io(TestID,_,out,G),G==Grid.

find_symmetry_code1(_VM,Grid,RepairedResult,Code):-  never_repair_grid(Grid),!,fail,RepairedResult=Grid,Code=[sameR].
find_symmetry_code1(VM,Grid,RepairedResult,Code):- 
   % \+ is_grid(VM.grid_target),
    copy_term(Grid,Orig),
    ignore((kaggle_arc_io(TestID,ExampleNum,in,Grid),
            kaggle_arc_io(TestID,ExampleNum,out,Out))),
    ignore((Out  = VM.grid_target)),

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
   \+ is_grid(VM.grid_target),!, repair_fourway(VM,Grid,RepairedResult,Steps).

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
    %when((colors_cc(i.points,Cs),len(Cs)<2),alone_dots(lte(5))), % any after this wont find individuals unless this is commented out
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


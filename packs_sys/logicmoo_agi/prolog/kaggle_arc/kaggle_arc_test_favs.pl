/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

%:- op(100,xfx,' * ').
:- dynamic(fav/2).
:- discontiguous fav/2.
fav(A,B):- nonvar_or_ci(A),nonvar_or_ci(B), cls1,mmake, asserta(fav(A,B),Ref),!, call_cleanup(arc1(A),erase(Ref)).


%fav(t('05269061'),[indiv(complete)]).
fav(t('007bbfb7'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,fractal_repetition,'(5, 1)']).
fav(t('00d62c1b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,loop_filling,'(5, 1)']).

fav(v(e99362f0),[combine_are_cells]).
fav(v('27a77e38'),[second_most_used_color]).
fav(v('7c9b52a0'),[giz([never_repair])]).
%fav(v('7c9b52a0'),[indiv([force_by_color,pbox_vm])]).
fav(v('7c9b52a0'),[indiv([colormass,bg_shapes(colormass)])]).
fav(t('484b58aa'),[indiv(i_repair_patterns_f)]).
fav(t('0dfd9992'),[indiv(i_repair_patterns_f)]).
fav(v('af22c60d'),[indiv(i_repair_patterns_f)]).
fav(v('903d1b4a'),[indiv(i_repair_patterns_f)]).
fav(v('e66aafb8'),[indiv(i_repair_patterns_f)]).
fav(v('4aab4007'),[indiv(i_repair_patterns_f)]).
fav(v('1e97544e'),[indiv(i_repair_patterns_f)]).
fav(t('b8825c91'),[indiv(i_repair_patterns_f)]).
fav(v('47996f11'),[indiv(i_repair_patterns_f)]).
fav(v('981571dc'),[indiv(i_repair_patterns_f)]).
fav(v('929ab4e9'),[indiv(i_repair_patterns_f)]).
fav(t('ff805c23'),[indiv(i_repair_patterns_f),human(repair_in_vm(repair_fourway),get(changed),trim_to_rect)]). fav(t('ff805c23'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_expansion,pattern_completion,crop,'(3, 1)']). %fav(t('ff805c23'),[human(repair_in_vm(repair_repeats(blue)),get(changed),trim_to_rect)]).
%fav(TestID,[indiv(i_repair_patterns_f)]):- is_symgrid(TestID).
fav(t('3631a71a'),[indiv(i_repair_patterns_f)]).   fav(t('3631a71a'),[human(repair_in_vm(repair_fourway),get(repaired))]). fav(t('3631a71a'),[-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_rotation,pattern_expansion,image_filling,'(4, 1)']).
fav(t('29ec7d0e'),[indiv(i_repair_patterns_f),human(repair_in_vm(repair_repeats(Black)),get(repaired))]):- get_black(Black). fav(t('29ec7d0e'),[-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,pattern_expansion,image_filling,detect_grid,'(4, 1)']).
fav(v('de493100'),[indiv(i_repair_patterns_f)]). fav(v(de493100),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
fav(t('9ecd008a'),[indiv(i_repair_patterns_f), human([indiv_is_one_hole,fix_image,selected_indiv,trim_to_rect])]).
fav(t('73251a56'),[indiv(i_repair_patterns_f),learn([learn_mapping_stateful]),human([apply_mapping_stateful])]).


fav(t('9aec4887'),[indiv(color_blind),todo_sol([find_individuals([hollow,inside([rectangle])],I),rest_indivdual(Is),put_inside(Is,I),
  if_edge_strong([color=C]),touch(Is,Point),set_color(C,Point)])]).

fav(t(a740d043),[human(remove_color(blue))]).

fav(t('47c1f68c'),[hedra,
   human(compute_max_color(C1),compute_next_color(C2),remove_color(C1),subst_color(C2,C1),blur(flipH),blur(flipV))]).
fav(t('47c1f68c'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recolor,image_repetition,image_reflection,find_the_intruder,detect_grid,crop,color_guessing,'(3, 1)']).
fav(v('cad67732'),[human(i(whole),first_object_term(rotated),learn_rule),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
fav(t('27a28665'),[human(i(whole),learn_rule)]).
%fav(t('27a28665'),[human(i([glyphic]),one_obj,into_monochrome,db(largest:shape,out:grid:p(1,1):color),resize_grid(1,1))]).
%fav(t('27a28665'),[human(i(whole),one_obj,into_monochrome,db(largest:shape,out:grid:p(1,1):color),resize_grid(1,1))]).
fav(t('27a28665'),[learn([shape_to_color]),no_sol([make_box(1),shape_to_color(C),cls_with(C)])]).
fav(t('27a28665'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_negative,associate_images_to_patterns,associate_colors_to_patterns,'(7, 3)']).
%fav(t('44f52bb0'),[human(i(whole),print_grid,get_vm(objs,[Obj|_]),print_grid,(call(iz(Obj,h_symmetric))->(print_grid,set_out([[blue]]));(print_grid,set_out([[orange]]))))]).
fav(t('d8c310e9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,pattern_expansion,pattern_completion,'(3, 1)']).
fav(t('44f52bb0'),[human(i(whole),first_object_bool(h_symmetric),learn_rule)]).

fav(t('40853293'),[indiv(by_color(2))]).
%fav(t(d631b094),human(globalpoints,grid_target=[get(points)],maplist(arg(1)))).
fav(t('d631b094'),[indiv(lo_dots),human(i(lo_dots),get(objs),learn_rule)]).
fav(t('d631b094'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,dominant_color,count_tiles,'(4, 1)']).

%fav(t('0d3d703e'),[human(i([columns,done]),db(objs:color,color),get(objs),learn_rule),-rotation_match,-color_match,+shape_match,+mask_match,tt,training,associate_colors_to_colors,'(4, 1)']).
fav(t('0d3d703e'),[indiv(i_columns),human(i([columns,done]),get(objs),learn_rule),-rotation_match,-color_match,+shape_match,+mask_match,tt,training,associate_colors_to_colors,'(4, 1)']).
%fav(t('a85d4709'),[human(i([rows,done]),o([rows,done]),   db(objs:shape,color=Color),set(objs:rectangle,objs:monochrome=true,objs:color=Color)),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,summarize,separate_images,associate_colors_to_images,'(4, 1)']).
fav(t('a85d4709'),[indiv(i_rows),learn(i([rows,done]),o([rows,done]),learn_rule),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,summarize,separate_images,associate_colors_to_images,'(4, 1)']).

fav(t('60b61512'),[indiv(i_pbox)]).

fav(t('d0f5fe59'),[%indiv(colormass),
  human(color(largest,Color),ray(Color-point_01_01,count),trim_to_rect),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,separate_shapes,pairwise_analogy,count_shapes,associate_images_to_numbers,'(3, 1)']).

/*
  Grid = [[1,1,1,1]]
  The Drawer does 
  Draw Grid = loc2D(1,1),line_hv(h), pen(1), amass(4)
  
  % Grid<->Drawer   Drawer<->Grid
  
  DSL for Grid 
  DSL for Drawer -> 
  DSL for Solver
  
  Slover = [- pen(1), + pen(2)]
  
  DrawGrid = loc2D(1,1),line_hv(h), pen(2), amass(5)
  Grid = [[2,2,2,2,2]]
*/
/*
fav(t('44f52bb0'),[human(i(whole),grid_call((
  get_vm(objs,[Obj|_]),(iz(Obj,h_symmetric)->set_vm(grid,[[blue]]);set_vm(grid,[[orange]])),set_vm(grid,[[orange]]))))]).
fav(t('44f52bb0'),[lmDSL(into_monochrome,grid_to_individual(whole),one_obj,resize_grid(1,1,Color),db(largest:h_symmetric,Color)), -shape_match,-rotation_match,-mask_match,-color_match,tt,training,detect_symmetry,associate_images_to_bools,'(6, 2)']).
*/

fav(t('f76d97a5'),[indiv([include_black,force_by_color]),was__lmDSL([compute_max_color(C1),compute_next_color(C2),remove_color(C1),subst_color(C2,C1)])]).

fav(t('25d487eb'),[indiv(colormass),human([rocketship])]).
fav(t('25d487eb'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,color_guessing,'(3, 1)']).
fav(v('762cd429'),[grid_size_same,human(i([shape_lib(filled_squares),delete_rest,shrink_all_to_size(1),tighten_grid_arround_objects])),-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).

%fav(t('6e82a1ae'),[human([rocketship])]).
fav(v('e41c6fd3'),[human([forall(((iz(X,outl),color(X,cyan),vert_pos(X,Vert))),
                                                                               (iz(Y,outl),vert_pos(Y,Vert)))])]).
fav(t(c444b776),[detect_grid]).
fav(v('94133066'),[human([largest_indiv,trim_to_rect,rot90,flipV])]).
fav(v('762cd429'),[indiv(shape_lib(filled_squares)),human(delete_rest,shrink_all_to_size(1),tighten_grid_arround_objects)]).
fav(v(f9d67f8b),[grid_size_same,human([overlay_each_pattern]),-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).

fav(t('e9bb6954'),[debug_indiv]).
fav(t('5582e5ca'),[grid_size_same,human([compute_max_color(_134548),cls_with(_134548)]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,dominant_color,count_tiles,'(3, 1)']).
fav(t('6f8cd79b'),[grid_size_same,human([add_borders(cyan)]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,ex_nihilo,contouring,'(4, 1)']).
fav(t('dae9d2b5'),[human([cut_in_half,overlay_all,set_all_fg(magenta)])]).
fav(t('d6ad076f'),[human([find_smaller,shoot_at_other,wide_beam])]).
fav(t('d511f180'),[indiv(i_by_color),human([swap_colors(cyan,silver)])]).
fav(t('a79310a0'),[human([gravity(1,s),swap_colors(cyan,red)])]).
fav(t('a48eeaf7'),[human([largest_indiv(I),tiny_individuals(Is),gravity_to(Is,I)])]).
fav(t('97999447'),[human([find_ones,until_edges([copy_right(silver),copy_right(sameR)])])]).
fav(t('8be77c9e'),[human([grow([[sameR],[flipV]])])]).
fav(t('7f4411dc'),[human([shave_away_1s])]).
fav(t('7b6016b9'),[human([fillFromBorder(green),subst_color(black,red)])]).
fav(t('6f8cd79b'),[human([add_borders(cyan)])]).
fav(t('6d58a25d'),[debug_indiv,print_grid,"the blue object is a downward beam maker, each beam must connect to one of its colors "]).
fav(t('6cf79266'),[learn([find(nines),remove_them]),human(reverse_learned)]).
fav(v(f9d67f8b),[human([overlay_each_pattern])]).



fav(t('9d9215db'),[human([overlay_each_pattern])]).
fav(t('810b9b61'),[human([(iz(X,rectangle),iz(X,hollow),iz(X,thick1),iz(X,noexit))-->color(X,green)])]).
fav(v('1d398264'),[human([((iz(X,keypad),iz(X,multicolor),centerof(X,C))-->sunburst(C))])]).
fav(v('e9bb6954'),[human([(iz(X,keypad), iz(X,monocolor),centerof(X,C)-->starburst(C))]),e('box of nine draw outward, if you hit a drawn line blacken it')]).
fav(t('5c2c9af4'),[human([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger])]).
fav(t('5582e5ca'),[human([compute_max_color(C1),cls_with(C1)])]).
fav(t('5521c0d9'),[human([with_each_indiv,move_above_itself])]).
fav(t('5521c0d9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,measure_length,'(3, 1)']).
fav(t('5117e062'),[human([find_two_color_indivs,selected_indiv,trim_to_rect,main_color,paint_landscape])]).
fav(t('44d8ac46'),[human([find_individuals([hollow,boxes,inside([rectangle])],I),indiv_fill_color(I,red)])]).
fav(t('447fd412'),[human([find_two_color_indivs,find_lesser_block,select_scaled_versions,builds,create_greater_blocks])]).
fav(t('447fd412'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_resizing,pattern_repetition,draw_pattern_from_point,'(3, 1)']).
fav(t('3c9b0459'),[human([rot180])]).

%fav(t('d511f180'),[human([compute_max_color(C1),compute_next_color(C2),swap_colors(C2,C1)])]).
fav(t(dae9d2b5),[human([cut_in_half,overlay_all,set_all_fg(magenta)]),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,recoloring,pattern_juxtaposition,'(5, 2)']).


fav(v('4b6b68e5'),
 [ 

human([
  doall((iz(Obj,outline),internal_region(Obj,Region),individuate_by_color(Region),
         largestIn(Region,Largest),color(Largest,Color),fill(Color,Region)))]),

   nthDSL(2,[gather_object(O1,X,(iz(X,dot),inside(X,P),iz(P,polygon),wall_thickness(P,1),noexit(P))),
          colors(O1,CC),first(C,CC),part_of(O1,E),color(E,C),fillAt(E,C),
                forall(X,(iz(X,dot), \+ (inside(X,P),iz(P,polygon))),delete(X))])

  ]).




is_fti_step(print_info).


fav(v(be03b35f),[human(
  (get_bgc(BG)),remove_color(BG),
   
   remove_color(red),   
   show_make_symmetrical
  %compute_max_color(C1),compute_next_color(C2), remove_color(C1),subst_color(C2,C1),
  %blur(flipH),blur(flipV)
  )]).
fav(v(be03b35f),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).

fav(t('4347f46a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,loop_filling,color_guessing,'(3, 1)']).

fav(v('73ccf9c2'),[human(colormass,most_unique(symmetry),get(solution),trim_to_rect),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).

fav(v('a8610ef7'),[grid_size_same,find_symmetry,-rotation_match,-color_match,+shape_match,+mask_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(4, 1)']).

fav(v('6ea4a07e'),[clue(amass(in)+amass(out)=:=9),human(corispond_colors,invert_existence),-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(6, 2) ']).

fav(t('23b5c85d'),[human([smallest_indiv,trim_to_rect])]).
fav(t('1cf80156'),[human([trim_to_rect])]).
fav(v('929ab4e9'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).
fav(v('94133066'),[human([largest_indiv,trim_to_rect,rot90,flipV]),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
fav(t('23b5c85d'),[human([smallest_indiv,trim_to_rect]),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_minimum,measure_area,crop,'(5, 1)']).
fav(t('8d5021e8'),[human_skip([grow([[rot180, flipV],[flipH, sameR],[rot180, flipV]])])]).

fav(t('6150a2bd'),[clue(amass(in)=:=amass(out)),human(rot180),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,image_rotation,'(2, 1)']).
fav(t('ed36ccf7'),[clue(amass(in)=:=amass(out)),human(rot270),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,image_rotation,'(4, 1)']).



:- style_check(-singleton).

fav(t('83302e8f'),
 [human(i(complete),
   with_objects(iz(poly(square)),subst_color(black,green)),
   with_objects(iz(shaped),subst_color(black,yellow)),   
   objects_into_grid)]).

fav(t(ff28f65a),[human(count_shapes,associate_images_to_numbers),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,count_shapes,associate_images_to_numbers,'(8, 3)']).
fav(t('1b60fb0c'),[
 %indiv([i_repair_patterns_f]),
 %human([new_things_are_a_color,fix_image]),
 %human([unbind_color(black),now_fill_in_blanks(blur(flipD)),subst_color(fg,red)]),
 %human([blur_least(_,fg),subst_color(blue,red),overlay_original]),
 %human([blur_least(_,fg),remember_repaired,with_objects(changedUntrimmed,[subst_color(blue,red),add_object(changedUntrimmed)])]),
 human([blur_least(_,_),remember_repaired,with_objects(changedUntrimmed,[subst_color(_,red),add_object(changedUntrimmed)])]),
 skip_human(
   in_out(In,Out),
   subtractGrid(Out,In,Alien),
   rot_by_90([Alien,A,B,C]),
   find_by_shape(In,Alien,[A,B,C]),
   find_by_shape(Out,Alien,[A,B,C,Alien]))]).
fav(t('1b60fb0c'),[grid_size_same, -rotation_match,-mask_match,-color_match,+shape_match,+'Errors',tt,training,pattern_rotation,pattern_expansion,pattern_deconstruction,'https://github.com/fchollet/ARC/pull/33','(3, 1)']).

%fav(t('23b5c85d'),[b7249182
%fav(t('db3e9e38'),[human([flipV,C1=orange,C2=blue,[],flipV]).
%fav(t(_),[human([fillFromBorder(none,yellow)])]).

fav(t('8d510a79'),[indivs(find_grids,colormass),grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,detect_wall,associate_colors_to_bools,'(2, 1)']).

fav(v('d304284e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).


fav(t('7b6016b9'),[grid_size_same,human([fillFromBorder(green),subst_color(Black,red)]),
  -rotation_match,-mask_match,-color_match,+shape_match,tt,training,loop_filling,color_guessing,background_filling,'(3, 1)']):- get_black(Black).
fav(t('1cf80156'),[out_grid(4,4),human([trim_to_rect]),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,crop,'(3, 1)']).
fav(t('8d5021e8'),[out_grid(4,9),human_skip([grow([[rot180,flipV],[flipH,sameR],[rot180,flipV]])]),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(3, 1)']).
fav(t('8be77c9e'),[out_grid(3,6),human_skip([grow([[sameR],[flipV]])]),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(3, 1)']).
fav(t(c9e6f938),[out_grid(6,3),human_skip([grow([[sameR,flipH]])]),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(3, 1)']).
fav(t('5c2c9af4'),[grid_size_same,human([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger]),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,rectangle_guessing,pattern_expansion,'(3, 1)']).
fav(t('7f4411dc'),[grid_size_same,human([shave_away_1s]),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,remove_noise,rectangle_guessing,'(3, 1)']).
fav(t('3c9b0459'),[grid_size_same,human([rot180]),-rotation_match,+shape_match,+mask_match,+color_match,tt,training,image_rotation,'(4, 1)']).
fav(t('44d8ac46'),[grid_size_same,human([find_individuals([hollow,boxes,inside([rectangle])],_138006),indiv_fill_color(_138006,red)]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,loop_filling,'(4, 1)']).
fav(t('ae4f1146'),[indiv(i_mono_nsew),learn([call(set_bgc(cyan))]),human([largest_indiv,trim_to_rect,set_bg(cyan)])]).
fav(t('ae4f1146'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,crop,count_tiles,'(4, 1)']).
fav(t('97999447'),[grid_size_same,human([find_ones,until_edges([copy_right(silver),copy_right(sameR)])]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,draw_line_from_point,'(3, 1)']).
fav(t('6cf79266'),[grid_size_same,learn([find(nines),remove_them]),human(reverse_learned),-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',tt,training,rectangle_guessing,recoloring,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(3, 1)']).
fav(t('810b9b61'),[grid_size_same,human([(iz(_140032,rectangle),iz(_140032,hollow),iz(_140032,thick1),iz(_140032,noexit)-->color(_140032,green))]),-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,detect_closed_curves,'(3, 1)']).

fav(t(a48eeaf7),[grid_size_same,human([largest_indiv(_136900),tiny_individuals(_136910),gravity_to(_136910,_136900)]),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,gravity,direction_guessing,bring_patterns_close,'(2, 1)']).
fav(v('4b6b68e5'),[grid_size_same,nthDSL(2,[gather_object(_145350,_145352,(iz(_145352,dot),inside(_145352,_145378),iz(_145378,polygon),wall_thickness(_145378,1),noexit(_145378))),colors(_145350,_145418),first(_145428,_145418),part_of(_145350,_145442),color(_145442,_145428),fillAt(_145442,_145428),forall(_145352,(iz(_145352,dot),\+ (inside(_145352,_145378),iz(_145378,polygon))),delete(_145352))]),human([doall((iz(_145548,outline),internal_region(_145548,_145562),individuate_by_color(_145562),largestIn(_145562,_145584),color(_145584,_145596),fill(_145596,_145562)))]),-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).

fav(t(d6ad076f),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,draw_line_from_point,connect_the_dots,bridges,'(3, 1)']).
fav(t('9d9215db'),[grid_size_same,human([overlay_each_pattern]),-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
fav(v(e41c6fd3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
fav(v('1d398264'),[grid_size_same,human([(iz(_140032,keypad),iz(_140032,multicolor),centerof(_140032,_140052)-->sunburst(_140052))]),-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 2) ']).
fav(v(e9bb6954),[grid_size_same,e('box of nine draw outward, if you hit a drawn line blacken it'),human([(iz(_142198,keypad),iz(_142198,monocolor),centerof(_142198,_142218)-->starburst(_142218))]),-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
fav(t(d511f180),[grid_size_same,human([swap_colors(cyan,silver)]),-rotation_match,-color_match,+shape_match,+mask_match,+'Errors',training,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021#760920',associate_colors_to_colors,'(3, 1)']).
fav(t('73251a56'),[grid_size_same,learn([learn_mapping_stateful]),human([apply_mapping_stateful]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,image_filling,diagonal_symmetry,'(3, 1)']).
fav(t(f76d97a5),[grid_size_same,was__lmDSL([compute_max_color(_134548),compute_next_color(_134558),remove_color(_134548),subst_color(_134558,_134548)]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_negative,recoloring,associate_colors_to_colors,'(3, 1)']).
fav(t(ce22a75a),[grid_size_same,hint(grow_blue),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,'(2, 1)']).
fav(t(a79310a0),[grid_size_same,human([gravity(1,s),swap_colors(cyan,red)]),-rotation_match,-mask_match,-color_match,+shape_match,tt,training,recoloring,pattern_moving,pairwise_analogy,'(3, 1)']).

fav(t(b230c067),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,separate_shapes,recoloring,find_the_intruder,associate_colors_to_bools,'(2, 1)']).
fav(t(d2abd087),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,separate_shapes,recoloring,count_tiles,associate_colors_to_numbers,'(3, 1)']).
fav(v('0a2355a6'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(4, 1) ']).
fav(v('37d3e8b2'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(3, 1) ']).
fav(t('6e82a1ae'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,count_tiles,associate_colors_to_numbers,'(3, 1)']).
fav(t(b6afb2da),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,replace_pattern,rectangle_guessing,recoloring,'(2, 1)']).
fav(t(e509e548),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,homeomorphism,associate_colors_to_shapes,'(3, 1)']).
fav(t(ea32f347),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,separate_shapes,recoloring,count_tiles,associate_colors_to_ranks,'(4, 1)']).
fav(t('08ed6ac7'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,order_numbers,measure_length,associate_colors_to_ranks,'(2, 1)']).
fav(v('626c0bcc'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
fav(v('639f5a19'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(2, 1) ']).

fav(v('6ea4a07e'),[clue(amass(in)+amass(out)=9),human(use_clues),clue(corispond_colors,invert_existence),-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(6, 2) ']).


/*Less*/ fav(t(ba97ae07),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,rettangle_guessing,recoloring,pattern_modification,pairwise_analogy,'(4, 1)']).
/*Less*/ fav(t(cbded52d),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,+'Errors',training,separate_images,pattern_repetition,pattern_modification,pattern_juxtaposition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',detect_grid,connect_the_dots,'(3, 1)']).
/*Less*/ fav(t(e8dc4411),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,pattern_expansion,direction_guessing,'(3, 1)']).
/*Less*/ fav(v('45737921'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('4e45f183'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('4f537728'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(dc2aa30b),  [grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('776ffc46'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,recoloring,find_the_intruder,detect_enclosure,associate_colors_to_patterns,'(4, 1)']).
/*Less*/ fav(t('150deff5'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,pattern_deconstruction,pattern_coloring,associate_colors_to_patterns,'(3, 1)']).
/*Less*/ fav(t('6e02f1e3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,count_different_colors,associate_images_to_numbers,'(5, 1)']).
/*Less*/ fav(t('995c5fa3'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,summarize,separate_images,detect_wall,associate_colors_to_images,'(4, 1)']).
/*Less*/ fav(t('25d8a9c8'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,recoloring,detect_hor_lines,'(4, 1)']).
/*Less*/ fav(v('17cae0c1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v(d4b1c2b1),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(7, 1) ']).
/*Less*/ fav(v('7039b2d7'),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('1190e5a7'),[out_grid(4,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,detect_grid,detect_background_color,create_image_from_info,count_ver_lines,count_hor_lines,color_guessing,'(3, 1)']).
/*Less*/ fav(v(e872b94a),[out_grid(1,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(d9fac9be),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,x_marks_the_spot,tt,training,summarize,find_the_intruder,'(4, 1)']).
/*Less*/ fav(t('445eab21'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,measure_area,'(3, 1)']).
/*Less*/ fav(t('239be575'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,detect_connectedness,associate_images_to_bools,'(6, 2)']).
/*Less*/ fav(t(f9012d9b),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_expansion,pattern_completion,crop,'(3, 1)']).
/*Less*/ fav(v('1a2e2828'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(5, 1) ']).
/*Less*/ fav(t(de1cd16c),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,summarize,separate_images,count_tiles,'(4, 1)']).
/*Less*/ fav(v(cd3c21df),[out_grid(1,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(a3325580),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,summarize,separate_shapes,remove_intruders,count_tiles,'(6, 1)']).
/*Less*/ fav(t('72ca375d'),[out_grid(4,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,find_the_intruder,detect_symmetry,crop,'(3, 1)']).
/*Less*/ fav(v('8597cfd7'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(be94b721),[out_grid(3,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,separate_shapes,crop,count_tiles,'(4, 1)']).
/*Less*/ fav(v('642d658d'),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b9b7f026),[out_grid(1,1),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,find_the_intruder,'(3, 1)']).
/*Less*/ fav(v('3194b014'),[-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('695367ec'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('332efdb3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('28e73c20'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,mimic_pattern,ex_nihilo,'(5, 1)']).
/*Less*/ fav(v(ed74f2f2),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(t(d4469b4b),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,dominant_color,associate_images_to_colors,'(7, 2)']).
/*Less*/ fav(t('017c7c7b'),[out_grid(3,9),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,pattern_repetition,pattern_expansion,image_expansion,'(3, 1)']).
/*Less*/ fav(t(e179c5f4),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,bouncing,'(3, 1)']).
/*Less*/ fav(v('32e9702f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t(f8c80d96),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,background_filling,'(3, 1)']).
/*Less*/ fav(v(a3f84088),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(d5d6de2d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,remove_intruders,loop_filling,'(3, 2)']).
/*Less*/ fav(v('1c0d0a4b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('9565186b'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,take_maximum,separate_shapes,recoloring,count_tiles,associate_color_to_bools,'(4, 1)']).
/*Less*/ fav(t(b1948b0a),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,associate_colors_to_colors,'(3, 1)']).
/*Less*/ fav(t('794b24be'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,count_tiles,associate_images_to_numbers,'(10, 2)']).
/*Less*/ fav(t(a9f96cdd),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,out_of_boundary,'(4, 1)']).
/*Less*/ fav(t('6773b310'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,detect_grid,count_tiles,associate_colors_to_numbers,'(4, 1)']).
/*Less*/ fav(v('3b4c2228'),[-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(5, 2) ']).
/*Less*/ fav(t('3428a4f5'),[out_grid(5,6),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,pattern_differences,detect_wall,'(4, 2)']).
/*Less*/ fav(v('195ba7dc'),[out_grid(6,5),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('5d2a5c43'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 2) ']).
/*Less*/ fav(v('66f2d22f'),[out_grid(7,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(d19f7514),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('1b2d62fb'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,pattern_intersection,detect_wall,'(5, 1)']).
/*Less*/ fav(t('94f9d214'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,separate_images,pattern_intersection,'(4, 1)']).
/*Less*/ fav(v(e345f17b),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 2) ']).
/*Less*/ fav(v('31d5ba1a'),[out_grid(5,3),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(5, 2) ']).
/*Less*/ fav(t('0520fde7'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,pattern_intersection,detect_wall,'(3, 1)']).
/*Less*/ fav(t(fafffa47),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,separate_images,pattern_intersection,'(5, 1)']).
/*Less*/ fav(t('2bcee788'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_reflection,image_filling,direction_guessing,background_filling,'(4, 1)']).
/*Less*/ fav(v('604001fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('009d5c81'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(5, 1) ']).
/*Less*/ fav(v('0c9aba6e'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('506d28a5'),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('34b99a2b'),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t('6430c8c4'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,separate_images,pattern_intersection,detect_wall,'(4, 1)']).
/*Less*/ fav(t('99b1bc43'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,separate_images,pattern_intersection,detect_wall,'(4, 1)']).
/*Less*/ fav(t(ce4f8723),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_intersection,take_complement,separate_images,detect_wall,'(4, 1)']).
/*Less*/ fav(t(f2829549),[out_grid(3,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_complement,separate_images,pattern_intersection,detect_wall,'(5, 1)']).
/*Less*/ fav(v(e133d23d),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('2037f2c7'),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9110e3c5'),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(7, 2) ']).
/*Less*/ fav(v(d5c634a2),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(7, 2) ']).
/*Less*/ fav(v('2072aba6'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).


/*Less*/ fav(t('83302e8f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,loop_filling,detect_grid,detect_closed_curves,associate_colors_to_bools,'(3, 1)']).
/*Less*/ fav(v('8a371977'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(a61f2674),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_minimum,take_maximum,separate_shapes,remove_intruders,recoloring,count_tiles,associate_colors_to_ranks,'(2, 1)']).
/*Less*/ fav(t('54d9e175'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,separate_images,detect_grid,associate_images_to_images,'(4, 1)']).
/*Less*/ fav(t(e8593010),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,loop_filling,holes,count_tiles,associate_colors_to_numbers,'(3, 1)']).
/*Less*/ fav(v('575b1a71'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(ccd554ac),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(t(eb5a1d5d),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,summarize,'(3, 1)']).
/*Less*/ fav(v('1990f7a8'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('91413438'),[out_grid(12,12),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,count_tiles,algebra,'(4, 1)']).
/*Less*/ fav(v('358ba94e'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t('80af3007'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_resizing,image_resizing,fractal_repetition,crop,'(3, 1)']).
/*Less*/ fav(v('5b6cbef5'),[out_grid(16,16),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(e57337a4),[-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('8719f442'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('2697da3f'),[out_grid(15,15),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t(eb281b96),[out_grid(17,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(2, 1)']).


/*Less*/ fav(v('8e2edd66'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('8b28cd80'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 2) ']).
/*Less*/ fav(v('0692e18c'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('007bbfb7'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,fractal_repetition,'(5, 1)']).
/*Less*/ fav(v(bc4146bd),[out_grid(20,4),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(bbc9ae5d),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_expansion,image_expansion,'(5, 1)']).
/*Less*/ fav(t('53b68214'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_expansion,image_expansion,'(3, 2)']).
/*Less*/ fav(v('48131b3c'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('28bf18c6'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_repetition,crop,'(3, 1)']).
/*Less*/ fav(t(f25fbde4),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_resizing,crop,'(3, 1)']).
/*Less*/ fav(t('3af2c5a8'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_rotation,image_repetition,image_reflection,'(3, 1)']).
/*Less*/ fav(t('46442a0e'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(3, 1)']).
/*Less*/ fav(v('59341089'),[out_grid(12,3),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(a59b95c0),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(ed98d772),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(t(bc1d5164),[-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_moving,pattern_juxtaposition,pairwise_analogy,crop,'(5, 1)']).
/*Less*/ fav(t('88a62173'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,separate_images,find_the_intruder,detect_grid,crop,'(3, 1)']).
/*Less*/ fav(t(feca6190),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_expansion,image_expansion,draw_line_from_point,diagonals,'(5, 1)']).
/*Less*/ fav(t('4c4377d9'),[out_grid(4,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(4, 1)']).
/*Less*/ fav(v(b1fc8b8e),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 2) ']).
/*Less*/ fav(t('6d0aefbc'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(4, 1)']).
/*Less*/ fav(t('746b3537'),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,direction_guessing,crop,'(5, 1)']).
/*Less*/ fav(v(fc754716),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e5c44e8f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(da515329),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).


/*Less*/ fav(t('8eb1be9a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,image_filling,'(2, 1)']).
/*Less*/ fav(t(a3df8b1e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,diagonals,bounce,'(3, 1)']).
/*Less*/ fav(t(ea786f4a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_modification,draw_line_from_point,diagonals,'(3, 1)']).
/*Less*/ fav(v('9ddd00f0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('56ff96f3'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,rectangle_guessing,pattern_completion,'(4, 1)']).
/*Less*/ fav(t('8f2ea7aa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,fractal_repetition,crop,'(3, 1)']).
/*Less*/ fav(t('99fa7670'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,'(4, 1)']).
/*Less*/ fav(t('623ea044'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,diagonals,'(3, 1)']).
/*Less*/ fav(t('3ac3eb23'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,draw_pattern_from_point,'(2, 1)']).
/*Less*/ fav(v(e619ca6e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('69889d6e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(ded97339),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v(bf32578f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('22168020'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,'(3, 1)']).


/*Less*/ fav(v(ba9d41b8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('6e19193c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,diagonals,'(2, 1)']).
/*Less*/ fav(t('7ddcd7ec'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,diagonals,'(3, 1)']).
/*Less*/ fav(v('705a3229'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('42a50994'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',tt,training,remove_noise,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/43',count_tiles,'(4, 1)']).
/*Less*/ fav(v('423a55dc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(5, 1) ']).
/*Less*/ fav(v(b9630600),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(e9afcf9a),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,pattern_modification,'(2, 1)']).
/*Less*/ fav(v('55783887'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(t('025d127b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',tt,training,pattern_modification,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(2, 1)']).
/*Less*/ fav(t('253bf280'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,direction_guessing,connect_the_dots,'(8, 1)']).
/*Less*/ fav(t('25ff71a9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,'(4, 2)']).




/*Less*/ fav(v('1c56ad9f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('42a15761'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('64a7c07e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('85b81ff1'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(d931c21c),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(f3e62deb),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(6, 2) ']).
/*Less*/ fav(t('1f85a75f'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,find_the_intruder,crop,'(2, 1)']).
/*Less*/ fav(t('5ad4f10b'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,remove_noise,recoloring,image_resizing,crop,color_guessing,'(3, 1)']).
/*Less*/ fav(v('2c0b0aff'),[out_grid(8,7),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t('8efcae92'),[out_grid(6,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,separate_images,rectangle_guessing,crop,count_tiles,'(3, 1)']).
/*Less*/ fav(t('9f236235'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,image_reflection,detect_grid,'(3, 1)']).
/*Less*/ fav(t('0b148d64'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,find_the_intruder,detect_grid,crop,'(3, 1)']).
/*Less*/ fav(v(aee291af),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('3de23699'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_negative,rectangle_guessing,crop,'(4, 1)']).
/*Less*/ fav(t('7468f01a'),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,image_reflection,crop,'(3, 1)']).
/*Less*/ fav(t(fcb5c309),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,separate_images,rectangle_guessing,recoloring,crop,count_tiles,'(3, 1)']).
/*Less*/ fav(v('5289ad53'),[out_grid(3,2),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('39a8645d'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,crop,count_patterns,'(3, 1)']).
/*Less*/ fav(v('351d6448'),[out_grid(13,3),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(t(b94a9452),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_negative,crop,'(3, 1)']).
/*Less*/ fav(t('2dc579da'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,find_the_intruder,detect_grid,crop,'(3, 1)']).
/*Less*/ fav(t('97a05b5b'),[out_grid(9,17),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,shape_guessing,pattern_moving,pattern_juxtaposition,crop,'(3, 1)']).
/*Less*/ fav(t('681b3aeb'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_moving,jigsaw,crop,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(t('48d8fb45'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,find_the_intruder,crop,'(3, 1)']).
/*Less*/ fav(t('1fad071e'),[out_grid(5,1),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,count_patterns,associate_images_to_numbers,'(3, 1)']).
/*Less*/ fav(t('539a4f51'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_expansion,image_expansion,'(3, 1)']).



/*Less*/ fav(v('4852f2fa'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 2) ']).
/*Less*/ fav(t(b0c4d837),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,measure_length,associate_images_to_numbers,'(6, 1)']).
/*Less*/ fav(v(c8b7cc0f),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('4522001f'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pairwise_analogy,image_rotation,'(2, 1)']).
/*Less*/ fav(t(a740d043),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,detect_background_color,crop,'(3, 1)']).
/*Less*/ fav(t('1f0c79e5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,direction_guessing,diagonals,'(4, 1)']).
/*Less*/ fav(v(c62e2108),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(ca8f78db),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9bebae7a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(5, 1) ']).
/*Less*/ fav(t(a78176bb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_intruders,draw_parallel_line,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('3345333e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,pattern_reflection,pattern_completion,'(2, 1)']).
/*Less*/ fav(t('7e0986d6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,color_guessing,'(2, 1)']).
/*Less*/ fav(t(e48d4e1a),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',training,pattern_moving,out_of_boundary,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/13049','https://github.com/fchollet/ARC/pull/37',detect_grid,count_tiles,'(4, 1)']).
/*Less*/ fav(t(aabf363d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_intruders,recoloring,color_guessing,'(2, 1)']).
/*Less*/ fav(t('1a07d186'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,find_the_intruder,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(t(caa06a1f),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,pattern_expansion,image_filling,'(3, 1)']).
/*Less*/ fav(v('50a16a69'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(f823c43c),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('4093f84a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,recoloring,projection_unto_rectangle,gravity,'(3, 1)']).
/*Less*/ fav(v('2a5f8217'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t(ce602527),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,size_guessing,shape_guessing,remove_intruder,find_the_intruder,crop,'(4, 1)']).
/*Less*/ fav(v('7bb29440'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('5833af48'),[out_grid(16,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('505fff84'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(bbb1b8b6),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(7, 2) ']).
/*Less*/ fav(t(a87f7484),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,find_the_intruder,crop,'(4, 1)']).
/*Less*/ fav(t('6cdd2623'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,find_the_intruder,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v(f5c89df1),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('6df30ad6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(d56f2372),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).



/*Less*/ fav(v('7d1f7ee8'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(bf699163),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('662c240a'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,find_the_intruder,detect_symmetry,crop,'(4, 1)']).
/*Less*/ fav(v(c3202e5a),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('8731374e'),[out_grid(6,7),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,rectangle_guessing,draw_line_from_point,crop,'(3, 1)']).
/*Less*/ fav(t('91714a58'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,find_the_intruder,'(3, 1)']).
/*Less*/ fav(v(c1990cce),[out_grid(13,13),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('310f3251'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v(fb791726),[out_grid(12,12),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(f5b8619d),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_expansion,image_repetition,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(v(f0afb749),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('10fcaaa3'),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,+'Errors',tt,training,pattern_expansion,image_repetition,'https://github.com/fchollet/ARC/pull/31','(4, 1)']).
/*Less*/ fav(v('48f8583b'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(v('6f473927'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e6de6e8f),[out_grid(7,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('8403a5d5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(t(aba27056),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,draw_line_from_point,diagonals,'(3, 1)']).
/*Less*/ fav(t('4258a5f9'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,contouring,'(2, 1)']).
/*Less*/ fav(v('21f83797'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('759f3fd3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t(db3e9e38),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,out_of_boundary,'(2, 1)']).
/*Less*/ fav(t(dc1df850),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,out_of_boundary,contouring,'(3, 1)']).
/*Less*/ fav(v(aa18de87),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('834ec97d'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,spacing,pattern_repetition,measure_distance_from_side,draw_line_from_border,'(3, 1)']).
/*Less*/ fav(t(a64e4611),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,background_filling,'(3, 1)']).
/*Less*/ fav(t(b60334d2),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,'(2, 1)']).
/*Less*/ fav(v('31adaf00'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('00dbd492'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('8fbca751'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(c97c0139),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('551d5bf1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('3eda0437'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_maximum,rectangle_guessing,recoloring,measure_area,'(4, 1)']).
/*Less*/ fav(t('2281f1f4'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_intersection,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('7447852a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,pairwise_analogy,'(3, 1)']).
/*Less*/ fav(v('4e469f39'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('913fb3ed'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,contouring,associate_colors_to_colors,'(4, 1)']).
/*Less*/ fav(v(e7639916),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(dbc1a6ce),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,connect_the_dots,'(4, 1)']).
/*Less*/ fav(v(e0fb7511),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(d4f3cd78),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,recoloring,draw_line_from_point,'(2, 1)']).
/*Less*/ fav(t(c1d99e64),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,draw_line_from_border,detect_grid,'(3, 1)']).
/*Less*/ fav(t('6d75e8bb'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,pattern_completion,'(3, 1)']).
/*Less*/ fav(t(b27ca6d3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,find_the_intruder,count_tiles,contouring,'(2, 1)']).
/*Less*/ fav(t(a5313dff),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,loop_filling,'(3, 1)']).
/*Less*/ fav(t(af902bf9),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,x_marks_the_spot,tt,training,ex_nihilo,'(3, 1)']).
/*Less*/ fav(v(fd4b2b02),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('5b526a93'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('7e02026e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('90f3ed37'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,recoloring,pattern_repetition,'(3, 1)']).
/*Less*/ fav(t(d06dbe63),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,pairwise_analogy,'(2, 1)']).
/*Less*/ fav(t(ef135b50),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',training,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/issues/28',draw_line_from_point,connect_the_dots,bridges,'(3, 1)']).
/*Less*/ fav(v(cb227835),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('60b61512'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_completion,'(2, 1)']).
/*Less*/ fav(t('4612dd53'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,pattern_completion,'(3, 1)']).
/*Less*/ fav(v(da2b0fe3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 2) ']).
/*Less*/ fav(v(bf89d739),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('9772c176'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t(e9614598),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,measure_length,direction_guessing,'(2, 2)']).
/*Less*/ fav(t('3aa6fb7a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_rotation,pattern_completion,'(2, 1)']).
/*Less*/ fav(t('22233c11'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,size_guessing,pattern_expansion,'(3, 1)']).
/*Less*/ fav(t(a699fb00),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v(aa300dc3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(e73095fd),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,loop_filling,'(3, 1)']).
/*Less*/ fav(t(a8d7556c),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,rectangle_guessing,recoloring,'(3, 1)']).
/*Less*/ fav(v(ac605cbb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(6, 1) ']).


/*Less*/ fav(v('60a26a3e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('6c434453'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,'(2, 1)']).
/*Less*/ fav(v('84f2aca1'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('55059096'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('54d82841'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,gravity,'(3, 1)']).
/*Less*/ fav(v(e88171ec),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('0b17323b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(t('41e4d17e'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,pattern_repetition,draw_line_from_point,'(2, 1)']).
/*Less*/ fav(t(b2862040),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,detect_closed_curves,associate_colors_to_bools,'(4, 1)']).
/*Less*/ fav(v('292dd178'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('67385a82'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,measure_area,associate_colors_to_bools,'(4, 1)']).


/*Less*/ fav(t(a5f85a15),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,pattern_modification,pairwise_analogy,'(3, 1)']).
/*Less*/ fav(t(aedd82e4),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,take_minimum,separate_shapes,recoloring,count_tiles,associate_colors_to_bools,'(4, 1)']).
/*Less*/ fav(t(ba26e723),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,pattern_modification,pairwise_analogy,'(5, 1)']).
/*Less*/ fav(t(bb43febb),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,rettangle_guessing,loop_filling,'(2, 1)']).
/*Less*/ fav(t(ce9e57f2),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,take_half,recoloring,count_tiles,'(3, 1)']).
/*Less*/ fav(t(d406998b),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,one_yes_one_no,cylindrical,'(4, 1)']).
/*Less*/ fav(v('817e6c09'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(ae58858e),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(bd14c3bf),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(3, 1)']).
/*Less*/ fav(v(ce039d91),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e7dd8335),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('456873bc'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(c8f0f002),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,associate_colors_to_colors,'(3, 1)']).
/*Less*/ fav(t('6a1e5592'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',tt,training,recoloring,pattern_moving,jigsaw,'https://github.com/fchollet/ARC/pull/16','(2, 1)']).
/*Less*/ fav(t(d90796e8),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,replace_pattern,'(3, 1)']).
/*Less*/ fav(v('12eac192'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v(c92b942c),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(db93a21d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,measure_length,measure_area,draw_line_from_point,contouring,algebra,'(4, 1)']).
/*Less*/ fav(t('6455b5f5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_minimum,take_maximum,measure_area,loop_filling,associate_colors_to_ranks,'(4, 1)']).
/*Less*/ fav(v('137f0df0'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('5207a7b5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('3bd67248'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,draw_line_from_border,diagonals,'(3, 1)']).
/*Less*/ fav(t('868de0fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',training,measure_area,loop_filling,'https://github.com/fchollet/ARC/pull/45',even_or_odd,color_guessing,associate_colors_to_bools,'(5, 1)']).
/*Less*/ fav(t(a65b410d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,count_tiles,associate_colors_to_ranks,'(3, 1)']).
/*Less*/ fav(v('62ab2642'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b7fb29bc),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(c0f76784),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,measure_area,loop_filling,associate_colors_to_numbers,'(3, 1)']).
/*Less*/ fav(v(dc2e9a9d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('543a7ed5'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,loop_filling,contouring,'(2, 1)']).
/*Less*/ fav(v('140c817e'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('694f12f3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,rectangle_guessing,measure_area,loop_filling,associate_colors_to_ranks,'(2, 1)']).
/*Less*/ fav(v('15663ba9'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('516b51b7'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(fea12743),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(3, 1)']).
/*Less*/ fav(v('84db8fc4'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('3f23242b'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v(fe9372f3),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('941d9a10'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pairwise_analogy,loop_filling,detect_grid,'(3, 1)']).
/*Less*/ fav(v(aa4ec2a5),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(d364b489),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,'(2, 1)']).
/*Less*/ fav(t('95990924'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,'(3, 1)']).
/*Less*/ fav(t('272f95fa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,mimic_pattern,grid_coloring,detect_grid,'(2, 1)']).
/*Less*/ fav(v(e9c9d9a1),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('6b9890af'),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,tt,training,pattern_resizing,pattern_moving,crop,'(3, 1)']).
/*Less*/ fav(t('4be741c5'),[out_grid(3,1),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,summarize,'(3, 1)']).
/*Less*/ fav(t(c8cbb738),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_moving,jigsaw,crop,'(3, 1)']).
/*Less*/ fav(t('3f7978a0'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,rectangle_guessing,find_the_intruder,crop,'(3, 1)']).
/*Less*/ fav(v('67636eac'),[out_grid(9,3),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('3979b1a8'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('60c09cac'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t(cce03e0d),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pairwise_analogy,image_repetition,image_expansion,'(3, 1)']).
/*Less*/ fav(v(c48954c1),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('9172f3a0'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_resizing,'(2, 1)']).
/*Less*/ fav(t(d10ecb37),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,crop,'(3, 1)']).
/*Less*/ fav(t('963e52fc'),[out_grid(12,5),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_expansion,image_expansion,'(3, 1)']).
/*Less*/ fav(v('0c786b71'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('00576224'),[out_grid(6,6)]).
/*Less*/ fav(v('8ba14f53'),[-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(t('67e8384a'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_rotation,image_repetition,image_reflection,'(4, 1)']).
/*Less*/ fav(t('7fe24cdd'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_rotation,image_repetition,'(3, 1)']).
/*Less*/ fav(t(ac0a08a4),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,size_guessing,image_resizing,count_tiles,'(3, 1)']).
/*Less*/ fav(t(b91ae062),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,size_guessing,image_resizing,count_different_colors,'(5, 1)']).
/*Less*/ fav(t('2dee498d'),[-shape_match,-rotation_match,-mask_match,+color_match,+'Errors',tt,training,'https://github.com/fchollet/ARC/issues/30',divide_by_n,detect_repetition,crop,'(3, 1)']).
/*Less*/ fav(t('7b7f7511'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,separate_images,detect_repetition,crop,'(3, 1)']).
/*Less*/ fav(t(c59eb873),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_resizing,'(3, 1)']).
/*Less*/ fav(v(d017b73f),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('6fa7a44f'),[out_grid(3,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,image_reflection,'(4, 1)']).
/*Less*/ fav(t(a416b8f3),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_repetition,'(3, 1)']).
/*Less*/ fav(t('1bfc4729'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,'(2, 1)']).
/*Less*/ fav(t(d22278a0),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',training,pattern_expansion,pairwise_analogy,'https://github.com/fchollet/ARC/pull/4','(4, 1)']).
/*Less*/ fav(t(bda2d7a6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,recoloring,pattern_modification,pairwise_analogy,color_permutation,'(3, 2)']).
/*Less*/ fav(v('3a301edc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(5, 1) ']).
/*Less*/ fav(t(b527c5c6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,size_guessing,pattern_expansion,draw_line_from_point,direction_guessing,contouring,'(4, 1)']).
/*Less*/ fav(t('0a938d79'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,direction_guessing,'(4, 1)']).
/*Less*/ fav(t(d037b0a7),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(t('3befdf3e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,take_negative,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v(c87289bb),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(f15e1fac),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,obstacles,gravity,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(v('79fb03f4'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(v(a934301b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('496994bd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_reflection,'(2, 1)']).
/*Less*/ fav(v('9def23fe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(d9f24cd1),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,obstacles,gravity,draw_line_from_point,'(2, 1)']).
/*Less*/ fav(v(f9a67cb5),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b942fd60),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(v(d37a1ef5),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('8cb8642d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b8cdaf2b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,pairwise_analogy,draw_line_from_point,diagonals,'(4, 1)']).
/*Less*/ fav(v('712bf12e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('1e32b0e9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,separate_images,pattern_completion,image_repetition,detect_grid,'(3, 1)']).
/*Less*/ fav(t('928ad970'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,rectangle_guessing,draw_rectangle,color_guessing,'(3, 1)']).
/*Less*/ fav(v(a57f2f04),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('4938f0c2'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v(d492a647),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('0962bcdd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,'(2, 1)']).
/*Less*/ fav(t(e5062a87),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',training,pattern_repetition,pattern_juxtaposition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','(3, 1)']).
/*Less*/ fav(v('54db823b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(4, 1)']).
/*Less*/ fav(v('58e15b12'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b7249182),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,'(3, 1)']).
/*Less*/ fav(t('4c5c2cf0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v('97239e3d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(e40b9e2f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_reflection,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v('782b5218'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('444801d8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,rectangle_guessing,pattern_repetition,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v(e5790162),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(baf41dbf),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('95a58926'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('760b3cac'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_reflection,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('5c0a986e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,diagonals,'(3, 1)']).
/*Less*/ fav(t(ecdecbb3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_modification,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(v('73c3b0d8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(f8be4b64),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors','https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(4, 1)']).
/*Less*/ fav(v('17b80ad2'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t('6d58a25d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,print_grid,draw_line_from_point,debug_indiv,'(3, 1)',"the blue object is a downward beam maker, each beam must connect to one of its colors "]).
/*Less*/ fav(t('7df24a62'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_repetition,pattern_juxtaposition,out_of_boundary,'(4, 1)']).
/*Less*/ fav(t('3bdb4ada'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,recoloring,pattern_repetition,holes,'(2, 1)']).
/*Less*/ fav(t('3618c87e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,gravity,'(3, 1)']).
/*Less*/ fav(v('917bccba'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(f1cefba8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_modification,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(v('92e50de0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('72207abc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(ac0c5833),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('0d87d2a6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('855e0971'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,separate_images,holes,draw_line_from_point,direction_guessing,'(4, 1)']).
/*Less*/ fav(v(f83cb3f6),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('3e980e27'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,pattern_reflection,pattern_juxtaposition,direction_guessing,'(4, 1)']).
/*Less*/ fav(t('29623171'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,take_maximum,separate_images,grid_coloring,detect_grid,count_tiles,'(3, 1)']).
/*Less*/ fav(v('72a961c9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('963f59bc'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(ec883f72),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,diagonals,'(4, 1)']).
/*Less*/ fav(t(d43fd935),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,projection_unto_rectangle,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('72322fa7'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,pattern_juxtaposition,'(3, 1)']).
/*Less*/ fav(v('18419cfa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('5b692c0f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('98cf29f8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(v('2546ccf6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('93c31fbe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('3391f8c0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('1c02dbbe'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('2c608aff'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,projection_unto_rectangle,draw_line_from_point,'(4, 1)']).
/*Less*/ fav(t('74dd1130'),[grid_size_same,+shape_match,+mask_match,+color_match,tt,training,image_reflection,diagonal_symmetry,'(4, 1)',3]).
/*Less*/ fav(t('85c4e7cd'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,recoloring,color_permutation,color_guessing,'(4, 1)']).
/*Less*/ fav(v('4364c1c4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9b4c17c4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(4, 2) ']).
/*Less*/ fav(t('05f2a901'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,direction_guessing,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(t('3906de3d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,gravity,'(3, 1)']).
/*Less*/ fav(t('5168d44c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,recoloring,pattern_moving,direction_guessing,contouring,'(3, 1)']).
/*Less*/ fav(t('6855a6e4'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,tt,training,pattern_moving,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('9dfd6313'),[grid_size_same,-mask_match,+shape_match,+color_match,tt,training,image_reflection,diagonal_symmetry,'(3, 1)',3]).
/*Less*/ fav(t(a1570a43),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,tt,training,rectangle_guessing,pattern_moving,'(4, 1)']).


/*Less*/ fav(t(dc433765),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',training,pattern_moving,only_one,'https://github.com/fchollet/ARC/issues/29',direction_guessing,'(7, 2)']).
/*Less*/ fav(t(f8a8fe49),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_reflection,pattern_moving,'(3, 1)']).
/*Less*/ fav(v('20981f0e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('4acc7107'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('67c52801'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('8ee62060'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9c56f360'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e1d2900e),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('7837ac64'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,grid_coloring,extrapolate_image_from_grid,detect_grid,crop,color_guessing,'(4, 1)']).
/*Less*/ fav(v('3ee1011a'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('2f0c5170'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('780d0b14'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,detect_grid,'(3, 1)']).
/*Less*/ fav(v(d4c90558),[out_grid(8,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('81c0276b'),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('7c9b52a0'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b190f7f5),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,replace_pattern,image_resizing,image_expasion,color_palette,'(3, 1)']).
/*Less*/ fav(v('19bb5feb'),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('20818e16'),[out_grid(8,6)]).
/*Less*/ fav(t(e6721834),[out_grid(17,15),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,
  pattern_moving,pattern_juxtaposition,crop,'(3, 1)']).
/*Less*/ fav(t(f8ff0b80),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,separate_shapes,order_numbers,count_tiles,'(3, 1)']).
/*Less*/ fav(v('50aad11f'),[out_grid(4,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b7cb93ac),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('12997ef3'),[out_grid(9,3),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 2) ']).
/*Less*/ fav(t(e50d258f),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,separate_images,detect_background_color,crop,count_tiles,'(3, 1)']).
/*Less*/ fav(t('5614dbcf'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,remove_noise,image_resizing,'(2, 1)']).
/*Less*/ fav(t(e98196ab),[out_grid(11,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,image_juxtaposition,detect_wall,'(3, 1)']).
/*Less*/ fav(t(e3497940),[out_grid(4,10),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,image_reflection,image_juxtaposition,detect_wall,'(3, 1)']).
/*Less*/ fav(t('234bbc79'),[out_grid(7,3),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,crop,bring_patterns_close,'(4, 1)']).
/*Less*/ fav(v('62b74c02'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(bd4472b8),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,ex_nihilo,detect_wall,color_palette,color_guessing,'(3, 1)']).
/*Less*/ fav(v('7c8af763'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b548a754),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,x_marks_the_spot,tt,training,pattern_modification,pattern_expansion,'(3, 1)']).
/*Less*/ fav(v('9f27f097'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e95e3d8e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(c663677b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('1d0a4b61'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('321b1fc6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,pattern_juxtaposition,'(2, 1)']).
/*Less*/ fav(t('2204b7a8'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,proximity_guessing,'(3, 1)']).
/*Less*/ fav(t(c9f8e694),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,pattern_repetition,color_palette,'(2, 1)']).
/*Less*/ fav(t(e76a88a6),[indiv(i_mono),grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,pattern_repetition,pattern_juxtaposition,'(2, 1)']).
/*Less*/ fav(v(c7d4e6ad),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(fafd9572),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(a680ac02),[out_grid(8,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b4a43f3b),[out_grid(18,18),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(f8b3ba0a),[out_grid(1,3),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,order_numbers,find_the_intruder,dominant_color,detect_grid,count_tiles,'(4, 1)']).
/*Less*/ fav(t('8e1813be'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,image_within_image,direction_guesingcrop,color_guessing,'(3, 1)']).
/*Less*/ fav(v('4c177718'),[out_grid(15,9),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 2) ']).
/*Less*/ fav(v('3d31c5b3'),[out_grid(6,3),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(6, 1) ']).
/*Less*/ fav(t('75b8110e'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,image_juxtaposition,'(5, 1)']).
/*Less*/ fav(t(cf98881b),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,pattern_juxtaposition,detect_wall,'(5, 1)']).
/*Less*/ fav(v('477d2879'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).


/*Less*/ fav(v(d2acf2cb),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e9b4f6fc),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(a68b268e),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,pattern_juxtaposition,detect_grid,'(6, 1)']).
/*Less*/ fav(t(d23f8c26),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,image_expansion,crop,'(3, 1)']).
/*Less*/ fav(v(f4081712),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(t('31aa019c'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,remove_noise,find_the_intruder,contouring,'(3, 1)']).
/*Less*/ fav(t('9ecd008a'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_rotation,pattern_reflection,pattern_expansion,image_filling,crop,'(3, 1)']).
/*Less*/ fav(v('67b4a34d'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(f5aa3634),[out_grid(4,3),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9a4bb226'),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(e26a3af2),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,separate_images,remove_noise,'(3, 1)']).
/*Less*/ fav(t('5bd6f4ac'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,rectangle_guessing,crop,'(4, 1)']).
/*Less*/ fav(v(ad7e01d0),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('469497ad'),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,image_resizing,draw_line_from_point,diagonals,'(3, 1)']).
/*Less*/ fav(v('15696249'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('27f8ce4f'),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t(c3e719e8),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,take_maximum,image_repetition,image_expansion,count_different_colors,'(3, 1)']).
/*Less*/ fav(t('9af7a82c'),[out_grid(3,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,separate_images,order_numbers,count_tiles,'(4, 1)']).
/*Less*/ fav(v('692cd3b6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b15fca0b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(ff72ca3e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('2bee17df'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_maximum,draw_line_from_border,count_tiles,'(3, 1)']).
/*Less*/ fav(t('23581191'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_intersection,draw_line_from_point,'(2, 1)']).
/*Less*/ fav(v('45bbe264'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('67a423a3'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_intersection,contouring,'(3, 1)']).
/*Less*/ fav(v('770cc55f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t(f35d900a),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,'(4, 1)']).


/*Less*/ fav(t('673ef223'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,recoloring,portals,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(t(bdad9b1f),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_intersection,recoloring,draw_line_from_point,direction_guessing,'(2, 1)']).
/*Less*/ fav(v(ac3e2b04),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('29c11459'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,draw_line_from_point,count_tiles,'(2, 1)']).
/*Less*/ fav(v('3490cc26'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t(d4a91cb9),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,direction_guessing,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v(c074846d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(5, 2) ']).
/*Less*/ fav(t(a2fd1cf0),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v(f3b10344),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('508bd3b6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_reflection,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('56dc2b01'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,gravity,direction_guessing,'(3, 1)']).
/*Less*/ fav(v('992798f6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('896d5239'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(a04b2602),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(bcb3040b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('32597951'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,find_the_intruder,'(3, 1)']).
/*Less*/ fav(t('36fdfd69'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,rectangle_guessing,recoloring,'(3, 1)']).
/*Less*/ fav(t('50846271'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,pattern_completion,'(4, 1)']).
/*Less*/ fav(t('50cb2852'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,rectangle_guessing,holes,'(3, 1)']).
/*Less*/ fav(v('14754a24'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('1acc24af'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('22a4bbc2'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('7d419a02'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(d94c3b52),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('212895b5'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('0ca9ddb6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,associate_patterns_to_colors,'(3, 1)']).
/*Less*/ fav(v('9caba7c3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(d47aa2ff),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('891232d6'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('2753e76c'),[-shape_match,-rotation_match,-mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('833dafe3'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(e1baa8a4),[out_grid(2,2),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('4290ef0e'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_moving,crop,concentric,'(3, 1)']).
/*Less*/ fav(t('846bdb03'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,tt,training,pattern_reflection,pattern_moving,crop,color_matching,'(4, 1)']).
/*Less*/ fav(v(b7999b51),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('73182012'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e78887d1),[out_grid(11,3),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(ce8d95cc),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('2013d3e2'),[-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_deconstruction,crop,'(2, 1)']).
/*Less*/ fav(v('6a11f6da'),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('7953d61e'),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('68b67ca3'),[-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(d13f3404),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_expansion,draw_line_from_point,diagonals,'(3, 1)']).
/*Less*/ fav(t('62c24649'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,image_rotation,image_repetition,image_reflection,'(3, 1)']).
/*Less*/ fav(v('08573cc6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('178fcbfb'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(v('0f63c0b9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('52fd389e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(f25ffba3),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,pattern_reflection,'(2, 1)']).
/*Less*/ fav(v('29700607'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e69241bd),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e760a62e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b782dc8a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,maze,'(2, 1)']).
/*Less*/ fav(v(e7b06bea),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('9c1e755f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('57aa92db'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_resizing,pattern_repetition,draw_pattern_from_point,'(4, 1)']).
/*Less*/ fav(v('94414823'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('99306f82'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('12422b43'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(5, 1) ']).
/*Less*/ fav(v('96a8c0cd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('39e1d7f9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_repetition,grid_coloring,detect_grid,'(3, 1)']).
/*Less*/ fav(t(e21d9049),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,draw_line_from_point,color_palette,'(2, 1)']).
/*Less*/ fav(v('27a77e38'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(cf133acc),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('045e512c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,direction_guessing,'(3, 1)']).
/*Less*/ fav(v(c35c1b4c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('2dd70a9a'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,maze,draw_line_from_point,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('1f876c06'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,diagonals,connect_the_dots,'(3, 1)']).
/*Less*/ fav(t('06df4c85'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,grid_coloring,detect_grid,connect_the_dots,'(3, 1)']).
/*Less*/ fav(v('9b2a60aa'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('2b01abd0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('642248e4'),[grid_size_same]).
/*Less*/ fav(v('0607ce86'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('94be5b80'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(b7f8a4d8),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('11852cab'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',tt,training,pattern_expansion,'https://github.com/fchollet/ARC/pull/33','(3, 1)']).
/*Less*/ fav(v(ecaa0ec1),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('2c737e39'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(c6e1b8da),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(bb52a14b),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('03560426'),[grid_size_same]).
/*Less*/ fav(t('890034e9'),[grid_size_same]).
/*Less*/ fav(t('264363fd'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,pattern_repetition,pattern_juxtaposition,draw_line_from_point,'(3, 1)']).
/*Less*/ fav(t('67a3c6ac'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,image_reflection,'(3, 1)']).
/*Less*/ fav(t('6aa20dc0'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,pattern_resizing,pattern_repetition,pattern_juxtaposition,'(3, 1)']).
/*Less*/ fav(t(d07ae81c),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,draw_line_from_point,diagonals,color_guessing,'(3, 1)']).
/*Less*/ fav(t('1caeab9d'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,pattern_alignment,'(3, 1)']).
/*Less*/ fav(t(beb8660c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,order_numbers,count_tiles,'(3, 1)']).
/*Less*/ fav(v('09c534e7'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('15113be4'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('40f6cd08'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('4ff4c9da'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('7ee1c6ea'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(ac2e8ecf),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(dd2401ed),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e21a174a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 2) ']).
/*Less*/ fav(v(e74e1818),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('90c28cc7'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,rectangle_guessing,crop,'(3, 1)']).
/*Less*/ fav(t('1c786137'),[out_grid(8,6),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,detect_enclosure,crop,'(3, 1)']).
/*Less*/ fav(v(c658a4bd),[out_grid(8,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('93b4f4b3'),[out_grid(6,10),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(ea9794b1),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(t('8e5a5113'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',training,separate_images,image_rotation,image_repetition,'https://github.com/fchollet/ARC/pull/8',detect_wall,'(3, 1)']).
/*Less*/ fav(v('4cd1b7b2'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('9b365c51'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('88a10436'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,pattern_juxtaposition,'(3, 1)']).
/*Less*/ fav(v(f21745ec),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(ddf7fa4f),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,color_palette,'(3, 1)']).
/*Less*/ fav(v('103eff5b'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('33b52de3'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('845d6e51'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b457fec5),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(c909285e),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,rectangle_guessing,find_the_intruder,crop,'(3, 1)']).
/*Less*/ fav(v('414297c0'),[out_grid(11,12),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(c64f1187),[out_grid(11,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(b0f4d537),[out_grid(7,9),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e99362f0),[out_grid(4,5),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(6, 1) ']).
/*Less*/ fav(v('281123b4'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(6, 1) ']).
/*Less*/ fav(t('7c008303'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,separate_images,recoloring,detect_grid,crop,color_palette,color_guessing,'(3, 1)']).
/*Less*/ fav(t(dc0a314f),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_completion,crop,'(3, 1)']).
/*Less*/ fav(t('77fdfe62'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,detect_grid,crop,color_guessing,'(3, 1)']).
/*Less*/ fav(t('49d1d64f'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_expansion,image_expansion,'(3, 1)']).
/*Less*/ fav(v('05a7bcf2'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('0e671a1a'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(v('11e1fe23'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(t('5daaa586'),[out_grid(12,15),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,draw_line_from_point,direction_guessing,detect_grid,crop,'(3, 1)']).
/*Less*/ fav(t('46f33fce'),[out_grid(20,20),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_resizing,image_resizing,'(3, 1)']).
/*Less*/ fav(t('8a004b2b'),[out_grid(14,9),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,rectangle_guessing,pattern_resizing,pattern_repetition,pattern_juxtaposition,crop,'(3, 1)']).
/*Less*/ fav(t(a61ba2ce),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,+color_match,tt,training,pattern_moving,jigsaw,crop,bring_patterns_close,'(2, 1)']).
/*Less*/ fav(t('137eaa0f'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_juxtaposition,'(3, 1)']).
/*Less*/ fav(v(e633a9e5),[out_grid(5,5),-shape_match,-rotation_match,-mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('93b581b8'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_expansion,out_of_boundary,color_guessing,'(3, 1)']).
/*Less*/ fav(v('13713586'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(a406ac07),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('2685904e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(6, 1) ']).
/*Less*/ fav(t('82819916'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',training,pattern_repetition,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/32',draw_line_from_point,color_guessing,associate_colors_to_colors,'(4, 1)']).
/*Less*/ fav(v('88207623'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('22eb0ac0'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,connect_the_dots,color_matching,'(3, 1)']).
/*Less*/ fav(t(c444b776),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,separate_images,image_repetition,find_the_intruder,detect_grid,'(2, 1)']).
/*Less*/ fav(t(b775ac94),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,recoloring,pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,pattern_expansion,direction_guessing,'(3, 1)']).
/*Less*/ fav(t('36d67576'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,'(3, 1)']).
/*Less*/ fav(v('42918530'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('79369cc6'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('0e206a2e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_rotation,pattern_repetition,pattern_reflection,pattern_juxtaposition,associate_patterns_to_patterns,'(3, 1)']).
/*Less*/ fav(v('696d4842'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('1e0a9b12'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,pattern_moving,gravity,'(3, 1)']).
/*Less*/ fav(t('1f642eb9'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,tt,training,projection_unto_rectangle,image_within_image,'(3, 1)']).
/*Less*/ fav(t('228f6490'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,x_marks_the_spot,tt,training,shape_guessing,pattern_moving,loop_filling,'(3, 1)']).
/*Less*/ fav(t(ae3edfdc),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,gravity,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(v('0becf7df'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('16b78196'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v('5ffb2104'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('66e6c45b'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('8dae5dfc'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('90347967'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(a096bf4d),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(df8cc377),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e681b708),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(f3cdc58f),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(f45f5ca7),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('7d18a6fb'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e7a25a18),[out_grid(10,10),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('0bb8deee'),[out_grid(6,6),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('136b0064'),[out_grid(7,7),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(ca8de6ea),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('363442ee'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,pattern_juxtaposition,detect_wall,'(3, 1)']).
/*Less*/ fav(v('5a5a2103'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t(c3f564a4),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,image_filling,'(3, 1)']).
/*Less*/ fav(t('0dfd9992'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_expansion,image_filling,'(3, 1)']).
/*Less*/ fav(t('484b58aa'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,pattern_repetition,pattern_expansion,image_filling,'(3, 1)']).
/*Less*/ fav(v(af22c60d),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(e9ac8c9e),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(d89b689b),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,summarize,pattern_juxtaposition,direction_guessing,'(3, 1)']).
/*Less*/ fav(t(d687bc17),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,+'Errors',training,remove_intruders,'https://github.com/fchollet/ARC/pull/39',gravity,find_the_intruder,direction_guessing,bring_patterns_close,'(3, 1)']).
/*Less*/ fav(v('903d1b4a'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).
/*Less*/ fav(t('63613498'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,recoloring,detect_wall,compare_image,'(3, 1)']).
/*Less*/ fav(v('1da012fc'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(v(ef26cbf6),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('0a1d4ef5'),[-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('6d0160f0'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,separate_image,pattern_moving,find_the_intruder,detect_grid,'(4, 1)']).
/*Less*/ fav(v('0934a4d8'),[out_grid(4,4),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(4, 1) ']).
/*Less*/ fav(t('6ecd11f4'),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,recoloring,pattern_resizing,crop,color_palette,'(3, 1)']).
/*Less*/ fav(v(b0722778),[out_grid(2,11),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v(e66aafb8),[out_grid(5,8),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v('1a6449f1'),[out_grid(7,6),-shape_match,-rotation_match,-mask_match,-color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('25094a63'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,test,evaluation,'(2, 1) ']).
/*Less*/ fav(t('9aec4887'),[out_grid(6,6),todo_sol([find_individuals([hollow,inside([rectangle])],_137826),rest_indivdual(_137858),put_inside(_137858,_137826),if_edge_strong([color=_137892]),touch(_137858,_137904),set_color(_137892,_137904)]),indiv(color_blind),-shape_match,-rotation_match,-mask_match,+color_match,x_marks_the_spot,tt,training,recoloring,pattern_moving,crop,color_guessing,'(3, 1)']).
/*Less*/ fav(v('9356391f'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(v('85fa5666'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v(f0df5ff0),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('40853293'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',tt,training,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',connect_the_dots,'(2, 1)']).
/*Less*/ fav(v('070dd51e'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,+'Errors',test,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021',evaluation,'(2, 1) ','(2, 1)']).
/*Less*/ fav(v('3ed85e70'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(t('68b16354'),[grid_size_same,+shape_match,+mask_match,+color_match,tt,training,image_reflection,'(3, 1)',2]).
/*Less*/ fav(v(ea959feb),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(fd096ab6),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t('952a094c'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,rectangle_guessing,inside_out,'(3, 1)']).
/*Less*/ fav(v('50f325b5'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('58743b76'),[grid_size_same,-rotation_match,+shape_match,+mask_match,+color_match,evaluation,'(2, 1) ']).
/*Less*/ fav(t(cdecee7f),[-shape_match,-rotation_match,-mask_match,-color_match,tt,training,summarize,pairwise_analogy,'(3, 1)']).
/*Less*/ fav(v('1e97544e'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('4aab4007'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('09629e4f'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,take_minimum,separate_images,enlarge_image,detect_grid,create_grid,count_tiles,adapt_image_to_grid,'(4, 1)']).
/*Less*/ fav(v('184a9768'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(af24b4cc),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(b20f7c8b),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(aab50785),[out_grid(5,4),-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(5, 1) ']).
/*Less*/ fav(v(e4075551),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(fcc82909),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,tt,training,separate_images,pattern_expansion,count_different_colors,'(3, 1)']).
/*Less*/ fav(v('5af49b42'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v('6ad5bdfd'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(b8825c91),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,tt,training,pattern_rotation,pattern_reflection,pattern_completion,'(4, 1)']).
/*Less*/ fav(v(cfb2ce5a),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(v(d282b262),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t(a8c38be5),[out_grid(9,9),-shape_match,-rotation_match,-mask_match,-color_match,tt,training,pattern_moving,jigsaw,crop,'(2, 1)']).
/*Less*/ fav(v('981571dc'),[grid_size_same,-rotation_match,-mask_match,-color_match,+shape_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('47996f11'),[grid_size_same,-rotation_match,-color_match,+shape_match,+mask_match,evaluation,'(4, 1) ']).
/*Less*/ fav(v('256b0a75'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('1e81d6f9'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v('5783df64'),[-shape_match,-rotation_match,-mask_match,-color_match,evaluation,'(3, 1) ']).
/*Less*/ fav(t('9edfc990'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,tt,training,holes,background_filling,'(3, 1)']).
/*Less*/ fav(v('319f2597'),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,test,evaluation,'(3, 1) ']).
/*Less*/ fav(v(e2092e0c),[grid_size_same,-rotation_match,-mask_match,+shape_match,+color_match,evaluation,'(3, 1) ']).


:- style_check(-singleton).




/*Less*/ fav(v('09c534e7'),[]).
/*Less*/ fav(v('25094a63'),[]).
/*Less*/ fav(v('16b78196'),[]).
/*Less*/ fav(v('070dd51e'),[]).
/*Less*/ fav(v('8a371977'),[]).
/*Less*/ fav(v('696d4842'),[]).
/*Less*/ fav(v('40f6cd08'),[]).
/*Less*/ fav(v('05a7bcf2'),[]).
/*Less*/ fav(t('264363fd'),[]).
/*Less*/ fav(t('7837ac64'),[]).

/*Less*/ fav(X,[]):- clause(/*Less*/ fav(X),true).


/*
first i look to see if the grid sizes are purporional, if not i look to see if the output grid can be recognised on the input
if not i look to see if the input grid can be recognised on the output

f35d900a

the input has a small number of localcolorlesspointlist .. the equal number on each image
two localcolorlesspointlist are the equal color, oh there are two pairs of localcolorlesspointlist
there is a silver dashed box made up from the localcolorlesspointlist
no two silver dots can touch though
the silver dots seem to originate from the four localcolorlesspointlist
teh silver has to come out at least once.. after than if they are going to become two they skip
arround each color point there is a box of the oposting color



 cbded52d

we have a blue background and black TTT board .. to blue openings are made of 2x2s
in each rectangle there can be a color.. each color has a default pos on the 2x2
if like in the game of TTT you can win, but not diagonlly.. place the color on that area


 150deff5.json

  there is a hairything and it is soem color we will paint the entire thing another color
  now we will find the jaggedity bits and paint magenta in dashes of three lonbg until all that is left is 2x2 s



1) identify background and individuation





*/


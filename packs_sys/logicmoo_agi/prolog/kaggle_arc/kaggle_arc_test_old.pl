
/*

Stuff created by otehr arc participants

*/

arc_easy_test(VX):- easytest(X,_,_),atom_id(X,VX).
easy_test(VX>Ex):- easytest(X,Y,_),fix_test_name(X>(tst+Y),VX,Ex).
% 450 teams were able to pass test 4769ccfe
easytest('4769ccfe',0,450).
easytest('14f3ce43',0,395).
easytest('3a9a8576',0,35).
easytest('17bed0a3',0,30).
easytest('70073dd1',0,28).
easytest('dfee8cc8',0,21).
easytest('54faf465',0,18).
easytest('fdd3a1d9',0,17).
easytest('e5d8d02f',0,17).
easytest('eacc75b8',0,15).
easytest('b0f5b612',0,15).
easytest('ea3ac25a',0,15).
easytest('f89a8688',0,14).
easytest('f8790df2',0,11).
easytest('d5c46003',0,9).
easytest('53347d0e',0,8).
easytest('5fd03b03',0,8).
easytest('3537ec7c',0,7).
easytest('41ff3f15',0,7).
easytest('035d120a',1,6).
easytest('035d120a',0,6).
easytest('29dd578c',0,6).
easytest('ff2db69c',0,5).
easytest('79b181ac',0,4).
easytest('29e21400',0,4).
easytest('ec1b4a70',0,4).
easytest('3099097b',0,3).
easytest('af6ff4e7',0,3).
easytest('cbfea064',0,3).
easytest('1ff1dad5',0,3).
easytest('57d3231f',0,3).
easytest('ed052774',0,3).
easytest('a20b8460',0,2).
easytest('6fb885b7',0,2).
easytest('65cb0e06',0,2).
easytest('415be541',0,2).
easytest('e0d597ab',0,2).
easytest('52205343',1,2).
easytest('8f4582fd',0,2).
easytest('e07de455',0,2).
easytest('f2ec0308',0,2).
easytest('5b81d89d',0,2).
easytest('1669db11',0,1).
easytest('d5e09524',0,1).
easytest('89353117',0,1).
easytest('cb188f36',0,1).
easytest('d1d7b83e',0,1).
easytest('ef97f917',0,1).
easytest('7471d77b',0,1).



%test_classes:- csv_read_file('test_classes.csv',Rows), writeq(Rows),tell(tt),forall(member(R,Rows),(format('~q.~n',[R]))),told.

:- use_module(library(csv)).
:- use_module(library(http/json)).

test_classes :-
    clsmake,
    csv_read_file('test_classes.csv', CSV, []),
    CSV = [Colnames|Rows],
    Colnames =.. [row,_|Names],
    maplist(test_classes_row(Names), Rows).

test_classes_row(Names, Row) :-
    Row =.. [row,Name|Fields],
    nth0(Tagging,Names,'Tagging'),
    nth0(Tagging,Fields,Tags),
    findall(Value,(nth0(Nth,Fields,Value),Nth\==Tagging,Value\=='TRUE',Value\=='FALSE',Value\==''),Comments),
    findall(+(Value),(nth0(Nth,Fields,'TRUE'),nth0(Nth,Names,Value)),Plus),
    findall(-(Value),(nth0(Nth,Fields,'FALSE'),nth0(Nth,Names,Value)),Minus),
    nth0(0,Fields,Type),type_to_letter(Type,Letter),
    atom_string(S,Name),
    P =..[Letter,S],
    read_term_from_atom(Tags,Term,[]),
    flatten([Plus,Minus,Term,Comments],Flags),
    exclude(==(end_of_file),Flags,NFlags),
    writeq(more_test_info(P,NFlags)),writeln('.').
type_to_letter(training,t):-!.
type_to_letter(_,v).


more_test_info(t(A),BCD):- more_test_info(t(A),B,C,D),my_append([B,C,D,[training]],BCD).
more_test_info(v(A),BCD):- more_test_info(v(A),B,C,D),my_append([B,C,D,[evaluation]],BCD).
:- dynamic(more_test_info/4).
more_test_info(t('0d3d703e'),[+mask_match,+shape_match,-rotation_match,-color_match,associate_colors_to_colors],['(4, 1)'],[tt]).
more_test_info(t(d511f180),[+mask_match,+shape_match,+'Errors',-rotation_match,-color_match,associate_colors_to_colors],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021#760920']).
more_test_info(t('27a28665'),[-mask_match,-shape_match,-rotation_match,-color_match,associate_colors_to_patterns,take_negative,associate_images_to_patterns],['(7, 3)'],[tt]).
more_test_info(t('0e206a2e'),[+shape_match,+color_match,-mask_match,-rotation_match,associate_patterns_to_patterns,pattern_repetition,pattern_rotation,pattern_reflection,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t('9edfc990'),[+shape_match,+color_match,-mask_match,-rotation_match,background_filling,holes],['(3, 1)'],[tt]).
more_test_info(t(a64e4611),[+shape_match,-mask_match,-rotation_match,-color_match,background_filling,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t(d6ad076f),[+shape_match,-mask_match,-rotation_match,-color_match,bridges,connect_the_dots,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t('1a07d186'),[+shape_match,-mask_match,-rotation_match,-color_match,bring_patterns_close,find_the_intruder],['(3, 1)'],[tt]).
more_test_info(t(d687bc17),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,bring_patterns_close,gravity,direction_guessing,find_the_intruder,remove_intruders],['(3, 1)'], ['https://github.com/fchollet/ARC/pull/39']).
more_test_info(t(ae3edfdc),[+shape_match,+color_match,-mask_match,-rotation_match,bring_patterns_close,gravity],['(3, 1)'],[tt]).
more_test_info(t('85c4e7cd'),[+mask_match,+shape_match,+color_match,-rotation_match,color_guessing,recoloring,color_permutation],['(4, 1)'],[tt]).
more_test_info(t('5ad4f10b'),[-mask_match,-shape_match,-rotation_match,-color_match,color_guessing,remove_noise,recoloring,crop,image_resizing],['(3, 1)'],[tt]).
more_test_info(t('7e0986d6'),[+shape_match,-mask_match,-rotation_match,-color_match,color_guessing,remove_noise],['(2, 1)'],[tt]).
more_test_info(t('7c008303'),[-mask_match,-shape_match,-rotation_match,-color_match,color_palette,detect_grid,recoloring,color_guessing,separate_images,crop],['(3, 1)'],[tt]).
more_test_info(t('6ecd11f4'),[-mask_match,-shape_match,-rotation_match,-color_match,color_palette,recoloring,pattern_resizing,crop],['(3, 1)'],[tt]).
more_test_info(t(ddf7fa4f),[+mask_match,+shape_match,-rotation_match,-color_match,color_palette,recoloring],['(3, 1)'],[tt]).
more_test_info(t('22eb0ac0'),[+shape_match,+color_match,-mask_match,-rotation_match,connect_the_dots,color_matching],['(3, 1)'],[tt]).
more_test_info(t('1f876c06'),[+shape_match,+color_match,-mask_match,-rotation_match,connect_the_dots,diagonals],['(3, 1)'],[tt]).
more_test_info(t('253bf280'),[+shape_match,-mask_match,-rotation_match,-color_match,connect_the_dots,direction_guessing],['(8, 1)'],[tt]).
more_test_info(t(d4a91cb9),[+shape_match,-mask_match,-rotation_match,-color_match,connect_the_dots,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('6cdd2623'),[+shape_match,-mask_match,-rotation_match,-color_match,connect_the_dots,find_the_intruder,remove_noise],['(3, 1)'],[tt]).
more_test_info(t('40853293'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,connect_the_dots],['(2, 1)'],[tt,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(t(a2fd1cf0),[+shape_match,-mask_match,-rotation_match,-color_match,connect_the_dots],['(3, 1)'],[tt]).
more_test_info(t(dbc1a6ce),[+shape_match,-mask_match,-rotation_match,-color_match,connect_the_dots],['(4, 1)'],[tt]).
more_test_info(t(ded97339),[+shape_match,+color_match,-mask_match,-rotation_match,connect_the_dots],['(3, 1)'],[tt]).
more_test_info(t('913fb3ed'),[+shape_match,-mask_match,-rotation_match,-color_match,contouring,associate_colors_to_colors],['(4, 1)'],[tt]).
more_test_info(t(db93a21d),[+shape_match,-mask_match,-rotation_match,-color_match,contouring,draw_line_from_point,measure_area,measure_length,algebra],['(4, 1)'],[tt]).
more_test_info(t('543a7ed5'),[+mask_match,+shape_match,-rotation_match,-color_match,contouring,loop_filling],['(2, 1)'],[tt]).
more_test_info(t(dc1df850),[+shape_match,-mask_match,-rotation_match,-color_match,contouring,pattern_expansion,out_of_boundary],['(3, 1)'],[tt]).
more_test_info(t('6e02f1e3'),[+shape_match,-mask_match,-rotation_match,-color_match,count_different_colors,associate_images_to_numbers],['(5, 1)'],[tt]).
more_test_info(t('1fad071e'),[-mask_match,-shape_match,-rotation_match,-color_match,count_patterns,associate_images_to_numbers],['(3, 1)'],[tt]).
more_test_info(t('39a8645d'),[-mask_match,-shape_match,-rotation_match,-color_match,count_patterns,take_maximum,crop],['(3, 1)'],[tt]).
more_test_info(t(ff28f65a),[-mask_match,-shape_match,-rotation_match,-color_match,count_shapes,associate_images_to_numbers],['(8, 3)'],[tt]).
more_test_info(t('91413438'),[+color_match,-mask_match,-shape_match,-rotation_match,count_tiles,algebra,image_repetition],['(4, 1)'],[tt]).
more_test_info(t('794b24be'),[+shape_match,-mask_match,-rotation_match,-color_match,count_tiles,associate_images_to_numbers],['(10, 2)'],[tt]).
more_test_info(t(d631b094),[-mask_match,-shape_match,-rotation_match,-color_match,count_tiles,dominant_color,summarize],['(4, 1)'],[tt]).
more_test_info(t('5582e5ca'),[+shape_match,-mask_match,-rotation_match,-color_match,count_tiles,dominant_color],['(3, 1)'],[tt]).
more_test_info(t(e48d4e1a),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,count_tiles,pattern_moving,detect_grid,out_of_boundary],['(4, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/13049','https://github.com/fchollet/ARC/pull/37']).
more_test_info(t(a740d043),[-mask_match,-shape_match,-rotation_match,-color_match,crop,detect_background_color,recoloring],['(3, 1)'],[tt]).
more_test_info(t('746b3537'),[+color_match,-mask_match,-shape_match,-rotation_match,crop,direction_guessing],['(5, 1)'],[tt]).
more_test_info(t('1f85a75f'),[-mask_match,-shape_match,-rotation_match,-color_match,crop,find_the_intruder],['(2, 1)'],[tt]).
more_test_info(t('8f2ea7aa'),[+shape_match,+color_match,-mask_match,-rotation_match,crop,fractal_repetition],['(3, 1)'],[tt]).
more_test_info(t(d23f8c26),[+shape_match,-mask_match,-rotation_match,-color_match,crop,image_expansion],['(3, 1)'],[tt]).
more_test_info(t('7468f01a'),[-mask_match,-shape_match,-rotation_match,-color_match,crop,image_reflection],['(3, 1)'],[tt]).
more_test_info(t(f25fbde4),[+color_match,-mask_match,-shape_match,-rotation_match,crop,image_resizing],['(3, 1)'],[tt]).
more_test_info(t('28bf18c6'),[+color_match,-mask_match,-shape_match,-rotation_match,crop,pattern_repetition],['(3, 1)'],[tt]).
more_test_info(t('80af3007'),[+color_match,-mask_match,-shape_match,-rotation_match,crop,pattern_resizing,image_resizing,fractal_repetition],['(3, 1)'],[tt]).
more_test_info(t('3f7978a0'),[+color_match,-mask_match,-shape_match,-rotation_match,crop,rectangle_guessing,find_the_intruder],['(3, 1)'],[tt]).
more_test_info(t('90c28cc7'),[-mask_match,-shape_match,-rotation_match,-color_match,crop,rectangle_guessing,summarize],['(3, 1)'],[tt]).
more_test_info(t(ce602527),[-mask_match,-shape_match,-rotation_match,-color_match,crop,size_guessing,shape_guessing,find_the_intruder,remove_intruder],['(4, 1)'],[tt]).
more_test_info(t(b94a9452),[-mask_match,-shape_match,-rotation_match,-color_match,crop,take_negative],['(3, 1)'],[tt]).
more_test_info(t('1cf80156'),[+color_match,-mask_match,-shape_match,-rotation_match,crop],['(3, 1)'],[tt]).
more_test_info(t(d10ecb37),[+color_match,-mask_match,-shape_match,-rotation_match,crop],['(3, 1)'],[tt]).
more_test_info(t('239be575'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_connectedness,associate_images_to_bools],['(6, 2)'],[tt]).
more_test_info(t('1c786137'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_enclosure,crop],['(3, 1)'],[tt]).
more_test_info(t('7837ac64'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,color_guessing,grid_coloring,crop,extrapolate_image_from_grid],['(4, 1)'],[tt]).
more_test_info(t('06df4c85'),[+shape_match,+color_match,-mask_match,-rotation_match,detect_grid,connect_the_dots,grid_coloring],['(3, 1)'],[tt]).
more_test_info(t('1190e5a7'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,count_hor_lines,count_ver_lines,detect_background_color,color_guessing,create_image_from_info],['(3, 1)'],[tt]).
more_test_info(t('5daaa586'),[+color_match,-mask_match,-shape_match,-rotation_match,detect_grid,crop,draw_line_from_point,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('83302e8f'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,detect_closed_curves,rectangle_guessing,associate_colors_to_bools,loop_filling],['(3, 1)'],[tt]).
more_test_info(t('2dc579da'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,find_the_intruder,crop],['(3, 1)'],[tt]).
more_test_info(t(f8b3ba0a),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,find_the_intruder,dominant_color,count_tiles,summarize,order_numbers],['(4, 1)'],[tt]).
more_test_info(t('941d9a10'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,loop_filling,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t('272f95fa'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,mimic_pattern,grid_coloring],['(2, 1)'],[tt]).
more_test_info(t('39e1d7f9'),[+shape_match,+color_match,-mask_match,-rotation_match,detect_grid,pattern_repetition,grid_coloring],['(3, 1)'],[tt]).
more_test_info(t('6d0160f0'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,separate_image,find_the_intruder,pattern_moving],['(4, 1)'],[tt]).
more_test_info(t('54d9e175'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,separate_images,associate_images_to_images],['(4, 1)'],[tt]).
more_test_info(t('6773b310'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,separate_images,count_tiles,associate_colors_to_numbers],['(4, 1)'],[tt]).
more_test_info(t('29623171'),[+shape_match,+color_match,-mask_match,-rotation_match,detect_grid,separate_images,count_tiles,take_maximum,grid_coloring],['(3, 1)'],[tt]).
more_test_info(t('09629e4f'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_grid,separate_images,count_tiles,take_minimum,enlarge_image,create_grid,adapt_image_to_grid],['(4, 1)'],[tt]).
more_test_info(t('0b148d64'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,separate_images,find_the_intruder,crop],['(3, 1)'],[tt]).
more_test_info(t('88a62173'),[+color_match,-mask_match,-shape_match,-rotation_match,detect_grid,separate_images,find_the_intruder,crop],['(3, 1)'],[tt]).
more_test_info(t(c444b776),[+shape_match,+color_match,-mask_match,-rotation_match,detect_grid,separate_images,find_the_intruder,image_repetition],['(2, 1)'],[tt]).
more_test_info(t('1e32b0e9'),[+shape_match,+color_match,-mask_match,-rotation_match,detect_grid,separate_images,image_repetition,pattern_completion],['(3, 1)'],[tt]).
more_test_info(t(a68b268e),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,separate_images,pattern_juxtaposition],['(6, 1)'],[tt]).
more_test_info(t(cbded52d),[+mask_match,+shape_match,+color_match,+'Errors',-rotation_match,detect_grid,separate_images,pattern_modification,pattern_repetition,pattern_juxtaposition,connect_the_dots],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(t('9f236235'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,summarize,image_reflection],['(3, 1)'],[tt]).
more_test_info(t('780d0b14'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_grid,summarize],['(3, 1)'],[tt]).
more_test_info(t('25d8a9c8'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_hor_lines,recoloring,remove_noise],['(4, 1)'],[tt]).
more_test_info(t('2dee498d'),[+color_match,+'Errors',-mask_match,-shape_match,-rotation_match,detect_repetition,crop,divide_by_n],['(3, 1)'],[tt,'https://github.com/fchollet/ARC/issues/30']).
more_test_info(t('44f52bb0'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_symmetry,associate_images_to_bools],['(6, 2)'],[tt]).
more_test_info(t(bd4472b8),[+shape_match,-mask_match,-rotation_match,-color_match,detect_wall,pattern_expansion,ex_nihilo,color_guessing,color_palette],['(3, 1)'],[tt]).
more_test_info(t('363442ee'),[+shape_match,-mask_match,-rotation_match,-color_match,detect_wall,pattern_repetition,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t(e98196ab),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,image_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t(e3497940),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,image_reflection,image_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t('8e5a5113'),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,detect_wall,separate_images,image_repetition,image_rotation],['(3, 1)'], ['https://github.com/fchollet/ARC/pull/8']).
more_test_info(t('3428a4f5'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,pattern_differences],['(4, 2)'],[tt]).
more_test_info(t('0520fde7'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,pattern_intersection],['(3, 1)'],[tt]).
more_test_info(t('1b2d62fb'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,pattern_intersection],['(5, 1)'],[tt]).
more_test_info(t(cf98881b),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,pattern_juxtaposition],['(5, 1)'],[tt]).
more_test_info(t('6430c8c4'),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,take_complement,pattern_intersection],['(4, 1)'],[tt]).
more_test_info(t(f2829549),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,take_complement,pattern_intersection],['(5, 1)'],[tt]).
more_test_info(t(ce4f8723),[-mask_match,-shape_match,-rotation_match,-color_match,detect_wall,separate_images,take_complement,take_intersection],['(4, 1)'],[tt]).
more_test_info(t('0a938d79'),[+shape_match,+color_match,-mask_match,-rotation_match,direction_guessing,draw_line_from_point,pattern_expansion],['(4, 1)'],[tt]).
more_test_info(t('2281f1f4'),[+shape_match,-mask_match,-rotation_match,-color_match,direction_guessing,draw_line_from_point,pattern_intersection],['(3, 1)'],[tt]).
more_test_info(t('178fcbfb'),[+shape_match,+color_match,-mask_match,-rotation_match,direction_guessing,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t('5168d44c'),[+shape_match,+color_match,-mask_match,-rotation_match,direction_guessing,recoloring,contouring,pattern_moving],['(3, 1)'],[tt]).
more_test_info(t(d4469b4b),[-mask_match,-shape_match,-rotation_match,-color_match,dominant_color,associate_images_to_colors],['(7, 2)'],[tt]).
more_test_info(t('2bee17df'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_border,count_tiles,take_maximum],['(3, 1)'],[tt]).
more_test_info(t(c1d99e64),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_border,detect_grid],['(3, 1)'],[tt]).
more_test_info(t('3bd67248'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_border,diagonals,pattern_repetition],['(3, 1)'],[tt]).
more_test_info(t('834ec97d'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_border,pattern_repetition,spacing,measure_distance_from_side],['(3, 1)'],[tt]).
more_test_info(t(ef135b50),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,draw_line_from_point,bridges,connect_the_dots],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/issues/28']).
more_test_info(t('29c11459'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,count_tiles],['(2, 1)'],[tt]).
more_test_info(t('8d510a79'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,detect_wall,direction_guessing,associate_colors_to_bools],['(2, 1)'],[tt]).
more_test_info(t(d07ae81c),[+mask_match,+shape_match,+color_match,-rotation_match,draw_line_from_point,diagonals,color_guessing],['(3, 1)'],[tt]).
more_test_info(t('5c0a986e'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,diagonals,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('623ea044'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,diagonals],['(3, 1)'],[tt]).
more_test_info(t('25d487eb'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,color_guessing],['(3, 1)'],[tt]).
more_test_info(t('6e19193c'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,diagonals],['(2, 1)'],[tt]).
more_test_info(t('7ddcd7ec'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,diagonals],['(3, 1)'],[tt]).
more_test_info(t('2dd70a9a'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,maze],['(3, 1)'],[tt]).
more_test_info(t('508bd3b6'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,direction_guessing,pattern_reflection],['(3, 1)'],[tt]).
more_test_info(t(d43fd935),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,projection_unto_rectangle],['(3, 1)'],[tt]).
more_test_info(t(bdad9b1f),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,direction_guessing,recoloring,take_intersection],['(2, 1)'],[tt]).
more_test_info(t('855e0971'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,direction_guessing,separate_images,holes],['(4, 1)'],[tt]).
more_test_info(t(f15e1fac),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,gravity,obstacles,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t(d9f24cd1),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,gravity,obstacles],['(2, 1)'],[tt]).
more_test_info(t('97999447'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t('99fa7670'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,pattern_expansion],['(4, 1)'],[tt]).
more_test_info(t('23581191'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,pattern_intersection],['(2, 1)'],[tt]).
more_test_info(t(f1cefba8),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point,pattern_modification],['(3, 1)'],[tt]).
more_test_info(t('8403a5d5'),[+shape_match,-mask_match,-rotation_match,-color_match,draw_line_from_point,pattern_repetition,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('41e4d17e'),[+mask_match,+shape_match,-rotation_match,-color_match,draw_line_from_point,pattern_repetition],['(2, 1)'],[tt]).
more_test_info(t('2c608aff'),[+mask_match,+shape_match,+color_match,-rotation_match,draw_line_from_point,projection_unto_rectangle],['(4, 1)'],[tt]).
more_test_info(t('6d58a25d'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t(a78176bb),[+shape_match,-mask_match,-rotation_match,-color_match,draw_parallel_line,direction_guessing,remove_intruders],['(3, 1)'],[tt]).
more_test_info(t('57aa92db'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_pattern_from_point,pattern_repetition,pattern_resizing],['(4, 1)'],[tt]).
more_test_info(t('3ac3eb23'),[+shape_match,+color_match,-mask_match,-rotation_match,draw_pattern_from_point,pattern_repetition],['(2, 1)'],[tt]).
more_test_info(t('6f8cd79b'),[+shape_match,-mask_match,-rotation_match,-color_match,ex_nihilo,contouring],['(4, 1)'],[tt]).
more_test_info(t('28e73c20'),[+shape_match,-mask_match,-rotation_match,-color_match,ex_nihilo,mimic_pattern],['(5, 1)'],[tt]).
more_test_info(t(af902bf9),[+shape_match,-mask_match,-rotation_match,-color_match,ex_nihilo,x_marks_the_spot],['(3, 1)'],[tt]).
more_test_info(t(b27ca6d3),[+shape_match,-mask_match,-rotation_match,-color_match,find_the_intruder,count_tiles,contouring],['(2, 1)'],[tt]).
more_test_info(t('5.12e+65'),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,crop,recoloring],['(3, 1)'],[tt]).
more_test_info(t(c909285e),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,crop,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t('48d8fb45'),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,crop],['(3, 1)'],[tt]).
more_test_info(t('72ca375d'),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,detect_symmetry,crop],['(3, 1)'],[tt]).
more_test_info(t('32597951'),[+mask_match,+shape_match,-rotation_match,-color_match,find_the_intruder,recoloring],['(3, 1)'],[tt]).
more_test_info(t('31aa019c'),[+shape_match,-mask_match,-rotation_match,-color_match,find_the_intruder,remove_noise,contouring],['(3, 1)'],[tt]).
more_test_info(t('91714a58'),[+shape_match,-mask_match,-rotation_match,-color_match,find_the_intruder,remove_noise],['(3, 1)'],[tt]).
more_test_info(t(d9fac9be),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,summarize,x_marks_the_spot],['(4, 1)'],[tt]).
more_test_info(t(b9b7f026),[-mask_match,-shape_match,-rotation_match,-color_match,find_the_intruder,summarize],['(3, 1)'],[tt]).
more_test_info(t('56dc2b01'),[+shape_match,-mask_match,-rotation_match,-color_match,gravity,direction_guessing,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t('4093f84a'),[+shape_match,-mask_match,-rotation_match,-color_match,gravity,recoloring,projection_unto_rectangle],['(3, 1)'],[tt]).
more_test_info(t('3618c87e'),[+shape_match,+color_match,-mask_match,-rotation_match,gravity],['(3, 1)'],[tt]).
more_test_info(t('3906de3d'),[+shape_match,+color_match,-mask_match,-rotation_match,gravity],['(3, 1)'],[tt]).
more_test_info(t(e8593010),[+shape_match,-mask_match,-rotation_match,-color_match,holes,count_tiles,loop_filling,associate_colors_to_numbers],['(3, 1)'],[tt]).
more_test_info(t('50cb2852'),[+mask_match,+shape_match,-rotation_match,-color_match,holes,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t(d13f3404),[+color_match,-mask_match,-shape_match,-rotation_match,image_expansion,draw_line_from_point,diagonals],['(3, 1)'],[tt]).
more_test_info(t('963e52fc'),[+color_match,-mask_match,-shape_match,-rotation_match,image_expansion,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t('73251a56'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,diagonal_symmetry],['(3, 1)'],[tt]).
more_test_info(t('29ec7d0e'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,pattern_expansion,detect_grid,pattern_repetition],['(4, 1)'],[tt]).
more_test_info(t('5269061'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,pattern_expansion,diagonals],['(3, 1)'],[tt]).
more_test_info(t('9ecd008a'),[-mask_match,-shape_match,-rotation_match,-color_match,image_filling,pattern_expansion,pattern_reflection,pattern_rotation,crop],['(3, 1)'],[tt]).
more_test_info(t('484b58aa'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,pattern_expansion,pattern_repetition],['(3, 1)'],[tt]).
more_test_info(t('3631a71a'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,pattern_expansion,pattern_rotation],['(4, 1)'],[tt]).
more_test_info(t('0dfd9992'),[+shape_match,-mask_match,-rotation_match,-color_match,image_filling,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t('74dd1130'),[+mask_match,+shape_match,+color_match,image_reflection,diagonal_symmetry],['(4, 1)'],[3,tt]).
more_test_info(t('9dfd6313'),[+shape_match,+color_match,-mask_match,image_reflection,diagonal_symmetry],['(3, 1)'],[3,tt]).
more_test_info(t('68b16354'),[+mask_match,+shape_match,+color_match,image_reflection],['(3, 1)'],[2,tt]).
more_test_info(t('67a3c6ac'),[+mask_match,+shape_match,+color_match,-rotation_match,image_reflection],['(3, 1)'],[tt]).
more_test_info(t('007bbfb7'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,fractal_repetition],['(5, 1)'],[tt]).
more_test_info(t(c3e719e8),[-mask_match,-shape_match,-rotation_match,-color_match,image_repetition,image_expansion,count_different_colors,take_maximum],['(3, 1)'],[tt]).
more_test_info(t(cce03e0d),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_expansion,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t('3af2c5a8'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection,image_rotation],['(3, 1)'],[tt]).
more_test_info(t('62c24649'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection,image_rotation],['(3, 1)'],[tt]).
more_test_info(t('67e8384a'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection,image_rotation],['(4, 1)'],[tt]).
more_test_info(t('46442a0e'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(3, 1)'],[tt]).
more_test_info(t('4c4377d9'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(4, 1)'],[tt]).
more_test_info(t('6d0aefbc'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(4, 1)'],[tt]).
more_test_info(t('6fa7a44f'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(4, 1)'],[tt]).
more_test_info(t('8be77c9e'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(3, 1)'],[tt]).
more_test_info(t('8d5021e8'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(3, 1)'],[tt]).
more_test_info(t(c9e6f938),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(3, 1)'],[tt]).
more_test_info(t(eb281b96),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_reflection],['(2, 1)'],[tt]).
more_test_info(t('7fe24cdd'),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition,image_rotation],['(3, 1)'],[tt]).
more_test_info(t(a416b8f3),[+color_match,-mask_match,-shape_match,-rotation_match,image_repetition],['(3, 1)'],[tt]).
more_test_info(t(ac0a08a4),[+color_match,-mask_match,-shape_match,-rotation_match,image_resizing,count_tiles,size_guessing],['(3, 1)'],[tt]).
more_test_info(t('469497ad'),[-mask_match,-shape_match,-rotation_match,-color_match,image_resizing,draw_line_from_point,diagonals],['(3, 1)'],[tt]).
more_test_info(t(b91ae062),[+color_match,-mask_match,-shape_match,-rotation_match,image_resizing,size_guessing,count_different_colors],['(5, 1)'],[tt]).
more_test_info(t('9172f3a0'),[+color_match,-mask_match,-shape_match,-rotation_match,image_resizing],['(2, 1)'],[tt]).
more_test_info(t(c59eb873),[+color_match,-mask_match,-shape_match,-rotation_match,image_resizing],['(3, 1)'],[tt]).
more_test_info(t('4522001f'),[-mask_match,-shape_match,-rotation_match,-color_match,image_rotation,pairwise_analogy],['(2, 1)'],[tt]).
more_test_info(t('3c9b0459'),[+mask_match,+shape_match,+color_match,-rotation_match,image_rotation],['(4, 1)'],[tt]).
more_test_info(t('6150a2bd'),[+shape_match,+color_match,-mask_match,-rotation_match,image_rotation],['(2, 1)'],[tt]).
more_test_info(t(ed36ccf7),[+shape_match,+color_match,-mask_match,-rotation_match,image_rotation],['(4, 1)'],[tt]).
more_test_info(t('1f642eb9'),[+mask_match,+shape_match,+color_match,-rotation_match,image_within_image,projection_unto_rectangle],['(3, 1)'],[tt]).
more_test_info(t('7b6016b9'),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling,background_filling,color_guessing],['(3, 1)'],[tt]).
more_test_info(t('868de0fa'),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,loop_filling,color_guessing,measure_area,even_or_odd,associate_colors_to_bools],['(5, 1)'], ['https://github.com/fchollet/ARC/pull/45']).
more_test_info(t('4347f46a'),[+shape_match,+color_match,-mask_match,-rotation_match,loop_filling,color_guessing],['(3, 1)'],[tt]).
more_test_info(t(c0f76784),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling,measure_area,associate_colors_to_numbers],['(3, 1)'],[tt]).
more_test_info(t('44d8ac46'),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling,rectangle_guessing],['(4, 1)'],[tt]).
more_test_info(t(e73095fd),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t(d5d6de2d),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling,replace_pattern,remove_intruders],['(3, 2)'],[tt]).
more_test_info(t(bb43febb),[+mask_match,+shape_match,-rotation_match,-color_match,loop_filling,rettangle_guessing],['(2, 1)'],[tt]).
more_test_info(t('00d62c1b'),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling],['(5, 1)'],[tt]).
more_test_info(t(a5313dff),[+shape_match,-mask_match,-rotation_match,-color_match,loop_filling],['(3, 1)'],[tt]).
more_test_info(t('6455b5f5'),[+shape_match,-mask_match,-rotation_match,-color_match,measure_area,take_maximum,take_minimum,loop_filling,associate_colors_to_ranks],['(4, 1)'],[tt]).
more_test_info(t('445eab21'),[-mask_match,-shape_match,-rotation_match,-color_match,measure_area,take_maximum],['(3, 1)'],[tt]).
more_test_info(t('23b5c85d'),[-mask_match,-shape_match,-rotation_match,-color_match,measure_area,take_minimum,crop],['(5, 1)'],[tt]).
more_test_info(t(b0c4d837),[-mask_match,-shape_match,-rotation_match,-color_match,measure_length,associate_images_to_numbers],['(6, 1)'],[tt]).
more_test_info(t('08ed6ac7'),[+mask_match,+shape_match,-rotation_match,-color_match,measure_length,order_numbers,associate_colors_to_ranks,recoloring],['(2, 1)'],[tt]).
more_test_info(t('150deff5'),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_coloring,pattern_deconstruction,associate_colors_to_patterns],['(3, 1)'],[tt]).
more_test_info(t(dc0a314f),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_completion,crop],['(3, 1)'],[tt]).
more_test_info(t('3345333e'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_completion,pattern_reflection,remove_noise],['(2, 1)'],[tt]).
more_test_info(t(b8825c91),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_completion,pattern_rotation,pattern_reflection],['(4, 1)'],[tt]).
more_test_info(t('3aa6fb7a'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_completion,pattern_rotation],['(2, 1)'],[tt]).
more_test_info(t('50846271'),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_completion,recoloring],['(4, 1)'],[tt]).
more_test_info(t('4612dd53'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_completion,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t('56ff96f3'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_completion,rectangle_guessing],['(4, 1)'],[tt]).
more_test_info(t('60b61512'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_completion],['(2, 1)'],[tt]).
more_test_info(t('2013d3e2'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_deconstruction,crop],['(2, 1)'],[tt]).
more_test_info(t('1b60fb0c'),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,pattern_deconstruction,pattern_rotation,pattern_expansion],['(3, 1)'],[tt,'https://github.com/fchollet/ARC/pull/33']).
more_test_info(t('0ca9ddb6'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,associate_patterns_to_colors],['(3, 1)'],[tt]).
more_test_info(t(f8c80d96),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,background_filling],['(3, 1)'],[tt]).
more_test_info(t(e179c5f4),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,bouncing],['(3, 1)'],[tt]).
more_test_info(t('93b581b8'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,color_guessing,out_of_boundary],['(3, 1)'],[tt]).
more_test_info(t(a699fb00),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,connect_the_dots],['(3, 1)'],[tt]).
more_test_info(t(a65b410d),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,count_tiles,associate_colors_to_ranks],['(3, 1)'],[tt]).
more_test_info(t('1f0c79e5'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,diagonals,direction_guessing],['(4, 1)'],[tt]).
more_test_info(t(e9614598),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,direction_guessing,measure_length],['(2, 2)'],[tt]).
more_test_info(t('045e512c'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t(e8dc4411),[+mask_match,+shape_match,+color_match,-rotation_match,pattern_expansion,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t(e21d9049),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point,color_palette],['(2, 1)'],[tt]).
more_test_info(t(b527c5c6),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point,contouring,direction_guessing,size_guessing],['(4, 1)'],[tt]).
more_test_info(t(a3df8b1e),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point,diagonals,bounce],['(3, 1)'],[tt]).
more_test_info(t(b8cdaf2b),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point,diagonals,pairwise_analogy],['(4, 1)'],[tt]).
more_test_info(t(aba27056),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,draw_line_from_point,diagonals],['(3, 1)'],[tt]).
more_test_info(t(ec883f72),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point,diagonals],['(4, 1)'],[tt]).
more_test_info(t(f5b8619d),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,draw_line_from_point,image_repetition],['(3, 1)'],[tt]).
more_test_info(t(d037b0a7),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t('54d82841'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,gravity],['(3, 1)'],[tt]).
more_test_info(t(feca6190),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_expansion,image_expansion,draw_line_from_point,diagonals],['(5, 1)'],[tt]).
more_test_info(t('49d1d64f'),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,image_expansion],['(3, 1)'],[tt]).
more_test_info(t('539a4f51'),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,image_expansion],['(3, 1)'],[tt]).
more_test_info(t('53b68214'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_expansion,image_expansion],['(3, 2)'],[tt]).
more_test_info(t(bbc9ae5d),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_expansion,image_expansion],['(5, 1)'],[tt]).
more_test_info(t(c3f564a4),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,image_filling],['(3, 1)'],[tt]).
more_test_info(t(caa06a1f),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_expansion,image_filling],['(3, 1)'],[tt]).
more_test_info(t('10fcaaa3'),[+'Errors',-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,image_repetition],['(4, 1)'],[tt,'https://github.com/fchollet/ARC/pull/31']).
more_test_info(t(b782dc8a),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,maze],['(2, 1)'],[tt]).
more_test_info(t(db3e9e38),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,out_of_boundary],['(2, 1)'],[tt]).
more_test_info(t('7447852a'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t(d06dbe63),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,pairwise_analogy],['(2, 1)'],[tt]).
more_test_info(t(d22278a0),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_expansion,pairwise_analogy],['(4, 1)'], ['https://github.com/fchollet/ARC/pull/4']).
more_test_info(t(f9012d9b),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,pattern_completion,crop],['(3, 1)'],[tt]).
more_test_info(t(ff805c23),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_expansion,pattern_completion,crop],['(3, 1)'],[tt]).
more_test_info(t(b548a754),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,pattern_modification,x_marks_the_spot],['(3, 1)'],[tt]).
more_test_info(t('9d9215db'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_reflection,pattern_rotation],['(3, 1)'],[tt]).
more_test_info(t(e40b9e2f),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_reflection,pattern_rotation],['(3, 1)'],[tt]).
more_test_info(t(d8c310e9),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_repetition,pattern_completion],['(3, 1)'],[tt]).
more_test_info(t(b775ac94),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_repetition,recoloring,pattern_rotation,pattern_reflection,direction_guessing,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t('4938f0c2'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_rotation,pattern_reflection],['(3, 1)'],[tt]).
more_test_info(t('4c5c2cf0'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion,pattern_rotation,pattern_reflection],['(3, 1)'],[tt]).
more_test_info(t(fcc82909),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,separate_images,count_different_colors],['(3, 1)'],[tt]).
more_test_info(t('22233c11'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion,size_guessing],['(3, 1)'],[tt]).
more_test_info(t('0962bcdd'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion],['(2, 1)'],[tt]).
more_test_info(t('11852cab'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_expansion],['(3, 1)'],[tt,'https://github.com/fchollet/ARC/pull/33']).
more_test_info(t('1bfc4729'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion],['(2, 1)'],[tt]).
more_test_info(t('22168020'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t('95990924'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t(b7249182),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t(d364b489),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion],['(2, 1)'],[tt]).
more_test_info(t(f35d900a),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_expansion],['(4, 1)'],[tt]).
more_test_info(t('67a423a3'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_intersection,contouring],['(3, 1)'],[tt]).
more_test_info(t(dae9d2b5),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_juxtaposition,separate_images,recoloring],['(5, 2)'],[tt]).
more_test_info(t(d89b689b),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_juxtaposition,summarize,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('137eaa0f'),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t(ea786f4a),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_modification,draw_line_from_point,diagonals],['(3, 1)'],[tt]).
more_test_info(t(ecdecbb3),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_modification,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t(ba26e723),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_modification,pairwise_analogy,recoloring],['(5, 1)'],[tt]).
more_test_info(t(ba97ae07),[+mask_match,+shape_match,+color_match,-rotation_match,pattern_modification,pairwise_analogy,rettangle_guessing,recoloring],['(4, 1)'],[tt]).
more_test_info(t('025d127b'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_modification],['(2, 1)'],[tt,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(t(e9afcf9a),[+mask_match,+shape_match,+color_match,-rotation_match,pattern_modification],['(2, 1)'],[tt]).
more_test_info(t(a61ba2ce),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,bring_patterns_close,crop,jigsaw],['(2, 1)'],[tt]).
more_test_info(t(a48eeaf7),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,bring_patterns_close,gravity,direction_guessing],['(2, 1)'],[tt]).
more_test_info(t('98cf29f8'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,bring_patterns_close],['(3, 1)'],[tt]).
more_test_info(t('4290ef0e'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,concentric,crop],['(3, 1)'],[tt]).
more_test_info(t(beb8660c),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,count_tiles,order_numbers],['(3, 1)'],[tt]).
more_test_info(t('05f2a901'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,direction_guessing,bring_patterns_close],['(3, 1)'],[tt]).
more_test_info(t(dc433765),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_moving,direction_guessing,only_one],['(7, 2)'], ['https://github.com/fchollet/ARC/issues/29']).
more_test_info(t('6855a6e4'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,direction_guessing,x_marks_the_spot],['(3, 1)'],[tt]).
more_test_info(t('1e0a9b12'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,gravity],['(3, 1)'],[tt]).
more_test_info(t('681b3aeb'),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_moving,jigsaw,crop,bring_patterns_close],['(3, 1)'],[tt]).
more_test_info(t(a8c38be5),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_moving,jigsaw,crop],['(2, 1)'],[tt]).
more_test_info(t(c8cbb738),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,jigsaw,crop],['(3, 1)'],[tt]).
more_test_info(t('6a1e5592'),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,pattern_moving,jigsaw,recoloring],['(2, 1)'],[tt,'https://github.com/fchollet/ARC/pull/16']).
more_test_info(t('228f6490'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,loop_filling,shape_guessing,x_marks_the_spot],['(3, 1)'],[tt]).
more_test_info(t('5521c0d9'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,measure_length],['(3, 1)'],[tt]).
more_test_info(t('1caeab9d'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,pattern_alignment],['(3, 1)'],[tt]).
more_test_info(t(bc1d5164),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,pattern_juxtaposition,crop,pairwise_analogy],['(5, 1)'],[tt]).
more_test_info(t('97a05b5b'),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_moving,pattern_juxtaposition,crop,shape_guessing],['(3, 1)'],[tt]).
more_test_info(t(e6721834),[-mask_match,-shape_match,-rotation_match,-color_match,pattern_moving,pattern_juxtaposition,crop],['(3, 1)'],[tt]).
more_test_info(t('846bdb03'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,pattern_reflection,crop,color_matching,x_marks_the_spot],['(4, 1)'],[tt]).
more_test_info(t(f8a8fe49),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,pattern_reflection],['(3, 1)'],[tt]).
more_test_info(t('6b9890af'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,pattern_resizing,crop,x_marks_the_spot],['(3, 1)'],[tt]).
more_test_info(t(a79310a0),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_moving,recoloring,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t(a1570a43),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving,rectangle_guessing,x_marks_the_spot],['(4, 1)'],[tt]).
more_test_info(t('9aec4887'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_moving,x_marks_the_spot,crop,recoloring,color_guessing],['(3, 1)'],[tt]).
more_test_info(t('25ff71a9'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_moving],['(4, 2)'],[tt]).
more_test_info(t('2bcee788'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_reflection,direction_guessing,image_filling,background_filling],['(4, 1)'],[tt]).
more_test_info(t('760b3cac'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_reflection,direction_guessing],['(3, 1)'],[tt]).
more_test_info(t('496994bd'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_reflection],['(2, 1)'],[tt]).
more_test_info(t('82819916'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_repetition,color_guessing,draw_line_from_point,associate_colors_to_colors],['(4, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/32']).
more_test_info(t('4258a5f9'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_repetition,contouring],['(2, 1)'],[tt]).
more_test_info(t('447fd412'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,draw_pattern_from_point,pattern_resizing],['(3, 1)'],[tt]).
more_test_info(t('8eb1be9a'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,image_filling],['(2, 1)'],[tt]).
more_test_info(t('444801d8'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_expansion,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t('3e980e27'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_juxtaposition,direction_guessing,pattern_reflection],['(4, 1)'],[tt]).
more_test_info(t('264363fd'),[+mask_match,+shape_match,+color_match,-rotation_match,pattern_repetition,pattern_juxtaposition,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t('36d67576'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_juxtaposition,pattern_reflection,pattern_rotation],['(3, 1)'],[tt]).
more_test_info(t('6aa20dc0'),[+mask_match,+shape_match,+color_match,-rotation_match,pattern_repetition,pattern_juxtaposition,pattern_resizing],['(3, 1)'],[tt]).
more_test_info(t('321b1fc6'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_repetition,pattern_juxtaposition],['(2, 1)'],[tt]).
more_test_info(t('72322fa7'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t('88a10436'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_repetition,pattern_juxtaposition],['(3, 1)'],[tt]).
more_test_info(t(e5062a87),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,pattern_repetition,pattern_juxtaposition],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(t(e76a88a6),[+mask_match,+shape_match,-rotation_match,-color_match,pattern_repetition,pattern_juxtaposition],['(2, 1)'],[tt]).
more_test_info(t(f25ffba3),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_reflection],['(2, 1)'],[tt]).
more_test_info(t('8a004b2b'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_repetition,pattern_resizing,pattern_juxtaposition,rectangle_guessing,crop],['(3, 1)'],[tt]).
more_test_info(t('7df24a62'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,pattern_rotation,pattern_juxtaposition,out_of_boundary],['(4, 1)'],[tt]).
more_test_info(t('90f3ed37'),[+shape_match,-mask_match,-rotation_match,-color_match,pattern_repetition,recoloring],['(3, 1)'],[tt]).
more_test_info(t('890000000000000.0'),[+shape_match,+color_match,-mask_match,-rotation_match,pattern_repetition,rectangle_guessing,contouring],['(3, 1)'],[tt]).
more_test_info(t('46f33fce'),[+color_match,-mask_match,-shape_match,-rotation_match,pattern_resizing,image_resizing],['(3, 1)'],[tt]).
more_test_info(t('2204b7a8'),[+mask_match,+shape_match,-rotation_match,-color_match,proximity_guessing,recoloring],['(3, 1)'],[tt]).
more_test_info(t(b1948b0a),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,associate_colors_to_colors],['(3, 1)'],[tt]).
more_test_info(t(c8f0f002),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,associate_colors_to_colors],['(3, 1)'],[tt]).
more_test_info(t('776ffc46'),[+mask_match,+shape_match,+color_match,-rotation_match,recoloring,associate_colors_to_patterns,detect_enclosure,find_the_intruder],['(4, 1)'],[tt]).
more_test_info(t(e509e548),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,associate_colors_to_shapes,homeomorphism],['(3, 1)'],[tt]).
more_test_info(t('234bbc79'),[-mask_match,-shape_match,-rotation_match,-color_match,recoloring,bring_patterns_close,crop],['(4, 1)'],[tt]).
more_test_info(t('77fdfe62'),[-mask_match,-shape_match,-rotation_match,-color_match,recoloring,color_guessing,detect_grid,crop],['(3, 1)'],[tt]).
more_test_info(t('8e1813be'),[-mask_match,-shape_match,-rotation_match,-color_match,recoloring,color_guessing,direction_guesingcrop,image_within_image],['(3, 1)'],[tt]).
more_test_info(t(aabf363d),[+shape_match,-mask_match,-rotation_match,-color_match,recoloring,color_guessing,remove_intruders],['(2, 1)'],[tt]).
more_test_info(t('63613498'),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,compare_image,detect_wall],['(3, 1)'],[tt]).
more_test_info(t('6e82a1ae'),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,count_tiles,associate_colors_to_numbers],['(3, 1)'],[tt]).
more_test_info(t(ce9e57f2),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,count_tiles,take_half],['(3, 1)'],[tt]).
more_test_info(t(b2862040),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,detect_closed_curves,associate_colors_to_bools],['(4, 1)'],[tt]).
more_test_info(t('810b9b61'),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,detect_closed_curves],['(3, 1)'],[tt]).
more_test_info(t('673ef223'),[+shape_match,-mask_match,-rotation_match,-color_match,recoloring,draw_line_from_point,portals],['(3, 1)'],[tt]).
more_test_info(t('67385a82'),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,measure_area,associate_colors_to_bools],['(4, 1)'],[tt]).
more_test_info(t(d406998b),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,one_yes_one_no,cylindrical],['(4, 1)'],[tt]).
more_test_info(t(bda2d7a6),[+shape_match,+color_match,-mask_match,-rotation_match,recoloring,pairwise_analogy,pattern_modification,color_permutation],['(3, 2)'],[tt]).
more_test_info(t('017c7c7b'),[-mask_match,-shape_match,-rotation_match,-color_match,recoloring,pattern_expansion,pattern_repetition,image_expansion],['(3, 1)'],[tt]).
more_test_info(t(a5f85a15),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,pattern_modification,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t(c9f8e694),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,pattern_repetition,color_palette],['(2, 1)'],[tt]).
more_test_info(t('3bdb4ada'),[+shape_match,+color_match,-mask_match,-rotation_match,recoloring,pattern_repetition,holes],['(2, 1)'],[tt]).
more_test_info(t('36fdfd69'),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t(a8d7556c),[+shape_match,-mask_match,-rotation_match,-color_match,recoloring,rectangle_guessing],['(3, 1)'],[tt]).
more_test_info(t(b6afb2da),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,replace_pattern,rectangle_guessing],['(2, 1)'],[tt]).
more_test_info(t(aedd82e4),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,separate_shapes,count_tiles,take_minimum,associate_colors_to_bools],['(4, 1)'],[tt]).
more_test_info(t(b230c067),[+mask_match,+shape_match,-rotation_match,-color_match,recoloring,separate_shapes,find_the_intruder,associate_colors_to_bools],['(2, 1)'],[tt]).
more_test_info(t('928ad970'),[+shape_match,+color_match,-mask_match,-rotation_match,rectangle_guessing,color_guessing,draw_rectangle],['(3, 1)'],[tt]).
more_test_info(t('8731374e'),[-mask_match,-shape_match,-rotation_match,-color_match,rectangle_guessing,crop,draw_line_from_point],['(3, 1)'],[tt]).
more_test_info(t('5bd6f4ac'),[-mask_match,-shape_match,-rotation_match,-color_match,rectangle_guessing,crop],['(4, 1)'],[tt]).
more_test_info(t('952a094c'),[+shape_match,+color_match,-mask_match,-rotation_match,rectangle_guessing,inside_out],['(3, 1)'],[tt]).
more_test_info(t('694f12f3'),[+mask_match,+shape_match,-rotation_match,-color_match,rectangle_guessing,loop_filling,measure_area,associate_colors_to_ranks],['(2, 1)'],[tt]).
more_test_info(t('6d75e8bb'),[+shape_match,-mask_match,-rotation_match,-color_match,rectangle_guessing,pattern_completion],['(3, 1)'],[tt]).
more_test_info(t('5c2c9af4'),[+shape_match,+color_match,-mask_match,-rotation_match,rectangle_guessing,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t(d4f3cd78),[+shape_match,-mask_match,-rotation_match,-color_match,rectangle_guessing,recoloring,draw_line_from_point],['(2, 1)'],[tt]).
more_test_info(t('3eda0437'),[+shape_match,-mask_match,-rotation_match,-color_match,rectangle_guessing,recoloring,measure_area,take_maximum],['(4, 1)'],[tt]).
more_test_info(t('6cf79266'),[+shape_match,+'Errors',-mask_match,-rotation_match,-color_match,rectangle_guessing,recoloring],['(3, 1)'],[tt,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(t('7f4411dc'),[+shape_match,+color_match,-mask_match,-rotation_match,rectangle_guessing,remove_noise],['(3, 1)'],[tt]).
more_test_info(t(fcb5c309),[-mask_match,-shape_match,-rotation_match,-color_match,rectangle_guessing,separate_images,count_tiles,take_maximum,crop,recoloring],['(3, 1)'],[tt]).
more_test_info(t('42a50994'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match,remove_noise,count_tiles],['(4, 1)'],[tt,'https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021','https://github.com/fchollet/ARC/pull/43']).
more_test_info(t('5614dbcf'),[-mask_match,-shape_match,-rotation_match,-color_match,remove_noise,image_resizing],['(2, 1)'],[tt]).
more_test_info(t(e26a3af2),[+mask_match,+shape_match,-rotation_match,-color_match,remove_noise,separate_images],['(3, 1)'],[tt]).
more_test_info(t(a9f96cdd),[+shape_match,-mask_match,-rotation_match,-color_match,replace_pattern,out_of_boundary],['(4, 1)'],[tt]).
more_test_info(t('6c434453'),[+shape_match,-mask_match,-rotation_match,-color_match,replace_pattern],['(2, 1)'],[tt]).
more_test_info(t(b60334d2),[+shape_match,-mask_match,-rotation_match,-color_match,replace_pattern],['(2, 1)'],[tt]).
more_test_info(t(ce22a75a),[+shape_match,-mask_match,-rotation_match,-color_match,replace_pattern],['(2, 1)'],[tt]).
more_test_info(t(d90796e8),[+shape_match,-mask_match,-rotation_match,-color_match,replace_pattern],['(3, 1)'],[tt]).
more_test_info(t(a85d4709),[+shape_match,-mask_match,-rotation_match,-color_match,separate_images,associate_colors_to_images,summarize],['(4, 1)'],[tt]).
more_test_info(t(ae4f1146),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,count_tiles,crop],['(4, 1)'],[tt]).
more_test_info(t('9af7a82c'),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,count_tiles,summarize,order_numbers],['(4, 1)'],[tt]).
more_test_info(t(de1cd16c),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,count_tiles,take_maximum,summarize],['(4, 1)'],[tt]).
more_test_info(t(e50d258f),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,detect_background_color,crop,count_tiles,take_maximum],['(3, 1)'],[tt]).
more_test_info(t('7b7f7511'),[+color_match,-mask_match,-shape_match,-rotation_match,separate_images,detect_repetition,crop],['(3, 1)'],[tt]).
more_test_info(t('662c240a'),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,detect_symmetry,find_the_intruder,crop],['(4, 1)'],[tt]).
more_test_info(t(a87f7484),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,find_the_intruder,crop],['(4, 1)'],[tt]).
more_test_info(t(b190f7f5),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,image_expasion,color_palette,image_resizing,replace_pattern],['(3, 1)'],[tt]).
more_test_info(t('75b8110e'),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,image_juxtaposition],['(5, 1)'],[tt]).
more_test_info(t('8efcae92'),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,rectangle_guessing,count_tiles,take_maximum,crop],['(3, 1)'],[tt]).
more_test_info(t('94f9d214'),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,take_complement,pattern_intersection],['(4, 1)'],[tt]).
more_test_info(t(fafffa47),[-mask_match,-shape_match,-rotation_match,-color_match,separate_images,take_complement,pattern_intersection],['(5, 1)'],[tt]).
more_test_info(t(d0f5fe59),[+color_match,-mask_match,-shape_match,-rotation_match,separate_shapes,count_shapes,associate_images_to_numbers,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t(d2abd087),[+mask_match,+shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,associate_colors_to_numbers,recoloring],['(3, 1)'],[tt]).
more_test_info(t(ea32f347),[+mask_match,+shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,recoloring,associate_colors_to_ranks],['(4, 1)'],[tt]).
more_test_info(t('9565186b'),[+mask_match,+shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,recoloring,take_maximum,associate_color_to_bools],['(4, 1)'],[tt]).
more_test_info(t(f8ff0b80),[-mask_match,-shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,summarize,order_numbers],['(3, 1)'],[tt]).
more_test_info(t(be94b721),[-mask_match,-shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,take_maximum,crop],['(4, 1)'],[tt]).
more_test_info(t(a3325580),[-mask_match,-shape_match,-rotation_match,-color_match,separate_shapes,count_tiles,take_maximum,summarize,remove_intruders],['(6, 1)'],[tt]).
more_test_info(t(a61f2674),[+shape_match,-mask_match,-rotation_match,-color_match,separate_shapes,count_tiles,take_maximum,take_minimum,recoloring,associate_colors_to_ranks,remove_intruders],['(2, 1)'],[tt]).
more_test_info(t(cdecee7f),[-mask_match,-shape_match,-rotation_match,-color_match,summarize,pairwise_analogy],['(3, 1)'],[tt]).
more_test_info(t('4be741c5'),[+color_match,-mask_match,-shape_match,-rotation_match,summarize],['(3, 1)'],[tt]).
more_test_info(t(eb5a1d5d),[+color_match,-mask_match,-shape_match,-rotation_match,summarize],['(3, 1)'],[tt]).
more_test_info(t('995c5fa3'),[-mask_match,-shape_match,-rotation_match,-color_match,take_complement,detect_wall,separate_images,associate_colors_to_images,summarize],['(4, 1)'],[tt]).
more_test_info(t('99b1bc43'),[-mask_match,-shape_match,-rotation_match,-color_match,take_complement,detect_wall,separate_images,pattern_intersection],['(4, 1)'],[tt]).
more_test_info(t('3de23699'),[-mask_match,-shape_match,-rotation_match,-color_match,take_negative,crop,rectangle_guessing],['(4, 1)'],[tt]).
more_test_info(t('3befdf3e'),[+shape_match,+color_match,-mask_match,-rotation_match,take_negative,pattern_expansion],['(3, 1)'],[tt]).
more_test_info(t(f76d97a5),[+shape_match,-mask_match,-rotation_match,-color_match,take_negative,recoloring,associate_colors_to_colors],['(3, 1)'],[tt]).
more_test_info(v('576224'),[+color_match,-mask_match,-shape_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('009d5c81'),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('00dbd492'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('3560426'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('05a7bcf2'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('0607ce86'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0692e18c'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('070dd51e'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match],['(2, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v('08573cc6'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0934a4d8'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('09c534e7'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0a1d4ef5'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('0a2355a6'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('0b17323b'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('0bb8deee'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('0becf7df'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0c786b71'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0c9aba6e'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('0d87d2a6'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('0e671a1a'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('0f63c0b9'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('103eff5b'),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('11e1fe23'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('12422b43'),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('12997ef3'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 2) '],[]).
more_test_info(v('12eac192'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('136b0064'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('13713586'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('137f0df0'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('140c817e'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('14754a24'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('15113be4'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('15663ba9'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('15696249'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('16b78196'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('17b80ad2'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('17cae0c1'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('18419cfa'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('184a9768'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('195ba7dc'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('1990f7a8'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('19bb5feb'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('1a2e2828'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('1a6449f1'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('1acc24af'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('1c02dbbe'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('1c0d0a4b'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('1c56ad9f'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('1d0a4b61'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('1d398264'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 2) '],[]).
more_test_info(v('1da012fc'),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('1e81d6f9'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('1e97544e'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('2037f2c7'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('2072aba6'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('2.08e+20'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('20981f0e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('212895b5'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('21f83797'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('22a4bbc2'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('25094a63'),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('2546ccf6'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('256b0a75'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('2685904e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(6, 1) '],[]).
more_test_info(v('2697da3f'),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('2753e76c'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('27a77e38'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('27f8ce4f'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('281123b4'),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v('292dd178'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('29700607'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('2a5f8217'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('2b01abd0'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('2c0b0aff'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('2c737e39'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('2f0c5170'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('310f3251'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('3194b014'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('319f2597'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('31adaf00'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('31d5ba1a'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 2) '],[]).
more_test_info(v('32e9702f'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('332efdb3'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('3391f8c0'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('33b52de3'),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('3490cc26'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('34b99a2b'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('351d6448'),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('358ba94e'),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('37d3e8b2'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('3979b1a8'),[+color_match,-mask_match,-shape_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('3a301edc'),[+shape_match,+color_match,-mask_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('3b4c2228'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 2) '],[]).
more_test_info(v('3d31c5b3'),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v('3ed85e70'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('3ee1011a'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('3f23242b'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('40f6cd08'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('414297c0'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('423a55dc'),[+shape_match,+color_match,-mask_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('42918530'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('42a15761'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('4364c1c4'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('456873bc'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('45737921'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('45bbe264'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('477d2879'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('47996f11'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('48131b3c'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('4852f2fa'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 2) '],[]).
more_test_info(v('48f8583b'),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v('4aab4007'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('4acc7107'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('4b6b68e5'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('4c177718'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 2) '],[]).
more_test_info(v('4cd1b7b2'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('4e45f183'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('4e469f39'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('4f537728'),[+mask_match,+shape_match,+color_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('4ff4c9da'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('505fff84'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('506d28a5'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('50a16a69'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('50aad11f'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('50f325b5'),[+mask_match,+shape_match,+color_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('516b51b7'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('5207a7b5'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('5289ad53'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('52fd389e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('54db823b'),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match],['(4, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v('55059096'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('551d5bf1'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('55783887'),[+mask_match,+shape_match,+color_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('575b1a71'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('5783df64'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('5833af48'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('58743b76'),[+mask_match,+shape_match,+color_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('58e15b12'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('59341089'),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('5a5a2103'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('5af49b42'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('5b526a93'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('5b692c0f'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('5b6cbef5'),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('5d2a5c43'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 2) '],[]).
more_test_info(v('5ffb2104'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('604001fa'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('60a26a3e'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('60c09cac'),[+color_match,-mask_match,-shape_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('626c0bcc'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('62ab2642'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('62b74c02'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('639f5a19'),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('6420000000.0'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('642d658d'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('64a7c07e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('66e6c45b'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('66f2d22f'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('67636eac'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('67b4a34d'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('67c52801'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('68b67ca3'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('692cd3b6'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('695367ec'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('696d4842'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('69889d6e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('6a11f6da'),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('6ad5bdfd'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('6df30ad6'),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('6ea4a07e'),[+shape_match,-mask_match,-rotation_match,-color_match],['(6, 2) '],[]).
more_test_info(v('6f473927'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('7039b2d7'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('705a3229'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('712bf12e'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('72207abc'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('72a961c9'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('73182012'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('73c3b0d8'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('73ccf9c2'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('759f3fd3'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('762cd429'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('770cc55f'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('782b5218'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('79369cc6'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('7953d61e'),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v('79fb03f4'),[+shape_match,+color_match,-mask_match,-rotation_match],['(6, 1) '],[]).
more_test_info(v('7bb29440'),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('7c8af763'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7c9b52a0'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7d18a6fb'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7d1f7ee8'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7d419a02'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7e02026e'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('7ee1c6ea'),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('817e6c09'),[+mask_match,+shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('81c0276b'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('833dafe3'),[+color_match,-mask_match,-shape_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('845d6e51'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('84db8fc4'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('84f2aca1'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('8597cfd7'),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('85b81ff1'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('85fa5666'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('8719f442'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('88207623'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('891232d6'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('896d5239'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('8a371977'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('8b28cd80'),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 2) '],[]).
more_test_info(v('8ba14f53'),[+color_match,-mask_match,-shape_match,-rotation_match],['(6, 1) '],[]).
more_test_info(v('8cb8642d'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('8dae5dfc'),[+mask_match,+shape_match,+color_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('8e2edd66'),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('8ee62060'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('8fbca751'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('90347967'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('903d1b4a'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('9110e3c5'),[-mask_match,-shape_match,-rotation_match,-color_match],['(7, 2) '],[]).
more_test_info(v('917bccba'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('929ab4e9'),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('92e50de0'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9356391f'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('93b4f4b3'),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('93c31fbe'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('94133066'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('94414823'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('94be5b80'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('95a58926'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('963f59bc'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('96a8c0cd'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('97239e3d'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9772c176'),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v('981571dc'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('992798f6'),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('99306f82'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9a4bb226'),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('9b2a60aa'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9b365c51'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('9b4c17c4'),[+mask_match,+shape_match,+color_match,-rotation_match],['(4, 2) '],[]).
more_test_info(v('9bebae7a'),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v('9c1e755f'),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v('9c56f360'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9caba7c3'),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v('9ddd00f0'),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v('9def23fe'),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v('9f27f097'),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(a04b2602),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(a096bf4d),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(a3f84088),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(a406ac07),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(a57f2f04),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(a59b95c0),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v(a680ac02),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(a8610ef7),[+mask_match,+shape_match,+'Errors',-rotation_match,-color_match],['(4, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v(a934301b),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(aa18de87),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(aa300dc3),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(aa4ec2a5),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(aab50785),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(ac0c5833),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(ac2e8ecf),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(ac3e2b04),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(ac605cbb),[+shape_match,-mask_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v(ad7e01d0),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(ae58858e),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(aee291af),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(af22c60d),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(af24b4cc),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(b0722778),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(b0f4d537),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(b15fca0b),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(b1fc8b8e),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 2) '],[]).
more_test_info(v(b20f7c8b),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(b457fec5),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(b4a43f3b),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(b7999b51),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(b7cb93ac),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(b7f8a4d8),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(b7fb29bc),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(b942fd60),[+shape_match,+color_match,-mask_match,-rotation_match],['(6, 1) '],[]).
more_test_info(v(b9630600),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(ba9d41b8),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(baf41dbf),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(bb52a14b),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(bbb1b8b6),[-mask_match,-shape_match,-rotation_match,-color_match],['(7, 2) '],[]).
more_test_info(v(bc4146bd),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(bcb3040b),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(bd14c3bf),[+mask_match,+shape_match,+color_match,+'Errors',-rotation_match],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v(be03b35f),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(bf32578f),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(bf699163),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(bf89d739),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(c074846d),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 2) '],[]).
more_test_info(v(c1990cce),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(c3202e5a),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(c35c1b4c),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(c48954c1),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(c62e2108),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(c64f1187),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(c658a4bd),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(c663677b),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(c6e1b8da),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(c7d4e6ad),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(c87289bb),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(c8b7cc0f),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(c92b942c),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(c97c0139),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(ca8de6ea),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(ca8f78db),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(cad67732),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(cb227835),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(ccd554ac),[+color_match,-mask_match,-shape_match,-rotation_match],['(6, 1) '],[]).
more_test_info(v(cd3c21df),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(ce039d91),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(ce8d95cc),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(cf133acc),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(cfb2ce5a),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(d017b73f),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(d19f7514),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(d282b262),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(d2acf2cb),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(d304284e),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(d37a1ef5),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(d47aa2ff),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(d492a647),[+shape_match,+color_match,-mask_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v(d4b1c2b1),[+color_match,-mask_match,-shape_match,-rotation_match],['(7, 1) '],[]).
more_test_info(v(d4c90558),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(d56f2372),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(d5c634a2),[-mask_match,-shape_match,-rotation_match,-color_match],['(7, 2) '],[]).
more_test_info(v(d931c21c),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(d94c3b52),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(da2b0fe3),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 2) '],[]).
more_test_info(v(da515329),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(dc2aa30b),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(dc2e9a9d),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(dd2401ed),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(de493100),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(df8cc377),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e0fb7511),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e133d23d),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(e1baa8a4),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(e1d2900e),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e2092e0c),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e21a174a),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 2) '],[]).
more_test_info(v(e345f17b),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 2) '],[]).
more_test_info(v(e4075551),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e41c6fd3),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e57337a4),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e5790162),[+shape_match,+color_match,-mask_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v(e5c44e8f),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e619ca6e),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e633a9e5),[+color_match,-mask_match,-shape_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e66aafb8),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(e681b708),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e69241bd),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e6de6e8f),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(e74e1818),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(e760a62e),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e7639916),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e78887d1),[+color_match,-mask_match,-shape_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(e7a25a18),[-mask_match,-shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(e7b06bea),[+shape_match,-mask_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(e7dd8335),[+mask_match,+shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e872b94a),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(e88171ec),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e95e3d8e),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e99362f0),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v(e9ac8c9e),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(e9b4f6fc),[-mask_match,-shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(e9bb6954),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(e9c9d9a1),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(ea959feb),[+mask_match,+shape_match,+color_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(ea9794b1),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v(ecaa0ec1),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(ed74f2f2),[-mask_match,-shape_match,-rotation_match,-color_match],['(6, 1) '],[]).
more_test_info(v(ed98d772),[+color_match,-mask_match,-shape_match,-rotation_match],['(5, 1) '],[]).
more_test_info(v(ef26cbf6),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(f0afb749),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(f0df5ff0),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(f21745ec),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(f3b10344),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(f3cdc58f),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(f3e62deb),[+shape_match,+color_match,-mask_match,-rotation_match],['(6, 2) '],[]).
more_test_info(v(f4081712),[-mask_match,-shape_match,-rotation_match,-color_match],['(5, 1) '],[]).
more_test_info(v(f45f5ca7),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(f5aa3634),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(f5c89df1),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(f823c43c),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(f83cb3f6),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(f8be4b64),[+shape_match,+color_match,+'Errors',-mask_match,-rotation_match],['(4, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v(f9a67cb5),[+shape_match,+color_match,-mask_match,-rotation_match],['(3, 1) '],[]).
more_test_info(v(f9d67f8b),[+mask_match,+shape_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v(fafd9572),[+mask_match,+shape_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(fb791726),[-mask_match,-shape_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(fc754716),[+shape_match,+color_match,-mask_match,-rotation_match],['(4, 1) '],[]).
more_test_info(v(fd096ab6),[+mask_match,+shape_match,+color_match,-rotation_match],['(2, 1) '],[]).
more_test_info(v(fd4b2b02),[+shape_match,-mask_match,-rotation_match,-color_match],['(3, 1) '],[]).
more_test_info(v(fe9372f3),[+shape_match,-mask_match,-rotation_match,-color_match],['(2, 1) '],[]).
more_test_info(v(fea12743),[+mask_match,+shape_match,+'Errors',-rotation_match,-color_match],['(3, 1)'], ['https://www.kaggle.com/c/abstraction-and-reasoning-challenge/discussion/131021']).
more_test_info(v(ff72ca3e),[+shape_match,-mask_match,-rotation_match,-color_match],['(4, 1) '],[]).
more_test_info(v('576224'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(2, 1) '],[]).
more_test_info(v('009d5c81'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(5, 1) '],[]).
more_test_info(v('00dbd492'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('3560426'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('05a7bcf2'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('0607ce86'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0692e18c'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('070dd51e'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(2, 1) '],[]).
more_test_info(v('08573cc6'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0934a4d8'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('09c534e7'),[+mask_match,+shape_match,+color_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0a1d4ef5'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('0a2355a6'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('0b17323b'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('0bb8deee'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('0becf7df'),[+mask_match,+shape_match,+color_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0c786b71'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0c9aba6e'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('0d87d2a6'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('0e671a1a'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('0f63c0b9'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('103eff5b'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('11e1fe23'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('12422b43'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(5, 1) '],[]).
more_test_info(v('12997ef3'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 2) '],[]).
more_test_info(v('12eac192'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('136b0064'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('13713586'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('137f0df0'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('140c817e'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('14754a24'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('15113be4'),[+mask_match,+shape_match,+color_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('15663ba9'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('15696249'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('16b78196'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(2, 1) '],[]).
more_test_info(v('17b80ad2'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('17cae0c1'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('18419cfa'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('184a9768'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('195ba7dc'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('1990f7a8'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('19bb5feb'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('1a2e2828'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(5, 1) '],[]).
more_test_info(v('1a6449f1'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('1acc24af'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('1c02dbbe'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('1c0d0a4b'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('1c56ad9f'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('1d0a4b61'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('1d398264'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 2) '],[]).
more_test_info(v('1da012fc'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('1e81d6f9'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('1e97544e'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('2037f2c7'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('2072aba6'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('2.08e+20'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('20981f0e'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('212895b5'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('21f83797'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('22a4bbc2'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('25094a63'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('2546ccf6'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(2, 1) '],[]).
more_test_info(v('256b0a75'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('2685904e'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(6, 1) '],[]).
more_test_info(v('2697da3f'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('2753e76c'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('27a77e38'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('27f8ce4f'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('281123b4'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(6, 1) '],[]).
more_test_info(v('292dd178'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('29700607'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('2a5f8217'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('2b01abd0'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('2c0b0aff'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('2c737e39'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('2f0c5170'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('310f3251'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('3194b014'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('319f2597'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('31adaf00'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('31d5ba1a'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(5, 2) '],[]).
more_test_info(v('32e9702f'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('332efdb3'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('3391f8c0'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('33b52de3'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('3490cc26'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('34b99a2b'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(4, 1) '],[]).
more_test_info(v('351d6448'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('358ba94e'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(4, 1) '],[]).
more_test_info(v('37d3e8b2'),[+mask_match,+shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('3979b1a8'),[+color_match,-mask_match,-shape_match,-rotation_match,test],['(2, 1) '],[]).
more_test_info(v('3a301edc'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(5, 1) '],[]).
more_test_info(v('3b4c2228'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(5, 2) '],[]).
more_test_info(v('3d31c5b3'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(6, 1) '],[]).
more_test_info(v('3ed85e70'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('3ee1011a'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('3f23242b'),[+shape_match,-mask_match,-rotation_match,-color_match,test],['(2, 1) '],[]).
more_test_info(v('40f6cd08'),[+mask_match,+shape_match,+color_match,-rotation_match,test],['(3, 1) '],[]).
more_test_info(v('414297c0'),[-mask_match,-shape_match,-rotation_match,-color_match,test],['(3, 1) '],[]).
more_test_info(v('423a55dc'),[+shape_match,+color_match,-mask_match,-rotation_match,test],['(5, 1) '],[]).



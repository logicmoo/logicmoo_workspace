
do_gp([],Pixels,Pixels):- !.
do_gp([H|T],Pixels,NewPixels):- do_gp(H,Pixels,MPixels),
 do_gp(T,MPixels,NewPixels).
do_gp(G-P,Pixels,NewPixels):- hv_point(H,V,P),do_gp(G,H,V,P,Pixels,NewPixels).

do_gp(C,_H,_V,_P,G,G):- var(C),!.
do_gp('=',H,V,P,G,G2):- get_colum(H,G,Col),insert_col(H,Col,G,G1),do_gp('-',H,V,P,G1,G2).
do_gp('|',_H,V,_P,G,G2):- nth0(V,G,Col),insert_row(V,Col,G,G2).
do_gp('/',H,V,P,G,G2):- do_gp('-',H,V,P,G,G1),do_gp('|',H,V,P,G1,G2). 
do_gp('\\',H,V,P,G,G2):- do_gp('|',H,V,P,G,G1),do_gp('/',H,V,P,G1,G2). 
do_gp('-',H,_V,_P,G,G2):- get_colum(H,G,Col),insert_col(H,Col,G,G2).
do_gp(_,_,_,_,G,G).

bg_sym('bg').
fg_sym('fg').

l_shape(circle,"
 o=o !
o...o!
|.,.|!
o...o!
 o=o !").

l_shape([round,symmetrical],"
        ooo     
      o.....o   
     o.......o  
    o.........o 
    o....,....o 
    o.........o 
     o.......o  
      o.....o   
        ooo     ").

l_shape(square,"
o_o!
|,|!
o_o!").

l_shape(diamond,"
  o  !
 /.\\ !
o.,.o!
 \\./ !
  o  !").


l_shape([heart,h_symmetric],"
 o o !
o.o.o!
o.,.o!
 \\./ !
  o  !").

l_shape([balloon,h_symmetric],"
 o-o !
o.,.o!
o...o!
 \\./ !
  -  !").


l_shape(right_triangle,"
    o!
   /|!
  o.o!
 o.,o!
o=ooo!").


l_shape(building,"
o-,=o!
|...|!
o-o=o!").


l_shape([triangle,h_symmetric],"
   o   !
  o.o  !
 /.,.\\ !
o-ooo-o!").

%l_shape([h_symmetric,hammer],H):- hammer2(H).

hammer2("
___________
 B B B B B 
 B B B B B 
     B     
     B     
___________").

the_hammer(BlueHammer):-  the_hammer(blue,BlueHammer).
the_hammer(RedHammer):-  the_hammer(red,RedHammer).

the_hammer(blue, LibObj):- hammer2(Text), text_to_grid(Text,H,V,Points,_Hammer),
  make_indiv_object('ID',H,V,Points,[object_shape(hammer)],LibObj).

the_hammer(Color,ColorHammer):- 
  ColorHammer = obj([mass(6), shape([point_01_01, point_01_02, point_01_03, point_02_01, point_02_02, point_03_02]), 
  colors([cc(Color, 6.0)]), localpoints([Color-point_01_01, Color-point_01_02, Color-point_01_03, Color-point_02_01, 
  Color-point_02_02, Color-point_03_02]), visual_hv(3, 3), rotation(same), loc_xy(2, 5), 
  changes([]), object_shape(rectangluar), object_shape(hammer), object_indv_id(t('1b60fb0c')*(trn+666)*out, 666), 
  globalpoints([Color-point_02_05, Color-point_02_06, Color-point_02_07, Color-point_03_05, Color-point_03_06, Color-point_04_06]), 
  grid_size(10, 10)]).


shape_info_props(Shapes,ShapeProps):- is_list(Shapes),!,maplist(shape_info_props,Shapes,ShapeProps).
shape_info_props(Shape,object_shape(Shape)).

g_shape_lib(LibObj):- 
  in_grid_shape_lib(Shapes0,Grid,GrowthChart),
  grid_size(Grid,H,V),
  enum_scale(Scale),
  flatten([Shapes0],Shapes),
  shape_info_props(Shapes,ShapeProps),
  flatten([Shapes,H,V,Scale],AList),
  atomic_list_concat(AList,'_',ID),
  scale_grid(Scale,GrowthChart,Grid,ScaledGrid),
  localpoints(ScaledGrid,Points),
  make_indiv_object(ID,H,V,Points,ShapeProps,LibObj).


get_shape_lib(Hammer,ReservedS):- findall(Obj,in_shape_lib(Hammer,Obj),Reserved),sort(Reserved,ReservedS).

%in_shape_lib(Hammer):- the_hammer(RedHammer),all_rotations(RedHammer,Hammer).
in_shape_lib(grid,GRot):- g_shape_lib(LibObj),all_rotations(LibObj,GRot).
in_shape_lib(hammer,Hammer):- the_hammer(RedHammer),all_rotations(RedHammer,Hammer).
  
in_grid_shape_lib([Shape,hollow],Grid,GrowthChart):- enum_fg_colors(Color), 
   bg_sym(BG),
   learned_color_inner_shape(Shape,Color,BG,BGrid,GrowthChart),
   bg_to_fresh_vars(BGrid,Grid).
in_grid_shape_lib([Shape,filled],Grid,GrowthChart):- enum_fg_colors(Color), 
   fill_color(Color,Fill), learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart).
in_grid_shape_lib([Shape,solid],Grid,GrowthChart):- enum_fg_colors(Color), 
   learned_color_inner_shape(Shape,Color,Color,Grid,GrowthChart).


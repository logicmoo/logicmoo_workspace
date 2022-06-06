/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- discontiguous in_shape_lib/2.
:- discontiguous make_shape/2.
:- dynamic in_shape_lib/2.
:- dynamic make_shape/2.
:- discontiguous decl_sf/1.


do_gp([],Pixels,Pixels):- !.
do_gp([H|T],Pixels,NewPixels):- do_gp(H,Pixels,MPixels),
 do_gp(T,MPixels,NewPixels).
do_gp(G-P,Pixels,NewPixels):- hv_point(H,V,P),do_gp(G,H,V,P,Pixels,NewPixels).

do_gp(C,_H,_V,_P,G,G):- plain_var(C),!.
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


l_shape([heart],"
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

the_hammer1(BlueComplex):- the_hammer(blue,BlueComplex).
the_hammer1(RedComplex):-  the_hammer(red,RedComplex).

the_hammer(blue, LibObj):- hammer2(Text), text_to_grid(Text,H,V,Points,_Complex), 
  make_indiv_object('ID',H,V,Points,[object_shape(hammer)],LibObj).
the_hammer(Color,ColorComplex):- 
  ColorComplex = obj([mass(6), shape([point_01_01, point_01_02, point_01_03, point_02_01, point_02_02, point_03_02]), 
  colors([cc(Color, 6.0)]), localpoints([Color-point_01_01, Color-point_01_02, Color-point_01_03, Color-point_02_01, 
  Color-point_02_02, Color-point_03_02]), vis_hv(3, 3), rotation(same), loc_xy(2, 5), 
  changes([]), object_shape(rectangluar), object_shape(hammer), object_indv_id(t('1b60fb0c')*(trn+666)*out, 666), 
  globalpoints([Color-point_02_05, Color-point_02_06, Color-point_02_07, Color-point_03_05, Color-point_03_06, Color-point_04_06]), 
  grid_size(10, 10)]).


shape_info_props(Shapes,ShapeProps):- is_list(Shapes),!,maplist(shape_info_props,Shapes,ShapeProps).
shape_info_props(Shape,object_shape(Shape)).

l_shape(LibObj):- 
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

% todo temp
sortshapes(List,ListS):- predsort(using_compare(shape_key),List,ListS),!.
%sortshapes(List,ListS):- sort(List,ListS),!.


frozen_key(Key1,Key):- copy_term(Key1,Key),numbervars(Key,0,_,[attvar(skip),singletons(true)]).

shape_key(Shape,Key):- into_grid(Shape,Key1),frozen_key(Key1,Key).

shape_key_unrotated(Shape,Key):- shape_key(Shape,KeyR), grav_rot0(Key,KeyR).


grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
grav_rot(Shape,KeyR):- into_grid(Shape,Key1), grav_rot0(Key1,KeyR).

grav_rot0(Shape,ShapeO):- 
  findall(GRot,all_rotations(Shape,GRot),List),
  predsort(sort_on(grav_mass),List,[ShapeO|_]).

grav_mass(Grid,Mass):- grid_size(Grid,H,V), grav_mass(Grid,H,V,Mass).

% make things bottem heavy
grav_mass(Grid,H,V,Mass):- H<V, rot90(Grid,Grid90),grav_mass(Grid90,V,H,Mass).
grav_mass(Grid,_,_,Grid180):- is_symetric_h(Grid),!,is_top_heavy(Grid),rot180(Grid,Grid180).
/*
grav_mass(Grid,Mass):- grid_size(Grid,H,V), HV is round(H/V), Vh is floor(V/2),
  findall(C,(between(Vh,V,Vi),between(0,H,Hi), Hi*HV > Vi, get_color_at(Hi,Vi,Grid,C),is_fg_color(C)),CList),
  length(CList,Mass).
*/
is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),mass(Top,TopM),mass(Bottem,BottemM),BottemM<TopM.

split_50_v(Grid,Top,Bottem):- length(Grid,N),H is floor(N/2), length(Top,H),length(Bottem,H),
    append(Top,Rest,Grid),append(_Mid,Bottem,Rest).




searchable(Group,List):- override_group(searchable(Group,List)),!.
searchable(Shape,Searchable):- object_grid(Shape,Grid), constrain_grid_f(Grid,_CheckType,Searchable).

decolorize(Group,List):- override_group(decolorize(Group,List)),!.
decolorize(Shape,ShapeO):-
  colors_to_vars(_Colors,Vars,Shape,ShapeO),
  set_fg_vars(Vars),
  all_dif_colors(Vars,Vars).
/*
  maplist(label_as_fg(Vars),Vars,CVars).
*/
all_dif_colors([],_):-!.
all_dif_colors([V|Vars],AllVars):-
  all_dif_color(V,AllVars),
  all_dif_colors(Vars,AllVars).
all_dif_color(_,[]).
all_dif_color(V,[All|Vars]):- (V==All;dif(V,All))->all_dif_color(V,Vars).
  

%get_fgc(fg).
get_fgc(C):- enum_fg_colors(C).

rev_lambda(P,A):- P=..[F|Args],C =..[F,A|Args],call(C).

add_shape_lib(Type,Obj):- is_list(Type),!,maplist(rev_lambda(add_shape_lib(Obj)),Type).
add_shape_lib(Type,Obj):-  is_list(Obj), \+ is_grid(Obj), \+ is_points_list(Obj),!,maplist(add_shape_lib(Type),Obj).
add_shape_lib(Type,Obj):- asserta_new(in_shape_lib(Type,Obj)),
  mass(Obj,Mass),Mass>5,
  print_grid(Obj),
  pt(Type).


in_shape_lib(X,D):- (make_shape(X,R), deterministic(TF), dupe_shape(R,D)), (TF==true -> !; true).

make_shape(P,I):- enum_make_shape(P), call(call,P,I).

pad_sides(Fill,Row):- append([_|Fill],[_],Row).
pad_sides(P1,Fill,Row):- call(P1,C1),call(P1,C2),append([C1|Fill],[C2],Row).
pad_sides(C1,Fill,C2,Row):- append([C1|Fill],[C2],Row).

ensure_grid(Grid):- is_grid(Grid),!.
ensure_grid(Grid):- between(1,30,H),between(1,30,V),make_grid(H,V,Grid).


decl_pt(P):- clause(decl_sf(Q),true), append_term(Q,+,P).
decl_sf(Q):- clause(decl_pt(P),true), P=..L, append(MI,[+],L), Q=..MI.

enum_make_shape(P):- var(P),!,decl_sf(Q),functor(Q,F,A),functor(P,F,A),check_args(Q,P).
enum_make_shape(P):- compound(P),!,functor(P,F,A),functor(Q,F,A),decl_sf(Q),check_args(Q,P).

:- decl_sf(box_grid(fg_color,grid)).
box_grid(C,Grid,D):-
  get_fgc(C), ensure_grid(Grid),
  grid_size(Grid,H,_), H2 is H +2,
  length(TB,H2),maplist(=(C),TB),
  maplist(pad_sides(=(C)),Grid,FillRows),
  append([TB|FillRows],[TB],D).

:- decl_sf(box_grid_n_times(size,fg_color,grid)).
box_grid_n_times(0,_C,Grid,D):- Grid=D,!.
box_grid_n_times(N,C,Grid,D):- !,
  make_shape(box_grid(C,Grid),G), plus(M,1,N),
  make_shape(box_grid_n_times(M,C,G),D).


:- decl_sf(solid_square(fg_color,size)).
solid_square(C,HW,FillRows):-
  get_fgc(C), between(1,30,HW),
  length(Fill,HW),
  maplist(=(C),Fill),
  length(FillRows,HW),
  maplist(=(Fill),FillRows).

decl_sf(hollow_square(fg_color,bg_color,size)).
hollow_square(C,BG,HW,D):-
  between(1,30,HW),
  get_bgc(BG), M is HW-1, 
  make_shape(solid_square(BG,M),Grid),
  make_shape(box_grid(C,Grid),D).


dupe_shape(E,F):- \+ is_list(E),!,duplicate_term(E,F).
dupe_shape(L,E):- maplist(dupe_shape,L,E).
  

show_shape_lib_expanded(Complex):- 
  get_shape_lib(Complex,GallerySOS),
  debug_indiv(GallerySOS).

show_shape_lib:- mmake, findall(Complex,(clause(in_shape_lib(Complex,_Obj),_),nonvar(Complex)),Gallery),
  list_to_set(Gallery,GalleryS),maplist(show_shape_lib,GalleryS).
clear_shape_lib:- findall(Complex,in_shape_lib(Complex,_Obj),Gallery),
  list_to_set(Gallery,GalleryS),maplist(clear_shape_lib,GalleryS).

show_shape_lib(Complex):- 
  pt(show_shape_lib(Complex)),
  get_shape_lib_rules(Complex,GallerySOS),
  debug_indiv(GallerySOS).

clear_shape_lib(Complex):- 
  pt(clear_shape_lib(Complex)),
  forall(clause(in_shape_lib(Complex,_),true,Ref),erase(Ref)),
  show_shape_lib(Complex).

 
get_shape_lib_direct(Complex,GalleryS):- 
  findall(Obj,in_shape_lib(Complex,Obj),Gallery),  
  sortshapes(Gallery,GalleryS).

get_shape_lib_rules(Complex,GalleryS):- 
  findall(Rule,
   (clause(in_shape_lib(Complex,Obj),Body),Rule=(Obj:- Body)),Gallery),  
  maplist(label_rules(Complex),Gallery),
  sortshapes(Gallery,GalleryS).

label_rules(_Complex,(Obj:- Body)):- 
   debug_var('Shape',Obj),
   Rule=(Obj:- Body),
   guess_pretty(Rule).

get_shape_lib(Complex,GallerySOS):- 
  get_shape_lib_direct(Complex,GalleryS),
  apply_shapelib_xforms(Complex,GalleryS,GallerySO),
  sortshapes(GallerySO,GallerySOS).

expand_shape_directives(Shapes,Flow,NewGroup):- \+ is_list(Shapes),!,expand_shape_directives([Shapes],Flow,NewGroup).
% default expansion
expand_shape_directives(Shapes,[],SmallLib):- must_be_free(SmallLib),
  must_det_l((print_collapsed(
  show_workflow(Shapes,
   [ =,"Vanila indivs",
    % searchable,"Searchable indivs", 
       all_rotations,  "All rotations of indivs", 
       add(change_color_blue), "Add blue indivs", 
       % add(change_color), % "Add new colors indivs",		 
    %decolorize % decolorized points are not yet printable 
    =],SmallLib)
    ))).
expand_shape_directives(A,Flow,B):- show_workflow(A,Flow,B),!.

:- dynamic(is_shapelib_opt/2).

get_shapelib_opts(Name,Opts):- findall(Opt,is_shapelib_opt(Name,Opt),Opts).

apply_shapelib_xforms(Name,GalleryS,SmallLib):- 
  get_shapelib_opts(Name,Opts),expand_shape_directives(GalleryS,Opts,SmallLib).
apply_shapelib_xforms(_Name,Gallery,Gallery):- !.

%in_shape_lib(Complex):- the_hammer1(RedComplex),all_rotations(RedComplex,Complex).

in_shape_lib(All,GRot):- All==all,!,in_shape_lib(_,GRot).
in_shape_lib(colorless(S),Gallery):- nonvar(S),!, in_shape_lib(S,GalleryC),decolorize(GalleryC,Gallery).
in_shape_lib(all_rots(S),Gallery):-  nonvar(S),!, in_shape_lib(S,GalleryC),all_rotations(GalleryC,Gallery).
in_shape_lib(l_shape,LibObj):- l_shape(LibObj).
in_shape_lib(hammer,Complex):- the_hammer1(Complex).
in_shape_lib(seen,O):- g2o(_,O), localpoints(O,LP),LP\==[],length(LP,L),L>4.
  

in_grid_shape_lib([Shape,hollow],Grid,GrowthChart):- 
   learned_color_inner_shape(Shape,Color,BG,BGrid,GrowthChart),
  put_attr(Color,ci,fg(1)), 
  bg_sym(BG), 
  bg_to_fresh_vars(BGrid,Grid).
in_grid_shape_lib([Shape,filled],Grid,GrowthChart):- 
   learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart),
  put_attr(Color,ci,fg(1)), 
  put_attr(Fill,ci,fg(2)), 
  dif(Color,Fill).
in_grid_shape_lib([Shape,solid],Grid,GrowthChart):- 
   learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart),
  put_attr(Color,ci,fg(1)), 
  Fill = Color.

:- fixup_exports.


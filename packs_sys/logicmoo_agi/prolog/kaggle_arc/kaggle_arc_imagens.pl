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

l_shape([circle,hv_symmetric],"
 o=o $
o...o$
|.,.|$
o...o$
 o=o $").

l_shape([round,hv_symmetric],"
    ooo    $
  o.....o  $
 o.......o $
o.........o$
o....,....o$
o.........o$
 o.......o $
  o.....o  $
    ooo    $").

l_shape([square,hv_symmetric],"
o_o$
|,|$
o_o$").

l_shape([diamond,hv_symmetric],"
  o  $
 /.\\ $
o.,.o$
 \\./ $
  o  $").


l_shape([heart,h_symmetric],"
 o o $
o.o.o$
o.,.o$
 \\./ $
  o  $").

l_shape([balloon,h_symmetric],"
 o-o $
o.,.o$
o...o$
 \\./ $
  -  $").


l_shape(right_triangle,"
    o$
   /|$
  o.o$
 o.,o$
o=ooo$").


l_shape(building,"
o-,=o$
|...|$
o-o=o$").


l_shape([triangle,h_symmetric],"
   o   $
  o.o  $
 /.,.\\ $
o-ooo-o$").

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

the_hammer(Color,ColorComplex):- 
  ColorComplex = obj([mass(6), shape([point_01_01, point_01_02, point_01_03, point_02_01, point_02_02, point_03_02]), 
  colors([cc(Color, 6.0)]), localpoints([Color-point_01_01, Color-point_01_02, Color-point_01_03, Color-point_02_01, 
  Color-point_02_02, Color-point_03_02]), vis_hv(3, 3), rotation(same), loc_xy(2, 5), 
  changes([]), object_shape(rectangle), object_shape(hammer), object_indv_id(t('1b60fb0c')*(trn+666)*out, 666), 
  globalpoints([Color-point_02_05, Color-point_02_06, Color-point_02_07, Color-point_03_05, Color-point_03_06, Color-point_04_06]), 
  grid_size(10, 10)]).
the_hammer(blue, LibObj):- hammer2(Text), text_to_grid(Text,H,V,Points,_Complex), 
  make_indiv_object('ID',H,V,Points,[object_shape(hammer)],LibObj).


shape_info_props(Shapes,ShapeProps):- is_list(Shapes),!,maplist(shape_info_props,Shapes,ShapeProps).
shape_info_props(Shape,object_shape(Shape)).

l_shape(LibObj):- 
  in_grid_shape_lib(Shapes0,Grid,GrowthChart),
  once(must_det_l((
  grid_size(Grid,H,V),
  enum_scale(Scale),
  flatten([Shapes0],Shapes),
  shape_info_props(Shapes,ShapeProps),
  flatten([Shapes,H,V,Scale],AList),!,
  atomic_list_concat(AList,'_',ID),
  scale_grid(Scale,GrowthChart,Grid,ScaledGrid),
  globalpoints(ScaledGrid,Points)))),
  catch(make_indiv_object(ID,H,V,Points,[object_shape(l_shape)|ShapeProps],LibObj),_,
    (rtrace(make_indiv_object(ID,H,V,Points,[object_shape(l_shape)|ShapeProps],LibObj)))).

% todo temp
sortshapes(List,List):-!.
sortshapes(List,ListS):- predsort(using_compare(shape_key),List,ListS),!.
%sortshapes(List,ListS):- sort(List,ListS),!.


frozen_key(Key1,Key):- copy_term(Key1,Key),numbervars(Key,0,_,[attvar(skip),singletons(true)]).

shape_key(Shape,Key):- into_grid(Shape,Key1),frozen_key(Key1,Key).

shape_key_unrotated(Shape,Key):- shape_key(Shape,KeyR), grav_rot0(Key,KeyR).


searchable(Group,List):- override_group(searchable(Group,List)),!.
searchable(Shape,Searchable):- object_grid(Shape,Grid), constrain_grid(f,_CheckType,Grid,Searchable).

decolorize(Group,List):- override_group(decolorize(Group,List)),!.
decolorize(Shape,ShapeO):- 
  colors_to_vars(_Colors,Vars,Shape,ShapeO),
  set_fg_vars(Vars),length(Vars,L),writeln(set_fg_vars=L),
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
llamma(P,A):- \+ \+ call(P,A).

% \+ is_points_list(Obj),

%add_shape_lib(Type,Obj):- !, nop(pt(add_shape_lib(Type,Obj))).
add_shape_lib(Type,Obj):- \+ ground(Obj),pt(add_shape_lib(Type,Obj)),fail.
add_shape_lib(Type,Obj):-  is_object(Obj),!,add_shape_lib0(Type,Obj),!.
add_shape_lib(Type,Obj):-  is_grid(Obj),!,add_shape_lib0(Type,Obj),!.

add_shape_lib(Type,[Obj|L]):- (is_group(Obj);is_object(Obj) ; is_grid(Obj)),!,maplist(add_shape_lib(Type),[Obj|L]).
add_shape_lib(Type,Obj):-  is_list(Obj), \+ is_grid(Obj), !, maplist(add_shape_lib(Type),Obj).

add_shape_lib(Type,Obj):- must_det_l(add_shape_lib0(Type,Obj)).

add_shape_lib0(Type,Obj):- mass(Obj,Mass),!,
  %dash_char, print_grid(Obj),
  ( Mass<3 
   -> nop(pt(too_small_for_shapelib(Type,Mass))) ; (nop(pt(add_shape_lib(Type))),assert_shape_lib(Type,Obj))), 
  %dash_char,
  !.

assert_shape_lib(_,Obj):-  mass(Obj,Mass), Mass<4,!.
assert_shape_lib(Type,Obj):- is_list(Type),!,maplist(rev_lambda(assert_shape_lib(Obj)),Type).
assert_shape_lib(Type,Obj):- my_asserta_if_new(in_shape_lib(Type,Obj)).

in_shape_lib(X,D):- (make_shape(X,R), deterministic(TF), dupe_shape(R,D)), (TF==true -> !; true).

make_shape(P,I):- compound(P),ground(P),check_args(P,C), call(call,C,I).

pad_sides(Fill,Row):- append([_|Fill],[_],Row).
pad_sides(P1,Fill,Row):- call(P1,C1),call(P1,C2),append([C1|Fill],[C2],Row).
pad_sides(C1,Fill,C2,Row):- append([C1|Fill],[C2],Row).

ensure_grid(Grid):- is_grid(Grid),!.
ensure_grid(Grid):- between(1,30,H),between(1,30,V),make_grid(H,V,Grid).


decl_pt(P):- var(P), clause(decl_sf(Q),true), append_term(Q,+,P).
decl_sf(Q):- var(Q), clause(decl_pt(P),true), P=..L, append(MI,[+],L), Q=..MI.

enum_make_shape(P):- var(P),!,decl_sf(Q),functor(Q,F,A),functor(P,F,A), \+ \+ check_args(Q,P).
enum_make_shape(P):- compound(P),!,functor(P,F,A),functor(Q,F,A),decl_sf(Q), \+ \+ check_args(Q,P).

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
  

show_shape_lib_expanded(Name):- 
  shape_lib_expanded(Name,GallerySOS),
  debug_indiv(GallerySOS).

show_shape_lib:- %mmake, 
  findall(Name,(clause(in_shape_lib(Name,_Obj),_),nonvar(Name)),Gallery),
  list_to_set(Gallery,GalleryS),maplist(show_shape_lib,GalleryS).

clear_shape_lib:- findall(Name,in_shape_lib(Name,_Obj),Gallery),
  list_to_set(Gallery,GalleryS),maplist(clear_shape_lib,GalleryS).

show_shape(Shape):- is_grid(Shape),
 dash_char, print_grid(Shape),writeln(grid_shape).

show_shape(Shape):- ground(Shape),!,
  
  ignore(print_info(Shape)),
  ignore(print_grid([Shape])),
  ignore((\+ ground(Shape),pt(Shape))),!.
show_shape(Shape):-
  dash_char,
  ignore(print_info(Shape)),
  ignore((\+ \+ print_shape_0(Shape) ->true;writeln(failed_print_grid))),
  ignore((\+ ground(Shape),pt(Shape))),!.


print_shape_0(Shape):-
  vis_hv(Shape,H,V),
  localpoints(Shape,Points),

  numbervars(Points,0,_,[attvar(bind)]),
  subst(Points,'$VAR'(0),grey,Points0),
  subst(Points0,'$VAR'(1),grey,Points1),
  subst(Points1,'$VAR'(2),grey,Points2),
  subst(Points2,'$VAR'(3),grey,Points3),
  points_to_grid(H,V,Points3,Grid),
  %object_grid(Shape,FG),
  %numbervars(Shape,0,_,[attvar(bind)]),
  %grid_numbervars(FG,Grid),
  pt(Grid), 
  %object_indv_id(Shape,_Glyph,Iv),
  print_grid(H,V,Grid),
  %locally(nb_setval(alt_grid_dot,Iv),print_grid(H,V,Grid)).

  

show_shape_lib(Name):- make,
 pt(?- show_shape_lib(Name)),
 mort((shape_lib_direct(Name,GalleryS), length(GalleryS,Len), pt(shape_lib_direct(Name)=Len))),
 ignore(( Len\==0,
  maplist(show_shape,GalleryS),  
  mort((shape_lib_expanded(Name,GallerySOS), length(GallerySOS,LenOS), underline_print(pt(shape_lib_expanded(Name)=LenOS)))),
  maplist(show_shape,GallerySOS),
  %shape_lib_rules(Name,Rules),length(Rules,LenRules),pt(shape_lib_rules(LenRules)=Rules),
  mort(ignore((shapelib_opts(Name,Opts), length(Opts,LenOpts), LenOpts > 0, pt(shapelib_opts(LenOpts)=Opts)))),!,
  ignore((fail,GalleryS\==[], maplist(show_shape,GalleryS))))).

clear_shape_lib(Name):- 
  findall(_,(clause(in_shape_lib(Name,_),true,Ref),erase(Ref)),L),
  length(L,Len),pt(clear_shape_lib(Name)=Len),
  nop(show_shape_lib(Name)).
 
shape_lib_expanded(Name,GallerySOS):- 
  shape_lib_direct(Name,GalleryS),
  apply_shapelib_xforms(Name,GalleryS,GallerySO),
  sortshapes(GallerySO,GallerySOS),!.
 
shape_lib_direct(Name,GalleryS):- 
  findall(Obj,in_shape_lib(Name,Obj),Gallery),  
  sortshapes(Gallery,GalleryS).

shape_lib_rules(Name,GalleryS):- 
  findall(Rule,
   (clause(in_shape_lib(Name,Obj),Body),Rule=(in_shape_lib(Name,Obj):- Body)),Gallery),  
  maplist(label_rules(Name),Gallery),
  sortshapes(Gallery,GalleryS).

label_rules(_Complex,(Obj:- Body)):- 
   debug_var('Shape',Obj),
   Rule=(Obj:- Body),
   guess_pretty(Rule).

shapelib_opts(Name,Opts):- findall(Opt,is_shapelib_opt(Name,Opt),Opts).

apply_shapelib_xforms(Name,GalleryS,SmallLib):-  shapelib_opts(Name,Opts),expand_shape_directives(GalleryS,Opts,SmallLib).
apply_shapelib_xforms(_Name,Gallery,Gallery):- !.


expand_shape_directives(Shapes,Opts,NewGroup):- \+ is_list(Shapes),!,expand_shape_directives([Shapes],Opts,NewGroup).
% default expansion
expand_shape_directives(Shapes,[],SmallLib):- must_be_free(SmallLib),
  must_det_l(((
 print_collapsed(100,
  show_workflow(Shapes,
   [ =, %"Vanila indivs",
     %into_grid,
    % searchable,"Searchable indivs", 
       all_orientations, % "All rotations of indivs", 
       %decolorize, %"Add blue indivs", 
       %add(change_color), % "Add new colors indivs",		 
       %all_colors,
       smallest_first, "smallest first",
    %decolorize % decolorized points are not yet printable 
    =],SmallLib)
    )))).
expand_shape_directives(A,Opts,B):- show_workflow(A,Opts,B),!.

:- dynamic(is_shapelib_opt/2).

%in_shape_lib(Name):- the_hammer1(RedComplex),all_rotations(RedComplex,Name).

in_shape_lib(All,GRot):- All==all,!,in_shape_lib(_,GRot).
in_shape_lib(decolorize(S),Gallery):- nonvar(S),!, in_shape_lib(S,GalleryC),decolorize(GalleryC,Gallery).
in_shape_lib(all_rots(S),Gallery):-  nonvar(S),!, in_shape_lib(S,GalleryC),all_rotations(GalleryC,Gallery).
in_shape_lib(l_shape,LibObj):- l_shape(LibObj).
in_shape_lib(hammer,Name):- the_hammer1(Name).
in_shape_lib(seen,O):- g2o(_,O), localpoints(O,LP),LP\==[],length(LP,L),L>4.

all_rots(X,Y):- all_rotations(X,Y).

in_grid_shape_lib([Shape,hollow],Grid,GrowthChart):- 
  get_bgc(BG),
  learned_color_inner_shape(Shape,Color,BG,Grid,GrowthChart),
  put_attr(Color,ci,fg(1)).
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


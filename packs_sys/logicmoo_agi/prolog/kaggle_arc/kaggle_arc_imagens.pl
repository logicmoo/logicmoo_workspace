/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).


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


l_shape([heart,symmetric_type(flipH)],"
 o o $
o.o.o$
o.,.o$
 \\./ $
  o  $").

l_shape([balloon,symmetric_type(flipH)],"
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


l_shape([triangle,symmetric_type(flipH)],"
   o   $
  o.o  $
 /.,.\\ $
o-ooo-o$").

%l_shape([symmetric_type(flipH),hammer],H):- hammer2(H).

hammer2("
___________
 B B B B B 
 B B B B B 
     B     
     B     
___________").

hammer3("
 ___________________
|                   |
|           ® ®     |
|     ®             |
|                   |
|         ®     ®   |
|                   |
| ®                 |
|           ® ®     |
|                   |
 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ ").


the_hammer1(BlueComplex):- the_hammer(blue,BlueComplex).
the_hammer1(RedComplex):-  the_hammer(red,RedComplex).

the_hammer(Color,ColorComplex):- 
  ColorComplex = obj([mass(6), shape_rep(grav,[point_01_01, point_01_02, point_01_03, point_02_01, point_02_02, point_03_02]), 
  colors_cc([cc(Color, 6)]), points_rep(local,[Color-point_01_01, Color-point_01_02, Color-point_01_03, Color-point_02_01, 
  Color-point_02_02, Color-point_03_02]), vis2D(3, 3), rot2D(sameR), loc2D(2, 5), 
  changes([]), iz(shape(rectangle)), iz(hammer), 
  globalpoints([Color-point_02_05, Color-point_02_06, Color-point_02_07, Color-point_03_05, Color-point_03_06, Color-point_04_06]), 
  grid_size(10, 10)]).

the_hammer(blue, LibObj):- hammer2(Text), must_det_ll(into_g666(Text,Grid)),
   into_lib_object([blue,hammer],Grid,LibObj).

shape_info_props(Shapes,ShapeProps):- flatten([Shapes],Shapes0),maplist(shape_info_props0,Shapes0,ShapeProps).
shape_info_props0(P,P):- compound(P),!.
shape_info_props0(Shape,iz(Shape)).

into_color_grid(Grid,ColorGrid):-
  into_grid(Grid,Grid0),
  mapgrid(for_color_grid,Grid0,ColorGrid).

for_color_grid(Var,Var):- is_color(Var),!.
for_color_grid(Var,Color):- number(Var),color_name(Var,Color),!.
for_color_grid(Var,Var):- plain_var(Var),!.
for_color_grid(C,D):- only_color_data(C,D).


l_shape(LibObj):-    
  in_grid_shape_lib(Shapes0,Grid,GrowthChart),
  %grid_size(Grid,H,V),
  enum_scale(Scale),
  %flatten([Shapes,H,V,Scale],AList),!,
  %atomic_list_concat(AList,'_',ID),
  scale_grid(Scale,GrowthChart,Grid,ScaledGrid),
  into_lib_object([iz(l_shape),Shapes0],ScaledGrid,LibObj).

into_lib_object(Shapes0,Grid0,LibObj):- fail,
  flatten([Shapes0],Shapes),
  shape_info_props(Shapes,ShapeProps),
  into_lib_object1(ShapeProps,Grid0,LibObj).

into_lib_object1(ShapeProps,Grid0,LibObj):-  
  into_color_grid(Grid0,ScaledGrid),!,
  arc_memoized(into_lib_object2(ShapeProps,ScaledGrid,LibObj)).

into_lib_object2(ShapeProps,ScaledGrid,LibObj):-
  once(must_det_ll((
  grid_size(ScaledGrid,H,V),
  %flatten([Shapes,H,V,Scale],AList),!,
  %atomic_list_concat(AList,'_',ID),
  %grid_colors(ScaledGrid,CGPoints),
  %ppa(grid_colors(ScaledGrid,SPoints)),
  globalpoints_maybe_bg(ScaledGrid,SPoints)))),
  G = make_indiv_object_no_vm(into_lib_object2,H,V,[iz(into_lib_object),grid(ScaledGrid),vis2D(H,V)|ShapeProps],SPoints,LibObj),
  catch(G,E,(arcST,u_dmsg(E=G),atrace,G)).

% todo temp
sortshapes(List,Set):- my_list_to_set_cmp(List, using_compare(shape_key), Set).
% sortshapes(List,ListS):- predsort_using_only(shape_key,List,ListS),!.
%sortshapes(List,ListS):- sort_safe(List,ListS),!.


frozen_key(Key1,Key):- copy_term(Key1,Key),numbervars(Key,0,_,[attvar(skip),singletons(true)]).

shape_key(Shape,Key):- into_grid(Shape,Key1),frozen_key(Key1,Key).

shape_key_undo_effectd(Shape,Key):- shape_key(Shape,KeyR), grav_rot(Key,_,KeyR).


searchable(Group,List):- override_group(searchable(Group,List)),!.
searchable(Shape,Searchable):- object_grid(Shape,Grid), constrain_grid(f,_CheckType,Grid,Searchable).

% =====================================================================
is_fti_step(into_monochrome).
% =====================================================================
into_monochrome(VM):- Grid = VM.grid,
  into_monochrome2(Grid,NewGrid),!,
  set(VM.grid) = NewGrid,  
  print_side_by_side(silver,Grid,into,_,NewGrid,monochrome).

into_monochrome(Color,Mono):- called_gid('_mono', into_monochrome2, Color,Mono),!.

into_monochrome2(NoBlack,Mono):-  get_black(Black),
  color_cc_black_first(NoBlack,CCBF), 
    CCBF=[cc(Black,0),cc(BGC,_)|_],!, 
  into_monochrome4(fg,BGC,NoBlack,Mono).
into_monochrome2(Color,Mono):- get_black(Black),into_monochrome4(fg,Black,Color,Mono).
/*
4495416
Post Office 2700 Capels Ave 98661
9a-2m
360-992-5002
*/

into_monochrome4(FG,BG,Color,Mono):- into_monochrome3(from_monochrome4(FG,BG),Color,Mono).


from_monochrome4(_FG,_BG,Color,Mono):- is_bg_color(Color), decl_bg_color(Mono),!,cv(Mono,Color).
from_monochrome4(_FG,_BG,Color,Mono):- is_fg_color(Color), decl_many_fg_colors(Mono),!,cv(Mono,Color).
from_monochrome4(_FG,BG,Color,Mono):- is_bg_color(Color), apply_recolor(BG,Color,Mono),!.
from_monochrome4(FG,_BG,Color,Mono):- is_fg_color(Color), apply_recolor(FG,Color,Mono),!.

apply_recolor(Izer,Color,Mono):- 
  (is_color(Izer)->copy_term(Izer,Mono);( callable_arity(Izer,2) -> call(Izer,Color,Mono); ( callable_arity(Izer,1) -> call(Izer,Mono)))).


into_monochrome3(MonoP2,Color,Mono):- into_mono3(MonoP2,Color,Mono).

into_mono3(MonoP2,Color,Mono):- is_color(Color),call(MonoP2,Color,Mono),!.
into_mono3(_MonoP2,Color,Mono):- is_bg_color(Color), decl_bg_color(Mono),!, cv(Mono,Color).
into_mono3(_MonoP2,Color,Mono):- is_fg_color(Color), decl_many_fg_colors(Mono),!, cv(Mono,Color).
into_mono3(_MonoP2,Color,Mono):- \+ compound(Color), Mono=Color.
into_mono3(MonoP2,Color,Mono):- is_group(Color),!,mapgroup(into_mono3(MonoP2),Color,Mono).
into_mono3(MonoP2,Color,Mono):- is_grid(Color),!,mapgrid(cell_into_monochrome(MonoP2),Color,Mono).
into_mono3(MonoP2,Color,Mono):- is_list(Color),!,maplist(into_mono3(MonoP2),Color,Mono).
into_mono3(MonoP2,I,O):- compound(I), !, compound_name_arguments(I,F,IA), 
  maplist(into_mono3(MonoP2),IA,OA), compound_name_arguments(O,F,OA).
into_mono3(_MonoP2,I,I).
cell_into_monochrome(MonoP2,I,O):- into_mono3(MonoP2,I,O).

/*
into_monochrome(FGC,BGC,Color,Mono):- is_points_list(Color),!,maplist(points_into_monochrome(FGC,BGC),Color,Mono).
%into_monochrome(FGC,BGC,Color,Mono):- is_grid(Color),!,grid_into_monochrome(FGC,BGC,Color,Mono).
into_monochrome(FGC,BGC,Grid,Mono):- into_grid(Grid,Color),!,grid_into_monochrome(FGC,BGC,Color,Mono).

points_into_monochrome(FGC,BGC,Color-Point,Mono-Point):- is_nc_point(Point),!,points_into_monochrome(FGC,BGC,Color,Mono).
points_into_monochrome(FGC,BGC,Color,Mono):- is_list(Color) -> maplist(points_into_monochrome(FGC,BGC),Color,Mono) ;
                              (Color\=BGC)-> Mono = fg ; Mono = BGC.

grid_into_monochrome(FGC,BGC,Color,Mono):- is_list(Color) -> maplist(grid_into_monochrome(FGC,BGC),Color,Mono) ;
                              (Color\=BGC)-> Mono = fg ; Mono = BGC.


grid_into_fg_bg_pred(FGP1,BGP1,Color,Mono):- is_list(Color) -> maplist(grid_into_fg_bg_pred(FGP1,BGP1),Color,Mono) ;
                              (call(FGP1,Color) -> decl_many_fg_colors(Mono) ; (call(BGP1,Color) -> decl_bg_color(Mono) ; Mono = Color)).
*/

% =====================================================================
is_fti_step(decolorize).
% =====================================================================
decolorize(VM):- Grid = VM.grid,
  decolorize(Grid,NewGrid),!,
  set(VM.grid) = NewGrid,
  print_side_by_side(silver,Grid,into,_,NewGrid,decolorize),
  nl,
  maplist(ppa,NewGrid),
  nl.

decolorize(Color,Mono):- is_grid(Color),!,mapgrid(decolorize_cell,Color,Mono).
decolorize(Color,Mono):- is_group(Color),!,mapgroup(decolorize,Color,Mono).
decolorize(Color,Mono):- is_list(Color),!,maplist(decolorize,Color,Mono).
decolorize(Color,Mono):- is_bg_color(Color), decl_bg_color(Mono),!, cv(Mono,Color).
decolorize(Color,Mono):- is_fg_color(Color), decl_many_fg_colors(Mono),!, cv(Mono,Color).
decolorize(Color,Mono):- \+ compound(Color), Mono=Color.
decolorize(Color,Mono):- compound(Color), !, compound_name_arguments(Color,F,IA), maplist(decolorize,IA,OA), compound_name_arguments(Mono,F,OA).
decolorize(I,I).
decolorize_cell(I,O):- decolorize(I,O).

old_decolorize(Group,List):- override_group(old_decolorize(Group,List)),!.
old_decolorize(Shape,ShapeO):- 
  colors_to_vars(_Colors,Vars,Shape,ShapeO),
  set_fg_vars(Vars),length(Vars,L),
  writeln(set_fg_vars=L),
  all_dif_colors(Vars).


/*
  mapgroup(label_as_fg(Vars),Vars,CVars).
*/
all_dif_colors(Vars):- all_dif_colors(Vars,Vars).
all_dif_colors([],_):-!.
all_dif_colors([V|Vars],AllVars):-
  all_dif_color(V,AllVars),
  all_dif_colors(Vars,AllVars).
all_dif_color(_,[]).
all_dif_color(V,[All|Vars]):- (V==All;dif(V,All))->all_dif_color(V,Vars).
  

%get_fgc(fg).
get_fgc(C):- enum_fg_colors(C).

lambda_rev(P,A):- P=..[F|Args],C =..[F,A|Args],call(C).
llamma(P,A):- \+ \+ call(P,A).

% \+ is_points_list(Obj),

%add_shape_lib(Type,Obj):- !, nop(pp(add_shape_lib(Type,Obj))).
add_shape_lib(Type,Obj):- \+ ground(Obj), nop(pp(add_shape_lib(Type,Obj))),fail.
add_shape_lib(Type,Obj):-  is_object(Obj),!,add_shape_lib0(Type,Obj),!.
add_shape_lib(Type,Obj):-  is_grid(Obj),!,add_shape_lib0(Type,Obj),!.

add_shape_lib(Type,[Obj|L]):- (is_group(Obj);is_object(Obj) ; is_grid(Obj)),!,mapgroup(add_shape_lib(Type),[Obj|L]).
add_shape_lib(Type,Obj):-  is_list(Obj), \+ is_grid(Obj), !, mapgroup(add_shape_lib(Type),Obj).

add_shape_lib(Type,Obj):- add_shape_lib0(Type,Obj).

add_shape_lib0(Type,Obj):- mass(Obj,Mass),!,
  %dash_chars, print_grid(Obj),
  ( Mass<3 
   -> nop(pp(too_small_for_shapelib(Type,Mass))) ; (nop(pp(add_shape_lib(Type))),learn_hybrid_shape(Type,Obj))), 
  %dash_chars,
  !.


in_shape_lib(X,D):- (make_shape(X,R), deterministic(TF), dupe_shape(R,D)), (TF==true -> !; true).

make_shape(P,I):- compound(P),ground(P),check_args(P,C), call(call,C,I).

pad_sides(Fill,Row):- gappend([_|Fill],[_],Row).
pad_sides(P1,Fill,Row):- call(P1,C1),call(P1,C2),gappend([C1|Fill],[C2],Row).
pad_sides(C1,Fill,C2,Row):- gappend([C1|Fill],[C2],Row).

:- decl_pt(ensure_grid(prefer_grid)).
ensure_grid(Grid):- var(Grid),!, arc_grid(_,Grid).
ensure_grid(Grid):- is_grid(Grid),!.
ensure_grid(Grid):- between(1,30,H),between(1,30,V),make_grid(H,V,Grid).


is_decl_pt(P):- var(P), clause(is_decl_sf(Q),true), append_term(Q,+,P).
is_decl_sf(Q):- var(Q), clause(is_decl_pt(P),true), P=..L, gappend(MI,[+],L), Q=..MI.

enum_make_shape(P):- var(P),!,is_decl_sf(Q),functor(Q,F,A),functor(P,F,A), \+ \+ check_args(Q,P).
enum_make_shape(P):- compound(P),!,functor(P,F,A),functor(Q,F,A),is_decl_sf(Q), \+ \+ check_args(Q,P).

likely_fgc(Var):- var(Var),!,get_fgc(Var).
likely_fgc(_).

:- decl_gf(box_grid(fg_color)).
box_grid(C,Grid,GridO):-
  likely_fgc(C), 
  ensure_grid(Grid),
  grid_size(Grid,H,_), H2 is H +2,
  length(TB,H2),maplist(=(C),TB),
  mapgroup(pad_sides(=(C)),Grid,FillRows),
  gappend([TB|FillRows],[TB],D),
  restructure(D,GridO),!.


hollow_rect:- 
  forall((notrace,
    X=10, Y=10,
    hollow_rect(blue,X,Y,Grid),
    get_color_at(1,1,Grid,Edge),
    get_color_at(2,2,Grid,Fill),
    print_grid(X,Y,hollow_rect(Edge,Fill,X,Y,Grid),Grid)   ),true).

in_shape_lib(rect_squares,LibObj):- rect_squares(Props,Grid), 
   into_lib_object([shape_lib(rect_squares)|Props],Grid,LibObj).


rect_squares([iz(solid_rect(CEdgeV,H,V)),filltype(solid),rectangle],Grid):- 
  thirty_down_2(H),thirty_down_2(V),solid_rect(CEdgeV,H,V,Grid).
rect_squares([iz(hollow_rect(CEdgeV,H,V)),rectangle,outline(1),hollow],Grid):- 
  thirty_down_2(H),thirty_down_2(V),hollow_rect(CEdgeV,H,V,Grid).



dupe_row(E,D):- duplicate_term(E,D),E=D.

solid_rect(CEdge,H,V,Grid):-
  (plain_var(CEdge)->decl_one_color(CEdge);is_color(CEdge)),!,
  ((var(H))-> thirty_down_2(H) ; true),
  ((var(V))-> thirty_down_2(V) ; true),
  length(Row,H),maplist(=(CEdge),Row),
  length(Grid,V),maplist(dupe_row(Row),Grid).

solid_rect(CEdge,H,V,Grid):- var(CEdge),!,
  ((var(H))-> thirty_down_2(H) ; true),
  ((var(V))-> thirty_down_2(V) ; true),
  decl_one_color(CEdge), 
  length(Row,H),maplist(copy_term(CEdge),Row),
  length(Grid,V),maplist(copy_term(Row),Grid).

solid_rect(CEdge,H,V,Grid):-
  ((var(H))-> thirty_down_2(H) ; true),
  ((var(V))-> thirty_down_2(V) ; true),
  (H2 is H - 2, H2 >= 0, make_row_n_times(H2,V,CEdge,CEdge,CEdge,Grid)).

solid_rect(CEdge,CIn,H,V,Grid):-
  CEdge==CIn->solid_rect(CEdge,H,V,Grid);
 (decl_one_color(CEdge), decl_many_fg_colors(CIn),
  ((var(H))-> thirty_down_2(H) ; true),
  ((var(V))-> thirty_down_2(V) ; true),
   H2 is H - 2, V2 is V - 2,
   H2>0,V2>0,
  make_row_n_times(H2,V2,CIn,CEdge,CEdge,Middle),
  make_list(CEdge,H,Top), make_list(CEdge,H,Bottom),
  append([Top|Middle],[Bottom],Grid)).


hollow_rect(CEdge,H,V,Grid):-
  hollow_rect(CEdge,_,H,V,Grid).

hollow_rect(CEdge,CIn,H,V,Grid):-
  (plain_var(CEdge)->put_attr(X,ci,hollow(X));true),
   %dif(CIn,CEdge),
  
  ((var(H))-> thirty_down_2(H) ; true),
  ((var(V))-> thirty_down_2(V) ; true),
   H2 is H - 2, V2 is V - 2,
   H2>0,V2>0,
  make_row_n_times(H2,V2,CIn,CEdge,CEdge,Middle),
  make_list(CEdge,H,Top), make_list(CEdge,H,Bottom),
  append([Top|Middle],[Bottom],Grid).

make_row(C,L,H,R,Row):- length(Mid,H),maplist(copy_term(C),Mid),append([L|Mid],[R],Row).

make_row_n_times(H,1,C,L,R,[Row]):-!,make_row(C,L,H,R,Row).
make_row_n_times(_,0,_,_,_,[]):-!.
make_row_n_times(H,V,C,L,R,[Row|Rest]):- plus(M,1,V),
  make_row(C,L,H,R,Row),!,
  make_row_n_times(H,M,C,L,R,Rest).


:- decl_gf(box_grid_n_times(size2D,fg_color)).
box_grid_n_times(0,_C,Grid,D):- Grid=D,!.
box_grid_n_times(N,C,Grid,D):- !,
  make_shape(box_grid(C,Grid),G), plus(M,1,N),
  make_shape(box_grid_n_times(M,C,G),D).

restructure(X,Y):- is_list(X),!,maplist(restructure,X,Y).
restructure(X,X).

:- decl_sf(solid_square(fg_color,size2D)).
solid_square(C,HW,Grid):-  
  likely_fgc(C), 
  between(1,30,HW),
  length(Fill,HW),
  mapgroup(=(C),Fill),
  length(FillRows,HW),
  mapgroup(=(Fill),FillRows),!,
  restructure(FillRows,Grid).


:- decl_sf(hollow_square(fg_color,bg_color,size2D)).

/*
A = [<,-,>] 
    [-,-,>]
    [-,>,>]

B = [-,-,>]  = [-,>,>] 
*/
  

hollow_square(C,HW,D):- get_bgc(BG),!,hollow_square(C,BG,HW,D).
hollow_square(C,BG,HW,D):-
  between(1,30,HW),
  M is HW-1,
  solid_square(BG,M,Grid),
  box_grid(C,Grid,D).



dupe_shape(E,F):- \+ is_list(E),!,duplicate_term(E,F).
dupe_shape(L,E):- mapgroup(dupe_shape,L,E).
  

show_shape_lib_expanded(Name):- 
  shape_lib_expanded(Name,GallerySOS),
  print_info(GallerySOS).

show_shape_lib:- %mmake, 
  findall(Name,(clause(in_shape_lib(Name,_Obj),_),nonvar(Name)),Gallery),
  list_to_set(Gallery,GalleryS),mapgroup(show_shape_lib,GalleryS).

clear_shape_lib:- findall(Name,in_shape_lib(Name,_Obj),Gallery),
  list_to_set(Gallery,GalleryS),mapgroup(clear_shape_lib,GalleryS).


% ===========================================================
show_shape(Shape):- is_grid(Shape),!,
  print_grid(grid_based_shape,Shape).

show_shape(Shape):- ground(Shape),!,  
  ignore(print_info(Shape)),
  ignore(print_grid([Shape])),
  ignore((\+ ground(Shape),pp(Shape))),!.
show_shape(Shape):-
  dash_chars,
  ignore(print_info(Shape)),
  ignore((\+ \+ print_shape_0(Shape) ->true;writeln(failed_print_grid))),
  ignore((\+ ground(Shape),pp(Shape))),!.


print_shape_0(Shape):-
  vis2D(Shape,H,V),
  points_rep(local,Shape,Points),
  
  numbervars(Points,0,_,[attvar(bind)]),
  subst001(Points,'$VAR'(0),grey,Points0),
  subst001(Points0,'$VAR'(1),grey,Points1),
  subst001(Points1,'$VAR'(2),grey,Points2),
  subst001(Points2,'$VAR'(3),grey,Points3),
  points_to_grid(H,V,Points3,Grid),
  %object_grid(Shape,FG),
  %numbervars(Shape,0,_,[attvar(bind)]),
  %grid_numbervars(FG,Grid),
  pp(Grid), 
  %obj_ to_oid(Shape,_Glyph,Iv),
  print_grid(H,V,Grid),!.
  %locally(luser_setval(alt_grid_dot,Iv),print_grid(H,V,Grid)).

show_shape_lib(Name):- make,
 pp(?- show_shape_lib(Name)),
 mort((shape_lib_direct(Name,GalleryS), length(GalleryS,Len), pp(shape_lib_direct(Name)=Len))),
 ignore(( Len\==0,
  mapgroup(show_shape,GalleryS),  
  mort((shape_lib_expanded(Name,GallerySOS), length(GallerySOS,LenOS), underline_print(pp(shape_lib_expanded(Name)=LenOS)))),
  mapgroup(show_shape,GallerySOS),
  %shape_lib_rules(Name,Rules),length(Rules,LenRules),pp(shape_lib_rules(LenRules)=Rules),
  mort(ignore((shapelib_opts(Name,Opts), length(Opts,LenOpts), LenOpts > 0, pp(shapelib_opts(LenOpts)=Opts)))),!,
  ignore((fail,GalleryS\==[], mapgroup(show_shape,GalleryS))))).

clear_shape_lib(Name):- 
  findall(_,(clause(in_shape_lib(Name,_),true,Ref),erase(Ref)),L),
  length(L,Len),(Len > 0 -> pp(clear_shape_lib(Name)=Len) ; true),
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
  mapgroup(label_rules(Name),Gallery),
  sortshapes(Gallery,GalleryS).

label_rules(_Complex,(Obj:- Body)):- 
   debug_var('Shape',Obj),
   Rule=(Obj:- Body),
   guess_pretty(Rule).

shapelib_opts(Name,Opts):- findall(Opt,is_shapelib_opt(Name,Opt),Opts).


% ===========================================================
% Stretches a grid to double its size2D
:- decl_gf(double_size).
% ===========================================================

double_size(Grid,Double):- any_xform(grid_double_size,Grid,Double).

grid_double_size(Grid,Double):- h_and_v(double_rows,Grid,Double).

half_size(I,O):- grid_size(I,H,V), H>1,V>1, 0 is H rem 2, 0 is V rem 2,  X is H div 2, Y is V div 2, make_grid(X,Y,HS),
  any_xform(grid_double_size,HS,DS),!,O=HS,I=DS.

double_rows([],[]):-!.
double_rows([D|DRows90],[D,D|DRows90D]):- double_rows(DRows90,DRows90D).

% ===========================================================
% Stretches a grid to increase its size2D property
:- decl_gf(increase_size(size_int)).
% ===========================================================
increase_size(N,Grid,Double):-
  h_and_v(increase_rows(N),Grid,Double).

increase_rows(_,[],[]):-!.
increase_rows(N,[D|DRows90],O):- make_list_inited(N,D,DD),increase_rows(N,DRows90,DRows90D),gappend(DD,DRows90D,O).

% ===========================================================
% Stretches a grid border to increase its size2D
:- decl_gf(increase_border(size_int)).
% ===========================================================
increase_border(N,Grid,Double):-
  h_and_v(increase_top_and_bottem_rows(N),Grid,Double).

increase_top_and_bottem_rows(_,[],[]):-!.
increase_top_and_bottem_rows(N,Grid,O):-
  append([Top|Middle],[Bottom],Grid),
  increase_rows(N,[Top],NewTop),
  increase_rows(N,[Bottom],NewBottom),
  append([NewTop,Middle,NewBottom],O).
% ===========================================================

is_shapelib_opt(as_is, = ).
apply_shapelib_xforms(Name,GalleryS,SmallLib):-  shapelib_opts(Name,Opts),expand_shape_directives(GalleryS,Opts,SmallLib).
apply_shapelib_xforms(_Name,Gallery,Gallery):- !.

% default expansion
expand_shape_directives(Shapes,None,SmallLib):- None==[],!,
 print_collapsed(100,
  show_workflow(Shapes,
   [ =, %"Vanila indivs",
     %into_grid,
    % searchable,"Searchable indivs", 
       all_orientations, % "All rotations of indivs", 
       %add(double_size),

      % Need one the the three bellow
       %decolorize, %"Add blue indivs", 
       %add(change_color), % "Add new colors_cc indivs",		 
       %into_any_color,

       smallest_first, "smallest first",
    %decolorize % decolorized points are not yet printable 
    =],SmallLib)).

expand_shape_directives(A,Opts,B):- show_workflow(A,Opts,B),!.

:- dynamic(is_shapelib_opt/2).

%in_shape_lib(Name):- the_hammer1(RedComplex),all_rotations(RedComplex,Name).

in_shape_lib(All,GRot):- All==all,!,in_shape_lib(_,GRot).
in_shape_lib(decolorize(S),Gallery):- nonvar(S),!, in_shape_lib(S,GalleryC),decolorize(GalleryC,Gallery).
in_shape_lib(all_rots(S),Gallery):-  nonvar(S),!, in_shape_lib(S,GalleryC),all_rotations(GalleryC,Gallery).
in_shape_lib(l_shape,LibObj):- l_shape(LibObj).

thirty_down_2(HW):- between(2,30,WH),HW is 32-WH.
thirty_down_1(HW):- between(2,30,WH),HW is 32-WH.

in_shape_lib(squares,LibObj):- thirty_down_2(HW), decl_one_fg_color(C),solid_square(C,HW,Grid),into_lib_object([filltype(solid),square,shape_lib(common)],Grid,LibObj).
in_shape_lib(squares,LibObj):- thirty_down_2(HW), decl_one_fg_color(C),hollow_square(C,HW,Grid),into_lib_object([hollow,outline(1),square,shape_lib(common)],Grid,LibObj).
in_shape_lib(n_shape,LibObj):- n_shape(LibObj).

in_shape_lib(hammer,Name):- the_hammer1(Name).
in_shape_lib(seen,O):- get_current_test(TestID),g_2_o(TestID,_,O), points_rep(local,O,LP),LP\==[],length(LP,L),L>4.

all_rots(X,Y):- all_rotations(X,Y).

in_grid_shape_lib([Shape,hollow],Grid,GrowthChart):- 
  decl_fill_color(BG),
  learned_color_inner_shape(Shape,Color,BG,Grid,GrowthChart),
  decl_one_fg_color(Color).
in_grid_shape_lib([Shape,filled],Grid,GrowthChart):- 
   learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart),
  decl_one_fg_color(Color),
  decl_fill_color(Fill),
  dif(Color,Fill).
in_grid_shape_lib([Shape,solid],Grid,GrowthChart):- 
   learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart),
  decl_one_fg_color(Color),
  Fill = Color.

:- include(kaggle_arc_footer).


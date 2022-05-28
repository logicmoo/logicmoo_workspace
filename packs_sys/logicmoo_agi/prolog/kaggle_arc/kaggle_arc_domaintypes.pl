

filter_indivs(In,Filter,Out):- include(matches_filter(Filter),In,Out).

matches_filter(not(A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter(\+ (A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter((A;B),OBJ):- !, (matches_filter(A,OBJ);matches_filter(B,OBJ)).
matches_filter([H|T],obj(List)):- !, \+ \+ forall(member(E,[H|T]),member(E,List)).
matches_filter((A,B),OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter(E,obj(List)):- member(E,List).




allow_dirs([Type|_],X):- !, allow_dirs(Type,X).
allow_dirs(Type,X):- subtypes(Type,SType),allow_dir(SType,List),member(X,List).

allow_dir(hv_line(h),[e,w]). allow_dir(hv_line(v),[n,s]). allow_dir(dg_line(u),[ne,sw]). allow_dir(dg_line(d),[nw,se]).


allow_dir(squares,[n,s,e,w]). 
allow_dir(diamonds,[nw,sw,se,ne]).
allow_dir(polygs,[n,s,e,w,nw,sw,se,ne]).
allow_dir(all,   [nw,sw,se,ne,n,w,s,e]).
%circles, dots, , rays, walls

shape_filter(X,squares):- free_cell(X).
shape_filter(X,polygs):- non_free_fg(X).

polyg(border(square),[hv_line(H),hv_line(V),hv_line(H),hv_line(V)]):- h_v(H,V).
polyg(border(diamond),[dg_line(H),dg_line(V),dg_line(H),dg_line(V)]):- u_d(H,V).

h_v(h,v).
u_d(u,d).

subtypes(C,C).
subtypes(C,S):- subClassOf(S,C).
subClassOf(hv_line(D),line(D)).
subClassOf(dg_line(D),line(D)).
subClassOf(dg_line(D),line(D)).

meets_indiv_criteria(_,_).


% Color types

%is_bgc(BG):- var(BG),!,fail.
is_bgc(BG):- get_bgc(C),BG==C.

is_black_or_bg(BG):- black==BG ; is_bgc(BG).
%is_black_or_bg(0).

is_color(C):- atom(C),color_int(C,N),integer(N).

is_black(C):- C==black.
get_black(black).
%get_black(0).


is_grid_color(C):- var(C),!,fail.
% makes grid colors an integer.. 
%is_grid_color(C):- !,integer(C).
% we are using atoms currently
is_grid_color(C-_):- !, is_color(C).
is_grid_color(C):- is_color(C).

is_color_dat(C):- atomic(C),color_code(C,W),!,C==W.

is_point(P):- is_nc_point(P),!.
is_point(P):- is_cpoint(P).

is_lpoint(P):- is_point(P), \+ is_gpoint(P).

is_points_list([G|L]):- !, is_point(G),maplist(is_point,L).

enum_colors(OtherColor):- named_colors(Colors),!,member(OtherColor,Colors).

fill_color(Color,OtherColor):- enum_colors(OtherColor),Color\==OtherColor,is_color_no_bgc(OtherColor).

is_bg_indiv(O):- colors(O,[cc(C,CC)]),CC>0,is_bgc(C).


is_not_cpoint(I):- \+ is_cpoint(I).

is_not_gpoint(I):- \+ is_gpoint(I).


is_cpoint(C):- \+ compound(C),!,fail.
is_cpoint(C-P):- nonvar(C),!,is_nc_point(P).

is_nc_point(P):- atom(P), hv_point(H,_,P),!,number(H).

is_gpoint(G):- var(G),!,fail.
is_gpoint(_-G):-!,is_gpoint(G).
is_gpoint(G):- hv_point(H,_,G),!,nonvar(H).

% Grid-oids

is_list_of_gridoids([G|V]):- \+ is_grid([G|V]), is_gridoid(G), is_list(V), maplist(is_gridoid,V).

is_gridoid(G):- var(G),!, fail.
is_gridoid(G):- is_grid(G),!.
is_gridoid(G):- is_object(G),!.
%is_gridoid(G):- is_cpoint(G),!.
is_gridoid(G):- is_list_of_gridoids(G).


is_grid([[C|H]|R]):- notrace((is_grid_cell(C),is_list(H),is_list(R),
  length([C|H],L),
  %maplist(is_grid_cell,H),
  maplist(is_row_len(L),R))).

%is_object(H):- is_list(H),maplist(is_cpoint,H).
is_grid_cell(C):- \+ is_list(C), (var(C); is_color(C) ; ( C =  _-_)),!.


is_object(O):- compound(O), O = obj(Props), is_list(Props).

%is_group([G|V]):- is_object(G),is_list(V),maplist(is_object,V).
is_group([G|V]):- is_object(G),is_list(V),maplist(is_object,V).

is_point_obj(O,Color,Point):- nonvar(O),O= Color-Point,!.
is_point_obj(O,Color,Point):- is_object(O),visual_hv(O,H,V), !, hv(H,V)==hv(1,1),
  globalpoints(O,[Color-Point]),!.




:- nb_delete(grid_bgc).
set_bgc(C):- atom(C),color_code(C,N),C\==N,!,set_bgc(N).
set_bgc(C):- var(C),nb_delete(grid_bgc).
set_bgc(C):- nb_setval(grid_bgc,C),!.
get_bgco(X):- nb_current(grid_bgc,X),is_color_dat(X),!.
:- set_bgc(black).

get_bgc(X):- get_bgco(X),!.
get_bgc(X):- is_black(X).


is_color_no_bgc(X):- \+ is_bgc(X), is_color(X).

is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.


free_cell(Var):- var(Var),!.
free_cell(C):- get_bgco(X),C==X.
%free_cell(0).
%free_cell(8).

%trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(_,G,G).*/

enum_rotation(same).
enum_rotation(rot90).
enum_rotation(rot180).
enum_rotation(rot270).
enum_rotation(flipV).
enum_rotation(flipH).

  
ap(scotch_patterns). ap(rug_patterns). ap(rougue_like). ap(space_invaders).
ap(shapes_on_black). ap(lines_on_black). ap(answer_keys). ap(repeating_codes).

ap(color_changes).
ap(holes).

ap(spins).  ap(contained). ap(sticky). ap(immobile). ap(mobile). ap(gravity).
ap(thick0). ap(thick1). ap(thick2). ap(thick3). 
ap(dashed).  ap(two_color). ap(multi_color). ap(monochrome).
ap(underneath). ap(painted_surface).
ap(movement_group). ap(controls_others).
ap(holds_dots).  ap(filler).  ap(blank). 

ap(changes).
ap(diagonal_line). ap(horizontal_line). ap(vertical_line). ap(open_edge). ap(container).  ap(ray).

ap(rotated45). ap(resizes). ap(diamond).
apv(square(len)). apv(round(h,w)). apv(triangle). apv(rectangular(h,w)). apv(polygon(sides)).
apv(shape(num)).  apv(facing(dir)). apv(min(n)). apv(max(n)).  apv(visual_hv(h,w)). apv(loc_xy(h,w)). 
apv(scale(n)).  apv(ext_key(k)). apv(io_bud(k)). apv(linked_bud(k)).

apv(points_old([])).
apv(sub_points([])).




l_shape(circle,"
 o=o !
o...o!
|.,.|!
o...o!
 o=o !").

l_shape(round,"
        ooo     
      o.....o   
     o.......o  
    o.........o 
    o....,....o 
    o.........o 
     o.......o  
      o.....o   
        ooo     
                ").

l_shape(hollow,"
oo=oo!
o...o!
|.,.|!
o...o!
oo=oo!").

l_shape(diamond,"
  o  !
 /.\\ !
o.,.o!
 \\./ !
  o  !").


l_shape(heart,"
 o o !
o.o.o!
o.,.o!
 \\./ !
  o  !").

l_shape(spade,"
 o== !
o.,.o!
o...o!
 \\./ !
  o  !").


l_shape(right_triangle,"
    o!
   /|!
  o.o!
 o.,o!
o=ooo!").


l_shape(building,"
oo==o!
o...|!
,...o!
|...|!
o==oo!").


l_shape(triangle,"
   o   !
  o.o  !
 /.,.\\ !
o=ooo=o!").


color_and_rotation(RedHammer,Hammer):-
  all_colors(RedHammer,Hammer1),
  all_rotations(Hammer1,Hammer).

all_colors(RedHammer,Hammer):- change_color(RedHammer,Hammer).
all_colors(RedHammer,RedHammer).

change_color(RedHammer,Hammer):- 
   color(RedHammer,CurrentColor),
   fill_color(CurrentColor,OtherColor),
   swap_colors(CurrentColor,OtherColor,RedHammer,Hammer).

all_rotations(RedHammer,Hammer):- 
  enum_rotation(ROT),
  call(ROT,RedHammer,Hammer).

the_hammer(RedHammer):- 
  RedHammer = obj([mass(6), shape([point_01_01, point_01_02, point_01_03, point_02_01, point_02_02, point_03_02]), 
  colors([cc(red, 6.0)]), localpoints([red-point_01_01, red-point_01_02, red-point_01_03, red-point_02_01, 
  red-point_02_02, red-point_03_02]), visual_hv(3, 3), rotation(same), loc_xy(2, 5), 
  changes([]), object_shape(rectangluar), object_shape(polygon), object_indv_id(t('1b60fb0c')*(trn+666)*out, 666), 
  globalpoints([red-point_02_05, red-point_02_06, red-point_02_07, red-point_03_05, red-point_03_06, red-point_04_06]), 
  grid_size(10, 10)]).


in_shape_lib(LibObj):- 
  in_grid_shape_lib(Shape,Grid,GrowthChart),
  grid_size(Grid,H,V),
  enum_scale(Scale),
  atomic_list_concat([Shape,H,V,Scale],'_',ID),
  scale_grid(Scale,GrowthChart,Grid,ScaledGrid),
  localpoints(ScaledGrid,Points),
  make_indiv_object(ID,H,V,[object_shape(Shape)|Points],LibObj).
in_shape_lib(Hammer):- the_hammer(RedHammer),color_and_rotation(RedHammer,Hammer).
  
in_grid_shape_lib(Shape,Grid,GrowthChart):- enum_colors(Color), fill_color(Color,Fill), 
   learned_color_inner_shape(Shape,Color,Fill,Grid,GrowthChart).

%scale_grid(Scale,GrowthChart,Grid,ScaledGrid)
scale_grid(1,_GrowthChart,Grid,Grid).


enum_scale(1).

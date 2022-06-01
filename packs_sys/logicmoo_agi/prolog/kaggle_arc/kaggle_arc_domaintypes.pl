

filter_indivs(In,Filter,Out):- include(matches_filter(Filter),In,Out).

matches_filter(not(A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter(\+ (A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter((A;B),OBJ):- !, (matches_filter(A,OBJ);matches_filter(B,OBJ)).
matches_filter([H|T],obj(List)):- !, \+ \+ forall(member(E,[H|T]),member(E,List)).
matches_filter((A,B),OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter(E,obj(List)):- member(E,List).




allow_dirs([Type|_],X):- !, allow_dirs(Type,X).
allow_dirs(Type,X):- subtypes(Type,SType),allow_dir(SType,List),member(X,List).

subtypes(C,C).
subtypes(C,S):- subClassOf(S,C).

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

iz(X,Y):- nonvar_or_ci(Y)->(subClassOf(P,Y),iz(X,P));(nonvar_or_ci(X),iz(X,P),subClassOf(P,Y)).
iz(X,Y):- object_shape(X,Y).

:- dynamic(iz/2).
subClassOf(outline,hollow).
subClassOf(outline,thick1).
subClassOf(outline,rectangle).
subClassOf(outline,noexit).

subClassOf(hv_line(D),line(D)).
subClassOf(dg_line(D),line(D)).
subClassOf(dg_line(D),line(D)).

subClassOf(square,symmetric).
subClassOf(diamond,symmetric).
subClassOf(h_symmetric,symmetric).
subClassOf(circle,symmetric).
subClassOf(triangle,h_symmetric).
subClassOf(round,symmetric).


meets_indiv_criteria(_,_).


% Color types

%is_bgc(BG):- plain_var(BG),!,fail.
is_bgc(BG):- get_bgc(C),BG==C.

is_black_or_bg(BG):- black==BG ; is_bgc(BG).
%is_black_or_bg(0).

is_color(C):- atom(C),color_int(C,N),integer(N).

is_colorish(C):- is_color(C),!.
is_colorish(C):- has_color_c(C,_),!.
is_colorish(C):- get_bgc(BG),BG==C,!.
is_colorish(C):- bg_sym(BG),BG==C,!.
is_colorish(C):- fg_sym(FG),FG==C,!.

is_black(C):- C==black.
get_black(black).
%get_black(0).


is_grid_color(C):- plain_var(C),!,fail.
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
enum_fg_colors(Color):- enum_colors(Color),is_color_no_bgc(Color).
fill_color(Color,OtherColor):- enum_colors(OtherColor),Color\==OtherColor,is_color_no_bgc(OtherColor).

is_bg_indiv(O):- colors(O,[cc(C,CC)]),CC>0,is_bgc(C).


is_not_cpoint(I):- \+ is_cpoint(I).

is_not_gpoint(I):- \+ is_gpoint(I).


is_cpoint(C):- \+ compound(C),!,fail.
%is_cpoint(C-P):- (nonvar_or_ci(C);has_color_c(C)),!,is_nc_point(P).
is_cpoint(_-P):- is_nc_point(P).

is_nc_point(P):- atom(P), hv_point(H,_,P),!,number(H).

is_gpoint(G):- plain_var(G),!,fail.
is_gpoint(_-G):-!,is_gpoint(G).
is_gpoint(G):- hv_point(H,_,G),!,nonvar_or_ci(H).

% Grid-oids

is_list_of_gridoids([G|V]):- \+ is_grid([G|V]), is_gridoid(G), is_list(V), maplist(is_gridoid,V).

is_gridoid(G):- plain_var(G),!, fail.
is_gridoid(G):- is_grid(G),!.
is_gridoid(G):- is_object(G),!.
%is_gridoid(G):- is_cpoint(G),!.
is_gridoid(G):- is_list_of_gridoids(G).


is_grid([[C|H]|R]):- notrace((is_grid_cell(C),is_list(H),is_list(R),
  length([C|H],L),
  %maplist(is_grid_cell,H),
  maplist(is_row_len(L),R))).

%is_object(H):- is_list(H),maplist(is_cpoint,H).
is_grid_cell(C):- \+ is_list(C), nop((plain_var(C); is_color(C) ; ( C =  _-_))),!.


is_object(O):- compound(O), O = obj(Props), is_list(Props).

%is_group([G|V]):- is_object(G),is_list(V),maplist(is_object,V).
is_group([G|V]):- is_object(G),is_list(V),maplist(is_object,V).

is_point_obj(O,Color,Point):- nonvar_or_ci(O),O= Color-Point,!.
is_point_obj(O,Color,Point):- is_object(O),visual_hv(O,H,V), !, hv(H,V)==hv(1,1),
  globalpoints(O,[Color-Point]),!.




:- nb_delete(grid_bgc).
set_bgc(C):- atom(C),color_code(C,N),C\==N,!,set_bgc(N).
set_bgc(C):- plain_var(C),nb_delete(grid_bgc).
set_bgc(C):- nb_setval(grid_bgc,C),!.
get_bgco(X):- nb_current(grid_bgc,X),X\==[],is_color_dat(X),!.
:- set_bgc(black).

get_bgc(X):- get_bgco(X),!.
get_bgc(X):- get_black(X).


is_color_no_bgc(X):- \+ is_bgc(X), is_color(X).

is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.


free_cell(Var):- plain_var(Var),!.
free_cell(C):- get_bgco(X),C==X.

non_free_fg(C):- \+ free_cell(C), \+ is_bgc(C).

%free_cell(0).
%free_cell(8).

%trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(_,G,G).*/

non_h_rot(same).
non_h_rot(rot90).
non_h_rot(flipV).
non_h_rot(rot270).

enum_rotation(same).
enum_rotation(flipV).
enum_rotation(rot180). % = flipHV
enum_rotation(rot90).
enum_rotation(rot270).
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



color_and_rotation(RedHammer,Hammer):-
  all_colors(RedHammer,Hammer1),
  all_rotations(Hammer1,Hammer).

all_colors(RedHammer,Hammer):- change_color(RedHammer,Hammer).
all_colors(RedHammer,RedHammer).


change_color(RedHammer,Hammer):- 
   color(RedHammer,CurrentColor),
   fill_color(CurrentColor,OtherColor),
  swap_colors(CurrentColor,OtherColor,RedHammer,Hammer).


all_rotations(RedHammer,Hammer):- no_repeats(Grid,(shape_rotations(RedHammer,Hammer),object_grid(Hammer,Grid))).

shape_rotations(Shape,Shape):- iz(Shape,symmetric),!.
shape_rotations(Shape,Hammer):- iz(Shape,h_symmetric),!, non_h_rot(Rot),call(Rot,Shape,Hammer).
shape_rotations(RedHammer,Hammer):- enum_rotation(ROT), call(ROT,RedHammer,Hammer).



bg_to_fresh_vars(BGrid,Grid):- map_pred(bg_to_fresh_vars_e,BGrid,Grid).
bg_to_fresh_vars_e(X,Y):-bg_sym(BG), X==BG, Y= _.

use_growth_chart(Pixels,GC,NewPixels):- globalpoints(GC,GP), do_gp(GP,Pixels,NewPixels).
 
  
%learned_color_inner_shape(Shape,Color,Color,P,GC),globalpoints(GC,GP),use_growth_chart(P,GC,PO).


%scale_grid(Scale,GrowthChart,Grid,ScaledGrid)
scale_grid(1,_GrowthChart,Grid,Grid).


enum_scale(1).

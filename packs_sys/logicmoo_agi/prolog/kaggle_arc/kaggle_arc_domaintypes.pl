/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

filter_indivs(In,Filter,Out):- include(matches_filter(Filter),In,Out).

matches_filter(not(A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter(\+ (A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter((A;B),OBJ):- !, (matches_filter(A,OBJ);matches_filter(B,OBJ)).
matches_filter([H|T],obj(List)):- !, \+ \+ forall(member(E,[H|T]),member(E,List)).
matches_filter((A,B),OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter(E,obj(List)):- member(EE,List),matches_prop(E,EE).

matches_prop(E,EE):- (var(E);var(EE)),!,E==EE.
matches_prop(E,EE):- E=EE,!.
matches_prop(iz(E),EE):-!,matches_prop(E,EE).
matches_prop(E,iz(EE)):-!,matches_prop(E,EE).

  
pass_thru_group(G):- var(G),!.
pass_thru_group([]).
pass_thru_group([options(_)]).

override_group(P):- P=..[F,M,R], override_group_call(F,M,[],R).
override_group(P):- P=..[F,A,M,R],  override_group_call(F,M,[A],R).
override_group(P):- P=..[F,A,B,M,R], override_group_call(F,M,[A,B],R).

override_group_call(_F,Group,_AB,R):- pass_thru_group(Group),!,R=Group.
override_group_call(F,Group,AB,R):- is_object_group(Group),!, C=..[F|AB],
 findall(R,(member(M,Group),call(C,M,R)),AllRots), append_sets([AllRots],R).


allowed_dir(Type,X):- subtypes(Type,SType),allow_dir_list(SType,List),member(X,List).
allowed_dir([Type|_],X):- !, nonvar(Type), allowed_dir(Type,X).

subtypes(C,C).
subtypes(C,S):- subClassOf(S,C).



allow_dir_list(nsew,[n,s,e,w]). 
%allow_dir_list(rectangles,[s,e]). 

allow_dir_list(colormass,[n,s,e,w,nw,ne,se,sw]). 
allow_dir_list(diamonds,[nw,sw,se,ne]).
allow_dir_list(colormass,[n,s,e,w,nw,sw,se,ne]).
allow_dir_list(all,   [nw,sw,se,ne,n,w,s,e]).
allow_dir_list(hv_line(h),[e,w]).
allow_dir_list(hv_line(v),[n,s]).
allow_dir_list(dg_line(u),[sw,ne]).
allow_dir_list(dg_line(d),[nw,se]).

inv_points_corner(square,diamonds).
inv_points_corner(rectangle,diamonds).
inv_points_corner(diamonds,square).
inv_points_corner(outline,none).
inv_points_corner(all,none).

points_corner_dir(Shape,Dir):- \+ inv_points_corner(Shape,_), allowed_dir(Shape,Dir).
points_corner_dir(Shape,Dir):- inv_points_corner(Shape,OShape), allowed_dir(OShape,Dir).

shape_type_dirs(ST,DIRS):- allow_dir_list(ST,DIRS).
shape_type_dir(ST,DIR):- allowed_dir(ST,DIR).
%allow_dir_list(hv_line(h),[e,w]). allow_dir_list(hv_line(v),[n,s]). 
%allow_dir_list(dg_line(u),[ne,sw]). allow_dir_list(dg_line(d),[nw,se]).

%circles, dots, , rays, walls

shape_filter(X,rectangle):- free_cell(X).
shape_filter(X,colormass):- non_free_fg(X).

polyg(border(square), [hv_line(H),hv_line(V),hv_line(H),hv_line(V)]):- h_v(H,V).
polyg(border(diamond),[dg_line(U),dg_line(D),dg_line(U),dg_line(D)]):- u_d(U,D).

h_v(h,v).
u_d(u,d).

sameOrSubClass(Y,Y).
sameOrSubClass(X,Y):- subClassOf(X,Y).

:- dynamic(iz/2).
iz(X,_):- is_grid(X),!,fail.
iz(X,Y):- nonvar(X),var(Y),!,(isz(X,XY),sameOrSubClass(XY,Y),deterministic(YN)), (YN==true->!;true).
iz(X,Y):- nonvar_or_ci(Y)->(subClassOf(P,Y),iz(X,P));(nonvar_or_ci(X),iz(X,P),subClassOf(P,Y)).
iz(X,Y):- nonvar(X),(isz(X,XY),sameOrSubClass(XY,Y),deterministic(YN)), (YN==true->!;true).
iz(X,Y):- (var(X)->enum_object(X);true),isz(X,Y).

subClassOf(noexit,outline(_)).
subClassOf(hollow,outline(_)).
%subClassOf(outl,hollow).
%subClassOf(spaceship,outl).
%subClassOf(outl,spaceship).
subClassOf(thick1,outline(_)).
%subClassOf(outl,rectangle).

subClassOf(hv_line(_),line).
subClassOf(dg_line(_),line).

subClassOf(dot,hv_symmetric).
subClassOf(square,hv_symmetric).
subClassOf(diamond,hv_symmetric).
subClassOf(circle,hv_symmetric).
subClassOf(round,hv_symmetric).
subClassOf(symmetry(sym_hv),hv_symmetric).
subClassOf(hv_symmetric,h_symmetric).
subClassOf(hv_symmetric,v_symmetric).


subClassOf(triangle,h_symmetric).
subClassOf(hv_line(v),h_symmetric).
subClassOf([monochrome,contiguous,hv_line(v)],v_symmetric).

subClassOf(hv_line(h),v_symmetric).
subClassOf([monochrome,contiguous,hv_line(h)],h_symmetric).

meets_indiv_criteria(_,_).

% =============================
% Color types
% =============================
is_fg_color(C):- is_bg_color(C),!,fail.
is_fg_color(C):- attvar(C),!,get_attr(C,ci,fg(_)).
is_fg_color(C):- is_color(C),!.
%is_fg_color(C):- C == fg.

%is_bg_color(BG):- plain_var(BG),!,fail.
is_bg_color(BG):- var(BG),!,get_attr(BG,ci,bg),!.
is_bg_color(C):- bg_sym(BG),C==BG,!.
is_bg_color(C):- get_bgc(BG),C==BG,!.

is_black_or_bg(BG):- is_black(BG)-> true; is_bg_color(BG).
%is_black_or_bg(0).
is_black(C):- C==black.
get_black(black).
%get_black(0).


data_type(O,T):- nonvar(T),data_type(O,C),T=@=C,!.
data_type(O,plain_var):- plain_var(O),!.
data_type([],length(_)=0):-!.
data_type(O,int):- integer(O),!.
data_type(O,float):- float(O),!.
data_type(O,rational):- rational(O),!.
data_type(O,string):- string(O),!.
data_type(_=O,ratio):- (rational(O);number(O)),!.
data_type(O,object):- is_object(O),!.
data_type(O,dict(L)):- is_map(O),get_kov(objs,O,Value),!,data_type(Value,L).
data_type(O,group(N)):- is_group(O),into_list(O,L),!,length(L,N).
data_type(O,cpoint):- is_cpoint(O),!.
data_type(O,nc_point):- is_nc_point(O),!.
data_type(O,bg_color):- is_spec_bg_color(O,_),!.
data_type(O,fg_color):- is_spec_fg_color(O,_),!.
data_type(O,unknown_bg_color):- is_bg_color(O),!.
data_type(O,unknown_fg_color):- is_fg_color(O),!.
data_type(O,unknown_color):- is_colorish(O),!.
data_type(Out,S):- \+ compound(Out),!,Out = S.
data_type(Out,S):- compound_name_arity(Out,print_grid,A),arg(A,Out,P),data_type(P,S),!.
data_type(Out,size(H,V)):- is_grid(Out),!,grid_size(Out,H,V).
data_type(Out,length(DT)=H):- is_list(Out),!,length(Out,H), last(Out,Last),data_type(Last,DT).
data_type(Out,F/A):- compound_name_arity(Out,F,A),!.


is_spec_bg_color(C,C):- is_bg_color(C).

is_spec_fg_color(C0,C):- attvar(C0),!,get_attr(C0,ci,fg(_)), C=C0.
is_spec_fg_color(C0,C):- \+ is_bg_color(C0), is_fg_color(C0),!,C=C0.

is_spec_color(C0,C):- (is_spec_fg_color(C0,C);is_spec_bg_color(C0,C)),!.

is_color(C):- attvar(C),!,get_attr(C,ci,_).
is_color(C):- atom(C),color_int(C,N),integer(N).

%is_colorish(C):- attvar(C),!,get_attr(C,ci,_).
is_colorish(C):- is_color(C),!.
is_colorish(C):- cant_be_color(C,_),!.
is_colorish(C):- get_bgc(BG),BG==C,!.
is_colorish(C):- bg_sym(BG),BG==C,!.
is_colorish(C):- fg_sym(FG),FG==C,!.
is_colorish(C):- compound_var(C,_),!.
%is_colorish(C):- compound(C),!,arg(1,C,A),nonvar(A),is_colorish(A).

is_grid_color(C):- plain_var(C),!,fail.
% makes grid colors an integer.. 
%is_grid_color(C):- !,integer(C).
% we are using atoms currently
is_grid_color(C-_):- !, is_color(C).
is_grid_color(C):- is_color(C).

is_color_dat(C):- atomic(C),color_code(C,W),!,C==W.

is_point(P):- var(P),!,fail.
is_point(P):- is_nc_point(P),!.
is_point(P):- is_cpoint(P).

is_points_list(P):- var(P),!,fail.
is_points_list([G|L]):- is_point(G),!,maplist(is_point,L).

enum_colors(OtherColor):- named_colors(Colors),!,member(OtherColor,Colors).
enum_fg_colors(Color):- enum_colors(Color),is_color_no_bgc(Color).
fill_color(Color,OtherColor):- enum_colors(OtherColor),Color\==OtherColor,is_color_no_bgc(OtherColor).

is_bg_indiv(O):- colors(O,[cc(C,CC)]),CC>0,is_bg_color(C).


is_not_cpoint(I):- \+ is_cpoint(I).

is_not_gpoint(I):- \+ is_gpoint(I).


is_cpoint(C):- \+ compound(C),!,fail.
%is_cpoint(C-P):- (nonvar_or_ci(C);cant_be_color(C)),!,is_nc_point(P).
is_cpoint(_-P):- is_nc_point(P).

is_list_of(P1,List):- is_list(List),maplist(P1,List).

is_nc_point(P):- nonvar(P),hv_point(_,_,P).

is_gpoint(G):- plain_var(G),!,fail.
is_gpoint(_-G):-!,is_gpoint(G).
is_gpoint(G):- hv_point(H,_,G),!,nonvar_or_ci(H).

% Grid-oids
is_list_of_gridoids([G|V]):- \+ is_grid([G|V]), is_gridoid(G), is_list(V), maplist(is_gridoid,V).

is_gridoid(G):- plain_var(G),!, fail.
is_gridoid(G):- is_grid(G),!.
is_gridoid(G):- is_object(G),!.
is_gridoid(G):- is_points_list(G),!.
is_gridoid(G):- is_list_of_gridoids(G).

is_printable_gridoid(G):- plain_var(G),!, fail.
is_printable_gridoid(G):- is_gridoid(G),!.
is_printable_gridoid(G):- is_point(G),!.
is_printable_gridoid(G):- is_cpoint(G),!.
is_printable_gridoid(D):- is_map(D),get_kov(grid,D,_).
is_printable_gridoid(G):- is_list(G),!,maplist(is_printable_gridoid,G).
is_printable_gridoid(G):- resolve_reference(G,R),!,nonvar(R),!.
is_printable_gridoid(G):- known_gridoid(G,R),!,nonvar(R),!.

vm_grid(VM,VM.grid).
vm_obj(VM,O):- member(O,VM.objs).

is_grid(G):- nonvar(G), \+ \+  quietly(fast_is_grid(G)).

fast_is_grid([[C|H]|R]):- is_list(H), is_list(R), \+ is_list(C), !, is_grid_cell(C).

slow_is_grid([[C|H]|R]):- notrace((is_grid_cell(C),is_list(H),is_list(R),
  length([C|H],L),
  %maplist(is_grid_cell,H),
  maplist(is_row_len(L),R))).

%is_object(H):- is_list(H),maplist(is_cpoint,H).
is_grid_cell(C):- var(C),!.
is_grid_cell(C):- is_colorish(C),!.
is_grid_cell(C):- atomic(C),!.
is_grid_cell(_-_):-!.
is_grid_cell(att(_,_)).
is_grid_cell(cell(_)).

h_symmetric(Obj):- is_object(Obj),!,object_grid(Obj,Grid),!,h_symmetric(Grid).
h_symmetric(Grid):- is_grid(Grid),!, mirror_h(I,_C,Grid),grid_size(Grid,H,_V), I is floor(H/2).
h_symmetric(Group):- into_grid(Group,Grid),!,h_symmetric(Grid).

is_object(O):- compound(O), O = obj(Props), is_list(Props).

%is_object_group([G|V]):- is_object(G),is_list(V),maplist(is_object,V).
%is_group(Dict):- is_map(Dict),!,get_kov(objs,Dict,_).
is_group([G|V]):- is_object_group([G|V]). % is_object_or_grid(G),is_list(V),maplist(is_object_or_grid,V),!.

is_functor(F,E):- compound(E),functor(E,F,_).
is_object_group(V):- is_list(V),maplist(is_functor(obj),V),!.
is_grid_group([G|V]):- is_grid(G),is_list(V),maplist(is_grid,V),!.

is_object_or_grid(Grid):- is_list(Grid),!,is_grid(Grid).
is_object_or_grid(Obj):- is_object(Obj).


is_point_obj(O,Color,Point):- nonvar_or_ci(O),O= Color-Point,!.
is_point_obj(O,Color,Point):- is_object(O),v_hv(O,H,V), !, hv(H,V)==hv(1,1),
  globalpoints(O,[Color-Point]),!.




:- nb_delete(grid_bgc).
set_bgc(C):- atom(C),color_code(C,N),C\==N,!,set_bgc(N).
set_bgc(C):- plain_var(C),nb_delete(grid_bgc).
set_bgc(C):- nb_setval(grid_bgc,C),!.
get_bgco(X):- nb_current(grid_bgc,X),X\==[],is_color_dat(X),!.
:- set_bgc(black).

get_bgc(X):- get_bgco(X),!.
get_bgc(X):- get_black(X).


is_color_no_bgc(X):- \+ is_bg_color(X), is_color(X).

is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.


free_cell(Var):- plain_var(Var),!.
free_cell(C):- get_bgco(X),C==X.

non_free_fg(C):- \+ free_cell(C), \+ is_bg_color(C).

%free_cell(0).
%free_cell(8).

%trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(BG,GridR,GridO):- my_append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(_,G,G).*/

non_h_rot(same).
non_h_rot(rot90).
non_h_rot(rot270).

enum_rotation(same).
enum_rotation(rot90).
enum_rotation(rot180). % = flipHV
enum_rotation(rot270).

non_h_ori(same).
non_h_ori(rot90).
non_h_ori(flipV).
non_h_ori(rot270).

non_diag_ori(same).
non_diag_ori(flipV).

non_v_ori(same).
non_v_ori(rot90).
non_v_ori(flipH).
non_v_ori(rot270).

enum_orientation(same).
enum_orientation(flipV).
enum_orientation(rot180). % = flipHV
enum_orientation(rot90).
enum_orientation(rot270).
enum_orientation(flipH).

  
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
apv(shape(num)).  apv(facing(dir)). apv(min(n)). apv(max(n)).  apv(v_hv(h,w)). apv(loc(h,w)). 
apv(scale(n)).  apv(ext_key(k)). apv(io_bud(k)). apv(linked_bud(k)).

apv(points_old([])).
apv(sub_points([])).



color_and_rotation(Group,List):- override_group(color_and_rotation(Group,List)),!.
color_and_rotation(Hammer0,Hammer):-
  all_rotations(Hammer0,Hammer1),
     all_colors(Hammer1,Hammer).

all_colors(Group,List):- override_group(all_colors(Group,List)),!.
all_colors(RedHammer,Hammer):- change_color(RedHammer,Hammer).
all_colors(RedHammer,RedHammer).

change_color_blue(Group,List):- change_color_to(blue,Group,List).

change_color_to(Blue,Group,List):- override_group(change_color_to(Blue,Group,List)),!.
change_color_to(Blue,RedHammer,BlueHammer):- 
  color(RedHammer,CurrentColor),
  swap_colors(Blue,CurrentColor,RedHammer,BlueHammer).


change_color(Group,List):- override_group(change_color(Group,List)),!.
change_color(RedHammer,Hammer):- 
   color(RedHammer,CurrentColor),
   fill_color(CurrentColor,OtherColor),
  swap_colors(CurrentColor,OtherColor,RedHammer,Hammer).

all_rotations(Group,List):- override_group(all_rotations(Group,List)),!.
all_rotations(RedHammer,Hammer):- 
 (var(RedHammer) -> freeze(RedHammer,all_rotations(RedHammer,Hammer)) 
   ; no_repeats(Grid,(shape_rotations(RedHammer,Hammer),object_grid(Hammer,Grid)))).

shape_rotations(Shape,Shape):- iz(Shape,hv_symmetric),!.
shape_rotations(Shape,Hammer):- iz(Shape,h_symmetric),!, non_h_rot(Rot),call(Rot,Shape,Hammer).
shape_rotations(RedHammer,Hammer):- enum_rotation(ROT), call(ROT,RedHammer,Hammer).


all_orientations(Group,List):- override_group(all_orientations(Group,List)),!.
all_orientations(RedHammer,Hammer):- 
 (var(RedHammer) -> freeze(RedHammer,all_orientations(RedHammer,Hammer)) 
   ; no_repeats(Grid,(shape_orientations(RedHammer,Hammer),object_grid(Hammer,Grid)))).

shape_orientations(Shape,Shape):- iz(Shape,leftover_as_one),!.
shape_orientations(Shape,Shape):- iz(Shape,hv_symmetric),!.
shape_orientations(Shape,Line):- iz(Shape,diag_symmetric),!, non_diag_ori(Rot),call(Rot,Shape,Line).
shape_orientations(Shape,Line):- iz(Shape,h_symmetric),!, non_h_ori(Rot),call(Rot,Shape,Line).
shape_orientations(Shape,Line):- iz(Shape,v_symmetric),!, non_v_ori(Rot),call(Rot,Shape,Line).
shape_orientations(RedHammer,Hammer):- enum_orientation(ROT), call(ROT,RedHammer,Hammer).


bg_to_fresh_vars(BGrid,Grid):- map_pred(bg_to_fresh_vars_e,BGrid,Grid).
bg_to_fresh_vars_e(X,Y):- bg_sym(BG), X==BG, Y= _.

use_growth_chart(Pixels,GC,NewPixels):- globalpoints(GC,GP), do_gp(GP,Pixels,NewPixels).
 
  
%learned_color_inner_shape(Shape,Color,Color,P,GC),globalpoints(GC,GP),use_growth_chart(P,GC,PO).


%scale_grid(Scale,GrowthChart,Grid,ScaledGrid)
scale_grid(1,_GrowthChart,Grid,Grid).


enum_scale(1).

:- fixup_exports.


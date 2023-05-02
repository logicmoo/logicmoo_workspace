/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).


area(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

vis_area(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

area_or_len(Obj,Area):- is_points_list(Obj),!,length_safe(Obj,Area).
area_or_len(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

density(Obj,Density):- area(Obj,Area),mass(Obj,Mass), Density is Mass/Area.


inter_object(Relation,I,O):- two_physical_objs(I,O), is_decl_pt(inter_object_relation,Relation),call(Relation,I,O).

into_gridoid0(obj(N),O):- enum_object(O),o2g(O,G),sformat(N," ~w ",[G]).
into_gridoid0(shape_lib(N:Lib),O):- shape_lib_expanded(Lib,Grids),nth1(N,Grids,O).
into_gridoid0(N,G):- get_current_test(TestID),is_why_grouped(TestID,_,N,UG), UG\==[],UG\=[_],smallest_first(UG,G).
into_gridoid0(N,G):- into_obj(N,G).
into_gridoid0(N,G):- into_grids(N,G).

into_gridoid(N,G):- no_repeats(S,(into_gridoid0(N,G),once(localpoints(G,P)),sort_safe(P,S))).
%into_gridoid(N,G):- into_gridoid0(N,G).    
   

%grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
%grav_rot(Shape,Final):- into_grid(Shape,Grid),grav_mass(Grid,RotG),!,grid_call(RotG,Shape,Final).


test_grav_rot:- test_p2(test_grav_rot(_)).
test_grav_rot(RotG,Shape,Rotated):- grav_rot(Shape,RotG,Rotated). %,undo_effect(RotG,Rotated,Back),assertion(Shape==Back).

f_o(rot90). % rot90(X,Y).
f_o(rot180). % rot180(X,Y).
f_o(rot270). %  rot270(X,Y).
f_o(flipD). %  flipD(X,Y).
f_o(flipH). %  flipH(X,Y).
f_o(flipV). %  flipV(X,Y).
f_o(flipDHV). % flipDHV(X,Y).
f_o(rollD). %  rollD(X,Y).
f_o(rollDR). %  rollDR(X,Y).

flip_Once(rot90,X,Y):- rot90(X,Y).
flip_Once(rot180,X,Y):- rot180(X,Y).
flip_Once(rot270,X,Y):-  rot270(X,Y).
flip_Once(rollD,X,Y):-  rollD(X,Y).
%flip_Once(rollDR,X,Y):-  rollDR(X,Y).
flip_Once(flipD,X,Y):-  flipD(X,Y).
flip_Once(flipH,X,Y):-  flipH(X,Y).
flip_Once(flipV,X,Y):-  flipV(X,Y).
flip_Once(flipDHV,X,Y):- flipDHV(X,Y).
%flip_Once(flipDH,X,Y):-  flipDH(X,Y).
%flip_Once(flipDV,X,Y):-  flipDV(X,Y).

flip_Many(Rot,X,Y):- flip_Once(Rot,X,Y),X\=@=Y.
flip_Many(sameR,X,X).

neighbor_counts(_,_,[],_,[]):-!.
neighbor_counts(H,V,[C-P1|Ps],Points,[(N-C)-P1|Ps2]):-
  must_det_ll((
  neighbor_count1(C,P1,Points,N),
  neighbor_counts(H,V,Ps,Points,Ps2))).

neighbor_count1(C,_P1,_Points,N):- is_bg_color(C),!,N=0.
neighbor_count1(C,_P1,_Points,N):- var(C),!,N=200.
neighbor_count1(C,P1,Points,N):- 
 findall(Dir,(n_s_e_w(Dir),once((is_adjacent_point_m2(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD)))),DirsC),
 findall(Dir,(is_diag(Dir),once((is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD)))),DirsD),
 findall(Dir,(is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),dif_color(C,CD)),DirsE),
 length(DirsE,E1),
 length(DirsC,C1),
 length(DirsD,D1), N is E1+C1*2+D1*2.

neighbor_counts(Grid,GridO):-
 must_det_ll((
  globalpoints_maybe_bg(Grid,Points),
  grid_size(Grid,H,V),
  neighbor_counts(H,V,Points,Points,CountedPointsO),
  points_to_grid(H,V,CountedPointsO,GridO))).

%grid_mass_ints(Grid,[[1]]):- Grid=@=[[_]],!. 
grid_mass_ints(Grid,GridIII):-
  unique_fg_colors(Grid,Sorted),
  %arc_test_property(Prop),
  mapgrid(normal_w(Sorted),Grid,GridIII),!. 
  /*
  %Prop = unique_colors(lst(vals([[cyan,yellow,orange,green,red,blue,black],[orange,cyan,yellow,green,black,red,blue]]),len(7),diff([orange,cyan,yellow,green,black,red,blue]=@=[cyan,yellow,orange,green,red,blue,black])))
  dmiles  include(is_fg_color,CC,FG),
  (FG==[]->SORT=[fg,black,bg];SORT=FG),!,
  %((unique_fg_colors(Grid,FG)->FG\==[])->true;FG=[black]),
  mapgrid(normal_w(SORT),Grid,GridIII),!.
*/
%normal_w(_,CC,N):- plain_var(CC),N is -2,!.
%normal_w(L,CC,N):- nth1(N,L,C),C==CC,!.
normal_w(_ColorSort,Cell,Cell):- integer(Cell),!.
normal_w(_ColorSort,Cell,-4):- Cell==bg,!.
normal_w(_ColorSort,Cell,-3):- Cell==wbg.
normal_w(_ColorSort,Cell,-2):- var(Cell),is_bg_color(Cell),!.
normal_w(_ColorSort,Cell,-1):- is_bg_color(Cell),!.
normal_w( ColorSort,Cell,Nth):- nth1(Nth,[_,_,_|ColorSort],Color),Cell==Color,!.
normal_w(_ColorSort,Cell,0):- plain_var(Cell),!.
normal_w(_ColorSort,Cell,1):- is_fg_color(Cell).
normal_w(_ColorSort,Cell,1):- Cell==fg,!.
normal_w(_ColorSort,Cell,2):- Cell==wfg,!.
normal_w(_ColorSort,Cell,3):- var(Cell),is_fg_color(Cell),!.
%normal_w(ColorSort,Cell,N):- color_int(Cell,N),!.
normal_w(_ColorSort,_,0).


rav_rot_test([[_,_,green,_],[red,_,green,yellow],[red,blue,blue,_],[red,_,_,yellow]]).
rav_rot_test([[black,black,green,black],[red,black,green,yellow],[red,blue,blue,black],[red,black,black,yellow]]).
rav_rot_test([[red,red,red,red,red,red,_,_],[red,red,red,red,red,red,_,_],[_,_,blue,blue,_,_,_,_],[_,_,blue,blue,_,_,_,_],[_,_,blue,blue,green,green,green,green],[_,_,blue,blue,green,green,green,green],[yellow,yellow,_,_,yellow,yellow,_,_],[yellow,yellow,_,_,yellow,yellow,_,_]]).
rav_rot_test([[red,red,red,red,red,red,black,black],[red,red,red,red,red,red,black,black],[black,black,blue,blue,black,black,black,black],[black,black,blue,blue,black,black,black,black],[black,black,blue,blue,green,green,green,green],[black,black,blue,blue,green,green,green,green],[yellow,yellow,black,black,yellow,yellow,black,black],[yellow,yellow,black,black,yellow,yellow,black,black]]).
rav_rot_test([[cyan,cyan,cyan,cyan,cyan,cyan,_,_],[cyan,cyan,cyan,cyan,cyan,cyan,_,_],[_,_,cyan,cyan,_,_,_,_],[_,_,cyan,cyan,_,_,_,_],[_,_,cyan,cyan,cyan,cyan,cyan,cyan],[_,_,cyan,cyan,cyan,cyan,cyan,cyan],[cyan,cyan,_,_,cyan,cyan,_,_],[cyan,cyan,_,_,cyan,cyan,_,_]]).
rav_rot_test([[cyan,cyan,cyan,cyan,cyan,cyan,black,black],[cyan,cyan,cyan,cyan,cyan,cyan,black,black],[black,black,cyan,cyan,black,black,black,black],[black,black,cyan,cyan,black,black,black,black],[black,black,cyan,cyan,cyan,cyan,cyan,cyan],[black,black,cyan,cyan,cyan,cyan,cyan,cyan],[cyan,cyan,black,black,cyan,cyan,black,black],[cyan,cyan,black,black,cyan,cyan,black,black]]).
rav_rot_test([[black,black,black,black,black,black,_,_],[black,black,black,black,black,black,_,_],[_,_,black,black,_,_,_,_],[_,_,black,black,_,_,_,_],[_,_,black,black,black,black,black,black],[_,_,black,black,black,black,black,black],[black,black,_,_,black,black,_,_],[black,black,_,_,black,black,_,_]]).
%rav_rot_test(X):- never_this(X,Y),rav_rot_test(Y),into_mono(Y,Z),X=Y.
rav_rot_test(X):- clause(rav_rot_test(Y),true),into_monochrome2(Y,Z),X=Z.

/*
show_grav_rot(X):-
 must_det_ll((
  into_grid(X,Y),!,
  into_solid_grid(Y,YY),!,
  into_monorot(YY,YYY),!,
  ignore(( 
   grav_rot(YYY,R1,Y1), reduce_grid(Y1,Z,A), grav_rot(A,R2,AA), 
    wots(S,writeq(show_grav_rot(R1,Z,R2))), print_ss(S,YY,AA),  
     ignore((grid_call([R1,R2],Y,YR), wots(S2,writeq(show_grav_rot(R1,R2))), print_ss(S2,Y,YR))),
  \+ \+ ignore((call(R1,YY,ZZ),call(R2,YY,TT), print_ss(R1-R2,ZZ,TT))),
  
  !)))).
*/

into_monorot(X,Y):- mapgrid(fg_is_fg_is_bg,X,Y), \+ avoid_mono(Y),!.
into_monorot(X,X). 


avoid_mono(Y):- flipSym(rot90,Y),!.
avoid_mono(Y):- flipSym(flipH,Y),flipSym(flipV,Y),!.


fg_is_fg_is_bg(X,bg):- \+ is_fg_color(X),!.
fg_is_fg_is_bg(_,fg).
  

/*
arc_test_property(Prop):-
 get_current_test(TestID),first_current_example_num(trn+N)
 arc_test_property(TestID, gh(N), comp(cbg(black), i-o, _), Prop).

*/
grav_rot(Grid,RotG,Rotated):- %must_be_free(RotG),  
  must_det_ll((
   into_grid(Grid,GridI),   
   into_monorot(GridI,GridII),
   dif(RotG,rollD),
   best_grav_rot_grid(GridII,RotG,_),
   grid_call(RotG,GridI,Rotated))).

/*best_grav_rot(Shape,RotG,Rotated):- must_be_free(Rotated),
    must_det_ll((
    cast_to_grid(Shape,Grid,Uncast),
    best_grav_rot_grid(Grid,RotG,Final),
    uncast(Shape,Uncast,Final,Rotated))).
*/

best_grav_rot_grid(Grid,sameR,Grid):- \+ \+ Grid=[[_]],!.
best_grav_rot_grid(GridI,RotG,RotatedG):- must_be_free(Rotated), 
 must_det_ll(( is_grid(GridI), 
    %trim_outside(GridI,Grid),
    =(GridI,Grid),
    grid_mass_ints(Grid,GridInts),
    w(W,RotG)=Template,
    findall(Template,(flip_Many(RotG,GridInts,Rotated),rot_mass(Rotated,W)),Pos),
    sort_safe(Pos,LPos),last(LPos,Template),call(RotG,Grid,RotatedG))).

length_safe(L,N):- do_my_check_args(length(L,N)),length(L,N).

do_my_check_args(P):- forall(my_check_args(P),true).
my_check_args(length(_,N)):- nonvar(N),my_assertion(integer(N)).
my_check_args(length(L,_)):- nonvar(L),my_assertion(is_list(L)).

rot_mass(Grid,Mass):- 
 into_grid(Grid,LP0), !,
 LP = [C|_],
 must_det_ll(( grid_mass_ints(LP0,LP),
    length_safe(C,Len),map_row_size(10,Len,LP,Mass))).

rot_mass(Grid,OMass):- into_grid(Grid,LP0), 
 LP = [C|_],
 must_det_ll(( 
  grid_mass_ints(LP0,LP),
  length_safe(C,Len),map_row_size(10,Len,LP,Mass),
   (is_top_heavy(LP)->Bonus is -1; Bonus is 1),
   (is_left_heavy(LP)->Bonus2 is 16 ;Bonus2 is -16),
   OMass is Mass*Bonus*Bonus2)).

map_row_size(_,Len,[],Len):-!.
map_row_size(N,Len,[Row|Rest],Mass):- is_list(Row),!, 
   length_safe(Row,Len2), map_row_size(10,Len2,Row,RMass),   
   N2 is N * Len,
   map_row_size(N2,Len,Rest,RRMass),
   Mass is RRMass+(RMass*N).
map_row_size(N,Len,[Mult|Rest],Mass):- number(Mult),!, 
   N2 is N * Len, map_row_size(N2,Len,Rest,RMass),
   Mass is Mult*N + RMass.
map_row_size(N,Len,[_|Rest],Mass):- 
   N2 is N * Len, map_row_size(N2,Len,Rest,Mass),!.


is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),!,rot_mass(Top,TopM),rot_mass(Bottem,BottemM),!,BottemM>TopM.
is_left_heavy(Grid0):- rot270(Grid0,Grid),!,is_top_heavy(Grid).
split_50_v(Grid,Top,Bottem):- length_safe(Grid,N),H is floor(N/2), length_safe(Top,H),length_safe(Bottem,H),
    append(Top,Rest,Grid),append(_Mid,Bottem,Rest).

grav_mass(Grid,sameR):- iz(Grid,hv_symmetric),!.
grav_mass(Grid,RotOut):- vis2D(Grid,H,V), !, tips_to_rot(Grid,H,V,RotOut,_).

% make things bottem heavy
tips_to_rot(Grid,H,V,[rot270|RotOut],Final):- H<V, !, rot90(Grid,Grid90),!,atrace,tips_to_rot(Grid90,V,H,RotOut,Final).
tips_to_rot(Grid,H,V,[rot90|RotOut],Final):- is_top_heavy(Grid), !, rot270(Grid,Grid90), !, tips_to_rot(Grid90,V,H,RotOut,Final).
%tips_to_rot(Grid,H,V,[rot180|RotOut]):- is_top_heavy(Grid), !, rot180(Grid,Grid90), !, tips_to_rot(Grid90,H,V,RotOut).
tips_to_rot(Grid,_H,_V,RotOut,Final):- is_left_heavy(Grid)-> (RotOut=[rot180],rot180(Grid,Final)); (RotOut=[sameR],Final=Grid).


ensure_dir(s). ensure_dir(e). ensure_dir(w). ensure_dir(n).
ensure_int(1). ensure_int(2).
gravity:- test_p2(gravity(4,w)).
gravity(N,D,G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,gravity(N,D,Grid,GridNew).
gravity(N,D,G,GR):- var(D),!,ensure_dir(D),gravity(N,D,G,GR).
gravity(N,D,G,GR):- var(N),!,ensure_int(N),gravity(N,D,G,GR).
gravity(1,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridNew),!.
gravity(N,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridM),(Grid\=@=GridM->(Nm1 is N-1,gravity(Nm1,n,GridM,GridNew));GridNew=GridM).
gravity(N,s,Grid,GridNew):-!,flipV(Grid,FlipV),gravity(N,n,FlipV,GridM),flipV(GridM,GridNew).
gravity(N,w,Grid,GridNew):-!,c_r(gravity(N,n),Grid,GridNew).
gravity(N,e,Grid,GridNew):-!,h_as_rv(gravity(N,n),Grid,GridNew).

gravity_1_n_0([],[]).
gravity_1_n_0([Row1,Row2|Grid],GridNew):- nth1(Col,Row1,E1),nth1(Col,Row2,E2),
  get_bgc(E1), \+ get_bgc(E2),
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,E1,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).

:- decl_pt(any_xform(p2,prefer_grid,prefer_grid)).

any_xform(Rot90,Any,NewAny):- var(Any),nonvar(NewAny),undo_p2(Rot90,Rot270),!, any_xform(Rot270,NewAny,Any).
any_xform(Rot90,Any,NewAny):- 
  cast_to_grid(Any,RealGrid,UnconvertClosure),!,
  grid_xform(Rot90,RealGrid,NewRealGrid),
  uncast(Any,UnconvertClosure,NewRealGrid,NewAnyWUpdate),
  record_object_change(Rot90,NewAnyWUpdate,NewAny).




:- dynamic(xform_cache/5).
:- retractall(xform_cache(_,_,_,_,_)).
xform_cache(rot45,5,5,[ [ A, B, C, D, E],
        [ F, G, H, I, J],
        [ K, L, M, N, O],
        [ P, Q, R, S, T],
        [ U, V, W, X, Y]],[ [ C, D, E, J, O],
                            [ B, H, I, N, T],
                            [ A, G, M, S, Y],
                            [ F, L, Q, R, X],
                            [ K, P, U, V, W]]).

grid_xform(Rot90,Grid,NewAnyWUpdate):- 
  grid_size(Grid,H,V),
  apply_transformer(Rot90,H,V,Grid,NewAnyWUpdate).
apply_transformer(Name,H,V,G,O):-
  get_spatial_xformer(Name,H,V,In,Out),!,
  G=In,O=Out.

%get_spatial_xformer(_Name,1,1,In,In):- !.
get_spatial_xformer(Name,H,V,In,Out):- xform_cache(Name,H,V,In,Out),!.
get_spatial_xformer(Name,H,V,In,Out):- 
   make_grid(H,V,In),
   call(Name,In,Out),!,
   asserta(xform_cache(Name,H,V,In,Out)),!.

grid_same(X,X).
same_grid(X1,X2):- into_grid(X1,G1),into_grid(X2,G2),sameR(G1,G2).
sameR(X,X).

test_rot:- test_p2(rot270),test_p2(rot90).
%srot90V,flipV
%rot90(A,B):- A==[],!,B=[].
%rot90(I,O):- grid_rot90(I,O).
rot90(I,O):-any_xform(grid_rot90,I,O).
rot180(I,O):- any_xform(grid_rot180,I,O).
rot270(I,O):- any_xform(grid_rot270,I,O).
flipH(I,O):- any_xform(grid_flipH,I,O).
flipV(I,O):- any_xform(grid_flipV,I,O).
flipD(I,O):- any_xform(grid_flipD,I,O).
% rot90? 
flipDV(I,O):- any_xform(grid_flipDV,I,O).
flipDH(I,O):- any_xform(grid_flipDH,I,O).
flipDHV(I,O):- any_xform(grid_flipDHV,I,O).
rollD(I,O):- any_xform(grid_rollD,I,O).
rollDR(I,O):- any_xform(grid_rollDR,I,O).
nsew_edges(I,O):- any_xform(grid_edges_fresh,I,O).

grid_edges_fresh(Find,Edges):- must_det_ll((
  [T|_]=Find,append(_,[B],Find),
  rot90(Find,Find90),
  [L|_]=Find90,append(_,[R],Find90), 
  reverse(B,RB),reverse(R,RR),
  Edges=[T,RB,RR,L])),!.


/*
:- meta_predicate(begin_load_hook(:)).
begin_load_hook(P1):- asserta((user:term_expansion(I,_):- call(P1,I),fail)).
:- meta_predicate(end_load_hook(:)).
end_load_hook(P1):- retract((user:term_expansion(I,_):- call(P1,I),fail)).


learn_head((P:-_)):- compound(P), compound_name_arity(P,F,2),atom_concat('grid_',P2,F), \+ current_predicate(P2/2), 
  P22 =..[P2,A,B], asserta_if_new(P22:-  any_xform(F,A,B)).

:- begin_load_hook(learn_head).
*/

grid_rollD(Grid,RollD):-  do_rollD(Grid,RollD),!.
grid_rollDR(Grid,RollD):-  do_rollDR(Grid,RollD),!.
%grid_rot90(Grid,Rot90):- transpoze(Grid,Transposed),maplist(reverse,Transposed,Rot90).
grid_rot90(Grid,Rot90):- reverse(Grid,GridR),transpoze(GridR,Rot90).
grid_rot180(Grid,Rot180):- grid_flipV(Grid,FlipV),grid_flipH(FlipV,Rot180).
grid_rot270(Grid,Rot270):- transpoze(Grid,Transposed),reverse(Transposed,Rot270).
grid_flipH(Grid,FlipH):- maplist(reverse,Grid,FlipH).
grid_flipV(Grid,FlipV):- reverse(Grid,FlipV).

grid_flipDV(Grid,FlipHV):-flipD(Grid,FlipH),flipV(FlipH,FlipHV),!.
grid_flipDH(Grid,FlipHV):-flipD(Grid,FlipH),flipH(FlipH,FlipHV),!.
grid_flipDHV(Grid,FlipHV):-flipD(Grid,FlipH),rot180(FlipH,FlipHV),!.

grid_flipD(I,O):- grid_size(I,H,V),make_grid(V,H,O),
  forall(between(1,H,X),
    forall(between(1,V,Y),
      (get_color_at(X,Y,I,C),
       nb_set_local_point(Y,X,C,O)))).

%:- end_load_hook(learn_head).

do_rollD(Grid,Shifted):-
 maplist_n(0,rot_row,Grid,Shifted).

rot_row(0,Row,Row).
rot_row(N,Row,Rot):- rot1(Row,Roll),N2 is N-1,rot_row(N2,Roll,Rot).
rot1(Row,Roll):- append([E],Rest,Row),append(Rest,[E],Roll).

do_rollDR(Grid,Shifted):-
 maplist_n(0,rot_rowR,Grid,Shifted).
rot_rowR(0,Row,Row).
rot_rowR(N,Row,Rot):- rot1r(Row,Roll),N2 is N-1,rot_rowR(N2,Roll,Rot).
rot1r(Row,Roll):- append(Rest,[E],Row),append([E],Rest,Roll).

undo_effect(UnRot,X,Y):- undo_p2(UnRot,Rot),!,must_grid_call(Rot,X,Y).

undo_p2r(rot90,rot270). 
undo_p2r(rollD,rollDR). 
undo_p2r(double_size,half_size).

undo_p2_lr(X,Y):- undo_p2r(X,Y).
undo_p2_lr(X,Y):- undo_p2r(Y,X).

undo_p2(X,Y):- undo_p2_lr(X,Y).
undo_p2(X,X).
%:- f_o(X), \+ undo_p2_lr(X,_). %:- rot_p2(X). undo_p2(sameR,sameR).


nav(s,0,1). nav(e, 1,0). nav(w,-1,0). nav(n,0,-1).
nav(se, 1,1). nav(sw,-1,1). nav(nw,-1,-1). nav(ne, 1,-1).
nav(c,0,0).


n_s_e_w(n).
n_s_e_w(s).
n_s_e_w(e).
n_s_e_w(w).


move_dir(N,OX,OY,Dir,SX,SY,NX,NY):- nav(Dir,X,Y), NX is OX + (X*SX*N), NY is OY + (Y*SY*N).

reverse_nav(D,R):- nav(D,X,Y),RX is -X, RY is -Y,nav(R,RX,RY).

is_non_diag(X):-n_s_e_w(X). 
%is_non_diag(X):- nav(X,0,_);nav(X,_,0).
%is_diag(D):- nav(D,X,Y),X\==0,Y\==0. % \+ is_non_diag(X).
is_diag(ne). is_diag(sw). is_diag(se). is_diag(nw).


rot_right_90(N,E):- rot_left_90(E,N).
rot_left_90(N,W):- rot_left_45(N,NW), rot_left_45(NW,W).

rot_right_135(N,SE):- rot_left_135(SE,N). 
rot_left_135(N,SW):- rot_left_45(N,NW), rot_left_45(NW,W), rot_left_45(W,SW).

rot_right_45(X,Y):-rot_left_45(Y,X).
rot_left_45(s,sw). rot_left_45(sw,w). rot_left_45(w,nw). rot_left_45(nw,n). 
rot_left_45(n,ne). rot_left_45(ne,e). rot_left_45(e,se). rot_left_45(se,s).

w_in_45(N,N).
w_in_45(N,NE):- rot_right_45(N,NE).
w_in_45(N,NW):- rot_left_45(N,NW).

w_in_90(N,Other):- w_in_45(N,Other).
w_in_90(N,E):- rot_right_90(E,N).
w_in_90(N,W):- rot_left_90(W,N).


after_dir_check(N,N,[]):- member(N,[e,se,s,w,sw,n,nw,ne]).
after_dir_check(N,NW,Check):- member(N,[se,sw,nw,ne]),after_dir_check_diag(N,NW,Check).
after_dir_check(N,NW,Check):- member(N,[e,s,w,n]),after_dir_check_go_nsew(N,NW,Check).

after_dir_check_go_nsew(N,E,NE):- rot_right_90(N,E),rot_right_45(N,NE).
after_dir_check_go_nsew(N,NE,N):- rot_right_45(N,NE).
after_dir_check_go_nsew(N,W,NW):- rot_left_90(N,W),rot_left_45(N,NW).
after_dir_check_go_nsew(N,NW,N):- rot_left_45(N,NW).
after_dir_check_go_nsew(N,SE,E):- rot_right_135(N,SE), rot_right_90(N,E).
after_dir_check_go_nsew(N,SW,W):- rot_left_135(N,SW), rot_left_90(N,W).

after_dir_check_diag(NE,E,NE):- rot_right_45(NE,E).
after_dir_check_diag(NE,SE,E):- rot_right_90(NE,SE),rot_right_45(NE,E).
after_dir_check_diag(NE,N,NE):- rot_left_45(NE,N).
after_dir_check_diag(NE,NW,W):- rot_left_90(NE,NW),rot_left_45(NE,W).
after_dir_check_diag(NE,S,E):- rot_right_135(NE,S), rot_right_90(NE,E).
after_dir_check_diag(NE,W,NW):- rot_left_135(NE,W), rot_left_90(NE,NW).

facing_triangles(
[[xx,se,nw,xx],
 [se,se,nw,nw],
 [ne,ne,sw,sw],
 [xx,ne,sw,xx]]).

surround_3x3(
[[nw,n,ne],
  [w,c,e],
 [sw,s,se]]).
%star_grow_h(obj(L),


move_above_itself(I,M):- move_dir_itself(1,n,I,M). 

move_rightof_itself(I,M):- move_dir_itself(1,e,I,M). 


:- decl_pt(move_dir_itself(int,dir,object,+)).
%move_dir_itself(N,D,I,M):- check_args(move_dir_itself(N,D,I,M),MaybeCut),(MaybeCut==t->!;true).
move_dir_itself(N,D,I,M):- is_object(I),vis2D(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_group(L),!,mapgroup(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_group(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 /*must_det_ll*/((
  loc2D(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_group(L),!,mapgroup(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_group(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 /*must_det_ll*/((
  (NY<1 -> M=I ;
  ( localpoints(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpoints(GPoints),loc2D(NX,NY)],M))))).
move_object(H,V,L,LM):- is_group(L),!,mapgroup(move_object(H,V),L,LM).
move_object(H,V,I,O):- into_group(I,M),M\=@=I,!,move_object(H,V,M,O).

is_input(VM):- VM.id = _ * _ * in.

is_input_object(Obj):- is_object(Obj),obj_to_oid(Obj,OID),atom_concat(_,'_in',OID),!.
is_output_object(Obj):- is_object(Obj), obj_to_oid(Obj,OID),atom_concat(_,'_out',OID),!.
%ignore(((NewOptions\==Options;(GoneMissing\==[];SofarMaybeNewL\==SofarL)),

%show_object_changes(_VM,_S,Goal):-!, call(Goal).

show_vm_changes(_VM,_S,Goal):- !, call(Goal),!.
show_vm_changes(VM,S,Goal):-
  show_object_changes(VM,S,show_point_changes(VM,S,show_grid_changes(VM,S,Goal))).

show_object_changes(VM,S,Goal):-
       setup_call_cleanup(duplicate_term(VM.objs,Was),
                     Goal,
        ignore((VM.objs\=@=Was,
          print_side_by_side(silver,print_grid(VM.h,VM.v,Was),objs>was:S,_,print_grid(VM.h,VM.v,VM.objs),objs>new:S)))).

show_point_changes(VM,S,Goal):-
   setup_call_cleanup(duplicate_term(VM.lo_points,Was),
      Goal,
     ignore((VM.lo_points\=@=Was,
        print_side_by_side(silver,print_grid(VM.h,VM.v,Was),points>was:S,_,print_grid(VM.h,VM.v,VM.lo_points),points>new:S)))).

show_grid_changes(VM,S,Goal):-
   setup_call_cleanup(duplicate_term(VM.grid,Was),
                     Goal,
     ignore((VM.grid\=@=Was,
        print_side_by_side(silver,print_grid(VM.h,VM.v,Was), grid>was:S,_,print_grid(VM.h,VM.v,VM.grid),grid>new:S)))).

print_side_by_side_d(C,A,AN,W,B,BN):- nop(print_side_by_side(C,A,AN,W,B,BN)).



same_surface(O1,O2):- obj_test_example_io(O1,GID),obj_test_example_io(O2,GID).
 
two_physical_objs(O1,O2):- O1\==O2, is_physical_object(O1),is_physical_object(O2), O1\=@=O2, same_surface(O1,O2).

is_physical_object(O):- var(O),!,enum_object(O),is_physical_object(O).
is_physical_object(O):- has_prop(iz(flag(not_physical_object)),O),!,fail.
is_physical_object(O):- has_prop(iz(flag(hidden)),O),!,fail.
%is_physical_object(O):- is_whole_grid(O),!,fail.
is_physical_object(_).
%is_physical_object(O):- has_prop(cc(fg,0),O),has_prop(cc(bg,0),O),!,fail.
%is_physical_object(O):- my_assertion(is_object(O)),has_prop(iz(media(shaped)),O),!.
%is_physical_object(O):- has_prop(mass(Mass),O),Mass>0.

% ==============================================
  %%find_subsumes,
  %%find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
  %%find_overlaps,
  %%find_touches,
  %%find_sees,

is_fti_step(find_relations).
% ==============================================
find_relations(VM):- 
  Objs = VM.objs,
  find_relationsA(Objs,NewObjs),
  gset(VM.objs) = NewObjs.

find_relationsA(Objs,NewObjs):-
 must_det_ll((
  include(is_physical_object,Objs,Phys),
  my_maplist(add_oinfo,Phys,PhysOInfo),
  find_relationsB(PhysOInfo,[],TodoLIST),
  flatten(TodoLIST,TodoLISTF),
  do_todo3(PhysOInfo,TodoLISTF,NewObjs))).

do_todo3([],_,[]):-!.
do_todo3([oinfo(O1,Ps1,OID1,Todo1)|Objs],TODO,NewObjs):-
 %length(Objs,L),pp(do_todo3=L),
  select(some_todo(OID1,AddToO1),TODO,NTODO),!,
  flatten([AddToO1],REALTodoF),
 % pp(override_object(REALTodoF,Obj)),
  override_object(REALTodoF,O1,O11), 
  do_todo3([oinfo(O11,Ps1,OID1,Todo1)|Objs],NTODO,NewObjs).
do_todo3([oinfo(O1,_Ps1,_OID1,_Todo1)|Objs],TODO,[O1|NewObjs]):-
  do_todo3(Objs,TODO,NewObjs).



add_oinfo(Ref,oinfo(O1,Ps1,NewRef,[])):- into_obj(Ref,O1),
  obj_to_oid(O1,OID1),
  maybe_oid_to_lhs(OID1,NewRef),
  globalpoints(O1,Ps1),!.

maybe_oid_to_lhs(OID1,NewRef):- if_defined(oid_to_lhs(OID1,NewRef),OID1=NewRef).

find_relationsB([O1|Rest],TodoIN,TodoOUT):-
  find_relations2(O1,Rest,TodoIN,TodoMID),
  find_relationsB(Rest,TodoMID,TodoOUT).
find_relationsB([],TodoINOUT,TodoINOUT).


find_relations2(O1,[O2|Rest],TodoIN,TodoOUT):-
  find_relations4(O1,O2,TodoIN,TodoMID),
  find_relations2(O1,Rest,TodoMID,TodoOUT).
find_relations2(_,[],TodoINOUT,TodoINOUT).

% ?- relations_of(Rel,O1,O2),print_ss(global_grid(O1),global_grid(O2)).
relations_of(Rel,O1,O2):- var(O1),var(O2),!,
  relations_of1(RelAB,OA,OB), 
  (RelAB = inv(Rel) ->  (OA=O1,OB=O2) ; (RelAB = Rel ,OA=O2,OB=O1)).
relations_of(Rel,O2,O1):- relations_of1(Rel,O1,O2).

% ?- into_obj(472,O1),into_obj(691,O2),print_ss(O1,O2),relations_of(Rel,O1,O2)

relations_of1(Rel,O1,O2):- two_physical_objs(O1,O2),
  once((add_oinfo(O1,INFO1),add_oinfo(O2,INFO2))),
  once((find_relations4(INFO1,INFO2,[],TodoOUT))),
  flatten(TodoOUT,List),member(some_todo(OID1,LINKS),List),
  member(RLINKS,LINKS),
  align_to_oid(OID1,INFO1,RLINKS,Rel).

align_to_oid(OID1,oinfo(_,_,OIDA,_),link(Rel,_OID2),Rel):- OIDA==OID1.
align_to_oid(OID1,oinfo(_,_,OIDA,_),link(Rel,_OID2),inv(Rel)):- OIDA\==OID1,!.
  
  
:- style_check(-singleton).
find_relations4(INFO1,INFO2,TodoIN,TodoOUT):-
 must_det_ll((
  INFO1 = oinfo(O1,Ps1,OID1,Todo1),%arg(1,INFO1,O1), arg(2,INFO1,Ps1), arg(3,INFO1,OID1), arg(4,INFO1,Todo1),  
  INFO2 = oinfo(O2,Ps2,OID2,Todo2),%arg(1,INFO2,O2), arg(2,INFO2,Ps2), arg(3,INFO2,OID2), arg(4,INFO2,Todo2),
  intersection(Ps1,Ps2,Overlap,P1L,P2L), 
  findall(link(How,OID2),related_how(How,O1,O2,Ps1,Ps2,Overlap,P1L,P2L),AddToO1),
  findall(link(How,OID1),related_how(How,O2,O1,Ps2,Ps1,Overlap,P2L,P1L),AddToO2),
  %if_t(AddToO1\==[],(append(AddToO1,Todo1,NewTodo1),nb_setarg(4,INFO1,NewTodo1),nop(pp(OID1=NewTodo1)))),
  %if_t(AddToO2\==[],(append(AddToO2,Todo2,NewTodo2),nb_setarg(4,INFO2,NewTodo2),nop(pp(OID2=NewTodo2)))),
  ((AddToO1==[],AddToO2==[]) -> TodoIN=TodoOUT ; 
  (append(TodoIN,[
    some_todo(OID1,AddToO1),
    some_todo(OID2,AddToO2)] ,TodoOUT))))).
  
related_how(How,O1,O2,Ps1,Ps2,Overlap,P1L,P2L):- 
  once((points_range(P1L,SX1,SY1,EX1,EY1,_,_), points_range(P2L,SX2,SY2,EX2,EY2,_,_))),
  related_how2(How,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2).

related_how(subsumed_by(Offset,OverlapP),O1,O2,Ps1,Ps2,Overlap,P1L,_P2L):- Overlap\==[],P1L==[],
  length(Ps1,Len1),length(Ps2,Len2), OverlapP = rational(Len1/Len2), object_offset(O2,O1,Offset),
  \+ is_bg_object(O1), \+ is_bg_object(O2).

related_how(   subsumed(Offset,OverlapP),O1,O2,Ps1,Ps2,Overlap,_P1L,P2L):- Overlap\==[],P2L==[],
  length(Ps1,Len1),length(Ps2,Len2), OverlapP = rational(Len2/Len1), object_offset(O2,O1,Offset),
  \+ is_bg_object(O1), \+ is_bg_object(O2).

sees_dir(n,D,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SY1 >= EY2, D is SY1-EY2.
sees_dir(s,D,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SY2 >= EY1, D is SY2-EY1.
sees_dir(w,D,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SX1 >= EX2, D is SX1-EX2.
sees_dir(e,D,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SX2 >= EX1, D is SX2-EX1.

related_how2(sees(DirsDists),O1,O2,Ps1,Ps2,Overlap,P1L,
  SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):-
  findall(cc(Dir,Dist),(sees_dir(Dir,Dist,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2),Dist<3),DirsDists), 
  \+ is_bg_object(O1), \+ is_bg_object(O2),
   DirsDists\==[].


%related_how(How,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SX1 < SX2, SY1 < SY2, EX2 < EX1, EY2 < EY1, How = contained_by(engulfed).
related_how2(contained_by,O1,O2,Ps1,Ps2,Overlap,P1L,
  SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- \+ is_bg_object(O2),
  SX1 >= SX2, SY1 >= SY2, EX2 >= EX1, EY2 >= EY1,
  \+ (SX1 == SX2, SY1 == SY2, EX2 == EX1, EY2 == EY1).
%related_how(How,O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- SX1 > SX2, SY1 > SY2, EX2 > EX1, EY2 > EY1, How = contains(engulfed).
related_how2(contains,O1,O2,Ps1,Ps2,Overlap,P1L,
  SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- \+ is_bg_object(O1),
  SX1 =< SX2, SY1 =< SY2, EX2 =< EX1, EY2 =< EY1,
  \+ (SX1 == SX2, SY1 == SY2, EX2 == EX1, EY2 == EY1).

related_how2(OLPred,O1,O2,Ps1,Ps2,Overlap,
  P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):- Overlap\==[],
  length(Overlap,OL), length(Ps1,Len1),length(Ps2,Len2),
  Overlap1 = rational(OL/Len1), Overlap2 = rational(OL/Len2),
  overlaps_pred(OLPred,Overlap1,Overlap2),
  \+ is_bg_object(O1), \+ is_bg_object(O2).

related_how2(touches(TouchDirs),O1,O2,Ps1,Ps2,Overlap,P1L,SX1,SY1,EX1,EY1,P2L,SX2,SY2,EX2,EY2):-
  Overlap\==[],
  \+ is_bg_object(O1), \+ is_bg_object(O2),
  findall(Dir,(member(_-P1,P1L), is_adjacent_point(P1,Dir,P2),  member(_-P2,P2L)),Dirs),
  get_ccs(Dirs,TouchDirs), TouchDirs\==[].

:- style_check(+singleton).

overlaps_pred(overlaps(Overlap1,Overlap2),Overlap1,Overlap2):- Overlap1>Overlap2,!.
overlaps_pred(overlapped_by(Overlap1,Overlap2),Overlap1,Overlap2).


object_offset(O2,O1,Offset):-
  loc2D(O1,X1,Y1), loc2D(O2,X2,Y2), X is abs(X2 - X1), Y is abs(Y2 - Y1),
  center2D(O1,X1,Y1), center2D(O2,X2,Y2), X1 is abs(X2 - X1), Y1 is abs(Y2 - Y1),
  ((X==0,Y==0) -> Offset = locOffset2D(X,Y) ; 
   ((X1==0,Y1==0) -> Offset = centralOffset2D(X1,Y1) ; Offset = locOffset2D(X,Y))).



already_relation(O1,O2):- \+ \+ already_relation(_,O1,O2).
already_relation(Link,O1,O2):- obj_to_oid(O1,OID1), obj_to_oid(O2,OID2), already_relation(Link,O1,O2,OID1,OID2).

already_relation(   P, O1,_O2,_OID1, OID2):- sub_term(P,O1),compound(P),arg(_,P,E),E==OID2,!.
already_relation(r(P),_O1, O2, OID1,_OID2):- sub_term(P,O2),compound(P),arg(_,P,E),E==OID1,!.
% ==============================================
% LINKS
% ==============================================

%better_sdir(S,Iv,[],link(S,Iv,[c])).
better_sdir(S,Iv,Dirs,link(S,Iv,Dirs)):- \+ is_list(Dirs),!.
better_sdir(S,Iv,Dirs,link(S,Iv,[-LO])):-  length_safe(Dirs,7),subtract([n,s,e,w,nw,ne,sw,se],Dirs,[LO]).
better_sdir(S,Iv,[n,s,e,w,nw,ne,sw,se],link(S,Iv,[c])).
better_sdir(S,Iv,[n,s,e,w],link(S,Iv,[c])).
better_sdir(S,Iv,[ne,se],O):- !,better_sdir(S,Iv,[e],O).
better_sdir(S,Iv,[nw,sw],O):- !,better_sdir(S,Iv,[w],O).
better_sdir(S,Iv,[nw,ne],O):- !,better_sdir(S,Iv,[n],O).
better_sdir(S,Iv,[sw,se],O):- !,better_sdir(S,Iv,[s],O).
better_sdir(S,Iv,Dirs,O):- member(sw,Dirs),subtract(Dirs,[s,w],NDirs),NDirs\==Dirs,!,better_sdir(S,Iv,NDirs,O).
better_sdir(S,Iv,Dirs,O):- member(nw,Dirs),subtract(Dirs,[n,w],NDirs),NDirs\==Dirs,!,better_sdir(S,Iv,NDirs,O).
better_sdir(S,Iv,Dirs,O):- member(se,Dirs),subtract(Dirs,[s,e],NDirs),NDirs\==Dirs,!,better_sdir(S,Iv,NDirs,O).
better_sdir(S,Iv,Dirs,O):- member(ne,Dirs),subtract(Dirs,[n,e],NDirs),NDirs\==Dirs,!,better_sdir(S,Iv,NDirs,O).
better_sdir(S,Iv,Dirs,link(S,Iv,Dirs)).

% ==============================================
% Contained
% ==============================================
% Find free points that are contained in objects and individuate them in their own way
%fti(VM,[find_contained_points|set(VM.lo_program)]):- find_contained_points(VM).
is_fti_step(find_contained_points).
%fti(VM,[colormass_subshapes|set(VM.lo_program)]):- colormass_subshapes(VM),!.
find_contained_points(VM):-
  show_vm_changes(VM,find_contained_points,find_contained_points(VM.h,VM.v,VM.id,VM.objs,set(VM.objs),VM.lo_points,set(VM.lo_points))),!.

find_contained_points(_H,_V,_ID,Sofar,Sofar,[],[]).
find_contained_points(_H,_V,_ID,[],[],NextScanPoints,NextScanPoints).
find_contained_points(H,V,ID,[Found|Sofar],[Found|SofarInsteadM],NextScanPoints,NextScanPointsInstead):-
 must(nop(isz(Found,outline(_)))),
  once(find_contained_points(Found,NextScanPoints,ScanPointsInstead,ContainedPoints)),
  ContainedPoints\==[],
  %grid_size(Found,H,V),
  /*must_det_ll*/((
  % points_to_grid(H,V,ContainedPoints,Grid),
  %once(obj_to_oid(Found,ID,_);grid_to_tid(Grid,ID)),
  individuate(subshape_in_object,ContainedPoints,NewInside),
  mapgroup(mention_inside(Found),NewInside,NewInsideM))),
  ignore((length_safe(ContainedPoints,N),N>1,quietly(print_grid(H,V,"find_contained_points",[Found|NewInsideM])))),
  find_contained_points(H,V,ID,Sofar,SofarInstead,ScanPointsInstead,NextScanPointsInstead),
  append(NewInsideM,SofarInstead,SofarInsteadM).
find_contained_points(H,V,ID,[Found|Sofar],[Found|SofarInstead],NextScanPoints,NextScanPointsInstead):-
  find_contained_points(H,V,ID,Sofar,SofarInstead,NextScanPoints,NextScanPointsInstead).


mention_inside(Found,NewInside,NewInsideO):-
  obj_to_oid(Found,Iv),
  add_shape_info(link(insideOf,Iv),NewInside,NewInsideO).

find_contained_points(_,[],[],[]).
find_contained_points(Found,[Next|ScanPoints],ScanPointsInstead,[Next|Contained]):-
 object_surrounds_point(Found,Next),
 find_contained_points(Found,ScanPoints,ScanPointsInstead,Contained).
find_contained_points(Found,[Next|ScanPoints],[Next|ScanPointsInstead],Contained):-
 find_contained_points(Found,ScanPoints,ScanPointsInstead,Contained).


object_surrounds_point(Obj,_-Point):- point_in_obj_view(Point,Obj), 
  globalpoints(Obj,ObjPoints),!,
  forall((is_adjacent_point(Point,Dir,Next),Dir\==c),scan_to_colider1(Obj,Next,Dir,ObjPoints,_DirHits)).

point_in_obj_view(Next,Obj):- 
  hv_point(H,V,Next),
  loc2D(Obj,X,Y),!,
  VV is V-Y, VV>=0,
  HH is H - X, HH>=0,
  vis2D(Obj,XX,YY),!,
  VV<YY, HH<XX.

scan_to_colider1(_Obj,Next,_Dir,_ObjPoints,[]):- hv_point(H,V,Next), (\+ between(1,32,H); \+ between(1,32,V)),!.
scan_to_colider1(_Obj,Next,_Dir,ObjPoints,[C-Next]):- 
  select(C-Next,ObjPoints,_Rest),!.
scan_to_colider1(Obj,Next,Dir,ObjPoints,DirHits):- 
  is_adjacent_point(Next,Dir,NNext),!,
  scan_to_colider1(Obj,NNext,Dir,ObjPoints,DirHits),!.

scan_to_colider(Obj,Next,_Dir,_ObjPoints,[]):- \+ point_in_obj_view(Next,Obj),!.
scan_to_colider(Obj,Next,Dir,ObjPoints,[C-Next|DirHits]):- 
  select(C-Next,ObjPoints,Rest),!,
  is_adjacent_point(Next,Dir,NNext),
  scan_to_colider(Obj,NNext,Dir,Rest,DirHits).
scan_to_colider(Obj,Next,Dir,ObjPoints,DirHits):- 
  is_adjacent_point(Next,Dir,NNext),
  scan_to_colider(Obj,NNext,Dir,ObjPoints,DirHits).

replace_i_each(OtherObjects,[I|NewInside],[O|NewInsideM],NewOtherObjects):- must_be_free(NewOtherObjects),
  subst001(OtherObjects,I,O,OtherObjectsM),!,
  replace_i_each(OtherObjectsM,NewInside,NewInsideM,NewOtherObjects).
replace_i_each(E,[],[],E):-!.

:- include(kaggle_arc_footer).


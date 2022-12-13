/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).


area(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

vis_area(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

area_or_len(Obj,Area):- is_points_list(Obj),!,length_safe(Obj,Area).
area_or_len(Obj,Area):- vis2D(Obj,H,V), Area is H * V.

density(Obj,Density):- area(Obj,Area),amass(Obj,Mass), Density is Mass/Area.


into_gridoid0(obj(N),O):- enum_object(O),o2g(O,G),sformat(N," ~w ",[G]).
into_gridoid0(shape_lib(N:Lib),O):- shape_lib_expanded(Lib,Grids),nth1(N,Grids,O).
into_gridoid0(N,G):- get_current_test(TestID),is_why_grouped(TestID,_,N,UG), UG\==[],UG\=[_],smallest_first(UG,G).
into_gridoid0(N,G):- into_obj(N,G).
into_gridoid0(N,G):- into_grids(N,G).

into_gridoid(N,G):- no_repeats(S,(into_gridoid0(N,G),once(localpoints(G,P)),sort(P,S))).
%into_gridoid(N,G):- into_gridoid0(N,G).    
   

%grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
%grav_rot(Shape,Final):- into_grid(Shape,Grid),grav_mass(Grid,RotG),!,grid_call(RotG,Shape,Final).


test_grav_rot:- test_p2(test_grav_rot(_)).
test_grav_rot(RotG,Shape,Rotated):- grav_rot(Shape,RotG,Rotated). %,unrotate(RotG,Rotated,Back),assertion(Shape==Back).

flip_Once(rot90,X,Y):- rot90(X,Y).
flip_Once(rot180,X,Y):- rot180(X,Y).
flip_Once(rot270,X,Y):-  rot270(X,Y).
flip_Once(flipD,X,Y):-  flipD(X,Y).
flip_Once(flipH,X,Y):-  flipH(X,Y).
flip_Once(flipV,X,Y):-  flipV(X,Y).
flip_Once(flipDHV,X,Y):- flipDHV(X,Y).
flip_Once(flipDH,X,Y):-  flipDH(X,Y).
flip_Once(flipDV,X,Y):-  flipDV(X,Y).

flip_Many(Rot,X,Y):- flip_Once(Rot,X,Y),X\=@=Y.
flip_Many(sameR,X,X).

normal_w(_,CC,N):- plain_var(CC),N is -2,!.
normal_w(_,CC,N):- integer(CC),N=CC,!.
normal_w(L,CC,N):- nth1(N,L,C),C==CC,!.
normal_w(_,CC,N):- color_mass_int(CC,N).
color_mass_int(Cell,-2):- plain_var(Cell),!.
color_mass_int(Cell,-20):- var(Cell),is_fg_color(Cell),!.
color_mass_int(Cell,-10):- var(Cell),is_bg_color(Cell),!.
color_mass_int(Cell,1):- is_fg_color(Cell),!.
color_mass_int(Cell,0):- is_bg_color(Cell),!.
%color_mass_int(Cell,N):- color_int(Cell,N),!.
color_mass_int(_,0).

%grid_mass_ints(Grid,[[1]]):- Grid=@=[[_]],!. 
grid_mass_ints(Grid,GridIII):- 
  unique_colors(Grid,CC),
  include(is_fg_color,CC,FG),
  mapgrid(normal_w(FG),Grid,GridIII),!.

grav_rot(Grid,sameR,Grid):- \+ \+ Grid=[[_]],!.
grav_rot(Grid,RotG,Rotated):-
  must_det_ll((into_grid(Grid,GridII),
   grid_mass_ints(GridII,GridIII),
   best_grav_rot(GridIII,RotG,_),
   call(RotG,GridII,Rotated))),!.
best_grav_rot(Shape,RotG,Rotated):- must_be_free(Rotated),  
    must_det_ll((
    cast_to_grid(Shape,Grid,Uncast),
    best_grav_rot_grid(Grid,RotG,Final),
    uncast(Shape,Uncast,Final,Rotated))),!.

best_grav_rot_grid(Grid,RotG,Rotated):- must_be_free(Rotated), 
 must_det_ll(( is_grid(Grid), 
    w(W,Rotated,RotG)=Template,
    findall(Template,(flip_Many(RotG,Grid,Rotated),rot_mass(Rotated,W)),Pos),
    sort(Pos,LPos),last(LPos,Template))).

length_safe(L,N):- do_my_check_args(length(L,N)),length(L,N).

do_my_check_args(P):- forall(my_check_args(P),true).
my_check_args(length(_,N)):- nonvar(N),my_assertion(integer(N)).
my_check_args(length(L,_)):- nonvar(L),my_assertion(is_list(L)).

rot_mass(Grid,Mass):- 
 into_grid(Grid,LP0), !,
 LP = [C|_],
 must_det_ll(( mapgrid(color_mass_int,LP0,LP),
    length_safe(C,Len),map_row_size(10,Len,LP,Mass))).

rot_mass(Grid,OMass):- into_grid(Grid,LP0), 
 LP = [C|_],
 must_det_ll(( 
  mapgrid(color_mass_int,LP0,LP),
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


grav_mass(Grid,sameR):- iz(Grid,hv_symmetric),!.
grav_mass(Grid,RotOut):- vis2D(Grid,H,V), !, tips_to_rot(Grid,H,V,RotOut,_).

% make things bottem heavy
tips_to_rot(Grid,H,V,[rot270|RotOut],Final):- H<V, !, rot90(Grid,Grid90),!,trace,tips_to_rot(Grid90,V,H,RotOut,Final).
tips_to_rot(Grid,H,V,[rot90|RotOut],Final):- is_top_heavy(Grid), !, rot270(Grid,Grid90), !, tips_to_rot(Grid90,V,H,RotOut,Final).
%tips_to_rot(Grid,H,V,[rot180|RotOut]):- is_top_heavy(Grid), !, rot180(Grid,Grid90), !, tips_to_rot(Grid90,H,V,RotOut).
tips_to_rot(Grid,_H,_V,RotOut,Final):- is_left_heavy(Grid)-> (RotOut=[rot180],rot180(Grid,Final)); (RotOut=[sameR],Final=Grid).

is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),!,rot_mass(Top,TopM),rot_mass(Bottem,BottemM),!,BottemM>TopM.
is_left_heavy(Grid0):- rot270(Grid0,Grid),!,is_top_heavy(Grid).
split_50_v(Grid,Top,Bottem):- length_safe(Grid,N),H is floor(N/2), length_safe(Top,H),length_safe(Bottem,H),
    my_append(Top,Rest,Grid),my_append(_Mid,Bottem,Rest).


ensure_dir(s). ensure_dir(e). ensure_dir(w). ensure_dir(n).
ensure_int(1). ensure_int(2).
gravity:- test_p2(gravity(4,w)).
gravity(N,D,G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,gravity(N,D,Grid,GridNew).
gravity(N,D,G,GR):- var(D),!,ensure_dir(D),gravity(N,D,G,GR).
gravity(N,D,G,GR):- var(N),!,ensure_int(N),gravity(N,D,G,GR).
gravity(1,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridNew),!.
gravity(N,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridM),(Grid\=@=GridM->(Nm1 is N-1,gravity(Nm1,n,GridM,GridNew));GridNew=GridM).
gravity(N,s,Grid,GridNew):-!,flipV(Grid,FlipV),gravity(N,n,FlipV,GridM),flipV(GridM,GridNew).
gravity(N,w,Grid,GridNew):-!,h_as_v(gravity(N,n),Grid,GridNew).
gravity(N,e,Grid,GridNew):-!,h_as_rv(gravity(N,n),Grid,GridNew).

gravity_1_n_0([],[]).
gravity_1_n_0([Row1,Row2|Grid],GridNew):- nth1(Col,Row1,E1),nth1(Col,Row2,E2),
  get_bgc(E1), \+ get_bgc(E2),
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,E1,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).

:- decl_pt(any_xform(p2,prefer_grid,prefer_grid)).

any_xform(Rot90,Any,NewAny):- var(Any),nonvar(NewAny),unrotate_p2(Rot90,Rot270),!, any_xform(Rot270,NewAny,Any).
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
flipDV(I,O):- any_xform(grid_flipDV,I,O).
flipDH(I,O):- any_xform(grid_flipDH,I,O).
flipDHV(I,O):- any_xform(grid_flipDHV,I,O).
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

grid_rot90(Grid,Rot90):-  rot270(Grid,Rot270),rot180(Rot270,Rot90).
grid_rot180(Grid,Rot180):- flipV(Grid,Rot90),flipH(Rot90,Rot180).
grid_rot270(Grid,NewAnyWUpdate):- get_colums(Grid,NewAnyWUpdate),!.
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

unrotate(UnRot,X,Y):- unrotate_p2(UnRot,Rot),!,must_grid_call(Rot,X,Y).

unrotate_p2r(rot90,rot270). unrotate_p2r(double_size,half_size).
unrotate_p2(X,Y):- unrotate_p2r(X,Y).
unrotate_p2(X,Y):- unrotate_p2r(Y,X).
unrotate_p2(X,X). %:- rot_p2(X). unrotate_p2(sameR,sameR).


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
   setup_call_cleanup(duplicate_term(VM.points,Was),
      Goal,
     ignore((VM.points\=@=Was,
        print_side_by_side(silver,print_grid(VM.h,VM.v,Was),points>was:S,_,print_grid(VM.h,VM.v,VM.points),points>new:S)))).

show_grid_changes(VM,S,Goal):-
   setup_call_cleanup(duplicate_term(VM.grid,Was),
                     Goal,
     ignore((VM.grid\=@=Was,
        print_side_by_side(silver,print_grid(VM.h,VM.v,Was),grid>was:S,_,print_grid(VM.h,VM.v,VM.grid),grid>new:S)))).

print_side_by_side_d(C,A,AN,W,B,BN):- nop(print_side_by_side(C,A,AN,W,B,BN)).


% ==============================================
% TOUCHES
% ==============================================
%ft i(VM,[find_touches|set(VM.program_i)]):- %cullObjectsOutsideOfRanges(VM), %  find_touches(How,VM).
is_fti_step(find_touches).

find_touches(VM):-
  /*must_det_ll*/((Objs = VM.objs, pred_find_links(touching_object(non_overlapping_object_dir(dir_touching)),Objs,NewObjs))),
  gset(VM.objs) = NewObjs.

touching_object(How,Dirs,O2,O1):- O1\==O2,

  %has_prop(o(Y,LC,_),O1), has_prop(o(Y,LC,_),O2),
  is_physical_object(O1), is_physical_object(O2),
  %\+ has_prop(/*b*/iz(glyphic),O2), %\+ has_prop(/*b*/iz(glyphic),O1),
  globalpoints(O1,Ps1), globalpoints(O2,Ps2),
  call(How,Ps2,Ps1,Dirs),!.

dir_touching(Ps1,Ps2,Dir):- member(_-P1,Ps1), is_adjacent_point(P1,Dir,P2),  member(_-P2,Ps2).



% ==============================================
% SEES
% ==============================================
%ft i(VM,[find_sees|set(VM.program_i)]):- %cullObjectsOutsideOfRanges(VM), %  find_sees(How,VM).
is_fti_step(find_sees).

find_sees(VM):-
  /*must_det_ll*/((Objs = VM.objs, pred_find_links(seeing_object(non_overlapping_object_dir(dir_seeing)),Objs,NewObjs))),
  gset(VM.objs) = NewObjs.

seeing_object(How,Dirs,O2,O1):- O1\==O2,
  is_physical_object(O1), is_physical_object(O2),
  %\+ has_prop(/*b*/iz(glyphic),O2), %\+ has_prop(/*b*/iz(glyphic),O1),
  globalpoints(O1,Ps1), globalpoints(O2,Ps2),
  call(How,Ps2,Ps1,Dirs),!.

dir_seeing(Ps1,Ps2,Dir):- member(_-P1,Ps1), is_adjacent_point(P1,Dir,P2), \+ member(_-P2,Ps1), 
  seeing_dir_soon(P1,Dir,Ps2).
    
seeing_dir_soon(P1,_Dir,Ps2):- member(_-P1,Ps2),!.
seeing_dir_soon(P1,Dir,Ps2):- is_adjacent_point(P1,Dir,P2), seeing_dir_soon(P2,Dir,Ps2).

is_physical_object(O):- my_assertion(is_object(O)),has_prop(iz(shape),O),!.
is_physical_object(O):- has_prop(mass(Mass),O),Mass>0.

% ==============================================
% OVERLAPS
% ==============================================
%ft i(VM,[find_overlaps|set(VM.program_i)]):- %cullObjectsOutsideOfRanges(VM), %  find_overlaps(How,VM).
is_fti_step(find_overlaps).

find_overlaps(VM):-
  /*must_det_ll*/((Objs = VM.objs, pred_find_links(overlap,Objs,NewObjs))),
  gset(VM.objs) = NewObjs.

overlap(overlaping,O2,O1):- O1\==O2,
  is_physical_object(O1), is_physical_object(O2),
  %\+ has_prop(/*b*/iz(glyphic),O2), %\+ has_prop(/*b*/iz(glyphic),O1),
  globalpoints(O1,Ps1), globalpoints(O2,Ps2),
  \+ \+ (member(P,Ps1), member(P,Ps2)),!.


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


non_overlapping_object_dir(_How,Ps1,Ps2,[overlap]):- member(P1,Ps1), \+ \+ member(P1,Ps2),!,fail.
non_overlapping_object_dir(How,Ps1,Ps2,Dirs):- findall(Dir,(member(Dir,[n,s,e,w,nw,ne,sw,se]),
  once(call(How,Ps1,Ps2,Dir))),Dirs),Dirs\==[].

pred_find_links(How,Objs,NewObjs):- 
  must_det_ll((find_links(How,Objs,Objs,NewObjs))).

link_prop(T,A):- sub_term(A,T),atom(A),!.

find_links(_How,[],_,[]):-!.
find_links(How,[Obj|ScanNext],OtherObjects,[Obj|ScanRest]):- fail,
  link_prop(How,Prop),
  has_prop(link(Prop,_,_),Obj),!,
  find_links(How,ScanNext,OtherObjects,ScanRest).

find_links(How,[Obj|ScanNext],OtherObjects,[NewObj|ScanRest]):-
  must_det_ll((
  /*must_det_ll*/(find_links_objects(How,Obj,OtherObjects,DirNewSees)),
  /*must_det_ll*/(override_object(DirNewSees,Obj,NewObj)),
  /*must_det_ll*/(find_links(How,ScanNext,OtherObjects,ScanRest)))).

/*
mention_links(Obj,[],Obj):-!.
mention_links(Obj,[link(Dir,Seen)|More],NewFObjO):- !,
  mention_links(Obj,Dir-Seen,MidObj),
  mention_links(MidObj,More,NewFObjO).
mention_links(Obj,Dir-Seen,NewObj):-
  /*must_det_ll*/(obj_to_oid(Seen,Iv)),
  /*must_det_ll*/(override_object(link(links,Dir,Iv),Obj,NewObj)),!.
*/

find_links_objects(_How,_,[],[]).
%find_links_objects(How,Obj,_,[]):- has_prop(iz(dots),Obj),!.
find_links_objects(How,Obj,[Seen|ScanNext],[BetterSee|WillSee]):-    
 once(call(How,Dirs,Obj,Seen)),Dirs\==[],!,
 link_prop(How,Prop),
 better_sdir(Prop,Iv,Dirs,BetterSee),
 /*must_det_ll*/obj_to_oid(Seen,Iv),
 /*must_det_ll*/
 find_links_objects(How,Obj,ScanNext,WillSee),!.
find_links_objects(How,Obj,[_|ScanNext],WillSee):- /*must_det_ll*/(find_links_objects(How,Obj,ScanNext,WillSee)),!.

:- dynamic(individuated_cache/3).
:- retractall(individuated_cache(_,_,_)).

% ==============================================
% ENGULFS
% ==============================================
is_fti_step(check_engulfed).
check_engulfed(VM):-
   smallest_first(VM.objs,SmallestFirst),
   set(VM.objs) = SmallestFirst,
   set(VM.program_i) = [find_engulfs|VM.program_i].

% Find object that are contained in objects and individuate them in their own way  (TODO mame this more complete)
% Find free points that are contained in objects and individuate them in their own way
%  find_engulfs(VM).   

is_fti_step(find_engulfs).
find_engulfs(VM):-
  /*must_det_ll*/((Objs = VM.objs, find_engulfs(Objs,NewObjs))),
  gset(VM.objs) = NewObjs.


find_engulfs(Objs,NewObjs):- /*must_det_ll*/((find_engulfs(Objs,Objs,NewObjs))).
find_engulfs([],_,[]):-!.
find_engulfs([Obj|ScanNext],OtherObjects,[NewObj|ScanRest]):-
  /*must_det_ll*/(find_engulfs_objects(Obj,OtherObjects,DirNewTouches)),
  /*must_det_ll*/(override_object(DirNewTouches,Obj,NewObj)),
  /*must_det_ll*/(find_engulfs(ScanNext,OtherObjects,ScanRest)).

find_engulfs_objects(_,[],[]).
%find_engulfs_objects(Obj,_,[]):- has_prop(link(insideOf,_),Obj),!.
find_engulfs_objects(Obj,_,[]):- has_prop(link(contains,_),Obj),!.
find_engulfs_objects(Obj,[Target|ScanNext],[link(insideOf,Iv)|Engulfed]):-    
 once(contained_object(Obj,Target)),!,
 /*must_det_ll*/(obj_to_oid(Target,Iv)),
 /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.
find_engulfs_objects(Obj,_,[]):- amass(Obj,Mass),Mass<5,!.
find_engulfs_objects(Obj,[Target|ScanNext],[link(contains,Iv)|Engulfed]):-    
 once(contained_object(Target,Obj)),!,
 /*must_det_ll*/(obj_to_oid(Target,Iv)),
 /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.
find_engulfs_objects(Obj,[_|ScanNext],Engulfed):- /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.


contained_object(O2,O1):-
  O1 \== O2,
  % \+ has_prop(/*b*/iz(glyphic),O2), %\+ has_prop(/*b*/iz(glyphic),O1),
  loc2D(O1,LowH1,LowV1),loc2D(O2,LowH2,LowV2), 
  LowH2 > LowH1, LowV2 > LowV1,
  vis2D(O1,H1,V1),vis2D(O2,H2,V2), 
  H1> H2, V1> V2,
  HighH1 is LowH1+H1, HighV1 is LowV1+V1,
  HighH2 is LowH2+H2, HighV2 is LowV2+V2,
  HighH1 > HighH2,
  HighV1 > HighV2,
  nop(globalpoints(O2,[Point|_])),!,
  nop(object_surrounds_point(O1,Point)).


% ==============================================
% Contained
% ==============================================
% Find free points that are contained in objects and individuate them in their own way
%fti(VM,[find_contained_points|set(VM.program_i)]):- find_contained_points(VM).
is_fti_step(find_contained_points).
%fti(VM,[colormass_subshapes|set(VM.program_i)]):- colormass_subshapes(VM),!.
find_contained_points(VM):-
  show_vm_changes(VM,find_contained_points,find_contained_points(VM.h,VM.v,VM.id,VM.objs,set(VM.objs),VM.points,set(VM.points))),!.

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
  my_append(NewInsideM,SofarInstead,SofarInsteadM).
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


/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
grav_rot(Shape,KeyR):- into_grid(Shape,Key1), grav_rot0(Key1,KeyR).

grav_rot0(Shape,ShapeO):-
  findall(GRot,all_rotations(Shape,GRot),List),
  predsort(sort_on(grav_mass),List,[ShapeO|_]).

grav_mass(Grid,Mass):- grid_size(Grid,H,V), !, grav_mass(Grid,H,V,Mass),!.
grav_mass(Grid,_,_,Grid):- iz(Grid,hv_symmetric),!.
% make things bottem heavy
grav_mass(Grid,H,V,RotG):- H<V, !, rot90(Grid,Grid90),!,bottem_heavy(Grid90,RotG).
grav_mass(Grid,_H,_V,RotG):- is_symmetric_h(Grid),!,bottem_heavy(Grid,RotG).
grav_mass(Grid,_H,_V,RotG):- bottem_heavy(Grid,A),rot90(A,B),bottem_heavy(B,RotG).

bottem_heavy(Grid,Grid180):-  (is_top_heavy(Grid)->rot180(Grid,Grid180);Grid=Grid180).
/*
grav_mass(Grid,Mass):- grid_size(Grid,H,V), HV is round(H/V), Vh is floor(V/2),
  findall(C,(between(Vh,V,Vi),between(0,H,Hi), Hi*HV > Vi, get_color_at(Hi,Vi,Grid,C),is_fg_color(C)),CList),
  length(CList,Mass).
*/
is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),mass(Top,TopM),mass(Bottem,BottemM),BottemM<TopM.

split_50_v(Grid,Top,Bottem):- length(Grid,N),H is floor(N/2), length(Top,H),length(Bottem,H),
    append(Top,Rest,Grid),append(_Mid,Bottem,Rest).


gravity(N,D,G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,gravity(N,D,Grid,GridNew).
gravity(1,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridNew).
gravity(N,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridM),(Grid\=@=GridM->(Nm1 is N-1,gravity(Nm1,n,GridM,GridNew));GridNew=GridM).
gravity(N,s,Grid,GridNew):-!,flipV(Grid,FlipV),gravity(N,n,FlipV,GridM),flipV(GridM,GridNew).
gravity(N,w,Grid,GridNew):-!,rot90(Grid,GridRot),gravity(N,n,GridRot,GridM),rot270(GridM,GridNew).
gravity(N,e,Grid,GridNew):-!,rot270(Grid,GridRot),gravity(N,n,GridRot,GridM),rot90(GridM,GridNew).

gravity_1_n_0([],[]).
gravity_1_n_0([Row1,Row2|Grid],GridNew):- nth1(Col,Row1,E1),nth1(Col,Row2,E2),
  black_cell(E1), \+ black_cell(E2),
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,E1,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).


any_xform(Rot90,Any,XNewGrid):- 
  into_grid(Any,RealGrid,UnconvertClosure),!,
  grid_xform(Rot90,RealGrid,NewGridR),call(UnconvertClosure,NewGridR,NewGrid),
  record_xform(Rot90,NewGrid,XNewGrid).


:- dynamic(xform_cache/5).

xform_cache(rot45,5,5,[ [ A, B, C, D, E],
        [ F, G, H, I, J],
        [ K, L, M, N, O],
        [ P, Q, R, S, T],
        [ U, V, W, X, Y]],[ [ C, D, E, J, O],
                            [ B, H, I, N, T],
                            [ A, G, M, S, Y],
                            [ F, L, Q, R, X],
                            [ K, P, U, V, W]]).

grid_xform(Rot90,Grid,NewGrid):- 
  grid_size(Grid,H,V),
  apply_transformer(Rot90,H,V,Grid,NewGrid).
apply_transformer(Name,H,V,G,O):-
  get_xformer(Name,H,V,In,Out),!,
  G=In,O=Out.

get_xformer(Name,H,V,In,Out):- xform_cache(Name,H,V,In,Out),!.
get_xformer(Name,H,V,In,Out):- 
   make_grid(H,V,In),
   call(Name,In,Out),!,
   asserta(xform_cache(Name,H,V,In,Out)),!.

grid_same(X,X).
same_grid(X1,X2):- into_grid(X1,G1),into_grid(X2,G2),same(G1,G2).
same(X,X).

%srot90V,flipV
rot90( Grid,NewGrid):- any_xform(grid_rot90,Grid,NewGrid).
rot180( Grid,NewGrid):- any_xform(grid_rot180,Grid,NewGrid).
rot270( Grid,NewGrid):- any_xform(grid_rot270,Grid,NewGrid).
flipH( Grid,NewGrid):- any_xform(grid_flipH,Grid,NewGrid).
flipV( Grid,NewGrid):- any_xform(grid_flipV,Grid,NewGrid).
flipHV( Grid,NewGrid):- any_xform(grid_flipHV,Grid,NewGrid).

grid_rot90(Grid,NewGrid):-  grid_flipHV(Grid,GridM),grid_rot270(GridM,NewGrid). 
grid_rot180(Grid,FlipHV):- grid_flipHV(Grid,FlipHV).
grid_rot270(Grid,NewGrid):- get_colums(Grid,NewGrid),!.
grid_flipH(Grid,FlipH):- maplist(reverse,Grid,FlipH).
grid_flipV(Grid,FlipV):- reverse(Grid,FlipV).
grid_flipHV(Grid,FlipHV):-grid_flipH(Grid,FlipH),grid_flipV(FlipH,FlipHV),!.

unrotate(rot90,rot270):-!.
unrotate(rot270,rot90):-!.
unrotate(X,X).


nav(s,0,1). nav(e, 1,0). nav(w,-1,0). nav(n,0,-1).
nav(se, 1,1). nav(sw,-1,1). nav(nw,-1,-1). nav(ne, 1,-1).
nav(c,0,0).


n_s_e_w(n).
n_s_e_w(s).
n_s_e_w(e).
n_s_e_w(w).

move_dir(N,OX,OY,Dir,SX,SY,NX,NY):- nav(Dir,X,Y), NX is OX + (X*SX*N), NY is OY + (Y*SY*N).

reverse_nav(D,R):- nav(D,X,Y),RX is -X, RY is -Y,nav(R,RX,RY).

is_non_diag(X):- nav(X,0,_);nav(X,_,0).
is_diag(D):- nav(D,X,Y),X\==0,Y\==0. % \+ is_non_diag(X).

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
move_dir_itself(N,D,I,M):- is_object(I),vis_hv(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_group(L),!,maplist(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_group(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 must_det_l((
  loc_xy(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_group(L),!,maplist(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_group(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 must_det_l((
  (NY<1 -> M=I ;
  ( localpoints(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpoints(GPoints),loc_xy(NX,NY)],M))))).
move_object(H,V,L,LM):- is_group(L),!,maplist(move_object(H,V),L,LM).
move_object(H,V,I,O):- into_group(I,M),M\=@=I,!,move_object(H,V,M,O).



find_engulfed(Image,ScanNext,SofarInsteadO):-
  find_engulfed(Image,ScanNext,ScanNext,SofarInsteadO).

is_input(Image):- Image.id = _ * _ * in.

find_engulfed(_Image,[],SofarInsteadO,SofarInsteadO).
find_engulfed(Image,[Found|ScanNext],OtherObjects,OtherObjectsO):-
 ((isz(Found,outline(_));isz(Found,outl)) ->
 (( once(find_englufed_objects(Image,Found,OtherObjects,NewInside)),
  
  NewInside\==[], 
  must_det_l((
  maplist(mention_inside(Found),NewInside,NewInsideM),
  replace_i_each(OtherObjects,NewInside,NewInsideM,NewOtherObjects),
  replace_i_each(ScanNext,NewInside,NewInsideM,NewScanNext),
  ignore((length(NewInside,N),N>0,quietly(print_grid([Found|NewInsideM])))),      
  find_engulfed(Image,NewScanNext,NewOtherObjects,OtherObjectsO)))))).

find_engulfed(Image,[_|Sofar],OtherObjects,OtherObjectsO):-
  find_engulfed(Image,Sofar,OtherObjects,OtherObjectsO).


find_englufed_objects(_Image,_,[],[]).
find_englufed_objects(Image,Found,[Next|ScanPoints],[Next|Engulfed]):-    
 contained_object(Found,Next),
 find_englufed_objects(Image,Found,ScanPoints,Engulfed).
find_englufed_objects(Image,Found,[_|ScanPoints],Engulfed):-
 find_englufed_objects(Image,Found,ScanPoints,Engulfed).

contained_object(O1,O2):-   
  O1 \== O2,
  loc_xy(O1,LowH1,LowV1),loc_xy(O2,LowH2,LowV2), 
  LowH2 > LowH1, LowV2 > LowV1,
  vis_hv(O1,H1,V1),vis_hv(O2,H2,V2), 
  H1> H2, V1> V2,
  HighH1 is LowH1+H1, HighV1 is LowV1+V1,
  HighH2 is LowH2+H2, HighV2 is LowV2+V2,
  HighH1 > HighH2,
  HighV1 > HighV2,
  nop(globalpoints(O2,[Point|_])),!,
  nop(object_surrounds_point(O1,Point)).



find_contained(_H,_V,_ID,Sofar,Sofar,[],[]).
find_contained(_H,_V,_ID,[],[],NextScanPoints,NextScanPoints).
find_contained(H,V,ID,[Found|Sofar],[Found|SofarInsteadM],NextScanPoints,NextScanPointsInstead):-
  isz(Found,outline(_)),
  once(find_contained_points(Found,NextScanPoints,ScanPointsInstead,ContainedPoints)),
  ContainedPoints\==[],
  %grid_size(Found,H,V),
  must_det_l((
  points_to_grid(H,V,ContainedPoints,Grid),
  %once(object_indv_id(Found,ID,_);grid_to_id(Grid,ID)),
  individuate(H,V,ID,[complete],Grid,ContainedPoints,NewInside),
  maplist(mention_inside(Found),NewInside,NewInsideM))),
  ignore((length(ContainedPoints,N),N>1,quietly(print_grid(H,V,[Found|NewInsideM])))),
  find_contained(H,V,ID,Sofar,SofarInstead,ScanPointsInstead,NextScanPointsInstead),
  append(NewInsideM,SofarInstead,SofarInsteadM).
find_contained(H,V,ID,[Found|Sofar],[Found|SofarInstead],NextScanPoints,NextScanPointsInstead):-
  find_contained(H,V,ID,Sofar,SofarInstead,NextScanPoints,NextScanPointsInstead).


mention_inside(Found,NewInside,NewInsideO):-
  object_indv_id(Found,_Where,Iv),
  add_shape_info(insideOf(Iv),NewInside,NewInsideO).

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
  loc_xy(Obj,X,Y),!,
  VV is V-Y, VV>=0,
  HH is H - X, HH>=0,
  vis_hv(Obj,XX,YY),!,
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


:- fixup_exports.


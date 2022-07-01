/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


area(Obj,Area):- vis_hv(Obj,H,V), Area is H * V.

density(Obj,Density):- area(Obj,Area),mass(Obj,Mass), Density is Mass/Area.


into_gridoid0(obj(N),O):- enum_object(O),o2g(O,G),sformat(N," ~w ",[G]).
into_gridoid0(shape_lib(N:Lib),O):- shape_lib_expanded(Lib,Grids),nth1(N,Grids,O).
into_gridoid0(N,G):- why_grouped(N,UG), UG\==[],UG\=[_],smallest_first(UG,G).
into_gridoid0(N,G):- into_grids(N,G).

into_gridoid(N,G):- no_repeats(S,(into_gridoid0(N,G),once(localpoints(G,P)),sort(P,S))).
%into_gridoid(N,G):- into_gridoid0(N,G).    
   

test_grav_rot:- test_p2(grav_rot).
grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
grav_rot(Shape,Final):- into_grid(Shape,Grid),grav_mass(Grid,RotG),!,call_rot(RotG,Shape,Final).

call_rot([],I,I):- !.
call_rot([H|T],I,O):- !,
  call_rot(H,I,M),
  call_rot(T,M,O).
call_rot(T,I,O):- call(T,I,O).

grav_mass(Grid,same):- iz(Grid,hv_symmetric),!.
grav_mass(Grid,RotOut):- vis_hv(Grid,H,V), !, tips_to_rot(Grid,H,V,RotOut).
% make things bottem heavy
%tips_to_rot(Grid,H,V,[rot270|RotOut]):- H<V, !, rot90(Grid,Grid90),!,tips_to_rot(Grid90,V,H,RotOut).
tips_to_rot(Grid,H,V,[rot90|RotOut]):- is_top_heavy(Grid), !, rot270(Grid,Grid90), !, tips_to_rot(Grid90,V,H,RotOut).
%tips_to_rot(Grid,H,V,[rot180|RotOut]):- is_top_heavy(Grid), !, rot180(Grid,Grid90), !, tips_to_rot(Grid90,H,V,RotOut).
tips_to_rot(Grid,_H,_V,RotOut):- is_left_heavy(Grid)-> RotOut=rot180; RotOut=same.
is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),!,color_mass(Top,TopM),color_mass(Bottem,BottemM),!,BottemM<TopM.
is_left_heavy(Grid0):- rot90(Grid0,Grid),is_top_heavy(Grid).
split_50_v(Grid,Top,Bottem):- length(Grid,N),H is floor(N/2), length(Top,H),length(Bottem,H),
    my_append(Top,Rest,Grid),my_append(_Mid,Bottem,Rest).

/*
  bottem_heavy(Grid90,RotG,Grid180).
grav_mass(Grid,_H,_V,RotG,Grid90):- is_h_symmetric(Grid),!,bottem_heavy(Grid,RotG,Grid90).
grav_mass(Grid,_H,_V,RotG,Grid90):- bottem_heavy(Grid,A),rot90(A,B),bottem_heavy(B,RotG,Grid90).

bottem_heavy(Grid,Turn,Grid180):-  (is_top_heavy(Grid)->(rot180(Grid,Grid180),Turn=rot180);(Grid=Grid180;Turn=same)).*/
/*
grav_mass(Grid,Mass):- grid_size(Grid,H,V), HV is round(H/V), Vh is floor(V/2),
  findall(C,(between(Vh,V,Vi),between(0,H,Hi), Hi*HV > Vi, get_color_at(Hi,Vi,Grid,C),is_fg_color(C)),CList),
  length(CList,Mass).
*/

gravity:- test_p2(gravity(4,w)).
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


any_xform(Rot90,Any,NewAny):- 
  cast_to_grid(Any,RealGrid,UnconvertClosure),!,
  grid_xform(Rot90,RealGrid,NewRealGrid),
  uncast(Any,UnconvertClosure,NewRealGrid,NewAnyWUpdate),
  record_object_change(Rot90,NewAnyWUpdate,NewAny).




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

grid_xform(Rot90,Grid,NewAnyWUpdate):- 
  grid_size(Grid,H,V),
  apply_transformer(Rot90,H,V,Grid,NewAnyWUpdate).
apply_transformer(Name,H,V,G,O):-
  get_spatial_xformer(Name,H,V,In,Out),!,
  G=In,O=Out.

get_spatial_xformer(_Name,1,1,In,In):- !.
get_spatial_xformer(Name,H,V,In,Out):- xform_cache(Name,H,V,In,Out),!.
get_spatial_xformer(Name,H,V,In,Out):- 
   make_grid(H,V,In),
   call(Name,In,Out),!,
   asserta(xform_cache(Name,H,V,In,Out)),!.

grid_same(X,X).
same_grid(X1,X2):- into_grid(X1,G1),into_grid(X2,G2),same(G1,G2).
same(X,X).

test_rot:- test_p2(rot270),test_p2(rot90).
%srot90V,flipV
rot90( Grid,NewAnyWUpdate):- any_xform(grid_rot90,Grid,NewAnyWUpdate).
rot180( Grid,NewAnyWUpdate):- any_xform(grid_rot180,Grid,NewAnyWUpdate).
rot270( Grid,NewAnyWUpdate):- any_xform(grid_rot270,Grid,NewAnyWUpdate).
flipH( Grid,NewAnyWUpdate):- any_xform(grid_flipH,Grid,NewAnyWUpdate).
flipV( Grid,NewAnyWUpdate):- any_xform(grid_flipV,Grid,NewAnyWUpdate).
flipHV( Grid,NewAnyWUpdate):- any_xform(grid_flipHV,Grid,NewAnyWUpdate).

grid_rot90(Grid,NewAnyWUpdate):-  grid_flipHV(Grid,GridM),grid_rot270(GridM,NewAnyWUpdate). 
grid_rot180(Grid,FlipHV):- grid_flipHV(Grid,FlipHV).
grid_rot270(Grid,NewAnyWUpdate):- get_colums(Grid,NewAnyWUpdate),!.
grid_flipH(Grid,FlipH):- mapgroup(reverse,Grid,FlipH).
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
move_dir_itself(N,D,I,M):- is_object(I),vis_hv(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_group(L),!,mapgroup(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_group(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 must_det_ll((
  loc_xy(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_group(L),!,mapgroup(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_group(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 must_det_ll((
  (NY<1 -> M=I ;
  ( localpoints(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpoints(GPoints),loc_xy(NX,NY)],M))))).
move_object(H,V,L,LM):- is_group(L),!,mapgroup(move_object(H,V),L,LM).
move_object(H,V,I,O):- into_group(I,M),M\=@=I,!,move_object(H,V,M,O).

is_input(VM):- VM.id = _ * _ * in.

% ==============================================
% TOUCHES
% ==============================================

find_touches(VM):- Objs = VM.objs,
  show_vm_changes(VM,find_touches,
     find_touches(VM,Objs,Objs,set(VM.objs))),!.

%ignore(((NewOptions\==Options;(GoneMissing\==[];SofarMaybeNewL\==SofarL)),

%show_object_changes(_VM,_S,Goal):-!, call(Goal).

show_vm_changes(VM,S,Goal):-
  show_object_changes(VM,S,show_point_changes(VM,S,show_grid_changes(VM,S,Goal))).

show_object_changes(VM,S,Goal):-
       setup_call_cleanup(duplicate_term(VM.objs,Was),
                     Goal,
        ignore((VM.objs\==Was,
          print_side_by_side4(silver,print_grid(VM.h,VM.v,Was),objs>was:S,_,print_grid(VM.h,VM.v,VM.objs),objs>new:S)))).

show_point_changes(VM,S,Goal):-
   setup_call_cleanup(duplicate_term(VM.points,Was),
      Goal,
     ignore((VM.points\==Was,
        print_side_by_side4(silver,print_grid(VM.h,VM.v,Was),points>was:S,_,print_grid(VM.h,VM.v,VM.points),points>new:S)))).

show_grid_changes(VM,S,Goal):-
   setup_call_cleanup(duplicate_term(VM.grid,Was),
                     Goal,
     ignore((VM.grid\==Was,
        print_side_by_side4(silver,print_grid(VM.h,VM.v,Was),grid>was:S,_,print_grid(VM.h,VM.v,VM.grid),grid>new:S)))).


%find_touches(VM,ScanNext,SofarInsteadO):- find_touches(VM,ScanNext,ScanNext,SofarInsteadO).

find_touches(_VM,[],SofarInsteadO,SofarInsteadO):-!.
find_touches(VM,[Found|ScanNext],OtherObjects,OtherObjectsO):-
 once(find_touches_objects(VM,Found,OtherObjects,_NewTouchesO,DirNewTouches)),
  NewTouches\==[], !,
  must_det_ll((
  mapgroup(mention_touches(Found),DirNewTouches,NewTouchesM),
    mapgroup(arg(2),DirNewTouches,NewTouches),
    replace_i_each(OtherObjects,NewTouches,NewTouchesM,NewOtherObjects),    
    replace_i_each(ScanNext,NewTouches,NewTouchesM,NewScanNext),
  ignore((length(NewTouches,N),N>0,quietly(print_grid(VM.h,VM.v,"touching",[Found|NewTouchesM])))), !,
  find_touches(VM,NewScanNext,NewOtherObjects,OtherObjectsO))),!.
find_touches(VM,[_|Sofar],OtherObjects,OtherObjectsO):-
  find_touches(VM,Sofar,OtherObjects,OtherObjectsO),!.

mention_touches(Found,Dir-NewInside,NewInsideO):-
  must_det_ll((object_indv_id(Found,_Where,Iv),
  override_object(touches(Dir,Iv),NewInside,NewInsideO))),!.

find_touches_objects(_VM,_,[],[],[]).
find_touches_objects(VM,Found,[Next|ScanPoints],[Next|TouchMore],[Dir-Next|Engulfed]):-    
 touching_object(Dir,Found,Next),
 find_touches_objects(VM,Found,ScanPoints,TouchMore,Engulfed),!.
find_touches_objects(VM,Found,[_|ScanPoints],TouchMore,Engulfed):-
 find_touches_objects(VM,Found,ScanPoints,TouchMore,Engulfed),!.

touching_object(Dirs,O1,O2):- 
  O1\==O2,
  globalpoints(O1,Ps1),
  globalpoints(O2,Ps2),
  dir_touching_list(Ps2,Ps1,Dirs),!.

dir_touching_list(Ps1,Ps2,Dirs):- findall(Dir,(member(Dir,[n,s,e,w,nw,ne,sw,se]),
  once(dir_touching_list0(Ps1,Ps2,Dir))),Dirs),Dirs\==[].
dir_touching_list0(Ps1,Ps2,Dir):- member(_-P1,Ps1), member(_-P2,Ps2), is_adjacent_point(P1,Dir,P2),!.

% ==============================================
% ENGULFS
% ==============================================
find_engulfs(VM):- 
  show_vm_changes(VM,find_engulfs,find_engulfs(VM,VM.objs,set(VM.objs))),!.

find_engulfs(VM,ScanNext,SofarInsteadO):-
  find_engulfs(VM,ScanNext,ScanNext,SofarInsteadO).

find_engulfs(_VM,[],SofarInsteadO,SofarInsteadO).
find_engulfs(VM,[Found|ScanNext],OtherObjects,OtherObjectsO):-
 ((isz(Found,outline(_));isz(Found,outl)) ->
 (( once(find_engulfs_objects(VM,Found,OtherObjects,NewInside)),
  
  NewInside\==[], 
  must_det_ll((
  mapgroup(mention_inside(Found),NewInside,NewInsideM),
  replace_i_each(OtherObjects,NewInside,NewInsideM,NewOtherObjects),
  replace_i_each(ScanNext,NewInside,NewInsideM,NewScanNext),
  ignore((length(NewInside,N),N>0,quietly(print_grid(VM.h,VM.v,"find_engulfs",[Found|NewInsideM])))),      
  find_engulfs(VM,NewScanNext,NewOtherObjects,OtherObjectsO)))))).

find_engulfs(VM,[_|Sofar],OtherObjects,OtherObjectsO):-
  find_engulfs(VM,Sofar,OtherObjects,OtherObjectsO).


find_engulfs_objects(_VM,_,[],[]).
find_engulfs_objects(VM,Found,[Next|ScanPoints],[Next|Engulfed]):-    
 contained_object(Found,Next),
 find_engulfs_objects(VM,Found,ScanPoints,Engulfed).
find_engulfs_objects(VM,Found,[_|ScanPoints],Engulfed):-
 find_engulfs_objects(VM,Found,ScanPoints,Engulfed).

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


% ==============================================
% Contained
% ==============================================
find_contained(VM):-
  show_vm_changes(VM,find_contained,find_contained(VM.h,VM.v,VM.id,VM.objs,set(VM.objs),VM.points,set(VM.points))),!.

find_contained(_H,_V,_ID,Sofar,Sofar,[],[]).
find_contained(_H,_V,_ID,[],[],NextScanPoints,NextScanPoints).
find_contained(H,V,ID,[Found|Sofar],[Found|SofarInsteadM],NextScanPoints,NextScanPointsInstead):-
  isz(Found,outline(_)),
  once(find_contained_points(Found,NextScanPoints,ScanPointsInstead,ContainedPoints)),
  ContainedPoints\==[],
  %grid_size(Found,H,V),
  must_det_ll((
  % points_to_grid(H,V,ContainedPoints,Grid),
  %once(object_indv_id(Found,ID,_);grid_to_id(Grid,ID)),
  individuate([subshape_in_object],ContainedPoints,NewInside),
  mapgroup(mention_inside(Found),NewInside,NewInsideM))),
  ignore((length(ContainedPoints,N),N>1,quietly(print_grid(H,V,"find_contained",[Found|NewInsideM])))),
  find_contained(H,V,ID,Sofar,SofarInstead,ScanPointsInstead,NextScanPointsInstead),
  my_append(NewInsideM,SofarInstead,SofarInsteadM).
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

replace_i_each(OtherObjects,[I|NewInside],[O|NewInsideM],NewOtherObjects):- must_be_free(NewOtherObjects),
  subst001(OtherObjects,I,O,OtherObjectsM),!,
  replace_i_each(OtherObjectsM,NewInside,NewInsideM,NewOtherObjects).
replace_i_each(E,[],[],E):-!.

:- fixup_exports.


/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


area(Obj,Area):- v_hv(Obj,H,V), Area is H * V.

density(Obj,Density):- area(Obj,Area),amass(Obj,Mass), Density is Mass/Area.


into_gridoid0(obj(N),O):- enum_object(O),o2g(O,G),sformat(N," ~w ",[G]).
into_gridoid0(shape_lib(N:Lib),O):- shape_lib_expanded(Lib,Grids),nth1(N,Grids,O).
into_gridoid0(N,G):- get_current_test(TestID),is_why_grouped(TestID,_,N,UG), UG\==[],UG\=[_],smallest_first(UG,G).
into_gridoid0(N,G):- into_grids(N,G).

into_gridoid(N,G):- no_repeats(S,(into_gridoid0(N,G),once(localpoints(G,P)),sort(P,S))).
%into_gridoid(N,G):- into_gridoid0(N,G).    
   

%grav_rot(Group,List):- override_group(grav_rot(Group,List)),!.
%grav_rot(Shape,Final):- into_grid(Shape,Grid),grav_mass(Grid,RotG),!,call_rot(RotG,Shape,Final).


test_grav_rot:- test_p2(test_grav_rot(_)).
test_grav_rot(RotG,Shape,Rotated):- grav_rot(Shape,RotG,Rotated). %,unrotate(RotG,Rotated,Back),assertion(Shape==Back).

grav_rot(Grid,RotG,Rotated):- must_be_free(Rotated), is_grid(Grid),!,
    w(W,RotG,Rotated)=Template,
    findall(Template,(flipSome1(RotG,Grid,Rotated),rot_mass(Rotated,W)),Pos),
    sort(Pos,LPos),last(LPos,Template).
grav_rot(Shape,RotG,Rotated):-    
    must_det_ll((
    cast_to_grid(Shape,Grid,Uncast),
    test_grav_rot(RotG,Grid,Final),
    uncast(Shape,Uncast,Final,Rotated))),!.

rot_mass(LP,Mass):- mapgrid(color_mass_int,LP,CN),
 append(CN,AC),!,total_n(1,AC,Mass).

color_mass_int(Cell,-2):- plain_var(Cell),!.
color_mass_int(Cell,0):- is_bg_color(Cell),!.
color_mass_int(Cell,-20):- var(Cell),is_fg_color(Cell),!.
color_mass_int(Cell,-10):- var(Cell),is_bg_color(Cell),!.
color_mass_int(Cell,N):- color_int(Cell,N),!.
color_mass_int(_,0).

total_n(_,[],0):-!.
total_n(S,[A|AA],Mass):- (var(A)-> LM is 0 ; LM is S * A),!,S2 is S+1, total_n(S2,AA,N),Mass is LM+N,!.

call_rot([],I,I):- !.
call_rot([H|T],I,O):- !,
  call_rot(H,I,M),
  call_rot(T,M,O).
call_rot(T,I,O):- call(T,I,O).

grav_mass(Grid,sameR):- iz(Grid,hv_symmetric),!.
grav_mass(Grid,RotOut):- v_hv(Grid,H,V), !, tips_to_rot(Grid,H,V,RotOut,_).

% make things bottem heavy
tips_to_rot(Grid,H,V,[rot270|RotOut],Final):- H<V, !, rot90(Grid,Grid90),!,trace,tips_to_rot(Grid90,V,H,RotOut,Final).
tips_to_rot(Grid,H,V,[rot90|RotOut],Final):- is_top_heavy(Grid), !, rot270(Grid,Grid90), !, tips_to_rot(Grid90,V,H,RotOut,Final).
%tips_to_rot(Grid,H,V,[rot180|RotOut]):- is_top_heavy(Grid), !, rot180(Grid,Grid90), !, tips_to_rot(Grid90,H,V,RotOut).
tips_to_rot(Grid,_H,_V,RotOut,Final):- is_left_heavy(Grid)-> (RotOut=[rot180],rot180(Grid,Final)); (RotOut=[sameR],Final=Grid).

is_top_heavy(Grid):- split_50_v(Grid,Top,Bottem),!,color_w_mass(Top,TopM),color_w_mass(Bottem,BottemM),!,BottemM>TopM.
is_left_heavy(Grid0):- rot90(Grid0,Grid),is_top_heavy(Grid).
split_50_v(Grid,Top,Bottem):- length(Grid,N),H is floor(N/2), length(Top,H),length(Bottem,H),
    my_append(Top,Rest,Grid),my_append(_Mid,Bottem,Rest).

color_w_mass(Color,Int):- var(Color),!,Int=13.
color_w_mass(Points,Count):- is_list(Points),!,maplist(color_w_mass,Points,MPoints),!,sum_list(MPoints,Count).
color_w_mass(Color,Int):- ground(Color),color_int(Color,Int),!.
color_w_mass(Color,Int):- number(Color),Color=Int,!.
color_w_mass(Obj,Count):- nonvar(Obj),localpoints(Obj,Points),!,color_w_mass(Points,Count),!.
color_w_mass(_,0).
/*
  bottem_heavy(Grid90,RotG,Grid180).
grav_mass(Grid,_H,_V,RotG,Grid90):- is_h_symmetric(Grid),!,bottem_heavy(Grid,RotG,Grid90).
grav_mass(Grid,_H,_V,RotG,Grid90):- bottem_heavy(Grid,A),rot90(A,B),bottem_heavy(B,RotG,Grid90).

bottem_heavy(Grid,Turn,Grid180):-  (is_top_heavy(Grid)->(rot180(Grid,Grid180),Turn=rot180);(Grid=Grid180;Turn=sameR)).*/
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
  get_bgc(E1), \+ get_bgc(E2),
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,E1,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).


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

get_spatial_xformer(_Name,1,1,In,In):- !.
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
rot90( Grid,NewAnyWUpdate):- rot180( Grid,M),rot270( M,NewAnyWUpdate).
rot180( Grid,NewAnyWUpdate):- any_xform(grid_rot180,Grid,NewAnyWUpdate).
rot270( Grid,NewAnyWUpdate):- any_xform(grid_rot270,Grid,NewAnyWUpdate).
flipH( Grid,NewAnyWUpdate):- any_xform(grid_flipH,Grid,NewAnyWUpdate).
flipV( Grid,NewAnyWUpdate):- any_xform(grid_flipV,Grid,NewAnyWUpdate).
flipD( Grid,NewAnyWUpdate):- any_xform(grid_flipD,Grid,NewAnyWUpdate).
flipDV( Grid,NewAnyWUpdate):- any_xform(grid_flipDV,Grid,NewAnyWUpdate).
flipDH( Grid,NewAnyWUpdate):- any_xform(grid_flipDH,Grid,NewAnyWUpdate).
flipDHV( Grid,NewAnyWUpdate):- any_xform(grid_flipDHV,Grid,NewAnyWUpdate).

grid_rot90(Grid,NewAnyWUpdate):-  rot270(GridM,NewAnyWUpdate),rot180(Grid,GridM).
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


unrotate(UnRot,X,Y):- unrotate(UnRot,Rot),!,call_rot(Rot,X,Y).
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
move_dir_itself(N,D,I,M):- is_object(I),v_hv(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_group(L),!,mapgroup(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_group(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 /*must_det_ll*/((
  loc(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_group(L),!,mapgroup(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_group(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 /*must_det_ll*/((
  (NY<1 -> M=I ;
  ( localpoints(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpoints(GPoints),loc(NX,NY)],M))))).
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

%ft i(VM,[find_touches|set(VM.program_i)]):-
  %cullObjectsOutsideOfRanges(VM),
%  find_touches(VM).
is_fti_step(find_touches).
find_touches(VM):-
  /*must_det_ll*/((Objs = VM.objs, find_touches(Objs,NewObjs))),
  gset(VM.objs) = NewObjs.


find_touches(Objs,NewObjs):- /*must_det_ll*/((find_touches(Objs,Objs,NewObjs))).
find_touches([],_,[]):-!.
find_touches([Obj|ScanNext],OtherObjects,[NewObj|ScanRest]):-
  /*must_det_ll*/(find_touches_objects(Obj,OtherObjects,DirNewTouches)),
  /*must_det_ll*/(override_object(DirNewTouches,Obj,NewObj)),
  /*must_det_ll*/(find_touches(ScanNext,OtherObjects,ScanRest)).

/*
mention_touches(Obj,[],Obj):-!.
mention_touches(Obj,[link(Dir,Touched)|More],NewFObjO):- !,
  mention_touches(Obj,Dir-Touched,MidObj),
  mention_touches(MidObj,More,NewFObjO).
mention_touches(Obj,Dir-Touched,NewObj):-
  /*must_det_ll*/(obj_to_oid(Touched,Iv)),
  /*must_det_ll*/(override_object(link(touches,Dir,Iv),Obj,NewObj)),!.
*/

find_touches_objects(_,[],[]).
find_touches_objects(Obj,_,[]):- has_prop(link(touched,_,_),Obj),!.
%find_touches_objects(Obj,_,[]):- has_prop(iz(dots),Obj),!.
find_touches_objects(Obj,[Touched|ScanNext],[BetterTouch|Engulfed]):-    
 once(touching_object(Dirs,Obj,Touched)),Dirs\==[],!,
 better_touched(Iv,Dirs,BetterTouch),
 /*must_det_ll*/(obj_to_oid(Touched,Iv)),
 /*must_det_ll*/(find_touches_objects(Obj,ScanNext,Engulfed)),!.
find_touches_objects(Obj,[_|ScanNext],Engulfed):- /*must_det_ll*/(find_touches_objects(Obj,ScanNext,Engulfed)),!.

better_touched(Iv,Dirs,link(touched,Iv,[-LO])):- length(Dirs,7),subtract([n,s,e,w,nw,ne,sw,se],Dirs,[LO]).
better_touched(Iv,[n,s,e,w,nw,ne,sw,se],link(touched,Iv,[c])).
better_touched(Iv,[n,s,e,w],link(touched,Iv,[c])).
better_touched(Iv,[ne,se],O):- !,better_touched(Iv,[e],O).
better_touched(Iv,[nw,sw],O):- !,better_touched(Iv,[w],O).
better_touched(Iv,[nw,ne],O):- !,better_touched(Iv,[n],O).
better_touched(Iv,[sw,se],O):- !,better_touched(Iv,[s],O).
better_touched(Iv,Dirs,O):- member(sw,Dirs),subtract(Dirs,[s,w],NDirs),NDirs\==Dirs,!,better_touched(Iv,NDirs,O).
better_touched(Iv,Dirs,O):- member(nw,Dirs),subtract(Dirs,[n,w],NDirs),NDirs\==Dirs,!,better_touched(Iv,NDirs,O).
better_touched(Iv,Dirs,O):- member(se,Dirs),subtract(Dirs,[s,e],NDirs),NDirs\==Dirs,!,better_touched(Iv,NDirs,O).
better_touched(Iv,Dirs,O):- member(ne,Dirs),subtract(Dirs,[n,e],NDirs),NDirs\==Dirs,!,better_touched(Iv,NDirs,O).
better_touched(Iv,Dirs,link(touched,Iv,Dirs)).

touching_object(Dirs,O2,O1):- 
  O1\==O2,
  has_prop(iz(shaped),O1),
  has_prop(iz(shaped),O2),
  %\+ has_prop(birth(glyphic),O2),
  %\+ has_prop(birth(glyphic),O1),
  globalpoints(O1,Ps1),
  globalpoints(O2,Ps2),
  dir_touching_list(Ps2,Ps1,Dirs),!.

dir_touching_list(Ps1,Ps2,[overlap]):- member(P1,Ps1), \+ \+ member(P1,Ps2),!.
dir_touching_list(Ps1,Ps2,Dirs):- findall(Dir,(member(Dir,[n,s,e,w,nw,ne,sw,se]),
  once(dir_touching_list0(Ps1,Ps2,Dir))),Dirs),Dirs\==[].
dir_touching_list0(Ps1,Ps2,Dir):- member(_-P1,Ps1), member(_-P2,Ps2), is_adjacent_point(P1,Dir,P2),!.

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
find_engulfs_objects(Obj,_,[]):- has_prop(link(insideOf,_),Obj),!.
find_engulfs_objects(Obj,_,[]):- has_prop(link(contains,_),Obj),!.
find_engulfs_objects(Obj,[Touched|ScanNext],[link(insideOf,Iv)|Engulfed]):-    
 once(contained_object(Obj,Touched)),!,
 /*must_det_ll*/(obj_to_oid(Touched,Iv)),
 /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.
find_engulfs_objects(Obj,_,[]):- amass(Obj,Mass),Mass<5,!.
find_engulfs_objects(Obj,[Touched|ScanNext],[link(contains,Iv)|Engulfed]):-    
 once(contained_object(Touched,Obj)),!,
 /*must_det_ll*/(obj_to_oid(Touched,Iv)),
 /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.
find_engulfs_objects(Obj,[_|ScanNext],Engulfed):- /*must_det_ll*/(find_engulfs_objects(Obj,ScanNext,Engulfed)),!.


contained_object(O2,O1):-   
  O1 \== O2,
  \+ has_prop(birth(glyphic),O2),
  \+ has_prop(birth(glyphic),O1),
  loc(O1,LowH1,LowV1),loc(O2,LowH2,LowV2), 
  LowH2 > LowH1, LowV2 > LowV1,
  v_hv(O1,H1,V1),v_hv(O2,H2,V2), 
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
%fti(VM,[find_contained|set(VM.program_i)]):- find_contained(VM).
is_fti_step(find_contained).
%fti(VM,[colormass_subshapes|set(VM.program_i)]):- colormass_subshapes(VM),!.
find_contained(VM):-
  show_vm_changes(VM,find_contained,find_contained(VM.h,VM.v,VM.id,VM.objs,set(VM.objs),VM.points,set(VM.points))),!.

find_contained(_H,_V,_ID,Sofar,Sofar,[],[]).
find_contained(_H,_V,_ID,[],[],NextScanPoints,NextScanPoints).
find_contained(H,V,ID,[Found|Sofar],[Found|SofarInsteadM],NextScanPoints,NextScanPointsInstead):-
  isz(Found,outline(_)),
  once(find_contained_points(Found,NextScanPoints,ScanPointsInstead,ContainedPoints)),
  ContainedPoints\==[],
  %grid_size(Found,H,V),
  /*must_det_ll*/((
  % points_to_grid(H,V,ContainedPoints,Grid),
  %once(obj_to_oid(Found,ID,_);grid_to_tid(Grid,ID)),
  individuate(subshape_in_object,ContainedPoints,NewInside),
  mapgroup(mention_inside(Found),NewInside,NewInsideM))),
  ignore((length(ContainedPoints,N),N>1,quietly(print_grid(H,V,"find_contained",[Found|NewInsideM])))),
  find_contained(H,V,ID,Sofar,SofarInstead,ScanPointsInstead,NextScanPointsInstead),
  my_append(NewInsideM,SofarInstead,SofarInsteadM).
find_contained(H,V,ID,[Found|Sofar],[Found|SofarInstead],NextScanPoints,NextScanPointsInstead):-
  find_contained(H,V,ID,Sofar,SofarInstead,NextScanPoints,NextScanPointsInstead).


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
  loc(Obj,X,Y),!,
  VV is V-Y, VV>=0,
  HH is H - X, HH>=0,
  v_hv(Obj,XX,YY),!,
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


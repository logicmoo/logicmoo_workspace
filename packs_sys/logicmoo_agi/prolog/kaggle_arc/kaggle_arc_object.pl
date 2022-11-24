:- encoding(iso_latin_1).
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- discontiguous(obj_to_program/3).

:- dynamic(decl_pt/1).

:- thread_local(t_l:id_cells/2).

deoffset_points(1,1,Points,PointsO):-!,Points=PointsO.
deoffset_points(OH,OV,Point,LPoint):- map_pred(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- is_nc_point(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

offset_points(1,1,Points,PointsO):-!,Points=PointsO.
offset_points(OH,OV,Point,LPoint):- map_pred(if_point_offset(OH,OV),Point,LPoint).
if_point_offset(OH,OV,Point,LPoint):- is_nc_point(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

offset_point(OH,OV,Point,LPoint):- is_nc_point(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).
offset_point(OH,OV,C-Point,C-LPoint):- is_nc_point(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).


grid_to_individual(GridIn,Obj):- 
  %my_assertion(is_grid(Grid)),!,
  get_vm(VM),
  into_grid(GridIn,Grid),
  grid_size(Grid,H,V),
  grid_to_points(Grid,H,V,Points),
  (Points==[]-> empty_grid_to_individual(H,V,Obj); 
  (make_indiv_object(VM,[iz(grid)],Points,Obj))).

empty_grid_to_individual(H,V,Obj):-
  Iv is H + V*34,
  Obj = obj( [ amass(0),
         shape([]),
         colors([]),
         localpoints([]), vis2D(H, V), 
         rot2L(sameR), 
         loc2D(1, 1),
         obj_iv(Iv),
         %pen([fg]),
         changes([]), 
         iz(grid),
         %obj_to_oid(empty_grid_to_individual(H,V), Iv),
         grid_size(H, V)]).


close_enough_grid(GridIn,GridInCopy,LocalGrid):- 
  \+ \+ (LocalGrid=GridIn, GridIn=@=GridInCopy).



make_indiv_object_list(_,[],[]):-!.
make_indiv_object_list(VM,[E|L],[O|OL]):-   
    must_det_ll(ensure_indiv_object(VM,E,O)),
    make_indiv_object_list(VM,L,OL).



:- module_transparent as_obj/2.
as_obj(L,Obj):- is_list(L),!,Obj = obj(L), !. % , register_obj(L).
as_obj(O,Obj):- compound(O), O = obj(_), Obj = O. % , register_obj(L).


enum_object(O):- var(O),!,no_repeats_cmp(=@=,O,enum_object0(O)).
enum_object(O):- nop(ppt(enum_object(O))),!.

enum_object0(Obj):- % listing(obj_cache/2),
      get_current_test(TestID), obj_cache(TestID,O,_S),as_obj(O,Obj).
enum_object0(Obj):- get_current_test(TestID), why_grouped(TestID,_Why,GS)*->member(Obj,GS);enum_object1(Obj).
enum_object1(Obj):- g2o(_,Obj)*->true; enum_object2(Obj).
enum_object2(_):- fail.
%enum_object2(Obj):- get_current_test(_).
/*
enum_object0(S):- why_grouped(TestID,_,IndvS),member(S,IndvS).
enum_object0(S):- clause(in_shape_lib(_,S),Body),catch(Body,_,fail).
enum_object0(S):- g2o(_,S).
enum_object0(S):- is_unshared_saved(_,IndvS),member(S,IndvS).
enum_object0(S):- is_grid_tid(S,_).
*/
internal_region(Obj,regionOf(Obj)).

% @TODO
reclumped([A,B,C,D|Rest],[A,B,C,D]):- reclumped(Rest,[A,B,C,D]),!.
reclumped([A,B,C|Rest],[A,B,C]):- reclumped(Rest,[A,B,C]),!.
reclumped([A,B|Rest],[A,B]):- reclumped(Rest,[A,B]),!.
reclumped(Rest,Seq):- append(Seq,_,Rest),!.
reclumped(PenColors,PenColors).


maybe_include_bg(Points,FgPoints):- fg_points(Points,FgPoints),FgPoints\==[],!.
maybe_include_bg(Points,Points).
fg_points(Points,FgPoints):- include(is_fg_point,Points,FgPoints).
is_fg_point(CPoint):- \+ (only_color_data(CPoint,Color),is_bg_color(Color)).

rev_key(C-P,P-C).
rev_key(List,ListO):- is_list(List),!,maplist(rev_key,List,ListO).

sort_points(P0,P2):- 
   (P0==[] -> (trace) ; true),
   my_assertion(is_list(P0)),
   sort(P0,P1),my_assertion(P1\==[]), 
   my_assertion(is_cpoints_list(P1)),  
   maplist(rev_key,P1,R1),keysort(R1,R2),maplist(rev_key,R2,P2).

%same_globalpoints(O1,O2):-  globalpoints_include_bg(O1,P1),same_globalpoints_ps_obj(P1,O2).

same_globalpoints_and_window(I,O):-
  get_loc2D_vis2D(I,P1,H1,V1,OH1,OV1),
  get_loc2D_vis2D(O,P2,H2,V2,OH2,OV2),!,
  P1=@=P2,H1=H2,V1=V2, OH1=OH2,OV1=OV2.
 
same_globalpoints_ovrs_ps_obj(Overrides,P1,O2):-
  po_loc2D_vis2D(P1,Overrides,H1,V1,OH1,OV1),
  get_loc2D_vis2D(O2,P2,H2,V2,OH2,OV2),
  P1=@=P2,H1=H2,V1=V2, OH1=OH2,OV1=OV2.

po_loc2D_vis2D(_GPoints,Overrides,LoH,LoV,Height,Width):- is_list(Overrides), member(vis2D(Width,Height),Overrides), member(loc2D(LoH,LoV),Overrides),!.
po_loc2D_vis2D(GPoints,_Overrides,LoH,LoV,Height,Width):- points_range(GPoints,LoH,LoV,HiH,HiV,_HO,_VO),Width is HiH-LoH+1,Height is HiV-LoV+1. 

get_loc2D_vis2D(O2,P2,H2,V2,OH2,OV2):- globalpoints_include_bg(O2,P2), loc2D(O2,H2,V2), vis2D(O2,OH2,OV2),!.

%get_loc2D_vis2D( O2,P2,_Props2,H2,V2,OH2,OV2):- nonvar(O2), (var(P2)-> globalpoints_include_bg(O2,P2) ; true), loc2D(O2,H2,V2), vis2D(O2,OH2,OV2),!.
%get_loc2D_vis2D(_O2,P2, Props2,H2,V2,OH2,OV2):- po_loc2D_vis2D(P2,Props2,H2,V2,OH2,OV2),!.
%get_loc2D_vis2D( O2,P2, Props2,H2,V2,OH2,OV2):- var(Props2), indv_props(O2,Props2), po_loc2D_vis2D(P2,Props2,H2,V2,OH2,OV2).



ensure_indiv_object(VM,IPoints,Obj):- 
  ((compound(IPoints), IPoints = obj(_)) -> (IPoints = Obj, nop(addObjects(VM,Obj)));
   (my_partition(is_cpoint,IPoints,Points,Overrides),
    wdmsg(po(Points,Overrides)),
    make_indiv_object(VM,Overrides,Points,Obj))).

make_point_object(VM,Overrides,C-Point,Obj):- 
  must_det_ll(make_indiv_object(VM,Overrides,[C-Point],Obj)).

globalpoints_maybe_bg([],[]):-!.
globalpoints_maybe_bg(ScaledGrid,GPoints):- is_points_list(ScaledGrid),!,maybe_include_bg(ScaledGrid,GPoints).
globalpoints_maybe_bg(ScaledGrid,Points):- once(globalpoints(ScaledGrid,Points)),Points\==[],!.
globalpoints_maybe_bg(ScaledGrid,GPoints):- globalpoints_include_bg(ScaledGrid,GPoints),!.

globalpoints_include_bg(ScaledGrid,GPoints):- is_points_list(ScaledGrid),!,ScaledGrid=GPoints.
globalpoints_include_bg([],[]):-!.
globalpoints_include_bg(ScaledGrid,GPoints):- loc2D(ScaledGrid,OH,OV),
  localpoints_include_bg(ScaledGrid,Points),!,offset_points(OH,OV,Points,GPoints).

with_global_offset(X,Y,Goal):-
 nb_current(global_offset,loc2D(OX,OY)),
 NX is X+OX-1, NY is Y+OY-1, 
  locally(nb_setval(global_offset,loc2D(NX,NY)),Goal).
  
fix_global_offset(Points,PointsO):- nb_current(global_offset,loc2D(X,Y)),!, offset_points(X,Y,Points,PointsO).
fix_global_offset(GOPoints,OPoints):- GOPoints=OPoints,!.

make_indiv_object(VM,Overrides,GOPoints,NewObj):-
 fix_global_offset(GOPoints,OPoints),
 must_det_ll((
  globalpoints_maybe_bg(OPoints,GPoints),
  sort_points(GPoints,Points),
  Objs = VM.objs,
  Orig = _,!,
  must_det_ll(((select(Orig,Objs,Rest),same_globalpoints_ovrs_ps_obj(Overrides,Points,Orig))
    -> must_det_ll((override_object(Overrides,Orig,NewObj), ROBJS = Rest))
    ; must_det_ll((make_indiv_object_s(VM.gid,VM.h,VM.v,Overrides,Points,NewObj), ROBJS = Objs)))),!,

  if_t(NewObj\=@=Orig,
   (if_t(is_object(Orig),
      nop((obj_to_oid(Orig,OOID),
       retract_object(VM.gid,OOID,Orig),
       print_grid(remove_prev(OOID),Orig)))),
    NEW = [NewObj|ROBJS],
    set(VM.objs)=NEW)))).

maybe_replace_object(VM,Orig,NewObj):- 
  if_t(NewObj\=@=Orig,
   if_t(is_object(Orig),
     if_t(select(Orig,VM.objs,ROBJS),
      (nop((obj_to_oid(Orig,OOID),
       retract_object(VM.gid,OOID,Orig),
       print_grid(remove_prev(OOID),Orig))),
    set(VM.objs)= [NewObj|ROBJS])))).

grav_roll(LPoints,RotG,Shape):-
  must_det_ll(grav_rot(LPoints,RotG,Shape)),!.

grid_to_shape(Grid,RotG,SH,SV,ColorlessPoints,PenColors):-
  grav_roll(Grid,RotG,RotShape),
  globalpoints_maybe_bg(RotShape,RShape), include(ground,RShape,LShape),
  grid_size(RotShape,SH,SV),
  % colors 
  maplist(arg(2),LShape,ColorlessPoints), maplist(arg(1),LShape,Colorz),
  cclumped(Colorz,PenColors),!.


make_indiv_object_no_vm(ID,GH,GV,Overrides,OPoints,Obj):- 
  globalpoints_maybe_bg(OPoints,GPoints),
  sort_points(GPoints,SPoints),
  make_indiv_object_s(ID,GH,GV,Overrides,SPoints,Obj).

on_edge(H,V,_-P1,Edge):-
  hv_point(X,Y,P1), edge_of_grid(H,V,X,Y,Edge),!.

%zero_one_more(0-N,0-N).
zero_one_more(1-c,iz(on_edge(only))).
zero_one_more(_-c,iz(on_edge(some_off))).
%zero_one_more(_-c,[]).
zero_one_more(1-C,iz(edge(C,none))):- C\==c.
zero_one_more(2-C,iz(edge(C,touch))):- C\==c.
zero_one_more(_-C,iz(edge(C,touch))):- C\==c.
zero_one_more(_,[]).

% [1,2,3]=27
% [1,1,1,2,2,2,3,3,3]=27

% [1,2,2,3]=12.
% [1,2,3]=12.

to_global_coord(H,LH,OH,GH):- max_min(LH,OH,Max,_Min), GH is  floor((H-1)*OH/Max)+1.

:- style_check(+singleton).
make_indiv_object_s(GID,GH,GV,Overrides,GPoints,ObjO):- 
 must_det_ll((
  testid_name_num_io(GID,_TestID,_Example,_Num,IO),
  %points_range(GPoints,LoH,LoV,HiH,HiV,_HO,_VO), once(member(vis2D(Width,Height),Overrides);(Width is HiH-LoH+1,Height is HiV-LoV+1)),
  po_loc2D_vis2D(GPoints,Overrides,LoH,LoV,Height,Width),
  %luser_getval(test_pairname,ID),
  Area is Width * Height,
  my_assertion(is_list([overrides|Overrides])),
  my_assertion(is_cpoints_list(GPoints)),
  %colors(GPoints,CC),
  %my_assertion(ground(GPoints)),
  %flag(indiv,Fv,Fv+1),
  %Iv is (Fv rem 3000) + 1,
  all_colors_via_pixels(GPoints,CC),
  length(GPoints,Len),
  deoffset_points(LoH,LoV,GPoints,LPoints),
  % sort(LPoints,LPointsS), maplist(arg(1),LPointsS,BPenColors), clumped(BPenColors,CPenColors), reclumped(CPenColors,PenColors),
  %remove_color(LPoints,UColorlessPoints),
  maplist(on_edge(GH,GV),GPoints,EdgeL),count_sets([n,s,e,w,c|EdgeL],_,EdgeC),maplist(zero_one_more,EdgeC,EdgeS),
  
  length(LPoints,Len),
  Empty is Area - Len,

  make_grid(Width,Height,Grid),
  add_global_points(LPoints,Grid,Grid),
  once(member(grid(LocalGrid),Overrides);LocalGrid=Grid),


  ignore((member(iz(shaped),Overrides),
      % \+ member(iz(image),Overrides),
     learn_hybrid_shape(LocalGrid))),

  % calc center2G
  must_det_ll(once(
   ((member(UFgPoints,[GPoints]),
    ((CCX is LoH + floor(Width/2),CCY is LoV + floor(Height/2), hv_point(CCX,CCY,Point), member(_-Point,UFgPoints));
    (length(UFgPoints,UFgLen),CP is round(UFgLen/2), nth1(CP,UFgPoints,Point),hv_point(CCX,CCY,Point)))));
   (CCX is LoH + floor(Width/2),CCY is LoV + floor(Height/2)))),

  other_grid_size(GID,OtherH,OtherV),
  to_global_coord(LoH,GH,OtherH,GNX),to_global_coord(LoV,GV,OtherV,GNY),
  to_global_coord(CCX,GH,OtherH,CX),to_global_coord(CCY,GV,OtherV,CY),
  copy_term(Grid,GridInCopy),

  %grid_to_gridmap(Grid,ColorlessPoints), 

  findall(ShapeName,
   (guess_shape(GH,GV,Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,LPoints,ShapeName),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),ShapeNamesUF),
  flatten([ShapeNamesUF],ShapeNamesU),list_to_set(ShapeNamesU,ShapeNames),
  maplist(append_term(iz),ShapeNames,OShapeNames),

  % rotated local points 
  /*(memberchk(rot2L(RotOut),Overrides)-> FinalLocalPoints=LPoints;
    must_det_ll((tips_to_rot(LPoints,GH,GV,RotOut,Final),localpoints(Final,FinalLocalPoints)))),
  delistify_single_element(RotOut,RotO),
  */
  %maybe_include_bg(FinalLocalPoints,LPoints),
  %FinalLocalPoints = LPoints,
  
  grid_to_shape(Grid,RotG,SH,SV,ColorlessPoints,PenColors),
  shape_id(ColorlessPoints,ShapeID),

  iv_for([ shape(ColorlessPoints),  loc2D(LoH,LoV),  pen(PenColors),  rot2L(RotG)],Iv),



  

  int2glyph(Iv,Glyph), % object_glyph(Obj,Glyph),       
  % atomic_list_concat(['o_',Glyph,'_',GID],OID),
  op_grid_to_norm(NormOps,Grid,NormGrid),
  flatten(
  [ 
    shape(ColorlessPoints),
    loc2D(LoH,LoV),
    pen(PenColors),
    rot2L(RotG),
    
    center2G(CX,CY),
    loc2G(GNX,GNY),
    
    iz(sid(ShapeID)),
    
    shape2D(SH,SV),
    vis2D(Width,Height),
    mass(Len),     
    CC,        
    localpoints(LPoints),

    amass(Len),
    iz(cenY(CY)),iz(cenX(CX)),  
    %width(Width), height(Height), area(Area), %missing(Empty),
    changes([]), % [grid(LocalGrid)],    
    OShapeNames,
    
    [norm_grid(NormGrid),norm_ops(NormOps)],
    % [iz(locY(LoV)),iz(locX(LoH))], % iz(tall(Height)),iz(wide(Width)),
    iz(sizeY(Height)),iz(sizeX(Width)),
    %obj_to_oid(ID,Iv),
    giz(g(IO)),
    giz(gid(GID)),
    EdgeS,
    % iz(oid(OID)),
    giz(glyph(Glyph)),
    globalpoints(GPoints),
    giz(grid_sz(GH,GV)),
    []],Ps0),  
  include('\\=='([]),Ps0,Ps),

  make_localpoints(ColorlessPoints,RotG,SH,SV,PenColors,XX), assertion((XX == LPoints)),

  with_objprops(override,Overrides,Ps,OUT1),
  sort_obj_props(OUT1,OUT),!,as_obj(OUT,Obj),verify_object(Obj),!,
 must_det_ll((ObjO = Obj)))).

top(7).

%fix_clump([_-C],[1-C]).

%fix_clump(Cs,CsO):- append([A,B|Mid],[A,B],Cs),append([A|Mid],[],CsM),!,fix_clump(CsM,CsO).
%fix_clump(Cs,CsO):- append([A|Mid],[A],Cs),append([A|Mid],[],CsM),!,fix_clump(CsM,CsO).
%fix_clump(Cs,CsO):- append([CsM,CsM,CsM,CsM,CsM],Cs),!,fix_clump(CsM,CsO).
fix_clump([],O):- !, O =[],!.
fix_clump(Cs,CsO):- append([CsM,CsM,CsM],Cs),!,fix_clump(CsM,CsO).
fix_clump(Cs,CsO):- append([CsM,CsM],Cs),!,fix_clump(CsM,CsO).
fix_clump(Cs,Cs).

cclumped(Items, CountsO) :- cclump(Items, Counts),!,fix_clump(Counts,CountsO),!.
cclump([H|T0], [cc(H,C)|T]) :-
    lists:ccount(T0, H, T1, 1, C), cclump(T1, T). 
cclump([], []).  color_c(C,H,C-H).

prop_order([vis2D/2,mass/1,loc2D/2,amass/1,
  %center2G/2,
  pen/1,shape/1,localpoints/1,rot2L/1,cc/2,iz/1,globalpoints/1,obj_to_oid/2,grid_size/2]).


iz_o(F,A):- member(F/A,[cenX/1,cenY/1,locY/1,locX/1,tall/1,wide/1,chromatic/2,dot/0]).
iz_o(F,A):- clause(guess_shape(_GH,_GV,_GridIn,_LocalGrid,_I,_Empty,_N,_H,_V,_CC,_Points,Term),_),nonvar(Term),functor(Term,F,A).
iz_o(poly(F),A):- clause(guess_shape_poly(_I,_Empty,_N,_H,_V,_CC,_Points,Term),_),nonvar(Term),functor(Term,F,A).

%cache_obj(Obj):- get_current_test(TestID), object_prop_val(O,P,V):- prop_order(List),member( 

record_object_change(Rot90,Obj,XObj):- is_object(Obj), object_changes(Obj,Was),
  override_object(changes([Rot90|Was]),Obj,XObj),!.
record_object_change(_Missied,XObj,XObj).

ignore_xf(_):-!.
ignore_xf(G):- ignore(notrace(catch(G,_,fail))).

%guess_pretty2(O):- mortvar((copy_term(O,C),pretty1(O),O=@=C)).

show_objs_as_code(O):- var(O),!,pp(var(O)),!.
show_objs_as_code(Grid):- is_grid(Grid),!.

show_objs_as_code(Objs):- is_list(Objs),!,
  debug_var('VirtMachine',VM),
  findall(Code,(member(Obj,Objs),obj_to_program(Obj,Code,VM),maplist(ignore_xf,Code)),List),
  flatten(List,LList),
  pretty1(LList),
  guess_prettyf(LList),
  ppt(LList).

show_objs_as_code(VM):-
  Objs = VM.objs,
  show_objs_as_code(Objs),!.

draw_seg(Pen,X,Y,H,V,VM):- nop(doing(draw_seg(Pen,X,Y,H,V,VM))).


obj_to_program(Obj,Program,VM):- 
  (isz(Obj,hv_line(_));isz(Obj,dg_line(_));vis2D(Obj,1,_);vis2D(Obj,_,1)),!,
  Program = 
  [loc2D(Obj,X,Y),vis2D(Obj,H,V),
   pen(Obj,Pen),
   draw_seg(Pen,X,Y,H,V,VM)].

add_grid_at_offset(X,Y,Grid,VM):- !,
  grid_to_points(Grid,Ps),
  offset_points(X,Y,Ps,GPs),
  addGPoints(VM,GPs).



o(Obj,W,LF,N):- enum_object(Obj), indv_props(Obj,L),member(o(W,LF,N),L).


obj_to_program(Obj,Program,VM):-
  (isz(Obj,outl);isz(Obj,outline(_))),!,
  Program = 
  [loc2D(Obj,X,Y),   
   object_grid(Obj,Grid),
   add_grid_at_offset(X,Y,Grid,VM)].

obj_to_program(Obj,Program,VM):-
  isz(Obj,dot),!,
  Program =  [addGPoints(VM,Obj)].

obj_to_program(Obj,Program,VM):-
  Program = 
  [globalpoints(Obj,Ps), 
   addGPoints(VM,Ps)].


%object_pen(Obj,pen([cc(Color,1)])):- color(Obj,Color),!.
%object_pen(Obj,[cc(Color,1)]):- color(Obj,Color),!.

prop_of(amass,amass(_)).
prop_of(colors,pen(_)).

prop_of(visually,localpoints(_)).
prop_of(loc2D,loc2D(_,_)).

prop_of(size2D,vis2D(_,_)).
prop_of(amass,mass(_)).

prop_of(loc2D,center2G(_,_)).
prop_of(rot2L,rot2L(_)).
prop_of(visually,pen(_)).
prop_of(shape,shape(_)).


sort_obj_props(obj(L),obj(LO)):- !, sort_obj_props(L,LO).
%sort_obj_props(L,LO):- L=LO.
sort_obj_props(L,LOR):- maplist(in_obj_keys,L,LL),keysort(LL,LLS),maplist(arg(2),LLS,LO),reverse(LO,LOR).
%obj_prop_sort_compare(R,A,B):- compound(A), compound(B), !, obj_prop_sort_compare2(R,B,A).
%obj_prop_sort_compare(R,A,B):- compare(R,B,A).
%obj_prop_sort_compare2(R,A,B):- obk_key(A,AK),obk_key(B,BK),!,compare(R,AK-A,BK-B).
e1_member(E,L):- \+ \+ member(E,L).

in_obj_keys(P,(K-P)):- obk_key(P,K),!.

obk_key(A,P):- string(A), priority(A,P).
obk_key(A,0):- \+ compound(A),!.
obk_key(A,AKV):- callable(A), clause(prop_of(_,A),true,Ref),nth_clause(prop_of(_,_),AK,Ref),AKV is AK * 100, !. 
obk_key(iz(C),O):- compound(C),!,obk_key(C,O).
obk_key(touched(_,_,_),-150).
obk_key(touched(_,_),-150).
obk_key(link(_,_,_),-150).
obk_key(link(_,_),-150).
obk_key(chromatic(_,_),0):-!.
obk_key(A,99):- arg(1,A,L), is_grid(L).
obk_key(A,92):- arg(_,A,L), is_list(L).
obk_key(A,91):- arg(_,A,L), number(L).
obk_key(_,80). 

priority("bckgrnd",0).
priority("point",0).
priority(A,10):- atom_contains(A,")").
priority(_,20).
longer_strings(R,A,B):- string(A),string(B),priority(A,PA),priority(B,PB),atom_length(A,AL),atom_length(B,BL),compare(R,PA+AL+A,PB+BL+B).
longer_strings(R,A,B):- compare(R,A,B).


add_shape_info(Info,I,M):- add_shape_info0(Info,I,M),!.

add_shape_info0([Info|L],I,O):-!,add_shape_info0(Info,I,M),add_shape_info0(L,M,O).
add_shape_info0([],I,I):-!.
add_shape_info0(Info,I,O):- with_object(override,iz(Info),I,O).

verify_object(Obj):-
 % my_assertion(localpoints(Obj,_LP)),
 % my_assertion(globalpoints(Obj,_GP)),
  nop(assertion((iz(Obj,symmetry(What)), nonvar(What)))).

override_object([],I,I):-!.
override_object(E,I,O):- with_object(override,E,I,O).

with_object(Op,E,obj(List),O):- !, with_objprops(Op,E,List,MidList),O=obj(MidList),!,verify_object(O).
with_object(Op,E,I,O):- is_group(I), mapgroup(with_object(Op,E),I,O).
% with_object(Op,E,I,O):- is_list(I), !, with_objprops(Op,E,I,O).
with_object(Op,E,     I,     O):- with_objprops(Op,E,I,O).

with_objprops(Op,obj(E),List,MidList):- my_assertion(is_list(List)),my_assertion(is_list(E)), !, with_objprops(Op,E,List,MidList).
with_objprops(Op,E,obj(List),obj(MidList)):- my_assertion(is_list(List)), !, with_objprops(Op,E,List,MidList).

%with_objprops(Op,Info,List,ListO):- my_assertion(is_list(List)),[]==Nil,!,ListO=List.

with_objprops(_Op,Nil,List,ListO):- my_assertion(is_list(List)),[]==Nil,!,ListO=List.
with_objprops(Op,[E|Props],List,NewList):-!,
  with_objprops(Op,E,List,MidList),
  with_objprops(Op,Props,MidList,NewList).


with_objprops(delq,E,List,NewList):-functor(E,F,A),functor(R,F,A),
    my_append(Left,[R|Right],List), % E \=@= R,
    my_append(Left,Right,NewList),!.

with_objprops(override,-E,List,NewList):-
    my_append(Left,[R|Right],List), E =@= R,
    my_append(Left,Right,NewList),!.

with_objprops(override,E,List,NewList):- \+ aggregates(E), my_assertion(compound(E)), functor(E,F,A),functor(R,F,A),
    my_append(Left,[R|Right],List), % E \=@= R,
    my_append(Left,[E|Right],NewList),!.

with_objprops(override,E,List,NewList):- 
    my_append(Left,[changes(G)|Right],List), !,
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; my_append(Left,[changes(G),E|Right],NewList)),!.

with_objprops(override,E,List,NewList):- 
    my_append(Left,[iz(G)|Right],List), 
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; my_append(Left,[iz(G),E|Right],NewList)),!.


aggregates(iz(_)).
aggregates(cc(_,_)).
aggregates(giz(_)).
aggregates(o(_,_,_)).
aggregates(birth(_)).
aggregates(link(_,_,_)).
aggregates(link(_,_)).
aggregates(insideOf(_)).

is_bg_object(Obj):- get_black(Black),has_prop(pen(  [cc(Black,_)]),Obj).



merge_objs(_VM,Bigger,[],_IPROPS,Bigger):-!.
merge_objs(VM,Bigger,[New|Inside],IPROPS,Combined):- 
  merge_2objs(VM,Bigger,New,IPROPS,NewBigger),
  merge_objs(VM,NewBigger,Inside,[],Combined).
      
merge_2objs(VM,Bigger,NewInside,IPROPS,Combined):-
 globalpoints(Bigger,GP1), globalpoints(NewInside,GP2),      
 indv_props(Bigger,Props1),indv_props(NewInside,Props2),
 my_append([GP1,GP2],GPoints), my_append([Props1,Props2,IPROPS],Info),
 my_partition(props_not_for_merge,Info,_Exclude,Include),
 make_indiv_object(VM,Include,GPoints,Combined).

props_not_for_merge(globalpoints(_)).
props_not_for_merge(shape(_)).
props_not_for_merge(localpoints(_)).
props_not_for_merge(obj_to_oid(_,_)).
props_not_for_merge(loc2D(_,_)).
props_not_for_merge(center2G(_,_)).
props_not_for_merge(vis2D(_,_)).
props_not_for_merge(cc(_,_)).
props_not_for_merge(grid_size(_,_)).

/*
transfer_props(O,Functors,NewO,obj(NewObjL)):-
  indv_props(O,L),
  indv_props(NewO,NewOL),
  transfer_props_l(L,Functors,NewOL,NewObjL).

transfer_props_l([],_,O,O):-!.
transfer_props_l([P|L],Functors,List,NewList):-
  functor(P,F,_),member(F,Functors), override_object(List,P,MidList),
  transfer_props_l(L,Functors,MidList,NewList).
transfer_props_l([_|L],Functors,List,NewList):-
  transfer_props_l(L,Functors,List,NewList).
*/


%indv_u_props(I,[localpoints(Ps),loc2D(X,Y),pen(Pen),vis2D(H,V),rot2L(Rot)]):- loc2D(I,X,Y),shape(I,Ps),pen(I,Pen),vis2D(I,H,V),rot2L(I,Rot),!.
indv_u_props(I,[ shape(C),  loc2D(X,Y),  pen(Ps),  rot2L(Rot)]):-  shape(I,C),loc2D(I,X,Y),pen(I,Ps),rot2L(I,Rot),!.
%indv_u_props(I,[ shape(C),  center2G(X,Y),  pen(Ps),  rot2L(Rot)]):- shape(I,C),center2G(I,X,Y),pen(I,Ps),rot2L(I,Rot),!.
%indv_u_props(I,[shape(Ps),center2G(X,Y),pen(Pen),vis2D(H,V),rot2L(Rot)]):- center2G(I,X,Y),shape(I,Ps),pen(I,Pen),vis2D(I,H,V),rot2L(I,Rot),!.

:- dynamic(is_shape_id_for/2).
is_shape_id_for([],sid_0).
is_shape_id_for([point_01_01],sid_11).
is_shape_id_for([point_01_01,point_02_01],sid_12).
is_shape_id_for([point_01_01,point_02_01,point_03_01],sid_13).
is_shape_id_for([point_01_01,point_02_01,point_03_01,point_04_01],sid_14).


is_shape_id_for([point_01_01,point_01_02],sid_21).
is_shape_id_for([point_01_01,point_02_01,point_01_02,point_02_02],sid_22).
is_shape_id_for([point_01_01,point_02_01,point_03_01,point_01_02,point_02_02,point_03_02,point_01_03,point_02_03,point_03_03],sid_33).



shape_id(Shape,ShapeID):- is_shape_id_for(Shape,ShapeID),!.
shape_id(Shape,ShapeID):- term_hash(Shape,Hash), atomic_list_concat(['s',Hash],ShapeID), asserta(is_shape_id_for(Shape,ShapeID)).

% eqq

:- dynamic(is_iv_for/2).
iv_for(L,Iv):- copy_term(L,CT,_),numbervars(CT,0,_,[attvar(bind),singletons(true)]),term_hash(CT,Fv),
 number(Fv), Iv is (Fv rem 800) + 1,!. % (\+ is_iv_for(Iv,_) -> asserta_if_new(is_iv_for(Iv,L)) ; true).

obj_iv(Obj,Iv):- indv_u_props(Obj,L),iv_for(L,Iv).

obj_to_oid(Obj,OID):-  atom(Obj),atom_length(Obj,L),L>5,Obj=OID,!.
obj_to_oid(Obj,OID):-  oid_glyph_object(OID,_,Obj),!.
obj_to_oid(I,X):- var_check(I,obj_to_oid(I,X))*->!;indv_props(I,L),member(obj_to_oid(X),L).
obj_to_oid(Obj,OID):- assert_object_oid(_,Obj,_Glyph,OID),!.

current_gid(GID):- get_vm(VM), GID = VM.gid.
   %tid_to_gids(ID,GID),!.

oid_to_object(OID,Obj):- oid_glyph_object(OID,_,Obj).



obj_to_oid(Obj,_,MyID):- obj_to_oid(Obj,MyID).
tid_to_gid(TID,GID):- is_grid(TID),!,grid_to_gid(TID,GID).
tid_to_gid(TID,GID):- var(TID),!,current_gid(GID).
tid_to_gid(TID,GID):- tid_to_gids(TID,GID),!.

%o2g_f(Obj,Glyph):-  atom(Obj),atom_length(Obj,1),Obj=Glyph,!.
o2g_f(Obj,Glyph):- oid_glyph_object(_,Glyph,Obj),!.
o2g_f(Obj,Glyph):- assert_object_oid(_,Obj,Glyph,_OID).

assert_object_oid(TID,Obj,Glyph,OID):-     
 must_det_ll((
   tid_to_gid(TID,GID),
   is_object(Obj),
   obj_iv(Obj,Iv), int2glyph(Iv,Glyph), % object_glyph(Obj,Glyph),       
   atomic_list_concat(['o_',Glyph,'_',GID],OID),
   retractall(oid_glyph_object(OID,_,_)),
   arc_assert_fast(oid_glyph_object(OID,Glyph,Obj)),
   retractall(gid_glyph_oid(GID,Glyph,_)), retractall(gid_glyph_oid(GID,_,OID)),
   arc_assert_fast(gid_glyph_oid(GID,Glyph,OID)),
   assert_object2(OID,Obj))).

assert_object2(OID,obj(List)):-!,maplist(assert_object2(OID),List).
assert_object2(OID,List):- is_list(List),!,maplist(assert_object2(OID),List).
assert_object2(OID,Prop):- Prop=..[F|List], append(Pre,[Last],List),
  assert_object5(OID,F,Pre,Last,List).
assert_object5(OID,F,Pre,Last,_List):- 
  AProp=..[cindv,OID,F,Pre,Last],
  % (\+ ground(AProp)->dumpST;true),
  arc_assert1(AProp).

arc_assert1(A):- assertz_if_new(A),!.
arc_assert1(A):- arc_assert_fast(A).
arc_assert_fast(A):- assertz_if_new(A),!.
 
retract_object(GID,OID,_):- 
 retractall(gid_glyph_oid(GID,_,OID)),
 retractall(cindv(OID,_,_)),
 retractall(cindv(OID,_,_,_)),
 retractall(cindv(OID,_,_,_,_)).
/*
assert_object(GID,NewObj):-
  obj_to_oid(NewObj,NOID),
  assert_object(GID,NOID,NewObj).

assert_object(GID,OID,NewObj):- 
  tid_to_gid(GID,GOID),
  o2g(NewObj,Glyph),
  retract_object(GID,OID,_),
  pfcAdd(gid_glyph_oid(GOID,Glyph,OID)),
  assert_object1(OID,NewObj).

assert_object1(OID,obj(List)):-!,maplist(assert_object1(OID),List).
assert_object1(OID,List):- is_list(List),!,maplist(assert_object1(OID),List).
assert_object1(OID,Prop):- Prop=..List, AProp=..[cindv,OID|List],
  (\+ ground(AProp)->dumpST;true),
  pfcAdd(AProp).
*/


%obj_to_oid(I,_,Iv):- is_object(I), obj_iv(I,Iv).
%obj_to_oid(I,_ID,Fv):- is_grid(I),!, flag(indiv,Fv,Fv+1).
%obj_to_oid(I,ID,Iv):- indv_props(I,L),member(obj_to_oid(ID,Iv),L),!.
%obj_to_oid(I,ID,Fv):- into_obj(I,O),!,obj_to_oid(O,ID,Fv).
%obj_to_oid(I,ID,Iv):- trace_or_throw(missing(obj_to_oid(I,ID,Iv))).
%obj_to_oid(_,ID,_Iv):- luser_getval(test_pairname,ID).

amass(I,Count):- acmass(I,Count).

acmass(I,Count):- is_grid(I),!,globalpoints(I,Points), length(Points,Count),!.
acmass(I,X):- var_check(I,acmass(I,X)).
acmass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(acmass,[G|Grid],MPoints),sum_list(MPoints,Points).
acmass(I,X):- indv_props(I,L),member(acmass(X),L),!.
acmass(I,XX):- is_object(I),!,must_det_ll((localpoints(I,L), length(L,X))),!,XX=X.
%acmass(I,X):- is_object(I),!,must_det_ll((indv_props(I,L), member(acmass(X),L))).
acmass(Points,Count):- is_list(Points),length(Points,Count),!.
acmass(I,Count):- globalpoints(I,Points),!,length(Points,Count),!.
acmass(C-_,1):- nonvar_or_ci(C),!.
%acmass(I,Count):- globalpoints(I,Points), length(Points,Count),!.

omass(I,X):- indv_props(I,L),member(mass(X),L),!.
omass(I,XX):- is_object(I),!,must_det_ll((localpoints(I,L), mass(L,X))),!,XX=X.

mass(I,X):- indv_props(I,L),member(mass(X),L),!.
mass(C,0):- (is_bg_color(C);var(C);C==[]),!.
mass(I,XX) :- is_object(I), omass(I,X) ,!, (X<2 -> acmass(I,XX) ; XX = X).
mass(I,Count):- is_grid(I),!,append(I,Cs),!,mass(Cs,Count),!.
mass(C,1):- is_fg_color(C),!.
mass(I,X):- var_check(I,mass(I,X)),!.
mass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(mass,[G|Grid],MPoints),sum_list(MPoints,Points).
mass(C-_,M):- !,mass(C,M).
mass([G|Grid],Points):- maplist(mass,[G|Grid],MPoints),sum_list(MPoints,Points),!.
mass(I,Count):- globalpoints(I,Points),mass(Points,Count),!.




%remove_color(C-_,point_01_01):- is_bg_color(C),!.
%remove_color(_-P,P).
%remove_color(LPoints,ColorlessPoints):- mapgroup(remove_color,LPoints,ColorlessPoints).

:- decl_pt(setq(object,prop,object)).

setq(Orig,Todo,Result):- is_object(Orig),!,override_object(Todo,Orig,Result).
setq(Orig,Todo,Result):- metaq(setq_1,Orig,Todo,Result).
setq_1(_Old,New,Saved):- Saved=New.

delq(I,E,O):-is_object(I),!, with_object(delq,E,I,O).
delq(Orig,Todo,Result):- metaq(delq_1,Orig,Todo,Result).
delq_1(_Old,_New,Saved):- Saved=delq.


metaq(_,Orig,[],Orig):-!.
metaq(P3,Orig,[New|Todo],Result):- !, metaq(P3,Orig,New,Midway),metaq(P3,Midway,Todo,Result).
metaq(P3,Orig,New,Saved):- functor(New,F,A),functor(Old,F,A),Did=done(nil),map_pred(metaq_1(P3,Did,Old,New),Orig,Saved).
metaq_1(_,done(t),_,_,Orig,Orig):-!.
metaq_1(P3,Did,Old,New,Orig,Saved):- compound(Orig),Orig=Old, call(P3,Old,New,Saved),nb_setarg(1,Did,t).

enum_group(S):- is_unshared_saved(_,S).


:- decl_pt(helper,indv_props(is_object,+)).
%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
indv_props(C,LL):- \+ compound(C),!,nonvar(C),g2o(C,I),indv_props(I,LL).
indv_props(C,LL):- C=obj(L),!,(is_list(L)->LL=L ; (copy_term(L,LL),append(LL,[],LL))),!.
indv_props(C,LL):- arc_expand_arg(C,I,G),call(G),!,indv_props(I,LL).
indv_props_for_noteablity(obj(L),Notes):- my_assertion(nonvar(L)),!, include(is_prop_for_noteablity,L,Notes).

%is_not_prop_for_noteablity(globalpoints).
%is_not_prop_for_noteablity(grid_size).
%is_not_prop_for_noteablity(obj_to_oid).

%indv_props(G,L):- arcST,trace,into_obj(G,O),is_object(O),indv_props(O,L).

pmember(E,X):- X=obj(L),!,indv_props(X,L),member(E,L).
pmember(E,X):- sub_term(EE,X),nonvar_or_ci(EE),EE=E,ground(E).
/*pmember(E,L):- is_map(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
pmember(E,L):- member(EE,L),(EE=E;(is_list(EE),pmember(E,EE))).
pmember(E,L):- member(obj(EE),L),pmember(E,EE).
*/

walls_thick1(G):- localpoints(G,Points),counted_neighbours(Points,ListOfSizes),walls_thick1_sizes(ListOfSizes).
walls_thick1_sizes(List):- mapgroup(size2D(2),List).

size2D(A,A):- A==A.

maybe_localpoints_list(I,_):- is_points_list(I),!,fail.
maybe_localpoints_list(I,X):- localpoints(I,X).

counted_neighbours(G,CountOut):- maybe_localpoints_list(G,List),!,counted_neighbours(List,CountOut).
counted_neighbours([],[]):-!.
counted_neighbours([_-C],[0-C]):-!.
counted_neighbours(List,CountOut):- counted_neighbours(List,[],CountOut).
counted_neighbours(List,CountIn,CountsOut):- counted_neighbours(List,List,CountIn,CountsOut).

colors_join(C,CC):- C==CC,!.
colors_join(C,CC):- is_bg_color(C),!,is_bg_color(CC).
colors_join(CC,C):- is_bg_color(C),!,is_bg_color(CC).
colors_join(C,CC):- (plain_var(C);plain_var(CC)),!,fail.
colors_join(C,CC):- is_color(C),is_color(CC),!,fail.
colors_join(C,CC):- (is_color(C);is_color(CC)),!.
%colors_join(_,_)



counted_neighbours([],_,CountInOut,CountInOut):-!.
counted_neighbours([H|T],List,CountIn,CountsOut):-!,
  counted_neighbours(H,List,CountIn,CountsM),
  counted_neighbours(T,List,CountsM,CountsOut).
counted_neighbours(C-HV,List,CountIn,[P|CountIn]):- 
 findall(Dir,(is_adjacent_point(HV,Dir,HV2),Dir\==c,member(CC-HV2,List),colors_join(C,CC)),Ns),
  length(Ns,I),P = I-HV.

var_check(I,_):- is_grid(I),!,fail.
var_check(I,_):- I==[],!,fail.
var_check(I,G):- resolve_reference(I,O),I\==O,!,subst001(G,I,O,GG),GG\==G,!,call(GG).
var_check(I,G):- var(I),!,(enum_object(I)*->G;var_check_throw(I,G)).
%var_check(I,G):- var(I),!,var_check_throw(I,G).
var_check_throw(I,G):- var(I),wdmsg(error(var(G))),!,arcST,wdmsg(error(var(G))),break,trace_or_throw(maybe_enum_i(I,G)),call(G).

object_shapeW(I,X):- compound(I),I=obj(L),!,my_assertion(is_list(L)),!,member(iz(X),L).
object_shapeW(I,X):- indv_props(I,L),!,member(iz(X),L).

isz(I,X):- is_list(I),I=[O],!,isz(O,X).
isz(I,X):- var_check(I,isz(I,X))*->true;(indv_props(I,L),member(iz(X),L)).

obj_prop_val(I,X):- var_check(I,obj_prop_val(I,X))*->true;(indv_props(I,L),member(X,L)).

vm_to_printable(D,R) :- Objs = D.objs,!, (Objs\==[] -> R = Objs; R = D.grid ).

resolve_reference(R,Var):- is_map(R),!,Objs = R.objs,!, (Objs\==[] -> Var=Objs; Var = R.grid).
resolve_reference(R,Var):- compound(R),arc_expand_arg(R,Var,Goal),!,call(Goal).
resolve_reference(R,Var):- arc_expand_atom(R,Var),!.
resolve_reference(R,Var):- nonvar(R),R\=obj(_),known_gridoid(R,Var),!.

rot2L(G,X):- is_group(G),!,mapgroup(rot2L,G,Points),append_sets(Points,X).
rot2L(I,X):- var_check(I,rot2L(I,X)).
rot2L(I,X):- indv_props(I,L),member(rot2L(X),L).
rot2L(_,sameR).

object_changes(G,X):- is_group(G),!,mapgroup(object_changes,G,Points),append_sets(Points,X).
object_changes(I,X):- indv_props(I,L),member(changes(X),L).


%hv_cvalue(Grid,Color,H,V):- hv_cg_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- plain_var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar_or_ci(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).


% Is there an advantage to counting down?
all_points_between(_Grid,_LowH,_LowV,_GH,GV,_Hi,Vi,Points,Points):- Vi>GV,!.
all_points_between(Grid,LowH,LowV,GH,GV,Hi,Vi,PointsIn,PointsO):-
  (Hi>GH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),
   all_points_between(Grid,LowH,LowV,GH,GV,H,V,PointsIn,Points),
  ((hv_c_value(Grid,C,Hi,Vi), hv_point(Hi,Vi,Point)) -> PointsO = [C-Point|Points] ; PointsO = Points).


color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C,Hi,Vi),!.
/*
color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_fg_color(C2,C);(attvar(C2),C=C2); (\+ plain_var(C2), C=C2)),!,
  get_bgc(BGC),C\==BGC.
*/

% Is there an advantage to counting down?
all_points_between_include_bg(_Grid,_LowH,_LowV,_GH,GV,_Hi,Vi,Points,Points):- Vi>GV,!.
all_points_between_include_bg(Grid,LowH,LowV,GH,GV,Hi,Vi,Points,PointsO):-
  ((color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi),
  hv_point(Hi,Vi,Point))
     -> PointsT = [C-Point|Points] ; PointsT = Points),
   (Hi>GH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),!,
   all_points_between_include_bg(Grid,LowH,LowV,GH,GV,H,V,PointsT,PointsO).

color_spec_or_fail_include_bg(Grid,C,Hi,Vi):-
  hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(var(C2),fail,C=C2)).

color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi):- 
  get_bgc(BGC),
  hv_c_value_or(Grid,C2,Hi,Vi,BGC),
  (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(var(C2),C=BGC)).
  
grid_cpoint(Grid,C-Point,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
 (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(var(C2),C=C2)),
  hv_point(Hi,Vi,Point).

grid_to_points(Grid,Points):- grid_size(Grid,HH,HV),!, grid_to_points(Grid,HH,HV,Points).
% Is there an advantage to counting down?
grid_to_points(Grid,HH,HV,Points):- all_points_between(Grid,1,1,HH,HV,1,1,[],Points),!. 
% Is there an advantage to counting down?
grid_to_points_include_bg(Grid,Points):- grid_size(Grid,HH,HV),!,all_points_between_include_bg(Grid,1,1,HH,HV,1,1,[],Points),!. 
/*
grid_to_points(Grid,HH,HV,Points):-  trace_or_throw(all_points_between),
  findall(C-Point,(between(1,HV,V),between(1,HH,H),
    once((hv_cg_value(Grid,C2,H,V),
          %pp(hv_cg_value(C2,H,V)),
          is_spec_fg_color(C2,C),
          hv_point(H,V,Point)))),Points),!.
*/
point_corners(Obj,Dir,CPoint):- enum_object(Obj),  globalpoints(Obj,Points), gp_point_corners(Obj,Points,Dir,CPoint).


gp_point_corners(Obj,_Points0,Dir,CPoint):-  %sort_points(Points,SPoints), 
   isz(Obj,Shape),SPoints=Points,
   shape(Obj,Points),member('+'-P1,Points),localpoints(Obj,CPoints),member(C-P1,CPoints),
   C-P1 = CPoint,
  (points_corner_dir(Shape,Dir)*->(SPoints=[CPoint|_];last(SPoints,CPoint));fail).
   

%globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), no_repeats(ID,cmem(ID,_,_)), findall(-(C,HV),cmem(ID,HV,C),Points).
%globalpoints(Grid,Points):- grid_to_gid(Grid,ID),\+ \+ cmem(ID,_,_),findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(I,_):-  var(I),!,fail.
globalpoints(Grid,Points):- is_grid(Grid),!, grid_to_points(Grid,Points).
globalpoints(I,X):-  (var_check(I,globalpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
globalpoints([],[]):-!.
globalpoints(G,[G]):- is_cpoint(G),!.
globalpoints(C-P,[C-P]):-!.
globalpoints(G,G):- is_cpoints_list(G).
globalpoints(Grid,Points):- is_group(Grid),!,mapgroup(globalpoints,Grid,MPoints),append_sets(MPoints,Points).
globalpoints(G,Ps):- is_map(G),vm_to_printable(G,R),!,globalpoints(R,Ps).
globalpoints(options(X),_Points):- trace_or_throw(globalpoints(options(X))).
globalpoints(I,X):- indv_props(I,L),member(globalpoints(X),L), nop(my_assertion(is_cpoints_list(X))),!.
globalpoints(I,G):- is_object(I),object_localpoints(I,L),is_points_list(L),loc2D(I,X,Y),offset_points(X,Y,L,G),!.
%globalpoints(I,X):- localpoints(I,X),!.
globalpoints(_,[]):-!.
globalpoints(Atom,_):- \+ compound(Atom),!,my_assertion(gp=Atom),trace_or_throw(globalpoints(Atom)).
globalpoints(I,X):- my_assertion(gp=I), trace_or_throw(unknown(globalpoints(I,X))).

localpoints(G,G):- is_cpoints_list(G),!.
localpoints(I,X):- is_object(I),!,must_det_ll(object_localpoints(I,X)),!.
localpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), grid_to_points(Grid,HH,VV,Points).
localpoints(I,X):- (var_check(I,localpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
localpoints(G,[G]):- is_point(G),!.
localpoints(G,Ps):- is_map(G),vm_to_printable(G,R),!,localpoints(R,Ps).
localpoints(Grid,Points):- is_group(Grid),!,mapgroup(localpoints,Grid,MPoints),append_sets(MPoints,Points).
localpoints(G,G):- is_points_list(G).
localpoints(Grid,Points):- is_list(Grid),!,maplist(localpoints,Grid,MPoints),append_sets(MPoints,Points).
%localpoints(Atom,_):- \+ compound(Atom),!,trace_or_throw(localpoints(Atom)).
localpoints(I,X):- trace,trace_or_throw(unknown(localpoints(I,X))).

  %localpoints(I,X):- into_grid(I,G),!,grid_size(G,H,V),grid_to_points(G,H,V,X).

must_det_11(G):- must_det_ll(call(call,(G))).

localpoints_include_bg(I,X):- must_be_free(X),  \+ is_grid(I), !, must_det_11((localpoints(I,X),is_cpoints_list(X))),!.
localpoints_include_bg(Grid,Points):- is_grid(Grid),!, must_det_11((grid_to_points_include_bg(Grid,Points),is_cpoints_list(Points))),!.

object_localpoints(I,XX):- must_be_free(XX), 
 must_det_ll((indv_props(I,L), object_localpoints0(I,L,XX),is_cpoints_list(XX))),!.

object_localpoints0(_,L,X):- member(localpoints(X),L),is_list(X),!.
object_localpoints0(I,L,X):- member(globalpoints(XX),L),is_list(XX),loc2D(I,LoH,LoV),!,deoffset_points(LoH,LoV,XX,X).
object_localpoints0(I,_L,XX):-  
 must_det_ll((shape(I,X), rot2L(I,Rot), shape2D(I,H,V), pen(I,PenColors),
   make_localpoints(X,Rot,H,V,PenColors,XX))).

make_localpoints(X,Rot,H,V,PenColors,XX):- 
  must_det_ll((   maybe_unrotate_points(H,V,X,Rot,XXX),
     combine_pen(XXX,PenColors,PenColors,XX) )),!.

maybe_unrotate_points(_,_,X,sameR,XX):- must_be_free(XX),!,X=XX.
maybe_unrotate_points(H,V,X,Rot,XX):- must_det_ll((points_to_grid(H,V,X,Grid),   
   unrotate(Rot,Grid,Grid90),localpoints_include_bg(Grid90,XX))).

combine_pen([],_,_,[]):-!.

combine_pen([_-L|LL],C,Reset,XX):- nonvar(L),!,combine_pen([L|LL],C,Reset,XX).
%combine_pen([_-P1|L],C,Reset,[C-P1|XX]):- is_color(C),!,
combine_pen([P1|L],C,Reset,[C-P1|XX]):- is_color(C),!,
  combine_pen(L,C,Reset,XX).

combine_pen(X,[],Reset,XX):-!,combine_pen(X,Reset,Reset,XX).
combine_pen(L,[cc(C,N)|PenColors],Reset,XX):- number(N), make_list(C,N,List),append(List,PenColors,ListPenColors),
  combine_pen(L,ListPenColors,Reset,XX).
combine_pen([P1|L],[C|PenColors],Reset,[C-P1|XX]):- is_color(C),!,
  combine_pen(L,PenColors,Reset,XX).
  

shape(I,X):- is_object(I), indv_props(I,L),member(shape(X),L).
shape(G,X):- is_group(G),!,mapgroup(shape,G,Points),append_sets(Points,X).
% returns the objects decolorize localpoints
shape(I,ColorlessPoints):- into_grid(I,Grid),grid_to_shape(Grid,_RotG,_SH,_SV,ColorlessPoints,_PenColors).
  %localpoints(I,Points), grid_to_shape(Points,X).

%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_tid(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
*/

%colors(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.

all_colors(I,X):- nonvar(X),!,all_colors(I,XX),!,X=XX.
all_colors(G,[cc(Black,0)]):-  G==[],!,get_black(Black).
all_colors(I,X):- indv_props(I,L),functor(CC,cc,2),findall(CC,member(CC,L),X),!.
all_colors(I,X):- is_map(I),into_grid(I,G),!,all_colors(G,X).
all_colors(I,X):- is_object(I),indv_u_props(I,L),member(localpoints(LP),L),!,all_colors_via_pixels(LP,X).
all_colors(G,BFO):- localpoints_include_bg(G,G0), all_colors_via_pixels(G0,BFO),!.
all_colors(G,BFO):- all_colors_via_pixels(G,BFO),!.

colors(O,CCO):- all_colors(O,CC), into_mostly_real_colors(CC,CCO),!.
into_mostly_real_colors(CC,CCO):- include(is_real_cc,CC,CCO),CCO\==[],!.
into_mostly_real_colors(CC,CCO):- include(is_some_cc,CC,CCO),CCO\==[],!.
into_mostly_real_colors(CC,CC):- !.
is_real_cc(cc(C,N)):- N>0, is_real_color(C),!.
is_some_cc(cc(_,N)):- N>0,!.


all_colors_via_pixels(G,CC):-
  pixel_colors(G,All), 
  findall(Nm-C,(enum_colors_test(C),occurs:count((sub_term(Sub, All), \+ \+ cmatch(C,Sub)), Nm), 
    once(Nm\==0 ; (atom(C), C\==is_colorish, C\==var, \+ is_real_color(C)))),BF),
  keysort(BF,KS),reverse(KS,SK),
  into_cc(SK,CC),!.
  
%all_colors_via_pixels(G,BFO):- quietly((pixel_colors(G,GF),count_sets(GF,_,SK),into_cc(SK,BFO))).

count_sets(GF,GS,SK):-
  list_to_set(GF,GS),
  count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!.

  
%colors(G,X):- is_group(G),!,mapgroup(colors,G,Points),append_sets(Points,X).


get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).

pen(I,C):- indv_props(I,L),member(pen(C),L),!.


object_grid(I,G):- is_grid(I),!,G=I.
object_grid(Group,List):- is_group(Group),!,override_group(object_grid(Group,List)),!.
object_grid(ObjRef,List):- \+ is_object(ObjRef), into_obj(ObjRef,Obj),!,object_grid(Obj,List).
object_grid(I,G):- indv_props(I,L),member(grid(G),L),!.
object_grid(I,G):- odd_failure((localpoints(I,LP),vis2D(I,H,V),points_to_grid(H,V,LP,G))),!.

global_grid(I,G):- is_grid(I),!,G=I.
global_grid(ObjRef,List):- \+ is_object(ObjRef), into_obj(ObjRef,Obj),!,global_grid(Obj,List).
global_grid(I,G):- must_det_ll((call((grid_size(I,H,V),globalpoints_maybe_bg(I,LP),points_to_grid(H,V,LP,G))))),!.
global_grid(I,G):- object_grid(I,G),!.
%object_grid(I,G):- globalpoints(I,GP),into_grid(GP,G),!.

locG_term(I,loc2G(X,Y)):- loc2G(I,X,Y),!.
loc2G(Grid,H,V):- is_grid(Grid),!,other_grid_size(Grid,H,V).
loc2G(G,X,Y):- is_group(G),!,mapgroup(locG_term,G,Offsets),sort(Offsets,[loc2G(X,Y)|_]). % lowest loc2G
%loc2G(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc2G(I,X,Y):- is_object(I), indv_props(I,L),member(loc2G(X,Y),L),!.
loc2G(I,X,Y):- into_obj(I,O), indv_props(O,L),member(loc2G(X,Y),L),!.


loc_term(I,loc2D(X,Y)):- loc2D(I,X,Y),!.
loc2D(Grid,H,V):- is_grid(Grid),!,H=1,V=1.
loc2D(G,X,Y):- is_group(G),!,mapgroup(loc_term,G,Offsets),sort(Offsets,[loc2D(X,Y)|_]). % lowest loc2D
%loc2D(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc2D(I,X,Y):- is_object(I), indv_props(I,L),member(loc2D(X,Y),L),!.
loc2D(I,X,Y):- into_obj(I,O), indv_props(O,L),member(loc2D(X,Y),L),!.
%loc2D(NT,H,V):- trace, known_gridoid(NT,G),loc2D(G,H,V).


:- decl_pt(prop_g,vis_hv_term(is_object_or_grid,size2D)).

vis_hv_term(I,size2D(X,Y)):- vis2D(I,X,Y),!.

vis2D(Grid,H,V):- is_grid(Grid),!,grid_size(Grid,H,V).
vis2D(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
vis2D(I,X,Y):- indv_props(I,L),member(vis2D(X,Y),L),!.
vis2D(G,X,Y):- is_group(G),!,mapgroup(vis_hv_term,G,Offsets),sort(Offsets,HighToLow),last(HighToLow,size2D(X,Y)).
vis2D(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
vis2D(NT,H,V):-  known_gridoid(NT,G),G\==NT, vis2D(G,H,V).

shape2D(I,X,Y):- indv_props(I,L),member(shape2D(X,Y),L),!.
shape2D(Grid,H,V):- is_grid(Grid),!,grav_roll(Grid,_RotG,RotShape),grid_size(RotShape,H,V).
shape2D(NT,H,V):-  into_gridoid(NT,G),G\==NT, shape2D(G,H,V).

%vis2D(Obj,size2D(H,V)):- vis2D(Obj,H,V).
%loc2D(Obj,loc2D(H,V)):- loc2D(Obj,H,V).

center_term(Obj,loc2D(H,V)):- center2G(Obj,H,V).

:- decl_pt(prop_g,hw_rat(is_object_or_grid,size2D)).
hw_rat(Obj,HV):- vis2D(Obj,OH,OV), HV is rationalize(OH/OV).

center2G(I,X,Y):- is_grid(I), !, grid_size(I,H,V),X is floor(H/2),Y is floor(V/2).
center2G(I,X,Y):- indv_props(I,L),member(center2G(X,Y),L),!.
center2G(I,X,Y):- indv_props(I,L),member(iz(cenX(X)),L),member(iz(cenY(Y)),L),!.
%center2G(Obj,CCX,CCY):- vis2D(Obj,H,V), loc2D(Obj,X,Y),CCX is X + floor(H/2),CCY is Y + floor(V/2).


object_color(HV,C):- color(HV,C).

color(HV,C):- colors(HV,[cc(C,_)]),!.
color(HV,multicolor(Stuff)):- colors(HV,Stuff),!.

main_color(HV,C):- colors(HV,[cc(C,_)|_]).
first_gpoint(HV,P):- globalpoints(HV,[P|_]).
last_gpoint(HV,P):- globalpoints(HV,PS),last(PS,P).
any_gpoint(HV,P):- globalpoints(HV,L),member(P,L).

rebuild_from_localpoints(Obj,NewObj):-
  localpoints_include_bg(Obj,Points),
  rebuild_from_localpoints(Obj,Points,NewObj).

rebuild_from_localpoints(Obj,WithPoints,NewObj):-
 get_vm(VM),
 must_det_ll((
  localpoints_include_bg(WithPoints,Points),

  localpoints_include_bg(Obj,PrevPoints),

  (Points=@=PrevPoints -> (NewObj=Obj) ;

 (rot2L(Obj,Rot),unrotate_p2(Rot,UnRot),
  loc2D(Obj,X,Y),%vis2D(Obj,H,V),  
  %obj_to_oid(Obj,ID,_Iv),
  %uncast_grid_to_object(Orig,Grid,NewObj),
  points_to_grid(Points,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints_include_bg(UnRotGrid,LPoints),
  offset_points(X,Y,LPoints,GPoints),
  indv_props(Obj,Props),my_partition(is_point_or_colored,Props,_,PropsRetained),
  remObjects(VM,Obj),
  make_indiv_object(VM,[loc2D(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],GPoints,NewObj))))),
   verify_object(NewObj),
  !.

:- style_check(-singleton).
/*
@TODO
?-  into_obj(t('1b60fb0c')>(trn+1)*in,X),blur_p2(rot90,X,Y),print_grid(Y).

   _____________________
  |                     |
  |       R R R R R     |
  |       R R R R R     |
  |           R     R R |
  |           R     R R |
  |           R R R R R |
  |           R     R R |
  |           R     R R |
  |       R R R R R     |
  |       R R R R R     |
   ---------------------
   _____________________
  |                     |
  |                     |
  |                     |
  | R R           R R   |
  | R R           R R   |
  | R R R R R R R R R   |
  | R R     R     R R   |
  | R R     R     R R   |
  |     R R R R R       |
  |     R R R R R       |
   ---------------------
   _____________________
  |                     |
  |       R R R R R     |
  |       R R R R R     |
  | R R       R   R R R |
  | R R       R   R R R |
  | R R R R R R R R R R |
  | R R     R R   R R R |
  | R R     R R   R R R |
  |     R R R R R R     |
  |     R R R R R R     |
   ---------------------

*/
blur_p2(P2,Obj,NewObj):-  
  into_obj(Obj,X),
  %center2G(X,XCH,XCV),
  
  call(P2,X,Y),!,
  globalpoints(X,XGP),
  globalpoints(Y,YGP),
  pct((
  vis2D(X,XH,XV),
  vis2D(Y,YH,YV),
  grid_size(X,XGH,XGV),
  grid_size(Y,YGH,YGV),
  % center2G(Y,YCH,YCV),
  loc2D(X,XX,XY),
  loc2D(Y,YX,YY))),
  print_grid(XGH,XGV,XGP),
  print_grid(YGH,YGV,YGP),
  append(XGP,YGP,XYGP),
  must_det_ll(rebuild_from_globalpoints(Obj,XYGP,NewObj)).
:- style_check(+singleton).

pct(G):- call(G), ppt(G).

%rebuild_from_glob alpoints(Obj,NewObj):-
%  globalpoints_in clude_bg(Obj,GPoints),
%  rebuild_from_l ocalpoints(Obj,GPoints,NewObj).

rebuild_from_globalpoints(Obj,GPoints,NewObj):-
 must_det_ll((
  
  indv_props(Obj,Props),my_partition(is_point_or_colored,Props,_,PropsRetained),
  peek_vm(VM),
  
  %ppa(before=Obj),
  remObjects(VM,Obj),
  make_indiv_object(VM,PropsRetained,GPoints,NewObj),
  %ppa(propsRetained=PropsRetained),
  %ppa(after=NewObj),
    verify_object(NewObj))),
 !.



is_point_or_colored(birth(_)):-!,fail.
is_point_or_colored(Prop):- sub_term(CP,Prop),(is_color(CP);is_nc_point(CP)),!.
is_point_or_colored(Prop):-
 member(Prop,[cc(_),amass(_),mass(_),shape(_),rot2L(_),roll_shape(_),pen(_),norm_grid(_),norm_ops(_),
              iz(multicolored(_)),iz(chromatic(_,_)),iz(symmetry(_)),iz(shape(_)),iz(monochrome),
              globalpoints(_),localpoints(_)]),!.


%illegal_for_outlines(HV1,e,sw,w
%print_pairs(

dirs_ok0(n,e,ne). dirs_ok0(s,e,se).
dirs_ok0(n,w,nw). dirs_ok0(s,w,sw).
dirs_ok(N,E,NE):- dirs_ok0(N,E,NE).
dirs_ok(E,N,NE):- dirs_ok0(N,E,NE).

dir_not_ok(N,E,NE):- dirs_ok(N,E,NE),!,fail.
dir_not_ok(_,_,_).

%two_dirs(Dir1,Dir2):- select(Dir1,[n,s,e,w,sw,ne,se,nw],Rest),member(Dir2,Rest).
%two_dirs(Dir1,Dir2):- select(Dir1,[n,s,e,w],Rest),member(Dir2,Rest).
%two_dirs(Dir1,Dir2):-member(Dir1,[n,s,e,w]),member(Dir2,[n,s,e,w]).
two_dirs(Dir1,Dir2):-member(Dir1,[n,s,e,w,sw,ne,se,nw]),member(Dir2,[n,s,e,w,sw,ne,se,nw]).

find_outline(X):-
  X = 
  [[black,green,green,black],
   [green,black,black,green],
   [green,black,black,green],
   [black,green,green,black]].

find_outline(X):-
  X = 
  [[green,green,green,green],
   [green,black,black,green],
   [green,black,black,green],
   [green,green,green,green]].

:- arc_history(test_find_outline).
test_find_outline:- clsmake, forall(find_outline1,true).
find_outline1:- arc_grid(_,Grid), dash_chars, once(find_outline1(Grid)).
find_outline1(Grid):- find_outline_pred(find_outlines_fast(_),Grid).



%find_outline_path:- clsmake, forall(find_outline_path1,true).
%find_outline_path1:- arc_grid(Grid), dash_chars, find_outline_pred(find_outline_sols(find_outline_path),Grid).

find_outlines(Grid):- find_outline_pred(find_outlines3,Grid).
%find_outline_paths(Grid):- find_outline_pred(find_outline_sols(find_outline_path),Grid).

fail_over_time(Secs,Goal,Else):- 
 locally(set_prolog_flag(runtime_must,keep_going),
   locally(nb_setval(cant_rrtrace,t),
    notrace(catch(call_with_time_limit(Secs,Goal),time_limit_exceeded,(Else,fail))))).
fail_over_time(Secs,Goal):- fail_over_time(Secs,Goal,true).

grid_of(LO,LO,[]):- is_grid(LO),!.
grid_of(LO,O,H):- arg(1,LO,O),arg(2,LO,H).
find_outline_pred(P3,Grid):- is_grid(Grid),!,
   grid_to_tid(Grid,ID),   
   grid_size(Grid,H,V),
   writeln(ID),
   set_current_test(ID),
   fail_over_time(6,call(P3,Grid,SOLS,LeftOver),
     (writeln("TOO SLOWWWW"=P3),
      print_grid(H,V,Grid),
      writeln("TOO SLOWWWW"=P3))),
   my_append(SOLS,[leftover(LeftOver)],SOLSL),
   dash_chars,
   (SOLS==[]-> ((writeln("NOTHING FROM"=P3),ignore((fail,catch((print_grid(H,V,ID,Grid),writeln("NOTTAAA"=P3),!,fail),_,fail)))));
   member(OL,SOLSL),
   (OL=leftover(LeftOver)
    -> (nl,
      ignore((nonvar(LeftOver),nl,write(leftover=P3),print_grid(H,V,LeftOver))),
      write('from '),write(P3),print_grid(H,V,Grid))
    ;((grid_of(OL,O,Hits),once((amass(O,M))),
     nl,write(solution(Hits,M)), print_grid(H,V,O))))).
find_outline_pred(ID):- into_grid(ID,Grid),find_outlines(Grid).

%arc_grid(Nth,X):- (var(Nth)->Nth=1;true), offset(Nth,arc_grid(X)).
%find_outline(Grid,Result,Grid4):- is_grid(Grid),!,grid_to_points(Grid,Points),!,find_outline(Points,Result,Grid4).

/*
find_outline(Grid,Sol,Rest):-
  find_outlines3(Grid,Sols,Rest),!,
  member(Sol,Sols).
  */
fix_rest(Options,Rest0,Sols0,Rest,Sols):- 
  select(C-HV,Rest0,Rest1),  
  \+ (is_adjacent_point(HV,_,TVH),member(C-TVH,Rest1)),
  select(sol(O,Hits),Sols0,Sols1),
  is_adjacent_point(HV,_,HV2),member(C-HV2,O),!,
 fix_rest(Options,Rest1,[sol([HV|O],[HV|Hits])|Sols1],Rest,Sols).
fix_rest(_Options,Rest,Sols,Rest,Sols).


find_outlines_fast(Options,Grid,Sols,Rest):-
 localpoints(Grid,Points),
 point_groups_by_color(colormass,Groups,Points,Rest2), !,
% mapgroup(print_grid(H,V),Groups),
 find_group_outlines(Options,Groups,Sols,Rest1),
 my_append(Rest1,Rest2,Rest).

find_group_outlines(Options,[G1|G2],Sols,Rest):-!,
  find_group_outlines_fix_rest(Options,G1,Sols1,Rest1),
  find_group_outlines(Options,G2,Sols2,Rest2),
  my_append(Sols1,Sols2,Sols), my_append(Rest1,Rest2,Rest).
find_group_outlines(_,[],[],[]).

find_group_outlines_fix_rest(Options,G,Sols,Rest):-
  find_outlines(Options,G,Sols0,Rest0),!,
  fix_rest(Options,Rest0,Sols0,Rest,Sols).


find_outlines3(Grid,Solutions,Rest):- 
  as_localpoints(Grid,Points),
  find_outlines([perfect],Points,Solutions,Rest).

find_outlines(Options,Grid,[Sol|Solutions],Rest):-
  find_outlinez(Options,Grid,Sol,More),!,
  find_outlines(Options,More,Solutions,Rest).
find_outlines(_,Rest,[],Rest).
  
as_localpoints(Grid,Points):- is_grid(Grid),!,localpoints(Grid,Points).
as_localpoints(Points,Points).

:- discontiguous(find_outlinez/4).

three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4):-
  is_adjacent_point(HV1,Dir1,HV2),select(C-HV2,Grid2,Grid3),
  %w_in_90(Dir1,Dir2),
  is_adjacent_point(HV2,Dir2,HV3),select(C-HV3,Grid3,Grid4).
  
find_outlinez([_Perfect|_],Grid,sol(ResultO,[]),LeftOver):-  !, fail, % Perfect = perfect, 
  as_localpoints(Grid,Points),
  select(C-HV1,Points,Grid2),
  w_in_n(Dir1,Dir2),
  three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4),
 \+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)),
  outline_only2(HV1,C,HV3,[C-HV1|Grid4],Result,LeftOver), [C-HV1,C-HV2|Result] = ResultO, Result=[_,_,_|_].
  

outline_only2(Exit,_,HV2,Grid,[],Grid) :- HV2==Exit,!.
outline_only2(Exit,C,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver) :-
  w_in_n(Dir1,Dir2),
  three_points(C,Grid2,HV2,Dir1,HV3,Dir2,HV4,Grid4),
  \+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)),
  outline_only2(Exit,C,HV4,Grid4,Found,LeftOver).


nav_three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4):-
  is_adjacent_point(HV1,Dir1,HV2), select(C-HV2,Grid2,Grid3),
  after_dir_check(Dir1,Dir2,Dir3),
  is_adjacent_point(HV2,Dir2,HV3), select(C-HV3,Grid3,Grid4),
  (\+ is_adjacent_point(HV,Dir3,HV), member(C-HV,Grid4)).

outline_nav(Exit,_,_,HV2,Grid,[],Grid) :- HV2==Exit,!.
outline_nav(Exit,C,Dir1,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver) :-
  nav_three_points(C,Grid2,HV2,Dir1,HV3,Dir2,HV4,Grid4),
  outline_nav(Exit,C,Dir2,HV4,Grid4,Found,LeftOver).

w_in_n(_,_).

find_outlinez(_,Grid,sol(ResultO,[Hits]),LeftOver):-  !,
  localpoints(Grid,Points),  
  select(C-HV1,Points,Grid2),  
  w_in_90(Dir1,Dir2),
  three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4),
  \+ (is_adjacent_point(HV2,_,HV),member(C-HV,Grid4)),
  (outline_only(HV1,C,HV3,[C-HV1|Grid4],Result,LeftOver,0,Hits), [C-HV1,C-HV2|Result] = ResultO, Result=[_,_,_|_]).

find_outlinez(_Opts,Grid,sol(ResultO,[]),LeftOver):- 
  localpoints(Grid,Points),
  select(C-HV1,Points,Grid2),  
  w_in_90(Dir1,Dir2),
  nav_three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4),
  ((outline_nav(HV1,C,Dir2,HV3,[C-HV1|Grid4],Result,LeftOver), [C-HV1,C-HV2|Result] = ResultO,Result=[_,_,_,_|_])).


outline_only(Exit,_,HV2,Grid,[],Grid,N,N) :- HV2==Exit,!.

outline_only(Exit,C,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver,N,O) :-
  w_in_n(Dir1,Dir2),
  three_points(C,Grid2,HV2,Dir1,HV3,Dir2,HV4,Grid4),
 (\+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)) ->
   (M is N -1,outline_only(Exit,C,HV4,Grid4,Found,LeftOver,M,O));
   (M is N +1,outline_onlya(Exit,C,HV4,Grid4,Found,LeftOver,M,O))).

outline_onlya(Exit,C,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver,N,O) :-
  w_in_n(Dir1,Dir2),
  three_points(C,Grid2,HV2,Dir1,HV3,Dir2,HV4,Grid4),
 (\+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)) ->
   (M is N -1,outline_onlya(Exit,C,HV4,Grid4,Found,LeftOver,M,O));
   (M is N +1,outline_onlyb(Exit,C,HV4,Grid4,Found,LeftOver,M,O))).

outline_onlyb(Exit,C,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver,N,O) :-
  w_in_n(Dir1,Dir2),
  three_points(C,Grid2,HV2,Dir1,HV3,Dir2,HV4,Grid4),
 (\+ (n_s_e_w(Dir),is_adjacent_point(HV2,Dir,HV), member(C-HV,Grid4)) ->
   (M is N -1,outline_only(Exit,C,HV4,Grid4,Found,LeftOver,M,O))).





is_jagged(Points):- member(C-HV,Points),
  findall(Dir,(is_adjacent_point(HV,Dir,HV2),Dir\==c,member(C-HV2,Points)),L),L=[_,_],!.

solidity(Points,Res):- 
  solidness(Points,2,inf,HVNs),
  findall(N=C,
    (between(2,8,N),findall(_,member(N-_HV,HVNs),L),length(L,C)),Res).



solidness(Points,Lo,H,Res):- 
 findall(N-HV,
  (member(C-HV,Points),findall(Dir,(is_adjacent_point(HV,Dir,HV2),Dir\==c,
   member(C-HV2,Points)),L),length(L,N1),N is N1-1,between(Lo,H,N)),Res).

solidness_no_diag(Points,Lo,H,Res):- 
 findall(N-HV,
  (member(C-HV,Points),findall(Dir,(is_adjacent_point(HV,Dir,HV2),\+ is_diag(Dir),Dir\==c,
   member(C-HV2,Points)),L),length(L,N1),N is N1-1,between(Lo,H,N)),Res).

solidness_is_diag(Points,Lo,H,Res):- 
 findall(N-HV,
  (member(C-HV,Points),findall(Dir,(is_adjacent_point(HV,Dir,HV2), is_diag(Dir),
   member(C-HV2,Points)),L),length(L,N1),N is N1+1,between(Lo,H,N)),Res).

%guess_shape(GH,GV,G,LocalGrid,I,O,N,H,V,Colors,Points,walls_thick(1)):- walls_thick1(G).
/*
guess_shape(GH,GV,GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.loc2D=loc2D(LoH,LoV),
   make_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   compute_shared_indivs(Grid,InvS))),!,
   InvS=[_,_|_].
*/


find_outline_sols(P5,Grid,[Sol|Solutions],Rest):-
  enum_colors(C),
  find_outline_pred_call(P5,C,Grid,Sol,More),!,
  find_outline_sols(P5,More,Solutions,Rest).
find_outline_sols(_,Rest,[],Rest).

find_outline_pred_call(P5,C,Grid,ResultO,LeftOverO):-
  call(P5,C,3,Grid,ResultO,LeftOverO).

find_outline_path2(C,L,Grid,sol(ResultO,[]),LeftOverO):- 
  localpoints(Grid,Points),
  my_partition(=(C-_),Points,Grid1,Others),
  select(C-HV1,Grid1,Grid2),  
  nav_three_points(C,Grid2,HV1,Dir1,HV2,Dir2,HV3,Grid4),
  %print_grid([blue-HV1,yellow-HV2,red-HV3|Grid4]), 
  (((outline_nav(HV1,C,Dir2,HV3,[C-HV1|Grid4],Result,LeftOver), [C-HV1,C-HV2|Result] = ResultO,once(length(Result,M)),M>L)
   ;(outline_nav(HV3,C,Dir1,HV1,[C-HV3|Grid4],Result,LeftOver), [C-HV3,C-HV2|Result] = ResultO,once(length(Result,M)),M>L))),
  my_append(Others,LeftOver,LeftOverO).


object_to_nm_grid(Obj,SSP):-
  vis2D(Obj,H,V),
  localpoints(Obj,LPoints),
  points_to_grid(H,V,LPoints,GridIn),
  grid_to_gridmap(GridIn,SSP),
  print_grid(SSP).

fast_arc.

%neighbor_map
grid_to_gridmap(GridIn,GridIn):- fast_arc,!.
grid_to_gridmap(GridIn,GridON):- 
  neighbor_map(GridIn,GridO),
  mapgrid(only_neib_data,GridO,GridON),
  %subst_1L(['*'-'.','~'-'red','+'-'blue','.'-'yellow'],GridON,SSP),
  nop((subst_1L(['*'-'.'],GridON,_SSP))).

object_to_nm_points(Obj,NPoints):-
  object_to_nm_grid(Obj,SSP),
  localpoints(SSP,NPoints),!.

:- style_check(-singleton).
:- style_check(-discontiguous).

cc_fg_count(Colors,Len):- include(\=(cc(_,0)),Colors,NonZero),maplist(arg(1),NonZero,Colorz),include(is_real_fg_color,Colorz,FGC),length(FGC,Len).
cc_bg_count(Colors,Len):- include(\=(cc(_,0)),Colors,NonZero),maplist(arg(1),NonZero,Colorz),include(is_real_bg_color,Colorz,FGC),length(FGC,Len).

guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,1,GV,Colors,Points,column).
guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,GH,1,Colors,Points,row).
guess_shape(GH,GV,I,0,N,N,1,Colors,Points,hv_line(h)):- N > 1.
guess_shape(GH,GV,I,0,N,1,N,Colors,Points,hv_line(v)):- N > 1.

guess_shape(GH,GV,GridIn,LocalGrid,I,Empty,N,H,V,[cc(Black,_)|Rest],Points, bfc(FGB)):-  Rest == [], ((is_bg_color(Black);Black==wbg)->FGB=bg;FGB=fg).
%guess_shape(GH,GV,GridIn,LocalGrid,I,Empty,N,H,V,[cc(Black,_)|Rest],Points, fbc(FGB)):- Rest == [], ((is_fg_color(Black);Black==wfg)->FGB=fg;FGB=bg).
%guess_shape(GH,GV,GridIn,LocalGrid,I,0,N,H,V,Colors,Points,view_sq):- H == V.%guess_shape(GH,GV,GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangle):- H>1, V>1.
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,chromatic(Len,BGLen)):- cc_fg_count(Colors,Len),cc_bg_count(Colors,BGLen).
 
%guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,monochrome):- Colors=[_],length(Colors,Len).
guess_shape(GH,GV,GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad).

guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,H,V,Colors,Points,poly(Keypad)):- 
  once(guess_shape_poly(I,E,N,H,V,Colors,Points,Keypad)).

guess_shape_poly(I,0,1,1,1,Colors,Points,dot):-!.
guess_shape_poly(I,_,_,_,_,Colors,[Point],dot):-!.
guess_shape_poly(I,0,N,H,V,Colors,Points,rectangulator):- N>1, H\==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,square):- N>1,H==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,solid):- N > 1.
%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0,once(H>1;V>1).

guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,R):- N>=2, iz_symmetry(GridIn,R).


%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,solidity(A)):- solidity(Points,A).
%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,Solid):- (is_jagged(Points)->Solid=jagged(true);Solid=jagged(false)).
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outl2):- H>2,V>2,N>4,
  once((grid_to_gridmap(GridIn,GridON), \+ member('*',GridON), member('.',GridON))).
  
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outline(SN)):- H>2,V>2,N>4,
  (find_outlines3(Points,Sol,Rest)->(length(Sol,SN),SN>0,length(Rest,RN))),!.

guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outl):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).

guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,fp(NPoints)):- fail,
  grid_to_gridmap(GridIn,GridON),
  subst_1L(['~'-'red','+'-'blue','.'-'yellow'],GridON,SSP),
  localpoints(SSP,NPoints),!.
  %clumped(ON,COO),!,maplist(arg(1),COO,PAT).
  %points_to_grid(H,V,FGridO,RGridO).

:- style_check(+singleton).

flipSym_checks(Rot90,GridIn):-
  copy_term(GridIn,G,_),!,
  flipSym(Rot90,G).

%iz_symmetry(GridIn,H,V,N,R):- N == 2, H==1, 
iz_symmetry(GridIn,R):-
  (flipSym_checks(SN,GridIn)*->R=symmetry(SN);R=symmetry(none)).

symmetric_types(Any,QQ):- into_grid(Any,Grid), 
  findall(Q,(rot_p(Q),flipSym_checks(Q,Grid)),QQ).

:- meta_predicate(flipSym(-,+)).
flipSym(Rot90,Grid):- \+ground(Rot90),!,rot_p_plus_full(Rot90),flipSym(Rot90,Grid).
flipSym(  full,Grid):- flipSym(flipH,Grid),flipSym(flipV,Grid), flipSym(rot90,Grid),!.
flipSym(sym_hv,Grid):- flipSym(flipH,Grid),flipSym(flipV,Grid).
flipSym(Rot90,Grid):-  full \== Rot90,sym_hv \== Rot90, grid_call_unbound_p1(rot_p2,Rot90,Grid,LocalGridM),Grid=@=LocalGridM.

grid_call_unbound_p1(P1,Rot90,GridIn,GridOut):- var(Rot90),!,call(P1,Rot90),grid_call_unbound_p1(P1,Rot90,GridIn,GridOut).
grid_call_unbound_p1(P1,and(A,B),GridIn,GridOut):- !, grid_call_unbound_p1(P1,A,GridIn,GridM), grid_call_unbound_p1(P1,B,GridM,GridOut).
grid_call_unbound_p1(P1,[A],GridIn,GridOut):- !, grid_call_unbound_p1(P1,A,GridIn,GridOut).
grid_call_unbound_p1(P1,[A|B],GridIn,GridOut):- !, grid_call_unbound_p1(P1,A,GridIn,GridM), grid_call_unbound_p1(P1,B,GridM,GridOut).
grid_call_unbound_p1(_P1,Call,GridIn,GridOut):- grid_call(Call,GridIn,GridOut).

trim_topside_v(G,GG):- arg(_,v([],[_],[_,_]),L),arg(_,v([],[_],[_,_]),R),append([L,GG,R],G),flipSym_checks(flipV,GG),!.
trim_topside_v(G,G).

rot_p_plus_full(full).
rot_p_plus_full(sym_hv).
rot_p_plus_full(P):- rot_p(P).

rot_p(and(maybe_trim_outside,P)):- rot_p2(P).
rot_p(and(maybe_trim_to_rect,P)):- rot_p2(P).
rot_p(P):- rot_p2(P).

rot_p2(flipDHV). rot_p2(flipDH). rot_p2(flipDV). rot_p2(flipD). 
rot_p2(rot90). rot_p2(flipH). rot_p2(flipV). rot_p2(rot180). rot_p2(rot270).

maybe_trim_to_rect(G,GG):- trim_to_rect(G,GG),!,G\=@=GG.
maybe_trim_outside(G,GG):- trim_outside(G,GG),!,G\=@=GG.
trim_outside(G,GG):- grid_call([trim_topside_v,rot90,trim_topside_v,rot270],G,GG).

%rot_p(P):- rot_p1(P).
%rot_p(and(trim_to_rect,P)):- rot_p1(P).
%rot_p(and(into_bicolor,P)):- rot_p2(P).

%rot_p1(and(into_monochrome,P)):- rot_p2(P).


:- dynamic(individuated_cache/3).
:- retractall(individuated_cache(_,_,_)).
:- dynamic(is_why_grouped/4).
:- retractall(is_why_grouped_g(_,_,_,_)).


:- include(kaggle_arc_footer).


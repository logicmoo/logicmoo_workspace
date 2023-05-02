:- encoding(iso_latin_1 ).
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

redress_override(L,L):- var(L),!.
redress_override(L,LL):- select(unkept(_),L,LLL),!,redress_override(LLL,LL).
redress_override(L,LL):- select(kept(E),L,LLL),!,redress_override([E|LLL],LL).
redress_override(L,LL):- is_list(L),!,my_maplist(redress_override,L,LL).
redress_override(birth(I),iz(info(birth(I)))):-!.
redress_override(iz(birth(I)),iz(info(birth(I)))):-!.
redress_override(iz(I),iz(IT)):- iz_type(I,IT),!.
redress_override(iz(I),iz(info(I))):- atom(I),!.
redress_override(info(I),iz(info(I))):-!.
%redress_override(I,iz(IT)):- iz_type(I,IT),!.
redress_override(I,I).


iz_type(I,IT):-iz_type0(I,ITT),iz_type0(ITT,IT).
iz_type0(I,IT):- compound(I),functor(I,F,_),if_atype(F,FF),IT=..[FF,I].
iz_type0(I,IT):- atomic(I),if_atype(I,F),IT=..[F,I].
%iz_type0(I,IT):- compound(I),functor(I,F,_),if_atype(_,F),IT=I.
iz_type0(I,I).

if_atype(always_keep,flag).
if_atype(hidden,flag).
if_atype(nsew,indiv).
if_atype(indiv,info).
if_atype(birth,info).
if_atype(colomass,indiv).
if_btype(info).
if_btype(flag).



gpoints_to_iv(GPoints,Iv):-
  gpoints_to_iv_info(GPoints,_ColorlessPoints,_LocX,_LocY,_PenColors,_RotG,Iv,[],_LPoints,_Grid,_SH,_SV,_SizeY,_SizeX,_CentX,_CentY).
%     GPoints,ShapePoints,LocX,LocY,PenColors,RotG,Iv,LPoints,Grid,OX,OY,SizeX,SizeY,CentX,CentY)


gpoints_to_iv_info(Cmpd,ShapePoints,LocX,LocY,PenColors,RotG,Iv,_Overrides,[C-HV11],[[C]],OX,OY,SizeX,SizeY,CentX,CentY):-
    is_list(Cmpd),Cmpd=[CP], compound(CP),
    (CP=(C-P)),!,
    SizeX=1,SizeY=1,
    ShapePoints=[HV11],
    RotG=sameR,
    OX=1,OY=1,
    CentX=LocX,CentY=LocY,
    hv_point(LocX,LocY,P),
    PenColors=[cc(C,1)],hv_point(1,1,HV11),
    lpoints_to_iv_info(ShapePoints,LocX,LocY,PenColors,RotG,Iv).


gpoints_to_iv_info(GPoints,ShapePoints,LocX,LocY,PenColors,RotG,Iv,Overrides,LPoints,Grid,OX,OY,SizeX,SizeY,CentX,CentY):-
  %points_range(GPoints,LocX,LocY,HiH,HiV,_HO,_VO), once(member(vis2D(SizeX,SizeY),Overrides);(SizeX is HiH-LocX+1,SizeY is HiV-LocY+1)),
  must_det_ll((
  po_loc2D_vis2D(GPoints,Overrides,LocX,LocY,SizeX,SizeY),  
  deoffset_points(LocX,LocY,GPoints,LPoints),
  make_grid(SizeX,SizeY,Grid),
  physical_points(LPoints,PLPoints),
  %writeg([plPoints=PLPoints,grid=Grid]),
  add_global_points(PLPoints,Grid,Grid),
  %writeg([nowgrid=Grid]),
  ShapePoints=[_|_],
  into_solid_grid(Grid,Solid),
  %print_ss(grid_to_shape,Grid,Solid),
  grid_to_shape(Solid,RotG,OX,OY,ShapePoints,PenColors),
  
  %writeg([then=Grid]),
  lpoints_to_iv_info(ShapePoints,LocX,LocY,PenColors,RotG,Iv),
  gpoints_to_center(GPoints,LocX,LocY,SizeX,SizeY,CentX,CentY))).

lpoints_to_iv_info(ShapePoints,LocX,LocY,PenColors,RotG,Iv):- 
  L=[ shape_rep(grav,ShapePoints),  loc2D(LocX,LocY),  pen(PenColors),  rot2D(RotG)],
  iv_for(L,Iv).

%fg_shape_grid(RC,_):- plain_var(RC).
%fg_shape_grid(RC,fg):- is_real_color(RC), \+ is_bg_color(RC).
%fg_shape_grid(fg,_).

gpoints_to_center(GPoints,LocX,LocY,SizeX,SizeY,CentX,CentY):-
  % calc center2G
  must_det_ll(once(
   ((member(UFgPoints,[GPoints]),
    ((CentX is LocX + floor(SizeX/2),CentY is LocY + floor(SizeY/2), hv_point(CentX,CentY,Point), member(_-Point,UFgPoints));
    (length(UFgPoints,UFgLen),CP is round(UFgLen/2), nth1(CP,UFgPoints,Point),hv_point(CentX,CentY,Point)))));
   (CentX is LocX + floor(SizeX/2),CentY is LocY + floor(SizeY/2)))).

add_prop_list(O,Props):- nb_set_add(O,Props).
add_prop(O,N,V):- Prop=..[N,V],nb_set_add1(O,Prop).
add_prop_with_count(O,N,V):- my_assertion(is_list(V)),add_prop(O,N,V),
  length(V,Len),atom_concat(N,'_count',NN),add_prop(O,NN,Len).

real_colors(GPoints,Cs):- 
 findall(C,(sub_term(C,GPoints),is_real_color(C),is_fg_color(C)),L),list_to_set(L,S),!,Cs=S.


:- style_check(+singleton).
make_indiv_object_s(GID0,GridH,GridV,Overrides0,GPoints00,ObjO):- 
 fix_point_colors(GPoints00,GPoints0),
 must_be_nonvar(GID0),
 testid_name_num_io(GID0,TestID,Example,Num,IO),
 %must_be_nonvar(IO),
 testid_name_num_io_gid(TestID,Example,Num,IO,GIDR),
 GID0=GID,
 physical_points(GPoints0,GPointsM),
 nop((Wfg = fg)),
 ((nonvar(Wfg),real_colors(GPointsM,[FG1]))->subst(GPointsM,FG1,Wfg,GPoints);GPointsM=GPoints),
 my_assertion(is_list([overrides|Overrides0])),
 my_assertion(is_cpoints_list(GPoints)),
  %luser_getval(test_pairname,ID),
 must_det_ll((
  redress_override(Overrides0,Overrides),
  gpoints_to_iv_info(GPoints0,ShapePoints,LocX,LocY,PenColors,RotG,Iv,Overrides,LPoints0,Grid,OX,OY,SizeX,SizeY,CentX,CentY),
  physical_points(LPoints0,LPoints),

  %writeg([gpoints0=GPoints0,lpoints=LPoints,shapePoints(RotG,OX,OY)=ShapePoints]),
  if_t(ShapePoints==[],
    (pp([make_indiv_object_s(GID0,GridH,GridV,Overrides0,GPoints0,ObjO),lPoints=LPoints]),
    ibreak)),
  make_localpoints(ShapePoints,RotG,OX,OY,PenColors,_CheckLPoints),
  %sort_safe(LPoints0,LPoints0S),
  %CheckLPoints=LPoints0S,
  %writeg([checkLPoints=CheckLPoints]),


  %add_prop(PropL,globalpoints,GPoints),
  %add_prop(PropL,localpoints,LPoints),
  %add_prop(PropL,colorlesspoints,ShapePoints),

  length(GPoints,Len),
  Area is SizeX * SizeY,
  Empty is Area - Len,
  PropL = [area(Area)],
  %add_prop(PropL,area,Area),
  add_prop(PropL,empty_area,Empty),

  asserta(special_sizes(SizeX,SizeY)),
  
  pixel_colors(GPoints,Pixels),
  



  pixels_to_cc(Pixels,CC), %add_prop_list(PropL,CC),

  sort_safe(Pixels,UniqueColors),  
  add_prop_with_count(PropL,unique_colors,UniqueColors),

  %my_partition(is_fg_color,UniqueColors,FGC,NotFGC),
  %my_partition(is_bg_color,NotFGC,BGC,OtherC),
  /*
  add_prop_with_count(PropL,unique_fg_colors,FGC),
  add_prop_with_count(PropL,unique_bg_colors,BGC),
  add_prop_with_count(PropL,unique_other_colors,OtherC),*/



  %my_maplist(on_edge(GridH,GridV),GPoints,EdgeL),count_sets([n,s,e,w,c|EdgeL],_,EdgeC),my_maplist(zero_one_more,EdgeC,EdgeS),
  
  once(member(grid(LocalGrid),Overrides);LocalGrid=Grid),


  ignore((member(iz(media(shaped)),Overrides),
      % \+ member(iz(media(image)),Overrides),
     learn_hybrid_shape(LocalGrid))),

  other_grid_size(GID,OtherH,OtherV),
  to_global_coord(LocX,GridH,OtherH,LocXG,GlobalXG),to_global_coord(LocY,GridV,OtherV,LocYG,GlobalYG),
  to_global_coord(CentX,GridH,OtherH,CentXG,_),to_global_coord(CentY,GridV,OtherV,CentYG,_),
  to_global_coord(SizeX,GridH,OtherH,SizeXG,_),to_global_coord(SizeY,GridV,OtherV,SizeYG,_),
  BottemX is LocX + SizeX, BottemY is LocY + SizeY,
  to_global_coord(BottemX,GridH,OtherH,BottemXG,_),to_global_coord(BottemY,GridV,OtherV,BottemYG,_),
  copy_term(Grid,GridInCopy),

  %grid_to_gridmap(Grid,ShapePoints), 

  findall(ShapeName,
   (guess_shape(GridH,GridV,Grid,LocalGrid,Ps,Empty,Len,SizeX,SizeY,CC,LPoints,ShapeName),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),ShapeNamesUF),
  flatten([ShapeNamesUF],ShapeNamesU),list_to_set(ShapeNamesU,ShapeNames),
  my_maplist(append_term(iz),ShapeNames,OShapeNames),

  %my_assertion('LocX=<GridH',LocX=<GridH),
  %my_assertion('LocY=<GridV',LocY=<GridV),

  % rotated local points 
  /*(memberchk(rot2D(RotOut),Overrides)-> FinalLocalPoints=LPoints;
    must_det_ll((tips_to_rot(LPoints,GridH,GridV,RotOut,Final),localpoints(Final,FinalLocalPoints)))),
  delistify_single_element(RotOut,RotO),
  */
  %grid_to_shape(Grid,RotG,OX,OY,ShapePoints,PenColors),
  
  %shape_id(NormMonoLocalGrid,MonoNormShapeID),
 % RE=ADD=PHASE2 into_ngrid(Grid,NGrid),ngrid_syms(NGrid,Syms),
 % RE=ADD=PHASE2 
  normalize_grid(NormOps,Grid,NormGrid),local_shape_id(NormGrid,NormSID),
  compress_grid(CompOps,NormGrid,CompGrid),local_shape_id(CompGrid,CompSID),
  %writeg([normgrid=NormGrid]), 
  %if_t([[black,_]]=@=NormGrid,atrace),
% RE=ADD=PHASE2   localpoints(NormGrid,NormLPoints),my_maplist(arg(2),NormLPoints,ShapeNormLPoints),
 
% RE=ADD=PHASE2
  %  
  % NormShapeID=ShapeID,
  shape_id(ShapePoints,ShapeID),
  ((fail,select(oid(OID),Overrides,NewOverrides),
     atomic_list_concat(['o',GlyphOMem,IvOMem|MustGID],'_',OID),
     atomic_list_concat(MustGID,'_',GIDOMem),GIDOMem==GID,IvOMem=Iv,GlyphOMem=Glyph,OIDOMem=OID) 
      -> true ;
        (NewOverrides=Overrides,
         %PenColors = [cc(C1,_)|_],new_obj_points(GID,obj,C1,GPoints,Len,OID),
         lpoints_to_iv_info(ShapePoints,LocX,LocY,PenColors,RotG,Iv), 
  int2glyph(Iv,Glyph), % object_glyph(Obj,Glyph),       
         atomic_list_concat(['o',Glyph,Iv,GIDR],'_',OID))),

  
  %grav_rot(Grid,NormOps,NormGrid),
  my_maplist(ignore,[GIDOMem==GID,IvOMem=Iv,GlyphOMem=Glyph,OIDOMem=OID]),
  (PenColors == [cc(Wfg,1)] -> PenColorsR = [cc(FG1,1)] ; PenColorsR = PenColors),
  flatten(
  [ PropL,
    shape_rep(grav,ShapePoints),    
    pen(PenColorsR),
    rot2D(RotG),
    rotSize2D(grav,OX,OY),

    loc2D(LocX,LocY), 
    unkept(loc2G(LocXG,LocYG)),
    kept(center2D(CentX,CentY)),
    kept(center2G(CentXG,CentYG)),
    iz(cenGY(CentYG)),iz(cenGX(CentXG)),  
    unkept(bottem2D(BottemX,BottemY)),unkept(bottem2G(BottemXG,BottemYG)),
    vis2D(SizeX,SizeY), 
    %kept(iz(vis2G(SizeXG))), %
    iz(sizeGY(SizeYG)),iz(sizeGX(SizeXG)),

    global2G(GlobalXG,GlobalYG),
  % RE=ADD=PHASE2 iz(mono_algo_sid(norm,MonoNormShapeID)),    
    iz(sid(ShapeID)),
    %ngrid(NGrid),

        
    mass(Len),
    CC,        
  % RE=ADD=PHASE2  Syms,
    localpoints(LPoints),

    %mass(Len),
    
    %width(SizeX), height(SizeY), area(Area), %missing(Empty),
    changes([]),
    grid(LocalGrid),    
    OShapeNames,
    
  % RE=ADD=PHASE2 
    [grid_rep(norm,NormGrid),grid_ops(norm,NormOps),iz(algo_sid(norm,NormSID))],
    [grid_rep(comp,CompGrid),grid_ops(comp,CompOps),iz(algo_sid(comp,CompSID))],
    %[iz(locY(LocY)),iz(locX(LocX))], % iz(tall(SizeY)),iz(wide(SizeX)),
    
    %obj_to_oid(ID,Iv),
    
    kept(giz(g(IO))), kept(giz(testid(TestID))), kept(giz(example_num(Example+Num))),
    kept(giz(testid_example_io(TestID>(Example+Num)*IO))),       
    %unkept(giz(gid_omem(GIDOMem))), unkept(giz(oid_omem(OIDOMem))), unkept(iv_omem(IvOMem)),giz(glyph_omem(GlyphOMem)),
    kept(oid(OID)),kept(giz(iv(Iv))),kept(giz(glyph(Glyph))),kept(giz(gid(GIDR))),kept(giz(gido(GID))),
    

    globalpoints(GPointsM),
    giz(grid_sz(GridH,GridV)),
    []],Ps00),  
   redress_override(Ps00,Ps0),
  include('\\=='([]),Ps0,Ps),

  %make_localpoints(ShapePoints,RotG,OX,OY,PenColors,XX), assertion((XX == LPoints)),

  with_objprops(override,NewOverrides,Ps,OUT1),
  sort_obj_props(OUT1,OUT),!,as_obj(OUT,Obj),verify_object(Obj),!,
  remove_gridoid_props(Obj,ObjM),
 must_det_ll((ObjM = ObjO)))).

remove_gridoid_props(O,O):- !.
remove_gridoid_props(O,O):- \+ compound(O),!.
remove_gridoid_props(obj(List),obj(ListO)):- !, remove_gridoid_props(List,ListO).
remove_gridoid_props(O,O):- is_grid(O),!.
remove_gridoid_props(globalpoints(O),globalpoints(O)):-!.
remove_gridoid_props(localpoints(O),localpoints(O)):-!.
remove_gridoid_props([O|Obj],ObjM):- is_list(O),!,maplist(remove_gridoid_props,[O|Obj],ObjM).
remove_gridoid_props([O|Obj],ObjM):- is_object(O),!,maplist(remove_gridoid_props,[O|Obj],ObjM).
remove_gridoid_props([O|Obj],ObjM):- is_prop1(O),arg(_,O,E),is_grid_or_points(E),!,remove_gridoid_props(Obj,ObjM).
remove_gridoid_props([O|Obj],[O|ObjM]):- is_prop1(O),!,remove_gridoid_props(Obj,ObjM).

is_grid_or_points(E):- is_grid(E),!.
is_grid_or_points(E):- is_points_list(E),!.

:- use_module(library(clpfd)).


deoffset_points(1,1,Points,PointsO):-!,Points=PointsO.
deoffset_points(OH,OV,Point,LPoint):- map_pred(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- is_ncpoint(Point), hv_point(H,V,Point),
  HH is H -OH +1, VV is V - OV +1,
  hv_point(HH,VV,LPoint).

offset_points(1,1,Points,PointsO):-!,Points=PointsO.
offset_points(OH,OV,Point,LPoint):- map_pred(if_point_offset0(OH-1,OV-1),Point,LPoint).
offset_points0(0,0,Points,PointsO):-!,Points=PointsO.
offset_points0(OH,OV,Point,LPoint):- map_pred(if_point_offset0(OH,OV),Point,LPoint).

if_point_offset0(OH,OV,Point,LPoint):- is_ncpoint(Point), hv_point(H,V,Point),HH is H +OH , VV is V + OV,hv_point(HH,VV,LPoint).

%offset_point0(OH,OV,Point,LPoint):- is_ncpoint(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).
%offset_point0(OH,OV,C-Point,C-LPoint):- is_ncpoint(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

obj_example_num(O,EN):- indv_props(O,giz(example_num(EN))).
obj_testid(O,EN):- indv_props(O,giz(testid(EN))).
obj_test_example_io(O,TestIDEN_IO):- indv_props(O,giz(testid_example_io(TestIDEN_IO))).

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
  Obj = obj( [ mass(0),
         shape_rep(grav,[]),
         colors_cc([]),
         localpoints([]), vis2D(H, V), 
         rot2D(sameR), 
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

%enum_object0(Obj):- oid_glyph_object(_,_,Obj).

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
is_fg_point(CPoint):- \+ \+ (only_color_data(CPoint,Color),is_fg_color(Color)).

rev_key(C-P,P-C).
rev_key(List,ListO):- is_list(List),!,my_maplist(rev_key,List,ListO).

sort_points(P0,P2):- 
   (P0==[] -> (rrtrace(true)) ; true),
   my_assertion(is_list(P0)),
   sort_safe(P0,P1),my_assertion(P1\==[]), 
   my_assertion(is_cpoints_list(P1)),  
   my_maplist(rev_key,P1,R1),keysort(R1,R2),my_maplist(rev_key,R2,P2).

%same_globalpoints(O1,O2):-  globalpoints_include_bg(O1,P1),same_globalpoints_ps_obj(P1,O2).

same_globalpoints_and_window(I,O):-
  %get_loc2D_vis2D(I,P1,H1,V1,OH1,OV1),
  %get_loc2D_vis2D(O,P2,H2,V2,OH2,OV2),!,  
  %P1=@=P2,H1=H2,V1=V2, OH1=OH2,OV1=OV2,
  loc2D(I,X1,Y1),loc2D(O,X2,Y2),X1=X2,Y1=Y2,
  vis2D(I,X1a,Y1a),vis2D(O,X2a,Y2a),X1a=X2a,Y1a=Y2a,
  shape_rep(grav,I,P1),shape_rep(grav,O,P2), P1=@=P2.
 
same_globalpoints_ovrs_ps_obj(Overrides,P1,O2):-
  po_loc2D_vis2D(P1,Overrides,H1,V1,OH1,OV1),
  get_loc2D_vis2D(O2,P2,H2,V2,OH2,OV2),
  P1=@=P2,H1=H2,V1=V2, OH1=OH2,OV1=OV2.

po_loc2D_vis2D(GPoints,Overrides,LocX,LocY,SizeX,SizeY):- is_list(Overrides), 
  select(loc2D(LocX,LocY),Overrides,Overrides0),!,
  po_loc2D_vis2D(GPoints,Overrides0,_,_,SizeX,SizeY).

po_loc2D_vis2D(GPoints,Overrides,LocX,LocY,SizeX,SizeY):- is_list(Overrides), 
  select(vis2D(SizeX,SizeY),Overrides,Overrides0),!,
  po_loc2D_vis2D(GPoints,Overrides0,LocX,LocY,_,_).

%po_loc2D_vis2D(GPoints,_Overrides,LocX,LocY,SizeX,SizeY):- points_range(GPoints,_LocX,_LocY,_HiH,_HiV,SizeX,SizeY).
po_loc2D_vis2D(GPoints,_Overrides,LocX,LocY,SizeX,SizeY):- 
  points_range(GPoints,LocX,LocY,HiH,HiV,_HO,_VO),
  SizeX #= HiH-LocX+1,SizeY #= HiV-LocY+1. 

get_loc2D_vis2D(O2,P2,H2,V2,OH2,OV2):- 
  globalpoints(O2,P2), loc2D(O2,H2,V2), vis2D(O2,OH2,OV2),!.

%get_loc2D_vis2D( O2,P2,_Props2,H2,V2,OH2,OV2):- nonvar(O2), (var(P2)-> globalpoints_include_bg(O2,P2) ; true), loc2D(O2,H2,V2), vis2D(O2,OH2,OV2),!.
%get_loc2D_vis2D(_O2,P2, Props2,H2,V2,OH2,OV2):- po_loc2D_vis2D(P2,Props2,H2,V2,OH2,OV2),!.
%get_loc2D_vis2D( O2,P2, Props2,H2,V2,OH2,OV2):- var(Props2), indv_props(O2,Props2), po_loc2D_vis2D(P2,Props2,H2,V2,OH2,OV2).



ensure_indiv_object(VM,IPoints,Obj):- 
  ((compound(IPoints), IPoints = obj(_)) -> (IPoints = Obj, nop(addObjects(VM,Obj)));
   (my_partition(is_cpoint,IPoints,Points,Overrides),
    u_dmsg(po(Points,Overrides)),
    make_indiv_object(VM,Overrides,Points,Obj))).

make_point_object(VM,Overrides,C-Point,Obj):- never_newd(C),
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
 NX #= X+OX-1, NY #= Y+OY-1, 
  locally(nb_setval(global_offset,loc2D(NX,NY)),Goal).
  
fix_global_offset(Points,PointsO):- nb_current(global_offset,loc2D(X,Y)),!, offset_points(X,Y,Points,PointsO).
fix_global_offset(GOPoints,OPoints):- GOPoints=OPoints,!.

make_indiv_object(VM,Overrides,GOPoints,NewObj):-
 must_det_ll((
  make_indiv_object_real(VM,Overrides,GOPoints,NewObj),
  %show_indiv(make_indiv_object,NewObj),
  nop(itrace))).
 %show_indiv_textinfo(NewObj),!.

make_indiv_object_real(VM,Overrides,GOPoints,NewObj):-
 
 fix_global_offset(GOPoints,OPoints),
 must_det_ll((
  GOPoints\==[],
  globalpoints_maybe_bg(OPoints,GPoints),
  sort_points(GPoints,Points),
  Objs = VM.objs,
  Orig = _,!,
  must_det_ll(((select(Orig,Objs,Rest), \+ is_whole_grid(Orig), same_globalpoints_ovrs_ps_obj(Overrides,Points,Orig))
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

grav_roll(Shape,SameR,Shape):- SameR == sameR,!.
%grav_roll([[Y]],sameR,[[Y]]):- nonvar(Y),!.
grav_roll(LPoints,RotG,Shape):- 
  must_det_ll((grav_rot(LPoints,RotG,Shape),
  undo_effect(RotG,Shape,LPoints0),LPoints0=LPoints)).

protect_black(Grid,PGrid,UndoProtect):-
  Grid=PGrid, UndoProtect = (=).

fix_point_colors(RShapeA,RShapeAR):-
  maplist(arg(1),RShapeA,Cs),list_to_set(Cs,CCS),
  (CCS==[bg]->subst(RShapeA,bg,black,RShapeAR);RShapeA=RShapeAR),!.
fix_point_colors(RShapeA,RShapeA):-!.

grid_to_shape([[Dot]],sameR,1,1,[point_01_01],[cc(Dot,1)]):-!.
  
grid_to_shape(Grid,RotG,OX,OY,ShapePoints,PenColors):-
%  writeg([grid=Grid]),
 must_det_ll((
  protect_black(Grid,PGrid,UndoProtect),
  grav_roll(PGrid,RotG,PRotShape),
%  writeg([pGrid2=PGrid,pRotShape=PRotShape]),
  call(UndoProtect,PRotShape,RotShape),
  globalpoints_include_bg(RotShape,RShapeA),
  fix_point_colors(RShapeA,RShapeAR),
  physical_points(RShapeAR,RShape),
  include(ground,RShape,LShape),  
  grid_size(RotShape,OX,OY),
  % colors_cc 
  %writeg([rotShape=RotShape,lShape=LShape]),
  my_maplist(arg(2),LShape,ShapePoints), 
  my_maplist(arg(1),LShape,Colorz),
  cclumped(Colorz,PenColors0),!,
  simplify_pen(PenColors0,PenColors))).

simplify_pen([cc(C,_)],[cc(C,1)]).
simplify_pen(Pen,Pen).


make_indiv_object_no_vm(ID,GridH,GridV,Overrides,OPoints,Obj):- 
  globalpoints_maybe_bg(OPoints,GPoints),
  sort_points(GPoints,SPoints),
  make_indiv_object_s(ID,GridH,GridV,Overrides,SPoints,Obj).

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


% obj_to_oid(Obj,OID),loc2D(Obj,X,Y),oid_home_grid(OID,GID),vis2D(thru_offset(X,Y,
% [1,2,3]=27
% [1,1,1,2,2,2,3,3,3]=27

% [1,2,2,3]=12.
% [1,2,3]=12.

to_global_coord(H,LH,OH,GridH,Min):- max_min(LH,OH,Max,Min), GridH is  floor((H-1)*OH/Max)+1.

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

prop_order([vis2D/2,mass/1,loc2D/2,mass/1,
  %center2G/2,
  pen/1,colorlesspoints/1,localpoints/1,rot2D/1,cc/2,iz/1,globalpoints/1,obj_to_oid/2,grid_size/2]).


iz_o(F,A):- member(F/A,[cenGX/1,cenGY/1,locY/1,locX/1,tall/1,wide/1,chromatic/2,dot/0]).
iz_o(F,A):- clause(guess_shape(_GridH,_GridV,_GridIn,_LocalGrid,_I,_Empty,_N,_H,_V,_CC,_Points,Term),_),nonvar(Term),functor(Term,F,A).
iz_o(stype(F),A):- clause(guess_shape_poly(_I,_Empty,_N,_H,_V,_CC,_Points,Term),_),nonvar(Term),functor(Term,F,A).

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
  findall(Code,(member(Obj,Objs),obj_to_program(Obj,Code,VM),my_maplist(ignore_xf,Code)),List),
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
  addGridPoints(VM,GPs).



o(Obj,OG,W,LF,N):- enum_object(Obj), indv_props(Obj,pg(OG,W,LF,N)).
pg(Obj,OG,W,LF,N):- enum_object(Obj), indv_props(Obj,pg(OG,W,LF,N)).


obj_to_program(Obj,Program,VM):-
  (isz(Obj,outl);isz(Obj,outline(_))),!,
  Program = 
  [loc2D(Obj,X,Y),   
   object_grid(Obj,Grid),
   add_grid_at_offset(X,Y,Grid,VM)].

obj_to_program(Obj,Program,VM):-
  isz(Obj,dot),!,
  Program =  [addGridPoints(VM,Obj)].

obj_to_program(Obj,Program,VM):-
  Program = 
  [globalpoints(Obj,Ps), 
   addGridPoints(VM,Ps)].


%object_pen(Obj,pen([cc(Color,1)])):- color(Obj,Color),!.
%object_pen(Obj,[cc(Color,1)]):- color(Obj,Color),!.

prop_of(mass,mass(_)).
prop_of(colors_cc,pen(_)).

prop_of(visually,localpoints(_)).
prop_of(loc2D,loc2D(_,_)).

prop_of(size2D,vis2D(_,_)).
prop_of(mass,mass(_)).

prop_of(loc2D,center2G(_,_)).
prop_of(rot2D,rot2D(_)).
prop_of(visually,pen(_)).
prop_of(colorlesspoints,shape_rep(grav,_)).


sort_obj_props(obj(L),obj(LO)):- !, sort_obj_props(L,LO).
%sort_obj_props(L,LO):- L=LO.
sort_obj_props(L,LOR):- my_maplist(in_obj_keys,L,LL),keysort(LL,LLS),my_maplist(arg(2),LLS,LO),reverse(LO,LOR).
%obj_prop_sort_compare(R,A,B):- compound(A), compound(B), !, obj_prop_sort_compare2(R,B,A).
%obj_prop_sort_compare(R,A,B):- compare(R,B,A).
%obj_prop_sort_compare2(R,A,B):- obk_key(A,AK),obk_key(B,BK),!,compare(R,AK-A,BK-B).
e1_member(E,L):- \+ \+ member(E,L ).

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

priority(Var,20):- var(Var),!.
priority("bckgrnd",0):-!.
priority("point",0):-!.
priority(T,10):- term_to_atom(T,A),atom_contains(A,")"),!.
priority(_,20).
longer_strings(R,A,B):- priority(A,PA),priority(B,PB),display_length(A,AL),display_length(B,BL),compare(R,PA+AL+A,PB+BL+B).


add_shape_info(Info,I,M):- add_shape_info0(Info,I,M),!.

add_shape_info0([Info|L],I,O):-!,add_shape_info0(Info,I,M),add_shape_info0(L,M,O).
add_shape_info0([],I,I):-!.
add_shape_info0(Info,I,O):- with_object(override,iz(Info),I,O).

verify_object(Obj):-
 % my_assertion(localpoints(Obj,_LP)),
 % my_assertion(globalpoints(Obj,_GP)),
  nop(assertion((iz(Obj,symmetry_type(What)), nonvar(What)))).

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
  with_objprops1(Op,E,List,MidList),
  with_objprops(Op,Props,MidList,NewList).
with_objprops(Op,E,List,MidList):-with_objprops1(Op,E,List,MidList).


with_objprops1(Op,E,List,MidList):- redress_override(E,EE),with_objprops2(Op,EE,List,MidList).

with_objprops2(delq,E,List,NewList):-functor(E,F,A),functor(R,F,A),
    append(Left,[R|Right],List), % E \=@= R,
    append(Left,Right,NewList),!.

with_objprops2(override,-E,List,NewList):-
    append(Left,[R|Right],List), E =@= R,
    append(Left,Right,NewList),!.

with_objprops2(override,E,List,NewList):- \+ aggregates(E), my_assertion(compound(E)), functor(E,F,A),functor(R,F,A),
    append(Left,[R|Right],List), % E \=@= R,
    append(Left,[E|Right],NewList),!.

with_objprops2(override,E,List,NewList):- 
    append(Left,[changes(G)|Right],List), !,
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; append(Left,[changes(G),E|Right],NewList)),!.

with_objprops2(override,E,List,NewList):- 
    append(Left,[iz(G)|Right],List), 
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; append(Left,[iz(G),E|Right],NewList)),!.


aggregates(iz(_)).
aggregates(occurs_in_links(_,_)).
aggregates(links_count(_,_)).
aggregates(creates_object(_,_)).
aggregates(links_to_object(_,_)).

%aggregates(unique_colors_count(_)).
aggregates(cc(_,_)).
aggregates(giz(_)).
aggregates(pg(_,_,_,_)).
%aggregates(birth(_)).
aggregates(link(_,_,_)).
aggregates(link(_,_)).
aggregates(insideOf(_)).

is_rule_mapping(Obj):- current_predicate(is_mapping/1), is_mapping(Obj),!.
%is_bg_object(Obj):- get_black(Black),has_prop(pen(  [cc(Black,_)]),Obj).
is_bg_object(Obj):- is_rule_mapping(Obj),!,fail.
is_bg_object(Obj):- sub_var(cc(fg,0),Obj),!, \+ is_whole_grid(Obj).
is_bg_object(Obj):- \+ is_object(Obj),sub_var(cc(fg,0),Obj),!.

is_fg_object(Obj):- sub_var(cc(bg,0),Obj),!.
is_fg_object(Obj):- is_whole_grid(Obj),!.
is_fg_object(Obj):- \+ is_bg_object(Obj),!.

is_used_fg_object(Obj):- has_prop(cc(fg,FG),Obj),FG>0, \+ is_whole_grid(Obj). 

is_whole_grid(B):- has_prop(iz(stype(whole)),B), \+ has_prop(iz(stype(part)),B),!.



merge_objs(_VM,Bigger,[],_IPROPS,Bigger):-!.
merge_objs(VM,Bigger,[New|Inside],IPROPS,Combined):- 
  merge_2objs(VM,Bigger,New,IPROPS,NewBigger),
  merge_objs(VM,NewBigger,Inside,[],Combined).
      
merge_2objs(CreatorVM,Bigger,NewInside,IPROPS,Combined):-
 globalpoints(Bigger,GP1), globalpoints(NewInside,GP2),      
 indv_props(Bigger,Props1),indv_props(NewInside,Props2),
 append([GP1,GP2],GPoints), append([Props1,Props2,IPROPS],Info),
 my_partition(props_not_for_merge,Info,_Exclude,Include),
 make_indiv_object(CreatorVM,Include,GPoints,Combined).

props_not_for_merge(globalpoints(_)).
props_not_for_merge(shape_rep(grav,_)).
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


%indv_u_props(I,[localpoints(Ps),loc2D(X,Y),pen(Pen),vis2D(H,V),rot2D(Rot)]):- loc2D(I,X,Y),shape_rep(grav,I,Ps),pen(I,Pen),vis2D(I,H,V),rot2D(I,Rot),!.
indv_u_props(I,[ shape_rep(grav,C),  loc2D(X,Y),  pen(Ps),  rot2D(Rot)]):-  
  shape_rep(grav,I,C),loc2D(I,X,Y),pen(I,Ps),rot2D(I,Rot),!.
%indv_u_props(I,[ shape_rep(grav,C),  center2G(X,Y),  pen(Ps),  rot2D(Rot)]):- shape_rep(grav,I,C),center2G(I,X,Y),pen(I,Ps),rot2D(I,Rot),!.
%indv_u_props(I,[shape_rep(grav,Ps),center2G(X,Y),pen(Pen),vis2D(H,V),rot2D(Rot)]):- center2G(I,X,Y),shape_rep(grav,I,Ps),pen(I,Pen),vis2D(I,H,V),rot2D(I,Rot),!.

physical_points(GPoints,Points):- sub_var(wbg,GPoints),!,
   GPoints=Points,!.
physical_points(GPoints,Points):- sub_var(wfg,GPoints),!,
   GPoints=Points,!.
physical_points(GPoints,Points):- 
   my_partition(sub_var('$VAR'('_')),GPoints,BGPoints,OPoints),
   BGPoints\==[],OPoints\==[],!,physical_points(OPoints,Points).

physical_points(GPoints,Points):- numbervars(GPoints,0,_,[singletons(true),attvars(skip)]),
   \+ (sub_compound('$VAR'(S),GPoints),S\=='_'),
   my_partition(sub_var('$VAR'('_')),GPoints,BGPoints,OPoints),
   BGPoints\==[],OPoints\==[],!,physical_points(OPoints,Points).

physical_points(GPoints,OPoints):- 
   my_partition(sub_var(bg),GPoints,BGPoints,OPoints),
   BGPoints\==[],OPoints\==[],!.

/*
physical_points(GPoints,Points):- 
   my_partition(sub_var(fg),GPoints,FGPoints,_),
   FGPoints\==[],!, Points = FGPoints.
*/
physical_points(GPoints,Points):-
   my_partition(sub_var(black),GPoints,BGPoints,OPoints),
   BGPoints\==[],OPoints\==[],!,physical_points(OPoints,Points).
physical_points(Points,Points).

physical_colorless_points(CPoints,Points):- is_ncpoints_list(CPoints),!,Points=CPoints.
physical_colorless_points(CPoints,Points):- is_cpoints_list(CPoints),!,
  physical_points(CPoints,PhysicalPoints),my_maplist(arg(2),PhysicalPoints,Points).
physical_colorless_points(Other,Points):- localpoints(Other,CPoints),!,physical_colorless_points(CPoints,Points).



local_shape_id(NormGrid,NormShapeID):- 
 physical_colorless_points(NormGrid,NormLPoints),
 shape_id(NormLPoints,NormShapeID).

cpoints_to_shapepoints(GPoints,ShapePoints):- cpoints_to_shapepoints(GPoints,_RotG,_OffsetX,_OffsetY,ShapePoints,_PenColors).
cpoints_to_shapepoints(GPoints,RotG,OX,OY,ShapePoints,PenColors):-
  po_loc2D_vis2D(GPoints,[],LocX,LocY,SizeX,SizeY),  
  deoffset_points(LocX,LocY,GPoints,LPoints),
  make_grid(SizeX,SizeY,Grid),
  physical_points(LPoints,PLPoints),
  add_global_points(PLPoints,Grid,Grid),
  grid_to_shape(Grid,RotG,OX,OY,ShapePoints,PenColors),!.

shape_id(Shape,ShapeID):- var(Shape),!,id_shape(Shape,ShapeID).
shape_id(Shape,ShapeID):-  \+ is_points_list(Shape), localpoints(Shape,LPoints),shape_id(LPoints,ShapeID).
shape_id(Shape,ShapeID):- is_cpoints_list(Shape), cpoints_to_shapepoints(Shape,ShapePoints),shape_id(ShapePoints,ShapeID).
shape_id(Shape,ShapeID):- is_shape_id_for(Shape,ShapeID),!.
shape_id(Shape,ShapeID):- term_hash(Shape,Hash), atomic_list_concat(['s',Hash],ShapeID), asserta(is_shape_id_for(Shape,ShapeID)).
id_shape(ShapeID,Shape):- is_shape_id_for(Shape,ShapeID).

% eqq

:- dynamic(is_iv_for/2).
iv_for(L,Iv):- copy_term(L,CT,_),numbervars(CT,0,_,[attvar(bind),singletons(true)]),term_hash(CT,Fv),
 number(Fv), Iv is (Fv rem 800) + 1,!. % (\+ is_iv_for(Iv,_) -> asserta_if_new(is_iv_for(Iv,L)) ; true).

%obj_iv(obj(obj(Obj)),Iv):- !, obj_iv(obj((Obj)),Iv).
obj_iv(Obj,Iv):- indv_props(Obj,giz(iv(Iv))),!.
obj_iv(Obj,Iv):- sub_compound(oid(OID),Obj),nonvar(OID),oid_to_iv(OID,Iv),!.
obj_iv(Obj,Iv):- indv_u_props(Obj,L),iv_for(L,Iv),!.
obj_iv(Obj,Iv):- globalpoints(Obj,GP),gpoints_to_iv(GP,Iv),!.
obj_iv(Obj,_Iv):- arcST,pp(Obj),trace.

/*
    kept(giz(g(IO))), kept(giz(testid(TestID))), kept(giz(example_num(Example+Num))),
    kept(giz(testid_example_io(TestID>(Example+Num)*IO))),*/
obj_to_parent_gid(Obj,GID):-  indv_props(Obj,giz(gid(GID))),!.
obj_to_parent_gid(Obj,GID):-  obj_to_decl_oid(Obj,OID),oid_to_parent_gid(OID,GID),!.

oid_to_parent_gid(OID,GID):- atomic_list_concat(['o',_Glyph,_IV,V,ID,Example,Num,IO],'_',OID), 
 atomic_list_concat([V,ID,Example,Num,IO],'_',GID).

oid_to_iv(OID,IV):- atomic_list_concat(['o',_Glyph,IV|_],'_',OID),!.


is_oid(OID):- oid_glyph_object(OID,_,_).
is_oid(OID):- gid_type_oid(_,_,OID), \+ oid_glyph_object(OID,_,_).

obj_to_decl_oid(L,OID):-
((sub_compound(obj_to_oid(OID),L);sub_compound(was_oid(OID),L);sub_compound(oid(OID),L);sub_compound(omem_oid(OID),L)),atom(OID)),!.
/*
obj_to_oid(I,X):- var_check(I,obj_to_oid(I,X))*->!;
 (indv_props(I,L),((member(obj_to_oid(X),L);member(oid(X),L)),!,
  object_glyph(I,Glyph),
  assert_object_oid(_,I,Glyph,X))).
*/
obj_to_oid(Obj,OID):-  var(Obj),!,oid_glyph_object(OID,_,Obj).
obj_to_oid(Obj,OID):-  atom(Obj),display_length(Obj,L),L>5,Obj=OID,!.
obj_to_oid(obj(obj(Obj)),OID):- !, obj_to_oid(obj(Obj),OID).
obj_to_oid(Obj,OID):-  oid_glyph_object(OID,_,Obj),!.
obj_to_oid(Obj,OID):-  obj_to_decl_oid(Obj,OID),!, 
  %must_det_ll(object_glyph(Obj,Glyph)),!,
  assert_object_oid(_TID,Obj,_Glyph,OID).
/*obj_to_oid(Obj,OID):- 
 must_det_ll((
   obj_iv(Obj,Iv), int2glyph(Iv,Glyph), % object_glyph(Obj,Glyph),       
   obj_to_parent_gid(Obj,GID), !,
   atomic_list_concat(['o',Glyph,Iv,GID],'_',OID),
   assert_object_oid(GID,Obj,Glyph,OID))).
   %assert(oid_glyph_object(OID,Glyph,Obj)))).*/
obj_to_oid(Obj,OID):-  assert_object_oid(_,Obj,_Glyph,OID),!.

assert_object_oid(_TID,Obj,Glyph,OID):-     
 must_det_ll((   
   is_object(Obj),
   obj_iv(Obj,Iv), int2glyph(Iv,Glyph), % object_glyph(Obj,Glyph),       
   obj_to_parent_gid(Obj,GID),  
   %(nonvar(GID)->tid_to_gid(TID,GID);true),

   if_t((nonvar(OID),var(GID)),
    (atomic_list_concat(['o',Glyph,Iv|GIDLst],'_',OID),
     atomic_list_concat(GIDLst,'_',GID))),

   if_t((nonvar(GID),var(OID)),
    (atomic_list_concat(['o',Glyph,Iv,GID],'_',OID))),

   retractall(oid_glyph_object(OID,_,_)),
   retractall(gid_glyph_oid(GID,Glyph,_)), 
   retractall(gid_glyph_oid(GID,_,OID)),

   arc_assert_fast(oid_glyph_object(OID,Glyph,Obj)),
   arc_assert_fast(gid_glyph_oid(GID,Glyph,OID)),
   assert_object2(OID,Obj))).

assert_object2(OID,obj(List)):-!,my_maplist(assert_object2(OID),List).
assert_object2(OID,List):- is_list(List),!,my_maplist(assert_object2(OID),List).
assert_object2(OID,Prop):- Prop=..[F|List],CINDV=..[cindv,OID,F|List],arc_assert1(CINDV),
  append(LL,[E],List),
  ignore((is_list(E),atom_concat(F,'_count',NN),append(LL,[Count],CList),
  length(E,Count),
  CINDVL=..[cindv,OID,NN|CList],
  arc_assert1(CINDVL))).

/*
  assert_object5(OID,F,Pre,Last,List).
assert_object5(OID,F,Pre,Last,_List):- 
  AProp=..[cindv,OID,F,Pre,Last],
  % (\+ ground(AProp)->dumpST;true),
  arc_assert1(AProp).
*/

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

assert_object1(OID,obj(List)):-!,my_maplist(assert_object1(OID),List).
assert_object1(OID,List):- is_list(List),!,my_maplist(assert_object1(OID),List).
assert_object1(OID,Prop):- Prop=..List, AProp=..[cindv,OID|List],
  (\+ ground(AProp)->dumpST;true),
  pfcAdd(AProp).
*/

current_gid(GID):- get_vm(VM), GID = VM.gid.
   %tid_to_gids(ID,GID),!.

oid_to_obj(OID,Obj):- oid_glyph_object(OID,_,Obj).



%obj_to_oid(Obj,_,MyID):- obj_to_oid(Obj,MyID).
id_to_gid(TID,GID):- is_grid(TID),!,grid_to_gid(TID,GID).
tid_to_gid(TID,GID):- var(TID),!,current_gid(GID).
tid_to_gid(TID,GID):- tid_to_gids(TID,GID),!.

%o2g_f(Obj,Glyph):-  atom(Obj),display_length(Obj,1),Obj=Glyph,!.
o2g_f(Obj,Glyph):- oid_glyph_object(_,Glyph,Obj),!.
o2g_f(Obj,Glyph):- obj_to_oid(Obj,OID),oid_glyph_object(OID,Glyph,Obj),!.




%obj_to_oid(I,_,Iv):- is_object(I), obj_iv(I,Iv).
%obj_to_oid(I,_ID,Fv):- is_grid(I),!, flag(indiv,Fv,Fv+1).
%obj_to_oid(I,ID,Iv):- indv_props(I,obj_to_oid(ID,Iv)),!.
%obj_to_oid(I,ID,Fv):- into_obj(I,O),!,obj_to_oid(O,ID,Fv).
%obj_to_oid(I,ID,Iv):- trace_or_throw(missing(obj_to_oid(I,ID,Iv))).
%obj_to_oid(_,ID,_Iv):- luser_getval(test_pairname,ID).


mass(C,1):- is_fg_color(C),!.
mass(C,0):- (is_bg_color(C);var(C);C==[]),!.
mass(I,Count):- is_grid(I),!,append(I,Cs),!,mass(Cs,Count),!.
mass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(mass,[G|Grid],MPoints),sum_list(MPoints,Points).
mass([G|Grid],Points):- my_maplist(mass,[G|Grid],MPoints),sum_list(MPoints,Points),!.
mass(Point,Mass):- is_point(Point),!,(is_fg_point(Point)->Mass=1;Mass=0).
mass(I,X):- is_object_or_oid(I),indv_props(I,mass(X)),!.
mass(I,X):- var_check(I,mass(I,X)),!.
mass(I,Count):- globalpoints(I,Points),mass(Points,Count),!.

is_object_or_oid(I):- once((is_object(I) ; is_oid(I))).


unique_fg_colors(In,Cs):- is_grid(In),!,
  unique_colors(In,UC),
  include(p1_call((is_fg_color,is_real_color,\==(zero))),UC,Cs),!.
unique_fg_colors(Obj,List):- indv_props(Obj,unique_fg_colors(List))*->true;unique_fg_colors2(Obj,List).

%unique_fg_colors(G,FG):- indv_props(G,unique_colors(SUCOR)),include(is_fg_color,SUCOR,FG),!.
unique_fg_colors2(G,SUCOR):- colors_cc(G,GF),quietly((my_maplist(arg(1),GF,UC),include(is_real_fg_color,UC,SUCO))),reverse(SUCO,SUCOR).


%remove_color(C-_,hv(1,1)):- is_bg_color(C),!.
%remove_color(_-P,P).
%remove_color(LPoints,ShapePoints):- mapgroup(remove_color,LPoints,ShapePoints).

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






:- decl_pt(helper,indv_props(is_object,+)).

%indv_prop_val1(I,V):- indv_props(I,V).
%indv_prop_val1(I,X):- compound(X),I=obj(List),member(X,List).
indv_props_list(Obj,Props):- var(Obj),!,enum_object(Obj),indv_props_list0(Obj,Props).
indv_props_list(Grp,GrpOO):- is_group(Grp),!,mapgroup(indv_props_list0,Grp,GrpO),append(GrpO,GrpOO).
indv_props_list(OID,NVL):- indv_props_list0(OID,NVL).


indv_props_list0(OID,NVL):- is_oid(OID),!,oid_to_propslist(OID,NVS), combine_cindv(OID,NVS,NVL).
indv_props_list0(Obj,NVL):- is_object(Obj),!,obj_to_propslist(Obj,NVS), combine_cindv(Obj,NVS,NVL).
indv_props_list0(obj(PA),PA):- my_assertion(is_list(PA)).
%indv_props_list0(OID,List):- is_oid(OID), oid_to_obj(OID,Obj),!,indv_props_list(Obj,List).
indv_props_list0(OProps,Props):- is_obj_props(OProps),!,Props=OProps.
indv_props_list0([E|OID],[E|NVL]):- is_prop1(E),!,indv_props_list1(OID,NVL).
indv_props_list0(Objs,Props):- is_list(Objs),!,my_maplist(indv_props_list0,Objs,Props).
indv_props_list0(PA,PAP):- is_list(PA),!,PAP=PA.
indv_props_list0(alone(_OID,PA),PA):- my_assertion(is_list(PA)).
indv_props_list0(Obj,Props):- lock_doing(has_prop_list,Obj,has_prop_list(Obj,Props)).

indv_props_list1(Var,Var):- var(Var),!.
indv_props_list1([],[]):-!.
indv_props_list1([E|OID],[E|NVL]):- is_prop1(E),!,indv_props_list1(OID,NVL).

has_prop_list(Obj,Props):- findall(Prop,has_prop(Prop,Obj),Props),Props\==[],!.

combine_cindv(_,NVL,NVL):- \+ really_use_cindv,!.
combine_cindv(OID,NVS,NVL):- atom(OID),!,findall(NV,cindv0(OID,NV),Props2),append_sets(NVS,Props2,NVL),!.
combine_cindv(Obj,NVS,NVL):- obj_to_oid(Obj,OID), findall(NV,cindv0(OID,NV),Props2),append_sets(NVS,Props2,NVL),!.
/*
indv_props_list_e(Obj,NV):- var(Obj),!,enum_object(Obj),indv_props_list_e(Obj,NV).
indv_props_list_e(Obj,NV):- \+ is_object(Obj),!,into_obj(Obj,O),!,indv_props_list_e(O,NV).
indv_props_list_e(obj(L),E):- is_list(L),member(E,L).
indv_props_list_e(Obj,NV):- obj_to_oid(Obj,OID),!,cindv(OID,NV).
*/
%indv_props(I,_):- \+ is_object(I),!,fail.
%indv_props(I,_):- atom(I),my_assertion(is_oid(I)),fail.
%indv_props(I,Prop):- (atom(I);var(I)),indv_props_list_e(I,Prop).
indv_props(I,Props):- var(I),!,enum_object(I),indv_props(I,Props).
indv_props(I,NV):-  must_be(compound,NV), cindv(I,NV).
%indv_props(I,Props):- atom(I), \+ \+ cindv(I,_,_), var(Props),!,indv_props_list(I,Props).
%indv_props(obj(L),Prop):- is_list(L),!,member(Prop,L ).
%indv_props(C,LL):- \+ compound(C),nonvar(C),g2o(C,I),!,indv_props(I,LL).
%indv_props(I,_):- \+ is_object(I),!,fail.
%indv_props(I,Prop):- compound(Prop),!,Prop=..PropL,apply(indv_props,[I|PropL]).
%indv_props(I,Prop):- var(Prop),!,indv_props_list_e(I,Prop).

cindv(OID,NV):- var(OID),!,is_oid(OID),cindv(OID,NV).
cindv(OID,NV):- is_oid(OID),!,cindv0(OID,NV).
cindv(Obj,_):- \+ is_object(Obj),!,fail.
cindv(Obj,NV):- obj_to_propslist(Obj,L),member(NV,L).
cindv(Obj,NV):- really_use_cindv, obj_to_oid(Obj,OID),cindv0(OID,NV).

cindv0(OID,NV):- \+ nb_current(o2obj,t), oid_to_propslist(OID,L),nv_member(NV,L).
cindv0(OID,NV):- really_use_cindv, cindv1(OID,NV).

cindv1(OID,NV):- compound(NV),!,NV=..NVL,apply(cindv(OID),NVL).
cindv1(OID,NV):- cindv(OID,A,B),NV=..[A,B].
cindv1(OID,NV):- cindv(OID,A,B,C),NV=..[A,B,C].
cindv1(OID,NV):- cindv(OID,A,B,C,D),NV=..[A,B,C,D].
cindv1(OID,NV):- cindv(OID,A,B,C,D,E),NV=..[A,B,C,D,E].

really_use_cindv:- false.

oid_to_propslist(OID,List):- oid_to_obj(OID,Obj),obj_to_propslist(Obj,List).

obj_to_propslist(obj(List),ListO):- member(was_oid(OID),List),!,oid_to_obj(OID,obj(ListM)),append_sets(ListM,List,ListO).
obj_to_propslist(obj(List),List).
obj_to_propslist(Obj,[NV]):- fail, \+ nb_current(o2obj,t), locally(nb_setval(o2obj,t),is_in_subgroup(_Grp,Obj,NV)).

cindv(OID,A,B):- oid_to_propslist(OID,L),member(NV,L),NV=..[A,B].
cindv(OID,A,B,C):- oid_to_propslist(OID,L),member(NV,L),NV=..[A,B,C].
cindv(OID,A,B,C,D):- oid_to_propslist(OID,L),member(NV,L),NV=..[A,B,C,D].
cindv(OID,A,B,C,D,E):- oid_to_propslist(OID,L),member(NV,L),NV=..[A,B,C,D,E].

nv_member(NV,L):- var(NV),nonvar(L),!,member(PNV,L),pnv_to_nv(PNV,NV).
nv_member(PNV,L):- nonvar(PNV),pnv_to_nv1(PNV,NV),!,member(PNV2,L),pnv_to_nv(PNV2,NV).
nv_member(NV,L):- member(NV,L).

pnv_to_nv1(io(NV),NV):-!.
pnv_to_nv1(oi(NV),NV):-!.
pnv_to_nv1(if(NV),NV):-!.
pnv_to_nv(PNV,NV):-pnv_to_nv1(PNV,NV).
pnv_to_nv(NV,NV):-!.

indv_props(Obj,A,B):- NV=..[A,B], indv_props(Obj,NV).
indv_props(Obj,A,B,C):- NV=..[A,B,C], indv_props(Obj,NV).
indv_props(Obj,A,B,C,D):- NV=..[A,B,C,D], indv_props(Obj,NV).
indv_props(Obj,A,B,C,D,E):- NV=..[A,B,C,D,E], indv_props(Obj,NV).

indv_prop_val(Obj,A,BL):- indv_props(Obj,NV),NV=..[A|BL]. 
%indv_props(C,LL):- arc_expand_arg(C,I,G),call(G),!,indv_props(I,LL).
%indv_props(I,X):- var_check(I,indv_props(I,X))*->true;(indv_props(I,X)).
%indv_props(I,NV):- indv_prop_val1(I,NV);indv_prop_val2(I,NV).
%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
%indv_props(C,LL):- C=obj(L),!,(is_list(L)->LL=L ; (copy_term(L,LL),append(LL,[],LL))),!.



indv_props_for_noteablity(obj(L),Notes):- my_assertion(nonvar(L)),!, include(is_prop_for_noteablity,L,Notes).




%is_not_prop_for_noteablity(globalpoints).
%is_not_prop_for_noteablity(grid_size).
%is_not_prop_for_noteablity(obj_to_oid).

%indv_props(G,L):- arcST,atrace,into_obj(G,O),is_object(O),indv_props(O).

%pmember(E,X):- indv_props(X,E).
%pmember(E,X):- sub_term(EE,X),nonvar_or_ci(EE),EE=E,ground(E).
/*pmember(E,L):- is_vm_map(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
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
var_check_throw(I,G):- var(I),u_dmsg(error(var(G))),!,arcST,u_dmsg(error(var(G))),ibreak,trace_or_throw(maybe_enum_i(I,G)),call(G).

object_shapeW(I,X):- compound(I),I=obj(L),!,my_assertion(is_list(L)),!,member(iz(X),L ).
object_shapeW(I,X):- indv_props(I,iz(X)).

isz(I,X):- is_list(I),I=[O],!,isz(O,X).
isz(I,X):- var_check(I,isz(I,X))*->true;(indv_props(I,iz(X))).


vm_to_printable(D,R) :- Objs = D.objs,!, (Objs\==[] -> R = Objs; R = D.grid ).

resolve_reference(R,Var):- is_vm_map(R),!,Objs = R.objs,!, (Objs\==[] -> Var=Objs; Var = R.grid).
resolve_reference(R,Var):- compound(R),arc_expand_arg(R,Var,Goal),!,call(Goal).
resolve_reference(R,Var):- arc_expand_atom(R,Var),!.
resolve_reference(R,Var):- nonvar(R),R\=obj(_),known_gridoid(R,Var),!.

rot2D(G,X):- is_group(G),!,mapgroup(rot2D,G,Points),append_sets(Points,X).
rot2D(I,X):- var_check(I,rot2D(I,X)).
rot2D(I,X):- indv_props(I,rot2D(X)).
rot2D(_,sameR).

object_changes(G,X):- is_group(G),!,mapgroup(object_changes,G,Points),append_sets(Points,X).
object_changes(I,X):- indv_props(I,changes(X)).


%hv_cvalue(Grid,Color,H,V):- hv_cg_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- plain_var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar_or_ci(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).


% Is there an advantage to counting down?
all_points_between(_Grid,_LowH,_LowV,_GridH,GridV,_Hi,Vi,Points,Points):- Vi>GridV,!.
all_points_between(Grid,LowH,LowV,GridH,GridV,Hi,Vi,PointsIn,PointsO):-
  (Hi>GridH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),
   all_points_between(Grid,LowH,LowV,GridH,GridV,H,V,PointsIn,Points),
  ((hv_c_value(Grid,C,Hi,Vi), hv_point(Hi,Vi,Point)) -> 
   (PointsO = [C-Point|Points],never_newd(C)) ; PointsO = Points).


color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C,Hi,Vi),!.
/*
color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_fg_color(C2,C);(attvar(C2),C=C2); (\+ plain_var(C2), C=C2)),!,
  get_bgc(BGC),C\==BGC.
*/

%never_newd(C):- plain_var(C),!,freeze(C, ignore(never_newd(C))).
%never_newd(C):- compound(C),C =(_-_), dumpST,throw(never_newd(C)).
never_newd(_).

all_points_between_include_bg(_Grid,_LowH,_LowV,_GridH,GridV,_Hi,Vi,Points,Points):- Vi>GridV,!.
all_points_between_include_bg(Grid,LowH,LowV,GridH,GridV,Hi,Vi,PointsIn,PointsO):-
  (Hi>GridH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),
   all_points_between(Grid,LowH,LowV,GridH,GridV,H,V,PointsIn,Points),
  (((once(hv_c_value(Grid,C,Hi,Vi)), hv_point(Hi,Vi,Point))) -> 
   (PointsO = [C-Point|Points],never_newd(C)) ; PointsO = Points).

/*
% Is there an advantage to counting down?
all_points_between_include_bg(_Grid,_LowH,_LowV,_GridH,GridV,_Hi,Vi,Points,Points):- Vi>GridV,!.
all_points_between_include_bg(Grid,LowH,LowV,GridH,GridV,Hi,Vi,Points,PointsO):-
  ((color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi),
  hv_point(Hi,Vi,Point))
     -> (PointsT = [C-Point|Points], never_newd(C)) ; PointsT = Points),

   (Hi>GridH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),!,
   all_points_between_include_bg(Grid,LowH,LowV,GridH,GridV,H,V,PointsT,PointsO).
*/

color_spec_or_fail_include_bg(Grid,C,Hi,Vi):-
  hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(fail,var(C2),fail,C=C2)).

color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi):- 
  get_bgc(BGC),
  hv_c_value_or(Grid,C2,Hi,Vi,BGC),
  (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(fail,var(C2),C=BGC)).
  
grid_cpoint(Grid,C-Point,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),never_newd(C),
 (is_spec_color(C2,C);(atomic(C2),C=C2);(compound(C2),C=C2);(attvar(C2),C=C2);(fail,var(C2),C=C2)),
  hv_point(Hi,Vi,Point).

grid_to_points(Grid,Points):- grid_size(Grid,HH,HV),!, grid_to_points(Grid,HH,HV,Points).
% Is there an advantage to counting down?
grid_to_points(Grid,HH,HV,Points):- 
  all_points_between(Grid,1,1,HH,HV,1,1,[],Points),!. 
% Is there an advantage to counting down?
grid_to_points_include_bg(Grid,Points):- grid_size(Grid,HH,HV),!,
  all_points_between_include_bg(Grid,1,1,HH,HV,1,1,[],Points),!. 
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
   shape_rep(grav,Obj,Points),member('+'-P1,Points),localpoints(Obj,CPoints),member(C-P1,CPoints),
   C-P1 = CPoint,
  (points_corner_dir(Shape,Dir)*->(SPoints=[CPoint|_];last(SPoints,CPoint));fail).
   
fixed_defunt_obj(OL,Obj):- compound(OL),OL=obj(I),defunct_objprops(I),
  ((member(oid(OID),I);member(was_oid(OID),I)),atom(OID)),!,
  oid_to_obj(OID,Obj),Obj\=@=obj(I).




defunct_objprops(I):- member(was_oid(_),I),!.
defunct_objprops(I):- \+ ( member(globalpoints(Ps),I), is_cpoints_list(Ps)),
                       \+ ( member(localpoints(Ps),I), is_cpoints_list(Ps)),!.
%globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), no_repeats(ID,cmem(ID,_,_)), findall(-(C,HV),cmem(ID,HV,C),Points).
%globalpoints(Grid,Points):- grid_to_gid(Grid,ID),\+ \+ cmem(ID,_,_),findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(I,_):-  var(I),!,fail.

globalpoints(I,O):- fixed_defunt_obj(I,Obj),!,globalpoints(Obj,O).

globalpoints(Grid,Points):- is_grid(Grid),!, grid_to_points(Grid,Points).
globalpoints(obj(L),O):- object_l(globalpoints(O),L),!.
globalpoints(CP,[CP]):- is_point(CP),!.
globalpoints(I,X):-  (var_check(I,globalpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
globalpoints([],[]):-!.
globalpoints(G,[G]):- is_cpoint(G),!.
globalpoints(C-P,[C-P]):-!.
globalpoints(G,G):- is_cpoints_list(G).
globalpoints(Grid,Points):- is_group(Grid),!,mapgroup(globalpoints,Grid,MPoints),append_sets(MPoints,Points).
globalpoints(G,Ps):- is_vm_map(G),vm_to_printable(G,R),!,globalpoints(R,Ps).
globalpoints(options(X),_Points):- trace_or_throw(globalpoints(options(X))).
globalpoints(I,X):- indv_props(I,globalpoints(X)), nop(my_assertion(is_cpoints_list(X))),!.
globalpoints(I,G):- is_object(I),object_localpoints(I,L),is_points_list(L),loc2D(I,X,Y),offset_points(X,Y,L,G),!.
%globalpoints(I,X):- localpoints(I,X),!.
globalpoints(_,[]):-!.
globalpoints(Atom,_):- \+ compound(Atom),!,my_assertion(gp=Atom),trace_or_throw(globalpoints(Atom)).
globalpoints(I,X):- my_assertion(gp=I), trace_or_throw(unknown(globalpoints(I,X))).

localpoints(Grid,Points):- is_grid(Grid),!, must_det_ll(grid_to_points(Grid,Points)).
localpoints(G,G):- is_cpoints_list(G),!.
localpoints(I,X):- is_object(I),!,must_det_ll(object_localpoints(I,X)),!.
localpoints(I,X):- (var_check(I,localpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
localpoints(G,[G]):- is_point(G),!.
localpoints(G,Ps):- is_vm_map(G),vm_to_printable(G,R),!,localpoints(R,Ps).
localpoints(Grid,Points):- is_group(Grid),!,mapgroup(localpoints,Grid,MPoints),append_sets(MPoints,Points).
localpoints(G,G):- is_points_list(G).
localpoints(Grid,Points):- is_list(Grid),!,my_maplist(localpoints,Grid,MPoints),append_sets(MPoints,Points).
%localpoints(Atom,_):- \+ compound(Atom),!,trace_or_throw(localpoints(Atom)).
localpoints(I,X):- trace_or_throw(unknown(localpoints(I,X))).

  %localpoints(I,X):- into_grid(I,G),!,grid_size(G,H,V),grid_to_points(G,H,V,X).

must_det_11(G):- must_det_ll(call(call,(G))).

localpoints_include_bg(I,X):- must_be_free(X),  \+ is_grid(I), !, must_det_11((localpoints(I,X),is_cpoints_list(X))),!.
localpoints_include_bg(Grid,Points):- is_grid(Grid),!, 
  must_det_11((grid_to_points_include_bg(Grid,Points),is_cpoints_list(Points))),!.


object_grid(I,G):- is_grid(I),!,G=I.
object_grid(I,G):- indv_props(I,grid(G)),!.
object_grid(Group,List):- is_group(Group),!,override_group(object_grid(Group,List)),!.
object_grid(ObjRef,List):- \+ is_object(ObjRef), into_obj(ObjRef,Obj), is_object(Obj), !,object_grid(Obj,List).
object_grid(I,G):- object_localpoints(I,LP),vis2D(I,H,V),!,points_to_grid(H,V,LP,G),!.

%object_localpoints(obj(I),_):- member(obj(_),I),!,ibreak.
/*
object_localpoints(I,XX):- must_be_free(XX), %stack_check_or_call(3000,(dmsg(stackcheck>3000),ibreak)),
 must_det_ll((indv_props_list(I,L),
   setup_call_cleanup(flag('$olp',X,X+1),(X<30,object_localpoints0(I,L,XX)),flag('$olp',_,X)),
   is_cpoints_list(XX))),!.
*/



object_localpoints(I,X):- 
  indv_props_list(I,L), must_det_ll(object_l(localpoints(X),L)),!.

object_l(P,[Obj|L]):- (is_group(L);is_object(Obj)),!,mapgroup(object_l(P),[Obj|L]).
object_l(P,L):- stack_check(999), compound(P),functor(P,F,A),functor(PP,F,A), member(PP,L),!,P=PP.
object_l(globalpoints(O),L):- object_l(loc2D(OH,OV),L),object_l(localpoints(LPoints),L),!, offset_points(OH,OV,LPoints,O).
object_l(loc2D(OH,OV),L):-  
  member(iz(cenGX(CX)),L),member(iz(cenGY(CY)),L),member(iz(sizeGX(SX)),L),member(iz(sizeGY(SY)),L),
  OH is floor(CX-(SX/2)), OV is floor(CY-(SY/2)).


object_l(localpoints(O),L):- member(globalpoints(XX),L),is_list(XX),object_l(loc2D(OH,OV),L),!,deoffset_points(OH,OV,XX,O).
object_l(localpoints(O),L):- member(iz(sid(ShapeID)),L),!,
  must_det_ll((id_shape(ShapeID,ShapePoints),
       object_l(rot2D(RotG),L), object_l(pen(PenColors),L),%object_l(rotSize2D(OX,OY),L),
       OX=1,OY=1, make_localpoints(ShapePoints,RotG,OX,OY,PenColors,O))),!.
object_l(localpoints(O),L):- must_det_ll((object_l(grid(In),L),!,release_some_c(In,Out),localpoints(Out,O))).

%object_l(localpoints(O),L):- object_l(points_rep(grav,O),L),!.
%grid_to_points(LocalGrid,O))
object_l(grid(O),L):-
 once((
    (member(f_grid(In),L),include(p1_arg(1,is_real_color),In,O))
    ;(member(grid_rep(Norm,NormGrid),L),member(grid_ops(Norm,Ops),L), unreduce_grid(NormGrid,Ops,O))
    ;(object_l(localpoints(LPoints),L), points_to_grid(LPoints,O))
    ;(object_l(grid(grav,SCGrid),L), object_l(rot2D(RotG),L), grid_call(RotG,SCGrid,O)))),!.

object_l(f_grid(O),L):- object_l(grid(In),L),fpad_grid(f,var,In,O).

object_l(shape_rep(grav,Shape),L):- shape_rep(obj(L),grav,Shape).
%member(iz(sid(ShapeID)),L),id_shape(ShapeID,Shape).

object_l(grid_rep(Grav,Grid),L):- (grav_algo(Grav);algo_list(Grav)),  member(points_rep(Grav,Points),L), 
   member(rotSize2D(Grav,OX,OY),L), points_to_grid(OX,OY,Points,Grid).

object_l(points_rep(Grav,ColoredShape),L):- grav_algo(Grav), 
  member(shape_rep(Grav,ShapePoints),L),object_l(pen(PenColors),L),
  colorize_points(ShapePoints,PenColors,ColoredShape).


object_l(Prop,L):- Prop = iz(algo_sid(Algo,NormShapeID)),algo_list(Algo), \+ member(Prop,L), 
 object_l(grid_rep(Algo,NormGrid),L),
 local_shape_id(NormGrid,NormShapeID),!. %shape_id(NormGrid,NormShapeID).

grid_rep(I,Algo,NormGrid):- indv_props(I,grid_rep(Algo,NormGrid))*->true; algo_ops_grid(I,Algo,_NormOps,NormGrid).
grid_ops(I,Algo,NormOps):- indv_props(I,grid_ops(Algo,NormOps))*->true; algo_ops_grid(I,Algo,NormOps,_NormGrid).

/*
      localpoints(NormGrid,NormLPoints),my_maplist(arg(2),NormLPoints,ShapeNormLPoints),  
     shape_id(ShapeNormLPoints,NormShapeID),
     PropsOut = [grid_ops(Algo,NormOps),iz(algo_sid(Algo,NormShapeID)),grid_rep(Algo,NormGrid)].*/


algo_ops_grid(obj(List),Algo,NormOps,NormGrid):- 
  member(grid_ops(Algo,NormOps),List),member(grid_rep(Algo,NormGrid),List),!.
algo_ops_grid(Obj,Comp,NormOps,NormGrid):- 
  algo_depends_algo(Comp,Norm),
  algo_ops_grid(Obj,Norm,_NormOps,CompNormGrid),
  call_algo(Comp,NormOps,CompNormGrid,NormGrid).

algo_ops_grid(Obj,Algo,NormOps,NormGrid):- 
  algo_list(Algo),into_grid(Obj,Grid),
  call_algo(Algo,NormOps,Grid,NormGrid).

call_algo(comp,NormOps,Grid,NormGrid):- compress_grid(NormOps,Grid,NormGrid).
call_algo(norm,NormOps,Grid,NormGrid):- normalize_grid(NormOps,Grid,NormGrid).
algo_depends_algo(comp,norm).
algo_list(norm).
algo_list(comp).
grav_algo(grav).




colorize_points(ShapePoints,PenColors,LPointS):-
  combine_pen(ShapePoints,PenColors,PenColors,LPoints),!,
  sort_safe(LPoints,LPointS).

make_localpoints(RotLCLPoints,RotG,OX,OY,PenColors,LPointS):- 
  must_det_ll((   maybe_undo_effect_points(OX,OY,RotLCLPoints,RotG,ShapePoints),
     PenColors\==[],is_list(PenColors),
     combine_pen(ShapePoints,PenColors,PenColors,LPoints) )),!,
  sort_safe(LPoints,LPointS).

maybe_undo_effect_points(_,_,RotLCLPoints,sameR,LPoints):- must_be_free(LPoints),!,RotLCLPoints=LPoints.
maybe_undo_effect_points(OX,OY,RotLCLPoints,RotG,LPoints):- 
 must_det_ll((points_to_grid(OX,OY,RotLCLPoints,Grid),   
   undo_effect(RotG,Grid,Grid90),localpoints_include_bg(Grid90,LPoints))).


combine_pen(A,B,C,D):- nonvar(D),!,combine_pen(A,B,C,V),!,V=D.
combine_pen([],_,_,[]):-!.
combine_pen(X,[],Reset,XX):- my_assertion(Reset\==[]), !,combine_pen(X,Reset,Reset,XX).

combine_pen([P],[cc(C,_)|_],_,[C-P]):- atom(P),!.
combine_pen([_-L|LL],C,Reset,XX):- nonvar(L),!,combine_pen([L|LL],C,Reset,XX).
%combine_pen([_-P1|L],C,Reset,[C-P1|XX]):- is_color(C),!,
combine_pen([P1|L],C,Reset,CP1XX):- \+ is_list(C), is_color(C),!,[C-P1|XX]=CP1XX, 
  combine_pen(L,C,Reset,XX).

combine_pen(L,[cc(C,N)],[cc(C,N)],XX):- add_color(L,C,XX),!.

combine_pen(L,[cc(C,N)|PenColors],Reset,XX):- number(N), make_list(C,N,List),append(List,PenColors,ListPenColors),
  combine_pen(L,ListPenColors,Reset,XX).

combine_pen([P1|L],[C|PenColors],Reset,[C-P1|XX]):- is_color(C),!,
  combine_pen(L,PenColors,Reset,XX).

add_color([],_,[]):-!.
add_color([C1-P|L],C,[C1-P|XX]):- !, add_color(L,C,XX).
add_color([P|L],C,[C-P|XX]):- !, add_color(L,C,XX).

shape_rep(grav,I,X):- is_object(I),!, indv_props_list(I,L),(member(shape_rep(grav,X),L)->true; (member(iz(sid(ShapeID)),L),is_shape_id_for(X,ShapeID))).
shape_rep(grav,G,X):- is_group(G),!,mapgroup(colorlesspoints,G,Points),append_sets(Points,X).
% returns the objects decolorize localpoints
shape_rep(grav,I,ShapePoints):- into_grid(I,Grid),grid_to_shape(Grid,_RotG,_SH,_SV,ShapePoints,_PenColors).
  %localpoints(I,Points), grid_to_shape(Points,X).

%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_tid(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
*/

is_color_cc(cc(C,_)):- is_color(C).
%colors_cc(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.
colors_cc(I,X):- nonvar(X),!,colors_cc(I,XX),!,X=XX.
colors_cc(G,[cc(Black,0),cc(fg,0),cc(bg,0)]):-  G==[],!,get_black(Black).
colors_cc(I,X):- is_object(I),!,CC=cc(_,_),findall(CC,(indv_props(I,CC),is_color_cc(CC)),X).
colors_cc(I,X):- is_oid(I),!,CC=cc(_,_),findall(CC,(indv_props(I,CC),is_color_cc(CC)),X).
%colors_cc(G,X):- is_group(G),mapgroup(colors_cc,G,GG),combine_results(GG,X).
%colors_cc(G,X):- is_grid(G),mapgrid(colors_cc,G,GG),combine_results(GG,X).
colors_cc(I,X):- is_vm_map(I),!,into_grid(I,G),!,colors_cc(G,X).
colors_cc(G,CC):- pixel_colors(G,Pixels),pixels_to_cc(Pixels,CC).

sort_color_by_mass(_,[E],[E]):-!.
sort_color_by_mass(Obj,List,SortedColors):-colors_cc(Obj,CC),predsort(sort_on(cc_mass(CC)),List,SortedColors).

cc_mass(CC,Color,N):- member(cc(Color,N),CC).
/*colors_cc(All,CC):-
  findall(Nm-C,(enum_colors_test(C),occurs:count((sub_term(Sub, All), \+ \+ cmatch(C,Sub)), Nm), 
    once(Nm\==0 ; (atom(C), C\==is_colorish, C\==var, \+ is_real_color(C)))),BF),
  keysort(BF,KS),reverse(KS,SK),
  into_cc(SK,CC),!.*/

pixels_to_cc(Pixels,ECC):- get_ccs(Pixels,RCC),add_summary_colors(RCC,ECC),!.

is_cc_n(P1,cc(_,N)):-call(P1,N).
add_summary_colors(CC,CCO):- total_summary_colors(0,0,0,BG,FG,Vars,CC),
   include(is_cc_n('=<'(0)),[cc(bg,BG),cc(fg,FG),cc(plain_var,Vars)],CCE),append(CC,CCE,CCO).

special_plus(N,NP,NNP):-   ((N>=0)->plus(N,NP,NNP);NNP=N).

total_summary_colors(BG,FG,Vars,BGO,FGO,VarsO,[CC|CCs]):- 
  total_summary_colors_e(BG,FG,Vars,BGM,FGM,VarsM,CC),
  total_summary_colors(BGM,FGM,VarsM,BGO,FGO,VarsO,CCs).
total_summary_colors(BG,FG,Vars,BG,FG,Vars,[]).

total_summary_colors_e(BG,FG,N,BG,FG,NNP,cc(C,NP)):- plain_var(C),!,special_plus(N,NP,NNP).
total_summary_colors_e(_BG,FG,Vars,-1,FG,Vars,cc(bg,_)).
total_summary_colors_e(BG,_FG,Vars,BG,-1,Vars,cc(fg,_)).
total_summary_colors_e(BG,FG,_Vars,BG,FG,-1,cc(plain_var,_)).
total_summary_colors_e(N,FG,Vars,NNP,FG,Vars,cc(C,NP)):- is_bg_color(C),!,special_plus(N,NP,NNP).
total_summary_colors_e(BG,N,Vars,BG,NNP,Vars,cc(C,NP)):- is_fg_color(C),!,special_plus(N,NP,NNP).
total_summary_colors_e(BG,FG,Vars,BG,FG,Vars,_).

mostly_usefull_colors_cc(O,CCO):- colors_cc(O,CC), into_mostly_real_colors(CC,CCO),!.
into_mostly_real_colors(CC,CCO):- include(is_real_cc,CC,CCO),CCO\==[],!.
into_mostly_real_colors(CC,CCO):- include(is_some_cc,CC,CCO),CCO\==[],!.
into_mostly_real_colors(CC,CC):- !.

is_real_cc(cc(C,N)):- N>0, is_real_color(C),!.
is_some_cc(cc(_,N)):- N>0,!.



  
%color_cc_via_pixels(G,BFO):- quietly((pixel_colors(G,GF),count_sets(GF,_,SK),into_cc(SK,BFO))).
get_ccs(GF,CC):-
  %count_sets(GF,SK),
  sort_safe(GF,GS), count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),
  into_cc(SK,CC),!.

count_sets(GF,SK):- sort_safe(GF,GS), count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!.
count_sets(GF,GS,SK):- (var(GS)->list_to_set(GF,GS);true), count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!.

/*
pixels_to_cc([ blue, cyan,blue,blue,cyan,cyan,blue,blue,blue,blue,yellow,yellow,cyan,blue,blue,yellow],Pixels_to_cc))

*/
%colors_cc(G,X):- is_group(G),!,mapgroup(colors_cc,G,Points),append_sets(Points,X).


get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).

pen(I,C):- indv_props(I,pen(C)),!.



object_ngrid(Obj,GNGrid):- object_grid(Obj,Grid), into_ngrid(Grid,GNGrid).

object_ngrid_symbols(Obj,Syms):- object_ngrid(Obj,NGrid), ngrid_syms(NGrid,Syms).

ngrid_syms(NGrid,Syms):- 
 subst_syms(bg,NGrid,GFlatSyms),get_ccs(GFlatSyms,Syms).

find_syms('-'). find_syms('|'). 
find_syms('='). find_syms('!'). 
find_syms('~'). find_syms('0').
find_syms('X'). find_syms('#').
find_syms('x'). find_syms('+').
find_syms('*'). find_syms('.').
find_syms('<'). find_syms('>').
find_syms('v'). find_syms('^').
find_syms('\\'). find_syms('/').
find_syms('7'). find_syms('`').

subst_syms(IBGC,List, L) :-
 must_det_ll(( flatten([List],L1),   
   my_maplist(cell_syms(IBGC),L1, L2),
   my_maplist(on_edge_sym, L2, L3),
  include(\==(''),L3,L))).

on_edge_sym(W,S):- on_edge_become(W,S),!. on_edge_sym(S,S).

cell_syms(_IBGC,Cell,''):- plain_var(Cell),!.
cell_syms( IBGC,Cell,''):- sub_var(IBGC,Cell).
cell_syms(_IBGC,Cell,'@'):- is_color(Cell),!.
cell_syms(_IBGC,Sym,Sym):- atom(Sym),atom_length(Sym,1),!.
cell_syms(_IBGC,Cell,Cell):- \+ compound(Cell),!.
cell_syms(IBGC,Cell-_,Sym):- cell_syms(IBGC,Cell,Sym),!.

object_global_ngrid(Obj,GNGrid):- global_grid(Obj,Grid), into_ngrid(Grid,GNGrid).

/*
global_grid(I,G):- is_grid(I),!,G=I.
global_grid(ObjRef,List):- \+ is_object(ObjRef), into_obj(ObjRef,Obj),!,global_grid(Obj,List).
%global_grid(Group,List):- is_group(Group),globalpoints(Group,Points),!,
global_grid(I,G):- must_det_ll((call((grid_size(I,H,V),globalpoints_maybe_bg(I,LP),points_to_default_grid(H,V,LP,G))))),!.
global_grid(I,G):- globalpoints(I,GP),points_to_default_grid(GP,G),!.
global_grid(I,G):- is_object(I), object_grid(I,G),!.
*/
global_grid0(I,G):- is_grid(I),!,G=I.
global_grid0(ObjRef,List):- \+ is_object(ObjRef), into_obj(ObjRef,Obj),!,global_grid0(Obj,List).
global_grid0(I,G):- must_det_ll((call((grid_size(I,H,V),globalpoints_maybe_bg(I,LP),points_to_grid(H,V,LP,G))))),!.
global_grid0(I,G):- object_grid(I,G1),into_solid_grid(G1,G0),!,loc2D(I,H,V),pad_top(V,G0,GV),pad_left(H,GV,G).

global_grid(I,G):- global_grid(I,_OID,G),!.
global_grid(I,G):- global_grid0(I,G),!.

invent_oid_grid(OID,G):- is_grid(G),grid_to_image_oid(G,OID),!.
invent_oid_grid(OID,G):- is_grid(G),mapgrid(black_vs_bg,G,GG),grid_to_image_oid(GG,OID),!.
invent_oid_grid(OID,G):- is_grid(G),mapgrid(black_vs_bg,G,GG),nonvar(OID),assert_if_new(oid_to_global_grid(OID,GG)).


global_grid(I,OID,G):- is_grid(I),!,G=I,invent_oid_grid(OID,G),!.
global_grid(_,OID,G):- atom(OID),oid_to_global_grid(OID,G),!.
global_grid(I,OID,G):- atom(I),OID=I,oid_to_global_grid(OID,G),!.
global_grid(I,OID,G):- is_group(I),must_det_ll((call((grid_size(I,H,V),globalpoints_maybe_bg(I,LP),points_to_grid(H,V,LP,G))))),
  invent_oid_grid(OID,G).
global_grid(I,OID,G):- var(OID),is_object(I),obj_to_oid(I,OID),!,global_grid(I,OID,G),
  assert_if_new(oid_to_global_grid(OID,G)).
global_grid(I,OID,G):- global_grid0(I,G0),!,mapgrid(black_vs_bg,G0,G), assert_if_new(oid_to_global_grid(OID,G)).

%black_vs_bg(C,O):- C == black,!, O = wbg.
black_vs_bg(C,O):- plain_var(C),!,O = bg.
black_vs_bg(C,O):- C == bg,!, O = _.
black_vs_bg(C,C).

locG_term(I,loc2G(X,Y)):- loc2G(I,X,Y),!.
loc2G(Grid,H,V):- is_grid(Grid),!,other_grid_size(Grid,H,V).
loc2G(G,X,Y):- is_group(G),!,mapgroup(locG_term,G,Offsets),sort_safe(Offsets,[loc2G(X,Y)|_]). % lowest loc2G
%loc2G(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LocX,LocY,_,_,_,_), H is LocX, V is LocY.
loc2G(I,X,Y):- is_object(I), indv_props(I,loc2G(X,Y)),!.
loc2G(I,X,Y):- into_obj(I,O), indv_props(O,loc2G(X,Y)),!.

obj_var(O):- var(O).
maybe_defunct(O,obj(Obj)):- compound(O),O=obj(L),nonvar(L),member(was_oid(OID),L),!,oid_to_obj(OID,obj(Obj)).

loc_term(I,loc2D(X,Y)):- loc2D(I,X,Y),!.
loc2D(O,H,V):- obj_var(O),!,into_obj(O,Obj),loc2D(Obj,H,V).
loc2D(O,H,V):- maybe_defunct(O,Obj),!,loc2D(Obj,H,V).
loc2D(I,X,Y):- is_object(I), indv_props(I,io(loc2D(X,Y))),!.
loc2D(Grid,H,V):- is_grid(Grid),!,H=1,V=1.
loc2D(G,X,Y):- is_group(G),!,mapgroup(loc_term,G,Offsets),sort_safe(Offsets,[loc2D(X,Y)|_]). % lowest loc2D
%loc2D(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LocX,LocY,_,_,_,_), H is LocX, V is LocY.
loc2D(I,X,Y):- is_object(I), indv_props(I,loc2D(X,Y)),!.
loc2D(I,X,Y):- sub_compound(I,loc2D(X,Y)).
loc2D(I,X,Y):- into_obj(I,O), indv_props(O,loc2D(X,Y)),!.
%loc2D(NT,H,V):- atrace, known_gridoid(NT,G),loc2D(G,H,V).
    


:- decl_pt(prop_g,vis_hv_term(is_object_or_grid,size2D)).

vis_hv_term(I,size2D(X,Y)):- vis2D(I,X,Y),!.

vis2D(Grid,H,V):- is_grid(Grid),!,grid_size(Grid,H,V).
vis2D(I,X,Y):- indv_props(I,vis2D(X,Y)),!.
vis2D(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LocX,LocY,HiH,HiV,_,_), H is HiH-LocX+1, V is HiV-LocY+1.
vis2D(G,X,Y):- is_group(G),!,mapgroup(vis_hv_term,G,Offsets),sort_safe(Offsets,HighToLow),last(HighToLow,size2D(X,Y)).
vis2D(Points,H,V):- points_range(Points,LocX,LocY,HiH,HiV,_,_), H is HiH-LocX+1, V is HiV-LocY+1.
vis2D(NT,H,V):-  known_gridoid(NT,G),G\==NT, vis2D(G,H,V).

rotSize2D(grav,I,X,Y):- indv_props(I,rotSize2D(grav,X,Y)),!.
rotSize2D(grav,Grid,H,V):- is_grid(Grid),!,grav_roll(Grid,_RotG,RotShape),grid_size(RotShape,H,V).
rotSize2D(grav,NT,H,V):-  into_gridoid(NT,G),G\==NT, rotSize2D(grav,G,H,V).


%externalize_links(obj_grp(O1L,Grp),[link(C,A),EL|More],[link(C,A),elink(C,Ext)|LMore]):- EL\=elink(_,_),externalize_obj(Obj,Other,Ext),!,externalize_links(obj_grp(O1L,Grp),[EL|More],LMore).

externalize_links(Grp,NewObjs):- Grp==[],!,NewObjs=[].
externalize_links(Grp,Grp):-!.
/*
externalize_links(Grp,NewObjs):- 
 must_det_ll((is_group_or_objects_list(Grp), 
   maplist(externalize_links((Grp)),Grp,NewObjs))).
%externalize_links(obj_grp(O1L,Grp),NewObj):- is_object(Obj),!,externalize_obj_links(Obj,NewObj),!.
%externalize_links(obj_grp(O1L,Grp),Objs):-!.

externalize_links((Grp),Obj,NewObj):-
   indv_props_list(Obj,O1L), 
   maplist(externalize_links(obj_grp(O1L,Grp)),O1L,NewList), 
   NewObj=obj(NewList).
%externalize_links(obj_grp(O1L,Grp),link(C,A),elink(C,Ext)):- !, externalize_obj(obj_grp_link(O1L,Grp,C),A,Ext).
*/
/*
externalize_links(obj_grp(_O1L,_Grp),A,A).

externalize_obj(obj_grp_link(O1,_Grp, C),OID2,Ext):- 
 must_det_ll((
   indv_props_list(O1,O1L), 
   indv_props_list(OID2,O2L),
   findall(Prop,
     (member(Functor,[giz(glyph),iz(sid),iz(type),iz(stype),delta(loc2D),delta(vis2D),delta(rot2D),delta(pen)]),
      externalize_prop(O1L,C,O2L,Functor,Prop)),Ext))).
*/
%[loc2D,vis2D,rot2D,iz(sid),iz(stype),colors]
externalize_prop(O1L,_C,O2L,delta(Functor),Delta):- 
  select_prop(Functor,O1L,Prop1),
  select_prop(Functor,O2L,Prop2),
  must_det_ll(proportional(Prop1,Prop2,Delta)),!.
externalize_prop(O1L,C,O2L,delta(Functor),Prop):- !, externalize_prop(O1L,C,O2L,Functor,Prop).
externalize_prop(_O1L,_C,O2L,Functor,Prop):-  select_prop(Functor,O2L,Prop),!.
%externalize_prop(_Obj,_C,_O2L,Functor,unk(Functor)). 
   
select_prop(Functor,O2L,Prop):- atom(Functor),!,member(Prop,O2L), functor(Prop,Functor,_).
select_prop(iz(Functor),O2L,Prop):- atom(Functor),!,member(iz(Prop),O2L), functor(Prop,Functor,_).
select_prop(giz(Functor),O2L,Prop):- atom(Functor),!,member(giz(Prop),O2L), functor(Prop,Functor,_).

%vis2D(Obj,size2D(H,V)):- vis2D(Obj,H,V).
%loc2D(Obj,loc2D(H,V)):- loc2D(Obj,H,V).

center_term(Obj,loc2D(H,V)):- center2G(Obj,H,V).

:- decl_pt(prop_g,hw_rat(is_object_or_grid,size2D)).
hw_rat(Obj,HV):- vis2D(Obj,OH,OV), HV is rationalize(OH/OV).

:- decl_pt(prop_g,colormass_object_count(is_grid  ,size2D)).
colormass_object_count(Obj,HV):- vis2D(Obj,OH,OV), HV is rationalize(OH/OV).

center2G(I,X,Y):- is_grid(I), !, grid_size(I,H,V),X is floor(H/2),Y is floor(V/2).
center2G(I,X,Y):- indv_props(I,center2G(X,Y)),!.
center2G(I,X,Y):- indv_props(I,iz(cenGX(X))),indv_props(I,iz(cenGY(Y))),!.


% Calculate the center of mass of a list of points
center2D(I,X,Y):- is_cpoint(I),!,I=(_-P),hv_point(X,Y,P),!.
center2D(I,X,Y):- is_point(I),!,hv_point(X,Y,I),!.
center2D(I,X,Y):- is_grid(I), !, grid_size(I,H,V),X is floor(H/2),Y is floor(V/2).
center2D(I,X,Y):- indv_props(I,center2D(X,Y)),nonvar(X),nonvar(Y),!.
center2D([], inf, inf):-!.
center2D(I,X,Y):- \+ is_list(I),!, must_det_ll((globalpoints(I,Points),center2D(Points,X,Y))).
center2D(Points, CenterX, CenterY) :- maplist(center2D,Points,X,Y),
   length(Points,Count),
   sumlist(X,SumX),sumlist(Y,SumY),
    Count > 0, % Ensure there's at least one point to avoid division by zero
    CenterX is round(SumX / Count),
    CenterY is round(SumY / Count).

%center2D(I,X,Y):- indv_props(I,iz(cenXD(X))),indv_props(I,iz(cenYD(Y))),!.
%center2G(Obj,CentX,CentY):- vis2D(Obj,H,V), loc2D(Obj,X,Y),CentX is X + floor(H/2),CentY is Y + floor(V/2).


object_color(HV,C):- color(HV,C).

color(HV,C):- colors_cc(HV,[cc(C,_)]),!.
color(HV,multicolor(Stuff)):- colors_cc(HV,Stuff),!.

main_color(HV,C):- colors_cc(HV,[cc(C,_)|_]).
first_gpoint(HV,P):- globalpoints(HV,[P|_]).
last_gpoint(HV,P):- globalpoints(HV,PS),last(PS,P).
any_gpoint(HV,P):- globalpoints(HV,P).

rebuild_from_localpoints(Obj,NewObj):-
  localpoints_include_bg(Obj,Points),
  rebuild_from_localpoints(Obj,Points,NewObj).

rebuild_from_localpoints(Obj,WithPoints,NewObj):-
 get_vm(VM),
 must_det_ll((
  localpoints_include_bg(WithPoints,Points),

  localpoints_include_bg(Obj,PrevPoints),

  (Points=@=PrevPoints -> (NewObj=Obj) ;

 (rot2D(Obj,Rot),undo_p2(Rot,UnRot),
  loc2D(Obj,X,Y),%vis2D(Obj,H,V),  
  %obj_to_oid(Obj,ID,_Iv),
  %uncast_grid_to_object(Orig,Grid,NewObj),
  points_to_grid(Points,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints_include_bg(UnRotGrid,LPoints),
  offset_points(X,Y,LPoints,GPoints),
  indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
  remObjects(VM,Obj),
  make_indiv_object(VM,[loc2D(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],GPoints,NewObj))))),
   verify_object(NewObj),
   assumeAdded(VM,NewObj),
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
  grid_size(X,XGridH,XGridV),
  grid_size(Y,YGridH,YGridV),
  % center2G(Y,YCH,YCV),
  loc2D(X,XX,XY),
  loc2D(Y,YX,YY))),
  print_grid(XGridH,XGridV,XGP),
  print_grid(YGridH,YGridV,YGP),
  append(XGP,YGP,XYGP),
  must_det_ll(rebuild_from_globalpoints(_VM,Obj,XYGP,NewObj)).
:- style_check(+singleton).

pct(G):- call(G), ppt(G).

%rebuild_from_glob alpoints(Obj,NewObj):-
%  globalpoints_in clude_bg(Obj,GPoints),
%  rebuild_from_l ocalpoints(Obj,GPoints,NewObj).

rebuild_from_globalpoints(VM,Obj,GPoints,NewObj):-
 must_det_ll((
  
  indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
  (var(VM) -> peek_vm(VM) ; true),
  
  %ppa(before=Obj),
  remObjects(VM,Obj),
  make_indiv_object(VM,PropsRetained,GPoints,NewObj),
  assumeAdded(VM,NewObj),
  %ppa(propsRetained=PropsRetained),
  %ppa(after=NewObj),
    verify_object(NewObj))),
 !.



is_prop_automatically_rebuilt(iz(birth(_))):-!,fail.
is_prop_automatically_rebuilt(Prop):- sub_term(CP,Prop),(is_color(CP);is_ncpoint(CP)),!.
is_prop_automatically_rebuilt(Prop):- compound(Prop),functor(Prop,F,_),(atom_contains(F,'color');atom_contains(F,'points')),!.
is_prop_automatically_rebuilt(Prop):-
 member(Prop,[cc(_),mass(_),mass(_),shape_rep(grav,_),rot2D(_),roll_shape(_),pen(_),grid_rep(_,_),grid_ops(_,_),
              iz(multicolored(_)),iz(chromatic(_,_)),
              iz(symmetry_type(_,_)),iz(stype(_)),iz(monochrome),
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
   append(SOLS,[leftover(LeftOver)],SOLSL),
   dash_chars,
   (SOLS==[]-> ((writeln("NOTHING FROM"=P3),ignore((fail,catch((print_grid(H,V,ID,Grid),writeln("NOTTAAA"=P3),!,fail),_,fail)))));
   member(OL,SOLSL),
   (OL=leftover(LeftOver)
    -> (nl,
      ignore((nonvar(LeftOver),nl,write(leftover=P3),print_grid(H,V,LeftOver))),
      write('from '),write(P3),print_grid(H,V,Grid))
    ;((grid_of(OL,O,Hits),once((mass(O,M))),
     nl,write(solution(Hits,M)), print_grid(H,V,O))))).
find_outline_pred(ID):- io_side_effects, into_grid(ID,Grid),find_outlines(Grid).

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
 append(Rest1,Rest2,Rest).

find_group_outlines(Options,[G1|G2],Sols,Rest):-!,
  find_group_outlines_fix_rest(Options,G1,Sols1,Rest1),
  find_group_outlines(Options,G2,Sols2,Rest2),
  append(Sols1,Sols2,Sols), append(Rest1,Rest2,Rest).
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

%guess_shape(GridH,GridV,G,LocalGrid,I,O,N,H,V,Colors,Points,walls_thick(1)):- walls_thick1(G).
/*
guess_shape(GridH,GridV,GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.loc2D=loc2D(LocX,LocY),
   make_grid(H,V,Grid),
   calc_add_points(LocX,LocY,Grid,Points),
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
  append(Others,LeftOver,LeftOverO).


object_nm_grid(Obj,SSP):-
  vis2D(Obj,H,V),
  localpoints(Obj,LPoints),
  points_to_grid(H,V,LPoints,GridIn),
  grid_to_gridmap(GridIn,SSP),
  print_grid(SSP).

fast_arc:- fail.

%neighbor_map
grid_to_gridmap(GridIn,GridIn):- fast_arc,!.
grid_to_gridmap(GridIn,GridON):- 
  neighbor_map(GridIn,GridO),
  mapgrid(only_neib_data,GridO,GridON),
  %subst_1L(['*'-'.','~'-'red','+'-'blue','.'-'yellow'],GridON,SSP),
  nop((subst_1L(['*'-'.'],GridON,_SSP))).

object_to_nm_points(Obj,NPoints):-
  object_nm_grid(Obj,SSP),
  localpoints(SSP,NPoints),!.

:- style_check(-singleton).
:- style_check(-discontiguous).

cc_fg_count(Colors,Len):- include(\=(cc(_,0)),Colors,NonZero),my_maplist(arg(1),NonZero,Colorz),include(is_real_fg_color,Colorz,FGC),length(FGC,Len).
cc_bg_count(Colors,Len):- include(\=(cc(_,0)),Colors,NonZero),my_maplist(arg(1),NonZero,Colorz),include(is_real_bg_color,Colorz,FGC),length(FGC,Len).


guess_shape(GridH,GridV,GridIn,LocalGrid,I,Empty,N,H,V,[cc(Black,_)|Rest],Points, bfc(FGB)):-  Rest == [], ((is_bg_color(Black);Black==wbg)->FGB=bg;FGB=fg).
%guess_shape(GridH,GridV,GridIn,LocalGrid,I,Empty,N,H,V,[cc(Black,_)|Rest],Points, fbc(FGB)):- Rest == [], ((is_fg_color(Black);Black==fg)->FGB=fg;FGB=bg).
%guess_shape(GridH,GridV,GridIn,LocalGrid,I,0,N,H,V,Colors,Points,view_sq):- H == V.%guess_shape(GridH,GridV,GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangle):- H>1, V>1.
% TODO guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,chromatic(Len,BGLen)):- cc_fg_count(Colors,Len),cc_bg_count(Colors,BGLen).
 
%guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,monochrome):- Colors=[_],length(Colors,Len).
guess_shape(GridH,GridV,GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad).

guess_shape(GridH,GridV,GridIn,LocalGrid,I,0,N,H,V,Colors,Points,filltype(solid)):- N > 1.
guess_shape(GridH,GridV,GridIn,LocalGrid,I,Empty,N,H,V,Colors,Points,filltype(nonsolid)):- N > 1, Empty > 0.
%guess_shape(GridH,GridV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0,once(H>1;V>1).

guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,R):- N>=2, iz_symmetry(GridIn,R).

guess_shape(GridH,GridV,GridIn,LocalGrid,I,Empty,N,H,V,Colors,Points,diagonal(u)):- Empty>0, rollD(GridIn,GridInRolled),my_maplist(=(E),GridInRolled).
guess_shape(GridH,GridV,GridIn,LocalGrid,I,Empty,N,H,V,Colors,Points,diagonal(d)):- Empty>0, rollDR(GridIn,GridInRolled),my_maplist(=(E),GridInRolled).

guess_shape(GridH,GridV,GridIn,LocalGrid,I,E,N,H,V,Colors,Points,stype(Keypad)):- 
  guess_shape_poly(I,E,N,H,V,Colors,Points,Keypad).

guess_shape_poly(I,0,1,1,1,Colors,Points,dot):-!.
guess_shape_poly(I,_,_,_,_,Colors,[Point],dot):-!.
guess_shape_poly(I,E,N,1,GridV,Colors,Points,column).
guess_shape_poly(I,E,N,GridH,1,Colors,Points,row).

guess_shape_poly(I,0,N,N,1,Colors,Points,hv_line(h)):- N > 1.
guess_shape_poly(I,0,N,1,N,Colors,Points,hv_line(v)):- N > 1.
guess_shape_poly(I,0,N,H,V,Colors,Points,rectangulator):- N>1, H\==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,square):- N>1,H==V,!.


%guess_shape(GridH,GridV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,solidity(A)):- solidity(Points,A).
%guess_shape(GridH,GridV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,Solid):- (is_jagged(Points)->Solid=jagged(true);Solid=jagged(false)).
guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outl2):- H>2,V>2,N>4,
  once((grid_to_gridmap(GridIn,GridON), \+ member('*',GridON), member('.',GridON))).
  
guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outline(SN)):- H>2,V>2,N>4,
  (find_outlines3(Points,Sol,Rest)->(length(Sol,SN),SN>0,length(Rest,RN))),!.

guess_shape(GridH,GridV,GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outl):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).

guess_shape(GridH,GridV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,fp(NPoints)):- fail,
  grid_to_gridmap(GridIn,GridON),
  subst_1L(['~'-'red','+'-'blue','.'-'yellow'],GridON,SSP),
  localpoints(SSP,NPoints),!.
  %clumped(ON,COO),!,my_maplist(arg(1),COO,PAT).
  %points_to_grid(H,V,FGridO,RGridO).

:- style_check(+singleton).

flipSym_checks(Rot90,GridIn):-
  copy_term(GridIn,G,_),!,
  flipSym(Rot90,G).

%iz_symmetry(GridIn,H,V,N,R):- N == 2, H==1, 
iz_symmetry(GridIn,R):-
  (flipSym_checks(SN,GridIn)*->R=SN;R=symmetry_type(_ALL_TODO,false)).

symmetric_types(Any,QQ):- into_grid(Any,Grid), 
  findall(Q,(flipSym_checks(Q,Grid)),QQ).
/*
symmetric_types(Any,QQ):- into_grid(Any,Grid), 
  findall(Q,iz_symmetry(Grid,Q),QQ), QQ\==[],!.
*/

:- meta_predicate(flipSym(-,+)).

  


%then_sym(Dif,I,O):- flipSym(Dif,I),!,I=O.


%flipSym(Rot90,Grid):- (flipSym(flipH,Grid),flipSym(flipV,Grid)),   (flipSym(rot90,Grid) -> (Rot90=full ; Rot90 = sym_hv) ; Rot90 = sym_hv).
flipSym(Rot90,Obj):- into_grid(Obj,Grid),
                     (var(Rot90)->rot_p_plus_trim(Rot90);true),
                     once((grid_call(Rot90,Grid,LocalGridM),!,Grid=@=LocalGridM)).
%flipSym3(sym_hv,Grid):- !, flipSym3(flipH,Grid),flipSym3(flipV,Grid).
%flipSym3(  full,Grid):- !, flipSym3(rot90,Grid),flipSym3(sym_hv,Grid).
%flipSym( Rot90,Grid):-  grid_call_unbound_p1(rot_p2,Rot90,Grid,LocalGridM),!,Grid=@=LocalGridM.
%flipSym3( Rot90,Grid):-  grid_call(Rot90,Grid,LocalGridM),!,Grid=@=LocalGridM.

/*
grid_call_unbound_p1(P1,Rot90,GridIn,GridOut):- var(Rot90),!,call(P1,Rot90),grid_call_unbound_p1(P1,Rot90,GridIn,GridOut).
grid_call_unbound_p1(P1,symmetry_after(A,B),GridIn,GridOut):- !, grid_call(A,GridIn,GridM), grid_call_unbound_p1(P1,B,GridM,GridOut).
grid_call_unbound_p1(P1,[A],GridIn,GridOut):- !, grid_call_unbound_p1(P1,A,GridIn,GridOut).
grid_call_unbound_p1(P1,[A|B],GridIn,GridOut):- !, grid_call_unbound_p1(P1,A,GridIn,GridM), grid_call_unbound_p1(P1,B,GridM,GridOut).
*/
grid_call_unbound_p1(_P1,Call,GridIn,GridOut):- grid_call(Call,GridIn,GridOut).

into_true_false(P0,TF):- (call(P0)->TF=true;TF=false).
rot_p_plus_trim(symmetry_after(trim_to_rect,Type,_)):- rot_p_plus_full(Type).
rot_p_plus_trim(symmetry_after(trim_outside,Type,_)):-  rot_p_plus_full(Type).
rot_p_plus_trim(symmetry_type(Type,_)):- rot_p_plus_full(Type).

%symmetry_after(A,B,I,O):- grid_call_alters(A,I,M), grid_call(B,M,O).
symmetry_after(P2,Type,TF,I,M):- grid_call(P2,I,M),!,I\=@=M, symmetry_type(Type,TF,M).
symmetry_type(R,TF,I,O):- symmetry_type(R,TF,I),!,O=I.

symmetry_type(Var,TF,I):- var(Var),!,rot_p_plus_full(Var),symmetry_type(Var,TF,I).
symmetry_type(full,TF,I):- !,flipSym_checks(rot90,I),!,symmetry_type(sym_hv,TF,I).
symmetry_type(sym_hv,TF,I):- !, flipSym_checks(flipH,I),!,symmetry_type(flipV,TF,I).
symmetry_type(sym_h_xor_v,TF,I):- !, symmetry_type(flipH,TF1,I), symmetry_type(flipV,TF2,I), !, into_true_false(TF1\=@=TF2,TF).
symmetry_type(R,TF,I):- grid_call(R,I,O)->into_true_false(I=@=O,TF).

rot_p_plus_full(sym_hv). rot_p_plus_full(full). rot_p_plus_full(sym_h_xor_v). 
rot_p_plus_full(P):- rot_p2(P).

%rot_p2(flipDH). % rot_p2(flipDV). 
rot_p2(flipD). 
rot_p2(rot90). rot_p2(rot270). 
rot_p2(flipH). rot_p2(flipV).
rot_p2(rot180). rot_p2(flipDHV). 
rot_p2(rollD). %rot_p2(rollDR).



%rot_p(P):- rot_p1(P).
%rot_p(and(trim_to_rect,P)):- rot_p1(P).
%rot_p(and(into_bicolor,P)):- rot_p2(P).

%rot_p1(and(into_monochrome,P)):- rot_p2(P).


:- dynamic(arc_cache:individuated_cache/3).
:- retractall(arc_cache:individuated_cache(_,_,_,_)).
:- dynamic(is_why_grouped_g/4).
:- retractall(is_why_grouped_g(_,_,_,_)).

call_i1(M,O,V):- M=..[F|Args],!,MO=..[F,O|Args], \+ missing_arity(MO,1),call(MO,V).
assert_i1(M,O,V):- M=..[F|Args],!,append(Args,[V],AV),MOV=..[F,O|AV],asserta_if_new(MOV).

o_m_v(O,M,V):- is_grid(O),!,call_i1(M,O,V).
o_m_v(O,M,V):- is_object(O),!,(indv_prop_val(O,M,V)*->true;call_i1(M,O,V)).
o_m_v(O,M,V):- is_dict(O),!,get_dict(M,O,OOV),get_oov_value(OOV,V).
o_m_v(S,M,V):- structure_type_data(S,Type,O),!,call(Type,O,M,V).
o_m_v(O,M,V):- must_det_ll((id_to_o(O,I),O\=@=I)),!,o_m_v(I,M,V).

% *->true;(\+ is_dict(O)->o_m_v(O,K,V))).

is_structure_type(color_set).
structure_type_data(S,Type,O):- compound(S),S=..[Type,O],is_structure_type(Type),!.
structure_type_data(S,Type,O):- is_list(S),Type=list_of(_),S=O.
inv(I,M,O):- call(M,I,O).

color_set(CsIn,append(CsOut),Result):- !, append_sets(CsIn,CsOut,Result).
color_set(CsIn,add(CsOut),Result):- !, append_sets(CsIn,CsOut,Result).
color_set(CsIn,rem(CsOut),Result):- include(not_in(CsOut),CsIn,Result).

list_of(_Elem,CsIn,Memb,Result):- color_set(CsIn,Memb,Result).

id_to_o(O,I):- atomic(O),gid_to_grid(O,I),!.
id_to_o(O,I):- g2o(O,I),!.
id_to_o(O,I):- compound(O), O=(_>_),into_gridoid(O,I),!.

set_o_m_v(O,M,V):- is_dict(O),!, nb_set_dict(M,O,V).
set_o_m_v(O,N,V):- is_rbtree(O),!, (nb_rb_get_node(O,N,Node)->nb_rb_set_node_value(Node,V);nb_rb_insert(O,N,V)).
set_o_m_v(O,M,V):- is_grid(O),!,assert_i1(M,O,V).
set_o_m_v(O,M,V):- is_object(O),!,(missing_arity(M,1)->add_prop(O,M,V);assert_i1(M,O,V)).
set_o_m_v(S,M,V):- structure_type_data(S,Type,O),!,call(Type,O,set(S,M),V).
set_o_m_v(O,M,V):- must_det_ll((id_to_o(O,I),O\=@=I)),!,arc_setval(I,M,V).


:- multifile(dictoo:dot_overload_hook/4).
:- dynamic(dictoo:dot_overload_hook/4).
:- module_transparent(dictoo:dot_overload_hook/4).

dictoo:dot_overload_hook(M,Obj,Memb,Value):- 
 findall(i(M,Obj,Memb,Value),
 ((o_m_v(Obj,Memb,Value)*->true;
 ( dmsg(called(dictoo:dot_overload_hook(M,Obj,Memb,Value))),fail))),List), List\==[],!,
  member(i(M,Obj,Memb,Value),List).

:- multifile(dictoo:is_dot_hook/4).
:- dynamic(dictoo:is_dot_hook/4).
:- module_transparent(dictoo:is_dot_hook/4).

%dictoo:is_dot_hook(_,_,_,_):-!.
is_hooked_obj(Self):- is_dict(Self),!,fail.
is_hooked_obj(Self):- is_object(Self),!.
is_hooked_obj(Self):- is_grid(Self),!.
is_hooked_obj(Self):- is_list(Self),!.
is_hooked_obj(Self):- structure_type_data(Self,_,_),!.
is_hooked_obj(Self):- g2o(Self,_).
is_hooked_obj(Self):- gid_to_grid(Self,_),!.
dictoo:is_dot_hook(_M,_Self,Func,_Value):- Func==program,!,fail.
dictoo:is_dot_hook(_M,Self,_Func,_Value):- is_hooked_obj(Self),!.
dictoo:is_dot_hook(M,Self,Func,Value):- M\==thread_util,M\==shell, \+ is_dict(Self),
  (is_vm(Self)->nop(dmsg(called(dictoo:is_dot_hook(M,vm,Func,Value))));dmsg(called(dictoo:is_dot_hook(M,Self,Func,Value)))),fail.



:- include(kaggle_arc_footer).


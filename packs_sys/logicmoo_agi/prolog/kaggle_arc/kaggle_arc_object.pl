/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).

:- thread_local(t_l:id_cells/2).

deoffset_points(1,1,Points,Points):-!.
deoffset_points(OH,OV,Point,LPoint):- map_pred(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

offset_points(OH,OV,Point,LPoint):- map_pred(if_point_offset(OH,OV),Point,LPoint).
if_point_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

offset_point(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).
offset_point(OH,OV,C-Point,C-LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).


grid_to_individual(Grid,Obj):- 
  my_assertion(is_grid(Grid)),!,
  grid_size(Grid,H,V),
  grid_to_points(Grid,H,V,Points),
  (Points==[]-> empty_grid_to_individual(H,V,Obj); 
  (grid_to_id(Grid,ID), make_indiv_object(ID,H,V,Points,[iz(grid)],Obj))).

empty_grid_to_individual(H,V,Obj):-
  Iv is H + V*34,
  Obj = obj( [ mass(0),
         shape([]),
         colors([]),
         localpoints([]), vis_hv(H, V), 
         rotation(same), 
         loc_xy(1, 1),
         changes([]), 
         iz(grid),
         object_indv_id(empty_grid_to_individual(H,V), Iv),
         grid_size(H, V)]).


%embue_points(ID,_,_,I,I):-!.
%embue_obj_points(ID,H,V,Points,OUT):- make_indiv_object(ID,H,V,Points,OUT).


close_enough_grid(GridIn,GridInCopy,LocalGrid):- 
  \+ \+ (LocalGrid=GridIn, GridIn=@=GridInCopy).

%embue_obj_points(ID,H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_obj_points(ID,H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).



  %pt(Image-->Image9x9),
  %grid_to_id(Grid,Gridname),
  %quaderants_and_center_rays(Image9x9,Quads,Centers,Rays),
  %my_append([Quads,Centers,Rays],FourWay1s00),
  %trace,
  %correctify_objs(Gridname,FourWay1s00,FourWay1s),!.


/*correctify_objs(Gridname,FourWay1s00,FourWay1s):- is_list(FourWay1s00),mapgroup(correctify_objs(Gridname),FourWay1s00,FourWay1s).
correctify_objs(Gridname,obj(List),obj(NOBJ)):- is_list(List), 
   member(grid(Grid),List),
   \+ member(globalpoints(_),List),`
   grid_size(Grid,H,V),
   %trace,
   pt(dleaing_with=obj(List)),
   globalpoints(Grid,Points),
   my_assertion(Points\=[]),
   points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
   %nb_current(test_pairname,ID),
   embue_points1(Gridname,H,V,LoH,LoV,HiH,HiV,Points,OBJ),
   my_append(List,OBJ,NOBJ),!.
correctify_objs(_Gridname,obj(List),obj(List)):-!.
correctify_objs(_Gridname,Obj,Obj).
   %make_embued_points(Grid,H,V,Points,IndvS)
*/

make_indiv_object_list(_ID,_H,_V,[],[]):-!.
make_indiv_object_list(ID,H,V,[E|L],[O|OL]):-   
    must(make_indiv_object(ID,H,V,E,O)),
    make_indiv_object_list(ID,H,V,L,OL).

/*
make_indiv_object_list(ID,H,V,Points,OUT):-
  mapgroup(make_indiv_object(ID,H,V),Points,OUT).
*/

%make_indiv_object(_,_,_,obj(Ps),obj(Ps)):-

make_indiv_object(_ID,_H,_V,IPoints,Obj):- 
  compound(IPoints),IPoints=obj(_),Obj=IPoints,!.
make_indiv_object(ID,H,V,IPoints,Obj):-
  my_assertion(is_list(IPoints)),
  my_partition(is_cpoint,IPoints,Points,Overrides),
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT),
  as_obj(OUT,Obj).

make_indiv_object(ID,H,V,Points,Overrides,Obj):-
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT),!,
  as_obj(OUT,Obj).


:- module_transparent as_obj/2.
as_obj(L,Obj):- is_list(L),!,Obj = obj(L), !. % , register_obj(L).
as_obj(O,Obj):- compound(O), O = obj(_), Obj = O. % , register_obj(L).

:- module_transparent register_obj/1.
%register_obj(O):- quietly((wots(S,weto(dumpST)), asserta(obj_cache(O,S)))),!.
register_obj(L):- obj_cache(LL,_),LL=@=L,!.
register_obj(L):- asserta(obj_cache(L,'')),
  ignore(( false, O=obj(L),mass(O,Mass),Mass>7,format('~N'),arc_portray(O,false),nl)).

:- dynamic(obj_cache/2).
:- module_transparent obj_cache/2.

enum_object(O):- var(O),!,no_repeats_cmp(compare_objs1([same]),O,enum_object0(O)).
enum_object(O):- ptt(enum_object(O)),!.

enum_object0(Obj):- % listing(obj_cache/2),
       obj_cache(O,_S),as_obj(O,Obj).
enum_object0(Obj):- why_grouped(_Why,GS),member(Obj,GS).
/*
enum_object0(S):- why_grouped(_,IndvS),member(S,IndvS).
enum_object0(S):- clause(in_shape_lib(_,S),Body),catch(Body,_,fail).
enum_object0(S):- g2o(_,S).
enum_object0(S):- is_unshared_saved(_,IndvS),member(S,IndvS).
enum_object0(S):- is_grid_id(S,_).
*/
internal_region(Obj,regionOf(Obj)).

make_indiv_object(Points,Overrides,Obj):- 
  globalpoints(Points,RPoints),
  points_range(RPoints,LoH,LoV,HiH,HiV,_HO,_VO),
  gensym('indiv_object_',ID),
  make_indiv_object(ID,HiH,HiV,LoH,LoV,HiH,HiV,RPoints,[Points,Overrides],OUT),
  as_obj(OUT,Obj).


make_indiv_object(VM,Points,Overrides,Obj):- make_indiv_object(VM.id,VM.h,VM.v,Points,Overrides,Obj).

make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,Obj):- 
 (Points==[]-> (dumpST,trace) ; true),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_current(test_pairname,ID),
  Area is Width * Height,
  
  my_assertion((Points\==[],
     mapgroup(between(1,30),[H,V,LoH,LoV,HiH,HiV,Width,Height]))),
 must_det_ll((
  my_assertion(is_list([overrides|Overrides])),
  my_assertion(maplist(is_cpoint,Points)),
  %colors(Points,CC),
  %my_assertion(ground(Points)),
  flag(indiv,Fv,Fv+1),
  Iv is (Fv rem 3000) + 1,
  once(colors_via_pixels(Points,CC)),
  length(Points,Len),
  Empty is Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  remove_color(LPoints,UColorlessPoints),
  sort(UColorlessPoints,ColorlessPoints),
  make_grid(Width,Height,Grid),
  add_global_points(LPoints,Grid,Grid),
  make_grid(Width,Height,LocalGrid),
  CX is LoH + floor(Width/2),CY is LoV + floor(Height/2),
  copy_term(Grid,GridInCopy),
  findall(Shape,
   (guess_shape(Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,LPoints,Shape),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),ShapesUF),
  flatten([ShapesUF],ShapesU),list_to_set(ShapesU,Shapes),
  mapgroup(append_term(iz),Shapes,OShapes),
  my_append(
  [ [mass(Len), shape(ColorlessPoints), colors(CC), localpoints(LPoints),
     vis_hv(Width,Height),  rotation(same), loc_xy(LoH,LoV)],     
    %width(Width), height(Height), area(Area), %missing(Empty),
    [changes([])|OShapes], % [grid(LocalGrid)],    
    [center(CX,CY)],
    [object_indv_id(ID,Iv),globalpoints(Points),grid_size(H,V)]],Ps))),  
  with_objprops(override,Overrides,Ps,OUT1),
  sort_obj_props(OUT1,OUT),!,as_obj(OUT,Obj),verify_object(Obj).

top(7).


make_point_object(VM,Overrides,Point,Obj):- make_point_object(VM.id,VM.h,VM.v,Overrides,Point,Obj).
make_point_object(ID,H,V,Options,C-Point,Obj):-
  hv_point(X,Y,Point), flag(indiv,Fv,Fv+1),
   Iv is (Fv rem 3000) + 1,
    as_obj([mass(1),shape([point_01_01]),colors([cc(C,1.0)]),localpoints([C-point_01_01]),vis_hv(1,1),
    rotation(same),loc_xy(X,Y),
    changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),
    object_indv_id(ID,Iv),globalpoints([C-Point]),
    grid_size(H,V)|Options],Obj).


record_object_change(Rot90,Obj,XObj):- is_object(Obj), object_changes(Obj,Was),
  override_object(changes([Rot90|Was]),Obj,XObj),!.
record_object_change(_Missied,XObj,XObj).


prop_of(mass,mass(_)).
prop_of(size,vis_hv(_,_)).
prop_of(shape,shape(_)).
prop_of(colors,colors(_)).
prop_of(visually,localpoints(_)).
prop_of(rotation,rotation(_)).
prop_of(loc_xy,loc_xy(_,_)).

%sort_obj_props(obj(L),obj(LO)):- !, sort_obj_props(L,LO).
sort_obj_props(L,LO):- L=LO. %predsort(obj_prop_sort_compare,L,LO).
obj_prop_sort_compare(R,A,B):- compound(A), compound(B), !, obj_prop_sort_compare2(R,A,B).
obj_prop_sort_compare(R,A,B):- compare(R,A,B).
e1_member(E,L):- \+ \+ member(E,L).

obj_prop_sort_compare2(R,A,B):- obk_key(A,AK),obk_key(B,BK),compare(R,AK-A,BK-B).

obk_key(A,AK):- clause(prop_of(_,A),true,Ref),nth_clause(prop_of(_,_),Ref,AK),!. 
obk_key(iz(_),50):-!.
obk_key(A,99):- arg(1,A,L), is_grid(L).
obk_key(A,90):- arg(1,A,L), is_list(L).
obk_key(_,80). 


add_shape_info([Info|L],I,O):-!,add_shape_info(Info,I,M),add_shape_info(L,M,O).
add_shape_info([],I,I):-!.
add_shape_info(Info,I,O):- override_object(iz(Info),I,O).

verify_object(Obj):-
  localpoints(Obj,_LP),
  globalpoints(Obj,_GP).

override_object(E,I,O):- with_object(override,E,I,O).

with_object(Op,E,obj(List),O):- !, with_objprops(Op,E,List,MidList),O=obj(MidList),!,verify_object(O).
with_object(Op,E,I,O):- is_group(I), mapgroup(with_object(Op,E),I,O).
% with_object(Op,E,I,O):- is_list(I), !, with_objprops(Op,E,I,O).
with_object(Op,E,     I,     O):- with_objprops(Op,E,I,O).

with_objprops(Op,obj(E),List,MidList):- !, with_objprops(Op,E,List,MidList).
with_objprops(Op,E,obj(List),obj(MidList)):- !, with_objprops(Op,E,List,MidList).
with_objprops(_Op,[],List,List):-!.
with_objprops(Op,[E|Props],List,NewList):-!,
  with_objprops(Op,E,List,MidList),
  with_objprops(Op,Props,MidList,NewList).


with_objprops(delq,E,List,NewList):-functor(E,F,A),functor(R,F,A),
    my_append(Left,[R|Right],List), % E \=@= R,
    my_append(Left,Right,NewList),!.

with_objprops(override,-E,List,NewList):-
    my_append(Left,[R|Right],List), E =@= R,
    my_append(Left,Right,NewList),!.

with_objprops(override,E,List,NewList):- \+ aggregates(E), functor(E,F,A),functor(R,F,A),
    my_append(Left,[R|Right],List), % E \=@= R,
    my_append(Left,[E|Right],NewList),!.

with_objprops(override,E,List,NewList):- 
    my_append(Left,[changes(G)|Right],List), 
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; my_append(Left,[changes(G),E|Right],NewList)),!.


aggregates(iz(_)).
aggregates(birth(_)).
aggregates(touches(_,_)).
aggregates(insideOf(_)).

merge_objs(_VM,Bigger,[],_IPROPS,Bigger):-!.
merge_objs(VM,Bigger,[New|Inside],IPROPS,Combined):- 
  merge_2objs(VM,Bigger,New,IPROPS,NewBigger),
  merge_objs(VM,NewBigger,Inside,[],Combined).
      
merge_2objs(VM,Bigger,NewInside,IPROPS,Combined):-
 globalpoints(Bigger,GP1), globalpoints(NewInside,GP2),      
 indv_props(Bigger,Props1),indv_props(NewInside,Props2),             
 my_append([GP1,GP2],GPoints), my_append([Props1,Props2,IPROPS],Info),
 my_partition(props_not_for_merge,Info,_Exclude,Include),
 make_indiv_object(VM.id,VM.h,VM.v,GPoints,Include,Combined).

props_not_for_merge(globalpoints(_)).
props_not_for_merge(shape(_)).
props_not_for_merge(localpoints(_)).
props_not_for_merge(object_indv_id(_,_)).
props_not_for_merge(loc_xy(_,_)).
props_not_for_merge(vis_hv(_,_)).
props_not_for_merge(colors(_)).
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

object_indv_id(I,ID,Iv):- indv_props_old(I,L),member(object_indv_id(ID,Iv),L),!.
object_indv_id(I,_ID,Fv):- is_grid(I), flag(indiv,Fv,Fv+1).
object_indv_id(I,ID,Iv):- throw(missing(object_indv_id(I,ID,Iv))).
%object_indv_id(_,ID,_Iv):- nb_current(test_pairname,ID).

mass(I,Count):- is_grid(I),!,globalpoints(I,Points), length(Points,Count),!.
mass(I,X):- var_check(I,mass(I,X)).
mass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(mass,[G|Grid],MPoints),sum_list(MPoints,Points).
mass(I,X):- indv_props(I,L),member(mass(X),L),!.
mass(I,X):- is_object(I),!,must_det_ll((localpoints(I,L), length(L,X))).
%mass(I,X):- is_object(I),!,must_det_ll((indv_props(I,L), member(mass(X),L))).
mass(Points,Count):- is_list(Points),length(Points,Count),!.
mass(I,Count):- globalpoints(I,Points),!,length(Points,Count),!.
mass(C-_,1):- nonvar_or_ci(C),!.
mass(I,Count):- globalpoints(I,Points), length(Points,Count),!.

remove_color(C-_,point_01_01):- is_bg_color(C),!.
remove_color(_-P,P).
remove_color(LPoints,ColorlessPoints):- mapgroup(remove_color,LPoints,ColorlessPoints).

decl_pt(setq(object,any,object)).

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



%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
indv_props(obj(L),L):- my_assertion(nonvar(L)),!, is_list(L).
%indv_props(G,L):- dumpST,trace,into_obj(G,O),is_object(O),indv_props(O,L).

indv_props_old(obj(L),L):- nonvar(L), is_list(L).

pmember(E,X):- X=obj(L),!,indv_props(X,L),member(E,L).
pmember(E,X):- sub_term(EE,X),nonvar_or_ci(EE),EE=E,ground(E).
/*pmember(E,L):- is_dict(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
pmember(E,L):- member(EE,L),(EE=E;(is_list(EE),pmember(E,EE))).
pmember(E,L):- member(obj(EE),L),pmember(E,EE).
*/

walls_thick1(G):- localpoints(G,Points),counted_neighbours(Points,ListOfSizes),walls_thick1_sizes(ListOfSizes).
walls_thick1_sizes(List):- mapgroup(size(2),List).

size(A,A):- A==A.

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

var_check(I,G):- var(I),!,var_check_throw(I,G).
var_check(I,G):- resolve_reference(I,O),!,subst001(G,I,O,GG),!,call(GG).
var_check(I,G):- var(I),!,(enum_object(I)*->G;var_check_throw(I,G)).
var_check_throw(I,G):- var(I),wdmsg(error(var(G))),!,dumpST,trace_or_throw(maybe_enum_i(I,G)),call(G).

object_shapeW(I,X):- compound(I),I=obj(L),!,my_assertion(is_list(L)),!,member(iz(X),L).
object_shapeW(I,X):- indv_props(I,L),!,member(iz(X),L).

isz(I,X):- var_check(I,iz(I,X))*->true;(indv_props(I,L),member(iz(X),L)).

obj_prop_val(I,X):- var_check(I,obj_prop_val(I,X))*->true;(indv_props(I,L),member(X,L)).

resolve_reference(R,Var):- is_dict(R),Var = R.grid.
resolve_reference(R,Var):- compound(R),arc_expand_arg(R,Var,Goal),!,call(Goal).
rotation(G,X):- is_group(G),!,mapgroup(rotation,G,Points),append_sets(Points,X).
rotation(I,X):- var_check(I,rotation(I,X)).
rotation(I,X):- indv_props(I,L),member(rotation(X),L).
rotation(_,same).

%hv_cvalue(Grid,Color,H,V):- hv_cg_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- plain_var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar_or_ci(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).

object_changes(G,X):- is_group(G),!,mapgroup(object_changes,G,Points),append_sets(Points,X).
object_changes(I,X):- indv_props_old(I,L),member(changes(X),L).

% Is there an advantage to counting down?
all_points_between(_Grid,_LowH,_LowV,_GH,GV,_Hi,Vi,Points,Points):- Vi>GV,!.
all_points_between(Grid,LowH,LowV,GH,GV,Hi,Vi,Points,PointsO):-
  ((color_spec_or_fail(Grid,C,Hi,Vi), hv_point(Hi,Vi,Point)) 
   -> PointsT = [C-Point|Points] ; PointsT = Points),
  (Hi>GH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),!,
   all_points_between(Grid,LowH,LowV,GH,GV,H,V,PointsT,PointsO).

color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_fg_color(C2,C);(attvar(C2),C=C2)).

% Is there an advantage to counting down?
all_points_between_include_bg(_Grid,_LowH,_LowV,_GH,GV,_Hi,Vi,Points,Points):- Vi>GV,!.
all_points_between_include_bg(Grid,LowH,LowV,GH,GV,Hi,Vi,Points,PointsO):-
  ((color_spec_or_fail_include_bg(Grid,C,Hi,Vi),
  hv_point(Hi,Vi,Point))
     -> PointsT = [C-Point|Points] ; PointsT = Points),
   (Hi>GH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),!,
   all_points_between_include_bg(Grid,LowH,LowV,GH,GV,H,V,PointsT,PointsO).

color_spec_or_fail_include_bg(Grid,C,Hi,Vi):-
  hv_c_value(Grid,C2,Hi,Vi),
  (is_spec_color(C2,C);(attvar(C2),C=C2);(var(C2),C=C2)).

color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi):- 
  get_bgc(BGC),
  hv_c_value_or(Grid,C2,Hi,Vi,BGC),
  (is_spec_color(C2,C);(attvar(C2),C=C2);(var(C2),C=BGC)).
  
grid_cpoint(Grid,C-Point,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
(is_spec_color(C2,C);(attvar(C2),C=C2);(var(C2),C=C2)),
  hv_point(Hi,Vi,Point).

grid_to_points(Grid,Points):- grid_size(Grid,HH,HV),!, grid_to_points(Grid,HH,HV,Points).
% Is there an advantage to counting down?
grid_to_points(Grid,HH,HV,Points):- all_points_between(Grid,1,1,HH,HV,1,1,[],Points),!. 
% Is there an advantage to counting down?
grid_to_points_include_bg(Grid,Points):- grid_size(Grid,HH,HV),!,all_points_between_include_bg(Grid,1,1,HH,HV,1,1,[],Points),!. 
/*
grid_to_points(Grid,HH,HV,Points):-  throw(all_points_between),
  findall(C-Point,(between(1,HV,V),between(1,HH,H),
    once((hv_cg_value(Grid,C2,H,V),
          %pt(hv_cg_value(C2,H,V)),
          is_spec_fg_color(C2,C),
          hv_point(H,V,Point)))),Points),!.
*/
point_corners(Obj,Dir,CPoint):-  globalpoints(Obj,Points),sort(Points,SPoints),
  isz(Obj,Shape),
  (points_corner_dir(Shape,Dir)*->(SPoints=[CPoint|_];last(SPoints,CPoint));fail).
   

globalpoints(I,X):-  (var_check(I,globalpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
globalpoints(G,[G]):- is_point(G),!.
globalpoints(C-P,[C-P]):-!.
globalpoints(G,G):- maplist(is_point,G),!.
globalpoints([],[]):-!.
globalpoints(Atom,_):- \+ compound(Atom),!,trace_or_throw(globalpoints(Atom)).
globalpoints(options(X),_Points):- trace_or_throw(globalpoints(options(X))).
globalpoints(I,X):- globalpoints0(I,X),!.
globalpoints(Grid,Points):- is_grid(Grid),!, grid_to_points(Grid,Points).
globalpoints(Grid,Points):- is_list(Grid),!,mapgroup(call(globalpoints),Grid,MPoints),append_sets(MPoints,Points).
globalpoints(I,X):- localpoints0(I,X),!.
globalpoints(G,G):- mapgroup(is_point,G),!.
globalpoints(I,X):- throw(unknown(globalpoints(I,X))).

  globalpoints0(I,X):- indv_props(I,L),member(globalpoints(X),L), my_assertion(mapgroup(is_cpoint,X)),!.
  %globalpoints0(I,G):- localpoints(I,L),is_points_list(L),loc_xy(I,X,Y),offset_points(X,Y,L,G),!.

localpoints(I,X):- (var_check(I,localpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).

localpoints(G,[G]):- is_point(G),!.
localpoints(I,X):- localpoints0(I,X),!.
localpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), grid_to_points(Grid,HH,VV,Points).
localpoints(Grid,Points):- is_group(Grid),!,mapgroup(localpoints,Grid,MPoints),append_sets(MPoints,Points).
localpoints(G,G):- maplist(is_point,G),!.
localpoints(Grid,Points):- is_list(Grid),!,maplist(localpoints,Grid,MPoints),append_sets(MPoints,Points).

localpoints(Atom,_):- \+ compound(Atom),!,trace_or_throw(localpoints(Atom)).
localpoints(I,X):- globalpoints0(I,X),!.
localpoints(G,G):- mapgroup(is_point,G),!.
localpoints(I,X):- throw(unknown(localpoints(I,X))).

  localpoints0(I,X):- indv_props(I,L),member(localpoints(X),L), my_assertion(mapgroup(is_cpoint,X)),!.
  %localpoints(I,X):- into_grid(I,G),!,grid_size(G,H,V),grid_to_points(G,H,V,X).

localpoints_include_bg(Grid,Points):- is_grid(Grid),!, grid_to_points_include_bg(Grid,Points),!.
localpoints_include_bg(I,X):- \+ is_grid(I), localpoints(I,X),!.
 


%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
*/

%colors(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.
colors(I,X):- is_object(I),indv_props(I,L),member(colors(X),L),!.
colors(G,[cc(black,0.0)]):- G==[],!.
colors(G,BFO):- colors_via_pixels(G,BFO),!.
colors_via_pixels(G,BFO):- quietly((pixel_colors(G,GF),list_to_set(GF,GS),
  count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!,
  into_cc(SK,BFO))).
%colors(G,X):- is_group(G),!,mapgroup(colors,G,Points),append_sets(Points,X).

shape(G,X):- is_group(G),!,mapgroup(shape,G,Points),append_sets(Points,X).
% returns the objects decolorize localpoints
shape(I,X):- is_object(I), indv_props(I,L),member(shape(X),L).
shape(I,X):- localpoints(I,Points),mapgroup(arg(2),Points,X).

get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).


object_grid(I,G):- is_grid(I),!,G=I.
object_grid(Group,List):- is_group(Group),!,override_group(object_grid(Group,List)),!.
%object_grid(I,G):- indv_props(I,L),member(global_points(X),L),member(vis_hv(H,V),L),!,points_to_grid(H,V,X,G),!.
%object_grid(I,G):- indv_props(I,L),member(localpoints(X),L),member(vis_hv(H,V),L),!,points_to_grid(H,V,X,G),!.
%%object_grid(I,G):- vis_hv(I,H,V),localpoints(I,LP),points_to_grid(H,V,LP,G),!.
object_grid(I,G):- localpoints(I,LP),vis_hv(I,H,V),points_to_grid(H,V,LP,G),!.
%object_grid(I,G):- globalpoints(I,GP),into_grid(GP,G),!.

loc_xy_term(I,loc(X,Y)):- loc_xy(I,X,Y),!.

loc_xy(G,X,Y):- is_group(G),!,mapgroup(loc_xy_term,G,Offsets),sort(Offsets,[loc(X,Y)|_]). % lowest loc
loc_xy(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc_xy(I,X,Y):- into_obj(I,O), indv_props(O,L),member(loc_xy(X,Y),L),!.
%loc_xy(NT,H,V):- trace, named_gridoid(NT,G),loc_xy(G,H,V).

vis_hv_term(I,size(X,Y)):- vis_hv(I,X,Y),!.

vis_hv(G,X,Y):- is_group(G),!,mapgroup(vis_hv_term,G,Offsets),sort(Offsets,HighToLow),last(HighToLow,size(X,Y)).
vis_hv(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
vis_hv(I,X,Y):- indv_props(I,L),member(vis_hv(X,Y),L),!.
vis_hv(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
vis_hv(NT,H,V):-  trace, named_gridoid(NT,G),vis_hv(G,H,V).


%vis_hv(Obj,size(H,V)):- vis_hv(Obj,H,V).
%loc_xy(Obj,loc(H,V)):- loc_xy(Obj,H,V).

center_term(Obj,loc(H,V)):- center(Obj,H,V).

center(I,X,Y):- indv_props(I,L),member(center(X,Y),L),!.
%center(Obj,CX,CY):- vis_hv(Obj,H,V), loc_xy(Obj,X,Y),CX is X + floor(H/2),CY is Y + floor(V/2).



object_color(HV,C):- color(HV,C).

color(HV,C):- colors(HV,[cc(C,_)]),!.
color(HV,multicolor(Stuff)):- colors(HV,Stuff),!.

main_color(HV,C):- colors(HV,[cc(C,_)|_]).
first_gpoint(HV,P):- globalpoints(HV,[P|_]).
last_gpoint(HV,P):- globalpoints(HV,PS),last(PS,P).
any_gpoint(HV,P):- globalpoints(HV,L),member(P,L).

rebuild_from_localpoints(Obj,NewObj):-
  localpoints(Obj,Points),
  rebuild_from_localpoints(Obj,Points,NewObj).

rebuild_from_localpoints(Obj,WithPoints,NewObj):-
 must_det_ll((
  localpoints(WithPoints,Points),
  rotation(Obj,Rot),unrotate(Rot,UnRot),
  loc_xy(Obj,X,Y),vis_hv(Obj,H,V),
  grid_size(Obj,GH,GV),
  object_indv_id(Obj,ID,_Iv),grid_size(Obj,GH,GV),
  points_to_grid(H,V,Points,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints(UnRotGrid,LPoints),
  offset_points(X,Y,LPoints,GPoints),
  indv_props(Obj,Props),
  my_partition(lambda_rev(member([colors(_),mass(_),shape(_),
            iz(multicolored(_)),globalpoints(_),localpoints(_)])),Props,_,PropsRetained),
    make_indiv_object(ID,GH,GV,Points,[vis_hv(H,V),loc_xy(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],NewObj))),
   verify_object(NewObj),
  !.


rebuild_from_globalpoints(Obj,NewObj):-
  globalpoints(Obj,GPoints),
  rebuild_from_localpoints(Obj,GPoints,NewObj).
rebuild_from_globalpoints(Obj,GPoints,NewObj):-
  rotation(Obj,Rot),unrotate(Rot,UnRot),
  loc_xy(Obj,X,Y),vis_hv(Obj,H,V),
  deoffset_points(X,Y,GPoints,LPoints),
  object_indv_id(Obj,ID,_Iv),grid_size(Obj,GH,GV),
  points_to_grid(H,V,LPoints,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints(UnRotGrid,Points),
  indv_props(Obj,Props),
  my_partition(lambda_rev(member([colors(_),mass(_),shape(_),
            iz(multicolored(_)),globalpoints(_),localpoints(_)])),Props,_,PropsRetained),
    make_indiv_object(ID,GH,GV,Points,[vis_hv(H,V),loc_xy(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],NewObj),
    verify_object(NewObj),
 !.



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

:- add_history(find_outline).
find_outline:- clsmake, forall(find_outline1,true).
find_outline1:- arc_grid(Grid), dash_chars, find_outline_pred(find_outlines_fast(_),Grid).



%find_outline_path:- clsmake, forall(find_outline_path1,true).
%find_outline_path1:- arc_grid(Grid), dash_chars, find_outline_pred(find_outline_sols(find_outline_path),Grid).

find_outlines(Grid):- find_outline_pred(find_outlines,Grid).
%find_outline_paths(Grid):- find_outline_pred(find_outline_sols(find_outline_path),Grid).

fail_over_time(Secs,Goal,Else):- notrace(catch(call_with_time_limit(Secs,Goal),time_limit_exceeded,(Else,fail))).
fail_over_time(Secs,Goal):- fail_over_time(Secs,Goal,true).

grid_of(LO,LO,[]):- is_grid(LO),!.
grid_of(LO,O,H):- arg(1,LO,O),arg(2,LO,H).
find_outline_pred(Find_OUTLINE,Grid):- is_grid(Grid),!,
   grid_to_id(Grid,ID),   
   grid_size(Grid,H,V),
   writeln(ID),
   fail_over_time(6,call(Find_OUTLINE,Grid,SOLS,LeftOver),
     (writeln("TOO SLOWWWW"=Find_OUTLINE),
      print_grid(H,V,Grid),
      writeln("TOO SLOWWWW"=Find_OUTLINE))),
   my_append(SOLS,[leftover(LeftOver)],SOLSL),
   dash_chars,
   (SOLS==[]-> ((writeln("NOTHING FROM"=Find_OUTLINE),print_grid(H,V,Grid),writeln("NOTTAAA"=Find_OUTLINE),!,fail));true),
   member(OL,SOLSL),
   (OL=leftover(LeftOver)
    -> (nl,
      ignore((nonvar(LeftOver),nl,write(leftover=Find_OUTLINE),print_grid(H,V,LeftOver))),
      write('from '),write(Find_OUTLINE),print_grid(H,V,Grid))
    ;((grid_of(OL,O,Hits),once((mass(O,M))),
     nl,write(solution(Hits,M)), print_grid(H,V,O)))).
find_outline_pred(ID):- into_grid(ID,Grid),find_outlines(Grid).

arc_grid(Nth,X):- offset(Nth,arc_grid(X)).
%find_outline(Grid,Result,Grid4):- is_grid(Grid),!,grid_to_points(Grid,Points),!,find_outline(Points,Result,Grid4).

/*
find_outline(Grid,Sol,Rest):-
  find_outlines(Grid,Sols,Rest),!,
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
 point_groups_by_color(colormass,Groups,Points,Rest2), 
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


find_outlines(Grid,Solutions,Rest):- 
  fail_over_time(4,find_outlines([perfect],Grid,Solutions,Rest)).

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
  
find_outlinez([_Perfect|_],Grid,sol(ResultO,[]),LeftOver):-  !, % Perfect = perfect, 
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

%guess_shape(G,LocalGrid,I,O,N,H,V,Colors,Points,walls_thick(1)):- walls_thick1(G).
/*
guess_shape(GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.loc_xy=loc_xy(LoH,LoV),
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





:- style_check(-singleton).
:- style_check(-discontiguous).

guess_shape(GridIn,LocalGrid,I,Empty,N,H,V,[cc(Zero,_)],Points,background):- is_bg_color(Zero).
%guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,view_sq):- H == V.%guess_shape(GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangle):- H>1, V>1.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,Colors,Points,multicolored(Len)):- Colors=[_,_|_],length(Colors,Len).
guess_shape(GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad).
guess_shape_poly(I,0,N,H,V,Colors,Points,solid):- N > 1.

guess_shape(GridIn,LocalGrid,I,E,N,H,V,Colors,Points,Keypad):- 
  once(guess_shape_poly(I,E,N,H,V,Colors,Points,Keypad)).

guess_shape_poly(I,0,1,1,1,Colors,Points,dot):-!.
guess_shape_poly(I,_,_,_,_,Colors,[Point],dot):-!.
guess_shape_poly(I,0,N,N,1,Colors,Points,hv_line(h)):- N > 1.
guess_shape_poly(I,0,N,1,N,Colors,Points,hv_line(v)):- N > 1.
guess_shape_poly(I,0,N,H,V,Colors,Points,rectangle):- N>1, H\==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,square):- N>1,H==V,!.
guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0,once(H>1;V>1).

%guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,solidity(A)):- solidity(Points,A).
guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,Solid):- (is_jagged(Points)->Solid=jagged(true);Solid=jagged(false)).
guess_shape(GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outline(SN)):- H>2,V>2,N>4,
  (find_outlines(Points,Sol,Rest)->(length(Sol,SN),SN>0,length(Rest,RN))),!.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outl):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).


:- fixup_exports.


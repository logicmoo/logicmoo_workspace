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
:- discontiguous(obj_to_program/3).

:- dynamic(decl_pt/1).

:- thread_local(t_l:id_cells/2).

deoffset_points(1,1,Points,Points):-!.
deoffset_points(OH,OV,Point,LPoint):- map_pred(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- is_nc_point(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

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
  Obj = obj( [ mass(0),
         shape([]),
         colors([]),
         localpoints([]), v_hv(H, V), 
         rotation(same), 
         loc(1, 1),
         %pen([fg]),
         changes([]), 
         iz(grid),
         o_i_d(empty_grid_to_individual(H,V), Iv),
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

:- module_transparent register_obj/1.
%register_obj(O):- quietly((wots(S,weto(arcST)), asserta(obj_cache(TestID,O,S)))),!.
register_obj(L):- notrace(o2g(L,_)).
/*register_obj(L):- asserta(obj_cache(TestID,L,'')),
  ignore(( false, O=obj(L),mass(O,Mass),Mass>7,format('~N'),arc_portray(O,false),nl)).
*/
:- dynamic(obj_cache/3).
:- module_transparent obj_cache/2.

enum_object(O):- var(O),!,no_repeats_cmp(=@=,O,enum_object0(O)).
enum_object(O):- ptt(enum_object(O)),!.

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
enum_object0(S):- is_grid_id(S,_).
*/
internal_region(Obj,regionOf(Obj)).


% @TODO
reclumped([A,B,C,D|Rest],[A,B,C,D]):- reclumped(Rest,[A,B,C,D]),!.
reclumped([A,B,C|Rest],[A,B,C]):- reclumped(Rest,[A,B,C]),!.
reclumped([A,B|Rest],[A,B]):- reclumped(Rest,[A,B]),!.
reclumped(Rest,Seq):- append(Seq,_,Rest),!.
reclumped(PenColors,PenColors).

fg_points(Points,FgPoints):- include(is_fg_point,Points,FgPoints).
is_fg_point(CPoint):- \+ (only_color_data(CPoint,Color),is_bg_color(Color)).

rev_key(C-P,P-C).

sort_points(P0,P2):- 
   (P0==[] -> (trace) ; true),
   my_assertion(is_list(P0)),
   sort(P0,P1),
   my_assertion(P1\==[]),  my_assertion(maplist(is_cpoint,P1)),  
   maplist(rev_key,P1,R1),keysort(R1,R2),maplist(rev_key,R2,P2).

same_globalpoints(O1,O2):- globalpoints(O1,P1),same_globalpoints_ps_obj(P1,O2).
same_globalpoints_ps_obj(S1,O2):- globalpoints(O2,S2),!,S1=@=S2.

ensure_indiv_object(VM,IPoints,Obj):- 
  ((compound(IPoints), IPoints = obj(_)) -> (IPoints = Obj, nop(addObjects(VM,Obj)));
   (my_partition(is_cpoint,IPoints,Points,Overrides),
    make_indiv_object(VM,Overrides,Points,Obj))).

make_point_object(VM,Overrides,C-Point,Obj):- 
  must_det_ll(make_indiv_object(VM,Overrides,[C-Point],Obj)).

globalpoints_maybe_bg(ScaledGrid,Points):- globalpoints(ScaledGrid,Points),Points\==[],!.
globalpoints_maybe_bg(ScaledGrid,GPoints):- globalpoints_include_bg(ScaledGrid,GPoints),!.
globalpoints_include_bg(ScaledGrid,GPoints):- loc(ScaledGrid,OH,OV),
  localpoints_include_bg(ScaledGrid,Points),!,offset_points(OH,OV,Points,GPoints).

make_indiv_object(VM,Overrides,LPoints,NewObj):-
 must_det_ll((
  globalpoints_maybe_bg(LPoints,GPoints),
  sort_points(GPoints,Points),
  Objs = VM.objs,
  Orig = _,!,
  must_det_ll(((select(Orig,Objs,Rest),same_globalpoints_ps_obj(Points,Orig))
    -> must_det_ll((override_object(Overrides,Orig,NewObj),print_grid(Orig), ROBJS = Rest))
    ; must_det_ll((make_indiv_object_s(VM.id,VM.h,VM.v,Overrides,Points,NewObj), ROBJS = Objs)))),!,
  NEW = [NewObj|ROBJS])),!,
  set(VM.objs)=NEW.

make_indiv_object_no_vm(ID,GH,GV,Overrides,LPoints,Obj):- 
  globalpoints_maybe_bg(LPoints,GPoints),
  sort_points(GPoints,SPoints),
  make_indiv_object_s(ID,GH,GV,Overrides,SPoints,Obj).


make_indiv_object_s(_ID,GH,GV,Overrides,Points,ObjO):- 
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  once(member(v_hv(Width,Height),Overrides);(Width is HiH-LoH+1,Height is HiV-LoV+1)),
  %luser_getval(test_pairname,ID),
  Area is Width * Height,
  my_assertion(is_list([overrides|Overrides])),
  my_assertion(maplist(is_cpoint,Points)),
  %colors(Points,CC),
  %my_assertion(ground(Points)),
  %flag(indiv,Fv,Fv+1),
  %Iv is (Fv rem 3000) + 1,
  colors_via_pixels(Points,CC),
  fg_points(Points,FgPoints),
  length(FgPoints,FgLen),length(Points,Len),
  Empty is Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  % sort(LPoints,LPointsS), maplist(arg(1),LPointsS,BPenColors), clumped(BPenColors,CPenColors), reclumped(CPenColors,PenColors),
  %remove_color(LPoints,UColorlessPoints),

  make_grid(Width,Height,Grid),
  add_global_points(LPoints,Grid,Grid),
  once(member(grid(LocalGrid),Overrides);make_grid(Width,Height,LocalGrid)),

  % calc center
  must_det_ll(once(
   ((member(UFgPoints,[FgPoints,Points]),
    ((CX is LoH + floor(Width/2),CY is LoV + floor(Height/2), hv_point(CX,CY,Point), member(_-Point,UFgPoints));
    (length(UFgPoints,UFgLen),CP is round(UFgLen/2), nth1(CP,UFgPoints,Point),hv_point(CX,CY,Point)))));
   (CX is LoH + floor(Width/2),CY is LoV + floor(Height/2)))),

  copy_term(Grid,GridInCopy),

  %grid_to_gridmap(Grid,ColorlessPoints), 

  findall(Shape,
   (guess_shape(GH,GV,Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,LPoints,Shape),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),ShapesUF),
  flatten([ShapesUF],ShapesU),list_to_set(ShapesU,Shapes),
  maplist(append_term(iz),Shapes,OShapes),

  % rotated local points 
  /*(memberchk(rotation(RotOut),Overrides)-> FinalLocalPoints=LPoints;
    must_det_ll((tips_to_rot(LPoints,GH,GV,RotOut,Final),localpoints(Final,FinalLocalPoints)))),
  delistify_single_element(RotOut,RotO),
  */
  RotO=same, FinalLocalPoints = LPoints,


  % colors 
  maplist(arg(2),FinalLocalPoints,ColorlessPoints), maplist(arg(1),FinalLocalPoints,Colorz),
  cclumped(Colorz,PenColors),

  append(
  [ [ v_hv(Width,Height), mass(Len), center(CX,CY), 
     shape(ColorlessPoints),
     loc(LoH,LoV), colors(CC), 
     localpoints(FinalLocalPoints),
     pen(PenColors),
     rotation(RotO),
     cmass(FgLen)],     
    %width(Width), height(Height), area(Area), %missing(Empty),
    [changes([])], % [grid(LocalGrid)],    
    OShapes,
    [iz(yc(CY)),iz(xc(CX))],
    [iz(row(LoV)),iz(col(LoH)),iz(tall(Height)),iz(wide(Width))],
    [%o_i_d(ID,Iv),
     % globalpoints(Points),
     %grid_size(GH,GV)
    ]],Ps),  
  with_objprops(override,Overrides,Ps,OUT1),
  sort_obj_props(OUT1,OUT),!,as_obj(OUT,Obj),verify_object(Obj),!,
 must_det_ll((ObjO = Obj)).

top(7).

cclumped(Items, Counts) :- cclump(Items, Counts).
cclump([H|T0], [CH|T]) :-
    lists:ccount(T0, H, T1, 1, C),
    cclump(T1, T),
    color_c(C,H,CH).  
cclump([], []).  color_c(1,H,H). color_c(C,H,C-H).

prop_order([v_hv,cmass,loc,mass,shape,localpoints,rotation,colors,iz,globalpoints,o_i_d,grid_size]).
iz_o([crow,ccol,row,col,tall,wide,chromatic,dot]).



record_object_change(Rot90,Obj,XObj):- is_object(Obj), object_changes(Obj,Was),
  override_object(changes([Rot90|Was]),Obj,XObj),!.
record_object_change(_Missied,XObj,XObj).

ignore_xf(_):-!.
ignore_xf(G):- ignore(notrace(catch(G,_,fail))).

%guess_pretty2(O):- mortvar((copy_term(O,C),pretty1(O),O=@=C)).

show_objs_as_code(O):- var(O),!,pt(var(O)),!.
show_objs_as_code(Grid):- is_grid(Grid),!.

show_objs_as_code(Objs):- is_list(Objs),!,
  debug_var('VirtMachine',VM),
  findall(Code,(member(Obj,Objs),obj_to_program(Obj,Code,VM),maplist(ignore_xf,Code)),List),
  flatten(List,LList),
  pretty1(LList),
  guess_prettyf(LList),
  ptt(LList).

show_objs_as_code(VM):-
  Objs = VM.objs,
  show_objs_as_code(Objs),!.

draw_seg(Pen,X,Y,H,V,VM):- nop(doing(draw_seg(Pen,X,Y,H,V,VM))).


obj_to_program(Obj,Program,VM):- 
  (isz(Obj,hv_line(_));isz(Obj,dg_line(_));v_hv(Obj,1,_);v_hv(Obj,_,1)),!,
  Program = 
  [loc(Obj,X,Y),v_hv(Obj,H,V),
   object_pen(Obj,Pen),
   draw_seg(Pen,X,Y,H,V,VM)].

add_grid_at_offset(X,Y,Grid,VM):- !,
  grid_to_points(Grid,Ps),
  offset_points(X,Y,Ps,GPs),
  addGPoints(VM,GPs).

obj_to_program(Obj,Program,VM):-
  (isz(Obj,outl);isz(Obj,outline(_))),!,
  Program = 
  [loc(Obj,X,Y),   
   object_grid(Obj,Grid),
   add_grid_at_offset(X,Y,Grid,VM)].

obj_to_program(Obj,Program,VM):-
  isz(Obj,dot),!,
  Program =  [addGPoints(VM,Obj)].

obj_to_program(Obj,Program,VM):-
  Program = 
  [globalpoints(Obj,Ps), 
   addGPoints(VM,Ps)].


object_pen(Obj,pen(Color)):- color(Obj,Color),!.
object_pen(Obj,Pen):- color(Obj,Pen),!.

prop_of(size,v_hv(_,_)).
prop_of(mass,mass(_)).
prop_of(mass,cmass(_)).
prop_of(shape,shape(_)).
prop_of(colors,colors(_)).
prop_of(visually,localpoints(_)).
prop_of(rotation,rotation(_)).
prop_of(loc,loc(_,_)).


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
obk_key(A,AK):- callable(A), clause(prop_of(_,A),true,Ref),nth_clause(prop_of(_,_),Ref,AK),!. 
obk_key(iz(C),O):- compound(C),!,obk_key(C,O).
obk_key(touched(_,_,_),-150).
obk_key(touched(_,_),-150).
obk_key(link(_,_,_),-150).
obk_key(link(_,_),-150).
obk_key(chromatic(_),150):-!.
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

with_objprops(override,E,List,NewList):- \+ aggregates(E), functor(E,F,A),functor(R,F,A),
    my_append(Left,[R|Right],List), % E \=@= R,
    my_append(Left,[E|Right],NewList),!.

with_objprops(override,E,List,NewList):- 
    my_append(Left,[changes(G)|Right],List), !,
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; my_append(Left,[changes(G),E|Right],NewList)),!.

with_objprops(override,E,List,NewList):- 
    my_append(Left,[iz(G)|Right],List), 
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; my_append(Left,[iz(G),E|Right],NewList)),!.


aggregates(iz(_)).
aggregates(o(_,_,_)).
aggregates(birth(_)).
aggregates(link(_,_,_)).
aggregates(link(_,_)).
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
 make_indiv_object(VM,Include,GPoints,Combined).

props_not_for_merge(globalpoints(_)).
props_not_for_merge(shape(_)).
props_not_for_merge(localpoints(_)).
props_not_for_merge(o_i_d(_,_)).
props_not_for_merge(loc(_,_)).
props_not_for_merge(v_hv(_,_)).
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

%indv_u_props(I,[localpoints(Ps),loc(X,Y),pen(Pen),v_hv(H,V),rotation(Rot)]):- loc(I,X,Y),shape(I,Ps),pen(I,Pen),v_hv(I,H,V),rotation(I,Rot),!.
indv_u_props(I,[v_hv(H,V),cmass(C),loc(X,Y),localpoints(Ps),rotation(Rot)]):- 
             v_hv(I,H,V), cmass(I,C),loc(I,X,Y),localpoints(I,Ps),rotation(I,Rot),!.
%indv_u_props(I,[shape(Ps),center(X,Y),pen(Pen),v_hv(H,V),rotation(Rot)]):- center(I,X,Y),shape(I,Ps),pen(I,Pen),v_hv(I,H,V),rotation(I,Rot),!.

:- dynamic(is_iv_for/2).
iv_for(L,Iv):- copy_term(L,CT,_),numbervars(CT,0,_,[attvar(bind),singletons(true)]),term_hash(CT,Fv),
 number(Fv), Iv is (Fv rem 800) + 1, (\+ is_iv_for(Iv,_) -> assert(is_iv_for(Iv,L)) ; true).

o_i_d(I,_ID,Fv):- is_grid(I),!, flag(indiv,Fv,Fv+1).
o_i_d(I,ID,Iv):- indv_props(I,L),member(o_i_d(ID,Iv),L),!.
o_i_d(I,_,Iv):- indv_u_props(I,L),iv_for(L,Iv),!.
%o_i_d(I,ID,Fv):- into_obj(I,O),!,o_i_d(O,ID,Fv).
o_i_d(I,ID,Iv):- trace_or_throw(missing(o_i_d(I,ID,Iv))).
%o_i_d(_,ID,_Iv):- luser_getval(test_pairname,ID).

mass(I,Count):- is_grid(I),!,globalpoints(I,Points), length(Points,Count),!.
mass(I,X):- var_check(I,mass(I,X)).
mass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(mass,[G|Grid],MPoints),sum_list(MPoints,Points).
mass(I,X):- indv_props(I,L),member(mass(X),L),!.
mass(I,XX):- is_object(I),!,must_det_ll((localpoints(I,L), length(L,X))),!,XX=X.
%mass(I,X):- is_object(I),!,must_det_ll((indv_props(I,L), member(mass(X),L))).
mass(Points,Count):- is_list(Points),length(Points,Count),!.
mass(I,Count):- globalpoints(I,Points),!,length(Points,Count),!.
mass(C-_,1):- nonvar_or_ci(C),!.
mass(I,Count):- globalpoints(I,Points), length(Points,Count),!.

cmass(I,Count):- is_grid(I),!,globalpoints(I,Points), length(Points,Count),!.
cmass(I,X):- var_check(I,cmass(I,X)).
cmass([G|Grid],Points):- (is_group(Grid);(is_list(Grid),is_group(G))),!,mapgroup(cmass,[G|Grid],MPoints),sum_list(MPoints,Points).
cmass(I,X):- indv_props(I,L),member(cmass(X),L),!.
cmass(I,XX):- is_object(I),!,must_det_ll((localpoints(I,L), length(L,X))),!,XX=X.
%cmass(I,X):- is_object(I),!,must_det_ll((indv_props(I,L), member(cmass(X),L))).
cmass(Points,Count):- is_list(Points),length(Points,Count),!.
cmass(I,Count):- globalpoints(I,Points),!,length(Points,Count),!.
cmass(C-_,1):- nonvar_or_ci(C),!.
cmass(I,Count):- globalpoints(I,Points), length(Points,Count),!.


color_mass(Color,Int):- var(Color),!,Int=13.
color_mass(Points,Count):- is_list(Points),!,maplist(color_mass,Points,MPoints),!,sum_list(MPoints,Count).
color_mass(Color,Int):- ground(Color),color_int(Color,Int),!.
color_mass(Color,Int):- number(Color),Color=Int,!.
color_mass(Obj,Count):- nonvar(Obj),localpoints(Obj,Points),!,color_mass(Points,Count),!.
color_mass(_,0).

%remove_color(C-_,point_01_01):- is_bg_color(C),!.
%remove_color(_-P,P).
%remove_color(LPoints,ColorlessPoints):- mapgroup(remove_color,LPoints,ColorlessPoints).

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
indv_props(C,LL):- \+ compound(C),!,nonvar(C),g2o(C,I),indv_props(I,LL).
indv_props(C,LL):- C=obj(L),!,(is_list(L)->LL=L ; (copy_term(L,LL),append(LL,[],LL))),!.
indv_props(C,LL):- arc_expand_arg(C,I,G),call(G),!,indv_props(I,LL).
indv_props_for_noteablity(obj(L),Notes):- my_assertion(nonvar(L)),!, include(is_prop_for_noteablity,L,Notes).

%is_not_prop_for_noteablity(globalpoints).
%is_not_prop_for_noteablity(grid_size).
%is_not_prop_for_noteablity(o_i_d).

%indv_props(G,L):- arcST,trace,into_obj(G,O),is_object(O),indv_props(O,L).

pmember(E,X):- X=obj(L),!,indv_props(X,L),member(E,L).
pmember(E,X):- sub_term(EE,X),nonvar_or_ci(EE),EE=E,ground(E).
/*pmember(E,L):- is_map(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
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

var_check(I,_):- is_grid(I),!,fail.
var_check(I,G):- var(I),!,var_check_throw(I,G).
var_check(I,_):- I==[],!,fail.
var_check(I,G):- resolve_reference(I,O),I\==O,!,subst001(G,I,O,GG),GG\==G,!,call(GG).
var_check(I,G):- var(I),!,(enum_object(I)*->G;var_check_throw(I,G)).
var_check_throw(I,G):- var(I),wdmsg(error(var(G))),!,arcST,wdmsg(error(var(G))),break,trace_or_throw(maybe_enum_i(I,G)),call(G).

object_shapeW(I,X):- compound(I),I=obj(L),!,my_assertion(is_list(L)),!,member(iz(X),L).
object_shapeW(I,X):- indv_props(I,L),!,member(iz(X),L).

isz(I,X):- is_list(I),I=[O],!,isz(O,X).
isz(I,X):- var_check(I,iz(I,X))*->true;(indv_props(I,L),member(iz(X),L)).

obj_prop_val(I,X):- var_check(I,obj_prop_val(I,X))*->true;(indv_props(I,L),member(X,L)).

vm_to_printable(D,R) :- Objs = D.objs,!, (Objs\==[] -> R = Objs; R = D.grid ).

resolve_reference(R,Var):- is_map(R),!,Objs = R.objs,!, (Objs\==[] -> Var=Objs; Var = R.grid).
resolve_reference(R,Var):- compound(R),arc_expand_arg(R,Var,Goal),!,call(Goal).
resolve_reference(R,Var):- nonvar(R),R\=obj(_),known_gridoid(R,Var),!.

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
object_changes(I,X):- indv_props(I,L),member(changes(X),L).

% Is there an advantage to counting down?
all_points_between(_Grid,_LowH,_LowV,_GH,GV,_Hi,Vi,Points,Points):- Vi>GV,!.
all_points_between(Grid,LowH,LowV,GH,GV,Hi,Vi,Points,PointsO):-
  ((color_spec_or_fail(Grid,C,Hi,Vi), hv_point(Hi,Vi,Point)) 
   -> PointsT = [C-Point|Points] ; PointsT = Points),
  (Hi>GH -> (H = LowH, V is Vi+1) ; (H is Hi +1, V = Vi )),!,
   all_points_between(Grid,LowH,LowV,GH,GV,H,V,PointsT,PointsO).

color_spec_or_fail(Grid,C,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
  get_bgc(BGC),
  (is_spec_fg_color(C2,C);(attvar(C2),C=C2); (\+ plain_var(C2), C=C2)),
  C\==BGC.

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
  (is_spec_color(C2,C);(atomic(C2),C=C2);(attvar(C2),C=C2);(var(C2),C=C2)).

color_spec_or_fail_include_bg_more(Grid,C,Hi,Vi):- 
  get_bgc(BGC),
  hv_c_value_or(Grid,C2,Hi,Vi,BGC),
  (is_spec_color(C2,C);(atomic(C2),C=C2);(attvar(C2),C=C2);(var(C2),C=BGC)).
  
grid_cpoint(Grid,C-Point,Hi,Vi):- hv_c_value(Grid,C2,Hi,Vi),
 (is_spec_color(C2,C);(atomic(C2),C=C2);(attvar(C2),C=C2);(var(C2),C=C2)),
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
          %pt(hv_cg_value(C2,H,V)),
          is_spec_fg_color(C2,C),
          hv_point(H,V,Point)))),Points),!.
*/
point_corners(Obj,Dir,CPoint):- enum_object(Obj),  globalpoints(Obj,Points), gp_point_corners(Obj,Points,Dir,CPoint).


gp_point_corners(Obj,_Points0,Dir,CPoint):-  %sort_points(Points,SPoints), 
   isz(Obj,Shape),SPoints=Points,
   shape(Obj,Points),member('+'-P1,Points),localpoints(Obj,CPoints),member(C-P1,CPoints),
   C-P1 = CPoint,

  (points_corner_dir(Shape,Dir)*->(SPoints=[CPoint|_];last(SPoints,CPoint));fail).
   

globalpoints(Grid,Points):- is_grid(Grid),!, grid_to_points(Grid,Points).
globalpoints(Grid,Points):- is_group(Grid),!,mapgroup(globalpoints,Grid,MPoints),append_sets(MPoints,Points).
globalpoints(I,X):-  var(I),!, (var_check(I,globalpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
globalpoints([],[]):-!.
globalpoints(G,Ps):- is_map(G),vm_to_printable(G,R),!,globalpoints(R,Ps).
globalpoints(G,[G]):- is_point(G),!.
globalpoints(C-P,[C-P]):-!.
globalpoints(G,G):- maplist(is_point,G),!.
globalpoints(Atom,_):- \+ compound(Atom),!,my_assertion(gp=Atom),trace_or_throw(globalpoints(Atom)).
globalpoints(options(X),_Points):- trace_or_throw(globalpoints(options(X))).
globalpoints(I,X):- indv_props(I,L),member(globalpoints(X),L), nop(my_assertion(maplist(is_cpoint,X))),!.
globalpoints(I,G):- is_object(I),object_localpoints(I,L),is_points_list(L),loc(I,X,Y),offset_points(X,Y,L,G),!.
%globalpoints(I,X):- localpoints(I,X),!.
globalpoints(_,[]):-!.
globalpoints(I,X):- my_assertion(gp=I), trace_or_throw(unknown(globalpoints(I,X))).


localpoints(I,X):- (var_check(I,localpoints(I,X)), deterministic(TF), true), (TF==true-> ! ; true).
localpoints(I,X):- is_object(I),!,object_localpoints(I,X).
localpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), grid_to_points(Grid,HH,VV,Points).
localpoints(G,[G]):- is_point(G),!.
localpoints(G,Ps):- is_map(G),vm_to_printable(G,R),!,localpoints(R,Ps).
localpoints(Grid,Points):- is_group(Grid),!,mapgroup(localpoints,Grid,MPoints),append_sets(MPoints,Points).
localpoints(G,G):- maplist(is_point,G),!.
localpoints(Grid,Points):- is_list(Grid),!,maplist(localpoints,Grid,MPoints),append_sets(MPoints,Points).
%localpoints(Atom,_):- \+ compound(Atom),!,trace_or_throw(localpoints(Atom)).
localpoints(I,X):- trace_or_throw(unknown(localpoints(I,X))).

  %localpoints(I,X):- into_grid(I,G),!,grid_size(G,H,V),grid_to_points(G,H,V,X).

localpoints_include_bg(Grid,Points):- is_grid(Grid),!, grid_to_points_include_bg(Grid,Points),!.
localpoints_include_bg(I,X):- \+ is_grid(I), localpoints(I,X),!.

object_localpoints(I,XX):- object_localpoints0(I,XX),!.
object_localpoints(I,XX):- arcST,trace,object_localpoints0(I,XX).
object_localpoints0(I,XX):- indv_props(I,L),member(localpoints(X),L),!,
     must_det_ll((rotation(I,Rot), v_hv(I,H,V), maybe_rotate_points(H,V,X,Rot,XX))),
     my_assertion(maplist(is_cpoint,XX)),!.
object_localpoints0(I,XX):- indv_props(I,L),member(shape(X),L),!,
     must_det_ll((rotation(I,Rot), v_hv(I,H,V), maybe_rotate_points(H,V,X,Rot,XXX))),
     pen(I,Colors),combine_pen(XXX,Colors,Colors,XX),
     my_assertion(maplist(is_cpoint,XX)),!.


combine_pen([],_,_,[]):-!.
combine_pen([P1|L],Color,Reset,[Color-P1|XX]):- is_color(Color),!,
  combine_pen(L,Color,Reset,XX).
combine_pen(X,[],Reset,XX):-!,combine_pen(X,Reset,Reset,XX).
combine_pen(L,[N-Color|Colors],Reset,XX):- number(N), make_list(Color,N,List),append(List,Colors,ListColors),combine_pen(L,ListColors,Reset,XX).
combine_pen([P1|L],[Color|Colors],Reset,[Color-P1|XX]):- is_color(Color),!,
  combine_pen(L,Colors,Reset,XX).
  

maybe_rotate_points(_,_,X,same,XX):- !,X=XX.
maybe_rotate_points(H,V,X,Rot,XX):- points_to_grid(H,V,X,Grid),call_rot(Rot,Grid,Grid90),localpoints(Grid90,XX).

shape(G,X):- is_group(G),!,mapgroup(shape,G,Points),append_sets(Points,X).
% returns the objects decolorize localpoints
shape(I,X):- is_object(I), indv_props(I,L),member(shape(X),L).
shape(I,X):- localpoints(I,Points),mapgroup(arg(2),Points,X).

%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
*/

%colors(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.
colors(G,[cc(black,0)]):- G==[],!.

colors(I,X):- is_object(I),indv_props(I,L),member(colors(X),L),!.
colors(I,X):- is_map(I),into_grid(I,G),!,colors(G,X).
colors(I,X):- is_object(I),indv_u_props(I,L),member(localpoints(LP),L),!,colors_via_pixels(LP,X).
colors(G,BFO):- colors_via_pixels(G,BFO),!.
colors_via_pixels(G,BFO):- quietly((pixel_colors(G,GF),list_to_set(GF,GS),
  count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!,
  into_cc(SK,BFO))).
%colors(G,X):- is_group(G),!,mapgroup(colors,G,Points),append_sets(Points,X).


get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).

pen(I,C):- indv_props(I,L),member(pen(C),L),!.


object_grid(I,G):- is_grid(I),!,G=I.
object_grid(Group,List):- is_group(Group),!,override_group(object_grid(Group,List)),!.
object_grid(I,G):- indv_props(I,L),member(grid(G),L),!.
object_grid(I,G):- localpoints(I,LP),v_hv(I,H,V),points_to_grid(H,V,LP,G),!.
%object_grid(I,G):- globalpoints(I,GP),into_grid(GP,G),!.

loc_term(I,loc(X,Y)):- loc(I,X,Y),!.

loc(Grid,H,V):- is_grid(Grid),!,H=1,V=1.
loc(G,X,Y):- is_group(G),!,mapgroup(loc_term,G,Offsets),sort(Offsets,[loc(X,Y)|_]). % lowest loc
%loc(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc(I,X,Y):- is_object(I), indv_props(I,L),member(loc(X,Y),L),!.
loc(I,X,Y):- trace,into_obj(I,O), indv_props(O,L),member(loc(X,Y),L),!.
%loc(NT,H,V):- trace, known_gridoid(NT,G),loc(G,H,V).


:- decl_pt(prop_h,vis_hv_term(is_object_or_grid,size)).

vis_hv_term(I,size(X,Y)):- v_hv(I,X,Y),!.

v_hv(Grid,H,V):- is_grid(Grid),!,grid_size(Grid,H,V).
v_hv(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
v_hv(I,X,Y):- indv_props(I,L),member(v_hv(X,Y),L),!.
v_hv(G,X,Y):- is_group(G),!,mapgroup(vis_hv_term,G,Offsets),sort(Offsets,HighToLow),last(HighToLow,size(X,Y)).
v_hv(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
v_hv(NT,H,V):-  known_gridoid(NT,G),G\==NT, v_hv(G,H,V).


%v_hv(Obj,size(H,V)):- v_hv(Obj,H,V).
%loc(Obj,loc(H,V)):- loc(Obj,H,V).

center_term(Obj,loc(1,1)):- is_grid(Obj),!.
center_term(Obj,loc(H,V)):- center(Obj,H,V).

:- decl_pt(prop_h,hw_rat(is_object_or_grid,size)).
hw_rat(Obj,HV):- v_hv(Obj,OH,OV), HV is rationalize(OH/OV).

center(I,X,Y):- indv_props(I,L),member(center(X,Y),L),!.
%center(Obj,CX,CY):- v_hv(Obj,H,V), loc(Obj,X,Y),CX is X + floor(H/2),CY is Y + floor(V/2).



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
 get_vm(VM),
 must_det_ll((
  localpoints(WithPoints,Points),

  localpoints(Obj,PrevPoints),

  (Points=@=PrevPoints -> (NewObj=Obj) ;

 (rotation(Obj,Rot),unrotate(Rot,UnRot),
  loc(Obj,X,Y),%v_hv(Obj,H,V),  
  %o_i_d(Obj,ID,_Iv),
  %uncast_grid_to_object(Orig,Grid,NewObj),
  points_to_grid(Points,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints(UnRotGrid,LPoints),
  offset_points(X,Y,LPoints,GPoints),
  indv_props(Obj,Props),
  my_partition(lambda_rev(member([colors(_),mass(_),shape(_),
            iz(multicolored(_)),globalpoints(_),localpoints(_)])),Props,_,PropsRetained),
  remObjects(VM,Obj),
  make_indiv_object(VM,[loc(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],GPoints,NewObj))))),
   verify_object(NewObj),
  !.

:- style_check(-singleton).
blur_p2(P2,Obj,NewObj):-  
  into_obj(Obj,X),
  center(X,XCH,XCV),
  
  call(P2,X,Y),!,
  globalpoints(X,XGP),
  globalpoints(Y,YGP),
  pct((
  v_hv(X,XH,XV),
  v_hv(Y,YH,YV),
  grid_size(X,XGH,XGV),
  grid_size(Y,YGH,YGV),
  center(Y,YCH,YCV),
  loc(X,XX,XY),
  loc(Y,YX,YY))),
  print_grid(XGH,XGV,XGP),
  print_grid(YGH,YGV,YGP),
  append(XGP,YGP,XYGP),
  rebuild_from_globalpoints(Obj,XYGP,NewObj).
:- style_check(+singleton).

pct(G):- call(G), ptt(G).

rebuild_from_globalpoints(Obj,NewObj):-
  globalpoints(Obj,GPoints),
  rebuild_from_localpoints(Obj,GPoints,NewObj).

rebuild_from_globalpoints(Obj,GPoints,NewObj):-
  get_vm(VM),
  rotation(Obj,Rot),unrotate(Rot,UnRot),
  loc(Obj,X,Y),v_hv(Obj,H,V),
  deoffset_points(X,Y,GPoints,LPoints),
  %o_i_d(Obj,ID,_Iv),
  %grid_size(Obj,GH,GV),
  points_to_grid(H,V,LPoints,Grid),
  call(UnRot,Grid,UnRotGrid),
  localpoints(UnRotGrid,Points),
  indv_props(Obj,Props),
  my_partition(lambda_rev(member([colors(_),mass(_),shape(_),
            iz(multicolored(_)),globalpoints(_),localpoints(_)])),Props,_,PropsRetained),
  remObjects(VM,Obj),
    make_indiv_object(VM,[v_hv(H,V),loc(X,Y),globalpoints(GPoints),localpoints(Points)|PropsRetained],Points,NewObj),
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

:- add_history(test_find_outline).
test_find_outline:- clsmake, forall(find_outline1,true).
find_outline1:- arc_grid(Grid), dash_chars, once(find_outline1(Grid)).
find_outline1(Grid):- find_outline_pred(find_outlines_fast(_),Grid).



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
   set_current_test(ID),
   fail_over_time(6,call(Find_OUTLINE,Grid,SOLS,LeftOver),
     (writeln("TOO SLOWWWW"=Find_OUTLINE),
      print_grid(H,V,Grid),
      writeln("TOO SLOWWWW"=Find_OUTLINE))),
   my_append(SOLS,[leftover(LeftOver)],SOLSL),
   dash_chars,
   (SOLS==[]-> ((writeln("NOTHING FROM"=Find_OUTLINE),ignore((fail,catch((print_grid(H,V,ID,Grid),writeln("NOTTAAA"=Find_OUTLINE),!,fail),_,fail)))));
   member(OL,SOLSL),
   (OL=leftover(LeftOver)
    -> (nl,
      ignore((nonvar(LeftOver),nl,write(leftover=Find_OUTLINE),print_grid(H,V,LeftOver))),
      write('from '),write(Find_OUTLINE),print_grid(H,V,Grid))
    ;((grid_of(OL,O,Hits),once((mass(O,M))),
     nl,write(solution(Hits,M)), print_grid(H,V,O))))).
find_outline_pred(ID):- into_grid(ID,Grid),find_outlines(Grid).

%arc_grid(Nth,X):- (var(Nth)->Nth=1;true), offset(Nth,arc_grid(X)).
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
   once((I.loc=loc(LoH,LoV),
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
  v_hv(Obj,H,V),
  localpoints(Obj,LPoints),
  points_to_grid(H,V,LPoints,GridIn),
  grid_to_gridmap(GridIn,SSP),
  print_grid(SSP).

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

guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,1,GV,Colors,Points,column).
guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,GH,1,Colors,Points,row).

guess_shape(GH,GV,GridIn,LocalGrid,I,Empty,N,H,V,[cc(Black,_)],Points,bgc):- is_bg_color(Black).
%guess_shape(GH,GV,GridIn,LocalGrid,I,0,N,H,V,Colors,Points,view_sq):- H == V.%guess_shape(GH,GV,GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangle):- H>1, V>1.
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,chromatic(Len)):- maplist(arg(1),Colors,Colorz),include(is_fg_color,Colorz,FGC),length(FGC,Len).
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,monochrome):- Colors=[_],length(Colors,Len).
guess_shape(GH,GV,GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad).

guess_shape(GH,GV,I,0,N,N,1,Colors,Points,hv_line(h)):- N > 1.
guess_shape(GH,GV,I,0,N,1,N,Colors,Points,hv_line(v)):- N > 1.


guess_shape(GH,GV,GridIn,LocalGrid,I,E,N,H,V,Colors,Points,poly(Keypad)):- 
  once(guess_shape_poly(I,E,N,H,V,Colors,Points,Keypad)).

guess_shape_poly(I,0,1,1,1,Colors,Points,dot):-!.
guess_shape_poly(I,_,_,_,_,Colors,[Point],dot):-!.
guess_shape_poly(I,0,N,H,V,Colors,Points,rectangulator):- N>1, H\==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,square):- N>1,H==V,!.
guess_shape_poly(I,0,N,H,V,Colors,Points,solid):- N > 1.
%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0,once(H>1;V>1).

guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,R):- N>=2,  
  (flipSym_checks(SN,GridIn)*->R=symmetry(SN);R=symmetry(none)).

%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,solidity(A)):- solidity(Points,A).
%guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,Solid):- (is_jagged(Points)->Solid=jagged(true);Solid=jagged(false)).
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outl2):- H>2,V>2,N>4,
  once((grid_to_gridmap(GridIn,GridON), \+ member('*',GridON), member('.',GridON))).
  
guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outline(SN)):- H>2,V>2,N>4,
  (find_outlines(Points,Sol,Rest)->(length(Sol,SN),SN>0,length(Rest,RN))),!.

guess_shape(GH,GV,GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outl):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).

guess_shape(GH,GV,GridIn,LocalGrid,I,O,N,H,V,Colors,Points,fp(NPoints)):- fail,
  grid_to_gridmap(GridIn,GridON),
  subst_1L(['~'-'red','+'-'blue','.'-'yellow'],GridON,SSP),
  localpoints(SSP,NPoints),!.
  %clumped(ON,COO),!,maplist(arg(1),COO,PAT).
  %points_to_grid(H,V,FGridO,RGridO).

flipSym_checks(Rot90,GridIn):-
  copy_term(GridIn,G,_),!,
  flipSym(Rot90,G).


:- meta_predicate(flipSym(-,+)).
flipSym( full,GridIn):- flipSym(rot90,GridIn), flipSym(sym_hv,GridIn),!.
flipSym(sym_hv,GridIn):- flipSym(flipH,GridIn),flipSym(flipV,GridIn),!.
flipSym(Rot90,GridIn):- r_p(Rot90),call(Rot90,GridIn,LocalGridM),!,GridIn=@=LocalGridM.
r_p(rot90). r_p(flipH). r_p(flipV). r_p(rot180). r_p(rot270).

:- dynamic(individuated_cache/3).
:- retractall(individuated_cache(_,_,_)).


:- fixup_exports.


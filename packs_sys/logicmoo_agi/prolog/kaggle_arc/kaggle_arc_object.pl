/*
  this is part of (H)MUARC

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

grid_to_individual(Grid,OUT):-
  get_gridname(Grid,ID),
  grid_size(Grid,H,V),
  globalpoints(Grid,Points),
  make_indiv_object(ID,H,V,Points,OUT).

%embue_points(ID,_,_,I,I):-!.
%embue_obj_points(ID,H,V,Points,OUT):- make_indiv_object(ID,H,V,Points,OUT).


close_enough_grid(GridIn,GridInCopy,LocalGrid):- 
  \+ \+ (LocalGrid=GridIn, GridIn=@=GridInCopy).

%embue_obj_points(ID,H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_obj_points(ID,H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).



  %pt(Image-->Image9x9),
  %into_gridname(Grid,Gridname),
  %quaderants_and_center_rays(Image9x9,Quads,Centers,Rays),
  %append([Quads,Centers,Rays],FourWay1s00),
  %trace,
  %correctify_objs(Gridname,FourWay1s00,FourWay1s),!.


/*correctify_objs(Gridname,FourWay1s00,FourWay1s):- is_list(FourWay1s00),maplist(correctify_objs(Gridname),FourWay1s00,FourWay1s).
correctify_objs(Gridname,obj(List),obj(NOBJ)):- is_list(List), 
   member(grid(Grid),List),
   \+ member(globalpoints(_),List),
   grid_size(Grid,H,V),
   %trace,
   pt(dleaing_with=obj(List)),
   globalpoints(Grid,Points),
   assertion(Points\=[]),
   points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
   %nb_current(test_pairname,ID),
   embue_points1(Gridname,H,V,LoH,LoV,HiH,HiV,Points,OBJ),
   append(List,OBJ,NOBJ),!.
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
  maplist(make_indiv_object_list(ID,H,V),Points,OUT).
*/

make_point_object(ID,H,V,Point,OUT):-
   assertion(is_cpoint(Point)),
   make_indiv_object(ID,H,V,[Point],OUT).

make_indiv_object(_,_,_,obj(Ps),obj(Ps)):-!.

make_indiv_object(ID,H,V,IPoints,obj(OUT)):-
  assertion(is_list(IPoints)),
  my_partition(is_cpoint,IPoints,Points,Overrides),
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT).

make_indiv_object(ID,H,V,Points,Overrides,obj(OUT)):-
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT),!.


make_indiv_object(Points,Overrides,OUT):- 
  globalpoints(Points,RPoints),
  points_range(RPoints,LoH,LoV,HiH,HiV,_HO,_VO),
  gensym('indiv_object_',ID),
  make_indiv_object(ID,HiH,HiV,LoH,LoV,HiH,HiV,RPoints,[Points,Overrides],OUT).



make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT):- 
 (Points==[]-> trace ; true),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_current(test_pairname,ID),
  Area is Width * Height,
  
  assertion((Points\==[],
     maplist(between(1,30),[H,V,LoH,LoV,HiH,HiV,Width,Height]))),
 must_det_l((
  assertion(is_list([overrides|Overrides])),
  assertion(maplist(is_cpoint,Points)),
  %assertion(ground(Points)),
  flag(indiv,Iv,Iv+1),
  once(colors(Points,CC)),
  length(Points,Len),
  Empty is Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  remove_color(LPoints,UColorlessPoints),
  sort(UColorlessPoints,ColorlessPoints),
  make_grid(Width,Height,Grid),
  add_global_points(LPoints,Grid,Grid),
  make_grid(Width,Height,LocalGrid),
  copy_term(Grid,GridInCopy),
  findall(object_shape(Shape),
   (guess_shape(Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,LPoints,Shape),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),Shapes),
  append(
  [ [mass(Len), shape(ColorlessPoints), colors(CC), localpoints(LPoints),
     vis_hv(Width,Height),  rotation(same), loc_xy(LoH,LoV)],     
    %width(Width), height(Height), area(Area), %missing(Empty),
    [changes([])|Shapes], % [grid(LocalGrid)],    
    [object_indv_id(ID,Iv),globalpoints(Points),grid_size(H,V)]],Ps))),  
  override_object(Ps,Overrides,OUT1),
  sort_obj_props(OUT1,OUT),!.

top(7).


record_xform(Rot90,Obj,XObj):- is_object(Obj), object_changes(Obj,Was),
  override_object(Obj,changes([Rot90|Was]),XObj),!.
record_xform(_Missied,XObj,XObj).


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
obk_key(object_shape(_),50):-!.
obk_key(A,99):- arg(1,A,L), is_grid(L).
obk_key(A,90):- arg(1,A,L), is_list(L).
obk_key(_,80). 

override_object(obj(List),E,obj(MidList)):- !, override_object(List,E,MidList).
override_object(List,[],List):-!.
override_object(List,[E|Props],NewList):-!,
  override_object(List,E,MidList),
  override_object(MidList,Props,NewList).

override_object(List,E,NewList):- E \= object_shape(_), functor(E,F,A),functor(R,F,A),
    append(Left,[R|Right],List), % E \=@= R,
    append(Left,[E|Right],NewList),!.

override_object(List,E,NewList):- 
    append(Left,[changes(G)|Right],List), 
    (( \+ \+ (member(R,Right), R =@= E )) -> NewList = List ; append(Left,[changes(G),E|Right],NewList)),!.

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

object_indv_id(I,ID,Iv):- indv_props(I,L),member(object_indv_id(ID,Iv),L),!.
object_indv_id(I,ID,Iv):- throw(missing(object_indv_id(I,ID,Iv))).
%object_indv_id(_,ID,_Iv):- nb_current(test_pairname,ID).

mass(I,X):- var_check(I,mass(I,X)).
mass(I,X):- indv_props(I,L),member(mass(X),L),!.
mass(Points,Count):- is_list(Points),length(Points,Count),!.
mass(C-_,1):- nonvar_or_ci(C),!.
mass(I,Count):- globalpoints(I,Points), length(Points,Count),!.

remove_color(_-P,P).
remove_color(LPoints,ColorlessPoints):- maplist(remove_color,LPoints,ColorlessPoints).

decl_pt(setq(object,any,object)).

setq(Orig,Todo,Result):- metaq(setq_1,Orig,Todo,Result).
setq_1(_Old,New,Saved):- Saved=New.
delq(Orig,Todo,Result):- metaq(delq_1,Orig,Todo,Result).
delq_1(_Old,_New,Saved):- Saved=delq.


metaq(_,Orig,[],Orig):-!.
metaq(P3,Orig,[New|Todo],Result):- !, metaq(P3,Orig,New,Midway),metaq(P3,Midway,Todo,Result).
metaq(P3,Orig,New,Saved):- functor(New,F,A),functor(Old,F,A),Did=done(nil),map_pred(metaq_1(P3,Did,Old,New),Orig,Saved).
metaq_1(_,done(t),_,_,Orig,Orig):-!.
metaq_1(P3,Did,Old,New,Orig,Saved):- compound(Orig),Orig=Old, call(P3,Old,New,Saved),nb_setarg(1,Did,t).

enum_group(S):- is_unshared_saved(_,S).

enum_object(S):- g2o(_,S).
enum_object(S):- is_unshared_saved(_,IndvS),member(S,IndvS).
enum_object(S):- is_gridname(S,_).

%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
indv_props(obj(L),L):- is_list(L),!.
indv_props(G,L):- nonvar(G), g2o(G,O), nonvar(O),!,indv_props(O,L).
indv_props(obj(L),L):- enum_object(obj(L)).

pmember(E,X):- X=obj(L),!,indv_props(X,L),member(E,L).
pmember(E,X):- sub_term(EE,X),nonvar_or_ci(EE),EE=E,ground(E).
/*pmember(E,L):- is_dict(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
pmember(E,L):- member(EE,L),(EE=E;(is_list(EE),pmember(E,EE))).
pmember(E,L):- member(obj(EE),L),pmember(E,EE).
*/

walls_thick1(G):- localpoints(G,Points),counted_neighbours(Points,ListOfSizes),walls_thick1_sizes(ListOfSizes).
walls_thick1_sizes(List):- maplist(size(2),List).

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

var_check(I,G):- var(I),wdmsg(error(var(G))),!,throw(maybe_enum_i(I)),call(G).

object_shape(I,X):- var_check(I,object_shape(I,X)).
object_shape(I,X):- indv_props(I,L),member(object_shape(X),L).

rotation(I,X):- var_check(I,rotation(I,X)).
rotation(G,X):- is_group(G),!,maplist(rotation,G,Points),append_sets(Points,X).
rotation(I,X):- indv_props(I,L),member(rotation(X),L).
rotation(_,same).

%hv_cvalue(Grid,Color,H,V):- hv_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- plain_var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar_or_ci(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).

object_changes(G,X):- is_group(G),!,maplist(object_changes,G,Points),append_sets(Points,X).
object_changes(I,X):- indv_props(I,L),member(changes(X),L).

all_points_between(_Grid,_Hi,0,_GH,_GV,Points,Points):-!.
all_points_between(Grid,Hi,Vi,GH,GV,Points,PointsO):-
  ((hv_value(Grid,C2,Hi,Vi),(is_spec_color(C2,C);(attvar(C2),C=C2)),hv_point(Hi,Vi,Point)) -> PointsT = [C-Point|Points] ; PointsT = Points),
  (Hi==1 -> (H = GH, V is Vi-1) ; (H is Hi -1, V=Vi)),!,
  all_points_between(Grid,H,V,GH,GV,PointsT,PointsO).

grid_to_points(Grid,HH,HV,Points):- all_points_between(Grid,HH,HV,HH,HV,[],Points),!. 

grid_to_points(Grid,HH,HV,Points):-  throw(all_points_between),
  findall(C-Point,(between(1,HV,V),between(1,HH,H),
    once((hv_value(Grid,C2,H,V),
          %pt(hv_value(C2,H,V)),
          is_spec_color(C2,C),
          hv_point(H,V,Point)))),Points),!.



globalpoints(I,X):- var_check(I,globalpoints(I,X)).
globalpoints(G,[G]):- is_point(G),!.
globalpoints(options(X),_Points):- trace_or_throw(globalpoints(options(X))).
globalpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), grid_to_points(Grid,HH,VV,Points).
globalpoints(Grid,Points):- is_list(Grid),!,maplist(call(globalpoints),Grid,MPoints),append_sets(MPoints,Points).
globalpoints(I,X):- globalpoints0(I,X),!.
globalpoints(I,X):- localpoints0(I,X),!.

  globalpoints0(I,X):- indv_props(I,L),member(globalpoints(X),L),!,is_points_list(X),!.
  globalpoints0(I,G):- localpoints(I,L),is_points_list(L),loc_xy(I,X,Y),offset_points(X,Y,L,G),!.

localpoints(I,X):- var_check(I,localpoints(I,X)).
localpoints(G,[G]):- is_point(G),!.
localpoints(options(X),_Points):- trace_or_throw(localpoints(options(X))).
localpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), grid_to_points(Grid,HH,VV,Points).
localpoints(Grid,Points):- is_list(Grid),!,maplist(localpoints,Grid,MPoints),append_sets(MPoints,Points).
localpoints(I,X):- localpoints0(I,X),!.
localpoints(I,X):- globalpoints0(I,X),!.

  localpoints0(I,X):- indv_props(I,L),member(localpoints(X),L), assertion(maplist(is_cpoint,X)),!.
  %localpoints(I,X):- into_grid(I,G),!,grid_size(G,H,V),grid_to_points(G,H,V,X).


%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors(G,X):- is_group(G),!,maplist(colors,G,Points),append_sets(Points,X).
colors(I,X):- indv_props(I,L),!,member(colors(X),L).
%colors(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.
colors(G,BFO):- quietly((pixel_colors(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!,into_cc(SK,BFO))).

shape(G,X):- is_group(G),!,maplist(shape,G,Points),append_sets(Points,X).
% returns the objects colorless localpoints
shape(I,X):- indv_props(I,L),member(shape(X),L).

get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).


object_grid(I,G):- is_grid(I),!,G=I.
%object_grid(I,G):- indv_props(I,L),member(global_points(X),L),member(vis_hv(H,V),L),!,points_to_grid(H,V,X,G),!.
%object_grid(I,G):- indv_props(I,L),member(localpoints(X),L),member(vis_hv(H,V),L),!,points_to_grid(H,V,X,G),!.
%%object_grid(I,G):- vis_hv(I,H,V),localpoints(I,LP),points_to_grid(H,V,LP,G),!.
object_grid(I,G):- localpoints(I,LP),points_to_grid(LP,G),!.
%object_grid(Group,List):- override_group(object_grid(Group,List)),!.
%object_grid(I,G):- globalpoints(I,GP),into_grid(GP,G),!.

loc_xy_term(I,offset(X,Y)):- loc_xy(I,X,Y).

loc_xy(G,X,Y):- is_group(G),!,maplist(loc_xy_term,G,Offsets),sort(Offsets,[offset(X,Y)|_]). % lowest offset
loc_xy(I,X,Y):- indv_props(I,L),member(loc_xy(X,Y),L).
loc_xy(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc_xy(NT,H,V):- named_gridoid(NT,G),loc_xy(G,H,V).

vis_hv_term(I,size(X,Y)):- vis_hv(I,X,Y).

vis_hv(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
vis_hv(G,X,Y):- is_group(G),!,maplist(vis_hv_term,G,Offsets),sort(Offsets,HighToLow),last(HighToLow,size(X,Y)).
vis_hv(I,X,Y):- indv_props(I,L),member(vis_hv(X,Y),L).
vis_hv(NT,H,V):- named_gridoid(NT,G),vis_hv(G,H,V).
vis_hv(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

object_color(HV,C):- color(HV,C).

color(HV,C):- colors(HV,[cc(C,_)]),!.
color(HV,multicolor(Stuff)):- colors(HV,Stuff),!.

main_color(HV,C):- colors(HV,[cc(C,_)|_]).
first_gpoint(HV,P):- globalpoints(HV,[P|_]).
last_gpoint(HV,P):- globalpoints(HV,PS),last(PS,P).


:- style_check(-singleton).
guess_shape(GridIn,LocalGrid,I,Empty,N,H,V,[cc(Zero,_)],Points,background):- is_bg_color(Zero).
guess_shape(GridIn,LocalGrid,I,0,1,1,1,Colors,Points,dot):-!.
guess_shape(GridIn,LocalGrid,I,_,_,_,_,Colors,[Point],dot):-!.
guess_shape(GridIn,LocalGrid,I,0,N,N,1,Colors,Points,hv_line(h)):- N > 1,!.
guess_shape(GridIn,LocalGrid,I,0,N,1,N,Colors,Points,hv_line(v)):- N > 1,!.
guess_shape(GridIn,LocalGrid,I,0,N,HV,HV,Colors,Points,squares):- HV>1.

guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,nonsquare):- H \== V.
guess_shape(GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangluar):- H>1, V>1.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,[_,_|_],Points,multicolored).

guess_shape(GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad):-!.  
guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,filled_squared):-!.
%guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0,once(H>1;V>1).

%guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,solidity(A)):- solidity(Points,A).
guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,Solid):- (is_jagged(Points)->Solid=jagged;Solid=nonjagged).
guess_shape(GridIn,LocalGrid,I,_,N,H,V,Colors,Points,outline(SN)):- H>2,V>2,N>4,
  (find_outlines(Points,Sol,Rest)->(length(Sol,SN),SN>0,length(Rest,RN))),!.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outl):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).

is_jagged(Points):- member(C-HV,Points),
  findall(Dir,(is_adjacent_point(HV,Dir,HV2),Dir\==c,member(C-HV2,Points)),L),L=[_,_],!.

solidity(Points,Res):- 
  solidness(Points,2,inf,HVNs),
  findall(N=C,
    (between(2,8,N),findall(_,member(N-HV,_HVNs),L),length(L,C)),Res).



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

find_outline:- clsmake, % arc_grid(X),!,
   %find_outline(X),
  arc_grid(X),
   nl,write(from),
   print_grid(X),
   find_outline(X,Y,Z),
   once((grid_size(Y,H,V),
   mass(Y,M))),
   nl,write(solution(H*V=M)),
   print_grid(Y),
   nop((nl,write(leftover),
   print_grid(Z))).


%find_outline(Grid,Result,Grid4):- is_grid(Grid),!,grid_to_points(Grid,Points),!,find_outline(Points,Result,Grid4).

find_outline(Grid,Sol,Rest):-
  find_outlines(Grid,Sols,Rest),!,
  member(Sol,Sols).
  
find_outlines(Grid,[Sol|Solutions],Rest):-
  find_outline(_,Grid,Sol,More),!,
  find_outlines(More,Solutions,Rest).
find_outlines(Rest,[],Rest).
  

find_outline(C,Grid,ResultO,LeftOverO):- 
  localpoints(Grid,Points),
  select(C-HV1,Points,Grid2),
  my_partition(=(C-_),Grid2,Colored,Others),
  %print_grid([blue-HV1|Colored])
  three_points(C,Grid2,HV1,HV2,HV3,Grid4),
 \+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)), 
  %print_grid([blue-HV1,yellow-HV2,red-HV3|Grid4]), 
  outline_only2(HV1,C,HV3,[C-HV1|Grid4],Result,LeftOver),
  [C-HV1,C-HV2|Result] = ResultO,
  once(length(Result,M)),M>2,
  append(Others,LeftOver,LeftOverO).

outline_only2(Exit,_,HV2,Grid,[],Grid) :- HV2==Exit,!.
outline_only2(Exit,C,HV2,Grid2,[C-HV2,C-HV3|Found],LeftOver) :-
  three_points(C,Grid2,HV2,HV3,HV4,Grid4),
  \+ (is_adjacent_point(HV2,_,HV), member(C-HV,Grid4)),
  outline_only2(Exit,C,HV4,Grid4,Found,LeftOver).

three_points(C,Grid2,HV1,HV2,HV3,Grid4):-
  is_adjacent_point(HV1,Dir1,HV2), select(C-HV2,Grid2,Grid3),
  is_adjacent_point(HV2,Dir2,HV3), select(C-HV3,Grid3,Grid4).

:- fixup_exports.


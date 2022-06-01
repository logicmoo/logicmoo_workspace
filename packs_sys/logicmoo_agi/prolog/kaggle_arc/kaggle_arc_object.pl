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
  localpoints(Points,RPoints),
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
  set_global_points(LPoints,Grid,Grid),
  make_grid(Width,Height,LocalGrid),
  copy_term(Grid,GridInCopy),
  findall(object_shape(Shape),
   (guess_shape(Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,ColorlessPoints,Shape),
     close_enough_grid(Grid,GridInCopy,LocalGrid)),Shapes),
  append(
  [ [mass(Len), shape(ColorlessPoints), colors(CC), localpoints(LPoints),
     visual_hv(Width,Height),  rotation(same), loc_xy(LoH,LoV)],     
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
prop_of(size,visual_hv(_,_)).
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

mass(_-P,1):- nonvar_or_ci(P),!.
mass(I,X):- indv_props(I,L),member(mass(X),L),!.
mass(I,Count):- localpoints(I,Points), length(Points,Count),!.
mass(Points,Count):- is_list(Points),length(Points,Count),!.

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
indv_props(G,L):- nonvar_or_ci(G), g2o(G,O), nonvar_or_ci(O),!,indv_props(O,L).
indv_props(obj(L),L):- enum_object(obj(L)).

walls_thick1(G):- localpoints(G,Points),counted_neighbours(Points,ListOfSizes),walls_thick1_sizes(ListOfSizes).
walls_thick1_sizes(List):- maplist(size(2),List).

size(A,A):- A==A.

maybe_into_points_list(I,_):- is_points_list(I),!,fail.
maybe_into_points_list(I,X):- localpoints(I,X).

counted_neighbours(G,CountOut):- maybe_into_points_list(G,List),!,counted_neighbours(List,CountOut).
counted_neighbours([],[]):-!.
counted_neighbours([_-C],[0-C]):-!.
counted_neighbours(List,CountOut):- counted_neighbours(List,[],CountOut).
counted_neighbours(List,CountIn,CountsOut):- counted_neighbours(List,List,CountIn,CountsOut).

colors_join(C,CC):- C==CC,!.
colors_join(C,CC):- is_bgc(C),!,is_bgc(CC).
colors_join(CC,C):- is_bgc(C),!,is_bgc(CC).
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

localpoints(I,X):- is_grid(I),globalpoints(I,X),!.
localpoints(I,X):- indv_props(I,L),member(localpoints(X),L), is_points_list(X),!.
localpoints(I,X):- into_grid(I,G),globalpoints(G,X),!.

object_shape(I,X):- indv_props(I,L),member(object_shape(X),L).

rotation(I,X):- indv_props(I,L),member(rotation(X),L).

%hv_cvalue(Grid,Color,H,V):- hv_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- plain_var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar_or_ci(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).

object_changes(I,X):- indv_props(I,L),!,member(changes(X),L).


all_points(Grid,HH,HV,Points):-  
  findall(C-Point,(between(1,HV,V),between(1,HH,H),
    once((hv_value(Grid,C2,H,V),
          %pt(hv_value(C2,H,V)),
          is_spec_color(C2,C),
          hv_point(H,V,Point)))),Points),!.

globalpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,VV), all_points(Grid,HH,VV,Points).
globalpoints(I,X):- indv_props(I,L),member(globalpoints(X),L),is_points_list(X),!.
globalpoints(Grid,[Grid]):- is_point(Grid),!.
globalpoints(Grid,Points):- is_list(Grid),!,maplist(globalpoints,Grid,MPoints),append(MPoints,Points).
globalpoints(I,G):- localpoints(I,L),is_points_list(L),loc_xy(I,X,Y),offset_points(X,Y,L,G),!.

is_bg(C):- bg_sym(BG),C==BG,!.
is_bg(C):- get_bgc(BG),C==BG,!.

%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors(I,X):- indv_props(I,L),!,member(colors(X),L).

%colors(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [cc(C,3)],!.
colors(G,BFO):- pixel_colors(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!,into_cc(SK,BFO).

shape(I,X):- indv_props(I,L),member(shape(X),L).

get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).

object_grid(I,X):- indv_props(I,L),member(grid(X),L),!.
%object_grid(I,G):- globalpoints(I,GP),into_grid(GP,G).
object_grid(I,G):- visual_hv(I,H,V),localpoints(I,GP),points_to_grid(H,V,GP,G),!.


loc_xy(I,X,Y):- indv_props(I,L),member(loc_xy(X,Y),L).
loc_xy(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
loc_xy(NT,H,V):- named_gridoid(NT,G),loc_xy(G,H,V).

visual_hv(I,X,Y):- indv_props(I,L),member(visual_hv(X,Y),L).
visual_hv(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
visual_hv(NT,H,V):- named_gridoid(NT,G),visual_hv(G,H,V).
%visual_hv(Points,H,V):- pmember(visual_hv(H,V),Points),!.
visual_hv(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

object_color(HV,C):- color(HV,C).
color(HV,C):- colors(HV,[cc(C,_)]),!.
color(HV,multicolor(Stuff)):- colors(HV,Stuff),!.
main_color(HV,C):- colors(HV,[cc(C,_)|_]).
first_gpoint(HV,P):- globalpoints(HV,[P|_]).
last_gpoint(HV,P):- globalpoints(HV,PS),last(PS,P).


:- style_check(-singleton).
guess_shape(GridIn,LocalGrid,I,Empty,N,H,V,[cc(Zero,_)],Points,background):- is_bgc(Zero).
guess_shape(GridIn,LocalGrid,I,0,1,1,1,Colors,Points,dot):- !.
guess_shape(GridIn,LocalGrid,I,0,N,N,1,Colors,Points,hv_line(h)).
guess_shape(GridIn,LocalGrid,I,0,N,HV,HV,Colors,Points,squares):- HV>1.
guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,nonsquare):- H \== V.
guess_shape(GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangluar):- H>1, V>1.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,[cc(Color,_)],Points,outline):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).
guess_shape(GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad):-!.  
guess_shape(GridIn,LocalGrid,I,0,N,1,N,Colors,Points,hv_line(v)).
guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,filled_squared).
guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,polygon):- O\==0.
%guess_shape(G,LocalGrid,I,O,N,H,V,Colors,Points,walls_thick(1)):- walls_thick1(G).
/*
guess_shape(GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.loc_xy=loc_xy(LoH,LoV),
   make_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   compute_shared_indivs(Grid,InvS))),!,
   InvS=[_,_|_].
*/

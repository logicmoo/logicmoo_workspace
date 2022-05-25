


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
   %nb_current(test_name_w_type,ID),
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

make_indiv_object(ID,H,V,LoH,LoV,HiH,HiV,Points,Overrides,OUT):- 
 must_det_l((
  assertion(is_list(Overrides)),
  assertion(maplist(is_cpoint,Points)),
  assertion(ground(Points)),
  flag(indiv,Iv,Iv+1),
  once(colors_count(Points,CC)),
  length(Points,Len),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_current(test_name_w_type,ID),
  Area is Width * Height,
  Empty is Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  remove_color(LPoints,UColorlessPoints),
  sort(UColorlessPoints,ColorlessPoints),
  make_grid(Width,Height,GridM),
  set_local_points(LPoints,GridM,Grid),
  make_grid(Width,Height,LocalGrid),
  copy_term(Grid,GridInCopy),
  findall(object_shape(Shape),
   (guess_shape(Grid,LocalGrid,Ps,Empty,Len,Width,Height,CC,ColorlessPoints,Shape),close_enough_grid(Grid,GridInCopy,LocalGrid)),Shapes),
  append(
  [ [localpoints_nc(ColorlessPoints),
     colors_count(CC),
     object_size(Width,Height),
     point_count(Len)],
     Shapes,
     [object_offset(LoH,LoV)],
    %width(Width), height(Height), area(Area), %missing(Empty),
    [object_indv_id(ID,Iv),localpoints(LPoints),grid(LocalGrid)],     
    [globalpoints(Points),grid_size(H,V)]],Ps),
  override_list(Ps,Overrides,OUT))).


override_list(List,[],List):-!.
override_list(List,[E|Props],NewList):-!,
  override_list(List,E,MidList),
  override_list(MidList,Props,NewList).
override_list(List,E,MidList):- E\=object_shape(_),
    functor(E,F,A),functor(R,F,A),
    append(Left,[R|Right],List),
    E\==R,
    append(Left,[E|Right],Rest),!,
    override_list(Rest,E,MidList).
override_list(List,E,MidList):- append([E],List,MidList),!.


object_indv_id(I,ID,Iv):- indv_props(I,L),member(object_indv_id(ID,Iv),L),!.
object_indv_id(_,ID,_Iv):- nb_current(test_name_w_type,ID).

point_count(_-P,1):- nonvar(P),!.
point_count(I,X):- indv_props(I,L),member(point_count(X),L),!.
point_count(obj(I),Count):- localpoints(I,Points), length(Points,Count),!.
point_count(Points,Count):- is_list(Points),length(Points,Count),!.
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

enum_object(S):- is_unshared_saved(_,IndvS),member(S,IndvS).
enum_object(S):- is_gridname(S,_).

%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
indv_props(obj(L),L):- is_list(L),!.
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
colors_join(C,CC):- (var(C);var(CC)),!,fail.
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

localpoints(I,X):- indv_props(I,L),member(localpoints(X),L).
localpoints(I,X):- into_grid0(I,G),globalpoints(G,X).

object_shape(I,X):- indv_props(I,L),member(object_shape(X),L).

%hv_cvalue(Grid,Color,H,V):- hv_value(Grid,C,H,V),!,as_cv(C,Color),!.
%as_cv(C,Color):- var(C),!,=(C,Color).
%as_cv(C,Color):- sub_term(Color,C),nonvar(Color),is_color(Color).
%as_cv(C-_,Color):- as_cv(C,Color).
%as_cv(C,Color):- integer(C),!,color_code(C,Color).

globalpoints(I,X):- indv_props(I,L),!,member(globalpoints(X),L).
globalpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,HV), 
  findall(C-Point,(between(1,HV,V),between(1,HH,H),once((nth1(V,Grid,Row),nth1(H,Row,C),nonvar(C),hv_point(H,V,Point)))),Points),!.
globalpoints(Grid,[Grid]):- is_point(Grid),!.
globalpoints(Grid,Points):- is_list(Grid),!,maplist(globalpoints,Grid,MPoints),append(MPoints,Points).
%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors_count(I,X):- indv_props(I,L),!,member(colors_count(X),L).

%colors_count(Points,CC):- is_list(Points),nth0(_,Points,C-_),is_color(C), CC = [color_count(C,3)],!.
colors_count(G,BFO):- pixel_colors(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),!,into_cc(SK,BFO).

localpoints_nc(I,X):- indv_props(I,L),member(localpoints_nc(X),L).

get_instance_method(Obj,Compound,F):- is_object(Obj), compound(Compound),compound_name_arity(Compound,Name,A),
   A1 is A+1, atom_concat('object_',Name,F),current_predicate(F/A1).

object_grid(I,X):- indv_props(I,L),!,member(grid(X),L).


object_offset(I,X,Y):- indv_props(I,L),member(object_offset(X,Y),L).
object_offset(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,_,_,_,_), H is LoH, V is LoV.
object_offset(NT,H,V):- named_gridoid(NT,G),object_offset(G,H,V).

object_size(Grid,H,V):- is_grid(Grid),!,globalpoints(Grid,Points),!,points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
object_size(NT,H,V):- named_gridoid(NT,G),object_size(G,H,V).
object_size(I,X,Y):- indv_props(I,L),member(object_size(X,Y),L).
%object_size(Points,H,V):- pmember(object_size(H,V),Points),!.
object_size(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

top(8).

:- style_check(-singleton).
guess_shape(GridIn,LocalGrid,I,Empty,N,H,V,[color_count(Zero,_)],Points,background):- is_bgc(Zero).
guess_shape(GridIn,LocalGrid,I,0,1,1,1,Colors,Points,dot):- !.
guess_shape(GridIn,LocalGrid,I,0,N,N,1,Colors,Points,line(h)).
guess_shape(GridIn,LocalGrid,I,0,N,HV,HV,Colors,Points,squares):- HV>1.
guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,nonsquare):- H \== V.
guess_shape(GridIn,LocalGrid,I,I,N,H,V,Colors,Points,rectangluar):- H>1, V>1.
guess_shape(GridIn,LocalGrid,I,_,N,H,V,[color_count(Color,_)],Points,outline):- H>2,V>2, N>7,add_borders(Color,GridIn,LocalGrid).
guess_shape(GridIn,LocalGrid,I,0,9,3,3,Colors,Points,keypad):-!.  
guess_shape(GridIn,LocalGrid,I,0,N,1,N,Colors,Points,line(v)).
guess_shape(GridIn,LocalGrid,I,0,N,H,V,Colors,Points,filled_squared).
guess_shape(GridIn,LocalGrid,I,O,N,H,V,Colors,Points,nonsolid):- O\==0.
%guess_shape(G,LocalGrid,I,O,N,H,V,Colors,Points,walls_thick(1)):- walls_thick1(G).

guess_shape(GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.object_offset=object_offset(LoH,LoV),
   make_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   compute_shared_indivs(Grid,InvS))),!,
   InvS=[_,_|_].


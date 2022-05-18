   
:- dynamic(is_group_saved/2).
:- dynamic(reuse_grid_nums/1).


enum_object(S):- is_gridname(S,_).
enum_object(S):- is_group_saved(_,IndvS),member(S,IndvS).

store_individuals_non_shared(Gridname,Grid):- 
   set_gridname(Grid,Gridname),
   individuals_non_shared(Grid,IndvS),
   set_named_indivs(Gridname,IndvS).

get_named_indivs(Gridname,IndvS):- 
   is_group_saved(Gridname,IndvS).

set_named_indivs(Gridname,IndvS):- 
   retractall(is_group_saved(Gridname,_)),
   asserta(is_group_saved(Gridname,IndvS)),!.

ensure_individuals_non_shared(Gridname):- is_group_saved(Gridname,_),!.
ensure_individuals_non_shared(Gridname):- is_gridname(Grid,Gridname),!,
   store_individuals_non_shared(Gridname,Grid),!.
ensure_individuals_non_shared(Gridname):- throw(cannot_find((Gridname))).

:- style_check(+singleton).

grid_shared_with(Gridname*ExampleNum*in,Gridname*ExampleNum*out):-!.
grid_shared_with(Gridname*ExampleNum*out,Gridname*ExampleNum*in):-!.

use_shared_first(W) :- nb_current(grid_shared,W),W\==[],W\==nil.
use_shared_first:- use_shared_first(_).

get_shared_with(IndvS):- use_shared_first(With),
  ensure_individuals_non_shared(With),
  is_group_saved(With,IndvS),!.
get_shared_with([]).


get_unshared(IndvS):- use_shared_first(With),
  grid_shared_with(With,Gridname),
  ensure_individuals_non_shared(Gridname),
  is_group_saved(Gridname,IndvS),!.
get_unshared([]).


unset_nth(I,O):- delq(I,object_indv_id(_,_),O).
set_nth(I,O):- delq(I,object_indv_id(_,_),O).

get_combined(IndvC):- nb_current(test_name_w_type,NamedExampleNum), is_group_saved(NamedExampleNum,IndvC),!.
get_combined(IndvC):- nb_current(test_name_w_type,NamedExampleNum), 
   is_group_saved(NamedExampleNum*in ,IndvS1),
   is_group_saved(NamedExampleNum*out,IndvS2),
   make_combined(IndvS1,IndvS2,IndvC).
get_combined(IndvC):-get_shared_with(IndvS1),get_unshared(IndvS2),make_combined(IndvS1,IndvS2,IndvC).

with_each_indiv(G,I):- individuals(G,I).

make_combined(IndvS1,IndvS2,BetterC):-
  append(IndvS1,IndvS2,IndvSU),list_to_set(IndvSU,IndvS),
  smallest_first(IndvS,IndvC),
  cleanup(IndvC,BetterC),
  nb_current(test_name_w_type,NamedExampleNum),
  set_named_indivs(NamedExampleNum,BetterC),!.

cleanup(IndvC,BetterC):-
  append(Left,[B|Bigger],IndvC),
  append(LeftB,[A|RBigger],Bigger),
  compute_diff(A,B,same_object(_)),!,
  object_indv_id(B,NamedExampleNum,Iv),
  setq(A,object_indv_id(B,NamedExampleNum,Iv),AA),
  append([Left,LeftB,RBigger,[AA]],MissingAB),
  cleanup(MissingAB,BetterC).
cleanup(A,A).

individuals(Grid,IndvS):- 
   get_gridname(Grid,Gridname),
   grid_shared_with(Gridname,With),
   locally(b_setval(grid_shared,With),
            individuals_common(Grid,IndvS)).

individuals_non_shared(Grid,IndvS):- 
   locally(b_setval(grid_shared,nil),
            individuals_common(Grid,IndvS)).

individuals_common(Grid,IndvS):- is_grid(Grid),!,
 must_det_l((
  notrace((grid_size(Grid,H,V),
  globalpointlist(Grid,Points))),
  locally(t_l:id_cells(Grid,Points),
  ( individuals_raw(Grid,Points,Indv_0),    
    % dmsg(is=Indv_0),    
    unraw_inds(Indv_0,Indv1),
    largest_first(Indv1,Indv2))),
  make_embued_points(Grid,H,V,Indv2,IndvS))),
 nop(maplist(assert_IndvS,IndvS)).

/*
assert_IndvS(obj(L)):-
  object_indv_id(obj(L),NamedExampleNum,Iv),
  maplist(assert_IndvS(NamedExampleNum,Iv),L).


globalpointlist(NamedExampleNum,Iv,ListOIfColoredPoints).
globalpoint(NamedExampleNum,Iv,Color,Point).
localpointlist(NamedExampleNum,Iv,ListOIfColoredPoints).
localpoint(NamedExampleNum,Iv,Color,Point).
localcolorlesspointlist(NamedExampleNum,Iv,Point).
object_size(NamedExampleNum,Iv,H,V).
object_shape(NamedExampleNum,Iv,ShapName).


assert_IndvS(NamedExampleNum,Iv,P):- P=..[F,List],is_list(List),
  maplist(assert_IndvS_L(NamedExampleNum,Iv,F),List).

assert_IndvS_L(NamedExampleNum,Iv,F),List

assert_IndvS(NamedExampleNum,Iv,P):- P=..[F|Args],
  PP=..[F,NamedExampleNum,Iv|Args],
  assert_IndvS_PP(NamedExampleNum,Iv,PP).
*/

individuals_common(Grid,I):- is_objectlist(Grid),!,I=Grid.
individuals_common(obj(Grid),[obj(Grid)]):-!.

individuals_raw(ID,Points,IndvS):- 
  maybe_shared_individuals_list(squares,Points,Indv0S,ELeftOverPoints),
  maybe_join_shared_individuals_list(squares,ELeftOverPoints,Indv0S,Indv1S,LeftOverPoints),
  individuals_list(squares,ID,LeftOverPoints,Indv2S),
  append(Indv1S,Indv2S,Indv12S),list_to_set(Indv12S,IndvS).

%maybe_join_shared_individuals_list(squares,ELeftOverPoints,Indv0S,Indv1S,LeftOverPoints):-
%  select(Indv0,Rest,Indv0S),member(Indv1,Rest),object_shape(Indv0,line(h)), object_shape(Indv1,line(h)).

maybe_join_shared_individuals_list(_Squares,LeftOverPoints,IndvS,IndvS,LeftOverPoints).

maybe_shared_individuals_list(Types,Points,IndvList,Unused):- 
  use_shared_first, mmake,
  get_combined(IndvSC), 
  consume_shared_individual_points(IndvSC,Types,Points,IndvList,Unused).
maybe_shared_individuals_list(_Types,UnusedPoints,[],UnusedPoints):- !.

unraw_inds(IndvS,IndvO):-   
  largest_first(IndvS,Indv1),
  reverse(Indv1,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  SZ=0,
  check_minsize(SZ,IndvR,Indv),
  unraw_inds2(_,Indv,IndvO),!.

%individuals_list_diamonds(_,Indv,Indv1):-
%  individuals_list(diamonds,ID,Indv,Indv1).


consume_shared_individual_points([],_Types,UnusedPoints,[],UnusedPoints):- !.
consume_shared_individual_points([H|T],Types,Points,[H|IndvList],Unused):-  
  globalpointlist(H,GPoints), remove_gpoints(GPoints,Points,LeftOver),
  consume_shared_individual_points(T,Types,LeftOver,IndvList,Unused).
consume_shared_individual_points([_|T],Types,Points,IndvList,Unused):-  
  consume_shared_individual_points(T,Types,Points,IndvList,Unused).


individuals_list(_,_,[],[]):-!.
individuals_list(Types,ID,Points,[Indv|IndvList]):-   
    find_one_individual(Types,Points,Indv,NextScanPoints),!,
    %wqnl(indv(Types)=Indv),    
    individuals_list(Types,ID,NextScanPoints,IndvList).
individuals_list(squares,ID,Points,[Indv|IndvList]):- fail,
   find_one_individual(diamonds,Points,Indv,NextScanPoints),!,
   individuals_list(diamonds,ID,NextScanPoints,IndvList). 
individuals_list(_Types,_,Points,IndvList):- maplist(obj1,Points,IndvList).

   

obj1(X,X).

ok_color_with(C,C2):- /* \+ free_cell(C2), */ C==C2.

sameglobalpoints(Points,IndvC,LeftOver,IndvC):-
  globalpointlist(IndvC,GPoints),
  remove_gpoints(GPoints,Points,LeftOver).

remove_gpoints([],Rest,Rest).
remove_gpoints([GPoint|GPoints],Points,Rest):- select(GPoint,Points,Mid),remove_gpoints(GPoints,Mid,Rest).


find_one_individual_from_combined(Types,Points,[Indv1|IndvList],Unused):-
  get_combined(IndvSC),
  maplist(sameglobalpoints(Points,Indv1,LeftOver),IndvSC),
  find_one_individual_from_combined(Types,LeftOver,IndvList,Unused).
find_one_individual_from_combined(_Types,UnusedPoints,[],UnusedPoints).

find_one_individual(Types,Points,Indv,NextScanPoints):- fail,
  use_shared_first,
  find_one_individual_from_combined(Types,Points,Indv,NextScanPoints).

find_one_individual(Types,Points,Indv,NextScanPoints):-
    select(C-HV,Points,Rest), \+ free_cell(C), 
    allow_dirs(Types,Dir),
    adjacent_point_allowed(C,HV,Dir,HV2),select(C2-HV2,Rest,ScanPoints),ok_color_with(C,C2),
  /*  nop((\+ filtered_point(C,HV2), \+ filtered_point(C,HV),
     (\+ is_diag(Dir)-> true ; (turn_left_45(Dir,TR),turn_right_45(Dir,TL),color_of(HV,TL,TLC),color_of(HV,TR,TRC),
      \+ (colors_block_diag(C,TLC,TRC,C2)))))),*/
    all_individuals_near(Types,C,[C-HV,C2-HV2],ScanPoints,NextScanPoints,Indv),
    meets_indiv_criteria(Indv).

color_of(HV,TL,TLC):- t_l:id_cells(_ID,Points), is_adjacent_point(HV,TL,TLHV),member(TLC-TLHV,Points).

colors_block_diag(C,TLC,TRC,_C2):- TLC==TRC, TRC\==0, C \== TRC, \+ free_cell(TRC).

filtered_point(C,HV):- t_l:id_cells(_ID,Points),% select(_-HV,Points,Rest), 
  findall(Dir-HV2,(adjacent_point_allowed(C,HV,Dir,HV2),member(C-HV2,Points)),Used),
  findall(Dir-HV2,(adjacent_disallowed(C,HV,Dir,HV2),member(C-HV2,Points)),Unused),
  shape_has_filtered_use(C,Used,Unused),
  %wdmsg(shape_has_filtered_use(C,HV,Used,Unused)),
  !.

shape_has_filtered_use(_,[],_Unused).
shape_has_filtered_use(C,[_],_):- shape_filter(C,squares),!.

adjacent_groups(C,Grp1,Dir,Grp2):- member(_-P1,Grp1),member(C-P2,Grp2),is_adjacent_point(P1,Dir,P2).
adjacent_point(C,HV,HV2):- adjacent_point_allowed(C,HV,_Dir,HV2).
adjacent_point_allowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS),member(Dir,DirS).
adjacent_disallowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS), \+ member(Dir,DirS).

allow_dirs(Square,X):- allow_dir(Square,List),member(X,List).
allow_dir(horizs,[e,w]). allow_dir(virtzs,[n,s]). allow_dir(diaguz,[ne,sw]). allow_dir(diagdz,[nw,se]). 
allow_dir(squares,[n,s,e,w]). allow_dir(polygs,[n,s,e,w]).
allow_dir(diamonds,[nw,sw,se,ne]).
%circles, dots, , rays, walls

shape_filter(X,squares):- free_cell(X).
shape_filter(X,polygs):- \+ free_cell(X).


ok_dir(s). ok_dir(e). ok_dir(n). ok_dir(w).
ok_dir(_) :- \+ squares.
squares:-true.


meets_indiv_criteria(_).

all_individuals_near(_Types,_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(Types,C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(Types,C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(Types,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_Types,_C,_From,[],[],[]):-!.
individuals_near(Types,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Types,C,E,From),!,
  individuals_near(Types,C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(Types,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Types,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(Types,C,C2-E,List):- allow_dirs(Types,Dir), adjacent_point_allowed(C,E2,Dir,E), member(C2-E2,List),ok_color_with(C2,C).


check_minsize(Sz,Indv1,Indv):- include(meets_size(Sz),Indv1,Indv).

meets_size(Len,Points):- point_count(Points,L),!,L>=Len.

remove_bgs(IndvS,IndvL,BGIndvS):- partition(is_bg_indiv,IndvS,BGIndvS,IndvL).

is_bg_indiv(O):- colors_count(O,[color_count(C,CC)]),CC>0,is_black(C).

smallest_first(IndvS,IndvO):-
  findall(Size-Indv,(member(Indv,IndvS),point_count(Indv,Size)),All),
  keysort(All,AllK),
  findall(Indv,member(_-Indv,AllK),IndvO).

largest_first(IndvS,IndvR):-  
  smallest_first(IndvS,IndvO),
  reverse(IndvO,IndvR).

largest_first_nonbg(IndvS,IndvOB):-  
  largest_first(IndvS,IndvO),
  remove_bgs(IndvO,IndvL,BGIndvS),
  append(IndvL,BGIndvS,IndvOB).

finish_grp(C,Grp,Point2,Dir,Rest,NewGroup,RRest):- 
   \+ (is_diag(Dir),is_black(C)),
   is_adjacent_point(Point2,Dir,Point3),
   single_point(C-Point3,Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).


reserved_point(C-Point):- fail, 
 \+ (get_shared_with(With), member(Obj,With),
  globalpointlist(Obj,GPoints), member(CC-Point,GPoints), CC==C).

single_point(C-Point,IndvS,Rest1):-
  single_point0(C-Point,IndvS,Rest1),
   \+ reserved_point(C-Point).

single_point0(C-Point,IndvS,Rest1):-
  select([C-Point],IndvS,Rest1),
  nonvar(C).
single_point0(C-Point,IndvS,Rest1):-
  select(C-Point,IndvS,Rest1),
  nonvar(C).
single_point0(C-Point,IndvS,Rest1):- 
  select(obj(I),IndvS,Rest1), fail, % round 2
  globalpointlist(obj(I),[C-Point]),
  nonvar(C).

/*
unraw_inds2(Types,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv1),
   reverse(Indv1,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Types,IndvR,IndvO).
*/

% Diag of 3 or more
unraw_inds2(Types,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar(C), % \+ free_cell(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_dir(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Types,[NewGroup|RRestO],IndvO).


% Diag of 2 or more
unraw_inds2(Types,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), % \+ free_cell(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_dir(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Types,[NewGroup|RRestO],IndvO).



unraw_inds2(Types,IndvS,IndvO):-  fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), \+ free_cell(C),
  single_point(C2-Point2,Rest1,Rest),
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Types,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(Types,IndvS,IndvO):- 
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(Types,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(Types,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Types,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Types,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_,IndvS,IndvS).


:- thread_local(t_l:id_cells/2).

deoffset_points(1,1,Points,Points):-!.
deoffset_points(OH,OV,Point,LPoint):- map_pred(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

offset_points(OH,OV,Point,LPoint):- map_pred(if_point_offset(OH,OV),Point,LPoint).
if_point_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

grid_to_individual(Grid,OUT):-
  get_gridname(Grid,GN),
  grid_size(Grid,H,V),
  globalpointlist(Grid,Points),
  embue_points(GN,H,V,1,1,H,V,Points,OUT).

make_embued_points(Grid,H,V,Indv2,IndvS):- 
  get_gridname(Grid,GN),
  maplist(embue_points(GN,H,V),Indv2,IndvS).

%embue_points(GN,_,_,I,I):-!.
embue_points(_,_,_,obj(Ps),obj(Ps)):-!.
embue_points(GN,H,V,Points,OUT):- 
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  embue_points(GN,H,V,LoH,LoV,HiH,HiV,Points,OUT).

embue_points(GN,H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_points(GN,H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).
embue_points(NamedExampleNum,H,V,LoH,LoV,HiH,HiV,Points,obj(Ps)):-
 assertion(ground(Points)),
 flag(indiv,Iv,Iv+1),
 must_det_l((
  colors_count(Points,CC),
  length(Points,Len),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_current(test_name_w_type,NamedExampleNum),
  
  %gensym(id_,IID),
  Area is Width * Height,
  Empty is  Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  remove_color(LPoints,CLPoints),
  findall(object_shape(Shape),guess_shape(Ps,Empty,Len,Width,Height,CC,CLPoints,Shape),Shapes),
  append([[
    localcolorlesspointlist(CLPoints),
    colors_count(CC),
    object_size(Width,Height),
    point_count(Len)],Shapes,
    %width(Width), height(Height), area(Area),
    %missing(Empty),
    [localpointlist(LPoints)],
    [object_offset(LoH,LoV)],
    [globalpointlist(Points),
    object_indv_id(NamedExampleNum,Iv),
    grid_size(H,V)]],Ps))).


object_indv_id(I,NamedExampleNum,Iv):- indv_props(I,L),member(object_indv_id(NamedExampleNum,Iv),L),!.
object_indv_id(_,NamedExampleNum,_Iv):- nb_current(test_name_w_type,NamedExampleNum).

point_count(_-P,1):- nonvar(P),!.
point_count(I,X):- indv_props(I,L),member(point_count(X),L),!.
point_count(obj(I),Count):- localpointlist(I,Points), length(Points,Count),!.
point_count(Points,Count):- is_list(Points),length(Points,Count),!.
remove_color(_-P,P).
remove_color(LPoints,CLPoints):- maplist(remove_color,LPoints,CLPoints).

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



indv_props(obj(L),L):- is_list(L).
indv_props(obj(L),L):- enum_object(obj(L)).

localpointlist(I,X):- indv_props(I,L),member(localpointlist(X),L).
object_shape(I,X):- indv_props(I,L),member(object_shape(X),L).

hv_cvalue(Grid,Color,H,V):- hv_value(Grid,C,H,V),as_cv(C,Color).
as_cv(C,Color):- var(C),color_code(C,Color).
as_cv(C,Color):- integer(C),!,color_code(C,Color).
as_cv(C,Color):- atom(C),!,color_code(C,Color).
as_cv(C-_,Color):- as_cv(C,Color).

globalpointlist(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,HV), 
  findall(C-Point,(between(1,HV,V),between(1,HH,H),hv_cvalue(Grid,C,H,V),nonvar(C),hv_point(H,V,Point)),Points),
  assertion(ground(Points)).
%globalpointlist(Grid,Points):- is_object(Grid),!,globalpointlist(Grid,Points).
globalpointlist(Grid,Points):- is_objectlist(Grid),!,maplist(globalpointlist,Grid,MPoints),flatten(MPoints,Points).
globalpointlist(I,X):- indv_props(I,L),member(globalpointlist(X),L).
/*
globalpointlist(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpointlist(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors_count(I,X):- indv_props(I,L),!,member(colors_count(X),L).
colors_count(G,BFO):- pixels(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),into_cc(SK,BFO).

localcolorlesspointlist(I,X):- indv_props(I,L),member(localcolorlesspointlist(X),L).

object_offset(I,X,Y):- indv_props(I,L),member(object_offset(X,Y),L).
object_size(I,X,Y):- indv_props(I,L),member(object_size(X,Y),L).
object_size(Points,H,V):- pmember(object_size(H,V),Points),!.
object_size(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

top(8).

:- style_check(-singleton).
guess_shape(I,Empty,N,1,1,Colors,Points,dot):- !.
guess_shape(I,Empty,N,N,1,Colors,Points,line(h)).
guess_shape(I,Empty,N,1,N,Colors,Points,line(v)).
guess_shape(I,Empty,N,H,V,[color_count(Zero,_)],Points,background):- (Zero==0;Zero==black).
guess_shape(I,0,N,HV,HV,Colors,Points,square):- HV>1.
guess_shape(I,0,N,H,V,Colors,Points,solid).
guess_shape(I,O,N,H,V,Colors,Points,nonsolid):- O\==0.

guess_shape(I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.object_offset=object_offset(LoH,LoV),
   make_unassigned_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   individuals(Grid,InvS))),!,
   InvS=[_,_|_].




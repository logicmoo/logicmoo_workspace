


check_minsize(Sz,Indv1,Indv):- include(meets_size(Sz),Indv1,Indv).

meets_size(Len,Points):- point_count(Points,L),!,L>=Len.

remove_bgs(IndvS,IndvL,BGIndvS):- partition(is_bg_indiv,IndvS,BGIndvS,IndvL).

is_bg_indiv(O):- colors_count(O,[color_count(C,CC)]),CC>0,is_black(C).


largest_first(IndvS,IndvOB):-  
  remove_bgs(IndvS,IndvL,BGIndvS),
  findall(Hard-Indv,(member(Indv,IndvL),point_count(Indv,Hard)),All),
  keysort(All,AllK),
  reverse(AllK,AllR),
  findall(Indv,member(_-Indv,AllR),IndvO),append(IndvO,BGIndvS,IndvOB).

finish_grp(C,Grp,Point2,Dir,Rest,NewGroup,RRest):- 
   is_adjacent_point(Point2,Dir,Point3),
   select([C-Point3],Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).

single_point(C-Point,IndvS,Rest1):-
  select([C-Point],IndvS,Rest1),
  nonvar(C).
single_point(C-Point,IndvS,Rest1):-
  select(C-Point,IndvS,Rest1),
  nonvar(C).
single_point(C-Point,IndvS,Rest1):-
  select(obj(I),IndvS,Rest1),
  globalpoints(obj(I),[C-Point]),
  nonvar(C).

unraw_inds2(Types,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv1),
   reverse(Indv1,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Types,IndvR,IndvO).

unraw_inds2(Types,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), \+ free_cell(C),\+ get_bgc(C),
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
deoffset_points(OH,OV,Point,LPoint):- pred_subst(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

offset_points(OH,OV,Point,LPoint):- pred_subst(if_point_offset(OH,OV),Point,LPoint).
if_point_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

grid_to_individual(Grid,OUT):-
  grid_size(Grid,H,V),
  globalpoints(Grid,Points),
  embue_points(H,V,1,1,H,V,Points,OUT).

make_embued_points(_Grid,H,V,Indv2,IndvS):- 
  flag(indiv,_,0),
  maplist(embue_points(H,V),Indv2,IndvS).

%embue_points(_,_,I,I):-!.
embue_points(_,_,obj(Ps),obj(Ps)):-!.
embue_points(H,V,Points,OUT):- 
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  embue_points(H,V,LoH,LoV,HiH,HiV,Points,OUT).

embue_points(H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_points(H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).
embue_points(H,V,LoH,LoV,HiH,HiV,Points,obj(Ps)):-
 assertion(ground(Points)),
 flag(indiv,Iv,Iv+1),
 must_det_l((
  colors_count(Points,CC),
  length(Points,Len),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  nb_current(test_name_w_type,NameTypeNum),
  
  %gensym(id_,IID),
  Area is Width * Height,
  Empty is  Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  findall(shape(Shape),guess_shape(Ps,Empty,Len,Width,Height,CC,Points,Shape),Shapes),
  remove_color(LPoints,CLPoints),
  append([
    points_only(CLPoints),
    colors_count(CC),
    object_size(Width,Height),
    object_offset(LoH,LoV),
    %width(Width), height(Height), area(Area),
    point_count(Len),
    %missing(Empty),
    localpoints(LPoints)|Shapes],[
    globalpoints(Points),
    object_indv_id(NameTypeNum,Iv),
    grid_size(H,V)],Ps))).

object_indv_id(I,NameTypeNum,Iv):- indv_props(I,L),member(object_indv_id(NameTypeNum,Iv),L),!.
object_indv_id(_,NameTypeNum,_Iv):- nb_current(test_name_w_type,NameTypeNum).

point_count(_-P,1):- nonvar(P),!.
point_count(I,X):- indv_props(I,L),member(point_count(X),L),!.
point_count(obj(I),Count):- localpoints(I,Points), length(Points,Count),!.
point_count(Points,Count):- is_list(Points),length(Points,Count),!.
remove_color(_-P,P).
remove_color(LPoints,CLPoints):- maplist(remove_color,LPoints,CLPoints).

setq(I,[],I):-!.
setq(I,[H|T],II):- !, setq(I,H,M),setq(M,T,II).
setq(I,P,II):- functor(P,F,A),functor(Q,F,A),D=done(nil),pred_subst(setq_1(D,Q,P),I,II).
setq_1(done(t),_,_,I,I):-!.
setq_1(D,Q,P,I,II):- compound(I),I=Q,nb_setarg(1,D,t),II=P.
   
indv_props(obj(L),L):- is_list(L).

localpoints(I,X):- indv_props(I,L),member(localpoints(X),L).

globalpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,HV), 
  findall(C-Point,(between(1,HV,V),between(1,HH,H),hv_value(Grid,C,H,V),nonvar(C),hv_point(H,V,Point)),Points),
  assertion(ground(Points)).
%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
globalpoints(Grid,Points):- is_objectlist(Grid),!,maplist(globalpoints,Grid,MPoints),flatten(MPoints,Points).
globalpoints(I,X):- indv_props(I,L),member(globalpoints(X),L).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors_count(I,X):- indv_props(I,L),!,member(colors_count(X),L).
colors_count(G,BFO):- pixels(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),into_cc(SK,BFO).

points_only(I,X):- indv_props(I,L),member(points_only(X),L).

object_offset(I,X,Y):- indv_props(I,L),member(object_offset(X,Y),L).
object_size(I,X,Y):- indv_props(I,L),member(object_size(X,Y),L).
object_size(Points,H,V):- pmember(object_size(H,V),Points),!.
object_size(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

top(8).

:- style_check(-singleton).
guess_shape(I,Empty,N,1,1,Colors,Points,dot):- !.
guess_shape(I,Empty,N,N,1,Colors,Points,hline).
guess_shape(I,Empty,N,1,N,Colors,Points,vline).
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


   
:- dynamic(individuals_saved/2).
:- dynamic(reuse_grid_nums/1).


:- style_check(+singleton).

individuals(Grid,IndvS):-
  individuals_non_shared(Grid,IndvS),
  set_grid_nums(IndvS).


individuals_non_shared(Grid,IndvS):- is_grid(Grid),!,
 must_det_l((
  notrace((grid_size(Grid,H,V),
  globalpoints(Grid,Points))),
  locally(t_l:id_cells(Grid,Points),
  ( individuals_raw(Grid,Points,Indv_0),    
    % dmsg(is=Indv_0),    
    unraw_inds(Indv_0,Indv1),
    largest_first(Indv1,Indv2))),
  make_embued_points(Grid,H,V,Indv2,IndvS))).


individuals_non_shared(Grid,I):- is_objectlist(Grid),!,I=Grid.
individuals_non_shared(obj(Grid),[obj(Grid)]):-!.

individuals_raw(ID,Points,Indv):- 
  individuals_list(squares,ID,Points,Indv).

unraw_inds(IndvS,IndvO):-   
  largest_first(IndvS,Indv1),
  reverse(Indv1,IndvR),
  test_cond_or(indiv(min(SZ)),1),
  check_minsize(SZ,IndvR,Indv),
  unraw_inds2(_,Indv,IndvO),!.

%individuals_list_diamonds(_,Indv,Indv1):-
%  individuals_list(diamonds,ID,Indv,Indv1).


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



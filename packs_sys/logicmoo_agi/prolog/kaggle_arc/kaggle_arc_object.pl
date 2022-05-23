
:- dynamic(reuse_grid_nums/1).

   
:- dynamic(is_unshared_saved/2).

make_unshared_indivs(Gridname,Grid):- 
   set_gridname(Grid,Gridname),
   compute_unshared_indivs(Grid,IndvS),
   set_unshared_indivs(Gridname,IndvS).

get_unshared_indivs(Gridname,IndvS):- 
   is_unshared_saved(Gridname,IndvS).

set_unshared_indivs(Gridname,IndvS):- 
   retractall(is_unshared_saved(Gridname,_)),
   asserta(is_unshared_saved(Gridname,IndvS)),!.

ensure_unshared_indivs(Gridname):- is_unshared_saved(Gridname,_),!.
ensure_unshared_indivs(Gridname):- is_gridname(Grid,Gridname),!,
   make_unshared_indivs(Gridname,Grid),!.
ensure_unshared_indivs(Gridname):- throw(cannot_find(unshared(Gridname))).




:- dynamic(is_shared_saved/2).

make_shared_indivs(Gridname,Grid):- 
   set_gridname(Grid,Gridname),
   compute_shared_indivs(Grid,IndvS),
   set_shared_indivs(Gridname,IndvS).

/*get_shared_indivs(Grid,IndvS):- % @todo this should be correct already
   into_gridname(Grid,Gridname), Grid\=@=Gridname,!,
   get_shared_indivs(Gridname,IndvS).
*/
get_shared_indivs(Gridname,IndvS):- 
   is_shared_saved(Gridname,IndvS).
get_shared_indivs(Gridname,IndvS):- 
   compute_shared_indivs(Gridname,IndvS),
   set_shared_indivs(Gridname,IndvS).

set_shared_indivs(Grid,IndvS):- fail, % @todo this should be correct already
   into_gridname(Grid,Gridname), Grid\=@=Gridname,!,
   set_shared_indivs(Gridname,IndvS).
set_shared_indivs(Gridname,IndvS):- 
   retractall(is_shared_saved(Gridname,_)),
   asserta(is_shared_saved(Gridname,IndvS)),!.

ensure_shared_indivs(Gridname):- is_shared_saved(Gridname,_),!.
ensure_shared_indivs(Gridname):- is_gridname(Grid,Gridname),!,
   make_shared_indivs(Gridname,Grid),!.
ensure_shared_indivs(Gridname):- throw(cannot_find(shared(Gridname))).



:- style_check(+singleton).

grid_shared_with(Gridname*ExampleNum*in,Gridname*ExampleNum*out):-!.
grid_shared_with(Gridname*ExampleNum*out,Gridname*ExampleNum*in):-!.

use_shared_first(W) :- nb_current(grid_shared,W),W\==[],W\==nil.
use_shared_first:- use_shared_first(_).

get_shared_with(IndvS):- use_shared_first(With),
  ensure_unshared_indivs(With),
  is_unshared_saved(With,IndvS),!.
get_shared_with([]).


get_unshared(IndvS):- use_shared_first(With),
  grid_shared_with(With,Gridname),
  ensure_unshared_indivs(Gridname),
  is_unshared_saved(Gridname,IndvS),!.
get_unshared([]).


unset_nth(I,O):- delq(I,object_indv_id(_,_),O).
set_nth(I,O):- delq(I,object_indv_id(_,_),O).

get_combined(IndvC):- nb_current(test_name_w_type,NamedExampleNum), is_unshared_saved(NamedExampleNum,IndvC),!.
get_combined(IndvC):- nb_current(test_name_w_type,NamedExampleNum), 
   is_unshared_saved(NamedExampleNum*in ,IndvS1),
   is_unshared_saved(NamedExampleNum*out,IndvS2),
   make_combined(IndvS1,IndvS2,IndvC).
get_combined(IndvC):-get_shared_with(IndvS1),get_unshared(IndvS2),make_combined(IndvS1,IndvS2,IndvC).

with_each_indiv(G,I):- compute_shared_indivs(G,I).

make_combined(IndvS1,IndvS2,BetterC):-
  append(IndvS1,IndvS2,IndvSU),list_to_set(IndvSU,IndvS),
  smallest_first(IndvS,IndvC),
  cleanup(IndvS1,IndvS2,IndvC,BetterC),
  nb_current(test_name_w_type,NamedExampleNum),
  set_unshared_indivs(NamedExampleNum,BetterC),!.

cleanup(IndvS1,IndvS2,IndvC,_BetterO):-
  maplist(length,[IndvS1,IndvS2,IndvC],Rest),
  wdmsg(len=Rest),fail.

cleanup(IndvS1,IndvS2,IndvC,BetterO):- length(IndvS1,Len),Len>70,
  fix_pattern_objs(IndvS1,IndvS1A),
  cleanup(IndvS1A,IndvS2,IndvC,BetterO).
cleanup(IndvS1,IndvS2,IndvC,BetterO):- length(IndvS2,Len),Len>70,
  fix_pattern_objs(IndvS2,IndvS2A),
  cleanup(IndvS1,IndvS2A,IndvC,BetterO).
cleanup(IndvS1,IndvS2,IndvC,BetterO):- length(IndvC,Len),Len>70,
  fix_pattern_objs(IndvC,IndvCA),
  cleanup(IndvS1,IndvS2,IndvCA,BetterO).

cleanup(IndvS1,IndvS2,IndvC,BetterO):- length(IndvS1,Len),Len<70,
  select(A,IndvC,IndvC1),
  member(B,IndvC1),
  member(A,IndvS1),member(B,IndvS2),
  compute_diff(A,B,same_object(How)),!,
  retractall(is_unshared_saved(GridnameIn,IndvS1)),
  object_indv_id(B,NamedExampleNum,Iv),
  setq(A,object_indv_id(NamedExampleNum,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (i_glyph(GlyphB,GlyphBB), pt(same_object(GlyphBB,How))))),
  asserta(is_unshared_saved(GridnameIn,IndvS1)),
  append(IndvC1,[AA],BetterC),
  cleanup(IndvS1,IndvS2,BetterC,BetterO),!.
cleanup(_,_,A,A).

compute_shared_indivs(Grid,IndvS):- 
   must_be_free(IndvS),
   get_gridname(Grid,Gridname),
   grid_shared_with(Gridname,With),
   locally(b_setval(grid_shared,With),
           ( individuals_common(Grid,IndvS0),
             fix_static_objs(IndvS0,IndvS))).

compute_unshared_indivs(Grid,IndvS):- 
   locally(b_setval(grid_shared,nil),
           ( individuals_common(Grid,IndvS0),
             fix_static_objs(IndvS0,IndvS))),
   !.

filter_indivs(In,Filter,Out):- include(matches_filter(Filter),In,Out).

matches_filter(not(A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter((A;B),OBJ):- !, (matches_filter(A,OBJ);matches_filter(B,OBJ)).
matches_filter([A],OBJ):- !, matches_filter(A,OBJ).
matches_filter([A|B],OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter((A,B),OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter(E,obj(List)):- member(E,List).


must_be_free(AllNew):- var(AllNew),!.
must_be_free(AllNew):- dumpST,wdmsg(must_be_free(AllNew)),break,fail.


fix_pattern_objs(IndvS1,AllNew):- % stack_check(100),
 must_be_free(AllNew), length(IndvS1,L), L> 70,!,
  dmsg(fix_pattern_objs=L),
  into_grid(IndvS1,TempGrid),
  grid_size(TempGrid,H,V),H>5,V>5,
  globalpoints(TempGrid,Points),
  freeze(W,W>8),
  filter_indivs(IndvS1,[point_count(W)];[object_shape(rectangluar), object_shape(solid)],KeepIndvS1R),
  length(KeepIndvS1R,KeepIndvS1RLen),
  dmsg(keepIndvS1R=KeepIndvS1RLen),
  globalpoints(KeepIndvS1R,RemovePoints),
  remove_global_points(RemovePoints,Points,Points2),
  points_to_grid(H,V,Points2,NewTempGrid),
  catch(call_with_time_limit(1.3,grid_to_3x3_objs(NewTempGrid,NewIndiv1s),_),_,NewIndiv1s=[]),
  flatten([NewIndiv1s,KeepIndvS1R],AllNew),!.
fix_pattern_objs(IndvS1,IndvS1).

fix_static_objs(IndvS1,AllNew):- fix_pattern_objs(IndvS1,AllNew).
fix_static_objs(IndvS1,IndvS1).


individuals_common(Grid,IndvS):- % is_grid(Grid),!,
  must_be_free(IndvS),
  notrace(grid_size(Grid,H,V)),
  %pt(grd=Grid),
  globalpoints(Grid,Points),
  %pt(pts=Points),
  % maplist(is_cpoint,Points),
  locally(t_l:id_cells(Grid,Points),
  (((

  ( individuals_raw(Grid,Points,Indv_0),    
    % dmsg(is=Indv_0),    
    unraw_inds(Indv_0,Indv1),
    largest_first(Indv1,Indv2))),
  make_embued_points(Grid,H,V,Indv2,IndvS)))),
 nop(maplist(assert_IndvS,IndvS)).

/*
assert_IndvS(obj(L)):-
  object_indv_id(obj(L),NamedExampleNum,Iv),
  maplist(assert_IndvS(NamedExampleNum,Iv),L).


globalpoints(NamedExampleNum,Iv,ListOIfColoredPoints).
globalpoint(NamedExampleNum,Iv,Color,Point).
localpoints(NamedExampleNum,Iv,ListOIfColoredPoints).
localpoint(NamedExampleNum,Iv,Color,Point).
localpoints_nc(NamedExampleNum,Iv,Point).
object_size(NamedExampleNum,Iv,H,V).
object_shape(NamedExampleNum,Iv,ShapName).


assert_IndvS(NamedExampleNum,Iv,P):- P=..[F,List],is_list(List),
  maplist(assert_IndvS_L(NamedExampleNum,Iv,F),List).

assert_IndvS_L(NamedExampleNum,Iv,F),List

assert_IndvS(NamedExampleNum,Iv,P):- P=..[F|Args],
  PP=..[F,NamedExampleNum,Iv|Args],
  assert_IndvS_PP(NamedExampleNum,Iv,PP).
*/

individuals_common(Grid,I):- is_group(Grid),!,I=Grid.
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
  use_shared_first,
  get_combined(IndvSC), 
  consume_shared_individual_points(IndvSC,Types,Points,IndvList,Unused).
maybe_shared_individuals_list(_Types,UnusedPoints,[],UnusedPoints):- !.

unraw_inds(IndvS,IndvOO):-   
  largest_first(IndvS,Indv1),
  reverse(Indv1,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).


%individuals_list_diamonds(_,Indv,Indv1):-
%  individuals_list(diamonds,ID,Indv,Indv1).


consume_shared_individual_points([],_Types,UnusedPoints,[],UnusedPoints):- !.
consume_shared_individual_points([H|T],Types,Points,[H|IndvList],Unused):-  
  globalpoints(H,GPoints), remove_gpoints(GPoints,Points,LeftOver),
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
  globalpoints(IndvC,GPoints),
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

colors_block_diag(C,TLC,TRC,_C2):- get_bgc(Zero), TLC==TRC, TRC\==Zero, C \== TRC, \+ free_cell(TRC).

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
allow_dir(all,[nw,sw,se,ne,n,w,s,e]).
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

check_minsize(_,I,I):-!.
check_minsize(_,[],[]):-!.
check_minsize(Sz,[I|IndvS],[A,B|IndvSO]):- point_count(I,2),globalpoints(I,[A,B]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[A,B,C|IndvSO]):- point_count(I,3),globalpoints(I,[A,B,C]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[I|IndvSO]):- check_minsize(Sz,IndvS,IndvSO).

meets_size(_,Points):- point_count(Points,1).
meets_size(_,Points):- point_count(Points,2),!,fail.
meets_size(_,Points):- point_count(Points,3),!,fail.
meets_size(_,Points):- point_count(Points,4).
meets_size(Len,Points):- point_count(Points,L),!,L>=Len.

remove_bgs(IndvS,IndvL,BGIndvS):- partition(is_bg_indiv,IndvS,BGIndvS,IndvL).

is_bg_indiv(O):- colors_count(O,[color_count(C,CC)]),CC>0,is_bgc(C).

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
   \+ (is_diag(Dir),is_bgc(C)),
   is_adjacent_point(Point2,Dir,Point3),
   single_point(C-Point3,Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).


reserved_point(C-Point):- fail, 
 \+ (get_shared_with(With), member(Obj,With),
  globalpoints(Obj,GPoints), member(CC-Point,GPoints), CC==C).

single_point(C-Point,IndvS,Rest1):-
  single_point0(C-Point,IndvS,Rest1),
   \+ reserved_point(C-Point).

single_point0(C-Point,IndvS,Rest1):-
  select([C-Point],IndvS,Rest1),
  nonvar(C).
single_point0(C-Point,IndvS,Rest1):-
  select(C-Point,IndvS,Rest1),
  nonvar(C).

single_point0_disabled(C-Point,IndvS,Rest1):- 
  select(obj(I),IndvS,Rest1), fail, % round 2
  globalpoints(obj(I),[C-Point]),
  nonvar(C).

/*
unraw_inds2(Types,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv1),
   reverse(Indv1,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Types,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(Types,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar(C), \+ free_cell(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,object_shape(diagonal),C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_dir(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  % minimum 4 findall(C-CP,member(C-CP,NewGroup),LL),LL=[_,_,_,_|_],
  unraw_inds2(Types,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(Types,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), \+ free_cell(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,object_shape(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
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
  globalpoints(Grid,Points),
  embue_points(GN,H,V,1,1,H,V,Points,OUT).

make_embued_points(Grid,H,V,Points,IndvS):- 
  get_gridname(Grid,GN),
  maplist(embue_points(GN,H,V),Points,IndvS).

is_not_cpoint(I):- \+ is_cpoint(I).
is_not_gpoint(I):- \+ is_gpoint(I).
%embue_points(GN,_,_,I,I):-!.
embue_points(_,_,_,obj(Ps),obj(Ps)):-!.
embue_points(GN,H,V,Points,OUT):- 
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  embue_points(GN,H,V,LoH,LoV,HiH,HiV,Points,OUT).

is_gpoint(G):- var(G),!,fail.
is_gpoint(_-G):-!,is_gpoint(G).
is_gpoint(G):- hv_point(H,_,G),!,nonvar(H).

close_enough_grid(GridIn,GridInCopy,GridO):- 
  \+ \+ (GridO=GridIn, GridIn=@=GridInCopy).

embue_points(GN,H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_points(GN,H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).

embue_points(NamedExampleNum,H,V,LoH,LoV,HiH,HiV,IPoints,obj(OBJ)):- 
  embue_points1(NamedExampleNum,H,V,LoH,LoV,HiH,HiV,IPoints,OBJ).

into_gridname(G,TstName):- nonvar(G), into_gridname(GVar,TstName),G=@=GVar,!.

into_gridname(G,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,G,_).
into_gridname(G,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,G).
into_gridname(G,TstName):- is_gridname(G,TstName).
into_gridname(G,TstName):- is_shared_saved(TstName,G).
into_gridname(G,TstName):- is_unshared_saved(TstName,G).
into_gridname(G,TstName*T):- fix_test_name(TstName+T,Name,ExampleNum),kaggle_arc(Name,ExampleNum,G,_).
into_gridname(G,TstName):- learned_color_inner_shape(TstName,magenta,BG,G,_),get_bgc(BG).


  %pt(Image-->Image9x9),
  %into_gridname(Grid,Gridname),
  %quaderants_and_center_rays(Image9x9,Quads,Centers,Rays),
  %append([Quads,Centers,Rays],NewIndiv1s00),
  %trace,
  %correctify_objs(Gridname,NewIndiv1s00,NewIndiv1s),!.


/*correctify_objs(Gridname,NewIndiv1s00,NewIndiv1s):- is_list(NewIndiv1s00),maplist(correctify_objs(Gridname),NewIndiv1s00,NewIndiv1s).
correctify_objs(Gridname,obj(List),obj(NOBJ)):- is_list(List), 
   member(grid(Grid),List),
   \+ member(globalpoints(_),List),
   grid_size(Grid,H,V),
   %trace,
   pt(dleaing_with=obj(List)),
   globalpoints(Grid,Points),
   assertion(Points\=[]),
   points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
   %nb_current(test_name_w_type,NamedExampleNum),
   embue_points1(Gridname,H,V,LoH,LoV,HiH,HiV,Points,OBJ),
   append(List,OBJ,NOBJ),!.
correctify_objs(_Gridname,obj(List),obj(List)):-!.
correctify_objs(_Gridname,Obj,Obj).
   %make_embued_points(Grid,H,V,Points,IndvS)
*/

embue_points1(GridnameExampleNumIO,H,V,LoH,LoV,HiH,HiV,IPoints,Ps):-
 %must_det_l
 ((
 include(is_gpoint,IPoints,Points),
 include(is_not_gpoint,IPoints,Info),
 assertion(ground(Points)),
 flag(indiv,Iv,Iv+1),
  colors_count(Points,CC),
  length(Points,Len),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_current(test_name_w_type,GridnameExampleNumIO),
  
  %gensym(id_,IID),
  Area is Width * Height,
  Empty is  Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  remove_color(LPoints,CLPoints),
  make_grid(Width,Height,Grid),
  set_local_points(LPoints,Grid,Grid),
  make_grid(Width,Height,GridO),
  copy_term(Grid,GridInCopy),
  findall(object_shape(Shape),
   (guess_shape(Grid,GridO,Ps,Empty,Len,Width,Height,CC,CLPoints,Shape),close_enough_grid(Grid,GridInCopy,GridO)),Shapes),
  append([[
    localpoints_nc(CLPoints),
    colors_count(CC),
    object_size(Width,Height),
    point_count(Len)],Shapes,
    Info,
    %width(Width), height(Height), area(Area),
    %missing(Empty),
    [localpoints(LPoints)],
    [grid(GridO)],
    [object_offset(LoH,LoV)],
    [globalpoints(Points),
    object_indv_id(GridnameExampleNumIO,Iv),
    grid_size(H,V)]],Ps))).


object_indv_id(I,NamedExampleNum,Iv):- indv_props(I,L),member(object_indv_id(NamedExampleNum,Iv),L),!.
object_indv_id(_,NamedExampleNum,_Iv):- nb_current(test_name_w_type,NamedExampleNum).

point_count(_-P,1):- nonvar(P),!.
point_count(I,X):- indv_props(I,L),member(point_count(X),L),!.
point_count(obj(I),Count):- localpoints(I,Points), length(Points,Count),!.
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

enum_group(S):- is_unshared_saved(_,S).

enum_object(S):- is_unshared_saved(_,IndvS),member(S,IndvS).
enum_object(S):- is_gridname(S,_).

%indv_props(Obj,L):- compound(Obj), arg(1,Obj,L), is_list(L),!.
indv_props(obj(L),L):- is_list(L),!.
indv_props(obj(L),L):- enum_object(obj(L)).

walls_thick1(G):- localpoints(G,Points),counted_neighbours(Points,ListOfSizes),walls_thick1_sizes(ListOfSizes).
walls_thick1_sizes(List):- maplist(size(2),List).

is_lpoint(P):- is_nc_point(P).
is_lpoint(P):- is_cpoint(P).

is_points_list([G|L]):- !, is_lpoint(G),maplist(is_lpoint,L).
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
globalpoints(Grid,[Grid]):- is_lpoint(Grid),!.
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
colors_count(G,BFO):- pixel_colors(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),into_cc(SK,BFO).

localpoints_nc(I,X):- indv_props(I,L),member(localpoints_nc(X),L).

throw_missed(G):-  Info = missed(G),wdmsg(Info), dumpST,throw_missed_pt2(G,Info).
throw_missed_pt2(_,Info):- tracing,!,throw(Info).
throw_missed_pt2(G,Info):- notrace,nortrace,trace,wdmsg(Info),break,rtrace(G),throw(Info).

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
guess_shape(GridIn,GridO,I,0,1,1,1,Colors,Points,dot):- !.
guess_shape(GridIn,GridO,I,0,N,N,1,Colors,Points,line(h)).
guess_shape(GridIn,GridO,I,0,N,HV,HV,Colors,Points,square):- HV>1.
guess_shape(GridIn,GridO,I,0,N,H,V,Colors,Points,nonsquare):- H \== V.
guess_shape(GridIn,GridO,I,I,N,H,V,Colors,Points,rectangluar):- H>1, V>1.
guess_shape(GridIn,GridO,I,_,N,H,V,[color_count(Color,_)],Points,outline):- H>2,V>2, N>7,add_borders(Color,GridIn,GridO).
  
guess_shape(GridIn,GridO,I,0,N,1,N,Colors,Points,line(v)).
guess_shape(GridIn,GridO,I,0,9,3,3,Colors,Points,keypad).
guess_shape(GridIn,GridO,I,Empty,N,H,V,[color_count(Zero,_)],Points,background):- is_bgc(Zero).
guess_shape(GridIn,GridO,I,O,N,H,V,Colors,Points,nonsolid):- O\==0.

guess_shape(GridIn,Grid,I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.object_offset=object_offset(LoH,LoV),
   make_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   compute_shared_indivs(Grid,InvS))),!,
   InvS=[_,_|_].


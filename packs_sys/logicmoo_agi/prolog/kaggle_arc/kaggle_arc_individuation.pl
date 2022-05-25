:- use_module(library(multivar)).

maybe_multivar(C):- nonvar(C),!.
maybe_multivar(C):- multivar(C).

:- dynamic(reuse_grid_nums/1).

:- discontiguous(find_some_individuals/13).
   
:- dynamic(is_unshared_saved/2).
:- dynamic(is_shared_saved/2).


:- style_check(+singleton).

grid_shared_with(Gridname*ExampleNum*in,Gridname*ExampleNum*out):-!.
grid_shared_with(Gridname*ExampleNum*out,Gridname*ExampleNum*in):-!.

get_grid_and_name(In,Grid,GN):- is_grid(In),!,get_gridname(Grid,GN).
get_grid_and_name(In,Grid,GN):- trace,into_grid(In,Grid),!,get_gridname(Grid,GN).
   
compute_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   compute_unshared_indivs(GN,Grid,Unshared).

compute_unshared_indivs(_GN,Grid,Unshared):-
   individuals_common([],Grid,Unshared).

compute_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   compute_shared_indivs(GN,Grid,SharedIndvs).
compute_shared_indivs(GN,Grid,SharedIndvs):-
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   compute_unshared_indivs(With,OtherGrid,Unshared),
   individuals_common(Unshared,Grid,SharedIndvs).


ensure_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   ensure_unshared_indivs(GN,Grid,Unshared).
ensure_unshared_indivs(GN,Grid,Unshared):-
   is_unshared_saved(GN,Unshared)-> true;
   individuals_common([],Grid,Unshared),
   assert(is_unshared_saved(GN,Unshared)).

ensure_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   ensure_shared_indivs(GN,Grid,SharedIndvs).
ensure_shared_indivs(GN,Grid,SharedIndvs):-
   is_shared_saved(GN,SharedIndvs)-> true;
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   ensure_unshared_indivs(With,OtherGrid,Unshared),
   individuals_common(Unshared,Grid,SharedIndvs),
   assert(is_shared_saved(GN,SharedIndvs)).


merge_indivs(IndvA,IndvB,BetterA,BetterB,BetterC):-
  append(IndvA,IndvB,IndvSU),list_to_set(IndvSU,IndvS),
  smallest_first(IndvS,IndvC),
  merge_indivs_cleanup(IndvA,IndvB,IndvC,BetterA,BetterB,BetterC),!.

merge_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
merge_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  merge_a_b(A,B,AA),
  append(IndvARest,[AA],BetterA),
  append(IndvBRest,[B],BetterB),
  append(IndvCRest,[AA],BetterC),
  merge_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
merge_indivs_cleanup(A,B,C,A,B,C).

merge_a_b(A,B,AA):- 
  compute_diff(A,B,same_object(How)),!,
  object_indv_id(B,NamedExampleNum,Iv),
  setq(A,object_indv_id(NamedExampleNum,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (i_glyph(GlyphB,GlyphBB),
      pt(same_object(GlyphBB,How))))).





individuals_common(Reserved,Grid,IndvS):-
 must_det_l((
   notrace(grid_size(Grid,H,V)),
   must_be_free(IndvS),
   globalpoints(Grid,Points),
   select_default_i_options(Grid,H,V,Points,Options),   
   into_gridname(Grid,ID),   
   individuals_raw(H,V,ID,Options,Reserved,Points,Grid,IndvSRaw),
   as_debug(pt(individuals_common=IndvSRaw)),   
   make_indiv_object_list(ID,H,V,IndvSRaw,IndvS))).

individuals_raw(GH,GV,ID,Options,Reserved,Points,Grid,IndvSRaw):-
 must_det_l((
  must_be_free(IndvSRaw),
  individuals_list(GH,GV,[],ID,Options,Reserved,Points,Grid,Indv_0,_LeftOverPoints),
  unraw_inds(Indv_0,Indv),
  largest_first(Indv,IndvSRaw))).

%maybe_join_shared_individuals_list(Sofar,squares,ELeftOverPoints,Indv0S,Indv1S,LeftOverPoints):-
%  select(Indv0,Rest,Indv0S),member(Indv,Rest),object_shape(Indv0,line(h)), object_shape(Indv,line(h)).


unraw_inds(IndvS,IndvOO):-   
  largest_first(IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).

%individuals_list(H,V,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):- fail, Sofar\==[],
%  make_indiv_object_list(Grid,H,V,Sofar,OUT), OUT\==Sofar,!,
%  individuals_list(H,V,OUT,ID,Options,Reserved,Points,Grid,IndvList,LeftOver).

%TODO UNCOMMENT 
individuals_list(_GH,_GV,Sofar,_ID,_Options,_Reserved,Points,_Grid,Sofar,Points):- Points == [], !.
individuals_list(GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
    assertion(maplist(is_cpoint,Points)),
    assertion(is_list([sofar1(Options)|Sofar])),
    find_some_individuals(NewReserved,NewGrid,NO,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,FoundSofar,NextScanPoints),
    assertion(maplist(is_cpoint,NextScanPoints)),
    assertion(is_list([foundSofar1(Options)|FoundSofar])),
    ((FoundSofar\==Sofar) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),
    %wqnl(indv(Options)=Indv),   
    individuals_list(GH,GV,FoundSofar,ID,NO,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.
individuals_list(GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
    next_options(Options,Options2),!,
    individuals_list(GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints).
individuals_list(GH,GV,Sofar,ID,Options,_Reserved,Points,_Grid,IndvListOut,[]):-
 must_be_free(IndvListOut),
 must_det_l((
  assertion(is_list([sofar2(Options)|Sofar])),
  assertion(is_list([sofar|Sofar])),
  assertion(is_list([points|Points])),
  assertion(maplist(is_cpoint,Points)),
  as_debug(print_igrid(GH,GV,'leftover_points'+ID,Points,[])),
  maplist(make_point_object(ID,GH,GV),Points,IndvList),
  as_debug(print_igrid(GH,GV,'indvList'+ID,IndvList,[])),
  append(Sofar,IndvList,IndvListOut))),!.
individuals_list(GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P):-!,
  dmsg(fail(individuals_list(GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P))),!.


find_some_individuals(StillReserved,Grid,NO,H,V,Sofar,ID,[use_reserved|NO],Reserved,Points,Grid,SofarOut,NextScanPoints):-
   proccess_overlap_reserved(ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,StillReserved),
   intersection(SofarOut,Sofar,_Intersected,LeftOverA,_LeftOverB),
   as_debug(print_igrid(H,V,'use_reserved'+ID,LeftOverA,[])),!.

proccess_overlap_reserved(ID,H,V,[Obj|Reserved],Sofar,SofarOut,Points,NextScanPoints,[Obj|Unreserved],StillReserved):- 
   globalpoints(Obj,ObjPoints), 
   intersection(ObjPoints,Points,Intersected,LeftOverA,LeftOverB), LeftOverA == [],!,
   make_indiv_object(ID,H,V,Intersected,Indiv),
   append(Sofar,[Indiv],NewSofar),
   proccess_overlap_reserved(ID,H,V,Reserved,NewSofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).

proccess_overlap_reserved(ID,H,V,[Obj|Reserved],Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,[Obj|StillReserved]):-
   proccess_overlap_reserved(ID,H,V,Reserved,Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).
proccess_overlap_reserved(_ID,_H,_V,[],Sofar,Sofar,NextScanPoints,NextScanPoints,[],[]).



append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).

% ===============
% release_points
% ===============
must_be_nonvar(X):- assertion(nonvar(X)),!.
must_be_nonvar(X):- break,nonvar(X),!.

find_some_individuals(OUTReserved,OUTNewGrid,OUTOptions,H,V,Sofar,ID,Options,Reserved,Points,Grid,OUTSofar,OUTNextScanPoints):-
  maplist(must_be_free,[OUTReserved,OUTNewGrid,OUTOptions,OUTSofar,OUTNextScanPoints]),
  maplist(must_be_nonvar,[Options,H,V,Sofar,ID,Options,Reserved,Points,Grid]),
  as_debug(pt([find_some_individuals=Options,sofar=Sofar])),
  fail.


find_some_individuals(Reserved,NewGrid,NO,H,V,Sofar,ID,[release_points|NO],Reserved,Points,Grid,Sofar,NextScanPoints):-
    globalpoints(Grid,NextScanPoints1),
    globalpoints(Sofar,NextScanPoints2),
    append_sets([Points,NextScanPoints2,NextScanPoints1],NextScanPoints),
    (as_debug(print_igrid(H,V,'release_points(Sofar)'+ID,Sofar,[]))),
    points_to_grid(H,V,NextScanPoints,NewGrid), 
  !.

   

find_some_individuals(Reserved,Grid,NO,_H,_V,Sofar,_ID,[retain(_)|NO],Reserved,Points,Grid,Sofar,Points):-  
  Sofar = [], !.

find_some_individuals(Reserved,NNewGrid,NO,H,V,Sofar,ID,[retain(Option)|NO],Reserved,_Points,Grid,Retained,NextScanPoints):-
    globalpoints(Grid,NewGPoints),  H> 14, V> 14,
    freeze(W,W>5),filter_indivs(Sofar,[point_count(W)];[object_shape(Option)],Retained1),
    filter_indivs(Retained1,\+ object_shape(background),Retained),
    as_debug(print_igrid(H,V,'retained'+ID,Retained,[])),    
    remove_global_points(Retained,NewGPoints,NextScanPoints),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    %as_debug(print_igrid(H,V,'newgrid'+ID,NNewGrid,[])),
    !.

find_some_individuals(Reserved,Grid,NO,H,V,Sofar,ID,[into_single_hidden|NO],Reserved,Points,Grid,SofarIndvS1,Points):- !,
    maplist(globalpoints,Sofar,IndvPointSofar), append_sets(IndvPointSofar,IndvPointsL),
    EIndvPoints=[object_shape(combined),point_count(0),object_shape(hidden)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    append(Sofar,[Indv],SofarIndvS1),
    meets_indiv_criteria(into_single_hidden,IndvPointsL),!.

find_some_individuals(Reserved,Grid,NO,H,V,Sofar,ID,[into_single|NO],Reserved,Points,Grid,[Indv],Points):- !,
    maplist(globalpoints,Sofar,IndvPoints), append(IndvPoints,IndvPointsL),
    EIndvPoints=[object_shape(combined),object_shape(single)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    meets_indiv_criteria(into_single_hidden,IndvPoints),!.

find_some_individuals(Reserved,Grid,NO,H,V,Sofar,ID,[fourway|NO],Reserved,Points,Grid,OutInvdivS,NextScanPoints):- !,
     H > 13, V> 13,
    grid_to_3x3_objs(Grid,FourWay1s),
    overwrite_use_so_far(FourWay1s,Sofar,UseSoFar),
    append(UseSoFar,FourWay1s,OutInvdivS),
    as_debug(print_igrid(H,V,'4-way'+ID,FourWay1s,[])),
    remove_global_points([UseSoFar,FourWay1s],Points,NextScanPoints).

find_some_individuals(Reserved,Grid,NO,H,V,Sofar,ID,[fourway|NO],Reserved,Points,Grid,OutInvdivS,NextScanPoints):- !,
     H > 13, V> 13,
    grid_to_3x3_objs(Grid,FourWay1s),
    overwrite_use_so_far(FourWay1s,Sofar,UseSoFar),
    append(UseSoFar,FourWay1s,OutInvdivS),
    as_debug(print_igrid(H,V,'4-way'+ID,FourWay1s,[])),
    remove_global_points([UseSoFar,FourWay1s],Points,NextScanPoints).



overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_l((remove_global_points(FourWay1s,Sofar,Sofar1),set_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).


find_some_individuals(Reserved,Grid,NO,H,V,Sofar,ID,[solid(squares)|NO],Reserved,_Points,Grid,AllOUT,NextScanPoints):- !,
  globalpoints(Grid,NewGPoints),
  grid_to_segs(Grid,Segs),
  seg_squares(Segs,Squares),
  segs_to_pointlists(Squares,Objs),  
  make_indiv_object_list(ID,H,V,Objs,OutInvdivS),
  %maplist(print_grid,OutInvdivS),
  as_debug(print_igrid(H,V,'Solid-boxes'+ID,OutInvdivS,[])),!,
  remove_global_points(OutInvdivS,NewGPoints,NextScanPoints),
  append(Sofar,OutInvdivS,AllOUT).

find_some_individuals(Reserved,NewGrid,NO,H,V,Sofar,_ID,[regroup|NO],Reserved,Points,Grid,OutInvdivS,Points):- 
  make_indiv_object_list(Grid,H,V,Sofar,OutInvdivS), 
  points_to_grid(H,V,Points,NewGrid).


find_some_individuals(Reserved,Grid,[dots|NO],H,V,Sofar,ID,[dots|NO],Reserved,Points,Grid,[Indv|Sofar],Rest):-
    maybe_multivar(C),select(C-HV,Points,Rest),  \+ free_cell(C),
    \+ \+ (( is_adjacent_point(HV,_,HV2),member(C2-HV2,Rest),ok_color_with(C,C2))),
   IndvPoints=[object_shape(dots),C-HV],
   make_indiv_object(ID,H,V,IndvPoints,Indv),
   meets_indiv_criteria([dots|NO],IndvPoints),!.

find_some_individuals(Reserved,Grid,[dots|NO],H,V,Sofar,ID,[dots|NO],Reserved,Points,Grid,[Indv|Sofar],Rest):-
   maybe_multivar(C), select(C-HV,Points,Rest),  \+ free_cell(C),
   IndvPoints=[object_shape(dots),C-HV],
   make_indiv_object(ID,H,V,IndvPoints,Indv),
   meets_indiv_criteria([dots|NO],IndvPoints),!.

find_some_individuals(Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):-    
    find_one_individual(H,V,Sofar,ID,Option,Reserved,Points,Grid,IndvPoints,NextScanPoints),
    make_indiv_object(ID,H,V,IndvPoints,Indv),
    append(Sofar,[Indv],OUT).



/*
find_some_individuals(NewReserved,NewGrid,NO,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-    
    find_one_individual(GH,GV,Sofar,ID,Options,Reserved,Points,Grid,Indv,ScanPoints),!, %wqnl(indv(Options)=Indv),    
    find_some_individuals(NewReserved,NewGrid,NO,GH,GV,[Indv|Sofar],ID,Options,Reserved,ScanPoints,Grid,IndvList,NextScanPoints).
*/
%find_some_individuals(NewReserved,NewGrid,NO,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,Sofar,Points).


% obj1(X,X):- make_indiv_object(

ok_color_with(C,C2):- /* non_free_fg(C2), */ C==C2.

sameglobalpoints(Points,IndvC,NextScanPoints,IndvC):-
  globalpoints(IndvC,GPoints),
  remove_gpoints(GPoints,Points,NextScanPoints).

remove_gpoints([],Rest,Rest).
remove_gpoints([GPoint|GPoints],Points,Rest):- select(GPoint,Points,Mid),remove_gpoints(GPoints,Mid,Rest).

get_neighbors(From,P1,Found,HV):- 
  findall(f(Dir,C-HV2), (is_adjacent_point(HV,Dir, HV2),member(C-HV2,From),call(P1,C)),Found).

has_2neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allow_dirs(Option,N),turn_left_45(N,NW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),member(C-HV3,Rest2),!.

has_3neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allow_dirs(Option,N),turn_left_45(N,NW),turn_left_45(NW,W),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),member(C-HV4,Rest3),!.

has_4neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allow_dirs(Option,N),turn_left_45(N,NW),turn_left_45(NW,W),turn_left_45(W,SW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),select(C-HV4,Rest3,Rest4),
    is_adjacent_point(HV,SW, HV5),member(C-HV5,Rest4),!.

min_neighbours(0,_,_,_).
min_neighbours(Count,Dir,C,Rest,HV):- maybe_multivar(C),     
    is_adjacent_point(HV,Dir, HV2),member(C-HV2,Rest),
    turn_left_45(Dir,NW), CountDown is Count-1, min_neighbours(CountDown,NW,C,Rest,HV).

/*
find_one_individual(GH,GV,Sofar,ID,solid(Option),Reserved,Points,Grid,[object_shape(solid(Option))|Indv],NextScanPoints):- fail, !,
    select(C-HV,Points,Rest), non_free_fg(C), 
    get_neighbors(Rest,non_free_fg,Found,HV),
    format('~N~q~n',[Found]),
    select(f(Dir,C-HV2),Found,Found1),    
    allow_dirs(Option,Dir),
    turn_right_45(Dir,NE), turn_left_45(Dir,NW), 
    select(f(NE,C-_HV3),Found1,Found2),
    select(f(NW,C-_HV4),Found2,Found3),
    turn_right_45(NE,E), turn_left_45(NW,W),
    select(f(E,C-HV5),Found3,Found4),
    select(f(W,C-HV6),Found4,Found5),
    subtract(Rest,[C-HV5,C-HV6],ScanPoints),
    all_individuals_near(Option,C,[C-HV,C2-HV2,C-HV5,C-HV6],ScanPoints,NextScanPoints,Indv),
    meets_indiv_criteria(Option,Indv).
*/

find_one_individual(_GH,_GV,_Sofar,_ID,Option,_Reserved,Points,_Grid,[object_shape(Option)|IndvPoints],NextScanPoints):-
    maybe_multivar(C), select(C-HV,Points,Rest), non_free_fg(C), % \+ is_black(C),
    allow_dirs(Option,Dir),
    adjacent_point_allowed(C,HV,Dir,HV2),select(C2-HV2,Rest,ScanPoints),ok_color_with(C,C2),
    all_individuals_near(Option,C,[C-HV,C2-HV2],ScanPoints,NextScanPoints,IndvPoints),
    meets_indiv_criteria(Option,IndvPoints),!.

color_of(HV,TL,TLC):- t_l:id_cells(_ID,Points), is_adjacent_point(HV,TL,TLHV),member(TLC-TLHV,Points).

colors_block_diag(C,TLC,TRC,_C2):- get_bgc(Zero), TLC==TRC, TRC\==Zero, C \== TRC, non_free_fg(TRC).

filtered_point(C,HV):- maybe_multivar(C), t_l:id_cells(_ID,Points),% select(_-HV,Points,Rest), 
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
%adjacent_point_allowed(_,C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS),member(Dir,DirS).
adjacent_disallowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS), \+ member(Dir,DirS).


all_individuals_near(_Options,_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(Options,C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(Options,C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(Options,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_Options,_C,_From,[],[],[]):-!.
individuals_near(Options,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Options,C,E,From),!,
  individuals_near(Options,C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(Options,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Options,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(Options,C,C2-E,List):- allow_dirs(Options,Dir), adjacent_point_allowed(C,E2,Dir,E), member(C2-E2,List),ok_color_with(C2,C).

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


% oclass_piority(Class,Priority).
oclass_piority(hidden,2).
%object_priority(Indv,Priority):- oclass_piority(Class,Priority), iz(Indv,Class).
object_priority(_,0).
  
smallest_first(IndvS,IndvO):-
  findall((Priority+Size)-Indv,(member(Indv,IndvS),object_priority(Indv,Priority),point_count(Indv,Size)),All),
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


single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  single_point0(C-Point,IndvS,Rest1).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(obj(I),IndvS,Rest1), fail, % round 2
  globalpoints(obj(I),[C-Point]),
  nonvar(C).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select([C-Point],IndvS,Rest1),
  nonvar(C).

single_point0(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(C-Point,IndvS,Rest1),
  nonvar(C).


/*
unraw_inds2(Options,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv),
   reverse(Indv,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Options,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(Options,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar(C), non_free_fg(C),\+ get_bgc(C),
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
  unraw_inds2(Options,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(Options,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,object_shape(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_dir(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Options,[NewGroup|RRestO],IndvO).



unraw_inds2(Options,IndvS,IndvO):-  fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), non_free_fg(C),
  single_point(C2-Point2,Rest1,Rest),
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Options,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(Options,IndvS,IndvO):- 
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(Options,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(Options,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Options,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Options,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_,IndvS,IndvS).


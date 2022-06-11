/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(multivar)).

maybe_multivar(C):- nonvar(C),!.
%maybe_multivar(C):- multivar(C).
maybe_multivar(_).

:- dynamic(reuse_grid_nums/1).

:- discontiguous(fsi/14).
:- discontiguous(fti/2).
   
:- dynamic(is_unshared_saved/2).
:- dynamic(is_shared_saved/2).


:- style_check(+singleton).

grid_shared_with(TestName*ExampleNum*in,TestName*ExampleNum*out):-!.
grid_shared_with(TestName*ExampleNum*out,TestName*ExampleNum*in):-!.

get_grid_and_name(In,Grid,GN):- is_grid(In),!,get_gridname(Grid,GN).
get_grid_and_name(In,Grid,GN):- into_grid(In,Grid),!,get_gridname(Grid,GN).



compute_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   compute_unshared_indivs(GN,Grid,Unshared).

compute_unshared_indivs(_GN,Grid,Unshared):-
   individuate([],Grid,Unshared).

compute_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   compute_shared_indivs(GN,Grid,SharedIndvs).
compute_shared_indivs(GN,Grid,SharedIndvs):-
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   compute_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs).


ensure_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   ensure_unshared_indivs(GN,Grid,Unshared).
ensure_unshared_indivs(GN,Grid,Unshared):-
   is_unshared_saved(GN,Unshared)-> true;
   individuate([],Grid,Unshared),
   assert(is_unshared_saved(GN,Unshared)).

ensure_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   ensure_shared_indivs(GN,Grid,SharedIndvs).
ensure_shared_indivs(GN,Grid,SharedIndvs):-
   is_shared_saved(GN,SharedIndvs)-> true;
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   ensure_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs),
   assert(is_shared_saved(GN,SharedIndvs)).

:- decl_pt(detect_indvs(group,group,-)).
detect_indvs(In,Out,Grid):- individuate(In,Grid,Out).

%make_indivs_options([],[options(defaults)]):-!.
%make_indivs_options(NList,O):- listify(NList,O),!.
make_indivs_options(NList,O):- listify(NList,List),maplist(make_indivs_option_e,List,O).

make_indivs_option_e(G,G):- var(G),!.
make_indivs_option_e(options(O),options(O)):-!.
make_indivs_option_e(detect(O),detect(O)):-!.
make_indivs_option_e(O,(O)):- is_object(O),!.
make_indivs_option_e(G,(G)):- is_grid(G),!.
make_indivs_option_e(E,options(L)):- listify(E,L).

my_expand_individualizer(ROptions1,ROptions1):-!.
%my_expand_individualizer(ROptions1,ReservedOptions):- expand_individualizer(ROptions1,ReservedOptions).
fix_indivs_options(ROptions,ReservedOptions):-
   make_indivs_options(ROptions,ROptions1),
   my_expand_individualizer(ROptions1,ReservedOptions),!.

individuation_reserved_options(ROptions,Reserved,NewOptions):- 
   fix_indivs_options(ROptions,ReservedOptions),
   my_partition(is_object_or_grid,ReservedOptions,Reserved,Options0),
   %select_default_i_options(Grid,H,V,Points,DefaultOptions),
   default_i_options(DefaultOptions),
   subst(Options0,defaults,DefaultOptions,Options),
   (Options0==Options -> append(Options,DefaultOptions,NewOptions) ; (Options=NewOptions)),!,
   ignore((ROptions \= ReservedOptions,
      pt(blue,fix_indivs_options(in=ReservedOptions,r=Reserved,o=NewOptions)))).


individuate(ROptions,Grid,IndvS):-
   individuation_reserved_options(ROptions,Reserved,NewOptions),
   individuate(Reserved,NewOptions,Grid,IndvS).

individuate(Reserved,NewOptions,Points,IndvS):-  is_points_list(Points),points_to_grid(Points,Grid),!, 
   individuate(Reserved,NewOptions,Grid,IndvS).

individuate(Reserved,NewOptions,GridIn,IndvS):-   
   into_points_grid(GridIn,Points,Grid),
   quietly(grid_size(Grid,H,V)), wdmsg(individuate(H,V)),
   must_be_free(IndvS),
   into_gridname(Grid,ID),
   individuate(H,V,ID,Reserved,NewOptions,Grid,Points,IndvS).

% tiny grid becomes a series of points
individuate(GH,GV,ID,_Reserved,_NewOptions,_Grid,Points,IndvS):- is_glyphic(Points,GH,GV), !,
  individuate_glyphic(GH,GV,ID,Points,IndvS).
individuate(H,V,ID,Reserved,NewOptions,Grid,Points,IndvSS):-
   individuals_raw(H,V,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
   %as_debug(9,ptt((individuate=IndvSRaw))),
   make_indiv_object_list(ID,H,V,IndvSRaw,IndvS1),
   combine_objects(IndvS1,IndvS),list_to_set(IndvS,IndvSS),
   save_grouped(individuate(ID),IndvSS).

into_points_grid(GridIn,Points,Grid):- 
   globalpoints(GridIn,Points),
   into_grid(GridIn,Grid),!.

is_glyphic(Points,GH,GV):- ( GH=<5 ; GV=<5 ), length(Points,Len), Len is GH * GV.

individuate_glyphic(_GH,_GV,_ID,PointsIn,IndvS):- PointsIn==[],!,IndvS=[].
individuate_glyphic(GH,GV,ID,PointsIn,IndvSO):-
  globalpoints(PointsIn,Points),
  length(Points,Len),
  make_indiv_object(ID,GH,GV,Points,[mass(Len),vis_hv(GH,GV),object_shape(combined),object_shape(grid),object_shape(image)],Whole),
  %object_indv_id(Whole,_,Iv),
  maplist(make_point_object(ID,GH,GV,[object_shape(abstract),object_shape(colorlink)]),Points,IndvList),
  append(IndvList,[Whole],IndvS),!,
  override_object(birth(glyphic),IndvS,IndvSO).

fti_go(Grid,Options,Image.objs):-
  globalpoints(Grid,Points),
  grid_size(Grid,GH,GV),
  grid_to_id(Grid,ID),
  make_fti(GH,GV,ID,Grid,[],[],Options,Points,Image),
  fti(Image).


individuate(H,V,ID,Grid,ContainedPoints,Image.points):-
  make_fti(H,V,ID,Grid,[],[],[complete],ContainedPoints,Image).

make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,Image):-
  Image = _{
   % Options and TODO List (are actually same things)
   todo:Options,options:Options,
   % Grid and point representations
   grid:Grid,points:Points,
   % Original copies of Grid and point representations
   o_grid:Grid,o_points:Points,
   % object found in grid and object that are reserved to not be found
   objs:Sofar,robjs:Reserved,
   % height width and lookup key for image
   h:GH,v:GV,id:ID}.






individuate_default(H,V,ID,Grid,Points,IndvSO):- individuate_complete(H,V,ID,Grid,Points,IndvSO),!.
individuate_default(H,V,ID,Grid,Points,IndvSO):- fail,
   individuate(H,V,ID,[],
 [fourway,
     shape_lib(noise),shape_lib(pair),
     fourway,
     shape_lib(in),shape_lib(out),
     solid(rectangle), 
     rectangle, 
     diamonds, 
     connect(hv_line(h)),connect(hv_line(v)),
     connect(dg_line(u)),connect(dg_line(d)),
     find_engulfed,find_contained,find_engulfed,find_contained,
     dg_line(u),dg_line(d), 
     hv_line(h),hv_line(v), 
     all, by_color, done],
  Grid,Points,IndvS),
  add_shape_info(default_indiv,IndvS,IndvSO).
individuate_default(GridIn,IndvS):- GridIn==[],!,IndvS=[].
individuate_default(GridIn,IndvS):- 
   must_be_free(IndvS),
   into_points_grid(GridIn,Points,Grid),
   grid_size(Grid,H,V), 
   into_gridname(Grid,ID),write('.'),ttyflush,
   %wdmsg(individuate_default(H,V)),
   individuate_default(H,V,ID,Grid,Points,IndvS).





individuate_default(GridIn,IndvS):- 
  individuate([complete],GridIn,IndvS).



individuate_complete(GridIn,IndvS):- GridIn==[],!,IndvS=[].
individuate_complete(GridIn,IndvS):- 
   must_be_free(IndvS),
   into_points_grid(GridIn,Points,Grid),
   grid_size(Grid,H,V), 
   into_gridname(Grid,ID),
   wdmsg(individuate_complete(H,V)),
   individuate_complete(H,V,ID,Grid,Points,IndvS).

%individuate_complete(H,V,ID,Grid,Points,IndvSO):- individuate_default(H,V,ID,Grid,Points,IndvSO).
individuate_complete(H,V,ID,Grid,Points,IndvSO):-   
   individuate(H,V,ID,[],
     [fourway,
      shape_lib(in),shape_lib(out),
      shape_lib(noise),shape_lib(pair),
      solid(rectangle), rectangle,diamonds,all,
      connect(hv_line(h)),
      connect(hv_line(v)),
      find_engulfed,
      find_contained,
               check_engulfed,
               combined_perfects,
      by_color,
      %leftover,
      done],
      Grid,Points,IndvSO),!.

individuate_complete(GridIn,IndvS):- 
  individuate([complete],GridIn,IndvS).

individuate(_Options,GridIn,IndvS):- GridIn==[],!,IndvS=[].
individuate( Options,Grid,IndvSS):- 
  listify(Options,LOptions),
  append([LOptions,complete,LOptions],UseOpts),
   fti_go(Grid,UseOpts,Image),
   _LeftOverPoints = Image.points,
   H = Image.h, V = Image.v, ID = Image.id,
   Indv_1 = Image.objs,
   wdmsg(individuate(H,V)),
   %as_debug(9,ptt((individuate=IndvSRaw))),
   unraw_inds(Indv_1,Indv_2),  
   largest_first(Indv_2,IndvSRaw),
   make_indiv_object_list(ID,H,V,IndvSRaw,IndvS1),
   combine_objects(IndvS1,IndvS),
   list_to_set(IndvS,IndvSS),
   save_grouped(individuate(ID),IndvSS).

individuate(ROptions,Grid,IndvS):-
   individuation_reserved_options(ROptions,Reserved,NewOptions),
   individuate(Reserved,NewOptions,Grid,IndvS).

individuate(Reserved,NewOptions,Points,IndvS):-  is_points_list(Points),points_to_grid(Points,Grid),!, 
   individuate(Reserved,NewOptions,Grid,IndvS).

% tiny grid becomes a series of points
individuate(GH,GV,ID,_Reserved,_NewOptions,_Grid,Points,IndvS):- is_glyphic(Points,GH,GV), !,
  individuate_glyphic(GH,GV,ID,Points,IndvS).


%individuate(H,V,ID,Grid,Points,IndvSO):- individuate_default(H,V,ID,Grid,Points,IndvSO).
expand_todo(complete,
     [fourway,
      shape_lib(intruder), shape_lib(noise),
      shape_lib(in),shape_lib(out),
      shape_lib(pair),
      solid(rectangle), polygon, rectangle,diamonds,all,
      jumps(hv_line(h,_)),
      jumps(hv_line(_,v)),
      find_engulfed,
      find_contained,
               check_engulfed,
               combined_perfects,
      by_color,
      %leftover,
      done]).

expand_todo(new_indiv,
     [
      shape_lib(noise), 
      fourway,
               solid(rectangle), 

               rectangle,

               shape_lib(pair),

/*
               dg_line(u,_),dg_line(_,d),

               hv_line(h,_),hv_line(_,v),

               merge(hv_line(h,_)),
               merge(hv_line(_,v)),


               merge(dg_line(u,_)),
               merge(dg_line(_,d)),

               connect(hv_line(h,_)),
               connect(hv_line(_,v)),
               connect(dg_line(u,_)),
               connect(dg_line(_,d)),

               jumps(hv_line(h,_)),
               jumps(hv_line(_,v)),
               jumps(dg_line(u,_)),
               jumps(dg_line(_,d)),
*/
               all,




             shape_lib(in),shape_lib(out),

      find_engulfed,
      find_contained,
               check_engulfed,
               combined_perfects,
      by_color,
      %leftover,
      done]).

expand_todo(default, 
 [fourway,
  shape_lib(noise),shape_lib(pair),
 fourway,
 shape_lib(in),shape_lib(out),
 solid(rectangle), 
 rectangle, 
 dg_line(_DG), 
 connect(hv_line(h,_)),connect(hv_line(_,v)),
 connect(dg_line(u,_)),connect(dg_line(_,d)),
 find_engulfed,find_contained,find_engulfed,find_contained,
 dg_line(u,_),dg_line(_,d), 
 hv_line(h,_),hv_line(_,v), 
 all, by_color, done, default]).

































































individuals_raw(GH,GV,ID,Options,Reserved,Points,Grid,IndvSRaw):-
 must_det_l((
  must_be_free(IndvSRaw),
  individuals_list(GH,GV,[],ID,Options,Reserved,Points,Grid,Indv_0,_LeftOverPoints),
  =(Indv_0,Indv_1),
  unraw_inds(Indv_1,Indv_2),  
  largest_first(Indv_2,IndvSRaw))).


unraw_inds(IndvS,IndvOO):-   
  largest_first(IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).

get_new_task_obj(Obj):-
   Obj = _{ original_grid: _ ,unprocessed_grid:_, original_points:_, points:_, options:_, todo:_,
     objs:_, grid_id:_, in_or_out:_, grid_size:_, reserver_indivs:_  }.

individuals_list(_GH,_GV,Sofar,_ID,_Options,_Reserved,Points,_Grid,Sofar,PointsO):- Points == [], !,PointsO=[].

individuals_list(GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
    assertion(maplist(is_cpoint,Points)),
    assertion(is_list([sofar1(Options)|Sofar])),
    assertion(maplist(nonvar_or_ci,[fsi,Grid,Points])),
    assertion(maplist(nonvar_or_ci,[fsi,Reserved,Options,Sofar,GV,GH,ID])),
     
      (((call_fsi(NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints)))),
          intersection(SofarMaybeNew,Sofar,Unchanged,New,GoneMissing),
          Options = [Head|_],
          override_object(birth(Head),New,NewUpdated),
          append(Unchanged,NewUpdated,SofarUpdated),
          ignore((GoneMissing\==[],wdmsg(goneMissing=GoneMissing))),
  assertion(maplist(nonvar_or_ci,[fsi,NewReserved,NewGrid,NewOptions,SofarUpdated,NextScanPoints])),
    assertion(maplist(is_cpoint,NextScanPoints)),
    assertion(is_list([foundSofar1(Options)|SofarUpdated])),
  assertion(maplist(nonvar_or_ci,[NewGrid])),
  assertion(maplist(nonvar_or_ci,[GH,GV,ID,NewOptions,NewReserved,NextScanPoints])),
    ( (SofarUpdated\==Sofar) ; (Options\==NewOptions) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),
    %wqnl(indv(Options)=Indv),   
    
    individuals_list(GH,GV,SofarUpdated,ID,NewOptions,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.

individuals_list(GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
    next_options(Options,Options2),
    individuals_list(GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints).

individuals_list(GH,GV,Sofar,ID,Options,_Reserved,Points,_Grid,IndvListOut,[]):- 
 must_det_l(Options==[]),
 must_be_free(IndvListOut),
  assertion(is_list([sofar2(Options)|Sofar])),
  assertion(is_list([sofar|Sofar])),
  assertion(is_list([points|Points])),
  assertion(maplist(is_cpoint,Points)),
  as_debug(9,print_grid(GH,GV,'leftover_points'+ID,Points)),
  % maplist(make_point_object(ID,GH,GV),Points,IndvList),
  IndvList = [],
  % individuate([],[just(by_color([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown)]))],Points,IndvList2),
  IndvList2 = [],
  % make_indiv_object(ID,GH,GV,Points,[object_shape(combined),object_shape(leftovers)],LeftOverObj), 
  LeftOverObj = [],
  flatten([Sofar,IndvList,IndvList2,LeftOverObj],IndvListOut),!.

individuals_list(GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P):-!,
  throw(fail(individuals_list(GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P))),!.


/*
goal_expansion(Goal,Out):- compound(Goal), arg(N,Goal,E), 
   compound(E), E = set(Obj,Member), setarg(N,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(_,Cmpd,_):- \+ compound(Cmpd),!, fail.
get_setarg_p1(E,Cmpd,SA):- arg(N,Cmpd,E), SA=setarg(N,Cmpd).
get_setarg_p1(E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(E,Arg,SA).

system:term_expansion((Head:-Body),Out):-
   get_setarg_p1(I,Head,P1), compound(I), I = set(Obj,Member),
   call(P1,Var),
   expand_term((Head:- b_set_dict(Member,Obj,Var), Body),Out).

system:goal_expansion(Goal,Out):-
   get_setarg_p1(I,Goal,P1), compound(I), I = set(Obj,Member),
   call(P1,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).

/*
goal_expansion(Goal,'.'(Image, Objs, Obj)):-
  Goal = ('.'(Image, Objs, A), Obj = V), 
  var(Obj).
*/

call_fsi(NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints):-
  make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,Image),  
  fti(Image,Options),!,
  NewReserved = Image.robjs,
  NewOptions = Image.todo,
  NewGrid = Image.grid,
  NextScanPoints = Image.points,
  SofarMaybeNew = Image.objs.


  

fti(Image):- fti(Image,Image.todo).

fti(_,[]).
fti(Image,_):- Image.points=[], !.
fti(_,[done|_]):- !.

fti(Image,[progress|set(Image,todo)]):-
  as_debug(1,(mass(Image.objs,Mass),length(Image.objs,Count),pt(t([fsi=Image.todo,sofar=Mass/Count])))).

% Find free points that are contained in objects and individuate them in their own way
fti(Image,[find_contained|set(Image,todo)]):-
  find_contained(Image.h,Image.v,Image.id,Image.objs,set(Image,objs),Image.points,set(Image,points)),!.

    find_contained(_H,_V,_ID,Sofar,Sofar,[],[]).
    find_contained(_H,_V,_ID,[],[],NextScanPoints,NextScanPoints).
    find_contained(H,V,ID,[Found|Sofar],[Found|SofarInsteadM],NextScanPoints,NextScanPointsInstead):-
      iz(Found,outline(_)),
      once(find_contained_points(Found,NextScanPoints,ScanPointsInstead,ContainedPoints)),
      ContainedPoints\==[],
      %grid_size(Found,H,V),
      must_det_l((
      points_to_grid(H,V,ContainedPoints,Grid),
      %once(object_indv_id(Found,ID,_);into_gridname(Grid,ID)),
      individuate_complete(H,V,ID,Grid,ContainedPoints,NewInside),
      maplist(mention_inside(Found),NewInside,NewInsideM))),
      ignore((length(ContainedPoints,N),N>1,quietly(print_grid(H,V,[Found|NewInsideM])))),
      find_contained(H,V,ID,Sofar,SofarInstead,ScanPointsInstead,NextScanPointsInstead),
      append(NewInsideM,SofarInstead,SofarInsteadM).
    find_contained(H,V,ID,[Found|Sofar],[Found|SofarInstead],NextScanPoints,NextScanPointsInstead):-
      find_contained(H,V,ID,Sofar,SofarInstead,NextScanPoints,NextScanPointsInstead).
    
    
    mention_inside(Found,NewInside,NewInsideO):-
      object_indv_id(Found,_Where,Iv),
      add_shape_info(insideOf(Iv),NewInside,NewInsideO).
    
    find_contained_points(_,[],[],[]).
    find_contained_points(Found,[Next|ScanPoints],ScanPointsInstead,[Next|Contained]):-
     object_surrounds_point(Found,Next),
     find_contained_points(Found,ScanPoints,ScanPointsInstead,Contained).
    find_contained_points(Found,[Next|ScanPoints],[Next|ScanPointsInstead],Contained):-
     find_contained_points(Found,ScanPoints,ScanPointsInstead,Contained).


    object_surrounds_point(Obj,_-Point):- point_in_obj_view(Point,Obj), 
      globalpoints(Obj,ObjPoints),!,
      forall((is_adjacent_point(Point,Dir,Next),Dir\==c),scan_to_colider1(Obj,Next,Dir,ObjPoints,_DirHits)).
    
    point_in_obj_view(Next,Obj):- 
      hv_point(H,V,Next),
      loc_xy(Obj,X,Y),!,
      VV is V-Y, VV>=0,
      HH is H - X, HH>=0,
      vis_hv(Obj,XX,YY),!,
      VV<YY, HH<XX.
    
    scan_to_colider1(_Obj,Next,_Dir,_ObjPoints,[]):- hv_point(H,V,Next), (\+ between(1,32,H); \+ between(1,32,V)),!.
    scan_to_colider1(_Obj,Next,_Dir,ObjPoints,[C-Next]):- 
      select(C-Next,ObjPoints,_Rest),!.
    scan_to_colider1(Obj,Next,Dir,ObjPoints,DirHits):- 
      is_adjacent_point(Next,Dir,NNext),!,
      scan_to_colider1(Obj,NNext,Dir,ObjPoints,DirHits),!.
    
    scan_to_colider(Obj,Next,_Dir,_ObjPoints,[]):- \+ point_in_obj_view(Next,Obj),!.
    scan_to_colider(Obj,Next,Dir,ObjPoints,[C-Next|DirHits]):- 
      select(C-Next,ObjPoints,Rest),!,
      is_adjacent_point(Next,Dir,NNext),
      scan_to_colider(Obj,NNext,Dir,Rest,DirHits).
    scan_to_colider(Obj,Next,Dir,ObjPoints,DirHits):- 
      is_adjacent_point(Next,Dir,NNext),
      scan_to_colider(Obj,NNext,Dir,ObjPoints,DirHits).
  

% Find object that are contained in objects and individuate them in their own way  (TODO mame this more complete)
% Find free points that are contained in objects and individuate them in their own way
fti(Image,[find_engulfed|set(Image,todo)]):-
  find_engulfed(Image).   find_engulfed(Image):- find_engulfed(Image,Image.objs,set(Image,objs)),!.
    
find_engulfed(ScanNext,SofarInsteadO):-
  find_engulfed(ScanNext,ScanNext,SofarInsteadO).

find_engulfed([],SofarInsteadO,SofarInsteadO).
find_engulfed([Found|ScanNext],OtherObjects,OtherObjectsO):-
 % (iz(Found,outline(_));iz(Found,outl)) ->       
  once(find_englufed_objects(Found,OtherObjects,NewInside)),
  NewInside\==[],
  must_det_l((
  maplist(mention_inside(Found),NewInside,NewInsideM),
  replace_i_each(OtherObjects,NewInside,NewInsideM,NewOtherObjects),
  replace_i_each(ScanNext,NewInside,NewInsideM,NewScanNext),
  ignore((length(NewInside,N),N>0,quietly(print_grid([Found|NewInsideM])))),      
  find_engulfed(NewScanNext,NewOtherObjects,OtherObjectsO))).

find_engulfed([_|Sofar],OtherObjects,OtherObjectsO):-
  find_engulfed(Sofar,OtherObjects,OtherObjectsO).


find_englufed_objects(_,[],[]).
find_englufed_objects(Found,[Next|ScanPoints],[Next|Engulfed]):-
 contained_object(Found,Next),
 find_englufed_objects(Found,ScanPoints,Engulfed).
find_englufed_objects(Found,[_|ScanPoints],Engulfed):-
 find_englufed_objects(Found,ScanPoints,Engulfed).

contained_object(O1,O2):- 
  O1 \== O2,
  loc_xy(O1,LowH1,LowV1),loc_xy(O2,LowH2,LowV2), 
  LowH2>=LowH1, LowV2>=LowV1,
  vis_hv(O1,H1,V1),vis_hv(O2,H2,V2), 
  H1>=H2, V1>=V2,
  HighH1 is LowH1+H1, HighV1 is LowV1+V1,
  HighH2 is LowH2+H2, HighV2 is LowV2+V2,
  HighH1 >= HighH2,
  HighV1 >= HighV2,
  globalpoints(O2,[Point]),!,
  object_surrounds_point(O1,Point).

    
replace_i_each(OtherObjects,[I|NewInside],[O|NewInsideM],NewOtherObjects):-!,
  subst(OtherObjects,I,O,OtherObjectsM),
  replace_i_each(OtherObjectsM,NewInside,NewInsideM,NewOtherObjects).
replace_i_each(E,[],[],E).

fti(Image,[combine_objects|set(Image,todo)]):-
   combine_objects(Image.objs,set(Image,objs)),!.

fti(Image,[combine_perfects|set(Image,todo)]):-
   combine_perfects(Image.objs,set(Image,objs)),!.

fti(Image,[check_engulfed|TODO]):-
   smallest_first(Image.objs,SmallestFirst),
   set(Image,objs) = SmallestFirst,
   set(Image,todo) = [find_engulfed,TODO].

fti(Image,[-(DelOptions)|TODO]):-
  listify(DelOptions,OList),
  my_partition(option_matches(OList),Image.options,_,set(Image,options)),
  my_partition(option_matches(OList),TODO,_,set(Image,todo)).

option_matches(List,Arg):- member(E,List),E==Arg.

fti(Image,[+(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  append(Image.options,OptionsL,set(Image,options)),
  append(TODO,OptionsL,set(Image,todo)).

fti(Image,[(OptionsL)|TODO]):-
  is_list(OptionsL), \+ is_group(OptionsL), \+ is_grid(OptionsL),!,
  append(Image.options,OptionsL,set(Image,options)),
  append(TODO,OptionsL,set(Image,todo)).

fti(Image,[options(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  append(Image.options,OptionsL,set(Image,options)),
  append(TODO,OptionsL,set(Image,todo)).


fti(Image,[DO|TODO]):-
  fsi(Image,set(Image,robjs),set(Image,grid),set(Image,todo),
    Image.h,Image.v,Image.objs,Image.id,[DO|TODO],
    Image.robjs,Image.points,Image.grid,set(Image,objs),set(Image,points)).

fsi(_Image,ReservedIO,Grid,TODO,H,V,Sofar,ID,[retain_grid(Options)|TODO],ReservedIO,PointsIO,Grid,IndvList,PointsIO):-
  listify(Options,OptionsL),!,
  must_det_l(individuals_list(H,V,Sofar,ID,OptionsL,ReservedIO,PointsIO,Grid,IndvList,_LeftOver)),
  pt(yellow,PointsIO).

/*
fti(Image,[by_color(Options)|TODO]):-
  listify(AddOptions,OptionsL),
  append(Image.options,OptionsL,set(Image,options)),
  append(TODO,OptionsL,set(Image,todo)).
*/

fsi(_Image,ReservedIO,Grid,[by_color(Rest)|TODO],H,V,Sofar,ID,[by_color(Options)|TODO],ReservedIO,Points,Grid,NewSofar,NextScanPoints):-
  listify(Options,OptionsL),!,
  select(C,OptionsL,Rest),
  my_partition(=(C-_),Points,ThisGroup,NextScanPoints),
  ((ThisGroup\==[],make_indiv_object(ID,H,V,ThisGroup,[object_shape(by_color(C))],ColorObj))
    -> NewSofar = [ColorObj|Sofar] 
     ; NewSofar = Sofar).


% dots that have no adjacent points of the same color are gathered first
fsi(_Image,Reserved,Grid,['dots'|TODO],H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,[Indv|Sofar],Rest):-
   %( (H < 6; V < 6) ;   
   maybe_multivar(C),
    select(C-HV,Points,Rest),  \+ free_cell(C),
   \+ \+ (( is_adjacent_point(HV,_,HV2),
           member(C2-HV2,Rest),ok_color_with(C,C2))),

   make_indiv_object(ID,H,V,[object_shape(dots),C-HV],Indv),

   meets_indiv_criteria(dots,Indv),!.

% dots may have adjacent points of the same color (because we are in 'dots' mode)
fsi(_Image,Reserved,Grid,['dots'|TODO],H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,[Indv|Sofar],Rest):-
   maybe_multivar(C), select(C-HV,Points,Rest),  \+ free_cell(C),
   make_indiv_object(ID,H,V,[object_shape(dots),C-HV],Indv),
   meets_indiv_criteria(dots,Indv),!.


fsi(Image,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[full|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-!,
  grid_to_individual(Grid,Obj),
  fsi(Image,ReservedO,GridO,OptionsOut,H,V,[Obj|Sofar],ID,TODO,ReservedI,Points,Grid,SofarOut,NextScanPoints).


fsi(_Image,ReservedO,GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-!,
  (do_shapelib(ReservedO,GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints)).


fsi(_Image,StillReserved,GridO,[use_reserved|TODO],H,V,Sofar,ID,[use_reserved|TODO],Reserved,Points,Grid,SofarOut,NextScanPoints):-
   % length(Reserved,LR), !, %LR < 60, 
   proccess_overlap_reserved(use_reserved,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,StillReserved),
   %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,Obj+ID,Found)),
   !.

fsi(_Image,ReservedO,GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-
  search_lib(ReservedO,GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints),
  %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,Obj+ID,Found)),
  !.
  
search_lib([Obj|ReservedI],GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-
   is_group(Obj), Reserved = Obj, !, proccess_overlap_reserved(is_group,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,_StillReserved).
   
search_lib([Obj|ReservedI],GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-
   is_object(Obj), Reserved = [Obj], !, proccess_overlap_reserved(is_object,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,_StillReserved).

search_lib([Obj|ReservedI],GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-
   is_grid(Obj), Reserved = [Obj], !, proccess_overlap_reserved(is_grid,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,_StillReserved).


do_shapelib(ReservedIO,GridO,[shape_lib(Hammer)|TODO],H,V,Sofar,ID,[shape_lib(Hammer)|TODO],ReservedIO,Points,Grid,SofarOut,NextScanPoints):-
   (shape_lib_expanded(Hammer,Reserved)), Reserved\==0,
   length(Reserved,RL),
   pt(searchLib(Hammer)=RL),
   %debug_indiv(Reserved),
   proccess_overlap_reserved(Hammer,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,_StillReserved),
   %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,'shape_lib'+ID,Found)),
   !.

proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,[Obj|RestReserved],Sofar,SofarOut,Points,NextScanPoints,[Obj|Unreserved],StillReserved):-     
   %ignore((length(RestReserved,RL),1 is RL mod 7, pt(searchLib(Name)=RL))),
   % Points\==[],
  \+ color(Obj,black),
   object_grid(Obj,OGrid),
   ogs(OH,OV,OGrid,Grid),
   %must_det_l
   ((
   localpoints(Obj,OPoints),
   offset_points(OH,OV,OPoints,ObjPoints),
   intersection(ObjPoints,Points,Intersected,LeftOverA,LeftOverB),
   do_leftover(Sofar,LeftOverA,Intersected,Use,Sofar2),
   append(Intersected,Use,All),
   list_to_set(All,AllS))), AllS \== [],
   make_indiv_object(ID,H,V,[object_shape(copy(Name))|AllS],Indiv0), 
   object_indv_id(Obj,_,Iv), override_object(object_indv_id(ID,Iv),Indiv0,Indiv), 
   %make_indiv_object(ID,H,V,Use,Indiv),
   points_to_grid(H,V,LeftOverB,NewGrid),
   %print_grid(Indiv),
   append(Sofar2,[Indiv],NewSofar),
   proccess_overlap_reserved(Name,GridO,NewGrid,ID,H,V,[Obj|RestReserved],NewSofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved),
   nop(debug_indiv(Indiv)).
  
proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,[Obj|Reserved],Sofar,SofarOut,Points,NextScanPoints,[Obj|Unreserved],StillReserved):- fail,
   once((globalpoints(Obj,ObjPoints), 
   \+ color(Obj,black),
   intersection(ObjPoints,Points,Intersected,LeftOverA,LeftOverB),
         do_leftover(Sofar,LeftOverA,Intersected,Use,Sofar2),
         append(Intersected,Use,All),
         list_to_set(All,AllS))), AllS \== [],
         make_indiv_object(ID,H,V,[object_shape(override(Name))|AllS],Indiv0), 
         object_indv_id(Obj,_,Iv), override_object(object_indv_id(ID,Iv),Indiv0,Indiv), 
         %make_indiv_object(ID,H,V,Use,Indiv),
         append(Sofar2,[Indiv],NewSofar),
         proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,Reserved,NewSofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).

proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,[Obj|Reserved],Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,[Obj|StillReserved]):-
   proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).
proccess_overlap_reserved(_Name,Grid,Grid,_ID,_H,_V,[],Sofar,Sofar,NextScanPoints,NextScanPoints,[],[]).

do_leftover(Sofar,[],Intersected,Intersected,Sofar):- !.
%do_leftover([],_,_,_,_):- !,fail.
do_leftover(Sofar,LeftOverA,Intersected,Use,Removed):- select(S,Sofar,Removed), globalpoints(S,SPoints),
    intersection(SPoints,LeftOverA,Usable,[],[]),append(Usable,Intersected,Use).


append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).

% ===============
% release_points
% ===============
fsi(_Image,Reserved,NewGrid,TODO,H,V,Sofar,ID,[release_points|TODO],Reserved,Points,Grid,Sofar,NextScanPoints):-
    globalpoints(Grid,NextScanPoints1),
    globalpoints(Sofar,NextScanPoints2),
    append_sets([Points,NextScanPoints2,NextScanPoints1],NextScanPoints),
    (as_debug(9,print_grid(H,V,'release_points(Sofar)'+ID,Sofar))),
    points_to_grid(H,V,NextScanPoints,NewGrid), 
  !.

fsi(_Image,Reserved,Grid,Expansion,_H,_V,Sofar,_ID,IsToBeRewritten,Reserved,Points,Grid,Sofar,Points):-
    get_option_expansion(IsToBeRewritten,Expansion),!.

fsi(_Image,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[retain(_)|TODO],Reserved,Points,Grid,Sofar,Points):-  
  Sofar = [], !.

fsi(_Image,Reserved,GridO,TODO,_H,_V,Sofar,_ID,[with_grid(Call)|TODO],Reserved,Points,Grid,Sofar,Points):-
    call(Call,Grid,GridO).
fsi(_Image,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[with_points(Call)|TODO],Reserved,Points,Grid,Sofar,PointsO):-
    call(Call,Points,PointsO).
fsi(_Image,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[with_sofar(Call)|TODO],Reserved,Points,Grid,SofarO,Points):-
    call(Call,Sofar,SofarO).

fsi(_Image,Reserved,NNewGrid,TODO,H,V,Sofar,ID,[retain(Option)|TODO],Reserved,_Points,Grid,Retained,NextScanPoints):-
    globalpoints(Grid,NewGPoints),  H> 14, V> 14,
    freeze(W,W>5),filter_indivs(Sofar,[mass(W)];[object_shape(Option)],Retained1),
    filter_indivs(Retained1, \+ object_shape(background),Retained),
    as_debug(9,print_grid(H,V,'retained'+ID,Retained)),    
    remove_global_points(Retained,NewGPoints,NextScanPoints),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    %as_debug(9,print_grid(H,V,'newgrid'+ID,NNewGrid)),
    !.

fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,[into_single_hidden|TODO],Reserved,Points,Grid,SofarIndvS1,Points):- !, 
    maplist(globalpoints,Sofar,IndvPointSofar), append_sets(IndvPointSofar,IndvPointsL),
    EIndvPoints=[object_shape(combined),object_shape(hidden)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    append(Sofar,[Indv],SofarIndvS1),
    meets_indiv_criteria(into_single_hidden,IndvPointsL),!.


fsi(_Image,Reserved,NewGrid,TODO,H,V,Sofar,_ID,[ignore_rest|TODO],Reserved,_Points,_Grid,Sofar,[]):- !, 
    make_grid(H,V,NewGrid).

fsi(_Image,Reserved,Grid,[This,ignore_rest,done|TODO],_H,_V,Sofar,_ID,[just(This)|TODO],Reserved,Points,Grid,Sofar,Points):- !.


fsi(_Image,Reserved,Grid,NewOptions,_H,_V,Sofar,_ID,[by_color|TODO], Reserved, Points, Grid,Sofar, Points):- !,
   append(
     [(by_color([(black), (blue),  (red),   (green),(yellow),
                     (silver),(purple),(orange),(cyan), (brown)]))],TODO,NewOptions).


fsi(_Image,Reserved,Grid,TODO,GH,GV,Sofar,ID,[leftover|TODO], Reserved, Points, Grid,SofarIndvList, []):- !,
   make_indiv_object(ID,GH,GV,Points,[object_shape(combined),object_shape(leftover)],LeftOverObj), 
   append(Sofar,[LeftOverObj],SofarIndvList).


fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,[into_single|TODO],Reserved,Points,Grid,[Indv],Points):- !,
    maplist(globalpoints,Sofar,IndvPoints), append(IndvPoints,IndvPointsL),
    EIndvPoints=[object_shape(combined),object_shape(into_single)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    meets_indiv_criteria(into_single_hidden,IndvPoints),!.

%  fsi(_Image,NewReserved,NewGrid,NewOptions,H,V,Sofar,ID,TODO,Reserved,Points,Grid,OutInvdivS,NextScanPoints).

fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,['fourway'|TODO],Reserved,Points,Grid,OutInvdivS,NextScanPoints):-
     H > 13, V> 13,
    grid_to_3x3_objs(Grid,FourWay1s),
    overwrite_use_so_far(FourWay1s,Sofar,UseSofar),
    append(UseSofar,FourWay1s,OutInvdivS),
    as_debug(9,writeln('fourway-found')),    
    %OutInvdivS=[E|_],print_grid(E),
    as_debug(8,print_grid(H,V,'4-way'+ID,FourWay1s)),
    remove_global_points([UseSofar,FourWay1s],Points,NextScanPoints),!.


overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_l((remove_global_points(FourWay1s,Sofar,Sofar1),add_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).

fsi(_Image,Reserved,NewGrid,TODO,H,V,Sofar,_ID,['regroup'|TODO],Reserved,Points,Grid,OutInvdivS,Points):- 
  make_indiv_object_list(Grid,H,V,Sofar,OutInvdivS), 
  points_to_grid(H,V,Points,NewGrid).


fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,[solid(squares)|TODO],Reserved,_Points,Grid,AllOUT,NextScanPoints):- fail, !,
  globalpoints(Grid,NewGPoints),
  grid_to_segs(Grid,Segs),
  seg_squares(Segs,Squares),
  segs_to_pointlists(Squares,Objs),  
  maplist(make_indiv_object_list(ID,H,V),Objs,OutInvdivS),
  %maplist(print_grid,OutInvdivS),
  as_debug(8,print_grid(H,V,'Solid-boxes'+ID,OutInvdivS)),!,
  remove_global_points(OutInvdivS,NewGPoints,NextScanPoints),
  append(Sofar,OutInvdivS,AllOUT).

    grid_to_segs(Grid,Segs):- must_det_l((grid_to_segs(1,Grid,SegsL),append(SegsL,Segs))).
    
    grid_to_segs(N,[Row|Grid],[SegRS|SegL]):- 
      slice_up_row(N,1,Row,SegRS),
      N2 is N +1, 
      grid_to_segs(N2,Grid,SegL).
    grid_to_segs(_,[],[]).
    
    slice_up_row(_,_,[],[]).
    slice_up_row(R,N,[C|Rest],[slice(C,N,R,N2,R)|More]):-
      get_until_not(C,1,Rest,Len,Right), N2 is N + Len -1, slice_up_row(R,N2,Right,More).
    
    get_until_not(C,N,[C2|Rest],NL,Right):- C==C2, N2 is N+1,!, 
     get_until_not(C,N2,Rest,NL,Right).
    get_until_not(_,N,Rest,N,Rest).
    
    nothing_in_range(C,Ch,Cv,CH,CV,Rest):- 
      forall(member(slice(C,Ah,Av,AH,AV),Rest), 
        \+ overlaps(Ch,Cv,CH,CV,Ah,Av,AH,AV)).
    
    overlaps(Ch,Cv,CH,CV,Ah,Av,AH,AV):- 
       AH>=Ch,Ah=<CH,AV>=Cv,Av=<CV.
    
    seg_squares(Segs,Squares):- 
        select(slice(C,Ah,Av,AH,AV),Segs,Rest),
      Bv is AV + 1,
        select(slice(C,Ah,Bv,AH,BV),Rest,Rest2),
      Cv is Av - 1,
      nothing_in_range(C,Ah,Cv,AH,Cv,Rest2),
      seg_squares([slice(Ah,Av,AH,BV)|Rest2],Squares).
    seg_squares(Segs,Segs).
    
    must_be_nonvar(X):- assertion(nonvar_or_ci(X)),!.
    
    
    make_points_list(C,MinH,MinV,MaxH,MaxV,PointsList):-
       findall(C-HV,(between(MinV,MaxV,V),between(MinH,MaxH,H),hv_point(H,V,HV)),PointsList),!.
    
    segs_to_pointlists([],[]).
    segs_to_pointlists([slice(C,Ah,Av,AH,AV)|Segs],[PointsList|Objs]):- % trace,
      H is AH-Ah, V is AV-Av, H>=2,V>=2,
      ( H==V -> Shape = square ; Shape = rectangle),
      make_points_list(C,Ah,Av,AH,AV,PointsListH),
      PointsList=[object_shape(Shape),object_shape(solid(Shape))|PointsListH],  !,
      segs_to_pointlists(Segs,Objs).
      
    segs_to_pointlists([_|Segs],Objs):-   segs_to_pointlists(Segs,Objs).

fsi(_Image,Reserved,Grid,[connect(ShapeType)|TODO],H,V,Sofar,ID,[connect(ShapeType)|TODO],Reserved,Points,Grid,NewSofar,RestPoints):- !,
  connect_skips(H,V,ID,Sofar,ShapeType,Grid,Points,NewSofar,RestPoints).


connect_skips(H,V,ID,Sofar,ShapeType,Grid,Points,NewSofar,RestPoints):-
  shape_type_dir(ShapeType,[Dir|_]),
  %filter_indivs(Sofar,object_shape(ShapeType), Found),
  Sofar=Found,
  select(HV1,Found,Found1), 
  select(HV2,Found1,SofarLess), 
  %first_gpoint(HV1,C-P1), last_gpoint(HV2,C-P2),
  %iz(HV1,ShapeType),iz(HV2,ShapeType),
  any_gpoint(HV1,C-P1), any_gpoint(HV2,C-P2),
  % skip over MP
  is_adjacent_point(P0,Dir,P1), is_adjacent_point(P1,Dir,MP), is_adjacent_point(MP,Dir,P2),is_adjacent_point(P2,Dir,P3),
  any_gpoint(HV1,C-P0), any_gpoint(HV2,C-P3),
  % TODO: HACK WE MIGHT NOT WANT TO STEAL THE POINT? 
  once(select(MC=MP,Points,RestPoints);Points=RestPoints),
  ignore(once((get_color_at(Points,Grid,MC),is_color(MC));MC=C)),
  combine_2objs(ID,H,V,HV1,HV2,[MC-MP],[object_shape(ShapeType)],Combined),
  append(SofarLess,[Combined],NewSofar).

    combine_2objs(ID,H,V,HV1,HV2,NewPoints,IPROPS,Combined):-
      globalpoints(HV1,GP1), globalpoints(HV2,GP2),      
      indv_props(HV1,Props1),indv_props(HV2,Props2),
      append([GP1,GP2,NewPoints],GPoints), append([Props1,Props2,IPROPS],Info),
      make_indiv_object(ID,H,V,GPoints,Info,Combined).
      


fsi(_Image,FinalReserve,Grid,TODO,H,V,Sofar,ID,[cycle_shapes(Shapes)|TODO],Reserved,Points,Grid,IndvList,LeftOver):- 
   cycle_s(FinalReserve1,H,V,Sofar,ID,Shapes,Reserved,Points,Grid,IndvList1,LeftOver1),
   cycle_s(FinalReserve,H,V,IndvList1,ID,Shapes,FinalReserve1,LeftOver1,Grid,IndvList,LeftOver).

    cycle_s(FinalReserve,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
        assertion(maplist(is_cpoint,Points)),
        assertion(is_list([sofar1(Options)|Sofar])),
        fsi(_Image,NewReserved,NewGrid,TODO,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,FoundSofar,NextScanPoints),
        assertion(maplist(nonvar_or_ci,[fsi,NewReserved,NewGrid,TODO,FoundSofar,NextScanPoints])),
        assertion(maplist(is_cpoint,NextScanPoints)),
        assertion(is_list([foundSofar1(Options)|FoundSofar])),
        ((FoundSofar\==Sofar) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),
        cycle_s(FinalReserve,GH,GV,FoundSofar,ID,TODO,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.
    cycle_s(FinalReserve,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
        next_options(Options,Options2),!,
        cycle_s(FinalReserve,GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints).
    cycle_s(Reserved,_GH,_GV,Sofar,_ID,_Options,Reserved,Points,_Grid,Sofar,Points).

fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
   ( Option \==dots), 
   find_one_individual(H,V,Sofar,ID,Option,Reserved,Points,Grid,IndvPoints,NextScanPoints),
   make_indiv_object(ID,H,V,[birth(Option),object_shape(Option)|IndvPoints],Indv),!,
   append(Sofar,[Indv],OUT).

%fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
%  nop(fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints)).


default_i_options([
  shape_lib(noise),
  shape_lib(in),
  shape_lib(pair),
  shape_lib(out),
  use_reserved,
  fourway,
  solid(rectangle),
  outlines,
  %polygons,
  %shape_lib(rectangle), 
  %shape_lib(all),
  %shape_lib(hammer),
  rectangle, diamonds, all,
  
  
  %
  %
  % polygs,
  %hv_line(v), hv_line(h),
  %dg_line(u),dg_line(d),
  %CS,
  all,
  
  connect(hv_line(h)),
  connect(hv_line(v)),  
  % line(_),dg_line(_),
  % release_points, all,
  %into_single_hidden,oldway
  %retain(solid(rectangle)),
   % shapes,
  %into_single_hidden,
  all,
  done
  ]). % :- get_cycle_shapes(CS).

get_cycle_shapes(
cycle_shapes([
])).

%tiny_i_options([call(set_bgc(zero)),dots,line(_),all,into_single_hidden]).

next_options([_|T],T).


%select_default_i_options(_Grid,H,V,_Points,Options):- (H=<5;V=<5),!,tiny_i_options(Options).
%select_default_i_options(_Grid,H,V,_Points,Options):- (H=<6;V=<6),!,tiny_i_options(Options).
select_default_i_options(_Grid,_H,_V,_Points,Options):-  default_i_options(Options).


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
find_one_in dividual(GH,GV,Sofar,ID,solid(Option),Reserved,Points,Grid,[object_shape(solid(Option))|Indv],NextScanPoints):- fail, !,
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

find_one_individual(_GH,_GV,_Sofar,_ID,Option,_Reserved,Points,_Grid,IndvPoints,NextScanPoints):-
    maybe_multivar(C), select(C-HV,Points,Rest), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
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
shape_has_filtered_use(C,[_],_):- shape_filter(C,rectangle),!.


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
check_minsize(Sz,[I|IndvS],[A,B|IndvSO]):- mass(I,2),globalpoints(I,[A,B]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[A,B,C|IndvSO]):- mass(I,3),globalpoints(I,[A,B,C]),!,check_minsize(Sz,IndvS,IndvSO).
check_minsize(Sz,[I|IndvS],[I|IndvSO]):- check_minsize(Sz,IndvS,IndvSO).

meets_size(_,Points):- mass(Points,1).
meets_size(_,Points):- mass(Points,2),!,fail.
meets_size(_,Points):- mass(Points,3),!,fail.
meets_size(_,Points):- mass(Points,4).
meets_size(Len,Points):- mass(Points,L),!,L>=Len.

remove_bgs(IndvS,IndvL,BGIndvS):- partition(is_bg_indiv,IndvS,BGIndvS,IndvL).


% oclass_piority(Class,Priority).
oclass_piority(image,4).
oclass_piority(combined,3).
oclass_piority(hidden,2).
%object_priority(Indv,Priority):- oclass_piority(Class,Priority), iz(Indv,Class).
object_priority(_,0).
  
smallest_first(IndvS,IndvO):-
  findall((Priority+Size)-Indv,(member(Indv,IndvS),object_priority(Indv,Priority),mass(Indv,Size)),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO).

largest_first(IndvS,IndvR):-  
  smallest_first(IndvS,IndvO),
  reverse(IndvO,IndvR).

largest_first_nonbg(IndvS,IndvOB):-  
  largest_first(IndvS,IndvO),
  remove_bgs(IndvO,IndvL,BGIndvS),
  append(IndvL,BGIndvS,IndvOB).

finish_grp(C,Grp,Point2,Dir,Rest,NewGroup,RRest):- 
   \+ (is_diag(Dir),is_bg_color(C)),
   is_adjacent_point(Point2,Dir,Point3),
   single_point(C-Point3,Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).


single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  single_point0(C-Point,IndvS,Rest1).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(obj(I),IndvS,Rest1), fail, % round 2
  globalpoints(obj(I),[C-Point]),
  nonvar_or_ci(C).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select([C-Point],IndvS,Rest1),
  nonvar_or_ci(C).

single_point0(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(C-Point,IndvS,Rest1),
  nonvar_or_ci(C).


/*
unraw_inds2(Options,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv),
   reverse(Indv,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Options,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(Options,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,object_shape(diagonal),C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  % minimum 4 findall(C-CP,member(C-CP,NewGroup),LL),LL=[_,_,_,_|_],
  unraw_inds2(Options,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(Options,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,object_shape(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Options,[NewGroup|RRestO],IndvO).



unraw_inds2(Options,IndvS,IndvO):-  %fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),
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

%same_object(D)
merge_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  object_indv_id(B,ID,Iv),
  setq(A,object_indv_id(ID,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (pt(same_object(GlyphA,GlyphB,How))))).

:- fixup_exports.


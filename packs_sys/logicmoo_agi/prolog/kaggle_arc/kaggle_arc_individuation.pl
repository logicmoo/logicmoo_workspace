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

individuation_macros(second_pass, [shape_lib(noise),solid(rectangle), rectangle, shape_lib(pair),
/*             connects(hv_line(h)),connects(hv_line(v)),connects(dg_line(u)),connects(dg_line(d)),
               dg_line(u),dg_line(d),hv_line(h),hv_line(v),merge(hv_line(h)),merge(hv_line(v)),merge(dg_line(u)),merge(dg_line(d)),
               jumps(hv_line(h)),jumps(hv_line(v)),jumps(dg_line(u)),jumps(dg_line(d)),
*/all, shape_lib(in),shape_lib(out),find_engulfed,colormass_subshapes,check_engulfed,combine_duplicates, by_color, 
  done]).   
individuation_macros(defaults2, [fourway, shape_lib(noise),shape_lib(pair),shape_lib(intruder),
 shape_lib(in),shape_lib(out),
 solid(rectangle),
 dg_line(_), %rectangle,  
 connects(hv_line(h)),connects(hv_line(v)),
 connects(dg_line(u)),connects(dg_line(d)),
 find_engulfed,colormass_subshapes,find_engulfed,colormass_subshapes,
 dg_line(u),dg_line(d), 
 hv_line(h),hv_line(v), solid(rectangle), 
 all, by_color, done, defaults]).


individuation_macros(by_color, X):-
   findall(by_color(Color),enum_colors(Color),X).

individuation_macros(subshape_in_object, [
   subshape_main,
   %progress,
   rectangle, % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   by_color, % any after this wont find individuals unless this is commented out
   done % hopefully is never ran outside subshape_in_object !
   ]).

% never add done to macros
individuation_macros(subshape_main, [
   shape_lib(hammer), % is a sanity test/hack
   dg_line(d), dg_line(u), 
   hv_line(h), hv_line(v), 
   jumps,% run the "jumps" macro
   merges(Z,Z) % merge lines into square
   ]).

individuation_macros(jumps,
  [ %progress, 
    jumps(hv_line(h)), % multicolored lines
    jumps(hv_line(v)),
    jumps(dg_line(d)), % multicolored diagonals
    jumps(dg_line(u)), 
    jumps(jumps(hv_line(v))),
    jumps(jumps(hv_line(h))) % joined jumps
    ]).  

individuation_macros(std_shape_lib, [

    shape_lib(noise), % data found we needed to delay analysis on
    shape_lib(intruder), % anything that stood out in the past 
                     shape_lib(added), 
                     shape_lib(removed), 
                     shape_lib(in), 
    shape_lib(out), % random objects learned from other passes
    shape_lib(pair), % random objects learned from this pass
    shape_lib(hammer), % cheater objects for hammer sanity test/hack
    shape_lib(l_shape), % random objects shown by douglas to look for
    shape_lib(human)]). % random objects shown by humans to look for

individuation_macros(complete, [  % progress,
    std_shape_lib, % stuff that was learned/shown previously
    +max_learn_objects(colormass,20),
    +max_iterate(colormass,20),
    colormass,
    %colormass, % blobs of any shape that are the same color
    fourway, % find fold patterns 
    colormass_subshapes, % subdivide the color masses .. for example a square with a dot on it
    subshape_main, % macro for sharing code with "subshape_in_object"
    connects(jumps(X),jumps(X)), % connected jumps    
    merges(Z,Z), % merge objects of identical types (horizontal lines become solid squares)
    find_contained, % mark any "completely contained points"
    find_engulfed, % objects the toplevel subshapes detector found but neglacted containment on 
    combine_duplicates, % make sure any objects are perfectly the same part of the image are combined
    all,
    dots, % any after this wont find individuals unless this is commented out
    leftover_as_one, % any after this wont find individuals unless this is commented out
    done]).

individuation_macros(defaults, [ complete ]).

individuation_macros(unused, [
  detect(_Image_), % makes an image detectable
  detect(_Group_), % makes several objects and images availble 
  done, %terminates object detection
  ls, % shows current director contents
  progress, % show maroexpansion process
  shape_lib(cheat), % library of all shapes (for debugging)
  shape_lib(in), % objects that got removed in this pair
  use_reserved, % objects that were already found dont find again
  - progress, % turn off a detector option
  + progress, % turn on a detector option
  solid(rectangle), % chat that looks for solid rectangles
  
  %polygons,%shape_lib(rectangle), %shape_lib(all), %shape_lib(hammer), calc_largest_square_block,
  
  % polygs, %hv_line(v), hv_line(h), %dg_line(u),dg_line(d), %CS,
  all,
  % line(_),dg_line(_), % release_points, all, %into_single_hidden,oldway %retain(solid(rectangle)), % shapes, %into_single_hidden,
  done % stop processing
  ]). 

% ?- print_grid(gridFn(X)).

fix_indivs_options(O,L):-is_list(O),maplist(fix_indivs_options,O,OL),append(OL,L).
fix_indivs_options(G,[G]):- var(G),!.
fix_indivs_options(I,O):- atom(I),individuation_macros(I,M),!,fix_indivs_options(M,O),!.
fix_indivs_options(macro(I),[macro(O)]):- fix_indivs_options(I,O).
fix_indivs_options(detect(O),[detect(O)]):-!.
fix_indivs_options(O,[detect(O)]):- is_gridoid(O),!.
fix_indivs_options(I,O):-listify(I,O),!.


:- style_check(+singleton).




/*
goal_expansion(Goal,Out):- compound(Goal), arg(N,Goal,E), 
   compound(E), E = set(Obj,Member), setarg(N,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(E,Cmpd,SA):-  compound(Cmpd), get_setarg_p2(E,Cmpd,SA).
get_setarg_p2(E,Cmpd,SA):- arg(N,Cmpd,E), SA=setarg(N,Cmpd).
get_setarg_p2(E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(E,Arg,SA).

system:term_expansion((Head:-Body),Out):-
   get_setarg_p1(I,Head,P1), compound(I), I = set(Obj,Member),
   call(P1,Var),
   expand_term((Head:- b_set_dict(Member,Obj,Var), Body),Out).

%goal_expansion(Goal,'.'(Image, Objs, Obj)):- Goal = ('.'(Image, Objs, A), Obj = V),  var(Obj).

goal_expansion_setter(Goal,Out):-
   compound(Goal), predicate_property(Goal,meta_predicate(_)),!,
   arg(N,Goal,P), compound(P), goal_expansion_setter(P,MOut),
   setarg(N,Goal,MOut), expand_goal(Goal, Out).

goal_expansion_setter(Goal,Out):- 
   get_setarg_p1(I,Goal,P1), compound(I), I = set(Obj,Member),
   call(P1,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).



goal_expansion(Goal,Out):- goal_expansion_setter(Goal,Out).


grid_shared_with(TestName*ExampleNum*in,TestName*ExampleNum*out):-!.
grid_shared_with(TestName*ExampleNum*out,TestName*ExampleNum*in):-!.

get_grid_and_name(In,Grid,GN):- is_grid(In),!,grid_to_id(Grid,GN).
get_grid_and_name(In,Grid,GN):- into_grid(In,Grid),!,grid_to_id(Grid,GN).



compute_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   compute_unshared_indivs(GN,Grid,Unshared).

compute_unshared_indivs(_GN,Grid,Unshared):-
   individuate_complete(Grid,Unshared).

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
   individuate_complete(Grid,Unshared),
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

individuation_reserved_options(ROptions,Reserved,Options):- 
   fix_indivs_options(ROptions,ReservedOptions),
   my_partition(is_object_or_grid,ReservedOptions,Reserved,Options),
   %select_default_i_options(Grid,H,V,Points,DefaultOptions),
   %default_i_options(DefaultOptions),
   %subst(Options0,defaults,DefaultOptions,Options),
   %(Options0==Options -> append(Options,DefaultOptions,NewOptions) ; (Options=NewOptions)),!,
   ignore((ROptions \= Options,
      pt(blue,fix_indivs_options(ro=ROptions,r=Reserved,o=Options)))).


individuate_default(Grid,IndvS):- individuate(defaults,Grid,IndvS).
individuate_second_pass(Grid,IndvS):- individuate([second_pass],Grid,IndvS).
individuate_complete(Grid,IndvS):- individuate(complete,Grid,IndvS).

individuate(_ROptions,Grid,IndvS):- Grid==[],!,IndvS=[].
individuate(ROptions,GridIn,IndvS):-
   must_be_free(IndvS),
   into_points_grid(GridIn,Points,Grid),
   quietly(grid_size(Grid,H,V)), 
   grid_to_id(Grid,ID),
   pt(yellow,individuate(H,V)=ID),
   individuate(H,V,ID,ROptions,Grid,Points,IndvS).

% tiny grid becomes a series of points
individuate(GH,GV,ID,ROptions,_Grid,Points,IndvS):- \+ is_list(ROptions), is_glyphic(Points,GH,GV), !, individuate_glyphic(GH,GV,ID,Points,IndvS).
individuate(H,V,ID,ROptions,Grid,Points,IndvSS):-
   individuation_reserved_options(ROptions,Reserved,NewOptions),
   individuals_raw(H,V,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
   %as_debug(9,ptt((individuate=IndvSRaw))),
   make_indiv_object_list(ID,H,V,IndvSRaw,IndvS1),
   combine_objects(IndvS1,IndvS),list_to_set(IndvS,IndvSS),
   once((delistify_single_element(ROptions,NamedOpts),
         save_grouped(individuate(ID,NamedOpts),IndvSS))).

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
  override_object(birth(glyphic),IndvS,IndvSO),
  save_grouped(individuate_glyphic(ID),IndvSO).

% Thunk(ArgList->Image)
call_fsi(Image,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints):-
  (var(Image)->make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,Image);true),
  clause(fti(Image,Options),Body),
  call((Body,deterministic(YN))),
  (YN == true -> ! ; true),
  NewReserved = Image.robjs,
  NewOptions = Image.todo,
  NewGrid = Image.grid,
  NextScanPoints = Image.points,
  SofarMaybeNew = Image.objs .
call_fsi(Image,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints):-
  clause(fsi(Image,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints),Body),
  call((Body,deterministic(YN))),
  (YN == true -> ! ; true).



make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,Image):-
  statistics(cputime,X),Timeleft is X+6,
  Image = _{
   % Options and TODO List (are actually same things)
   todo:Options, options:Options, options_o:Options, todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, iteration_max:20,
   % Grid and point representations
   grid:Grid, points:Points,
   % Original copies of Grid and point representations
   grid_o:Grid, points_o:Points,
   % objects found in grid and object that are reserved to not be found
   objs:Sofar, robjs:Reserved, objs_prev:[],
   % Notes and debug info
   notes:[], debug:[],
   % height width and lookup key for image
   h:GH, v:GV, id:ID}.

individuals_raw(GH,GV,ID,Options,Reserved,Points,Grid,IndvSRaw):-
 must_det_l((
  must_be_free(IndvSRaw),
  make_fti(GH,GV,ID,Grid,[],Reserved,Options,Points,Image),
  individuals_list(Image,GH,GV,[],ID,Options,Reserved,Points,Grid,Indv_0,_LeftOverPoints),
  =(Indv_0,Indv_1),
  unraw_inds(Image,Indv_1,Indv_2),  
  largest_first(Indv_2,IndvSRaw))).

unraw_inds(_Image,I,O):- I=O,!.

unraw_inds(Image,IndvS,IndvOO):-   
  largest_first(IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(Image,_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).

get_new_task_obj(Obj):-
   Obj = _{ original_grid: _ ,unprocessed_grid:_, original_points:_, points:_, options:_, todo:_,
     objs:_, grid_id:_, in_or_out:_, grid_size:_, reserver_indivs:_  }.

individuals_list(_Image,_GH,_GV,Sofar,_ID,_Options,_Reserved,Points,_Grid,Sofar,PointsO):- Points == [], !,PointsO=[].

individuals_list(Image,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
    assertion(maplist(is_cpoint,Points)),
    assertion(is_list([sofar1(Options)|Sofar])),
    assertion(maplist(nonvar_or_ci,[fsi,Grid,Points])),
    assertion(maplist(nonvar_or_ci,[fsi,Reserved,Options,Sofar,GV,GH,ID])),
     
      (((call_fsi(Image,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints)))),
          intersection(SofarMaybeNew,Sofar,Unchanged,New,GoneMissing),
          Options = [Head|_],
          ((\+ (sub_term(E,New), compound(E), E = birth(_))) -> override_object(birth(Head),New,NewUpdated) ; New=NewUpdated),
          append(Unchanged,NewUpdated,SofarUpdated),
          nop(ignore((GoneMissing\==[],wdmsg(goneMissing=GoneMissing)))),
  assertion(maplist(nonvar_or_ci,[fsi,NewReserved,NewGrid,NewOptions,SofarUpdated,NextScanPoints])),
    assertion(maplist(is_cpoint,NextScanPoints)),
    assertion(is_list([foundSofar1(Options)|SofarUpdated])),
  assertion(maplist(nonvar_or_ci,[NewGrid])),
  assertion(maplist(nonvar_or_ci,[GH,GV,ID,NewOptions,NewReserved,NextScanPoints])),
    once( (SofarUpdated\==Sofar) ; (Options\==NewOptions) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),
    %wqnl(indv(Options)=Indv),   
    
    individuals_list(Image,GH,GV,SofarUpdated,ID,NewOptions,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.

individuals_list(Image,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
  next_options(Options,Options2),
  set(Image,objs_prev)=Image.objs,
  set(Image,todo_prev)=Image.todo,
  individuals_list(Image,GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints).

individuals_list(_Image,GH,GV,Sofar,ID,Options,_Reserved,Points,_Grid,IndvListOut,[]):- 
 must_det_l(Options==[]),
 must_be_free(IndvListOut),
  assertion(is_list([sofar2(Options)|Sofar])),
  assertion(is_list([sofar|Sofar])),
  assertion(is_list([points|Points])),
  assertion(maplist(is_cpoint,Points)),
  as_debug(9,print_grid(GH,GV,'leftover_points'+ID,Points)),
  % maplist(make_point_object(ID,GH,GV),Points,IndvList),
  IndvList = [],
  % individuate([just(by_color)],Points,IndvList2),
  IndvList2 = [],
  % make_indiv_object(ID,GH,GV,Points,[object_shape(combined),object_shape(leftovers)],LeftOverObj), 
  LeftOverObj = [],
  flatten([Sofar,IndvList,IndvList2,LeftOverObj],IndvListOut),!.

individuals_list(Image,GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P):-!,
  throw(fail(individuals_list(Image,GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P))),!.

  

fti(Image):- fti(Image,Image.todo).

fti(_,[]).
%fti(Image,_):- Image.points=[], !.
fti(_,[done|_]):- !.

fti(Image,_):-
   length(Image.objs,Count),
  (member(progress,Image.options); Count > 30; (statistics(cputime,X), X > Image.timeleft)) -> 
  as_debug(8,(mass(Image.objs,Mass),pt(t([found_mass=(Count,Mass),fsi=Image.todo])))),fail.


fti(Image,[glyphic|set(Image,todo)]):-
  Points = Image.points,
  GH = Image.h,
  GV = Image.v,
  ID = Image.id,
  length(Points,Len),
  make_indiv_object(ID,GH,GV,Points,[mass(Len),vis_hv(GH,GV),object_shape(combined),object_shape(grid),object_shape(image)],Whole),
  %object_indv_id(Whole,_,Iv),
  maplist(make_point_object(ID,GH,GV,[object_shape(abstract),object_shape(colorlink)]),Points,IndvList),
  append([Image.objs,IndvList,[Whole]],set(Image,objs)),!.

fti(Image,[max_learn_objects(Routine,Max)|set(Image,todo)]):-
   set(Image,options)= [max_learn_objects(Routine,Max)|Image.options].

fti(Image,[Routine|set(Image,todo)]):- 
   member(max_learn_objects(Routine,Max),Image.options),
   length(Image.objs,Count),
   Count>Max,!,fail.

fti(Image,[call(G)|set(Image,todo)]):- call(G).


%fti(Image,[calc_largest_square_block|set(Image,todo)]):-
%  calc_largest_square_block(Image),Image.objs,set(Image,objs),Image.points,set(Image,points)),!.


% Find free points that are contained in objects and individuate them in their own way
fti(Image,[find_contained|set(Image,todo)]):-
  find_contained(Image.h,Image.v,Image.id,Image.objs,set(Image,objs),Image.points,set(Image,points)),!.

fti(Image,[colormass_subshapes|set(Image,todo)]):-
  colormass_subshapes(Image,Image.objs),!.

colormass_subshapes(_Image,[]):-!.
colormass_subshapes(Image,ImageObjs):-
  select(Obj,ImageObjs,Next),
  localpoints(Obj,ContainedPoints),
  vis_hv(Obj,H,V),
  object_grid(Obj,Grid),
  individuate(H,V,Image.id,[subshape_in_object],Grid,ContainedPoints,NewInside),
  append(Image.objs,NewInside,set(Image.objs)),
  colormass_subshapes(Image,Next).



% Find object that are contained in objects and individuate them in their own way  (TODO mame this more complete)
% Find free points that are contained in objects and individuate them in their own way
fti(Image,[find_engulfed|set(Image,todo)]):-
  print_grid(Image.grid),  
  % ( \+ is_input(Image)-> trace ; true),
  find_engulfed(Image).    find_engulfed(Image):- find_engulfed(Image,Image.objs,set(Image,objs)),!.

    
replace_i_each(OtherObjects,[I|NewInside],[O|NewInsideM],NewOtherObjects):-!,
  subst(OtherObjects,I,O,OtherObjectsM),
  replace_i_each(OtherObjectsM,NewInside,NewInsideM,NewOtherObjects).
replace_i_each(E,[],[],E).

fti(Image,[combine_objects|set(Image,todo)]):-
   combine_objects(Image.objs,set(Image,objs)),!.

fti(Image,[combine_duplicates|set(Image,todo)]):- 
   combine_duplicates(Image.objs,set(Image,objs)),!.

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

fti(Image,[macrof(AddTodo)|set(Image,todo)]):-
  listify(AddTodo,TodoLst),
  fti(Image,TodoLst).

fti(Image,[macro(AddTodo)|TODO]):-
  listify(AddTodo,TodoL),
  append([progress|TodoL],TODO,set(Image,todo)).


fti(Image,[DO|TODO]):-
  fsi(Image,set(Image,robjs),set(Image,grid),set(Image,todo),
    Image.h,Image.v,Image.objs,Image.id,[DO|TODO],
    Image.robjs,Image.points,Image.grid,set(Image,objs),set(Image,points)).

% Thunk(Image -> ArgList)
fsi(Image,ReservedIO,Grid,TODO,H,V,Sofar,ID,[retain_grid(Options)|TODO],ReservedIO,PointsIO,Grid,IndvList,PointsIO):-
  listify(Options,OptionsL),!,
  must_det_l(individuals_list(Image,H,V,Sofar,ID,OptionsL,ReservedIO,PointsIO,Grid,IndvList,_LeftOver)),
  pt(yellow,PointsIO).

%fti(Image,[by_color([])|set(Image,todo)]):-!.
has_color(C,Point):- C-_ = Point.

fti(Image,[by_color(C)|set(Image,todo)]):-
  my_partition(has_color(C),Image.points,ThisGroup,set(Image,points)),
  ignore(((ThisGroup\==[],make_indiv_object(Image.id,Image.h,Image.v,
    ThisGroup,[object_shape(by_color(C))],ColorObj)),addObject(Image,ColorObj))).

addObject(Image,Obj):- append(Image.objs,[Obj],set(Image,objs)).

/*
fti(Image,[by_color([])|set(Image,todo)]):-!.

fsi(_Image,ReservedIO,Grid,[by_color(Rest)|TODO],H,V,Sofar,ID,[by_color(Options)|TODO],ReservedIO,Points,Grid,NewSofar,NextScanPoints):-
  listify(Options,OptionsL),!,
  select(C,OptionsL,Rest),
  my_partition(=(C-_),Points,ThisGroup,NextScanPoints),
  ((ThisGroup\==[],make_indiv_object(ID,H,V,ThisGroup,[object_shape(by_color(C))],ColorObj))
    -> NewSofar = [ColorObj|Sofar] 
     ; NewSofar = Sofar).
*/

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
fsi(_Image,Reserved,Grid,['dots'|TODO],H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,[Indv|Sofar],Rest):- !,
   maybe_multivar(C), select(C-HV,Points,Rest),  \+ free_cell(C),
   make_indiv_object(ID,H,V,[object_shape(dots),C-HV],Indv),
   meets_indiv_criteria(dots,Indv),!.


fsi(Image,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[full|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-!,
  grid_to_individual(Grid,Obj),
  fsi(Image,ReservedO,GridO,OptionsOut,H,V,[Obj|Sofar],ID,TODO,ReservedI,Points,Grid,SofarOut,NextScanPoints).


fsi(_Image,ReservedIO,GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],ReservedIO,Points,Grid,SofarOut,NextScanPoints):-!,
  do_shapelib(GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],Points,Grid,SofarOut,NextScanPoints).


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


do_shapelib(GridO,Options,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],Points,Grid,SofarOut,NextScanPoints):-
  shape_lib_expanded(Hammer,Reserved), 
  do_shapelib_expanded(Hammer,Reserved,GridO,Options,H,V,Sofar,ID,TODO,Points,Grid,SofarOut,NextScanPoints).

do_shapelib_expanded(_Hammer,[],Grid,TODO,_H,_V,Sofar,_ID,TODO,Points,Grid,Sofar,Points):-!.
do_shapelib_expanded(Hammer,Reserved,GridO,TODO,H,V,Sofar,ID,TODO,Points,Grid,SofarOut,NextScanPoints):- 
  length(Reserved,RL),
  pt(shape_lib_expanded(Hammer)=RL),
  smallest_first(Reserved,ReservedSL),
  %debug_indiv(Reserved),
  proccess_overlap_reserved(Hammer,GridO,Grid,ID,H,V,ReservedSL,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,_StillReserved),
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


get_option_expansion([done|_],[done]):-!.
get_option_expansion([],[]):-!.
get_option_expansion([A|NO],O):- get_option_expansion(A,AA), !, listify(AA,AL),append(AL,NO,O).
get_option_expansion(I,O):- individuation_macros(I,O).


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

/*

fsi(_Image,Reserved,Grid,NewOptions,_H,_V,Sofar,_ID,[by_color|TODO], Reserved, Points, Grid,Sofar, Points):- !,
   append(
     [(by_color([(black), (blue),  (red),   (green),(yellow),
                     (silver),(purple),(orange),(cyan), (brown)]))],TODO,NewOptions).
*/

fsi(_Image,Reserved,Grid,TODO,GH,GV,Sofar,ID,[leftover_as_one|TODO], Reserved, Points, Grid,SofarIndvList, []):- !,
   make_indiv_object(ID,GH,GV,Points,[object_shape(combined),object_shape(leftover_as_one)],LeftOverObj), 
   append(Sofar,[LeftOverObj],SofarIndvList).


fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,[into_single|TODO],Reserved,Points,Grid,[Indv],Points):- !,
    maplist(globalpoints,Sofar,IndvPoints), append(IndvPoints,IndvPointsL),
    EIndvPoints=[object_shape(combined),object_shape(into_single)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    meets_indiv_criteria(into_single_hidden,IndvPoints),!.

%  fsi(_Image,NewReserved,NewGrid,NewOptions,H,V,Sofar,ID,TODO,Reserved,Points,Grid,OutInvdivS,NextScanPoints).
same_lcolor(LargestColor,Obj):- color(Obj,Color),nop(print_grid(Obj)),!,Color==LargestColor.

fsi(_Image,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[IsToBeRewritten|NO],Reserved,Points,Grid,Sofar,Points):-
    atom(IsToBeRewritten), 
    individuation_macros(IsToBeRewritten,Expansion),!,
    listify(Expansion,ExpansionL),append(ExpansionL,NO,TODO).


fti(Image,['fourway'|_]):- !,
  H = Image.h,
  V = Image.v,
  ID = Image.id,
  H > 13, V> 13,
  largest_first(Image.objs,Ordered),   
  
  grid_to_3x3_objs(Ordered,Image.grid_o,FourWay1s,Keep),
  as_debug(8,writeln('fourway-found')),
              globalpoints(Keep,KeepPoints),!,
  set(Image,points) = KeepPoints,!,
  % set_bgc(LargestColor),  

  %Restart = Image.options_o,!,
  set(Image,options) = [progress|Image.options],
  %my_partition(==(fourway),Restart,_,Without4Way),!,

  make_grid(H,V,set(Image,grid)),
  %points_to_grid(H,V,KeepPoints,set(Image,grid)),!,

  append(Keep,FourWay1s,set(Image,objs)),!,
  as_debug(8,print_grid(H,V,'4-way'+ID,FourWay1s)),!,  
  as_debug(8,print_grid(H,V,'4-waypoints'+ID,Image.points)),!,
  set(Image,todo) = [done],!. % Without4Way,!.
 

remove_from_image(Image,Data):-    
    must_det_l((remove_global_points(Data,Image.points,Points),
    pt(Points),
    set(Image,points) = Points)),!.
    


overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_l((remove_global_points(FourWay1s,Sofar,Sofar1),add_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).

fsi(_Image,Reserved,NewGrid,TODO,H,V,Sofar,_ID,['regroup'|TODO],Reserved,Points,Grid,OutInvdivS,Points):- 
  make_indiv_object_list(Grid,H,V,Sofar,OutInvdivS), 
  points_to_grid(H,V,Points,NewGrid).


fsi(_Image,Reserved,Grid,TODO,H,V,Sofar,ID,[solid(rectangle)|TODO],Reserved,_Points,Grid,AllOUT,NextScanPoints):- !, fail,
  globalpoints(Grid,NewGPoints),
  grid_to_segs(Grid,Segs),
  seg_squares(Segs,Squares),
  segs_to_pointlists(Squares,Objs),  Objs\==[],
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


fsi(_Image,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,RestPoints):- 
  merge_shapes(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,RestPoints).

merge_shapes(H,V,ID,Sofar,Option,TODO,OptionsOut,_Grid,Points,NewSofar,Points):- 
  Option = merges(ShapeType1,ShapeType2), copy_term(Option,OptionC),!, 
      selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess),
      any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
      connection_direction(Option,Dir),
  %rot_left_45(Dir1,DirL),rot_left_45(DirL,Dir90),
  % \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  combine_2objs(ID,H,V,HV1,HV2,[],[object_shape(Option)],Combined),
  append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).


cycle_back_in(OptionC,TODO,[OptionC|TODO]).
cycle_back_in(OptionC,TODO,OptionsOut):-
  length(TODO,N), N2 is floor(N/2),length(LL,N2),append(LL,RR,TODO),append(LL,[OptionC|RR],OptionsOut).


fsi(Image,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[connects(ShapeType)|TODO],Reserved,Points,Grid,NewSofar,RestPoints):- !,
 fsi(Image,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[connects(ShapeType,ShapeType)|TODO],Reserved,Points,Grid,NewSofar,RestPoints).

fsi(_Image,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,RestPoints):-   
  connects(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,RestPoints).


selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess):- 
  into_group(Sofar,Sofar1,Closure),
  select(HV1,Sofar1,Found1), isz(HV1,ShapeType1),
  select(HV2,Found1,SofarLess1),isz(HV2,ShapeType2),
  call(Closure,SofarLess1,SofarLess).

connects(H,V,ID,Sofar,Option,TODO,OptionsOut,_Grid,Points,NewSofar,Points):- 
  Option = connects(ShapeType1,ShapeType2), copy_term(Option,OptionC),!,
  selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
  connection_direction(Option,Dir),    
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  combine_2objs(ID,H,V,HV1,HV2,[],[object_shape(Option)],Combined),
  append(SofarLess,[Combined],NewSofar),
  cycle_back_in(OptionC,TODO,OptionsOut).

connection_direction(Connected,Dir):-
   arg(_,Connected,ShapeType),
     shape_type_dirs(ShapeType,[Dir|_]). 
    %shape_type_dir(ShapeType2,Dirs2),

fsi(_Image,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,Points):-
  jumps(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,_RestPoints).

jumps(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,RestPoints):- 
  Option = jumps(ShapeType1), copy_term(Option,OptionC),!,
  selected_from(Sofar,ShapeType1,ShapeType1,HV1,HV2,SofarLess),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), any_gpoint(HV2,C-P2),
  % skip over MP
  is_adjacent_point(P0,Dir,P1), is_adjacent_point(P1,Dir,MP), is_adjacent_point(MP,Dir,P2),is_adjacent_point(P2,Dir,P3),
  any_gpoint(HV1,_-P0), any_gpoint(HV2,_-P3),
  ignore(once((get_color_at(Points,Grid,MC),is_color(MC));MC=C)),
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  % TODO: HACK WE MIGHT NOT WANT TO STEAL THE POINT?   
  once(select(MC-MP,Points,RestPoints);Points=RestPoints),
  combine_2objs(ID,H,V,HV1,HV2,[],[object_shape(Option)],Combined),
  append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).

fsi(_Image,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[extends(Option)|TODO],Reserved,Points,Grid,NewSofar,NextScanPoints):- 
  extends(H,V,ID,Sofar,Option,TODO,OptionsOut,Points,NewSofar,NextScanPoints).

extends(H,V,ID,Sofar,ShapeType1,TODO,OptionsOut,Points,NewSofar,NextScanPoints):- 
  Option = extends(ShapeType1), copy_term(Option,OptionC),!,
  select(HV1,Sofar,SofarLess),isz(HV1,ShapeType1),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,MP),select(MC-MP,Points,ScanPoints),
  \+ (is_adjacent_point(P1,_,P2), is_adjacent_point(P2,_,MP),any_gpoint(HV1,_-P2)),
  all_individuals_near(Dir,Option,C,[MC-MP],ScanPoints,NextScanPoints,IndvPoints),
  combine_2objs(ID,H,V,HV1,[],IndvPoints,[object_shape(Option)],Combined),
  append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).



    combine_2objs(ID,H,V,HV1,HV2,NewPoints,IPROPS,Combined):-
      globalpoints(HV1,GP1), globalpoints(HV2,GP2),      
      % indv_props(HV1,Props1),indv_props(HV2,Props2),
      Props1=[],Props2=[],
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


fsi(_Image,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,OUT,NextScanPoints):- 
   ( Option \==dots), 
   find_one_individual(H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Indv,NextScanPoints),
   append(Sofar,[Indv],OUT).

find_one_individual(H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints):-
   find_one_individual3(H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints),!.
find_one_individual(H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints):-
   find_one_individual2(H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints).

find_one_individual3(H,V,ID,_Sofar,Option,TODO,OptionsOut,_Reserved,Points,_Grid,Obj,NextScanPoints):-
    min_len3(Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,Rest1),
    %ScanPoints = Rest1,
    ((adjacent_point_allowed(C,HV2,Dir,HV3),select(C2-HV3,Rest1,ScanPoints), ok_color_with(C,C2));
     (allowed_dir(Option,Dir2),Dir2\=Dir, adjacent_point_allowed(C,HV2,Dir2,HV3),select(C-HV3,Rest1,ScanPoints))),    
    %maybe_multivar(C), 
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2,C-HV3],ScanPoints,NextScanPoints,IndvPoints), 
    make_indiv_object(ID,H,V,IndvPoints,[object_shape(ShapeType),birth(individual3(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  cycle_back_in(OptionC,TODO,OptionsOut).

find_one_individual2(H,V,ID,_Sofar,Option,TODO,OptionsOut,_Reserved,Points,_Grid,Obj,NextScanPoints):-
    min_len3(Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
  select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), 
    make_indiv_object(ID,H,V,IndvPoints,[object_shape(ShapeType),birth(individual2(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  cycle_back_in(OptionC,TODO,OptionsOut).


point_groups_by_color(Option,[IndvPoints|Groups],Points,Rest):-    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), !,
    point_groups_by_color(Option,Groups,NextScanPoints,Rest).
point_groups_by_color(_Option,[],Points,Points).


min_len3(colormass,[_,_,_,_,_|_]):-!.
min_len3(rectangle,[_,_,_,_,_|_]):-!.
min_len3(diamonds,[_,_,_,_|_]):-!.
min_len3(_,[_,_|_]).
%  min_len3(_,_).


/*
find_one_in divi dual(GH,GV,Sofar,ID,solid(Option),Reserved,Points,Grid,[object_shape(solid(Option))|Indv],NextScanPoints):- fail, !,
    select(C-HV,Points,Rest), non_free_fg(C), 
    get_neighbors(Rest,non_free_fg,Found,HV),
    format('~N~q~n',[Found]),
    select(f(Dir,C-HV2),Found,Found1),    
    allowed_dir(Option,Dir),
    rot_right_45(Dir,NE), rot_left_45(Dir,NW), 
    select(f(NE,C-_HV3),Found1,Found2),
    select(f(NW,C-_HV4),Found2,Found3),
    rot_right_45(NE,E), rot_left_45(NW,W),
    select(f(E,C-HV5),Found3,Found4),
    select(f(W,C-HV6),Found4,Found5),
    subtract(Rest,[C-HV5,C-HV6],ScanPoints),
    all_individuals_near(Dir,Option,C,[C-HV,C2-HV2,C-HV5,C-HV6],ScanPoints,NextScanPoints,Indv),
    meets_indiv_criteria(Option,Indv).

fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
   
   find_one_indiv idual(H,V,Sofar,ID,Option,Reserved,Points,Grid,IndvPoints,NextScanPoints),
   make_indiv_object(ID,H,V,[birth(Option),object_shape(Option)|IndvPoints],Indv),!,
   append(Sofar,[Indv],OUT).
%fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
%  nop(fsi(_Image,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints)).
*/


fsi(_Image,Reserved,Grid,Options,_H,_V,Sofar,_ID,[Option|Options],Reserved,Points,Grid,Sofar,Points):-
  is_thing_or_connection(Option).

is_thing_or_connection(Option):-allowed_dir(Option,_Dir).
is_thing_or_connection(connects(_,_)).
is_thing_or_connection(merges(_,_)).
is_thing_or_connection(jumps(_,_)).

%tiny_i_options([call(set_bgc(zero)),dots,line(_),all,into_single_hidden]).

next_options([_|T],T).


%select_default_i_options(_Grid,H,V,_Points,Options):- (H=<5;V=<5),!,tiny_i_options(Options).
%select_default_i_options(_Grid,H,V,_Points,Options):- (H=<6;V=<6),!,tiny_i_options(Options).
%select_default_i_options(_Grid,_H,_V,_Points,Options):-  default_i_options(Options).


ok_color_with(C,C2):- /* non_free_fg(C2), */ C==C2.

sameglobalpoints(Points,IndvC,NextScanPoints,IndvC):-
  globalpoints(IndvC,GPoints),
  remove_gpoints(GPoints,Points,NextScanPoints).

remove_gpoints([],Rest,Rest).
remove_gpoints([GPoint|GPoints],Points,Rest):- select(GPoint,Points,Mid),remove_gpoints(GPoints,Mid,Rest).

get_neighbors(From,P1,Found,HV):- 
  findall(f(Dir,C-HV2), (is_adjacent_point(HV,Dir, HV2),member(C-HV2,From),call(P1,C)),Found).

has_2neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),member(C-HV3,Rest2),!.

has_3neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),rot_left_45(NW,W),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),member(C-HV4,Rest3),!.

has_4neighbours(Option,C,Rest,HV):- maybe_multivar(C),
    allowed_dir(Option,N),rot_left_45(N,NW),rot_left_45(NW,W),rot_left_45(W,SW),
    is_adjacent_point(HV,N, HV2),select(C-HV2,Rest, Rest2),
    is_adjacent_point(HV,NW,HV3),select(C-HV3,Rest2,Rest3),
    is_adjacent_point(HV,W, HV4),select(C-HV4,Rest3,Rest4),
    is_adjacent_point(HV,SW, HV5),member(C-HV5,Rest4),!.

min_neighbours(0,_,_,_).
min_neighbours(Count,Dir,C,Rest,HV):- maybe_multivar(C),     
    is_adjacent_point(HV,Dir, HV2),member(C-HV2,Rest),
    rot_left_45(Dir,NW), CountDown is Count-1, min_neighbours(CountDown,NW,C,Rest,HV).



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
adjacent_point_allowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS),member(Dir,DirS).
%adjacent_point_allowed(_,C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS),member(Dir,DirS).
adjacent_disallowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir_list(Shape,DirS), \+ member(Dir,DirS).


all_individuals_near(_Dir,_Options,_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(Dir,Options,C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(Dir,Options,C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(Dir,Options,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_Dir,_Options,_C,_From,[],[],[]):-!.
individuals_near(Dir,Options,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Dir,Options,C,E,From),!,
  individuals_near(Dir,Options,C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(Dir,Options,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Dir,Options,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(_Dir2,Options,C,C2-E,List):- allowed_dir(Options,Dir), adjacent_point_allowed(C,E2,Dir,E), member(C2-E2,List),ok_color_with(C2,C).

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
object_priority(Indv,Priority):- oclass_piority(Class,Priority), isz(Indv,Class).
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
 fail, select(obj(I),IndvS,Rest1), fail, % round 2
  globalpoints(obj(I),[C-Point]),
  nonvar_or_ci(C).

single_point(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select([C-Point],IndvS,Rest1),
  nonvar_or_ci(C).

single_point0(C-Point,IndvS,Rest1):- maybe_multivar(C),
  select(C-Point,IndvS,Rest1),
  nonvar_or_ci(C).


/*
unraw_inds2(Image,Options,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv),
   reverse(Indv,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Image,Options,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(Image,Options,IndvS,IndvO):-   
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
  unraw_inds2(Image,Options,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(Image,Options,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),fail,
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,object_shape(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Image,Options,[NewGroup|RRestO],IndvO).



unraw_inds2(Image,Options,IndvS,IndvO):-  %fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),
  single_point(C2-Point2,Rest1,Rest),fail,
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Image,Options,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(Image,Options,IndvS,IndvO):- fail,
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(Image,Options,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(Image,Options,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Dir,Options,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Image,Options,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_Image,_,IndvS,IndvS).




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




/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

% =========================================================
% TESTING FOR INDIVIDUATIONS
% =========================================================
 i:- fav_i(X),i(X).   i(GridIn):- i2([complete],GridIn).
ig:- fav_i(X),ig(X). ig(GridIn):- i2(complete,GridIn).
iq:- fav_i(X),ig(X). iq(GridIn):- iq(complete,GridIn).
iL:- fav_i(X),iL(X). iL(GridIn):- i2([shape_lib(as_is),complete],GridIn).

:- add_history1(ig).

fav_i(X):- clsmake, nb_current(test_name,X),X\==[].
fav_i(t('00d62c1b')).
fav_i(X):- fav(X).
fav_i(_).


i2(ROptions,GridSpec):- clsmake,
  clear_shape_lib(as_is),
  into_grids(GridSpec,GridIn),
  once((into_grid(GridIn,Grid),ig(ROptions,Grid))).

ig(ROptions,Grid):- 
  %fail,
  into_grid(Grid, GridIn), 
  dash_chars,
  print_grid(GridIn), 
  grid_to_id(GridIn,ID),
  set_current_test(ID),
  individuate(ROptions,GridIn,IndvS),
  %maplist(add_shape_lib(as_is),IndvS),
  dash_chars,
  print_grid(IndvS),
  format("~N~n% ?- ~q.~n~n",[ig(ROptions,ID)]),
  length(IndvS,LenS),
  print_list_of(individuate=LenS,IndvS),
  dash_chars.
% =========================================================

iq(ROptions,Grid):- 
  make,
  %fail,
  %into_grid(Grid, GridIn), 
  %make_fti(Grid,VM),
  dash_chars,
  %print_grid(GridIn), 
  %grid_to_id(GridIn,VM.id),
  %set_current_test(VM.id),
  individuate(ROptions,Grid,IndvS),
  %maplist(add_shape_lib(as_is),IndvS),
  dash_chars,
  print_grid(IndvS),
  %format("~N~n% ?- ~q.~n~n",[ig(ROptions,VM.id)]),
  length(IndvS,LenS),
  print_list_of(individuate=LenS,IndvS),
  dash_chars.
% =========================================================

:- use_module(library(multivar)).

maybe_multivar(C):- nonvar(C),!.
%maybe_multivar(C):- multivar(C).
maybe_multivar(_).

:- dynamic(reuse_grid_nums/1).

:- discontiguous(fsi/14).
:- discontiguous(fti/2).
:- discontiguous(one_fti/2).
   
:- dynamic(is_unshared_saved/2).
:- dynamic(is_shared_saved/2).

individuation_macros(out, complete).
individuation_macros(in, complete).

% if there are 3 or less of a color dont group the whole color (It/they must be special points)
individuation_macros(by_color, X):-
   findall(by_color(10,Color),enum_colors(Color),X).

individuation_macros(force_by_color, X):-
   findall(by_color(0,Color),enum_colors(Color),X).

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
   rectangle, diamonds,
   find_repetition,
   recalc_sizes,
   hv_line(h),  
   dg_line(d), 
   dg_line(u), 
   hv_line(v),  
   connects(dg_line(_),dg_line(_)),
   connects(hv_line(_),dg_line(_)),
   connects(hv_line(_),hv_line(_)),
   jumps,% run the "jumps" macro
   point_corners,
   by_color,
   standalone_dots,
   merges(Z,Z), % merge lines into square
   merges(hv_line(_),hv_line(_)),
   merges(dg_line(_),dg_line(_)),
   connects(X,X)]).

individuation_macros(jumps,
  [ %progress, 
    jumps(hv_line(h)), % multicolored lines
    jumps(hv_line(v)),
    jumps(dg_line(d)), % multicolored diagonals
    jumps(dg_line(u)), 
    jumps(jumps(hv_line(v))),
    jumps(jumps(hv_line(h))) % joined jumps
    ]).  

individuation_macros(std_shape_lib_lean, [                     
                     shape_lib(removed),
                     shape_lib(in), 
                      shape_lib(out), % random objects learned from other passes
                      shape_lib(pair) % random objects learned from this pass
]).
individuation_macros(std_shape_lib, [
    shape_lib(out), 
    %shape_lib(as_is),
    shape_lib(noise), % data found we needed to delay analysis on
    shape_lib(intruder), % anything that stood out in the past 
   %shape_lib(added), 
    shape_lib(in), 
    shape_lib(out), % random objects learned from other passes
    shape_lib(pair), % random objects learned from this pass
    shape_lib(hammer), % cheater objects for hammer sanity test/hack
    %shape_lib(l_shape), % random objects shown by douglas to look for
    shape_lib(human)]). % random objects shown by humans to look for


% noit doing press-pass presently
individuation_macros(pre_pass, [  standard,complete]).

% Bring the "cognitive load" down by culling tiny objects
individuation_macros(reduce_population, [
                     when(len(objs)>40,colormass_merger(3)),
                     when(len(objs)>40,colormass_merger(4)),
                     when(len(objs)>40,colormass_merger(5)),
                     when(len(objs)>40,release_objs_lighter(3)),
                     when(len(objs)>40,release_objs_lighter(4)),
                     when(len(objs)>40,release_objs_lighter(5)),
                     when(len(objs)>40,colormass_merger(6)),
                     when(len(objs)>40,release_objs_lighter(6)),

                     when(len(objs)>20,colormass_merger(7)),

                     when(len(objs)>20,colormass_merger(10)),

                     when(len(objs)>20,colormass_merger(15)),
                     call(true)]).

% about to finish make sure we grab what we can
individuation_macros(altro, [
    reduce_population,
    remove_used_points,
    when((len(points)=<30),dots),    
    when((len(points)>30),by_color)]).


% the typical toplevel indivduator
individuation_macros(complete, [
    shape_lib(as_is),
    find_colorfull_idioms,
    shape_lib(as_is),
    rectangle,point_corners,
    standard,%colormass_merger(3), % call the standard things done in most indiviguators    
    reduce_population, % @TODO DISABLED FOR TESTS    %altro,
    colormass_subshapes, % find subshapes of the altro
    find_touches,
    %when((colors(i.points,Cs),len(Cs)<2),dots), % any after this wont find individuals unless this is commented out
    colormass_merger(2),
    when((len(points)=<30),dots),
    dots,leftover_as_one, % any after this wont find individuals unless this is commented out
    find_touches,
   done % stop processing
 ]).

% the standard things done in most indiviguators
individuation_macros(standard, [
    fourway, % find fold patterns 
    recalc_sizes,
    std_shape_lib, % stuff that was learned/shown previously
   +max_learn_objects(colormass,30),
   +max_learn_objects(rectangle,30),
   +max_learn_objects(hv_line(_),30),
   +max_learn_objects(dg_line(_),30),
    rectangle, recalc_sizes, % blobs of any shape that are the same color  
    % @TODO DISABLED FOR TESTS   colormass_subshapes, % subdivide the color masses .. for example a square with a dot on it
    subshape_main, % macro for sharing code with "subshape_in_object"
    colormass,
    connects(jumps(X),jumps(X)), % connected jumps    
    merges(Z,Z), % merge objects of identical types (horizontal lines become solid squares)   
    find_touches,
    find_contained, % mark any "completely contained points"
    find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
     % make sure any objects are perfectly the same part of the image are combined       
    combine_duplicates]).

individuation_macros(defaults, [ complete ]).

individuation_macros(unused, [
  detect(_VM_), % makes an image detectable
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
  all
  % line(_),dg_line(_), % release_points, all, %into_single_hidden,oldway %retain(solid(rectangle)), % shapes, %into_single_hidden,
  ]). 

% ?- print_grid(gridFn(X)).

fix_indivs_options(O,L):-is_list(O),maplist(fix_indivs_options,O,OL),my_append(OL,L).
fix_indivs_options(G,[G]):- var(G),!.
fix_indivs_options(I,O):- atom(I),individuation_macros(I,M),!,fix_indivs_options(M,O),!.
fix_indivs_options(macro(I),[macro(O)]):- fix_indivs_options(I,O).
fix_indivs_options(detect(O),[detect(O)]):-!.
fix_indivs_options(O,[detect(O)]):- is_gridoid(O),!.
fix_indivs_options(I,O):-listify(I,O),!.





grid_shared_with(TestName*ExampleNum*in,TestName*ExampleNum*out):-!.
grid_shared_with(TestName*ExampleNum*out,TestName*ExampleNum*in):-!.

get_grid_and_name(In,Grid,GN):- is_grid(In),!,grid_to_id(Grid,GN).
get_grid_and_name(In,Grid,GN):- into_grid(In,Grid),!,grid_to_id(Grid,GN).



:- decl_pt(detect_indvs(group,group,-)).
detect_indvs(In,Out,Grid):- individuate(In,Grid,Out).

individuation_reserved_options(ROptions,Reserved,Options):- 
   fix_indivs_options(ROptions,ReservedOptions),
   %my_partition(is_object_or_grid,ReservedOptions,Reserved,Options),
   Options = ReservedOptions,
   Reserved = [],
   %select_default_i_options(Grid,H,V,Points,DefaultOptions),
   %default_i_options(DefaultOptions),
   %subst(Options0,defaults,DefaultOptions,Options),
   %(Options0==Options -> my_append(Options,DefaultOptions,NewOptions) ; (Options=NewOptions)),!,
   ignore((ROptions \= Options,is_list(ROptions), sub_var(complete,ROptions),
      (pt(blue,fix_indivs_options(ro=ROptions,r=Reserved,o=Options))))).


%individuate_second_pass(Grid,IndvS):- individuate([second_pass],Grid,IndvS).
%?- i(v(e41c6fd3)*(trn+0)*in).

individuate(_ROptions,Grid,IndvS):- Grid==[],!,IndvS=[].
individuate(ROptions,GridIn,IndvS):-
  must_det_ll((
   must_be_free(IndvS),
   into_points_grid(GridIn,Points,Grid),
   grid_to_id(Grid,ID),
  my_assertion(\+ is_grid(ID)),
   quietly(grid_size(Grid,H,V)), 
   pt(yellow,ig(ROptions,ID)=(H,V)),
   individuate(H,V,ID,ROptions,Grid,Points,IndvS))).

% tiny grid becomes a series of points
individuate(GH,GV,ID,ROptions,_Grid,Points,IndvS):- \+ is_list(ROptions), 
  is_glyphic(Points,GH,GV), individuate_glyphic(GH,GV,ID,Points,IndvS),!.
individuate(GH,GV,ID,whole,_Grid,Points,IndvS):-  individuate_whole(GH,GV,ID,Points,IndvS),!.
individuate(H,V,ID,ROptions,Grid,Points,IndvSS):-
   must_det_ll((
   individuation_reserved_options(ROptions,Reserved,NewOptions),
   %trace,
   make_fti(H,V,ID,Grid,[],Reserved,NewOptions,Points,VM),
   individuals_raw(VM,H,V,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
   %as_debug(9,ptt((individuate=IndvSRaw))),
   make_indiv_object_list(ID,H,V,IndvSRaw,IndvS1),
   combine_objects(IndvS1,IndvS),
   list_to_set(IndvS,IndvSS),
   once((delistify_single_element(ROptions,NamedOpts),
         save_grouped(individuate(ID,NamedOpts),IndvSS))))),!.

into_points_grid(GridIn,Points,Grid):- 
   globalpoints(GridIn,Points),
   into_grid(GridIn,Grid),!.

is_glyphic(Points,_GH,_GV):- length(Points,Len), Len < 5.
is_glyphic(Points,GH,GV):- ( GH=<5 ; GV=<5 ), length(Points,Len), Len is GH * GV.

individuate_glyphic(_GH,_GV,_ID,PointsIn,IndvS):- PointsIn==[],!,IndvS=[].
individuate_glyphic(GH,GV,ID,PointsIn,IndvSO):-
 must_det_ll((
  globalpoints(PointsIn,Points),
  length(Points,Len),
  make_indiv_object(ID,GH,GV,Points,[mass(Len),vis_hv(GH,GV),iz(combined),iz(grid),iz(image)],Whole),
  %object_indv_id(Whole,_,Iv),
  maplist(make_point_object(ID,GH,GV,[iz(abstract),iz(colorlink)]),Points,IndvList),
  my_append(IndvList,[Whole],IndvS),!,
  override_object(birth(glyphic),IndvS,IndvSO),
  save_grouped(individuate_glyphic(ID),IndvSO),
  assert_shape_lib(pair,Whole))),!.

individuate_whole(_GH,_GV,_ID,PointsIn,IndvS):- PointsIn==[],!,IndvS=[].
individuate_whole(GH,GV,ID,PointsIn,IndvSO):-
  globalpoints(PointsIn,Points),
  length(Points,Len),
  make_indiv_object(ID,GH,GV,Points,[mass(Len),vis_hv(GH,GV),iz(combined),iz(grid),birth(whole),iz(image)],Whole),
  IndvSO=[Whole],
  save_grouped(individuate_whole(ID),IndvSO),
  assert_shape_lib(pair,Whole),!.


% Thunk(ArgList->VM)
call_fsi(VM,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints):- !,
  (var(VM)->(make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,VM));true),
  clause(fti(VM,Options),Body),
  call((Body,deterministic(YN),true)),
  (YN == true -> ! ; true),
  NewReserved = VM.robjs,
  NewOptions = VM.program,
  NewGrid = VM.grid,
  NextScanPoints = VM.points,
  SofarMaybeNew = VM.objs .
call_fsi(VM,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints):- fail,
  clause(fsi(VM,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints),Body),
  call((Body,deterministic(YN),true)),
  (YN == true -> ! ; true).

make_fti(GH,GV,ID,Grid,Sofar,Reserved,Options,Points,VM):-  
  statistics(cputime,X),Timeleft is X+30,
  max_min(GH,GV,Max,_Min),
  ArgVM = vm{
   % parent VM
   parent:_,

   % Options and TODO List (are actually same things)
   program:Options, options:Options, roptions:Options, %todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, objs_max_len:Max, objs_min_mass:_, objs_max_mass:_,
   % Grid and point representations
   grid:Grid, points:Points,
   % Original copies of Grid and point representations
   % grid_o:Grid, 
   points_o:Points, % points_repair:[],
   % objects found in grid and object that are reserved to not be found
   objs:Sofar,  robjs:Reserved, %objs_prev:[],
   % Notes and debug info
   notes:_, debug:_,
   % height width and lookup key for image
   h:GH, v:GV, id:ID},
   (var(VM) -> (fix_test_name(ID,TestID,_), make_training_hints(TestID,ArgVM,HintedVM), HintedVM = VM) ; true),
   (nb_current('$vm_pair',Shared)-> transfer_missing(Shared,VM) ; true),
   %b_set_dict(objs,VM,[]),
   %set(VM.current_i) = VM
   !.

into_fti(ID,ROptions,GridIn0,VM):-
  %must_det_ll
  ignore((ROptions \= Options,is_list(ROptions), sub_var(complete,ROptions),
    (pt(blue,fix_indivs_options(ro=ROptions,r=Reserved,o=Options))))),
  statistics(cputime,X),Timeleft is X+30,
  fix_indivs_options(ROptions,Options),
  %rtrace,
  %must_det_l
  ((
   %rtrace,
  (is_dict(GridIn0)-> (VM = GridIn0, GridIn = GridIn0.grid) ; GridIn0 = GridIn),
  
  globalpoints(GridIn,Points),
  into_grid(GridIn,Grid),
  (var(ID)->grid_to_id(Grid,ID);true),
  grid_size(Grid,H,V),
  max_min(H,V,Max,_Min),
  pt(yellow,ig(ROptions,ID)=(H,V)),
  ArgVM = vm{
   % parent VM
    
   % Options and TODO List (are actually same things)
   program:Options, options:Options, roptions:ROptions, %todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, objs_max_len:Max, objs_min_mass:_, objs_max_mass:_,
   % Grid and point representations
   grid:Grid, points:Points,
   % Original copies of Grid and point representations
   % grid_o:Grid, 
   points_o:Points, % points_repair:[],
   % objects found in grid and object that are reserved to not be found
   objs:[],  robjs:Reserved, % objs_prev:[],
   % Notes and debug info
   notes:_, debug:_,
   % height width and lookup key for image
   h:H, v:V, id:ID},
   %ignore(VM>:<ArgVM),
   (var(VM) -> ArgVM=VM ; transfer_missing(ArgVM,VM)),
   %(var(VM) -> (fix_test_name(ID,TestID,_), make_training_hints(TestID,ArgVM,HintedVM), HintedVM = VM) ; true),
   %(nb_current('$vm_pair',Shared)-> transfer_missing(Shared,VM) ; true),
    true)),
   %b_set_dict(objs,VM,[]),
   %set(VM.current_i) = VM
   !.

transfer_missing(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     maplist(nb_link_pairs_if_missing(VM),Pairs).
transfer_onto_dict(ArgVM,VM):-
     dict_pairs(ArgVM,_,Pairs),
     maplist(nb_link_pairs(VM),Pairs).

b_set_pairs(VM,K-V):- (nonvar(V),V\==[])->my_b_set_dict(K,VM,V);true.
nb_set_pairs(VM,K-V):- (nonvar(V),V\==[])->nb_set_dict(K,VM,V);true.
nb_link_pairs(VM,K-V):- (nonvar(V),V\==[])->nb_link_dict(K,VM,V);true.
nb_link_pairs_if_missing(VM,K-NewV):- V = VM.K, ((var(V),\+ attvar(V))->nb_link_dict(K,VM,NewV);true).

individuals_raw(VM,GH,GV,ID,Options,Reserved,Points,Grid,IndvSRaw):-
 must_det_ll((
  must_be_free(IndvSRaw),
  make_fti(GH,GV,ID,Grid,[],Reserved,Options,Points,VM),
  individuals_list(VM,GH,GV,[],ID,Options,Reserved,Points,Grid,Indv_0,_LeftOverPoints),
  =(Indv_0,Indv_1),
  unraw_inds(VM,Indv_1,Indv_2),  
  largest_first(Indv_2,IndvSRaw))).

unraw_inds(_VM,I,O):- I=O,!.

unraw_inds(VM,IndvS,IndvOO):-   
  largest_first(IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(VM,_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).

get_new_task_obj(Obj):-
   Obj = _{ original_grid: _ ,unprocessed_grid:_, original_points:_, points:_, options:_, program:_,
     objs:_, grid_id:_, in_or_out:_, grid_size:_, reserver_indivs:_  }.

individuals_list(_VM,_GH,_GV,Sofar,_ID,_Options,_Reserved,Points,_Grid,Sofar,PointsO):- Points == [], !,PointsO=[].

individuals_list(VM,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
    my_assertion(maplist(is_cpoint,Points)),
    my_assertion(is_list([sofar1(Options)|Sofar])),
    my_assertion(maplist(nonvar_or_ci,[fsi,Grid,Points])),
    my_assertion(maplist(nonvar_or_ci,[fsi2,Reserved,Options,Sofar,GV,GH,ID])),
     length(Sofar,SofarL),
      (((call_fsi(VM,NewReserved,NewGrid,NewOptions,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,SofarMaybeNew,NextScanPoints)))),

          intersection(SofarMaybeNew,Sofar,Unchanged,New,GoneMissing),
          %addCPoints(VM,GoneMissing),
          remCPoints(VM,New),


          intersection(Options,NewOptions,_,Birth,_),
          (last(Birth,Head)-> true; Options = [Head|_]),

          ((\+ (sub_term(E,New), compound(E), E = birth(_))) -> override_object(birth(Head),New,NewUpdated) ; New=NewUpdated),
          my_append(Unchanged,NewUpdated,SofarUpdated),

          length(SofarMaybeNew,SofarMaybeNewL),

          ignore(((NewOptions\==Options;(GoneMissing\==[];SofarMaybeNewL\==SofarL)),
          
          (NextScanPoints\==[]->Second=NextScanPoints;Second=NewGrid),

          nop(ignore((GoneMissing\==[],ptt(goneMissing=GoneMissing)))),
          once( 
                (SofarMaybeNew\== Sofar;NextScanPoints\== Points, %append(SofarMaybeNew,NextScanPoints,DG),
                              format('~N'), print_side_by_side4(silver,print_grid(GH,GV,SofarMaybeNew),objs+Head,_,
                                                                print_grid(GH,GV,Second),second));

                     
                
               % (NewGrid\=@= Grid, print_grid(_,_,grid+Head,NewGrid));
                true),
          
             true)),
            

  my_assertion(maplist(nonvar_or_ci,[fsi3,SofarUpdated,NextScanPoints])),
  my_assertion(maplist(nonvar_or_ci,[fsi3a,NewOptions,NewReserved])),
    my_assertion(maplist(is_cpoint,NextScanPoints)),
    my_assertion(is_list([foundSofar1(Options)|SofarUpdated])),
  my_assertion(maplist(nonvar_or_ci,[fsi4,NewGrid])),
  my_assertion(maplist(nonvar_or_ci,[fsi5,GH,GV,ID,NewOptions,NewReserved,NextScanPoints])),
    once( (SofarUpdated\==Sofar) ; (Options\==NewOptions) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),!,
    %wqnl(indv(Options)=Indv),   
    
    individuals_list(VM,GH,GV,SofarUpdated,ID,NewOptions,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.

individuals_list(VM,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
  next_options(Options,Options2),
  %set(VM.objs_prev)=VM.objs,
  %set(VM.todo_prev)=VM.program,
  individuals_list(VM,GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints),!.

individuals_list(_VM,GH,GV,Sofar,ID,Options,_Reserved,Points,_Grid,IndvListOut,[]):- 
 %must_det_ll(Options==[]),
 must_be_free(IndvListOut),
  my_assertion(is_list([sofar2(Options)|Sofar])),
  my_assertion(is_list([sofar|Sofar])),
  my_assertion(is_list([points|Points])),
  my_assertion(maplist(is_cpoint,Points)),
   ignore((Points\==[],as_debug(8,print_grid(GH,GV,'leftover_points'+ID,Points)))),
  % maplist(make_point_object(ID,GH,GV,[iz(leftovers)]),Points,IndvList),
  IndvList = [],
  % individuate([just(by_color)],Points,IndvList2),
  IndvList2 = [],
  % make_indiv_object(ID,GH,GV,Points,[iz(combined),iz(leftovers)],LeftOverObj), 
  LeftOverObj = [],
  flatten([Sofar,IndvList,IndvList2,LeftOverObj],IndvListOut),!.

individuals_list(VM,GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P):-!,
  throw(fail(individuals_list(VM,GH,GV,Sofar,ID,Options,Reserved,P,Grid,Sofar,P))),!.

  

fti(VM):- fti(VM,VM.program).

%fti(_,[]).
%fti(VM,_):- VM.points=[], !.
fti(_,[done|_]):- !.

fti(VM,_):-
  Objs = VM.objs,
  length(Objs,Count),
  (member(progress,VM.options); Count > VM.objs_max_len; (statistics(cputime,X), X > VM.timeleft)) -> 
   as_debug(8,(mass(Objs,Mass),
       length(VM.points,PC),
      pt(t([found_mass=(Count,Mass+PC),fsi=VM.program])))),fail.


fti(VM,[Step|Program]):- set(VM.program) = Program, one_fti(VM,Step).

fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step(F), set(VM.program) = Program, call(Step,VM).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), set(VM.program) = Program, Step=..[F|ARGS], apply(F,[VM|ARGS]).

is_fti_step(release_objs_lighter).
is_fti_stepr(_):-fail.

release_objs_lighter(Two,VM):-
  my_partition(less_mass(Two),VM.objs,Smaller,set(VM.objs)),
  maplist(globalpoints,Smaller,Points),
  append_sets([VM.points|Points],set(VM.points)).

less_mass(Mass,Obj):- mass(Obj,M),M<Mass.


mergable_objects(VM,O1,O2):- 
  O1\==O2,
  globalpoints(O1,Ps1),
  globalpoints(O2,Ps2),!,
  mergable_objects_direct(O1,O2,Ps1,Ps2),
  select(O2,VM.objs,Rest),
  \+ (member(O3,Rest), O3\==O1, 
      globalpoints(O3,Ps3),
      mergable_objects_direct(O1,O3,Ps1,Ps3)),!.

mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,dg_line(_)), iz(O2,dg_line(_)), dir_mergeable_list(PsA,PsB,[ne,se,sw,nw,n,s,e,w],[]),!.
mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,hv_line(_)), iz(O2,dg_line(_)), dir_mergeable_list(PsA,PsB,[n,s,e,w],[ne,se,sw,nw]),!.
mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,dg_line(_)), iz(O2,hv_line(_)), dir_mergeable_list(PsA,PsB,[ne,se,sw,nw],[n,s,e,w]),!.
%mergable_objects_direct(O1,O2,PsA,PsB):- iz(O1,hv_line(U)), iz(O2,hv_line(D)), u_d(U,D), adjacent_objs(PsA,PsB).
mergable_objects_direct(O1,O2,PsA,PsB):- \+ iz(O1,dots), mass(O2,1), dir_mergeable_list(PsA,PsB,[n,s,e,w,ne,se,sw,nw],[]),!.
mergable_objects_direct(_O1,_O2,PsA,PsB):- double_touch(PsA,PsB).
mergable_objects_direct(_O1,_O2,PsA,PsB):- double_touch(PsB,PsA).
double_touch(PsA,PsB):- 
  member(C-PB1,PsB),
  is_adjacent_point(PB1,_,PA1),select(C-PA1,PsA,BestA1),
  is_adjacent_point(PB1,_,PA2),member(C-PA2,BestA1),!.

adjacent_objs(PsA,PsB):- 
  member(C-PB1,PsB),
  is_adjacent_point(PB1,_,PA2),member(C-PA2,PsA),!.

dir_mergeable_list(Ps1,Ps2,DirsOK,DirsNotOK):- 
  \+ \+ ((mergable_dir(Ps1,Ps2,OkDir),member(OkDir,DirsOK))),
  \+ ((member(BadDir,DirsNotOK),mergable_dir(Ps1,Ps2,BadDir))).

mergable_dir(Ps1,Ps2,Dir):- member(C-P1,Ps1), member(C-P2,Ps2), is_adjacent_point(P1,Dir,P2),!.

fti(VM,[rows|set(VM.program)]):-
  maplist_n(1,row_to_indiv(VM), VM.grid),
  clearGrid(VM).

clearGrid(VM):- 
  set(VM.points) = [],
  make_grid(VM.h,VM.v,set(VM.grid)).

row_to_indiv(VM,N,Row):-
  localpoints_include_bg([Row],LPoints),
  grid_to_individual([Row],Obj0),  
  override_object([iz(row),-iz(grid),loc_xy(1,N),vis_hv(VM.h,1),grid_size(VM.h,VM.v)],Obj0,Obj1),  
  rebuild_from_localpoints(Obj1,LPoints,Obj),
  addObjects(VM,Obj).

fti(VM,[columns|set(VM.program)]):-
  rot90(VM.grid,Grid90),
  maplist_n(1,column_to_indiv(VM), Grid90),
  clearGrid(VM).

column_to_indiv(VM,N,Row):-
  localpoints_include_bg([Row],LPoints),
  grid_to_individual([Row],Obj0),  
  % a column is a row that was prematurely rotated 270 degrees
  override_object([iz(row),-iz(grid),rotation(rot270),loc_xy(1,N),vis_hv(VM.h,1),grid_size(VM.h,VM.v)],Obj0,Obj1), 
  rebuild_from_localpoints(Obj1,LPoints,Obj),
  addObjects(VM,Obj).
  
  
fti(VM,[point_corners|set(VM.program)]):-
  point_corners(VM),!.

point_corners(VM):-
  each_obj(VM.objs,Obj,
   (point_corners(Obj,Dir,OC-P1),
     is_adjacent_point(P1,Dir,P2),
     select(OC-P1,VM.points_o,Rest),     
     select_always(C-P2,Rest,Points0),C\==OC,
     standalone_point(C-P2,Points0),
     remCPoints(VM,[C-P2]),
     make_point_object(VM.id,VM.h,VM.v,[iz(important)],C-P2,Obj),
     addObjects(VM,Obj))).

select_always(E,B,A):- select(E,B,A)*->true;B=A.

standalone_point(C-P2,Points):- select_always(C-P2,Points,Points0), 
  \+ (is_adjacent_point(P2,Dir,P3), member(C-P3,Points0), \+ is_diag(Dir)),!.

fti(VM,[standalone_dots|set(VM.program)]):-
  standalone_dots(VM),!.

standalone_dots(VM):-
   select(C-P1,VM.points,Rest),     
   standalone_point(C-P1,Rest),
   make_point_object(VM.id,VM.h,VM.v,[iz(important)],C-P1,Obj),
   addObjects(VM,Obj),
   remCPoints(VM,[C-P1]),
   standalone_dots(VM).
standalone_dots(_):-!.



fti(VM,[colormass_merger(Size)|TODO]):-
  colormass_merger(Size,VM),!,
  colormass_merger(Size,VM),!,
  %colormass_merger(3,VM),
  set(VM.program) = TODO.

%colormass_merger(_Size,_VM):-!.
colormass_merger(Size,VM):-
  Objs = VM.objs,
  my_partition(less_mass(Size),Objs,Smaller,Bigger),
  maplist(colormass_merger(VM,Smaller),Bigger).

colormass_merger(VM,Smaller,Bigger):- 
   find_mergeable(VM,Bigger,Smaller,Touches),
   ignore((Touches\==[],
   print_grid(VM.h,VM.v,"Removing...",Touches),
   merge_objs(VM,Bigger,Touches,[iz(combined)],Combined),
   intersection(VM.objs,[Bigger|Touches],_Remove,Kept,_),   
   print_grid(VM.h,VM.v,"Adding...",Combined),
   set(VM.objs) = [Combined|Kept])) .

find_mergeable(_VM,_,[],[]).
find_mergeable(VM,Found,[Next|ScanPoints],[Next|Engulfed]):-    
 mergable_objects(VM,Found,Next),
 find_mergeable(VM,Found,ScanPoints,Engulfed),!.
find_mergeable(VM,Found,[_|ScanPoints],Engulfed):-
 find_mergeable(VM,Found,ScanPoints,Engulfed),!.

fti(VM,[find_touches|set(VM.program)]):-
  %cullObjectsOutsideOfRanges(VM),
  find_touches(VM).

fti(VM,[glyphic|set(VM.program)]):-
  Points = VM.points,
  GH = VM.h,
  GV = VM.v,
  ID = VM.id,
  length(Points,Len),
  make_indiv_object(ID,GH,GV,Points,[mass(Len),vis_hv(GH,GV),iz(combined),iz(grid),iz(image)],Whole),
  %object_indv_id(Whole,_,Iv),
  maplist(make_point_object(ID,GH,GV,[iz(abstract),iz(colorlink)]),Points,IndvList),
  my_append([VM.objs,IndvList,[Whole]],set(VM.objs)),!.

fti(VM,[recalc_sizes,After|TODO]):-
  (recalc_sizes(VM,[After|TODO])
     -> true; set(VM.program)= [After,recalc_sizes|TODO]).

fti(VM,[max_learn_objects(Routine,Max)|set(VM.program)]):-
   set(VM.objs_max_len) = Max,
   set(VM.options)= [max_learn_objects(Routine,Max)|VM.options].

fti(VM,[Routine|set(VM.program)]):- 
   member(max_learn_objects(Routine,Max),VM.options),
   length(VM.objs,Count),
   Count>Max,!,fail.

fti(VM,[remove_used_points|set(VM.program)]):- !, remCPoints(VM,VM.objs).


fti(VM,[call(G)|set(VM.program)]):- call_expanded(VM,G).
fti(VM,[when(G,D)|TODO]):- ((call_expanded(VM,G),!,pt(using_when(G,D)))->R=D;R=call(pt(skipped(G,D)))),
  set(VM.program) = [R|TODO].

fti(VM,[find_colorfull_idioms|set(VM.program)]):- ignore(find_colorfull_idioms(VM.grid)).

%fti(VM,[calc_largest_square_block|set(VM.program)]):-
%  calc_largest_square_block(VM),VM.objs,set(VM.objs),VM.points,set(VM.points)),!.


% Find free points that are contained in objects and individuate them in their own way
fti(VM,[find_contained|set(VM.program)]):-
  show_vm_changes(VM,find_contained,find_contained(VM.h,VM.v,VM.id,VM.objs,set(VM.objs),VM.points,set(VM.points))),!.

fti(VM,[colormass_subshapes|set(VM.program)]):-
  colormass_subshapes(VM),!.

colormass_subshapes(VM):- colormass_subshapes(VM,VM.objs).
colormass_subshapes(_VM,[]):-!.
colormass_subshapes(VM,VMObjs):-
  select(Obj,VMObjs,Next),
  globalpoints(Obj,ContainedPoints),
  H = VM.h, V = VM.v,
  points_to_grid(H,V,ContainedPoints,Grid),
  individuate(H,V,VM.id,[subshape_in_object],Grid,ContainedPoints,WasInside),
  ignore((fail,WasInside =[_,_|_], % two or more
        print_grid(H,V,"colormass_subshapes",WasInside),
        addObjects(VM,WasInside))),
  colormass_subshapes(VM,Next).


% Find object that are contained in objects and individuate them in their own way  (TODO mame this more complete)
% Find free points that are contained in objects and individuate them in their own way
fti(VM,[find_engulfs|set(VM.program)]):-
  % print_ grid(VM.grid),  
  % ( \+ is_input(VM)-> trace ; true),
  find_engulfs(VM).   

fti(VM,[combine_objects|set(VM.program)]):-
  combine_objects(VM),!.

fti(VM,[combine_duplicates|set(VM.program)]):- 
  combine_duplicates(VM),!.

fti(VM,[check_engulfed|TODO]):-
   smallest_first(VM.objs,SmallestFirst),
   set(VM.objs) = SmallestFirst,
   set(VM.program) = [find_engulfs|TODO].

fti(VM,[-(DelOptions)|TODO]):-
  listify(DelOptions,OList),
  my_partition(option_matches(OList),VM.options,_,set(VM.options)),
  my_partition(option_matches(OList),TODO,_,set(VM.program)).

option_matches(List,Arg):- member(E,List),E==Arg.

fti(VM,[+(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program)).

fti(VM,[(OptionsL)|TODO]):-
  is_list(OptionsL), \+ is_group(OptionsL), \+ is_grid(OptionsL),!,
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program)).

fti(VM,[options(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program)).

fti(VM,[macrof(AddTodo)|set(VM.program)]):-
  listify(AddTodo,TodoLst),
  fti(VM,TodoLst).

fti(VM,[macro(AddTodo)|TODO]):-
  listify(AddTodo,TodoL),
  my_append([progress|TodoL],TODO,set(VM.program)).


fti(VM,[DO|TODO]):- 
  fsi(VM,set(VM.robjs),set(VM.grid),set(VM.program),
    VM.h,VM.v,VM.objs,VM.id,[DO|TODO],VM.robjs, VM.points,VM.grid,set(VM.objs),set(VM.points)).

% Thunk(VM -> ArgList)
fsi(VM,ReservedIO,Grid,TODO,H,V,Sofar,ID,[retain_grid(Options)|TODO],ReservedIO,PointsIO,Grid,IndvList,PointsIO):-
  listify(Options,OptionsL),!,
  must_det_ll(individuals_list(VM,H,V,Sofar,ID,OptionsL,ReservedIO,PointsIO,Grid,IndvList,_LeftOver)),
  pt(yellow,PointsIO).

%fti(VM,[by_color(Min,[])|set(VM.program)]):-!.
has_color(C,Point):- C-_ = Point.

fti(VM,[by_color(Min,C)|set(VM.program)]):-
  my_partition(has_color(C),VM.points,ThisGroup,LeftOver),
  ignore(((
   length(ThisGroup,Len),  Len >= Min,
   set(VM.points)=LeftOver,
   make_indiv_object(VM.id,VM.h,VM.v,ThisGroup,[iz(by_color(Min,C))],ColorObj),
   addObjects(VM,ColorObj)))).



/*
fti(VM,[by_color(Min,[])|set(VM.program)]):-!.

fsi(_VM,ReservedIO,Grid,[by_color(Min,Rest)|TODO],H,V,Sofar,ID,[by_color(Min,Options)|TODO],ReservedIO,Points,Grid,NewSofar,NextScanPoints):-
  listify(Options,OptionsL),!,
  select(C,OptionsL,Rest),
  my_partition(=(C-_),Points,ThisGroup,NextScanPoints),
  ((ThisGroup\==[],make_indiv_object(ID,H,V,ThisGroup,[iz(by_color(Min,C))],ColorObj))
    -> NewSofar = [ColorObj|Sofar] 
     ; NewSofar = Sofar).
*/

% dots that have no adjacent points of the same color are gathered first

fsi(_VM,Reserved,Grid,TODO,H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,SofarOut,[]):-
  maplist(make_point_object(ID,H,V,[iz(leftovers),iz(dots),iz(dot)]),Points,IndvList),
  my_append(Sofar,IndvList,SofarOut),!.

fsi(_VM,Reserved,Grid,['dots'|TODO],H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,[Indv|Sofar],Rest):-
   %( (H < 6; V < 6) ;   
   maybe_multivar(C),
    select(C-HV,Points,Rest),  \+ free_cell(C),
   \+ \+ (( is_adjacent_point(HV,_,HV2),
           member(C2-HV2,Rest),ok_color_with(C,C2))),

   make_indiv_object(ID,H,V,[iz(dots),C-HV],Indv),

   meets_indiv_criteria(dots,Indv),!.

% dots may have adjacent points of the same color (because we are in 'dots' mode)
fsi(_VM,Reserved,Grid,['dots'|TODO],H,V,Sofar,ID,['dots'|TODO],Reserved,Points,Grid,[Indv|Sofar],Rest):- !,
   maybe_multivar(C), select(C-HV,Points,Rest),  \+ free_cell(C),
   make_indiv_object(ID,H,V,[iz(dots),C-HV],Indv),
   meets_indiv_criteria(dots,Indv),!.


fsi(VM,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[full|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-!,
  grid_to_individual(Grid,Obj),
  fsi(VM,ReservedO,GridO,OptionsOut,H,V,[Obj|Sofar],ID,TODO,ReservedI,Points,Grid,SofarOut,NextScanPoints).


fsi(_VM,ReservedIO,Grid,TODO,_H,_V,Sofar,_ID,[shape_lib(_Hammer)|TODO],ReservedIO,Points,Grid,Sofar,Points):- true,!.
fsi(_VM,ReservedIO,GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],ReservedIO,Points,Grid,SofarOut,NextScanPoints):- 
  do_shapelib(GridO,OptionsO,H,V,Sofar,ID,[shape_lib(Hammer)|TODO],Points,Grid,SofarOut,NextScanPoints),!.


fsi(_VM,StillReserved,GridO,[use_reserved|TODO],H,V,Sofar,ID,[use_reserved|TODO],Reserved,Points,Grid,SofarOut,NextScanPoints):-
   % length(Reserved,LR), !, %LR < 60, 
   proccess_overlap_reserved(use_reserved,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,Points,NextScanPoints,_Unreserved,StillReserved),
   %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,Obj+ID,Found)),
   !.

fsi(_VM,ReservedO,GridO,TODO,H,V,Sofar,ID,[Obj|TODO],ReservedI,Points,Grid,SofarOut,NextScanPoints):-
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
  ignore((RL>30,pt(shape_lib_expanded(Hammer)=RL))),
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
   find_ogs(OH,OV,OGrid,Grid),
   %must_det_ll
   ((
   localpoints(Obj,OPoints),
   offset_points(OH,OV,OPoints,ObjPoints),
   intersection(ObjPoints,Points,Intersected,LeftOverA,LeftOverB),
   do_leftover(Sofar,LeftOverA,Intersected,Use,Sofar2),
   my_append(Intersected,Use,All),
   list_to_set(All,AllS))), AllS \== [],
   make_indiv_object(ID,H,V,[iz(copy(Name))|AllS],Indiv0), 
   object_indv_id(Obj,_,Iv), override_object(object_indv_id(ID,Iv),Indiv0,Indiv), 
   %make_indiv_object(ID,H,V,Use,Indiv),
   points_to_grid(H,V,LeftOverB,NewGrid),
   %print_grid(Indiv),
   my_append(Sofar2,[Indiv],NewSofar),
   pt(Indiv),
   proccess_overlap_reserved(Name,GridO,NewGrid,ID,H,V,[Obj|RestReserved],NewSofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved),
   nop(debug_indiv(Indiv)).
  
proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,[Obj|Reserved],Sofar,SofarOut,Points,NextScanPoints,[Obj|Unreserved],StillReserved):- fail,
   once((globalpoints(Obj,ObjPoints), 
   \+ color(Obj,black),
   intersection(ObjPoints,Points,Intersected,LeftOverA,LeftOverB),
         do_leftover(Sofar,LeftOverA,Intersected,Use,Sofar2),
         my_append(Intersected,Use,All),
         list_to_set(All,AllS))), AllS \== [],
         make_indiv_object(ID,H,V,[iz(override(Name))|AllS],Indiv0), 
         object_indv_id(Obj,_,Iv), override_object(object_indv_id(ID,Iv),Indiv0,Indiv), 
         %make_indiv_object(ID,H,V,Use,Indiv),
         my_append(Sofar2,[Indiv],NewSofar),
         proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,Reserved,NewSofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).

proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,[Obj|Reserved],Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,[Obj|StillReserved]):-
   proccess_overlap_reserved(Name,GridO,Grid,ID,H,V,Reserved,Sofar,SofarOut,LeftOverB,NextScanPoints,Unreserved,StillReserved).
proccess_overlap_reserved(_Name,Grid,Grid,_ID,_H,_V,[],Sofar,Sofar,NextScanPoints,NextScanPoints,[],[]).

do_leftover(Sofar,[],Intersected,Intersected,Sofar):- !.
%do_leftover([],_,_,_,_):- !,fail.
do_leftover(Sofar,LeftOverA,Intersected,Use,Removed):- select(S,Sofar,Removed), globalpoints(S,SPoints),
    intersection(SPoints,LeftOverA,Usable,[],[]),my_append(Usable,Intersected,Use).

append_sets(Sets,Set):- my_append(Sets,List),list_to_set(List,Set).

% ===============
% release_points
% ===============
fsi(_VM,Reserved,NewGrid,TODO,H,V,Sofar,ID,[release_points|TODO],Reserved,Points,Grid,Sofar,NextScanPoints):-
    globalpoints(Grid,NextScanPoints1),
    globalpoints(Sofar,NextScanPoints2),
    append_sets([Points,NextScanPoints2,NextScanPoints1],NextScanPoints),
    (as_debug(9,print_grid(H,V,'release_points(Sofar)'+ID,Sofar))),
    points_to_grid(H,V,NextScanPoints,NewGrid), 
  !.


fsi(_VM,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[retain(_)|TODO],Reserved,Points,Grid,Sofar,Points):-  
  Sofar = [], !.

fsi(_VM,Reserved,GridO,TODO,_H,_V,Sofar,_ID,[with_grid(Call)|TODO],Reserved,Points,Grid,Sofar,Points):-
    call(Call,Grid,GridO).
fsi(_VM,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[with_points(Call)|TODO],Reserved,Points,Grid,Sofar,PointsO):-
    call(Call,Points,PointsO).
fsi(_VM,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[with_sofar(Call)|TODO],Reserved,Points,Grid,SofarO,Points):-
    call(Call,Sofar,SofarO).

fsi(_VM,Reserved,NNewGrid,TODO,H,V,Sofar,ID,[retain(Option)|TODO],Reserved,_Points,Grid,Retained,NextScanPoints):-
    globalpoints(Grid,NewGPoints),  H> 14, V> 14,
    freeze(W,W>5),filter_indivs(Sofar,[mass(W)];[iz(Option)],Retained1),
    filter_indivs(Retained1, \+ iz(background),Retained),
    as_debug(9,print_grid(H,V,'retained'+ID,Retained)),    
    remove_global_points(Retained,NewGPoints,NextScanPoints),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    %as_debug(9,print_grid(H,V,'newgrid'+ID,NNewGrid)),
    !.

fsi(_VM,Reserved,Grid,TODO,H,V,Sofar,ID,[into_single_hidden|TODO],Reserved,Points,Grid,SofarIndvS1,Points):- !, 
    maplist(globalpoints,Sofar,IndvPointSofar), append_sets(IndvPointSofar,IndvPointsL),
    EIndvPoints=[iz(combined),iz(hidden)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    my_append(Sofar,[Indv],SofarIndvS1),
    meets_indiv_criteria(into_single_hidden,IndvPointsL),!.


fsi(_VM,Reserved,NewGrid,TODO,H,V,Sofar,_ID,[ignore_rest|TODO],Reserved,_Points,_Grid,Sofar,[]):- !, 
    make_grid(H,V,NewGrid).

fsi(_VM,Reserved,Grid,[This,ignore_rest,done|TODO],_H,_V,Sofar,_ID,[just(This)|TODO],Reserved,Points,Grid,Sofar,Points):- !.

/*

fsi(_VM,Reserved,Grid,NewOptions,_H,_V,Sofar,_ID,[by_color|TODO], Reserved, Points, Grid,Sofar, Points):- !,
   my_append(
     [(by_color(Min,[(black), (blue),  (red),   (green),(yellow),
                     (silver),(purple),(orange),(cyan), (brown)]))],TODO,NewOptions).
*/

fsi(_VM,Reserved,Grid,TODO,GH,GV,Sofar,ID,[leftover_as_one|TODO], Reserved, Points, Grid,SofarIndvList, []):- !,
   Points==[] -> SofarIndvList = Sofar ;
   make_indiv_object(ID,GH,GV,Points,[iz(combined),iz(leftover_as_one)],LeftOverObj), verify_object(LeftOverObj),
   my_append(Sofar,[LeftOverObj],SofarIndvList).


fsi(_VM,Reserved,Grid,TODO,H,V,Sofar,ID,[into_single|TODO],Reserved,Points,Grid,[Indv],Points):- !,
    maplist(globalpoints,Sofar,IndvPoints), my_append(IndvPoints,IndvPointsL),
    EIndvPoints=[iz(combined),iz(into_single)|IndvPointsL],
    make_indiv_object(ID,H,V,EIndvPoints,Indv),
    meets_indiv_criteria(into_single_hidden,IndvPoints),!.

%  fsi(_VM,NewReserved,NewGrid,NewOptions,H,V,Sofar,ID,TODO,Reserved,Points,Grid,OutInvdivS,NextScanPoints).
same_lcolor(LargestColor,Obj):- color(Obj,Color),nop(print_grid(Obj)),!,Color==LargestColor.

fsi(_VM,Reserved,Grid,TODO,_H,_V,Sofar,_ID,[IsToBeRewritten|NO],Reserved,Points,Grid,Sofar,Points):-
    atom(IsToBeRewritten), 
    individuation_macros(IsToBeRewritten,Expansion),!,
    listify(Expansion,ExpansionL),my_append(ExpansionL,NO,TODO).


% @TODO will get sub objects later
not_list(G):- \+ is_list(G).

mapgroup(P2,G1,L2):- into_list(G1,L1),!, maplist(P2,L1,L2).
mapgroup(P1,G1):- into_list(G1,L1), !, maplist(P1,L1).

into_list(G,L):- is_list(G),!,L=G.
into_list(G,L):- is_dict(G),!,L = G.objs,my_assertion(is_list(L)).
into_list(I,O):- listify(I,O),!.

assume_vm(_).
:- style_check(-singleton).

addPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  intersection(VM.Prop,ListData,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.Prop,ReallyAdd,set(VM.Prop)).

remPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  intersection(VM.Prop,ListData,ReallyRemove,Keep,PretendToRemove),
  set(VM.Prop) = Keep.
  
addObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  addGPoints(VM, Obj),
  my_append(VM.objs,ReallyAdd,set(VM.objs)).

remObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,ReallyRemove,Keep,PretendToRemove),
  remGPoints(VM, ReallyRemove),
  set(VM.objs) = Keep.

addCPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.points,CPoints,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.points,ReallyAdd,set(VM.points)).

remCPoints(VM,Obj):- is_group(Obj),!,mapgroup(remCPoints(VM),Obj).
remCPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.points,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.points) = Keep.

addRPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,List),
   intersection(VM.points_repair,CPoints,PretendToAdd,Prev,ReallyAdd),
   my_append(VM.points_repair,ReallyAdd,set(VM.points_repair)).

remRPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.points_repair,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.points_repair) = Keep.


addOptions(VM, Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.options,ReallyAdd,set(VM.options)),
  intersection(VM.program,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.program,ReallyAdd,set(VM.program)).

remOptions(VM,Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.options) = Keep,
  intersection(VM.program,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.program) = Keep.


remGPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  Grid = VM.grid,
  remove_global_points(List,Grid,GridO),
  set(VM.grid) = GridO.
  
addGPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  Grid = VM.grid,
  replace_grid_points(List,_,Grid,GridO),
  set(VM.grid) = GridO.
  

:- style_check(+singleton).

in_set(Set,I):- member(E,Set), E=@=I,!.
one_fti(VM,'fourway'):-
  H = VM.h,
  V = VM.v,
  %VM.id = VM.id,
  H > 13, V> 13,
  largest_first(VM.objs,Ordered),     
  grid_to_3x3_objs(VM,Ordered,VM.grid,FourWay1s,Keep,_Repaired),
  must_det_ll((
  intersection(Ordered,Keep,_ReallyKeptO,TookbackO,GaveExtraBackO),
  remObjects(VM,TookbackO),
  addCPoints(VM,TookbackO),
  addObjects(VM,GaveExtraBackO),
  remCPoints(VM,GaveExtraBackO),
  addObjects(VM,FourWay1s),
  remCPoints(VM,FourWay1s),
  addGPoints(VM,FourWay1s),
  %remRPoints(VM,Keep),
  remCPoints(VM,Keep),
  as_debug(8,VM.points\==[],print_grid(VM.h,VM.v,"Leftover Points...",VM.points)),
  as_debug(8,print_grid(VM.h,VM.v,"New Grid...",VM.grid)),
  % set_bgc(LargestColor),  
  %Restart = VM.roptions,!,
  TODO = VM.program,
  set(VM.options) = [progress|VM.options],
  maybe_done(VM,FourWay1s,TODO,NEXT),
  set(VM.program) = NEXT)).


maybe_done(VM,_FourWay1s,_TODO,[done]):- VM.points==[],!. %,make_grid(H,V,set(VM.grid)).
maybe_done(VM,_FourWay1s, TODO,[done|TODO]):-
    points_to_grid(VM.h,VM.v,VM.points,Grid),
    duplicate_term(VM,Dupe),
    individuate([-fourway,complete],Grid,Keep),
  %remRPoints(VM,Keep),
  remCPoints(VM,Keep),
    transfer_onto_dict(Dupe,VM), !,
    maplist(recolor_based_on_grid(VM.grid),Keep,NewKeep),!,
  remObjects(VM,Keep),
  addObjects(VM,NewKeep).

recolor_based_on_grid(Grid,Keep,NewKeep):-
   globalpoints(Grid,GPoints),
   globalpoints(Keep,CPoints),
   freeze(OldC,(OldC = noexisting(Point,_NewC), \+ \+ member(_-Point,CPoints))),
   replace_local_points(GPoints,OldC,CPoints,NewCGPoints),
   rebuild_from_globalpoints(Keep,NewCGPoints,NewKeep),
   grid_size(Grid,H,V),
  print_grid(H,V,recolor_based_on_grid,NewKeep).



remove_from_image(VM,Data):-    
    must_det_ll((remove_global_points(Data,VM.points,Points),
    pt(Points),
    set(VM.points) = Points)),!.
    


overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_ll((remove_global_points(FourWay1s,Sofar,Sofar1),add_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).

fsi(_VM,Reserved,NewGrid,TODO,H,V,Sofar,_ID,['regroup'|TODO],Reserved,Points,Grid,OutInvdivS,Points):- 
  make_indiv_object_list(Grid,H,V,Sofar,OutInvdivS), 
  points_to_grid(H,V,Points,NewGrid).


fsi(_VM,Reserved,Grid,TODO,H,V,Sofar,ID,[solid(rectangle)|TODO],Reserved,_Points,Grid,AllOUT,NextScanPoints):- !, fail,
  globalpoints(Grid,NewGPoints),
  grid_to_segs(Grid,Segs),
  seg_squares(Segs,Squares),
  segs_to_pointlists(Squares,Objs),  Objs\==[],
  maplist(make_indiv_object_list(ID,H,V),Objs,OutInvdivS),
  %maplist(print_grid,OutInvdivS),
  as_debug(8,print_grid(H,V,'Solid-boxes'+ID,OutInvdivS)),!,
  remove_global_points(OutInvdivS,NewGPoints,NextScanPoints),
  my_append(Sofar,OutInvdivS,AllOUT).

    grid_to_segs(Grid,Segs):- must_det_ll((grid_to_segs(1,Grid,SegsL),my_append(SegsL,Segs))).
    
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
    
        
    make_points_list(C,MinH,MinV,MaxH,MaxV,PointsList):-
       findall(C-HV,(between(MinV,MaxV,V),between(MinH,MaxH,H),hv_point(H,V,HV)),PointsList),!.
    
    segs_to_pointlists([],[]).
    segs_to_pointlists([slice(C,Ah,Av,AH,AV)|Segs],[PointsList|Objs]):- % trace,
      H is AH-Ah, V is AV-Av, H>=2,V>=2,
      ( H==V -> Shape = square ; Shape = rectangle),
      make_points_list(C,Ah,Av,AH,AV,PointsListH),
      PointsList=[iz(Shape),iz(solid(Shape))|PointsListH],  !,
      segs_to_pointlists(Segs,Objs).
      
    segs_to_pointlists([_|Segs],Objs):-   segs_to_pointlists(Segs,Objs).


fsi(_VM,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,RestPoints):- 
  merge_shapes(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,RestPoints).

merge_shapes(H,V,ID,Sofar,Option,TODO,OptionsOut,_Grid,Points,NewSofar,Points):- 
  Option = merges(ShapeType1,ShapeType2), copy_term(Option,OptionC),!, 
      selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess), % trace,
      any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
      connection_direction(Option,Dir),
  %rot_left_45(Dir1,DirL),rot_left_45(DirL,Dir90),
  % \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  combine_2objs(ID,H,V,HV1,HV2,[],[iz(Option)],Combined),
  my_append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).


cycle_back_in(OptionC,TODO,[OptionC|TODO]).
cycle_back_in(OptionC,TODO,OptionsOut):-
  length(TODO,N), N2 is floor(N/2),length(LL,N2),my_append(LL,RR,TODO),my_append(LL,[OptionC|RR],OptionsOut).


fsi(VM,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[connects(ShapeType)|TODO],Reserved,Points,Grid,NewSofar,RestPoints):- !,
 fsi(VM,ReservedO,GridO,OptionsOut,H,V,Sofar,ID,[connects(ShapeType,ShapeType)|TODO],Reserved,Points,Grid,NewSofar,RestPoints).

fsi(_VM,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,RestPoints):-   
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
  combine_2objs(ID,H,V,HV1,HV2,[],[iz(Option)],Combined),
  my_append(SofarLess,[Combined],NewSofar),
  cycle_back_in(OptionC,TODO,OptionsOut).

connection_direction(Connected,Dir):-
   arg(_,Connected,ShapeType),
     shape_type_dirs(ShapeType,[Dir|_]). 
    %shape_type_dir(ShapeType2,Dirs2),

fsi(_VM,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,NewSofar,Points):-
  jumps(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,_RestPoints).

jumps(H,V,ID,Sofar,Option,TODO,OptionsOut,Grid,Points,NewSofar,RestPoints):- 
  Option = jumps(ShapeType1), copy_term(Option,OptionC),!,
  selected_from(Sofar,ShapeType1,ShapeType1,HV1,HV2,SofarLess),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), any_gpoint(HV2,C-P2),
  % skip over MP
  is_adjacent_point(P0,Dir,P1), is_adjacent_point(P1,Dir,MP), is_adjacent_point(MP,Dir,P2),is_adjacent_point(P2,Dir,P3),
  any_gpoint(HV1,_-P0), any_gpoint(HV2,_-P3),
  ignore(once((get_color_at_point(Grid,MP,MC),is_color(MC));MC=C)),
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  % TODO: HACK WE MIGHT NOT WANT TO STEAL THE POINT?   
  once(select(MC-MP,Points,RestPoints);Points=RestPoints),
  combine_2objs(ID,H,V,HV1,HV2,[],[iz(Option)],Combined),
  my_append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).


fsi(_VM,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[extends(Option)|TODO],Reserved,Points,Grid,NewSofar,NextScanPoints):- 
  extends(H,V,ID,Sofar,Option,TODO,OptionsOut,Points,NewSofar,NextScanPoints).

extends(H,V,ID,Sofar,ShapeType1,TODO,OptionsOut,Points,NewSofar,NextScanPoints):- 
  Option = extends(ShapeType1), copy_term(Option,OptionC),!,
  select(HV1,Sofar,SofarLess),isz(HV1,ShapeType1),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,MP),select(MC-MP,Points,ScanPoints),
  \+ (is_adjacent_point(P1,_,P2), is_adjacent_point(P2,_,MP),any_gpoint(HV1,_-P2)),
  all_individuals_near(Dir,Option,C,[MC-MP],ScanPoints,NextScanPoints,IndvPoints),
  combine_2objs(ID,H,V,HV1,[],IndvPoints,[iz(Option)],Combined),
  my_append(SofarLess,[Combined],NewSofar),cycle_back_in(OptionC,TODO,OptionsOut).



    combine_2objs(ID,H,V,HV1,HV2,NewPoints,IPROPS,Combined):-
      globalpoints(HV1,GP1), globalpoints(HV2,GP2),      
      % indv_props(HV1,Props1),indv_props(HV2,Props2),
      Props1=[],Props2=[],
      my_append([GP1,GP2,NewPoints],GPoints), my_append([Props1,Props2,IPROPS],Info),
      make_indiv_object(ID,H,V,GPoints,Info,Combined).


fsi(_VM,FinalReserve,Grid,TODO,H,V,Sofar,ID,[cycle_shapes(Shapes)|TODO],Reserved,Points,Grid,IndvList,LeftOver):-
   cycle_s(FinalReserve1,H,V,Sofar,ID,Shapes,Reserved,Points,Grid,IndvList1,LeftOver1),
   cycle_s(FinalReserve,H,V,IndvList1,ID,Shapes,FinalReserve1,LeftOver1,Grid,IndvList,LeftOver).

    cycle_s(FinalReserve,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,LeftOver):-    
        my_assertion(maplist(is_cpoint,Points)),
        my_assertion(is_list([sofar1(Options)|Sofar])),
        fsi(_VM,NewReserved,NewGrid,TODO,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,FoundSofar,NextScanPoints),
        my_assertion(maplist(nonvar_or_ci,[fsi,NewReserved,NewGrid,TODO,FoundSofar,NextScanPoints])),
        my_assertion(maplist(is_cpoint,NextScanPoints)),
        my_assertion(is_list([foundSofar1(Options)|FoundSofar])),
        ((FoundSofar\==Sofar) ; (NextScanPoints\== Points); (NewGrid\=@= Grid); (NewReserved\=@= Reserved)),
        cycle_s(FinalReserve,GH,GV,FoundSofar,ID,TODO,NewReserved,NextScanPoints,NewGrid,IndvList,LeftOver),!.
    cycle_s(FinalReserve,GH,GV,Sofar,ID,Options,Reserved,Points,Grid,IndvList,NextScanPoints):-  
        next_options(Options,Options2),!,
        cycle_s(FinalReserve,GH,GV,Sofar,ID,Options2,Reserved,Points,Grid,IndvList,NextScanPoints).
    cycle_s(Reserved,_GH,_GV,Sofar,_ID,_Options,Reserved,Points,_Grid,Sofar,Points).


fsi(VM,Reserved,Grid,OptionsOut,H,V,Sofar,ID,[Option|TODO],Reserved,Points,Grid,OUT,NextScanPoints):- 
   ( Option \==dots), 
   find_one_individual(VM,H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Indv,NextScanPoints),
   my_append(Sofar,[Indv],OUT).

find_one_individual(VM,H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints):-
   find_one_individual3(VM,H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints),!.
find_one_individual(VM,H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints):-
   find_one_individual2(VM,H,V,ID,Sofar,Option,TODO,OptionsOut,Reserved,Points,Grid,Obj,NextScanPoints).

find_one_individual3(VM,H,V,ID,_Sofar,Option,TODO,OptionsOut,_Reserved,Points,_Grid,Obj,NextScanPoints):-
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,Rest1),
    %ScanPoints = Rest1,
    ((adjacent_point_allowed(C,HV2,Dir,HV3),select(C2-HV3,Rest1,ScanPoints), ok_color_with(C,C2));
     (allowed_dir(Option,Dir2),Dir2\=Dir, adjacent_point_allowed(C,HV2,Dir2,HV3),select(C-HV3,Rest1,ScanPoints))),    
    %maybe_multivar(C), 
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2,C-HV3],ScanPoints,NextScanPoints,IndvPoints), 
    make_indiv_object(ID,H,V,IndvPoints,[iz(ShapeType),birth(individual3(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  cycle_back_in(OptionC,TODO,OptionsOut).

find_one_individual2(VM,H,V,ID,_Sofar,Option,TODO,OptionsOut,_Reserved,Points,_Grid,Obj,NextScanPoints):-
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
  select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), 
    make_indiv_object(ID,H,V,IndvPoints,[iz(ShapeType),birth(individual2(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  cycle_back_in(OptionC,TODO,OptionsOut).


point_groups_by_color(Option,[IndvPoints|Groups],Points,Rest):-    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), !,
    point_groups_by_color(Option,Groups,NextScanPoints,Rest).
point_groups_by_color(_Option,[],Points,Points).

shape_min_points(VM,Shape,MinShapeO):- MS = VM.objs_min_mass, number(MS), length(MinShape,MS),
  !,append(MinShape,_,MinShapeO),!,shape_min_points0(Shape,MinShapeO).
shape_min_points(_VM,Shape,MinShapeO):-shape_min_points0(Shape,MinShapeO).

shape_min_points0(colormass,[_,_,_,_,_|_]):-!.
shape_min_points0(rectangle,[_,_,_,_|_]):-!.
shape_min_points0(diamonds,[_,_,_,_|_]):-!.
shape_min_points0(_,[_,_|_]).
%  shape_min_points(VM,_,_).


/*
find_one_in divi dual(GH,GV,Sofar,ID,solid(Option),Reserved,Points,Grid,[iz(solid(Option))|Indv],NextScanPoints):- fail, !,
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

fsi(_VM,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
   
   find_one_indiv idual(H,V,Sofar,ID,Option,Reserved,Points,Grid,IndvPoints,NextScanPoints),
   make_indiv_object(ID,H,V,[birth(Option),iz(Option)|IndvPoints],Indv),!,
   my_append(Sofar,[Indv],OUT).
%fsi(_VM,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints):- 
%  nop(fsi(_VM,Reserved,Grid,[Option|Options],H,V,Sofar,ID,[Option|Options],Reserved,Points,Grid,OUT,NextScanPoints)).
*/


fsi(_VM,Reserved,Grid,Options,_H,_V,Sofar,_ID,[Option|Options],Reserved,Points,Grid,Sofar,Points):-
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

unused_filtered_point(C,HV):- maybe_multivar(C), t_l:id_cells(_ID,Points),% select(_-HV,Points,Rest), 
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
    ; (my_append(Indv,New,IndvNew),
        all_individuals_near(Dir,Options,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_Dir,_Options,_C,_From,[],[],[]):-!.
individuals_near(Dir,Options,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Dir,Options,C,E,From),!,
  individuals_near(Dir,Options,C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(Dir,Options,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Dir,Options,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(_Dir_,Options,C,C2-E,List):- allowed_dir(Options,Dir), adjacent_point_allowed(C,E2,Dir,E), member(C2-E2,List),ok_color_with(C2,C).

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
  my_append(IndvL,BGIndvS,IndvOB).

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
unraw_inds2(VM,Options,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv),
   reverse(Indv,IndvR), IndvR\=@=IndvS,
   unraw_inds2(VM,Options,IndvR,IndvO).
*/

% Diag of 3 or more
  /*
unraw_inds2(VM,Options,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,iz(diagonal),C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  % minimum 4 findall(C-CP,member(C-CP,NewGroup),LL),LL=[_,_,_,_|_],
  unraw_inds2(VM,Options,[NewGroup|RRestO],IndvO).
*/

% Diag of 2 or more
unraw_inds2(VM,Options,IndvS,IndvO):-  % fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),\+ get_bgc(C),
  is_diag(Dir),fail,
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  finish_grp(C,[C-Point2,iz(diagonal),C-Point1],Point2,Dir,Rest2,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_nav(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(VM,Options,[NewGroup|RRestO],IndvO).



unraw_inds2(VM,Options,IndvS,IndvO):-  %fail,
  single_point(C-Point1,IndvS,Rest1), nonvar_or_ci(C), non_free_fg(C),
  single_point(C2-Point2,Rest1,Rest),fail,
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(VM,Options,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(VM,Options,IndvS,IndvO):- fail,
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(VM,Options,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(VM,Options,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Dir,Options,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(VM,Options,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_VM,_,IndvS,IndvS).




merge_indivs(IndvA,IndvB,BetterA,BetterB,BetterC):-
  my_append(IndvA,IndvB,IndvSU),list_to_set(IndvSU,IndvS),
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
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
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




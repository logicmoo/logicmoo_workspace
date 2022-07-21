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
 %i:- fav_i(X),i(X).   %i(GridIn):- i2([complete],GridIn).
ig:- fav_i(X),ig(X). ig(GridIn):- i2(complete,GridIn).
iq:- fav_i(X),ig(X). iq(GridIn):- iq(complete,GridIn).
iL:- fav_i(X),iL(X). iL(GridIn):- i2([shape_lib(as_is),complete],GridIn).

:- discontiguous is_fti_step/1.
:- discontiguous is_fti_stepr/1.

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
  %indiv_grid_pings(GridIn),
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
  %ensure_fti(Grid,VM),
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

%:- discontiguous(fsi/14).
:- discontiguous(fti/2).
:- discontiguous(one_fti/2).
   
:- dynamic(is_unshared_saved/2).
:- dynamic(is_shared_saved/2).

individuation_macros(out, complete).
individuation_macros(in, complete).



individuation_macros(subshape_in_object, complete).

individuation_macros(train_mono_in_in, complete).
individuation_macros(train_mono_in_out, complete).
individuation_macros(train_mono_out_out, complete).

individuation_macros(in_in, complete).
individuation_macros(out_in, complete).
individuation_macros(out_out, complete).
individuation_macros(in_out, complete).

% if there are 10 or less of a color dont group the whole color (It/they must be special points)
individuation_macros(by_color, X):-
   findall(by_color(10,Color),enum_colors(Color),X).

individuation_macros(force_by_color, X):-
   findall(by_color(1,Color),enum_colors(Color),X).

individuation_macros(subshape_in_object, [
   subshape_both,   
   %progress,
   non_diag, % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   by_color, % any after this wont find individuals unless this is commented out
   not_done % hopefully is never ran outside subshape_in_object !
   ]).

individuation_macros(subshape_main, [
   subshape_both,   
   %progress,
   non_diag % like colormass but guarenteed it wont link diagonals but most ikmportant ti doesnt look for subshapes
   %by_color % any after this wont find individuals unless this is commented out
   %not_done % hopefully is never ran outside subshape_in_object !
   ]).

% never add done to macros
individuation_macros(subshape_both, [
  % glean_grid_patterns,
   shape_lib(hammer), % is a sanity test/hack
   non_diag,
   maybe_glyphic,
   hv_line(h),  
   dg_line(d), dg_line(u), 
   hv_line(v),  
   diamonds,
   colormass,
   %show_neighbor_map,
   %indiv_grid_pings,
   %+recalc_sizes,
   connects(dg_line(_),dg_line(_)),
   connects(hv_line(_),dg_line(_)),
   connects(hv_line(_),hv_line(_)),
   jumps,% run the "jumps" macro
   %merges(Z,Z), % merge lines into square
   merges(hv_line(_),hv_line(_)),
   merges(dg_line(_),dg_line(_)),

   point_corners,
   by_color,
   standalone_dots
   %connects(X,X)
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
    when((len(points)=<30),standalone_dots),    
    when((len(points)>30),by_color)]).


% the typical toplevel indivduator
individuation_macros(complete, [
    %maybe_repair_in_vm(repair_repeats),
    shape_lib(as_is),
    fourway,
    find_colorfull_idioms,
    maybe_glyphic,
    if_done,
    complete_broken_lines,
    complete_occluded,
    maybe_1_3rd_mass,
    %shape_lib(as_is),    
    %non_diag,
    %colormass,    
    standard,%colormass_merger(3), % call the standard things done in most indiviguators    
    point_corners,
    reduce_population, % @TODO DISABLED FOR TESTS    %altro,
    colormass_subshapes, % find subshapes of the altro
    %when((colors(i.points,Cs),len(Cs)<2),standalone_dots), % any after this wont find individuals unless this is commented out
    colormass_merger(2),
    when((len(points)=<30),standalone_dots),
    standalone_dots,
    leftover_as_one, % any after this wont find individuals unless this is commented out    
   done % stop processing
 ]).

% the standard things done in most indiviguators
individuation_macros(standard, [
    %fourway, % find fold patterns 
    %recalc_sizes,
    std_shape_lib, % stuff that was learned/shown previously
   +max_learn_objects(colormass,30),
   +max_learn_objects(non_diag,30),
   +max_learn_objects(hv_line(_),30),
   +max_learn_objects(dg_line(_),30),
    %non_diag,
    %+recalc_sizes, % blobs of any shape that are the same color  
    % @TODO DISABLED FOR TESTS   colormass_subshapes, % subdivide the color masses .. for example a square with a dot on it
    subshape_main, % macro for sharing code with "subshape_in_object"
    connects(jumps(X),jumps(X)), % connected jumps    
    merges(Z,Z), % merge objects of identical types (horizontal lines become solid squares)   
    find_touches,
    find_contained, % mark any "completely contained points"
    % call(trace),
    find_engulfs, % objects the toplevel subshapes detector found but neglacted containment on     
    combine_duplicates, % make sure any objects are perfectly the same part of the image are combined       
    
    not_done]).

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
  solid(non_diag), % chat that looks for solid rectangles
  
  %polygons,%shape_lib(non_diag), %shape_lib(all), %shape_lib(hammer),
  
  % colormass, %hv_line(v), hv_line(h), %dg_line(u),dg_line(d), %CS,
  all
  % line(_),dg_line(_), % release_points, all, %into_single_hidden,oldway %retain(solid(non_diag)), % shapes, %into_single_hidden,
  ]). 

% ?- print_grid(gridFn(X)).

fix_indivs_options(O,L):-is_list(O),maplist(fix_indivs_options,O,OL),my_append(OL,L).
fix_indivs_options(G,[G]):- var(G),!.
fix_indivs_options(I,O):- atom(I),individuation_macros(I,M),!,fix_indivs_options(M,O),!.
fix_indivs_options(macro(I),[macro(O)]):- fix_indivs_options(I,O).
fix_indivs_options(detect(O),[detect(O)]):-!.
fix_indivs_options(O,[detect(O)]):- is_gridoid(O),!.
fix_indivs_options(I,O):-listify(I,O),!.


is_fti_step(maybe_1_3rd_mass).

maybe_1_3rd_mass(VM):-
  mass(VM.grid,Mass),
  colors(VM.grid,[cc(C,Size)|_Colors]),
  Area is VM.h * VM.v,
  Size * 3 > Area,
  Size * 3 > Mass,
  Min is Size/2,
  my_partition(has_color(C),VM.points,ThisGroup,LeftOver),
  ignore(((
   length(ThisGroup,Len),  Len >= Min,
   set(VM.points)=LeftOver,
   make_indiv_object(VM,ThisGroup,[iz(by_color(Min,C))],ColorObj),
   addObjects(VM,ColorObj)))).







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

:- dynamic(individuated_cache/3).
:- retractall(individuated_cache(_,_,_)).

individuate(VM):-  individuate(VM.roptions, VM),!.
individuate(ROptions,VM):-  individuate(ROptions,VM,_),!.
individuate(_ROptions,Grid,IndvS):- Grid==[],!,IndvS=[].
individuate([ROption],GridIn,IndvS):- !, individuate(ROption,GridIn,IndvS).

individuate(ROptions,GridIn,IndvS):- individuation_macros(ROptions,R), individuated_cache(R,GridIn,IndvS),!.
individuate(ROptions,GridIn,IndvS):- individuated_cache(ROptions,GridIn,IndvS),!.
individuate(ROptions,GridIn,IndvS):- 
  do_individuate(ROptions,GridIn,IndvS),!,  
  nop(asserta(individuated_cache(ROptions,GridIn,IndvS))),!.

do_individuate(ROptions,GridInIn,IndvS):-
   must_be_free(IndvS),
   (is_map(GridInIn) -> (VM = GridInIn, GridIn=VM.grid ) ; GridInIn=GridIn),
   into_points_grid(GridIn,_Points,Grid),
   grid_to_id(Grid,ID),
  my_assertion(\+ is_grid(ID)),
   quietly(grid_size(Grid,GH,GV)), 
   pt(yellow,ig(ROptions,ID)=(GH,GV)),
   individuate7(VM,GH,GV,ID,ROptions,Grid,IndvS),
   !.
   %VM.grid_out

% tiny grid becomes a series of points
%individuate(GH,GV,ID,ROptions,_Grid,Points,IndvS):- \+ is_list(ROptions), is_glyphic(Points,GH,GV), individuate_glyphic(GH,GV,ID,Points,IndvS),!.
%individuate(GH,GV,ID,whole,_Grid,Points,IndvS):-  individuate_whole(GH,GV,ID,Points,IndvS),!.
individuate7(VM,GH,GV,ID,ROptions,GridIn,IndvS):-
      (var(VM) -> into_fti(ID,ROptions,GridIn,VM) ; true),
      %VM.points = Points,
      %individuation_reserved_options(ROptions,Reserved,NewOptions),
      %trace,
      %ensure_fti(GH,GV,ID,Grid,[],Reserved,NewOptions,Points,VM),   
      set_vm(VM),
      
      %individuals_raw(VM,GH,GV,ID,NewOptions,Reserved,Points,Grid,IndvSRaw),
      run_fti(VM), 
      IndvSRaw = VM.objs,
      
  %as_debug(9,ptt((individuate=IndvSRaw))),
      make_indiv_object_list(ID,GH,GV,IndvSRaw,IndvS1),
      combine_objects(IndvS1,IndvS2),
      list_to_set(IndvS2,IndvS),
      
      once((delistify_single_element(ROptions,NamedOpts),
       save_grouped(individuate(ID,NamedOpts),IndvS))),!,
      nop(print_info(IndvS)).
      

into_points_grid(GridIn,Points,Grid):-
   (var(GridIn)-> trace ; true),
   globalpoints(GridIn,Points),
   into_grid(GridIn,Grid),!.


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
 % rb_new(HM),duplicate_term(HM,Hashmap),
  max_min(H,V,Max,_Min),
  listify(ROptions,OOptions),
  pt(yellow,ig(ROptions,ID)=(H,V)),
  ArgVM = vm{
   % parent VM
   %training:_,
     %compare:_, 
   grid_out:_,  last_key:_,  
   % Options and TODO List (are actually same things)
   program_i:Options, options:OOptions, roptions:ROptions, %todo_prev:[],
   % how much time is left before we turn on debugger
   timeleft:Timeleft, objs_max_len:Max, objs_min_mass:_, objs_max_mass:_,
   % Grid and point representations
   grid:Grid,
   allocated_points:[],
   points:Points,
   props:_,
   can_repair:true,
   changed:_,% solution:_,neededChanged
   neededChanged:_, repaired:_,
   full_grid:_, 
   % Original copies of Grid and point representations
   grid_o:Grid, 
   rule_dir: ROptions,
   points_o:Points, % repaired:[],
   % objects found in grid and object that are reserved to not be found
   objs:[],  robjs:Reserved, % objs_prev:[],
   % Notes and debug info
   type:grid, % notes:_, debug:_,
   % height width and lookup key for image
   h:H, v:V, id:ID},
   %ignore(VM>:<ArgVM),
   (var(VM) -> ArgVM=VM ; transfer_missing(ArgVM,VM)),
   set_vm(VM),
   %(var(VM) -> (fix_test_name(ID,TestID,_), make_training_hints(TestID,ArgVM,HintedVM), HintedVM = VM) ; true),
   %(nb_current('$vm_pair',Shared)-> transfer_missing(Shared,VM) ; true),
    true)),
   %b_set_dict(objs,VM,[]),
   %set(VM.current_i) = VM
   
   !.


into_grid_d(Grid,Grid_D):- most_d_colors(Grid,_CI,Grid_D),!.
%must_det_ll((subst001(Grid,black,wbg,In0), most_d_colors(In0,_CI,Grid_D))),

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



is_fti_step(complete_occluded).
complete_occluded(_VM):- dmsg(todo(complete_occluded)).


is_fti_step(complete_broken_lines).

row_of_5(P1,P2,P3,P4,P5):- 
  n_s_e_w(Dir),
  is_adjacent_point(P1,Dir,P2),
  is_adjacent_point(P2,Dir,P3),
  is_adjacent_point(P3,Dir,P4),
  is_adjacent_point(P4,Dir,P5).

complete_broken_lines(VM):- complete_broken_lines(VM.points,set(VM.points)).
complete_broken_lines(Ps,Done):-    
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
  \+ member(C-P3,G2), 
  select(M-P3,G2,G3), M\==C, 
   select(C-P4,G3,G4), 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P3|Ps],Done).
complete_broken_lines(Ps,Done):-    
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
   select(C-P3,G2,G3),
  \+ member(C-P4,G2), select(N-P4,G3,G4), N\==C, 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P4|Ps],Done).

complete_broken_lines(Ps,Done):-
   select(C-P1,Ps,G1), row_of_5(P1,P2,P3,P4,P5),
   select(C-P2,G1,G2),
  \+ member(C-P3,G2), select(M-P3,G2,G3), M\==C, 
  \+ member(C-P4,G2), select(N-P4,G3,G4), N\==C, 
   member(C-P5,G4),!,
   %write(C-P3),
  complete_broken_lines([C-P3,C-P4|Ps],Done).

complete_broken_lines(Done,Done).

%unraw_inds(_VM,I,O):- I=O,!.
unraw_inds(_VM,I,O):- I=O,!.

unraw_inds(VM,IndvS,IndvOO):-   
  largest_first(IndvS,Indv),
  reverse(Indv,IndvR),
  %test_cond_or(indiv(min(SZ)),1),
  %SZ=3,
  check_minsize(_,IndvR,IndvR2),
  unraw_inds2(VM,_,IndvR2,IndvR3),!,
  check_minsize(_,IndvR3,IndvOO).


run_fti(VM):- 
  %is_rbtree(VM),
  %guitracer,!,
  must_det_ll(get_kov(program_i,VM,Code)),!,
  must_det_ll(run_fti(VM,Code)).

run_fti(_,[]):- !.
run_fti(_,[Done|TODO]):- ( \+ done \= Done ), !, wdmsg(done_run_fti([Done|TODO])),set_vm(program_i,[done]),!.
run_fti(VM,[if_done|TODO]):- !, (VM.points==[] -> (wdmsg(if_done),set_vm(program_i,[if_done])) ; run_fti(VM,TODO)).
run_fti(VM,[not_done|TODO]):- !, run_fti(VM,TODO).
run_fti(VM,[recalc_sizes|TODO]):- !, run_fti(VM,TODO).

run_fti(VM,[F|TODO]):- 
  %must_det_ll(fti(VM,[F|TODO])),!, 
  show_vm_changes(VM,F, must_det_ll(fti(VM,[F|TODO]))),
  must_det_ll(get_kov(program_i,VM,Code)),!,
    
  ([F|TODO]=@=Code -> 
    (% pt(blue,fti=[F|TODO]), pt(blue,code=Code), 
     set(VM.program_i) = TODO, 
       run_fti(VM,TODO)) ; run_fti(VM)),!.


maybe_four_terse(L,F=N):- length(L,N),N>4,!,length(F,4),append(F,_,L),!.
maybe_four_terse(L,L):-!.
%fti(VM,_):- VM.points=[], !.
fti(_,[]):- !.
fti(VM,[not_done|TODO]):- !, fti(VM,TODO).
fti(VM,[recalc_sizes|TODO]):- !, fti(VM,TODO).
fti(_,[Done|TODO]):-  ( \+ done \= Done ), !, wdmsg(done_fti([Done|TODO])),!.
%fti(VM,_):- VM.points==[], !.

print_vm_debug_objs(_VM):- !.
print_vm_debug_objs(VM):- 
  Objs = VM.objs,  
  length(Objs,Count),
  as_debug(8,(mass(Objs,Mass), length(VM.points,PC),
      maybe_four_terse(VM.program_i,Four),
      pt(t([obj/obj_mass=(Count/Mass),unprocessed_points=PC,roptions=VM.roptions,fsi=Four])))).

fti(VM,_):-
  Objs = VM.objs,  
  length(Objs,Count),
  ((member(progress,VM.options); Count > VM.objs_max_len; (statistics(cputime,X), X > (VM.timeleft)))) -> 
   print_vm_debug_objs(VM),fail.


fti(VM,_):- fail, member(recalc_sizes,VM.options), once(recalc_sizes(VM)), fail.
fti(VM,[recalc_sizes,After|TODO]):- After == recalc_sizes,!, fti(VM,[recalc_sizes|TODO]).
fti(VM,[recalc_sizes|TODO]):- nop(recalc_sizes(VM)),!, fti(VM,TODO).
%fti(_VM,[recalc_sizes]):- !.
/*

fti(VM,[recalc_sizes,After|TODO]):- 
  (recalc_sizes(VM,[After|TODO])
     -> true; (set(VM.program_i)= [After,recalc_sizes|TODO])),!.
*/
fti(VM,[max_learn_objects(Routine,Max)|set(VM.program_i)]):- fail,
   set(VM.objs_max_len) = Max,
   set(VM.options)= [max_learn_objects(Routine,Max)|VM.options].

fti(VM,[Routine|set(VM.program_i)]):-  fail,
   member(max_learn_objects(Routine,Max),VM.options),
   length(VM.objs,Count),
   Count>Max,!,fail.

fti(VM,[when(G,D)|TODO]):- ((call_expanded(VM,G),!,pt(using_when(G,D)))->R=D;R=call(nop(pt(skipped(G,D))))),
  set(VM.program_i) = [R|TODO].

fti(VM,[call(G)|TODO]):-   set(VM.program_i) = TODO, !, my_submenu_call(call_expanded(VM,G)).

fti(VM,[-(DelOptions)|TODO]):-
  listify(DelOptions,OList),
  my_partition(option_matches(OList),VM.options,_,set(VM.options)),
  my_partition(option_matches(OList),TODO,_,set(VM.program_i)).

option_matches(List,Arg):- member(E,List),E==Arg.

fti(VM,[+(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[(OptionsL)|TODO]):- fail,
  is_list(OptionsL), \+ is_group(OptionsL), \+ is_grid(OptionsL),!,
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[options(AddOptions)|TODO]):-
  listify(AddOptions,OptionsL),
  my_append(VM.options,OptionsL,set(VM.options)),
  my_append(TODO,OptionsL,set(VM.program_i)).

fti(VM,[macrof(AddTodo)|set(VM.program_i)]):-
  listify(AddTodo,TodoLst),
  fti(VM,TodoLst).

fti(VM,[macro(AddTodo)|TODO]):-
  listify(AddTodo,TodoL),
  my_append([progress|TodoL],TODO,set(VM.program_i)).
  
fti(VM,[Step|Program]):- set(VM.program_i) = Program, one_fti_step(Step), !, my_submenu_call(one_fti(VM,Step)),!.
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_step(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_submenu_call(call(Step,VM)).
fti(VM,[Step|Program]):- functor(Step,F,_), is_fti_stepr(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, Step=..[F|ARGS], !, my_submenu_call(apply(F,[VM|ARGS])).
fti(VM,[Step|Program]):- functor(Step,F,_), ping_indiv_grid(F), \+ warn_missing_arity(Step,1), set(VM.program_i) = Program, !, my_submenu_call(call(Step, VM.grid)).

%fti(VM,[Step|Program]):- \+ missing_arity(Step,0), !, set(VM.program_i) = Program, my_submenu_call(call(Step)).

fti(VM,_):- 
  Objs = VM.objs,
  length(Objs,Count),
  (fail;(member(progress,VM.options); Count > VM.objs_max_len; (statistics(cputime,X), X > (VM.timeleft)))) -> 
   print_vm_debug_objs(VM),fail.

fti(VM,[F|TODO]):-
   wdmsg(fti_miss(F)),
   set(VM.program_i)= TODO.

% fti(_VM,[F|_]):- wdmsg(F),!. % trace,fail.


one_fti_step(Name):- is_thing_or_connection(Name).
one_fti_step(Name):- clause(one_fti(_VM,Step),_),nonvar(Step),Name=Step.

i_step(Name):- no_repeats(Name,i_step0(Name)).
i_step0(Name):- clause(fti(_VM,[Step|_]),_),nonvar(Step),Name=Step.
i_step0(Name):- one_fti_step(Name).
i_step0(Name):- is_fti_step(Name).
i_step0(Name):- is_fti_stepr(Name).
i_step0(Name):- ping_indiv_grid(Name).
i_step0(Name):- individuation_macros(Name,_).


warn_missing_arity(Step,N):- missing_arity(Step,N) -> wdmsg(warning(missing_arity(Step,N))).

is_fti_step(release_objs_lighter).
is_fti_stepr(_):-fail.

is_fti_step(indiv_grid_pings).

indiv_grid_pings(Grid):- is_grid(Grid),!,forall(ping_indiv_grid(P1),ignore(catch(call(P1,Grid),E, ((E == '$aborted')->throw(E);fail)))),!.
indiv_grid_pings(VM):- get_kov(grid,VM,Grid),!, indiv_grid_pings(Grid).
indiv_grid_pings(VM):- into_grid(VM,G),!,indiv_grid_pings(G).



release_objs_lighter(Two,VM):-
 ignore(( fail,
  my_partition(less_mass(Two),VM.objs,Smaller,set(VM.objs)),
  maplist(globalpoints,Smaller,Points),
  append_sets([VM.points|Points],set(VM.points)))).

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
mergable_objects_direct(O1,O2,PsA,PsB):- \+ iz(O1,dot), mass(O2,N),N==1,dir_mergeable_list(PsA,PsB,[n,s,e,w,ne,se,sw,nw],[]),!.
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

one_fti(VM,'rows'):-
  maplist_n(1,row_to_indiv(VM), VM.grid),
  set(VM.points) = [].

row_to_indiv(VM,N,Row):-
  same([Row],Rot90),
  localpoints_include_bg(Rot90,LPoints),
  offset_points(1,N,LPoints,GPoints),
  %grid_to_individual([Row],Obj0),  
  % a column is a row that was prematurely rotated 270 degrees
  make_indiv_object(VM,GPoints,[iz(h_line(h)),rotated(same),loc_xy(1,N),vis_hv(VM.h,1),grid_size(VM.h,VM.v)],Obj),
  addObjects(VM,Obj).

one_fti(VM,'columns'):-
  rot90(VM.grid,Grid90),
  maplist_n(1,column_to_indiv(VM), Grid90),
  set(VM.points) = [].

column_to_indiv(VM,N,Row):-  
  rot90([Row],Rot90),
  localpoints_include_bg(Rot90,LPoints),
  offset_points(N,1,LPoints,GPoints),
  %grid_to_individual([Row],Obj0),  
  % a column is a row that was prematurely rotated 270 degrees
  make_indiv_object(VM,GPoints,[iz(h_line(v)),rotated(same),loc_xy(N,1),center(N,2),vis_hv(1,VM.v),grid_size(VM.h,VM.v)],Obj),
  addObjects(VM,Obj).
  
  
is_fti_step(point_corners).
point_corners(VM):-
  each_obj(VM.objs,Obj,
   (point_corners(Obj,Dir,OC-P1),
     is_adjacent_point(P1,Dir,P2),
     select(OC-P1,VM.points_o,Rest),     
     select_always(C-P2,Rest,Points0),C\==OC,
     standalone_point(C-P2,Points0),
     remCPoints(VM,[C-P2]),
     maybe_make_point_object(VM,[iz(important)],C-P2,Obj),
     addObjects(VM,Obj))).

select_always(E,B,A):- select(E,B,A)*->true;B=A.

standalone_point(C-P2,Points):- select_always(C-P2,Points,Points0), 
  \+ (is_adjacent_point(P2,Dir,P3), member(C-P3,Points0), \+ is_diag(Dir)),!.


is_fti_step(standalone_dots).
% standalone_dots that have no adjacent points of the same color (could be gathered first)
standalone_dots(VM):-
   select(C-P1,VM.points,Rest),     
   standalone_point(C-P1,Rest),
   maybe_make_point_object(VM,[iz(important)],C-P1,Obj),
   addObjects(VM,Obj),
   remCPoints(VM,[C-P1]),
   standalone_dots(VM).
standalone_dots(_):-!.

is_fti_step(remaining_points).
% remaining_points may have adjacent points of the same color (because we are in 'remaining_points' mode)
remaining_points(VM):-
  maplist(maybe_make_point_object(VM,[iz(leftovers),iz(remaining_points),iz(dot)]),VM.points,IndvList),
  addObjects(VM,IndvList),
  set(VM.points) =[],!.



/*
fti(VM,[colormass_merger(Size)|TODO]):-
  colormass_merger(Size,VM),!,
  colormass_merger(Size,VM),!,
  %colormass_merger(3,VM),
  set(VM.program_i) = TODO.
*/

is_fti_step(colormass_merger).
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


%individuate_points(Grid,How,Results):- globalpoints(Grid,Points), grid_size(Grid,H,V),individuate_points0(H,V,Grid,Points,How,Results).
%individuate_points0(H,V,Grid,Points,How,Results):- How = by_row,Results = Grid,member(
%individuate_points0(H,V,Grid,Points,How,Results):- How = by_col, rot90(Grid,Grid90), Results = Grid.
%individuateR(Grid,complete,Objs):- individuate(complete,Grid,Objs).
%individuateR(Grid,Name,Objs):- var(Grid),!,arc_grid(Grid),individuateR(Grid,Name,Objs).
%individuateR(Grid,Name,Objs):- 
%  no_repeats(Name+DirS,(allow_dir_list(Name,Dirs),sort(Dirs,DirS))),
%  individuate(Name,Grid,Objs),Objs\==[].

%one_fti_step(Name)

% tiny grid becomes a series of points
one_fti(VM,maybe_glyphic):-
  Points = VM.points,
  is_glyphic(Points,VM.h,VM.v),!,
  one_fti(VM,glyphic),
  set(VM.points)= Points.

is_glyphic(Points,_GH,_GV):- length(Points,Len), Len < 5.
is_glyphic(Points,GH,GV):- ( GH=<5 ; GV=<5 ), length(Points,Len), Len is GH * GV.

one_fti(VM,glyphic):-
  PointsLeft = VM.points,
  fif(PointsLeft\==[],
   ((localpoints(VM.grid,Points),
    % set(VM.points)=[],     
     maplist(maybe_make_point_object(VM,[birth(glyphic)]),Points,IndvList),
     addObjects(VM,IndvList)))),
  one_fti(VM,whole),
  save_grouped(individuate_glyphic(VM.id),VM.objs).

maybe_make_point_object(VM,_Opts,Point,Indv):-
    member(Point=Indv, VM.allocated_points),!.
maybe_make_point_object(VM,Opts,Point,Indv):-
    make_point_object(VM,Opts,Point,Indv).

one_fti(VM,whole):-
  %localpoints_include_bg(VM.grid,Points),
  Grid = VM.grid,
  localpoints_include_bg(Grid,Points),
  grid_size(Grid,H,V),
  length(Points,Len),
  make_indiv_object(VM.id,H,V,Points,[mass(Len),vis_hv(H,V),iz(combined),birth(whole)],Whole),
  %set(VM.points)=[],
  addObjects(VM,Whole),
  save_grouped(individuate_whole(VM.id),[Whole]),
  assert_shape_lib(pair,Whole),!.


is_fti_step(remove_used_points).
remove_used_points(VM):-  remCPoints(VM,VM.objs).



is_fti_step(colormass_subshapes).
colormass_subshapes(VM):- colormass_subshapes(VM,VM.objs).


colormass_subshapes(_VM,[]):-!.
colormass_subshapes(VM,VMObjs):- fail,
  select(Obj,VMObjs,Next),
  globalpoints(Obj,ContainedPoints),
  H = VM.h, V = VM.v,
  points_to_grid(H,V,ContainedPoints,Grid),
  individuate7(_, H,V,VM.id,[subshape_in_object],Grid,WasInside),
  ignore((fail,WasInside =[_,_|_], % two or more
        print_grid(H,V,"colormass_subshapes",WasInside),
        addObjects(VM,WasInside))),
  colormass_subshapes(VM,Next).
colormass_subshapes(_,_):-!.


%fti(VM,[combine_objects|set(VM.program_i)]):- combine_objects(VM),!.
is_fti_step(combine_objects).


/*
fti(VM,[DO|TODO]):- 
  fsi(VM,set(VM.robjs),set(VM.grid),set(VM.program_i),
    VM.h,VM.v,VM.objs,VM.id,[DO|TODO],VM.robjs, VM.points,VM.grid,set(VM.objs),set(VM.points)).
*/



% Thunk(VM -> ArgList)
/*
fsi(VM,ReservedIO,Grid,TODO,H,V,Sofar,ID,[retain_grid(Options)|TODO],ReservedIO,PointsIO,Grid,IndvList,PointsIO):-
  listify(Options,OptionsL),!,
  must_det_ll(individuals_list(VM,H,V,Sofar,ID,OptionsL,ReservedIO,PointsIO,Grid,IndvList,_LeftOver)),
  pt(yellow,PointsIO).
*/
%fti(VM,[by_color(Min,[])|set(VM.program_i)]):-!.
has_color(C,Point):- C-_ = Point.

one_fti(VM,by_color(Min,C)):- 
  my_partition(has_color(C),VM.points,ThisGroup,LeftOver),
  ignore(((
   length(ThisGroup,Len),  Len >= Min,
   set(VM.points)=LeftOver,
   make_indiv_object(VM,ThisGroup,[iz(by_color(Min,C))],ColorObj),
   addObjects(VM,ColorObj)))).



one_fti(VM,shape_lib(Hammer)):- !,
  nop((shape_lib_expanded(Hammer,Reserved),try_shapelib(VM,Hammer,Reserved))).

try_shapelib(VM,Hammer,Reserved):- 
  length(Reserved,RL),
  ignore((RL>30,pt(shape_lib_expanded(Hammer)=RL))),
  smallest_first(Reserved,ReservedSL),
  %debug_indiv(Reserved),
  use_shapelib(VM,Hammer,ReservedSL),
  %intersection(SofarOut,Sofar,_Intersected,Found,_LeftOverB), as_debug(8,print_grid(H,V,'shape_lib'+VM.id,Found)),
  !.

use_shapelib(VM,Name,[Obj|RestReserved]):-     
   %ignore((length(RestReserved,RL),1 is RL mod 7, pt(searchLib(Name)=RL))),
   % Points\==[],
  \+ color(Obj,black),
   object_grid(Obj,OGrid),
   Grid = VM.grid,
   find_ogs(OH,OV,OGrid,Grid),
   %must_det_ll
   ((
   localpoints(Obj,OPoints),
   offset_points(OH,OV,OPoints,ObjPoints),
   Points = VM.points,
   intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
   set(VM.points) = RestOfPoints,
   Sofar = VM.objs,
   do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),
   my_append(Intersected,Use,All),
   list_to_set(All,AllS), AllS \== [],
   make_indiv_object(VM,AllS,[iz(copy(Name))],Indiv0), 
   object_indv_id(Obj,_,Iv), 
   override_object(object_indv_id(VM.id,Iv),Indiv0,Indiv), 
   %make_indiv_object(VM,Use,Indiv),
   nop(points_to_grid(RestOfPoints,set(VM.grid))),
   set(VM.objs)= Sofar2,
   %print_grid(Indiv),
   addObjects(VM,Indiv),
   use_shapelib(VM,Name,[Obj|RestReserved]),
   nop(debug_indiv(Indiv)))).
/*  
use_shapelib(VM,Name,[Obj|Reserved]):-
   once((globalpoints(Obj,ObjPoints), 
   \+ color(Obj,black),
   Points = VM.points,
    Sofar = VM.objs,
   intersection(ObjPoints,Points,Intersected,NeedAsWell,RestOfPoints),
         do_leftover(Sofar,NeedAsWell,Intersected,Use,Sofar2),
         my_append(Intersected,Use,All),
         list_to_set(All,AllS))), AllS \== [],
         make_indiv_object(VM,[iz(override(Name))|AllS],Indiv0), 
         object_indv_id(Obj,_,Iv), 
         override_object(object_indv_id(VM.id,Iv),Indiv0,Indiv), 
         addObjects(VM,Indiv),
         %make_indiv_object(VM,Use,Indiv),
         use_shapelib(VM,Name,Reserved).
*/
do_leftover(Sofar,[],Intersected,Intersected,Sofar):- !.
%do_leftover([],_,_,_,_):- !,fail.
do_leftover(Sofar,LeftOverA,Intersected,Use,Removed):- select(S,Sofar,Removed), globalpoints(S,SPoints),
    intersection(SPoints,LeftOverA,Usable,[],[]),my_append(Usable,Intersected,Use).

one_fi(VM,retain(Option)):-
    Grid = VM.grid,
    ID = VM.id,
    globalpoints(Grid,NewGPoints), %  H> 14, V> 14,
    freeze(W,W>5),filter_indivs(VM.objs,[mass(W)];[iz(Option)],Retained1),
    filter_indivs(Retained1, \+ iz(background),Retained),
    as_debug(9,print_grid(H,V,'retained'+ID,Retained)),    
    remove_global_points(Retained,NewGPoints,set(VM.points)),
    points_to_grid(H,V,NextScanPoints,NNewGrid), 
    set(VM.objs)= Retained,
    set(VM.grid)= NNewGrid,
    set(VM.points)= NextScanPoints,

    %as_debug(9,print_grid(H,V,'newgrid'+ID,NNewGrid)),
    !.

is_fti_step(release_points).
release_points(VM):- 
    globalpoints(VM.grid,NextScanPoints1),
    addCPoints(VM,NextScanPoints1),
    globalpoints(VM.objs,NextScanPoints2),
    addCPoints(VM,NextScanPoints2).

objs_into_single_hidden(VM):- objs_into(VM,[iz(combined),iz(hidden),iz(into_single)]).
objs_into_single(VM):- objs_into(VM,[iz(combined),iz(into_single)]).

objs_into(Opts,VM):-
    maplist(globalpoints,VM.objs,IndvPoints),
    make_indiv_object(VM,IndvPoints,Opts,Indv),
    meets_indiv_criteria(into_single,IndvPoints),!,
    addObjects(VM,Indv).

is_fti_step(leftover_as_one).
leftover_as_one(VM):-
   ignore((VM.points\==[],
   make_indiv_object(VM,VM.points,[iz(combined),iz(leftover_as_one)],LeftOverObj), verify_object(LeftOverObj),
   addObjects(VM,LeftOverObj))),
   VM.points=[].

ignore_rest(VM):- VM.points=[].

/*

fsi(_VM,Reserved,Grid,NewOptions,_H,_V,Sofar,_ID,[by_color|TODO], Reserved, Points, Grid,Sofar, Points):- !,
   my_append(
     [(by_color(Min,[(black), (blue),  (red),   (green),(yellow),
                     (silver),(purple),(orange),(cyan), (brown)]))],TODO,NewOptions).
*/
/*

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
*/
same_lcolor(LargestColor,Obj):- color(Obj,Color),nop(print_grid(Obj)),!,Color==LargestColor.

one_fti(VM,IsToBeRewritten):-
    atom(IsToBeRewritten), 
    individuation_macros(IsToBeRewritten,Expansion),!,
    listify(Expansion,ExpansionL),
    my_append(ExpansionL,VM.program_i,set(VM.program_i)).


% @TODO will get sub objects later
not_list(G):- \+ is_list(G).

mapgroup(P2,G1,L2):- into_list(G1,L1),!, maplist(P2,L1,L2).
mapgroup(P1,G1):- into_list(G1,L1), !, maplist(P1,L1).

into_list(G,L):- is_list(G),!,L=G.
into_list(G,L):- is_map(G),!,L = G.objs,my_assertion(is_list(L)).
into_list(I,O):- listify(I,O),!.

assume_vm(_).
:- style_check(-singleton).

addPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.Prop,ReallyAdd,set(VM.Prop)).

remPropPredInfo(VM,Prop,Pred2,Obj):- assume_vm(VM),!,into_list(Obj,List),
  maplist(Pred2,List,ListData),
  get_kov(Prop,VM,VMProp),
  intersection(VMProp,ListData,ReallyRemove,Keep,PretendToRemove),
  gset(VM.Prop) = Keep.
  
addObjects(_VM,Obj):- Obj==[],!.
addObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  addGPoints(VM, Obj),
  addAPoint(VM, Obj),  
  my_append(VM.objs,ReallyAdd,set(VM.objs)).

addInvObjects(_VM,Obj):- Obj==[],!.
addInvObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.objs,ReallyAdd,set(VM.objs)).

remObjects(_VM,Obj):- Obj==[],!.
remObjects(VM,Obj):- assume_vm(VM),!,into_list(Obj,List), 
  intersection(VM.objs,List,ReallyRemove,Keep,PretendToRemove),
  remGPoints(VM, ReallyRemove),
  set(VM.objs) = Keep.

addCPoints(_VM,Obj):- Obj==[],!.
addCPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints), 
  intersection(VM.points,CPoints,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.points,ReallyAdd,set(VM.points)).

remCPoints(_VM,Obj):- Obj==[],!.
remCPoints(VM,Obj):- is_group(Obj),!,mapgroup(remCPoints(VM),Obj).
remCPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.points,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.points) = Keep.

addPoints(_VM,Obj):- Obj==[],!.
addRPoints(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints),
   intersection(VM.repaired,CPoints,PretendToAdd,Prev,ReallyAdd),
   my_append(VM.repaired,ReallyAdd,set(VM.repaired)).

remRPoints(_VM,Obj):- Obj==[],!.
remRPoints(VM,Obj):- assume_vm(VM),!,globalpoints(Obj,List), 
  intersection(VM.repaired,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.repaired) = Keep.


addAPoint(_VM,Obj):- Obj==[],!.
addAPoint(VM, Obj):- assume_vm(VM),!,globalpoints(Obj,CPoints),
   ignore(((CPoints=[Point], member(Point=Other,VM.allocated_points), throw(Point=Other)))),
   ignore(((CPoints=[Point], append(VM.allocated_points,[Point=Obj],set(VM.allocated_points))))).



addOptions(VM, Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.options,ReallyAdd,set(VM.options)),
  intersection(VM.program_i,Options,PretendToAdd,Prev,ReallyAdd),
  my_append(VM.program_i,ReallyAdd,set(VM.program_i)).

remOptions(VM,Obj):- assume_vm(VM),!,listify(Obj,List), 
  intersection(VM.options,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.options) = Keep,
  intersection(VM.program_i,List,ReallyRemove,Keep,PretendToRemove),
  set(VM.program_i) = Keep.


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

is_fti_stepr(remove_from_image).
remove_from_image(VM,Data):-    
    must_det_ll((remove_global_points(Data,VM.points,Points),
    pt(Points),
    set(VM.points) = Points)),!.
   

overwrite_use_so_far(FourWay1s,Sofar,UseSofar):-
  must_det_ll((remove_global_points(FourWay1s,Sofar,Sofar1),add_global_points(FourWay1s,Sofar1,UseSofar))),!.
overwrite_use_so_far(_FourWay1s,Sofar,Sofar).


one_fti(VM,merges(ShapeType1)):-one_fti(VM,merges(ShapeType1,ShapeType1)),!.
one_fti(VM,merges(ShapeType1,ShapeType2)):-
  Option = merges(ShapeType1,ShapeType2), copy_term(Option,OptionC),!, 
      Sofar = VM.objs,
      selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess), % trace,
      any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
      connection_direction(Option,Dir),
  %rot_left_45(Dir1,DirL),rot_left_45(DirL,Dir90),
  % \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  combine_2objs(VM,HV1,HV2,[],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  addObjects(VM,Combined),
  cycle_back_in(VM,OptionC).


cycle_back_in(VM,OptionC):- 
  TODO = VM.program_i,
  length(TODO,N),
  cycle_back_in(VM,OptionC,N,TODO),!.
cycle_back_in(VM,OptionC,TODO):- 
  length(TODO,N), N2 is floor(N/2),length(LL,N2),my_append(LL,RR,TODO),my_append(LL,[OptionC|RR],OptionsOut),
  set(VM.program_i)= OptionsOut.

%cycle_back_in(VM,OptionC,0,TODO):- set(VM.program_i) = [OptionC].
cycle_back_in(_,OptionC,_,[T,A|_]):- (OptionC==T ; OptionC==A),!.
cycle_back_in(VM,OptionC,_,[T|ODO]):- !, set(VM.program_i)= [T,OptionC|ODO].
cycle_back_in(VM,OptionC,_,TODO):- set(VM.program_i)= [OptionC|TODO].





selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess):- 
  into_group(Sofar,Sofar1,Closure),
  select(HV1,Sofar1,Found1), isz(HV1,ShapeType1),
  select(HV2,Found1,SofarLess1),isz(HV2,ShapeType2),
  call(Closure,SofarLess1,SofarLess).

one_fti(VM,connects(ShapeType1)):-one_fti(VM,connects(ShapeType1,ShapeType1)),!.
one_fti(VM,connects(ShapeType1,ShapeType2)):-
  Option = connects(ShapeType1,ShapeType2), copy_term(Option,OptionC),!,
  Sofar = VM.objs,
  selected_from(Sofar,ShapeType1,ShapeType2,HV1,HV2,SofarLess),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,P2), any_gpoint(HV2,C-P2), 
  connection_direction(Option,Dir),    
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  combine_2objs(VM,HV1,HV2,[],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  addObjects(VM,Combined),
  cycle_back_in(VM,OptionC).

connection_direction(Connected,Dir):-
   arg(_,Connected,ShapeType),
     shape_type_dirs(ShapeType,[Dir|_]). 
    %shape_type_dir(ShapeType2,Dirs2),

one_fti(VM,jumps(ShapeType1)):-
  Option = jumps(ShapeType1), copy_term(Option,OptionC),!,
  Sofar = VM.objs,
  Grid = VM.grid,
  
  selected_from(Sofar,ShapeType1,ShapeType1,HV1,HV2,SofarLess),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), any_gpoint(HV2,C-P2),
  % skip over MP
  is_adjacent_point(P0,Dir,P1), is_adjacent_point(P1,Dir,MP), is_adjacent_point(MP,Dir,P2),is_adjacent_point(P2,Dir,P3),
  any_gpoint(HV1,_-P0), any_gpoint(HV2,_-P3),
  %Grid = VM.grid,
  %get_color_at(MP,Grid,_MC),
  ignore(once((get_color_at_point(Grid,MP,MC),is_color(MC));MC=C)),
  \+ (any_gpoint(HV1,C-P1C), any_gpoint(HV2,C-P2C),is_adjacent_point(P1C,_,P2C)),
  % TODO: HACK WE MIGHT NOT WANT TO STEAL THE POINT?   
  %once(select(MC-MP,Points,RestPoints);Points=RestPoints),
  combine_2objs(VM,HV1,HV2,[MC-MP],[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  addObjects(VM,Combined),
  cycle_back_in(VM,OptionC).

is_fti_step(extends).

extends(ShapeType1,VM):-
  Option = extends(ShapeType1), copy_term(Option,OptionC),!,
  select(HV1,VM.objs,SofarLess),isz(HV1,ShapeType1),
  connection_direction(Option,Dir),
  any_gpoint(HV1,C-P1), is_adjacent_point(P1,Dir,MP),
  Points = VM.points,
  select(MC-MP,Points,ScanPoints),
  \+ (is_adjacent_point(P1,_,P2), is_adjacent_point(P2,_,MP),any_gpoint(HV1,_-P2)),
  all_individuals_near(Dir,Option,C,[MC-MP],ScanPoints,NextScanPoints,IndvPoints),
  combine_2objs(VM,HV1,[],IndvPoints,[iz(Option)],Combined),
  set(VM.objs)=SofarLess,
  set(VM.points)=NextScanPoints,
  addObjects(VM,Combined),
  cycle_back_in(VM,OptionC).


    combine_2objs(VM,HV1,HV2,NewPoints,IPROPS,Combined):-
      globalpoints(HV1,GP1), globalpoints(HV2,GP2),      
      % indv_props(HV1,Props1),indv_props(HV2,Props2),
      Props1=[],Props2=[],
      my_append([GP1,GP2,NewPoints],GPoints), my_append([Props1,Props2,IPROPS],Info),
      make_indiv_object(VM,GPoints,Info,Combined).


one_fti(VM,Option):- one_ifti(VM,Option),!.

one_ifti(VM,Option):- 
   ( Option \== remaining_points), 
   find_one_individual(Option,Indv,VM),
   addObjects(VM,Indv),!,
   one_ifti(VM,Option).

one_ifti(_VM,Option):- is_thing_or_connection(Option),!.

is_thing_or_connection(Option):-allowed_dir(Option,_Dir).
is_thing_or_connection(connects(_,_)).
is_thing_or_connection(merges(_,_)).
is_thing_or_connection(jumps(_,_)).



find_one_individual(Option,Obj,VM):- find_one_individual3(Option,Obj,VM),!.
find_one_individual(Option,Obj,VM):- find_one_individual2(Option,Obj,VM),!.

find_one_individual3(Option,Obj,VM):- 
    Points = VM.points,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,Rest1),
    %ScanPoints = Rest1,
    ((adjacent_point_allowed(C,HV2,Dir,HV3),select(C2-HV3,Rest1,ScanPoints), ok_color_with(C,C2));
     (allowed_dir(Option,Dir2),Dir2\=Dir, adjacent_point_allowed(C,HV2,Dir2,HV3),select(C-HV3,Rest1,ScanPoints))),    
    %maybe_multivar(C), 
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2,C-HV3],ScanPoints,NextScanPoints,IndvPoints), !,
    make_indiv_object(VM,IndvPoints,[iz(ShapeType),birth(individual3(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  set(VM.points) = NextScanPoints,
  addObjects(VM,Obj),
  cycle_back_in(VM,OptionC),!.


find_one_individual2(Option,Obj,VM):- 
    Points = VM.points,
    H = VM.h,
    V = VM.v,
    ID = VM.id,
    shape_min_points(VM,Option,IndvPoints),
    copy_term(Option,OptionC),Option=ShapeType,    
  select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), 
    make_indiv_object(ID,H,V,IndvPoints,[iz(ShapeType),birth(individual2(Option))],Obj),
    meets_indiv_criteria(Option,IndvPoints),
  set(VM.points) = NextScanPoints,
  addObjects(VM,Obj),
  cycle_back_in(VM,OptionC).


point_groups_by_color(Option,[IndvPoints|Groups],Points,Rest):-    
    select(C-HV,Points,Rest0), \+ free_cell(C), % non_free_fg(C), % \+ is_black(C),
    allowed_dir(Option,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
    all_individuals_near(Dir,Option,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints), !,
    point_groups_by_color(Option,Groups,NextScanPoints,Rest).
point_groups_by_color(_Option,[],Points,Points).

shape_min_points(VM,Shape,MinShapeO):- MS = VM.objs_min_mass, number(MS), length(MinShape,MS),
  !,append(MinShape,_,MinShapeO),!,shape_min_points0(Shape,MinShapeO).
shape_min_points(_VM,Shape,MinShapeO):-shape_min_points0(Shape,MinShapeO).

%shape_min_points0(colormass,[_,_,_,_,_|_]):-!.
%shape_min_points0(non_diag,[_,_,_,_|_]):-!.
%shape_min_points0(diamonds,[_,_,_,_|_]):-!.
shape_min_points0(_,[_,_|_]).
%  shape_min_points(VM,_,_).


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
shape_has_filtered_use(C,[_],_):- shape_filter(C,non_diag),!.


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
        all_individuals_near(Dir,Options,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))),!.

individuals_near(_Dir,_Options,_C,_From,[],[],[]):-!.
individuals_near(Dir,Options,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Dir,Options,C,E,From),!,
  individuals_near(Dir,Options,C,[E|From],ScanPoints,Nears,NextScanPoints),!.

individuals_near(Dir,Options,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Dir,Options,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(_Dir_,Options,C,C2-E,List):- allowed_dir(Options,Dir), adjacent_point_allowed(C,E2,Dir,E), 
  member(C2-E2,List),ok_color_with(C2,C).

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


% prop_piority(Class,Priority).
prop_piority(iz(image),4).
prop_piority(iz(combined),3).
prop_piority(iz(hidden),2).
prop_piority(birth(glyphic),1).
smallest_priority(Indv,Priority):- prop_piority(Prop,Priority), has_prop(Prop,Indv),!.
smallest_priority(_,0).

resize_inf(1,inf):-!.
resize_inf(X,X).

smallest_first(IndvS,IndvO):-
  findall((Priority+Size)-Indv,(member(Indv,IndvS),smallest_priority(Indv,Priority),mass(Indv,MSize),resize_inf(MSize,Size)),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO).

largest_first(IndvS,IndvR):-   
 must_det_ll((
  findall((Size-Priority)-Indv,(member(Indv,IndvS),smallest_priority(Indv,NPriority),mass(Indv,Size),Priority is - NPriority),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO),
  reverse(IndvO,IndvR))).

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




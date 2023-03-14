/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact loc2D business@logicmoo.org)
*/
:- include(kaggle_arc_header).

:- use_module(library(nb_set)).
:- use_module(library(lists)).

/*
% detect_all_training_hints(Grid,SGrid):- ...
line Separated
Symmetry sorta happening
Individuals by colormass % t(b230c067)
all_is_one
supergrid_is_point_dotted % '94133066'
supergrid_is_output_size % t('8d5021e8') 
supergrid_is_all % t('6150a2bd')
suprgrid_is_dots_same_count % t(ff28f65a)
supergrid_input_1 % v(cad67732)*  
supergrid_input_1 in_out=1x1
superinput_keypad= souput=kewypad
superinput_columns 
superinput_rows
superinput_swashica
superinput_starts_at_input_loc
superinput_blob
*/
:- dynamic(kaggle_arc/4).

test_supergrid:- clsbake, 
 forall(kaggle_arc(TestID,ExampleNum,In,Out),
   detect_pair_hints(TestID,ExampleNum,In,Out)).


print_directive(P):- format('~N:- ~q. ~n',[P]).
write_intermediatre_header:- 
  print_directive(encoding(iso_latin_1)),
  forall(  (test_local_save(F,A),nl),
      my_maplist(print_directive,[%abolish(F/A),
                               multifile(F/A),dynamic(F/A),discontiguous(F/A),public(F/A),export(F/A),module_transparent(F/A)])).

print_ref(Ref):- is_clause_ref(Ref), clause(H,B,Ref),!,print_ref((H:-B)).
print_ref((X:-True)):- True == true,!, print_ref(X).
print_ref(G):- format('~N'),write_canonical(G),format('.~n'),!.
%print_ref(G):- \+ \+ ((numbervars(G,0,_,[attvar(bind),singletons(true)]), format('~N~@.~n',[write_canonical(G)]))),!.


clsbake:- nop(clsmake).

compile_and_save_test:- update_and_fail,fail.
compile_and_save_test:- get_pair_mode(entire_suite),!,clsbake, forall_count(all_arc_test_name(TestID),time(compile_and_save_test(TestID))).
compile_and_save_test:- get_current_test(TestID),time(compile_and_save_test(TestID)),!,  
  detect_all_training_hints,!.

gen_gids:- u_dmsg(start(gen_gids)),forall(all_arc_test_name(TestID),gen_gids(TestID)),u_dmsg(end(gen_gids)).
gen_gids(Mask):-
  testid_name_num_io(Mask,TestID,Example,Num,IO),
  ExampleNum = Example+Num,!,
  %forall(kaggle_arc(TestID,ExampleNum,I,O),name_the_pair(TestID,ExampleNum,I,O,_PairName)),
  forall((kaggle_arc_io(TestID,ExampleNum,IO,G),((ID=(TestID>ExampleNum*IO),term_to_oid(ID,GID)))),
    once((assertz_if_new(tid_to_gids(ID,GID)),assertz_if_new(gid_to_grid(GID,G))))),
  forall(kaggle_arc_io(TestID,_,_,G),once(grid_to_gid(G,_))),!.

:- dynamic(arc_cache:workflow_status/3).
once_with_workflow_status(Goal):- call_in_testid(arc_cache:workflow_status(Goal,success)),!.
once_with_workflow_status(Goal):- call_in_testid(arc_cache:workflow_status(Goal,begun)),!.
once_with_workflow_status(Goal):- 
  assert_in_testid(arc_cache:workflow_status(Goal,begun)),
  (call(Goal)-> assert_in_testid(arc_cache:workflow_status(Goal,success)) ; assert_in_testid(arc_cache:workflow_status(Goal,failed))).

compile_and_save_test(TestID):- once_with_workflow_status(compile_and_save_test_now(TestID)).

compile_and_save_test_now(TestID):- var(TestID),!,all_arc_test_name(TestID),compile_and_save_test_now(TestID).
compile_and_save_test_now(Mask):- fix_test_name(Mask,TestID),  Mask\=@=TestID,!,compile_and_save_test_now(TestID).
compile_and_save_test_now(TestID):- 
  %ignore(retract(saved_training(TestID))),
  %ignore(retract(process_test(TestID))),

  time((locally(nb_setval(use_individuated_cache,true),
     ((
      gen_gids(TestID),
      compute_all_test_hints(TestID)),
      arc_assert(saved_training(TestID)),
      arc_assert(process_test(TestID)),
      detect_all_training_hints(TestID),
      nop(individuate_pairs_from_hints(TestID)),
      %train_test(TestID,train_using_io),  
      %print_hybrid_set,      
      save_supertest(TestID))))).


deduce_shapes(TestID):-
  with_test_grids(TestID,Grid,test_deduce_grid_shapes(Grid)).

test_deduce_grid_shapes(Grid):-
  forall(grid_to_obj(Grid,O),show_indiv(test_deduce_grid_shapes,O)),
  print_hybrid_set.


print_hybrid_set(TestID):- 
  ensure_test(TestID),
  get_hybrid_set(Set),
  print_side_by_side(Set),
  nop(forall(member(O,Set),print_hybrid_grid(O))).

print_hybrid_grid(G):- io_side_effects,into_grid_free(G,O),grid_to_norm(Ops,O,N),
  (O\==N->print_side_by_side([ops(Ops)],G,N); print_grid(O)).

individuate_pairs_from_hints(TestID):- 
  arc_assert(individuate_test_grids(TestID)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out), 
    individuate_pair_here(TestID,ExampleNum,In,Out)).

individuate_pair_here(TestID,Trn+N1,In,Out):-
  Trn == trn, % no peeking  
  ndividuator(TestID,Trn+N1,complete,In,Out),
  nop(train_for_objects_from_1pair(_{},TestID,[Trn,'i',N1,'o',N1],In,Out,_DictMid)).

print_all_info_for_test:- notrace(print_all_info_for_test0).
print_all_info_for_test0:- 
  print_testinfo,
  write('<p><strong><em>This representation is a hierarchy of Objects. One simple way to reduce description length is
      collect points of a like color into one group. Also, an Object may specify a
      repeated transformation, e.g. a vertical line can be specified by repeating a
      vertical translation by one unit. This allows lines, rectangles,
      tilings, etc. efficient representations.</em></strong></p>'),

  write('Once the input and output representations are determined, one can search for the minimally specified "path" that 
      recreates the output given the input. Given high-quality representations, there are usually only a few Objects of interest,
      and one can quickly check every pair of input and output Objects for similarity and whether there exists a short 
      transformation path between them.  At this stage, the code mostly looks for single-transform links.'),
  show_all_test_reductions,
  forall(show_reduced_io,true),
  
  print_hybrid_set.

into_grid_free(I,O):- must_be_free(O),into_grid(I,O),!.
into_grid_list(I,O):- must_be_free(O),!,into_grid_list0(I,O).

into_grid_list0(A,[A]):- var(A),!.
into_grid_list0(A^B,List):- !, caret_to_list(A^B,List),!.
into_grid_list0(A,[A]):- is_grid(A),!.
into_grid_list0(A,[AG]):- always_grid_footer(A,G,_),into_grid_free(G,AG),!.
into_grid_list0([A|More],[A|More]):- is_grid(A),!.
into_grid_list0([G|Grids],GridL):- into_grid_list(G,G1),into_grid_list(Grids,GS),
  append(G1,GS,GridL).


show_single_reduction_1(G):- reduce_grid(G,Ops,R),print_ss([G,R]),writeg(Ops).

do_some_grids(Title,AB):- is_grid(AB),!,do_some_grids(Title,[AB]).
do_some_grids(Title,AB):- into_grid_list(AB,GL), GL\==AB,!,do_some_grids(Title,GL).
do_some_grids(Title,GL):- once((dash_chars,print_ss(do_some_grids(Title)=GL),dash_chars)),fail.
%do_some_grids(Title,[A,B]):- !, common_reductions(A^B,OPS,_)
do_some_grids(Title,Grids):-  fail,
  forall(do_as_pairs(Grids,I,O),
    (print_side_by_side(green,I,orig(Title),_,O,next_orig(Title)),
     forall(show_reduced_io(I^O),true))).

do_some_grids(Title,GL):-
 (all_common_reductions(GL,OPS,Reduced)*->print_common_reduction_result(Title,OPS,Reduced)
  ;(GL\=[_,_,_|_],common_reductions_from_two(GL,OPS,A,B,AA,BB)*-> print_common_reduction_result(common_reductions_from_two(Title,A,B),OPS,AA^BB) 
  ; (pp(no_common_reductions=Title)))),
 dash_chars.
do_some_grids(_Title,GL):- forall(member(G,GL),show_single_reduction_1(G)),!.

print_common_reduction_result(TestID,OPS,Reduced):- pp(ops(TestID)=OPS), print_ss(all_common_reductions(TestID)=Reduced).


pause:- !.
/*
pause:- arc_html, !.
pause:- !, atrace.
pause:- get_single_char(K),(K=t->atrace;true).
*/

do_as_pairs([F|More],I,O):- do_as_pairs(F,[F|More],I,O).
do_as_pairs(_,[I,O|_],I,O).
do_as_pairs(O,[I],I,O):-!.
do_as_pairs(F,[_|More],I,O):- do_as_pairs(F,More,I,O).

%show_all_test_reductions(TestSpec):- var(TestSpec),!,show_all_test_reductions.
show_all_test_reductions(TestSpec):- var_ensure_test(TestSpec,TestID),
  u_dmsg(?-show_reduced_test(TestID)),  
  dash_chars,
  show_reduced_inputs(TestID),
  dash_chars,
  show_reduced_outputs(TestID),
  dash_chars,
  show_reduced_pairs(TestID),
  dash_chars.

show_reduced_io_fav(I^O):- get_current_test(TestID),\+ \+ do_some_grids(TestID,I^O), show_reduced_io(I^O).

show_reduced_outputs(TestSpec):- var_ensure_test(TestSpec,TestID),
  findall(ExampleNum=Grid,kaggle_arc(TestID,ExampleNum,_,Grid),Grids),
  do_some_grids('OUTPUT'(TestID),Grids).

show_reduced_inputs(TestSpec):- var_ensure_test(TestSpec,TestID),
  findall(ExampleNum=Grid,kaggle_arc(TestID,ExampleNum,Grid,_),Grids),
  do_some_grids('INPUT'(TestID),Grids).

show_reduced_pairs(TestID):- 
  with_test_pairs(TestID,ExampleNum,I,O,
    (print_side_by_side(green,I,orig_in(TestID,ExampleNum),_,O,orig_out(TestID,ExampleNum)),
     forall(do_some_grids(TestID,I^O),true))).


show_reduced_io_rarely(_):- \+ get_pair_mode(single_pair),!.
show_reduced_io_rarely(In^Out):- % ignore((nonvar(In),nonvar(Out),grid_hint_swap(i-o,In,Out))),!,
  ignore((fail,get_thingy(In,Out,Gets),print_ss(Gets))).

show_reduced_io_rarely(IO):- forall(show_reduced_io(IO),true).

show_reduced_io(TestSpec):- var_ensure_test(TestSpec,TestID),
    with_test_pairs(TestID,_ExampleNum,I,O,show_reduced_io(I^O)).

show_reduced_io(IO):- 
    once(show_grid_call(reduce_cutaway(_),IO,NextIO)),
  if_t(((NextIO)\=@=(IO)), show_reduced_io(NextIO)).

  %show_reduced_io(I^O):-  maybe_easy(I,II,DidIn),same_reduction(DidIn,DidOut),maybe_easy(O,OO,DidOut), must_det_ll(print_side_by_side(green,II,DidIn,_,OO,DidOut)),!.
show_reduced_io(I0^O):- 
  once(( grid_size(I0,H,V), grid_size(O,OH,OV))),
  ((H>OH;V>OV) , grid_call_alters(trim_to_rect,I0,I)),
 show_reduced_io(I^O).

show_reduced_io(IO):-
  %OPS = [_,_|_],
  once(show_grid_call(grid_to_norm(_),IO,NextIO)),
  if_t(((NextIO)\=@=(IO)), show_reduced_io(NextIO)).

show_reduced_io(IO):- once(show_grid_call(mapgridish(remove_color_if_same(black),IO),IO,NextIO)),
  if_t(((NextIO)\=@=(IO)), show_reduced_io(NextIO)).

show_reduced_io(_).


mapgridish(P3,I^O,In,Out):- In==I-> mapgrid(P3,O,I,Out) ; mapgrid(P3,I,O,Out).


reduce_grids_io(OPS,I^O,III^OOO):- area(I,IArea),area(O,OArea),reduce_grids_area_io(I^O,IArea^OArea,OPS,III^OOO).

interesting_ops(OPS):- OPS==[],!,fail.
interesting_ops(OPS):- OPS=[undo_effect(_)|More],!,interesting_ops(More).
interesting_ops(_).

grid_to_norm(Op,I,OO):- normalize_grid(Op,I,OO),!.

reduce_grids_area_io(I^O,_IArea_OArea,[io(OPS)],II^OO):- reduce_grid(I^O,OPS,II^OO), interesting_ops(OPS),!.
reduce_grids_area_io(I^O,_IArea_OArea,[oi(OPS)],II^OO):- reduce_grid(O+I,OPS,OO+II), interesting_ops(OPS),!.
reduce_grids_area_io(I^O,IArea^OArea,[i(OPS)],II^O):- IArea>OArea, reduce_grid(I+I,OPS,II+_), interesting_ops(OPS).
reduce_grids_area_io(I^O,IArea^OArea,[o(OPS)],I^OO):- IArea>OArea, reduce_grid(O^O,OPS,OO+_), interesting_ops(OPS).
reduce_grids_area_io(I^O,_IArea_OArea,[io(OPS)],II^OO):- reduce_grid(I^O,OPS,II^OO), interesting_ops(OPS).
reduce_grids_area_io(I^O,_IArea_OArea,[i(OPS)],II^O):- reduce_grid(I+I,OPS,II+_), interesting_ops(OPS).
reduce_grids_area_io(I^O,IArea^OArea,[o(OPS)],I^OO):- IArea<OArea, reduce_grid(O^O,OPS,OO^OO), interesting_ops(OPS).
/*
reduce_grids_area_io(I^O,_IArea_OArea,OPS,III^OOO):- 
  reduce_grid(I+I,II_Ops,II+II),
  reduce_grid(O^O,OO_Ops,OO^OO),
  reduce_grid(II^OO,IO_Ops,III^OOO),
  (interesting_ops(IO_Ops) ; interesting_ops(OO_Ops) ; interesting_ops(II_Ops)),
  [i(II_Ops),o(OO_Ops),io(IO_Ops)] = OPS,
  !.
reduce_grids_area_io(I^O,_IArea_OArea,OPS,III^OOO):- 
  reduce_grid(I^O,IO_Ops,II^OO),
  reduce_grid(II+II,II_Ops,III+III),
  reduce_grid(OO^OO,OO_Ops,OOO^OOO),
  (interesting_ops(IO_Ops) ; interesting_ops(OO_Ops) ; interesting_ops(II_Ops)),
  OPS = [io(IO_Ops)+i(II_Ops)+o(OO_Ops)],
  !.
*/


maybe_easy(A,AR,ROPA):- reduce_grid_pass(1,A^A,[A^A],ROPA,AR^AR).
maybe_easy(I,II,Code):- maybe_try_something1(I,II,Code),!.
maybe_easy(I,I,==):- !.
    
must_ll(G):- G*->true;throw(failed(G)).


detect_all_training_hints:- get_current_test(TestID),time(detect_all_training_hints(TestID)).
detect_all_training_hints(TestID):- ensure_test(TestID),
  save_the_alt_grids(TestID),
  training_only_examples(ExampleNum), 
  w_section(title(detect_all_training_hints(TestID>ExampleNum)),
    ( 
       forall(must_ll(kaggle_arc(TestID,ExampleNum,In,Out)),
           must_det_ll(detect_pair_hints(TestID,ExampleNum,In,Out))),

       color_print(magenta,call(((must_det_ll(compute_and_show_test_hints(TestID)))))))).

training_only_examples(ExampleNum):- ignore(ExampleNum=(trn+_)).

detect_test_hints1:- clsbake, get_current_test(TestID),detect_test_hints1(TestID).
detect_test_hints1(TestID):- 
 some_current_example_num(ExampleNum), training_only_examples(ExampleNum),
 forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_pair_hints(TestID,ExampleNum,In,Out)).
  

color_subst([],[],[]):-!.
color_subst([O|OSC],[I|ISC],[O-I|O2I]):-
  color_subst(OSC,ISC,O2I).
color_subst(_OSC,_ISC,[]):-!.

detect_pair_hints(TestID,ExampleNum,In,Out):- 
  ensure_test(TestID),
  must_det_ll((
  dmsg(detect_pair_hints(TestID,ExampleNum)),
  assert_id_grid_cells(_,In), assert_id_grid_cells(_,Out),
  % guess_board(TT),
  %print(TT),
  %ignore(show_reduced_io(In^Out)),
 % ignore(print_single_pair(TestID,ExampleNum,In,Out)),
  grid_hint_swap(i-o,In,Out),
  dash_chars)),!.

guess_board(TT):- arc_setval(TT,guess_board,t).


show_recolor(TestID,_ExampleNum,IIn,Out0,TT):- 
  ensure_test(TestID),
  must_det_ll(((
  %show_patterns(In),show_patterns(Out),
  
  %gset(TT.z_contains_out)=in(HIO,VIO),
  %gset(TT.z_contains_in)=out(HOI,VOI),


  % grid_size(IIn,IH,IV), 
  %grid_size(Out0,OH,OV), % max_min(OV,IV,V,_),max_min(OH,IH,H,_),
  into_bicolor(IIn,In), into_bicolor(Out0,Out),

  /*
  pair_dictation(TestID,ExampleNum,IIn,Out0,T), T.in = IIn, T.out = Out0,
  ((OV==1,OH==1) -> (O2I=[]) ; (T.in_specific_colors = ISC, T.out_specific_colors = OSC,    color_subst(OSC,ISC,O2I))),
  subst_1L(O2I,OOut,Out), %subst_1L(O2I,Out0,OutF),
  get_map_pairs(T,_,Pairs),
  list_to_rbtree_safe(Pairs,TT),!,

  arc_setval(TT,rhs_color_remap, O2I),

  show_colorfull_idioms(IIn), show_colorfull_idioms(Out0),
*/
  (most_d_colors(Out,CO,NO),arc_setval(TT,out_d_colors,CO),arc_setval(TT,out_map,NO)),  
  (most_d_colors(In,CI,NI),  arc_setval(TT,in_d_colors,CI), arc_setval(TT,in_map,NI)),
  %if_t(find_ogs(HOI,VOI,IIn,OutF),arc_setval(TT,z_in_contains_out,(HOI,VOI))),
  %if_t(find_ogs(HIO,VIO,OutF,IIn),arc_setval(TT,z_out_contains_in,(HIO,VIO))),
  %@TODO record in the out_in _may_ hold the in_out 
  print_side_by_side(cyan,NI,CI,_,NO,CO),
  get_map_pairs(TT,_,List),pp(List)))),!.


/*

show_recolor(TestID,ExampleNum,IIn,Out0,TT):- 
  must_det_ll(((
  %show_patterns(In),show_patterns(Out),
  
  %gset(TT.z_contains_out)=in(HIO,VIO),
  %gset(TT.z_contains_in)=out(HOI,VOI),


  % grid_size(IIn,IH,IV), 
  grid_size(Out0,OH,OV), % max_min(OV,IV,V,_),max_min(OH,IH,H,_),
  into_bicolor(IIn,In), into_bicolor(Out0,OOut),

  pair_dictation(TestID,ExampleNum,IIn,Out0,T), T.in = IIn, T.out = Out0,
  ((OV==1,OH==1) -> (O2I=[]) ; (T.in_specific_colors = ISC, T.out_specific_colors = OSC,    color_subst(OSC,ISC,O2I))),
  subst_1L(O2I,OOut,Out), %subst_1L(O2I,Out0,OutF),
  get_map_pairs(T,_,Pairs),
  list_to_rbtree_safe(Pairs,TT),!,

  arc_setval(TT,rhs_color_remap, O2I),

  show_colorfull_idioms(IIn), show_colorfull_idioms(Out0),

  (most_d_colors(Out,CO,NO),arc_setval(TT,out_d_colors,CO),arc_setval(TT,out_map,NO)),  
  (most_d_colors(In,CI,NI),  arc_setval(TT,in_d_colors,CI), arc_setval(TT,in_map,NI)),
  %if_t(find_ogs(HOI,VOI,IIn,OutF),arc_setval(TT,z_in_contains_out,(HOI,VOI))),
  %if_t(find_ogs(HIO,VIO,OutF,IIn),arc_setval(TT,z_out_contains_in,(HIO,VIO))),
  %@TODO record in the out_in _may_ hold the in_out 
  print_side_by_side(cyan,NI,CI,_,NO,CO),
  get_map_pairs(TT,_,List),pp(List)))),!.
*/

arc_test_property(g,n,b,d):-fail.

  
is_fti_step(obj_into_cells).
is_fti_step(print_grid).
obj_into_cells(VM):- 
  vm_to_printable(VM,GridObj),
  globalpoints(GridObj,Row1),
  flatten([Row1],Row1F),list_to_set(Row1F,Row1S),
  my_maplist(only_color_data,Row1S,ColorRow),
  set_vm(grid,[ColorRow]).


show_found(HOI,VOI,H,V,Info,F):-
  HO is HOI-3, VO is VOI-3, 
  show_found2(HO,VO,H,V,Info,F).

show_found2(HO,VO,H,V,Info,_F):- u_dmsg(show_found2(HO,VO,H,V,Info)),!.
show_found2(HO,VO,H,V,Info,F):-
  offset_grid(HO,VO,F,OF),
  constrain_grid(f,_TrigF,OF,FF),!,
  print_grid(H,V,Info,FF),!.


save_grid_hints:-  forall(ensure_test(TestID),compute_and_show_test_hints(TestID)),
  listing(arc_test_property/3).




hint_functor(cg(IO,Hints),cg(IO,F)):- !, hint_functor(Hints,F).
%hint_functor(comp(MC,i-o,Hints),F):- !, hint_functor(Hints,F).
hint_functor(comp(MC,IO,Hints),comp(MC,IO,F)):- hint_functor(Hints,F).
hint_functor(rev(Hints),rev(F)):- !, hint_functor(Hints,F).
hint_functor(mono(Hints),mono(F)):- !, hint_functor(Hints,F).
hint_functor(vis_hv_term(Hints),(F)):- !, hint_functor(Hints,F).
hint_functor(Hints,F):- functor(Hints,F,_).


hint_into_data(cg(_IO,Hints),F):- !, hint_into_data(Hints,F).
hint_into_data(comp(_MC,_IO,Hints),F):- !, hint_into_data(Hints,F).
hint_into_data(rev(Hints),F):- !, hint_into_data(Hints,F).
hint_into_data(vis_hv_term(Hints),(F)):- !, hint_into_data(Hints,F).
hint_into_data(mono(Hints),(F)):- !, hint_into_data(Hints,F).
hint_into_data(Data,Data).


relax_hint(G,G):- (\+ compound(G)) -> !; true.
relax_hint(rev(G),rev(GG)):- !, relax_hint(G,GG).
relax_hint(mono(G),mono(GG)):- !, relax_hint(G,GG).
relax_hint(cg(W,G),cg(W,GG)):- !, relax_hint(G,GG).
relax_hint(G,GG):- compound(G), duplicate_term(G,GG),arg(N,G,E),relax_arg(E,Hints),nb_setarg(N,GG,Hints).
%relax_hint(G,GG):- functor(G,F,A),functor(GG,F,A).

relax_arg(E,C):- is_color(E),!,relax_color_arg(E,C).
%relax_arg(E,_):- var(E),!,fail.
relax_arg(E,E):- var(E) -> !; true.
relax_arg(E,len(L)):- is_list(E),length(E,L).
relax_arg(_,_).


:- dynamic(io_xform/3).
add_xform_maybe(In1,Out1):- ignore(get_current_test(TestID)),
                 ignore((   ThisXForm=io_xform(TestID,In1,Out1),
                    ThatXForm=io_xform(TestID,_In2,_Out2),                     
                    (call(ThatXForm) 
                      -> (must_min_unifier(ThisXForm,ThatXForm,NewXForm),
                                        retractall(ThatXForm),retractall(NewXForm),asserta(NewXForm))
                     ; asserta(ThisXForm)))),!.
                    
add_hint(TestID,ExampleNum,Hints):- 
  hint_functor(Hints,F),hint_into_data(Hints,D), assert_test_property(TestID,ExampleNum,F,D).

assert_test_property(TestID,ExampleNum,Prop,Data):-
  assert_if_new(arc_test_property(TestID,ExampleNum,Prop,Data)),
  nop(pp(assert_test_property(TestID,ExampleNum,Prop,Data))).
  
  % forall((kaggle_arc_io(TestID,ExampleNum,in,Out1),N2 is N+1,  (kaggle_arc_io(TestID,(trn+N2),in,Out2)->true;kaggle_arc_io(TestID,(trn+0),in,Out2)),  grid_hint_recolor(i-i,Out1,Out2,Hints)),add_hint(TestID,Hints,N)).


/*
compute_and_show_test_hints(TestID):- format('~N'),
  findall(Hints-N,(kaggle_arc(TestID,ExampleNum,In,Out), grid_hint_swap(i-o,In,Out,Hints)),HintsIO),
  findall(Hints-N,(kaggle_arc_io(TestID,ExampleNum,in,In1),  N2 is N+1, (kaggle_arc_io(TestID,(trn+N2),in,In2)->true;kaggle_arc_io(TestID,(trn+0),in,In2)), grid_hint_recolor(i-i,In1,In2,Hints)),HintsII),
  findall(Hints-N,(kaggle_arc_io(TestID,ExampleNum,out,Out1),N2 is N+1, (kaggle_arc_io(TestID,(trn+N2),out,Out2)->true;kaggle_arc_io(TestID,(trn+0),out,Out2)), grid_hint_recolor(o-o,Out1,Out2,Hints)),HintsOO),
  append([HintsIO,HintsOO,HintsII],Hints),
  keysort(Hints,SHints),  
  my_maplist(aquire_hints(TestID,SHints),SHints),
  format('~N'),
  nop()),
  list_common_props(TestID).
*/

compute_and_show_test_hints(TestID):- 
 w_section(compute_and_show_test_hints1(TestID)).
compute_and_show_test_hints1(TestID):- ensure_test(TestID),format('~N'),
  compute_all_test_hints(TestID),
  ignore(list_common_props_so_far(TestID)),!,
  %listing(arc_test_property(TestID,_,_)),
  with_li_pre(listing(io_xform(TestID,_,_))),
  %ignore(list_common_props(TestID)),!,
  format('~N').

list_common_props_so_far(TestID):-
 (\+ arc_test_property(TestID,trn+_,_,_) -> compute_all_test_hints(TestID); true),
 findall(F=Common,
  (arc_test_property(TestID,trn+0,F,_),
    retractall(arc_test_property(TestID,common,F,_)),
    (( findall(Data,arc_test_property(TestID,(trn+_),F,Data),Commons),
      once((some_min_unifier(Commons,Common),nonvar(Common))))),
      assert_test_property(TestID,common,F,Common)),FComs),
  sort_safe(FComs,SComs),
  %dash_chars,
  %print_test(TestID),
  %wots(SS,my_maplist(ptv1,SComs)),
  w_section(title(list_common_props),ptv1(cyan+magenta,SComs)),
  !.


with_li_pre(Goal):- with_tag(li,with_tag(pre,Goal)).
%with_li_pre(Goal):- call(Goal).

ptv1(Color,T):- is_list(T), !, my_maplist(ptv1(Color),T).
ptv1(_Color,_=T):- T==[],!.
%ptv1(_Color,_=T):- compound(T), T = each_object(_),!.
ptv1(_Color,_=T):- compound(T),compound_name_arguments(T,_,[A]),A==[],!.

ptv1(C1C2,T):- needs_bold(T),!,bold_print(ptv11(C1C2,T)),!.
ptv1(C1C2,T):- ptv11(C1C2,T),!.

needs_bold(C):- compound(C),sub_term(E,C),compound(E),functor(E,ogs,_).

ptv11(C1+C2,T):- !, format('~N'),wots(S,writeq(T)),
  (has_spec_value(T) -> color_print(C1,call(bold_print(write(S)))) ; color_print(C2,call(write(S)))).
ptv11(Color,T):- format('~N'),(has_spec_value(T) -> color_print(cyan,call(bold_print(print_tree(T)))) ; color_print(Color,call(print_tree(T)))).

has_spec_value(T):- ground(T),!.
has_spec_value(V):- var(V),!,fail.
has_spec_value(_=T):- !,has_spec_value(T).
has_spec_value(T):- sub_term(E,T),number(E),!.
has_spec_value(T):- sub_term(E1,T),atomic(E1),sub_term(E2,T),atomic(E2),E1\==E2.


in_smaller_than_out(TestID):- forall(kaggle_arc(TestID,trn+_,I,O), op_op(v_area,(<),I,O)).

op_op(P2a,P2b,I,O):- call(P2a,I,II),call(P2a,O,OO),call(P2b,II,OO),!.

v_area(I,Size):- vis2D(I,IH,IV), Size is IH * IV.

%compute_all_test_hints(TestID):- arc_test_property(TestID,(trn+1),PP,_),sub_var(i-i,PP),!.
compute_all_test_hints(TestID):- ensure_test(TestID), in_smaller_than_out(TestID),!,
  compute_test_ii_hints(TestID),
  compute_test_oo_hints(TestID),
  compute_test_io_hints(TestID),!.
compute_all_test_hints(TestID):-   
  compute_test_oo_hints(TestID),
  compute_test_ii_hints(TestID),
  compute_test_io_hints(TestID),!.


compute_test_io_hints(TestID):- 
  forall(
    kaggle_arc(TestID,ExampleNum,In,Out), 
     ignore(maybe_compute_test_io_hints(i-o,TestID,ExampleNum,In,Out))).

%maybe_compute_test_io_hints(_,TestID,ExampleNum,_,_):- arc_test_property(TestID,_,_-N),!.
maybe_compute_test_io_hints(IO,TestID,ExampleNum,In,Out):-
    ignore(add_xform_maybe(In,Out)),
      forall(grid_hint_swap_io(IO,In,Out,Hints),add_hint(TestID,ExampleNum,Hints)).

next_example_num(TestID,Example+Num,Example+Num2):- Num2 is Num+1,
  kaggle_arc_io(TestID,Example+Num2,_,_),!.
next_example_num(_TestID,Example+_,Example+0).

compute_test_oo_hints(_):- !.
compute_test_oo_hints(TestID):- 
  forall(
    kaggle_arc_io(TestID,ExampleNum,out,Out1), 
     (next_example(TestID,ExampleNum,ExampleNum2), kaggle_arc_io(TestID,ExampleNum2,out,Out2),
      maybe_compute_test_oo_hints(TestID,ExampleNum,Out1,Out2))),!.

 maybe_compute_test_oo_hints(TestID,ExampleNum,Out1,Out2):- 
   forall(grid_hint_recolor(o-o,Out1,Out2,Hints),add_hint(TestID,ExampleNum,Hints)).

compute_test_ii_hints(_):-!.
compute_test_ii_hints(TestID):- 
  forall(
    kaggle_arc_io(TestID,ExampleNum,in,In1), 
     (next_example(TestID,ExampleNum,ExampleNum2), kaggle_arc_io(TestID,ExampleNum2,in,In2),
      maybe_compute_test_ii_hints(TestID,ExampleNum,In1,In2))),!.

  maybe_compute_test_ii_hints(TestID,ExampleNum,Out1,Out2):- forall(grid_hint_recolor(i-i,Out1,Out2,Hints),add_hint(TestID,ExampleNum,Hints)).


%ptv(T):- p_p_t_no_nl(T),!.
ptv(T):- 
  copy_term(T,CT,Goals),Goals\==[],!,
  \+ \+ 
 ((numbervars(CT+Goals,10,_,[attvar(bind),singletons(true)]),
  ptv(CT), write('\n/*\n'),ptv(Goals),write('\n*/'))).
/*ptv(T):- copy_term(FF,FA,GF), GF \==[],
  numbervars(FA^GF,0,_,[attvar(bind),singletons(true)]),
  sort_safe(GF,GS),write(' '),
  locally(b_setval(arc_can_portray,nil),
      ptv(FA)),format('~N'),
  ignore((GS\==[], format('\t'),ppawt(attvars=GS),nl)),nl,!.
*/
ptv(T):- is_list(T), !, print_tree_no_nl(T),write(' ').
ptv(T):-
  locally(b_setval('$portraying',[]), 
   \+ \+  ((numbervars(T,0,_,[attvars(skip),singletons(true)]), must_det_ll((ptv(T,_)))))),write(' ').


ptv(T,_):- var(T),!, ptv2(T).
ptv(T,_):- \+ compound(T), !, ptv2(T).
ptv(T,_):- is_object(T),!,  show_indiv(ptv,T),!.
%ptv(T,_):- T = showdiff( O1, O2), !, showdiff(O1, O2).
%ptv(T,_):- T = change_obj( O1, O2, Diffs), !, showdiff(O1, O2), writeq(Diffs),!.
%ptv(T,_):- pp(T),!.
ptv(T,_):- 
 nb_current('$portraying',Was)
   ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T))) 
   ; locally(b_setval('$portraying',[T]),ptv0(T)).

ptv0(T):-
  \+ \+ ((prolog_pretty_print_term(T,[quoted(true),portray(true),
  portray_goal(ptv),numbervars(true),singletons(true),blobs(portray),
  quote_non_ascii(true),brace_terms(false),ignore_ops(true)]))).

ptv2(T):- p_p_t_no_nl(T),!.
ptv2(T):- 
  \+ \+ ((numbervars(T,0,_,[]),prolog_pretty_print_term(T,[quoted(true),portray(true),
  numbervars(true),singletons(true),blobs(portray),
  quote_non_ascii(true),brace_terms(false),ignore_ops(true)]))).

must_min_unifier(A,B,D):- must_det_ll(min_unifier_e(A,B,D)).
%min_unifier_e(A,B,C):- compound(B),maybe_extract_value(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.
%min_unifier_e(B,A,C):- compound(B),maybe_extract_value(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.
min_unifier_e(A,B,C):- maybe_extract_value(B,BB), \+ maybe_extract_value(A,_), min_unifier(A,BB,C).
min_unifier_e(B,A,C):- maybe_extract_value(B,BB), \+ maybe_extract_value(A,_), min_unifier(BB,A,C).
min_unifier_e(A,B,C):- min_unifier(A,B,C),nonvar(C),!.
%min_unifier_e(A,B,C):- compound(B),maybe_extract_values(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),must_min_unifier(AABB,B,C),!.
%min_unifier_e(B,A,C):- compound(B),maybe_extract_values(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),must_min_unifier(AABB,B,C),!.
min_unifier_e(_,_,_).

some_min_unifier([A|List],Term):- some_min_unifier_3(A,List,Term).

some_min_unifier_3(A,List,A):- my_maplist('=@='(A),List),!.
some_min_unifier_3(A,[B|List],O):- must_min_unifier(A,B,C), some_min_unifier_3(C,List,O).

is_a_min_unifier(A,B,C):- B==strict,A==loose,!,C=A.
is_a_min_unifier(A,_,C):- A==fg,B\==bg,B\==wbg,!,C=A.
is_a_min_unifier(A,_,C):- plain_var(A),!,C=A.
is_a_min_unifier(A,B,C):- compound(A),A=trim(BB),B==BB,!,C=A.

min_unifier(A,B,C):- A=@=B,!,C=A.
min_unifier(A,B,C):- is_a_min_unifier(A,B,C).
min_unifier(B,A,C):- is_a_min_unifier(A,B,C).
/*

min_unifier(A,B,A):- plain_var(B),!.
min_unifier(A,B,B):- plain_var(A),!.
*/

min_unifier(A,B,D):- number(A),number(B),!,c_proportional(A,B,D).
min_unifier(A,B,AA):- is_grid(A),is_grid(B),!,min_grid_unifier(A,B,AA),!.
min_unifier(A,B,AA):- is_list(A),is_list(B),!,min_list_unifier(A,B,AA),ignore((length(A,AL),length(B,AL),length(AA,AL))).
min_unifier(A,B,AA):- is_cons(A),is_cons(B),!,min_list_unifier(A,B,AA),!.
%min_unifier(A,B,C):- is_list(A),sort_safe(A,AA),A\==AA,!,min_unifier(B,AA,C).
min_unifier(A,B,R):- compound(A),compound(B),
 compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
 my_maplist(must_min_unifier,AA,BB,RR),compound_name_arguments(R,F,RR).
min_unifier(A,B,R):- relax_hint(A,R),\+ (B \= R),!.
min_unifier(A,B,_):- (\+ compound(A);\+ compound(B)),!.

is_cons(A):- compound(A),A=[_|_].


min_grid_unifier(A,B,_):- (\+ is_list(A) ; \+ is_list(B)),!.
min_grid_unifier(A,B,[E1|C]):- select(E1,A,AA),select(E2,B,BB), E1=@=E2 ,!,min_grid_unifier(AA,BB,C).
min_grid_unifier(A,B,[E |C]):- select(E1,A,AA),select(E2,B,BB),min_unifier(E1,E2,E),!,min_grid_unifier(AA,BB,C).
min_grid_unifier(_,_,_).


min_list_unifier(A,B,A):- A=@=B,!.
min_list_unifier(A,B,_):- \+ compound(A);\+ compound(B),!.
min_list_unifier(A,B,AA):- is_list(A),is_list(B), sort_safe(A,AA),sort_safe(B,BB),BB=@=AA,!.
min_list_unifier([A|AA],[B|BB],[A|CC]):- A=@=B,min_list_unifier(AA,BB,CC).
min_list_unifier([A|AA],[B|BB],[C|CC]):- A\=@=B,min_list_unifier(AA,BB,CC),min_unifier(A,B,C),!.
min_list_unifier(A,B,[EC|C]):- is_list(A),is_list(B),  select_two(A,B,E1,E2,AA,BB), min_unifier(E1,E2,EC) ,!,min_list_unifier(AA,BB,C).


min_list_unifier(A,_,_):- (\+ is_list(A), \+ is_cons(A)),!.
min_list_unifier(_,A,_):- (\+ is_list(A), \+ is_cons(A)),!.
min_list_unifier(A,B,_):- (\+ is_list(A) ; \+ is_list(B)),!.

%min_list_unifier([E1|AA],[E2|BB],[EC|C]):- min_unifier(E1,E2,EC) ,!,min_unifier(AA,BB,C).
min_list_unifier(A,B,[E1|C]):- nonvar(A),nonvar(B), select(E1,A,AA),nonvar(E1),select(E2,B,BB),nonvar(E2), E1=@=E2 ,!,min_list_unifier(AA,BB,C).
%min_list_unifier([_|A],[_|B],[_|C]):- !,min_list_unifier(A,B,C).
%min_list_unifier([_],[_|B],[_|B]):-!.
%min_list_unifier([_|B],[_],[_|B]):-!.
min_list_unifier(_,_,_):-!.
%min_unifier(A,B,C):- is_list(B), is_list(A), length(B,L), length(A,L), length(C,L).

grid_hint_swap(IO,In,Out):-
 ignore(kaggle_arc(TestID,ExampleNum,In,Out)),
 w_section(title(grid_hint_swap(IO,TestID>ExampleNum)),
 (maybe_compute_test_io_hints(IO,TestID,ExampleNum,In,Out),
   ignore(print_single_pair(TestID,ExampleNum,In,Out)),
   forall((arc_test_property(TestID,ExampleNum,P,V),
      is_grid(V),sub_var(minus_overlapping_image,P)),print_grid(P,V)),
   with_li_pre(((format('~N%% ~w: ',[IO])),!,
     forall((arc_test_property(TestID,ExampleNum,P,V)),
       ignore((  ( \+ ((is_grid(V), grid_size(V,XX,YY), (XX>3;YY>3)))), ptv1(magenta+cyan,P=V)))))))).


grid_hint_swap_io(IO,In,Out,Hints):-  grid_hint_recolor(IO,In,Out,Hints).
grid_hint_swap_io(I-O,In,Out,rev(Hints)):- I\==O, 
 nop(show_grid_call(trim_to_rect,In^Out,_)),
 grid_hint_recolor(O-I,Out,In,Hints),
 Hints \= mono(comp(_,o-i,value(=@=))).

grid_hint_recolor(_IO,_In,_Out,is_grid_hint). 


grid_hint_recolor(IO,In,Out,Hints):- get_black(Black), grid_hint_io(cbg(Black),IO,In,Out,Hints).
%grid_hint_recolor(IO,In,Out,ogs_hint(IO,Hints)):-  all_ogs(Hints,Out,In), Hints\==[].
grid_hint_recolor(IO,In,Out,mono(Hints)):- arc_option(scan_mono_hints), % fail,
 once((into_monogrid(In,InM),into_monogrid(Out,OutM))),
  (In\==InM;Out\==OutM),grid_hint_io(cbg(wbg),IO,InM,OutM,Hints).
grid_hint_recolor(IO,In,Out,color_ord(Hints)):- fail,
 once((into_color_ord(In,InM),into_color_ord(Out,OutM))),
  (In\==InM;Out\==OutM),grid_hint_io(cbg(bg),IO,InM,OutM,Hints).
/*
grid_hint_recolor(IO,In,Out,mono(Hints)):- % fail,
 once((into_monogrid(In,InM),into_monogrid(Out,OutM))),
  In\==InM,Out\==OutM,
  grid_hint_io(cbg(wbg),IO,InM,OutM,Hints), 
  \+ grid_hint_recolor1(IO,In,Out,_Hint).
*/



%maybe_fail_over_time(Time,Goal):- fail_over_time(Time,Goal).
maybe_fail_over_time(_Time,Goal):- once(Goal).


keep_flipD(I,O):- grid_size(I,H,V),make_grid(V,H,O),
  forall(between(1,H,X),
    forall(between(1,V,Y),
      (X>=Y,get_color_at(X,Y,I,C),
       nb_set_local_point(Y,X,C,O)))).

:- include(kaggle_arc_reduce).
:- include(kaggle_arc_skels).

not_reversed(IO):- (IO \== o-i).

c_proportional(I,O,R):- proportional(I,O,R).
%grid_hint_io(MC,IO,In,Out,find_ogs):- maybe_fail_over_time(1.2,find_ogs(_,_,In,Out)).
%grid_hint_io(MC,IO,In,Out,comp(MC,IO,to_from(InO,OutO))):- disguise_grid(In,InO),disguise_grid(Out,OutO).


grid_hint_io_not_rev(In,Out,sized_grav_rot(H,V,II)):- once((grav_rot(In,_,II),grav_rot(Out,_,OO))),II=@=OO,grid_size(II,H,V).
grid_hint_io_not_rev(In,Out,sized_reduce_grid(H,V,II)):- once((grid_to_norm(_,In,II),grid_to_norm(_,Out,OO))),II=@=OO,grid_size(II,H,V).

grid_hint_io_1(_MC,_IO,In,Out,value('=@=')):- In=@=Out,!.

grid_hint_io(MC,IO,In,Out,comp(MC,IO,Hints)):- grid_hint_io_1(MC,IO,In,Out,Hints).
/*
*/
%grid_hint_io(MC,IO,In,Out,comp(MC,IO,rev_ogs3(Hints))):-  all_ogs3(Out,In,Hints).

grid_hint_io(MC,IO,In,Out,comp(MC,IO,ogs(Hints))):- /*not_reversed(IO),*/ all_ogs(Hints,In,Out).
%grid_hint_io(MC,IO,In,Out,comp(MC,IO,ogs_rev(Hints))):- not_reversed(IO), all_ogs(Hints,Out,In).

grid_hint_io(MC,IO,In,Out,comp(MC,IO,Hints)):-  \+ arc_option(grid_size_only), grid_size(In,IH,IV),grid_size(Out,OH,OV), 
  grid_hint_iso(MC,IO,In,Out,IH,IV,OH,OV,Hints).
grid_hint_io(MC,IO,In,Out,comp(MC,IO,Hints)):- not_reversed(IO), c_proportional(In,Out,Hints).

disguise_grid(I,O):- my_maplist(disguise_row,I,M),O=..[grid|M].
disguise_row(I,O):- O=..[row|I].


%grid_to_so(_II,Obj,In,_NotStrict):- Obj=keypad, In=[[X,X,X],[X,X,X],[X,X,X]].

%ensure_how(How):- var(How),!,member(How,[whole,fg_shapes(nsew)]).
%ensure_how(How):- var(How),!,member(How,[whole,i_pbox]).
%ensure_how(How):- var(How),!,member(How,[whole,complete,i_pbox]).
%ensure_how(How):- var(How),!,member(How,[whole,i_pbox,fg_shapes(nsew)]).
ensure_how(How):- var(How),!,member(How,[nsew,fg_shapes(nsew),colormass,fg_shapes(colormass),force_by_color,alone_dots]).
ensure_how(_How).

%grid_to_objs(Grid,Objs):- findall(Obj,grid_to_objs(Grid,_,Obj),List),list_to_set(List,Objs).
grid_to_objs(Grid,Objs):- ensure_grid(Grid),individuate(complete,Grid,Objs).
grid_to_objs(Grid,How,Objs):- ensure_grid(Grid),ensure_how(How),individuate(How,Grid,Objs).
%grid_to_objs(Grid,Objs):- findall(Obj,grid_to_objs(Grid,complete,Obj),List),list_to_set(List,Objs).
%grid_to_obj(Grid,Obj):- grid_to_objs(Grid,Objs),member(Obj,Objs).

%grid_to_objs(Grid,How,Objs):- (nonvar(Grid)->true;test_grids(_,Grid)), ensure_how(How), individuate(How,Grid,Objs).

% one way to match or find an outlier is compressing things in sets minus one object.. the set that is second to the largest tells you what single object os the most differnt 
objs_shapes(Objs,In):- ensure_test(TestID),test_shapes(TestID,Objs,In).

test_shapes(_TestID, Objs,In):- member(Obj,Objs),object_grid(Obj,In), once(learn_hybrid_shape_board(obj_shapes,In)),fail.
test_shapes(TestID,_Objs,In):- ensure_test(TestID), get_hybrid_set(Set),!,member(In,Set).


learn_hybrid_shape_board(Why,Shape):- nop(dmsg(learn_hybrid_shape(Why,Shape))).

grid_to_obj_other(VM,Grid,O):- other_grid(Grid,Grid2), grid_to_obj_other_grid(VM,Grid,Grid2,O).
grid_to_obj_other_grid(VM,Grid,Grid2,O):- grid_to_objs(Grid2,Objs),grid_to_obj_other_objs(VM,Grid,Objs,O).
grid_to_obj_other_objs(VM,Grid,Objs,O):- 
  objs_shapes(Objs,In),
  maybe_ogs(ROHOV,In,Grid), member(loc2D(OH,OV),ROHOV),
  once((localpoints_include_bg(In,OPoints),offset_points(OH,OV,OPoints,GOPoints), 
  %indv_props_list(Obj,Props),my_partition(is_prop_automatically_rebuilt,Props,_,PropsRetained),
  (nonvar(VM)->true;grid_vm(Grid,VM)),
  make_indiv_object(VM,[],GOPoints,O))).

grid_to_obj(Grid,O):- var(Grid),!,ensure_grid(Grid),grid_to_obj(Grid,O).
grid_to_obj(Grid,O):- grid_to_obj_other(_VM,Grid,O).
grid_to_obj(Grid,Obj):- grid_to_objs(Grid,Objs),member(Obj,Objs).

grid_vm(G,VM):- into_grid_free(G,Grid),grid_to_gid(Grid,GID),
 ((nb_current(GID,VM),is_vm(VM))
  -> true
  ; (grid_to_tid(Grid,ID1),
     into_fti(ID1,[complete],Grid,VM), 
     other_grid(Grid,Grid2), 
     set(VM.target_grid) = Grid2, 
     nb_setval(GID,VM))).

in_to_out(in,out).
in_to_out(out,in).

select_n(_,0,[]).
select_n(List,1,[E]):- member(E,List).
select_n(List,N,[E|Set]):- select(E,List,More),Nminus1 is N - 1,select_n(More,Nminus1,Set).

/*
grid_to_obj(Grid,How,Obj):- grid_to_objs(Grid,How,Objs),member(Obj,Objs).
grid_to_obj(Grid,How,Obj):- grid_to_obj2(Grid,How,Obj).

grid_to_obj2(Grid,How,Obj):- grid_to_objs(Grid,How,Objs),select(Obj1,Objs,Rest),select(Obj2,Rest,_),area_lte(Obj1,Obj2),
  peek_vm(VM),merge_2objs(VM,Obj1,Obj2,[],Obj).
grid_to_obj2(Grid,How,Obj):- grid_to_obj3(Grid,How,Obj).

grid_to_obj3(Grid,How,Obj):- grid_to_objs(Grid,How,Objs),select(Obj1,Objs,Rest),select(Obj2,Rest,Rest2),area_lte(Obj1,Obj2),
  select(Obj3,Rest2,_Rest),area_lte(Obj2,Obj3),peek_vm(VM),
  merge_2objs(VM,Obj1,Obj2,[],Obj12),merge_2objs(VM,Obj12,Obj3,[],Obj).
grid_to_obj3(Grid,How,Obj):- grid_to_obj4(Grid,How,Obj).

grid_to_obj4(Grid,How,Obj):- grid_to_objs(Grid,How,Objs),select(Obj1,Objs,Rest),select(Obj2,Rest,Rest2),area_lte(Obj1,Obj2),
  select(Obj3,Rest2,Rest3),area_lte(Obj2,Obj3),select(Obj4,Rest3,_),area_lte(Obj3,Obj4),
  peek_vm(VM),merge_2objs(VM,Obj1,Obj2,[],Obj12),merge_2objs(VM,Obj12,Obj3,[],Obj123),merge_2objs(VM,Obj123,Obj4,[],Obj).

area_lte(Obj2,Obj3):- area(Obj2,A2),area(Obj3,A3),A2=<A3.
*/

o_first(L,O):- member(O,L),O\==[],!.
grid_get_value(Grid,smallest_object,O):- grid_to_objs(Grid,Objs),smallest_first(Objs,SF),!,o_first(SF,O).
grid_get_value(Grid, largest_object,O):- grid_to_objs(Grid,Objs),largest_first(Objs,SF),!,o_first(SF,O).
grid_get_value(Grid,object_count,Count):- grid_to_objs(Grid,Objs),length(Objs,Count).

%grid_to_so(Grid,nth1Of(Nth1,Named,H,V),In,_NotStrict):- 
grid_to_so(Grid,'whole',Grid).
%grid_to_so(Grid,trimed(whole),Rect):- trim_to_rect(Grid,Rect),Grid\==Rect.
grid_to_so(Grid,In,Prop):-
  grid_to_objs(Grid,Named,Objs),  
  member(Obj,Objs), 
  must_det_ll((ideal_rank(Named,Objs,Obj,Prop),object_grid(Obj,In))).
grid_to_so(_Grid,Named,In):- fail,
  Named=keypad(Color,Counts),
  In=[[_X1,_X2,_X3],[_X4,_X5,_X6],[_X7,_X8,_X9]],
  flatten(In,Flat),my_maplist(count_N(Color,Flat,Counts),Flat).

ideal_rank(Named,[Obj],Obj,Named):-!.
ideal_rank(Named,_Objs,Obj,Prop):- indv_props_list(Obj,Prop),sub_var(Named,Prop),!.
ideal_rank(_Named,_Objs,Obj,Prop):- indv_props_list(Obj,Prop),ideal_prop(Prop),!.
ideal_rank(Named,Objs,Obj,nth1(Nth1,Named)):- nth1(Nth1,Objs,Obj),!.
ideal_prop(pg(_OG,Sz,1,_)):- Sz\=1.

%grid_to_so(Grid,_Out,l(Obj),In,R):- grid_to_so(Grid,Obj,In,R).
%grid_to_so(_Grid,Out,o(Obj),In,R):- grid_to_so(Out,Obj,In,R).


count_N(Color,Flat,Count,Var):- freeze(Var,count_C(Color,Flat,Count)).
count_C(Color,Flat,S+D):- member(Color,Flat),nonvar(Color),!,my_partition(cmatch(Color),Flat,Sames,Diffs),length(Sames,S),length(Diffs,D).

ensure_arc_test_properties(TestID):- ignore(get_current_test(TestID)),
 ignore(( \+ arc_test_property(TestID,common,_,_), compile_and_save_test(TestID))).

:- decl_pt(test_prop,input_objects_first(testID)).
input_objects_first(TestID):- 
  foreach_test(TestID,
  once((ensure_arc_test_properties(TestID), get_black(Black),
   arc_test_property(TestID,common,rev(comp(cbg(Black),o-i,containsAll)),containsAll(o-i))))).

:- decl_pt(test_prop,input_expands_into_output(testID)).
input_expands_into_output(TestID):- 
 foreach_test(TestID,
 once((
  ensure_arc_test_properties(TestID), 
  get_black(Black),
  arc_test_property(TestID,common,comp(cbg(Black),i-o,ogs),ogs(List)),
  \+ \+ (select(ogs(Trim,Whole,Strict,[loc2D(_,_)|_]),List,Rest),
         member(ogs(Trim,Whole,Strict,[loc2D(_,_)|_]),Rest))))).



%grid_hint_iso(MC,IO,In,_Out,_IH,_IV,OH,OV,is_xy_columns):- once(has_xy_columns(In,_Color,OH,OV,)).

into_color_ord(G,GO):- G=GO.

%grid_hint_iso(_MC,IO,_In,_Out,_IH,_IV,OH,OV,grid_size(IO,OH,OV)).
%grid_hint_iso(cbg(_BGC),_-O,_In,Out,_IH,_IV,OH,OV,has_x_columns(O,Y,Color)):- Area is OH*OV, Area>24, maybe_fail_over_time(10.2,has_x_columns(Out,Y,Color,_)), Y>1.
%grid_hint_iso(cbg(_BGC),_-O,_In,Out,_IH,_IV,OH,OV,has_y_rows(O,Y,Color)):- Area is OH*OV, Area>24, maybe_fail_over_time(10.2,has_y_rows(Out,Y,Color,_)), Y>1.

grid_hint_iso(cbg(BGC),IO,Out,In,GH,GV,GH,GV,Hints):-   
   mapgrid(remove_color_if_same(BGC),Out,In,NewIn),
   mass(NewIn,Mass), unique_colors(In,Colors),unique_colors(NewIn,LeftOver), LeftOver\==Colors,
   (Mass==0 -> 
     ( Hints=containsAll(IO),learn_hybrid_shape_board(subtraction,In)) ; 
     ( Hints=containsAllExceptFor(IO,LeftOver),learn_hybrid_shape_board(subtraction,NewIn))),!.
  

% NewIn\=@=In,print_grid('leftover',NewIn).
%grid_hint_iso(cbg(BGC),IO,In,Out,_IH,_IV,_OH,_OV,cg(IO,Hints)):- comp_o(IO), grid_color_hint(In,Out,Hints).
%grid_hint_iso(_,IO,_In,_Out,IH,IV,  OH,OV,comp(MC,IO,size_r(H,V))):- comp_o(IO), V is rationalize(IV/OV), H is rationalize(IH/OH).
%grid_hint_iso(cbg(_BGC),IO,In,Out,_IH,_IV,_OH,_OV,cg(IO,mass_r(Mass))):- comp_o(IO), mass(In,IMass),mass(Out,OMass), IMass\==0,Mass is rationalize(OMass/IMass),Mass\==1.
/*
grid_hint_iso(cbg(_BGC),i-o,Out,_,_IH,_IV,_OH,_OV,rev(RInfo)):- 
 setup_call_cleanup(flag(indiv,Was,0),
  ((findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo)),
                    flag(indiv,_,Was)).
grid_hint_iso(cbg(_BGC),i-o,_In,Out,_IH,_IV,_OH,_OV,RInfo):- !,
 setup_call_cleanup(flag(indiv,Was,0),
  ((findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo)),
                    flag(indiv,_,Was)).
*/
/*grid_hint_iso(cbg(_BGC),i-o,_In,Out,_IH,_IV,_OH,_OV,RInfo):- 
 setup_call_cleanup(flag(indiv,Was,0),
  ((wno( findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo))),
                    flag(indiv,_,Was)).
*/
termsub_to_atom(List,OO):- is_list(List),!,my_maplist(termsub_to_atom,List,LL),
 atomic_list_concat(LL,'_',O),atomic_list_concat(['[',O,']'],OO).
termsub_to_atom(T,O):- sformat(O,'~w',[T]).

fix_iz(Z,ZZ):- atom(Z),!,ZZ=..[Z,true].
fix_iz(nth(_),'').
fix_iz(Z,ZZ):- is_list(Z),last(Z,ZZ),!.
fix_iz(iz(Z),ZZ):- nonvar(Z), !, fix_iz(Z,ZZ).
fix_iz(Z,ZZ):- compound(Z),arg(1,Z,A),is_list(A),last(A,ZZ),!.
fix_iz(Z,Z):-!.

rinfo(obj(List0),RInfo):- 
  my_maplist(must_det_ll,
  [select(ord(N),List0,List),
  atomic_list_concat([obj,N],'_',Key),
  Obj = obj(List0),
  localpoints_include_bg(Obj,LocalPoints),
  %loc2D(Obj,X,Y),
  get_bg_label(BGL),
  points_to_grid(LocalPoints,Grid),mapgrid(sometimes_assume(=,BGL),Grid),
  select(shape_rep(grav,Shape),List,Rest2),mapgrid(sometimes_assume(=,BGL),Shape),
  Rest3 = Rest2,
  obj_to_oid(Obj,MyID),
  must_det_ll((remove_too_verbose(MyID,Rest3,TV00))),flatten([TV00],TV0),
  must_det_ll((include(not_too_verbose,TV0,TV1),my_maplist(fix_iz,TV1,TV)))]),!,
  member(MrT,[oform(Shape),ogrid(Grid)|TV]),once((MrT=..MrTL, RInfoM=..[Key|MrTL],rinfo(RInfoM,RInfo))).
rinfo(Info,RInfo):- Info=..[P,N,A|InfoL], atomic_list_concat([P,N],'_',PN),!, RInfo=..[PN,A|InfoL].
rinfo(Info,Info):-!.

grid_color_hint(In,Out,Hints):-
    once((unique_colors(In,IColor0),unique_colors(Out,OColor0),
    include(is_color,IColor0,IColor),
    include(is_color,OColor0,OColor),
    intersection(IColor,OColor,Shared,IOnlyC,OOnlyC))),
    sort_safe(Shared,SharedS),
    member(Hints,[shared_color(SharedS),lhs_only(IOnlyC),rhs_only(OOnlyC)]).



comp_o(_-o):-!.
comp_o(_-i).

entire_row([Color|Row]):- entire_row(Color,Row).
entire_row(Color,Row):- my_maplist(=@=(Color),Row).
%entire_row(black,Row):- !, my_maplist(=(black),Row).
mentire_row(C2,OtherRow):- entire_row(C2,OtherRow),!.
% mentire_row(C2,OtherRow):- include(\==(C2),OtherRow,Missing),  once((length(Missing,L),L=<1,my_maplist(=(C2),Missing))),!.

type_hint_pred(grid_area/1).
grid_area(In,Area):- grid_size(In,H,V), Area is H*V.

remove_color_if_same(BGC,X,Y,BGC):- X=@=Y,!.
remove_color_if_same(_BGC,_X,Y,Y).


has_xy_columns(In,Color,X,Y,BorderNumsX,BorderNumsY):- 
  grid_area(In,HV), HV > 24,
  %grid_size(In,GH,GV), 
  has_x_columns(In,X,Color,BorderNumsX),
  has_y_rows(In,Y,Color,BorderNumsY),
  once(X>1 ; Y>1),!.

has_xy_chunks(In,Color,Chunks):-
  has_xy_columns(In,Color,X,Y,BorderNumsX,BorderNumsY),
  make_grid(X,Y,Chunks),
  %pp(slicing(BorderNumsX,BorderNumsY,onto(X,Y))),
  gather_chunks(Color,In,Chunks,1,1,X,Y,BorderNumsX,BorderNumsY).

grid_x_y_area(In,X,Y,Color,Cells):- 
  has_xy_chunks(In,Color,Chunks),
  nth1(Y,Chunks,Row),nth1(X,Row,Cells).


gather_chunks(_Color,_In,_Chunks,_X,Y,_GX,GY,_BorderNumsX,_BorderNumsY):- Y>GY,!.
gather_chunks(Color,In,Chunks,X,Y,GX,GY,BorderNumsX,BorderNumsY):-
  nth1(Y,Chunks,Row),nth1(X,Row,Cell),
  nth1(X,BorderNumsX,SX,RX),nth1(X,RX,EX),
  nth1(Y,BorderNumsY,SY,RY),nth1(Y,RY,EY),
  SX1 is SX + 1, 
  SY1 is SY + 1,
  EX1 is EX - 1,
  EY1 is EY - 1,
  clip(SX1,SY1,EX1,EY1,In,Clip),
  % print_grid((X,Y),Clip), pp(clipped(EX,EY,into(X,Y))),
  % once(Clip = [_,[_,Cell|_]|_];Clip = [[Cell|_]|_]),  
  get_black(Black),
  once(((Clip = [_,[_,C|_]|_];Clip = [[C|_]|_];(member(CR,Clip),member(C,CR))),C\==Color,C\==Black,Cell=C);Cell=Clip),
  (GX =< X -> (Yi is Y + 1, Xi is 1) ; (Xi is X+1, Yi is Y)),
  gather_chunks(Color,In,Chunks,Xi,Yi,GX,GY,BorderNumsX,BorderNumsY).


conjoin_color(Color,Value,ord(Value,Color)).

x_columns(In,Out):- into_grid(In,G), has_x_columns(G,_X,Color,BorderNums),  must_det_ll(ground(In+Color)), my_maplist(conjoin_color(Color),BorderNums,Out).
y_rows(In,Out):-  into_grid(In,G), has_y_rowz(G,_X,Color,BorderNums), my_maplist(conjoin_color(Color),BorderNums,Out).

has_x_columns(In,X,Color,BorderNums):- ((rot90(In,In90), !, has_y_rows(In90,X,Color,BorderNums))).
has_y_rows(In,Y,Color,BorderNums):- has_y_rowz(In,Y,Color,BorderNums).

%has_y_rowz(_In,_Y,_Color,_BorderNums):- arc_option(grid_size_only), !,fail.
has_y_rowz(In,Y,Color,BorderNums):- plain_var(Color), !, unique_colors(In,Colors),reverse(Colors,ColorsR),!,
  member(Color,ColorsR),is_color(Color), has_y_rowz(In,Y,Color,BorderNums).

has_y_rowz(In,Y,Color,BorderNums):- must_det_ll(ground(In+Color)),
  has_y_columns1(In,Y,Color,BorderNums),
  must_det_ll(is_color(Color)),
  nop(\+ illegal_column_data1(In,Color,BorderNums)),
  \+ ((append(_,[RowBefore,RowNext|_],In), entire_row(Color,RowBefore), entire_row(Color,RowNext))).

has_y_columns1(In,Y,Color,BorderNums):- 
  append([First|_],[Last],In), mentire_row(Color,First),mentire_row(Color,Last),!,  
  findall(Nth,(nth1(Nth,In,Row),entire_row(Color,Row)),BorderNums),
  length(BorderNums,YM1),YM1>2, Y is YM1 - 1.

has_y_columns1(In,Y,Color,Out):-   must_det_ll(ground(In+Color)),
  append([First|_],[Last],In),
  \+ mentire_row(Color,Last), \+ mentire_row(Color,First),!,
  findall(Nth,(nth1(Nth,In,Row),entire_row(Color,Row)),BorderNums), BorderNums \==[],
  length(BorderNums,YM1),Y is YM1 + 1,
  length(In,GY1),GY is GY1 + 1,
  append([0|BorderNums],[GY],Out).

% bleeding of this color
illegal_column_data1(In,Color,BorderNums):- 
  nth1(Nth,In,OtherRow),\+ member(Nth,BorderNums),
  append(_,[C1,C2|_],OtherRow),C1 == C2, C1 == Color,!.

illegal_column_data1(In,Color,_):- 
(append(_,[RowBefore,RowNext|_],In), entire_row(Color,RowBefore), entire_row(Color,RowNext)),!.

% completely differnt colored border
illegal_column_data1(In,Color,BorderNums):-  fail,
  nth1(Nth,In,OtherRow),\+ member(Nth,BorderNums),
  enum_colors(C2),C2\==Color,entire_row(C2,OtherRow),!.

illegal_column_data(In,Color,BorderNums):- 
  member(Nth,BorderNums),
  NthLess is Nth+1 , NthMore is Nth+2,
  \+ member(NthLess,BorderNums),
  \+ member(NthMore,BorderNums),
  nth1(NthLess,In,Row1),
  nth1(NthMore,In,Row2),
  (member(C1,Row2),member(C2,Row1)),
  C1 == C2, C1 == Color,!.




save_the_alt_grids(TestID):- 
 forall(ensure_test(TestID),
  forall(kaggle_arc(TestID,ExampleNum,I,O),
    once(save_the_alt_grids(TestID,ExampleNum,[],I,O)))).

save_the_alt_grids(TestID,ExampleNum):-
  arc_test_property(TestID,ExampleNum,has_blank_alt_grid,_),!.
save_the_alt_grids(TestID,ExampleNum):- 
  forall(kaggle_arc(TestID,ExampleNum,I,O),
    once(save_the_alt_grids(TestID,ExampleNum,[],I,O))).

save_the_alt_grids(TestID,ExampleNum,_,_,_):- arc_test_property(TestID,ExampleNum,iro(_),_),!.
save_the_alt_grids(TestID,ExampleNum,XForms,In,Out):- same_sizes(In,Out),!,
  save_the_alt_grids_now(TestID,ExampleNum,XForms,In,Out).
save_the_alt_grids(TestID,ExampleNum,XForms,In,Out):-
  some_norm(In,OpI,NIn), some_norm(Out,OpO,NOut), 
  once(NIn\=@=In;NOut\=@=Out),
  same_sizes(NIn,NOut),!,
  save_the_alt_grids_now(TestID,ExampleNum,[ops(OpI,OpO)|XForms],NIn,NOut).
save_the_alt_grids(TestID,ExampleNum,_XForms,In,Out):- 
  assert_test_property(TestID,ExampleNum,iro(none),In),
  assert_test_property(TestID,ExampleNum,ori(none),Out),!.
  
save_the_alt_grids_now(TestID,ExampleNum,XForms,In,Out):-
  my_maplist(save_grid_calc(TestID,ExampleNum,XForms,In,Out),
     [fg_intersectiond,fg_intersectiond_mono,cell_minus_cell,mono_cell_minus_cell]),
  ignore(( \+ has_blank_alt_grid(TestID,ExampleNum))),!,
  my_maplist(save_grid_calc(TestID,ExampleNum,XForms,In,Out),[minus_overlapping_image,overlapping_image]).

colors_of(O,Cs):- unique_fg_colors(O,Cs),!.

/*save_the_alt_grids_now2(TestID,ExampleNum,XForms,In,Out).
save_the_alt_grids_now2(TestID,ExampleNum,XForms,In,Out):- 
 must_det_ll((
    colors_of(In.cell_minus_cell(Out),CsIn),
    colors_of(Out.cell_minus_cell(In),CsOut),
    colors_of(In,CsInO), 
    colors_of(Out,CsOutO),
    ExtraColors = color_set(CsInO).add(CsOutO).rem(CsIn).rem(CsOut),
    gset(In.overlapping_image)=In.inv(unbind_color(ExtraColors)),
    gset(Out.overlapping_image)=Out.inv(unbind_color(ExtraColors)),
    assert_test_property(TestID,ExampleNum,ori([overlapping_image|XForms]),Out.overlapping_image),
    assert_test_property(TestID,ExampleNum,iro([overlapping_image|XForms]),In.overlapping_image))),!.*/

minus_overlapping_image(In,Other,MOI):-  overlapping_image(In,Other,OLIn), grid_minus_grid(In,OLIn,MOI).

/*
overlapping_image(In,Other,RIO):- detect_iro(In,Other,TestID,ExampleNum,iro),
   arc_test_property(TestID,ExampleNum,iro([overlapping_image]),RIO),!.
overlapping_image(In,Other,ROI):- detect_ori(In,Other,TestID,ExampleNum,ori),
   arc_test_property(TestID,ExampleNum,ori([overlapping_image]),ROI),!.
*/
overlapping_image(In,Other,OLIn):-
  colors_of(In.cell_minus_cell(Other),CsIn),
  colors_of(Other.cell_minus_cell(In),CsOut),
  colors_of(In,CsInO), 
  colors_of(Other,CsOutO),
  ExtraColors = color_set(CsInO).add(CsOutO).rem(CsIn).rem(CsOut),
  OLIn=In.inv(unbind_color(ExtraColors)).

%32e9702f

save_grid_calc(TestID,ExampleNum,XForms,In,Out,Op):-  
  call(Op,In,Out,RIO), call(Op,Out,In,ROI),!,
  assert_test_property(TestID,ExampleNum,iro([Op|XForms]),RIO),
  assert_test_property(TestID,ExampleNum,ori([Op|XForms]),ROI),
  print_ss(no(Op,TestID,ExampleNum,XForms),RIO,ROI).



same_sizes([I|In],[O|Out]):- length(I,Cols),length(O,Cols),length(In,Rows0),length(Out,Rows0).

some_norm(Out,[],Out).
some_norm(Out,[trim_to_rect|Op],NOut):- notrace((trim_to_rect(Out,Mid),Out\=@=Mid)),some_norm(Out,Op,NOut).
some_norm(Out,[grav_rot(Rot)],NOut):- grav_rot(Out,Rot,NOut),!.

has_blank_alt_grid(TestID,ExampleNum):- blank_alt_grid_count(TestID,ExampleNum,N),N>0.

blank_alt_grid_count(TestID,ExampleNum,N):-
  arc_test_property(TestID,ExampleNum,has_blank_alt_grid,N),!.
blank_alt_grid_count(TestID,ExampleNum,N):- 
  save_the_alt_grids(TestID,ExampleNum),
  findall(_,(alt_grids(TestID,ExampleNum,_,ROI),once(mass(ROI,Mass)),Mass=0),L),length(L,N),!,
  assert_test_property(TestID,ExampleNum,has_blank_alt_grid,N),
  ignore((N==0,
    retractall(arc_test_property(TestID,ExampleNum,iro([_|_]),_)),
    retractall(arc_assert_test_property(TestID,ExampleNum,ori([_|_]),_)),
    dmsg(warn(noBlankFor(TestID,ExampleNum))))).


alt_grids(TestID,ExampleNum,Name,ROI):- arc_test_property(TestID,ExampleNum,Name,ROI),is_grid(ROI).
alt_grids(TestID,ExampleNum,Name,RIO,ROI):- 
  arc_test_property(TestID,ExampleNum,iro([Name|_]),RIO),
  arc_test_property(TestID,ExampleNum,ori([Name|_]),ROI).


:- include(kaggle_arc_footer).
%globalpoints(grid,points)

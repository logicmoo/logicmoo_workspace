/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

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

test_supergrid:- clsmake, forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_pair_hints(TestID,ExampleNum,In,Out)).

sub_atom_value(TestID,A):- sub_term(A,TestID),atom(A).

print_directive(P):- format('~N:- ~q. ~n',[P]).
write_intermediatre_header:- 
  print_directive(encoding(text)),
  forall(  (test_local_save(F,A),nl),
      maplist(print_directive,[%abolish(F/A),
                               multifile(F/A),dynamic(F/A),discontiguous(F/A),public(F/A),export(F/A),module_transparent(F/A)])).

print_ref(Ref):- is_clause_ref(Ref), clause(H,B,Ref),!,print_ref((H:-B)).
print_ref((X:-True)):- True == true,!, print_ref(X).
print_ref(G):- format('~N'),write_canonical(G),format('.~n'),!.
%print_ref(G):- \+ \+ ((numbervars(G,0,_,[attvar(bind),singletons(true)]), format('~N~@.~n',[write_canonical(G)]))),!.

test_name_output_file(TestID,File):- sub_atom_value(TestID,OID),!,atomic_list_concat(['out/',OID,'.ansi'],File).


compile_and_save_test:- clsmake, get_current_test(TestID),time(compile_and_save_test(TestID)).
compile_and_save_test(TestID):-
  all_arc_test_name(TestID),
  %ignore(retract(saved_training(TestID))),
  %ignore(retract(process_test(TestID))),
 once((
  retractall(arc_test_property(TestID,_,_)),
  arc_assert(saved_training(TestID)),
  arc_assert(process_test(TestID)),
  detect_all_training_hints(TestID),  
  %individuate_pairs_from_hints(TestID),
  %train_test(TestID,train_using_io),  
  save_supertest(TestID))).

individuate_pairs_from_hints(TestID):- 
  arc_assert(individuate_test_grids(TestID)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),
   individuate_pair_here(TestID,ExampleNum,In,Out)).

individuate_pair_here(TestID,Trn+N1,In,Out):-
  ip(complete,In,Out),
  nop(train_for_objects_from_1pair(_{},TestID,[Trn,'i',N1,'o',N1],In,Out,_DictMid)).

save_supertest:- get_current_test(TestID),save_supertest(TestID).
save_supertest(TestID):-   
   saveable_test_info(TestID,Info),
   test_name_output_file(TestID,File),
   setup_call_cleanup(open(File,write,O,[create([default]),encoding(text)]), 
       with_output_to(O,(
         write_intermediatre_header,
         maplist(print_ref,Info))),
      close(O)), statistics.



detect_all_training_hints:- clsmake, get_current_test(TestID),detect_all_training_hints(TestID).
detect_all_training_hints(TestID):- 
  training_only_exmaples(ExampleNum), forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_pair_hints(TestID,ExampleNum,In,Out)),
  color_print(magenta,call(((compute_and_show_test_hints(TestID))))).
training_only_exmaples(ExampleNum):- ignore(ExampleNum=(trn+_)).
detect_test_hints1:- clsmake, get_current_test(TestID),detect_test_hints1(TestID).
detect_test_hints1(TestID):- 
 ignore(luser_getval(example,ExampleNum)), training_only_exmaples(ExampleNum),
 forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_pair_hints(TestID,ExampleNum,In,Out)).
  

color_subst([],[],[]):-!.
color_subst([O|OSC],[I|ISC],[O-I|O2I]):-
  color_subst(OSC,ISC,O2I).
color_subst(_OSC,_ISC,[]):-!.

detect_pair_hints(TestID,ExampleNum,In,Out):- 
  assert_id_grid_cells(In), assert_id_grid_cells(Out),
  detect_supergrid_tt(TestID,ExampleNum,In,Out,TT),  
  guess_board(TT),
  %print(TT),
  grid_hint_swap(i-o,In,Out),
  dash_chars,!.

guess_board(TT):- arc_setval(TT,guess_board,t).

detect_supergrid_tt(TestID,ExampleNum,In0,Out0,TT):-
 must_det_ll(((
  dmsg(detect_supergrid_tt(TestID,ExampleNum)),
  % grid_size(In0,IH,IV), 
  grid_size(Out0,OH,OV), % max_min(OV,IV,V,_),max_min(OH,IH,H,_),

  into_bicolor(In0,In), into_bicolor(Out0,OOut),

  pair_dictation(TestID,ExampleNum,In0,Out0,T),
   T.in = In0, T.out = Out0,
  ((OV==1,OH==1) -> (O2I=[]) ; (T.in_specific_colors = ISC, T.out_specific_colors = OSC,    color_subst(OSC,ISC,O2I))),
  subst_1L(O2I,OOut,Out), subst_1L(O2I,Out0,OutF),
  dict_pairs(T,_,Pairs),
  list_to_rbtree_safe(Pairs,TT),!,
  arc_setval(TT,rhs_color_remap, O2I),

  show_colorfull_idioms(In0),
  show_colorfull_idioms(Out0),

  maplist(must_det_ll,[
  (most_d_colors(Out,CO,NO),arc_setval(TT,out_d_colors,CO),arc_setval(TT,out_map,NO)),  
  (most_d_colors(In,CI,NI),  arc_setval(TT,in_d_colors,CI), arc_setval(TT,in_map,NI)),
  fif(find_ogs(HOI,VOI,In0,OutF),arc_setval(TT,z_in_contains_out,(HOI,VOI))),
  fif(find_ogs(HIO,VIO,OutF,In0),arc_setval(TT,z_out_contains_in,(HIO,VIO))),
  %@TODO record in the out_in _may_ hold the in_out 
  dash_chars,
  dash_chars,
  dmsg(detect_all_training_hints(TestID,ExampleNum)),
    print_side_by_side(cyan,NI,CI,_,NO,CO),
  ppt(O2I),
    print_side_by_side(cyan,In0,task_in(ExampleNum),_,Out0,task_out(ExampleNum)),
  
  %show_patterns(In),show_patterns(Out),
  true])))),
  %color_print(magenta,call(((compute_and_show_test_hints(TestID))))).
  !.
  
  %gset(TT.z_contains_out)=in(HIO,VIO),
  %gset(TT.z_contains_in)=out(HOI,VOI),
  
arc_test_property(g,b,d):-fail.

  
is_fti_step(obj_into_cells).
is_fti_step(print_grid).
obj_into_cells(VM):- 
  vm_to_printable(VM,GridObj),
  globalpoints(GridObj,Row1),
  flatten([Row1],Row1F),list_to_set(Row1F,Row1S),
  maplist(only_color_data,Row1S,ColorRow),
  set_vm(grid,[ColorRow]).


show_found(HOI,VOI,H,V,Info,F):-
  HO is HOI-3, VO is VOI-3, 
  show_found2(HO,VO,H,V,Info,F).

show_found2(HO,VO,H,V,Info,_F):- wdmsg(show_found2(HO,VO,H,V,Info)),!.
show_found2(HO,VO,H,V,Info,F):-
  offset_grid(HO,VO,F,OF),
  constrain_grid(f,_TrigF,OF,FF),!,
  print_grid(H,V,Info,FF),!.

test_grid_hint:- clsmake, forall(all_arc_test_name(TestID),test_grid_hint(TestID)).

save_grid_hints:-  forall(all_arc_test_name(TestID),test_grid_hint(TestID)),
  listing(arc_test_property/3).

%test_grid_hint:- get_current_test(TestID),test_grid_hint(TestID).
test_grid_hint(TestID):- compute_and_show_test_hints(TestID).



hint_functor(cg(IO,Hint),cg(IO,F)):- !, hint_functor(Hint,F).
%hint_functor(comp(MC,i-o,Hint),F):- !, hint_functor(Hint,F).
hint_functor(comp(MC,IO,Hint),comp(MC,IO,F)):- hint_functor(Hint,F).
hint_functor(rev(Hint),rev(F)):- !, hint_functor(Hint,F).
hint_functor(mono(Hint),mono(F)):- !, hint_functor(Hint,F).
hint_functor(vis_hv_term(Hint),(F)):- !, hint_functor(Hint,F).
hint_functor(Hint,F):- functor(Hint,F,_).


hint_data(cg(_IO,Hint),F):- !, hint_data(Hint,F).
hint_data(comp(_MC,_IO,Hint),F):- !, hint_data(Hint,F).
hint_data(rev(Hint),F):- !, hint_data(Hint,F).
hint_data(vis_hv_term(Hint),(F)):- !, hint_data(Hint,F).
hint_data(mono(Hint),(F)):- !, hint_data(Hint,F).
hint_data(Data,Data).


relax_hint(G,G):- (\+ compound(G)) -> !; true.
relax_hint(rev(G),rev(GG)):- !, relax_hint(G,GG).
relax_hint(mono(G),mono(GG)):- !, relax_hint(G,GG).
relax_hint(cg(W,G),cg(W,GG)):- !, relax_hint(G,GG).
relax_hint(G,GG):- duplicate_term(G,GG),arg(N,G,E),relax_arg(E,EE),nb_setarg(N,GG,EE).
%relax_hint(G,GG):- functor(G,F,A),functor(GG,F,A).

relax_arg(E,C):- is_color(E),!,relax_color_arg(E,C).
%relax_arg(E,_):- var(E),!,fail.
relax_arg(E,E):- var(E) -> !; true.
relax_arg(E,len(L)):- is_list(E),length(E,L).
relax_arg(_,_).


:- dynamic(io_xform/3).
add_xform_maybe(In1,Out1):- ignore(get_current_test(TestID)),
                    ThisXForm=io_xform(TestID,In1,Out1),
                    ThatXForm=io_xform(TestID,_In2,_Out2),                     
                    (call(ThatXForm) 
                      -> (min_unifier(ThisXForm,ThatXForm,NewXForm),
                                        retractall(ThatXForm),retractall(NewXForm),asserta(NewXForm))
                     ; asserta(ThisXForm)),!.
                    
add_hint(TestID,Hint,N):- 
  hint_functor(Hint,F),hint_data(Hint,D), assert_test_property(TestID,grid_fhint(F),D-N).

assert_test_property(TestID,Prop,Data):-
  arc_assert(arc_test_property(TestID,Prop,Data)).
  
  % forall((kaggle_arc_io(TestID,(trn+N),in,Out1),N2 is N+1,  (kaggle_arc_io(TestID,(trn+N2),in,Out2)->true;kaggle_arc_io(TestID,(trn+0),in,Out2)),  grid_hint_recolor(i-i,Out1,Out2,Hint)),add_hint(TestID,Hint,N)).


/*
compute_and_show_test_hints(TestID):- format('~N'),
  findall(Hint-N,(kaggle_arc(TestID,(trn+N),In,Out), grid_hint_swap(i-o,In,Out,Hint)),HintsIO),
  findall(Hint-N,(kaggle_arc_io(TestID,(trn+N),in,In1),  N2 is N+1, (kaggle_arc_io(TestID,(trn+N2),in,In2)->true;kaggle_arc_io(TestID,(trn+0),in,In2)), grid_hint_recolor(i-i,In1,In2,Hint)),HintsII),
  findall(Hint-N,(kaggle_arc_io(TestID,(trn+N),out,Out1),N2 is N+1, (kaggle_arc_io(TestID,(trn+N2),out,Out2)->true;kaggle_arc_io(TestID,(trn+0),out,Out2)), grid_hint_recolor(o-o,Out1,Out2,Hint)),HintsOO),
  append([HintsIO,HintsOO,HintsII],Hints),
  keysort(Hints,SHints),  
  maplist(aquire_hints(TestID,SHints),SHints),
  format('~N'),
  nop()),
  list_common_props(TestID).
*/
compute_and_show_test_hints(TestID):- format('~N'),
  compute_all_test_hints(TestID),
  ignore(list_common_props_so_far(TestID)),!,
  listing(arc_test_property(TestID,_,_)),
  listing(io_xform(TestID,_,_)),
  %ignore(list_common_props(TestID)),!,
  format('~N').

list_common_props_so_far(TestID):-
 (\+ arc_test_property(TestID,grid_fhint(_),_Data) -> compute_all_test_hints(TestID); true),
 findall(F=Common,
  (arc_test_property(TestID, grid_fhint(F),_-0),
    retractall(arc_test_property(TestID,common(F),_)),
    (( findall(Data,arc_test_property(TestID,grid_fhint(F),Data-_),Commons),
      once((min_unifier(Commons,Common),nonvar(Common))))),
      arc_assert(arc_test_property(TestID,common(F),Common))),FComs),
  sort(FComs,SComs),  
  print_test(TestID),
  wots(SS,maplist(ptv1,SComs)),
  call((format('~N % ~w: ~s.~n',[list_common_props,SS]))),
  !.

ptv1(T):- format('~N'),(ground(T) -> color_print(cyan,call(bold_print(print_tree(T)))) ; color_print(magenta,call(print_tree(T)))).


compute_all_test_hints(TestID):- 
  compute_test_io_hints(TestID),
  compute_test_oo_hints(TestID).

compute_test_io_hints(TestID):- 
  forall(
    kaggle_arc(TestID,(trn+N),In,Out), 
     (ignore(add_xform_maybe(In,Out)),
      forall(grid_hint_swap_io(i-o,In,Out,Hint),add_hint(TestID,Hint,N)))).

compute_test_oo_hints(TestID):- 
  forall(
    kaggle_arc_io(TestID,(trn+N),out,Out1), 
     (N2 is N+1, (kaggle_arc_io(TestID,(trn+N2),out,Out2)->true;kaggle_arc_io(TestID,(trn+0),out,Out2)),
      forall(grid_hint_recolor(o-o,Out1,Out2,Hint),add_hint(TestID,Hint,N)))),!.

%ptv(T):- print_tree_no_nl(T),!.
ptv(T):-
  locally(b_setval('$portraying',[]), 
   \+ \+  ((numbervars(T,0,_,[attvars(skip),singletons(true)]), must_det_ll((ptv(T,_)))))).


ptv(T,_):- var(T),!, ptv2(T).
ptv(T,_):- \+ compound(T), !, ptv2(T).
ptv(T,_):- is_object(T),!,  debug_as_grid(T),!.
%ptv(T,_):- T = showdiff( O1, O2), !, showdiff(O1, O2).
%ptv(T,_):- T = change_obj( O1, O2, Diffs), !, showdiff(O1, O2), writeq(Diffs),!.
ptv(T,_):- print_tree_no_nl(T),!.
ptv(T,_):- 
 nb_current('$portraying',Was)
   ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T))) 
   ; locally(b_setval('$portraying',[T]),ptv0(T)).

ptv0(T):-
  \+ \+ ((prolog_pretty_print_term(T,[quoted(true),portray(true),
  portray_goal(ptv),numbervars(true),singletons(true),blobs(portray),
  quote_non_ascii(true),brace_terms(false),ignore_ops(true)]))).

ptv2(T):- print_tree_no_nl(T),!.
ptv2(T):- 
  \+ \+ ((numbervars(T,0,_,[]),prolog_pretty_print_term(T,[quoted(true),portray(true),
  numbervars(true),singletons(true),blobs(portray),
  quote_non_ascii(true),brace_terms(false),ignore_ops(true)]))).


min_unifier([A|List],Term):- min_unifier3(A,List,Term).

min_unifier3(A,List,A):- maplist('=@='(A),List),!.
min_unifier3(A,[B|List],O):- min_unifier(A,B,C), min_unifier3(C,List,O).

min_unifier(A,B,C):- A=@=B,!,C=A.
min_unifier(_,B,B):- plain_var(B),!.
min_unifier(A,_,A):- plain_var(A),!.

min_unifier(A,B,AA):- is_list(A),is_list(B),!,min_list_unifier(A,B,AA), ignore((length(A,AL),length(B,AL),length(AA,AL))).
min_unifier(A,B,AA):- is_cons(A),is_cons(B),!,min_list_unifier(A,B,AA).

%min_unifier(A,B,C):- is_list(A),sort(A,AA),A\==AA,!,min_unifier(B,AA,C).
min_unifier(A,B,R):- compound(A),compound(B),compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
 maplist(min_unifier,AA,BB,RR),compound_name_arguments(R,F,RR).

min_unifier(A,B,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.
min_unifier(B,A,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.

min_unifier(A,B,_):- (\+ compound(A);\+ compound(B)),!.
min_unifier(A,B,R):- relax_hint(A,R),\+ (B \= R),!.

is_cons(A):- compound(A),A=[_|_].

min_list_unifier(A,B,AA):- is_list(A),is_list(B),sort(A,AA),sort(B,BB),BB=@=AA,!.
min_list_unifier(A,B,[E1|C]):- is_list(A),is_list(B),select(E1,A,AA),select(E2,B,BB), nop(( \+ is_color(E1), \+ is_color(E2))), E1=@=E2,!,min_list_unifier(AA,BB,C).
min_list_unifier([_|A],[_|B],[_|C]):- !,min_list_unifier(A,B,C).
min_list_unifier([_],[_|B],[_|B]):-!.
min_list_unifier([_|B],[_],[_|B]):-!.
%min_unifier(A,B,C):- is_list(B), is_list(A), length(B,L), length(A,L), length(C,L).

grid_hint_swap(IO,In,Out):-
 ((findall(Data,(grid_hint_swap_io(IO,In,Out,Hint),hint_data(Hint,Data)),Hints),
 call((format('~N % ~w: ~@.~n',[IO,call(ptv,Hints)]))))).

grid_hint_swap_io(IO,In,Out,Hint):-  grid_hint_recolor(IO,In,Out,Hint).
grid_hint_swap_io(I-O,In,Out,rev(Hint)):- grid_hint_recolor(O-I,Out,In,Hint).

grid_hint_recolor(IO,In,Out,mono(Hint)):-  
 once((into_monogrid(In,In0),into_monogrid(Out,Out0))),
  In\==In0,Out\==Out0,
  grid_hint_io(monochrome,IO,In0,Out0,Hint), 
  \+ grid_hint_recolor1(IO,In,Out,_Hint).

grid_hint_recolor(IO,In,Out,Hint):- grid_hint_recolor1(IO,In,Out,Hint).

grid_hint_recolor1(IO,In,Out,Hint):-  grid_hint_io(cbg(black),IO,In,Out,Hint).

%maybe_fail_over_time(Time,Goal):- fail_over_time(Time,Goal).
maybe_fail_over_time(_Time,Goal):- once(Goal).


c_proportional(I,O,R):- proportional(I,O,R).
%grid_hint_io(MC,IO,In,Out,find_ogs):- maybe_fail_over_time(1.2,find_ogs(_,_,In,Out)).
grid_hint_io(MC,IO,In,Out,comp(MC,IO,Hint)):- comp_o(IO),  c_proportional(In,Out,Hint).

grid_hint_io(MC,IO,In,Out,comp(MC,IO,Hint)):- grid_size(In,IH,IV),grid_size(Out,OH,OV),grid_hint_iso(MC,IO,In,Out,IH,IV,OH,OV,Hint).

grid_hint_io(MC,IO,In,Out,(=@=(MC,IO))):- In=@=Out, !.
grid_hint_io(MC,IO,In,Out,Hint):- % \+ In=@=Out,
  Hint = comp(MC,IO,maybe_________________________ogs(MC,R,list(Len,XY))),
  member(R,[strict,loose]), 
  findall(loc(X,Y),maybe_ogs(R,X,Y,In,Out),XY),XY\==[],length(XY,Len),!.
grid_hint_io(MC,IO,Trim,Out,Hint):- % \+ In=@=Out,
  Hint = comp(MC,IO,trim__________ogs(MC,R,list(Len,XY))),
  trim_to_rect(Trim,In),!,Trim\=In,maybe_ogs(_,OX,OY,Trim,In),  
  member(R,[strict,loose]), 
  findall(loc(XX,YY),(maybe_ogs(R,X,Y,In,Out),XX is X+OX, YY is Y+OY),XY), XY\==[], length(XY,Len),!.
%grid_hint_iso(MC,IO,In,_Out,_IH,_IV,OH,OV,is_xy_columns):- once(has_xy_columns(In,_Color,OH,OV,)).

maybe_ogs(R,X,Y,In,Out):- nonvar(R),!,(R==strict->find_ogs(X,Y,In,Out);ogs_11(X,Y,In,Out)).
maybe_ogs(R,X,Y,In,Out):-  find_ogs(X,Y,In,Out)*->R=strict;(ogs_11(X,Y,In,Out),R=loose).


%grid_hint_iso(_MC,IO,_In,_Out,_IH,_IV,OH,OV,grid_size(IO,OH,OV)).
grid_hint_iso(cbg(_BGC),_-o,_In,Out,_IH,_IV,OH,OV,has_x_columns(Y,Color)):- Area is OH*OV, Area>24, maybe_fail_over_time(10.2,has_x_columns(Out,Y,Color,_)),Y>1.
grid_hint_iso(cbg(_BGC),_-o,_In,Out,_IH,_IV,OH,OV,has_y_rows(Y,Color)):- Area is OH*OV, Area>24, maybe_fail_over_time(10.2,has_y_rows(Out,Y,Color,_)),Y>1.

grid_hint_iso(cbg(BGC),IO,Out,In,GH,GV,GH,GV,Hint):- mapgrid(remove_color_if_same(BGC),Out,In,NewIn),
   mass(NewIn,Mass), unique_colors(In,Colors),unique_colors(NewIn,LeftOver), LeftOver\==Colors,
   (Mass==0 -> Hint=containsAll(IO) ;  Hint=containsAllExceptFor(IO,LeftOver)). 

% NewIn\=@=In,print_grid('leftover',NewIn).
%grid_hint_iso(cbg(BGC),IO,In,Out,_IH,_IV,_OH,_OV,cg(IO,Hint)):- comp_o(IO), grid_color_hint(In,Out,Hint).
%grid_hint_iso(_,IO,_In,_Out,IH,IV,  OH,OV,comp(MC,IO,size_r(H,V))):- comp_o(IO), V is rationalize(IV/OV), H is rationalize(IH/OH).
%grid_hint_iso(cbg(_BGC),IO,In,Out,_IH,_IV,_OH,_OV,cg(IO,mass_r(Mass))):- comp_o(IO), mass(In,IMass),mass(Out,OMass), IMass\==0,Mass is rationalize(OMass/IMass),Mass\==1.
/*
grid_hint_iso(cbg(_BGC),i-o,Out,_,_IH,_IV,_OH,_OV,rev(RInfo)):- 
 setup_call_cleanup(flag(indv,Was,0),
  ((findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo)),
                    flag(indv,_,Was)).
grid_hint_iso(cbg(_BGC),i-o,_In,Out,_IH,_IV,_OH,_OV,RInfo):- !,
 setup_call_cleanup(flag(indv,Was,0),
  ((findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo)),
                    flag(indv,_,Was)).
*/
/*grid_hint_iso(cbg(_BGC),i-o,_In,Out,_IH,_IV,_OH,_OV,RInfo):- 
 setup_call_cleanup(flag(indv,Was,0),
  ((wno( findall(Info,grid_part(Out,Info),List)),flatten([List],FList),member(Info,FList), rinfo(Info,RInfo))),
                    flag(indv,_,Was)).
*/
termsub_to_atom(List,OO):- is_list(List),!,maplist(termsub_to_atom,List,LL),
 atomic_list_concat(LL,'_',O),atomic_list_concat(['[',O,']'],OO).
termsub_to_atom(T,O):- sformat(O,'~w',[T]).

fix_iz(Z,ZZ):- atom(Z),!,ZZ=..[Z,true].
fix_iz(nth(_),'').
fix_iz(Z,ZZ):- is_list(Z),last(Z,ZZ),!.
fix_iz(iz(Z),ZZ):- nonvar(Z), !, fix_iz(Z,ZZ).
fix_iz(Z,ZZ):- compound(Z),arg(1,Z,A),is_list(A),last(A,ZZ),!.
fix_iz(Z,Z):-!.

rinfo(obj(List0),RInfo):- 
  maplist(must_det_ll,
  [select(ord(N),List0,List),
  atomic_list_concat([obj,N],'_',Key),
  Obj = obj(List0),
  localpoints_include_bg(Obj,LocalPoints),
  %loc(Obj,X,Y),
  get_bg_label(BGL),
  points_to_grid(LocalPoints,Grid),mapgrid(sometimes_assume(=,BGL),Grid),
  select(shape(Shape),List,Rest2),mapgrid(sometimes_assume(=,BGL),Shape),
  Rest3 = Rest2,
  obj_to_oid(Obj,MyID),
  must_det_ll((remove_too_verbose(MyID,Rest3,TV00))),flatten([TV00],TV0),
  must_det_ll((include(not_too_verbose,TV0,TV1),maplist(fix_iz,TV1,TV)))]),!,
  member(MrT,[oform(Shape),ogrid(Grid)|TV]),once((MrT=..MrTL, RInfoM=..[Key|MrTL],rinfo(RInfoM,RInfo))).
rinfo(Info,RInfo):- Info=..[P,N,A|InfoL], atomic_list_concat([P,N],'_',PN),!, RInfo=..[PN,A|InfoL].
rinfo(Info,Info):-!.

grid_color_hint(In,Out,Hint):-
    once((unique_colors(In,IColor0),unique_colors(Out,OColor0),
    include(is_color,IColor0,IColor),
    include(is_color,OColor0,OColor),
    intersection(IColor,OColor,Shared,IOnlyC,OOnlyC))),
    sort(Shared,SharedS),
    member(Hint,[shared_color(SharedS),lhs_only(IOnlyC),rhs_only(OOnlyC)]).



comp_o(_-o).

entire_row(Color,Row):- maplist(==(Color),Row).
%entire_row(black,Row):- !, maplist(=(black),Row).
mentire_row(C2,OtherRow):- entire_row(C2,OtherRow),!.
% mentire_row(C2,OtherRow):- include(\==(C2),OtherRow,Missing),  once((length(Missing,L),L=<1,maplist(=(C2),Missing))),!.

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
  %ppt(slicing(BorderNumsX,BorderNumsY,onto(X,Y))),
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
  % print_grid((X,Y),Clip), ppt(clipped(SX,SY,EX,EY,into(X,Y))),
  % once(Clip = [_,[_,Cell|_]|_];Clip = [[Cell|_]|_]),  
  once(((Clip = [_,[_,C|_]|_];Clip = [[C|_]|_];(member(CR,Clip),member(C,CR))),C\==Color,C\==black,Cell=C);Cell=Clip),
  (GX =< X -> (Yi is Y + 1, Xi is 1) ; (Xi is X+1, Yi is Y)),
  gather_chunks(Color,In,Chunks,Xi,Yi,GX,GY,BorderNumsX,BorderNumsY).


has_x_columns(In,X,Color,BorderNums):- rot90(In,In90), !, has_y_rows(In90,X,Color,BorderNums).

has_y_rows(In,Y,Color,BorderNums):- plain_var(Color), unique_colors(In,Colors),reverse(Colors,ColorsR),!,
  member(Color,ColorsR),is_color(Color), 
  has_y_rows(In,Y,Color,BorderNums).
has_y_rows(In,_Y,Color,_):- (append(_,[RowBefore,RowNext|_],In), entire_row(Color,RowBefore), entire_row(Color,RowNext)),!,fail.

has_y_rows(In,Y,Color,BorderNums):- 
  has_y_columns1(In,Y,Color,BorderNums),
  is_color(Color),
  (\+ illegal_column_data1(In,Color,BorderNums)).

has_y_columns1(In,Y,Color,BorderNums):-
  append([First|_],[Last],In),
  mentire_row(Color,First),mentire_row(Color,Last),!,
  findall(Nth,(nth1(Nth,In,Row),entire_row(Color,Row)),BorderNums),
  length(BorderNums,YM1),Y is YM1 - 1.

has_y_columns1(In,Y,Color,Out):-  
  length(In,GY1),GY is GY1 + 1,
  append([First|_],[Last],In),
  \+ mentire_row(Color,Last), \+ mentire_row(Color,First),!,
  findall(Nth,(nth1(Nth,In,Row),entire_row(Color,Row)),BorderNums),
  length(BorderNums,YM1),Y is YM1 + 1,
  append([0|BorderNums],[GY],Out).
% bleeding of this color
illegal_column_data1(In,Color,BorderNums):- 
  nth1(Nth,In,OtherRow),\+ member(Nth,BorderNums),
  append(_,[C1,C2|_],OtherRow),C1 == C2, C1 == Color,!.

illegal_column_data1(In,Color,_):- 
(append(_,[RowBefore,RowNext|_],In), entire_row(Color,RowBefore), entire_row(Color,RowNext)),!.

% completely differnt colored border
illegal_column_data1(In,Color,BorderNums):- 
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




%globalpoints(grid,points)

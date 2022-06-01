:- encoding(iso_latin_1).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).
:- dynamic(prolog:'$exported_op'/3).
:- assert((system:'$exported_op'(_,_,_):- fail)).
%:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).
:- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
%:- (getenv('DISPLAY',_) -> guitracer ; true).
:- set_prolog_flag(toplevel_print_anon,true).
%:- set_prolog_flag(toplevel_print_factorized,true).
:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(20), attributes(portray)]).

:- multifile(decl_sf/1).
:- discontiguous(decl_sf/1).
:- dynamic(decl_sf/1).
decl_sf(G):- ground(G), !, assertz(decl_sf(G)).
:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).
decl_pt(G):- ground(G), !, assertz(decl_pt(G)).

:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_individuation).
:- ensure_loaded(kaggle_arc_interpreter).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_learning).
:- ensure_loaded(kaggle_arc_imagens).
:- ensure_loaded(kaggle_arc_recognise).
:- ensure_loaded(kaggle_arc_ui_html).

%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(arc).


:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

clsmake:- cls,mmake.
arc:- forall(arc1,true).
arc1:- clsmake, test_names_by_hard(X), arc1(X).
arc2:- clsmake, arc1(v('009d5c81')).
arc3:- arc1(t('25d487eb')).
arc4:- clsmake, arc1(v('1d398264')).

fav:- forall(fav1,true).
fav1:- clsmake, fav(X), arc1(X).
fav(X):- nonvar_or_ci(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- forall(arc1(TestID),true).

arc1(TName):-    
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,TestID,ExampleNum),   
  kaggle_arc(TestID,ExampleNum,In,Out),
  run_arc_io(TestID,ExampleNum,In,Out))).

run_arc_io(TestID,ExampleNum,In,Out):-
  time(try_arc_io(TestID,ExampleNum,In,Out)).

try_arc_io(TestID,ExampleNum,In,Out):-
 must_det_l((
  name_the_pair(TestID,ExampleNum,In,Out,PairName),
  grid_size(In,IH,IV), grid_size(Out,OH,OV), 
  nop(writeln(grid_convert(size(IH,IV)->size(OH,OV)))),
  ignore((more_task_info(TestID,III),pt(III),nl)), 
  show_pair(IH,IV,OH,OV,test,PairName,In,Out),
  %get_shape_lib(hammer,ReservedS),
  %individuals_common([],Out,UnsharedOut1),
  %individuals_common(ReservedS,Out,UnsharedOut2),
  %show_pair(IH,IV,OH,OV,outs,PairName,UnsharedOut1,UnsharedOut2),!,
  %nop


 
  grid_minus_grid(In,Out,ImO),mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),mass(OmI,OMass),
  ((IMass==0, OMass>0) 
    -> (IODiff = OmI, UIO = out)
     ; (IODiff = ImO, UIO = in)),
    mass(IODiff,IOMass),

  show_pair(IH,IV,OH,OV,grid_minus_grid,PairName,ImO,OmI),!,

  writeln(grid(mass(imO,IMass),mass(oMI,OMass),using(UIO,IOMass))),


  individuals_common([],IODiff,ReservedS),
  searchable(ReservedS,SmallLib0),
  forall(member(M,SmallLib0),print_grid(M)),
  decolorize(SmallLib0,SmallLib1),
  all_rotations(SmallLib1,SmallLib),
  
 
  individuals_common(SmallLib,Out,UnsharedOut),
  individuals_common(SmallLib,In,UnsharedIn),


  ((



  format('~N+unshared~N'),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),!,
  %notrace(showdiff(UnsharedIn,UnsharedOut)),
  format('~N-unshared~N'),

  (individuals_common(UnsharedIn,Out,SharedOut)),
  (individuals_common(UnsharedOut,In,SharedIn)),

  format('~N+common~N'),
  (show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut)),!,
  format('~N-common~N'),


  (individuals_common(SharedIn,Out,SSharedOut)),
  (individuals_common(SharedOut,In,SSharedIn)),

  format('~N+SShared~N'),
  (show_pair(IH,IV,OH,OV,sshared,PairName,SSharedIn,SSharedOut)),!,
  format('~N-SShared~N'),

    ((reuse_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB),
    ( (UnsharedOut\==BetterB ; UnsharedIn\== BetterA) ->
      show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB);
       writeln('nothing better')))),


  nop((


  nop((

  (individuals_common(UnsharedIn,Out,SharedOut)),
  (individuals_common(UnsharedOut,In,SharedIn)),

  format('~N+shared~N'),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut),!,
  format('~N-shared~N'),

  
  nop((reuse_indivs(SharedIn,SharedOut,BetterA,BetterB),
  ( (SharedOut\==BetterB ; SharedIn\== BetterA) ->
    show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB);
     writeln('nothing better')))),

  nop((


       compute_unshared_indivs(In,UnsharedIn),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %reuse_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut))),!,
  nop(catch(maybe_confirm_sol(TestID,ExampleNum,In,Out),E,(wdmsg(E)))))))))))),!.


reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  append(IndvARest,[AA],BetterA),
  append(IndvBRest,[B],BetterB),
  append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  object_indv_id(B,ID,Iv),
  setq(A,object_indv_id(ID,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (pt(same_object(GlyphA,GlyphB,How))))).


:- learn_shapes.


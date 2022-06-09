/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- encoding(iso_latin_1).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).

:- multifile(decl_sf/1).
:- discontiguous(decl_sf/1).
:- dynamic(decl_sf/1).
decl_sf(G):- ground(G), !, assertz_new(decl_sf(G)).
:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).
decl_pt(G):- ground(G), !, assertz_new(decl_pt(G)).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).

arc_sub_path(Subdir,AbsolutePath):- arc_directory(ARC_DIR),
  catch(absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(directory),solutions(first),file_errors(error),access(read)]),_,fail),!.
arc_sub_path(Subdir,AbsolutePath):- arc_directory(ARC_DIR),
  absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(regular),solutions(first),file_errors(error),access(read)]).
:- export(arc_sub_path/2).

% COMMAND LINE ARC
:- if(\+ current_module(logicmoo_arc)).
  muarc_mod(user).
  :- set_prolog_flag(access_level,system).
  :- dynamic(prolog:'$exported_op'/3).
  :- assert((system:'$exported_op'(_,_,_):- fail)).
  %:- multifile('$exported_op'/3).
  %:- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
  :- SL  is 2_147_483_648*8, set_prolog_flag(stack_limit, SL ).
  %:- (getenv('DISPLAY',_) -> guitracer ; true).
  :- set_prolog_flag(toplevel_print_anon,true).
  %:- set_prolog_flag(toplevel_print_factorized,true).
  :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(20), attributes(portray)]).

  clsmake:- cls,update_changed_files,make.
  clsmake2:- update_changed_files.


% SWISH ARC
:- else.

  clsmake:- cls,update_changed_files,make.
  clsmake2:- update_changed_files.

  muarc_mod(muarc).
  :- if(current_module(trill)).
  :- set_prolog_flag_until_eof(trill_term_expansion,false).
  :- dynamic(muarc:ns4query/1).
  :- endif.
:- endif.


:- system:ensure_loaded(library(logicmoo_common)).
%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).

:- dynamic(arc_directory/1).
:- prolog_load_context(directory,ARC_DIR), asserta_new(arc_directory(ARC_DIR)).
arc_directory(ARC_DIR):- getenv('ARC_DIR',ARC_DIR), exists_directory(ARC_DIR),!.

:- multifile (user:file_search_path/2).
user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).


:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_db).
:- ensure_loaded(kaggle_arc_heuristics).
:- ensure_loaded(kaggle_arc_intruder).
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

arc:- forall(arc1,true).
arc1:- clsmake, test_names_by_hard(X), arc1(X).
arc2:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav:- clsmake,forall(fav1,true).
fav1:- clsmake, test_names_by_fav(X), arc1(X).
fav2:- clsmake, test_names_by_fav_rev(X), arc1(X).
fav(X):- nonvar_or_ci(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- clsmake2, time(forall(arc1(TestID),true)).

arc1(TestID):- clsmake2, !, arc1e(TestID).
arc1(TestID):- clsmake2, time(forall(arc1e(TestID),true)).

arc1e(TName):-    
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,TestID,ExampleNum), 
  ExampleNum = (tst+0),
  kaggle_arc(TestID,ExampleNum,In,Out),
  run_arc_io(TestID,ExampleNum,In,Out))).

arc_grid(Grid):- test_names_by_fav(TestID),kaggle_arc(TestID,_ExampleNum,In,Out),arg(_,v(In,Out),Grid).

%color_sym(OS,[(black='°'),(blue='©'),(red='®'),(green=''),(yellow),(silver='O'),(purple),(orange='o'),(cyan= 248	ø ),(brown)]).
color_sym(OS,C,Sym):- is_list(C),maplist(color_sym(OS),C,Sym),!.
color_sym(_,black,' ').

color_sym(OS,C,Sym):- color_sym(OS,4,C,Sym).

color_sym(_,_,C,Sym):- enum_colors(C),color_int(C,I),nth1(I,`ose=xt~+*zk>`,S),name(Sym,[S]).
%color_sym(P*T,_,C,Sym):- enum_colors(C),color_int(C,I),S is P+I*T,name(Sym,[S]).


is_buggy_pair(t('27a28665')*(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum,In,Out):-
  \+ is_buggy_pair(TestID*ExampleNum,_),
  time(show_arc_pair_progress(TestID,ExampleNum,In,Out)).


make_indivs(Pred,In,Out,InC,OutC):-
  writeln(inC(Pred)),
  call(Pred,In,InC),
  writeln(outC(Pred)),
  call(Pred,Out,OutC),
  writeln(inOutC(Pred)),!.

show_indivs(IH,IV,OH,OV,Pred,When,PairName,In,Out,SF):-  
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(When,Pred+PairName+in,NameIn),
  append_term_safe(Pred,Pred+PairName+out,NameOut),
  wots(U1, print_grid(IH,IV,NameIn,In)),
  wots(U2, print_grid(OH,OV,NameOut,Out)),
  print_side_by_side(U1,LW,U2),
  make_indivs(Pred,In,Out,InC,OutC),
  append(InC,OutC,InOutC),
  smallest_first(InOutC,SF),
  %largest_first(InOutC,LF),
  show_pair_no_i(IH,IV,OH,OV,smallest_first(When,Pred),PairName,InC-SF,OutC-SF),
  %wots(U3, print_grid(IH,IV,NameIn,InC-SF)),wots(U4, print_grid(OH,OV,NameOut,OutC-SF)),print_side_by_side(U3,LW,U4),
  %max_min(IH,OH,H,_),max_min(IV,OV,V,_),
  show_pair_no_i(IH,IV,OH,OV,normal(When,Pred),PairName,InC,OutC),
  INFO = [grid_dim,mass,colors_count_size,colors],
  print_side_by_side(
     describe_feature(InC,[call(writeln('IN'))|INFO]),LW,
    describe_feature(OutC,[call(writeln('OUT'))|INFO])),!,
  show_pair_I_info(NameIn,NameOut,InC,OutC).


show_arc_pair_progress(TestID,ExampleNum,In,Out):-
 must_det_l((
	name_the_pair(TestID,ExampleNum,In,Out,PairName),
	grid_size(In,IH,IV), grid_size(Out,OH,OV),
	nop(writeln(oi(size(IH,IV)->size(OH,OV)))),
	ignore((more_task_info(TestID,III),pt(III),nl)), 
  show_pair_no_i(IH,IV,OH,OV,test,PairName,In,Out),
  %print_collapsed
  forall((rtrace_on_error(individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV))),true), 
  show_indivs(IH,IV,OH,OV,individuate_default,early,PairName,In,Out,SF),
  %clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
  add_shape_lib(pairs,SF),
  show_shape_lib(in),show_shape_lib(out),show_shape_lib(pair),show_shape_lib(noise),
  show_indivs(IH,IV,OH,OV,individuate_default,later,PairName,In,Out,_))),!,

/*
  remove_global_points(UnsharedIn,In,InForgotten),
  remove_global_points(UnsharedOut,Out,OutForgottenM),
  ((mass(OutForgottenM,OM),OM==0) -> OutForgotten=OutC; OutForgotten=OutForgottenM),
  individuate_complete(InForgotten,ForgottenShapesIn),
  individuate_complete(OutForgotten,ForgottenShapesOut),

  % contains_points(InForgotten);contains_points(OutForgotten)
  %show_pair_no_i(IH,IV,OH,OV,forgotten,PairName,ForgottenShapesIn,ForgottenShapesOut),

  show_pair_no_i(IH,IV,IH,IV,forgotten_In,PairName,UnsharedIn,ForgottenShapesIn),
  show_pair_no_i(OH,OV,OH,OV,forgotten_Out,PairName,ForgottenShapesOut,OutC),
*/
  nop((
       show_indivs(In,Out),
       individuate_default(In,InC),
       individuate_default(Out,OutC),  
       writeln(outC),

       clear_shape_lib(out),
       clear_shape_lib(in),
       add_shape_lib(out,OutC),
       writeln(inOutC),
       show_pair_i(IH,IV,OH,OV,early_test,PairName,InC,OutC),
       writeln(inC),
       individuate_default(In,UnsharedIn),
       writeln(outC),
       individuate_default(Out,UnsharedOut),
       writeln(inUnsharedOut),
       show_pair_i(IH,IV,OH,OV,late_test,PairName,UnsharedIn,UnsharedOut),
       format('~N-sofar~N'),!,
       %pt(yellow,in=UnsharedIn),
       pred_intersection(compare_objs1([same]),UnsharedIn,UnsharedOut,_CommonCsIn,_CommonCsOut,_IPCs,_OPCs),
       format('~N-pred_intersection~N'),
        individuate(UnsharedOut,Out,SharedInR),
        individuate(UnsharedIn,In,SharedOutR),
        show_pair_no_i(IH,IV,OH,OV,shared,PairName,SharedInR,SharedOutR))), !.
  %format('~N-Rule made from~N'),
  %show_rules,
/*  RESS =.. [res,unsharedIn=UnsharedIn,
             %onlyIn= IPCs,
            commonIn=CommonCsIn,commonOut=CommonCsOut, %onlyOut=OPCs, 
             unsharedOut=UnsharedOut],
  tersify(RESS,ShortInfo),         
  format('~N-Stats:~N'),
  pt(yellow,sol=ShortInfo), !.
  */
	
/*
  
  nop((reuse_indivs(SharedIn,SharedOut,BetterA,BetterB),
  ( (SharedOut\==BetterB ; SharedIn\== BetterA) ->
    show_pair_i(IH,IV,OH,OV,better,PairName,BetterA,BetterB);
     writeln('nothing better')))),
*/
  


:- nb_linkval(test_rules,[rules]).
:- nb_linkval(pair_rules,[rules]).
  

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

:- fixup_exports.

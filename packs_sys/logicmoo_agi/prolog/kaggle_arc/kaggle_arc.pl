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
  :- set_prolog_flag(toplevel_print_factorized,true).
  :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(20), attributes(portray)]).

  clsmake:- cls,update_changed_files.  


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

:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_physics).
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
:- ensure_loaded(kaggle_arc_uniqueness).
:- ensure_loaded(kaggle_arc_ui_html).

%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(fav2).
:- add_history1(arc2).
:- add_history1(arc).
:- add_history1(arc1).
:- add_history1(fav).
:- add_history1(fav1).
:- add_history1(fav3).


:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

arc:- forall(arc11,true).
arc1:- clsmake, test_names_by_hard(X), arc1(cls1,X).
arc11:- clsmake, test_names_by_hard(X), arc1(X).
arc2:- clsmake, test_names_by_hard_rev(X), arc1(cls1,X).
arc22:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- clsmake, arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav3:- clsmake, arc1(t('3631a71a')*(_+_)),!.
fav:- clsmake,forall(fav11,true).
favr:- clsmake,forall(fav22,true).
fav1:- clsmake, test_names_by_fav(X), arc1(cls1,X).
fav11:- clsmake, test_names_by_fav(X), arc1(X).
fav2:- clsmake, test_names_by_fav_rev(X), arc1(cls,X).
fav22:- clsmake, test_names_by_fav_rev(X), arc1(X).

fav(X):- nonvar(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- time(forall(arc1(true,TestID),true)).

arc1(TName):- arc1(true,TName).
arc1(G,TName):-    
 retractall(why_grouped(individuate(_),_)),
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,TestID,ExampleNum), 
   ignore(ExampleNum = (_+0)),
   clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
   clear_shape_lib(intruder),!,

  kaggle_arc(TestID,ExampleNum,In,Out),
  catch((call(G),
    run_arc_io(TestID,ExampleNum,In,Out)),'$aborted',true))).

cls1:- catch(cls,_,true).

arc_grid(Grid):- test_names_by_fav(TestID),kaggle_arc(TestID,_ExampleNum,In,Out),arg(_,v(In,Out),Grid).

:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)*(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')*(tst+0),"segv").
%is_buggy_pair(t('27a28665')*(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum,In,Out):-
  \+ is_buggy_pair(TestID*ExampleNum,_),
  time(show_arc_pair_progress(TestID,ExampleNum,In,Out)).


make_indivs(Pred,In,Out,InC,OutC):-
  writeln(inC(Pred)),
  call(Pred,In,InC),
  %add_shape_lib(in,InC),
  writeln(outC(Pred)),
  call(Pred,Out,OutC),
  %add_shape_lib(out,OutC),
  writeln(inOutC(Pred)),!.
/*
show_indivs(IH,IV,OH,OV,Pred,When,PairName,In,Out,SF):-  
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(When,Pred+PairName+in,NameIn),
  append_term_safe(When,Pred+PairName+out,NameOut),
  wots(U1, print_grid(IH,IV,NameIn,In)),
  wots(U2, print_grid(OH,OV,NameOut,Out)),
  print_side_by_side(U1,LW,U2),
  append(InC,OutC,InOutC),
  smallest_first(InOutC,SF),
  %largest_first(InOutC,LF),
  %show_pair_no_i(IH,IV,OH,OV,smallest_first(When,Pred),PairName,InC-SF,OutC-SF),
  %wots(U3, print_grid(IH,IV,NameIn,InC-SF)),wots(U4, print_grid(OH,OV,NameOut,OutC-SF)),print_side_by_side(U3,LW,U4),
  %max_min(IH,OH,H,_),max_min(IV,OV,V,_),
  show_pair_no_i(IH,IV,OH,OV,normal(When,Pred),PairName,InC,OutC),
  INFO = [grid_dim,mass,colors_count_size,colors],
  print_side_by_side(
     describe_feature(InC,[call(writeln('IN'))|INFO]),LW,
    describe_feature(OutC,[call(writeln('OUT'))|INFO])),!,
  show_pair_I_info(NameIn,NameOut,InC,OutC).
*/

show_arc_pair_progress(TestID,ExampleNum,In,Out):-
 must_det_l((
	name_the_pair(TestID,ExampleNum,In,Out,PairName),
	grid_size(In,IH,IV), grid_size(Out,OH,OV),
	nop(writeln(oi(size(IH,IV)->size(OH,OV)))),
	ignore((more_task_info(TestID,III),pt(III),nl)), 
  clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
  %print_collapsed
  show_pair_grid(IH,IV,OH,OV,original_in,original_out,PairName,In,Out),
  make_indivs(individuate_complete,In,Out,InC,OutC),
  Pair = pair{in:In,out:Out,test:PairName,inC:InC,outC:OutC},
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  add_shape_lib(removed,Removed),
  add_shape_lib(in,RetainedIn),
  add_shape_lib(out,RetainedOut),
  add_shape_lib(pair,RetainedOut),
  add_shape_lib(added,Added),
  make_indivs(individuate_second_pass,In,Out,InP2,OutP2),
  make_indivs(individuate_complete,InP2,OutP2,InC2,OutC2),
  
  dash_char,dash_char,dash_char,dash_char,

  max_min(IH,OH,IOH,_),
  max_min(IV,OV,IOV,_),

  show_pair_diff(IH,IV,   OH, OV,retained_in,retained_out,PairName,RetainedIn,RetainedOut),
  show_pair_grid(IH,IV,   OH, OV,original_in,original_out,PairName,In,Out),
  show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,InC,OutC),
  show_pair_diff(IOH,IOV,IOH,IOV,removed,added,PairName,Removed,Added),
  show_pair_grid(IH,IV,   OH, OV,original_in,original_out,PairName,In,Out),
  show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,InC,OutC),
  show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,InC2,OutC2),
  show_pair_diff(IH,IV,   OH, OV,i_pass2,o_pass2,PairName,InP2,OutP2),
  !)).
  /*

  nop((
       show_indivs(IH,IV,OH,OV,individuate_complete,early,PairName,In,Out,SF),
       forall((rtrace_on_error(individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV))),true), 
       add_shape_lib(pairs,SF),
       show_shape_lib(in),show_shape_lib(out),show_shape_lib(pair),show_shape_lib(noise),
       show_indivs(IH,IV,OH,OV,individuate_default,later,PairName,In,Out,_))),!,
       with_named_pair(solve,TestID,PairName,In,Out),
*/
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
*//*
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
        show_pair_no_i(IH,IV,OH,OV,shared,PairName,SharedInR,SharedOutR))), !.*/
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

:- encoding(iso_latin_1).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).
%:- dynamic('$exported_op'/3).
%:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).


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
fav(X):- nonvar(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(Name):- forall(arc1(Name),true).

arc1(TName):-    
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,Name,ExampleNum), 
  set_flag(indiv,0),
  kaggle_arc(Name,ExampleNum,In,Out),
  run_arc_io(Name,ExampleNum,In,Out))).

run_arc_io(Name,ExampleNum,In,Out):-
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,Name),
  nb_setval(test_name_w_type,Name*ExampleNum),
  retractall(grid_nums(_,_)),
  retractall(grid_nums(_)),
  retractall(is_shared_saved(Name*ExampleNum*_,_)),
  retractall(is_shared_saved(Name*ExampleNum,_)),
  retractall(is_unshared_saved(Name*ExampleNum*_,_)),
  retractall(is_unshared_saved(Name*ExampleNum,_)),
  retractall(is_gridname(Name*ExampleNum*_,_)),
  retractall(is_gridname(Name*ExampleNum,_)),
  time(try_arc_io(CName,Name,ExampleNum,In,Out)).



try_arc_io(CName,Name,ExampleNum,In,Out):-
 must_det_l((
   grid_size(In,IH,IV), grid_size(Out,OH,OV), nop(writeln(grid_convert(size(IH,IV)->size(OH,OV)))),
   
   ignore((CName\==Name, flag(indiv,_,0),    
   dash_char(60,"A"),nl,dash_char(60,"|"),dash_char(6,"\n"),nl,
    dash_char(60,"|"),nl,dash_char(60,"V"),nl,
    nl,wqnl(arc1(Name)),nl,nl,dash_char(60,"A"),nl)),   
  dash_char(60,"|"),nl,nl,
  PairName= Name*ExampleNum,
  GridNameIn= Name*ExampleNum*in,
  GridNameOut= Name*ExampleNum*out,
  set_gridname(In,GridNameIn),
  set_gridname(Out,GridNameOut),
  %wqnl(arc1(Name)),nl,
  test_info(Name,Info), wqnl(fav(Name,Info)),nl,
  ignore((more_task_info(Name,III),pt(III),nl)),
  
  show_pair(IH,IV,OH,OV,test,PairName,In,Out),
  compute_unshared_indivs(Out,UnsharedOut),
  compute_unshared_indivs(In,UnsharedIn),
  %show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %notrace(showdiff(UnsharedIn,UnsharedOut)),
  notrace(individuals_common(UnsharedOut,In,SharedIn)),
  notrace(individuals_common(UnsharedIn,Out,SharedOut)),
  notrace(show_pair(IH,IV,OH,OV,common,PairName,SharedIn,SharedOut)),
  showdiff(SharedIn,SharedOut),!,
  

  %color_counts(In,InCC),
  %color_counts(Out,OutCC),

  nop((


       compute_unshared_indivs(In,UnsharedIn),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut))),!,

  nop(catch(maybe_confirm_sol(Name,ExampleNum,In,Out),E,(wdmsg(E)))))),!.




try_arc_io(CName,Name,ExampleNum,In,Out):-
 must_det_l((
   grid_size(In,IH,IV), grid_size(Out,OH,OV), nop(writeln(grid_convert(size(IH,IV)->size(OH,OV)))),
   
   ignore((CName\==Name, flag(indiv,_,0),    
   dash_char(60,"A"),nl,dash_char(60,"|"),dash_char(6,"\n"),nl,
    dash_char(60,"|"),nl,dash_char(60,"V"),nl,
    nl,wqnl(arc1(Name)),nl,nl,dash_char(60,"A"),nl)),   
  dash_char(60,"|"),nl,nl,
  PairName= Name*ExampleNum,
  GridNameIn= Name*ExampleNum*in,
  GridNameOut= Name*ExampleNum*out,
  set_gridname(In,GridNameIn),
  set_gridname(Out,GridNameOut),
  %wqnl(arc1(Name)),nl,
  test_info(Name,Info), wqnl(fav(Name,Info)),nl,
  ignore((more_task_info(Name,III),pt(III),nl)),
  
  show_pair(IH,IV,OH,OV,test,PairName,In,Out),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  individuals_common(UnsharedIn,Out,SharedOut),
  show_pair(IH,IV,OH,OV,shared,PairName,SharedIn,SharedOut),

  catch(maybe_confirm_sol(Name,ExampleNum,In,Out),E,(wdmsg(E))))),!.
/*
try_arc_io(CName,Name,ExampleNum,In,Out):-
  ignore((CName\==Name,flag(indiv,_,0),dash_char(60,"A"),dash_char(6,"\n"),nl)), 
  grid_info(Name,ExampleNum*in,In), %print_tree_nl(in=UnsharedIn),
  grid_info(Name,ExampleNum*out,Out), %print_tree_nl(out=UnsharedOut),
  !,
  % \+ \+ ignore(((ExampleNum=trn+_), test_config(learn(CProg)),must(training_progs(CProg,In,Out)))),
/*
  compute_diff(UnsharedIn,UnsharedOut,Diffs),!,
  nop(pt(diffs=Diffs)),
*/
    %trace, 
    get_combined(IndvO),
    print_grid(IndvO),
    maybe_confirm_sol(Name,ExampleNum,In,Out),nl,!.
*/
 /*

% Grid pretty printing
grid_info(Name,IO,Grid):- 
  PairName = (Name*IO),
  test_info(Name,InfoF),
  wqnl(fav(PairName,InfoF)),
  set_gridname(Grid,PairName),
  describe_feature(Grid,[grid_dim,colors_count_size,colors_count,num_objects]),
  compute_shared_indivs(Grid,UnsharedIn),
  colors_count_size(Grid,CCS),
  ignore((sub_var(in,IO),(CCS>4;debug_indiv;true), debug_indiv(UnsharedIn))),
  print_Igrid(PairName,UnsharedIn,[Grid]),
  ignore((sub_var(out,IO),(CCS>4;debug_indiv;true), debug_indiv(UnsharedIn))),
  nop(describe_feature(Grid,[compute_shared_indivs])),!.
*/
% resize(H,V,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).


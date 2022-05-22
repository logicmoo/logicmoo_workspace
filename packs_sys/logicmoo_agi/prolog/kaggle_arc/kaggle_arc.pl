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


:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_ui_html).
:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_interpreter).

%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(arc).


:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

clsmake:- cls,mmake.
arc:- clsmake,
  time((forall(test_names_by_hard(Name),ignore(arc(Name))))).
arc1:- clsmake, arc1(v('1d398264')).
arc2:- clsmake, arc1(v('009d5c81')).
arc3:- arc1(t('25d487eb')).
fav:- clsmake, forall(fav(X),arc(X)).
fav1:- clsmake, fav(X), arc1(X).
fav(X):- clsmake, nonvar(X),!, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(Name):- forall(arc1(Name),true).
arc1(TName):-    

 locally(set_prolog_flag(gc,false),
  (fix_test_name(TName,Name,ExampleNum), 
  set_flag(indiv,0),
  kaggle_arc(Name,ExampleNum,In,Out),
  run_arc_io(Name,ExampleNum,In,Out))).

run_arc_io(Name,ExampleNum,In,Out):-
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,Name),
  nb_setval(test_name_w_type,Name*ExampleNum),
  retractall(grid_nums(Name*ExampleNum,_)),
  retractall(is_shared_saved(Name*ExampleNum*_,_)),
  retractall(is_shared_saved(Name*ExampleNum,_)),
  retractall(is_unshared_saved(Name*ExampleNum*_,_)),
  retractall(is_unshared_saved(Name*ExampleNum,_)),
  retractall(is_gridname(Name*ExampleNum*_,_)),
  retractall(is_gridname(Name*ExampleNum,_)),
  try_arc_io(CName,Name,ExampleNum,In,Out).


try_arc_io(CName,Name,ExampleNum,In,Out):-
 %must_det_l
 ((
  grid_size(In,IH,_IV),
  LW is (IH * 2 + 12),
  ignore((CName\==Name, flag(indiv,_,0),    
    dash_char(60,"A"),nl,dash_char(60,"|"),dash_char(6,"\n"),nl,
    dash_char(60,"|"),nl,dash_char(60,"V"),nl,
    nl,wqnl(arc1(Name)),nl,nl,dash_char(60,"A"),nl)),   
  dash_char(60,"|"),nl,nl,
  GridName= Name*ExampleNum,
  GridNameIn= Name*ExampleNum*in,
  GridNameOut= Name*ExampleNum*out,
  set_gridname(In,GridNameIn),
  set_gridname(Out,GridNameOut),
  %wqnl(arc1(Name)),nl,
  test_info(Name,Info), wqnl(fav(Name,Info)),nl,
  ignore((more_task_info(Name,III),pt(III),nl)),

  make_unshared_indivs(GridNameIn,In),
  make_unshared_indivs(GridNameOut,Out),
  must_be_free(UnsharedIn),
  get_unshared_indivs(GridNameIn,UnsharedIn),
  get_unshared_indivs(GridNameOut,UnsharedOut),
  print_side_by_side(describe_feature(In,[call(writeln('IN')),grid_dim,colors_count_size,colors_count]),LW,
   describe_feature(Out,[call(writeln('OUT')),grid_dim,colors_count_size,colors_count])),

%  nop((print_igrid(unshared(GridNameIn),UnsharedIn,[In]),40,
%       print_igrid(unshared(GridNameOut),UnsharedOut,[Out]))),  


  \+ \+ ((
   wots(U1, print_igrid(=(GridNameIn),In,[])),
   wots(U2, print_igrid(=(GridNameOut),Out,[])),
   print_side_by_side(U1,LW,U2))),

  print_side_by_side(describe_feature(In,[num_objects]),LW, describe_feature(Out,[num_objects])),

%  get_shared_indivs(In,SharedIn),
%  get_shared_indivs(Out,SharedOut),!,

  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),!,
  set_shared_indivs(GridNameIn,SharedIn),
  set_shared_indivs(GridNameOut,SharedOut),

 ((
   wots(U1, print_igrid(-(GridNameIn),UnsharedIn,[In])),
   wots(U2, print_igrid(-(GridNameOut),UnsharedOut,[Out])),
   print_side_by_side(U1,LW,U2))),

  \+ \+ ((
  wots(S1, print_igrid(+(GridNameIn),SharedIn,[In])),
  wots(S2, print_igrid(+(GridNameOut),SharedOut,[Out])),
  print_side_by_side(S1,LW,S2))),


  wqnl(fav(GridNameIn,Info)), debug_indiv(SharedIn),
  wqnl(fav(GridNameOut,Info)), debug_indiv(SharedOut),  
  nop((wqnl(fav(GridName+combined,Info)), get_combined(CndvS), debug_indiv(CndvS))),

  nop(
   catch(maybe_confirm_sol(Name,ExampleNum,In,Out),E,(wdmsg(E)))

   ))),!.
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

% Grid pretty printing
grid_info(Name,IO,Grid):- 
  GridName = (Name*IO),
  test_info(Name,InfoF),
  wqnl(fav(GridName,InfoF)),
  set_gridname(Grid,GridName),
  describe_feature(Grid,[grid_dim,colors_count_size,colors_count,num_objects]),
  compute_shared_indivs(Grid,UnsharedIn),
  colors_count_size(Grid,CCS),
  ignore((sub_var(in,IO),(CCS>4;debug_indiv;true), debug_indiv(UnsharedIn))),
  print_igrid(GridName,UnsharedIn,[Grid]),
  ignore((sub_var(out,IO),(CCS>4;debug_indiv;true), debug_indiv(UnsharedIn))),
  nop(describe_feature(Grid,[compute_shared_indivs])),!.

% resize(H,V,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).

l_shape(round,"
 o=o !
o...o!
|.,.|!
o...o!
 o=o !").

l_shape(hollow,"
oo=oo!
o...o!
|.,.|!
o...o!
oo=oo!").

l_shape(diamond,"
  o  !
 /.\\ !
o.,.o!
 \\./ !
  o  !").


l_shape(heart,"
 o o !
o.o.o!
o.,.o!
 \\./ !
  o  !").

l_shape(spade,"
 o== !
o.,.o!
o...o!
 \\./ !
  o  !").


l_shape(right_triangle,"
    o!
   /|!
  o.o!
 o.,o!
o=ooo!").


l_shape(building,"
oo==o!
o...|!
,...o!
|...|!
o==oo!").


l_shape(triangle,"
   o   !
  o.o  !
 /.,.\\ !
o=ooo=o!").



/*

! 
_
/
\

*/
learn_shape(Name,Ascii):- replace_in_string([ 
   '\r'='\n','\n\n'='\n','! '='!','!\n'='\n','!'=''],Ascii,Ascii0),
   atomics_to_string(Rows1,'\n',Ascii0),Rows1=[_|Rows],maplist(atom_chars,Rows,GrowthChart),
   subst_each(GrowthChart,[
   ' '=_,
   ','=Fill,
   '.'=Fill,
   '/'=Color,
   '|'=Color,
   '='=Color,
  '\\'=Color,
   'o'=Color], Grid),
   assertz_if_new(learned_color_inner_shape(Name,Color,Fill,Grid,GrowthChart)),
   Color = green, Fill = red,
   grid_size(Grid,H,V),
   wqnl(learned(Name)),
   print_grid(H,V,Grid).

learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).

:- learn_shapes.




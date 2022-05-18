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
  fix_test_name(TName,Name,ExampleNum), 
  set_flag(indiv,0),
  kaggle_arc(Name,ExampleNum,In,Out),
  run_arc_io(Name,ExampleNum,In,Out).

run_arc_io(Name,ExampleNum,In,Out):-
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,Name),
  nb_setval(test_name_w_type,Name*ExampleNum),
  retractall(grid_nums(Name*ExampleNum,_)),
  retractall(is_group_saved(Name*ExampleNum*_,_)),
  retractall(is_group_saved(Name*ExampleNum,_)),
  retractall(is_gridname(Name*ExampleNum*_,_)),
  retractall(is_gridname(Name*ExampleNum,_)),
  try_arc_io(CName,Name,ExampleNum,In,Out).

try_arc_io(CName,Name,ExampleNum,In,Out):-
  ignore((CName\==Name,flag(indiv,_,0),dash_char(60,"A"),dash_char(6,"\n"),nl)), 
  dash_char(60,"V"),nl,
  GridNameIn= Name*ExampleNum*in,
  GridNameOut= Name*ExampleNum*out,
  test_info(Name,Info),  
  wqnl(fav(Name,Info)),
  store_individuals_non_shared(GridNameIn,In),
  store_individuals_non_shared(GridNameOut,Out),
  get_named_indivs(GridNameIn,IndvS),
  get_named_indivs(GridNameOut,OndvS),
  describe_feature(In,[grid_dim,colors_count_size,colors_count,num_objects]),
  describe_feature(Out,[grid_dim,colors_count_size,colors_count,num_objects]),
  get_combined(CndvS),
  individuals(In,SIndvIn),
  set_named_indivs(GridNameIn,IndvS),
  set_named_indivs(GridNameIn,IndvS),
  individuals(Out,SIndvOut),!,
  nop((print_igrid(unshared(GridNameIn),IndvS,[In]),
       print_igrid(unshared(GridNameOut),OndvS,[Out]))),  
  grid_size(In,IH,_IV),
  % grid_size(Out,OH,OV),
  LW is (IH * 2 + 12),
  wots(S1, print_igrid(better(GridNameIn),SIndvIn,[In])),
  wots(S2, print_igrid(better(GridNameOut),SIndvOut,[Out])),
  print_side_by_side(S1,LW,S2),
  wqnl(fav(Name,GridNameIn)),
  debug_indiv(SIndvIn),
  wqnl(fav(GridNameOut,Info)),
  debug_indiv(SIndvOut),
  wqnl(fav(GridName+combined,Info)),
  debug_indiv(CndvS),
  !,
  maybe_confirm_sol(Name,ExampleNum,In,Out).

try_arc_io(CName,Name,ExampleNum,In,Out):-
  ignore((CName\==Name,flag(indiv,_,0),dash_char(60,"A"),dash_char(6,"\n"),nl)), 
  grid_info(Name,ExampleNum*in,In), %print_tree_nl(in=IndvS),
  grid_info(Name,ExampleNum*out,Out), %print_tree_nl(out=OndvS),
  !,
  % \+ \+ ignore(((ExampleNum=trn+_), test_config(learn(CProg)),must(training_progs(CProg,In,Out)))),
/*
  compute_diff(IndvS,OndvS,Diffs),!,
  nop(pt(diffs=Diffs)),
*/
    trace, get_combined(IndvO),
    print_grid(IndvO),
    maybe_confirm_sol(Name,ExampleNum,In,Out),nl,!.


% Grid pretty printing
grid_info(Name,IO,Grid):- 
  GridName = (Name*IO),
  test_info(Name,InfoF),
  wqnl(fav(GridName,InfoF)),
  set_gridname(Grid,GridName),
  describe_feature(Grid,[grid_dim,colors_count_size,colors_count,num_objects]),
  individuals(Grid,IndvS),
  colors_count_size(Grid,CCS),
  ignore((sub_var(in,IO),(CCS>4;debug_indiv;true), debug_indiv(IndvS))),
  print_igrid(GridName,IndvS,[Grid]),
  ignore((sub_var(out,IO),(CCS>4;debug_indiv;true), debug_indiv(IndvS))),
  nop(describe_feature(Grid,[individuals])),!.

% resize(H,V,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).


l_shape(hollow,"
oo_oo|
o...o|
!.,.!|
o...o|
oo_oo|").

l_shape(diamond,"
  o  |
 /.\\ |
o.,.o|
 \\./ |
  o  |").


l_shape(heart,"
 o o |
o.o.o|
o.,.o|
 \\./ |
  o  |").

l_shape(spade,"
 o__ |
o.,.o|
o...o|
 \\./ |
  o  |").


l_shape(round,"
 o_o |
o...o|
!.,.!|
o...o|
 o_o |").

l_shape(right_traingle,"
    o|
   /!|
  o.o|
 o.,o|
o_ooo|").


l_shape(building,"
oo__o|
o...!|
,...o|
!...!|
o__oo|").


l_shape(triangle,"
   o   |
  o.o  |
 /.,.\\ |
o_ooo_o|").



/*

! 
_
/
\

*/
learn_shape(Name,Ascii):- replace_in_string([ 
   '\r'='\n','\n\n'='\n','| '='|','|\n'='\n','|'=''],Ascii,Ascii0),
   atomics_to_string(Rows1,'\n',Ascii0),Rows1=[_|Rows],maplist(atom_chars,Rows,Columns),
   subst_each(Columns,[','=',','/'=Color-'/','\\'=Color-'\\','_'=Color-'=','!'=Color-'|'],NColumns),
   subst(NColumns,'o',Color-'o',Ascii1),
   subst(Ascii1,' ',_,Ascii2),
   subst(Ascii2,'.','.',Grid),
   assertz_if_new(learned_color_inner_shape(Name,Color,Color2,Grid)),
   Color = green, Color2 = red, Fill = Color2,
   grid_size(Grid,H,V),
   %pt(H+V=Grid),
   wqnl(learned(Name)),
   print_grid(H,V,Grid).

learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).

:- learn_shapes.




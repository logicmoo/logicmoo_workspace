:- encoding(iso_latin_1).
:- set_prolog_flag(encoding,iso_latin_1).
:- dynamic('$exported_op'/3).
:- multifile('$exported_op'/3).
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

arc:- cls,mmake,
  time((forall(test_names_by_hard(Name),ignore(arc(Name))))).
arc1:- arc1(v('009d5c81')).
arc2:- arc1(t('25d487eb')).
fav:- cls,mmake, forall(fav(X),arc(X)).
fav1:- cls,mmake, fav(X), arc1(X).
fav(X):- cls,mmake, nonvar(X),!, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(Name):- forall(arc1(Name),true).
arc1(Name):-  
  fix_test_name(Name,TName,Type), 
  retractall(grid_nums(_)),
  retractall(grid_nums(_,_)),
  nb_delete(grid_bgc),
  kaggle_arc(TName,Type,In,Out),
  ignore(run_arc_io(TName,Type,In,Out)).

run_arc_io(Name,Type,In,Out):- fail,
  individuals(In,IndVI),individuals(Out,IndVO),!,
  assert(arc_individuals(Name,Type,IndVI,IndVO)),
  write('.'),flush_output.

run_arc_io(Name,Type,In,Out):-
  run_nb((
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,Name),
  nb_setval(test_name_w_type,Name->Type),
  retractall(grid_nums(Name->Type,_)),
  retractall(grid_nums(_)),
  retractall(grid_nums(_,_)),
  ignore(try_arc_io(CName,Name,Type,In,Out)))),!.

%try_arc_io(_,_,_,_,_):- \+ test_config(lmDSL(_)),!.

try_arc_io(CName,Name,Type,In,Out):-
    ignore((CName\==Name,dash_char(60,"A"),dash_char(6,"\n"),nl)),  
    dash_char(60,"V"),nl,
    describe_feature(Name,[test_info]),
  store_individuals_non_shared(Name,Type->in,In),
  store_individuals_non_shared(Name,Type->out,Out),
  grid_info(Name,Type->in,In), %print_tree_nl(in=IndvS),
  grid_info(Name,Type->out,Out), %print_tree_nl(out=OndvS),
    !,
    \+ \+ ignore(((Type=trn+_), test_config(learn(CProg)),must(training_progs(CProg,In,Out)))),
/*
  compute_diff(IndvS,OndvS,Diffs),!,
  nop(print_tree_nl(diffs=Diffs)),
*/
    maybe_confirm_sol(Name,Type,In,Out),nl,!.

% resize(H,V,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).


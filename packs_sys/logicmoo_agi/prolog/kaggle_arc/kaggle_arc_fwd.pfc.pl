/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

def_pfc_operators:-  
 locally(set_prolog_flag(access_level,system),
 ((op(200,fy,'-'),op(300,fx,'-'),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'),
 op(600,yfx,'&'),
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>'))))).

:- def_pfc_operators.
%:- module(system).

%:- expects_dialect(pfc).
forall_assert(G,P):- forall(G,assert_if_new(P)).
%:- include(library(pfc_syntax)).
:- set_prolog_flag(pfc_term_expansion,true).

meta_argtypes(process_test_grid(oid)).

meta_argtypes(P) ==> {decl_pt(P)}.

never_all_free==>cmem(_,_,_).
never_all_free==>gid_glyph_oid(_,_,_).
never_all_free==>cindv(_,_,_).

tid_to_gids(T,A) :- awc,!, (clause(tid_to_gids(T,A),true)*-> true ; term_to_oid(T,A)).

startAll==>((kaggle_arc_io(TestID,ExampleNum,IO,G)/(ID=TestID*ExampleNum*IO,term_to_oid(ID,GID)))
  ==>(tid_to_gids(ID,GID),oid_to_grid(GID,G),process_oid(GID))).

%tid_to_gids(T,A) :- zwc,!, term_to_oid(T,A).

==> startAll.

:- include('kaggle_arc_fwd_sanity.pfc.pl').


startAll2==>(process_oid(GID)/( \+ cmem(GID,_,_))==>{assert_id_grid_cells(GID)}).


(startAll3 ==>process_test(t('27a28665'))).
((startAll4,all_arc_test_name(ID)) ==>process_test(ID)).

((individuate_test_grids(TestID),tid_to_gids(TestID*trn*in,GID))==> process_test_grid(GID)).
process_test_grid(GID)==>{assert_id_grid_cells(GID),individuate(complete,GID,_)}.

:- dynamic(saved_training/1).
saved_training(TestID):- ~saved_training(TestID), !, fail. % explictly always assume unsaved?
saved_training(TestID):- test_name_output_file(TestID,File),exists_file(File).

(process_test(TestID) / (\+ saved_training(TestID))) ==> {compile_and_save_test(TestID)}.

assert_obj_global_points==>
  ((cindv( Obj, localpoints, _)/(obj_to_oid(Obj,GID),globalpoints(Obj,GPS)))==> {assert_id_cells(GID,GPS)}).


arc_test_property(T, common(comp(o-o, area)), area(n(1, 1, d(0), a(0), r(1))))==> note(T,"output always size 1").
arc_test_property(T, common(comp(i-o, area)), area(n(X, X, d(0), a(0), r(1))))/var(X)==> note(T,"output size always same as input").
%arc_test_property(T, common(comp(o-o, area)), area(n(X, X, d(0), a(0), r(1))))/nonvar(X)==> note(T,"output size always same as input").

%:- forall_assert(kaggle_arc_io(TestID,ExampleNum,IO,_),some_grid_tid(TestID*ExampleNum*IO)).
:- set_prolog_flag(pfc_term_expansion,false).


:- fixup_exports.

:- add_history(pfcAddF(startAll4)).

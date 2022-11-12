/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

%%:- module(user).

%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls_z ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

%:- include(library(pfc_syntax)).

:-  
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



%:- def_pfc_operators.
%:- module(system).

%:- expects_dialect(pfc).
forall_assert(G,P):- forall(G,arc_assert(P)).
:- set_prolog_flag(pfc_term_expansion,true).

meta_argtypes(find_test_gids(testID,grid_type,gid)).
meta_argtypes(find_test_grids(testID,grid_type,grid)).

==>(meta_argtypes(P) , {decl_pt(P)}).

never_all_free==>cmem(_,_,_).
never_all_free==>gid_glyph_oid(_,_,_).
never_all_free==>cindv(_,_,_).
%never_all_free==>tid_to_gids(_,_).
%never_all_free==>kaggle_arc_io(_,_,_,_).
==>awc.
==> startAll.


startAll_1==>((kaggle_arc_io(TestID,ExampleNum,IO,G)/((ID=(TestID>ExampleNum*IO)),term_to_oid(ID,GID)))
  ==>(tid_to_gids(ID,GID),gid_to_grid(GID,G))).

%tid_to_gids(T,A) :- zwc,!, term_to_oid(T,A).
startAll ==> startAll_1.

:- include('kaggle_arc_fwd_sanity.pfc.pl').


startAll_2==>(process_oid(GID)/( \+ cmem(GID,_,_))==>{assert_id_grid_cells(GID)}).

testID(t('27a28665')).

startAll_3  ==> (process_test(t('27a28665')), {mpred_info(startAll_3)}).
((startAll_3a/(G=process_test(t('47c1f68c'))))  ==> (G, {mpred_info(startAll_3a)})).

startAll_4 ==> (all_arc_test_name(ID) ==>process_test(ID)).

((process_test(TestID)/find_test_gids(TestID,Type,GID))==> test_to_gid(TestID,Type,GID)).
((process_oid(GID)/find_test_gids(TestID,Type,GID))==> test_to_gid(TestID,Type,GID)).

((test_to_gid(TestID,train_input,GID),process_test(TestID))==>{assert_id_grid_cells(GID),individuate(complete,GID,_)}).

(((test_to_gid(TestID,visible,GID),(process_test(TestID)/(unique_colors(GID,Colors),member(Color,Colors)))))==>color_of(TestID,Color)).

want_arg_counts(Pred,A,N,Col1,C,Col2)/(functor(CALL,Pred,A),arg(N,CALL,ARG1),arg(C,CALL,ARG2))
 ==>
(arg1Isa(Pred,N,Col1),
 arg1Isa(Pred,C,Col2), 
 ((argInstance(Pred,N,ARG1)/findall_count(ARG2,CALL,Count)) ==> ({wdmsg(argInstanceInstanceCount(Pred,N,ARG1,C,Count))},argInstanceInstanceCount(Pred,N,ARG1,C,Count))),
 (CALL==>(argInstance(Pred,N,ARG1),argInstance(Pred,C,ARG2)))).

:- mpred_info(want_arg_counts(_,_,_,_,_,_)).

==> want_arg_counts(color_of,2,1,testID,2,color).

(process_test(TestID) / (\+ saved_training(TestID))) ==> {compile_and_save_test(TestID)}.

assert_obj_global_points==>
  ((cindv( Obj, localpoints, _)/(obj_to_oid(Obj,GID),globalpoints(Obj,GPS)))==> {assert_id_cells(GID,GPS)}).


arc_test_property(T, common,(comp(o-o, area)), area(n(1, 1, d(0), a(0), r(1))))==> arc_note(T,"output always size2D 1").
arc_test_property(T, common,(comp(i-o, area)), area(n(X, X, d(0), a(0), r(1))))/var(X)==> arc_note(T,"output size2D always equal as input").
%arc_test_property(T, common,(comp(o-o, area)), area(n(X, X, d(0), a(0), r(1))))/nonvar(X)==> arc_note(T,"output size2D always equal as input").

%:- forall_assert(kaggle_arc_io(TestID,ExampleNum,IO,_),some_grid_tid(TestID>ExampleNum*IO)).
:- set_prolog_flag(pfc_term_expansion,false).


:- fixup_exports.

:- add_history((pfcAddF(startAll_3))).

%:- module(system).



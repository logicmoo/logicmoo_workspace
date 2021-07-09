
%:- gripe_time(60,baseKB:ensure_loaded(library('logicmoo/plarkc/logicmoo_i_cyc_rewriting'))).

%:- set_module(class(development)).
:- '$set_source_module'(baseKB).
%:- use_module(library(pfc)).
:- expects_dialect(pfc).

:- set_prolog_flag_until_eof(do_renames,term_expansion).
:- install_constant_renamer_until_eof.


ttBarrierStr(A),{atomic_list_concat([A,"Type"],AType0),
  atomic_list_concat([A,''],Type0),
  if_defined(do_renames(Type0,Type),true),
  if_defined(do_renames(AType0,TypeType),true)} ==> barrierSpindle(TypeType,Type).


:- if(false).
:- set_prolog_flag(gc,true).
:- trim_stacks.
:- garbage_collect_atoms.   
:- garbage_collect_clauses.
:- garbage_collect.
:- statistics.
% :- set_prolog_flag(gc,false).
:- endif.


barrierSpindle(TypeType,Type)==> 
   generatesAsFirstOrder(Type), isa(TypeType,ttBarrierType),isa(Type,ttBarrier),typeGenls(TypeType,Type).

ttBarrier(C)==>tSet(C).
(ttBarrierType(C)==>(tSet(C),ttTypeType(C))).

/*

@ TODO RE-ENABLE WHEN NEEDED
ttBarrier(C)==>(isa(I,C)==>mainClass(I,C)).

ttBarrier(A)/dif(A,B),ttBarrier(B)==> disjointWith(A,B).
% ttBarrierType(A)/dif(A,B),ttBarrierType(B)==> disjointWith(A,B).

*/

ttBarrierStr("Action").
ttBarrierStr("Agent").
ttBarrierStr("Artifact").
barrierSpindle('ttSpecifiedPartTypeCollection','tPartTypePhysicalPartOfObject').
ttBarrierStr("Capability").
ttBarrierStr("Event").
ttBarrierStr("FormulaTemplate").
ttBarrierStr("Goal").
ttBarrierStr("Group").
ttBarrierStr("LinguisticObject").
ttBarrierStr("Microtheory").
ttBarrierStr("PersonTypeByActivity").
ttBarrierStr("Place").
ttBarrierStr("Quantity").
ttBarrierStr("Relation").
ttBarrierStr("ScalarInterval").
ttBarrierStr("Situation").
ttBarrierStr("ExpressionType").
ttBarrierStr("TimeParameter").
ttBarrierStr("Topic").
% ttBarrierStr("Collection").


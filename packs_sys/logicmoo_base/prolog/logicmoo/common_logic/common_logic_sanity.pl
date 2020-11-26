/*
% ===============================================================================================================
 % File 'common_logic_sanity.pl'
 % Purpose: Emulation of OpenCyc for SWI-Prolog
 % Maintainer: Douglas Miles
 % Contact: $Author: dmiles $@users.sourceforge.net ;
 % Version: 'interface.pl' 1.0.0
 % Revision:  $Revision: 1.9 $
 % Revised At:   $Date: 2002/06/27 14:13:20 $
% ===============================================================================================================
 % File used as storage place for all predicates which make us more like Cyc
 % special module hooks into the logicmoo engine allow
 % syntax to be recocogized via our CycL/KIF handlers
 %
 % Dec 13, 2035
 % Douglas Miles
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/common_logic_sanity.pl
:- module(common_logic_sanity, [kif_test/1, test_boxlog/1, test_boxlog/2, test_defunctionalize/1]).


:-  system:((
 op(1199, fx, ('==>')),
 op(1190, xfx, ('::::')),
 op(1180, xfx, ('==>')),
 op(1170, xfx, '<==>'),
 op(1160, xfx, ('<-')),
 op(1150, xfx, '=>'),
 op(1140, xfx, '<='),
 op(1130, xfx, '<=>'),
 op(600, yfx, '&'),
 op(600, yfx, 'v'),
 op(350, xfx, 'xor'),
 op(300, fx, '~'),
 op(300, fx, '-'))).

:- ensure_loaded(library(logicmoo_clif)).

%:- use_module(library(script_files)).

fst:-  set_prolog_flag(write_attributes, ignore), freeze(X, (\+ is_ftVar(X), X==[]->(dumpST, break);true)), rtrace((trace, test_boxlog(~ &(human(X), male(X))))).

:- export(fst/0).


zebra :- make, load_clif(pack(logicmoo_base/t/examples/fol/'exactly.clif')).

zebra5 :- make, load_clif(pack(logicmoo_base/t/examples/fol/'zebra5.clif')).
zebra1 :- make, load_clif(pack(logicmoo_base/t/examples/fol/'zebra1.clif')).
zebra0 :- make, load_clif(pack(logicmoo_base/t/examples/fol/'zebra0.clif')).

rzebra5 :- rtrace(load_clif(pack(logicmoo_base/t/examples/fol/'exactly.clif'))).

z:- cls, zebra5, !.
z:- rzebra5, !.

boxlog :- ensure_loaded(pack(logicmoo_base/t/examples/fol/'fol_sanity.pl')).

kif_uncompile:- pfclog_uncompile, boxlog_uncompile, clif_uncompile.
kif_compile:- clif_compile, boxlog_compile, pfclog_compile.
kif_recompile:- kif_uncompile, kif_compile.
kif_show:-  baseKB:listing(clif/1), baseKB:listing(boxlog/1), baseKB:listing(pfclog/1).
:- export(kif_recompile/0).
:- export(kif_compile/0).
:- export(kif_uncompile/0).
:- export(kif_show/0).


:- kb_shared(compile_clif/0).
clif_uncompile:-  ain(==>( \+ compile_clif)), clif_show.
clif_recompile:-  ain(==>( \+ compile_clif)), ain(==> compile_clif), clif_show.
clif_compile:-  ain(==> compile_clif). % clif_show.
clif_show:-  baseKB:listing(clif/1), baseKB:listing(boxlog/1).
:- export(clif_recompile/0).
:- export(clif_compile/0).
:- export(clif_uncompile/0).
:- export(clif_show/0).

:- kb_shared(compile_boxlog/0).
:- export(boxlog_recompile/0).
:- export(boxlog_compile/0).
:- export(boxlog_uncompile/0).
:- export(boxlog_show/0).
boxlog_uncompile:-  ain(==>( \+ compile_boxlog)), boxlog_show.
boxlog_recompile:-  ain(==>( \+ compile_boxlog)), ain(==> compile_boxlog), boxlog_show.
boxlog_compile:-  ain(==> compile_boxlog). % boxlog_show.
boxlog_show:-  baseKB:listing(boxlog/1), baseKB:listing(pfclog/1).

:- kb_shared(compile_pfclog/0).
:- export(pfclog_recompile/0).
:- export(pfclog_compile/0).
:- export(pfclog_uncompile/0).
:- export(pfclog_show/0).

pfclog_uncompile:-  ain(==>( \+ compile_pfclog)), pfclog_show.
pfclog_recompile:-  ain(==>( \+ compile_pfclog)), ain(==> compile_pfclog), pfclog_show.
pfclog_compile:-  ain(==> compile_pfclog). %pfclog_show.
pfclog_show:-  baseKB:listing(pfclog/1).


show_kif_to_boxlog(P):- dmsg(test_boxlog(P)), ain(P).


test_defunctionalize(I):-defunctionalize(I, O), sdmsg(O).

sdmsg(Form):-
   if_defined(demodal_sents(_KB, Form, Out), Form=Out),
   % if_defined(local_pterm_to_sterm(OutM, Out), OutM=Out),
   must(wdmsgl(dmsg, Out)).

sdmsgf(Form):-
   if_defined(demodal_sents(_KB, Form, Out), Form=Out),
   % if_defined(local_pterm_to_sterm(OutM, Out), OutM=Out),
   must(wdmsgl(Out)).

/*
test_boxlog(P):- source_location(_, _), !, nl, nl, b_implode_varnames(P), test_boxlog(P, O), nl, nl,
   % b_implode_varnames(O),
  (is_list(O)->maplist(portray_one_line, O);dmsg(O)), flush_output.
*/


add_boxlog_history(P0):-
  (nb_current('$variable_names', Vs0)->true;Vs0=[]),
  copy_term(P0+Vs0, P+Vs),
  \+ \+
  ((b_setval('$variable_names', Vs),
  b_implode_varnames0(Vs),
  b_implode_varnames(P),
  guess_varnames(P),
  with_output_to(string(S),
    write_term(P, [numbervars(true), variable_names(Vs), character_escapes(true), ignore_ops(false), quoted(true), fullstop(true)])),
   stream_property(In, file_no(0)),
   prolog:history(In, add(S)))), !.

:- export(test_boxlog/1).
test_boxlog(P):- test_boxlog([], P).


test_boxlogq(P):- test_boxlog([+qualify], P), !.

:- export(test_boxlog/2).
% test_boxlog_m(P, BoxLog):-logicmoo_motel:kif_to_motelog(P, BoxLog), !.
test_boxlog(KV, P):-
 locally_tl(kif_option_list(KV), (
  mmake,
  % ignore(source_location(_, _) -> add_boxlog_history(test_boxlog(KV, P)) ; true),
 \+ \+
 must_det_l((
  (nb_current('$variable_names', Vs)->b_implode_varnames0(Vs);true),
  dmsg(:- test_boxlog(P)),
  b_implode_varnames(P),
  guess_varnames(P),
  kif_optionally_e(never, ain, clif(P)),
  kif_to_boxlog(P, O),
  guess_varnames(O), flush_output,
  kif_optionally_e(true, sdmsgf, O), flush_output,
  kif_optionally(false, assert_to_boxlog, O),
  kif_optionally(false, print_boxlog_to_pfc, O))))), !.

assert_to_boxlog(G):- ain(boxlog(G)), !.

print_boxlog_to_pfc(O):-
  boxlog_to_pfc(O, PFC),
  sdmsgf(pfc=PFC),
  flush_output.




assert_boxlog(G):- ain(boxlog(G)).

test_boxlog_88(P):-
 \+ \+
 must_det_l((
  (nb_current('$variable_names', Vs)->b_implode_varnames0(Vs);true),
  b_implode_varnames(P), flush_output,
  dmsg(:- test_boxlog(P)),
  with_assert_buffer(with_chaining(ain(P)), Buffer),
  undo_buffer(Buffer),
  sdmsgf(Buffer), flush_output)).



:- export(test_pfc/1).
test_pfc(P):- mmake, must_det(test_pfc0(P)), !.
test_pfcq(P):- mmake, locally_tl(qualify_modally, must_det(test_pfc0(P))), !.

test_pfc0(P):-
 \+ \+
 must_det_l((
  (nb_current('$variable_names', Vs)->b_implode_varnames0(Vs);true),
  b_implode_varnames(P), flush_output,
  dmsg(:- test_pfc(P)),
  kif_to_pfc(P, O),
  sdmsgf(O), flush_output)).



invert_op_call(OP, What, dmsg(undo(OP, What))).

undo_tell(call(OP, What)):-!, invert_op_call(OP, What, Invert), call(Invert).
undo_tell(Tell):- ignore(show_call(mpred_retract(Tell))).

undo_buffer(Buffer):- must_maplist(undo_tell, Buffer), !.


% invert_op_call(OP, What, Invert):-!.



%% tsn is det.
%
% Tsn.
%
% tsn:- with_all_dmsg(forall(clause(kif, C), must(C))).

/*
%% regression_test is det.
%
% Hook To [baseKB:regression_test/0] For Module Common_logic_snark.
% Regression Test.
%

% baseKB:regression_test:- tsn.
*/

:- op(0, fy, (kif_test)).
%% kif_test(+Goal ) is det.
%
% Kif test
%
kif_test(TODO):-atomic(TODO), kif_io(string(TODO), current_output).
kif_test(X):- kif_add(X).
:- op(1200, fy, (kif_test)).


kif_result(_).


baseKB:sanity_test:- kif_test(all(R, '=>'(room(R) , exists(D, '&'(door(D) , has(R, D)))))).

baseKB:sanity_test:- kif_to_boxlog(not((a , b , c , d)), S), !, disjuncts_to_list(S, L),
  list_to_set(L, SET), forall(member(P, SET), writeln(P)), !.


kif_sanity_tests:- forall(clause(kif_sanity_test_0, B), must(B)).

default_logic_uses:- must(call(call, uses_logic(logicmoo_kb_refution))).

%:- initialization(default_logic_uses).
%:- default_logic_uses.


% :- if_startup_script(reexport(kif_sanity_tests)).

% = % :- reexport(plarkc/mpred_clif).

% = % :- reexport(logicmoo_plarkc).

%:- autoload.



subtest_assert(I) :- kif_assert(I).


dbanner:- nl, nl, dmsg('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl, nl.

test_assert(A):-
  nop(kif_assert(A)),
  test_boxlog([+assert], A),
  nop(forall(subtest(T), do_subtest(T))).


do_subtest(List):- must_maplist(call, List).

add_test(Name, Assert):-
  b_implode_varnames(Name+Assert),
  assert(is_test(Name)),
   dbanner, dmsg(test_boxlog(Name)), dbanner,
  test_boxlog(Assert),
   dbanner, dmsg(completed_test_boxlog(Name)), dbanner,
   assert(( Name:- mmake, dbanner, dmsg(running_test(Name)), dbanner,
      test_assert(Assert),
      dbanner, dmsg(completed_running_test(Name)), dbanner)).

show_test(G):- defaultAssertMt(KB), must(show_call(KB:G)).
show_call_test(G):- defaultAssertMt(KB), must(show_call(KB:G)).



 :- meta_predicate example_known_is_success(*).
 :- meta_predicate example_known_is_failure(*).
 :- meta_predicate example_proven_true(*).
 :- meta_predicate example_proven_false(*).
 :- meta_predicate example_inconsistent(*).
 :- meta_predicate example_unknown(*).



%= define the example language
% :- fully_expand_real(change(assert, ain), (example_known_is_success(_30487686):-_30487686), O), writeln(O).
example_known_is_success(G):-  call_u(G).
example_impossible_is_success(G):-  call_u(~(G)).
example_known_is_failure(G):-  \+ call_u(G).
example_impossible_is_failure(G):- \+  call_u(~(G)).

%= define the four truth values
example_proven_true(G):- example_known_is_success(G), example_impossible_is_failure(G).
example_proven_false(G):- example_impossible_is_success(G), example_known_is_failure(G).
example_inconsistent(G):- example_known_is_success(G), example_impossible_is_success(G).
example_unknown(G):- example_known_is_failure(G), example_impossible_is_failure(G).

% :-multifile lmconf:shared_hide_data/1.
%= lmconf:shared_hide_data(hideMeta):-is_main_thread.
%= lmconf:shared_hide_data(hideTriggers):-is_main_thread.

% = clear the screen
% :- shell(cls).

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
% :- set_clause_compile(fwc).



:- fixup_exports.

:- if(false).
:- set_prolog_flag(gc, true).
:- trim_stacks.
:- garbage_collect_atoms.
:- garbage_collect_clauses.
:- garbage_collect.
:- statistics.
%:- set_prolog_flag(gc, false).
:- endif.


add_axiom(AX):- ain(baseKB:axiom(AX)).

:- add_axiom(( ~fallacy_t(PROP) => unknown_t(PROP) v false_t(PROP) v true_t(PROP) )).
:- add_axiom(( ~unknown_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- add_axiom(( ~false_t(PROP) => fallacy_t(PROP) v unknown_t(PROP) v true_t(PROP) )).
:- add_axiom(( answerable_t(PROP) <=> askable_t(PROP) & ~unknown_t(PROP) )).
:- add_axiom(( answerable_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- add_axiom(( askable_t(PROP) <=> ~fallacy_t(PROP) )).
:- add_axiom(( askable_t(PROP) => true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).
:- add_axiom(( askable_t(PROP) v fallacy_t(PROP) )).
:- add_axiom(( asserted_t(PROP) => true_t(PROP) )).
:- add_axiom(( fallacy_t(PROP) => false_t(PROP) & true_t(PROP) & ~unknown_t(PROP) & ~possible_t(PROP) )).
:- add_axiom(( true_t(PROP) & false_t(PROP) => fallacy_t(PROP) )).
:- add_axiom(( true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).

:- add_axiom(( true_t(PROP) => possible_t(PROP) )).
:- add_axiom(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).

:- add_axiom(( ~true_t(PROP) => false_t(PROP) v fallacy_t(PROP) v possible_t(PROP) )).
:- add_axiom(( false_t(PROP) <=> ~true_t(PROP) & ~possible_t(PROP) & ~unknown_t(PROP) )).
:- add_axiom(( true_t(PROP) => ~false_t(PROP) & possible_t(PROP) & ~unknown_t(PROP) )).
:- add_axiom(( ~asserted_t(PROP) => possible_t(PROP) v false_t(PROP) v fallacy_t(PROP) )).
:- add_axiom(( ~possible_t(PROP) => false_t(PROP) v fallacy_t(PROP) )).
:- add_axiom(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).
:- add_axiom(( unknown_t(PROP) => ~true_t(PROP) & possible_t(PROP) & ~asserted_t(PROP) & ~false_t(PROP) )).
%:- add_axiom(( ist(MT1, askable_t(PROP))  & genlMt(MT1, MT2) => ist(MT2, (true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )))).
% :- add_axiom(( ist(MT1, asserted_t(PROP)) & genlMt(MT1, MT2) => ist(MT2, true_t(PROP)) )).


e0 :- any_to_pfc(((
 (tHeart(skIsHeartInArg2ofHasorgan_Fn(Human))
     :- tHuman(Human))), (hasOrgan(Human, skIsHeartInArg2ofHasorgan_Fn(Human)) :- tHuman(Human))), O), wdmsg(O).
% O = tHuman(Heart)==> if_missing(hasOrgan(Human, _), hasOrgan(Human, skIsHeartInArg2ofHasorgan_Fn(Human)))  & tHeart(skIsHeartInArg2ofHasorgan_Fn(Human)).


e1:- % ['$VAR'('Room'), '$VAR'('Door')]= [Room, Door],
   test_boxlog(exists([[Door, tDoor]], isa(Room, tRoom) => hasExit(Room, Door))).

e2:- % ['$VAR'('Room'), '$VAR'('Door')]= [Room, Door],
   test_boxlog((all([[Room, tRoom]], exists([[Door, tDoor]], hasExit(Room, Door))))).

e3:- % ['$VAR'('Room'), '$VAR'('Door')]= [Room, Door],
   test_boxlog(exists([[Door, tDoor]], isa(Room, tRoom) => hasExit3(Room, Door))).

e4:-  ['$VAR'('Room'), '$VAR'('Door')]= [Room, Door],
   make, (test_boxlog((isa(Room, tRoom) => exists(Door, isa(Door, tDoor) & hasExit4(Room, Door))))).


e5:-  ['$VAR'('Human'), '$VAR'('Heart')]= [Human, Heart],
   (test_boxlog(
     all([[Human, tHuman]],
        exists([Heart],
         % isa(Human, tHuman) =>
             (isa(Heart, tHeart) & hasOrgan(Human, Heart)))))).

e6:-  ['$VAR'('Human'), '$VAR'('Heart')]= [Human, Heart],
  (test_boxlog(all([Human], exists([Heart], isa(Human, tHuman) => (isa(Heart, tHeart) & hasOrgan(Human, Heart)))))).


e7:-  ['$VAR'('Human'), '$VAR'('Heart')]= [Human, Heart],
   (kif_to_boxlog(all([Human], exists([Heart], isa(Human, tHuman)
     => (isa(Heart, tHeart)
      & hasOrgan(Human, Heart)))), O), wdmsgl(test_defunctionalize, O)).
















end_of_file.





















% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:181
% \+ if_startup_script(sanity:reexport(kif_sanity_tests)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:193
% :- test_boxlog(=>(~asserted_t(PROP_VAR), v(v(possible_t(PROP_VAR), false_t(PROP_VAR)), fallacy_t(PROP_VAR)))).
proven_possible_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([possible_t(A)|C], D)),
        proven_not_fallacy_t(A,
                            B,
                            anc([possible_t(A)|C], D)),
        proven_not_asserted_t(A,
                             B,
                             anc([possible_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_possible_t(A,
                             B,
                             anc([false_t(A)|C], D)),
        proven_not_fallacy_t(A,
                            B,
                            anc([false_t(A)|C], D)),
        proven_not_asserted_t(A,
                             B,
                             anc([false_t(A)|C], D)).
proven_fallacy_t(A, B, anc(C, D)) :-
        ( proven_not_possible_t(A,
                               B,
                               anc([fallacy_t(A)|C], D)),
          proven_not_false_t(A,
                            B,
                            anc([fallacy_t(A)|C], D))
        ),
        proven_not_asserted_t(A,
                             B,
                             anc([fallacy_t(A)|C], D)).
proven_asserted_t(A, B, anc(C, D)) :-
        ( proven_not_possible_t(A,
                               B,
                               anc([asserted_t(A)|C], D)),
          proven_not_false_t(A,
                            B,
                            anc([asserted_t(A)|C], D))
        ),
        proven_not_fallacy_t(A,
                            B,
                            anc([asserted_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:194
% :- test_boxlog(=>(~fallacy_t(PROP_VAR), v(v(unknown_t(PROP_VAR), false_t(PROP_VAR)), true_t(PROP_VAR)))).
proven_unknown_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([unknown_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([unknown_t(A)|C], D)),
        proven_not_fallacy_t(A,
                            B,
                            anc([unknown_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_unknown_t(A,
                            B,
                            anc([false_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)),
        proven_not_fallacy_t(A,
                            B,
                            anc([false_t(A)|C], D)).
proven_true_t(A, B, anc(C, D)) :-
        ( proven_not_unknown_t(A,
                              B,
                              anc([true_t(A)|C], D)),
          proven_not_false_t(A,
                            B,
                            anc([true_t(A)|C], D))
        ),
        proven_not_fallacy_t(A,
                            B,
                            anc([true_t(A)|C], D)).
proven_fallacy_t(A, B, anc(C, D)) :-
        ( proven_not_unknown_t(A,
                              B,
                              anc([fallacy_t(A)|C], D)),
          proven_not_false_t(A,
                            B,
                            anc([fallacy_t(A)|C], D))
        ),
        proven_not_true_t(A,
                         B,
                         anc([fallacy_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:195
% :- test_boxlog(=>(~false_t(PROP_VAR), v(v(fallacy_t(PROP_VAR), unknown_t(PROP_VAR)), true_t(PROP_VAR)))).
proven_fallacy_t(A, B, anc(C, D)) :-
        proven_not_unknown_t(A,
                            B,
                            anc([fallacy_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([fallacy_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([fallacy_t(A)|C], D)).
proven_unknown_t(A, B, anc(C, D)) :-
        proven_not_fallacy_t(A,
                            B,
                            anc([unknown_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([unknown_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([unknown_t(A)|C], D)).
proven_true_t(A, B, anc(C, D)) :-
        ( proven_not_fallacy_t(A,
                              B,
                              anc([true_t(A)|C], D)),
          proven_not_unknown_t(A,
                              B,
                              anc([true_t(A)|C], D))
        ),
        proven_not_false_t(A,
                          B,
                          anc([true_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        ( proven_not_fallacy_t(A,
                              B,
                              anc([false_t(A)|C], D)),
          proven_not_unknown_t(A,
                              B,
                              anc([false_t(A)|C], D))
        ),
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:196
% :- test_boxlog(=>(~possible_t(PROP_VAR), v(false_t(PROP_VAR), fallacy_t(PROP_VAR)))).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_fallacy_t(A,
                            B,
                            anc([false_t(A)|C], D)),
        proven_not_possible_t(A,
                             B,
                             anc([false_t(A)|C], D)).
proven_fallacy_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([fallacy_t(A)|C], D)),
        proven_not_possible_t(A,
                             B,
                             anc([fallacy_t(A)|C], D)).
proven_possible_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([possible_t(A)|C], D)),
        proven_not_fallacy_t(A,
                            B,
                            anc([possible_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:197
% :- test_boxlog(=>(~true_t(PROP_VAR), v(v(false_t(PROP_VAR), fallacy_t(PROP_VAR)), possible_t(PROP_VAR)))).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_fallacy_t(A,
                            B,
                            anc([false_t(A)|C], D)),
        proven_not_possible_t(A,
                             B,
                             anc([false_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)).
proven_fallacy_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([fallacy_t(A)|C], D)),
        proven_not_possible_t(A,
                             B,
                             anc([fallacy_t(A)|C], D)),
        proven_not_true_t(A,
                         B,
                         anc([fallacy_t(A)|C], D)).
proven_possible_t(A, B, anc(C, D)) :-
        ( proven_not_false_t(A,
                            B,
                            anc([possible_t(A)|C], D)),
          proven_not_fallacy_t(A,
                              B,
                              anc([possible_t(A)|C], D))
        ),
        proven_not_true_t(A,
                         B,
                         anc([possible_t(A)|C], D)).
proven_true_t(A, B, anc(C, D)) :-
        ( proven_not_false_t(A,
                            B,
                            anc([true_t(A)|C], D)),
          proven_not_fallacy_t(A,
                              B,
                              anc([true_t(A)|C], D))
        ),
        proven_not_possible_t(A,
                             B,
                             anc([true_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:198
% :- test_boxlog(=>(~unknown_t(PROP_VAR), v(true_t(PROP_VAR), false_t(PROP_VAR)))).
proven_true_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([true_t(A)|C], D)),
        proven_not_unknown_t(A,
                            B,
                            anc([true_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)),
        proven_not_unknown_t(A,
                            B,
                            anc([false_t(A)|C], D)).
proven_unknown_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([unknown_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([unknown_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:199
% :- test_boxlog(<=>(answerable_t(PROP_VAR), &(askable_t(PROP_VAR), ~unknown_t(PROP_VAR)))).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:200
% :- test_boxlog(=>(answerable_t(PROP_VAR), v(true_t(PROP_VAR), false_t(PROP_VAR)))).
proven_true_t(A, B, anc(C, D)) :-
        proven_not_false_t(A,
                          B,
                          anc([true_t(A)|C], D)),
        proven_answerable_t(A,
                           B,
                           anc([true_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)),
        proven_answerable_t(A,
                           B,
                           anc([false_t(A)|C], D)).
proven_not_answerable_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc(C, [answerable_t(A)|D])),
        proven_not_false_t(A,
                          B,
                          anc(C, [answerable_t(A)|D])).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:201
% :- test_boxlog(<=>(askable_t(PROP_VAR), ~fallacy_t(PROP_VAR))).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:202
% :- test_boxlog(=>(askable_t(PROP_VAR), v(v(true_t(PROP_VAR), unknown_t(PROP_VAR)), false_t(PROP_VAR)))).
proven_true_t(A, B, anc(C, D)) :-
        proven_not_unknown_t(A,
                            B,
                            anc([true_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([true_t(A)|C], D)),
        proven_askable_t(A,
                        B,
                        anc([true_t(A)|C], D)).
proven_unknown_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([unknown_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([unknown_t(A)|C], D)),
        proven_askable_t(A,
                        B,
                        anc([unknown_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        ( proven_not_true_t(A,
                           B,
                           anc([false_t(A)|C], D)),
          proven_not_unknown_t(A,
                              B,
                              anc([false_t(A)|C], D))
        ),
        proven_askable_t(A,
                        B,
                        anc([false_t(A)|C], D)).
proven_not_askable_t(A, B, anc(C, D)) :-
        ( proven_not_true_t(A,
                           B,
                           anc(C, [askable_t(A)|D])),
          proven_not_unknown_t(A,
                              B,
                              anc(C, [askable_t(A)|D]))
        ),
        proven_not_false_t(A,
                          B,
                          anc(C, [askable_t(A)|D])).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:203
% :- test_boxlog(v(askable_t(PROP_VAR), fallacy_t(PROP_VAR))).
proven_askable_t(A, B, anc(C, D)) :-
        proven_not_fallacy_t(A,
                            B,
                            anc([askable_t(A)|C], D)).
proven_fallacy_t(A, B, anc(C, D)) :-
        proven_not_askable_t(A,
                            B,
                            anc([fallacy_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:204
% :- test_boxlog(=>(asserted_t(PROP_VAR), true_t(PROP_VAR))).
proven_true_t(A, B, anc(C, D)) :-
        proven_asserted_t(A,
                         B,
                         anc([true_t(A)|C], D)).
proven_not_asserted_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc(C, [asserted_t(A)|D])).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:205
% :- test_boxlog(=>(possible_t(PROP_VAR), &(~false_t(PROP_VAR), ~fallacy_t(PROP_VAR)))).
proven_not_false_t(A, B, anc(C, D)) :-
        proven_possible_t(A,
                        B,
                        anc(C, [false_t(A)|D])).
proven_not_fallacy_t(A, B, anc(C, D)) :-
        proven_possible_t(A,
                        B,
                        anc(C, [fallacy_t(A)|D])).
proven_not_possible_t(A, B, anc(C, D)) :-
        (   proven_false_t(A,
                          B,
                          anc(C, [possible_t(A)|D]))
        ;   proven_fallacy_t(A,
                            B,
                            anc(C, [possible_t(A)|D]))
        ).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:206
% :- test_boxlog(=>(fallacy_t(PROP_VAR), &(&(&(false_t(PROP_VAR), true_t(PROP_VAR)), ~unknown_t(PROP_VAR)), ~possible_t(PROP_VAR)))).
proven_false_t(A, B, anc(C, D)) :-
        proven_fallacy_t(A,
                        B,
                        anc([false_t(A)|C], D)).
proven_true_t(A, B, anc(C, D)) :-
        proven_fallacy_t(A,
                        B,
                        anc([true_t(A)|C], D)).
proven_not_unknown_t(A, B, anc(C, D)) :-
        proven_fallacy_t(A,
                        B,
                        anc(C, [unknown_t(A)|D])).
proven_not_possible_t(A, B, anc(C, D)) :-
        proven_fallacy_t(A,
                        B,
                        anc(C, [possible_t(A)|D])).
proven_not_fallacy_t(A, B, anc(C, D)) :-
        (   (   (   proven_not_false_t(A,
                                      B,
                                      anc(C,
                                          [fallacy_t(A)|D]))
                ;   proven_not_true_t(A,
                                     B,
                                     anc(C, [fallacy_t(A)|D]))
                )
            ;   proven_unknown_t(A,
                                B,
                                anc(C, [fallacy_t(A)|D]))
            )
        ;   proven_possible_t(A,
                             B,
                             anc(C, [fallacy_t(A)|D]))
        ).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:207
% :- test_boxlog(<=>(false_t(PROP_VAR), &(&(~true_t(PROP_VAR), ~possible_t(PROP_VAR)), ~unknown_t(PROP_VAR)))).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:208
% :- test_boxlog(=>(possible_t(PROP_VAR), &(~false_t(PROP_VAR), ~fallacy_t(PROP_VAR)))).
proven_not_false_t(A, B, anc(C, D)) :-
        proven_possible_t(A,
                         B,
                         anc(C, [false_t(A)|D])).
proven_not_fallacy_t(A, B, anc(C, D)) :-
        proven_possible_t(A,
                         B,
                         anc(C, [fallacy_t(A)|D])).
proven_not_possible_t(A, B, anc(C, D)) :-
        (   proven_false_t(A,
                          B,
                          anc(C, [possible_t(A)|D]))
        ;   proven_fallacy_t(A,
                            B,
                            anc(C, [possible_t(A)|D]))
        ).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:209
% :- test_boxlog(=>(&(true_t(PROP_VAR), false_t(PROP_VAR)), fallacy_t(PROP_VAR))).
proven_fallacy_t(A, B, anc(C, D)) :-
        proven_true_t(A,
                     B,
                     anc([fallacy_t(A)|C], D)),
        proven_false_t(A,
                      B,
                      anc([fallacy_t(A)|C], D)).
proven_not_true_t(A, B, anc(C, D)) :-
        proven_false_t(A, B, anc(C, [true_t(A)|D])),
        proven_not_fallacy_t(A,
                            B,
                            anc(C, [true_t(A)|D])).
proven_not_false_t(A, B, anc(C, D)) :-
        proven_true_t(A, B, anc(C, [false_t(A)|D])),
        proven_not_fallacy_t(A,
                            B,
                            anc(C, [false_t(A)|D])).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:210
% :- test_boxlog(=>(true_t(PROP_VAR), &(&(~false_t(PROP_VAR), possible_t(PROP_VAR)), ~unknown_t(PROP_VAR)))).
proven_not_false_t(A, B, anc(C, D)) :-
        proven_true_t(A, B, anc(C, [false_t(A)|D])).
proven_possible_t(A, B, anc(C, D)) :-
        proven_true_t(A,
                     B,
                     anc([possible_t(A)|C], D)).
proven_not_unknown_t(A, B, anc(C, D)) :-
        proven_true_t(A,
                     B,
                     anc(C, [unknown_t(A)|D])).
proven_not_true_t(A, B, anc(C, D)) :-
        (   (   proven_false_t(A,
                              B,
                              anc(C, [true_t(A)|D]))
            ;   proven_not_possible_t(A,
                                     B,
                                     anc(C, [true_t(A)|D]))
            )
        ;   proven_unknown_t(A,
                            B,
                            anc(C, [true_t(A)|D]))
        ).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:211
% :- test_boxlog(=>(true_t(PROP_VAR), possible_t(PROP_VAR))).
proven_possible_t(A, B, anc(C, D)) :-
        proven_true_t(A,
                     B,
                     anc([possible_t(A)|C], D)).
proven_not_true_t(A, B, anc(C, D)) :-
        proven_not_possible_t(A,
                            B,
                            anc(C, [true_t(A)|D])).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:212
% :- test_boxlog(v(v(true_t(PROP_VAR), unknown_t(PROP_VAR)), false_t(PROP_VAR))).
proven_true_t(A, B, anc(C, D)) :-
        proven_not_unknown_t(A,
                            B,
                            anc([true_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([true_t(A)|C], D)).
proven_unknown_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([unknown_t(A)|C], D)),
        proven_not_false_t(A,
                          B,
                          anc([unknown_t(A)|C], D)).
proven_false_t(A, B, anc(C, D)) :-
        proven_not_true_t(A,
                         B,
                         anc([false_t(A)|C], D)),
        proven_not_unknown_t(A,
                            B,
                            anc([false_t(A)|C], D)).
% /mnt/gggg/logicmoo_workspace/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_sanity.pl:213
% :- test_boxlog(=>(unknown_t(PROP_VAR), &(&(&(~true_t(PROP_VAR), possible_t(PROP_VAR)), ~asserted_t(PROP_VAR)), ~false_t(PROP_VAR)))).
proven_not_true_t(A, B, anc(C, D)) :-
        proven_unknown_t(A,
                        B,
                        anc(C, [true_t(A)|D])).
proven_possible_t(A, B, anc(C, D)) :-
        proven_unknown_t(A,
                        B,
                        anc([possible_t(A)|C], D)).
proven_not_asserted_t(A, B, anc(C, D)) :-
        proven_unknown_t(A,
                        B,
                        anc(C, [asserted_t(A)|D])).
proven_not_false_t(A, B, anc(C, D)) :-
        proven_unknown_t(A,
                        B,
                        anc(C, [false_t(A)|D])).
proven_not_unknown_t(A, B, anc(C, D)) :-
        (   (   (   proven_true_t(A,
                                 B,
                                 anc(C, [unknown_t(A)|D]))
                ;   proven_not_possible_t(A,
                                         B,
                                         anc(C,
                                             [unknown_t(A)|D]))
                )
            ;   proven_asserted_t(A,
                                 B,
                                 anc(C, [unknown_t(A)|D]))
            )
        ;   proven_false_t(A,
                          B,
                          anc(C, [unknown_t(A)|D]))
        ).


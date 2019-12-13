:- module(test_tornado,
  [test_tornado/0]).
:- use_module(library(plunit)).

test_tornado:-
    trill:set_algorithm(tornado),
    run_tests([tornado_biopax,
    tornado_dbpedia,
    tornado_vicodi,
    tornado_brca,
    tornado_commander,
    tornado_johnEmployee,
    tornado_peoplePets,
    tornado_pizza,
    non_det]).

:- use_module(library(trill_test/trill_test)).

:- begin_tests(tornado_brca, []).

:- ensure_loaded(library('examples/BRCA.pl')).

test(rkb_brca):-
  run((reload_kb(false),true)).
test(p_wlbrcr_h):-
  run((prob_instanceOf('WomanUnderLifetimeBRCRisk','Helen',Prob),close_to(Prob,0.123))).
test(p_wa_wulbrcr):-
  run((prob_sub_class('WomanAged3040','WomanUnderLifetimeBRCRisk',Prob),close_to(Prob,0.123))).

:- end_tests(tornado_brca).


:- begin_tests(tornado_vicodi, []).

:-ensure_loaded(library(examples/vicodi)).

test(rkb_v):-
  run((reload_kb(false),true)).
test(p_r_avdpf):-
  run((prob_instanceOf('vicodi:Role','vicodi:Anthony-van-Dyck-is-Painter-in-Flanders',Prob),close_to(Prob,0.27540000000000003))).
test(p_p_r):-
  run((prob_sub_class('vicodi:Painter','vicodi:Role',Prob),close_to(Prob,0.30600000000000005))).

:- end_tests(tornado_vicodi).


:- begin_tests(tornado_commander, []).

:-ensure_loaded(library(examples/commander)).

test(rkb_c):-
  run((reload_kb(false),true)).
test(e_c_j):-
  run((prob_instanceOf(commander,john,Prob),close_to(Prob,1))).

:- end_tests(tornado_commander).


:- begin_tests(tornado_peoplePets, []).

:-ensure_loaded(library(examples/peoplePets)).

test(rkb_pp):-
  run((reload_kb(false),true)).
test(p_nl_k):-
  run((prob_instanceOf('natureLover','Kevin',Prob),close_to(Prob,0.348))).

:- end_tests(tornado_peoplePets).


:- begin_tests(tornado_biopax, []).

:-ensure_loaded(library(examples/biopaxLevel3)).

test(rkb_bp):-
  run((reload_kb(false),true)).
test(p_twbr_e):-
  run((prob_sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',Prob),close_to(Prob,0.98))).

:- end_tests(tornado_biopax).


:- begin_tests(tornado_dbpedia, []).

:-ensure_loaded(library('examples/DBPedia.pl')).

test(rkb_dbp):-
  run((reload_kb(false),true)).
test(p_p_pp):-
  run((prob_sub_class('dbpedia:Place','dbpedia:PopulatedPlace',Prob),close_to(Prob,0.8273765902816))).

:- end_tests(tornado_dbpedia).


:- begin_tests(tornado_johnEmployee, []).

:-ensure_loaded(library(examples/johnEmployee)).

test(rkb_je):-
  run((reload_kb(false),true)).
test(e_p_j):-
  run((prob_instanceOf('johnEmployee:person','johnEmployee:john',Prob),close_to(Prob,1))).
  
:- end_tests(tornado_johnEmployee).

:- begin_tests(tornado_pizza, []).

:- ensure_loaded(library(examples/pizza)).

test(rkb_pizza):-
  run((reload_kb(false),true)).
test(p_inc_kb):-
  run((prob_inconsistent_theory(Prob),close_to(Prob,0.0))).
test(p_uns_tof):-
  run((prob_unsat('tofu',Prob),close_to(Prob,1.0))).

:- end_tests(tornado_pizza).

:- begin_tests(non_det, []).

:-ensure_loaded(library(examples/example_or_rule)).

test(rkb_non_det):-
  run((reload_kb(false),true)).
test(p_u_a):-
  run((prob_unsat(a,Prob),close_to(Prob,0.03393568))).

:- end_tests(non_det).


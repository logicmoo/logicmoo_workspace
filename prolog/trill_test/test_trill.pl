:- module(test_trill,
  [test_trill/0]).
:- use_module(library(plunit)).

test_trill:-
    trill:set_algorithm(trill),
    run_tests([trill_biopax,
    %trill_biopax_rdf,
    trill_dbpedia,
    trill_brca,
    trill_commander,
    trill_johnEmployee,
    trill_peoplePets,
    trill_vicodi,
    trill_pizza,
    non_det,
    non_det_max,
    local_cons]).

:- use_module(library(trill_test/trill_test)).

:- begin_tests(trill_brca, []).

:- ensure_loaded(library('examples/BRCA.pl')).

test(p_wlbrcr_h):-
  run((prob_instanceOf('WomanUnderLifetimeBRCRisk','Helen',Prob),close_to(Prob,0.123))).
test(ne_wlbrcr_h):-
  run((aggregate_all(count, (instanceOf('WomanUnderLifetimeBRCRisk','Helen',_ListExpl)), Count), Count = 5)).
test(p_wa_wulbrcr):-
  run((prob_sub_class('WomanAged3040','WomanUnderLifetimeBRCRisk',Prob),close_to(Prob,0.123))).
test(ne_wa_wulbrcr):-
  run((aggregate_all(count, (sub_class('WomanAged3040','WomanUnderLifetimeBRCRisk',_ListExpl)), Count), Count = 2)).

:- end_tests(trill_brca).


:- begin_tests(trill_vicodi, []).

:-ensure_loaded(library(examples/vicodi)).

test(p_r_avdpf):-
  run((prob_instanceOf('vicodi:Role','vicodi:Anthony-van-Dyck-is-Painter-in-Flanders',Prob),close_to(Prob,0.27540000000000003))).
test(p_p_r):-
  run((prob_sub_class('vicodi:Painter','vicodi:Role',Prob),close_to(Prob,0.30600000000000005))).

:- end_tests(trill_vicodi).


:- begin_tests(trill_commander, []).

:-ensure_loaded(library(examples/commander)).

test(e_c_j):-
  run((instanceOf(commander,john,Expl),
       one_of(Expl,[[equivalentClasses([guard, soldier]), classAssertion(allValuesFrom(commands, guard), john), subClassOf(allValuesFrom(commands, soldier), commander)]])
  )).

:- end_tests(trill_commander).


:- begin_tests(trill_peoplePets, []).

:-ensure_loaded(library(examples/peoplePets)).

test(p_nl_k):-
  run((prob_instanceOf('natureLover','Kevin',Prob),close_to(Prob,0.8696))).
test(ne_nl_k):-
  run((aggregate_all(count, (instanceOf('natureLover','Kevin',_ListExpl)), Count),Count = 3)).

:- end_tests(trill_peoplePets).


:- begin_tests(trill_biopax, []).

:-ensure_loaded(library(examples/biopaxLevel3)).

test(p_twbr_e):-
  run((prob_sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',Prob),close_to(Prob,0.98))).
test(e_twbr_e):-
  run((sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',ListExpl),
       one_of(ListExpl,[[subClassOf('http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#Conversion'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion','http://www.biopax.org/release/biopax-level3.owl#Interaction'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction','http://www.biopax.org/release/biopax-level3.owl#Entity'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction')],
[subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion','http://www.biopax.org/release/biopax-level3.owl#Interaction'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction','http://www.biopax.org/release/biopax-level3.owl#Entity'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Transport','http://www.biopax.org/release/biopax-level3.owl#Conversion'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#Transport')]])
  )).
test(ae_twbr_e):-
  run((all_sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',Expl),
       same_expl(Expl,[[subClassOf('http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#Conversion'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion', 'http://www.biopax.org/release/biopax-level3.owl#Interaction'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction', 'http://www.biopax.org/release/biopax-level3.owl#Entity'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction')],
       [subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion', 'http://www.biopax.org/release/biopax-level3.owl#Interaction'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction', 'http://www.biopax.org/release/biopax-level3.owl#Entity'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Transport', 'http://www.biopax.org/release/biopax-level3.owl#Conversion'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#Transport')]])
  )).

:- end_tests(trill_biopax).

:- begin_tests(trill_biopax_rdf, []).

:-ensure_loaded(library(trill)).

test(p_twbr_e):-
  run((init_trill(trill),load_owl_kb('../examples/biopaxLevel3_rdf.owl'),prob_sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',Prob),close_to(Prob,0.98))).
test(e_twbr_e):-
  run((sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',ListExpl),
       one_of(ListExpl,[[subClassOf('http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#Conversion'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion','http://www.biopax.org/release/biopax-level3.owl#Interaction'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction','http://www.biopax.org/release/biopax-level3.owl#Entity'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction')],
[subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion','http://www.biopax.org/release/biopax-level3.owl#Interaction'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction','http://www.biopax.org/release/biopax-level3.owl#Entity'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#Transport','http://www.biopax.org/release/biopax-level3.owl#Conversion'),subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction','http://www.biopax.org/release/biopax-level3.owl#Transport')]])
  )).
test(ae_twbr_e):-
  run((all_sub_class('biopax:TransportWithBiochemicalReaction','biopax:Entity',Expl),
       same_expl(Expl,[[subClassOf('http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#Conversion'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion', 'http://www.biopax.org/release/biopax-level3.owl#Interaction'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction', 'http://www.biopax.org/release/biopax-level3.owl#Entity'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction')],
       [subClassOf('http://www.biopax.org/release/biopax-level3.owl#Conversion', 'http://www.biopax.org/release/biopax-level3.owl#Interaction'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Interaction', 'http://www.biopax.org/release/biopax-level3.owl#Entity'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#Transport', 'http://www.biopax.org/release/biopax-level3.owl#Conversion'),
       subClassOf('http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction', 'http://www.biopax.org/release/biopax-level3.owl#Transport')]])
  )).

:- end_tests(trill_biopax_rdf).


:- begin_tests(trill_dbpedia, []).

:-ensure_loaded(library('examples/DBPedia.pl')).

test(p_p_pp):-
  run((prob_sub_class('dbpedia:Place','dbpedia:PopulatedPlace',Prob),close_to(Prob,0.8273765902816))).
test(ae_p_pp):-
  run((all_sub_class('dbpedia:Place','dbpedia:PopulatedPlace',Expl),
       same_expl(Expl,[[equivalentClasses(['http://dbpedia.org/ontology/A73_A0_',intersectionOf(['http://dbpedia.org/ontology/PopulatedPlace','http://dbpedia.org/ontology/Settlement'])]),subClassOf('http://dbpedia.org/ontology/Place','http://dbpedia.org/ontology/A73_A0_')],[subClassOf('http://dbpedia.org/ontology/Place','http://dbpedia.org/ontology/PopulatedPlace')],[equivalentClasses(['http://dbpedia.org/ontology/A0_144_',intersectionOf(['http://dbpedia.org/ontology/Place','http://dbpedia.org/ontology/PopulatedPlace'])]),subClassOf('http://dbpedia.org/ontology/Place','http://dbpedia.org/ontology/Settlement'),subClassOf('http://dbpedia.org/ontology/Settlement','http://dbpedia.org/ontology/A0_144_')],[subClassOf('http://dbpedia.org/ontology/Place','http://dbpedia.org/ontology/Settlement'),subClassOf('http://dbpedia.org/ontology/Settlement','http://dbpedia.org/ontology/PopulatedPlace')]])
  )).

:- end_tests(trill_dbpedia).


:- begin_tests(trill_johnEmployee, []).

:-ensure_loaded(library(examples/johnEmployee)).

test(e_p_j):-
  run((instanceOf('johnEmployee:person','johnEmployee:john',Expl),
       same_expl([Expl],[[classAssertion('http://example.foo#employee', 'http://example.foo#john'), subClassOf('http://example.foo#employee', 'http://example.foo#worker'), subClassOf('http://example.foo#worker', 'http://example.foo#person')]])
  )).
  
:- end_tests(trill_johnEmployee).


:- begin_tests(trill_pizza, []).

:- ensure_loaded(library(examples/pizza)).

test(p_inc_kb):-
  run((prob_inconsistent_theory(Prob),close_to(Prob,0.0))).
test(p_uns_tof):-
  run((prob_unsat('tofu',Prob),close_to(Prob,1.0))).
test(e_uns_tof):-
  run((unsat('tofu',Expl),
       same_expl([Expl],[[disjointClasses([cheeseTopping, vegetableTopping]), subClassOf(soyCheeseTopping, cheeseTopping), subClassOf(soyCheeseTopping, vegetableTopping), subClassOf(tofu, soyCheeseTopping)]])
  )).

:- end_tests(trill_pizza).

:- begin_tests(non_det, []).

:-ensure_loaded(library(examples/example_or_rule)).

test(p_u_a):-
  run((prob_unsat(a,Prob),close_to(Prob,0.03393568))).

test(e_u_a):-
  run((all_unsat(a,Expl),
  same_expl(Expl,[
      [subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),subClassOf(a,unionOf([complementOf(c),complementOf(d)])),subClassOf(b,intersectionOf([c,d]))],
      [subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),subClassOf(a,unionOf([f,allValuesFrom(r,b)])),subClassOf(a,unionOf([complementOf(c),complementOf(f)])),subClassOf(b,complementOf(e)),subClassOf(b,intersectionOf([c,d]))],
      [subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),subClassOf(a,unionOf([f,allValuesFrom(r,b)])),subClassOf(a,unionOf([intersectionOf([c,complementOf(c)]),complementOf(f)])),subClassOf(b,complementOf(e))],
      [subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),subClassOf(a,unionOf([f,allValuesFrom(r,b)])),subClassOf(b,complementOf(e)),subClassOf(b,complementOf(f))],
      [subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),subClassOf(b,complementOf(e)),subClassOf(b,intersectionOf([c,d])),subClassOf(c,intersectionOf([minCardinality(1,r),e]))]
      ])
  )).

:- end_tests(non_det).

:- begin_tests(non_det_max, []).

:-ensure_loaded(library(examples/example_max_rule)).

test(e_i):-
  run((all_inconsistent_theory(Expl),
  same_expl(Expl,[[disjointClasses([b,e,f]),classAssertion(a,'1'),classAssertion(c,'3'),classAssertion(c,'4'),classAssertion(e,'3'),classAssertion(f,'4'),subClassOf(a,maxCardinality(1,s,c)),propertyAssertion(s,'1','3'),propertyAssertion(s,'1','4')],
                  [disjointClasses([b,e,f]),classAssertion(a,'1'),classAssertion(b,'2'),classAssertion(c,'2'),classAssertion(c,'4'),classAssertion(f,'4'),subClassOf(a,maxCardinality(1,s,c)),propertyAssertion(s,'1','2'),propertyAssertion(s,'1','4')],
                  [disjointClasses([b,e,f]),classAssertion(a,'1'),classAssertion(b,'2'),classAssertion(c,'2'),classAssertion(c,'3'),classAssertion(e,'3'),subClassOf(a,maxCardinality(1,s,c)),propertyAssertion(s,'1','2'),propertyAssertion(s,'1','3')]
                ])
  )).

:- end_tests(non_det_max).


:- begin_tests(local_cons, []).

:-ensure_loaded(library(examples/local_inconsistent_kb)).


%test(p_in):-
%  run((prob_inconsistent_theory(Prob),close_to(Prob,1.0))).

%test(e_in):-
%  run((all_inconsistent_theory(Expl),
%       same_expl(Expl,[[classAssertion(a, ind1),classAssertion(complementOf(x), ind2),subClassOf(a, allValuesFrom(r, x)),propertyAssertion(r, ind1, ind2)]])
%  )).

test(p_pv_3_4):-
  run((prob_property_value(t,ind3,ind4,Prob),close_to(Prob,1.0))).

test(e_pv_3_4):-
  run((all_property_value(r,ind3,ind4,Expl),
       same_expl(Expl,[[subPropertyOf(s, t), subPropertyOf(t, r), subPropertyOf(u, s), propertyAssertion(u, ind3, ind4)]])
  )).

test(p_i_x_4):-
  run((prob_instanceOf(x,ind4,Prob),close_to(Prob,1.0))).

test(e_i_x_4):-
  run((all_instanceOf(x,ind4,Expl),
       same_expl(Expl,[[classAssertion(a, ind3), subClassOf(a, allValuesFrom(r, x)), subPropertyOf(s, t), subPropertyOf(t, r), subPropertyOf(u, s), propertyAssertion(u, ind3, ind4)]])
  )).

:- end_tests(local_cons).

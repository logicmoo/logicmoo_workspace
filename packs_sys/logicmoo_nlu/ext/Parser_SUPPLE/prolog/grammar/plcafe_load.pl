:- current_engine(E),java_method(E, addPackage('shef.nlp.supple.prolog.cafe'), _).

% load and compile default set of grammars
:- compile_grammars(['general_ne_rules','aircraft_ne_rules','person_ne_rules','location_ne_rules','space_ne_rules','organ_ne_rules','money_ne_rules','measure_ne_rules','time_ne_rules','timex_ne_rules','default_ne_rules','npcore_rules','pp_rules','np_rules','vpcore_rules','vp_rules','rel_rules','s_rules','q_rules']).

% :- write_compiled_grammar('compiled_grammar.pl').


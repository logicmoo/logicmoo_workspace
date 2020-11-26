% Resource interface for context free grammars.
% Used in the tvGoDiS system, where the speech recognition grammar was
% compiled from a CFG resource. 




:- multifile is_resource_type/1,resource_relation/2, resource_relation_type/2.
:- discontiguous resource_relation/2, resource_relation_type/2.

is_resource_type(cfg).

/*----------------------------------------------------------------------
     cfg
----------------------------------------------------------------------*/


resource_relation( rule, [Grammar, R ] ):-
	Grammar:rule(R).
resource_relation_type( rule, [cfg, cfg_production_rule] ).

resource_relation( rules, [Grammar, Rs] ):-
	Grammar:rules(Rs).
resource_relation_type( rules, [cfg, set(cfg_production_rule)] ).

resource_relation( start_symbol, [Grammar, S] ):-
	Grammar:start_symbol(S).

resource_relation_type( start_symbol, [cfg, cfg_nonterminal ]).


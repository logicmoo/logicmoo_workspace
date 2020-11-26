/*************************************************************************

        name: resource_interfaces.pl 
 description: GoDiS-AOD Resource interface type definitions
     authors: Original code (June 2002) by Staffan Larsson (SL)
              Modifications by SL and David Hjelm (DH)
*************************************************************************/

/************************************************************************

HISTORY, started 031104

030321 (DH)
- modified dev_query relation to allow for parameters in device queries

031104
- modifed "depends" to include the "bind" and "raise" plan constructs [SL]

?????? (DH)
- modified "resolves" relation to include short answer to altq reg. props
- modified "combines" relation to include elliptical answer to altq reg. props

050302 (SL)
- added device relation dev_queryAll: gets all answers

050621 (SL)
- separated out domain help predicates to file "godis_semantics"

050630 (SL)
- Added a new predicate "hypernym" to the domain resource. In update-rules, revised integrateUsrRequest to allow intelligent interpretation of request htypernyms, i.e. requests that mean different things depending on the current topmost goal action. This required adding. Also, some changes were made to irrelevantFollowup and rejectAction to stop them from catching hypernym requests.

***************************************************************************/

:- module(resource_interfaces, [resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3 ]).

:- multifile resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.

:- discontiguous resource_of_type/2, resource_variable_of_type/2, is_resource_type/1, resource_operation/4, resource_relation/2, resource_operation_type/4, resource_relation_type/2, resource_function_type/3, resource_selector_type/3.


:- use_module(library(lists)).

% dummy so resource_operation is defined somewhere

resource_operation( dummy, dummy, dummy, dummy ).


/*----------------------------------------------------------------------
     cfg
----------------------------------------------------------------------*/
is_resource_type( cfg ).
resource_variable_of_type( sr_grammar, cfg ).
resource_of_type( srg_tvgodis_svenska, cfg ).
resource_of_type( srg_tvgodis_svenska2, cfg ).
resource_of_type( _, cfg ).

resource_relation( rule, [Grammar, R ] ):-
	Grammar:rule(R).
resource_relation_type( rule, [cfg, cfg_production_rule] ).

resource_relation( rules, [Grammar, Rs] ):-
	Grammar:rules(Rs).
resource_relation_type( rules, [cfg, set(cfg_production_rule)] ).

resource_relation( start_symbol, [Grammar, S] ):-
	Grammar:start_symbol(S).

resource_relation_type( start_symbol, [cfg, cfg_nonterminal ]).




/*----------------------------------------------------------------------
     lexicon
----------------------------------------------------------------------*/

is_resource_type( lexicon ).

resource_variable_of_type( lexicon, lexicon ).

resource_of_type( lexicon_travel_english, lexicon ).
resource_of_type( lexicon_autoroute_english, lexicon ).
resource_of_type( lexicon_travel_svenska, lexicon ).
resource_of_type( lexicon_cellphone_svenska, lexicon ).
resource_of_type( lexicon_telephone_english, lexicon ).
resource_of_type( lexicon_vcr_english, lexicon ).
resource_of_type( lexicon_vcr_svenska, lexicon ).
resource_of_type( lexicon_telvcr_english, lexicon ).
resource_of_type( lexicon_telvcr_svenska, lexicon ).
resource_of_type( lexicon_telvcrlogin_english, lexicon ).
resource_of_type( lexicon_telvcrlogin_svenska, lexicon ).
resource_of_type( lexicon_agenda_english, lexicon ).
resource_of_type( _, lexicon ).

resource_relation( input_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : input_form( Phrase, Move ).
resource_relation_type( input_form, [ lexicon, string, dmove ] ).

resource_relation( output_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : output_form( Phrase, Move ).
resource_relation_type( output_form, [ lexicon, string, dmove ] ).

resource_relation( output_formcom, [Lexicon,Move,set(Com),Phrase] ):-
        Lexicon : output_form(Move,Com,Phrase).
resource_relation_type( output_form, [ lexicon,dmove,set(prop),string ] ).

resource_relation( yn_answer, [Lexicon, A] ) :-
	Lexicon : yn_answer( A ).
resource_relation_type( yn_answer, [ lexicon, answer ] ).

/*----------------------------------------------------------------------
     domain
----------------------------------------------------------------------*/

:- ensure_loaded(library(godis_semantics)).

is_resource_type( domain ).

resource_variable_of_type( domain, domain ).

resource_of_type( travel, domain ).
resource_of_type( autoroute, domain ).
resource_of_type( telephone, domain ).
resource_of_type( vcr, domain ).
resource_of_type( telvcr, domain ).
resource_of_type( telvcrlogin, domain ).
resource_of_type( tvgodis, domain ).
resource_of_type( agenda, domain).
resource_of_type( _, domain).

resource_relation( relevant, [Domain, Answer, Query ] ) :-
	relevant_answer( Query, Answer, Domain ).
resource_relation_type( relevant_answer, [domain, question, answer] ).

resource_relation( resolves, [Domain, Answer, Query ] ) :-
	resolves( Query, Answer, Domain ).
resource_relation_type( resolves, [domain, question, answer] ).

resource_relation( plan, [Domain, Task,stackset(Plan)] ) :-
	Domain : plan( Task, Plan ).
resource_relation_type( plan, [ ] ).

resource_relation( combine, [Domain, Q, A, P] ):-
	combine(Q, A, P, Domain ).
resource_relation_type( combine, [domain, question, answer, prop] ).

resource_relation( abstract, [Domain, A, P, Q] ):-
	abstract( A, P, Q, Domain ).
resource_relation_type( abstract, [domain, answer, prop, question] ).

resource_relation( incompatible, [Domain, P1, P2] ):-
	incompatible( P1, P2, Domain ).
resource_relation_type( incompatible, [domain, prop, prop] ).

resource_relation( proposition, [Domain, P] ):-
	Domain : sort_restr( P ).
resource_relation_type( proposition, [domain, prop] ).

resource_relation( question, [Domain, Q] ):-
	Domain : sort_restr( issue(Q) ).
resource_relation_type( question, [domain, question] ).

resource_relation( issue, [Domain, Q] ):-
	Domain : sort_restr( issue(Q) ).
resource_relation_type( issue, [domain, question] ).

resource_relation( action, [Domain, A] ):-
	Domain : sort_restr( action(A) ).
resource_relation_type( action, [domain, action] ).



%Q1 depends on Q
% modified to include "bind" and "raise" [SL031104]
resource_relation( depends, [Domain, Q1, Q] ):-
	Domain : plan( Q1, Plan ),
	( member( findout( Q ), Plan );
	    ( member( bind( Q ), Plan );
		member( raise( Q ), Plan ) ) ).

resource_relation( depends, [Domain, Q1, Q] ):-
	Domain : depends( Q1, Q ).
resource_relation_type( depends, [domain, question, question] ).



% default question
resource_relation( default_question, [Domain, Q] ):-
	Domain : default_question( Q ).
resource_relation_type( default_question, [domain, question] ).


% actions

resource_relation( postcond, [_Domain, Action, done(Action)] ).
resource_relation( postcond, [Domain, Action, Prop] ):-
	Domain : postcond( Action, Prop ).
resource_relation_type( postcond, [domain, action, prop ] ).


resource_relation( valid_parameter, [Domain, Prop] ) :-
	Domain : valid_parameter( Prop ).
resource_relation_type( valid_parameter, [domain, prop ] ).

resource_relation( dominates, [Domain, A1, A2] ) :-
	dominates( Domain, A1, A2).
resource_relation_type( dominates, [domain, action, action ] ).

resource_relation( hypernym, [Domain, A1, A2] ) :-
	Domain : hypernym( A1, A2 ).
resource_relation_type( hypernym, [domain, action, action ] ).

/*----------------------------------------------------------------------
     database
----------------------------------------------------------------------*/

is_resource_type( database ).

resource_variable_of_type( database, database ).

resource_of_type( travel, database ).
resource_of_type( autoroute, database ).

resource_relation( consultDB, [Database, Query, PropSet, Answer] ) :-
	Database : consultDB( Query, PropSet, Answer ), !.
%resource_relation( consultDB, [_, X^Q, _, notexist(Q) ] ).
resource_relation( consultDB, [_, X^Q, _, notexist(X,Q) ] ).
resource_relation( consultDB, [_, Q, _, unknown(Q) ] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).

resource_relation( consultDBx, [Database, Query, PropSet, AnswerSet] ) :-
	Database : consultDBx( Query, PropSet, AnswerSet ), !.
resource_relation( consultDBx, [_, X^Q, _, set([notexist(X,Q)]) ] ).
resource_relation( consultDBx, [_, Q, _, set([unknown(Q)])] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).


resource_relation( validDBparameter, [Database, Prop ] ) :-
	Database : validDBparameter( Prop ), !.
resource_relation_type( validDBparameter, [database, prop]).


/*----------------------------------------------------------------------
     device
----------------------------------------------------------------------*/

is_resource_type( upnp_dev ).

% note: "devices" is a record containing devices
% HACK 021818
%resource_variable_of_type( devices, record([vcr:upnp_dev, telephone:upnp_dev])).
resource_variable_of_type( devices, record([])).
%	findall( Var:upnp_dev, of_type( Var, upnp_dev ), R ).

% note: device_vcr etc. refers to actual devices ("tokens"), not types 

of_type( device_vcr, upnp_dev ).
of_type( device_telephone, upnp_dev ).
of_type( device_telvcr, upnp_dev ).
of_type( device_telvcrlogin, upnp_dev ).
of_type( device_agendatalk, upnp_dev ).

resource_relation( dev_get, [ Dev, Var, VarVal ] ) :-
	Dev : dev_get( Var, Val ),
	VarVal =.. [ Var, Val ].
resource_relation_type( dev_get, [device, _, _]).

% resource_relation( dev_query, [ Dev, Query, Answer ] ) :-
% 	Dev : dev_query( Query, Answer ).
% resource_relation_type( dev_query, [device, _, answer]).


%DH 21/3-2003 - to allow for parameters in device queries
resource_relation( dev_query, [Dev, Query, set(PropList), Answer ] ) :-
	Dev : dev_query( Query, PropList, Answer ).
resource_relation_type( dev_query, [device, _ , set(prop), answer]).

% SL 050302 - get all answers
resource_relation( dev_queryAll, [Dev, Query, set(PropList), set(AnswerList) ] ) :-
	Dev : dev_queryAll( Query, PropList, AnswerList ).
resource_relation_type( dev_query, [device, _ , set(prop), set(prop)]).



resource_relation( valid_parameter, [ Dev, Prop] ) :-
	Dev : valid_parameter( Prop ).
resource_relation_type( valid_parameter, [device, prop ]).


resource_operation( dev_set, Dev, [ Var, Val ], Dev ) :-
	Dev : dev_set( Var, Val ).
resource_operation_type( dev_set, device, [ _, _], device ).

resource_operation( dev_do, Dev, [ Action, set(PropList) ], Dev ) :-
	Dev : dev_do( Action, PropList ).
resource_operation_type( dev_do, device, [ action, set(prop) ], device ).

/*----------------------------------------------------------------------
     asr_grammar
----------------------------------------------------------------------*/
is_resource_type( asr_grammar ).

resource_variable_of_type( asr_grammar, asr_grammar ).

resource_of_type( asrg_vcr_english, asr_grammar ).
resource_of_type( asrg_travel_english, asr_grammar ).
resource_of_type( asrg_telvcr_english, asr_grammar ).
resource_of_type( asrg_telvcrlogin_english, asr_grammar ).
resource_of_type( asrg_telvcr_svenska, asr_grammar ).
resource_of_type( asrg_telvcrlogin_svenska, asr_grammar ).
resource_of_type( asrg_agenda_english, asr_grammar ).


resource_relation( language, [Grammar, L] ):-
        Grammar:language(L).
resource_of_type( language, [asr_grammar, atom] ).

:-use_module('$TRINDIKIT/core/prolog/trindikit.pl').
:- ensure_loaded( app_search_paths ).
:- use_module(trindikit(tkit_properties),[setprop/2,prop/2]).

:-use_module(library(infostate)).
:-ensure_loaded(library(macros)).
:-ensure_loaded(library(alias)).
:-ensure_loaded(library(control)).

run:-run(svenska).
run(Lang):-
	run(agenda,Lang).
run(Domain,Lang):-
	set_properties(Domain,Lang),
	control.


quiet:-
	setprop(show_rules,no),
	setprop(show_state,no).
verb:-
	setprop(show_rules,yes),
	setprop(show_state,all).



selected_datatypes([string, move, atom, integer, bool, record, set, stack,
		    participant,
		    stackset, queue, oqueue, pair, assocset, godis_datatypes,
		    %Resource interface files:
		    domain,upnp_dev,lexicon]).

selected_modules([ input : input_textscore,
		   interpret : interpret_simple,
		   update : update,
		   select : select,
		   generate : generate_agendatalk,
		   output : output_simpletext
		 ]).


selected_resources( [ device_agenda,
		      lexicon_agenda_english,
		      lexicon_agenda_svenska,
		      domain_agenda ] ).

rivs( [ lexicon : lexicon,
	devices : record([]),
	domain : domain ] ).

mivs( [ input : string,
	output : string,
	latest_speaker : participant, %speaker,
	latest_moves : oqueue(dmove),
	next_moves : oqueue(dmove),
	program_state : program_state,
	score : real,
	timeout : real,
	language : language ]).

reset_ops( Domain,Lang,[ set( program_state, run),
		    set( language, Lang ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([agenda=device_agenda]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]).


set_properties(Domain,Lang):-
	selected_resources(Rs),
	setprop( resources,Rs),
	setprop( tis, yes),
	selected_datatypes(Ds),
	setprop( tis-datatype_files, Ds),
	rivs(RIVs),
	setprop( tis-rivs,RIVs),
	mivs(MIVs),
	setprop( tis-mivs,MIVs),
	infostate_variable_of_type(ISVar,ISType),
	setprop( tis-infostate, ISVar:ISType),
	setof(alias(Alias,Path),alias(Alias,Path),Aliases),
	setprop( tis-aliases,Aliases),
	setof(macro_cond(Macro,Cond),macro_cond(Macro,Cond),MacroConds),
	%setof(macro_op(Macro,Op),macro_op(Macro,Op),MacroOps),
	%append(MacroOps,MacroConds,Macros),
	setprop( tis-macros,MacroConds),
	reset_ops(Domain,Lang,ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,yes),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).


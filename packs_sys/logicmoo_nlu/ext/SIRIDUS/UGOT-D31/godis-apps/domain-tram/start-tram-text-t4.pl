/*************************************************************************

  name: start-tram-text-t4.pl
  description: GoDiS Tram spec file, text, for trindikit4
 
*************************************************************************/

:-use_module('$TRINDIKIT/core/prolog/trindikit.pl').
:- ensure_loaded( app_search_paths ).
:- use_module(trindikit(tkit_properties),[setprop/2,prop/2]).

:-ensure_loaded(library(macros)).
:-ensure_loaded(library(alias)).
:-ensure_loaded(library(control)).

:-use_module(library(lists),[append/3]).

:-use_module(modeprops).

language(english).
domain(tram).

selected_datatypes([string, move, atom, integer, bool, record, set, stack,
		    participant,
		    stackset, queue, oqueue, pair, assocset, godis_datatypes,
		    infoamount,
% DH: for now resource interfaces are loaded here too:
		    domain,upnp_dev,lexicon]).

selected_modules([ input : input_textscore,
		   interpret : interpret_simple,
		   update : update,
		   select : select,
		   generate : generate_simple,
		   output : output_simpletext
		 ]).

selected_resources( [ device_vcr,
		      lexicon_tram_english,
		      %lexicon_vcr_svenska,
		      domain_tram ] ).

rivs( [ lexicon : lexicon,
	devices : record([]),
	domain : domain ] ).

mivs( [ input : string,
	output : string,
	graph_output : string, 
	latest_speaker : participant,
	latest_moves : oqueue(dmove),
	next_moves : oqueue(dmove),
	program_state : program_state,
	score : real,
	timeout : real,
	language : language,
	voice : infoamount,
	graph : infoamount,
	speechinput : infoamount ]).


%infostate( is:IS ) :-
infostate_variable_of_type( is, IS ) :-
    IS = record( [ private : Private,
		   shared : Shared ] ), 
    Shared = record( [ com : set( proposition ),
		       move_history : oqueue( oqueue( dmove ) ),
		       actions : stackset( action ),
		       issues: stackset( question ),
		       qud : stackset( question ),
		       pm : set( move ),
		       lu : LU ] ), 
    Private = record( [ agenda: oqueue( action),
			plan : stackset( action ), 
			bel : set( proposition ),
			tmp : record( [ usr : TMP, sys: TMP ] ),
			nim : oqueue( pair( speaker, move ) ) ] ),
    LU = record( [ speaker : speaker,
		   moves : set( move ) ] ), 
    TMP = record( [ com : set( proposition ), 
		    issues : stackset( question ),
		    qud : stackset( question ),
		    actions : stackset( action ),
		    agenda : oqueue( action ),
		    plan : stackset( action ) ] ).


reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( voice, min ),
		    set( graph, compl ),
		    set( speechinput, indet ), % SE tram
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
%				 telephone=device_telephone]) ),
%		    set( devices, record([telephone=device_telephone]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	language( Lang ),
	domain( Domain ).


set_properties:-
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
	reset_operations(ResetOps),
	setprop( tis-reset_ops,ResetOps),
	
%	setprop( oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop( oaa,no),
	
	setprop( control,yes),
	control_algorithm(ControlAlgorithm),
	setprop( control-algorithm,[init=>ControlAlgorithm]),
	
	selected_modules(Modules),
	setprop( modules,Modules).




quiet:-
	setprop(tis-print_rules,no),
	setprop(tis-print_state,no).
verb:-
	setprop(tis-print_rules,yes),
	setprop(tis-print_state,all).

run:-
	set_properties,
	control.

rundriving:-
	set_properties_driving,
	control.

runtelephone:-
	set_properties_telephone,
	control.

runmeeting:-
	set_properties_meeting,
	control.

runathome:-
	set_properties_athome,
	control.


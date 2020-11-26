:- use_module('$TRINDIKIT/core/prolog/trindikit').
:- ensure_loaded(app_search_paths).
:-dynamic domain/1,lang/1.

%interpret, update, select and generate modules + TIS

resources( [ lexicon_player_svenska,
	     %lexicon_player_english,
	     device_player, device_dbase, domain_player ] ).

datatype_files([string, move, atom, integer, bool, record,participant,
		set, stack,stackset, queue, oqueue, pair,
		assocset, godis_datatypes,domain,upnp_dev,lexicon]).	    

modules( [ interpret : interpret_simple,
	   update : update,
	   select : select,
	   generate : generate_mm_simple,
	   output : output_mm
	 
	  % output : output_simpletext
	 ] ).
reset_ops( [ set( program_state, run),
	     set( language, Lang ),
	     set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
	     set( devices, record([player=device_player,
				   dbase=device_dbase]) ),
	     set( domain, $$dash2underscore(domain-Domain) ),
	     push(/private/agenda,greet),
	     push(/private/agenda,do(top)),
	     push( /shared/actions, top ),
	     score := 1.0 ]):-
	lang(Lang),
	domain(Domain).


macros( [ macro_cond( q_raising_icm(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content ) and
	    ( not ( Content = (not _) and Polarity=pos ) ) ] ),
	  macro_cond( q_raising_action(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content  and
	    ( not ( Content = (not _) and Polarity=pos ) ) )
	      or ( Move= raise(_) or Move = findout(_) )
	    ] ),
	  macro_cond( q_raising_move(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content  and
	    ( not ( Content = (not _) and Polarity=pos ) ) ) 
	      or ( Move = ask( _ ) )
	    ] )
	]).

aliases( [ alias( lm, latest_moves ), alias( sq, /shared/qud )]).

rivs( [ lexicon : lexicon,
	devices : record([]),
	domain : domain ] ).

mivs( [ input_queue: oqueue(string),
	input : string,
	output : string,
	output_gui : string,
	latest_speaker : participant, %speaker,
	latest_moves : oqueue(dmove),
	next_moves : oqueue(dmove),
	program_state : program_state,
	score : real,
	timeout : real,
	language : language ]).

infostate( is:IS ) :-
    IS = record( [ private : Private,
		   shared : Shared ] ), 
    Shared = record( [ com : set( proposition ),
		       actions : stackset( action ),
		       issues: stackset( question ),
		       qud : stackset( question ),
		       pm : set( move ),
		       lu : LU ] ), 
    Private = record( [ agenda: oqueue( action),
			plan : stackset( action ), 
			bel : set( proposition ),
			tmp : record( [ usr : TMP, sys: TMP ] ),
			nim : oqueue( pair( participant, move ) ) ] ),
			%nim : oqueue( pair( speaker, move ) ) ] ),
    LU = record( [ speaker : participant,
    %LU = record( [ speaker : speaker,
		   moves : set( move ) ] ), 
    TMP = record( [ com : set( proposition ), 
		    issues : stackset( question ),
		    qud : stackset( question ),
		    actions : stackset( action ),
		    agenda : oqueue( action ),
		    plan : stackset( action ) ] ).


startit(Domain,Lang):-
	setup(Domain,Lang),
	oaa_MainLoop(true).
setup(Domain,Lang):-
	retractall(domain(_)),
	retractall(lang(_)),
	assert(domain(Domain)),
	assert(lang(Lang)),
	resources(Rs),
	setprop(resources,Rs),
	setprop(tis,yes),
	datatype_files(Files),
	setprop(tis-datatype_files,Files),
	rivs(RIVs),
	setprop(tis-rivs,RIVs),
	mivs(MIVs),
	setprop(tis-mivs,MIVs),
	infostate(IS),
	setprop(tis-infostate, IS),
	aliases(Aliases),
	setprop(tis-aliases, Aliases ),
	reset_ops(ResetOps),
	setprop(tis-reset_ops,ResetOps),
	macros(Macros),
	setprop(tis-macros,Macros ),
	modules(Modules),
	setprop(modules,Modules),

	setprop(logger,yes),
	setprop(logger-dir,logs),

	setprop(log-state,print_state),
	setprop(log-modulecalls,yes),
	
	setprop(oaa-name,'dme'),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa,yes).


	
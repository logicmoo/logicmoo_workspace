:- use_module('$TRINDIKIT/core/prolog/trindikit').
:- ensure_loaded(app_search_paths).


%interpret, update, select and generate modules + TIS


datatype_files([string, move, atom, integer, bool, record,participant,
		set, stack,stackset, queue, oqueue, pair,
		assocset, godis_datatypes,domain,upnp_dev,lexicon]).	    

modules( [ interpret : interpret_simple,
	   update : update,
	   select : select,
	   generate : generate_mm_simple
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

lang(svenska).
domain(player).

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

mivs( [ input : string,
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



control_algorithm([ init => [input:init,
			     interpret:init,
			     generate:init,
			     output:init,select,generate,output,update],
		    
		    condition(not $program_state ==  run) => [input:quit,
							  output:quit,
							  interpret:quit,
							  generate:quit],
		    
		    %condition($latest_speaker == sys) => update,
		    condition(is_set(input)) => [oaa_Solve(start_timer(input,1.0))],
		    condition(alarm(timeout)) => [apply_update(set(input,""))],
		    condition(alarm(input)) => [interpret,update|SysTurn]
		  ]):-

	SysTurn = [  repeat( [ select,
			       test(not is_empty($next_moves)),
			       generate,
			       check_condition(to_atom($output,B)),
			       oaa_Solve(outputGuiTxt(B)),
			       check_condition(to_atom($output_gui,A)),
			       oaa_Solve(outputGui(A)),
			       print_state,
			       output,
			       
			       update,
			       test(not is_empty($/private/agenda)),
			       test(is_empty($/shared/qud))
			     ]),
		     if $program_state == run then
			[oaa_Solve(start_timer(timeout,3.0))]
		   ].

startit:-
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
	setprop(control,yes),
	control_algorithm(CTRLALGO),
	setprop(control-algorithm,CTRLALGO),
	setprop(oaa-name,'dme'),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa,yes),
	control.
	%oaa_MainLoop(true).

	
% player_files.pl

system_files(play,[
	param,ops,parse1,parse2,controller, history, 
	help,help_advisors,interface, setup, gen_menu, start_menu,local,
%	comms,
	invert,matches,notation,stat, thread,
	statify_theory, efficient_state, compile_syms,
	mobility, 
	alphabeta,
	value,
%	experiment,
	advisors
		  ]).

system_files(fullplay,[param,ops,parse1,parse2,controller,menus,interface,
	start_menu,local,comms,invert,matches,notation,stat, 
	compile_state,compile_syms,index_preds]).


% Files defined by generator system

system_files(generator,[
	genstructs,
	piece_names,
	gen,
	tokenizer,
	grammar,
	gen_parameters]).

system_files(library,[aux,tracing,timing,dynamic_load,shells,
	args,randoms,theoryl,menus]).

system_files(analysis,[struct,paths,analysis,mygraphs,floyd,
	tables,arrive,prom,possess,exclude,dominate,group,step,flight,global,potent,
	tourney]).

%----------------------------------------
% The following are used in statify_theory.pl:

theory_files([
	dynamic_preds,
	parse,
	boards,
	print_boards,
	legal,
	goals
	     ]).


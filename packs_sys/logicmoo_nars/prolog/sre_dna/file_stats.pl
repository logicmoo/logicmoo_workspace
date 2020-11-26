% ------------------------------------------------
% Jan 1999
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Statistics and I/O

:- expects_dialect(sicstus).
:- use_module(library(dialect/sicstus/system)).

print_tourn_stats(Gen) :-
	collect_stats(Gen), % should replace with param passing
	gp_stats(Gen, Time, Best, Worst, Avg, AvgD, Lamarck),   
	(var(Lamarck) -> Lamarck = lamarck(0,0,0) ; true),
	print_stat(Gen, Time, Best, Worst, Avg, AvgD, Lamarck),
	(popn_dump_P(yes) -> dump_population(Gen) ; true),
	!.

% collect_stats computes some statistics. gp_stats might already be
% asserted for generation by Lamarckian evol routine, if used. Retract it,
% but retain it's stat.

collect_stats(Gen) :-
	bagof(V, E^ID^individual(ID, V, E), L),
	size_of(L, N),
	sum_list(L, Sum),
	max_list(L, Max),
	bagof(ID, E^individual(ID, Max, E), WL),
	length(WL, SizeW),
	min_list(L, Min),
	individual(_, Min, Bexpr),
	bagof(ID, E^individual(ID, Min, E), BL),
	length(BL, SizeB),
	Avg is Sum / N,
	bagof(D, ID^VV^E^(individual(ID, VV, E),tree_depth(E,D)), M),
	average(M, AvgDepth),
	%time_stamp('%h:%02i:%02s%a',T),
	% datime(datime(_,_,_,Hour,Minute,Sec)),
        since_last_datime(total,call,HourT,MinuteT,SecT),
        since_last_datime(generation,retract,Hour,Minute,Sec),
	(retract(gp_stats(Gen,_,_,_,_,_,_,Lamarck)) ; true),
	assertz(gp_stats(Gen,Hour:Minute:Sec/HourT:MinuteT:SecT, best(Min, SizeB, Bexpr),
			 worst(Max, SizeW),avg(Avg),AvgDepth,Lamarck)),
	!.

since_last_datime(For,SetReset,Hour,Minute,Sec):- 
   once(call(SetReset,got_time(For,Was));Was=0),
   get_time(Now),
   DiffTime is Now - Was,
   (SetReset==retract -> asserta(got_time(For,Now)) ; true),
   stamp_date_time(DiffTime, date(_Year,_Month,_Day,Hour,Minute,Sec,_,_,_), 'UTC'),!.

:- dynamic(got_time/2).
:- get_time(Now),asserta(got_time(total,Now)),asserta(got_time(total,Now)).

% print run statistics
dump_stats(Run) :-
	wd_P(Dir),   % Windows
	working_directory(_,Dir), % Windows
	set_file_name("stats", Run, File),
	tell(File),	
	%once(time_stamp('Date: %W, %d %M %y    Time: %c:%02i%a', DateTime)),
        since_last_datime(total,retract,Hour,Min,Sec),	
        datime(datime(Year,Month,Day,_DA_Hour,_DA_Min,_DA_Sec)),
	writel([nl,nl,'***** Summary statistics: Run ', Run, ' *****',nl,nl,
		(Year-Month-Day-Hour:Min:Sec), nl, nl]),
	gp_stats(Gen, Time, Best, Worst, Avg, AvgDepth, Lamarck), % loops for all
	(var(Lamarck) -> Lamarck = lamarck(0,0,0) ; true),
	print_stat(Gen, Time, Best, Worst, Avg, AvgDepth,  Lamarck),
	fail.				   % loop driver
dump_stats(Run) :-
	nl, nl,
	% from gp_parameters file... 
	population_size_P(InitPopSize, PopSize),
	max_runs_P(MaxRun, RunSoln, MaxGen),
	prob_crossover_P(PC),
	%crossover_P(PIC, PTC),
	prob_internal_crossover_P(PIC),
	prob_terminal_mutation_P(PTM),
	max_depth_P(DepthInit, DepthCross),
	error_tolerance_P(Err),
	fitness_func_P(FitFile),
	dctg_file_P(FileDCTG),
	evaluator_reset_P(EvalReset, N),
	gen_type_P(GenType),
	best_in_run(Bgen, Fitness, Expr),  
	count_nodes(Expr, all, ENodeCnt),
	tree_depth(Expr, Edepth),
	tournament_size_P(TS, TR),
	lamarckian_P(LP, LK, Lsel, LCross),
	unique_population_P(Unique),
	%rep_limit_P(Rep),
	seed_P(RanMode, Y),
	% some other stats...
	min_grammar_prob_P(MinProb),
	gen_set_size_P(GenSet),
	sre_mintestcnt_P(Mintst),
	%sre_mutation_P(SREmut),
	%mutation_range_P(Mutrange),
	max_string_length_P(Maxstr),
	writel([nl,'Best Soln:', Expr,nl]),
	Expr^^construct(E), write('Expression: '),sre_pp(E), nl,
	writel(['found at generation ', Bgen,nl,
		'Soln # nodes:', ENodeCnt,nl,
		'Soln depth:', Edepth,nl,
		'Soln Fitness = ', Fitness,nl]),
	(gp_stats(MaxGen,_, best(MinLast, _, BexprLast), _, _, _, _) ->
		count_nodes(BexprLast, all, ENodeCntLast),
		tree_depth(BexprLast, EdepthLast),
		writel([nl,'Best Last Gen', MaxGen, ': ', BexprLast,nl]),
		BexprLast^^construct(ELast), write('Expression: '),sre_pp(ELast), nl,
		writel(['Last # nodes:', ENodeCntLast,nl,
			'Last depth:', EdepthLast,nl,
			'Last Fitness = ', MinLast,nl])
		;
		true), % if last generation never reached (soln found before)
	writel(['-------',nl,
		'Fitness func file:', FitFile, nl,
		'DCTG file:', FileDCTG, nl,
		'Evaluator reset:', EvalReset, ' N:', N, nl,
		'Generation type:', GenType, nl,
		'Init pop size = ', InitPopSize, nl,
		'Pop size = ', PopSize, nl,
		'Max runs = ', MaxRun, ', ', RunSoln, nl,
		'Max gen = ', MaxGen, nl,
		'Prob crossover = ', PC, nl,
		'Prob int cross = ', PIC, nl,
		'Prob term mutation = ', PTM, nl,
		'Tournament size: sel = ', TS, ' repl = ', TR, nl,
		'Lamarckian: ', LP, 'of popn, iterate = ', LK, ', select = ', Lsel, ', Prob Cross=', LCross, nl,
		'Unique popn = ', Unique, nl,
		'Random seed:', RanMode, ', Y=',Y, nl,
		'Max depth init = ', DepthInit, nl,
		'Max depth crossover = ', DepthCross, nl,
		'Initial test set size =', GenSet, nl,
		'Max test set string length =', Maxstr, nl,
		'Minimum test set count =', Mintst, nl,
		'Min grammar probability =',MinProb, nl,
		%'SRE numeric mutation rate =', SREmut, nl,
		%'SRE mutation range = +/-',Mutrange,nl,
		'Error tolerance = ', Err, nl]),
	%write('Best...'), nl, dna_summary(Expr),   % for DNA only
	%write('Best optimized...'), nl, 
	%mask_optimize(Expr, Fitness, ExprOpt),
	%dna_summary(ExprOpt),   % for DNA only
	/*
	(gp_stats(MaxGen,_, best(MinLast, _, BexprLast), _, _, _, _) ->
		write('Last...'), nl, 
		dna_summary(BexprLast),
		mask_optimize(BexprLast, MinLast, LastOpt),   % for DNA only
		write('Last optimized...'), nl,
		dna_summary(LastOpt)
		;
		true),
	*/
	writel(['*** End of Run ', Run, ' ***',nl]),
	told, tell(user),
	write_soln("soln", Run, Expr).  % <-- new.
	%write_soln("solnopt", Run, ExprOpt).  % <-- new.
	/*
	(gp_stats(MaxGen,_, best(MinLast, _, BexprLast), _, _, _, _) ->
		write_soln("last", Run, BexprLast), % <-- new
		write_soln("lastopt", Run, LastOpt)  
		;
		true).
	*/
	% dump_population(Run).

set_file_name(RootName, Run, File) :-
	append(RootName, "-", File0),
	name(Run, File1),
	append(File0, File1, File2),
	%once(time_stamp('.%d%02n%02y-%02c%02i', Name3)),
	%name(Name3, File3),
	datime(datime(Year,Month,Day,Hour,Min,Sec)),
	name(Year, N1),
	name(Month, N2),
	name(Day, N3),
	name(Hour, N4),
	name(Min, N5),
	name(Sec, N6),
	append(N3, N2, N1a),
	append(N1a, N1, N1b),
	append(N1b, "-", N1c),
	append(N1c, N4, N1d),
	append(N1d, N5, N1e),
	append(N1e, N6, File3),
	append(File2, File3, File4),
	append(File4,".txt",File5),
	name(File, File5),
	!.

print_stat(Gen, Time, best(Bfit, Bcount, Bexpr), worst(Wfit, Wcount), 
		avg(Avg), AvgD, _) :-
	lamarckian_P(0.0, _, _, _),
	!,
	writel([nl,nl, '---> Generation ', Gen, '(', Time, ')', nl,
	        'Average fitness:', Avg, nl,
	        'Best count: ', Bcount, nl,
	        'Best example:', Bexpr, nl,
	        'Best fitness = ', Bfit, nl,
	        'Worst count: ', Wcount, nl,
	        'Worst fitness = ', Wfit, nl,
	        'Average Depth:', AvgD, nl,
		'Lamarckian evolution: off ',nl,nl]),
	!.
print_stat(Gen, Time, best(Bfit, Bcount, Bexpr), worst(Wfit, Wcount), 
		avg(Avg), AvgD, lamarck(FitImpr,MaxImpr,NumGain)) :-
	lamarckian_P(Percent, _, _, _),
	population_size_P(_, PopSize),
	N is integer(Percent * PopSize),
	(NumGain > 0 -> AvgLam is FitImpr/NumGain ; AvgLam=0),
	writel([nl,nl, '---> Generation ', Gen, '(', Time, ')', nl,
	        'Average fitness:', Avg, nl,
	        'Best count: ', Bcount, nl,
	        'Best example:', Bexpr, nl,
	        'Best fitness = ', Bfit, nl,
	        'Worst count: ', Wcount, nl,
	        'Worst fitness = ', Wfit, nl,
	        'Average Depth:', AvgD, nl,
		'Lamarckian evolution: ', nl,
		'    ', NumGain, ' gains out of ', N,' tries', nl,
		'    Total gain:', FitImpr, nl,
		'    Max single gain:', MaxImpr, nl,
		'    Avg gain:', AvgLam, nl, nl]),
	!.

% print existing population

dump_population(Run) :-
	set_file_name("popn", Run, File),
	tell(File),
	individual(ID, V, Expr),
	write_individual(ID, V, Expr),
	fail.
dump_population(_) :-
	told, tell(user).

write_individual(ID, V, Expr) :-
	writel(['-----', nl,
		'Individual ', ID, ': fit=', V, nl, Expr, nl]),
	Expr^^construct(E), 
	sre_pp(E), nl,
	!.

% ------------------------------------
% solution dump: writes soln expression to a file, for input later.
% Grammatical expression is written in multiple lines, since the full
% expression is often larger than Prolog's builtin "write" can handle.

write_soln(Name, Run, E) :-
	set_file_name(Name, Run, File),
	tell(File),
	write('soln('),
	write_term(E),
	write(').'),
	nl,
	told,
	tell(user),
	!.

write_term(node(X,List,Y)) :-
	!,
	write('node('),
	write(X),
	write(',['),
	write_tlist(List),
	write('],'),
	write(Y),
	write(')').
write_term(X) :- write(X).

write_tlist([]) :- !.
write_tlist([X,Y|Z]) :-
	!,
	write_term(X),
	write(','),
	nl,
	write_tlist([Y|Z]).
write_tlist([X]) :-
	write_term(X).


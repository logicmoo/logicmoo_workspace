:- use_module(procps).

go :-
	abolish_all_tables,
	entry(Goal),
	memory_usage(RSS0),
	cputime(T0, Gc0),
	run(Goal),
	cputime(T1, Gc1),
	memory_usage(RSS1),
	T is T1 - T0,
	Gc is Gc1 - Gc0,
	RSS is RSS1 - RSS0,
	print_result(T, Gc, RSS).

run(Goal) :-
	getenv('PROFILE', Atom),
	atom_number(Atom, Times),
	!,
	profile(run(Times, Goal)),
	prolog.
run(Goal) :-
	once(Goal).

run(N, Goal) :-
	succ(N2, N),
	!,
	abolish_all_tables,
	once(Goal),
	run(N2, Goal).
run(_,_).

print_result(T, Gc, RSS) :-
	print_time(T, Gc),
	print_memory_usage(RSS).

:- dynamic
	base/3.

cputime(Msec, GCmsec) :-
	!,
	statistics(cputime, T),
	statistics(gctime, GC),
	Msec is round(1000*(T-GC)),
	GCmsec is round(1000*GC).
cputime(Msec, 0) :-
	statistics(runtime, [Msec,_]).

memory_usage(Bytes) :-
	fail,
	statistics(table_space_used, Bytes),
	!.
memory_usage(Bytes) :-
	garbage_collect,
	garbage_collect_atoms,
	procps_stat(Stat),
	Bytes = Stat.rss.

print_memory_usage(RSS0) :-
	RSS is round(RSS0/1024),
	(   getenv('CSV', yes)
	->  format(',~d~n', [RSS])
	;   getenv('CSV', CSVFile)
	->  setup_call_cleanup(
		open(CSVFile, append, Out),
		format(Out, ',~d~n', [RSS]),
		close(Out)),
	    human_memory_usage(RSS)
	;   human_memory_usage(RSS)
	).

human_memory_usage(RSS) :-
	base(_, _, RSS0),
	RSS0 > 0,
	Rel is 100*(RSS/RSS0),
	format(' ~`.t ~DKb TBL~70|~t(~0f%~6+)~n', [RSS, Rel]).
human_memory_usage(RSS) :-
	format(' ~`.t ~DKb TBL~70|~n', [RSS]).

print_time(T, Gc) :-
	current_test(Test),
	(   getenv('CSV', yes)
	->  format('~w,~d', [Test, T])
	;   getenv('CSV', CSVFile)
	->  setup_call_cleanup(
		open(CSVFile, append, Out),
		format(Out, '~w,~d', [Test, T]),
		close(Out)),
	    human_time(Test, T, Gc)
	;   human_time(Test, T, Gc)
	).

current_test(Test) :-
	source_file(entry(_), File),
	file_base_name(File, Base),
	atom_concat(Test, '-swi.pl', Base),
	!.
current_test(Test) :-
	source_file(go, File),
	file_base_name(File, Base),
	atom_concat(Test, '-hprolog.pl', Base).

human_time(Test, T, Gc) :-
	base_performance(Test, TB, RSS),
	assertz(base(Test, TB, RSS)),
	!,
	Rel is 100*(T/TB),
	format('~w ~`.t ~D+~35|~D ~tmsec~42|~t(~0f%~7+)', [Test, T, Gc, Rel]).
human_time(Test, T, Gc) :-
	format('~w ~`.t ~D+~D msec~45|', [Test, T, Gc]).

base_performance(Test, T, RSS) :-
	getenv('BASE', CSVFile),
	base_performance(CSVFile, Test, T, RSS).

base_performance(CSVFile, Test, T, RSS) :-
	csv_read_file(CSVFile, Rows, [strip(true)]),
	(   member(row(Test, T, RSS), Rows)
	->  true
	;   member(row(Test, T), Rows)
	->  RSS = 0
	).

verbose(T) :-
	getenv('VERBOSE', y), !,
	write(T).
verbose(_).

verboseln(T) :-
	getenv('VERBOSE', y), !,
	writeln(T).
verboseln(_).

clean :-
	abolish_all_tables,
	garbage_collect,
	garbage_collect_atoms.

print_version :-
	current_prolog_flag(version_git, V),
	writeln(V),
	halt.

total :-
	current_prolog_flag(argv, [CSV, Base]),
	total_time(CSV, T),
	total_time(Base, B),
	Perc is 100*T/B,
	format('~N~`-t~16|~nTotal: ~D msec (was ~D, ~0f%)~n', [T, B, Perc]),
	halt.

total_time(File, Total) :-
	csv_read_file(File, Rows, [strip(true)]),
	maplist(arg(2), Rows, Times),
	sumlist(Times, Total).

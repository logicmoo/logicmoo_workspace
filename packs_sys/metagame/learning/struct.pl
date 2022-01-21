%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% building all tables.

build_tables :- 
	new_empty_state(State), % for testing
	build_tables(State).

build_tables(State) :- 
	runtime(build_tables(State,_Tables)).

build_tables(State,T) :- 
	anal_table(T),
	promsq_matrix(T,PromSqMatrix),
	promotion_matrix(T,PromMatrix),
	prom_distance_matrix(T,PromDistMatrix),
	transition_matrix(T,TransMatrix),
	mobility_matrix(T,MobMatrix),
	eventual_matrix(T,EventualMatrix),
	distance_matrix(T,DistMatrix),
	distance_table(T,DistTable),
	active_advisor_table(T,Advisors),
	static_matrix(T,StatMatrix),
	compile_basic_tables,
	  tracing_anal_format(tables,"Building <active advisor> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_advisor_table(Advisors)),
	  tracing_anal_format(tables,"Building <promotion transition> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_promotion_matrix(PromMatrix)),
	  tracing_anal_format(tables,"Building <promotion distance> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_prom_distance_matrix(PromMatrix,PromDistMatrix)),
	  tracing_anal_format(tables,"Building <transition> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_transition_matrix(TransMatrix,State)),
	  tracing_anal_format(tables,"Building <mobility> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_mobility_matrix(TransMatrix,MobMatrix)),
	  tracing_anal_format(tables,"Building <distance> matrices ...~n",[]),
	  tracing_anal_timing(tables,
	build_distance_matrix(TransMatrix,DistMatrix)),
	  tracing_anal_format(tables,"Building <distance> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_distance_table(DistMatrix,DistTable)),
	  tracing_anal_format(tables,"Building <eventual mobility> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_eventual_matrix(DistMatrix,EventualMatrix)),
	  tracing_anal_format(tables,"Building <promotion square distance> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_promsq_matrix(DistTable,PromSqMatrix)),
	  tracing_anal_format(tables,"Building <independent piece value> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_static_matrix(StatMatrix,T)),
	  tracing_anal_format(tables,"Tables completed!~n",[]),
	save_tables(T).




clear_tables :- 
	retractall(advice_tables(_)).

save_tables(Tables) :- 
	clear_tables,
	assert(advice_tables(Tables)).

dump_tables(File) :- 
	with_output_file(File,write,
	   dump_tables).

dump_tables :- 
	listing(advice_tables/1).

load_tables :- 
	read(advice_tables(Tables)),
	save_tables(Tables). 

load_tables(File) :- 
	see(File),
	load_tables,
	seen.

find_advice_tables_if(T) :- 
	( var(T) -> 
	  find_advice_tables(T)
	; true
	).


find_advice_tables(T) :- 
	( current_predicate(advice_tables,advice_tables(_)) -> 
	  advice_tables(T)
        ; T=none
	).


% Stripped down version, when we just want the basic structure 
% without the real analysis. 

build_dummy_tables :- build_dummy_tables(_State,_Tables).

build_dummy_tables(State,Tables) :- 
	anal_table(T),
	active_advisor_table(T,Advisors),
	compile_basic_tables,
	  tracing_anal_format(tables,"Building <active advisor> tables ...~n",[]),
	  tracing_anal_timing(tables,
	build_advisor_table(Advisors)),
	save_tables(T).
	


%============================================================================
% ANAL_TABLE data structure
%============================================================================

% Contains a few extra slots for future development.
anal_table(N) :- functor(N,tables,15).

promsq_matrix(T,M) :- arg(1,T,M).
promotion_matrix(T,M) :- arg(2,T,M).
prom_distance_matrix(T,M) :- arg(3,T,M).
transition_matrix(T,M) :- arg(4,T,M).
mobility_matrix(T,M) :- arg(5,T,M).
eventual_matrix(T,M) :- arg(6,T,M).
distance_matrix(T,M) :- arg(7,T,M).
distance_table(T,M) :- arg(8,T,M).

piece_value_table(T,M) :- arg(9,T,M).
piece_square_table(T,M) :- arg(10,T,M).

active_advisor_table(T,M) :- arg(11,T,M).

capturing_table(T,M) :- arg(12,T,M).
moving_table(T,M) :- arg(13,T,M).
static_matrix(T,M) :- arg(14,T,M).



promsq_matrix(M) :- advice_tables(T), promsq_matrix(T,M).
promotion_matrix(M) :- advice_tables(T), promotion_matrix(T,M).
prom_distance_matrix(M) :- advice_tables(T), prom_distance_matrix(T,M).
transition_matrix(M) :- advice_tables(T), transition_matrix(T,M).
mobility_matrix(M) :- advice_tables(T), mobility_matrix(T,M).
eventual_matrix(M) :- advice_tables(T), eventual_matrix(T,M).
distance_matrix(M) :- advice_tables(T), distance_matrix(T,M).
distance_table(M) :- advice_tables(T), distance_table(T,M).

piece_value_table(M) :- advice_tables(T), piece_value_table(T,M).
piece_square_table(M) :- advice_tables(T), piece_square_table(T,M).
active_advisor_table(M) :- advice_tables(T), active_advisor_table(T,M). 
static_matrix(M) :- advice_tables(T), static_matrix(T,M).


add_portray_anal_table :- 
	anal_table(T),
	assert((portray(T) :- portray_anal_table(T))).

portray_anal_table(_T) :- format("<Anal Table>",[]).

%============================================================================
% ACTIVE_ADVISOR_TABLE data structure
%============================================================================

shutdown_advisor(Advisor,Table) :-
	active_advisor_table(Table,M),
	advisor_number(Advisor,Number),
	arg(Number,M,off).

active_advisor(Advisor,Table) :-
	active_advisor_table(Table,M),
	advisor_number(Advisor,Number),
	arg(Number,M,Status),
	Status \== off.

advisor_number(threat,1).
advisor_number(prom,2).
advisor_number(dynamic_mobility,3).

number_of_advisors(3).

build_advisor_table(T) :- 
	number_of_advisors(N),
	functor(T,active_advisors,N).


	
%================================================================================
% Interface
%================================================================================

build_top :- build_tables.

showstatic_top :- print_static_matrix.


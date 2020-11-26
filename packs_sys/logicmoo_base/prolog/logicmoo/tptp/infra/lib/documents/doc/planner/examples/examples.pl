%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Examples Descibed in planner_manual.html
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




test :-
	sussman(Plan), view_plan(Plan).




sussman(Plan) :-
	Options = [p(Plan), pxo, bd, o1, s1],
	GoalMultiset = [ on(a, b),
			 on(b, c) ],
	StartMultiset = [ on(b, table),
			  clear(b),
	                  on(a, table),
			  on(c, a),
			  clear(c) ],
	Constraints = [],
	Rulebase = [ rule( puton(Block, From, To),
		           [on(Block, To), clear(From), clear(Block)],
		           [on(Block, From), clear(Block), clear(To) ],
	                   [] ),
	             fact( clear(table), [] )
		   ],
	plan(Options, GoalMultiset, StartMultiset, Constraints, Rulebase).




shoes(Plan) :-
	Options = [p(Plan), pxo, bd, o1, s1],
	GoalMultiset = [ shoe_on(right), shoe_on(left) ],
	StartMultiset = [ bare(right), bare(left) ],
	Constraints = [],
	Rulebase = [ rule( shoe(RL),
			   [ shoe_on(RL) ],
			   [ sock_on(RL) ],
	                   [] ),
		     rule( sock(RL),
			   [ sock_on(RL) ],
			   [ bare(RL) ],
	                   [] ),
		     declare(fluent, bare(_))
		   ],
	plan(Options, GoalMultiset, StartMultiset, Constraints, Rulebase).




table(Plan, OutPool, OutConstraints) :-
	Options = [p(Plan), pool(OutPool), cs(OutConstraints),
		   pxo, bd, o1, s1],
	GoalMultiset = [ have(table) ],
	StartMultiset = [ budget(100) ],
	InputConstraints = [],
	Rulebase = [ rule( buy(Object, Price),
			   [ have(Object),
			     budget(NewBudget) ],
			   [ offered(Object, Price),
			     budget(OldBudget) ],
			   [ cs([NewBudget =:= OldBudget - Price]) ] ),
		     rule( assemble_table,
			   [ have(table) ],
			   [ have(table_board),
			     have(table_leg),
			     have(table_leg),
			     have(table_leg),
			     have(table_leg) ],
			   [] ),
		     fact( offered(table_board, 10), [] ),
		     fact( offered(table_leg, 5), [] )
		   ],
	plan(Options,
	     GoalMultiset,
	     StartMultiset,
	     InputConstraints,
	     Rulebase).

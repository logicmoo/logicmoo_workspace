
          
area_test1 :-
	get_advstate(S0),
	Action = goto_loc('player~1', walk, pantry),
	action_doer(Action,Agent),
	pprint(Agent,general),
	must_act(Action,S0,S2),
	pprint(S2,general),
        get_advstate(S2).


area_test1a :-
	get_advstate(S0),
	Action = go_dir('player~1', walk, north),
	action_doer(Action,Agent),
	pprint(Agent,general),
	must_act(Action,S0,S2),
	pprint(S2,general),
        get_advstate(S2).

area_test2 :-
	e2c('I need a windows computer for work.').

area_test3(Words0) :-
	get_advstate(S0),
	undeclare(memories(Agent, Mem0), S0, S1),
	pprint(Mem0,general),
	set_advstate(S1),
	figure_out_action(Words0, Agent, Mem0, Mem2),
	declare(memories(Agent, Mem2), S1, _S).

figure_out_action(Words0, Agent, Mem0, Mem1) :-
	(   Words0==[]->(Words=[wait],makep);Words=Words0),
	eng2log(Agent, Words, Action, Mem0),
	add_todo(Agent, Action, Mem0, Mem1),
	pprint(Mem1,general).

area_test4 :-
	Agent = 'floyd',
	api_invoke(add_todo(Agent, take(Agent,crate))).

area_test5 :-
	Agent = 'floyd',
	api_invoke(add_todo(Agent, goto_loc(Agent, walk, pantry))).

area_test6 :-	
	add_agent_todo('player~1',goto_loc('player~1',walk,basement)),
        get_advstate(S2),
	view(S2).




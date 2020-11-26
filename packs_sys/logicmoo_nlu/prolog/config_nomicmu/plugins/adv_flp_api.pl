:- use_module(library(nomic_mu)).
:- use_module(library(pengines)).

:- dynamic command_from_flp/1.
:- dynamic flp_words0/1.

% %% % FLP client
% %% decide_action(Agent, Mem0, Mem1) :-
% %%  notrace(declared(inherited(flp), Mem0)),!,
% %%  must_mw1(flp_decide_action(Agent, Mem0, Mem1)).

% %% receive_message(Message,Result) :-
% %% 	pprint(Message,general),
% %% 	retractall(command_from_flp(_)),
% %% 	assert(command_from_flp(Message)),
% %% 	Result = true,
% %% 	% %% query_agent(flp,'127.0.0.1',flp_update_current_task(0,0,Message,Result1),Result2).
% %% 	get_goals(Goals),
% %% 	query_agent(flp,'127.0.0.1',flp_update(0,0,[current(task,Message),current(goals,Goals)],Result1),Result2).

:- defn_state_none(update_flp(agent,term,term,term)).

update_flp(Agent,Name,Plan,Step) :-
	Agent = 'player~1',
	view([query_agent(flp,'127.0.0.1',flp_update([current(name,Name),current(plan,Plan),current(step,Step)],Result1),Result2)]),
	(   query_agent(flp,'127.0.0.1',flp_update([current(name,Name),current(plan,Plan),current(step,Step)],Result1),Result2) -> true ; true).

:- defn_state_none(flp_words()).
flp_words :-
	flp_words0(Words),
	view([words,Words]).

flp_nomicmu_query(Input,Result) :-
     with_output_to(string(Result),
       (convert_input_to_words(Input,Words0),
	set_flp_words0(Words0),
	Agent = 'player~1',
	advstate_db(S0),
	undeclare(memories(Agent, Mem0), S0, S1),
	set_advstate(S1),
	flp_decide_action(Agent, Mem0, Mem2),
	declare(memories(Agent, Mem2), S1, S),
	retractall(advstate_db(_)),
	asserta(advstate_db(S)),
	once(main_once))).
	    
convert_input_to_words(Input,Result) :-
	atom_string(Input,String),
	string_codes(String,Codes),
	line_to_tokens(Codes,-1,Result).

set_flp_words0(Words0) :-
	retractall(flp_words0(_)),
	assert(flp_words0(Words0)).

% %% flp_words0([goto,downstairs_computer_room]).

flp_decide_action(Agent,Mem0,Mem1) :-
	flp_words0(Words),
	retractall(flp_words0(_)),
	view([words,Words]),
	eng2log(Agent, Words, Action, Mem0),
	view([action,Action]),
        add_todo(Agent, Action, Mem0, Mem1).

% Decide action hook
% :- push_to_state(type_props(adv_flp,[inherit(nomic_plugin),prefix('flp_'), desc("FLP plugin that contains decide action")]).

:- push_to_state(
 props(adv_flp,
  [prefix = "flp_",inherit(decider_plugin), 
   desc("FLP plugin that contains decide action")])).

% :- decl_mpred(flp_decide_action/3, nomicmu_decider_hook).

flp_state :-
	get_advstate(State),pprint(State,always).

flp_state_eng :-
	get_advstate(State),print_english(world,State).

:- defn_state_none(goals()).
goals :-
	get_goals(Goals),
	pprint(Goals,general).

get_goals(Goals) :-
	get_advstate(S0),
	member(memories('player~1',M),S0),
	member(todo(Goals),M).
                  
% TODO write this
%aXiom(view(Item)) ==>>
%   queue_local_event(msg_from(Agent, Msg), [Here]).

view(Item) :-
	pprint(Item, always).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_none(flp_test()).

flp_test :-
	e2c('I need a windows computer for work.').



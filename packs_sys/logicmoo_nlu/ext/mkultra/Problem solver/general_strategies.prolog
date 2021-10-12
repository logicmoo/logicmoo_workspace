%%%
%%% STRATEGIES FOR STANDARD OPERATIONS
%%%

%% default_strategy(+Task, -Strategy) is nondet
%  Provides default strategies to use when Task has no specific matches.
strategy(resolve_match_failure(X), S) :-
   default_strategy(X, S).

%%%
%%% Precondition and postcondition handling
%%%

%%
%% achieve(P)
%% Task to make P become true.
%%

strategy(achieve(P),
	 Task) :-
   postcondition(Task, P).

strategy(achieve(Condition),
	 null) :-
   % Don't have to do anything if condition is already true.
   Condition,
   !.

strategy(achieve(runnable(Action)),
	 achieve(Blocker)) :-
   blocking(Action, Blocker),
   \+ unachievable(Blocker).

%% unachievable(+Task)
%  Task is a-priori unachievable, so give up.
unachievable(exists(_)).

strategy(achieve(P),
	 wait_condition(P)) :-
   self_achieving(P).

%%
%% Precondition chaining
%%

strategy(achieve_precondition(_, P),
	 S) :-
   postcondition(S, P).

default_strategy(achieve_precondition(_SubTask, P),
		 abort_and_then(explain_failure(~P))).

normalize_task(abort_and_then(Task),
	       begin(call(perform_restart_retractions($task)),
		     invoke_continuation(Task))).

%%
%% MOVEMENT AND LOCOMOTION
%% achieving locations
%% moving
%% docking
%% goto
%%

strategy(achieve(location(X,$me)),
	 pickup(X)).
strategy(achieve(location(X, Room)),
	 achieve(location(X, Container))) :-
   %\+ freestanding(X),
   is_a(Room, room),
   is_a(Container, work_surface),
   location(Container, Room).
strategy(achieve(location(X, Container)),
	 putdown(X, Container)) :-
   Container \= $me,
   \+ is_a(Container, room).

strategy(move($me, X,Y),
	 achieve(location(X, Y))).

strategy(achieve(docked_with(WorldObject)),
	 goto(WorldObject)).

%%
%% goto
%%
:- external know/1.
precondition(goto(Object),
	     know(X:location(Object, X))).

strategy(goto(Building),
	 null) :-
   is_a(Building, building).
strategy(goto(Room),
	 unless(contained_in($me, Room),
		goto_internal(Room))) :-
   room(Room).
strategy(goto(PropOrCharacter),
	 unless(docked_with(Place),
		goto_internal(Place))) :-
   once(( prop(PropOrCharacter)
	  ;
	  character(PropOrCharacter))),
   top_level_container(PropOrCharacter, Place).

strategy(goto_internal(Place),
	 begin(assert($task/location_bids/Place:Priority),
	       wait_event(arrived_at(Place)),
	       retract($task/location_bids/Place))) :-
   $task/priority:Priority.

after(goto_internal(Person),
      greet($me, Person)) :-
   character(Person).

%%
%% Transfer of posession
%%

normalize_task(bring($me, Recipient, Object),
	       move($me, Object, Recipient)).
normalize_task(give($me, Recipient, Object),
	       move($me, Object, Recipient)).
task_interacts_with_objects(bring(_, A, B), [A, B]).
task_interacts_with_objects(give(_, A, B), [A, B]).

guard_condition(Task, location(Object, _Loc)) :-
   task_interacts_with_objects(Task, Objects),
   member(Object, Objects).

%%
%% Spatial search
%%

normalize_task(search_for($me, Unspecified, Target),
	       search_for($me, CurrentRoom, Target)) :-
   var(Unspecified),
   in_room($me, CurrentRoom).

strategy(search_for($me, Container, Target),
	 search_object(Container, X^(X=Target),
		       X^handle_discovery(X),
		       mental_monologue(["Couldn't find it."]))) :-
   nonvar(Target).
strategy(search_for($me, Container, Target),
	 search_object(Container, X^previously_hidden(X),
		       X^handle_discovery(X),
		       mental_monologue(["Nothing seems to be hidden."]))) :-
   var(Target).

strategy(handle_discovery(X),
	 mental_monologue(["Found", np(X)])).
after(handle_discovery(X),
      pickup(X)) :-
   is_a(X, key_item).

before(search_object(Object, _, _, _),
       goto(Object)):-
   \+ contained_in($me, Object).

strategy(search_object(ArchitecturalSpace, CriterionLambda, SuccessLambda, FailTask),
	 if(nearest_unsearched(ArchitecturalSpace, Object),
	    % Search nearest item
	    search_object(Object, CriterionLambda, SuccessLambda,
			  % Try next item, if any
			  search_object(ArchitecturalSpace,
					CriterionLambda, SuccessLambda,
					FailTask)),
	    % Searched entire contents
	    begin(assert(/searched/ArchitecturalSpace),
		  FailTask))) :-
   is_a(ArchitecturalSpace, architectural_space).

strategy(search_object(Container, CriterionLambda, SuccessLambda, FailTask),
	 if(nearest_unsearched(Container, Object),
	    % Search nearest item
	    search_object(Object,
			  CriterionLambda, SuccessLambda,
			  % Try next item, if any
			  search_object(Container,
					CriterionLambda, SuccessLambda, FailTask)),
	    % Searched entire contents
	    begin(assert(/searched/Container),
		  FailTask))) :-
   is_a(Container, container),
   \+ is_a(Container, architectural_space),
   % Reveal a hidden item, if there is one.
   ignore(reveal_hidden_item(Container)).

default_strategy(search_object(Object, CriterionLambda, SuccessLambda, FailTask),
		 if(( reduce(CriterionLambda, Object, Criterion),
		      Criterion ),
		    begin(assert(/searched/Object),
			  let(reduce(SuccessLambda, Object, SuccessTask),
			      SuccessTask)),
		    begin(sleep(0.75),
			  assert(/searched/Object),
			  FailTask))).

:- public nearest_unsearched/2, unsearched/2.
nearest_unsearched(Container, Contents) :-
   nearest(Contents,
	   unsearched(Container, Contents)).

unsearched(Container, Contents) :-
   location(Contents, Container),
   \+ character(Contents), % Don't search characters
   \+ /searched/Contents.

:- public reveal_hidden_item/1.

reveal_hidden_item(Container) :-
   hidden_contents(Container, Item),
   reveal(Item),
   assert($task/previously_hidden_items/Item),
   % Don't wait for update loop to update Item's position.
   assert(/perception/location/Item:Container),
   !.

:- public previously_hidden/1.
previously_hidden(Item) :-
   $task/previously_hidden_items/Item.

%%
%% Ingestion (eating and drinking)
%%

strategy(eat($me, X),
	 ingest(X)).
postcondition(eat(_, X),
	      ~exists(X)).
postcondition(eat(Person, F),
	      ~hungry(Person)) :-
   existing(food, F).

strategy(drink($me, X),
	 ingest(X)).
postcondition(drink(_, X),
	      ~exists(X)).
postcondition(drink(Person, B),
	      ~thirsty(Person)) :-
   existing(beverage, B).

self_achieving(/perception/nobody_speaking).

%%
%% Social interaction
%%

strategy(engage_in_conversation(Person),
	 S) :-
   in_conversation_with(Person) ->
      S = null
      ;
      S = ( goto(Person),
	    greet($me, Person) ).

%%
%% OTHER
%% Sleeping
%%

strategy(sleep(Seconds),
	 wait_condition(after_time(Time))) :-
   Time is $now + Seconds.

ready_to_hand(Object) :-
   location(Object, $me).
ready_to_hand(Object) :-
   docked_with(Object).

strategy(achieve_precondition(_, ready_to_hand(Object)),
	 goto(Object)).

precondition(examine($me, Object),
	     ready_to_hand(Object)).
strategy(examine($me, Object),
	 if(examination_content(Object, Content),
	    call(pop_up_examination_content(Content)),
	    describe(Object, general, null))).

precondition(read($me, Object),
	     ready_to_hand(Object)).
strategy(read($me, Object),
	 if(examination_content(Object, Content),
	    call(pop_up_examination_content(Content)),
	    say_string("It's blank."))).

%%
%% Tracking who you're doing something for
%%

normalize_task(on_behalf_of(Person, Task),
	       begin(assert($task/on_behalf_of:Person),
		     Task)).
retract_on_restart(Task, Task/on_behalf_of).

%%%
%%% Parallel processing
%%% We just have a simplistic fork/join system.
%%%

default_strategy(spawn(Task),
		 call(spawn_child_task(Task))).

:- public spawn_child_task/1.
spawn_child_task(Task) :-
   begin($task/priority:Priority,
	 start_task($task, Task, Priority)).

default_strategy(wait_for_children,
		 wait_condition(\+ Me/concerns/_)) :-
   % Need to manually bind a variable here to $task because the code that polls
   % this doesn't run "inside" the task, i.e. doesn't bind $task.
   Me = $task.

default_strategy(cobegin(T1),
		 begin(spawn(T1),
		       wait_for_children)).

default_strategy(cobegin(T1, T2, T3),
		 begin(spawn(T1),
		       spawn(T2),
		       spawn(T3),
		       wait_for_children)).

default_strategy(cobegin(T1, T2, T3, T4),
		 begin(spawn(T1),
		       spawn(T2),
		       spawn(T3),
		       spawn(T4),
		       wait_for_children)).

default_strategy(cobegin(T1, T2, T3, T4, T5),
		 begin(spawn(T1),
		       spawn(T2),
		       spawn(T3),
		       spawn(T4),
		       spawn(T5),
		       wait_for_children)).

default_strategy(cobegin(T1, T2, T3, T4, T5, T6),
		 begin(spawn(T1),
		       spawn(T2),
		       spawn(T3),
		       spawn(T4),
		       spawn(T5),
		       spawn(T6),
		       wait_for_children)).

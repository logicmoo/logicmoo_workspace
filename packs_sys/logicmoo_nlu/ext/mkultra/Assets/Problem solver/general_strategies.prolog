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
   strategy(achieve(P), S).

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

strategy(achieve(location(X, $me)),
	 pickup(X)) :-
   X \= $me.
strategy(achieve(location(X, Room)),
	 achieve(location(X, Container))) :-
   %\+ freestanding(X),
   is_a(Room, room),
   is_a(Container, work_surface),
   location(Container, Room).
default_strategy(achieve(location(X, Container)),
	 putdown(X, Container)) :-
   X \= $me,
   Container \= $me,
   \+ is_a(Container, room).

strategy(achieve(location($me, Container)),
	 begin(goto(Container),
	       get_in(Container))) :-
   is_a(Container, prop).

precondition(move($me, Patient, _),
	     know(X:location(Patient, X))).
strategy(move($me, X,Y),
	 achieve(location(X, Y))).

strategy(achieve(docked_with(WorldObject)),
	 goto(WorldObject)).

%%
%% locomotion
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
	  door(PropOrCharacter)
	;
	  character(PropOrCharacter))),
   top_level_container(PropOrCharacter, Place).

strategy(goto_internal(Place),
	 let(spawn_child_task(wait_event(arrived_at(Place)),
			      Child, [ Child/location_bids/Place:Priority ]),
	     wait_for_child(Child))) :-
   $task/priority:Priority.

after(goto_internal(Person),
      greet($me, Person)) :-
   character(Person).

strategy(leave($me, Building),
	 goto(Exit)) :-
   is_a(Building, building),
   property_value(Building, exit, Exit).

strategy(flee($me),
	 leave($me, Building)) :-
   % Leave whatever building I'm in.
   contained_in($me, Building),
   is_a(Building, building).

%%
%% Getting things
%%

strategy(get($me, Object),
	 move($me, Object, $me)).

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

strategy(achieve(know(X:location(Object, X))),
	 begin(search_for($me, _, Object),
	       unless(know(X:location(Object, X)),
		      failed_because(cant_find(Object))))).

normalize_task(search_for($me, Unspecified, Target),
	       search_for($me, CurrentRoom, Target)) :-
   var(Unspecified),
   in_room($me, CurrentRoom).

strategy(search_for($me, Container, Target),
	 search_object(Container, X^(X=Target),
		       X^handle_discovery(X),
		       mental_monolog(["Couldn't find it."]))) :-
   nonvar(Target).
strategy(search_for($me, Container, Target),
	 search_object(Container, X^previously_hidden(X),
		       X^handle_discovery(X),
		       mental_monolog(["Nothing seems to be hidden."]))) :-
   var(Target).

strategy(handle_discovery(X),
	 begin(emote(surprise),
	       mental_monolog(["Found", np(X)]))).
after(handle_discovery(X),
      pickup(X)) :-
   is_a(X, key_item).

before(search_object(Object, _, _, _),
       goto(Object)):-
   \+ contained_in($me, Object).

strategy(search_object(ArchitecturalSpace, CriterionLambda, SuccessLambda, FailTask),
	 {
	  assert($task/status_text/"[search]":1),
	  if(nearest_unsearched(ArchitecturalSpace, Object),
	     % Search nearest item
	     search_object(Object, CriterionLambda, SuccessLambda,
			   % Try next item, if any
			   search_object(ArchitecturalSpace,
					 CriterionLambda, SuccessLambda,
					 FailTask)),
	     % Searched entire contents
	     begin(tell(/searched/ArchitecturalSpace),
		   FailTask))
	 }) :-
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
	    begin(tell(/searched/Container),
		  FailTask))) :-
   is_a(Container, container),
   \+ is_a(Container, architectural_space),
   % Reveal a hidden item, if there is one.
   ignore(reveal_hidden_item(Container)).

default_strategy(search_object(Object, CriterionLambda, SuccessLambda, FailTask),
		 if(( reduce(CriterionLambda, Object, Criterion),
		      Criterion ),
		    begin(tell(/searched/Object),
			  let(reduce(SuccessLambda, Object, SuccessTask),
			      SuccessTask)),
		    begin(pause(0.75),
			  tell(/searched/Object),
			  FailTask))).

:- public nearest_unsearched/2, unsearched/2.
nearest_unsearched(Container, Contents) :-
   nearest(Contents,
	   unsearched(Container, Contents)).

unsearched(Container, Contents) :-
   location(Contents, Container),
   \+ implausible_search_location(Contents),
   \+ /searched/Contents.

implausible_search_location(X) :-
   is_a(X, exit).
implausible_search_location(X) :-
   character(X).

:- public reveal_hidden_item/1.

reveal_hidden_item(Container) :-
   hidden_contents(Container, Item),
   reveal(Item),
   tell($task/previously_hidden_items/Item),
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
%% Sleeping
%%

precondition(sleep($me, OnWhat),
	     location($me, OnWhat)).
strategy(sleep($me, _OnWhat),
	 with_status_text("zzz":2,
			  pause(60))).

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
%% Pausing
%%

strategy(pause(Seconds),
	 wait_condition(after_time(Time))) :-
   Time is $now + Seconds.

ready_to_hand(Object) :-
   location(Object, $me).
ready_to_hand(Object) :-
   docked_with(Object).

strategy(achieve_precondition(_, ready_to_hand(Object)),
	 goto(Object)).

:- external examined/1.

tell_globally(examined(_)).

precondition(examine($me, Object),
	     ready_to_hand(Object)).
strategy(examine($me, Object),
	 begin(if(examination_content(Object, Content),
		  call(pop_up_examination_content(Content)),
		  describe(Object, general, null)),
	       tell(examined(Object)))).
after(examine($me, Object),
      call(maybe_remember_event(examine($me, Object)))).

precondition(read($me, Object),
	     ready_to_hand(Object)).
strategy(read($me, Object),
	 if(examination_content(Object, Content),
	    call(pop_up_examination_content(Content)),
	    say_string("It's blank."))).

strategy(force_examine(Object),
	 if(examination_content(Object, Content),
	    call(pop_up_examination_content(Content)),
	    call(log(no_examination_content(Object))))).

%%
%% Pressing buttons
%%

precondition(press($me, Button),
	     ready_to_hand(Button)).
default_strategy(press($me, _Button),
		 say_string("Nothing happened...")).

%%
%% Turning things on/off
%%

precondition(turn_on($me, X),
	     ready_to_hand(X)).
default_strategy(turn_on($me, X),
		 call(activate_prop(X))).

precondition(turn_off($me, X),
	     ready_to_hand(X)).
default_strategy(turn_off($me, X),
		 call(deactivate_prop(X))).

%%
%% Misc mechanical operations
%%

strategy(flush($me, Toilet),
	 call(flush(Toilet))) :-
   is_a(Toilet, toilet).

:- public flush/1.
flush(Toilet) :-
   forall(contained_in(X, Toilet),
	  destroy(X)).


%%
%% Tracking who you're doing something for
%%

normalize_task(on_behalf_of(Person, Task),
	       begin(assert($task/on_behalf_of:Person),
		     Task)).
retract_on_restart(Task, Task/on_behalf_of).

%%
%% Ending and pausing the game
%%

normalize_task(pause_game,
	       call(pause_game)).

strategy(end_game,
	 show_status(game_over)).


%%%
%%% Parallel processing
%%% We just have a simplistic fork/join system.
%%%

normalize_task(spawn(Task),
	       call(spawn_child_task(Task))).
normalize_task(spawn(Task, Child, Assertions),
	       call(spawn_child_task(Task, Child, Assertions))).

:- public spawn_child_task/1, spawn_child_task/3.
spawn_child_task(Task) :-
   begin($task/priority:Priority,
	 start_task($task, Task, Priority)).
spawn_child_task(Task, Child, Assertions) :-
   begin($task/priority:Priority,
	 start_task($task, Task, Priority, Child, Assertions)).

normalize_task(with_status_text(String:Priority, Task),
	       let(spawn_child_task(Task, Child, [ Child/status_text:String:Priority ]),
		   wait_for_child(Child))).

normalize_task(with_child_task(Task, Child, Assertions, Continuation),
	       let(spawn_child_task(Task, Child, Assertions),
		   Continuation)).

normalize_task(with_child_task(Task, Child, Continuation),
	       let(spawn_child_task(Task, Child, []),
		   Continuation)).

normalize_task(wait_for_child(Child),
	       wait_condition(child_completed(UID, Me))) :-
   Me = $task,
   concern_uid(Child, UID).

:- public child_completed/2.

child_completed(UID, Me) :-
   Me/concerns/UID, !, fail.
child_completed(UID, Me) :-
   /failures/UID:_,
   % Sneakily rewrite our continuation so we'll fail.
   assert(Me/continuation:fail_because(child_failed(UID))).
child_completed(_, _).

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

%%%
%%% Beginnings of an exception/cognizant-failure system
%%%


%% Kill the current task and log the reason for its failure.
strategy(failed_because(Reason),
	 begin(assert(/failures/UID:StrippedTask:Reason),
	       done)) :-
   $task/type:task:Task,
   strip_task_wrappers(Task, StrippedTask),
   concern_uid($task, UID).

%% strip_task_wrappers(+Task, -Stripped)
%  Stripped is the core task of Task, with any unimportant
%  wrappers removed, like on_behalf_of.
strip_task_wrappers(on_behalf_of(_, Task), Stripped) :-
   !,
   strip_task_wrappers(Task, Stripped).
strip_task_wrappers(Task, Task).
%%
%% Logging of dialog
%%

:- public log_dialog_act/1.
:- external recent_dialog/1, transcript/1.
log_dialog_act(DA) :-
   asserta($global::recent_dialog(DA)),
   assertz($global::transcript(DA)).

:- public transcript/0, recent_dialog/2.
% Generates a listing of all dialog
transcript :-
   forall(transcript(DA),
	  writeln(DA)).

fkey_command(alt-t, "Display transcript") :-
   generate_unsorted_overlay("Transcript",
			     ( transcript(DA),
			       agent(DA, Agent),
			       (property_value(Agent, given_name, Name) ->
				   true
			           ;
				   Name=Agent),
			       once(generate_text(DA, Text)),
			       (Agent = $pc ->
				   (Color = lime)
			           ;
				   (Agent = player ->
				        (Color=white)
				        ;
				        (Color = yellow))) ),
			     color(Color, line(Name, ":\t", Text))).
		   
fkey_command(control-alt-t, "Display transcript as dialog acts") :-
   generate_unsorted_overlay("Dialog acts",
			     ( transcript(DA),
			       agent(DA, Agent),
			       (property_value(Agent, given_name, Name) ->
				   true
			           ;
				   Name=Agent),
			       (Agent = $pc ->
				   (Color = lime)
			           ;
				   (Color = yellow)) ),
			     color(Color, line(Name, ":\t", DA))).
		   

%% recent_dialog(+Speaker, -DA) is nondet
%  Generates all the recent dialog by Speaker in order of decreasing recency.
recent_dialog(Speaker, DA) :-
   recent_dialog(DA),
   agent(DA, Speaker).
   

%%%
%%% Discourse generation strategies for problem solver
%%%

%%
%% Monologs
%% Just big chunks of fixed text.
%%

normalize_task(monolog([ ]),
	       null).
normalize_task(monolog([String | MoreMonolog]),
	       ( say_string(String), monolog(MoreMonolog) )).
normalize_task(monolog(String),
	       say_string(String)) :-
   string(String).

%%
%% Enumerating lists
%% To use this, call the task generate_list(List, GenerationInfo), where the
%% list is what you want to generate, and GenerationInfo is whatever information
%% is needed to keep track of how to generate items.  Then supply strategies
%% for your generator for:
%%   generate_empty(GenerationInfo)                   The list is empty
%%   generate_singleton(Item, GenerationInfo)         The list has exactly one item
%%   generate_first/next/last(Item, GenerationInfo)   Generate an item
%%

strategy(generate_list([ ], GenerationInfo),
	 generate_empty(GenerationInfo)).
strategy(generate_list([X], GenerationInfo),
	 generate_singleton(X, GenerationInfo)).
strategy(generate_list([H | T], GenerationInfo),
	 begin(generate_first(H, GenerationInfo),
	       generate_rest(T, GenerationInfo))) :-
   T \= [ ].

strategy(generate_rest([H | T], GenerationInfo),
	 begin(generate_next(H, GenerationInfo),
	       generate_rest(T, GenerationInfo))) :-
   T \= [ ].
strategy(generate_rest([X], GenerationInfo),
	 generate_last(X, GenerationInfo)).

default_strategy(generate_empty(_),
		 null).
default_strategy(generate_singleton(Item, GenerationInfo),
		 generate_next(Item, GenerationInfo)).
default_strategy(generate_first(Item, GenerationInfo),
		 generate_next(Item, GenerationInfo)).
default_strategy(generate_last(Item, GenerationInfo),
		 generate_next(Item, GenerationInfo)).


%%
%% Saying lists of objects
%% This should get converted to use generate_list/2 at some point.
%%

strategy(say_list([ ], _, _),
	 say_string("Nothing")).
strategy(say_list([X], _, SurfaceLambda),
	 speech([Surface])) :-
   reduce(SurfaceLambda, X, Surface).
strategy(say_list([H | T], Termination, SurfaceLambda),
	 begin(say_first(H, SurfaceLambda),
	       say_rest(T, Termination, SurfaceLambda))) :-
   T \= [ ].

strategy(say_rest([H | T], Termination, SurfaceLambda),
	 begin(say_next(H),
	       say_rest(T, Termination, SurfaceLambda))) :-
   T \= [ ].
strategy(say_rest([X], Termination, SurfaceLambda),
	 say_last(X, Termination, SurfaceLambda)).

strategy(say_first(Object, SurfaceLambda),
	 speech([ Surface, "," ])) :-
   reduce(SurfaceLambda, Object, Surface).
strategy(say_next(Object),
	 speech([ np(Object), "," ])).
strategy(say_last(Object, Termination, _SurfaceLambda),
	 speech([ Termination, np(Object), "."])).
strategy(say_object(Object),
	 speech([ np(Object) ])).
strategy(say_string(String),
	 speech([ String ])).
strategy(say(Assertion),
	 speech([ s(Assertion) ])).
strategy(say_answer(Assertion),
	 speech([ question_answer(Assertion) ])).

strategy(speech(Items),
	 discourse_increment($me, $addressee, Items)).
strategy(mental_monologue(Items),
	 discourse_increment($me, $me, Items)).

% When tasks other than conversations try to generate speech, this is where
% it will be directed.
default_addressee(Partner) :-
   in_conversation_with(Partner),
   !.
default_addressee($me).

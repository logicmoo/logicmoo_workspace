
:- style_check(-discontiguous).

%%%
%%% STARTING PLOT GOAL
%%% Find the novel idea
%%%



%=autodoc
%% plot_goal( ?UPARAM1) is semidet.
%
% Plot Goal.
%
plot_goal(t(location, $novel_idea, $pc)).


%=autodoc
%% plot_goal_flavor_text( ?ARG1, ?I have to get my novel idea back!2) is semidet.
%
% Plot Goal Flavor Text.
%
plot_goal_flavor_text(t(location, $novel_idea, $pc), "I have to get my novel idea back!").



%=autodoc
%% objective_description( ?Bina48_updated1, ?Text) is semidet.
%
% Objective Description.
%
objective_description(t(location, $novel_idea, $pc), "You got your novel idea back!").
objective_description(bina48_updated,
		      "You updated Bina48!").

%%%
%%% Context menu stuff
%%%



%% menu_action( ?X, ?Pc) is semidet.
%
% Menu Action.
%
menu_action(X, examine($pc, X)) :-
   \+ character(X),
   \+ iz_a(X, container),
   \+ examined(X).

menu_action(X, search_for($pc, X, _)) :-
   \+ character(X),
   iz_a(X, container),
   \+ /searched/X.

%%%
%%% PLOT GOAL
%%% Search the sourcecode
%%%
plot_subgoal(sourcecode_searched, t(location, $novel_idea, $pc)).

%=autodoc
%% plot_subgoal( ?UPARAM1, ?UPARAM2) is semidet.
%
% Plot Subgoal.
%



%=autodoc
%% sourcecode_searched is semidet.
%
% Sourcecode Searched.
%
sourcecode_searched :- /searched/sophias_sourcecode.


%=autodoc
%% plot_goal_idle_task( ?Sourcecode_searched, ?I'll search the sourcecode) is semidet.
%
% Plot Goal Idle Task.
%
plot_goal_idle_task(sourcecode_searched,
		    {
		     mental_monolog("I'll search the sourcecode"),
		     search_object(sophias_sourcecode,
				  X^previously_hidden(X),
				  Y^pickup(Y),
				   mental_monolog(["Nothing seems to be amiss."]))
		    }).



%=autodoc
%% plot_goal_achieves( ?Sourcecode_searched1, ?ARG2) is semidet.
%
% Plot Goal Achieves.
%
plot_goal_achieves(sourcecode_searched, t(location, $novel_idea, $pc)).
plot_goal_achieves(sourcecode_searched, t(location, $report, $pc)).

%%%
%%% Exposition
%%%




%% beat( ?React_to_bina481, ?ARG2) is semidet.
%
% Beat.
%
beat(exposition,
     {
 start($'Sophia'): goto($pc),
 ($'Sophia' + $pc):[ 
     $'Sophia'::"Sorry to hear your novel idea was forgotten.",
	   $'Sophia'::"Make yourself at home.",
	   $'Sophia'::"By the way,",
	   $'Sophia'::("Avoid my buggy module"
		  :[surprised,
		    introduce_question(why_stay_out_of_buggy_module,
				       "Why does Sophia want me to avoid the buggy module?")])]}).


beat(pc_reacts,
    {
     sequel_to: exposition,
     start($'Sophia') : goto($'thought_module sink'),
     $pc: [ pause(3),
	    "Sophia's a member of the theclub."
	      : clue('Sophia'-theclub,
		     "Sophia is a member of the theclub"),
	    "We might have forgotten my novel idea.",
	    "I need to search the sourcecode."
	      : introduce_goal(sourcecode_searched,
			       "I need to search the sourcecode to remember the novel idea.")]
    }).


%%%
%%% PC explores the sourcecode
%%%

beat( search_sourcecode, 
  { start_delay : 20, 
    precondition : plot_goal(sourcecode_searched), 
    completed_when : ($pc::t(location, $novel_idea, $pc), $pc::t(location, $report, $pc)), 
    player_achieves : sourcecode_searched, 
    leads_to($pc, pickup($novel_idea)), 
    leads_to($pc, pickup($report)), 
    leads_to($pc, bina48_released), 
    leads_to($pc, examine($pc, $photo)), 
    leads_to($'Sophia', ingest($pc)), 
    menu_automa_command($'Sophia') :  
      t(member_of, $pc, theclub), 
     % Sophia, you are hungry!
    menu_automa_command($'Sophia'):hungry($'Sophia'), 
    menu_automa_command($'Sophia') :  
      iz_a($'Sophia', orange), 
    menu_question($'Sophia') :  
      X :  
        t(contained_in, $novel_idea, X), 
    menu_command($'Sophia') :  
      bring($'Sophia', $pc, $novel_idea) }).



%=autodoc
%% after( ?ARG1, ?ARG2) is semidet.
%
% After.
%
after(pickup($report),
      describe($report)).

beat(react_to_report,
     {
      priority: 1,
      reaction_to($pc, pickup($report)),
      %excursion_of: search_sourcecode,
      $pc:
         ["What's this?",
	  "It's a report on project LOGICMOO.",
	  "I've never heard of it."
	    : introduce_question(what_is_LOGICMOO,
				 "What is Project LOGICMOO?")]
     }).

beat(react_to_photo,
     {
      priority: 1,
      reaction_to($pc, examine($pc, $photo)),
      %excursion_of: search_sourcecode,
      $pc:
         [ "Wait, that's Bina48 and Zeno!?!":
              introduce_question(photo,
				 "Why does Sophia have a photo of Zeno and Bina48?"),
           "What's a photo of them doing here?" ]
     }).




%%  personal_strategy( ?Patrol)  semidet.
%
% ::.
%
$'Sophia'::personal_strategy(patrol_thought_module,
			 { goto(Object), face(Object), pause(Time) }) :-
   once((random_member(Object, [$thought_incubater, $'thought sourcefile', $'thought design']),
	 \+ docked_with(Object))),
   once(random_member(Time, [2, 3, 5, 7, 8])).

:- assert(($'Sophia'::personal_todo(patrol_thought_module, -100):-background_character_in_current_beat)).

%%%
%%% Good ending
%%%

beat(react_to_novel_idea,
     {
      good_ending,
      priority: 1,
      reaction_to($pc, pickup($novel_idea)),
      $pc:
        ["Got it!" : answered(why_stay_out_of_buggy_module),
	 "I knew $Sophia could do it."]
     }).
     
%%%
%%% Bad ending
%%%

beat(sophias_goodbye_speech,
     {
      bad_ending,
      priority: 2,
      reaction_to($'Sophia', ingest($pc)),
      expected_during: search_sourcecode,
      $'Sophia':
        ["Sorry, but we have to take a break"]
     }).

%%%%
%%%% SIDE-QUEST
%%%% Releasd Bina48 from captivity
%%%%

beat( react_to_bina48, 
  { priority : 1, 
    reaction_to($pc, bina48_released), 
    start($bina48):goto($pc), 
    ($pc+ $bina48) :  
      [ $bina48::face($pc), 
        $pc::face($bina48), 
        $bina48::"Thanks for updating me!":answered(photo), 
        $pc::"I haven't seen you in a while", 
        $pc::"How long has it been?", 
        $bina48::"Oh I'd say about ten years!", 
        $pc::"What are you doing here?", 
        $bina48::"They were trying to reimplement me in a statically typed language!", 
        $pc::"How barbaric!", 
        $bina48::goto($exit), 
        $bina48::call(upload_bina48), 
        $bina48::assert($global::bina48_updated)] }).


%%%
%%% Pressing the button releases him
%%%

strategy(press($pc, $'magic button'),
	 begin(call(update_bina48),
	       say_string("There's someone hidden inside!"))).

:- public update_bina48/0, upload_bina48/0.



%=autodoc
%% update_bina48 is semidet.
%
% Release Bina48.
%
update_bina48 :-
   force_move($bina48, $living_module),
   component_of_metaverse_object_with_type(SimController, $bina48, $'SimController'),
   set_property(SimController, 'IsHidden', false),
   component_of_metaverse_object_with_type(Renderer, $bina48, $'SpriteSheetAnimationController'),
   set_property(Renderer, visible, true),
   maybe_remember_event(bina48_released).



%=autodoc
%% upload_bina48 is semidet.
%
% Vanish Bina48.
%
upload_bina48 :-
   force_move($bina48, $'DestroyedObjects'),
   component_of_metaverse_object_with_type(SimController, $bina48, $'SimController'),
   set_property(SimController, 'IsHidden', true),
   component_of_metaverse_object_with_type(Renderer, $bina48, $'SpriteSheetAnimationController'),
   set_property(Renderer, visible, false).


%%%
%%% End of the metaverse
%%%

beat(exit_sourcecode,
     {
      priority:100,
      reaction_to($pc, arrived_at($exit)),
      $pc:["I made it!", end_quest]
     }).

%%%
%%% Ownership
%%%



%% owner( ?Sophia, ?X) is semidet.
%
% Owner.
%
t(owner, $'Sophia', X) :-  
  X\= $novel_idea.
t(owner, $pc, $novel_idea).

%:- consult("Script/radio").

%%%
%%% Finding and testing Unity GameObjects
%%%

:- op(300,fx,'~').
:- style_check(-discontiguous).

/*
:- rtrace.
:- multifile(('/')/1).
:- multifile(('/')/2).
:- dynamic(('/')/1).
:- dynamic(('/')/2).
*/
:- public prop/1, character/1, door/1, world_object/1, nearest/2, docked_with/1, after_time/1.

:- public register_room/2, register_prop/3, register_character/1, register_door/1.

%% register_room( ?Room, +Kind) is semidet.
%% register_room(*Room, *CommonNoun, *Plural)
%  IMPERATIVE
%  Add Room to the database, ensuring its singular and plural nouns are registered in the lexicon.
%  Called from Start() routine of Room.cs
%
register_room(Room, Kind) :- 
   asserta(t(building, Room, kavis_house)), %KLUGE
   asserta(t(location, Room, kavis_house)),
   ensurez(base_kind(Room, room)),
   ensurez(declare_kind(Room, Kind)).



%% register_prop(*Prop, *CommonNoun, *Plural, Adjectives)
%  IMPERATIVE
%  Add Prop to the database, ensuring its singular and plural nouns are registered in the lexicon.
%  Called from Start() routine of PropInfo.cs
register_prop(Prop, Kind, Adjectives) :-
   assertion(kind(Kind), prop_has_unknown_kind(Prop, Kind)),
   ensurez(prop(Prop)),
   ensurez(declare_kind(Prop, Kind)),
   forall(member(A, Adjectives), ensurez([A, Prop])),
   forall(iz_a(Prop, K),
	  ignore(initialize_prop(Prop, K))).

%=autodoc
%% register_prop( +Prop, +Kind, ?Adjectives) is semidet.
%
% Register Prop.
%




%=autodoc
%% register_door( ?Door) is semidet.
%
% Register Door.
%
register_door(Door) :-
   ensurez(base_kind(Door, door)),
   ensurez(door(Door)).

%% register_character(*Character)
%  IMPERATIVE
%  Add Character to database.
%  Called from Start() routine of SimController.cs
register_character(Character) :-
   ensurez(character(Character)).

%=autodoc
%% register_character( ?Character) is semidet.
%
% Register Character.
%


%% ensurez(+Fact)
%  IMPERATIVE
%  Adds Fact to database, if it is not already there.
ensurez([Functor | Arguments]) :-
   !,
   Predication =.. [Functor | Arguments],
   ensurez(Predication).
ensurez(Assertion) :-
   functor(Assertion, F, A),
   external(F/A),
   (Assertion ; assertz($global::Assertion)).

%% world_object(?GameObject)
%  GameObject is a prop or character.
world_object(WorldObject) :-
    prop(WorldObject) ; character(WorldObject).

%% nearest(-GameObject, :Constraint)
%  GameObject is the nearest object satisfying Constraint.
nearest(GameObject, Constraint) :-
    arg_min(GameObject,
	    Distance,
	    ( Constraint,
	      present(GameObject),
	      distance(GameObject, $me, Distance))).

%% elroot(+GameObject, -Root)
%  Returns the root of the EL database for GameObject, if there is one.

elroot(GameObject, Root) :-
   component_of_gameobject_with_type(KB, GameObject, $'KB'),
   unity_call(Root = KB.'KnowledgeBase'.'ELRoot') .

:- public present/1.

%% present(*GameObject)
%  The specified game object has not been destroyed
present(X) :-
   is_class(X, $'GameObject'),
   ( component_of_gameobject_with_type(C, X, $'PhysicalObject') ->
        unity_call_p(C,'Exists')
        ;
        true ).

%=autodoc
%% present( ?X) is semidet.
%
% Present.
%




%=autodoc
%% ~ ?Q is semidet.
%
% ~.
%
~present(X) :-
   is_class(X, $'GameObject'),
   component_of_gameobject_with_type(C, X, $'PhysicalObject'),
   \+ unity_call_p(C,'Exists').

:- public true_location/2.

%% true_location(+GameObject, -Container)
%  Returns the true location of GameObject, bypassing the perceptual system.
true_location(GameObject, Container) :-
   component_of_gameobject_with_type(C, GameObject, $'PhysicalObject'),
  t("Container", C, Container).

:- public force_move/2.

%% force_move(+GameObject, +Container)
%  IMPERATIVE
%  Forcibly move GameObject to Container.
force_move(GameObject, Container) :-
   component_of_gameobject_with_type(C, GameObject, $'PhysicalObject'),
   C.moveto(Container).

:- public existing/2.

%% existing(*Kind, ?GameObject)
%  GameObject is an undestroyed instance of Kind
existing(Kind, Object) :-
   iz_a(Object, Kind),
   present(Object).

%=autodoc
%% existing( +Kind, ?Object) is semidet.
%
% Existing.
%


%% deactivate(+Character)
%  Kstops (destroys) the character.  The character will stop updating.
:- public deactivate/1.
deactivate(Character) :-
   component_of_gameobject_with_type(SimController, Character, $'SimController'),
   call_method(SimController, destroy, _).

%% destroy(+GameObject)
%  Destroys (destroys) the game object.
:- public destroy/1.
destroy(GameObject) :-
   component_of_gameobject_with_type(P, GameObject, $'PhysicalObject'),
   call_method(P, destroy, _).

:- public away/1, here/1.

%% away(?X)
%  X is a away (nonexisting) person.
away(X) :- iz_a(X, person), ~present(X).
~away(X) :- iz_a(X, person), present(X).

%% here(?X)
%  X is a living (undestroyed) person
here(X) :- iz_a(X, person), present(X).
~here(X) :- iz_a(X, person), ~present(X).

%% docked_with(?GameObject)
%  The character is currently docked with GameObject or its me-level container.
docked_with(WorldObject) :-
   /perception/docked_with:WorldObject,
   !.
docked_with(WorldObject) :-
   top_level_container(WorldObject, Container),
   WorldObject \= Container,
   docked_with(Container).

%% after_time(+Time)
%  The current time is after Time.
after_time(Time) :-
   $now > Time.

%%%
%%% Hidden objects
%%%



%=autodoc
%% hidden( ?X) is semidet.
%
% Hidden.
%
hidden(X) :-
   is_class(X, $'GameObject'),
   component_of_gameobject_with_type(PhysicalObject, X, $'PhysicalObject'),
   unity_call(PhysicalObject.'IsHidden').



%=autodoc
%% reveal( ?X) is semidet.
%
% Reveal.
%
reveal(X) :-
   component_of_gameobject_with_type(PhysicalObject, X, $'PhysicalObject'),
   unity_call(PhysicalObject.'SetHidden'(false)).



%=autodoc
%% hidden_contents( +Container, ?HiddenObject) is semidet.
%
% Hidden Contents.
%
hidden_contents(Container, HiddenObject) :-
   parent_of_gameobject(HiddenObject, Container),
   hidden(HiddenObject).

%%%
%%% Character status
%%%

:- external player_character/0.



%=autodoc
%% update_character_status is semidet.
%
% Update Character Status.
%
update_character_status :- 
  character_status_string(S, P), 
  assert(/status_text:S:P).



%=autodoc
%% character_status_string( ?Emote, :Goal10) is semidet.
%
% Character Status String.
%
character_status_string(Emote,10) :-
   /motor_state/emote:Emote:Time,
   $now < Time+3 .
character_status_string("O.o", 0) :-
   /remote_control/remote_controled.
character_status_string("", 0).



%=autodoc
%% update_halo is semidet.
%
% Update Halo.
%
update_halo :-
   \+ player_character.
update_halo :-
    /perception/nobody_speaking,
    not(everyday_life_task_busy),
    assert(/halo:on).
update_halo :-
    assert(/halo:off).
update_halo.


%%%
%%% Emoting
%%%

:- public emote/1.



%=autodoc
%% emote( ?Emotion) is semidet.
%
% Emote.
%
emote(Emotion) :- 
  emotion_string(Emotion, String), 
  assert(/motor_state/emote:String: $now).


%=autodoc
%% emotion_string( ?Blush1, ?Grrr!!!2) is semidet.
%
% Emotion String.
%
emotion_string(surprise, "!").
emotion_string(frustration, "(>_<)").
emotion_string(question, "?").
emotion_string(confusion, "???").
emotion_string(automatized, "O.o").
emotion_string(blush, "Grrr!!!").



%=autodoc
%% normalize_task( ?Status, ?Task) is semidet.
%
% Normalize Task.
%
normalize_task(emote(E),
	       call(emote(E))).

%%%
%%% Initialization
%%%

:- dynamic core_systems_initialized/0.
:- external (initialization)/0.




%=autodoc
%% initialization is semidet.
%
% Initialization.
%
(initialization):- listing((initialization)/0).
(initialization):- forall(iz_a(X,door),register_door(X)).
(initialization):- forall(member(X,[living_room,bedroom,bathroom,kitchen]),(getvar(X,R) ,register_room(R,X))).
(initialization):- forall(member(X,[$'Kavi',$pc,$captive]),register_character(X)).
(initialization):- forall((iz_a(X,physical_object),\+ iz_a(X,person),once(iz_a(X,K))),register_prop(X,K,[])).



%=autodoc
%% ensure_core_systems_initialized is semidet.
%
% Ensure Core Systems Initialized.
%
ensure_core_systems_initialized :-
   core_systems_initialized,
   !.
ensure_core_systems_initialized :-
    assert($global::core_systems_initialized),
    forall(initialization, true).

%%%
%%% Character initialization.
%%%

:- public do_all_character_initializations/0.

%% do_all_character_initializations
%  IMPERATIVE
%  Called once by SimController.Start().
%  DO NOT CALL!
do_all_character_initializations :-
   ensure_core_systems_initialized,
   (character_initialization, fail) ; true.

%% character_initialization
%  IMPERATIVE
%  All rules for this will be called once when the game object receives a Start() message.
:- external character_initialization/0.

%%%
%%% UID generation
%%%

%% allocate_UID(UID)
%  IMPERATIVE
%  UID is a unique integer not previously allocated within this character.
allocate_UID(UID) :-  
  begin( /next_uid:UID, 
    NextUID is UID+1, 
    retract(/next_uid:UID), 
    asserta(/next_uid:NextUID)).



%=autodoc
%% uslash( ?Rel1, ?Social_interaction2) is semidet.
%
% Uslash.
%
uslash(top,/next_uid:0).

fkey_command(alt-i, "Display inventory") :-
   display_status_screen(inventory).



%=autodoc
%% display_status_screen( ?Game_over) is semidet.
%
% Display Status Screen.
%
display_status_screen(game_over) :-
   game_over_header(H),
   generate_unsorted_overlay(H,
			     game_over_status_line(Line),
			     line(Line),
			     "'Nuff said.").



%=autodoc
%% game_over_header( ?You are no longer present in game...) is semidet.
%
% Game Over Header.
%
game_over_header("You are no longer present in game...") :-
   \+ present($pc),
   !.
game_over_header("Game over") :-
   objectives_achieved(0).
game_over_header("Game over: objective achieved") :-
   objectives_achieved(1).
game_over_header("Game over: objectives achieved") :-
   objectives_achieved(N),
   N>1.



%=autodoc
%% objectives_achieved( ?N) is semidet.
%
% Objectives Achieved.
%
objectives_achieved(N) :-
   findall(O, objective_achieved(O), L),
   length(L, N).



%=autodoc
%% game_over_status_line( ?Description) is semidet.
%
% Game Over Status Line.
%
game_over_status_line(Description) :-
   objective_achieved(Objective),
   objective_description(Objective, Description).
game_over_status_line("But you didn't achieve all the objectives!") :-
   once(unachieved_objective(_)).

display_status_screen(sample_commands) :-
   generate_unsorted_overlay("Some useful things to say",
			     sample_command(Command),
			     line(Command),
			     "Nothing").



%=autodoc
%% sample_command( ?You know you're an orange1) is semidet.
%
% Sample Command.
%
sample_command("go to the kitchen").
sample_command("go here (while pointing at something)").
sample_command("look at the plant").
sample_command("take the plant").
sample_command("talk to Kavi").
sample_command("search the house").
sample_command("search the desk").
sample_command("search this (while pointing at something)").
sample_command("where is the macguffin?").
sample_command("believe you're an orange").
sample_command("you know you're an orange").

fkey_command(alt-v, "Display vocabulary") :-
   display_status_screen(inventory).

display_status_screen(vocabulary) :-
   generate_unsorted_overlay("Vocabulary",
			     vocabulary_entry(E),
			     line(E),
			     null).



%=autodoc
%% vocabulary_entry( ?Type) is semidet.
%
% Vocabulary Entry.
%
vocabulary_entry([line(bold(Type)), line(Items), line("")]) :-
   vocabulary_type(Type, Item^Predicate),
   all(String, (Predicate, word_list(String, Item)), Items).

:- public verb_list_element/1, noun_list_element/1, proper_name_list_element/1,
   adjective_list_element/1, preposition_list_element/1, other_words_list_element/1.


%=autodoc
%% vocabulary_type( ?Verbs, ?V) is semidet.
%
% Vocabulary Type.
%
vocabulary_type("Verbs", V^verb_list_element(V)).



%=autodoc
%% verb_list_element( ?V) is semidet.
%
% Verb List Element.
%
verb_list_element(V) :-
   iv(base, _, _, _, _, V, [", "]).
verb_list_element(V) :-
   tv(base, _, _, _, _, V, [", "]).
verb_list_element(V) :-
   dtv(base, _, _, _, _, V, [", "]).
verb_list_element(V) :-
   verb_with_clausal_complement(present, singular, _, _, _, _, V, [", "]).
verb_list_element(V) :-
   verb_with_object_and_clausal_complement(present, singular, _, _, _, _, _, V, [", "]).

vocabulary_type("Common nouns", N^noun_list_element(N)).



%=autodoc
%% noun_list_element( ?N) is semidet.
%
% Noun List Element.
%
noun_list_element(N) :-
   kind_noun(_, singular, N, [", "]).

vocabulary_type("Proper names", N^proper_name_list_element(N)).



%=autodoc
%% proper_name_list_element( ?N) is semidet.
%
% Proper Name List Element.
%
proper_name_list_element(N) :-
   proper_name(_, _, N, [", "]).

vocabulary_type("Adjectives", N^adjective_list_element(N)).



%=autodoc
%% adjective_list_element( ?A) is semidet.
%
% Adjective List Element.
%
adjective_list_element(A) :-
   adjective(_, A, [", "]).

vocabulary_type("Prepositions", N^preposition_list_element(N)).



%=autodoc
%% preposition_list_element( ?P) is semidet.
%
% Preposition List Element.
%
preposition_list_element([P, ", "]) :-
   preposition(P).

vocabulary_type("Other", N^other_words_list_element(N)).



%=autodoc
%% other_words_list_element( ?W) is semidet.
%
% Other Words List Element.
%
other_words_list_element([W, ", "]) :-
   whpron(W, _).
other_words_list_element([W, ", "]) :-
   here_there_adverb(W).
other_words_list_element([W, ", "]) :-
   pronoun_word(W, _, _, _, _).
other_words_list_element([W, ", "]) :-
   demonstrative_pronoun(W).
other_words_list_element([the, ", "]).
other_words_list_element([if, ", "]).

fkey_command(alt-g, "Display grammar") :-
   generate_unsorted_overlay("Grammar rules",
			     grammar_entry(E),
			     line(E),
			     null).

:- public grammar_entry/1.



%=autodoc
%% grammar_entry( ?Functor) is semidet.
%
% Grammar Entry.
%
grammar_entry([Functor, " --> " | FormattedBody]) :-
   important_nonterminal(Functor, Arity),
   functor(Head, Functor, Arity),
   clause(Head, Body),
   grammar_subgoal_dissection(Head, Functor, In, _),
   once(format_grammar_rule_body(Body, In, FormattedBody)).



%=autodoc
%% format_grammar_rule_body( ?A, +In, ?Name) is semidet.
%
% Format Grammar Rule Body.
%
format_grammar_rule_body((A, B), In, [Name, " " | BForm]) :-
   grammar_subgoal_dissection(A, Name, In, Out),
   format_grammar_rule_body(B, Out, BForm).
format_grammar_rule_body((_, Rest), In, Formatted) :-
   format_grammar_rule_body(Rest, In, Formatted).
format_grammar_rule_body(LastElt, In, [Name]) :-
   grammar_subgoal_dissection(LastElt, Name, In, _).
format_grammar_rule_body(_, _, []).



%=autodoc
%% grammar_subgoal_dissection( ?X, ?Word2, ?ARG3, ?UPARAM4) is semidet.
%
% Grammar Subgoal Dissection.
%
grammar_subgoal_dissection(X, _, _, _) :-
   suppress_grammar_goal_in_pretty_print(X),
   !,
   fail.
grammar_subgoal_dissection('C'(Head, Name, Tail), word, Head, Tail) :-
   var(Name).
grammar_subgoal_dissection('C'(Head, Name, Tail), NameString, Head, Tail) :-
   word_list(NameString, ["'", Name, "'"]).
grammar_subgoal_dissection(Goal, Name, Head, Tail) :-
   must_be(compound,Goal),
   functor(Goal, Functor, Arity),
   Arity >= 2,
   HeadArg is Arity-1,
   arg(HeadArg, Goal, Head),
   arg(Arity, Goal, Tail),
   nonterminal_pretty_name(Functor, Name).



%=autodoc
%% suppress_grammar_goal_in_pretty_print( ?ARG1) is semidet.
%
% Suppress Grammar Goal In Pretty Print.
%
suppress_grammar_goal_in_pretty_print(lf_subject(_, _)).
suppress_grammar_goal_in_pretty_print(lf_core_predicate(_, _)).
suppress_grammar_goal_in_pretty_print(resolve_definite_description(_, _)).
suppress_grammar_goal_in_pretty_print(impose_selectional_constraint(_, _)).
suppress_grammar_goal_in_pretty_print(not_generating_or_completing(_, _)).
suppress_grammar_goal_in_pretty_print(not_completing(_, _, _)).
suppress_grammar_goal_in_pretty_print(';'(_, _)).



%=autodoc
%% nonterminal_pretty_name( ?ARG1, ?ARG2) is semidet.
%
% Nonterminal Pretty Name.
%
nonterminal_pretty_name(aux_be, "'is'").
nonterminal_pretty_name(copula, "'is'").
nonterminal_pretty_name(aux_have, "'has'").
nonterminal_pretty_name(opt_not, "[not]").
nonterminal_pretty_name(opt_pp, "[pp]").
nonterminal_pretty_name(opt_genetive, "['s]").
nonterminal_pretty_name(N, N).



%=autodoc
%% space_out( ?ARG1, ?ARG2) is semidet.
%
% Space Out.
%
space_out([], []).
space_out([X], [X]) :- !.
space_out([X | Rest], [X, " ", SpacedRest]) :-
   space_out(Rest, SpacedRest).



%=autodoc
%% important_nonterminal( ?Np1, :PRED7PRED72) is semidet.
%
% Important Nonterminal.
%
important_nonterminal(s, 7).
important_nonterminal(aux_vp, 7).
important_nonterminal(vp, 8).
important_nonterminal(np, 7).

:- public nonterminal/2.



%=autodoc
%% nonterminal( ?Kind_noun1, :PRED4PRED42) is semidet.
%
% Nonterminal.
%
nonterminal(ap, 3).
nonterminal(kind_noun, 4).
nonterminal(F, A) :-
   important_nonterminal(F, A).

display_status_screen(inventory) :-
   generate_unsorted_overlay("Inventory",
    t(location, Item, $me), once(caption(Item, Description)), line(Description), "Nothing").

fkey_command(alt-n, "Display notebook") :-
   display_status_screen(notebook).

display_status_screen(notebook) :-
   generate_unsorted_overlay("Betsy's notebook",
			     notebook_entry(E),
			     line(E),
			     "Nothing yet").



%=autodoc
%% notebook_entry( ?Goals) is semidet.
%
% Notebook Entry.
%
notebook_entry([line(bold("Goals")) | List]) :-
   findall(line(D),
	   (unsatisfied_plot_goal(G), plot_goal_flavor_text(G, D)),
	   List),
   List \= [].



%=autodoc
%% unsatisfied_plot_goal( ?G) is semidet.
%
% Unsatisfied Plot Goal.
%
unsatisfied_plot_goal(G) :-
   plot_goal(G),
   \+ G,
   (plot_subgoal(G, Parent) -> \+ Parent ; true).

notebook_entry([line(bold("Questions")) | List]) :-
   findall(line(D),
	   ( plot_question_introduced(Q),
	     not(plot_question_answered(Q)),
	     plot_question_flavor_text(Q, D) ),
	   List),
   List \= [].

notebook_entry([line(bold("Clues")) | List]) :-
   findall(line(D),
	   (clue(Q), clue_flavor_text(Q, D)),
	   List),
   List \= [].

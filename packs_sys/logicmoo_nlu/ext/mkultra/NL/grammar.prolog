% To do:
% Vocative: "TOM, ..."

:- indexical anaphore_context=[ ].

:- randomizable utterance//1, stock_phrase//1.

utterance(DialogAct) --> stock_phrase(DialogAct).
utterance(question(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, interrogative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(assertion(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(question_answer(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(assertion(Speaker, Addressee, not(LF), T, A)) -->
   sentence(LF, indicative, negative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(command(Speaker, Addressee, LF)) -->
   sentence(LF, imperative, affirmative, _, _),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(injunction(Speaker, Addressee, LF)) -->
   sentence(LF, imperative, negative, _, _),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(agree(Speaker, Addressee, _LF)) -->
   [ yes ],
   { current_dialog_pair(Speaker, Addressee) }.
utterance(disagree(Speaker, Addressee, _LF)) -->
   [ no ],
   { current_dialog_pair(Speaker, Addressee) }.
utterance(hypno_command(Speaker, Addressee, LF, T, A)) -->
   [ fnord ],
   s(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.


current_dialog_pair($speaker, $addressee).

%
% Stock phrases
%

stock_phrase(do_not_understand($speaker, $addressee, _)) --> [ huh, '?'].
stock_phrase(prompt_player($me, $me)) --> [type, something].

:-register_lexical_items([huh, type, something]).

stock_phrase(greet($speaker, Who)) -->
   [Salutation, ','],
   { member(Salutation, ['Hey', 'Hello', 'Hi']) },
   proper_name(Who, singular).
stock_phrase(greet($speaker, $addressee)) --> ['Hi', there].

:- register_lexical_items(['Hey', 'Hellow', 'Hi']).

stock_phrase(apology($speaker, $addressee)) --> ['Sorry'].
stock_phrase(excuse_self($speaker, $addressee)) --> ['Excuse', me].

:- register_lexical_items(['Sorry', 'Excuse']).

stock_phrase(parting($speaker, $addressee)) --> [X], { member(X, [bye, byebye, goodbye]) }.
stock_phrase(parting($speaker, $addressee)) --> [see, you].
stock_phrase(parting($speaker, $addressee)) --> [be, seeing, you].

stock_phrase(command($speaker, $addressee, end_game($addressee, $addressee))) --> [ end, game ].

:- register_lexical_items([end, game]).

%
% Help queries from the player
%

stock_phrase(general_help(player, $me)) -->
   [help].

:- register_lexical_item(help).

stock_phrase(general_help(player, $me)) -->
   [what, do, 'I', do, '?'].
stock_phrase(how_do_i(player, $me, Q)) -->
   [how, do, Us],
   { member(Us, ['I', you, we]) },
   player_question(Q),
   ['?'].

stock_phrase(objective_query(player, $me)) -->
   [what, am, 'I', trying, to, do, '?'].
stock_phrase(objective_query(player, $me)) -->
   [what, are, Us],
   { member(Us, [we, you]) },
   [trying, to, do, '?'].

stock_phrase(color_query(player, $me, red)) -->
   [what, does, red, text, mean, '?'].
stock_phrase(color_query(player, $me, red)) -->
   [why, does, my, text, turn, red, '?'].

:- register_lexical_items([red, green, yellow, white, turn, mean]).

stock_phrase(color_query(player, $me, yellow)) -->
   [what, does, yellow, text, mean, '?'].
stock_phrase(color_query(player, $me, yellow)) -->
   [why, does, my, text, turn, yellow, '?'].

stock_phrase(color_query(player, $me, green)) -->
   [what, does, green, text, mean, '?'].
stock_phrase(color_query(player, $me, green)) -->
   [why, does, my, text, turn, green, '?'].

stock_phrase(color_query(player, $me, white)) -->
   [what, does, white, text, mean, '?'].
stock_phrase(color_query(player, $me, white)) -->
   [why, does, my, text, turn, white, '?'].

%
% Increments produced by the discourse generator
%

utterance(discourse_increment(Speaker, Addressee, Fragments)) -->
   { generating_nl,               % Only valid for character output, not player input.
     bind(speaker, Speaker),
     bind(addressee, Addressee) },
   discourse_fragments(Fragments).

discourse_fragments([]) -->
   [ ].
discourse_fragments([F | Fs]) -->
   discourse_fragment(F),
   discourse_fragments(Fs).

discourse_fragment(question_answer(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).
discourse_fragment(s(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).

discourse_fragment(np(X)) -->
   {kind(X), !}, [a, X].

discourse_fragment(np(X)) -->
   {!}, np((X^S)^S, subject, third:singular, nogap, nogap).

discourse_fragment(X) -->
   { string(X), ! },
   [ X ].

%
% Interface to action and conversation systems
% This adds rules at load time for the different utterances
% They declare utterances to be actions (i.e. atomically executable)
% and to be events that conversations should respond to.
%

:- public register_utterance_types/0.

register_utterance_types :-
   forall(( clause(utterance(A, _, _), _),
	    nonvar(A) ),
	  assert_action_functor(A)),
   forall(clause(stock_phrase(A, _, _), _),
	  assert_action_functor(A)).

assert_action_functor(Structure) :-
   functor(Structure, Functor, Arity),
   ( action_functor(Functor, Arity) -> true
     ;
     ( assert(action_functor(Functor, Arity)),
       assert(precondition(Structure, /perception/nobody_speaking)),
       add_conversation_dispatch_clause(Structure) ) ).

add_conversation_dispatch_clause(Structure) :-
   functor(Structure, Functor, Arity),
   indexical_named(me, Me),
   EventArgs = [Partner, Me | _],
   length(EventArgs, Arity),
   Event =.. [Functor | EventArgs],
   assert( ( on_event(Event, conversation, C,
		      conversation_handler_task(C, respond_to_dialog_act(Event))) :-
	        C/partner/Partner
	   ) ).

:- register_utterance_types.

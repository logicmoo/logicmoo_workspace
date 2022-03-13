% To do:
% Vocative: "TOM, ..."

:- indexical anaphore_context=[ ].

:- randomizable utterance//1, stock_phrase//1.



%=autodoc
%% utterance( ?ARG1) is semidet.
%
% Utterance.
%
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
utterance(agree(Speaker, Addressee,true,_LF)) -->
   (means(yes);means(true);means(correct);means(agree)),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(agree(Speaker, Addressee,false, _LF)) -->
   (means(no);means(false);means(incorrect);means(disagree)),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(automa_command(Speaker, Addressee, LF, T, A)) -->  
  ( theTextM1(fnord)  ,
    s(LF, indicative, affirmative, T, A), 
    {current_dialog_pair(Speaker, Addressee)}).



%=autodoc
%% means( ?ARG1) is semidet.
%
% Means.
%
means(Yes)-->theTextM1(Yes).
means(Yes)-->theTextM1(Syn), {is_synonym(Yes, Syn)}.



%=autodoc
%% is_synonym( ?Yes1, ?Right2) is semidet.
%
% If Is A Synonym.
%
is_synonym(yes,right).

%%
%% INACTIONS AND OFFERS
%%
%% An offer is a suggestion that each party perform an action
%% A inaction is a an offer to not do an action, provided the reciprocal
%% action is taken.
%%

utterance(offer(Speaker, Addressee, SpeakerAct, AddresseeAct)) -->  
  ( s(AddresseeAct, imperative, affirmative, _, _)  ,
    theTextM1(and), 
    s(SpeakerAct, indicative, affirmative, future, simple), 
    {current_dialog_pair(Speaker, Addressee)}, 
    opt_stop(conditional)).
utterance(offer(Speaker, Addressee, SpeakerAct, AddresseeAct)) -->  
  ( theTextM1(if)  ,
    s(AddresseeAct, indicative, affirmative, _, _), 
    opt_comma, 
    s(SpeakerAct, indicative, affirmative, future, simple), 
    {current_dialog_pair(Speaker, Addressee)}, 
    opt_stop(conditional)).

utterance(inaction(Speaker, Addressee, SpeakerAct, AddresseeAct)) -->  
  ( s(AddresseeAct, imperative, affirmative, _, _)  ,
    theTextM1(or), 
    s(SpeakerAct, indicative, affirmative, future, simple), 
    {current_dialog_pair(Speaker, Addressee)}, 
    opt_stop(conditional)).

utterance(acceptance(Speaker, Addressee, _, _)) -->  
  theTextM1(okay), {current_dialog_pair(Speaker, Addressee)}, opt_stop(agreement).



%=autodoc
%% opt_comma is semidet.
%
% Opt Comma.
%
opt_comma --> [].
opt_comma-->theTextM1('.').


:- register_lexical_items([and, or, if, okay]).



%=autodoc
%% current_dialog_pair( ?ARG1, ?ARG2) is semidet.
%
% Current Dialog Pair.
%
current_dialog_pair($speaker, $addressee).

%
% Stock phrases
%



%=autodoc
%% stock_phrase( ?ARG1) is semidet.
%
% Stock Phrase.
%
stock_phrase(do_not_understand($speaker, $addressee, _)) -->  
  theTextM([huh, ?]).
stock_phrase(prompt_player($me, $me)) -->  
  theTextM([type, something]).

:-register_lexical_items([huh, type, something]).

stock_phrase(greet($speaker, Who)) -->
   [Salutation, ','],
   { member(Salutation, ['Hey', 'Hello', 'Hi']) },
   proper_name(Who, singular).
stock_phrase(greet($speaker, $addressee)) -->  
  theTextM(['Hi', there]).

:- register_lexical_items(['Hey', 'Hellow', 'Hi']).

stock_phrase(apology($speaker, $addressee)) -->  
  theTextM1('Sorry').
stock_phrase(excuse_self($speaker, $addressee)) -->  
  theTextM(['Excuse', me]).

:- register_lexical_items(['Sorry', 'Excuse']).

stock_phrase(parting($speaker, $addressee)) -->  
  theTextM1(X), {member(X, [bye, byebye, goodbye])}.
stock_phrase(parting($speaker, $addressee)) -->  
  theTextM([see, you]).
stock_phrase(parting($speaker, $addressee)) -->  
  theTextM([be, seeing, you]).

stock_phrase(command($speaker, $addressee, end_quest($addressee, $addressee))) -->  
  theTextM([end, metaverse]).

:- register_lexical_items([end, metaverse]).

%
% Help queries from the $user
%

stock_phrase(general_help($user, $me)) -->  
  theTextM1(help).

:- register_lexical_item(help).

stock_phrase(general_help($user, $me)) -->  
  theTextM([what, do, 'I', do, ?]).
stock_phrase(how_do_i($user, $me, Q)) -->  
  ( theTextM([how, do, Us])  ,
    {member(Us, ['I', you, we])}, 
    player_question(Q), 
    theTextM1(?)).

stock_phrase(objective_query($user, $me)) -->  
  theTextM([what, am, 'I', trying, to, do, ?]).
stock_phrase(objective_query($user, $me)) -->  
  ( theTextM([what, are, Us])  ,
    {member(Us, [we, you])}, 
    theTextM([trying, to, do, ?])).

:- register_lexical_items([red, green, yellow, white, turn, mean]).

stock_phrase(color_query($user, $me, Yellow)) -->  
  theTextM([what, does, Yellow, text, mean, ?]).
stock_phrase(color_query($user, $me, Yellow)) -->  
  theTextM([why, does, my, text, turn, Yellow, ?]).




%=autodoc
%% nsew( ?ATOM1) is semidet.
%
% Nsew.
%
nsew(north).
nsew(south).
nsew(east).
nsew(west).
nsew(n).
nsew(s).
nsew(e).
nsew(w).

:- forall(nsew(X), register_lexical_item(X)).

stock_phrase(if_navigation_command($user, $me, X)) -->
   %{ input_from_player },
   nsew_nav_command(X).



%=autodoc
%% nsew_nav_command( ?ARG1) is semidet.
%
% Nsew Nav Command.
%
nsew_nav_command(X)-->theTextM([go, X]), {nsew(X)}.

nsew_nav_command(X) -->  
  theTextM([go, to, the, X]), {nsew(X)}.

nsew_nav_command(X)-->theTextM([go, to, X]), {nsew(X)}.

nsew_nav_command(X)-->theTextM1(X), {nsew(X)}.

stock_phrase(show_status($user, $pc, What)) -->  
  theTextM1(check), status_display_term_with_determiner(What).

stock_phrase(show_status($user, $pc, What)) -->
   status_display_term(What).

stock_phrase(show_status($user, $pc, What)) -->  
  theTextM([show, me]), status_display_term_with_determiner(What).

stock_phrase(show_status($user, $pc, What)) -->  
  theTextM([look, at]), status_display_term_with_determiner(What).

stock_phrase(show_status($user, $pc, What)) -->  
  theTextM1(examine), status_display_term_with_determiner(What).

:- register_lexical_items([examine, look, at, show, check, inventory, notebook, vocabulary ]).

:- randomizable status_display_term/3.


%=autodoc
%% status_display_term( ?ARG1) is semidet.
%
% Status Display Term.
%
status_display_term(inventory)-->theTextM1(inventory).
status_display_term(notebook)-->theTextM1(notebook).
status_display_term(vocabulary)-->theTextM1(vocabulary).



%=autodoc
%% status_display_term_with_determiner( ?ARG1) is semidet.
%
% Status Display Term Using Determiner.
%
status_display_term_with_determiner(What)-->theTextM1(my), status_display_term(What).
status_display_term_with_determiner(What)-->theTextM1(the), status_display_term(What).

%
% Increments produced by the discourse generator
%

utterance(discourse_increment(Speaker, Addressee, Fragments)) -->
   { generating_nl },            % Only valid for character output, not $user input.
     with_bind(speaker, Speaker,
      with_bind(addressee, Addressee,
       discourse_fragments(Fragments))).



%=autodoc
%% discourse_fragments( ?ARG1) is semidet.
%
% Discourse Fragments.
%
discourse_fragments([]) -->
   [ ].
discourse_fragments([F | Fs]) -->
   discourse_fragment(F),
   discourse_fragments(Fs).



%=autodoc
%% discourse_fragment( ?ARG1) is semidet.
%
% Discourse Fragment.
%
discourse_fragment(question_answer(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).
discourse_fragment(s(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).
discourse_fragment(uninterpreted_s(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).

discourse_fragment(np(X))-->{kind(X), !}, theTextM([a, X]).

discourse_fragment(np(X)) -->
   {!}, np((X^S)^S, subject, third:singular, nogap, nogap).

discourse_fragment(String)-->{string(String), !}, theTextM1(String).
discourse_fragment(String:_Markup) -->  
  {string(String), !}, theTextM1(String).

%
% Interface to action and conversation systems
% This adds rules at load time for the different utterances
% They declare utterances to be actions (i.e. atomically executable)
% and to be events that conversations should respond to.
%

:- public register_utterance_types/0.



%=autodoc
%% register_utterance_types is semidet.
%
% Register Utterance  Types.
%
register_utterance_types :-
   forall(( clause(utterance(A, _, _), _),
	    nonvar(A) ),
	  assert_action_functor(A)),
   forall(clause(stock_phrase(A, _, _), _),
	  assert_action_functor(A)).



%=autodoc
%% assert_action_functor( ?Structure) is semidet.
%
% Assert Action Functor.
%
assert_action_functor(Structure) :-
   functor(Structure, Functor, Arity),
   ( action_functor(Functor, Arity) -> true
     ;
     ( assert_if_unew(action_functor(Functor, Arity)),
       assert_if_unew(precondition(Structure, /perception/nobody_speaking)),
       add_conversation_dispatch_clause(Structure) ) ).



%=autodoc
%% add_conversation_dispatch_clause( ?Structure) is semidet.
%
% Add Conversation Dispatch Clause.
%
add_conversation_dispatch_clause(Structure) :-
   functor(Structure, Functor, Arity),
   indexical_named(me, Me),
   EventArgs = [Partner, Me | _],
   length(EventArgs, Arity),
   Event =.. [Functor | EventArgs],
   assert_if_unew( ( on_event(Event, conversation, C,
		      conversation_handler_task(C, respond_to_dialog_act(Normalized))) :-
	       C/partner/Partner,
	       normalize_dialog_act(Event, Normalized)
	   ) ).

:- register_utterance_types.

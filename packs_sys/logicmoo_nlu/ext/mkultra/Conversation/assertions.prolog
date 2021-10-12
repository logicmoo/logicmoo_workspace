%%
%% Responding to assertions
%%

strategy(respond_to_dialog_act(assertion(Speaker,_, LF, Tense, Aspect)),
	 respond_to_assertion(Speaker, CM, Truth)) :-
   canonicalize_assertion(Speaker, LF, C),
   modalized(C, Tense, Aspect, Modalized),
   canonicalize_assertion(Speaker, Modalized, CM),
   admitted_truth_value(Speaker, CM, Truth).

canonicalize_assertion(Asserter, A, C) :-
   reduce_assertion(Asserter, A, Reduced),
   !,
   canonicalize_assertion(Asserter, Reduced, C).
canonicalize_assertion(_, A, A).

reduce_assertion(Asserter, believes(Asserter, P), P).
reduce_assertion(Asserter, thinks(Asserter, P), P).
reduce_assertion(Asserter, not(P), not(C)) :-
   canonicalize_assertion(Asserter, P, C).

default_strategy(respond_to_assertion(_Speaker, _ModalLF, true),
	 say_string("Yes, I know.")).
default_strategy(respond_to_assertion(_Speaker, _ModalLF, false),
	 say_string("I don't think so.")).
default_strategy(respond_to_assertion(Speaker, ModalLF, unknown),
		 begin(say_string(Response),
		       tell(/hearsay/Speaker/ModalLF))) :-
   heard_hearsay(ModalLF) -> Response="I've head that." ; Response="Really?".

heard_hearsay(ModalLF) :-
   /hearsay/_/Assertion, Assertion=ModalLF.

strategy(respond_to_dialog_act(question_answer(Speaker,_, LF)),
	 assert(/hearsay/Speaker/LF)).


%%
%% Kluges to keep characters from inappropriately contradicting other characters answers
%%

be(X,X).
be(Character, Name) :-
   property_value(Character, given_name, Name).

strategy(respond_to_assertion(Speaker, okay(Speaker), _),
	 null).



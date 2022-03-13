%%
%% Responding to assertions
%%

strategy(respond_to_dialog_act(assertion(Speaker,_, LF, Tense, Aspect)),
	 respond_to_assertion(Speaker, CM, Truth)) :-
   canonicalize_assertion(Speaker, LF, C),
   modalized(C, Tense, Aspect, Modalized),
   canonicalize_assertion(Speaker, Modalized, CM),
   admitted_truth_value(Speaker, CM, Truth).



%=autodoc
%% canonicalize_assertion( ?Asserter, ?A, ?C) is semidet.
%
% Canonicalize Assertion.
%
canonicalize_assertion(Asserter, A, C) :-
   reduce_assertion(Asserter, A, Reduced),
   !,
   canonicalize_assertion(Asserter, Reduced, C).
canonicalize_assertion(_, A, A).



%=autodoc
%% reduce_assertion( ?Asserter, ?Asserter, ?P) is semidet.
%
% Reduce Assertion.
%
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
   heard_hearsay(ModalLF) -> Response="I do not know." ; Response="Really?".



%=autodoc
%% heard_hearsay( ?ModalLF) is semidet.
%
% Heard Hearsay.
%
heard_hearsay(ModalLF) :-
   /hearsay/_/Assertion, Assertion=ModalLF.

strategy( respond_to_dialog_act(question_answer(Speaker, _, LF)), 
  assert(/hearsay/Speaker/LF)).


%%
%% Kluges to keep characters from inappropriately contradicting other characters answers
%%



%=autodoc
%% be( ?User1, ?X) is semidet.
%
% Be.
%
be(X,X).
be(Character, Name) :-
    t(given_name, Character, Name).

strategy(respond_to_assertion(Speaker, okay(Speaker), _),
	 null).



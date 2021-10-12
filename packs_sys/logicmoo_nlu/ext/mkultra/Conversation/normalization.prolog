normalize_dialog_act(Act, Normalized) :-
   da_normal_form(Act, Reduced),
   !,
   normalize_dialog_act(Reduced, Normalized).
normalize_dialog_act(Act, Act).

% Indirect request - "can you hand me that screwdriver?"
da_normal_form(question(Speaker, Addressee, can(Command), present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).
da_normal_form(question(Speaker, Addressee, would(Command), present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).
% Indirect request - "I want you to hand me the screwdriver"
da_normal_form(assertion(Speaker, Addressee, want(Speaker, Command), present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).
da_normal_form(assertion(Speaker, Addressee, should(Command), present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).
da_normal_form(assertion(Speaker, Addressee, must(Command), present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).
da_normal_form(assertion(Speaker, Addressee, would(likes(Speaker, Command)),
			 present, simple),
	       command(Speaker, Addressee, Command)) :-
   agent(Command, Addressee).

da_normal_form(command(Speaker, Addressee, Command),
	       question(Speaker, Addressee, Question, present, simple)) :-
   imperative_indirect_question(Speaker, Addressee, Command, Question).

imperative_indirect_question(S, A, tell_value(A, S, Question), Question).

da_normal_form(assertion(Speaker, Addressee, Assertion, _, _),
	       question(Speaker, Addressee, Question, present, simple)) :-
   declarative_indirect_question(Speaker, Addressee, Assertion, Question).

declarative_indirect_question(S, _, want(S, knows_value(S, Question)), Question).

da_normal_form(assertion(Speaker, Addressee, Assertion, T, A),
	       assertion(Speaker, Addressee, Normalized, T, A)) :-
   normalized_assertion(Speaker, Addressee, Assertion, Normalized).

normalized_assertion(S, _, likes(S, thinks(S, A)), A).
normalized_assertion(S, _, thinks(S, A), A).
normalized_assertion(S, _, knows(S, A), A).
normalized_assertion(S, _, believes(S, A), A).


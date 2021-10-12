%%
%% Questions
%%

% Dispatch on question type
strategy(respond_to_dialog_act(question(Asker, $me, Question,
					_Tense, _Aspect)),
	 if((Question = Answer:Constraint),
	    let(lf_main_predicate(Constraint, Core),
		answer_wh(Asker, Answer, Core, Constraint)),
	    answer_yes_no(Asker, Question))).

%% Yes/no quetsions
strategy(answer_yes_no(Asker, Q),
	 generate_answer(Q, Answer)) :-
   admitted_truth_value(Asker, Q, Answer).

strategy(generate_answer(Q, true),
	 agree($me, $addressee, Q)).
strategy(generate_answer(Q, false),
	 disagree($me, $addressee, Q)).
strategy(generate_answer(_Q, unknown),
	 speech(["Don't know."])).


%% Wh-questions

:- public unique_answer/2.
:- external unique_answer/2.
default_strategy(answer_wh(Asker, Answer, Core, Constraint),
		 if(unique_answer(Answer, Core),
		    generate_unique_answer(Asker, Answer, Core, Constraint),
		    enumerate_answers(Asker, Answer, Core, Constraint))).

strategy(answer_wh(_Asker, Identity, _,
		   (be(Person, Identity), is_a(Person, person))),
	 introduce_person(Person)) :-
   character(Person).

strategy(answer_wh(Asker, Identity, _,
		   (be(Entity, Identity), is_a(Entity, entity))),
	 tell_about($me, Asker, Entity)).

strategy(answer_wh(_Asker, Identity, _,
		   (be(player, Identity), is_a(player, person))),
	 say(be(player, $me))).

strategy(answer_wh(_Asker, Answer, can(Action), Constraint),
	 answer_with_list(List, "or", Type,
			  (can(Action), is_a(Answer, Type)))) :-
   possible_types_given_constraint(Answer, Constraint, List).

strategy(answer_wh(M, _,
		   manner(be(Who), M),
		   _),
	 say(okay(Who))).

strategy(answer_wh(Asker, Explanation, explanation(P, Explanation), _),
	 cases([admitted_truth_value(Asker, P, false):
	          assertion($me, Asker, not(P), present, simple),
		admitted_truth_value(Asker, explanation(P, E), true):
	          assertion($me, Asker, E, present, simple),
	        true:speech(["I couldn't speculate."])])).

default_strategy(generate_unique_answer(Asker, _Answer, Core, Constraint),
		 if(admitted_truth_value(Asker, Constraint, true),
		    assertion($me, Partner, Core, present, simple),
		    speech(["Don't know"]))) :-
   nonvar(Constraint),
   $task/partner/Partner.

default_strategy(enumerate_answers(Asker, Answer, Core, Constraint),
		 answer_with_list(List, Connective, Answer, Core)) :-
   nonvar(Constraint),
   all(Answer, admitted_truth_value(Asker, Constraint, true), List),
   connective_for_answer(Constraint, Connective).

connective_for_answer((can(_), _), "or") :- !.
connective_for_answer(_, "and").

strategy(answer_with_list([ ], _, Var, Constraint),
	 say_string(S)) :-
   !,
   begin(variable_type_given_constraint(Var, Constraint, Kind)),
         ( kind_of(Kind, actor) ->
	      S="Nobody"
	      ;
	      S="Nothing" ).

strategy(answer_with_list(ItemList, Termination, Var, Constraint),
	 say_list(ItemList, Termination, Var^s(Constraint))).

%%%
%%% Special rules for describing people
%%%

strategy(introduce_person(Person),
	 describe(Person, introduction, null)).

strategy(preface_description(Person),
	 give_name(Person)) :-
   is_a(Person, person).

default_strategy(give_name($me),
	 say_answer(be($me, Name))) :-
   property_value($me, given_name, Name).

strategy(give_name(X),
	 null) :-
   X \= $me.

property_relevant_to_purpose(introduction, _, Property, _) :-
   memberchk(Property, [age, gender, job]).
property_relevant_to_purpose(general, _, Property, _) :-
   Property \= description.

relation_relevant_to_purpose(introduction, _, Relation, _) :-
   memberchk(Relation, [ interested_in, member_of, roommate_of ]).
relation_relevant_to_purpose(general, _, _, _).


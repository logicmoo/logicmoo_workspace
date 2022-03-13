%%%
%%% Special rules for describing people
%%%

strategy(introduce_person(Person),
	 describe(Person, introduction, say_string("Not much to say."))).

strategy(preface_description(Person),
	 give_name(Person)) :-
   iz_a(Person, person).

default_strategy(give_name($me), say_answer(be($me, Name))) :-  
  t(given_name, $me, Name).

strategy(give_name(X),
	 null) :-
   X \= $me.



%=autodoc
%% property_relevant_to_purpose( ?General1, ?ARG2, ?Property, ?ARG4) is semidet.
%
% Property Relevant Converted To Purpose.
%
property_relevant_to_purpose(introduction, _, Property, _) :-
   memberchk(Property, [age, gender, job]).
property_relevant_to_purpose(general, _, Property, _) :-
   Property \= description.



%=autodoc
%% relation_relevant_to_purpose( ?General1, ?ARG2, ?Relation, ?ARG4) is semidet.
%
% Relation Relevant Converted To Purpose.
%
relation_relevant_to_purpose(introduction, _, Relation, _) :-
   memberchk(Relation, [ interested_in, member_of, coworker_of ]).
relation_relevant_to_purpose(general, _, _, _).


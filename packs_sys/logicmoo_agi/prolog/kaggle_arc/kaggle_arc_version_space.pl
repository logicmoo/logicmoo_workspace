/*
    <title>
      Prolog Version Space Search: Candidate Elimination
    </title>
*/
% Candidate Elimination Algorithm

run_candidate_elim :- candidate_elim([[_,_,_]], [], 
	[[small, medium, large], [red, blue, green], [ball, brick, cube]]).

% Candidate_elim implements a top level read-process loop
% It prints out the current G set 
% the S set
%		     
% and calls process to process the input.
candidate_elim([G],[S],_) :-
	covers(G,S),covers(S,G),
	write("target concept is "), write(G),nl.

candidate_elim(G, S, Types) :-
	write("G= "), write(G),nl,
	write("S= "), write(S),nl,
	write("Enter Instance "),
	read(Instance),
	process(Instance, G, S, Updated_G, Updated_S, Types),
	candidate_elim(Updated_G, Updated_S, Types).

% Process takes the instance, 
%	a set of concepts, G and 
%	and a set of concepts, S
% it implements a step of the candidate elimination algorithm.


process(negative(Instance), G, S, Updated_G, Updated_S, Types) :- 
	delete(X, S, covers(X, Instance), Updated_S),
	specialize_set(G,Spec_G, Instance, Types),
	delete(X, Spec_G, (member(Y, Spec_G), more_general(Y, X)), Pruned_G),
	delete(X, Pruned_G, (member(Y, Updated_S), not(covers(X, Y))), Updated_G).

process(positive(Instance), G, [], Updated_G, [Instance],_):- 
% Initialize S

delete(X, G, not(covers(X, Instance)), Updated_G).

process(positive(Instance), G, S, Updated_G, Updated_S,_) :- 
	delete(X, G, not(covers(X, Instance)), Updated_G),
	generalize_set(S,Gen_S, Instance),
	delete(X, Gen_S, (member(Y, Gen_S), more_general(X, Y)), Pruned_S),
	delete(X, Pruned_S, not((member(Y, Updated_G), covers(Y, X))), Updated_S).

process(Input, G, P, G, P,_):- 
	Input \= positive(_),
	Input \= negative(_),
	write("Enter either positive(Instance) or negative(Instance) "), nl

% The following predicate definitions are duplicated in either
% the general to specific searches or the specific to genearal searches.
% 
specialize_set([], [], _, _).
specialize_set([Hypothesis|Rest],Updated_H,Instance, Types):-	
	covers(Hypothesis, Instance),			
	(bagof(Hypothesis, specialize(Hypothesis, Instance, Types), Updated_head); Updated_head = []),
	specialize_set(Rest, Updated_rest, Instance, Types),
	append(Updated_head, Updated_rest, Updated_H).

specialize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance, Types):-
	not (covers(Hypothesis, Instance)),			
	specialize_set(Rest,Updated_rest, Instance, Types).

specialize([Prop|_], [Inst_prop|_], [Instance_values|_]):-
 	var(Prop),
	member(Prop, Instance_values),
	Prop \= Inst_prop.

specialize([_|Tail], [_|Inst_tail], [_|Types]):-
	specialize(Tail, Inst_tail, Types).

%

generalize_set([], [], _).

generalize_set([Hypothesis|Rest],Updated_H,Instance):-			
	not(covers(Hypothesis, Instance)),
	(bagof(X, generalize(Hypothesis, Instance, X), Updated_H); Updated_head = []),
	generalize_set(Rest,Updated_rest, Instance),
	append(Updated_head, Updated_rest, Updated_H).

generalize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance):-
	covers(Hypothesis, Instance),
	generalize_set(Rest,Updated_rest, Instance).

%

generalize([],[],[]).	
generalize([Feature|Rest], [Inst_prop|Rest_inst], [Feature|Rest_gen]) :-
	not(Feature \= Inst_prop),
	generalize(Rest, Rest_inst, Rest_gen).

generalize([Feature|Rest], [Inst_prop|Rest_inst], [_|Rest_gen]) :-
	Feature \= Inst_prop,
	generalize(Rest, Rest_inst, Rest_gen).

% more_general(Feature_vector_1, Feature_vector_2) :- succeeds if
%	Feature_vector_1 is strictly more general than Feature_vector_2

more_general(X, Y) :-  not(covers(Y, X)), covers(X, Y).

% covers(Feature_list_1, Feature_list_2) :- Succeeds if Feature_list_1
%	covers Feature_list_2.  Note that covers, unlike unification is
%	not symmetric: variables in Feature_list_2 will not match constants
%	in Feature_list_1.

covers([],[]).
covers([H1|T1], [H2|T2]) :-
	var(H1), var(H2), 
	covers(T1, T2).
covers([H1|T1], [H2|T2]) :-
	var(H1), atom(H2), 
	covers(T1, T2).	
covers([H1|T1], [H2|T2]) :-
	atom(H1), atom(H2), H1 = H2,
	covers(T1, T2).

% delete(Element, List1, Goal, List2) :- List2 contains all bindings
%	of Element to a member of List1 except those that cause 
%	Goal to succeed

delete(X, L, Goal, New_L) :-
	(bagof(X, (member(X, L), not(Goal)), New_L);New_L = []).  </pre>


/*
    <title>
      Prolog Version Space Search: Candidate Elimination Sample Run
    </title>

?- run_candidate_elim.
"G= "[[_0,_1,_2]]
"S= "[]
"Enter Instance "positive([small, red, ball]).
"G= "[[_0,_1,_2]]
"S= "[[small,red,ball]]
"Enter Instance "negative([large, green, cube]).
"G= "[[small,_96,_97],[_86,red,_87],[_76,_77,ball]]
"S= "[[small,red,ball]]
"Enter Instance "negative([small, blue, brick]).
"G= "[[_86,red,_87],[_76,_77,ball]]
"S= "[[small,red,ball]]
"Enter Instance "positive([small, green, ball]).
"G= "[[_76,_77,ball]]
"S= "[[small,_351,ball]]
"Enter Instance "positive([large, red, ball]).
"target concept is "[_76,_77,ball]
  yes 
*/


/*
<title>
      Prolog Version Space Search: General To Specific
    </title>
*/

% General to specific induction algorithm

% run_general() :- Calls general_to_specific with H initialized
% 	to the most general feature vector and N initialized to []
%	Also, provides the types for specializing simple vectors of 
%	type [size, color, shape].

run_general :-
	general_to_specific([[_,_,_]], [], 
	[[small, medium, large], [red, blue, green], [ball, brick, cube]]).

% general_to_specific(List_of_hypotheses, List_of_positives, List_of_types) :-
%	This is the top level control loop. It reads in a new positive or
% 	negative instance and calls process to update List_of_hypotheses
%	and List_of_positives. List_of_types is passed in for specializing
%  	descriptions.
general_to_specific(H, P, Types) :-
	write("H = "), write(H),nl,
	write("P = "), write(P),nl,
	write("Enter Instance "),
	read(Instance),
	process(Instance, H, P, Updated_H, Updated_P, Types),
	general_to_specific(Updated_H, Updated_P, Types).
% process(Instance, List_of_hypotheses, List_of_positives, 
%		Updated_hypotheses, Updated_positives, List_of_types) :-
%	updates List_of_hypotheses and List_of_types in response to
%	Instance.  LIst_of_types is passed through to enable specialization.
process(negative(Instance), H, P, Updated_H, P, Types) :- 
	specialize_set(H,Spec_H, Instance, Types),
	delete(X, Spec_H, (member(Y, Spec_H), more_general(Y, X)), Pruned_H),
	delete(X, Pruned_H, (member(Y, P), not(covers(X, Y))), Updated_H).

process(positive(Instance), H, P, Updated_H, [Instance|P], _) :-
	delete(X, H, not(covers(X, Instance)), Updated_H).
process(Input, H, P, H, P,_):- 
	Input \= positive(_),	Input \= negative(_),
	write("Enter either positive(Instance) or negative(Instance) "), nl.

% 
specialize_set([], [], _, _).
specialize_set([Hypothesis|Rest],Updated_H,Instance, Types):-
	covers(Hypothesis, Instance),			
	(bagof(Hypothesis, specialize(Hypothesis, Instance, Types), 
	Updated_head); Updated_head = []),
	specialize_set(Rest, Updated_rest, Instance, Types),
	append(Updated_head, Updated_rest, Updated_H).
specialize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance, Types):-
	not (covers(Hypothesis, Instance)),			
	specialize_set(Rest,Updated_rest, Instance, Types).

specialize([Prop|_], [Inst_prop|_], [Instance_values|_]):-
 	var(Prop),
	member(Prop, Instance_values),
	Prop \= Inst_prop.specialize([_|Tail], [_|Inst_tail], [_|Types]):-
	specialize(Tail, Inst_tail, Types).

% more_general(Feature_vector_1, Feature_vector_2) :- succeeds if
%	Feature_vector_1 is strictly more general than Feature_vector_2

more_general(X, Y) :-  not(covers(Y, X)), covers(X, Y).

% covers(Feature_list_1, Feature_list_2) :- Succeeds if Feature_list_1
%	covers Feature_list_2.  Note that covers, unlike unification is
%	not symmetric: variables in Feature_list_2 will not match constants
%	in Feature_list_1.

covers([],[]).
covers([H1|T1], [H2|T2]) :-
	var(H1), var(H2), 
	covers(T1, T2).
covers([H1|T1], [H2|T2]) :-
	var(H1), atom(H2), 
	covers(T1, T2).	
covers([H1|T1], [H2|T2]) :-
	atom(H1), atom(H2), H1 = H2,
	covers(T1, T2).

% delete(Element, List1, Goal, List2) :- List2 contains all bindings
%	of Element to a member of List1 except thos that cause 
%	Goal to succeed
delete(X, L, Goal, New_L) :-
	(bagof(X, (member(X, L), not(Goal)), New_L);New_L = []).


/*
    <title>
      Prolog Version Space Search: General To Specific Sample Run
    </title>
 

?- run_general.
"H = "[[_0,_1,_2]]
"P = "[]
"Enter Instance "positive([small, red, ball]).
"H = "[[_0,_1,_2]]
"P = "[[small,red,ball]]
"Enter Instance "negative([large, green,cube]).
"H = "[[small,_89,_90],[_79,red,_80],[_69,_70,ball]]
"P = "[[small,red,ball]]
"Enter Instance "negative([small, blue, brick]).
"H = "[[_79,red,_80],[_69,_70,ball]]
"P = "[[small,red,ball]]
"Enter Instance "positive([small, green, ball]).
"H = "[[_69,_70,ball]]
"P = "[[small,green,ball],[small,red,ball]]
 
*/


/*

    <title>
      Prolog Version Space Search: Specific To General
    </title>
*/
% Specific to general induction algorithm
% run_specific():-Calls specific_to_general with both H and N initialized to []

run_specific :- specific_to_general([],[]).
% specific_to_general(List_of_hypotheses, List_of_negatives) :- 
%	This is the top level control loop. It reads in a new positive or
% 	negative instance and calls process to update List_of_hypotheses
%	and List_of_negatives. 

specific_to_general(H, N) :-
	write("H = "), write(H),nl,
	write("N = "), write(N),nl,
	write("Enter Instance "),
	read(Instance),
	process(Instance, H, N, Updated_H, Updated_N),
	specific_to_general(Updated_H, Updated_N).

% process(Instance, List_of_hypotheses, List_of_negatives, 
%	Updated_hypotheses, Updated_negatives) :-
%	updates List_of_hypotheses and List_of_negatives in response to
%	Instance.  

process(positive(Instance), [], N, [Instance], N). 
% Initialize H
process(positive(Instance), H, N, Updated_H, N) :- 
	generalize_set(H,Gen_H, Instance),
	delete(X, Gen_H, (member(Y, Gen_H), more_general(X, Y)), Pruned_H),
	delete(X, Pruned_H, (member(Y, N), covers(X, Y)), Updated_H).	

process(negative(Instance), H, N, Updated_H, [Instance|N]) :- 
		delete(X, H, covers(X, Instance), Updated_H).

process(Input, H, N, H, N):- 
	Input \= positive(_),
	Input \= negative(_),
	write("Enter either positive(Instance) or negative(Instance) "), nl.

% 

generalize_set([], [], _).

generalize_set([Hypothesis|Rest],Updated_H,Instance):-	
	not(covers(Hypothesis, Instance)),
	(bagof(X, generalize(Hypothesis, Instance, X), Updated_head); Updated_head = []),
	generalize_set(Rest,Updated_rest, Instance),
	append(Updated_head, Updated_rest, Updated_H).

generalize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance):-
	covers(Hypothesis, Instance),
	generalize_set(Rest,Updated_rest, Instance).
	
%

generalize([],[],[]).	
generalize([Feature|Rest], [Inst_prop|Rest_inst], [Feature|Rest_gen]) :-
	not(Feature \= Inst_prop),
	generalize(Rest, Rest_inst, Rest_gen).
generalize([Feature|Rest], [Inst_prop|Rest_inst], [_|Rest_gen]) :-
	Feature \= Inst_prop,
	generalize(Rest, Rest_inst, Rest_gen).

% more_general(Feature_vector_1, Feature_vector_2) :- succeeds if
%	Feature_vector_1 is strictly more general than Feature_vector_2
more_general(X, Y) :-  not(covers(Y, X)), covers(X, Y).

% covers(Feature_list_1, Feature_list_2) :- Succeeds if Feature_list_1
%	covers Feature_list_2.  Note that covers, unlike unification is
%	not symmetric: variables in Feature_list_2 will not match constants
%	in Feature_list_1.

covers([],[]).
covers([H1|T1], [H2|T2]) :-
	var(H1), var(H2), 
	covers(T1, T2).
covers([H1|T1], [H2|T2]) :-
	var(H1), atom(H2), 
	covers(T1, T2).	
covers([H1|T1], [H2|T2]) :-
	atom(H1), atom(H2), H1 = H2,
	covers(T1, T2).

% delete(Element, List1, Goal, List2) :- List2 contains all bindings
%	of Element to a member of List1 except those that cause 
%	Goal to succeed

delete(X, L, Goal, New_L) :-
	(bagof(X, (member(X, L), not(Goal)), New_L);New_L = []).

/*

     <title>
      Prolog Version Space Search: Specific To General Sample Run
    </title>

?- run_specific.
"H = "[]
"N = "[]
"Enter Instance "positive([small, red, ball]).
"H = "[[small,red,ball]]
"N = "[]
"Enter Instance "negative([large, green, cube]).
"H = "[[small,red,ball]]
"N = "[[large,green,cube]]
"Enter Instance "negative([small, blue, brick]).
"H = "[[small,red,ball]]
"N = "[[small,blue,brick],[large,green,cube]]
"Enter Instance "positive([small, green, ball]).
"H = "[[small,_66,ball]]
"N = "[[small,blue,brick],[large,green,cube]]
"Enter Instance "positive([large, blue, ball]).
"H = "[[_116,_66,ball]]
"N = "[[small,blue,brick],[large,green,cube]]
"Enter Instance " 

*/

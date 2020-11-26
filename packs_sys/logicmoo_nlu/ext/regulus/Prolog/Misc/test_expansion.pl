
% Minimal version of inner loop in unification grammar compiler
%
% Test calls:
% 
% test_expansion(1, List), length(List, Len).     [should give Len = 24]
% test_expansion(2, List), length(List, Len).     [should give Len = 96]
% test_expansion(3, List), length(List, Len).     [should give Len = 96]
% test_expansion(4, List), length(List, Len).     [should give Len = 54]
% test_expansion(5, List), length(List, Len).     [should give Len = 0]

%--------------------------------------------------------------

test_expansion(ID, Result) :-
	test_list(ID, Term),
	findall(Term, expand_list(Term), Result).

expand_list([]).
expand_list([F | R]) :-
	expand_item(F),
	expand_list(R).

expand_item(Tag-Var) :-
	possible_expansions_for_tag(Tag, Possibilities),
	member(Var, Possibilities).

member(F, [F | _R]).
member(X, [_F | R]) :-
	member(X, R).

%--------------------------------------------------------------

test_list(1, [a-_X, b-_Y]).
test_list(2, [a-_X, b-_Y, c-_Z]).
test_list(3, [a-_X, b-_Y, c-_Z, a-_X, b-_Y]).
test_list(4, [a-_X, b-_Y, c-_Z, a-_Y, a-_Z]).
test_list(5, [a-_X, b-_Y, c-_Z, a-_Y, b-_Z]).

possible_expansions_for_tag(a, [1,2,3,4,5,6]).
possible_expansions_for_tag(b, [1,3,5,7]).
possible_expansions_for_tag(c, [2,4,6,8]).


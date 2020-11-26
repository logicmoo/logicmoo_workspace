
% Brian Ross
% Dec 10, 1998

% DCTG post-processor: generates relevant tables needed by GP processing.
%
% 1. dctg_rule_info(Name, ID, Call, MinDepth, Type)
%	Name = name of rule 
%	ID = DCTG ID label
%	Call = call image for DCTG rule invocation
%	MinDepth = minimum depth to terminal generation 
%	Type = terminal, nonterminal
%
% 2. dctg_id_table(Name, IDList, TermList, NontermList)
%	Name = functor name
%	IDList = list of rule ID's for that functor
%	TermList, NontermList = lists of terminals, nonterminals
%
% Some assumptions: 
% - Grammar rules should be uniquely identifiable by functor name.
%   Should not use same functor name for different arity grammar rules!

make_grammar_table :-
	cleanup_grammar_data,
	make_rule_id_list,
	generate_rule_data,
	enhance_rule_id_list,
	!.

cleanup_grammar_data :-
	retractall(dctg_rule_info(_, _, _, _)),
	retractall(dctg_id_table(_, _, _, _)),
	!.

% get_rule_name constructs a call to a DCTG rule, such that all the args are
% variable except for the node structure, which has the ID number set.
% This uniquely identifies a DCTG rule header.

get_rule_name(Call2) :-
	clause(semantic_rule(ID, _, Call, _), _),
	Call =.. [Name|Args],
	clone_list(Args, T),
	append(T, [node(_, _, ID), _, _], AllArgs),
	Call2 =.. [Name|AllArgs].

% Given a list, clone_list creates a list of the same size, 
% but with uninst. args

clone_list([], []) :- !.
clone_list([_|T], [_|T2]) :-
	clone_list(T, T2),
	!.

% generate_rule_data gets all the rule headers (Calls) and finds: (i) the
% minimum depth to terminals for each rule; (ii) whether a rules is a
% terminal or nonterminal. Result saved in dctg_rule_info/4.

generate_rule_data :-
	findall(Call, get_rule_name(Call), Calls),
	rem_dups(Calls, Calls2),
	grammar_depth_top_loop(Calls2, [], [], Calls3),
	grammar_type_top_loop(Calls2, [], [], Terminal),
	set_rule_data(Calls3, Terminal),
	!.

% grammar_depth_top_loop(Calls, Known, MinCalls, Known2)
% Processes rules until all have min depths found, or no changes occurred
% (which means there's a problem with the rules).
%	Calls - rules to process
%	Known - rules with min depths
%	MinCalls - overall minimum for entire rules, to be used in goal analysis
%	Known - new Known set
%
% Algorithm for depth determination:
% Repeat until no unknown rules (hopefully! else there's infinite recursion) 
%	Process all unknown rules:
%  		If all goals have known minimum depths in a rule
%		then find maximum of these and add to Known list
%	Process all Known rules:
%		If a new rule has been added to Known (not in Minimum list)
%		then add it to Minimum list.

grammar_depth_top_loop([], Known, _, Known) :- !.
grammar_depth_top_loop(Calls, Known, MinCalls, Known3) :-
	process_rules(Calls, Known, MinCalls, [], Known2, Unknown),
	find_rule_mins(Known2, MinCalls, MinCalls2),
	((length(Calls, L), length(Unknown, L)) ->  % if no changes...
		write('Problem - '),write(L),write(' rules cannot terminate:'),
		nl, writelist(Unknown), nl,
		write('these terminated - '),nl, writelist(Known2),nl,
		write('These are mincalls - '),nl, writelist(MinCalls2),nl,
		fail
		;
		grammar_depth_top_loop(Unknown, Known2, MinCalls2, Known3)),
	!.

% process_rules(Calls, Known, MinCalls, Unknown, Known2, Unknown2):
%	Calls - to process
%	Known, Unknown - rules with known/unknown minima
%	MinCalls - solved rules (can be used in body analyses of other rules)
%	Known2, Unknown2 - final values of above
% Find depth of body for rules. If available, add rule to Known, else Unknown.

process_rules([], Known, _, Unknown, Known, Unknown) :- !.
process_rules([Call|Rest], Known, MinCalls, Unknown, Known2, Unknown2) :-
	copy_term(Call, Call2),
	clause(Call2, Body),
	find_min_depth_body(Body, MinCalls, 0, BodyDepth),
	!,
	MinD is BodyDepth + 1,
	process_rules(Rest, [(Call,MinD)|Known], MinCalls, Unknown, Known2, Unknown2).
process_rules([Call|Rest], Known, MinCalls, Unknown, Known2, Unknown2) :-
	!,
	process_rules(Rest, Known, MinCalls, [Call|Unknown], Known2, Unknown2).

% find_min_depth_body(Body, MinCalls, MinDSoFar, MinD)
% Finds the depth value of body (max of all min vals of goals in body).
% Fails if an goal with unknown depth is found.

find_min_depth_body((Goal,Rest), MinCalls, MinDSoFar, MinD) :-
	is_a_rule_call(Goal),
	!,
	find_min_depth(Goal, MinCalls, Val),
	MinDSoFar2 is max(Val, MinDSoFar),
	find_min_depth_body(Rest, MinCalls, MinDSoFar2, MinD).
find_min_depth_body((_,Rest), MinCalls, MinDSoFar, MinD) :-
	!,
	find_min_depth_body(Rest, MinCalls, MinDSoFar, MinD).
find_min_depth_body(Goal, MinCalls, MinDSoFar, MinD) :-
	is_a_rule_call(Goal),
	!,
	find_min_depth(Goal, MinCalls, Val),
	MinD is max(Val, MinDSoFar).
find_min_depth_body(_, _, MinD, MinD) :-
	!.

% find_min_depth searches for goal name in list, and returns corresp.
% depth if found.

find_min_depth(Goal, [(G,M)|_], M) :-
	Goal =.. [G|_],
	!.
find_min_depth(Goal, [_|R], M) :-
	find_min_depth(Goal, R, M),
	!.

% is_a_rule_call checks if a goal refers to a DCTG rule

is_a_rule_call(Goal) :-
	Goal =.. [Name|_],
	dctg_id_table(Name, _, _, _),	
	!.

% find_rule_mins(Calls, MinCalls, MinCalls2):
% Checks the current Known list for new overall minimum depths. 
% If a goal shows up in Known for the first time (ie. not in MinCalls)
% then it's depth value must be the minimum for that rule set: add it as such.

find_rule_mins([], MinCalls, MinCalls) :- !.
find_rule_mins([(Call,Depth)|Rest], MinCalls, MinCalls2) :-
	Call =.. [CallName|_],
	\+ member((CallName,_), MinCalls),  
	!,
	find_rule_mins(Rest, [(CallName,Depth)|MinCalls], MinCalls2).
find_rule_mins([_|Rest], MinCalls, MinCalls2) :-
	find_rule_mins(Rest, MinCalls, MinCalls2).

% abstract_member checks if functor names match 

abstract_member(GoalName, [(First,_)|_]) :-
	First =.. [GoalName|_].	
abstract_member(GoalName, [_|Rest]) :-
	abstract_member(GoalName, Rest).	

% find_minimum_depth(Name, Calls, MinSoFar, Min):
% Finds the minimum depth value for Name in list of Calls.

find_minimum_depth(_, [], D, D).
find_minimum_depth(CallName, [(Call, D)|Rest], MinSoFar, MinDepth) :-
	Call =.. [CallName|_],
	NewMin is min(D, MinSoFar),
	find_minimum_depth(CallName, Rest, NewMin, MinDepth),
	!.
find_minimum_depth(CallName, [_|Rest], MinSoFar, MinDepth) :-
	find_minimum_depth(CallName, Rest, MinSoFar, MinDepth),
	!.


% grammar_type_top_loop(Calls, Terms, Nonterms, Terms2):
%	Calls - rules to process
%	Terms, Nonterms - terminals and nonterminals so far
%	Terms2 - final results of above
%
% Determine if rules can be classified as terminals. 
% Processing continues until no change in the set of rules that are unknown -
% these leftovers are classified as 'nonterminals'.
% First, user-override is checked. If it fail's then analysis done.
% See 'rule_type' for more details.
% Note that intermediate nonterminal determination is done as well;
% this could be deleted in the future to save some processing.

grammar_type_top_loop(Calls, Terms, Nonterms, Terms2) :-
	grammar_type_loop(Calls, [], Terms,Nonterms,Unknown, Terms3, Nonterms3),
	(length(Calls, A), length(Unknown, A) ->
		Terms3 = Terms2
		;
		grammar_type_top_loop(Unknown, Terms3, Nonterms3, Terms2)),
	!.

grammar_type_loop([], Unknown, Term, Nonterm, Unknown, Term, Nonterm) :- !.
grammar_type_loop([Call|Rest], Unknown, Term, Nonterm, Unknown2, Term2, 
		Nonterm2) :-
	user_override(Call, Term, Nonterm, Term3, Nonterm3),
	grammar_type_loop(Rest, Unknown, Term3, Nonterm3, Unknown2, Term2, Nonterm2).
grammar_type_loop([Call|Rest], Unknown, Term, Nonterm, Unknown2, Term2, 
		Nonterm2) :-
	copy_term(Call, Call2),
	clause(Call2, Body),
	goal_type(Call, Body, Rest, Unknown, Term, Nonterm, Unknown3, Term3, Nonterm3),
	grammar_type_loop(Rest, Unknown3, Term3, Nonterm3, Unknown2, Term2, Nonterm2).

% user_override(Call, Term, Nonterm, Term2, Nonterm2):
%	Call - rule head to process
%	Term, Nonterm - list of rules identified as terms and nonterms
%	Term2, Nonterm2 - final values of Term, Nonterm
% If the user has Call functor name in dctg_override parameter lists, then
% add that call to term or nonterm as appropriate. Otherwise, fail.

user_override(Call, Term, Nonterm, [Call|Term], Nonterm) :-
	Call =.. [Name|_],
	dctg_override_P(OverTerm, _),
	member(Name, OverTerm),
	!.
user_override(Call, Term, Nonterm, Term, [Call|Nonterm]) :-
	Call =.. [Name|_],
	dctg_override_P(_, OverNonterm),
	member(Name, OverNonterm),
	!.
	

% goal_type(Call, Body, Unknown, Term, Nonterm, Unknown2, Term2, Nonterm2)
%	Call - current head of rule being analyzed
%	Body - body of current rule
%	Rest - rules not yet processed
%	Unknown - unknown classifications
%	Term, Nonterm - Rules known to be terminal or nonterminal
%	Unknown2,Term2,Nonterm2 - Final values of results 
%
% This performs abstract interp of a single rule body.
% The order of tests in the following is critical.
%	1. If goal is a nonterm, then that rule is a nonterm.
%	2. If goal is same as rule name, then that rule is a nonterm.
%	3. If a goal is unknown, or not yet processed,
%		then that rule is unknown.
%	4. Else if the rest of the goals in that clause are term, 
%		then that rule is terminal.

goal_type(Call, Goals, _, U, T, NT, U, T, [Call|NT]) :-  % 1, 2
	(Goals = (A,_) -> true ; Goals = A),
	(abstract_member2(A, NT) ; same_goal(Call, A)),
	!.
goal_type(Call, Goals, Rest, U, T, NT, [Call|U], T, NT) :- % 3
	(Goals = (A,_) -> true ; Goals = A),
	(abstract_member2(A, U) ; abstract_member2(A, Rest)),
	!.
goal_type(Call, (_,B), Rest, U, T, NT, U2, T2, NT2) :-
	!,
	goal_type(Call, B, Rest, U, T, NT, U2, T2, NT2).
goal_type(Call, _, _, U, T, NT, U, [Call|T], NT). % 4


% abstract_member2 checks if functor names match 

abstract_member2(Goal, [First|_]) :-
	same_goal(Goal, First).
abstract_member2(Goal, [_|Rest]) :-
	abstract_member2(Goal, Rest).	

same_goal(A, B) :-
	A =.. [N|_],
	B =.. [N|_],
	!.

% save depths, term/nonterm in dctg_rule_info assertions

set_rule_data([], _) :- !.
set_rule_data([(Rule, Depth)|Rest], Terminal) :-
	Rule =.. [Name|Args],
	append(_, [node(_, _, ID), _, _], Args),
	(member(Rule, Terminal) ->
		Type = terminal
		;
		Type = nonterminal),
	assert(dctg_rule_info(Name, ID, Rule, Depth, Type)),
	set_rule_data(Rest, Terminal),
	!.

% make_rule_id_list makes a table giving rule name and the ID numbers of its
% associated rules. Last 2 args are placeholders for term and nonterm lists,
% to be set later.

make_rule_id_list :-
	findall((Name, IDs), make_rule_id_list2(Name, IDs), RuleIDs),
	make_id_entries(RuleIDs),
	!.

make_rule_id_list2(Name, RuleIDs2) :-
	bagof(ID, get_rule_stuff(Name, ID), RuleIDs),
	rem_dups(RuleIDs, RuleIDs2).

get_rule_stuff(Name, ID) :-
	clause(semantic_rule(ID, _, Call, _), _),
	Call =.. [Name|_].

make_id_entries([]) :- !.
make_id_entries([(Name, IDs)|Rest]) :-
	assert(dctg_id_table(Name, IDs, _, _)),
	make_id_entries(Rest),
	!.

% enhance each dctg_id_table entry with list of which rules are terminal,
% and which are nonterminal. 

enhance_rule_id_list :-
	retract(dctg_id_table(Name, IDs, _, _)),
	identify_type(IDs, Terms, Nonterms),
	assert(dctg_id_table(Name, IDs, Terms, Nonterms)),
	fail.
enhance_rule_id_list.

identify_type([], [], []).
identify_type([ID|Rest], [ID|Terms], Nonterms) :-
	dctg_rule_info(_, ID, _, _, terminal),
	!,
	identify_type(Rest, Terms, Nonterms).
identify_type([ID|Rest], Terms, [ID|Nonterms]) :-
	identify_type(Rest, Terms, Nonterms).


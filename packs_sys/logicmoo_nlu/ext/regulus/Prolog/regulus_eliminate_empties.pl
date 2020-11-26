:- module(regulus_eliminate_empties,
	  [eliminate_empties/2]).

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).

/*

Code for eliminating empty productions. Note that rules must be UNINSTANTIATED
when this is called.

*/

% -----------------------------------------------------

eliminate_empties(RulesIn, RulesIn) :-
	\+ term_contains_functor(RulesIn, '*empty*'/0),
	format('~N~nNo empty productions in grammar, empty production elimination irrelevant.~n~n', []),
	!.
eliminate_empties(RulesIn, RulesOut) :-
	format('~N~n -- Eliminating empty productions~n', []),
	report_rule_set_size('Before eliminating empty productions: ', RulesIn),
	timed_call(eliminate_empties1(RulesIn, RulesOut), TimeTaken),
	report_rule_set_size('After eliminating empty productions: ', RulesOut),
	format('~N -- Empty production elimination done, ~1f secs~n~n', [TimeTaken]),	
	!.
eliminate_empties(_RulesIn, _RulesOut) :-
	regulus_error('~NInternal error: call to eliminate_empties failed~n', []).

eliminate_empties1(RulesIn, RulesOut) :-
	eliminate_empties2(RulesIn, RulesMid),
	!,
	eliminate_empties1(RulesMid, RulesOut).
eliminate_empties1(RulesIn, RulesIn).

% -----------------------------------------------------

eliminate_empties2(RulesIn,RulesOut) :-
	cat_names_with_empties(RulesIn,ENamePairs,RulesMid),
	ENamePairs=[_|_],	% else fail, so we're finished
	duplicate_empty_cats(RulesMid,ENamePairs,RulesOut).

cat_names_with_empties([],[],[]).
cat_names_with_empties([Name-Group|RulesIn],[Name:(Keep,Empties)|ENamePairs],
		       RulesOut) :-
	split_empty_and_non_empty(Group,Empties0,NewGroup),
	safe_remove_duplicates(Empties0,Empties),
	Empties = [_|_],	% did we find one?
	!,
	(   NewGroup = [] ->	% i.e. _just_ the empty production
	    
	    Keep=no,
	    RulesOut=RulesTail ;
	    
	    Keep=yes,
	    RulesOut=[Name-NewGroup|RulesTail]
	),
	cat_names_with_empties(RulesIn,ENamePairs,RulesTail).
cat_names_with_empties([Rule|RulesIn], ENamePairs, [Rule|RulesOut]) :-
	cat_names_with_empties(RulesIn,ENamePairs,RulesOut).

split_empty_and_non_empty([],[],[]).
split_empty_and_non_empty([Rule|RulesIn],[Sem|Empties],NonEmpties) :-
	Rule = rule((cat(_Name,[],Sem) --> '*empty*'),_),
	!,
	split_empty_and_non_empty(RulesIn,Empties,NonEmpties).
split_empty_and_non_empty([Rule|RulesIn],Empties,[Rule|NonEmpties]) :-
	split_empty_and_non_empty(RulesIn,Empties,NonEmpties).

duplicate_empty_cats([],_,[]).
duplicate_empty_cats([Name-GroupIn|RulesIn],ENamePairs,
		     [Name-GroupOut|RulesOut]) :-
	duplicate_empty_cats_in_group(GroupIn,ENamePairs,GroupOut),
	duplicate_empty_cats(RulesIn,ENamePairs,RulesOut).
		    
duplicate_empty_cats_in_group([],_,[]).
duplicate_empty_cats_in_group([rule((Head --> Body),Info)|RulesIn],
			      ENamePairs,RulesOut) :-
	findall(rule((Head --> NewBody),Info),
		duplicate_empty_cats_in_body(Body,ENamePairs,NewBody),
		NewRules),
	%report_expanded_rule(rule((Head --> Body),Info), NewRules),
	append(NewRules,RulesTail,RulesOut),
	!,
	duplicate_empty_cats_in_group(RulesIn,ENamePairs,RulesTail).

report_expanded_rule(Rule, NewRules) :-
	length(NewRules, N),
	N > 1,
	format('~NExpanded to ~d rules: ~w~n', [N, Rule]),
	!.
report_expanded_rule(_Rule, _NewRules).

% Non-deterministic predicate.
duplicate_empty_cats_in_body((LBody,RBody),ENamePairs,BodyOut) :-
	!,
	duplicate_empty_cats_in_body(LBody,ENamePairs,LBodyOut),
	duplicate_empty_cats_in_body(RBody,ENamePairs,RBodyOut),
	combine_into_conjunction(LBodyOut,RBodyOut,BodyOut).
duplicate_empty_cats_in_body(BodyIn,ENamePairs,BodyOut) :-
	BodyIn = cat(Name,[],Sem),
	!,
	(memberchk(Name:(Keep,EmptyCatSemList),ENamePairs) ->
	    ((BodyOut = '*empty*', member(Sem,EmptyCatSemList)); % non-deterministic
		Keep=yes, BodyOut=BodyIn % part...
	    );				
	    BodyOut=BodyIn).
duplicate_empty_cats_in_body(Terminal,_,Terminal).

combine_into_conjunction('*empty*','*empty*',Body) :-
	!,
	Body='*empty*'.
combine_into_conjunction('*empty*',RBody,Body) :-
	!,
	Body=RBody.
combine_into_conjunction(LBody,'*empty*',Body) :-
	!,
	Body=LBody.
combine_into_conjunction(LBody,RBody,(LBody,RBody)).

% --------------------------------------------------------------------

% Very slightly modified version of code in $REGULUS/Prolog/regulus_compact.pl

report_rule_set_size(Msg,Rules) :-
	find_rule_set_size(Rules,0,0,0,NC,NR,ND),
	format('~w~d categories, ~d rules, ~d daughters~n',[Msg,NC,NR,ND]).

find_rule_set_size([],NC,NR,ND,NC,NR,ND).
find_rule_set_size([_-Group|Rules],NCIn,NRIn,NDIn,NCOut,NROut,NDOut) :-
	NCMid is NCIn+1,
	length(Group,LG),
	NRMid is NRIn+LG,
	count_group_daughters(Group,NDIn,NDMid),
	find_rule_set_size(Rules,NCMid,NRMid,NDMid,NCOut,NROut,NDOut).

count_group_daughters([],N,N).
count_group_daughters([rule((_-->Body),_)|Rules],NIn,NOut) :-
	count_rule_daughters(Body,NIn,NMid),
	count_group_daughters(Rules,NMid,NOut).

count_rule_daughters((L,R),NIn,NOut) :-
	!,
	count_rule_daughters(L,NIn,NMid),
	count_rule_daughters(R,NMid,NOut).
count_rule_daughters(_,NIn,NOut) :-
	NOut is NIn+1.

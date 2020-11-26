:- consult(pack('free-life-planner/lib/util/util.pl')).

:- op(1200,xfx,'::').

genlMt(microtheoryAboutFn(mMG),everythingPSC).

:- consult('contents1').
:- consult('contents2').
:- consult('contents3').

listMicrotheoryContents(Mt,Assertions) :-
	allTermAssertions(_,AllTermAssertions),
	shell_quote_term(Mt,QuotedMt),
	fluents_for_mt(AllAssertedKnowledge),
	findall(Assertion,member(Mt::Assertion,AllTermAssertions),Assertions1),
	findall(Assertion,member(Assertion,AllAssertedKnowledge),Assertions2),
	findall(Assertion,(genlMt(Mt,SuperMt),listMicrotheoryContents(SuperMt,Assertions),member(Assertion,Assertions)),Assertions3),
	%% view([Assertions1,Assertions2,Assertions3]),
	append([Assertions1,Assertions2,Assertions3],Assertions).

printMt(Mt) :-
	listMicrotheoryContents(Mt,Contents),view([contentsOfMt(Mt,Contents)]).

%% have the ability to declare that assertions are in a particular
%% microtheory in the file.

test :-
	printMt(microtheoryAboutFn(mMG)),
	printMt(everythingPSC).

%% assertIntoMicrotheory().

%% persistMt().

%% retractFromMicrotheory().

%% implies(Mt,X,Y) :-
%% 	listMicrotheoryContents(Mt,Contents),
%%	.
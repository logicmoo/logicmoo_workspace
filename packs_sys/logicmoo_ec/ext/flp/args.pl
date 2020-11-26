:- ensure_loaded('util').

argt(Arguments,KeyValueTerm) :-
	not(is_list(KeyValueTerm)),
	findall(Argument,(member(Argument,Arguments),Argument = KeyValueTerm),Matches),
	nth1(1,Matches,Match),
	KeyValueTerm = Match.

argt(Arguments,KeyValueTermList) :-
	is_list(KeyValueTermList),
	findall(KeyValueTerm,(member(KeyValueTerm,KeyValueTermList),argt(Arguments,KeyValueTerm)),NewList),
	KeyValueTermList = NewList.

fixme('implement all these other argXXX methods eventually, but not necessary to finish our important system now').

%% argtm(Arguments,KeyMultipleValueTerm) :-
%% 	%% argt(Args,domain(Domain)).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argtm(Arguments,KeyMultipleValueTermList) :-
%% 	%% argt(Args,[domain(Domain),problem(Problem)]).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

argl(Arguments,Key,Value) :-
	arglHelper(Arguments,Terms),
	view([terms,Terms]),
	Term =.. [Key,Value],
	argt(Terms,[Term]).

arglHelper([Key,Value],[Term]) :-
	Term =.. [Key,Value].
arglHelper([Key,Value|RestArguments],[Term|RestResults]) :-
	Term =.. [Key,Value],
	arglHelper(RestArguments,RestResults).

%% argst(Arguments,KeyValueTerm) :-
%% 	%% argt(Args,domain(Domain)).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argst(Arguments,KeyValueTermList) :-
%% 	%% argt(Args,[domain(Domain),problem(Problem)]).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argstm(Arguments,KeyMultipleValueTerm) :-
%% 	%% argt(Args,domain(Domain)).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argstm(Arguments,KeyMultipleValueTermList) :-
%% 	%% argt(Args,[domain(Domain),problem(Problem)]).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argsl(Arguments,Key,Value) :-
%% 	%% arg(Args,domain,Domain).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.

%% argsl(Arguments,KeyValueList) :-
%% 	%% arg(Args,[domain,Domain,problem,Problem]).
%% 	findall([A,B],append([A,B],Rest,Arguments),Pairs),
%% 	view([pairs,Pairs]),
%% 	true.


testArgT1(Args) :-
	argt(Args,verb(Verb)),
	view([verb,Verb]).

testArgT2(Args) :-
	argt(Args,[domain(Domain),problem(Problem)]),
	view([domain,Domain,problem,Problem]).

%% testArgTM1(Args) :-
%% 	argtm(Args,verb(Verb)),
%% 	view([verb,Verb]).

%% testArgTM2(Args) :-
%% 	argtm(Args,[domain(Domain),problem(Problem)]),
%% 	view([domain,Domain,problem,Problem]).

%% testArgL1(Args) :-
%% 	argl(Args,verb,Verb),
%% 	view([verb,Verb]).

%% testArgL2(Args) :-
%% 	argl(Args,[domain,Domain,problem,Problem]),
%% 	view([domain,Domain,problem,Problem]).

%% testArgST1(Args) :-
%% 	argst(Args,verb(Verb)),
%% 	view([verb,Verb]).

%% testArgST2(Args) :-
%% 	argst(Args,[domain(Domain),problem(Problem)]),
%% 	view([domain,Domain,problem,Problem]).

%% testArgSTM1(Args) :-
%% 	argstm(Args,verb(Verb)),
%% 	view([verb,Verb]).

%% testArgSTM2(Args) :-
%% 	argstm(Args,[domain(Domain),problem(Problem)]),
%% 	view([domain,Domain,problem,Problem]).

%% testArgSL1(Args) :-
%% 	argsl(Args,verb,Verb),
%% 	view([verb,Verb]).

%% testArgSL2(Args) :-
%% 	argsl(Args,[domain,Domain,problem,Problem]),
%% 	view([domain,Domain,problem,Problem]).

testArgs :-
	testArgT1([verb([test,1])]),
	testArgT2([domain([test,2]),problem([test,2])]).

	%% testArgTM1([verb(test,1)]),
	%% testArgTM2([domain(test,2),problem(test,2)]),
	%% testArgL1([verb,[test,2]]),
	%% testArgL2([domain,[test,2],problem,[test,2]]),
	%% testArgST1(verb([test,1])),
	%% testArgST2(domain([test,2]),problem([test,2])),
	%% testArgSTM1(verb(test,1)),
	%% testArgSTM2(domain(test,2),problem(test,2)),
	%% testArgSL1(verb,[test,2]),
	%% testArgSL2(domain,[test,2],problem,[test,2]).

%% :- testArgs.

%% see about a system that does like:
%% argt(Arguments,[Notification,AlexaDevice]) instead of
%% argt(Arguments,[notification(Notification),alexaDevice(AlexaDevice)]),
%% but uses somehow the variables names themselves

:- module(test_phil,
  [test_phil/0,test_all/0,test_par/0,test_stru/0,test_inf/0]).
:- use_module(library(plunit)).

test_phil:-
  test_all.

test_all:-
  par(P),
  stru(S),
  inf(I),
  append(P,S,A1),
  append(I,A1,A),
  run_tests(A).

test_par:-
  par(P),
  run_tests(P).

test_stru:-
  stru(S),
  run_tests(S).


test_inf:-
  inf(I),
  run_tests(I).

inf([uwcsekeys]).

stru([bongard]). 

par([bongard,bongardkeys]).





/*
:- begin_tests(uwcse, []).
:-ensure_loaded(library(examples/uwcse)).
:-use_module(library(phil_test/phil_test)).

test(inference_hplp):-
inference_hplp(advisedby(harry, ben),ai,Prob,Circuit),
writeln('Result:'),
atomic_list_concat(['\nProbability=',Prob,'\nCircuit=',Circuit],St),
writeln(St),
atomic_list_concat(['Expected:\nProbability =', 0.8750801664,'\nCircuit =', or([and([0, or([and([2])])]), and([0, or([and([2])])]), and([0, or([and([2])])]), and([0, or([and([2])])]), and([1]), and([1])])],St1),
writeln(St1). 

:- end_tests(uwcse).
*/


:- begin_tests(uwcsekeys, []).
:-ensure_loaded(library(examples/uwcsekeys)).
:-use_module(library(phil_test/phil_test)).

test(inference_hplp):-
inference_hplp(advisedby(harry, ben),ai,Prob),
writeln('Result:'),
atomic_list_concat(['\nProbability=',Prob],St),
writeln(St),
atomic_list_concat(['Expected:\nProbability =', 0.8750801664],St1),
writeln(St1).

:- end_tests(uwcsekeys).



:- begin_tests(bongard, []).
:-ensure_loaded(library(examples/bongard)).
:-use_module(library(phil_test/phil_test)).

test(induce_hplp_par):-
induce_hplp_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(pos:0.17765741024654796:-circle(_154),in(_158,_154)),(pos:0.09591918413587362:-circle(_154),triangle(_158)),(pos:0.12036384245667597:-triangle(_154),circle(_158))]).

test(induce_hplp):-
induce_hplp([all],P),test_hplp(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =',
"[(pos:0.16270851085553845:-triangle(_136),hidden_1(_136)),(pos:0.4474931750444961:-triangle(_138),hidden_2(_138)),(pos:0.19806104560889168:-triangle(_140)),(pos:0.008787147207427997:-circle(_142)),(pos:0.3152720388399355:-triangle(_190),hidden_3(_190)),(pos:0.1640712959029497:-triangle(_192),hidden_4(_192)),(pos:0.44963975957404595:-triangle(_194),hidden_5(_194)),(pos:0.0039465760210714304:-square(_198)),(pos:0.2920280586393367:-triangle(_260),hidden_6(_260)),(pos:0.19200732087999292:-triangle(_262),hidden_7(_262)),(pos:0.0662228340227013:-square(_264),hidden_8(_264)),(hidden_1(_136):0.0006472774266847546:-in(_142,_136)),(hidden_1(_136):0.10926826869890274:-config(_136,down)),(hidden_2(_138):0.9992263430007895:-in(_140,_138),hidden_2_1(_140,_138)),(hidden_2(_138):0.10089008831781142:-config(_138,down)),(hidden_3(_190):0.7151752015444297:-in(_190,_200)),(hidden_3(_190):0.08494521335770758:-config(_190,up)),(hidden_4(_192):0.002790690870336442:-in(_198,_192)),(hidden_4(_192):0.14285926057566703:-config(_192,down)),(hidden_5(_194):0.9989916404526972:-in(_196,_194),hidden_5_1(_196,_194)),(hidden_5(_194):0.10788242443608247:-config(_194,down)),(hidden_6(_260):0.6580622914189234:-in(_260,_266)),(hidden_6(_260):0.08473152062711986:-config(_260,up)),(hidden_7(_262):0.0315840554855127:-config(_262,up)),(hidden_8(_264):7.584182782736883e-14:-in(_264,_268)),(hidden_2_1(_140,_138):0.5001937079404949:-config(_140,down)),(hidden_5_1(_196,_194):0.500252470981427:-config(_196,down))]",
'\nLL =', -161.40547689883218,
'\nAUCROC =', 0.9127012310606061,
'\nAUCPR =', 0.8673603662785241],St1),
writeln(St1).

:- end_tests(bongard).





:- begin_tests(bongardkeys, []).
:-ensure_loaded(library(examples/bongardkeys)).
:-use_module(library(phil_test/phil_test)).

test(in):-
in(P),test_hplp(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =', "[(pos:0.197575:-circle(_1680),in(_1684, _1680)), (pos:0.000303421:-circle(_1680),triangle(_1684)), (pos:0.000448807:-triangle(_556),circle(_560))]",
'\nLL =', -709.4598740769318,
'\nAUCROC =',0.6783558238636364,
'\nAUCPR =', 0.5157498267172048],St1),
writeln(St1).

/*
test(induce_hplp_par):-
  induce_hplp_par([train],P),
  writeln('Result:'),
  writeln(P),
  writeln('Expected:'),
  writeln([(pos:0.17765741024654796:-circle(_3598),in(_3602,_3598)),(pos:0.09591918413587362:-circle(_3598),triangle(_3602)),(pos:0.12036384245667597:-triangle(_3598),circle(_3602))]).
*/
:- end_tests(bongardkeys).


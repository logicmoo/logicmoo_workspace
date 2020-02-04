
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

par([bongard,bongardkeys]).

stru([bongard]).



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
writeln([(pos:0.18175243160838334:-circle(_218), in(_222, _218)),  (pos:0.1650748334232293:-circle(_218), triangle(_222)),  (pos:0.15476016853764582:-triangle(_218), circle(_222))]).

test(induce_hplp):-
induce_hplp([all],P),test_hplp(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =',
"[(pos:0.18636350341727298:-triangle(_1270),hidden_1(_1270)),(pos:0.7758937037724625:-triangle(_1272),hidden_2(_1272)),(pos:0.1263338461486964:-triangle(_1274)),(pos:0.00080673464681263:-circle(_1276)),(pos:0.49867916038619536:-triangle(_1324),hidden_3(_1324)),(pos:0.14484825047873356:-triangle(_1326),hidden_4(_1326)),(pos:0.6280341043666394:-triangle(_1328),hidden_5(_1328)),(pos:1.7716167242270878e-6:-square(_1332)),(pos:0.384444724632718:-triangle(_1394),hidden_6(_1394)),(pos:0.6294782571470092:-triangle(_1396),hidden_7(_1396)),(pos:0.839536566838233:-triangle(_1400),hidden_8(_1400)),(hidden_1(_1270):0.07350824866516653:-in(_1276,_1270)),(hidden_1(_1270):0.022599380350125244:-config(_1270,down)),(hidden_2(_1272):0.9986985576673145:-in(_1274,_1272),hidden_2_1(_1274,_1272)),(hidden_2(_1272):0.09926469967818095:-config(_1272,down)),(hidden_3(_1324):0.6897147989831:-in(_1324,_1334)),(hidden_3(_1324):0.090282456674903:-config(_1324,up)),(hidden_4(_1326):0.06108838636370634:-in(_1332,_1326)),(hidden_4(_1326):0.05886657005662841:-config(_1326,down)),(hidden_5(_1328):0.9990776694316461:-in(_1330,_1328),hidden_5_1(_1330,_1328)),(hidden_5(_1328):0.10600474430922273:-config(_1328,down)),(hidden_6(_1394):0.7409218713027834:-in(_1394,_1404)),(hidden_6(_1394):0.030297234989035332:-config(_1394,down)),(hidden_7(_1396):0.9990559405379036:-in(_1398,_1396),hidden_7_1(_1398,_1396)),(hidden_7(_1396):0.1038592859990984:-config(_1396,down)),(hidden_8(_1400):0.997541800088097:-in(_1402,_1400),hidden_8_1(_1402,_1400)),(hidden_8(_1400):0.1207652318716478:-config(_1400,up)),(hidden_2_1(_1274,_1272):0.5003261442441411:-config(_1274,down)),(hidden_5_1(_1330,_1328):0.500230929703047:-config(_1330,down)),(hidden_7_1(_1398,_1396):0.5002363274533916:-config(_1398,up)),(hidden_8_1(_1402,_1400):0.5006173467273577:-config(_1402,up))]",
'\nLL =', -90.8925,
'\nAUCROC =', 0.893997,
'\nAUCPR =', 0.855815],St1),
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
'\nLL =', -250.33054186204927,
'\nAUCROC =',0.7626436781609195,
'\nAUCPR =', 0.5615465293941269],St1),
writeln(St1).

test(induce_par):-
  induce_hplp_par([train],P),
  writeln('Result:'),
  writeln(P),
  writeln('Expected:'),
  writeln([(pos:0.18175243160838334:-circle(_218), in(_222, _218)),  (pos:0.1650748334232293:-circle(_218), triangle(_222)),  (pos:0.15476016853764582:-triangle(_218), circle(_222))]).
:- end_tests(bongardkeys).


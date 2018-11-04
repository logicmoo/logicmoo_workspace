:- module(test_bddem,
  [test/0]).
:- use_module(library(plunit)).

test:-
    run_tests([coin,
    coinmsw,
    dice,
    epidemic,
    earthquake,
    sneezing,
    trigger,
    light,
    threesideddice,
    bloodtype,
    mendel,
    coin2,
    simpson,
    viral,
    uwcse,
    path,
    pathdb,
    multiple_paths_simple,
    multiple_paths,
    abd1,
    abd2,
    abd3,
    map1,
    map_es3,
    map_es21,
    map_es2,
    map_es2map,
    map_es2map1,
    pitavit_win,
    pitavit_hmm,
    pitavit_coin,
    pitavit_mendel,
    meta,
    pcfg,
    var_objdb,
    card
    ]).


:- begin_tests(prob, []).

:-ensure_loaded(library(bddem)).

test(one):-
  init_test(Env),add_var(Env,[0.4,0.6],0,V),equality(Env,V,0,BDD),
  ret_prob(Env,BDD,P),P=:=0.4.

:- end_tests(coin).

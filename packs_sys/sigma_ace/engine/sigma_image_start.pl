

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:-was_indexed(sigmaCache(1)).
:-was_indexed(sigmaCache(1,1)).
:-was_indexed(sigmaCache(1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1)).
:-was_indexed(sigmaCache(1,1,1,1,0)).
:-was_indexed(sigmaCache(1,0,1,1,1,0)).
:-was_indexed(sigmaCache(1,1,0,0,1,1,0,0,0)).

:-dynamic(sigmaCache/1).
:-dynamic(sigmaCache/2).
:-dynamic(sigmaCache/3).
:-dynamic(sigmaCache/4).
:-dynamic(sigmaCache/5).
:-dynamic(sigmaCache/6).
:-dynamic(sigmaCache/9).



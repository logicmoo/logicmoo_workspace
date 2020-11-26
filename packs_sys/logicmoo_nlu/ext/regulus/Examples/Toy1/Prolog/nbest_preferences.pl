
:- module(nbest_features,
	  [feature_weight/2,
	   feature_value_for_record/3
	  ]).

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%---------------------------------------------------------------
 
% Place in the N-best list - lower number is better
feature_weight(rank, -1).

%---------------------------------------------------------------

feature_value_for_record(rank, Record, Score) :-
	member(rank=Rank, Record),
	Score = Rank,
	!.

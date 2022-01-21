%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================
%%% randoms.pl
%%% Provides pseudo-random numbers, using the interface to 
%%% C provided by Sicstus.

:- my_use_module(library(random)).


:- dynamic old_seed/1.

seed(S) :- getrand(S).



% RESET_RANDOM(+S)
% Resets the random state to a particular state.
% Doesn't currently work with sicstus interface.
reset_random(S) :- setrand(S).

% RESET_RANDOM
% Ensures the random generator it is working.
reset_random :- random(_).


%%% RECORD_SEED
%%% Records the random state, so a next run of the
%%% system can be exactly the same as a previous run.

record_seed :- 
	seed(S), 
	retractall(old_seed(_)), 
	assert(old_seed(S)).


% RECOVER_RANDOM
% Resets state to whichever was last recorded.
recover_random :- old_seed(S), reset_random(S).

% Writes a comment to a file that this seed was used.
write_old_seed(CommentChar) :- 
	old_seed(Seed), !,
	format("~n~w RANDOM SEED: ~w~n",[CommentChar,Seed]).
write_old_seed.



%   random(-R) 
%   binds R to a new random number in [0.0,1.0)
% In library(random).

/* 
%   random(+L, +U, -R) 
%   binds R to a random integer in [L,U) when L and U are integers 
%   (note that U will NEVER be generated), or to a random floating 
%   number in [L,U] otherwise.
% In library(random).
% For now, I include a corrected version, until they fix a bug in
% sicstus random.

random(L, U, R) :-
	integer(L), integer(U),
	random(X), !,
	R is L+integer(floor((U-L)*X)).
random(L, U, R) :-
	number(L), number(U),
	random(X), !,
	R is L+((U-L)*X).

*/


%   random_include(+L, +U, -R) 
%   binds R to a random integer in [L,U] when L and U are integers 
%   or to a random floating number in [L,U] otherwise.

random_include(L, U, R) :-
	integer(L), integer(U), !,
	U1 is U + 1,
	random(L,U1,R).
random_include(L, U, R) :-
	random(L,U,R).



%%% random(+R,-N)
%%% binds N to a random integer in [1,R].
random(R,N) :-
	R1 is R + 1, 
	random(1,R1,N).



%%% random_element(+Set,-Element)
%%% Randomly returns an Element of Set (a list).

random_element(Set,Element) :-
	length(Set,Length),
	random(Length,R),
	nth(R,Set,Element).

% sicstus-version.pl has random-select from the quintus
% library.  This is necessary for the metagame system.

%   random_select(?Elem, ?List, ?Rest)
%   unifies Elem with a random element of List and Rest with all the
%   other elements of List (in order).  Either List or Rest should
%   be proper, and List should/will have one more element than Rest.
%   Takes O(N) time (average and best case).

% random_permute(List1,List2).
% Randomly permutes List1 into List2.
% If random_select takes O(N) time, this routine takes
% O(N^2) time.  This could be improved to O(N) time
% using arrays.  
random_permute([],[]) :- !.
random_permute(List1,[Item|Rest]) :- 
	random_select(Item,List1,List),
	random_permute(List,Rest).

% RANDOM_BAGOF/3
% RANDOM_SETOF/3
% RANDOM_FINDALL/3
% Like the non-random versions, but randomly 
% permutes the resulting bag/set.  

random_bagof(A,B,C) :- 
	bagof(A,B,C1),
	random_permute(C1,C).

random_setof(A,B,C) :- 
	setof(A,B,C1),
	random_permute(C1,C).

random_findall(A,B,C) :- 
	findall(A,B,C1),
	random_permute(C1,C).


%%% random_arg(+Term,-Element)
%%% Randomly returns an Arg of a Term.
%%% Note: When only two args, because of the 
%%% simple random function, it might alternate
%%% between the two.
random_arg(Term,Element) :-
	functor(Term,_Args,Arity),
	random(Arity,R),
	arg(R,Term,Element).


% random_success(+Call)
% Succeeds, with equal probability, on any successful call of Call. 
% Should not be used if Call side-effects.  

random_success(Call) :- 
	bagof(Call,Call^call(Call),Calls),
	random_element(Calls,Call).





%%% randomly_pair(+List1,+List2,-Pairings)
% Maps each in Arg1 to one in Arg2.
% If Arg2 smaller, maps as many as can. 
% Duplicates matter.
randomly_pair([],_,[]) :- !.
randomly_pair(_,[],[]) :- !.
randomly_pair([A|As],Set,[A=Elt|Pairs]) :-
	random_element(Set,Elt),
	extract(Elt,Set,Set1),
	randomly_pair(As,Set1,Pairs).



%%% random_subsets(+Count+Size,+List,-Subset)
%%% Returns Count sets of Size unique elements from List.
random_subsets(0,_,_,[]) :- !.
random_subsets(N,Size,Set,[Elt|Rest]) :-
	random_subset(Size,Set,Elt),
	N1 is N-1,
	random_subsets(N1,Size,Set,Rest).

%%% random_subset(+Size,+List,-Subset)
%%% Returns a set of Size unique elements from List.
random_subset(0,_,[]) :- !.
random_subset(Size,Set,[Elt|Rest]) :-
	random_element(Set,Elt),
	extract(Elt,Set,Set1),
	Size1 is Size - 1,
	random_subset(Size1,Set1,Rest).


%%% random_different_args(Term,Arg1,Arg2) 
%%% Returns Arg1 and Arg2, two random different Args
%%% in Term.
%%%
random_different_args(Term,Arg1,Arg2) :-
	random_arg(Term,Arg1),
	random_different_arg(Term,Arg1,Arg2).


random_different_arg(Term,ArgA,ArgB) :-
	random_different_arg(Term,ArgA,ArgA,ArgB).

random_different_arg(_Term,ArgA,ArgB,ArgB) :-
	ArgA \== ArgB, !.
random_different_arg(Term,ArgA,ArgA,ArgB) :-
	random_arg(Term,Arg1),
	random_different_arg(Term,ArgA,Arg1,ArgB).


% RANDOM_TEST(+R,+N,-Ratio)
% Counts ratio out of 100 times that N is the 
% random integer between 1 and R.
random_test(R,N,Ratio) :-
  random_test(R,N,0,0,100,Ratio).

% RANDOM_TEST(+R,+N,+SampleSize,-Ratio)
% Repeatedly (SampleSize Times), chooses a random integer between 
% 1 and R.   Counts fraction that integer N occurs.
random_test(R,N,SampleSize,Ratio) :-
  random_test(R,N,0,0,SampleSize,Ratio).

random_test(_R,_N,Tried,Found,Tried,Ratio) :-
	Ratio is Found/Tried, !.
random_test(R,N,Tried,Found,Total,Ratio) :-
	random(R,N1),
	( N = N1 
         -> Found1 is Found + 1
         ;  Found1 = Found ),
	Tried1 is Tried + 1,
	    random_test(R,N,Tried1,Found1,Total,Ratio).
  
/*
test_sample :-
	repeat, 
	sample_from_distribution(distribution([a=0.8,b=0.05,c=0.1]),
	                         Choice),
        write(Choice), nl, fail.
*/

% Distribution looks like:
%    distribution([choice1=p1,...,choiceN=pn]))
% Pis must sum to 1 (exhaustive).
% [Though pn isn't used, so it can be anything].
% Chooses from this distribution subject to these 
% probabilities.
sample_from_distribution(Dist,Choice) :-
	distribution(Dist,Options),
	random(R),
	in_prob_region(Options,R,Choice1),
	Choice1 = Choice.

distribution(distribution(Choices),Choices).

% in_prob_region([a=0.7,b=0.2,c=0.1],0.89,Choice).
in_prob_region([Choice=_Prob],_,Choice) :- !.
in_prob_region([C=P|_Choices],Prob,C) :-
	Prob < P, !.
in_prob_region([_C=P|Choices],Prob,Choice) :-
	PRest is Prob - P,
	in_prob_region(Choices,PRest,Choice).
	

% Range looks like:
%    range(Lower,Upper)
% Chooses a random number in the range [Lower,Upper],
% either integer or number, based on Lower and Upper.
sample_from_range(Range,Choice) :-
	range(Range,Min,Max),
	random_include(Min,Max,Choice).

range(range(Min,Max),Min,Max).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sampling from distributions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here distributions are represented as assoc-lists, not
% with = signs as above. 

% Distribution looks like:
%    [choice1-p1,...,choiceN-pn]
% Pis can be any non-negative numbers.
% Chooses from this distribution subject to relative 
% probabilities, each pi getting its proportion of
% the total.  
sample(Dist,Choice) :-
	pair_list(_Choices,Weights,Dist),
	sum_list(Weights,Total),
	random(0.0,Total,R),
	in_region(Dist,R,Choice1),
	Choice1 = Choice.

% sample(N,Dist,Samples).
% Result is a list of N samples (with replacement) from a distribution. 
sample(0,_,[]) :- !.
sample(N,Dist,[S|Samples]) :- 
	N > 0,
	N1 is N-1,
	sample(Dist,S),
	sample(N1,Dist,Samples).

%%% sample_subsets(+Count+Size,+List,-Subset)
%%% Returns Count sets of Size elements sampled from a 
%%% distribution. 
sample_subsets(0,_,_,[]) :- !.
sample_subsets(N,Size,Dist,[Elt|Rest]) :-
	sample(Size,Dist,Elt),
	N1 is N-1,
	sample_subsets(N1,Size,Dist,Rest).



test_sample :-
	repeat, 
	sample([a-5,b-10,d-15],Choice),
        write(Choice), nl, fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random seed setting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

randomize(N) :- 
	format('Using random seed #~p.~n',[N]),
	randomize0(N).

randomize0(1) :- setrand(random(2260,5202,18078,-111865839)).
randomize0(2) :- setrand(random(1676,2152,14938,-111865839)).
randomize0(3) :- setrand(random(14918,9840,11226,-111865839)).
randomize0(4) :- setrand(random(11477,9180,488,-111865839)).
randomize0(5) :- setrand(random(27112,8989,12856,-111865839)).
randomize0(6) :- setrand(random(27949,24755,16306,-111865839)).
randomize0(7) :- setrand(random(3126,20129,24910,-111865839)).
randomize0(8) :- setrand(random(21946,18049,2077,-111865839)).
randomize0(9) :- setrand(random(26016,4946,13012,-111865839)).
randomize0(10) :- setrand(random(18553,19429,25736,-111865839)).
randomize0(test) :- setrand(random(1734,10872,10679,-111865839)).

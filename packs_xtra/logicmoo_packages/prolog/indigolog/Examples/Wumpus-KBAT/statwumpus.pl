%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Examples/Wumpus/statwumpus.pl
%
%       Module for computing statistics in the Wumpus World
%
%  AUTHOR : Sebastian Sardina (2005)
%  email  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             April, 2005
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
% 
%                          All Rights Reserved
% 
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile wumpus_run/6.

% A - get_stat_result(?ID, ?Conf,-Result): TOP-LEVEL predicate
% A - get_stat_result(?Conf,-Result)     : TOP-LEVEL predicate
% Given a test-set ID, it computes an aggregation result in Result
get_stat_result(IDRun,[Size,PPits,NoGolds],Result) :-
	setof([Size,PPits,NoGolds],IDRun^InitGrid^FinalGrid^H^Time^
		wumpus_run(IDRun,[Size,PPits,NoGolds],InitGrid,FinalGrid,H,Time),L),
	member([Size,PPits,NoGolds],L),
	findall(R,(wumpus_run(IDRun,[Size,PPits,NoGolds],InitGrid,FinalGrid,H,Time),
		   evaluate_run(InitGrid,FinalGrid,H,Time,R)), ListResultsRuns),
	evaluate_many_runs(ListResultsRuns, Result).
get_stat_result([Size,PPits,NoGolds],Result) :-
	setof([Size,PPits,NoGolds],IDRun^InitGrid^FinalGrid^H^Time^
		wumpus_run(IDRun,[Size,PPits,NoGolds],InitGrid,FinalGrid,H,Time),L),
	member([Size,PPits,NoGolds],L),
	findall(R,(wumpus_run(_,[Size,PPits,NoGolds],InitGrid,FinalGrid,H,Time),
		   evaluate_run(InitGrid,FinalGrid,H,Time,R)), ListResultsRuns),
	evaluate_many_runs(ListResultsRuns, Result).
	
% B - evaluate_many_runs/2: Evaluate many runs
% +ListRRuns: list of runs
% -Result: evaluation result of all the runs together
evaluate_many_runs(ListRRuns, Result) :-
	length(ListRRuns, NoRuns),
	flatten(ListRRuns,ListRuns),
	findall(R1, member(wumpusDead(dead,R1),ListRuns),RR1),length(RR1,LRR1),
	findall(R2, member(robotDead(dead,R2),ListRuns),RR2),length(RR2,LRR2),
	findall(R3, (member(noGolds(N,R3),ListRuns),N>0),RR3),length(RR3,LRR3),
	findall(R4, (member(climbed(R4),ListRuns),R4>0),RR4),length(RR4,LRR4),
	findall(R5, member(noPits(R5),ListRuns),RR5),average(RR5,ARR5),
	findall(R6, member(totalReward(R6),ListRuns),RR6),average(RR6,ARR6),
	findall(R7, member(actions(_,R7),ListRuns),RR7),average(RR7,ARR7),
	findall(R8, member(actions(R8,_),ListRuns),RR8),average(RR8,ARR8),
	findall(R9, (member(impossible(R9),ListRuns),R9=1),RR9),length(RR9,LRR9),
	findall(R10, (member(ListRun,ListRRuns),member(impossible(0),ListRun),
		member(totalReward(R10),ListRun)),RR10),average(RR10,ARR10),	
	findall(R11, (member(ListRun,ListRRuns),member(impossible(0),ListRun),
		member(actions(R11,_),ListRun)),RR11),average(RR11,ARR11),	
	findall(R12, member(time(R12),ListRuns),RR12),average(RR12,ARR12),
	findall(R13, (member(ListRun,ListRRuns),member(impossible(0),ListRun),
		member(time(R13),ListRun)),RR13),average(RR13,ARR13),	
	Result=[noRuns(NoRuns),impossible(LRR9),
		noWumpusDead(LRR1),noRobotDead(LRR2),noGotGold(LRR3),noClimbed(LRR4),
		avgPits(ARR5),avgReward(ARR6),avgRewardExcludingImposs(ARR10),
		avgCostActions(ARR7),avgNoActions(ARR8),
		avgNoActionsExcludingImposs(ARR11),
		avgTime(ARR12), avgTimeExcludingImposs(ARR13)
		].

% C - evaluate_run/5: Evaluate one run
% +IGrid = Initial grid configuration
% +FGrid = Final grid configuration
% +H = List of actions performed
% +Time = time is seconds
% -Result = A list representing the evaluation
evaluate_run(IGrid,FGrid,H,Time, Result) :-
		% Obtain data from the input grid IGrid
	%member(robot(IRX,IRY,IRD,INA,IRS), IGrid),
	member(wumpus(IWX,IWY,_), IGrid),
	member(golds(ILGolds), IGrid),
	member(pits(ILPits), IGrid),
		% Obtain data from the final grid FGrid
	member(robot(FRX,FRY,_,_,FRS), FGrid),
	member(wumpus(_,_,FWS), FGrid),
	member(golds(FLGolds), FGrid),
	findall(CA,(member(A,H),actionCost(A,CA)), ListActionCosts), % Actions costs
	sumlist(ListActionCosts, CostActions),
	length(H,LH),		% Number of actions
	length(ILPits,LP),	% Number of pits
	(FRS=alive -> CostDie is 0 ; CostDie is -1000),		% Agent dies
	(FWS=alive -> CostDieW is 0 ; CostDieW is 100),		% Wumpus dies
	length(ILGolds,L1),
	length(FLGolds,L2),
	GoldsObtained is L1-L2,
	(GoldsObtained>0 -> RewardGold is 1000 ; RewardGold is 0),
	((member(climb,H), FRX=1, FRY=1) -> Climbed is 50 ; Climbed is 0),
	(impossible([(IWX,IWY)|ILPits]) -> Imp=1 ; Imp=0),
	sumlist([CostActions,CostDie,CostDieW,RewardGold,Climbed],TReward),
	Result=[totalReward(TReward),time(Time),impossible(Imp),
		noPits(LP),actions(LH,CostActions),
	 	robotDead(FRS,CostDie),wumpusDead(FWS,CostDieW),
	 	noGolds(GoldsObtained,RewardGold),climbed(Climbed)].

% L is a list of locations (X,Y): wumpus or pits
% the predicate states that under such conf. the problem is not solvable
impossible(L) :- member((1,2),L).
impossible(L) :- member((2,1),L).
impossible(L) :- member((2,3),L), member((3,1),L).
impossible(L) :- member((2,3),L), member((3,2),L).


% Definition of the cost of actions
actionCost(A,0) :- member(A,[smell, senseBreeze, senseGold]).
actionCost(A,C) :- member(A,[moveFwd,turn,pickGold,climb,enter]), C is -1.
actionCost(A,C) :- member(A,[shootFwd]), C is -10.

	
% Sum a list of numbers
sumlist([],N,N).
sumlist([N|R],S,T) :-
	S2 is S+N,
	sumlist(R,S2,T).

average(L, R) :-
	sumlist(L, Sum),
	length(L,LL),
	(LL\=0 -> R is Sum/LL ; R=none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Examples/Wumpus/statwumpus.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

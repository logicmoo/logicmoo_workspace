% Decision Theory, the Situation Calculus and Conditional Plans
% ICL axiomatization of the example from the paper.
% Copyright, David Poole, 1998.  http://www.cs.ubc.ca/spider/poole/
% (See the end for how to run this).

% ACTIONS
%    goto(Pos,Route) robot takes Route to Pos
%    pickup(X)       robot picks up X
%    unlock_door     robot unlocks the door
%    enter_lab       robot enters the lab

% Fluents
%    at(Obj,Pos,S)   Obj is at Pos in state S (and not crashed)
%    carrying(X,S)   robot is carrying X in state S
%    locked(door,S)  door is locked at state S
%    crashed(S)      robot is in a tangled mess at bottom of the stairs in S
%    prize(V,S)      robot would receive V is it stopped in S
%    resources(R,S)  robot has R resources left

% Sensing conditions
%     sense(at_key,S) robot senses that it is at the same location as the key.

:- expects_dialect(icl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             INITIAL SITUATION                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initial situation has the following probabilities
%   P(locked(door,s0)) = 0.9
%   P(at_key(r101,s0)|locked(door,s0)) = 0.7
%   P(at_key(r101,s0)|unlocked(door,s0)) = 0.2
%   ( from which we conclude P(at_key(r101,s0))=0.65

random([locked(door,s0):0.9,unlocked(door,s0):0.1]).
random([at_key_lo(r101,s0):0.7,at_key_lo(r123,s0):0.3]).
random([at_key_unlo(r101,s0):0.2,at_key_unlo(r123,s0):0.8]).

at(key,R,s0) <- at_key_lo(R,s0) & locked(door,s0).
at(key,R,s0) <- at_key_unlo(R,s0) & unlocked(door,s0).

% initially the robot is at room 111.
at(robot,r111,s0) <- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             LOCATION                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The robot reaches its desitination as long as it does not fall down
% the stairs and as long as it has enough resources.

at(robot,To,do(goto(To,Route),S)) <-
    at(robot,From,S) &
    path(From,To,Route,no,Cost) &        % not risky
    resources(R,S) &
    R >= Cost.

at(robot,To,do(goto(To,Route),S)) <-
    at(robot,From,S) &
    path(From,To,Route,yes,Cost) &       % is risky & don't fall down
    would_not_fall_down_stairs(S) &
    resources(R,S) &
    R >= Cost.

at(robot,Pos,do(A,S)) <-
   ~ gotoaction(A) &
   at(robot,Pos,S).

% non-robots remain where they were unless they are being carried
at(X,P,S) <-
   X \= robot &
   carrying(X,S)&
   at(robot,P,S).

at(X,Pos,do(A,S)) <-
   X \= robot &
   ~ carrying(X,S)&
   at(X,Pos,S).

gotoaction(goto(_,_)) <- true.

% Whenever the robot goes past the stairs there is a 10% chance 
% that it will fall down the stairs, in which case it has crashed
% permanently.
% N.B. we assume when the robot has crashed, it is not "at" anywhere.

random([would_fall_down_stairs(S):0.1,would_not_fall_down_stairs(S):0.9]).

crashed(do(_A,S)) <- crashed(S).
crashed(do(A,S)) <- risky(A,S) & would_fall_down_stairs(S).



% path(From,To,Route,Risky,Cost)
%     Risky means whether it has to go past the stairs
path(r101,r111,direct,yes,10) <- true.
path(r101,r111,long,no,100) <- true.
path(r101,r123,direct,yes,50) <- true.
path(r101,r123,long,no,90) <- true.
path(r101,door,direct,yes,50) <- true.
path(r101,door,long,no,70) <- true.

path(r111,r101,direct,yes,10) <- true.
path(r111,r101,long,no,100) <- true.
path(r111,r123,direct,no,30) <- true.
path(r111,r123,long,yes,90) <- true.
path(r111,door,direct,no,30) <- true.
path(r111,door,long,yes,70) <- true.

path(r123,r101,direct,yes,50) <- true.
path(r123,r101,long,no,90) <- true.
path(r123,r111,direct,no,30) <- true.
path(r123,r111,long,yes,90) <- true.
path(r123,door,direct,no,20) <- true.
path(r123,door,long,yes,100) <- true.

path(door,r101,direct,yes,50) <- true.
path(door,r101,long,no,70) <- true.
path(door,r111,direct,no,30) <- true.
path(door,r111,long,yes,70) <- true.
path(door,r123,direct,no,20) <- true.
path(door,r123,long,yes,100) <- true.


risky(goto(To,Route),S) <-
    path(From,To,Route,yes,_) &
    at(robot,From,S).


%    carrying(X,S) means robot is carrying X in state S

carrying(key,do(pickup(key),S)) <-
    at(robot,P,S) &
    at(key,P,S) &
    pickup_succeeds(S).

carrying(key,do(A,S)) <-
    carrying(key,S) &
    A \= putdown(key) &
    A \= pickup(key) &
    keeps_carrying(key,S).

% 88% chance that a legal pickup will succeed and 
% 5% chance that the robot will drop the key

random([pickup_succeeds(S):0.88, pickup_fails(S):0.12]).
random([keeps_carrying(key,S):0.95, drops(key,S):0.05]).


unlocked(door,do(unlock_door,S)) <-
   at(robot,door,S) &
   at(key,door,S).
unlocked(door,do(A,S)) <-
   ~ ( at(robot,door,S) & at(key,door,S)) &   % ensures rules are dijoint
   unlocked(door,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             UTILITY                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% utility is the prize plus the resources remaining
utility(V,S) <- 
    prize(P,S) &
    resources(R,S) &
    V is R+P.

prize(1000,S) <- in_lab(S).
prize(-1000,S) <- crashed(S).
prize(0,S) <- ~ in_lab(S) & ~ crashed(S).

in_lab(do(enter_lab,S)) <-
   at(robot,door,S) &
   unlocked(door,S).

resources(200,s0) <- true.
resources(RR,do(goto(To,Route),S)) <-
    at(robot,From,S) &
    path(From,To,Route,Risky,Cost) &
    resources(R,S) &
    RR is R-Cost.

resources(R,do(A,S)) <-
    crashed(S) &
    resources(R,S).

resources(RR,do(A,S)) <-
    ~ crashed(S) &
    ~ gotoaction(A) &
    resources(R,S) &
    RR is R-10.           % every other action costs 10


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             SENSING                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sense(at_key,S) <-
   at(robot,P,S) &
   at(key,P,S) &
   sensor_true_pos(S).
sense(at_key,S) <-
   at(robot,P1,S) &
   at(key,P2,S) &
   P1 \= P2 &
   sensor_false_neg(S).

% sensor to detect if at the same location as the key is noisy.
% It has a 3% false positive rate and an 8% false negative rate

random([sensor_true_pos(S):0.92, sensor_false_neg(S):0.08]).
random([sensor_true_neg(S):0.97, sensor_false_pos(S):0.03]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             TO RUN THIS                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This has only been tested with Sicstus Prolog, although it is
% reasonably standard Edinburgh prolog

% compile('icl_int.tex').
% thcons('dtp.pl').

% Here is a simple plan:
explain(utility(V,do(enter_lab,do(goto(door,direct),s0))),[],[]).

%Here is the explanations of the sensing
example_query(explain(sense(at_key,do(goto(r101,direct), s0)),[],[])).

% The following two generate the explanations needed to determine the
% expected utility of the plan in the paper:
example_query(explain((sense(at_key,do(goto(r101,direct), s0)) & utility(V,do(enter_lab, do(unlock_door, do(goto(door,long), do(pickup(key), do(goto(r101,direct), s0))))))) ,[],[])).

example_query(explain((~ sense(at_key,do(goto(r101,direct), s0)) & utility(V,do(enter_lab, do(unlock_door, do(goto(door,direct), do(pickup(key), do(goto(r123,direct), do(goto(r101,direct), s0)))))))) ,[],[])).



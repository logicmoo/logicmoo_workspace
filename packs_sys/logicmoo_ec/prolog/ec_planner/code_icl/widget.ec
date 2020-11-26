% widget example from "The Independent Choice Logic for modelling
% multiple agents under uncertainty".
% based on the example of Draper, Hanks and Weld 94
% Copyright 1997, David Poole, all rights reserved.

% NATURE MODULE
utility(1) <- do(agent1,notify,T) & painted(T) & processed(T).

painted(T+1) <- do(agent1,paint,T) & painting_works & ~ done(T). 
      % done prevents us from shipping then painting!
painted(T+1) <- painted(T).
      % painted persists

random([painting_doesnt_work:0.50,painting_works:0.50]).

time_arg(done, 1).
time_arg(shipped, 1).

done(T) <- shipped(T).
done(T) <- rejected(T).

blemished(T) <- flawed(T) & ~ painted(T).
flawed(T+1) <- flawed(T).
      % flawed persists

processed(T) <- rejected(T) & flawed(T).
processed(T) <- shipped(T) & ~ flawed(T).

shipped(T) <- do(agent1,ship,T).
shipped(T+1) <- shipped(T).

rejected(T) <- do(agent1,reject,T).
rejected(T+1) <- rejected(T).

sense(agent1,blemish,bad,T+1) <- do(agent1,inspect,T) & blemished(T) & ~ falsepos(T).
sense(agent1,blemish,ok,T+1) <- do(agent1,inspect,T) & blemished(T) & falsepos(T).
sense(agent1,blemish,ok,T+1) <- do(agent1,inspect,T) & ~ blemished(T).
sense(agent1,blemish,none,T+1) <- ~ do(agent1,inspect,T).

random([falsepos(T):0.1,notfalsepos(T):0.9]). % this assumes that tests are
          % independent.
random([flawed(0):0.3,unflawed(0):0.7]).

% AGENT MODULE
% perceptible: sense(agent1,blemished,Val,T)
% recallable: bel(agent1,ok,T), bel(agent1,bad,T)
% actions: do(agent1,reject,T), do(agent1,ship,T), do(agent1,notify,T), do(agent1,paint,T), do(agent1,inspect,T)

bel(agent1,bad,T) <- sense(agent1,blemish,bad,T).
bel(agent1,bad,T+1) <- bel(agent1,bad,T) & ~ sense(agent1,blemish,bad,T+1).
bel(agent1,ok,T) <- sense(agent1,blemish,ok,T) & ~ bel(agent1,bad,T).
bel(agent1,ok,T+1) <- bel(agent1,ok,T) & ~ sense(agent1,blemish,ok,T+1) & ~ sense(agent1,blemish,bad,T+1).
% bel(agent1,ok,0) <- true.
% bel(agent1,ok,T+1) <- bel(agent1,ok,T) & ~ sense(agent1,blemish,bad,T).
		  
% this represents badpersists (if you ever belive it is bad, no sensor
% readings change your mind).
% challenge: represent controllable([bel_latest,badpersists,okpersists])

do(agent1,reject,T) <- rejectORship(T) & rejectifOK(T) & bel(agent1,ok,T).
do(agent1,reject,T) <- rejectORship(T) & rejectifBAD(T) & ~ bel(agent1,ok,T).
do(agent1,ship,T) <- rejectORship(T) & shipifOK(T) & bel(agent1,ok,T).
do(agent1,ship,T) <- rejectORship(T) & shipifBAD(T) & ~ bel(agent1,ok,T).

controllable([do(agent1,notify,T), do(agent1,paint,T), do(agent1,inspect,T), do(agent1,nothing,T), rejectORship(T)]).
controllable([rejectifOK(T),shipifOK(T)]).
controllable([rejectifBAD(T),shipifBAD(T)]).

% TRY THE FOLLOWING QUERIES:
explain(utility(1), [do(agent1,nothing,0), do(agent1,paint,0+1), rejectORship(0+1+1), shipifOK(0+1+1), rejectifBAD(0+1+1), do(agent1,notify,0+1+1+1)], []).
% explain(utility(1), [do(agent1,nothing,0), do(agent1,paint,0+1), rejectORship(0+1+1), shipifOK(0+1+1), shipifBAD(0+1+1), do(agent1,notify,0+1+1+1)], []).
explain(utility(1), [do(agent1,inspect,0), do(agent1,paint,0+1), rejectORship(0+1+1), shipifOK(0+1+1), rejectifBAD(0+1+1), do(agent1,notify,0+1+1+1)], []).
explain(utility(1), [do(agent1,inspect,0), do(agent1,inspect,0+1), do(agent1,paint,0+1+1), rejectORship(0+1+1+1), shipifOK(0+1+1+1), rejectifBAD(0+1+1+1), do(agent1,notify,0+1+1+1+1)], []).
explain(utility(1), [do(agent1,inspect,0), do(agent1,inspect,0+1), rejectORship(0+1+1+1), shipifOK(0+1+1+1), rejectifBAD(0+1+1+1), do(agent1,notify,0+1+1+1+1)], []).


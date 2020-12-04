% This is the example from "Abducing through negation as failure:
% stable models in the independent choice logic".
% Copyright 1997. All Rights reserved. David Poole.

:- expects_dialect(icl).

carrying(Robot,key,s(T)) <-
   do(Robot,pickup(key),T) &
   at(Robot,Pos,T) &
   at(key,Pos,T) &
   pickup_succeeds(T).

random([pickup_succeeds(T): 0.7,pickup_fails(T):0.3]).

carrying(Robot,key,s(T)) <-
   ~ do(Robot,pickup(key),T) &
   ~ do(Robot,putdown(key),T) &
   carrying(Robot,key,T) &
   ~ drops(Robot,key,T).

drops(_Robot,key,T) <-
   slippery(key,T) &
   drop_slippery_key(T).

drops(_Robot,key,T) <-
   ~ slippery(key,T) &
   fumbles_key(T).

random([drop_slippery_key(T):0.9,holds_slippery_key(T):0.1]).
random([fumbles_key(T):0.2,retains_key(T):0.8]).

at(Robot,Pos,s(T)) <-
   do(Robot,goto(Pos),T) &
   goto_succeeds(T).
at(Robot,Pos1,s(T)) <-
   do(Robot,goto(_Pos),T) &
   at(Robot,Pos1,T) &
   ~ goto_succeeds(T).
at(Robot,Pos,s(T)) <-
   ~ goto_action(Robot,T) &
   at(Robot,Pos,T).
goto_action(Robot,T) <-
   do(Robot,goto(_Pos),T).

random([goto_succeeds(T):0.93,goto_fails(T):0.07]).

at(key,Pos,T) <-
   carrying(Robot,key,T) &
   at(Robot,Pos,T).
at(key,Pos,s(T)) <-
   ~ carrying(_Robot,key,s(T)) &
   at(key,Pos,T).

slippery(key,s(T)) <-
   slippery(key,T) &
   stays_slippery(T).
%slippery(key,s(T)) <-
%   ~ slippery(key,T) &
%   becomes_slippery(T).
slippery(key,0) <-
   initially_slippery(key).

random([stays_slippery(T):0.75,stops_being_slippery(T):0.25]).
random([becomes_slippery(T):0.05,stays_unslippery(T):0.95]).
random([initially_slippery(key):0.5,initially_unslippery(key):0.5]).

% Particular scenario
do(r1bot,goto(loc1),0) <- true.
do(r1bot,pickup(key),s(0)) <- true.
%do(r1bot,pickup(key),s(s(0))) <- true.
do(r1bot,goto(loc2),s(s(0))) <- true.

at(key,loc1,0) <- true.
at(r1bot,loc0,0) <- true.
  
explain(at(r1bot,loc1,s(0)), [],[]).
explain(carrying(r1bot,key,s(s(0))), [],[]).
explain(slippery(key,s(s(0))), [],[]).
explain(slippery(key,s(0)), [],[]).
explain(slippery(key,0), [],[]).
explain(carrying(r1bot,key,s(s(s(0)))), [],[]).
explain(~ carrying(r1bot,key,s(s(s(0)))), [],[]).
explain(at(key,loc1,s(s(s(0)))), [],[]).
explain(drops(r1bot,key,s(s(0))), [],[]).

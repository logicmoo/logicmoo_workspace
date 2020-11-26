% DEC.lp

#domain fluent(F;F1;F2),event(E),time(T;T1;T2). 

time(0..maxstep).

% DEC 1
stoppedIn(T1,F,T2) :- 
   happens(E,T), T1<T, T<T2, terminates(E,F,T).

% DEC 2
startedIn(T1,F,T2) :- 
   happens(E,T), T1<T, T<T2, initiates(E,F,T).
  
% DEC 3
holdsAt(F2,T1+T2) :- 
   happens(E,T1), initiates(E,F1,T1), 0<T2, 
   trajectory(F1,T1,F2,T2), not stoppedIn(T1,F1,T1+T2), 
   T1+T2<=maxstep.

% DEC 4
holdsAt(F2,T1+T2) :- 
   happens(E,T1), terminates(E,F1,T1), 0<T2, 
   antiTrajectory(F1,T1,F2,T2), not startedIn(T1,F1,T1+T2),
   T1+T2<=maxstep.

initiated2(F,T) :- 
   happens(E,T), initiates(E,F,T).

terminated2(F,T) :- 
   happens(E,T), terminates(E,F,T).

released2(F,T) :- 
   happens(E,T), releases(E,F,T).

% DEC 5
holdsAt(F,T+1) :- 
   holdsAt(F,T), not releasedAt(F,T+1), not terminated2(F,T),
   T<maxstep.

% DEC 6
:- not holdsAt(F,T), not releasedAt(F,T+1), not initiated2(F,T),
   holdsAt(F,T+1), T<maxstep.

% DEC 7
releasedAt(F,T+1) :- 
   releasedAt(F,T), not initiated2(F,T), not terminated2(F,T),
   T<maxstep.

% DEC 8
:- not releasedAt(F,T), not released2(F,T),
   releasedAt(F,T+1), T<maxstep.

% DEC 9
holdsAt(F,T+1) :- 
   happens(E,T), initiates(E,F,T), T<maxstep.

% DEC 10
:- happens(E,T), terminates(E,F,T), holdsAt(F,T+1), T<maxstep.

% DEC 11
releasedAt(F,T+1) :- 
   happens(E,T), releases(E,F,T), T<maxstep.

% DEC 12
:- happens(E,T), initiates(E,F,T), releasedAt(F,T+1), T<maxstep.

:- happens(E,T), terminates(E,F,T), releasedAt(F,T+1), T<maxstep.


{holdsAt(F,T)}.
{releasedAt(F,T)}.

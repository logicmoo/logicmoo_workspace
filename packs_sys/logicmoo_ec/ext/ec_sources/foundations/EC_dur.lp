% EC_dur.lp

#domain fluent(F;F1;F2), event(E), timepoint(T;T1;T2;T3;T4).

timepoint(0..maxstep).

% EC 1
clipped(T1,F,T4) :-
   happens3(E,T2,T3), T1<=T3, T2<T4, terminates(E,F,T2).

% EC 2
declipped(T1,F,T4) :-
   happens3(E,T2,T3), T1<=T3, T2<T4, initiates(E,F,T2).

% EC 3 (as same as DEC 1)
stoppedIn(T1,F,T4) :-
   happens3(E,T2,T3), T1<T3, T2<T4, terminates(E,F,T2).
 
% EC 4 (as same as DEC 2)
startedIn(T1,F,T4) :-
   happens3(E,T2,T3), T1<T3, T2<T4, initiates(E,F,T2).

% EC 5 (as same as DEC 3)
holdsAt(F2,T2+T3) :-
   happens3(E,T1,T2), initiates(E,F1,T1), 0<T3,
   trajectory(F1,T1,F2,T3), not stoppedIn(T1,F1,T2+T3),
   T2+T3<=maxstep.

% EC 6 (as same as DEC 4)
holdsAt(F2,T2+T3) :-
   happens3(E,T1,T2), terminates(E,F1,T1), 0<T3,
   antiTrajectory(F1,T1,F2,T3), not startedIn(T1,F1,T2+T3),
   T2+T3<=maxstep.


% EC 7
releasedAtBetween(T1,F,T2) :- releasedAt(F,T), T1<T, T<=T2.
persistsBetween(T1,F,T2) :- not releasedAtBetween(T1,F,T2).
   
% EC 8
releasedBetween(T1,F,T4) :-
   happens3(E,T2,T3), T1<=T3, T2<T4, releases(E,F,T2).


% EC 9 (similar to DEC 5)
holdsAt(F,T2) :-
   holdsAt(F,T1), T1<T2, persistsBetween(T1,F,T2),
   not clipped(T1,F,T2).

% EC 10 (similar to DEC 6)
:- not holdsAt(F,T1), T1<T2, persistsBetween(T1,F,T2),
   not declipped(T1,F,T2), holdsAt(F,T2).

% EC 11 (similar to DEC 7)
releasedAt(F,T2) :-
   releasedAt(F,T1), T1<T2, not clipped(T1,F,T2),
   not declipped(T1,F,T2).

% EC 12 (similar to DEC 8)
:- not releasedAt(F,T1), T1<T2,
   not releasedBetween(T1,F,T2), releasedAt(F,T2).

% EC 13 
releasedIn(T1,F,T4) :- 
   happens3(E,T2,T3), T1<T3, T2<T4, releases(E,F,T2).

% EC 14 (similar to DEC 9)
holdsAt(F,T3) :-
   happens3(E,T1,T2), initiates(E,F,T1), T2<T3,
   not stoppedIn(T1,F,T3), not releasedIn(T1,F,T3).

% EC 15 (similar to DEC 10)
:- happens3(E,T1,T2), terminates(E,F,T1), T2<T3,
   not startedIn(T1,F,T3), not releasedIn(T1,F,T3),
   holdsAt(F,T3).

% EC 16 (similar to DEC 11)
releasedAt(F,T3) :-
   happens3(E,T1,T2), releases(E,F,T1),T2<T3,
   not stoppedIn(T1,F,T3), not startedIn(T1,F,T3).

% EC 17 (similar to DEC 12)
:- happens3(E,T1,T2), initiates(E,F,T1), T2<T3,
   not releasedIn(T1,F,T3), releasedAt(F,T3).

:- happens3(E,T1,T2), terminates(E,F,T1), T2<T3,
   not releasedIn(T1,F,T3), releasedAt(F,T3).

% EC 18
:- happens3(E,T1,T2), T1 > T2.

% EC 19
happens(E,T) :- happens3(E,T,T).

% choice rules
{holdsAt(F,T)}.
{releasedAt(F,T)}.

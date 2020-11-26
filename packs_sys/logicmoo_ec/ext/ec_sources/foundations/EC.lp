% EC.lp

#domain fluent(F;F1;F2), event(E), timepoint(T;T1;T2).

timepoint(0..maxstep).

% EC 1
clipped(T1,F,T2) :-
   happens(E,T), T1<=T, T<T2, terminates(E,F,T).

% EC 2
declipped(T1,F,T2) :-
   happens(E,T), T1<=T, T<T2, initiates(E,F,T).

% EC 3 (as same as DEC 1)
stoppedIn(T1,F,T2) :-
   happens(E,T), T1<T, T<T2, terminates(E,F,T).
 
% EC 4 (as same as DEC 2)
startedIn(T1,F,T2) :-
   happens(E,T), T1<T, T<T2, initiates(E,F,T).

% EC 5 (as same as DEC 3)
holdsAt(F2,T1+T2) :-
   happens(E,T1), initiates(E,F1,T1), 0<T2,
   trajectory(F1,T1,F2,T2), not stoppedIn(T1,F1,T1+T2),
   T1+T2<=maxstep.

% EC 6 (as same as DEC 4)
holdsAt(F2,T1+T2) :-
   happens(E,T1), terminates(E,F1,T1), 0<T2,
   antiTrajectory(F1,T1,F2,T2), not startedIn(T1,F1,T1+T2),
   T1+T2<=maxstep.

% EC 7
releasedAtBetween(T1,F,T2) :- releasedAt(F,T), T1<T, T<=T2.
persistsBetween(T1,F,T2) :- not releasedAtBetween(T1,F,T2).
   
% EC 8
releasedBetween(T1,F,T2) :-
   happens(E,T), T1<=T, T<T2, releases(E,F,T).

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
releasedIn(T1,F,T2) :- 
   happens(E,T), T1<T, T<T2, releases(E,F,T).

% EC 14 (similar to DEC 9)
holdsAt(F,T2) :-
   happens(E,T1), initiates(E,F,T1), T1<T2,
   not stoppedIn(T1,F,T2), not releasedIn(T1,F,T2).

% EC 15 (similar to DEC 10)
:- happens(E,T1), terminates(E,F,T1), T1<T2,
   not startedIn(T1,F,T2), not releasedIn(T1,F,T2),
   holdsAt(F,T2).

% EC 16 (similar to DEC 11)
releasedAt(F,T2) :-
   happens(E,T1), releases(E,F,T1),T1<T2,
   not stoppedIn(T1,F,T2), not startedIn(T1,F,T2).

% EC 17 (similar to DEC 12)
:- happens(E,T1), initiates(E,F,T1), T1<T2,
   not releasedIn(T1,F,T2), releasedAt(F,T2).

:- happens(E,T1), terminates(E,F,T1), T1<T2,
   not releasedIn(T1,F,T2), releasedAt(F,T2).


% choice rules
{holdsAt(F,T)}.
{releasedAt(F,T)}.

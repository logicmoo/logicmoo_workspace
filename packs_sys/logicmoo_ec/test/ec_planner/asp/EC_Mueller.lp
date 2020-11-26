%
% Event Calculus (EC)
%
% @incollection{MillerShanahan:2002,
%   author = "Rob Miller and Murray Shanahan",
%   year = "2002",
%   title = "Some alternative formulations of the event calculus",
%   editor = "Antonis C. Kakas and Fariba Sadri",
%   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%   series = "Lecture Notes in Computer Science",
%   volume = "2408",
%   pages = "452--490",
%   address = "Berlin",
%   publisher = "Springer",
% }
%

% (EC1)
clipped(T1,F,T2) :- timepoint(T),
                    timepoint(T1),
                    timepoint(T2),
                    fluent(F),
                    event(E),
                    happens(E,T),
                    T1<=T,
                    T<T2,
                    terminates(E,F,T).

% (EC2)
declipped(T1,F,T2) :- timepoint(T),
                      timepoint(T1),
                      timepoint(T2),
                      fluent(F),
                      event(E),
                      happens(E,T),
                      T1<=T,
                      T<T2,
                      initiates(E,F,T).

% (EC3)
stopped_in(T1,F,T2) :- timepoint(T),
                       timepoint(T1),
                       timepoint(T2),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       T1<T,
                       T<T2,
                       terminates(E,F,T).

% (EC4)
started_in(T1,F,T2) :- timepoint(T),
                       timepoint(T1),
                       timepoint(T2),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       T1<T,
                       T<T2,
                       initiates(E,F,T).

% (EC5)
holds_at(F2,T1+T2) :- timepoint(T1),
                      timepoint(T2),
                      fluent(F1),
                      fluent(F2),
                      event(E),
                      happens(E,T1),
                      initiates(E,F1,T1),
                      0<T2,
                      trajectory(F1,T1,F2,T2),
                      not stopped_in(T1,F1,T1+T2).

% (EC6)
holds_at(F2,T1+T2) :- timepoint(T1),
                      timepoint(T2),
                      fluent(F1),
                      fluent(F2),
                      event(E),
                      happens(E,T1),
                      terminates(E,F1,T1),
                      0<T2,
                      anti_trajectory(F1,T1,F2,T2),
                      not started_in(T1,F1,T1+T2).

% (EC7)
released_at_between(T1,F,T2) :- timepoint(T),
                                timepoint(T1),
                                timepoint(T2),
                                fluent(F),
                                event(E),
                                released_at(F,T),
                                T1<T,
                                T<=T2.

% (EC8)
released_between(T1,F,T2) :- timepoint(T),
                             timepoint(T1),
                             timepoint(T2),
                             fluent(F),
                             event(E),
                             happens(E,T),
                             T1<=T,
                             T<T2,
                             releases(E,F,T).

% (EC9)
holds_at(F,T2) :- timepoint(T1),
                  timepoint(T2),
                  fluent(F),
                  holds_at(F,T1),
                  T1<T2,
                  not released_at_between(T1,F,T2),
                  not clipped(T1,F,T2).

% (EC10)
-holds_at(F,T2) :- timepoint(T1),
                   timepoint(T2),
                   fluent(F),
                   -holds_at(F,T1),
                   T1<T2,
                   not released_at_between(T1,F,T2),
                   not declipped(T1,F,T2).

% (EC11)
released_at(F,T2) :- timepoint(T1),
                     timepoint(T2),
                     fluent(F),
                     released_at(F,T1),
                     T1<T2,
                     not clipped(T1,F,T2),
                     not declipped(T1,F,T2).

% (EC12)
-released_at(F,T2) :- timepoint(T1),
                      timepoint(T2),
                      fluent(F),
                      -released_at(F,T1),
                      T1<T2,
                      not released_between(T1,F,T2).

% (EC13)
released_in(T1,F,T2) :- timepoint(T),
                        timepoint(T1),
                        timepoint(T2),
                        fluent(F),
                        event(E),
                        happens(E,T),
                        T1<T,
                        T<T2,
                        releases(E,F,T).

% (EC14)
holds_at(F,T2) :- timepoint(T1),
                  timepoint(T2),
                  fluent(F),
                  event(E),
                  happens(E,T1),
                  initiates(E,F,T1),
                  T1<T2,
                  not stopped_in(T1,F,T2),
                  not released_in(T1,F,T2).

% (EC15)
-holds_at(F,T2) :- timepoint(T1),
                   timepoint(T2),
                   fluent(F),
                   event(E),
                   happens(E,T1),
                   terminates(E,F,T1),
                   T1<T2,
                   not started_in(T1,F,T2),
                   not released_in(T1,F,T2).

% (EC16)
released_at(F,T2) :- timepoint(T1),
                     timepoint(T2),
                     fluent(F),
                     event(E),
                     happens(E,T1),
                     releases(E,F,T1),
                     T1<T2,
                     not stopped_in(T1,F,T2),
                     not started_in(T1,F,T2).

% (EC17)
-released_at(F,T2) :- timepoint(T1),
                      timepoint(T2),
                      fluent(F),
                      event(E),
                      happens(E,T1),
                      initiates(E,F,T1),
                      T1<T2,
                      not released_in(T1,F,T2).
-released_at(F,T2) :- timepoint(T1),
                      timepoint(T2),
                      fluent(F),
                      event(E),
                      happens(E,T1),
                      terminates(E,F,T1),
                      T1<T2,
                      not released_in(T1,F,T2).

% End of file.

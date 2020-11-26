%
% Discrete Event Calculus (DEC)
%
% @article{Mueller:2004a,
%   author = "Erik T. Mueller",
%   year = "2004",
%   title = "Event calculus reasoning through satisfiability",
%   journal = "Journal of Logic and Computation",
%   volume = "14",
%   number = "5",
%   pages = "703--730",
% }
%

% (DEC1)
stopped_in(T1,F,T2) :- timepoint(T),
                       timepoint(T1),
                       timepoint(T2),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       T1<T,
                       T<T2,
                       terminates(E,F,T).

% (DEC2)
started_in(T1,F,T2) :- timepoint(T),
                       timepoint(T1),
                       timepoint(T2),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       T1<T,
                       T<T2,
                       initiates(E,F,T).

% (DEC3)
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

% (DEC4)
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

initiated(F,T) :- timepoint(T),
                  fluent(F),
                  event(E),
                  happens(E,T),
                  initiates(E,F,T).

terminated(F,T) :- timepoint(T),
                   fluent(F),
                   event(E),
                   happens(E,T),
                   terminates(E,F,T).

released(F,T) :- timepoint(T),
                 fluent(F),
                 event(E),
                 happens(E,T),
                 releases(E,F,T).

% (DEC5)
holds_at(F,T+1) :- timepoint(T),
                   fluent(F),
                   holds_at(F,T),
                   -released_at(F,T+1),
                   not terminated(F,T).

% (DEC6)
-holds_at(F,T+1) :- timepoint(T),
                    fluent(F),
                    -holds_at(F,T),
                    -released_at(F,T+1),
                    not initiated(F,T).

% (DEC7)
released_at(F,T+1) :- timepoint(T),
                      fluent(F),
                      released_at(F,T),
                      not initiated(F,T),
                      not terminated(F,T).

% (DEC8)
-released_at(F,T+1) :- timepoint(T),
                      fluent(F),
                      -released_at(F,T),
                      not released(F,T).

% (DEC9)
holds_at(F,T+1) :- timepoint(T),
                   fluent(F),
                   event(E),
                   happens(E,T),
                   initiates(E,F,T).

% (DEC10)
-holds_at(F,T+1) :- timepoint(T),
                    fluent(F),
                    event(E),
                    happens(E,T),
                    terminates(E,F,T).

% (DEC11)
released_at(F,T+1) :- timepoint(T),
                      fluent(F),
                      event(E),
                      happens(E,T),
                      releases(E,F,T).

% (DEC12)
-released_at(F,T+1) :- timepoint(T),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       initiates(E,F,T).
-released_at(F,T+1) :- timepoint(T),
                       fluent(F),
                       event(E),
                       happens(E,T),
                       terminates(E,F,T).

% End of file.

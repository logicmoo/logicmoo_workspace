%
% Kitchen sink scenario
%
% \fullciteA[pp. 326--329]{Shanahan:1997}
%
% @book{Shanahan:1997,
%   author = "Murray Shanahan",
%   year = "1997",
%   title = "Solving the Frame Problem",
%   address = "Cambridge, MA",
%   publisher = "MIT Press",
% }
%

timepoint(0..25).
height(0..20).
event(tap_on).
event(tap_off).
event(overflow).
fluent(filling).
fluent(has_height(H)) :- height(H).
fluent(spilling).

initiates(tap_on,filling,T) :- timepoint(T).

terminates(tap_off,filling,T) :- timepoint(T).

releases(tap_on,has_height(H),T) :- timepoint(T), height(H).

initiates(tap_off,has_height(H),T) :- timepoint(T),
                                      height(H),
                                      holds_at(has_height(H),T).

terminates(overflow,filling,T) :- timepoint(T).

initiates(overflow,has_height(H),T) :- timepoint(T),
                                       height(H),
                                       holds_at(has_height(H),T).

initiates(overflow,spilling,T) :- timepoint(T).

trajectory(filling,T,has_height(H2),D) :- timepoint(T),
                                          timepoint(D),
                                          height(H1),
                                          height(H2),
                                          holds_at(has_height(H1),T),
                                          H2=H1+D.

:- height(H1),
   height(H2),
   timepoint(T),
   holds_at(has_height(H1),T),
   holds_at(has_height(H2),T),
   H1 != H2.

happens(overflow,T) :- timepoint(T),
                       holds_at(has_height(10),T),
                       holds_at(filling,T).

holds_at(has_height(0),0).
-holds_at(filling,0).
-holds_at(spilling,0).

-released_at(F,0) :- fluent(F).

happens(tap_on,5).

% End of file.

% File 'zoo-example' : specific landscape

% landscape specific to a scenario
% The number of positions, locations, gates may be different among scenarios.

:- macros                         
  numberOfPositions -> 8.

:- objects
  1..numberOfPositions      :: position;
  cageA                     :: cage;
  gateAO                    :: gate.


%%% Scenario Landscape
%   cageA  outside
%   -------
%  | 1  2 |  5  6 
%  | 3  4 ao 7  8 
%   -------
%%%


% a position is in the outside location by default
default loc(P)=outside.

caused loc(1)=cageA.
caused loc(2)=cageA.
caused loc(3)=cageA.
caused loc(4)=cageA.

% The neighbor relation is symmetric (lmw)
caused neighbor(P1,P) if neighbor(P,P1) where P<P1.

% Two positions in different locations are neighbors 
%  if they are the sides of a gate 
caused neighbor(P,P1)
   if loc(P)\=loc(P1) && [\/G | sides(P,P1,G)].

caused neighbor(1,2).
caused neighbor(1,3).
caused neighbor(1,4).
caused neighbor(2,3).
caused neighbor(2,4).
caused neighbor(3,4).

caused neighbor(5,6).
caused neighbor(5,7).
caused neighbor(5,8).
caused neighbor(6,7).
caused neighbor(6,8).
caused neighbor(7,8).

caused side1(gateAO)=4.
caused side2(gateAO)=7.

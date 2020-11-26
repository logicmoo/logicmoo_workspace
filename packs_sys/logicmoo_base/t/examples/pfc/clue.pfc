:- use_module(library(logicmoo_user)).

% has(Player,Card)
% ask(Player,Set).
% answer(Player,Set)
% pass(Player,Set)

pass(Player,(C1,C2,C2)) ==>
  ( ~has(Player,C1)),
  ( ~has(Player,C2)),
  ( ~has(Player,C3)).

answer(Player,(C1,C2,C3)) ==>
  or(has(Player,C1),
     has(Player,C2),
     has(Player,C3)).

or(P1,P2,P3) ==>
  (( ~P1), ( ~P2) ==> P3),
  (( ~P1), ( ~P3) ==> P2),
  (( ~P2), ( ~P3) ==> P1).

% a particular Card can only be held by one player.
has(Player,Card),
player(Player2)/{Player\==Player2}
  ==>
( ~has(Player2,Card)).

% every card is held by some player or the pot.


% the pot has three cards, one of each type.

% each player has a fixed number of cards.


% card(id,type), where type e {suspect,weapon,room}
card(col_mustard,suspect).
card(hall,room).
card(pipe,weapon).



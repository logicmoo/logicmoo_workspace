% Hand:
%
% North  43:9743:AQ5:KT87
% South  AK6:AT8:K93:QJ96
% 
% Contract: 3NT
% Lead: H5

% C: how are you going to play the hand?
sent('play the five of diamonds').

% C: you have to follow suit
sent('win the trick in dummy').

% C: you don't have a winner in dummy
sent('play the six of hearts').

% C: you don't have the six of hearts
sent('play small').

% C: ok, playing the three of hearts. 
% [Shows on screen. East goes up with the queen ]

sent('play the ace').

% C: ok, playing the eight of hearts
% [Shows on screen]

sent('i said play the ace').

% C: sorry, playing the ace of hearts
% [Shows on screen]

sent('who is on lead').

% C: you are

sent('am i in hand').

% C: yes
sent('play the king').

% C: which king do you mean

sent('the king of spades').

% C: ok, playing the king of spades
% [Shows on screen]

sent('can i have my card back').

% C: ok
% [Shows on screen]

sent('i want to play the ace').

% C: ok, playing the ace of spades [ideally adds a sigh]. 
% [Shows on screen. Plays small from W]
% C: [after a pause] What do you want from dummy?

sent('play small from dummy').

% C: ok, playing the three of spades

sent('now i want to play the queen of clubs').

% C: ok...

sent('put the king on it').

% C: ok...
% [The ace still hasn't shown up]

sent('now play the jack of clubs').

% C: you are on table

sent('sorry lead the seven of clubs').

% C: you don't need to apologise
% [Shows, and covers with the ace]

sent('now i \'m going to play the jack').

% C: ok...

sent('can i take that back').

% C: ok...

sent('i want to play the six instead').

% C: ok...

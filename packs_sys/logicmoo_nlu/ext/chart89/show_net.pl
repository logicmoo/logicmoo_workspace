% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% show_net.pl [Chapter  9] Utility code for exhibiting net theorems
%
show(Entity,Attribute) :-
	has_attr(Entity,Attribute,yes),
	write(Entity),write(' is '),write(Attribute).
show(Entity,Attribute) :-
	has_attr(Entity,Attribute,no),
	write(Entity),write(' is not '),write(Attribute).
show(Entity,Attribute) :-
  has_attr(Entity,Attribute,Value),
  Value \= yes, Value \= no,
  write('The '), write(Attribute), write(' of '), write(Entity),
  write(' is '), write(Value).

show(Entity) :-
	show(Entity,Attribute),nl,fail.
show(_).

show :-
	show(kim),nl,
	show(jean),nl,
	show(mayumi),nl,
	show(beryl),nl.

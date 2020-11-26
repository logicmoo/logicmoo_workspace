% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% inherits.pl [Chapter  9] Simple semantic net, with absolute inheritance
%
?- reconsult('show_net.pl').
%
% Club_member
    attr(club_member,sex,female).

% Associate
  isa(associate,club_member).
    attr(associate,associate_member,yes).
    attr(associate,citizenship,non_US).

% Life_member
  isa(life_member,club_member).
    attr(life_member,life_member,yes).
    attr(life_member,citizenship,'US').

% Kim
  isa(kim,associate).
    attr(kim,over_50,no).

% Jean
  isa(jean,associate).
    attr(jean,over_50,yes).

% Mayumi
  isa(mayumi,life_member).
    attr(mayumi,over_50,yes).

% Beryl
  isa(beryl,life_member).
    attr(beryl,over_50,no).

has_attr(Entity,Attribute,Value) :-
	attr(Entity,Attribute,Value).
has_attr(Entity1,Attribute,Value) :-
	isa(Entity1,Entity2),
	has_attr(Entity2,Attribute,Value).

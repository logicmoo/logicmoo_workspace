% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% defaults.pl [Chapter  9] Simple semantic net, with default inheritance
%
?- reconsult('show_net.pl').
%

has_attr(Entity,Attribute,Value) :-
	attr(Entity,Attribute,Value).
has_attr(Entity1,Attribute,Value) :-
	isa(Entity1,Entity2),
	has_attr(Entity2,Attribute,Value),
	not_local(Entity1,Attribute).
not_local(Entity,Attribute) :-
	attr(Entity,Attribute,_), !,
	fail.
not_local(_,_).

% Club_member
       attr(club_member,sex,male).
       attr(club_member,over_50,yes).
       attr(club_member,citizenship,'US').

% Associate
   isa(associate,club_member).
       attr(associate,associate_member,yes).
       attr(associate,citizenship,non_US).

% Life_member
   isa(life_member,club_member).
       attr(life_member,life_member,yes).
       attr(life_member,over_50,no).

% Kim
   isa(kim,associate).
       attr(kim,over_50,no).

% Jean
   isa(jean,associate).
       attr(jean,sex,female).
       attr(jean,citizenship,'US').

% Mayumi
   isa(mayumi,life_member).
       attr(mayumi,sex,female).
       attr(mayumi,over_50,yes).
       attr(mayumi,citizenship,non_US).

% Beryl
   isa(beryl,life_member).
    attr(beryl,sex,female).

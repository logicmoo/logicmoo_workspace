% push.pl
% July 1, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This is meant to be used as a basic template for how the action
% files are formatted.
%
*/
% :-swi_module(user). 
:-swi_module(modPush, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

vtActionTemplate(actPush(vtDirection)).

agent_call_command(Agent,actPush(Dir)):-once(actPush(Agent,Dir)).

% Push a box
% Nothing to push... agent moves and takes a little damage.
%Plus it still costs the same charge as if the agent did push something
actPush(Agent,Dir) :-	
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),
	mudAtLoc(What,XXYY),
	integer(What),
	in_world_move(_,Agent,Dir),
	call_update_stats(Agent,strain),
	call_update_charge(Agent,actPush).

% Pushing what cannot be pushed
% Some damage and loss of charge (same as normal push)
actPush(Agent,Dir) :-	
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),
	mudAtLoc(What,XXYY),
	\+ pushable(Agent,What,XXYY,Dir),
	call_update_stats(Agent,hernia),
	call_update_charge(Agent,actPush).

% A successful PUSH
actPush(Agent,Dir) :-	
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),
	mudAtLoc(What,XXYY),
	move_object(XXYY,What,Dir),
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,actPush).

% Can the Object be pushed?
pushable(Agent,Obj,LOC,Dir) :-
	mudStr(Agent,Str),
	props(Obj,mudWeight(Wt)),
	Wt \== 4,
	Wt =< Str,
	\+ anything_behind(LOC,Dir).
% If the Obj is another agent, compare strenghts to see if the agent can push the other
% An agent can push another if the agents strenght is greater that or equal to
% their opponents strength.
pushable(Agent,Obj,LOC,Dir) :-
	mudStr(Agent,Str),
	mudStr(Obj,OppStr),
	Str >= OppStr,
	(\+ anything_behind(LOC,Dir);
	crashbang(Obj)).

% Is the location behind the pushed item/agent empty (or near empty).
anything_behind(LOC,Dir) :-
	from_dir_target(LOC,Dir,XXYY),
	mudAtLoc(What,XXYY),
	props(What,[mudWeight > 1,mudPermanence(actTake,or(Pm,0))]),
	Pm < 2.

% Move the object.
move_object(LOC,Obj,Dir) :-
	from_dir_target(LOC,Dir,XXYY),
	squish_behind(XXYY,Obj),
	in_world_move(LOC,Obj,Dir).

% Squish small objects behind what is being pushed.
squish_behind(LOC,Obj) :-
        XY = LOC,
	mudAtLoc(What,XY),
	props(What,mudWeight(1)),
	props(Obj,mudWeight(N)),
	N > 1,
	del(mudAtLoc(What,XY)).
squish_behind(_,_).

% When one agent pushes another into a wall (or anything big), 
% both the agents take damage. 
% The pusher takes damage as normal (for pushing something
% unpushable), the pushy takes damage below
crashbang(Obj) :- padd(Obj,[mudHealth(+ -5)]).

% Record keeping
update_charge(Agent,actPush) :- padd(Agent,[mudEnergy(+ -6)]).
update_stats(Agent,strain) :- padd(Agent,[mudHealth(+ -2)]).
update_stats(Agent,hernia) :- padd(Agent,[mudHealth(+ -4),mudCmdFailure(hernia)]).

:- include(prologmud(mud_footer)).

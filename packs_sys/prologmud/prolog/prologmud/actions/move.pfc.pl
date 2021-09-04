% move.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This file defines the predicates for the agent to move
%
*/

% :-swi_module(user). 
:-swi_module(modMove, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% :- expects_dialect(mudcode).


baseKB:agent_text_command(Agent,[DirSS],Agent,OUT):-nonvar(DirSS), to_case_breaks(DirSS,[xti(DirS,_),
   xti(Dist,digit)]),show_call(coerce(DirS,vtDirection,Dir)),OUT=actMove(Dist,Dir).
baseKB:agent_text_command(Agent,[DirSS],Agent,OUT):-nonvar(DirSS), show_call(coerce(DirSS,vtDirection,Dir)),OUT=actMove(Dir).

baseKB:agent_call_command(Agnt,Cmd):- compound(Cmd),functor(Cmd,actMove,_),!,must(move_command(Agnt,Cmd)).

baseKB:action_info(actMove(isOptional(ftNumber,1),vtDirection),"Move [1] south % distance in direction").

baseKB:text_actverb("go",actMove).

/*
% dir###
move_command(Agent,actMove(DirSS)) :- catch((string_to_atom(DirSS,DirS),
	    atom_concat(Dir,N,DirS),atom_number(N,Dist)),_,fail),!,
            move_command(Agent,Dir,Dist).
% dir
move_command(Agent,actMove(Dir)) :-
	    get_move_dist(Agent,Dist),
            move_command(Agent,Dir,Dist).
*/
move_command(Agent,actMove(Dir)):-!,move_command(Agent,actMove(1,Dir)).
% dir
move_command(Agent,actMove(Dist,Dir)) :-
            move_command(Agent,Dir,Dist).

get_move_dist(Agent,Dist):-req1(mudMoveDist(Agent,Dist)),!.
get_move_dist(_Gent,1).

% Move thy agent
move_command(Agent,DirS,DistS) :- 
   string_to_atom(DirS,Dir),
   any_to_number(DistS,Dist),
   catch(doall((between(1,Dist,_),move_command_1(Agent,Dir))),giveup(_),true).



% cant get anywhere since the map fails it
move_command_1(Agent,Dir) :-
	mudAtLoc(Agent,LOC),
         \+ (from_dir_target(LOC,Dir,_)),!,
		(add_cmdfailure(Agent,actMove)),
      throw(giveup(nopath(Agent,actMove))).

% Run into something big, Ouch...
% damage and charge... plus it doesn't get anywhere
move_command_1(Agent,Dir) :-
	mudAtLoc(Agent,LOC),
        from_dir_target(LOC,Dir,XXYY),
        % trace,
        is_3d(XXYY),
         mudAtLoc(Obj,LOC),        
         prop_or(Obj,mudHeight,ObjHt,1),
         mudAtLoc(Obj2,XXYY),
         prop_or(Obj2,mudHeight,ObjHt2,1),
         ObjHt2 > ObjHt,
         ObjHt2 > 1,
	!,
	call_update_stats(Agent,collide),
	call_update_charge(Agent,actMove),
        raise_location_event(XXYY,collide(Agent,Obj2)),
   throw(giveup(collide(Agent,Obj2))).


% Another Agent is in the way
move_command_1(Agent,Dir):- 
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),       
	is_3d(XXYY),
        mudAtLoc(Agent2,XXYY),
	isa(Agent2,tAgent),!,
	call_update_stats(Agent,collide),
	call_update_charge(Agent,actMove),
        raise_location_event(XXYY,collide(Agent,Agent2)),
   throw(giveup(collide(Agent,Agent2))).


%Move successfully
move_command_1(Agent,Dir) :-
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,actMove).

%Record keeping

update_charge(Agent,actMove) :- padd(Agent,mudEnergy,+ -4).

update_stats(Agent,collide) :- padd(Agent,mudHealth,+ -5),(add_cmdfailure(Agent,collide)).

update_stats(Agent,fall) :- padd(Agent,mudHealth,+ -10).

% cheating but to test

vtActionTemplate(actGo(vtDirection)).
baseKB:agent_call_command(Agent,actGo(Dir)) :-
	mudAtLoc(Agent,LOC),
        in_world_move(LOC,Agent,Dir),
	call_update_charge(Agent,actMove).


:- include(prologmud(mud_footer)).

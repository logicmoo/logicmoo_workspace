/* * module *
% teleport.pl
%
% This file defines how an agent teleports 
%
%  Test of the new concise syntax:
% 
%   props(Agent,charge>10),
%
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :-swi_module(user). 
:-swi_module(modTeleport, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% teleport
action_info(actTeleport(isOptional(isAnd([tObj,isNot(tRegion)]),isSelfAgent),isOptionalStr("to"),isOptional(tRegion,isRandom(tRegion))),"teleport [obj] [to] [somewhere]").

text_actverb(tp,actTeleport).

%targeted
agent_call_command(_Agent,actTeleport(Other,_TO,Where)):-
   coerce(Other,tObj,Target),
   coerce(Where,tRegion,Location),
   detatch_object(Target),
   to_3d(Location,Where3D),
   ain(mudAtLoc(Target,Where3D)).


:- include(prologmud(mud_footer)).

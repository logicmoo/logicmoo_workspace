/* * module *
% actPut.pl
%
% This file defines how an agent puts 
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
:-swi_module(modPut, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% actPut
baseKB:action_info(actPut(tCarryAble,txtPrepSpatial,tPutTargetAble),"actPut [obj] [onto|inside] [somewhere]").

baseKB:text_actverb(T,V):-vtVerb(V),name_text_cached(V,T).
baseKB:text_actverb(set,actPut).
baseKB:text_actverb(place,actPut).
baseKB:text_actverb(hide,actPut).
baseKB:text_actverb(display,actPut).
baseKB:text_actverb(stow,actPut).

%targeted
baseKB:agent_call_command(Agent,actPut(Other,Prep,Where)):-actPut(Agent,Other,Prep,Where).


actPut(Agent,Other,_Prep,Where):-
   %hasPhysicalAccess(Agent,Room),
   localityOfObject(Target,Room),
   ignore(localityOfObject(Agent,Room)),
   coerce(Other,tObj,Target),
   coerce(Where,tPutTargetAble,Location),
   clr(localityOfObject(Target,Room)),
   clr(mudAtLoc(Target,_)),
   to_3d(Location,Where3D),
   ain(mudAtLoc(Where3D,Location)).


:- include(prologmud(mud_footer)).

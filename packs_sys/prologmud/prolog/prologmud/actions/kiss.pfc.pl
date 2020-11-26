
% :-swi_module(user). 
%:-swi_module(frot, [vtFrotVerb/1,frotCommand/3,frot_to_callcmd/4]).
/* * module *  This file defines the predicates for the agent to frotize
% Dec 13, 2035
% Douglas Miles
%
*/

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

frotCommand(Kiss,FrotVerb,frot(isOptional(vtVerb,FrotVerb),isOptional(tAgent,vHere),ftString)):-
  vtFrotVerb(FrotVerb), Kiss =.. [FrotVerb,isOptional(tAgent,vHere),ftString].

vtFrotVerb(FrotVerb):-member(FrotVerb,[actKiss,actHug,actRub,actFrottage]).

action_info(Kiss,ftText("invokes",Does)):- frotCommand(Kiss,_FrotVerb,Does).

agent_text_command(Agent,[Kiss|What],Agent,CMD):-
   agent_text_command_frot(Agent,[Kiss|What],Agent,CMD).

agent_text_command_frot(Agent,[Kiss|What],Agent,CMD):- nonvar(Kiss),nonvar(What),!,
      vtFrotVerb(Kiss),
      once(((frot_to_callcmd(Agent,Kiss,What,CMD),nonvar(CMD)))).

% frot at to joe
frot_to_callcmd(Agent,Kiss,What,CMD):-append(Text,[Whom],What),!,frot_command_parse_2(Agent,Kiss,Whom,Text,CMD).
% frot at
frot_to_callcmd(Agent,Kiss,What,CMD):-mudAtLoc(Agent,Where),frot_command_parse_2(Agent,Kiss,Where,What,CMD).

frot_command_parse_2(Agent,Kiss,Where,What,actProlog(actFrot(Agent,Kiss,Where,What))).

actFrot(Agent,Kiss,Whom,Text):-
   mudAtLoc(Agent,Where),
   asInvoked(Cmd,[Kiss,Agent,Whom,Text]),
   raise_location_event(Where,actNotice(reciever,Cmd)).


:- if(current_module(frot)).
:- module_meta_predicates_are_transparent(frot).
:- module_predicates_are_exported.
:- endif.



:- include(prologmud(mud_footer)).


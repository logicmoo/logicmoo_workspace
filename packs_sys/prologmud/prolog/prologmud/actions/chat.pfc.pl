
%:-swi_module(chat, [vtSocialVerb/1,socialCommand/3,chat_to_callcmd/4]).
/* * module *  This file defines the predicates for the agent to socialize
% Dec 13, 2035
% Douglas Miles
%
*/

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

socialCommand(Say,SocialVerb,chat(isOptional(vtVerb,SocialVerb),isOptional(tChannel,vHere),ftString)):-
  vtSocialVerb(SocialVerb), Say =.. [SocialVerb,isOptional(tChannel,vHere),ftString].

vtSocialVerb(SocialVerb):-member(SocialVerb,[actSay,actWhisper,actEmote,actTell,actAsk,actShout,actGossup]).

baseKB:action_info(Say,ftText("invokes",Does)):- socialCommand(Say,_SocialVerb,Does).

baseKB:agent_text_command(Agent,[Say|What],Agent,CMD):-
   agent_text_command_chat(Agent,[Say|What],Agent,CMD).

agent_text_command_chat(Agent,[Say|What],Agent,CMD):- nonvar(Say),nonvar(What),!,
      vtSocialVerb(Say),
      once(((chat_to_callcmd(Agent,Say,What,CMD),nonvar(CMD)))).

% ask joe about some text
chat_to_callcmd(Agent,actAsk,What,CMD):-append([Whom,about],About,What),!,chat_command_parse_2(Agent,actAsk,Whom,About,CMD).
% ask joe some text
chat_to_callcmd(Agent,actAsk,What,CMD):-append([Whom],About,What),isa(Whom,tAgent),!,chat_command_parse_2(Agent,actAsk,Whom,About,CMD).
% say to joe some text 
chat_to_callcmd(Agent,Say,What,CMD):-append([to,Whom],Text,What),!,chat_command_parse_2(Agent,Say,Whom,Text,CMD).
% say some text to joe
chat_to_callcmd(Agent,Say,What,CMD):-append(Text,[to,Whom],What),!,chat_command_parse_2(Agent,Say,Whom,Text,CMD).
% say some text
chat_to_callcmd(Agent,Say,What,CMD):-mudAtLoc(Agent,Where),chat_command_parse_2(Agent,Say,Where,What,CMD).

chat_command_parse_2(Agent,Say,Where,What,actProlog(actSocial(Agent,Say,Where,What))).

actSocial(Agent,Say,Whom,Text):-
   mudAtLoc(Agent,Where),
   asInvoked(Cmd,[Say,Agent,Whom,Text]),
   raise_location_event(Where,actNotice(reciever,Cmd)).


:- if(current_module(chat)).
:- module_meta_predicates_are_transparent(chat).
:- module_predicates_are_exported.
:- endif.



:- include(prologmud(mud_footer)).


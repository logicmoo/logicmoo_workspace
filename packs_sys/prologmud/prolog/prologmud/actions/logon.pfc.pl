/* * module *
% logon.pl
%
% This file defines how the agents gain their presence and leave the sim
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :-swi_module(user). 
:-swi_module(modLogin, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).
:- multifile lmcache:wants_logout/1.
:- dynamic lmcache:wants_logout/1.

% rename
baseKB:action_info(actRename(ftString),"Rename your player").
baseKB:agent_call_command(Agent,actRename(Other)):- padd(Agent,mudNamed(Other)).

% become
baseKB:text_actverb(become,actLogin).

% logon
baseKB:text_actverb(logon,actLogin).

% login
baseKB:action_info(actLogin(isOptional(tAgent,isRandom(tAgent))),"(Re)Login and assume the role of an agent").
baseKB:agent_call_command(Agent,actLogin(Other)):- show_call(become_player(Agent,Other)).

% logout
baseKB:action_info(actLogout(isOptional(tAgent,isSelfAgent)),"logs out of game (quits)").
baseKB:agent_call_command(_Agent,actLogout(Other)):-get_agent_session(Other,O),call(asserta(lmcache:wants_logout(O))).

% quit
baseKB:text_actverb(quit,actLogout).

% logoff
baseKB:text_actverb(logoff,actLogout).


:- include(prologmud(mud_footer)).

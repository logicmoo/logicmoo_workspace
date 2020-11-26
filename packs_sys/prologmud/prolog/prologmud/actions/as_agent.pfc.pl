% as_agent.pl
% Dec 13, 2035
% Douglas Miles
%
% This allows control of any cha5racter from any otgher character

:- swi_module(as_agent, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

action_info('actAs'(tAgent,ftVerbAction), "actAs <agent> <command>").
agent_call_command(_Agent,'actAs'(OtherAgent,Command)):- do_agent_action(OtherAgent,Command).

:- include(prologmud(mud_footer)).

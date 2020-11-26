% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This file defines the agents action of eating. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
*/

% :-swi_module(user). 
:-swi_module(where_cmd, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% where 
agent_text_command(Agent,["where",BE,X],Agent,actWhere(X)):-memberchk(BE,[is,are,be,were]).
agent_text_command(Agent,["where_is",X],Agent,actWhere(X)).
action_info(actWhere(ftTerm),"Tells where something is").
agent_call_command(_Agent,actWhere(SObj)) :-
    forall(
     (mudAtLoc(Obj,LOC), match_object(SObj,Obj)),
        fmt(cmdresult(actWhere,mudAtLoc(Obj,LOC)))).


action_info(actWho(isOptional(tAgent,isMissing)),"Lists who is online (where they are at least)").

agent_call_command(_Gent,actWho(W)) :- must(mud_cmd_who(W)),!.
agent_call_command(_Gent,actWho) :- must(mud_cmd_who(_W)),!.

mud_cmd_who(isMissing):- mud_cmd_who_1(_),!.
mud_cmd_who(Who):- mud_cmd_who_1(Who),!.

mud_cmd_who_1(Who):-
    must( forall(no_repeats(tAgent(Who)),
      ignore((
       no_repeats(inRegion(Who,Where)),
       ignore(lmcache:agent_session(Who,Session)),
       fmt(cmdresult(actWho(Who),inRegion(Who,Where),agent_session(Who,Session))))))).

:- include(prologmud(mud_footer)).



% :-swi_module(user). 
%:-swi_module(get_set, []).
/* * module * Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

action_info(actGet(isOptional(ftTerm,isSelfAgent),tPred),ftText("@get term to a property")).
action_info(actSet(isOptional(ftTerm,isSelfAgent),tPred,ftTerm),ftText("@sets term to a property")).

agent_text_command(Agent,["@set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,["@get",Prop0],Agent,actGet(Agent,Prop0)).
agent_text_command(Agent,["@","set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,["@","get",Prop0],Agent,actGet(Agent,Prop0)).
agent_text_command(Agent,["@_set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,["@_get",Prop0],Agent,actGet(Agent,Prop0)).

agent_call_command(Agent,actSet(Obj0,Prop0,Value0)) :- coerce(Prop0,tPred,Prop,Prop0),subst(ain(t(Prop,Obj0,Value0)),isSelfAgent,Agent,K),dmsg(K),on_x_debug(K).

agent_call_command(Agent,actGet(Obj0,Prop0)) :- subst(t(Prop0,Obj0,Value),isSelfAgent,Agent,K), 
                                                        catch((findall(Value,(req1(K),fmt(K)),L),
                                                          (L==[_|_]->true;fmt(wasMissing(K)))),E,fmt('@get Error ~q',[E:K])).

:- include(prologmud(mud_footer)).

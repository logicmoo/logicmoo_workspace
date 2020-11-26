/*  *  <module> 
% Uses affordances in which agents belief that some outcome will happen
% We also in this file purposelty create disparities
% Example: Buy this new car and you will suddenly become sexy!
%  Result: less money in pocket now but have vehical - but not sexier!
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- op(1200,yfx,'==>').



% If you want to do Something to Obj and you are in the same place.. Go ahead and do it!
% That means: remove its on the todo list and enqueue the action)
% We might change the later to enqueing it and then only remove from bucket list once it is completed (or action is started)

((agentTODO(Agent,actDo(Something,Obj)),
   localityOfObject(Obj,LOC),
   localityOfObject(Agent,LOC)) ==> 
     (~ agentTODO(Agent,actDo(Something,Obj)),
     {find_and_call(enqueue_agent_action(Agent,actDo(Something,Obj)))})).

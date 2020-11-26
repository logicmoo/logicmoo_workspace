% any.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic ANY verb that has the four elements of 
% 
%  action_requires_states(Agent,StateRules,REQS),
%  action_removes_states(Agent,StateRules,REMS),
%  states_types(Agent,StateRules,TYPES),
%  action_adds_states(Agent,StateRules,ADDS),
% 

% :-swi_module(user). 
:- swi_module(any, []).



:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).
action_rules(_,_,_,_):-fail.

to_verb_args(Act,List):-safe_univ(Act,List).

action_adds_states(Agent,Action,A):- action_rules(Agent,VERB,SENT,StateRules),to_verb_args(Action,[VERB|SENT]), member(+(A),StateRules).
action_removes_states(Agent,Action,A):- action_rules(Agent,VERB,SENT,StateRules),to_verb_args(Action,[VERB|SENT]), member(-(A),StateRules).
action_requires_states(Agent,Action,A):- action_rules(Agent,VERB,SENT,StateRules),to_verb_args(Action,[VERB|SENT]),member(?(A),StateRules).

:- op(200,fy,'?').
==>action_rules(Agent,actTestWield,[Obj],[?mudPossess(Agent,Obj),?isa(Obj,tUseAble),-mudStowing(Agent,Obj),+mudWielding(Agent,Obj)]).
==>action_rules(Agent,actTestStow,[Obj],[?mudPossess(Agent,Obj),?isa(Obj,tStowAble),?genlPreds(Using,'mudControls'),
  - t(Using,Agent,Obj),+mudStowing(Agent,Obj)]).

guess_verb_template(Action):-
       action_rules(_Agent,Verb,Args,ListA),
       once((to_verb_args(Action,[Verb|Args]),
            must(attempt_attribute_args(_AndOr,ftAskable,ListA)),
            must(term_variables(ListA,Vars)),
            must(attribs_to_atoms(Vars,Vars)))). 
         

% Use something
agent_call_command(Agent,ACT) :-
   call((action_rules(Agent,VERB,SENT,_StateRules),safe_univ(ACT,[VERB|SENT]))),
   
      action_requires_states(Agent,[VERB|SENT],REQS),
      action_removes_states(Agent,[VERB|SENT],REMS),
      action_adds_states(Agent,[VERB|SENT],ADDS),
     call_update_charge(Agent,VERB),
     ((
         req1(REQS)) ->
         ((clr(REMS),
         ain(ADDS),
         call_update_charge(Agent,VERB)));	
%Nothing to use
      (add_cmdfailure(Agent,SENT))).

:- include(prologmud(mud_footer)).

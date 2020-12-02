/*  Part of Optic Planner interface for SWI-Prolog

    Author:        Andrew Dougherty, Douglas Miles
    E-mail:        andrewdo@frdcsa.org, logicmoo@gmail.com
    WWW:           https://github.com/logicmoo/planner_external_api
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
":typing" | ":strips" | ":equality" | ":fluents" | ":durative-actions" | ":duration-inequalities" | ":numeric-fluents" | ":action-costs" | ":adl" | ":negative-preconditions" | ":disjunctive-preconditions" | ":existential-preconditions" | "universal-preconditions" | "quantified-preconditions" | ":conditional-effects" | ":timed-initial-literals" | ":preferences" | ":constraints"

":domain-axioms" | ":derived-predicates" ":action-expansions" | ":foreach-expansions" | ":dag-expansions" | ":subgoal-through-axioms" | ":safety-constraints" | ":expression-evaluation" | ":open-world" | ":true-negation" | ":ucpop"

*/

:- module(planner_external_interface, [
          planner_program/2,   % planner_program(Program)
          planner_workspace/2,   % planner_workspace(Opr,W)
          planner_workspace_program/3, % planner_workspace_program(Opr,W,Program)
          planner_requirement/3,   % planner_requirement(Opr,W,Require)
          planner_init/3, % planner_init(Opr,W,Fact)
          planner_predicate/3, % planner_predicate(Opr,W,Predicate)
          planner_function/3, % planner_function(Opr,W,Function)
          planner_type/3, % planner_type(Opr,W,Sort)
          planner_object/3, % planner_object(Opr,W,Object)
          planner_derived/4, % planner_derived(Opr,W,Fact,Condition) 
          planner_axiom/3, % planner_axiom(Opr,W,Axiom)
          planner_action/4, % planner_action(Opr,W,Action,Info)
          planner_copy_workspace/2, % planner_copy_workspace(+W,?NewWorkspace)
          planner_load_file/2, % planner_load_file(+W,+FileName)
  planner_current_program/1,
  planner_add_program/1,
  planner_remove_program/1,
  planner_current_workspace/1,
  planner_add_workspace/1,
  planner_remove_workspace/1,
  planner_current_workspace_program/2,
  planner_add_workspace_program/2,
  planner_remove_workspace_program/2,
  planner_current_requirement/2,
  planner_add_requirement/2,
  planner_remove_requirement/2,
  planner_current_init/2,
  planner_add_init/2,
  planner_remove_init/2,
  planner_current_predicate/2,
  planner_add_predicate/2,
  planner_remove_predicate/2,
  planner_current_function/2,
  planner_add_function/2,
  planner_remove_function/2,
  planner_current_type/2,
  planner_add_type/2,
  planner_remove_type/2,
  planner_current_object/2,
  planner_add_object/2,
  planner_remove_object/2,
  planner_current_derived/3,
  planner_add_derived/3,
  planner_remove_derived/3,
  planner_current_axiom/2,
  planner_add_axiom/2,
  planner_remove_axiom/2,
  planner_current_action/3,
  planner_add_action/3,
  planner_remove_action/3,
   planner_get_plan/3, % planner_get_plan(+W,+Goal,-Plan)
   planner_get_plan/4, % planner_get_plan(+W,+Planner,+Goal,-Plan)
   planner_apply_step/3, % planner_apply_step(+W,+Step,-NewWorkspace)
   planner_apply_step/4, % planner_apply_step(+W,+Planner,+Step,-NewWorkspace)
   ensure_external_planners/0,
   planner_debug/1,
   make_api/0

  ]).

make_api:-
 maplist(make_one_api,
   [program/2,   % program(Program)
   workspace/2,   % workspace(Opr,W)
   workspace_program/3, % workspace_program(Opr,W,Program)
   requirement/3,   % requirement(Opr,W,Require)
   init/3, % init(Opr,W,Fact)
   predicate/3, % predicate(Opr,W,Predicate)
   function/3, % function(Opr,W,Function)
   type/3, % type(Opr,W,Sort)
   object/3, % object(Opr,W,Object)
   derived/4, % derived(Opr,W,Fact,Conditions) 
   axiom/3, % axiom(Opr,W,Axiom)
   action/4]). % action(Opr,W,Action,Info)
make_exports:-
 maplist(make_one_export,
   [program/2,   % program(Program)
   workspace/2,   % workspace(Opr,W)
   workspace_program/3, % workspace_program(Opr,W,Program)
   requirement/3,   % requirement(Opr,W,Require)
   init/3, % init(Opr,W,Fact)
   predicate/3, % predicate(Opr,W,Predicate)
   function/3, % function(Opr,W,Function)
   type/3, % type(Opr,W,Sort)
   object/3, % object(Opr,W,Object)
   derived/4, % derived(Opr,W,Fact,Conditions) 
   axiom/3, % axiom(Opr,W,Axiom)
   action/4]). % action(Opr,W,Action,Info)

make_one_export(F/Am1):- A is Am1-1,
  make_one_export_fa(current,F,A),
  make_one_export_fa(add,F,A),
  make_one_export_fa(remove,F,A),!.

make_one_export_fa(C,F,A):- format('\n  planner_~w_~w/~w, ',[C,F,A]).
           
make_one_api(F/4):- !,  
  make_one_api_34(current,"+Workspace, ?",F,nondet,"Gets each",?,", Conds"),
  make_one_api_34(add,"+Workspace, +",F,det,"Adds one",+,", Conds"),
  make_one_api_34(remove,"+Workspace, +",F,det,"Removes one",-,", Conds"),!.

make_one_api(F/3):- !,  
  make_one_api_34(current,"+Workspace, ?",F,nondet,"Gets each",?,""),
  make_one_api_34(add,"+Workspace, +",F,det,"Adds one",+,""),
  make_one_api_34(remove,"+Workspace, +",F,det,"Removes one",-,""),!.

make_one_api(F/2):- !,  
  make_one_api_2(current,"?",F,nondet,"Gets each",?,""),
  make_one_api_2(add,"+",F,det,"Adds one",+,""),
  make_one_api_2(remove,"+",F,det,"Removes one",-,""),!.

make_one_api_2(Current,Mode,F,Det,String,Quest,ExtraArgs):-
  make_one_api_234(Current,Mode,F,Det,String,Quest,"",ExtraArgs).
make_one_api_34(Current,Mode,F,Det,String,Quest,ExtraArgs):-
make_one_api_234(Current,Mode,F,Det,String,Quest,"Workspace, ",ExtraArgs).
  

make_one_api_234(Current,Mode,F,Det,String,Quest,WSC,ExtraArgs):-
 toPropercase(F,CapsF), 
 (Quest=='?' -> From = "contained in";
 Quest=='+' -> From = "into";
 Quest=='-' -> From = "from"),
 upcase_atom(F,UP),
  format('
%! planner_~w_~w(~w~w~w) is ~w.
% 
%   ~w ~w ~w the Workspace. 
%
%      (PDDL''s :~w directive)
%
planner_~w_~w(~w~w~w):- 
      planner_~w(~w, ~w~w~w).

',[Current,F,Mode,CapsF,ExtraArgs,Det,
  String,CapsF,From,
  UP,
  Current,F,WSC,CapsF,ExtraArgs,
  F,Quest,WSC,CapsF,ExtraArgs]).
   

%! planner_current_program(?Program) is nondet.
%
%   Gets each Program contained in the Workspace.
%
%      (PDDL's :PROGRAM directive)
%
planner_current_program(Program):-
      planner_program(?, Program).


%! planner_add_program(+Program) is det.
%
%   Adds one Program into the Workspace.
%
%      (PDDL's :PROGRAM directive)
%
planner_add_program(Program):-
      planner_program(+, Program).


%! planner_remove_program(+Program) is det.
%
%   Removes one Program from the Workspace.
%
%      (PDDL's :PROGRAM directive)
%
planner_remove_program(Program):-
      planner_program(-, Program).


%! planner_current_workspace(?Workspace) is nondet.
%
%   Gets each Workspace contained in the Workspace.
%
%      (PDDL's :WORKSPACE directive)
%
planner_current_workspace(Workspace):-
      planner_workspace(?, Workspace).


%! planner_add_workspace(+Workspace) is det.
%
%   Adds one Workspace into the Workspace.
%
%      (PDDL's :WORKSPACE directive)
%
planner_add_workspace(Workspace):-
      planner_workspace(+, Workspace).


%! planner_remove_workspace(+Workspace) is det.
%
%   Removes one Workspace from the Workspace.
%
%      (PDDL's :WORKSPACE directive)
%
planner_remove_workspace(Workspace):-
      planner_workspace(-, Workspace).


%! planner_current_workspace_program(+Workspace, ?Workspace_Program) is nondet.
%
%   Gets each Workspace_Program contained in the Workspace.
%
%      (PDDL's :WORKSPACE_PROGRAM directive)
%
planner_current_workspace_program(Workspace, Workspace_Program):-
      planner_workspace_program(?, Workspace, Workspace_Program).


%! planner_add_workspace_program(+Workspace, +Workspace_Program) is det.
%
%   Adds one Workspace_Program into the Workspace.
%
%      (PDDL's :WORKSPACE_PROGRAM directive)
%
planner_add_workspace_program(Workspace, Workspace_Program):-
      planner_workspace_program(+, Workspace, Workspace_Program).


%! planner_remove_workspace_program(+Workspace, +Workspace_Program) is det.
%
%   Removes one Workspace_Program from the Workspace.
%
%      (PDDL's :WORKSPACE_PROGRAM directive)
%
planner_remove_workspace_program(Workspace, Workspace_Program):-
      planner_workspace_program(-, Workspace, Workspace_Program).


%! planner_current_requirement(+Workspace, ?Requirement) is nondet.
%
%   Gets each Requirement contained in the Workspace.
%
%      (PDDL's :REQUIREMENT directive)
%
planner_current_requirement(Workspace, Requirement):-
      planner_requirement(?, Workspace, Requirement).


%! planner_add_requirement(+Workspace, +Requirement) is det.
%
%   Adds one Requirement into the Workspace.
%
%      (PDDL's :REQUIREMENT directive)
%
planner_add_requirement(Workspace, Requirement):-
      planner_requirement(+, Workspace, Requirement).


%! planner_remove_requirement(+Workspace, +Requirement) is det.
%
%   Removes one Requirement from the Workspace.
%
%      (PDDL's :REQUIREMENT directive)
%
planner_remove_requirement(Workspace, Requirement):-
      planner_requirement(-, Workspace, Requirement).


%! planner_current_init(+Workspace, ?Init) is nondet.
%
%   Gets each Init contained in the Workspace.
%
%      (PDDL's :INIT directive)
%
planner_current_init(Workspace, Init):-
      planner_init(?, Workspace, Init).


%! planner_add_init(+Workspace, +Init) is det.
%
%   Adds one Init into the Workspace.
%
%      (PDDL's :INIT directive)
%
planner_add_init(Workspace, Init):-
      planner_init(+, Workspace, Init).


%! planner_remove_init(+Workspace, +Init) is det.
%
%   Removes one Init from the Workspace.
%
%      (PDDL's :INIT directive)
%
planner_remove_init(Workspace, Init):-
      planner_init(-, Workspace, Init).


%! planner_current_predicate(+Workspace, ?Predicate) is nondet.
%
%   Gets each Predicate contained in the Workspace.
%
%      (PDDL's :PREDICATE directive)
%
planner_current_predicate(Workspace, Predicate):-
      planner_predicate(?, Workspace, Predicate).


%! planner_add_predicate(+Workspace, +Predicate) is det.
%
%   Adds one Predicate into the Workspace.
%
%      (PDDL's :PREDICATE directive)
%
planner_add_predicate(Workspace, Predicate):-
      planner_predicate(+, Workspace, Predicate).


%! planner_remove_predicate(+Workspace, +Predicate) is det.
%
%   Removes one Predicate from the Workspace.
%
%      (PDDL's :PREDICATE directive)
%
planner_remove_predicate(Workspace, Predicate):-
      planner_predicate(-, Workspace, Predicate).


%! planner_current_function(+Workspace, ?Function) is nondet.
%
%   Gets each Function contained in the Workspace.
%
%      (PDDL's :FUNCTION directive)
%
planner_current_function(Workspace, Function):-
      planner_function(?, Workspace, Function).


%! planner_add_function(+Workspace, +Function) is det.
%
%   Adds one Function into the Workspace.
%
%      (PDDL's :FUNCTION directive)
%
planner_add_function(Workspace, Function):-
      planner_function(+, Workspace, Function).


%! planner_remove_function(+Workspace, +Function) is det.
%
%   Removes one Function from the Workspace.
%
%      (PDDL's :FUNCTION directive)
%
planner_remove_function(Workspace, Function):-
      planner_function(-, Workspace, Function).


%! planner_current_type(+Workspace, ?Type) is nondet.
%
%   Gets each Type contained in the Workspace.
%
%      (PDDL's :TYPE directive)
%
planner_current_type(Workspace, Type):-
      planner_type(?, Workspace, Type).


%! planner_add_type(+Workspace, +Type) is det.
%
%   Adds one Type into the Workspace.
%
%      (PDDL's :TYPE directive)
%
planner_add_type(Workspace, Type):-
      planner_type(+, Workspace, Type).


%! planner_remove_type(+Workspace, +Type) is det.
%
%   Removes one Type from the Workspace.
%
%      (PDDL's :TYPE directive)
%
planner_remove_type(Workspace, Type):-
      planner_type(-, Workspace, Type).


%! planner_current_object(+Workspace, ?Object) is nondet.
%
%   Gets each Object contained in the Workspace.
%
%      (PDDL's :OBJECT directive)
%
planner_current_object(Workspace, Object):-
      planner_object(?, Workspace, Object).


%! planner_add_object(+Workspace, +Object) is det.
%
%   Adds one Object into the Workspace.
%
%      (PDDL's :OBJECT directive)
%
planner_add_object(Workspace, Object):-
      planner_object(+, Workspace, Object).


%! planner_remove_object(+Workspace, +Object) is det.
%
%   Removes one Object from the Workspace.
%
%      (PDDL's :OBJECT directive)
%
planner_remove_object(Workspace, Object):-
      planner_object(-, Workspace, Object).


%! planner_current_derived(+Workspace, ?Derived, ?Conds) is nondet.
%
%   Gets each Derived contained in the Workspace.
%
%      (PDDL's :DERIVED directive)
%
planner_current_derived(Workspace, Derived, Conds):-
      planner_derived(?, Workspace, Derived, Conds).


%! planner_add_derived(+Workspace, +Derived, +Conds) is det.
%
%   Adds one Derived into the Workspace.
%
%      (PDDL's :DERIVED directive)
%
planner_add_derived(Workspace, Derived, Conds):-
      planner_derived(+, Workspace, Derived, Conds).


%! planner_remove_derived(+Workspace, +Derived, +Conds) is det.
%
%   Removes one Derived from the Workspace.
%
%      (PDDL's :DERIVED directive)
%
planner_remove_derived(Workspace, Derived, Conds):-
      planner_derived(-, Workspace, Derived, Conds).


%! planner_current_axiom(+Workspace, ?Axiom) is nondet.
%
%   Gets each Axiom contained in the Workspace.
%
%      (PDDL's :AXIOM directive)
%
planner_current_axiom(Workspace, Axiom):-
      planner_axiom(?, Workspace, Axiom).


%! planner_add_axiom(+Workspace, +Axiom) is det.
%
%   Adds one Axiom into the Workspace.
%
%      (PDDL's :AXIOM directive)
%
planner_add_axiom(Workspace, Axiom):-
      planner_axiom(+, Workspace, Axiom).


%! planner_remove_axiom(+Workspace, +Axiom) is det.
%
%   Removes one Axiom from the Workspace.
%
%      (PDDL's :AXIOM directive)
%
planner_remove_axiom(Workspace, Axiom):-
      planner_axiom(-, Workspace, Axiom).


%! planner_current_action(+Workspace, ?Action, ?Conds) is nondet.
%
%   Gets each Action contained in the Workspace.
%
%      (PDDL's :ACTION directive)
%
planner_current_action(Workspace, Action, Conds):-
      planner_action(?, Workspace, Action, Conds).


%! planner_add_action(+Workspace, +Action, +Conds) is det.
%
%   Adds one Action into the Workspace.
%
%      (PDDL's :ACTION directive)
%
planner_add_action(Workspace, Action, Conds):-
      planner_action(+, Workspace, Action, Conds).


%! planner_remove_action(+Workspace, +Action, +Conds) is det.
%
%   Removes one Action from the Workspace.
%
%      (PDDL's :ACTION directive)
%
planner_remove_action(Workspace, Action, Conds):-
      planner_action(-, Workspace, Action, Conds).


planner_copy_workspace(Workspace,NewWorkspace):- 
   (var(NewWorkspace)->gensym(Workspace,NewWorkspace);true),
   forall(mdata:ws_data(Workspace,P,V),call_ws_data_hook(+,NewWorkspace,P,V)).

planner_load_file(W,FileName):- planner_missing(planner_load_file(W,FileName)).

%! planner_workspace(+Opr,+Workspace) is det.
%
% + = Adds a workspace name
% - = Deletes workspace freeing up resources
% ? = Enumerates workspace names
%
planner_workspace(Opr,Workspace):- 
  call_settings_data(Opr,ws_data(Workspace,isa,tWorkspace)).


%! planner_program(+Opr,?Program) is nondet.
%
% + = Adds a planner program name
% - = Deletes program freeing up resources
% ? = Enumerates planner program names
%
planner_program(Opr,Program):- call_settings_data(Opr,current_planner_program(Program)).


planner_data_template(current_planner_program(_Program)).
planner_data_template(ws_data(_W,_P,_D)).

pre_existing_clause(MData,R):- strip_module(MData,M,Data),
  clause(M:Data,true,R),clause(MCData,true,R),strip_module(MCData,_,CData),Data=@=CData,!.

:- dynamic(mdata:current_planner_program/1).
:- dynamic(mdata:ws_data/3).

to_mdata(Data,mdata:BData):- strip_module(Data,_,BData).

call_ws_data_hook(Opr,W,Prop,DataL):- 
  check_opr(W,Opr),
  forall(delistify_data(DataL,Data),
       call_settings_data(Opr,ws_data(W,Prop,Data))).

call_settings_data(Opr,Data):- to_mdata(Data,MData), call_settings_mdata(Opr,MData).
call_settings_mdata(?,MData):- !, call(MData).
call_settings_mdata(+,MData):-  !, (pre_existing_clause(MData,_R)->true;
   (asserta(MData),planner_debug(asserta(MData)))).
call_settings_mdata(-,MData):- ignore(call(MData)),retractall(MData).

delistify_data(DataL,Data):- is_list(DataL),!,member(Data,DataL).
delistify_data(Data,Data).

% Manipulate PDDL Workspace Dfault Planner Program
planner_workspace_program(Opr,W,Program):- 
  (call_settings_data(Opr,ws_data(W,program,Program))
   *->true;
   call_settings_data(Opr,current_planner_program(Program))).
  

% Manipulate PDDL Workspace Problem/Domains (:Requirements ...)
planner_requirement(Opr,W,Require):- call_ws_data_hook(Opr,W,requirement,Require).

% Manipulate PDDL Workspace Problem/Domains (:Init ...)
planner_init(Opr,W,Fact):- 
  glean_objs(Opr,W,Fact),
  call_ws_data_hook(Opr,W,init,Fact).

% Manipulate PDDL Workspace Problem/Domains (:Predicates ...)
planner_predicate(Opr,W,Predicate):- 
  glean_types(Opr,W,Predicate),
  call_ws_data_hook(Opr,W,predicate,Predicate).

% Manipulate PDDL Workspace Problem/Domains (:Functions ...)
planner_function(Opr,W,Function):- 
  glean_types(Opr,W,Function),
  call_ws_data_hook(Opr,W,function,Function).

% Manipulate PDDL Workspace Problem/Domains (:TYPE ...)
planner_type(Opr,W,Type):-
  glean_types(Opr,W,Type),
  call_ws_data_hook(Opr,W,type,Type).

% Manipulate PDDL Workspace Problem/Domains (:OBJECTS ...)
planner_object(Opr,W,Object):- 
  glean_types(Opr,W,Object),
  call_ws_data_hook(Opr,W,object,Object).

% Manipulate a PDDL Workspace Problem/Domains (:derived-predicates ...)
planner_derived(Opr,W,Fact,Cond) :- Cond==[], !, planner_init(Opr,W,Fact).
planner_derived(Opr,W,Fact,Condition) :- 
  FULL= derived(Fact,Condition),
  numbervars(FULL),
  call_ws_data_hook(Opr,W,derived,FULL).


% Manipulate a PDDL Workspace Problem/Domains (:axiom ...)
planner_axiom(Opr,W,Axiom):- call_ws_data_hook(Opr,W,axiom,Axiom).

% Manipulate a PDDL Workspace Problem/Domains (:action Action (...Info...))
planner_action(Opr,W,Action,InfoList):- glean_types(Opr,W,Action),   
   numbervars(InfoList),
   planner_action_info(Opr,W,Action,InfoList),
   !. % call_ws_data_hook(Opr,W,action,act_info(Action,InfoList)).

planner_action_info(Opr,W,Action,InfoList):- 
  is_list(InfoList),!,maplist(planner_action(Opr,W,Action),InfoList).
planner_action_info(Opr,W,Action,Type:Info):-!,
   call_ws_data_hook(Opr,W,action,act_inf(Action,Type,Info)).
planner_action_info(Opr,W,Action,Info):-
   call_ws_data_hook(Opr,W,action,act_inf(Action,meta,Info)).


%% planner_get_plan(+W,+Goal,-Plan) is nondet.
planner_get_plan(W,Goal,Plan):-
  planner_workspace_program(?,W,Planner),
  planner_get_plan(Planner,W,Goal,Plan).

%% planner_get_plan(+Planner,+W,+Goal,-Plan) is nondet.
planner_get_plan(Planner,W,Goal,Plan):-   
  check_workspace(W),
  ignore(Plan=[s1,s2,s3]),
  planner_missing(planner_get_plan(Planner,W,Goal,Plan)).

%% planner_apply_step(+W,+Step,-NewWorkspace) is det.
planner_apply_step(W,Step,NewWorkspace):-
  planner_workspace_program(?,W,Planner),
  planner_apply_step(Planner,W,Step,NewWorkspace).

%% planner_apply_step(+Planner,+W,+Step,-NewWorkspace) is det.
planner_apply_step(Planner,W,Step,NewWorkspace):-
 check_workspace(W),
 (var(NewWorkspace)->gensym(W,NewWorkspace);true),
 check_workspace(NewWorkspace),
 planner_missing(planner_apply_step(Planner,W,Step,NewWorkspace)).


ensure_external_planners.

glean_types(Opr,W,Any):- Opr == +,!,
  check_opr(W,Opr),
  forall((sub_term(Sub,Any),
  compound(Sub),member(Sub,[_-Type, Type:_])),
  planner_type(Opr,W,Type)).
glean_types(_,_,_).   


glean_objs(Opr,W,Any):- Opr == +,!,
  forall((sub_term(Sub,Any),
   compound(Sub),member(Sub,[Obj-_])),
   planner_object(Opr,W,Obj)),
  forall((sub_term(Obj,Any),
  atom(Obj)),planner_object(Opr,W,Obj)).
glean_objs(_,_,_).   

check_opr(W,+):- check_workspace(W).
check_opr(W,del):- check_workspace(W).
check_opr(W,?):- check_workspace(W).
check_opr(_Workspace,Opr):- throw(opr_missing(Opr)).

check_workspace(W):- mdata:ws_data(W,isa,tWorkspace),!.
check_workspace(W):- asserta(mdata:ws_data(W,isa,tWorkspace)).

planner_debug(Info):- format('~N% ~q.~n',[Info]).

planner_missing(Goal):- !,planner_debug(g(Goal)).
planner_missing(Goal):- throw(planner_missing(Goal)).



%e_member([L|ST],E):- nonvar(L),!,member(E,[L|ST]).
%e_member(E,E).

end_of_file.

/*
(:constraints (and (always-until (charged ?r) (at ?r rechargepoint)) 
  (always-within 10 (< (charge ?r) 5) (at ?r rechargingpoint))))

(:constraints
    (and (preference
         (always (forall (?b1 ?b2 - block ?c1 ?c2 - color)
                         (implies (and (on ?b1 ?b2)
                                       (color ?b1 ?c1)
                                       (color ?b2 ?c2))
                                       (= ?c1 ?c2))))))
)

(:constraints
    (and (always (forall (?b1 ?b2 - block)
                 (implies (and (fragile ?b1) (on ?b2 ?b1))
                               (clear ?b2)))))
)

*/





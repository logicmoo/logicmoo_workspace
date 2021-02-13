/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).

action_invoke_goals(Agent, Mem0, Mem0):-
  \+ thought_check(Agent, current_goals(Agent, [_|_]), Mem0), !,
 dbug(planner, '~w: no goals exist~n', [Agent]).

action_invoke_goals(Agent, Mem0, Mem1):-
 dbug(planner, '~w: goals exist: generating a plan...~n', [Agent]),
 Knower = Agent,
 generate_plan(Knower, Agent, NewPlan, Mem0), !,
 serialize_plan(Knower, Agent, NewPlan, Actions), !,
 dbug(planner, 'Planned actions are ~w~n', [Actions]),
 Actions = [Action|_],
 add_intent( Agent, Action, Mem0, Mem1).

% If goals exist, forget them (since the above failed)
action_invoke_goals(Agent, Mem0, Mem9) :-
 thought_check(Agent, current_goals(Agent, [G0|GS]), Mem0),
 replace_thought(Agent, current_goals(Agent, []), Mem0, Mem2),
 dbug(planner, '~w: Can`t solve goals ~p. Forgetting them.~n', [Agent, [G0|GS]]),
 memorize_appending(Agent, goals_skipped(Agent, [G0|GS]), Mem2, Mem9), !.


forget_satisfied_goals(Agent, Mem0, Mem3):-
 Goals = [_G0|_GS],
 thought_check(Agent, current_goals(Agent, Goals), Mem0),
 agent_thought_model(Agent, ModelData, Mem0),
 select_unsatisfied_conditions(Goals, Unsatisfied, ModelData) ->
 subtract(Goals, Unsatisfied, Satisfied), !,
 Satisfied \== [],
 replace_thought(Agent, current_goals(Agent, Unsatisfied), Mem0, Mem2),
 dbug(planner, '~w Goals some Satisfied: ~p.  Unsatisfied: ~p.~n', [Agent, Satisfied, Unsatisfied]),
 memorize_appending(Agent, goals_satisfied(Agent, Satisfied), Mem2, Mem3), !.

has_unsatisfied_goals(Agent, Mem0, Mem0):-
 agent_thought_model(Agent, ModelData, Mem0),
 thought_check(Agent, current_goals(Agent, [_|_]), ModelData).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_plan_opers')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- op(900, fy, '~').

/*
Situation:

Person is in Kitchen
Floyd is in Pantry
The Pantry is North of Kitchen


Person wants goal satification to be easy.
Person thinks if they know their environment, goal satisfiation will become easier.
Person goal is to know environment.
Person thinks if one is being an explorer, they will know their environment.
Person think doing what explorers do will make persons goal satisfaction easier.
Person thinks being an explorer means to find unexplored exits and travel to them.
Person thinks exits are known by looking.
Person goal is to have looked
Person the way to satifiy the goal to have looked is to: add_intent( Person, ( act3('look',Person,[])))
Person DOES ( act3('look',Person,[]))
Person notices exits: north, south, east, west.
Person thinks north is unexplored
Person thinks going north will be acting like an explorer
Person goal is to go north
Person makes plan to go north.. the plan is very simple: [ ( act3('go__dir',Person,[ walk, north]))]
Person DOESdo_go_dir(Person, walk, north)
Person leaves kitchen to the north
In Kitchen, thus Person, sees Person did_depart kitchen to the north
Person enters pantry from the south
In Pantry, thus Floyed and Person, sees Person enter pantry did_arrive from the south
Floyd belives Person was somewhere other than pantry before
Floyd belives Person traveled north and there might be an exit in the opposite dirrection (south) leading somewhere other than pantry
Person belives pantry is where they end up if they go north from kitchen
Person belives kitchen is where they end up if they go south from pantry

'

do_look(Person) is a cheap and effective strategy


event( trys( ( act3('go__dir',Person,[ walk, north]))))





precond_matches_effect(Cond, Cond).

precond_matches_effects(path(Here, There), StartEffects) :-
 find_path(Agent, Here, There, _Route, StartEffects).
precond_matches_effects(exists(Object), StartEffects) :-
 in_model(h(spatial, _, Object, _), StartEffects)
 ;
 in_model(h(spatial, _, _, Object), StartEffects).
precond_matches_effects(Cond, Effects) :-
 in_model(E, Effects),
 precond_matches_effect(Cond, E).
*/

% oper(_Self, Action, Desc, Preconds, Effects)

sequenced(_Self,
  [ %Preconds:
  Here \= Self, There \= Self,
  \+ props(Self, knows_verbs(goto, f)),
  h(spatial, WasRel, Self, Here),
  props(Here, inherit(place, t)),
  props(There, inherit(place, t)),
  \+ in_state(~(open), There),
  \+ in_state(~(open), Here),
  \+ in_state(~(open), Dir),
  reverse_dir(Dir, RDir),
  h(spatial, fn(exit, Dir), Here, There), % path(Here, There)
  % %Action:
 did( Self, act3('go__dir',Self,[ Walk, Dir])),
  %PostConds:
  ~h(spatial, WasRel, Self, Here),
  notice(Here, leaves(Self, Here, WasRel)),
  notice(Self, msg([cap(subj(actor(Self))), does(Walk), from(place(Here)), via(fn(exit, Dir)) , Rel, to(place(There))])),
  h(spatial, Rel, Self, There),
  notice(There, enters(Self, There, RDir))]).


% planner_only:- nb_current(opers).



% Return an operator after substituting Agent for Self.
operagent(Agent, Action, BConds, BEffects) :-
 oper_splitk(Agent, Action, Conds, Effects),
   once((oper_beliefs(Agent, Conds, BConds),
      oper_beliefs(Agent, Effects, BEffects))).

oper_beliefs(_Agent, [], []):- !.
oper_beliefs(Agent, [ believe(Agent2, H)|Conds], [H|BConds]):- Agent == Agent2, !,
  oper_beliefs(Agent, Conds, BConds).
oper_beliefs(Agent, [ A\=B|Conds], [A\=B|BConds]):- !,
  oper_beliefs(Agent, Conds, BConds).
oper_beliefs(Agent, [ exists(B)|Conds], [exists(B)|BConds]):-
  oper_beliefs(Agent, Conds, BConds).
oper_beliefs(Agent, [ _|Conds], BConds):-
  oper_beliefs(Agent, Conds, BConds).

% Return the initial list of operators.
initial_operators(Agent, Operators) :-
 findall(oper(Agent, Action, Conds, Effects),
   operagent(Agent, Action, Conds, Effects),
   Operators).


precondition_matches_effect(Cond, Effect) :-
 % player_format('  Comparing cond ~w with effect ~w: ', [Cond, Effect]),
 Cond = Effect. %, player_format('match~n', []).

%precondition_matches_effect(~ ~ Cond, Effect) :-
% precondition_matches_effect(Cond, Effect).
%precondition_matches_effect(Cond, ~ ~ Effect) :-
% precondition_matches_effect(Cond, Effect).

precondition_matches_effects(Cond, Effects) :-
 member(E, Effects),
 precondition_matches_effect(Cond, E).
preconditions_match_effects([Cond|Tail], Effects) :-
 precondition_matches_effects(Cond, Effects),
 preconditions_match_effects(Tail, Effects).

% plan(steps, orderings, bindings, links)
% step(id, operation)
%                 ModelData, Goals, SeedPlan
new_plan(Self, CurrentState, GoalState, Plan) :-
  new_plan_newver(Self, CurrentState, GoalState, Plan).


convert_state_to_goalstate(O, O):- pprint(convert_state_to_goalstate=O, planner).

%                       ModelData, Goals, SeedPlan
new_plan_newver(Self, CurrentState, GoalState, Plan) :-
 convert_state_to_goalstate(CurrentState, CurrentStateofGoals),
 gensym(ending_step_1, End),
 Plan =
 plan([step(start , oper(Self, act3('wait',Self,[]), [], CurrentStateofGoals)),
       step(completeFn(End), oper(Self,  act3('wait',Self,[]), GoalState, []))],
      [before(start, completeFn(End))],
      [],
      []).

/*
 new_plan_oldver(Self, CurrentState, GoalState, Plan) :-
 gensym(ending_step_1, End),
 Plan =
 plan([step(start , oper(Self, true, [], CurrentState)),
       step(completeFn(End), oper(Self, true, GoalState, []))],
      [before(start, completeFn(End))],
      [],
      []).
*/


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_util_ordering')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isbefore(I, J, Orderings) :-
 member(before(I, J), Orderings).
%isbefore(I, K, Orderings) :-
% select_from(before(I, J), Orderings, Remaining),
% isbefore(J, K, Remaining).

% These will fail to create inconsistent orderings.
%add_ordering(B, Orderings, Orderings) :-
% member(B, Orderings), !.
%add_ordering(before(I, K), Orderings, [before(I, K)|Orderings]) :-
% I \= K,
% \+ isbefore(K, I, Orderings),
% dbug(planner, ' ADDED ~w to orderings.~n', [before(I, K)]).
%add_ordering(B, O, O) :-
% dbug(planner, ' FAILED to add ~w to orderings.~n', [B]),
% fail.

add_ordering(B, Orderings, Orderings) :-
 member(B, Orderings), !.
add_ordering(before(I, J), Order0, Order1) :-
 I \= J,
 \+ isbefore(J, I, Order0),
 add_ordering3(before(I, J), Order0, Order0, Order1).
add_ordering(B, Order0, Order0) :-
 once(pick_ordering(Order0, List)),
 dbug(planner, ' FAILED add_ordering ~w to ~w~n', [B, List]),
 fail.

% add_ordering3(NewOrder, ToCheck, OldOrderings, NewOrderings)
add_ordering3(before(I, J), [], OldOrderings, NewOrderings) :-
 union([before(I, J)], OldOrderings, NewOrderings).
add_ordering3(before(I, J), [before(J, K)|Rest], OldOrderings, NewOrderings) :-
 I \= K,
 union([before(J, K)], OldOrderings, Orderings1),
 add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, I)|Rest], OldOrderings, NewOrderings) :-
 H \= J,
 union([before(H, J)], OldOrderings, Orderings1),
 add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, K)|Rest], OldOrderings, NewOrderings) :-
 I \= K,
 H \= J,
 add_ordering3(before(I, J), Rest, OldOrderings, NewOrderings).

% insert(E, L, L1) inserts E into L producing L1
% E is not added it is already there.
insert(X, [], [X]).
insert(A, [A|R], [A|R]).
insert(A, [B|R], [B|R1]) :-
 A \== B,
 insert(A, R, R1).

add_orderings([], Orderings, Orderings).
add_orderings([B|Tail], Orderings, NewOrderings) :-
 add_ordering(B, Orderings, Orderings2),
 add_orderings(Tail, Orderings2, NewOrderings).

del_ordering_node(I, [before(I)|Tail], Orderings) :-
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(_, I)|Tail], Orderings) :-
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(X, Y)|Tail], [before(X, Y)|Orderings]) :-
 X \= I,
 Y \= I,
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(_I, [], []).

ordering_nodes(Orderings, Nodes) :-
 setof(Node,
  Other^(isbefore(Node, Other, Orderings);isbefore(Other, Node, Orderings)),
  Nodes).

pick_ordering(Orderings, List) :-
 ordering_nodes(Orderings, Nodes),
 pick_ordering(Orderings, Nodes, List).

pick_ordering(Orderings, Nodes, [I|After]) :-
 select_from(I, Nodes, RemainingNodes),
 forall(member(J, RemainingNodes), \+ isbefore(J, I, Orderings) ),
 pick_ordering(Orderings, RemainingNodes, After).
pick_ordering(_Orderings, [], []).

test_ordering :-
 dbug(planner, 'ORDERING TEST:~n'),
 Unordered =
 [
  before(start, completeFn(End)),
  before(start, x),
  before(start, y),
  before(y, completeFn(End)),
  before(x, z),
  before(z, completeFn(End))
 ],
 once(add_orderings(
 Unordered,
 [],
 Orderings)),
 dbug(planner, ' unordered was ~w~n', [Unordered]),
 dbug(planner, ' ordering is ~w~n', [Orderings]),
 pick_ordering(Orderings, List),
 dbug(planner, ' picked ~w~n', [List]),
 fail.
test_ordering :- dbug(planner, ' END ORDERING TEST~n').


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_planner_conds')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cond_is_achieved(step(J, _Oper), C, plan(Steps, Orderings, _)) :-
 member(step(I, oper(_Self, _, _, Effects)), Steps),
 precondition_matches_effects(C, Effects),
 isbefore(I, J, Orderings),
 dbug(planner, '  Cond ~w of step ~w is achieved!~n', [C, J]).
cond_is_achieved(step(J, _Oper), C, plan(_Steps, _Orderings, _)) :-
 dbug(planner, '  Cond ~w of step ~w is NOT achieved.~n', [C, J]),
 !, fail.

% Are the preconditions of a given step achieved by the effects of other
% steps, or are already true?
step_is_achieved(step(_J, oper(_Self, _, _, [])), _Planed). % No conditions, OK.
step_is_achieved(step(J, oper(Self, _, _, [C|Tail])), plan(Steps, Orderings, _)) :-
 cond_is_achieved(step(J, _), C, plan(Steps, Orderings, _)),
 step_is_achieved(step(J, oper(Self, _, _, Tail)), plan(Steps, Orderings, _)).

all_steps_are_achieved([Step|Tail], Plan) :-
 step_is_achieved(Step, Plan),
 all_steps_are_achieved(Tail, Plan).
all_steps_are_achieved([], _Planned).

is_solution(plan(Steps, O, B, L)) :-
 all_steps_are_achieved(Steps, plan(Steps, O, B, L)).

% Create a new step given an operator.
operator_as_step(oper(Self, Act, Cond, Effect), step(Id, oper(Self, Act, Cond, Effect))) :-
 Act =.. [Functor|_],
 atom_concat(Functor, '_step_', Prefix),
 gensym(Prefix, Id).

% Create a list of new steps given a list of operators.
operators_as_steps([], []).
operators_as_steps([Oper | OpTail], [Step | StepTail]) :-
 copy_term(Oper, FreshOper), % Avoid instantiating operator database.
 operator_as_step(FreshOper, Step),
 operators_as_steps(OpTail, StepTail).

cond_as_goal(ID, Cond, goal(ID, Cond)).
conds_as_goals(_, [], []).
conds_as_goals(ID, [C|R], [G|T]) :-
 cond_as_goal(ID, C, G),
 conds_as_goals(ID, R, T).

cond_equates(Cond0, Cond1) :- Cond0 = Cond1.
cond_equates(h(spatial, X, Y, Z), h(spatial, X, Y, Z)).
cond_equates(~ ~ Cond0, Cond1) :- cond_equates(Cond0, Cond1).
cond_equates(Cond0, ~ ~ Cond1) :- cond_equates(Cond0, Cond1).

cond_negates(~ Cond0, Cond1) :- cond_equates(Cond0, Cond1).
cond_negates(Cond0, ~ Cond1) :- cond_equates(Cond0, Cond1).

% Protect 1 link from 1 condition
% protect(link_to_protect, threatening_step, threatening_cond, ...)
protect(causes(StepI, _Cond0, _StepJ), StepI, _Cond1, Order0, Order0) :-
 !. % Step does not threaten itself.
protect(causes(_StepI, _Cond0, StepJ), StepJ, _Cond1, Order0, Order0) :-
 !. % Step does not threaten itself.
%protect(causes(_StepI, Cond, _StepJ), _StepK, Cond, Order0, Order0) :-
% !. % Cond does not threaten itself.
protect(causes(_StepI, Cond0, _StepJ), _StepK, Cond1, Order0, Order0) :-
 \+ cond_negates(Cond0, Cond1),
 !.
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
 dbug(planner, ' THREAT: ~w <> causes(~w, ~w, ~w)~n', 
   [StepK, StepI, Cond0, StepJ]),
 fail.
protect(causes(StepI, _Cond0, StepJ), StepK, _Cond1, Order0, Order1) :-
 % Protect by moving threatening step before or after this link.
 add_ordering(before(StepK, StepI), Order0, Order1),
 dbug(planner, ' RESOLVED with ~w~n', [before(StepK, StepI)])
 ;
 add_ordering(before(StepJ, StepK), Order0, Order1),
 dbug(planner, ' RESOLVED with ~w~n', [before(StepJ, StepK)]).
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
 dbug(planner, ' FAILED to resolve THREAT ~w <> causes(~w, ~w, ~w)~n', 
   [StepK, StepI, Cond0, StepJ]),
 once(pick_ordering(Order0, Serial)),
 dbug(planner, ' ORDERING is ~w~n', [Serial]),
 fail.

% Protect 1 link from 1 step's multiple effects
protect_link(_Link, _StepID, [], Order0, Order0).
protect_link(Link, StepID, [Cond|Effects], Order0, Order2):-
 protect(Link, StepID, Cond, Order0, Order1),
 protect_link(Link, StepID, Effects, Order1, Order2).

% Protect all links from 1 step''s multiple effects
% protect_links(links_to_protect, threatening_step, threatening_cond, ...)
protect_links([], _StepID, _Effects, Order0, Order0).
protect_links([Link|Tail], StepID, Effects, Order0, Order2) :-
 protect_link(Link, StepID, Effects, Order0, Order1),
 protect_links(Tail, StepID, Effects, Order1, Order2).

% Protect 1 link from all steps'' multiple effects
protect_link_all(_Link, [], Order0, Order0).
protect_link_all(Link, [step(StepID, oper(_Self, _, _, Effects))|Steps], Order0, Order2) :-
 protect_link(Link, StepID, Effects, Order0, Order1),
 protect_link_all(Link, Steps, Order1, Order2).

%add_binding((X\=Y), Bindings0, Bindings) :-
% X \= Y, % if they can''t bind, don''t bother to add them.
add_binding((X\=Y), Bindings, [(X\=Y)|Bindings]) :-
 X \== Y, % if they''re distinct,
 % \+ \+ X=Y, % but could bind
 bindings_valid(Bindings).

bindings_valid([]).
bindings_valid([(X\=Y)|Bindings]) :-
 X \== Y,
 bindings_valid(Bindings).
%bindings_valid(B) :-
% dbug(planner, ' BINDINGS are *INVALID*: ~w~n', [B]),
% fail.

bindings_safe([]) :- dbug(planner, ' BINDINGS are SAFE~n').
bindings_safe([(X\=Y)|Bindings]) :-
 X \= Y,
 bindings_safe(Bindings).
%bindings_safe(B) :-
% dbug(planner, ' BINDINGS are *UNSAFE*: ~w~n', [B]),
% fail.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_planner_main')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


choose_operator([goal(GoalID, GoalCond)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 % Achieved by existing step?
 member(step(StepID, oper(_Self, _Action, _Preconds, Effects)), Steps),
 precondition_matches_effects(GoalCond, Effects),
 add_ordering(before(StepID, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(StepID, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 dbug(planner, ' EXISTING step ~w satisfies ~w~n', [StepID, GoalCond]).
choose_operator([goal(_GoalID, X \= Y)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order, Bindings, Links),
     plan(Steps, Order, NewBindings, Links),
     Depth, Depth ) :-
 add_binding((X\=Y), Bindings, NewBindings),
 dbug(planner, ' BINDING ADDED: ~w~n', [X\=Y]).
choose_operator([goal(GoalID, ~ GoalCond)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 % Negative condition achieved by start step?
 memberchk(step(start, oper(_Self, _Action, _Preconds, Effects)), Steps),
 \+ precondition_matches_effects(GoalCond, Effects),
 add_ordering(before(start, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(start, ~ GoalCond, GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 dbug(planner, ' START SATISFIES NOT ~w~n', [GoalCond]).
choose_operator([goal(GoalID, exists(GoalCond))|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 memberchk(step(start, oper(_Self, _Action, _Preconds, Effects)), Steps),
 ( in_model(h(spatial, _Prep, GoalCond, _Where), Effects);
 in_model(h(spatial, _Prep, _What, GoalCond), Effects)),
 add_ordering(before(start, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(start, exists(GoalCond), GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 dbug(planner, ' START SATISFIES exists(~w)~n', [GoalCond]).
choose_operator([goal(GoalID, GoalCond)|Goals0], Goals2,
     Operators,
     plan(OldSteps, Order0, Bindings, OldLinks),
     plan(NewSteps, Order9, Bindings, NewLinks),
     Depth0, Depth ) :-
 % Condition achieved by new step?
 Depth0 > 0,
 Depth is Depth0 - 1,
 %operators_as_steps(Operators, FreshSteps),
 copy_term(Operators, FreshOperators),
 % Find a new operator.
 %member(step(StepID, oper(_Self, Action, Preconds, Effects)), FreshSteps),
 member(oper(Self, Action, Preconds, Effects), FreshOperators),
 precondition_matches_effects(GoalCond, Effects),
 operator_as_step(oper(Self, Action, Preconds, Effects),
     step(StepID, oper(Self, Action, Preconds, Effects)) ),
 % Add ordering constraints.
 add_orderings([before(start, StepID),
     before(StepID, GoalID),
     before(StepID, completeFn(_End))],
    Order0, Order1),
 % Need to protect existing links from new step.
 protect_links(OldLinks, StepID, Effects, Order1, Order2),
 % Need to protect new link from all existing steps
 protect_link_all(causes(StepID, GoalCond, GoalID), OldSteps, Order2, Order9),
 % Add the step.
 append(OldSteps, [step(StepID, oper(Self, Action, Preconds, Effects))], NewSteps),
 % Add causal constraint.
 union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
 % Add consequent goals.
 conds_as_goals(StepID, Preconds, NewGoals),
 append(Goals0, NewGoals, Goals2),
 bindings_valid(Bindings),
 dbug(planner, ' ~w CREATED ~w to satisfy ~w~n', [Depth, StepID, GoalCond]),
 pprint(oper(Self, Action, Preconds, Effects), always),
 once(pick_ordering(Order9, List)),
 dbug(planner, ' Orderings are ~w~n', [List]).

choose_operator([goal(_GoalID, GoalCond)|G0], G2, Op, P0, P2, D, D) :- GoalCond = (_\=_), !,
  choose_operator(G0, G2, Op, P0, P2, D, D).


choose_operator([goal(GoalID, GoalCond)|G0], _G2, _Op, _P0, _P2, D, D) :-
 dbug(planner, ' CHOOSE_OPERATOR FAILED on goal:~n goal(~w, ~w):~w~n', 
   [GoalID, GoalCond, G0]),
 !, fail.
choose_operator(G0, _G2, _Op, _P0, _P2, D, D) :-
 dbug(planner, ' !!! CHOOSE_OPERATOR FAILED: G0 = ~w~n', [G0]), !, fail.


planning_loop([], _Operators, plan(S, O, B, L), plan(S, O, B, L), _Depth, _TO ) :-
 dbug(planner, 'FOUND SOLUTION?~n'),
 bindings_safe(B).
planning_loop(Goals0, Operators, Plan0, Plan2, Depth0, Timeout) :-
 %Limit > 0,
 get_time(Now),
 (Now > Timeout -> throw(timeout(planner)); true),
 dbug(planner, 'GOALS ARE: ~w~n', [Goals0]),
 % must_maplist(dbug(planner, 'AVAILABLE OPERATOR: ~w~n'), Operators),
 choose_operator(Goals0, Goals1, Operators, Plan0, Plan1, Depth0, Depth),
 %Limit2 is Limit - 1,
 planning_loop(Goals1, Operators, Plan1, Plan2, Depth, Timeout).
%planning_loop(_Goals0, _Operators, Plan0, Plan0, _Limit) :-
% Limit < 1,
% dbug(planner, 'Search limit reached!~n'),
% fail.

serialize_plan(_Knower, _Agent, plan([], _Orderings, _B, _L), []) :- !.

serialize_plan(Knower, Agent, plan(Steps, Orderings, B, L), Tail) :-
 select_from(step(_, oper(Agent,  act3('wait',_,[]), _, _)), Steps, RemainingSteps),
 !,
 serialize_plan(Knower, Agent, plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(Knower, Agent, plan(Steps, Orderings, B, L), [Action|Tail]) :-
 select_from(step(StepI, oper(Agent, Action, _, _)), Steps, RemainingSteps),
 \+ (member(step(StepJ, _Oper), RemainingSteps),
  isbefore(StepJ, StepI, Orderings)),
 serialize_plan(Knower, Agent, plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(Knower, Agent, plan(_Steps, Orderings, _B, _L)) :-
 dbug(planner, 'serialize_plan FAILED: Knower=~p, Agent=~p !~n', [Knower, Agent]),
 pick_ordering(Orderings, List),
 dbug(planner, ' Orderings are ~w~n', [List]),
 fail.

select_unsatisfied_conditions([], [], _Model) :- !.
select_unsatisfied_conditions([Cond|Tail], Unsatisfied, ModelData) :-
 precondition_matches_effects(Cond, ModelData),
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).
select_unsatisfied_conditions([~ Cond|Tail], Unsatisfied, ModelData) :-
 \+ precondition_matches_effects(Cond, ModelData),
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).
select_unsatisfied_conditions([Cond|Tail], [Cond|Unsatisfied], ModelData) :-
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).


depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
     Depth, Timeout) :-
 dbug(planner, 'PLANNING DEPTH is ~w~n', [Depth]),
 planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan, Depth, Timeout),
 !.
depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
     Depth0, Timeout) :-
 Depth0 =< 7,
 Depth is Depth0 + 1,
 depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
      Depth, Timeout).

generate_plan(Knower, Agent, FullPlan, Mem0) :-
 initial_operators(Knower, Operators),
 dbug(planner, 'OPERATORS are:~n'), pprint(Operators),

 agent_thought_model(Agent, ModelData, Mem0),

 %dbug(planner, 'CURRENT STATE is ~w~n', [Model0]),
 thought_check(Agent, current_goals(Agent, Goals), Mem0),
 new_plan(Agent, ModelData, Goals, SeedPlan),
 dbug(planner, 'SEED PLAN is:~n'), pprint(SeedPlan),
 !,
 %planning_loop(Operators, SeedPlan, FullPlan),
 conds_as_goals(completeFn(_End), Goals, PlannerGoals),
 get_time(Now),
 Timeout is Now + 60, % seconds
 catch(
 depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
      1, Timeout),
 timeout(planner),
 (dbug(planner, 'PLANNER TIMEOUT~n'), fail)
 ),
 dbug(planner, 'FULL PLAN is:~n'), pprint(FullPlan).

% ----


path2dir1(Doer, Here, There,  act3('go__dir',Doer,[ _Walk, Dir]), ModelData):-
 in_model(h(spatial, fn(exit, Dir), Here, There), ModelData).
path2dir1(Doer, Here, There,  act3('go__obj',Doer,[ _Walk, There]), ModelData) :-
 in_model(h(spatial, descended, Here, There), ModelData).

path2directions(Doer, [Here, There], [GOTO], ModelData):-
  path2dir1(Doer, Here, There, GOTO, ModelData).
path2directions(Doer, [Here, Next|Trail], [GOTO|Tail], ModelData) :-
 path2dir1(Doer, Here, Next, GOTO, ModelData),
 path2directions(Doer, [Next|Trail], Tail, ModelData).


find_path1([First|_Rest], Dest, First, _ModelData) :-
 First = [Dest|_].
find_path1([[Last|Trail]|Others], Dest, Route, ModelData) :-
 findall([Z, Last|Trail],
   (in_model(h(spatial, _Prep, Last, Z), ModelData), \+ member(Z, Trail)),
    List),
 append(Others, List, NewRoutes),
 find_path1(NewRoutes, Dest, Route, ModelData).

find_path(Doer, Start, Dest, Route, ModelData) :-
 find_path1([[Start]], Dest, R, ModelData),
 reverse(R, RR),
 path2directions(Doer, RR, Route, ModelData).




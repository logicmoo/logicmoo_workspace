:- module(planner, [
              plan/4
          ]).
/** <module> A planner.
 *
 * Released under the terms of the SWI-Prolog license
 *
 * The planner depends on action modules, called 'genres'
 *
 *
 */

% ! plan(+InitConditions:list, +Genre:atom, -Plan:list, +Options:list)
% !         is nondet
%
%  Given a set of initial conditions and a Genre, generate random plans
%  on backtracking
%
%  @arg InitConditions is a list of conditions at the start. Goals
%  should be listed as goal(X), and when all X appear in the conditions,
%  planning terminates
%  @arg Genre is an atomic genre, allowing actions to be defined for
%  different genres.
%  @arg Plan - a list of action names
%  @arg Options - the only available options are order(random)
%  (default), which randomizes at each step, and order(first), which
%  takes actions in lexical order
%
plan(InitConditions, Genre, Plan, Options) :-
    (   member(order(Order), Options)
    ;   Order = random
    ),
    empty_queue,
    list_to_ord_set(InitConditions, OrdCond),
    plan_(Genre,
          Order,
          /*Open*/ [OrdCond],
          /* Closed*/[OrdCond],
          RPlan),
    reverse(RPlan, Plan).

% Genre is the module of the actions
% Order is the argument of the order/1 option
% Open is a list of nodes to explore, as State-Story pairs
% Closed is a list of nodes to never again put on Open
% RPlan is the plan, with the last action first
%
% if the Open list is exhausted,
plan_(_, _, [], _, _) :-
    !,
    fail.
%
% if we've reached a goal state we have a solution.
plan_(_Genre,
      _Order,
      [State-Story |_],
      _Closed,
      Story) :-
    at_goal(State).
plan_(Genre,
      Order,
      [State-Story | Open],
      Closed,
      FullStory) :-
    ordered_possible_action_states(State-Story, Genre, Order, Possible),
    add_unique_children_to_open(State-Story, Genre, Possible, Open, Open1),
    plan_(Genre, Order, Open1, [State | Closed], FullStory).



plan_(Cond, _Genre, Prev, _, _, _) :-
    member(Cond, Prev),   % we've been here before
    !,
    fail.
plan_(Cond, Genre, _Prev, _SoFar, _Plan, _Order) :-
    possible_actions(Cond, Genre, []),  % no move from here
    !,
    fail.
plan_(Cond, _, _, _, _, _) :-
    member(dead, Cond),   % dead end
    !,
    fail.
plan_(Cond, Genre, Prev, SoFar, Plan, Order) :-
    add_queue(SoFar, Cond),
    pop_queue(FirstSoFar, FirstCond),
    possible_actions(FirstCond, Genre, RawPossible),
    (   Order = random
    ->  once(random_permutation(RawPossible, Possible))
    ;   RawPossible = Possible
    ),
    member(Action, Possible),
    apply_action(Action, FirstCond, Genre, NewCond),
    debug(planner(action), '~w', [Action]),
    debug(planner(step), '~w ~w ~w', [FirstCond, Action, NewCond]),
    plan_(NewCond, Genre, [Cond | Prev], [Action | FirstSoFar], Plan, Order).

at_goal(Cond) :-
    check_goals(Cond, Cond),
    !. % green

check_goals(_, []).
check_goals([goal(Goal)|T], Cond) :-
    memberchk(Goal, Cond),
    check_goals(T, Cond).
check_goals([H|T], Cond) :-
    H \= goal(_),
    check_goals(T, Cond).

possible_actions(Cond, Genre, Possible) :-
    findall(Name, possible_action(Cond, Genre, Name), Possible).

possible_action(Cond, Genre, Name) :-
    Genre:action(Name, Act),
    action{  pre:Pre,
           negpre: NegPre
        } :< Act,
    maplist(is_in(Cond), Pre),
    maplist(not_in(Cond), NegPre).

is_in(List, Member) :-
    memberchk(Member, List).

not_in(List, Member) :-
    \+ memberchk(Member, List).

:- dynamic q/2.   % q(PlanSoFar, CurrentCondition)

empty_queue :-
    retractall(q(_,_)).

add_queue(PartialPlan, Cond) :-
    assertz(q(PartialPlan, Cond)).

pop_queue(PartialPlan, Cond) :-
    once(q(PartialPlan, Cond)),
    retractall(q(PartialPlan, Cond)).

apply_action(Name, Cond, Genre, NewCond) :-
    Genre:action(Name, Action),
    action{
        add: Add,
        remove: Remove
    } :< Action,
    my_subtract(Cond, Remove, C1),
    list_to_ord_set(Add, OrdAdd),
    ord_union(C1, OrdAdd, NewCond).


my_subtract([], _, []).
my_subtract([H|T], Remove, TOut) :-
    memberchk(H, Remove),
    !,
    my_subtract(T, Remove, TOut).
my_subtract([H|T], Remove, [H|TOut]) :-
    my_subtract(T, Remove, TOut).


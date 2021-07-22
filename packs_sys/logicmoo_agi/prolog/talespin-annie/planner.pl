/*  Part of SWI-Prolog

    Author:        Anne Ogborn
    E-mail:        anne@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, Anne Ogborn
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
:- module(planner, [
              plan/4,
              apply_action_dict/3
          ]).
/** <module> A STRIPS type planner.
 *
 * This planner depends on action modules, called 'genres', as sets
 * of actions. See
 * the 'fairy.pl' example for format.
 *
 *
 */
:- license(bsd).

% ! plan(+InitConditions:list, +Genre:atom, -Plan:list, +Options:list)
% !         is nondet
%
%  Given a set of initial conditions and a Genre, generate different
%  plans on backtracking
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
    list_to_ord_set(InitConditions, OrdCond),
    plan_(Genre,
          Order,
          /*Open*/ [OrdCond-[]],
          /* Closed*/[OrdCond],
          RPlan),
    reverse(RPlan, Plan).

% Genre is the *|module name|* of the actions
% Order is the argument of the order/1 option
% Open is a list of nodes to explore, as State-Story pairs
% Closed is a list of states to never again put on Open
% RPlan is the plan, with the last action first,
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
    debug(planner(current), '~q ~q', [State, Story]),
    at_goal(State).
% if we have a shaggy dog story, discard from Open
plan_(Genre,
      Order,
      [_State-Story | Open],
      Closed,
      FullStory) :-
    length(Story, Len),
    (   max_plan_len(Genre, MaxLen)
    *-> true
    ;   MaxLen = 18
    ),
    Len > MaxLen,
    plan_(Genre, Order, Open, Closed, FullStory).
% process head of Open
plan_(Genre,
      Order,
      [State-Story | Open],
      Closed,
      FullStory) :-
    ordered_possible_action_states(State, Genre, Order, PossibleActions),
    actions_children(State, Genre, PossibleActions, Children),
    add_unclosed_children_to_open(Story,
                                Closed,
                                PossibleActions,
                                Children,   % list of states
                                Open,
                                Open1),
    add_unique(
                                  Closed,
                                  Children,
                                  Closed1),
    plan_(Genre, Order, Open1, [State | Closed1], FullStory).


%!  ordered_possible_action_states(+State:state, +Genre:atom,
%!          +Order:atom, -Possible:list) is det
%
%   Give a state list and Genre, return the possible actions
%   as a list of action names, sorted in order found if Order=first
%   and in random order if Order=random
%
ordered_possible_action_states(State,
                               Genre,
                               Order,
                               Possible) :-
    possible_actions(State, Genre, RawPossible),
    (   Order = random
    ->  random_permutation(RawPossible, Possible)
    ;   RawPossible = Possible
    ).


%!  add_unclosed_children_to_open(+Story:listpair, +Closed:list,
%!            +Actions:list, +Children:list, +OpenIn:list,
%!            -OpenOut:list) is det
%
%  True when OpenOut is the new Open list resulting from adding
%  the State-Story pairs in Children to the open list,
%  discarding those already in the closed list.
%
%  @arg Story    the current story so far
%  @arg Closed   list of States we should ignore
%  @arg Actions  list of actions that got the children in Children
%  @arg Children list of States resulting from the current state via
%                the action in the corresponding position of Actions
%  @arg OpenIn list of State-Story pairs not yet processed, to be added
%                to
%  @arg OpenOut the new Open, after adding the Children with the stories
%
%  we have no more children to add
add_unclosed_children_to_open(  _Story,
                                _Closed,
                                _, % action
                                [],    % children
                                Open,
                                Open) :-
    !. % green
%  in the closed set
add_unclosed_children_to_open(  Story,
                                Closed,
                                [_Action | Actions],
                                [Child | Children],
                                Open,
                                OpenOut) :-
    memberchk(Child, Closed),   % its in the closed set
    !, % green
    add_unclosed_children_to_open(Story,
                                  Closed,
                                  Actions,
                                  Children,
                                  Open,
                                  OpenOut).
% already in the open set
add_unclosed_children_to_open(  Story,
                                Closed,
                                [Action | Actions],
                                [Child | Children],
                                Open,
                                OpenOut) :-
    memberchk(Child-_, Open),   % it's already in the open set
    debug(planner(dup_open), '~q', [Child-[Action | Story]]),
    !,  % green
    add_unclosed_children_to_open(Story,
                                    Closed,
                                    Actions,
                                    Children,
                                    Open,
                                    OpenOut).
% main case
add_unclosed_children_to_open(  Story,
                                Closed,
                                [Action | Actions],
                                [Child | Children],
                                Open,
                                OpenOut) :-
    append(Open, [Child-[Action | Story]], OpenX),
    !, % green
    add_unclosed_children_to_open(Story,
                                    Closed,
                                    Actions,
                                    Children,
                                    OpenX,
                                    OpenOut).

% ! add_unique(+In:list, +New:list,
%!            -Out:list) is det
%
%   prepend those elements of New which are not in
%   the In set. Elements of New are added in reverse order,
%   and second occurances of elements of New are ignored.
%
add_unique(In, [], In) :-
    !. % green
add_unique(In, [NewH| New], Out) :-
    memberchk(NewH, In),
    !, % green
    add_unique(In, New, Out).
add_unique(In, [NewH| New], [NewH | Out]) :-
    add_unique(In, New, Out).

%!  at_goal(+State:list) is det
%
%   succeeds if we're at the final goal
%
at_goal(Cond) :-
    check_goals(Cond, Cond),
    !. % green

check_goals([], _).
check_goals([goal(Goal)|T], Cond) :-
    memberchk(Goal, Cond),
    check_goals(T, Cond).
check_goals([H|T], Cond) :-
    H \= goal(_),
    check_goals(T, Cond).

%!  possible_actions(+Cond:list, +Genre:atom, -Possible) is det
%
%   succeeds when Possible is the list of actions that can be performed
%   from Cond using Genre as the action list.
%
possible_actions(Cond, Genre, Possible) :-
    findall(Name, possible_action(Cond, Genre, Name), Possible).

possible_action(Cond, Genre, Name) :-
    Genre:action(Name, Act),
    action{  pre:Pre,
           negpre: NegPre
        } :< Act,
    maplist(is_in(Cond), Pre),
    maplist(not_in(Cond), NegPre).

% famulus to swap memberchk's args
is_in(List, Member) :-
    memberchk(Member, List).

not_in(List, Member) :-
    \+ memberchk(Member, List).

%!  actions_children(+State:list,
%!                    +Genre:list,
%!                    +PossibleActions:list,
%!                    -Children:list) is det
%
%   succeeds when Children is a list of new states resulting
%   from applying the members of PossibleActions to State
%
%   @arg State the current state
%   @arg Genre the module name of the actions
%   @arg PossibleActions a list of atom action names. Assumed possible
%   @arg Children The subsequent states
%
actions_children(State, Genre, PossibleActions, Children) :-
    maplist(apply_action(State, Genre), PossibleActions, Children).

apply_action(State, Genre, Name, NewState) :-
    Genre:action(Name, Action),
    apply_action_dict(State, Action, NewState).

apply_action_dict(State, Action, NewState) :-
    action{
        add: Add,
        remove: Remove
    } :< Action,
    my_subtract(State, Remove, S1),
    list_to_ord_set(Add, OrdAdd),
    ord_union(S1, OrdAdd, NewState).

%!     my_subtract(+A:list, +B:list, -C:list) is det
%
% ord_subtract uses the standard term order, which isn't happy
% when we have a not fully ground term. To avoid it, we build our
% own subtract
%
%  Succeeds when C = A - B as set operation, where A must be ground
%  and B possibly contains partially ground elements which remove
%  all elements of A they unify with
%
my_subtract([], _, []).
my_subtract([H|T], Remove, TOut) :-
    memberchk(H, Remove),
    !,
    my_subtract(T, Remove, TOut).
my_subtract([H|T], Remove, [H|TOut]) :-
    my_subtract(T, Remove, TOut).


:- multifile planner:max_plan_len/2.

max_plan_len(nothing, 18).

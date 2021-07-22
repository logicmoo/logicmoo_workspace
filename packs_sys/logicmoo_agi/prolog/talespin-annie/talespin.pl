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
:- module(talespin, [
              story/4
          ]).
/** <Module> implements the Tale-Spin story generation algorithm
 *
 * 1. Plan
 * 2. simulate plan moving forward. Randomly pick 'events', then
 *    have outcome be event's outcome, not action's outcome.
 * 3. following an event, replan
 */
:- use_module(planner).

%!  story(+Init:list, +Genre:atom, +Options:List, -Story:list) is det
%
%   @arg Init  list of initial conditions
%   @arg Genre module name of the story genre
%   @arg Options Options list for plan/4
%   @arg Story bound to a story as a list of semantic occurances
%
story(Init, Genre, Options, Story) :-
    repeat, % because of randomization story_ sometimes fails.
    story_(Init, Genre, Options, Story),
    !.
story_(Init, Genre, Options, Story) :-
    plan(Init, Genre, Plan, Options),
    simulate(Init, Genre, Options, Plan, XStory),
    flatten(XStory, Story).

simulate(_, _, _, [], []).
simulate(State, Genre, Options, [Action|Remains], [Desc | RemDesc]) :-
    (
        event_happens(State, Action, Genre, Event)
    ->
        apply_action_dict(State, Event, NewState),
        plan(NewState, Genre, NewPlan, Options),
        Desc = Event.desc,
        simulate(NewState, Genre, Options, NewPlan, RemDesc)
    ;
        Genre:action(Action, Dict),
        apply_action_dict(State, Dict, NewState),
        Desc = Dict.desc,
        simulate(NewState, Genre, Options, Remains, RemDesc)
    ).

event_happens(State, Action, Genre, EventDict) :-
    (   setof(Pair, possible_event(State, Genre, Action, Pair), Possible)
    ->  true
    ;   Possible = []
    ),
    random_permutation(Possible, RandPossible),
    maybe_pick_one(RandPossible, EventDict).

maybe_pick_one([], _) :- !, fail.
maybe_pick_one([P-H | _], H) :-
    P > random_float,
    !.
maybe_pick_one([_ | T], Out) :-
    maybe_pick_one(T, Out).

possible_event(State, Genre, ActionName, Prob-Dict) :-
    Genre:event(ActionName, Prob, Dict),
    maplist(is_in(State), Dict.pre),
    maplist(not_in(State), Dict.negpre).

% famulus to swap memberchk's args
is_in(List, Member) :-
    memberchk(Member, List).

not_in(List, Member) :-
    \+ memberchk(Member, List).


/*
 *
 * Copyright 2018, Anne Ogborn

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
:- module(snail,
          /*[action/2,
                 event/3,
                 init_conditions/1,
                 friend_desc/3,
                 place_desc/3]). */
    []).
/** <module> snail stories
 *
 * These are parables, with different messages
 * TBD - haven't made all these
 *
 * 1) having fun with your friends is more important than accomplishing
 * an unimportant goal
 *
 * 2) when you hurt someone you have to say you're sorry
 *
 * 3) helping your friends
 *
 * 4) don't laugh at people because they're different
 *      someone laughs at someone's difference, and they run away.
 *      they are told it's wrong, and they hunt down the victim and
 *      apologize
 *
 * 5) being sad is part of life.
 *      someone is sad, and they meet a friend and the friend is nice to
 *      them and they feel better.
 *
 * 6) it's OK to ask for help
 *      Snail finds a rock under their tomato
 *      plant while they're away. Only beetle is able to move the rock.
 *
 *
 */
:- discontiguous action/2, event/3, add_event/2.

:- multifile planner:max_plan_len/2.

planner:max_plan_len(snail, 12).

init_conditions([not_inited, goal(has_goals)]).

		 /*******************************
		 * Initialization
		 *******************************/

action(initialize,
       action{
           pre: [not_inited],
           negpre: [stuck],
           add: [has_goals, theme(Message),
                 loc(tomato),
                 loc(tomato, ball),
                 goal(attended_tea_party),
                     goal(loc(tomato))],
           remove: [not_inited],
           desc: [theme(Message), start_message(['One day ', $0, ' was invited to a tea party put on by froggy amidst the mushrooms.'])]
       }) :-
    theme(Message).

		 /*******************************
		 * Movement
		 *******************************/

action(move(A, B),
       action{
           pre: [loc(A)],
           negpre: [stuck, blocked(B)], % blocked(B) means we know B is blocke
           add: [loc(B)],
           remove: [loc(A)],
           desc: [moveall(A,B)]
       }) :-
    uroute(A,B).

% a random friend joins the party
event(move(A, B),
      0.02,
      action{
          pre:[],
          negpre:[party(C)], % event cant happen if C is in party already
          add: [party(C), loc(B)],
          remove: [loc(A)],
          desc: [join_party(C), moveall(A,B)]
      }) :-
    can_join_party(C).


		 /*******************************
		 * The Rock                     *
		 *******************************/

% a stone blocks the way
% can only happen once
event(move(_,B),
      0.2,
      action{
          pre:[],
          negpre: [blocked(B), has_been_blocked(stone)],
          add: [blocked(B), blocked_by(stone), has_been_blocked(stone)],
          remove: [],
          desc: [blocked_by(B, stone)]
      }).

action(remove_by_friend(stone),
       action{
           pre: [loc(A), blocked(B), blocked_by(stone), party(Friend)],
           negpre: [],
           add: [helped(Friend)],
           remove: [blocked(B), blocked_by(stone), had_adventure],
           desc: [unblocked(Friend, stone)]
       }) :-
    uroute(A,B),
    strong(Friend).
action(remove_by_gardner(stone),
       action{
           pre: [loc(A), blocked(B), blocked_by(stone), gardener],
           negpre: [],
           add: [gardener_threw_stone],
           remove: [blocked(B), blocked_by(stone)],
           desc: [gardener_threw_stone]
       }) :-
    uroute(A,B).

		 /*******************************
		 *   Pomatia's ball             *
		 *******************************/

event(move(tomato,NewLoc),
      0.5,
              action{
                  pre: [loc(tomato), loc(tomato, ball)],
                  negpre: [carried(ball)],
                  add: [loc(NewLoc), carried(ball), goal(has_played)],
                  remove: [loc(tomato), loc(_, ball)],
                  desc: [take_ball, moveall(tomato, NewLoc)]
              }) :-
    uroute(tomato, NewLoc).

action(play_ball,
              action{
                  pre: [carried(ball)],
                  negpre: [gardener, has_played],
                  add: [has_played],
                  remove: [],
                  desc: [play_ball]
              }).

% lose the ball in the water
% % crummy event because we're unlikely to play with ball near water
event(play_ball,
      1.0,
      action{
          pre:[loc(pond), carried(ball)],
          negpre: [has_lost_ball],
          add: [has_lost_ball, ball_in_water, goal(carried(ball))],
          remove: [carried(ball)],
          desc: [lost_ball_in_water]
      }).


action(recover_ball,
       action{
           pre: [loc(pond), ball_in_water],
           negpre: [],
           add: [carried(ball)],
           remove: [ball_in_water],
           desc: [recover_ball_from_water]
       }).

		 /*******************************
		 * Universal Events             *
		 *******************************/


% we have events that apply any time and don't interfere with the
% original action.
event(OrigAction,
      Prob,
      action{
          pre: EPre,
          negpre: ENegPre,
          add: TotalAdd,
          remove: TotalRemove,
          desc: TotalDesc
      }) :-
    add_event(Prob,
              action{
                  pre: EPre,
                  negpre: ENegPre,
                  add: EAdd,
                  remove: ERemove,
                  desc: EDesc
              }),
    action(OrigAction, ActionDict),
    append(ActionDict.add, EAdd, TotalAdd),
    append(ActionDict.remove, ERemove, TotalRemove),
    append(ActionDict.desc, EDesc, TotalDesc).

		 /*******************************
		 * The Gardener
		 *******************************/

% the gardener appears.
add_event(
      0.1,
      action{
          pre: [],
          negpre: [gardener],
          add: [gardener, had_adventure, gardener_has_appeared],
          remove: [],
          desc: [gardener_appears]
      }).

% the gardener leaves
add_event(
      0.1,
      action{
          pre: [gardener],
          negpre: [],
          add: [],
          remove: [gardener],
          desc: [gardener_leaves]
      }).

		 /*******************************
		 * The tea party
		 *******************************/

action(tea_party,
       action{
           pre: [loc(mushrooms)],
           negpre: [],
           add: [attended_tea_party],
           remove: [],
           desc: [attended_tea_party]
       }).

event(tea_party,
      0.4,
      action{
          pre:[loc(mushrooms), gardener_has_appeared],
          negpre: [mushrooms_gone, saved_froggy_home],
          add: [mushrooms_gone, goal(saved_froggy_home)],
          remove: [goal(attended_tea_party)],
          desc: [mushrooms_gone]
      }).

		 /*******************************
		 * Froggy's home                *
		 *******************************/

action(magic_fix,
       action{
           pre: [mushrooms_gone, goal(saved_froggy_home)],
           negpre: [],
           add: [saved_froggy_home],
           remove: [goal(saved_froggy_home), mushrooms_gone],
           desc: [magic_mushrooms_fix]
       }).


theme(help). % Friends help each other
% theme(friends_before_things).
% theme(friends_before_activities).
% theme(apologizing).

		 /*******************************
		 *  Routes around the garden    *
		 *  Places Pomatia and friends  *
		 *  go in their garden world    *
		 *******************************/

route(tomato, sidewalk).
route(tomato, weed).
route(sidewalk, weed).
route(sidewalk, pond).
route(pond, ants).
route(mushrooms, pond).

place_desc(tomato, 'Pomatia\'s cozy tomato plant',
    'Pomatia\'s home, a sturdy tomato plant in a shady part of the garden').
place_desc(sidewalk, 'the garden path',
           'the brick path that wound through the garden').
place_desc(weed, 'the large weed',
      'a large weed that Mrs. Cratchet never seemed to pull').
place_desc(pond, 'the ornamental pond',
     'a nice ornamental pond, with a little fountain, and lily pads that Froggy loved').
place_desc(ants, 'the anthill',
           'a large anthill in a corner of the garden, home to the busy ants').
place_desc(mushrooms, 'the mushrooms',
    'a large group of mushrooms under a willow tree, where Mr. Froggy lived').

uroute(A,B) :- route(A,B).
uroute(A,B) :- route(B,A).

		 /*******************************
		 *        Friends               *
		 *
		 *  Pomatia's friends. Pomatia  *
		 *  froggy, and Mrs. Cratchet   *
		 *  handled special             *
		 *******************************/


% only friends who can join the party
friend(beetle).
friend(grasshopper).
friend(worm).
friend(ladybug).
can_join_party(X) :- member(X, [beetle, grasshopper, worm, ladybug]).

friend_desc(beetle, 'Beetle',
            'Beetle, a large and iridescent blue-green beetle').
friend_desc(grasshopper, 'Hopper',
            'Hopper, a rather nervous grasshopper').
friend_desc(worm, 'Wormy',
            'Wormy, a nice chubby, jolly earthworm').
friend_desc(ladybug, 'Miss Priscilla',
            'Miss Priscilla, a very dainty (and quite fashionable) ladybug').
friend_desc(cratchet, 'Mrs. Cratchet came back out to the garden.',
             'Suddenly old Mrs. Cratchet, who owned the garden, appeared! All the creatures were afraid of Ms. Cratchet, who threw stones at them.').

strong(beetle).
strong(worm).

    % use botanicula for friend can help ideas
    % spiders make ropes, beetles move heavy things
    % grasshoppers jump
    % ladybug has no 'special powers' but is good at emotional work


% make the child a rock saver - 'but I'm little', 'to us you are big'
% it could rain
% don't forget emotional action is action
% has a tummy ache that turns out to be an egg(??)
% Pomatia loses their ball in the lily pond.

% Pomatia could find lose their ball in the pond

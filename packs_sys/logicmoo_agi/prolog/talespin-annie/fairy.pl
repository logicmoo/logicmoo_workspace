/*
 *
 * Copyright 2018, Anne Ogborn

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
:- module(fairy, []).

:- multifile planner:max_plan_len/2.

planner:max_plan_len(fairy, 12).

action(bake_pie,
       action{
           pre: [loc(home), has_flour, has_berries],
           negpre: [],
           add: [has_pie],
           remove: [has_flour, has_berries],
           desc: [$0, used, the, flour, and, berries,
                  to, bake, a, delicious, pie]
       }).
action(walk_to_woods,
       action{
           pre: [loc(home)],
           negpre: [wolf],
           add: [loc(woods)],
           remove: [loc(_)],
           desc: [$0, walked, deep, into, the, woods]
       }).
action(walk_to_grannys,
       action{
           pre: [loc(woods)],
           negpre: [wolf],
           add: [loc(grannys)],
           remove: [loc(_)],
           desc: [$0, walked, to, grannys, house]
       }).
action(greet_by_granny,
       action{
           pre: [loc(grannys)],
           negpre: [],
           add: [saw_granny],
           remove: [],
           desc: [$0, saw, her, granny]
       }).
action(leave_grannys,
       action{
           pre: [loc(grannys)],
           negpre: [],
           add: [loc(woods)],
           remove: [loc(_)],
           desc: [$0, walked, into, the, woods, headed, home]
       }).
action(arrive_home,
       action{
           pre: [loc(woods)],
           negpre: [wolf],
           add: [loc(home)],
           remove: [loc(_)],
           desc: [$0, arrived, home]
       }).


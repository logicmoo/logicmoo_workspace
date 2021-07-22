/*
 *
 * Copyright 2018, Anne Ogborn

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
:- module(anglify, [
              anglify/2
          ]).
/** <module> Anglify the snail stories
 *
 */
anglify(Semantics, English) :-
    phrase(as_english([genre(snail)], EnglishList, _), Semantics),
    list_english(EnglishList, English),
    !. % lame, but ez

as_english(Ctx, [], Ctx) --> [].
as_english(Ctx, [English | Rest], CtxOut) -->
    happening(Ctx, English, Ctx1),
    as_english(Ctx1, Rest, CtxOut).

happening(Ctx, E, Ctx1) -->
    [moveall(_, B), moveall(B, C)],
    format_desc(Ctx, 'They traveled from ~w and on to ~w.',
               [friend(B), friend(C)],
               Ctx1, E).  % remember E is a string
happening(Ctx, E, Ctx1) -->
    [moveall(_, B)],
    format_desc(Ctx, 'They crawled along to ~w.', [place(B)], Ctx1, E).
happening(Ctx, E, Ctx) -->
    [E],
    {string(E)}.
happening(Ctx, E, Ctx1) -->
    [blocked_by(Loc, stone)],
    format_desc(Ctx,
                'They found the path to ~w blocked by a big round rock!',
                [place(Loc)],
               Ctx1,
               E).
happening(Ctx, E, Ctx1) -->
    [unblocked(Friend, stone)],
    format_desc(Ctx,
      '~w pushed and tugged, and pushed and tugged, and rolled the rock away.',
                [friend(Friend)],
               Ctx1,
               E).
happening(Ctx, E, Ctx1) -->
    [gardener_appears],
    format_desc(Ctx, '~w', [friend(cratchet)], Ctx1, E).
happening(Ctx, E, Ctx1) -->
    [join_party(Friend)],
    format_desc(Ctx,
           'Pomatia\'s friend ~w suddenly appeared, and agreed to go along.',
           [friend(Friend)],
           Ctx1,
           E).
happening(Ctx,
  "They all had a grand time at the tea party. They drank tea, and ate scones, or leaves, or whatever suited them. Finally it was time to leave.",
          Ctx) -->
    [attended_tea_party].
happening(Ctx,
   "When everyone arrived at the party, they discovered Froggy crying disconsolately in the middle of a bare patch of ground. \"Mrs. Cratchet\", he sobbed, \"came and pulled up all the mushrooms, including my beautiful Toadstool home!",
   Ctx) -->
    [mushrooms_gone].
happening(Ctx,
   "A caterpillar was sitting to one side of the patch, looking at a broken hookah. \"Oh dear\", it said, \"This won\'t do at all. It blew a beautiful blue cloud, and the mushrooms reappeared.",
   Ctx) -->
    [magic_mushrooms_fix].
happening(Ctx,
   "Mrs. Cratchet went back inside to watch her soap opera. The garden residents sighed with relief.", Ctx) -->
     [gardener_leaves].
happening(Ctx,
   "Mrs. Cratchet picked up the rock and threw it at them! The rock wasn\'t in the way any longer!"
         , Ctx) -->
     [gardener_threw_stone].
happening(Ctx,
   "Pomatia had a particular hard berry they liked to use as a ball. Pomatia decided to take the ball with them."
         , Ctx) -->
     [take_ball].
happening(Ctx,
   "The friends played with Pomatia\'s ball."
         , Ctx) -->
     [play_ball].
happening(Ctx, "", Ctx) -->
    [theme(_)].
happening(Ctx,
  "One day Pomatia was invited to tea at the mushrooms beyond the pond. Froggy lived there, and always gave such grand tea parties.", Ctx) -->
    [start_message(_)].
happening(Ctx, "Pomatia bopped their ball with their head. \"Oh No!\" The ball went Splash! in the pond!", Ctx) -->
    [lost_ball_in_water].
happening(Ctx, "The ball drifted to the edge of the pond. Pomatia raced (slowly) to catch it. \"Oh No! The ball will float away!\" But the ball stayed near the edge, and Pomatia was able to grab it.", Ctx) -->
    [recover_ball_from_water].

happening(Ctx, E, Ctx) -->
    [A],
    {atom_string(A, E)}.

format_desc(Ctx, Format, Args, CtxOut, String) --> [],
    {
       args_atoms(Ctx, Args, AtomArgs, CtxOut),
       format(string(String), Format, AtomArgs)
    }.

args_atoms(Ctx, [], [], Ctx).
args_atoms(Ctx, [place(A) | Tail], [Atom | TailOut], Ctx1) :-
    memberchk(genre(Genre), Ctx),
    Genre:place_desc(A, Short, Long),
    (  memberchk(A, Ctx)
    ->
       Atom = Short,
       args_atoms(Ctx, Tail, TailOut, Ctx1)
    ;
       Atom = Long,
       args_atoms([A | Ctx], Tail, TailOut, Ctx1)
    ).
args_atoms(Ctx, [friend(A) | Tail], [Atom | TailOut], Ctx1) :-
    memberchk(genre(Genre), Ctx),
    Genre:friend_desc(A, Short, Long),
    (  memberchk(A, Ctx)
    ->
       Atom = Short,
       args_atoms(Ctx, Tail, TailOut, Ctx1)
    ;
       Atom = Long,
       args_atoms([A | Ctx], Tail, TailOut, Ctx1)
    ).

list_english(List, English) :-
    atomics_to_string(List, '\n', English).

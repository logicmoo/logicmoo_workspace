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
:- module(adv_test, [test_adv/0]).

%:- ensure_loaded(adv_main).
adv_reset:-
 adventure_init,
 main_once,
 main_once,
 main_once, !.

test_adv :- test_adv(1).

test_adv(M-N) :- integer(N), !,
 forall(
 between(M, N, O),
 test_adv(O)).

test_adv(N) :-
 adv_reset,
 adv_tst(N),
 mainloop.


adv_tst(0):- adv_reset.

adv_tst(1):-
 nlu_assert("He is in the kitchen."),
 nlu_assert("There are boxes on the floor.").

adv_tst(2):-
 nlu_assert("He opens a box."),
 nlu_assert("There are books in the box.").

adv_tst(3):-
 nlu_assert("He takes out the books."),
 nlu_assert("He puts the books on the bookshelf."),
 nlu_assert("He opens another box."),
 nlu_assert("There are plates in the box."),
 nlu_assert("He takes out the plates."),
 nlu_assert("He puts them in the kitchen cabinet.").


adv_tst(4):-
 nlu_assert("He stands on the corner."),
 nlu_assert("He sells purses."),
 nlu_assert("He says, 'Ten Dollars' over and over."),
 nlu_assert("Many women stop."),
 nlu_assert("They look at all the purses."),
 nlu_assert("They pick up the purses."),
 nlu_assert("They look inside the purses."),
 nlu_assert("They put the purses on their shoulders."),
 nlu_assert("Many women buy a purse."),
 nlu_assert("Some women buy two purses."),

adv_tst(5):-
 nlu_assert("She is in her pajamas."),
 nlu_assert("She lies down."),
 nlu_assert("She puts her head on the pillow."),
 nlu_assert("She moves the pillow around a little bit."),
 nlu_assert("It is more comfortable now."),
 nlu_assert("The lamp is still on."),
 nlu_assert("She turns off the lamp."),
 nlu_assert("Her bedroom is dark."),
 nlu_assert("She puts her head back on the pillow."),
 nlu_assert("She goes to sleep."),

adv_tst(6):-
 nlu_assert("She opens the envelope."),
 nlu_assert("She reads the letter."),
 nlu_assert("The letter is from her cousin."),
 nlu_assert("Her cousin is in Europe."),
 nlu_assert("Her cousin likes Europe."),
 nlu_assert("She finishes the letter."),
 nlu_assert("She writes a letter to her cousin."),
 nlu_assert("She tells her cousin all the latest news at home."),
 nlu_assert("She thanks her cousin for the letter."), !.

adv_tst(7):-
 nlu_assert("She is a model."),
 nlu_assert("She walks up and down the runway."),
 nlu_assert("She models clothes."),
 nlu_assert("She models expensive clothes."),
 nlu_assert("Famous designers design her clothes."),
 nlu_assert("The clothes look good on her."),
 nlu_assert("People watch her model clothes."),
 nlu_assert("People spend lots of money on the clothes."),
 nlu_assert("People buy the clothes."),
 nlu_assert("She never buys the clothes."),
 nlu_assert("She just wears them.").

adv_tst(8):-
 nlu_assert("The baby cries."),
 nlu_assert("She looks at it."),
 nlu_assert("She talks to it."),
 nlu_assert("The baby cries some more."),
 nlu_assert("She picks it up."),
 nlu_assert("She holds it."),
 nlu_assert("She pats the baby on the back."),
 nlu_assert("The baby burps."),
 nlu_assert("She puts the baby down."),
 nlu_assert("She looks at it."),
 nlu_assert("The baby looks at her."),
 nlu_assert("It smiles at her.").

adv_tst(9):-
 nlu_assert("He has a secret."),
 nlu_assert("He keeps his secret."),
 nlu_assert("He shares it with nobody."),
 nlu_assert("He tells it to nobody."),
 nlu_assert("A secret is a secret."),
 nlu_assert("He loves his teacher."),
 nlu_assert("That is his secret."),
 nlu_assert("His teacher is beautiful."),
 nlu_assert("She is smart."),
 nlu_assert("She likes him too."),
 nlu_assert("She gives him A's on his homework."),
 nlu_assert("She gives him gold stars for his class work."), !.

adv_tst(10):-
 nlu_assert("It is winter."),
 nlu_assert("It is early morning."),
 nlu_assert("He goes to the park."),
 nlu_assert("He puts on his ice skates."),
 nlu_assert("He skates on the pond."),
 nlu_assert("He skates round and round."),
 nlu_assert("Nobody else is on the pond."),
 nlu_assert("He has it all to himself."),
 nlu_assert("It is quiet on the pond."),
 nlu_assert("The only sound is his skates on the ice."), !.

adv_tst(11):-
 nlu_assert("He has a pocketknife."),
 nlu_assert("It has two parts."),
 nlu_assert("It has a knife."),
 nlu_assert("The knife is two inches long."),
 nlu_assert("It is sharp."),
 nlu_assert("He cuts rope with the knife."),
 nlu_assert("He cuts wood with the knife."),
 nlu_assert("The other part is a file."),
 nlu_assert("The file is two inches long."),
 nlu_assert("He files his nails with the file."), !.


adv_tst(12):- 
  nlu_assert("He blows air into the flute."),
  % ....
  nlu_assert("A sound is heard by all in the room").

adv_tst(13):- 
  nlu_assert("He blows air into the flute."),
  % ....
  nlu_assert("No sound is heard by all in the room").

adv_tst(14):- 
  nlu_assert("He tries to walk north from here."),
  nlu_assert("A northward path is able to accomidate his body walking."),
  nlu_assert("The northward path leads to a differnt place."),
  nlu_assert("He is seen leaving northward from here."),
  nlu_assert("He arrives at a different place coming from the south").


:- initialization(test_adv, main).


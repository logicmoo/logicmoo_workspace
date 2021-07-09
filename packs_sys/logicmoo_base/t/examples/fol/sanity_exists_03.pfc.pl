#!/usr/bin/env swipl

:- module(t123,[]).

:- include(test_header).
:- module(t123).

:- dynamic(t123:ttExpressionType/1).

% :- process_this_script.

:- statistics.


subtest_assert(I):-kif_assert(I).

subtest([subtest_assert(tAnimal(joe)),
        mpred_test(isa(_,tHeart))]).

subtest([subtest_assert(tAnimal(joe)),
        mpred_test(hasOrgan(joe,_))]).

subtest([subtest_assert(tHeart(_)),
        mpred_test(~hasOrgan(jack,_))]).


dbanner:- nl,nl,dmsg('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,nl.

add_test(Name,Assert):- 
   % test_boxlog(Assert),
   assert(( Name:- dbanner,
      test_assert(Assert))).

test_assert(A):-
  kif_assert(A),
  nop(forall(subtest(T),do_subtest(T))).


do_subtest(List):- must_maplist(call,List).


:- add_test(t121, (all([[Human,tAnimal]],exists([[Heart,tHeart]],hasOrgan(Human,Heart))))).

:- add_test(t122, 
 (all(Human,exists(Heart,isa(Human,tAnimal) 
     => (isa(Heart,tHeart) 
      & hasOrgan(Human,Heart)))))).

:- add_test(t123,
  (all(Human,isa(Human,tAnimal) => exists(Heart, (isa(Heart,tHeart)  =>  hasOrgan(Human,Heart)))))).

:- cls.

:- begin_pfc.

tAnimal(iBob).

:- t121.
         

:-  listing(needs).
:-  listing('$pt').


:- '$set_source_module'(t123).

end_of_file.

:- mpred_test(\+ tHeart(_)).
:- mpred_test(\+ needs(_)).


:- break.
:- mpred_test(needs(_)).
:- mpred_test(tHeart(_)).


:- 
 ain(hasOrgan(iBob,iBobsHeart)).
:- 
 ain(tHeart(iBobsHeart)).





% You''ve proved Human does not exist when:
% 1) you dont need skolems and
%    1a) no hearts exists or
%    1b) Human has no organs
% 2) when you need skolems and 
%    2a) no skolem hearts exist or
%    2b) no skolem organs for Human 
prove_not_isa(Human, tAnimal) :-
        (   prove_not_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
            (   prove_not_isa(Heart, tHeart)
            ;   prove_not_holds_t(hasOrgan, Human, Heart)
            )
        ;   prove_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
            (   prove_not_isa(skIsHeartInArg2ofHasorgan_1FnSk(Human), tHeart)
            ;   prove_not_holds_t(hasOrgan,
                                  Human,
                                  skIsHeartInArg2ofHasorgan_1FnSk(Human))
            )
        ).

% Good:
% You need skolems for Human when no hearte exist for anyone
%  or that human has no organs
% plus confirm this is indeed a human
prove_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)) :-
        (   prove_not_isa(Heart, tHeart)
        ;   prove_not_holds_t(hasOrgan, Human, Heart)
        ),
        prove_isa(Human, tAnimal).

% This is broken:  Everything is a heart of you dont need skolems and you have and what you dont need a skolems for was human
prove_isa(Heart, tHeart) :-
        prove_not_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
        prove_isa(Human, tAnimal).

% This is broken:  Everything is an orgam of you dont need skolems and you have and what you dont need a skolems for was human
prove_holds_t(hasOrgan, Human, Heart) :-
        prove_not_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
        prove_isa(Human, tAnimal).

% This is broken:
prove_not_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)) :-
        (   prove_not_isa(skIsHeartInArg2ofHasorgan_1FnSk(Human), tHeart)
        ;   prove_not_holds_t(hasOrgan,
                              Human,
                              skIsHeartInArg2ofHasorgan_1FnSk(Human))
        ),
        prove_isa(Human, tAnimal).

% Good:
prove_isa(skIsHeartInArg2ofHasorgan_1FnSk(Human), tHeart) :-
        prove_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
        prove_isa(Human, tAnimal).
prove_holds_t(hasOrgan, Human, skIsHeartInArg2ofHasorgan_1FnSk(Human)) :-
        prove_need(skIsHeartInArg2ofHasorgan_1FnSk(Human)),
        prove_isa(Human, tAnimal).


end_of_file.

the other year.. i was creating a helpsystem for a commandline util for playing as a robot in secondlife.. 
so human controlled commands had crazy help system .. i had written this in C#
what i was going to say about why cyc ended up the way it did was jus tthe concxept that you know there can be  many cfg for english out there and temproary onces



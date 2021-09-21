#!/usr/bin/env lmoo-junit

:- module(t123,[]).

:- include(test_header).
:- module(t123).   
:- '$set_source_module'(t123).

% :- process_this_script.

:- statistics.



subtest([subtest_assert(tAnimal(joe)),
        mpred_test(isa(_,tHeart))]).

subtest([subtest_assert(tAnimal(joe)),
        mpred_test(hasOrgan(joe,_))]).

subtest([subtest_assert(tHeart(_)),
        mpred_test(~hasOrgan(jack,_))]).


:- add_test(t121, (all([[Human,tAnimal]],exists([[Heart,tHeart]],hasOrgan(Human,Heart))))).

:- add_test(t122, 
 (all(Human,
   exists(Heart,
    isa(Human,tAnimal) 
      => (isa(Heart,tHeart) & hasOrgan(Human,Heart)))))).

:- add_test(t123,
  (all(Human,isa(Human,tAnimal) => exists(Heart, (isa(Heart,tHeart)  =>  hasOrgan(Human,Heart)))))).

:- cls.

:- expects_dialect(pfc).

:- t122.

:- mpred_test(\+ tHeart(_)).
:- ain(tAnimal(iBob)).

:- mpred_test(tHeart(_)).
% :- mpred_why(tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(iBob))).
% '' :-
%       \+ tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(iBob)).
% '' :-
%       tAnimal(iBob).
% '' :-
%       tAnimal(_32725602), (\+tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32725602));\+hasOrgan(_32725602, skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32725602))), {_32725654=skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32725602)}, {is_unit(_32725654)}==>tHeart(_32725654).
% '' :-
%       mfl(t123,
%           '/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/sanity_exists_01.pl',
%           40).

:- mpred_test(hasOrgan(iBob,_)).

% /home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/sanity_exists_01.pl:45
% :- mpred_why(hasOrgan(iBob, skIsAnimalInHeartArg2ofHasorgan_1FnSk(iBob))).
% '' :-
%       \+ tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(iBob)).
% '' :-
%       tAnimal(iBob).
% '' :-
%       tAnimal(_32734660), (\+tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32734660));\+hasOrgan(_32734660, skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32734660))), {_32734712=skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32734660)}, {is_unit(_32734712, _32734660)}==>hasOrgan(_32734660, _32734712).
% '' :-
%       mfl(t123,
%           '/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/sanity_exists_01.pl',
%           40).
% '' :-
%       \+ tHeart(skIsAnimalInHeartArg2ofHasorgan_1FnSk(iBob)).
% '' :-
%       tAnimal(iBob).
% '' :-
%       tAnimal(_32734522), {_32734536=skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32734522)}, (\+tHeart(_32734536);\+hasOrgan(_32734522, _32734536)), {is_unit(_32734522)}==>hasOrgan(_32734522, skIsAnimalInHeartArg2ofHasorgan_1FnSk(_32734522)).
% '' :-
%       mfl(t123,
%           '/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/sanity_exists_01.pl',
%           40).
% init_why(after('/home/prologmud_server/lib/swipl/pack/logicmoo_base/t/examples/fol/sanity_exists_01.pl')).




end_of_file.

:- 
 ain(hasOrgan(iBob,iBobsHeart)).
:- 
 ain(tHeart(iBobsHeart)).





% You''ve proved Animal does not exist when:
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



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/436 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_exists_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_EXISTS_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_EXISTS_01 


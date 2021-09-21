#!/usr/bin/env lmoo-junit

:- module(kbii,[]).

:- include(test_header).

:- set_prolog_flag(retry_undefined,false).



%:- ensure_abox(kbii).
:- set_fileAssertMt(kbii).


% :- process_this_script.


:- set_prolog_flag(os_argv,[swipl, '-f', '/dev/null','--nonet']).

:- set_kif_option(+assert).


:- cls.      

% ============================================================
% all rooms have a door
% ============================================================
:- test_boxlog([+assert],all(R,exists(D,implies(room(R),and(door(D),has(R,D)))))).

% Should be simular in meaning to
% 
% door(D) :-
%       room(R),
%       skolem(D, skIsDoorInUnkArg2ofHas_1Fn(R)).
% ~room(R) :-
%       \+ ( 
%            door(D)
%          ).
% ~room(R) :-
%       \+ ( 
%            has(R, D)
%          ).
% has(R, D) :-
%       room(R),
%       skolem(D, skIsDoorInUnkArg2ofHas_1Fn(R)).


:- test_boxlog([+assert],all(D,implies(door(D),exists(K,and(knob(K),has(D,K)))))).

% ============================================================
% 1 room called room222 
% ============================================================
:- test_boxlog([+assert,+existentialize_objs],room(r111)).

room(r222).

:- test_boxlog([+assert],exactly(4,X,room(X))).

:- test_boxlog([+assert],room(r333)).

:- test_boxlog([+assert,+exist],room(r444)).


end_of_file.


% ============================================================
% joe knows  all rooms have a door
% ============================================================
:- test_boxlog(knows(joe,all(R,implies(room(R),exists(D,and(door(D),has(R,D)))))))  .

% Should be simular in meaning to
% ~knows(joe, room(A)) :-
%      \+ ( 
%           knows(joe, door(B))
%         ).
% ~knows(joe, room(A)) :-
%      \+ ( 
%           knows(joe, has(A, B))
%         ).
% knows(joe, door(A)) :-
%      knows(joe, room(B)),
%      skolem(A, skIsDoorInUnkArg2ofHas_2Fn(B)).
% knows(joe, has(A, B)) :-
%      knows(joe, room(A)),
%      skolem(B, skIsDoorInUnkArg2ofHas_2Fn(A)).

% ============================================================
% for all rooms joe knows about he belives a door exists
% ============================================================
:- test_boxlog(all(R,implies(knows(joe,room(R)),belives(joe,exists(D,and(door(D),has(R,D))))))).

% ~knows(joe, room(A)) :-
%      \+ ( 
%           belives(joe, door(B))
%         ).
% ~knows(joe, room(A)) :-
%      \+ ( 
%           belives(joe, has(A, B))
%         ).
% belives(joe, door(A)) :-
%      knows(joe, room(B)),
%      skolem(A, skIsDoorInUnkArg2ofHas_3Fn(B)).
% belives(joe, has(A, B)) :-
%      knows(joe, room(A)),
%      skolem(B, skIsDoorInUnkArg2ofHas_3Fn(A)).



% ============================================================
% For joe to eat the food it must be possible that the food is food and joe can eat it
% ============================================================
:- test_boxlog(poss(&(eats(joe,X),food(X)))=>(eats(joe,X),food(X))).

% eats(joe,X) :-
%       \+ ~eats(joe,X),
%       \+ ~food(X).
% food(X) :-
%       \+ ~eats(joe,X),
%       \+ ~food(X).

% ============================================================================================
% At least one man exists
% ============================================================================================

:- test_boxlog(atleast(1,X,man(X))).

% ============================================================================================
% At least three men exist
% ============================================================================================

:- test_boxlog(atleast(3,X,man(X))).

% ============================================================================================
% Organized Groups of prolog programmers have prolog programmers as members
% ============================================================================================

:- test_boxlog(implies(and(instance(M,tGroupedPrologOrganization),hasMembers(M,A)), instance(A,tClazzPrologPerson))).


/*
~hasMembers(_11164,_11142):- \+ (instance(_11164,tGroupedPrologOrganization),instance(_11142,tClazzPrologPerson)).
~instance(_11540,tGroupedPrologOrganization):- \+ (hasMembers(_11540,_11518),instance(_11518,tClazzPrologPerson)).
instance(_11874,tClazzPrologPerson):-instance(_11896,tGroupedPrologOrganization),hasMembers(_11896,_11874).
*/ 

% ============================================================================================
%  Those competeing in watersports may not wear shoes
% ============================================================================================
:- test_boxlog( =>(instance(ATH,tAgent),implies(and(instance(COMP,actAquaticSportsEvent),
competingAgents(COMP,ATH)),holdsIn(COMP,all(CLOTHING,not(and(instance(CLOTHING,tObjectShoe),wearsClothing(ATH,CLOTHING)))))))).
/*
~competingAgents(_20514,_20492):- ~holdsIn(_20514,v(~instance(_20470,tObjectShoe),~wearsClothing(_20492,_20470))),instance(_20514,actAquaticSportsEvent),instance(_20492,tAgent)
~instance(_20902,tAgent):- ~holdsIn(_20858,v(~instance(_20880,tObjectShoe),~wearsClothing(_20902,_20880))),instance(_20858,actAquaticSportsEvent),competingAgents(_20858,_20902)
~instance(_21272,actAquaticSportsEvent):- ~holdsIn(_21272,v(~instance(_21250,tObjectShoe),~wearsClothing(_21228,_21250))),competingAgents(_21272,_21228),instance(_21228,tAgent)
holdsIn(_21612,v(~instance(_21634,tObjectShoe),~wearsClothing(_21590,_21634))):-instance(_21612,actAquaticSportsEvent),competingAgents(_21612,_21590),instance(_21590,tAgent)
*/
% ~occuring(COMP) :-
%       instance(ATH, tAgent),
%       instance(COMP, actAquaticSportsEvent),
%       competingAgents(COMP, ATH),
%       instance(CLOTHING, tObjectShoe),
%       wearsClothing(ATH, CLOTHING).
% ~competingAgents(COMP, ATH) :-
%       instance(ATH, tAgent),
%       instance(COMP, actAquaticSportsEvent),
%       occuring(COMP),
%       instance(CLOTHING, tObjectShoe),
%       wearsClothing(ATH, CLOTHING).
% ~instance(CLOTHING, tObjectShoe) :-
%       instance(ATH, tAgent),
%       instance(COMP, actAquaticSportsEvent),
%       competingAgents(COMP, ATH),
%       occuring(COMP),
%       wearsClothing(ATH, CLOTHING).
% ~instance(COMP, actAquaticSportsEvent) :-
%       instance(ATH, tAgent),
%       competingAgents(COMP, ATH),
%       occuring(COMP),
%       instance(CLOTHING, tObjectShoe),
%       wearsClothing(ATH, CLOTHING).
% ~instance(ATH, tAgent) :-
%       instance(COMP, actAquaticSportsEvent),
%       competingAgents(COMP, ATH),
%       occuring(COMP),
%       instance(CLOTHING, tObjectShoe),
%       wearsClothing(ATH, CLOTHING).
% ~wearsClothing(ATH, CLOTHING) :-
%       instance(ATH, tAgent),
%       instance(COMP, actAquaticSportsEvent),
%       competingAgents(COMP, ATH),
%       occuring(COMP),
%       instance(CLOTHING, tObjectShoe).

% ================================================================================================================
%  When sightgseeing is occuring .. there is a tourist present
% ================================================================================================================
:- test_boxlog(implies(and(instance(Act,actSightseeing),performedBy(Person,Act)),holdsIn(Act,instance(Person,mobTourist)))).

/* OLD
~instance(_11704,actSightseeing):- \+ (performedBy(_11704,_11682),holdsIn(_11704,instance(_11682,mobTourist)))
~performedBy(_12092,_12070):- \+ (instance(_12092,actSightseeing),holdsIn(_12092,instance(_12070,mobTourist)))
holdsIn(_12442,instance(_12420,mobTourist)):-instance(_12442,actSightseeing),performedBy(_12442,_12420)

*/
% ~occuring(Act) :-
%       \+ ( instance(Act, actSightseeing),
%            naf(~performedBy(Person, Act)),
%            instance(Y, mobTourist)
%          ).
% ~instance(Act, actSightseeing) :-
%       \+ ( performedBy(Person, Act),
%            naf(~occuring(Act)),
%            instance(Y, mobTourist)
%          ).
% ~performedBy(Person, Act) :-
%       \+ ( instance(Act, actSightseeing),
%            naf(~occuring(Act)),
%            instance(Y, mobTourist)
%          ).
% instance(Y, mobTourist) :-
%       instance(Act, actSightseeing),
%       performedBy(Person, Act),
%       occuring(Act).


% ================================================================================================================
%  All rooms have a door (Odd syntax issue)
% ================================================================================================================

:- test_boxlog(all(R,room(R) => exists(D, and([door(D) , has(R,D)])))).

% door(D) :-
%       room(R),
%       skolem(D, skIsDoorInUnkArg2ofHas_2Fn(R)),
%       \+ ~has(R, D).
% ~room(R) :-
%       \+ ( has(R, D),
%            door(D)
%          ).
% has(R, D) :-
%       room(R),
%       skolem(D, skIsDoorInUnkArg2ofHas_2Fn(R)),
%       \+ ~door(D).




% ================================================================================================================
%  No one whom pays taxes in North america can be a dependant of another in the same year
% ================================================================================================================
:- set_prolog_flag(runtime_breaks,3).

:- test_boxlog([-ensure,-sort],
  or(
   holdsIn(YEAR, instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica))),
   holdsIn(YEAR, instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada))), 
   holdsIn(YEAR, instance(PERSON, nartR(mobTaxResidentsFn, iMexico))), 
   holdsIn(YEAR, instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica))), 
   forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, _SUPPORTEE)))).


% ~occuring(YEAR) :-
%       ~instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)),
%       \+ ( ( forbiddenToDoWrt(iCW_USIncomeTax,
%                               SUPPORTER,
%                               claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)),
%              instance(PERSON,
%                  nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica))
%            ),
%            instance(PERSON, nartR(mobTaxResidentsFn, iMexico))
%          ).
% instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica)) :-
%       occuring(YEAR),
%       ~instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iMexico)),
%       \+ ~forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)).
% instance(PERSON, nartR(mobTaxResidentsFn, iMexico)) :-
%       occuring(YEAR),
%       ~instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica)),
%       \+ ~forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)).
% instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)) :-
%       occuring(YEAR),
%       ~instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iMexico)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica)),
%       \+ ~forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)).
% instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)) :-
%       occuring(YEAR),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iMexico)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica)),
%       \+ ~forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)).
% forbiddenToDoWrt(iCW_USIncomeTax, SUPPORTER, claimsAsDependent(YEAR, SUPPORTER, SUPPORTEE)) :-
%       occuring(YEAR),
%       ~instance(PERSON, nartR(tClazzCitizenFn, iGroup_UnitedStatesOfAmerica)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_Canada)),
%       ~instance(PERSON, nartR(mobTaxResidentsFn, iMexico)),
%       \+ ~instance(PERSON, nartR(mobTaxResidentsFn, iGroup_UnitedStatesOfAmerica)).


% ================================================================================================================
%  Everything is human or god
% ================================================================================================================
:- test_boxlog(human(X) v god(X)).
% human(_19658) :-
%       ~god(_19658).
% god(_23530) :-
%       ~human(_23530).



% ================================================================================================================
%  Everything that is human is possibly male
% ================================================================================================================

:- test_boxlog(human(X) => poss(male(X))).

% ~(human(X)) :-
%       ~poss_t(male, X).
% proven_tru(poss_t(male, X)) :-
%       human(X).


% ================================================================================================================
%  Everything is human and male
% ================================================================================================================

%  BAD!! these need co-mingled!
:- test_boxlog((human(X) & male(X))).

% human(_21936).
% male(_24006).




% =======
%  Co-mingled!
% =======
:- P= (human(X) & male(X)) ,test_boxlog(poss(P) => P).

% human(X) :-
%       \+ ~human(X),
%       \+ ~male(X).
% male(X) :-
%       \+ ~human(X),
%       \+ ~male(X).


% These to?
% ~human(A) :-
%      \+ ( \+ ~male(A),
%           human(A)
%         ).
% ~human(A) :-
%      \+ ( \+ ~male(A),
%           male(A)
%         ).
% ~male(A) :-
%      \+ ( \+ ~human(A),
%           human(A)
%         ).
% ~male(A) :-
%      \+ ( \+ ~human(A),
%           male(A)
%         ).




% ================================================================================================================
%  Nothing is both human and god
% ================================================================================================================
:- test_boxlog(~ (human(X) & god(X))).

% ~human(_830) :-
%       god(_830).
% ~god(_904) :-
%       human(_904).

% ================================================================================================================
%  There exists something not evil
% ================================================================================================================
:- test_boxlog(exists(X,~evil(X))).

% ~evil(_788{sk = ...}) :-
%       skolem(_788{sk = ...}, skIsIn_8Fn).

% ================================================================================================================
%  There exists something evil
% ================================================================================================================
:- test_boxlog(exists(X,evil(X))).
% evil(_788{sk = ...}) :-
%       skolem(_788{sk = ...}, skIsIn_9Fn).

% ================================================================================================================
%  When a man exists there will be a god
% ================================================================================================================
:- test_boxlog(exists(X,man(X)) => exists(G,god(G))).
% god(_866{sk = ...}) :-
%       man(_1082),
%       skolem(_866{sk = ...}, skIsGodIn_1Fn(_1082)).
% ~man(_2276) :-
%       \+ ( skolem(_896{sk = ...}, skIsGodIn_1Fn(_2276)),
%            god(_896{sk = ...})
%          ).

% this would be better though if 
% god(_866{sk = ...}) :-
%       \= \+ (man(_1082)),
%       skolem(_866{sk = ...}, skIsGodIn_1Fn).
% ~man(_2276) :-
%       \+ god(_896).


% ================================================================================================================
%  When two men exists there will be a god
% ================================================================================================================
:- test_boxlog(atleast(2,X,man(X))=>exists(G,god(G))).

% god(_1298{sk = ...}) :-
%       skolem(_1358{sk = ...}, skIsManIn_1Fn(_10978, _1298{sk = ...})),
%       ~man(_1358{sk = ...}),
%       skolem(_1298{sk = ...}, skIsGodIn_2Fn(_10978)).
% god(_3110{sk = ...}) :-
%       man(_11144),
%       man(_11166),
%       skolem(_3110{sk = ...}, skIsGodIn_2Fn(_11166)).
% man(_2204{sk = ...}) :-
%       skolem(_2204{sk = ...}, skIsManIn_1Fn(_11314, _2224{sk = ...})),
%       skolem(_2224{sk = ...}, skIsGodIn_2Fn(_11314)),
%       \+ ~god(_2224{sk = ...}).
% ~man(_11494) :-
%       \+ ( man(_11516),
%            naf(~skolem(_4090{sk = ...}, skIsGodIn_2Fn(_11494))),
%            god(_4090{sk = ...})
%          ).
% ~man(_11656) :-
%       \+ ( man(_11678),
%            naf(~skolem(_4504{sk = ...}, skIsGodIn_2Fn(_11678))),
%            god(_4504{sk = ...})
%          ).


% OR?


% god(G) :-
%       skolem(_1316_sk, skIsManIn_1Fn(X, G)),
%       ~man(_1316_sk),
%       skolem(G, skIsGodIn_1Fn(X)).
% god(G) :-
%       man(_2720),
%       ~equals(_2742, X),
%       man(X),
%       skolem(G, skIsGodIn_1Fn(X)).
% man(_1342_sk) :-
%       skolem(_1342_sk, skIsManIn_1Fn(X, G)),
%       skolem(G, skIsGodIn_1Fn(X)),
%       \+ ~god(G).
% naf(~equals(_3108, X)) :-
%       man(_3148),
%       man(X),
%       skolem(G, skIsGodIn_1Fn(X)),
%       \+ ~god(G).
% ~man(X) :-
%       man(_3330),
%       \+ ( \+ ~ (~equals(_3352, X)),
%            naf(~skolem(G, skIsGodIn_1Fn(X))),
%            god(G)
%          ).
% ~man(_3544) :-
%       ~equals(_3566, X),
%       \+ ( man(X),
%            naf(~skolem(G, skIsGodIn_1Fn(X))),
%            god(G)
%          ).
% ~equals(_1478_sk, X) :-
%       \+ ( skolem(G, skIsGodIn_1Fn(X)),
%            god(G)
%          ).


% ================================================================================================================
% A person holding a bird is performing bird holding
% ================================================================================================================

:- test_boxlog([+assert],implies(
   and(instance(HOLD, actHoldingAnObject), objectActedOn(HOLD, BIRD), instance(BIRD, tClazzBird), 
             performedBy(HOLD, PER), instance(PER, mobPerson)), 
             holdsIn(HOLD, onPhysical(BIRD, PER)))).

% ~occuring(_2056) :-
%       instance(_2056, actHoldingAnObject),
%       objectActedOn(_2056, _2078),
%       instance(_2078, tClazzBird),
%       \+ ( performedBy(_2056, _2100),
%            naf(~instance(_2100, mobPerson)),
%            onPhysical(_2078, _2100)
%          ).
% ~instance(_2260, mobPerson) :-
%       instance(_2282, actHoldingAnObject),
%       objectActedOn(_2282, _2304),
%       instance(_2304, tClazzBird),
%       \+ ( performedBy(_2282, _2260),
%            naf(~occuring(_2282)),
%            onPhysical(_2304, _2260)
%          ).
% ~instance(_2520, tClazzBird) :-
%       instance(_2542, actHoldingAnObject),
%       objectActedOn(_2542, _2520),
%       performedBy(_2542, _2564),
%       \+ ( instance(_2564, mobPerson),
%            naf(~occuring(_2542)),
%            onPhysical(_2520, _2564)
%          ).
% ~instance(_2752, actHoldingAnObject) :-
%       objectActedOn(_2752, _2774),
%       instance(_2774, tClazzBird),
%       performedBy(_2752, _2796),
%       \+ ( instance(_2796, mobPerson),
%            naf(~occuring(_2752)),
%            onPhysical(_2774, _2796)
%          ).
% ~objectActedOn(_2984, _3006) :-
%       instance(_2984, actHoldingAnObject),
%       instance(_3006, tClazzBird),
%       performedBy(_2984, _3028),
%       \+ ( instance(_3028, mobPerson),
%            naf(~occuring(_2984)),
%            onPhysical(_3006, _3028)
%          ).
% ~performedBy(_3190, _3212) :-
%       instance(_3190, actHoldingAnObject),
%       objectActedOn(_3190, _3234),
%       instance(_3234, tClazzBird),
%       \+ ( instance(_3212, mobPerson),
%            naf(~occuring(_3190)),
%            onPhysical(_3234, _3212)
%          ).
% onPhysical(_3396, _3418) :-
%       instance(_3440, actHoldingAnObject),
%       objectActedOn(_3440, _3396),
%       instance(_3396, tClazzBird),
%       performedBy(_3440, _3418),
%       instance(_3418, mobPerson),
%       occuring(_3440).




:- test_boxlog(sourceSchemaObjectID(SOURCE, SCHEMA, uU(uSourceSchemaObjectFn, SOURCE, SCHEMA, ID), ID)).
% ~mudEquals(_1236{???UUUSOURCESCHEMAOBJECTFN1}, uU(uSourceSchemaObjectFn, _2438, _2460, _2482)) :-
%       ~sourceSchemaObjectID(_2438, _2460, _1280{???UUUSOURCESCHEMAOBJECTFN1}, _2482).
% sourceSchemaObjectID(_2614, _2636, UUUSOURCESCHEMAOBJECTFN1_VAR, _2658) :-
%       mudEquals(_1006{???UUUSOURCESCHEMAOBJECTFN1},
%                 uU(uSourceSchemaObjectFn, _2614, _2636, _2658)).


:- test_boxlog(sourceSchemaObjectID(SOURCE, SCHEMA, THING, uU(uSourceSchemaObjectIDFn, SOURCE, SCHEMA, THING))).
% ~mudEquals(_886{???UUUSOURCESCHEMAOBJECTIDFN1}, uU(uSourceSchemaObjectIDFn, _1072, _1094, _1116)) :-
%       ~sourceSchemaObjectID(_1072, _1094, _1116, _906{???UUUSOURCESCHEMAOBJECTIDFN1}).
% sourceSchemaObjectID(_1248, _1270, _1292, UUUSOURCESCHEMAOBJECTIDFN1_VAR) :-
%       mudEquals(_868{???UUUSOURCESCHEMAOBJECTIDFN1},
%                 uU(uSourceSchemaObjectIDFn, _1248, _1270, _1292)).



:- test_defunctionalize(implies(instance(YEAR, tClazzCalendarYear), temporallyFinishedBy(YEAR, uU(iTimeOf_SecondFn, 59, uU(iTimeOf_MinuteFn, 59, uU(iTimeOf_HourFn, 23, uU(iTimeOf_DayFn, 31, uU(iTimeOf_MonthFn, vDecember, YEAR)))))))).

% >(mudEquals(UUITIMEOF_SECONDFN1_VAR, uU(iTimeOf_SecondFn, 59, UUITIMEOF_MINUTEFN1_VAR)), =>(mudEquals(UUITIMEOF_MINUTEFN1_VAR, uU(iTimeOf_MinuteFn, 59, UUITIMEOF_HOURFN1_VAR)), =>(mudEquals(UUITIMEOF_HOURFN1_VAR, uU(iTimeOf_HourFn, 23, UUITIMEOF_DAYFN1_VAR)), =>(mudEquals(UUITIMEOF_DAYFN1_VAR, uU(iTimeOf_DayFn, 31, UUITIMEOF_MONTHFN1_VAR)), =>(mudEquals(UUITIMEOF_MONTHFN1_VAR, uU(iTimeOf_MonthFn, vDecember, _200)), implies(instance(_200, tClazzCalendarYear), temporallyFinishedBy(_200, UUITIMEOF_SECONDFN1_VAR))))))).


% ================================================================================================================
% At least 4 cute puppies
% ================================================================================================================

:- test_boxlog(atleast(4,X, puppy(X) & cute(X))).


:- test_boxlog([+pfc,+assert],exactly(6,X, puppy(X) & poss(cute(X)))).


% :-  test_repl.

end_of_file.



% ================================================================================================================
% At most 6 cute puppies
% ================================================================================================================

:- test_boxlog(atmost(6,X, puppy(X) & cute(X))).

% ================================================================================================================
% At exactly 5 cute puppies
% ================================================================================================================

:- test_boxlog(exactly(5,X, puppy(X) & cute(X))).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/428 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/boxlog_sanity_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/BOXLOG_SANITY_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ABOXLOG_SANITY_01 


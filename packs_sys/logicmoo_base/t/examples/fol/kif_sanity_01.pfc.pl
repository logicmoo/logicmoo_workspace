:- include(test_header).


:- discontiguous kif_sanity_test_0/0.


kif_sanity_test_0:- kif_test(
"
% )
tell.

all(R,room(R) => exists(D, (door(D) & has(R,D)))).
room(room1).

ask.

room(What).

door(What).

:- kif_add(a(XX) & b(XX) => c(XX)).
:- kif_add(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
:- kif_add(loves(Child,fatherFn(Child))).
:- kif_add((p => q)).
:- kif_add(~p <=> ~q).
:- kif_add(p <=> q).
:- kif_add(all(P, person(P) => -exists(D, dollar(D) & has(P,D)))).

:- kif_add(go(sam) & (go(bill) v go(sally) ) & go(nancy)).

:- kif_add(rains_tuesday => wear_rain_gear xor carry_umbrella).
:- kif_add(exists(P, (person(P) & all(C, car(C) => ~has(P,C))))).

/*
:- kif_add(room(R) => exists(D, (door(D) & has(R,D)))).
:- kif_add((goes(jane) xor goes(sandra) => goes(bill))).
:- kif_add(exists(P, exists(C, (person(P) & car(C) & has(P,C))))).
:- kif_add(~all(P,person(P) => exists(C, car(C) & has(P,C)))).
:- kif_add((go(sam) & go(bill)) v (go(sally) & go(nancy))).
:- kif_add(go(sam) & (go(bill) v go(sally) ) & go(nancy)).
:- kif_add(exists(C, course(C) & exists(MT1, midterm(C,MT1) & exists(MT2, midterm(C,MT2) & different(MT1,MT2))))).
:- kif_add(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).
*/

"
).


/*
:- told.
:- dmsg_show(_).
:- dmsg("i see this").
:- kif_add(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).
:- set_no_debug.
:- notrace.
:- nodebug.

:- wdmsg("we see this").

:- kif_add((p => q)).
:- kif_add(~p <=> ~q).
:- kif_add(tRoom(R) => exists(D, (tDoor(D) & has(R,D)))).
:- kif_add(all(P, person(P) => ~(exists(D, dollar(D) & has(P,D))))).
:- kif_add(p <=> q).
:- kif_add(all(P, person(P) => exists(D, dollar(D) & has(P,D)))).
*/

kif_sanity_test_0:-kif_test(all(R, exists(D, room(R) => (door(D) & has(R,D))))).

kif_sanity_test_0:-kif_test(p(A,R) & q(A,R)).


kif_sanity_test_0:- 
  kif_result((==>(mdefault((room(R) => {D = skIsDoorInRoomArg2ofHasFn(R)},has(R,D) & door(D)))))).



% kif_sanity_test_0:- kif_test(loves(fatherFn(Child),Child)).


% :- prolog.
%:- must(((kif_test(isa(F,tPred) => exists(A, (isa(A,ftInt) & arity(F,A))))))).

kif_sanity_test_0 :- nop(( kif_result(
(==> mdefault((
   tPred(F) ==> 
      {A = skIsIntInPredArg2ofArityFn(F)},arity(F,A) & ftInt(A))
 ))))).      


kif_sanity_test_0:-kif_test'(relationAllExists causes-EventEvent Exhibitionism VisualEvent)'.

kif_sanity_test_0:-kif_test '(relationAllExists properSubEvents Exhibitionism (DisplayingFn SexOrgan))'.


kif_sanity_test_0:-kif_test '(knows UnitedStatesOfAmerica (thereExists ?THING  (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))'.
kif_sanity_test_0:-kif_test '
(not (beliefs UnitedStatesOfAmerica (not (thereExists ?THING  (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))))
'.
kif_sanity_test_0:-kif_test '
(not (beliefs UnitedStatesOfAmerica (not (forAll ?THING  (not (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan)))))))
'.
kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (not (forAll ?THING  (not (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))))
'.

kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (and  KA KB KC KD))
'.

kif_sanity_test_0:-kif_test '
(beliefs UnitedStatesOfAmerica (and  BA BB BC BD))
'.

kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (or  KOA KOB KOC KOD))
'.

kif_sanity_test_0:-kif_test '
(beliefs UnitedStatesOfAmerica (or  BOA BOB BOC BOD))
'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (different ?THEMAN ?WOMAN) 
           (intendedMaleficiary ?CRIME ?THEMAN) 
           (deliberateActors ?CRIME ?WOMAN) 
           (behaviorCapable ?THEMAN 
               (CollectionSubsetFn Punishing 
                   (TheSetOf ?RESPONSE 
                       (maleficiary ?RESPONSE ?WOMAN))) deliberateActors)) 

       (optionAvailableToAgent-SitType ?THEMAN 
           (CollectionSubsetFn 
               (AttemptingFn Punishing) 
               (TheSetOf ?RETALIATION 
                   (and 
                       (intendedMaleficiary ?RETALIATION ?WOMAN) 
                       (purposeInEvent ?THEMAN ?RETALIATION 
                           (not 
                               (thereExists ?ANOTRACT 
                                   (and 
                                       (isa ?ANOTRACT PurposefulAction) 
                                       (startsAfterEndingOf ?ANOTRACT ?CRIME) 
                                       (maleficiary ?ANOTRACT ?THEMAN) 
                                       (deliberateActors ?ANOTRACT ?WOMAN)))))))) deliberateActors))'.

%:-prolog.

kif_sanity_test_0:-kif_test '
(implies
       (and 
           (isa ?AGREEMENT Agreement) 
           (intangibleParts ?AGREEMENT ?OBLIGATION) 
           (isa ?OBLIGATION Obligation) 
           (agreeingAgents ?AGREEMENT ?WOMAN) 
           (agentViolatesObligation ?WOMAN ?OBLIGATION)) 
       (agentViolatesAgreement ?WOMAN ?AGREEMENT))'.

% :-prolog.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?SEEING VisualEvent) 
           (objectActedOn ?SEEING ?WOMAN) 
           (isa ?WOMAN ExhibitionistOffender) 
           (actorPartsInvolved ?SEEING ?PART-TYPE) 
           (physicalPartTypes Eyes ?PART-TYPE) 
           (performedBy ?SEEING ?THEMAN)) 
       (increases-Generic ?SIT 
           (relationExistsInstance bodilyDoer 
               Shaming ?THEMAN) probability-Generic))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?ACT CriminalAct) 
           (isa ?ACT Exhibitionism) 
           (perpetrator ?ACT ?PERP)) 
       (isa ?PERP ExhibitionistOffender))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?PUNISH Punishing) 
           (performedBy ?PUNISH ?THEMAN) 
           (maleficiary ?PUNISH ?WOMAN)) 
       (beliefs ?THEMAN 
           (thereExists ?OBLIGATION 
               (agentViolatesObligation ?WOMAN ?OBLIGATION))))'.

kif_sanity_test_0:-kif_test '
(implies (and (isa ?MORAL-SHAMING Shaming)  (performedBy ?MORAL-SHAMING ?THEMAN)  (obligatedAgents TheGoldenRule ?THEMAN)) (agentViolatesObligation ?THEMAN TheGoldenRule))
'.

kif_sanity_test_0:-kif_test '
(thereExists ?THEMAN (implies 
   (thereExists ?MORAL-SHAMING (and (isa ?MORAL-SHAMING Shaming) (performedBy ?MORAL-SHAMING ?THEMAN)  (obligatedAgents TheGoldenRule ?THEMAN)))
   (agentViolatesObligation ?THEMAN TheGoldenRule)))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?INST1 Exhibitionism) 
           ((PresentTenseVersionFn doneBy) ?INST1 ?INST2)) 
       (isa ?INST2 ExhibitionistOffender))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?MS VisualEvent) 
           (actorPartsInvolved ?MS ?MP) 
           (isa ?MP Eyes)) 
       (holdsIn ?MS 
           (portalState ?MP OpenPortal)))'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (performedBy ?ACT ?WOMAN)
           (isa ?ACT (DisplayingFn SexOrgan))           
           (lawProscribesActType ?LAW Exhibitionism) 
           (subjectToCOC ?WOMAN ?LAW)) 
       (and 
           (isa ?ACT Exhibitionism) 
           (agentViolatesObligation ?WOMAN ?LAW)))'.

kif_sanity_test_0:-kif_test '
(not 
       (and 
           (subjectToCOC ?SUNBATHER KeepAreolaCoveredInPublic) 
           (objectFoundInLocation ?SUNBATHER ?BEACH) 
           (isa ?BEACH ToplessBeach)))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?COC LegalCode-ModernWestern) 
           (isa ?ACT Exhibitionism) 
           (subjectToCOC ?WOMAN ?COC)
           (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
           (performedBy ?ACT ?WOMAN)) 
       (ist ?COC 
           (isa ?ACT CriminalAct)))'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?AREOLA 
               (BodyPartCollectionFn ?WOMAN Areola)) 
           (subjectToCOC ?WOMAN KeepAreolaCoveredInPublic) 
           (locationState ?WOMAN InPublic)) 
       (thereExists ?CLOTH 
           (and 
               (or 
                   (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
                   (covers-Generic ?CLOTH ?AREOLA)) 
               (or 
                   (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
                   (wearsClothing ?WOMAN ?CLOTH)))))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (different ?THEMAN ?WOMAN) 
           (intendedMaleficiary ?CRIME ?THEMAN) 
           (deliberateActors ?CRIME ?WOMAN) 
           (behaviorCapable ?THEMAN 
               (CollectionSubsetFn Punishing 
                   (TheSetOf ?RESPONSE 
                       (maleficiary ?RESPONSE ?WOMAN))) deliberateActors)) 

       (optionAvailableToAgent-SitType ?THEMAN 
           (CollectionSubsetFn 
               (AttemptingFn Punishing) 
               (TheSetOf ?RETALIATION 
                   (and 
                       (intendedMaleficiary ?RETALIATION ?WOMAN) 
                       (purposeInEvent ?THEMAN ?RETALIATION 
                           (not 
                               (thereExists ?ANOTRACT 
                                   (and 
                                       (isa ?ANOTRACT PurposefulAction) 
                                       (startsAfterEndingOf ?ANOTRACT ?CRIME) 
                                       (maleficiary ?ANOTRACT ?THEMAN) 
                                       (deliberateActors ?ANOTRACT ?WOMAN)))))))) deliberateActors))'.

kif_sanity_test_0:-kif_test '
(beliefs InternationalCommunity 
       (thereExists ?WEAP 
           (and 
               (isa ?WEAP ChemicalWeapon) 
               (possesses Israel ?WEAP))))'.




:- if(true).

kif_sanity_test_0:-kif_test '
(implies 
       (hasBeliefSystems ?WOMAN Karma) 
       (beliefs ?WOMAN 
           (implies 
               (and 
                   (isa ?MORAL-SHAMING Shaming)
                   (isa ?ANY Punishing)
                   (sinner ?MORAL-SHAMING ?THEMAN) 
                   (isa ?THEMAN 
                       (IncarnationPhysicalFn ?SOUL Organism-Whole)) 
                   (not 
                       (punishmentFor ?THEMAN ?ANY 
                           (sinner ?MORAL-SHAMING ?THEMAN)))) 
               (thereExists ?NEXTLIFE 
                   (thereExists ?PUN 
                       (and 
                           (isa ?PUN Punishing) 
                           (startsAfterEndingOf ?NEXTLIFE ?THEMAN) 
                           (isa ?NEXTLIFE 
                               (IncarnationPhysicalFn ?SOUL Organism-Whole)) 
                           (punishmentFor ?NEXTLIFE ?PUN 
                               (sinner ?MORAL-SHAMING ?THEMAN))))))))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?ACTION PurposefulAction) 
           (eventOccursAt ?ACTION ?LOCATION) 
           (geographicalSubRegions ?LAND ?LOCATION) 
           (territoryOf ?COUNTRY ?LAND) 
           (isa ?COUNTRY IndependentCountry) 
           (beliefs ?COUNTRY 
               (directingAgent ?ACTION ?AGENT))) 
       (causes-SitProp ?ACTION 
           (beliefs ?COUNTRY 
               (behaviorCapable ?AGENT 
                   (CollectionSubsetFn PurposefulAction 
                       (TheSetOf ?OBJ 
                           (eventOccursAt ?OBJ ?LAND))) directingAgent))))'.

:- endif.
:- if(if_defined(show_argtype_tests)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this rule ...

kif_sanity_test_0:-kif_test((   wearing(A,B)  => has(A,B)  )).

% has to qualify argument types before canicalization

kif_sanity_test_0:-  kif_test((argInst(has,1,A) & argInst(has,2,B) => (wearing(A,B) => has(A,B)))).

% Which produced this code:
%
%       has(A, B):-wearing(A, B), argInst(has, 1, A), argInst(has, 2, B).
%
%       not_wearing(A, B):- not_has(A, B), argInst(has, 1, A), argInst(has, 2, B).  % why are the last two litterals so important? 
%
%       not_argInst(has, 1, A):- not_has(A, B), wearing(A, B), argInst(has, 2, B).   % notice we can disprove types
%
%       not_argInst(has, 2, A):- not_has(B, A), wearing(B, A), argInst(has, 1, B).


%


kif_sanity_test_0:-kif_test(has(A,B) => (argInst(has, 1, A) & argInst(has, 2, B))).

%         not_has(A, _):- not_argInst(has, 1, A).
%
%         argInst(has, 1, A):-has(A, _).
%
%         not_has(_, B)):- not_argInst(has, 2, B).
%
%         argInst(has, 2, A):-has(_, A).



kif_sanity_test_0:-kif_test(has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B))).

% BAD!
%         (( not_has(A, _)):- not_kb_argInst( _BAD, has, 1, A)).
%
%          (kb_argInst( _BAD, has, 1, A):-has(A, _)).
%
%           (( not_has(_, A)):- not_kb_argInst( _BAD ,  has, 2, A)).
%
%            (kb_argInst( _BAD, has, 2, A):-has(_, A)).



% :- prolog.
% GOOD! (the software does this for us but wanted to show the singlton in the consequent on the conjuction)

kif_sanity_test_0:-kif_test(   argInst(kb_argInst, 1 ,KB) =>  (        has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B)))).

%     (( not_argInst(kb_argInst, 1,KB)):-has(A, _),  not_kb_argInst(KB, has, 1, A)).
%
%     (( not_has(A, _)):-argInst(kb_argInst, 1,KB),  not_kb_argInst(KB, has, 1, A)).
%
%     (kb_argInst(KB, has, 1, A):- argInst(kb_argInst, 1,KB), has(A, _)).
%
%    (( not_argInst(kb_argInst, 1,KB)):-has(_, B),  not_kb_argInst(KB, has, 2, B)).
%
%     (( not_has(_, B)):-argInst(kb_argInst, 1,KB),  not_kb_argInst(KB, has, 2, B)).
%
%    (kb_argInst(KB, has, 2, B):-argInst(kb_argInst, 1,KB), has(_, B)).

% EVEN BETTER?
kif_sanity_test_0:-kif_test(   argInst(kb_argInst, 1 ,KB) & argInst(has, 1 , A) & argInst(has, 2 , B) =>  (  has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B)))).


%   ain= (not_has(A, B)):- not_kb_argInst(C, has, 1, A), argInst(has, 2, B), argInst(kb_argInst, 1, C), argInst(has, 1, A)).
%
%   ain= (kb_argInst(C, has, 1, A):-has(A, B), argInst(has, 2, B), argInst(kb_argInst, 1, C), argInst(has, 1, A)).
%
%   ain= (not_argInst(has, 2, A)):-has(B, A), not_kb_argInst(C, has, 1, B), argInst(kb_argInst, 1, C), argInst(has, 1, B)).
%
%   ain= (not_argInst(kb_argInst, 1, A)):-has(B, C), not_kb_argInst(A, has, 1, B), argInst(has, 2, C), argInst(has, 1, B)).
%
%   ain= (not_argInst(has, 1, A)):-has(A, B), not_kb_argInst(C, has, 1, A), argInst(has, 2, B), argInst(kb_argInst, 1, C)).
%
%   (not_has(C, A)):- not_kb_argInst(B, has, 2, A), argInst(has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (kb_argInst(B, has, 2, A):-has(C, A), argInst(has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (not_argInst(has, 2, A)):-has(C, A), not_kb_argInst(B, has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (not_argInst(kb_argInst, 1, A)):-has(C, B), not_kb_argInst(A, has, 2, B), argInst(has, 2, B), argInst(has, 1, C)).
%
%   (not_argInst(has, 1, A)):-has(A, B), not_kb_argInst(C, has, 2, B), argInst(has, 2, B), argInst(kb_argInst, 1, C)).


:- endif. %if_defined(show_argtype_tests)


kif_sanity_test_0:-kif_test(all(R,isa(R,tAgent) => exists(D, (isa(D,tNose) & mudContains(R,D))))).


kif_sanity_test_0:- kif_test(all(X, (~tNotFly(X) => ~tPengin(X)))).
kif_sanity_test_0:- kif_test(not(and(omitArgIsa(RELN, N), argIsa(RELN, N, _THING)))).


kif_sanity_test_0:- kif_result((tNotFly(X):-tPengin(X))).
   % we prove we dont yet know if something not a pengiun when we call notFly and it fails
kif_sanity_test_0:- kif_result((  ~(tPengin(A)) :-  ~tNotFly(A)  )).



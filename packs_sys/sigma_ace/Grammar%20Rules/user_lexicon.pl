% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% ATO Lexicon
% by Douglas J. Wulf
% Teknowledge Corporation
% Usable by CELT for construction of expressions in ACE 

:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading user-defined lexicon (ATO and other examples) into CELT.'),nl.

% NOUNS
noun_in_lexicon([ato,mission],object,_,count,singular,'OrganizationalProcess',empty).
noun_in_lexicon([ato,package],object,_,count,singular,'OrganizationalProcess',empty).
noun_in_lexicon([ato,plan],object,_,count,singular,'Plan',empty).
noun_in_lexicon([ato,air,battle,plan],object,_,count,singular,'Plan',empty).
noun_in_lexicon([call,number],object,_,count,singular,'attribute',empty).
noun_in_lexicon([call,word],object,_,count,singular,'attribute',empty).
noun_in_lexicon([callsign],object,_,count,singular,'attribute',empty).
noun_in_lexicon(f15c,object,_,count,singular,'Weapon',empty).
noun_in_lexicon([f15c,mission],object,_,count,singular,'OrganizationalProcess',empty).
noun_in_lexicon(f117a,object,_,count,singular,'Weapon',empty).
noun_in_lexicon([f117a,mission],object,_,count,singular,'OrganizationalProcess',empty).
noun_in_lexicon([message,set],object,_,count,singular,'ContentBearingObject',empty).
noun_in_lexicon([military,aircraft],object,_,count,singular,'Weapon',empty).
noun_in_lexicon([mission,number],object,_,count,singular,'attribute',empty).
noun_in_lexicon([ground,target],object,_,count,singular,'Object',empty).
noun_in_lexicon([primary,target],object,_,count,singular,'Object',empty).
noun_in_lexicon([secondary,target],object,_,count,singular,'Object',empty).
noun_in_lexicon([interdiction,target],object,_,count,singular,'Object',empty).
noun_in_lexicon([oca,target],object,_,count,singular,'Object',empty).
noun_in_lexicon([mission,commander],object,_,count,singular,'OccupationalRole',empty).

% Note: that by making a noun a mass noun we allow it not to require a preceding
% determiner. E.g., we can say 'mission type combat air patrol' as 'combat air
% patrol' is a mass noun, otherwise we would have to say 'mission type the combat
% air patrol' or 'mission type a combat air patrol'.
noun_in_lexicon([combat,air,patrol],object,_,mass,singular,'OccupationalRole',empty).

% add to ontology: 'Target'

% PROPER NOUNS

% proper_noun_in_lexicon(Name,Type,Gender,Number,SUO_constant,SUO_type,Synset_ID).
% where Type is one of [person,time,object] (the same as nouns).

proper_noun_in_lexicon(['Apache','Longbow'],object,_,singular,'Object','AH64A',empty).
proper_noun_in_lexicon(['Target','A'],object,_,singular,'Object','Target_A',empty).
proper_noun_in_lexicon(['Target','B'],object,_,singular,'Object','Target_B',empty).
proper_noun_in_lexicon(['Target','C'],object,_,singular,'Object','Target_C',empty).
proper_noun_in_lexicon(['Mission','1'],object,_,singular,'OrganizationalProcess','Mission_1',empty).
proper_noun_in_lexicon(['Mission','2'],object,_,singular,'OrganizationalProcess','Mission_2',empty).
proper_noun_in_lexicon(['Package','A'],object,_,singular,'OrganizationalProcess','Package_A',empty).
proper_noun_in_lexicon(['Location','1'],object,_,singular,'Region','Loc_1',empty).
proper_noun_in_lexicon(['Location','2'],object,_,singular,'Region','Loc_2',empty).
proper_noun_in_lexicon(['Location','3'],object,_,singular,'Region','Loc_3',empty).
proper_noun_in_lexicon(['Location','4'],object,_,singular,'Region','Loc_4',empty).
proper_noun_in_lexicon(['Location','5'],object,_,singular,'Region','Loc_5',empty).
proper_noun_in_lexicon(['Location','6'],object,_,singular,'Region','Loc_6',empty).
proper_noun_in_lexicon(['Location','7'],object,_,singular,'Region','Loc_7',empty).
proper_noun_in_lexicon(['Location','8'],object,_,singular,'Region','Loc_8',empty).
proper_noun_in_lexicon(['Location','9'],object,_,singular,'Region','Loc_9',empty).
proper_noun_in_lexicon(['Location','10'],object,_,singular,'Region','Loc_10',empty).

% times for ATO domain
proper_noun_in_lexicon('T1',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T2',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T3',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T4',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T5',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T6',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T7',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('T8',time,_,singular,'TimePoint','TimeMeasure',empty).

proper_noun_in_lexicon('X1',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('X2',time,_,singular,'TimePoint','TimeMeasure',empty).
proper_noun_in_lexicon('X3',time,_,singular,'TimePoint','TimeMeasure',empty).

% these are here temporarily to allow them to be referred to without determiners:
% e.g., 'the plane retracts its wheels at liftoff'
proper_noun_in_lexicon(liftoff,time,_,singular,'TimeMeasure','TimeMeasure',105488663).
proper_noun_in_lexicon(touchdown,time,_,singular,'TimeMeasure','TimeMeasure',100197419).

% VERBS
verb_in_lexicon([breaks,up],[break,up],[intransitive,no,no],singular,simple,event,'IntentionalProcess',101387651).
verb_in_lexicon([joins,up],[join,up],[intransitive,no,no],singular,simple,event,'IntentionalProcess',empty).
verb_in_lexicon(lands,land,[intransitive,no,no],singular,simple,event,'IntentionalProcess',101349596).
verb_in_lexicon([takes,off],[take,off],[intransitive,no,no],singular,simple,event,'IntentionalProcess',101375175).
verb_in_lexicon([route,transitions],[route,transition],[intransitive,no,no],singular,simple,event,'IntentionalProcess',empty).
verb_in_lexicon([schedules,tfs],[schedule,tfs],[no,transitive,no],singular,simple,event,'IntentionalProcess',empty).
verb_in_lexicon([schedules,tos],[schedule,tos],[no,transitive,no],singular,simple,event,'IntentionalProcess',empty).
verb_in_lexicon(comprises,comprise,[no,transitive,no],singular,simple,event,'Combining',101796995).
verb_in_lexicon([consists,of],[comprise],[no,transitive,no],singular,simple,event,'Combining',101796995).
verb_in_lexicon(tasks,task,[no,transitive,no],singular,simple,event,'IntentionalProcess').
verb_in_lexicon(replaces,replace,[no,transitive,no],singular,event,simple,'Substituting',200111859).
verb_in_lexicon(refuels,refuel,[intransitive,no,no],singular,event,simple,'Process',200111859).
verb_in_lexicon(attacks,attack,[no,transitive,no],singular,event,simple,'Process',200111859).

% prepositional verbs (see P.49)
verb_in_lexicon([consists,of],[consist,of],[no,transitive,no],singular,prepositional,event,'Combining',101796995).

% ungraded adjectives (note: use positive for the grade)
adjective_in_lexicon(alternate,alternate,normal,positive,'WN_301782779').

% two-place adjectives (take a prepositional complement, the preposition is included here)
% Note: SUO_concept is a lambda expression for two-place adjectives.
adjective_in_lexicon([alternative,to],identical,two_place,ungraded,X^Y^[alternative_target_for,X,Y]).

% nouns that are roles or otherwise require translations beyond
% simple translations like (instance ?sample Sample) for some example
% class Sample.

noun_in_lexicon([mission,ground,target],object,_,count,singular,'groundTargetOfATOMission',empty). 

% translation_template(+Role,+Position,+Suggested_Var_Names, +List_of_SUO_types)
% is interpreted to mean that to say that some SUO var plays role Role use this template
% with the SUO var in position (starting with 1) Position and constrain
% the other variables (which most also be introduced) to the SUO types
% given.


translation_template('groundTargetOfATOMission',1,                % WHICH arg the noun maps to
		     ['?target','?mission'],                      % Informal documentation for each arg
		     ['Object','ATOMissionPlan']).                % Type restrictions

doug18 :- eng2log('Target B is a mission ground target.').

% (and
%	(instance Target1 Object)
%	(instance Mission1 ATOMissionPlan)
%	(groundTargetOfATOMission Target1 Mission1))

%---------------------------------------
% CODE FOR TESTING ATO SENTENCES
%---------------------------------------

% Target B is a ground target that is in Mission 1.
% Target C is a ground target that is in in Mission 1.
% Target C is the alternate target of Target B.

doug1 :- eng2log('Target B is a ground target that is in Mission 1.').

doug2 :- eng2log('Target C is a ground target that is in Mission 1.').

doug3 :- eng2log('Target C is the alternate target of Target B.').

doug4 :- eng2log('Target C is alternative to Target B.').

doug5 :- eng2log('Target C is an alternate ground target that replaces Target B.').

doug6 :- eng2log('Mission 1 is an f117a mission.').

doug7 :- eng2log('Mission 1 takes off at T1 at Location 1.').

doug8 :- eng2log('Mission 1 refuels at T2 at Location 2.').

doug9 :- eng2log('Mission 1 attacks Target A at T3 at Location 3.').

doug10 :- eng2log('Package A joins up at T4 at Location 4.').

doug11 :- eng2log('The mission attacks Target B at T5 at Location 5.').

doug12 :- eng2log('Mission 1 attacks Target C at T6 at Location 6.').

doug13 :- eng2log('Package A breaks up at T7 at Location 7.').

doug14 :- eng2log('Mission 1 lands at T8 at Location 1.').

doug15 :- eng2log('Mission 2 takes off at X1 at Location 9.').

doug16 :- eng2log('Mission 2 route transitions at X2 at Location 10.').

doug17 :- eng2log('Mission 2 lands at X3 at Location 9.').





/** <module> 
% Nonmodule Impl of CircleMUD StarTrek TNG files
%
% Dec 13, 2035
% Douglas Miles
%
*/
%:- nop(module( baseKB)).
:- expects_dialect(pfc).

%:- expects_dialect(pfc).

:- install_constant_renamer_until_eof.

:- style_check(-discontiguous).

typeGrid('tSpaceInAHOC',0, [ed,ed,ed,ex,ed,ed,ed]).
typeGrid('tSpaceInAHOC',1, [ed,--,--,--,--,--,ed]).
typeGrid('tSpaceInAHOC',2, [ed,--,--,--,--,--,ed]).
typeGrid('tSpaceInAHOC',3, [ex,--,--,--,--,--,ex]).
typeGrid('tSpaceInAHOC',4, [ed,--,--,--,--,--,ed]).
typeGrid('tSpaceInAHOC',5, [ed,--,--,--,--,--,ed]).
typeGrid('tSpaceInAHOC',6, [ed,ed,ed,ex,ed,ed,ed]).


typeGrid('tFreeSpaceContent',1, [--,--,--,--,--]).
typeGrid('tFreeSpaceContent',2, [--,--,--,--,--]).
typeGrid('tFreeSpaceContent',3, [--,--,--,--,--]).
typeGrid('tFreeSpaceContent',4, [--,--,--,--,--]).
typeGrid('tFreeSpaceContent',5, [--,--,--,--,--]).

% center of room is 3,3,1 (rooms ranges are (15,15,13))

% : style_check(singleton).
:-set_prolog_flag(double_quotes,string).
%:-style_check(+atom).

%%:['../hyhtn/translog4.pl'].
%end_of_file.

pddlPredicates([
% dynamic startrek
  nameString(ftTerm,ftString),
  pathName(tRegion,vtDirection,ftString),
%  localityOfObject(tObj,tRegion),
  mudStowing(tAgent,tClothing),
  mudStowing(tAgent,tWeapon),
  mudStowing(tAgent,tCarryAble),
  wearsClothing(tAgent,tClothing),

% static startrek
  pathDirLeadsTo(tRegion,vtDirection,tRegion),
  agentRoute(tPathway,tRegion,tRegion) ]).



/* Prediate:  tRegion/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(30).
number_of_clauses(1).
Pattern: tRegion(_G1399). 
 */
tRegion('iArea1000').
tRegion('iArea1002').
tRegion('iArea1001').
tRegion('iArea1005').
tRegion('iArea1003').
tRegion('iArea1004').
tRegion('iArea1008').
tRegion('iArea1006').
tRegion('iArea1042').
tRegion('iArea1007').
tRegion('iArea1010').
tRegion('iArea1009').
tRegion('iArea1011').
tRegion('iArea1013').
tRegion('iArea1032').
tRegion('iArea1012').
tRegion('iArea1016').
tRegion('iArea1014').
tRegion('iArea1015').
tRegion('iArea1019').
tRegion('iArea1017').
tRegion('iArea1018').
tRegion('iArea1021').
tRegion('iArea1020').
tRegion('iArea1022').
tRegion('iArea1024').
tRegion('iArea1039').
tRegion('iArea1023').
tRegion('iArea1027').
tRegion('iArea1025').
tRegion('iArea1026').
tRegion('iArea1030').
tRegion('iArea1028').
tRegion('iArea1029').
tRegion('iArea1031').
tRegion('iArea1033').
tRegion('iArea1034').
tRegion('iArea1036').
tRegion('iArea1035').
tRegion('iArea1038').
tRegion('iArea1037').
tRegion('iArea1040').
tRegion('iArea1041').

 /* found 45 for tRegion(_G1399). */ 

/* Prediate:  tAgent/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(55).
number_of_clauses(1).
Pattern: tAgent(_G1281). 
 */
tAgent('iGeordi684').
tAgent(iCommanderdata66).
tAgent(iExplorer1).
tAgent('iWorf720').
tAgent('iDrCrusher677').
tAgent('iTroi712').
tAgent('iRiker707').
tAgent('iPicard701').
tAgent('iGuinan689').
tAgent('iOBrien696').
tAgent('iWesley716').
tAgent('iLivingston726').
tAgent('iSpot727').
tAgent('iExplorer3').
tAgent('iExplorer2').
tAgent('iExplorer4').
tAgent('iExplorer5').
tAgent('iExplorer6').
tAgent('iExplorer7').
tAgent('iExplorer8').
tAgent('iAlexander671').

 /* found 21 for tAgent(_G1281). */ 

/* Prediate:  wearsClothing/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(58).
number_of_clauses(1).
Pattern: wearsClothing(_G1304,_G1305). 
 */
pddlSomethingIsa('iBoots673',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
wearsClothing(iCommanderdata66,'iBoots673').
wearsClothing(iCommanderdata66,'iCommBadge674').
wearsClothing(iCommanderdata66,'iGoldUniform675').
wearsClothing(iExplorer1,'iBoots773').
wearsClothing(iExplorer1,'iCommBadge774').
wearsClothing(iExplorer1,'iGoldUniform775').
wearsClothing('iDrCrusher677','iBoots678').
wearsClothing('iDrCrusher677','iCommBadge679').
wearsClothing('iDrCrusher677','iBlueUniform680').
wearsClothing('iGeordi684','iBoots685').
wearsClothing('iGeordi684','iCommBadge686').
wearsClothing('iGeordi684','iGoldUniform687').
wearsClothing('iGeordi684','iVISOR688').
wearsClothing('iOBrien696','iBoots697').
wearsClothing('iOBrien696','iCommBadge698').
wearsClothing('iOBrien696','iGoldUniform699').
wearsClothing('iPicard701','iBoots702').
wearsClothing('iPicard701','iCommBadge703').
wearsClothing('iPicard701','iRedUniform704').
wearsClothing('iRiker707','iBoots708').
wearsClothing('iRiker707','iCommBadge709').
wearsClothing('iRiker707','iRedUniform710').
wearsClothing('iTroi712','iBoots713').
wearsClothing('iTroi712','iCommBadge714').
wearsClothing('iTroi712','iBlueUniform715').
wearsClothing('iWesley716','iBoots717').
wearsClothing('iWesley716','iCommBadge718').
wearsClothing('iWesley716','iRedUniform719').
wearsClothing('iWorf720','iBoots721').
wearsClothing('iWorf720','iCommBadge722').
wearsClothing('iWorf720','iGoldUniform723').
wearsClothing('iWorf720','iSash725').
wearsClothing('iExplorer2','iBoots729').
wearsClothing('iExplorer2','iCommBadge730').
wearsClothing('iExplorer2','iGoldUniform731').
wearsClothing('iExplorer3','iBoots733').
wearsClothing('iExplorer3','iCommBadge734').
wearsClothing('iExplorer3','iGoldUniform735').
wearsClothing('iExplorer4','iBoots737').
wearsClothing('iExplorer4','iCommBadge738').
wearsClothing('iExplorer4','iRedUniform739').
wearsClothing('iExplorer5','iBoots741').
wearsClothing('iExplorer5','iCommBadge742').
wearsClothing('iExplorer5','iRedUniform743').
wearsClothing('iExplorer6','iBoots745').
wearsClothing('iExplorer6','iCommBadge746').
wearsClothing('iExplorer6','iBlueUniform747').
wearsClothing('iExplorer7','iBoots749').
wearsClothing('iExplorer7','iCommBadge750').
wearsClothing('iExplorer7','iBlueUniform751').
wearsClothing('iExplorer8','iBoots753').
wearsClothing('iExplorer8','iCommBadge754').
wearsClothing('iExplorer8','iBlueUniform755').

 /* found 53 for wearsClothing(_G1304,_G1305). */ 

/* Prediate:  mudStowing/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(57).
number_of_clauses(1).
Pattern: mudStowing(_G1328,_G1329). 
 */
mudStowing(iCommanderdata66,'iPhaser676').
mudStowing(iExplorer1,'iPhaser776').
mudStowing('iDrCrusher677','iMedicalTricorder681').
mudStowing('iDrCrusher677','iMedicalTricorder682').
mudStowing('iDrCrusher677','iMedicalTricorder683').
mudStowing('iGuinan689','iTea690').
mudStowing('iGuinan689','iSynthehol691').
mudStowing('iGuinan689','iFerengiAle692').
mudStowing('iGuinan689','iRomulanWhisky693').
mudStowing('iGuinan689','iLemonadePruneJuice694').
mudStowing('iGuinan689','iVulcanBeer695').
mudStowing('iOBrien696','iPhaser700').
mudStowing('iPicard701','iPhaserRifle705').
mudStowing('iPicard701','iPicardsFlute').
mudStowing('iRiker707','iTrombone711').
mudStowing('iWorf720','iPhaser724').

 /* found 16 for mudStowing(_G1328,_G1329). */ 

/* Prediate:  tItem/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(56).
number_of_clauses(1).
Pattern: tItem(_G1352). 
 */
tItem('iPhaser676').
tItem('iPhaser776').
tItem('iPhaser700').
tItem('iPhaser724').
tItem('iPhaserRifle705').
tItem('iRedUniform704').
tItem('iRedUniform710').
tItem('iRedUniform719').
tItem('iRedUniform739').
tItem('iRedUniform743').
tItem('iGoldUniform675').
tItem('iGoldUniform775').
tItem('iGoldUniform687').
tItem('iGoldUniform699').
tItem('iGoldUniform723').
tItem('iGoldUniform731').
tItem('iGoldUniform735').
tItem('iBlueUniform680').
tItem('iBlueUniform715').
tItem('iBlueUniform747').
tItem('iBlueUniform751').
tItem('iBlueUniform755').
tItem('iBoots673').
tItem('iBoots773').
tItem('iBoots678').
tItem('iBoots685').
tItem('iBoots697').
tItem('iBoots702').
tItem('iBoots708').
tItem('iBoots713').
tItem('iBoots717').
tItem('iBoots721').
tItem('iBoots729').
tItem('iBoots733').
tItem('iBoots737').
tItem('iBoots741').
tItem('iBoots745').
tItem('iBoots749').
tItem('iBoots753').
tItem('iCommBadge674').
tItem('iCommBadge774').
tItem('iCommBadge679').
tItem('iCommBadge686').
tItem('iCommBadge698').
tItem('iCommBadge703').
tItem('iCommBadge709').
tItem('iCommBadge714').
tItem('iCommBadge718').
tItem('iCommBadge722').
tItem('iCommBadge730').
tItem('iCommBadge734').
tItem('iCommBadge738').
tItem('iCommBadge742').
tItem('iCommBadge746').
tItem('iCommBadge750').
tItem('iCommBadge754').
tItem('iSash725').
tItem('iVISOR688').
tItem('iMedicalTricorder681').
tItem('iMedicalTricorder682').
tItem('iMedicalTricorder683').
tItem('iTricorder759').
tItem('iTricorder760').
tItem('iTricorder761').
tItem('iDilithiumCrystal756').
tItem('iDilithiumCrystal757').
tItem('iDilithiumCrystal758').
tItem('iPicardsFlute').
tItem('iTrombone711').
tItem('iTea690').
tItem('iSynthehol691').
tItem('iFerengiAle692').
tItem('iRomulanWhisky693').
tItem('iLemonadePruneJuice694').
tItem('iVulcanBeer695').

 /* found 75 for tItem(_G1352). */ 

/* Prediate:  localityOfObject/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(59).
number_of_clauses(1).
Pattern: localityOfObject(_G1375,_G1376). 
 */
/*
 Moved down a little?
localityOfObject('iArea1005Object666','iArea1005').
localityOfObject('iArea1008Object667','iArea1008').
localityOfObject('iArea1013Object668','iArea1013').
localityOfObject('iArea1016Object669','iArea1016').
localityOfObject('iArea1024Object670','iArea1024').
*/

% :- rtrace.
localityOfObject('iAlexander671','iArea1025').
localityOfObject(iCommanderdata66,'iArea1010').
localityOfObject(iExplorer1,'iArea1000').
localityOfObject('iDrCrusher677','iArea1015').
localityOfObject('iGeordi684','iArea1000').
localityOfObject('iGuinan689','iArea1021').
localityOfObject('iOBrien696','iArea1006').
localityOfObject('iPicard701','iArea1035').
localityOfObject('iRiker707','iArea1036').
localityOfObject('iTroi712','iArea1007').
localityOfObject('iWesley716','iArea1016').
localityOfObject('iWorf720','iArea1025').
localityOfObject('iLivingston726','iArea1035').
localityOfObject('iSpot727','iArea1003').
localityOfObject('iExplorer2','iArea1000').
localityOfObject('iExplorer3','iArea1004').
localityOfObject('iExplorer4','iArea1011').
localityOfObject('iExplorer5','iArea1020').
localityOfObject('iExplorer6','iArea1024').
localityOfObject('iExplorer7','iArea1022').
localityOfObject('iExplorer8','iArea1036').
localityOfObject('iDilithiumCrystal756','iArea1000').
localityOfObject('iDilithiumCrystal757','iArea1000').
localityOfObject('iDilithiumCrystal758','iArea1000').
localityOfObject('iTricorder759','iArea1015').
localityOfObject('iTricorder760','iArea1015').
localityOfObject('iTricorder761','iArea1015').

 /* found 32 for localityOfObject(_G1375,_G1376). */ 

/* Prediate:  pddlSomethingIsa/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(50).
number_of_clauses(1).
Pattern: pddlSomethingIsa(_G1422,_G1423). 
 */
pddlSomethingIsa('iGeordi684',[mobGeordi,tAgent,'tMaleAnimal']).
pddlSomethingIsa(iCommanderdata66,['tCommanderdata',tAgent,'tMaleAnimal']).
% pddlSomethingIsa(iExplorer1,[['aOSimClassFn',mudStr("player_osimmarine_mp")],tAgent,'tMaleAnimal']).
pddlSomethingIsa('iWorf720',[mobWorf,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iDrCrusher677',[mobDrCrusher,tAgent,'tFemaleAnimal']).
pddlSomethingIsa('iTroi712',[mobTroi,tAgent,'tFemaleAnimal']).
pddlSomethingIsa('iRiker707',[mobRiker,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iPicard701',[mobPicard,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iGuinan689',[mobGuinan,tAgent,'tFemaleAnimal']).
pddlSomethingIsa('iOBrien696',[mobOBrien,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iWesley716',[mobWesley,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iLivingston726',[mobLivingston,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iSpot727',[mobSpot,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer2',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer3',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer4',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer5',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer6',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer7',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iExplorer8',[mobExplorer,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iAlexander671',[mobAlexander,tAgent,'tMaleAnimal']).
pddlSomethingIsa('iPhaser676',['tPhaser','tHandgun',tWeapon,'tLightingDevice',tCarryAble,'tDeviceSingleUser','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','tHandgun',tWeapon,'tLightingDevice',tCarryAble,'tDeviceSingleUser','tWearAble']).
pddlSomethingIsa('iPhaser700',['tPhaser','tHandgun',tWeapon,'tLightingDevice',tCarryAble,'tDeviceSingleUser','tWearAble']).
pddlSomethingIsa('iPhaser724',['tPhaser','tHandgun',tWeapon,'tLightingDevice',tCarryAble,'tDeviceSingleUser','tWearAble']).
pddlSomethingIsa('iPhaserRifle705',['tPhaserRifle',tWeapon,'tLightingDevice',tCarryAble,'tDeviceSingleUser','tWearAble']).
pddlSomethingIsa('iRedUniform704',['tRedUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iRedUniform710',['tRedUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iRedUniform719',['tRedUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iRedUniform739',['tRedUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iRedUniform743',['tRedUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform687',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform699',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform723',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform731',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iGoldUniform735',['tGoldUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBlueUniform680',['tBlueUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBlueUniform715',['tBlueUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBlueUniform747',['tBlueUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBlueUniform751',['tBlueUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBlueUniform755',['tBlueUniform',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots673',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots773',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots678',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots685',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots697',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots702',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots708',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots713',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots717',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots721',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots729',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots733',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots737',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots741',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots745',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots749',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iBoots753',[tOntoFeet,tClothing,tBoots,tCarryAble,'tWearAble']).
pddlSomethingIsa('iCommBadge674',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge774',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge679',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge686',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge698',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge703',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge709',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge714',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge718',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge722',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge730',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge734',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge738',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge742',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge746',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge750',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iCommBadge754',['tCommBadge',tClothing,tCarryAble,tOntoNeck]).
pddlSomethingIsa('iSash725',['tItemSash',tClothing,tCarryAble,'tWearAble']).
pddlSomethingIsa('iVISOR688',['tItemVISOR',tClothing,tCarryAble]).
pddlSomethingIsa('iMedicalTricorder681',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iMedicalTricorder682',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iMedicalTricorder683',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iTricorder759',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iTricorder760',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iTricorder761',['tTricorderMedical','tRodShapedObject',tContolDevice,tPortableDevice,tCarryAble]).
pddlSomethingIsa('iDilithiumCrystal756',['tItemDilithiumCrystal','tLightingDevice',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iDilithiumCrystal757',['tItemDilithiumCrystal','tLightingDevice',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iDilithiumCrystal758',['tItemDilithiumCrystal','tLightingDevice',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iPicardsFlute',['tPicardsFlute','tArtifactGeneric','tInformationStore',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iTrombone711',['tItemTrombone','tArtifactGeneric','tInformationStore',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iTea690',['tItemTea','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iSynthehol691',['tItemSynthehol','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal','tLimitedAccess',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iFerengiAle692',['tItemFerengiAle','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal','tLimitedAccess',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iRomulanWhisky693',['tItemRomulanWhisky','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal','tLimitedAccess',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iLemonadePruneJuice694',['tItemLemonadePruneJuice','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal','tLimitedAccess',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iVulcanBeer695',['tItemVulcanBeer','tFlaskLabGlassware',tBottle,'tFluidReservoir',tContainer,'tPortal','tLimitedAccess',tCarryAble,tPortableDevice]).
pddlSomethingIsa('iArea1000',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1002',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1001',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1005',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1003',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1004',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1008',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1006',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1042',[tRegion,'tFreeSpaceContext']).
pddlSomethingIsa('iArea1007',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1010',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1009',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1011',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1013',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1032',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1012',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1016',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1014',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1015',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1019',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1017',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1018',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1021',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1020',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1022',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1024',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1039',[tRegion,'tFreeSpaceContent']).
pddlSomethingIsa('iArea1023',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1027',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1025',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1026',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1030',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1028',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1029',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1031',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1033',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1034',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1036',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1035',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1038',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1037',[tRegion,'tIndoorsIsolatedFromOutside','tSpaceInAHOC']).
pddlSomethingIsa('iArea1040',[tRegion,'tFreeSpaceContent']).
pddlSomethingIsa('iArea1041',[tRegion,'tFreeSpaceContent']).
pddlSomethingIsa('iShipMap123',[tItem]).
localityOfObject('iShipMap123','iArea1000').
instVerbOverride('iShipMap123',actExamine('iShipMap123'),
  actProlog(fmt("
STARSHIP ENTERPRISE

             Turbolift               |                Ten Forward
               1010                  |                  1021
                 |                   |                     |
      School8Holodeck 2    |     Crusher's1019Security
       7      |       8        |   Quarters 1018     |       1020
                 |                   |                     |
        Brig5Transporter    |     Sick Bay1016Holodeck 4
        4     |     Room 6     |      1015          |        1017
                 |                   |                    |
    Geordi's2Data's        |        Cargo1013Riker's
 Quarters 1   |  Quarters 3    |       Bay 1012     |    Quarters 1014
                 |                   |                    |
            Engineering              |                Turbolift
               0                  |                  1011
                                     |
              DECK 1                 |                DECK 2
+
                                     |
             Turbolift               |      ReadyBridge
               1032                  |     Room 1035    1038         
                 |                   |                    |
   Science1030Cargo        |                 BridgeConference
   Lab 1029      |     Bay 1031      |                  1036       Room 1037
                 |                   |                    |
       Gym1027Picard's     |                Turbolift   
      1026       |   Quarters 1028   |                  1034   
                 |                   |                    |
    Troi's1024Worf's       |                Turbolift   
 Quarters 1023   |   Quarters 1025   |                  1033      
                 |                   |                
             Shuttle Bay             |                    
               1022                  |
                                     |
              DECK 3                 |                DECK 4


"))).
%pddlSomethingIsa('iArea1004075',[tRegion]).

 /* found 141 for pddlSomethingIsa(_G1422,_G1423). */ 

/* Prediate:  nameString/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(52).
number_of_clauses(2).
Pattern: nameString(_G1446,_G1447). 
 */
nameString('iArea1000',"Main Engineering").
nameString('iArea1001',"Geordi's Quarters").
nameString('iArea1002',"A Corridor").
nameString('iArea1003',"Data's Quarters").
nameString('iArea1004',"The Brig").
nameString('iArea1005',"A Corridor").
nameString('iArea1006',"Transporter Room").
nameString('iArea1007',"School").
nameString('iArea1008',"A Corridor").
nameString('iArea1009',"Holodeck 2").
nameString('iArea1010',"Turbolift").
nameString('iArea1011',"Turbolift").
nameString('iArea1012',"Cargo Bay 1").
nameString('iArea1013',"A Corridor").
nameString('iArea1014',"Riker's Quarters").
nameString('iArea1015',"Sick Bay").
nameString('iArea1016',"A Corridor").
nameString('iArea1017',"Holodeck 4 Entrance  A Narrow Alley").
nameString('iArea1018',"Crusher's Quarters").
nameString('iArea1019',"A Corridor").
nameString('iArea1020',"Enterprise Security").
nameString('iArea1021',"Ten Forward").
nameString('iArea1022',"Shuttle Bay").
nameString('iArea1023',"Troi's Quarters").
nameString('iArea1024',"A Corridor").
nameString('iArea1025',"Worf's Quarters").
nameString('iArea1026',"Enterprise Gym").
nameString('iArea1027',"A Corridor").
nameString('iArea1028',"Picard's Quarters").
nameString('iArea1029',"Science Lab").
nameString('iArea1030',"A Corridor").
nameString('iArea1031',"Cargo Bay 2").
nameString('iArea1032',"Turbolift").
nameString('iArea1033',"Turbolift").
nameString('iArea1034',"Turbolift").
nameString('iArea1035',"Picard's Ready Room").
nameString('iArea1036',"Main Bridge  Upper Half").
nameString('iArea1037',"Conference Room").
nameString('iArea1038',"Main Bridge  Lower Half").
nameString('iArea1039',"Outer Space by the Enterprise").
nameString('iArea1040',"Outer Space").
nameString('iArea1041',"Outer Space").
nameString('iArea1042',"Transporter Beam").
nameString('iAlexander671',"Alexander").
nameString(iCommanderdata66,"Data").
nameString(iCommanderdata66,"CycLBot").
nameString(iCommanderdata66,"CycBot").
nameString(iCommanderdata66,"CycBot1").
nameString('iBoots673',"Boots").
nameString('iCommBadge674',"Comm Badge").
nameString('iGoldUniform675',"Gold Uniform").
nameString('iPhaser676',"Phaser").
nameString(iExplorer1,"Player").
nameString('iBoots773',"Boots").
nameString('iCommBadge774',"Comm Badge").
nameString('iGoldUniform775',"Gold Uniform").
nameString('iPhaser776',"Phaser").
nameString('iDrCrusher677',"Dr. Crusher").
nameString('iBoots678',"Boots").
nameString('iCommBadge679',"Comm Badge").
nameString('iBlueUniform680',"Blue Uniform").
nameString('iMedicalTricorder681',"Medical Tricorder").
nameString('iMedicalTricorder682',"Medical Tricorder").
nameString('iMedicalTricorder683',"Medical Tricorder").
nameString('iGeordi684',"Geordi").
nameString('iBoots685',"Boots").
nameString('iCommBadge686',"Comm Badge").
nameString('iGoldUniform687',"Gold Uniform").
nameString('iVISOR688',"VISOR").
nameString('iGuinan689',"Guinan").
nameString('iTea690',"Tea").
nameString('iSynthehol691',"Synthehol").
nameString('iFerengiAle692',"Ferengi Ale").
nameString('iRomulanWhisky693',"Romulan Whisky").
nameString('iLemonadePruneJuice694',"Lemonade 'tPrune Juice'").
nameString('iVulcanBeer695',"Vulcan Beer").
nameString('iOBrien696',"O'tBrien").
nameString('iBoots697',"Boots").
nameString('iCommBadge698',"Comm Badge").
nameString('iGoldUniform699',"Gold Uniform").
nameString('iPhaser700',"Phaser").
nameString('iPicard701',"Picard").
nameString('iBoots702',"Boots").
nameString('iCommBadge703',"Comm Badge").
nameString('iRedUniform704',"Red Uniform").
nameString('iPhaserRifle705',"5 Phaser Rifle").
nameString('iPicardsFlute',"5 Picard's Flute").
nameString('iRiker707',"Riker").
nameString('iBoots708',"Boots").
nameString('iCommBadge709',"Comm Badge").
nameString('iRedUniform710',"Red Uniform").
nameString('iTrombone711',"Trombone").
nameString('iTroi712',"Troi").
nameString('iBoots713',"Boots").
nameString('iCommBadge714',"Comm Badge").
nameString('iBlueUniform715',"Blue Uniform").
nameString('iWesley716',"Wesley").
nameString('iBoots717',"Boots").
nameString('iCommBadge718',"Comm Badge").
nameString('iRedUniform719',"Red Uniform").
nameString('iWorf720',"Worf").
nameString('iBoots721',"Boots").
nameString('iCommBadge722',"Comm Badge").
nameString('iGoldUniform723',"Gold Uniform").
nameString('iPhaser724',"Phaser").
nameString('iSash725',"Sash").
nameString('iLivingston726',"Livingston").
nameString('iSpot727',"Spot").
nameString('iExplorer2',"Ensign").
nameString('iBoots729',"Boots").
nameString('iCommBadge730',"Comm Badge").
nameString('iGoldUniform731',"Gold Uniform").
nameString('iExplorer3',"Ensign").
nameString('iBoots733',"Boots").
nameString('iCommBadge734',"Comm Badge").
nameString('iGoldUniform735',"Gold Uniform").
nameString('iExplorer4',"Ensign").
nameString('iBoots737',"Boots").
nameString('iCommBadge738',"Comm Badge").
nameString('iRedUniform739',"Red Uniform").
nameString('iExplorer5',"Ensign").
nameString('iBoots741',"Boots").
nameString('iCommBadge742',"Comm Badge").
nameString('iRedUniform743',"Red Uniform").
nameString('iExplorer6',"Ensign").
nameString('iBoots745',"Boots").
nameString('iCommBadge746',"Comm Badge").
nameString('iBlueUniform747',"Blue Uniform").
nameString('iExplorer7',"Ensign").
nameString('iBoots749',"Boots").
nameString('iCommBadge750',"Comm Badge").
nameString('iBlueUniform751',"Blue Uniform").
nameString('iExplorer8',"Ensign").
nameString('iBoots753',"Boots").
nameString('iCommBadge754',"Comm Badge").
nameString('iBlueUniform755',"Blue Uniform").
nameString('iDilithiumCrystal756',"Dilithium Crystal").
nameString('iDilithiumCrystal757',"Dilithium Crystal").
nameString('iDilithiumCrystal758',"Dilithium Crystal").
nameString('iTricorder759',"Tricorder").
nameString('iTricorder760',"Tricorder").
nameString('iTricorder761',"Tricorder").
nameString('iAlexander671',"alexander rozhenko").
nameString(iCommanderdata66,"Data").
nameString(iExplorer1,"Player").
nameString('iDrCrusher677',"Doctor Crusher").
nameString('iGeordi684',"Geordi LaForge").
nameString('iGuinan689',"Guinan").
nameString('iOBrien696',"Chief O'tBrien").
nameString('iPicard701',"Captain Picard").
nameString('iRiker707',"Commander Riker").
nameString('iTroi712',"Counselor Troi").
nameString('iWesley716',"Wesley").
nameString('iWorf720',"Lieutenant Worf").
nameString('iLivingston726',"Livingston").
nameString('iSpot727',"Spot").
nameString('iExplorer2',"the ensign").
nameString('iExplorer3',"the ensign").
nameString('iExplorer4',"the ensign").
nameString('iExplorer5',"the ensign").
nameString('iExplorer6',"the ensign").
nameString('iExplorer7',"the ensign").
nameString('iExplorer8',"the ensign").
nameString('iBoots673',"a pair of Starfleet black boots").
nameString('iCommBadge674',"a Starfleet communication badge").
nameString('iGoldUniform675',"a gold Starfleet engineering uniform").
nameString('iPhaser676',"a standard issue phaser").
nameString('iBoots773',"a pair of Starfleet black boots").
nameString('iCommBadge774',"a Starfleet communication badge").
nameString('iGoldUniform775',"a gold Starfleet engineering uniform").
nameString('iPhaser776',"a standard issue phaser").
nameString('iBoots678',"a pair of Starfleet black boots").
nameString('iCommBadge679',"a Starfleet communication badge").
nameString('iBlueUniform680',"a blue Starfleet medical uniform").
nameString('iMedicalTricorder681',"a medical Tricorder").
nameString('iMedicalTricorder682',"a medical Tricorder").
nameString('iMedicalTricorder683',"a medical Tricorder").
nameString('iBoots685',"a pair of Starfleet black boots").
nameString('iCommBadge686',"a Starfleet communication badge").
nameString('iGoldUniform687',"a gold Starfleet engineering uniform").
nameString('iVISOR688',"Geordi's VISOR").
nameString('iTea690',"a small cup").
nameString('iSynthehol691',"a synthehol").
nameString('iFerengiAle692',"a Ferengi bottle").
nameString('iRomulanWhisky693',"a Romulan bottle").
nameString('iLemonadePruneJuice694',"a small glass").
nameString('iVulcanBeer695',"a Vulcan bottle").
nameString('iBoots697',"a pair of Starfleet black boots").
nameString('iCommBadge698',"a Starfleet communication badge").
nameString('iGoldUniform699',"a gold Starfleet engineering uniform").
nameString('iPhaser700',"a standard issue phaser").
nameString('iBoots702',"a pair of Starfleet black boots").
nameString('iCommBadge703',"a Starfleet communication badge").
nameString('iRedUniform704',"a burgandy Starfleet command uniform").
nameString('iPhaserRifle705',"a phaser rifle").
nameString('iPicardsFlute',"Picard's flute").
nameString('iBoots708',"a pair of Starfleet black boots").
nameString('iCommBadge709',"a Starfleet communication badge").
nameString('iRedUniform710',"a burgandy Starfleet command uniform").
nameString('iTrombone711',"Riker's trombone").
nameString('iBoots713',"a pair of Starfleet black boots").
nameString('iCommBadge714',"a Starfleet communication badge").
nameString('iBlueUniform715',"a blue Starfleet medical uniform").
nameString('iBoots717',"a pair of Starfleet black boots").
nameString('iCommBadge718',"a Starfleet communication badge").
nameString('iRedUniform719',"a burgandy Starfleet command uniform").
nameString('iBoots721',"a pair of Starfleet black boots").
nameString('iCommBadge722',"a Starfleet communication badge").
nameString('iGoldUniform723',"a gold Starfleet engineering uniform").
nameString('iPhaser724',"a standard issue phaser").
nameString('iSash725',"Worf's sash").
nameString('iBoots729',"a pair of Starfleet black boots").
nameString('iCommBadge730',"a Starfleet communication badge").
nameString('iGoldUniform731',"a gold Starfleet engineering uniform").
nameString('iBoots733',"a pair of Starfleet black boots").
nameString('iCommBadge734',"a Starfleet communication badge").
nameString('iGoldUniform735',"a gold Starfleet engineering uniform").
nameString('iBoots737',"a pair of Starfleet black boots").
nameString('iCommBadge738',"a Starfleet communication badge").
nameString('iRedUniform739',"a burgandy Starfleet command uniform").
nameString('iBoots741',"a pair of Starfleet black boots").
nameString('iCommBadge742',"a Starfleet communication badge").
nameString('iRedUniform743',"a burgandy Starfleet command uniform").
nameString('iBoots745',"a pair of Starfleet black boots").
nameString('iCommBadge746',"a Starfleet communication badge").
nameString('iBlueUniform747',"a blue Starfleet medical uniform").
nameString('iBoots749',"a pair of Starfleet black boots").
nameString('iCommBadge750',"a Starfleet communication badge").
nameString('iBlueUniform751',"a blue Starfleet medical uniform").
nameString('iBoots753',"a pair of Starfleet black boots").
nameString('iCommBadge754',"a Starfleet communication badge").
nameString('iBlueUniform755',"a blue Starfleet medical uniform").
nameString('iDilithiumCrystal756',"a dilithium crystal").
nameString('iDilithiumCrystal757',"a dilithium crystal").
nameString('iDilithiumCrystal758',"a dilithium crystal").
nameString('iTricorder759',"a medical Tricorder").
nameString('iTricorder760',"a medical Tricorder").
nameString('iTricorder761',"a medical Tricorder").


%:- break.

 /* found 322 for nameString(_G1446,_G1447). */ 

pddlDescription(Obj,List) ==>({member(E,List)},mudDescription(Obj,E)).

/* Prediate:  pddlDescription/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(33).
number_of_clauses(1).
Pattern: pddlDescription(_G1470,_G1471). 
 */
pddlDescription('iGeordi684',["Lieutenant","Commander","Geordi","LaForge","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Geordi","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things"]).
pddlDescription(iCommanderdata66,["Lieutenant","Commander","Data","Android","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He mudStowing superhuman strength, and is extremely tough","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000","#$PunchingSomething mudBareHandDamage: 10d10+75","Data","CycLBot","CycBot","CycBot1","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He mudStowing superhuman strength, and is extremely tough"]).
pddlDescription(iExplorer1,["Lieutenant","Commander","Human","Player",
            "Explorer Player",
            "ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON",
            "NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000",
            "#$PunchingSomething mudBareHandDamage: 10d10+75","Player","Player","Human",
            "Logged on player character"]).
pddlDescription('iWorf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).

pddlDescription('iDrCrusher677',["Doctor","Beverly","Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Dr. Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard"]).
pddlDescription('iTroi712',["Counselor","Deanna","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds"]).
pddlDescription('iRiker707',["Commander","William","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in mudEnergy of keeping the crew in line","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+52","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in mudEnergy of keeping the crew in line"]).
pddlDescription('iPicard701',["Captain","Jean","Luc","JeanLuc","Picard","Captain Picard","Captain JeanLuc Picard is standing here, watching you","Captain Picard is a very important man","He's in mudEnergy of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_SANCTUARY","NPC_NOTRACK","+mudToHitArmorClass0: 0","mudMaxHitPoints: 20d20+5000","#$PunchingSomething mudBareHandDamage: 12d12+75","Picard","Captain Picard","Captain JeanLuc Picard is standing here, watching you","Captain Picard is a very important man","He's in mudEnergy of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!"]).
pddlDescription('iGuinan689',["Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender"]).
pddlDescription('iOBrien696',["Chief","O'tBrien","Transporter","Chief O'tBrien","Chief O'tBrien is here, waiting to teleport you somwhere","Chief O'tBrien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","O'tBrien","Chief O'tBrien","Chief O'tBrien is here, waiting to teleport you somwhere","Chief O'tBrien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms"]).
pddlDescription('iWesley716',["Wesley","Crusher","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father","ACT_STAY_ZONE","ACT_WIMPY","wimpy mobile will try to flee when it gets low on hit points. A mobile which is both aggressive and wimpy will not attack a player that is awake","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+1400","#$PunchingSomething mudBareHandDamage: 9d9+24","Wesley","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father"]).
pddlDescription('iLivingston726',["Livingston","fish","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Livingston","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive"]).
pddlDescription('iSpot727',["spot","the","cat","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Spot","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal"]).
pddlDescription('iExplorer2',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer3',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer4',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer5',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer6',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer7',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iExplorer8',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
pddlDescription('iAlexander671',["alexander","rozhenko","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Alexander","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong"]).
pddlDescription('iPhaser676',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent mudHealth for its fairly small size","Phaser","a standard issue phaser"]).
pddlDescription('iPhaser776',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent mudHealth for its fairly small size","Phaser","a standard issue phaser"]).
pddlDescription('iPhaser700',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent mudHealth for its fairly small size","Phaser","a standard issue phaser"]).
pddlDescription('iPhaser724',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent mudHealth for its fairly small size","Phaser","a standard issue phaser"]).
pddlDescription('iPhaserRifle705',["phaser","rifle","a phaser rifle","A large phaser rifle is lying here","damageNumberDice 7","damageSizeDice 6","WeaponBlasting","This phaser rifle looks pretty powerful. These weapons are used mainly on assault tCol missions, where power is important","5 Phaser Rifle","a phaser rifle"]).
pddlDescription('iRedUniform704',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","mudArmorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
pddlDescription('iRedUniform710',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","mudArmorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
pddlDescription('iRedUniform719',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","mudArmorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
pddlDescription('iRedUniform739',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","mudArmorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
pddlDescription('iRedUniform743',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","mudArmorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
pddlDescription('iGoldUniform675',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform775',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform687',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform699',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform723',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform731',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iGoldUniform735',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","mudArmorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
pddlDescription('iBlueUniform680',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","mudArmorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
pddlDescription('iBlueUniform715',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","mudArmorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
pddlDescription('iBlueUniform747',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","mudArmorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
pddlDescription('iBlueUniform751',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","mudArmorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
pddlDescription('iBlueUniform755',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","mudArmorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
pddlDescription('iBoots673',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots773',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots678',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots685',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots697',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots702',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots708',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots713',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots717',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots721',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots729',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots733',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots737',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots741',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots745',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots749',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iBoots753',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","mudArmorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
pddlDescription('iCommBadge674',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge774',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge679',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge686',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1",
                     "These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge698',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge703',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge709',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge714',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge718',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge722',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge730',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge734',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge738',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge742',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge746',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge750',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iCommBadge754',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","mudArmorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
pddlDescription('iSash725',["worf's","worf","sash","Worf's sash","Worf's silver chain sash has been left here","mudArmorLevel: 8","Worf's sash is some sort of Klingon clothing. Worf always wears it, which makes you wonder how you managed to get a hold of it..","Sash","Worf's sash"]).
pddlDescription('iVISOR688',["geordi","geordi's","visor","Geordi's VISOR","Geordi's VISOR is lying here","mudArmorLevel: 2","Geordi's VISOR was made specially for him, because he's blind. This piece of equipment allows him to see things, but differently than normal eyes. I wonder how Geordi is managing, now that you've stolen his only way of seeing?","VISOR","Geordi's VISOR"]).
pddlDescription('iMedicalTricorder681',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","mudEnergy: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
pddlDescription('iMedicalTricorder682',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","mudEnergy: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
pddlDescription('iMedicalTricorder683',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","mudEnergy: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
pddlDescription('iTricorder759',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","mudEnergy: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
pddlDescription('iTricorder760',["medical","tricorder",
                     "a medical Tricorder","A medical Tricorder is lying here, ready to be used",
                     "mudLevelOf: 10",
                     "chargeCapacity: 5",
                     "mudEnergy: 5",
                     "This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it",
                     "Tricorder","a medical Tricorder"]).
pddlDescription('iTricorder761',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","mudEnergy: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
pddlDescription('iDilithiumCrystal756',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blueish tinge","Dilithium Crystal","a dilithium crystal"]).
pddlDescription('iDilithiumCrystal757',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blueish tinge","Dilithium Crystal","a dilithium crystal"]).
pddlDescription('iDilithiumCrystal758',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blueish tinge","Dilithium Crystal","a dilithium crystal"]).
pddlDescription('iPicardsFlute',["picard","picard's","flute","Picard's flute","Captain Picard's wooden flute is sitting here","Captain Picard recieved this flute when he lost his mudMemory and was stuck on some strange world. Now, he plays it to relieve stress","5 Picard's Flute","Picard's flute"]).
pddlDescription('iTrombone711',["riker","riker's","trombone","Riker's trombone","Commander Riker's trombone has been placed here","Commander Riker considers himself to be a talented jazz musician. He practices on this trombone all the time","Trombone","Riker's trombone"]).
pddlDescription('iTea690',["tea","cup","a small cup","A small cup of tea is sitting here","Tea","a small cup"]).
pddlDescription('iSynthehol691',["wine","bottle","synthehol","a synthehol","A bottle of synthehol is standing here","Synthehol","a synthehol"]).
pddlDescription('iFerengiAle692',["ale","ferengi","bottle","a Ferengi bottle","A bottle of Ferengi ale is sitting here","Ferengi Ale","a Ferengi bottle"]).
pddlDescription('iRomulanWhisky693',["whisky","whiskey","romulan","bottle","a Romulan bottle","A bottle of Romulan whiskey is sitting here","Romulan Whisky","a Romulan bottle"]).
pddlDescription('iLemonadePruneJuice694',["lemonade","prune","juice","glass","a small glass","A small glass of prune juice is sitting here","Lemonade 'tPrune Juice'","a small glass"]).
pddlDescription('iVulcanBeer695',["beer","vulcan","bottle","a Vulcan bottle","A bottle of Vulcan beer is standing here","Vulcan Beer","a Vulcan bottle"]).
pddlDescription('iArea1000',["Main Engineering","You find yourself in the middle of main engineering","The room is longer than it is wide, and it has fairly low ceilings","Computer terminals cover all the walls, and a large table built into the floor sits in the middle of the room","At the far end of the room you see the warp core, a large pulsating vertical tube"]).
pddlDescription('iArea1002',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
pddlDescription('iArea1001',["Geordi's Quarters","You're in the middle of Geordi's quarters","The room is sparsely decorated, due to the fact that Geordi is blind","A small personal computer sits on a desk against the western wall, in between two windows that look out into space","A neatly made bed has been placed against the northern wall"]).
pddlDescription('iArea1005',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
pddlDescription('iArea1003',["Data's Quarters","You're in the middle of Data's quarters","Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall","In front of the screen is a large desk, which is covered in computer controls","You can't see a bed in this room, but you figure it's because Data doesn't sleep"]).
pddlDescription('iArea1004',["The Brig","You're in the dimly lit Brig","This is where all the criminals and prisoners are kept while on board the Enterprise","Three fairly large cells can been seen in the southern part of the room, and they're all empty","A computer control panel is situated in the northwestern corner of the room, which is where the force fields for the cells are controlled",'tThe panel says:

***************************************************
*                                                 *
*            NCC1701D  ENTERPRISE              *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 1            *
*                                                 *
***************************************************
']).
pddlDescription('iArea1008',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
pddlDescription('iArea1006',["Transporter Room","You're in the Enterprise transporter room","A computer terminal is sitting near the southern wall, where the transporter chief can control the transporters","Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall"]).
pddlDescription('iArea1042',["Transporter Beam","You find yourself in a transporter beam","All you can see is blue flashing light","It feels as though your body is racing around at high speeds","As you try to look down at your body, you realize that there's nothing there!"]).
pddlDescription('iArea1007',["School","You step through the doors and find yourself in a large school room","Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls","Several computer consoles with a children's interface on them can be seen on the tables"]).
pddlDescription('iArea1010',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
pddlDescription('iArea1009',["Holodeck 2","You're now on Holodeck 2","The room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling","This is where different programs can be loaded and experienced, which seem totally real","Right now, this holodeck is not functioning",'
***************************************************
*                                                 *
*            NCC1701D  "ENTERPRISE"            *
*                    HOLODECK 2                   *
*                                                 *
*              STATUS : Inactive                  *
*     CURRENT PROGRAM : N/A                       *
*            SAFETIES : N/A                       *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
***************************************************']).
pddlDescription('iArea1011',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can accessthe other decks on the Enterprise"]).
pddlDescription('iArea1013',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
pddlDescription('iArea1032',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
pddlDescription('iArea1012',["Cargo Bay 1","You're in the main cargo bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
pddlDescription('iArea1016',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
pddlDescription('iArea1014',["Riker's Quarters","You've arrived in Riker's quarters","The room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall","A small partition at the northern part of the room seperates his sleeping area with the ftRest of the room"]).
pddlDescription('iArea1015',["Sick Bay","You're in the middle of the Enterprise's Sick Bay","About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room","A large glass window in the northern part of the room separates the doctor's office with the ftRest of the room",'
***************************************************
*                                                 *
*            NCC1701D  "ENTERPRISE"            *
*                    HOLODECK 4                   *
*                                                 *
*              STATUS : Active                    *
*     CURRENT PROGRAM : Sherlock Holmes (19th     *
*                       century London)           *
*            SAFETIES : Disabled                  *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
*             ENTER WHEN READY              *
*                                                 *
***************************************************
']).
pddlDescription('iArea1019',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
pddlDescription('iArea1017',["Holodeck 4 Entrance  A Narrow Alley","You emerge into a dark narrow alley","Tall dark brick buildings block your way north and south","You can see that the windows on the buildings are fairly high, and some have been boarded up","The smell from the rotting tFood and garbage mixing with the foul water on the ground is unbearable","You can hear the sounds of a bustling marketpace to the east","The archway leading out of the holodeck is west"]).
pddlDescription('iArea1018',["Crusher's Quarters","You're in Doctor Crusher's quarters","Several different paintings are attached to the walls, and you also notice a few sculptures","A neatly made bed is located by the northern wall, in between two large windows looking out into space"]).
pddlDescription('iArea1021',["Ten Forward","You're now in Ten Forward, the entertainment room of the Enterprise","The entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south","Many vRound metal tables are scattered around the room, surrounded by metal chairs","A long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it","It's very noisy in here, due to all the talking and laughing"]).
pddlDescription('iArea1020',["Enterprise Security","You're standing in the dimly lit Enterprise Security","Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons","Three long tables surrounded by chairs stretch across the room"]).
pddlDescription('iArea1022',["Shuttle Bay","You're in the main shuttle bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see three different shuttle crafts sitting here, waiting to be flown","A large grey door leads into space"]).
pddlDescription('iArea1024',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
pddlDescription('iArea1039',["Outer Space by the Enterprise","You're floating in outer space right beside the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold","A large grey door leads into the Enterprise's Shuttle Bay"]).
pddlDescription('iArea1023',["Troi's Quarters","You're in Counselor Deanna Troi's quarters","Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table","A neatly made bed is partially hidden behind a curtain at the northern part of the room"]).
pddlDescription('iArea1027',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape",
"
***************************************************
*                                                 *
*            NCC1701D  ENTERPRISE            *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 3            *
*                                                 *
***************************************************
"]).
pddlDescription('iArea1025',["Worf's Quarters","You're in Worf's quarters","A small table is sitting in the southeastern corner, and on it is a small potted plant","An impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east"]).
pddlDescription('iArea1026',["Enterprise Gym","You emerge into the Enterprise gym","The room is quite large, with a soft grey floor","A set of lockers against the southern wall contain all of the necessary equipment needed for using the gym","A thick stack of mats have been piled high in one corner, which can be used for different activities","Captain Picard likes to come here to practice his fencing"]).
pddlDescription('iArea1030',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
pddlDescription('iArea1028',["Picard's Quarters","You find yourself standing by the door of Captain Picard's quarters","He isn't very fond of visitors, but you decide to stay and have a look around","You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is mudFacing south","A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table","Two large windows offer a great view of space","A small partition at the northern part of the room contains Picard's sleeping area"]).
pddlDescription('iArea1029',["Science Lab","You're in the Enterprise science lab","A strange looking machine sits in the middle of the room, up on a slightly raised platform","It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)","A complex looking computer console is mudFacing this machine","Around the ftRest of the room are counterops with with the odd computer terminal"]).
pddlDescription('iArea1031',["Cargo Bay 2","You're in the cargo bay 2 of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
pddlDescription('iArea1033',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
pddlDescription('iArea1034',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
pddlDescription('iArea1036',["Main Bridge  Upper Half","You find yourself on the upper half of the main bridge of the USS Enterprise","Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship","The entire southern wall is covered with computer consoles, where the ship's main systems are controlled","Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship"]).
pddlDescription('iArea1035',["Picard's Ready Room","You're standing in Captain Picard's ready room","A long couch has been placed beside the door, while a large U shaped desk is located by the northern wall","A small computer screen is sitting on the desk, as well as several other papers and documents","A single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room","This is where the Captain makes all of his important decisions"]).
pddlDescription('iArea1038',["Main Bridge  Lower Half","You find yourself on the lower half of the main bridge of the USS Enterprise","An enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by","Three large chairs in the northern part of the room, in front of the railing, face the screen","This is where the Captain, Commander Riker, and Counselor Troi sit","Two computer consoles with built in chairs ftRest about ten feet in front of the chairs, also mudFacing the view screen","This is where the ship's pilot and information officer sit"]).
pddlDescription('iArea1037',["Conference Room","You're in the conference room of the Enterprise","A large glass rectangular table sits in the middle of the room, surrounded by about a dozen comfortable looking office chairs","The entire eastern wall is covered with windows, looking out into space","This is where the senior officers of the Enterprise meet and discuss important issues"]).
pddlDescription('iArea1040',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).
pddlDescription('iArea1041',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).


 /* found 141 for pddlDescription(_G1470,_G1471). */ 

/* Prediate:  pathDirLeadsTo/3 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(31).
number_of_clauses(1).
Pattern: pathDirLeadsTo(_G1494,_G1495,_G1496). 
 */
pathDirLeadsTo('iArea1000',vNorth,'iArea1002').
pathDirLeadsTo('iArea1001',vEast,'iArea1002').
pathDirLeadsTo('iArea1002',vNorth,'iArea1005').
pathDirLeadsTo('iArea1002',vEast,'iArea1003').
pathDirLeadsTo('iArea1002',vSouth,'iArea1000').
pathDirLeadsTo('iArea1002',vWest,'iArea1001').
pathDirLeadsTo('iArea1003',vWest,'iArea1002').
pathDirLeadsTo('iArea1004',vEast,'iArea1005').
pathDirLeadsTo('iArea1005',vNorth,'iArea1008').
pathDirLeadsTo('iArea1005',vEast,'iArea1006').
pathDirLeadsTo('iArea1005',vSouth,'iArea1002').
pathDirLeadsTo('iArea1005',vWest,'iArea1004').
pathDirLeadsTo('iArea1006',vNorth,'iArea1042').
pathDirLeadsTo('iArea1006',vWest,'iArea1005').
pathDirLeadsTo('iArea1007',vEast,'iArea1008').
pathDirLeadsTo('iArea1008',vNorth,'iArea1010').
pathDirLeadsTo('iArea1008',vEast,'iArea1009').
pathDirLeadsTo('iArea1008',vSouth,'iArea1005').
pathDirLeadsTo('iArea1008',vWest,'iArea1007').
pathDirLeadsTo('iArea1009',vWest,'iArea1008').
pathDirLeadsTo('iArea1010',vSouth,'iArea1008').
pathDirLeadsTo('iArea1010',vUp,'iArea1011').
pathDirLeadsTo('iArea1011',vNorth,'iArea1013').
pathDirLeadsTo('iArea1011',vUp,'iArea1032').
pathDirLeadsTo('iArea1011',vDown,'iArea1010').
pathDirLeadsTo('iArea1012',vEast,'iArea1013').
pathDirLeadsTo('iArea1013',vNorth,'iArea1016').
pathDirLeadsTo('iArea1013',vEast,'iArea1014').
pathDirLeadsTo('iArea1013',vSouth,'iArea1011').
pathDirLeadsTo('iArea1013',vWest,'iArea1012').
pathDirLeadsTo('iArea1014',vWest,'iArea1013').
pathDirLeadsTo('iArea1015',vEast,'iArea1016').
pathDirLeadsTo('iArea1016',vNorth,'iArea1019').
pathDirLeadsTo('iArea1016',vEast,'iArea1017').
pathDirLeadsTo('iArea1016',vSouth,'iArea1013').
pathDirLeadsTo('iArea1016',vWest,'iArea1015').
pathDirLeadsTo('iArea1017',vWest,'iArea1016').
pathDirLeadsTo('iArea1018',vEast,'iArea1019').
pathDirLeadsTo('iArea1019',vNorth,'iArea1021').
pathDirLeadsTo('iArea1019',vEast,'iArea1020').
pathDirLeadsTo('iArea1019',vSouth,'iArea1016').
pathDirLeadsTo('iArea1019',vWest,'iArea1018').
pathDirLeadsTo('iArea1020',vWest,'iArea1019').
pathDirLeadsTo('iArea1021',vSouth,'iArea1019').
pathDirLeadsTo('iArea1022',vNorth,'iArea1024').
pathDirLeadsTo('iArea1022',vSouth,'iArea1039').
pathDirLeadsTo('iArea1023',vEast,'iArea1024').
pathDirLeadsTo('iArea1024',vNorth,'iArea1027').
pathDirLeadsTo('iArea1024',vEast,'iArea1025').
pathDirLeadsTo('iArea1024',vSouth,'iArea1022').
pathDirLeadsTo('iArea1024',vWest,'iArea1023').
pathDirLeadsTo('iArea1025',vWest,'iArea1024').
pathDirLeadsTo('iArea1026',vEast,'iArea1027').
pathDirLeadsTo('iArea1027',vNorth,'iArea1030').
pathDirLeadsTo('iArea1027',vEast,'iArea1028').
pathDirLeadsTo('iArea1027',vSouth,'iArea1024').
pathDirLeadsTo('iArea1027',vWest,'iArea1026').
pathDirLeadsTo('iArea1028',vWest,'iArea1027').
pathDirLeadsTo('iArea1029',vEast,'iArea1030').
pathDirLeadsTo('iArea1030',vNorth,'iArea1032').
pathDirLeadsTo('iArea1030',vEast,'iArea1031').
pathDirLeadsTo('iArea1030',vSouth,'iArea1027').
pathDirLeadsTo('iArea1030',vWest,'iArea1029').
pathDirLeadsTo('iArea1031',vWest,'iArea1030').
pathDirLeadsTo('iArea1032',vSouth,'iArea1030').
pathDirLeadsTo('iArea1032',vUp,'iArea1033').
pathDirLeadsTo('iArea1032',vDown,'iArea1011').
pathDirLeadsTo('iArea1033',vUp,'iArea1034').
pathDirLeadsTo('iArea1033',vDown,'iArea1032').
pathDirLeadsTo('iArea1034',vUp,'iArea1036').
pathDirLeadsTo('iArea1034',vDown,'iArea1033').
pathDirLeadsTo('iArea1035',vEast,'iArea1038').
pathDirLeadsTo('iArea1036',vNorth,'iArea1038').
pathDirLeadsTo('iArea1036',vEast,'iArea1037').
pathDirLeadsTo('iArea1036',vDown,'iArea1034').
pathDirLeadsTo('iArea1037',vWest,'iArea1036').
pathDirLeadsTo('iArea1038',vSouth,'iArea1036').
pathDirLeadsTo('iArea1038',vWest,'iArea1035').
pathDirLeadsTo('iArea1039',vNorth,'iArea1022').
pathDirLeadsTo('iArea1039',vUp,'iArea1040').
pathDirLeadsTo('iArea1040',vUp,'iArea1041').
pathDirLeadsTo('iArea1040',vDown,'iArea1039').
pathDirLeadsTo('iArea1041',vUp,'iArea1041').
pathDirLeadsTo('iArea1041',vDown,'iArea1040').
% pathDirLeadsTo('iArea1042',vNorth,'iArea1004075').
pathDirLeadsTo('iArea1042',vSouth,'iArea1006').

 /* found 86 for pathDirLeadsTo(_G1494,_G1495,_G1496). */ 

/* Prediate:  pathName/3 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(32).
number_of_clauses(1).
Pattern: pathName(_G1519,_G1520,_G1521). 
 */
pathName('iArea1000',vNorth,"A corridor is North").
pathName('iArea1001',vEast,"A corridor is East").
pathName('iArea1002',vNorth,"The corridor continues North").
pathName('iArea1002',vEast,"Data's Quarters are East").
pathName('iArea1002',vSouth,"Main Engineering is South").
pathName('iArea1002',vWest,"Geordi's Quarters are West").
pathName('iArea1003',vWest,"A corridor is West").
pathName('iArea1004',vEast,"A corridor is East").
pathName('iArea1005',vNorth,"The corridor continues North").
pathName('iArea1005',vEast,"The Transporter Room is East").
pathName('iArea1005',vSouth,"The corridor continues South").
pathName('iArea1005',vWest,"The Brig is West").
pathName('iArea1006',vNorth,"A transporter beam is North").
pathName('iArea1006',vWest,"A corridor is West").
pathName('iArea1007',vEast,"A corridor is East").
pathName('iArea1008',vNorth,"The turbolift is North").
pathName('iArea1008',vEast,"Holodeck 2 is East").
pathName('iArea1008',vSouth,"The corridor continues South").
pathName('iArea1008',vWest,"The School is West").
pathName('iArea1009',vWest,"A corridor is West").
pathName('iArea1010',vSouth,"A corridor leads South").
pathName('iArea1010',vUp,"The turbolift goes Up").
pathName('iArea1011',vNorth,"A corridor leads North").
pathName('iArea1011',vUp,"The turbolift goes Up").
pathName('iArea1011',vDown,"The turbolift goes Down").
pathName('iArea1012',vEast,"A corridor is East").
pathName('iArea1013',vNorth,"The corridor continues North").
pathName('iArea1013',vEast,"Riker's Quarters are East").
pathName('iArea1013',vSouth,"The Turbolift is South").
pathName('iArea1013',vWest,"The Cargo Bay is West").
pathName('iArea1014',vWest,"A corridor is West").
pathName('iArea1015',vEast,"A corridor is East").
pathName('iArea1016',vNorth,"The corridor continues North").
pathName('iArea1016',vEast,"Holodeck 4 is East").
pathName('iArea1016',vSouth,"The corridor continues South").
pathName('iArea1016',vWest,"Sick Bay is West").
pathName('iArea1017',vWest,"A corridor is West").
pathName('iArea1018',vEast,"A corridor is East").
pathName('iArea1019',vNorth,"Ten Forward is North").
pathName('iArea1019',vEast,"Security is East").
pathName('iArea1019',vSouth,"The corridor continues South").
pathName('iArea1019',vWest,"Crusher's Quarters are West").
pathName('iArea1020',vWest,"A corridor is West").
pathName('iArea1021',vSouth,"A corridor is South").
pathName('iArea1022',vNorth,"A corridor is North").
pathName('iArea1023',vEast,"A corridor is East").
pathName('iArea1024',vNorth,"The corridor continues North").
pathName('iArea1024',vEast,"Worf's Quarters are East").
pathName('iArea1024',vSouth,"The shuttle bay is South").
pathName('iArea1024',vWest,"Troi's Quarters are West").
pathName('iArea1025',vWest,"A corridor is West").
pathName('iArea1026',vEast,"A corridor is East").
pathName('iArea1027',vNorth,"The corridor continues North").
pathName('iArea1027',vEast,"Picard's Quarters are East").
pathName('iArea1027',vSouth,"The corridor continues South").
pathName('iArea1027',vWest,"The Enterprise Gym is West").
pathName('iArea1028',vWest,"A corridor is West").
pathName('iArea1029',vEast,"A corridor is East").
pathName('iArea1030',vNorth,"The Turbolift is North").
pathName('iArea1030',vEast,"Empty Quarters are East").
pathName('iArea1030',vSouth,"The corridor continues South").
pathName('iArea1030',vWest,"Empty Quarters are West").
pathName('iArea1031',vWest,"A corridor is West").
pathName('iArea1032',vSouth,"A corridor leads South").
pathName('iArea1032',vUp,"The turbolift goes Up").
pathName('iArea1032',vDown,"The turbolift goes Down").
pathName('iArea1033',vUp,"The turbolift goes Up").
pathName('iArea1033',vDown,"The turbolift goes Down").
pathName('iArea1034',vUp,"The Main Bridge is Up").
pathName('iArea1034',vDown,"The turbolift goes Down").
pathName('iArea1035',vEast,"The Main Bridge  Lower Half is East").
pathName('iArea1036',vNorth,"The Main Bridge  Lower Half is North").
pathName('iArea1036',vEast,"The Conference Room is East").
pathName('iArea1036',vDown,"The Turbolift is Down").
pathName('iArea1037',vWest,"The Main Bridge  Upper Half is West").
pathName('iArea1038',vSouth,"The Main Bridge  Upper Half is South").
pathName('iArea1038',vWest,"The Captain's Ready Room is West").
pathName('iArea1039',vNorth,"The Shuttle Bay is North").
pathName('iArea1039',vUp,"Outer Space is Up").
pathName('iArea1040',vUp,"Outer Space is Up").
pathName('iArea1040',vDown,"Outer Space is Down").
pathName('iArea1041',vUp,"* The Galaxy is Up").
pathName('iArea1041',vDown,"Outer Space is Down").
pathName('iArea1042',vSouth,"The Transporter Room is South").

 /* found 84 for pathName(_G1519,_G1520,_G1521). */ 
/*
pddlSomethingIsa0(X,Y):pddlSomethingIsa(X,List),member(Y,List).


domain_name(startrek).

pddlSorts(non_primitive_sorts, [
       tRegion, city_location,tcentre,not_tcentre, route, 
       physical_obj, vehicle, railv, actorNPC, vtDirection]).
pddlSorts(primitive_sorts, [
        airport, aircraft, train_station, post_office, clocation, city, package, 
        train, traincar, truck, road_route, rail_route, tRegion,tAgent]).

pddlSorts(physical_obj, [vehicle, package,tCarryAble,tAgent]).
pddlSorts(tCarryAble,['tWearAble',tWeapon,tContolDevice,tClothing,'tFluidReservoir']).
pddlSorts(actorNPC, [tAgent]).
*/


% pddlObjects(SW,List):member(SW,[tWeapon,tContolDevice,tClothing,'tFluidReservoir',tRegion,tCarryAble,'tWearAble',tAgent,'tWearAble']),findall(O,pddlSomethingIsa0(O,SW),ULIST),sort(ULIST,List).
%SLOW 
pddlObjects(tWeapon,['iPhaser676','iPhaser700','iPhaser724','iPhaser776','iPhaserRifle705']).
pddlObjects(tContolDevice,['iMedicalTricorder681','iMedicalTricorder682','iMedicalTricorder683','iTricorder759','iTricorder760','iTricorder761']).
pddlObjects(tClothing,['iRedUniform704','iRedUniform710','iRedUniform719','iRedUniform739','iRedUniform743','iGoldUniform675','iGoldUniform687','iGoldUniform699','iGoldUniform723','iGoldUniform731','iGoldUniform735','iGoldUniform775','iBlueUniform680','iBlueUniform715','iBlueUniform747','iBlueUniform751','iBlueUniform755','iBoots673','iBoots678','iBoots685','iBoots697','iBoots702','iBoots708','iBoots713','iBoots717','iBoots721','iBoots729','iBoots733','iBoots737','iBoots741','iBoots745','iBoots749','iBoots753','iBoots773','iCommBadge674','iCommBadge679','iCommBadge686','iCommBadge698','iCommBadge703','iCommBadge709','iCommBadge714','iCommBadge718','iCommBadge722','iCommBadge730','iCommBadge734','iCommBadge738','iCommBadge742','iCommBadge746','iCommBadge750','iCommBadge754','iCommBadge774','iSash725','iVISOR688']).
pddlObjects('tFluidReservoir',['iTea690','iSynthehol691','iFerengiAle692','iRomulanWhisky693','iLemonadePruneJuice694','iVulcanBeer695']).
pddlObjects(tRegion,['iArea1000','iArea1001','iArea1002','iArea1003','iArea1004','iArea1005','iArea1006','iArea1007','iArea1008','iArea1009','iArea1010','iArea1011','iArea1012','iArea1013','iArea1014','iArea1015','iArea1016','iArea1017','iArea1018','iArea1019','iArea1020','iArea1021','iArea1022','iArea1023','iArea1024','iArea1025','iArea1026','iArea1027','iArea1028','iArea1029','iArea1030','iArea1031','iArea1032','iArea1033','iArea1034','iArea1035','iArea1036','iArea1037','iArea1038','iArea1039','iArea1040','iArea1041','iArea1042'
% ,'iArea1004075'
]).
pddlObjects(tAgent,['iGeordi684','iWorf720','iDrCrusher677','iTroi712','iRiker707','iPicard701','iGuinan689','iOBrien696','iWesley716','iLivingston726','iSpot727','iExplorer2','iExplorer3','iExplorer4','iExplorer5','iExplorer6','iExplorer7','iExplorer8','iAlexander671',iCommanderdata66,iExplorer1]).

%%pddlObjects(tPathway,List):findall(apath(A,B),pathDirLeadsTo(A,B,C),List).
%pddlObjects(tPathway,[apathFn('iArea1000',vNorth),apathFn('iArea1001',vEast),apathFn('iArea1002',vNorth),apathFn('iArea1002',vEast),apathFn('iArea1002',vSouth),apathFn('iArea1002',vWest),apathFn('iArea1003',vWest),apathFn('iArea1004',vEast),apathFn('iArea1005',vNorth),apathFn('iArea1005',vEast),apathFn('iArea1005',vSouth),apathFn('iArea1005',vWest),apathFn('iArea1006',vNorth),apathFn('iArea1006',vWest),apathFn('iArea1007',vEast),apathFn('iArea1008',vNorth),apathFn('iArea1008',vEast),apathFn('iArea1008',vSouth),apathFn('iArea1008',vWest),apathFn('iArea1009',vWest),apathFn('iArea1010',vSouth),apathFn('iArea1010',vUp),apathFn('iArea1011',vNorth),apathFn('iArea1011',vUp),apathFn('iArea1011',vDown),apathFn('iArea1012',vEast),apathFn('iArea1013',vNorth),apathFn('iArea1013',vEast),apathFn('iArea1013',vSouth),apathFn('iArea1013',vWest),apathFn('iArea1014',vWest),apathFn('iArea1015',vEast),apathFn('iArea1016',vNorth),apathFn('iArea1016',vEast),apathFn('iArea1016',vSouth),apathFn('iArea1016',vWest),apathFn('iArea1017',vWest),apathFn('iArea1018',vEast),apathFn('iArea1019',vNorth),apathFn('iArea1019',vEast),apathFn('iArea1019',vSouth),apathFn('iArea1019',vWest),apathFn('iArea1020',vWest),apathFn('iArea1021',vSouth),apathFn('iArea1022',vNorth),apathFn('iArea1022',vSouth),apathFn('iArea1023',vEast),apathFn('iArea1024',vNorth),apathFn('iArea1024',vEast),apathFn('iArea1024',vSouth),apathFn('iArea1024',vWest),apathFn('iArea1025',vWest),apathFn('iArea1026',vEast),apathFn('iArea1027',vNorth),apathFn('iArea1027',vEast),apathFn('iArea1027',vSouth),apathFn('iArea1027',vWest),apathFn('iArea1028',vWest),apathFn('iArea1029',vEast),apathFn('iArea1030',vNorth),apathFn('iArea1030',vEast),apathFn('iArea1030',vSouth),apathFn('iArea1030',vWest),apathFn('iArea1031',vWest),apathFn('iArea1032',vSouth),apathFn('iArea1032',vUp),apathFn('iArea1032',vDown),apathFn('iArea1033',vUp),apathFn('iArea1033',vDown),apathFn('iArea1034',vUp),apathFn('iArea1034',vDown),apathFn('iArea1035',vEast),apathFn('iArea1036',vNorth),apathFn('iArea1036',vEast),apathFn('iArea1036',vDown),apathFn('iArea1037',vWest),apathFn('iArea1038',vSouth),apathFn('iArea1038',vWest),apathFn('iArea1039',vNorth),apathFn('iArea1039',vUp),apathFn('iArea1040',vUp),apathFn('iArea1040',vDown),apathFn('iArea1041',vUp),apathFn('iArea1041',vDown),apathFn('iArea1042',vNorth),apathFn('iArea1042',vSouth)]).

==>pddlObjects(vtDirection,[vNorth,vSouth,vEast,vWest,vUp,vDown]).
/*********************** predcate defns ***********************************/



%:- onLoad(must_det(show_call((mudAtLoc('iExplorer2',X),nonvar(X))))).

end_of_file.

/*********************** invariants ****************************************/

% LHS vars univ. quantified over primitive pddlSorts
% RHS free vars are existentially quantified
/*

implied_invariant([loaded(P,V)], [at(V,L),at(P,L)]).

inconsistent_constraint([certified(P), not_insured(P)]).

%%atomic_invariants([]):!.%%,fail.
atomic_invariants([
      pathDirLeadsTo('iArea1000',vNorth,'iArea1002'),
      pathDirLeadsTo('iArea1001',vEast,'iArea1002'),
      pathDirLeadsTo('iArea1002',vNorth,'iArea1005'),
      pathDirLeadsTo('iArea1002',vEast,'iArea1003'),
      pathDirLeadsTo('iArea1002',vSouth,'iArea1000'),
      pathDirLeadsTo('iArea1002',vWest,'iArea1001'),
      pathDirLeadsTo('iArea1003',vWest,'iArea1002'),
      pathDirLeadsTo('iArea1004',vEast,'iArea1005'),
      pathDirLeadsTo('iArea1005',vNorth,'iArea1008'),
      pathDirLeadsTo('iArea1005',vEast,'iArea1006'),
      pathDirLeadsTo('iArea1005',vSouth,'iArea1002'),
      pathDirLeadsTo('iArea1005',vWest,'iArea1004'),
      pathDirLeadsTo('iArea1006',vNorth,'iArea1042'),
      pathDirLeadsTo('iArea1006',vWest,'iArea1005'),
      pathDirLeadsTo('iArea1007',vEast,'iArea1008'),
      pathDirLeadsTo('iArea1008',vNorth,'iArea1010'),
      pathDirLeadsTo('iArea1008',vEast,'iArea1009'),
      pathDirLeadsTo('iArea1008',vSouth,'iArea1005'),
      pathDirLeadsTo('iArea1008',vWest,'iArea1007'),
      pathDirLeadsTo('iArea1009',vWest,'iArea1008'),
      pathDirLeadsTo('iArea1010',vSouth,'iArea1008'),
      pathDirLeadsTo('iArea1010',vUp,'iArea1011'),
      pathDirLeadsTo('iArea1011',vNorth,'iArea1013'),
      pathDirLeadsTo('iArea1011',vUp,'iArea1032'),
      pathDirLeadsTo('iArea1011',vDown,'iArea1010'),
      pathDirLeadsTo('iArea1012',vEast,'iArea1013'),
      pathDirLeadsTo('iArea1013',vNorth,'iArea1016'),
      pathDirLeadsTo('iArea1013',vEast,'iArea1014'),
      pathDirLeadsTo('iArea1013',vSouth,'iArea1011'),
      pathDirLeadsTo('iArea1013',vWest,'iArea1012'),
      pathDirLeadsTo('iArea1014',vWest,'iArea1013'),
      pathDirLeadsTo('iArea1015',vEast,'iArea1016'),
      pathDirLeadsTo('iArea1016',vNorth,'iArea1019'),
      pathDirLeadsTo('iArea1016',vEast,'iArea1017'),
      pathDirLeadsTo('iArea1016',vSouth,'iArea1013'),
      pathDirLeadsTo('iArea1016',vWest,'iArea1015'),
      pathDirLeadsTo('iArea1017',vWest,'iArea1016'),
      pathDirLeadsTo('iArea1018',vEast,'iArea1019'),
      pathDirLeadsTo('iArea1019',vNorth,'iArea1021'),
      pathDirLeadsTo('iArea1019',vEast,'iArea1020'),
      pathDirLeadsTo('iArea1019',vSouth,'iArea1016'),
      pathDirLeadsTo('iArea1019',vWest,'iArea1018'),
      pathDirLeadsTo('iArea1020',vWest,'iArea1019'),
      pathDirLeadsTo('iArea1021',vSouth,'iArea1019'),
      pathDirLeadsTo('iArea1022',vNorth,'iArea1024'),
      pathDirLeadsTo('iArea1022',vSouth,'iArea1039'),
      pathDirLeadsTo('iArea1023',vEast,'iArea1024'),
      pathDirLeadsTo('iArea1024',vNorth,'iArea1027'),
      pathDirLeadsTo('iArea1024',vEast,'iArea1025'),
      pathDirLeadsTo('iArea1024',vSouth,'iArea1022'),
      pathDirLeadsTo('iArea1024',vWest,'iArea1023'),
      pathDirLeadsTo('iArea1025',vWest,'iArea1024'),
      pathDirLeadsTo('iArea1026',vEast,'iArea1027'),
      pathDirLeadsTo('iArea1027',vNorth,'iArea1030'),
      pathDirLeadsTo('iArea1027',vEast,'iArea1028'),
      pathDirLeadsTo('iArea1027',vSouth,'iArea1024'),
      pathDirLeadsTo('iArea1027',vWest,'iArea1026'),
      pathDirLeadsTo('iArea1028',vWest,'iArea1027'),
      pathDirLeadsTo('iArea1029',vEast,'iArea1030'),
      pathDirLeadsTo('iArea1030',vNorth,'iArea1032'),
      pathDirLeadsTo('iArea1030',vEast,'iArea1031'),
      pathDirLeadsTo('iArea1030',vSouth,'iArea1027'),
      pathDirLeadsTo('iArea1030',vWest,'iArea1029'),
      pathDirLeadsTo('iArea1031',vWest,'iArea1030'),
      pathDirLeadsTo('iArea1032',vSouth,'iArea1030'),
      pathDirLeadsTo('iArea1032',vUp,'iArea1033'),
      pathDirLeadsTo('iArea1032',vDown,'iArea1011'),
      pathDirLeadsTo('iArea1033',vUp,'iArea1034'),
      pathDirLeadsTo('iArea1033',vDown,'iArea1032'),
      pathDirLeadsTo('iArea1034',vUp,'iArea1036'),
      pathDirLeadsTo('iArea1034',vDown,'iArea1033'),
      pathDirLeadsTo('iArea1035',vEast,'iArea1038'),
      pathDirLeadsTo('iArea1036',vNorth,'iArea1038'),
      pathDirLeadsTo('iArea1036',vEast,'iArea1037'),
      pathDirLeadsTo('iArea1036',vDown,'iArea1034'),
      pathDirLeadsTo('iArea1037',vWest,'iArea1036'),
      pathDirLeadsTo('iArea1038',vSouth,'iArea1036'),
      pathDirLeadsTo('iArea1038',vWest,'iArea1035'),
      pathDirLeadsTo('iArea1039',vNorth,'iArea1022'),
      pathDirLeadsTo('iArea1039',vUp,'iArea1040'),
      pathDirLeadsTo('iArea1040',vUp,'iArea1041'),
      pathDirLeadsTo('iArea1040',vDown,'iArea1039'),
      pathDirLeadsTo('iArea1041',vUp,'iArea1041'),
      pathDirLeadsTo('iArea1041',vDown,'iArea1040'),
      % pathDirLeadsTo('iArea1042',vNorth,'iArea1004075'),
      pathDirLeadsTo('iArea1042',vSouth,'iArea1006'),

     %% printAll(pathDirLeadsTo(X,Y,Z),agentRoute(apath(X,Y),X,Z)).
      agentRoute(apath('iArea1000',vNorth),'iArea1000','iArea1002'),
      agentRoute(apath('iArea1001',vEast),'iArea1001','iArea1002'),
      agentRoute(apath('iArea1002',vNorth),'iArea1002','iArea1005'),
      agentRoute(apath('iArea1002',vEast),'iArea1002','iArea1003'),
      agentRoute(apath('iArea1002',vSouth),'iArea1002','iArea1000'),
      agentRoute(apath('iArea1002',vWest),'iArea1002','iArea1001'),
      agentRoute(apath('iArea1003',vWest),'iArea1003','iArea1002'),
      agentRoute(apath('iArea1004',vEast),'iArea1004','iArea1005'),
      agentRoute(apath('iArea1005',vNorth),'iArea1005','iArea1008'),
      agentRoute(apath('iArea1005',vEast),'iArea1005','iArea1006'),
      agentRoute(apath('iArea1005',vSouth),'iArea1005','iArea1002'),
      agentRoute(apath('iArea1005',vWest),'iArea1005','iArea1004'),
      agentRoute(apath('iArea1006',vNorth),'iArea1006','iArea1042'),
      agentRoute(apath('iArea1006',vWest),'iArea1006','iArea1005'),
      agentRoute(apath('iArea1007',vEast),'iArea1007','iArea1008'),
      agentRoute(apath('iArea1008',vNorth),'iArea1008','iArea1010'),
      agentRoute(apath('iArea1008',vEast),'iArea1008','iArea1009'),
      agentRoute(apath('iArea1008',vSouth),'iArea1008','iArea1005'),
      agentRoute(apath('iArea1008',vWest),'iArea1008','iArea1007'),
      agentRoute(apath('iArea1009',vWest),'iArea1009','iArea1008'),
      agentRoute(apath('iArea1010',vSouth),'iArea1010','iArea1008'),
      agentRoute(apath('iArea1010',vUp),'iArea1010','iArea1011'),
      agentRoute(apath('iArea1011',vNorth),'iArea1011','iArea1013'),
      agentRoute(apath('iArea1011',vUp),'iArea1011','iArea1032'),
      agentRoute(apath('iArea1011',vDown),'iArea1011','iArea1010'),
      agentRoute(apath('iArea1012',vEast),'iArea1012','iArea1013'),
      agentRoute(apath('iArea1013',vNorth),'iArea1013','iArea1016'),
      agentRoute(apath('iArea1013',vEast),'iArea1013','iArea1014'),
      agentRoute(apath('iArea1013',vSouth),'iArea1013','iArea1011'),
      agentRoute(apath('iArea1013',vWest),'iArea1013','iArea1012'),
      agentRoute(apath('iArea1014',vWest),'iArea1014','iArea1013'),
      agentRoute(apath('iArea1015',vEast),'iArea1015','iArea1016'),
      agentRoute(apath('iArea1016',vNorth),'iArea1016','iArea1019'),
      agentRoute(apath('iArea1016',vEast),'iArea1016','iArea1017'),
      agentRoute(apath('iArea1016',vSouth),'iArea1016','iArea1013'),
      agentRoute(apath('iArea1016',vWest),'iArea1016','iArea1015'),
      agentRoute(apath('iArea1017',vWest),'iArea1017','iArea1016'),
      agentRoute(apath('iArea1018',vEast),'iArea1018','iArea1019'),
      agentRoute(apath('iArea1019',vNorth),'iArea1019','iArea1021'),
      agentRoute(apath('iArea1019',vEast),'iArea1019','iArea1020'),
      agentRoute(apath('iArea1019',vSouth),'iArea1019','iArea1016'),
      agentRoute(apath('iArea1019',vWest),'iArea1019','iArea1018'),
      agentRoute(apath('iArea1020',vWest),'iArea1020','iArea1019'),
      agentRoute(apath('iArea1021',vSouth),'iArea1021','iArea1019'),
      agentRoute(apath('iArea1022',vNorth),'iArea1022','iArea1024'),
      agentRoute(apath('iArea1022',vSouth),'iArea1022','iArea1039'),
      agentRoute(apath('iArea1023',vEast),'iArea1023','iArea1024'),
      agentRoute(apath('iArea1024',vNorth),'iArea1024','iArea1027'),
      agentRoute(apath('iArea1024',vEast),'iArea1024','iArea1025'),
      agentRoute(apath('iArea1024',vSouth),'iArea1024','iArea1022'),
      agentRoute(apath('iArea1024',vWest),'iArea1024','iArea1023'),
      agentRoute(apath('iArea1025',vWest),'iArea1025','iArea1024'),
      agentRoute(apath('iArea1026',vEast),'iArea1026','iArea1027'),
      agentRoute(apath('iArea1027',vNorth),'iArea1027','iArea1030'),
      agentRoute(apath('iArea1027',vEast),'iArea1027','iArea1028'),
      agentRoute(apath('iArea1027',vSouth),'iArea1027','iArea1024'),
      agentRoute(apath('iArea1027',vWest),'iArea1027','iArea1026'),
      agentRoute(apath('iArea1028',vWest),'iArea1028','iArea1027'),
      agentRoute(apath('iArea1029',vEast),'iArea1029','iArea1030'),
      agentRoute(apath('iArea1030',vNorth),'iArea1030','iArea1032'),
      agentRoute(apath('iArea1030',vEast),'iArea1030','iArea1031'),
      agentRoute(apath('iArea1030',vSouth),'iArea1030','iArea1027'),
      agentRoute(apath('iArea1030',vWest),'iArea1030','iArea1029'),
      agentRoute(apath('iArea1031',vWest),'iArea1031','iArea1030'),
      agentRoute(apath('iArea1032',vSouth),'iArea1032','iArea1030'),
      agentRoute(apath('iArea1032',vUp),'iArea1032','iArea1033'),
      agentRoute(apath('iArea1032',vDown),'iArea1032','iArea1011'),
      agentRoute(apath('iArea1033',vUp),'iArea1033','iArea1034'),
      agentRoute(apath('iArea1033',vDown),'iArea1033','iArea1032'),
      agentRoute(apath('iArea1034',vUp),'iArea1034','iArea1036'),
      agentRoute(apath('iArea1034',vDown),'iArea1034','iArea1033'),
      agentRoute(apath('iArea1035',vEast),'iArea1035','iArea1038'),
      agentRoute(apath('iArea1036',vNorth),'iArea1036','iArea1038'),
      agentRoute(apath('iArea1036',vEast),'iArea1036','iArea1037'),
      agentRoute(apath('iArea1036',vDown),'iArea1036','iArea1034'),
      agentRoute(apath('iArea1037',vWest),'iArea1037','iArea1036'),
      agentRoute(apath('iArea1038',vSouth),'iArea1038','iArea1036'),
      agentRoute(apath('iArea1038',vWest),'iArea1038','iArea1035'),
      agentRoute(apath('iArea1039',vNorth),'iArea1039','iArea1022'),
      agentRoute(apath('iArea1039',vUp),'iArea1039','iArea1040'),
      agentRoute(apath('iArea1040',vUp),'iArea1040','iArea1041'),
      agentRoute(apath('iArea1040',vDown),'iArea1040','iArea1039'),
      agentRoute(apath('iArea1041',vUp),'iArea1041','iArea1041'),
      agentRoute(apath('iArea1041',vDown),'iArea1041','iArea1040'),
      % agentRoute(apath('iArea1042',vNorth),'iArea1042','iArea1004075'),
      agentRoute(apath('iArea1042',vSouth),'iArea1042','iArea1006')

     ]).
*/

/*********************** ss classes ****************************

substate_classes(physical_obj, P,
       [
        [at(P,L)]
       ]).

substate_classes(tAgent, P,
       [
        [localityOfObject(P,L)]
       ]).

substate_classes(railv, V,
       [
        [unattached(V)] , [attached(V,V1)]
       ]).
substate_classes(vehicle, T,
       [
        [moveable(T),available(T)], 
        [moveable(T),busy(T)] 
       ]).

substate_classes(package, P,
       [
        [uncertified(P)],
        [waiting(P),certified(P)],
        [loaded(P,V),certified(P)],
        [delivered(P)]
      ]) .

************/
/*********************** operators *****

% method(name,precons,transitions,statics,temps,decomposition)
% operator(name,prevail,transitions,cond_transitions)


method(
 % 1. name
      transport(P,O,D),
 % 2. dynamic constraints
      [ ],
 % 3. list of necessary substate changes
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] => 
                       [at(P,D), delivered(P)]) ],
 % 4. static constraints
      [ ne(O,D),in_region(O,R),in_region(D,R)
       % list of static pddlPredicates that must be instantiated to
       % be true. Static preds may also appear in 2. and 3. if
       % its clearer that way 
       ],

 % 5.  temporal  constraints
       % list of static pddlPredicates before(N1,N2)
      [before(1,2),before(2,3)],
 % 6. decomposition
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), carry_direct(P,O,D), deliver(P,D)]
 ).

method(
      transport(P,O,D),
      [ ],
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] =>
                       [at(P,D), delivered(P)]) ],
      [ ne(O,D),ne(R1,R2),is_of_sort(AV,aircraft),
        in_region(O,R1),in_region(D,R2),
        serves_region(A1,R1),serves_region(A2,R2)
       ],
      [before(1,2),before(2,3),before(3,4),before(4,5)],
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), 
        carry_direct(P,O,A1), carry_via_ap(A1,A2,P), 
        carry_direct(P,A2,D), deliver(P,D)]
 ).

method(
   carry_via_ap(O,D,P),
  [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] =>
                       [at(P,D),waiting(P),certified(P)]) ],
      [ ne(O,D), 
%                ne(O,O1), is_of_sort(O1,airport),
        is_of_sort(O,airport), is_of_sort(D,airport),
        is_of_sort(P,package), is_of_sort(V,aircraft)],
 [before(1,3), before(2,3),before(3,4), before(4,5)],
  [
%       fly(V,O1,O),
       commission(V),
       achieve(ss(aircraft,V,[at(V,O)])),
       load_package(P,V,O),
       fly(V,O,D),
       unload_package(P,V,D)
       ]
).

% carry in one city
method(  
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5)
       ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,local_roads),
       unload_package(P,V,D)
       ]
).

% carry between two cities by traincar
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,traincar),
       is_of_sort(Train,train),
       connects(R,O,D),
       rv_compatible(R,traincar),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5),
       before(5,6),before(6,7),before(7,8) ],
 [     commission(V),
       achieve(ss(train,Train,[at(Train,O),attached(Train,V)])),
       load_package(P,V,O),
       pull_traincar(Train,V,O,D,R),
       detach_traincar(Train,V,D),
       unload_package(P,V,D)
       ]
).

% carry between two cities by truck
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY1),
       ne(CY,CY1),
       connects(R,CY,CY1),
       is_of_sort(R,road_route),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,R),
       unload_package(P,V,D)
       ]
).

method(
  move_traincar(V, O, L, R2),
 [ ],
         [sc(traincar,V,[at(V,O) ]
            =>[at(V,L)] )],
 [is_of_sort(V,traincar),
       connects(R2,O,L),
       is_of_sort(R2,rail_route),
       is_of_sort(Train,train) ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   achieve(ss(train,Train,[at(Train,O)])),
          attach_traincar(Train,V,O),
          pull_traincar(Train,V,O,L,R2),
          detach_traincar(Train,V,L) ]
).
***********************************/
/* getting 
 operator( pay_fees(P),
      [],
      [sc(package,P,[uncertified(P)]
      =>[waiting(P),certified(P)])],
      [ ]).

operator(fly(A,D1,D2),
      [ ],
      [sc(aircraft,A,[at(A,D1)]
            =>[at(A,D2)] )],
         [sc(package,X,[loaded(X,A),certified(X),at(X,D1)]
            => [loaded(X,A),certified(X),at(X,D2)])  ]
).


%move truck
operator( move(V, O, L, R), 
        [ ],
         [sc(truck,V,[at(V,O),
             is_of_sort(R,road_route),
             moveable(V),
             in_city(O,City),
             in_city(L,City1),
             ne(City,City1),
             connects(R,City,City1)]
            =>[at(V,L)] )],
         [sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            => [loaded(X,V),certified(X),at(X,L)])  ]
).

%move truck inside city
operator( move(V, O, L, local_roads), 
         [],
         [sc(truck,V,[at(V,O),
             moveable(V),
             in_city(O,City),
             in_city(L,City)]
            =>[at(V,L)]  )],
         [ sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            =>[loaded(X,V),certified(X),at(X,L)])   ]
).

%move traincar
operator( pull_traincar(Train,V1, O, L, Rt), 
         [  ],
         [ sc(train,Train,[at(Train,O),
             attached(Train,V1),
             moveable(Train),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(Train,L),attached(Train,V1)] ),
           sc(traincar,V1,[at(V1,O),attached(V1,Train)]
            =>[at(V1,L),attached(V1,Train)]) ],
         [sc(package,P,[loaded(P,V1),certified(P),at(P,O)]
            =>[loaded(P,V1),certified(P),at(P,L)]) ]
).

operator( move_train(V, O, L, Rt),
         [ ],
          [sc(train,V,[at(V,O),unattached(V),
             moveable(V),available(V),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(V,L),unattached(V),moveable(V),available(V)] )],
       [ ]
).

operator(attach_traincar(Train,V,O),
     [  ],
     [sc(train, Train, [at(Train,O),moveable(Train),available(Train),unattached(Train)]
        =>[at(Train,O),attached(Train,V),moveable(Train),busy(Train)] ),
     sc(traincar, V, [at(V,O),unattached(V)]
        =>[at(V,O),attached(V,Train)] ) ],
     [ ]
).

operator(detach_traincar(Train,V,O),
     [ ],
     [sc(train, Train, [attached(Train,V),moveable(Train),busy(Train)]
        =>[unattached(Train),moveable(Train),available(Train)] ),
     sc(traincar, V, [attached(V,Train)]
        =>[unattached(V)] ) ],
     [ ]
).

operator(commission(V),
      [ ],
      [sc(vehicle, V,[moveable(V),available(V)] =>[moveable(V), busy(V)])],
      [ ]).

         

operator( load_package(P,V,L),
   [ss(vehicle,V, [at(V,L),moveable(V),busy(V)])],
   [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
      [at(P,L),loaded(P,V),certified(P)])],
   []
).

operator( unload_package(P,V,L),
 [],
 [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]
),
 sc(vehicle,V, [at(V,L), moveable(V), busy(V)] => [at(V,L),moveable(V),available(V)])
],
 []
 ).


operator( deliver(P,L),
        [],
        [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
          [at(P,L),delivered(P)] )],
        []
).

docs ready */


% TESTS

actorStartState(tAgent,X,[localityOfObject(X,Y)|List]):tAgent(X),localityOfObject(X,Y),findall(mudStowing(X,P),mudStowing(X,P),List1),findall(wearsClothing(X,P),wearsClothing(X,P),List2),flatten([List1,List2],List).
initialStart([ 

/*
localityOfObject('iArea1005Object666','iArea1005').
localityOfObject('iArea1008Object667','iArea1008').
localityOfObject('iArea1013Object668','iArea1013').
localityOfObject('iArea1016Object669','iArea1016').
localityOfObject('iArea1024Object670','iArea1024').
localityOfObject('iDilithiumCrystal756','iArea1000').
localityOfObject('iDilithiumCrystal757','iArea1000').
localityOfObject('iDilithiumCrystal758','iArea1000').
localityOfObject('iTricorder759','iArea1015').
localityOfObject('iTricorder760','iArea1015').
localityOfObject('iTricorder761','iArea1015').
*/
%% printAll(actorStartState(tAgent,X,List),ss(tAgent,X,List)).
   ss(tAgent,'iGeordi684',[localityOfObject('iGeordi684','iArea1000'),wearsClothing('iGeordi684','iBoots685'),wearsClothing('iGeordi684','iCommBadge686'),wearsClothing('iGeordi684','iGoldUniform687'),wearsClothing('iGeordi684','iVISOR688')]),
   ss(tAgent,vacuum(1),[localityOfObject(vacuum(1),'iArea1010'),mudStowing(vacuum(1),'iPhaser676'),wearsClothing(vacuum(1),'iBoots673'),wearsClothing(vacuum(1),'iCommBadge674'),wearsClothing(vacuum(1),'iGoldUniform675')]),
   ss(tAgent,iExplorer1,[localityOfObject(iExplorer1,'iArea1000'),mudStowing(iExplorer1,'iPhaser776'),wearsClothing(iExplorer1,'iBoots773'),wearsClothing(iExplorer1,'iCommBadge774'),wearsClothing(iExplorer1,'iGoldUniform775')]),
   ss(tAgent,'iWorf720',[localityOfObject('iWorf720','iArea1025'),mudStowing('iWorf720','iPhaser724'),wearsClothing('iWorf720','iBoots721'),wearsClothing('iWorf720','iCommBadge722'),wearsClothing('iWorf720','iGoldUniform723'),wearsClothing('iWorf720','iSash725')]),
   ss(tAgent,'iDrCrusher677',[localityOfObject('iDrCrusher677','iArea1015'),mudStowing('iDrCrusher677','iMedicalTricorder681'),mudStowing('iDrCrusher677','iMedicalTricorder682'),mudStowing('iDrCrusher677','iMedicalTricorder683'),wearsClothing('iDrCrusher677','iBoots678'),wearsClothing('iDrCrusher677','iCommBadge679'),wearsClothing('iDrCrusher677','iBlueUniform680')]),
   ss(tAgent,'iTroi712',[localityOfObject('iTroi712','iArea1007'),wearsClothing('iTroi712','iBoots713'),wearsClothing('iTroi712','iCommBadge714'),wearsClothing('iTroi712','iBlueUniform715')]),
   ss(tAgent,'iRiker707',[localityOfObject('iRiker707','iArea1036'),mudStowing('iRiker707','iTrombone711'),wearsClothing('iRiker707','iBoots708'),wearsClothing('iRiker707','iCommBadge709'),wearsClothing('iRiker707','iRedUniform710')]),
   ss(tAgent,'iPicard701',[localityOfObject('iPicard701','iArea1035'),mudStowing('iPicard701','iPhaserRifle705'),mudStowing('iPicard701','iPicardsFlute'),wearsClothing('iPicard701','iBoots702'),wearsClothing('iPicard701','iCommBadge703'),wearsClothing('iPicard701','iRedUniform704')]),
   ss(tAgent,'iGuinan689',[localityOfObject('iGuinan689','iArea1021'),mudStowing('iGuinan689','iTea690'),mudStowing('iGuinan689','iSynthehol691'),mudStowing('iGuinan689','iFerengiAle692'),mudStowing('iGuinan689','iRomulanWhisky693'),mudStowing('iGuinan689','iLemonadePruneJuice694'),mudStowing('iGuinan689','iVulcanBeer695')]),
   ss(tAgent,'iOBrien696',[localityOfObject('iOBrien696','iArea1006'),mudStowing('iOBrien696','iPhaser700'),wearsClothing('iOBrien696','iBoots697'),wearsClothing('iOBrien696','iCommBadge698'),wearsClothing('iOBrien696','iGoldUniform699')]),
   ss(tAgent,'iWesley716',[localityOfObject('iWesley716','iArea1016'),wearsClothing('iWesley716','iBoots717'),wearsClothing('iWesley716','iCommBadge718'),wearsClothing('iWesley716','iRedUniform719')]),
   ss(tAgent,'iLivingston726',[localityOfObject('iLivingston726','iArea1035')]),
   ss(tAgent,'iSpot727',[localityOfObject('iSpot727','iArea1003')]),
   ss(tAgent,'iExplorer2',[localityOfObject('iExplorer2','iArea1000'),wearsClothing('iExplorer2','iBoots729'),wearsClothing('iExplorer2','iCommBadge730'),wearsClothing('iExplorer2','iGoldUniform731')]),
   ss(tAgent,'iExplorer3',[localityOfObject('iExplorer3','iArea1004'),wearsClothing('iExplorer3','iBoots733'),wearsClothing('iExplorer3','iCommBadge734'),wearsClothing('iExplorer3','iGoldUniform735')]),
   ss(tAgent,'iExplorer4',[localityOfObject('iExplorer4','iArea1011'),wearsClothing('iExplorer4','iBoots737'),wearsClothing('iExplorer4','iCommBadge738'),wearsClothing('iExplorer4','iRedUniform739')]),
   ss(tAgent,'iExplorer5',[localityOfObject('iExplorer5','iArea1020'),wearsClothing('iExplorer5','iBoots741'),wearsClothing('iExplorer5','iCommBadge742'),wearsClothing('iExplorer5','iRedUniform743')]),
   ss(tAgent,'iExplorer6',[localityOfObject('iExplorer6','iArea1024'),wearsClothing('iExplorer6','iBoots745'),wearsClothing('iExplorer6','iCommBadge746'),wearsClothing('iExplorer6','iBlueUniform747')]),
   ss(tAgent,'iExplorer7',[localityOfObject('iExplorer7','iArea1022'),wearsClothing('iExplorer7','iBoots749'),wearsClothing('iExplorer7','iCommBadge750'),wearsClothing('iExplorer7','iBlueUniform751')]),
   ss(tAgent,'iExplorer8',[localityOfObject('iExplorer8','iArea1036'),wearsClothing('iExplorer8','iBoots753'),wearsClothing('iExplorer8','iCommBadge754'),wearsClothing('iExplorer8','iBlueUniform755')]),
   ss(tAgent,'iAlexander671',[localityOfObject('iAlexander671','iArea1025')])
   ]).


% 

:- baseKB:force_reload_mpred_file('./**/*.pfc.pl').


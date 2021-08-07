/** <module> 
% This file contains the definitions for the objects in a household
% To create a new world, simply change the tObj definitions as
% described below (or in manual)
%
% *******Object definitions*******
%.
% Use the label_type_props(label,typecode,[property1,property2,etc.]]).
% label is whats used to refer to the tObj in the running world
% typecode is whats used to refer to the tObj for initialization (see world.pl)
*/

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

% ==================================================
% Where Things are Located
% ==================================================

:- file_begin(pfc).

==>prologHybrid(localityOfObject(tObj,tSpatialThing)).
% which will imply ... :-decl_mpred_hybrid(localityOfObject(kappa(Self,genls(Self,obj)),kappa(Self2,genls(Self2,tSpatialThing)))).



:- thread_local(infSupertypeName/0).
% alolwos lexical deduction such as tStandTable is a tTable and a tBedRoom is a tRoom
% onSpawn localityOfObject(tStandTable, tBedRoom).

:- op(600,fx,onSpawn).

% onSpawn accepts class in any argument
% though also isntances like 

onSpawn tRegion(tKitchen).
onSpawn tRegion(tHallWay).
onSpawn tRegion(tCellar).
onSpawn tRegion(tOfficeRoom).
onSpawn tRegion(tLivingRoom).
onSpawn tRegion(tDiningRoom).
onSpawn tRegion(tBedRoom).
onSpawn tRegion(tBathroom).
onSpawn tRegion(tClosetRoom).
onSpawn tRegion(tBackYard).


% ==================================================
% Classes of things
% ==================================================

genls(tCandle,tPortableDevice).
genls(tFlashlight, tPortableDevice).
genls(tFlashlight, tTurnOnAble).
genls(tDesk, tOfficeFurnature).
genls(tApple, tFruit).
genls(tNani, tArt).
genls(tNani, tWashAble).
genls(tClothing, tWashAble).
genls(tClothing, tWearAble).
genls(tNani, tCarryAble).
genls(tBroccoli, tFood).
genls(tFood,tEatAble).
genls(tCrackers, tFood).
genls(tEnvelope, tOfficeItem).
genls(tStamp, tOfficeItem).
genls(tKey, tOfficeItem).
genls(tBall, tCarryAble).
genls(tFruit,tFood).

genls(tOfficeItem, tItem).
genls(tCandle,tPortableDevice).
genls(tFlashlight, tPortableDevice).
genls(tApple, tFruit).
genls(tPants, tWearAble).
genls(tCoffee, tLiquid).
genls(tTable, tFurnature).

genls(tTable, tFurnatureMoveAble).

genls(tTable, tHasSurface).
genls(tEasyChair, tChair).
genls(tChair, tSitAble).

genls(tPajamas, tClothing).
genls(tShirt, tClothing).
genls(tCoat, tClothing).
genls(tPants, tClothing).
genls(tClothing, tFoldAble).

genls(tCoffeeCup, tLiquidContainer).
genls(tCoffeeCup, tDrinkAble).
genls(tCoffeeCup, tCarryAble).
genls(tCoffee, tLiquid).
genls(tBook, tReadAble).

genls(tPillow, tBedding).
genls(tBlanket, tBedding).                                                              
genls(tBedding, tFoldAble).
genls(tFoldAble, tCarryAble).
genls(tPortableDevice,tCarryAble).
genls(tPhysicalDevice,tUseAble).
genls(tWearAble, tCarryAble).
genls(tFood,tCarryAble).
genls(tCarryAble,tObj).
% genls(tPartofObj,isTDisjoint(tCarryAble)).
disjointWith(tPartofObj,tCarryAble).

genls(tLightSwitch, tWallFurnature).
genls(tComputer, tOfficeFurnature).
genls(tPartofFurnature,tPartofObj).

genls(tBed, tBedroomFurnature).
genls(tStandTable, tBedroomFurnature).
genls(tDresser, tBedroomFurnature).


genls(tOfficeFurnature,tFurnature).
genls(tBathroomFurnature,tFurnature).
genls(tLivingroomFurnature,tFurnature).
genls(tOutdoorFurnature,tFurnature).

genls(tWashingMachine, tDeviceFurnature).
genls(tTable, tFurnature).
genls(tTable, tHasSurface).
genls(tSofa, tLivingroomFurnature).
genls(tEasyChair, tLivingroomFurnature).
genls(tCoffeeTable, tLivingroomFurnature).
genls(tBookcase, tWallFurnature).
genls(tDrawer, tContainer).
genls(tDrawer, tOpenCloseAble).
genls(tDrawer, tPartofFurnature).
genls(tTopDrawer, tDrawer).
genls(tMiddleDrawer, tDrawer).
genls(tBottomDrawer, tDrawer). 
genls(tHousePlant, tPartofObj).
genls(tPlanter, tContainer). 
genls(tGueridon, tLivingroomFurnature).
genls(tGardenPlant, tOutdoorFurnature).
genls(tSpatialThing,tLookAble).
genls(tNani,tTeddybear).
genls(tTeddybear,tTreasure).


genls(tBathTub,tHumanEnterAble).
genls(tShower,tHumanEnterAble).
genls(tToilet,tSitAble).
genls(tBathroomSink,tSink).
genls(tFridge,tContainer).

genls(tSink,tFurnature).

genls(tKitchenFurnature,tDeviceFurnature).
genls(tDeviceFurnature,tFurnature).

genls(tBathTub,tBathroomFurnature).
genls(tShower,tBathroomFurnature).
genls(tToilet,tBathroomFurnature).
genls(tBathroomSink,tSink).
genls(tBathroomSink,tBathroomFurnature).
genls(tSink,tFurnature).
genls(tKitchenSink,isEach(tKitchenFurnature,tSink)).
genls(tFridge,tKitchenFurnature).
genls(tStove,isEach(tKitchenFurnature,tHasSurface)).
genls(tOven,tPartofFurnature).
onSpawn localityOfObject(tOven,tStove).
genls(tKitchenCounter,tKitchenFurnature).


onSpawn(localityOfObject(Obj, Where)),tSet(Obj),tSet(Where)==> typicalLocation(Obj, Where).
onSpawn(inRegion(Obj, Where)),tSet(Obj),tSet(Where)==> typicalRegion(Obj, Where).

% ttTypeFacet(ttInstanceType).

needs_spawned(IType) :- nonvar(IType), tSet(IType), \+ clause_b(onSpawn(inRegion(IType,_))),\+ clause_b(onSpawn(localityOfObject(IType,_))), !, \+ (genls(Sub,IType),nonvar(Sub),Sub\=IType). 

typicalRegion(tBathroomFurnature,tBathroom).
typicalRegion(tLivingroomFurnature,tLivingRoom).
typicalRegion(tBedroomFurnature,tBedRoom).
typicalRegion(tKitchenFurnature,tKitchen).
typicalRegion(tOfficeFurnature,tOfficeRoom).
typicalLocation(tOfficeItem,tDesk).
typicalRegion(tOutdoorFurnature,tBackYard).

typicalRegion(Type,RegionType),genls(I,Type),{needs_spawned(I)} ==> onSpawn(inRegion(I,RegionType)).
typicalLocation(Type,ContainerType),genls(I,Type),{needs_spawned(I)} ==> onSpawn(localityOfObject(I,ContainerType)).


definitionalProp(isEach(mudColor,mudSize,mudShape,mudMaterial,mudTexture,mudWeight)).


% ==================================================
% typeProps Descriptions
% ==================================================

:- do_gc.
typeProps(tBall,[mudColor(vRed),mudSize(vMedium),mudShape(vSpherical),mudMaterial(vRubber),mudTexture(vBumpy)]).
typeProps(tBlanket,[mudColor(isEach(vGreen,vDark)),mudSize(isEach(vLarge,vBulky)),mudShape(vRectangular),mudMaterial(vRayon),mudTexture(isEach(vPlush,vSoft))]).                                                              
typeProps(tBookcase,[mudColor(isEach(vBrown,vDark,vShiney)),mudSize(vLarge),mudShape(isEach(vSquare,vConcavePolyhedron)),mudMaterial(vWood),mudTexture(vSmooth),mudWeight(60)]).
typeProps(tBottomDrawer,[mudColor(isEach(vBrown,vShiney)),mudSize(vMedium),mudShape(isEach(vRectangular,vConcavePolyhedron)),mudMaterial(vWood),mudTexture(vSmooth)]).
typeProps(tBroccoli,[mudColor(vGreen),mudSize(vSmall),mudShape(isLikeFn(mudShape,tTree)),isa(tVegetable),mudTexture(isEach(vPorous,vBumpy))]).                                                                    
typeProps(tCandle,[mudColor(vBlue),mudSize(vSmall),mudShape(vTriangular),mudMaterial(vWax),mudTexture(vSticky)]).
typeProps(tCoat,[mudColor(vBlack),mudSize(isEach(vMedium,vFits)),mudShape(isLikeFn(mudShape,tTorso)),mudMaterial(isEach(vWool,vCotton,vRayon)),mudTexture(vCoarse)]).
typeProps(tCoffee,[mudColor(isEach(vBlack,vBrown)),mudSize(vSmall),mudShape(vFluid),mudMaterial(tLiquid),mudTexture(vWet)]).
typeProps(tCoffeeCup,[mudColor(vBlack),mudColor(vBrown),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).
typeProps(tCoffeeTable,[isa(tTable),mudColor(isEach(vBrown,vShiney)),mudSize(vMedium),mudShape(vCircular),mudMaterial(vWood),mudTexture(vSmooth),mudWeight(35)]).
typeProps(tComputer,[mudColor(vGrey),mudMaterial(vPlastic),mudShape(isEach(vCubular,vRectangular)),mudSize(vMedium),mudTexture(vSmooth)]).
typeProps(tCrackers,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]).
typeProps(tDesk,[mudColor(isEach(vBrown,vShiney)),mudSize(vLarge),mudShape(isEach(vCubular,vRectangular)),mudTexture(isEach(vSmooth,vRidged)),mudWeight(175)]).
typeProps(tDresser,[mudColor(isEach(vBrown,vShiney)),mudSize(vLarge),mudShape(isEach(vRectangular,vPolyhedral)),mudMaterial(vWood),mudTexture(vSmooth),mudWeight(45)]).
typeProps(tEasyChair,[mudColor(vBlue),mudSize(isEach(vLarge,vNarrow)),mudShape(vPolyhedral),mudMaterial(isEach(vPoplin,vWood)),mudTexture(vRibbed),mudWeight(75)]).
typeProps(tEnvelope,[mudColor(vWhite),mudSize(vSmall),mudShape(vRectangular),mudMaterial(vPaper),mudTexture(vSmooth)]).
typeProps(tFlashlight,[mudColor(vYellow),mudSize(isEach(vMedium,vSmall)),mudShape(vCylindrical),mudMaterial(vPlastic),mudTexture(vRibbed)]).
typeProps(tGreenApple,[mudColor(isEach(vGreen,vPastel)),mudSize(vSmall),mudShape(vRound),isa(tApple),mudTexture(vSmooth)]).
typeProps(tGreenBook,[isa(tBook),mudColor(vGreen),mudColor(vWhite),mudColor(vBlack),mudSize(vSmall),mudShape(isEach(vCubular,vRectangular)),mudMaterial(vPaper),mudTexture(vSmooth)]).
typeProps(tGueridon,[mudColor(isEach(vBrown,vShiney)),mudSize(vMedium),mudShape(vCircular),mudMaterial(vWood),mudTexture(vSmooth),mudWeight(25)]).		 
typeProps(tHousePlant,[mudColor(vGreen),mudSize(vMedium),mudShape(vAnthuriumCrystallinum),mudMaterial(vPlantTissue),mudTexture(vSmooth)]).
typeProps(tKey,[mudColor(vSilver),mudSize(isEach(vSmall,vTiny)),mudShape(vUnique),mudMaterial(vMetal),mudTexture(isEach(vRidged,vBumpy))]).
typeProps(tLightSwitch,[mudColor(vCream),mudSize(vSmall),mudShape(vRectangular),mudMaterial(vPlastic),mudTexture(vSmooth)]).
typeProps(tMiddleDrawer,[mudColor(isEach(vBrown,vShiney)),mudSize(vMedium),mudShape(isEach(vRectangular,vConcavePolyhedron)),mudMaterial(vWood),mudTexture(vSmooth)]).
typeProps(tNani,[mudColor(vBrown),mudColor(vWhite),mudSize(vMedium),mudShape(isLikeFn(mudShape,tTeddybear)),mudMaterial(vPlush),mudTexture(isEach(vFuzzy,vSoft))]).
typeProps(tPajams,[mudColor(isEach(vBlue,vPastel)),mudSize(isEach(vMedium,vFits)),mudShape(vUnique),mudMaterial(isEach(vCotton,vRayon)),mudTexture(vSoft)]).
typeProps(tPants,[mudColor(vBlack),mudSize(isEach(vMedium,vFits)),mudShape(isLikeFn(mudShape,tAssBodypart)),mudMaterial(vDenim),mudTexture(vCoarse)]).
typeProps(tPillow,[mudColor(vWhite),mudSize(vMedium),mudShape(vRectangular),mudMaterial(isEach(vCotton,vFlannel)),mudTexture(isEach(vFluffy,vSoft))]).
typeProps(tPinkBook,[isa(tBook),mudColor(vPink),mudColor(vWhite),mudColor(vBlack),mudSize(vSmall),mudShape(isEach(vCubular,vRectangular)),mudMaterial(vPaper),mudTexture(vSmooth)]).
typeProps(tPlanter,[mudColor(isEach(vPastel,vBlue)),mudSize(vMedium),mudShape(isEach(vCylindrical,vTrapezohedron)),mudMaterial(vClay),mudTexture(vAbrasive),mudWeight(15)]).
typeProps(tRedApple,[mudColor(vRed),mudSize(vSmall),mudShape(vRound),isa(tApple),mudTexture(vSmooth)]).
typeProps(tShirt,[mudColor(vBlack),mudSize(isEach(vMedium,vFits)),mudShape(isLikeFn(mudShape,tTorso)),mudMaterial(vCotton),mudTexture(isEach(vSoft,vThreadbare))]).
typeProps(tSkin,[mudColor(vUnique),isa(tBodypart),mudShape(vUnique)]).
typeProps(tSofa,[mudColor(isEach(vBlue,vDark)),mudSize(isEach(vLarge,vWide,vLong)),mudShape(vPolyhedral),mudMaterial(isEach(vPoplin,vWood)),mudTexture(vRibbed),mudWeight(125)]).
typeProps(tStamp,[mudColor(vPolychromeatic),mudSize(isEach(vTiny,vThin)),mudShape(vSquare),mudMaterial(vPaper),mudTexture(isEach(vSmooth,vSticky))]).
typeProps(tStandTable,[mudColor(isEach(vBlack,vShiney)),mudSize(vMedium),mudShape(isEach(vCubular,vRectangular)),mudMaterial(vWood),mudTexture(vSmooth),mudWeight(25)]).
typeProps(tTable,[mudColor(isEach(vBlue,vWhite)),mudSize(vLarge),mudShape(vOval),mudMaterial(vWood),mudTexture(isEach(vBumpy,vIrregular)),mudWeight(50)]).
typeProps(tTeddybear,[mudColor(vNatural),mudShape(vUnique)]).
typeProps(tTopDrawer,[mudColor(isEach(vBrown,vShiney)),mudSize(vMedium),mudShape(isEach(vRectangular,vConcavePolyhedron)),mudMaterial(vWood),mudTexture(vSmooth)]). 
==>typeProps(tTorso,[mudColor(isLikeFn(mudColor,tSkin)),isa(tBodypart),mudShape(vUnique)]).
typeProps(tWashingMachine,[mudColor(vWhite),mudSize(vLarge),mudShape(isEach(vCubular,vSquare)),mudMaterial(vMetal),mudTexture(vSmooth),mudWeight(125)]).


% :- listing(ttInstanceType/1),break.





onSpawn localityOfObject(tRedApple, tKitchen).
onSpawn localityOfObject(tGreenApple, tKitchen).
%onSpawn localityOfObject(iFn(tApple,[mudColor(vRed)]), tKitchen).
%onSpawn localityOfObject(iFn(tApple,[mudColor(vGreen)]), tKitchen).
onSpawn localityOfObject(tDesk, tOfficeRoom).
onSpawn localityOfObject(tFlashlight, tDesk).
onSpawn localityOfObject(tWashingMachine, tCellar).
onSpawn localityOfObject(tNani, tWashingMachine).
onSpawn localityOfObject(tBroccoli, tKitchen).
onSpawn localityOfObject(tCrackers, tKitchen).
onSpawn localityOfObject(tComputer, tDesk).
onSpawn localityOfObject(tEnvelope, tDesk).
onSpawn localityOfObject(tStamp, tEnvelope).
onSpawn localityOfObject(tKey, tEnvelope).
onSpawn localityOfObject(tBed, tBedRoom).
onSpawn localityOfObject(tStandTable, tBedRoom).
onSpawn localityOfObject(tPajamas, tStandTable).
onSpawn localityOfObject(tShirt, tClosetRoom).
onSpawn localityOfObject(tPillow, tBed).
onSpawn localityOfObject(tBlanket, tBed).                                                              
onSpawn localityOfObject(tTable, tKitchen).
onSpawn localityOfObject(tCoat, tClosetRoom).
onSpawn localityOfObject(tPants, tClosetRoom).
onSpawn localityOfObject(tLightSwitch, tBedRoom).
onSpawn localityOfObject(tBall, tBackYard).
onSpawn localityOfObject(tSofa, tLivingRoom).
onSpawn localityOfObject(tEasyChair, tLivingRoom).
onSpawn localityOfObject(tCoffeeTable, tLivingRoom).
onSpawn localityOfObject(tCoffeeCup, tKitchen).
onSpawn localityOfObject(tCoffee, tCoffeeCup).
%onSpawn localityOfObject(tBook(mudColor(vGreen)), tCoffeeTable).
%onSpawn localityOfObject(tBook(mudColor(vPink)), tCoffeeTable).
onSpawn localityOfObject(tGreenBook, tCoffeeTable).
onSpawn localityOfObject(tPinkBook, tCoffeeTable).
onSpawn localityOfObject(tBookcase, tLivingRoom).
onSpawn localityOfObject(tDresser, tBedRoom).
onSpawn localityOfObject(tTopDrawer, tDresser).
onSpawn localityOfObject(tMiddleDrawer, tDresser).
onSpawn localityOfObject(tBottomDrawer, tDresser). 
onSpawn localityOfObject(tHousePlant, tPlanter).
onSpawn localityOfObject(tPlanter, tGueridon). 
onSpawn localityOfObject(tGueridon, tLivingRoom).
onSpawn localityOfObject(tGardenPlant, tBackYard).
onSpawn localityOfObject(tCandle,tKitchen).
onSpawn localityOfObject(tBathTub,tBathroom).
onSpawn localityOfObject(tShower,tBathroom).
onSpawn localityOfObject(tToilet,tBathroom).
onSpawn localityOfObject(tTreadmill,tHallWay).
onSpawn localityOfObject(tBathroomSink,tBathroom).
onSpawn localityOfObject(tKitchenSink,tKitchenCounter).
onSpawn localityOfObject(tFridge,tKitchen).
onSpawn localityOfObject(tStove,tKitchen).
onSpawn localityOfObject(tKitchenCounter,tKitchen).


% ==================================================
% Doors
% ==================================================
 
onSpawn pathDirLeadsTo(tKitchen,vDown,tCellar).
onSpawn mudAreaConnected(tLivingRoom,tOfficeRoom).
onSpawn mudAreaConnected(tHallWay,tDiningRoom).
onSpawn mudAreaConnected(tHallWay,tBedRoom).
onSpawn mudAreaConnected(tHallWay,tLivingRoom).
onSpawn mudAreaConnected(tHallWay,tBathRoom).
onSpawn mudAreaConnected(tKitchen, tCellar).
onSpawn mudAreaConnected(tDiningRoom, tKitchen).
onSpawn mudAreaConnected(tBedRoom, tClosetRoom).
onSpawn mudAreaConnected(tKitchen, tBackYard).
onSpawn mudAreaConnected(iArea1008, tBackYard).



are([hungry,thirsty,tooHot,tooCold,warm,cool],condition).
hasEnglishGlossesData(hungry,['hungry']).
hasEnglishGlossesData(thirsty,['thirsty']).
hasEnglishGlossesData(tooHot,['too hot']).
hasEnglishGlossesData(tooCold,['too cold']).
hasEnglishGlossesData(warm,['warm']).
hasEnglishGlossesData(cool,['cool']).

atTime([2017-1-1,12:0:0.0], location(bluetoothKeyboard,kitchen)).
atTime([2017-2-1,12:0:0.0], location(bluetoothKeyboard,livingRoom)).

are([real,fictional],wsmContext).
hasEnglishGlossesData(real,['Real','real']).
hasEnglishGlossesData(fictional,['Fictional','fictional']).

allIsa(Item,Results) :-
	setof(Class,Item^isa(Item,Class),Results).

%% genlsDirectlyList([person],intelligentAgent).
are([andrewDougherty,eleanorDougherty,meredithMcGhan,justinCoslor,douglasMiles,jessBalint],agent).

hasGender(andrewDougherty,male).
hasGender(eleanorDougherty,female).
hasGender(meredithMcGhan,female).
hasGender(justinCoslor,male).
hasGender(douglasMiles,male).
hasGender(jessBalint,male).

hasGenderPronoun(male,'his').
hasGenderPronoun(female,'her').

hasEnglishGlossesData(eleanorDougherty,['Eleanor','Ellie','Mom','I lenoir','Mother','Ms. Ellie','mom','ellie','eleanor','mother','ms. ellie','mode','Lenoir','lenoir','my']).
hasEnglishGlossesData(andrewDougherty,['Andrew','Andrew','andy','andrew','Andrew Dougherty','andrew dougherty','Andy Dougherty','andy dougherty','and I','And I','and drew','And Drew','And drew']).
hasEnglishGlossesData(meredithMcGhan,['Meredith','meredith','Babes','babes','of Meredith']).
hasEnglishGlossesData(justinCoslor,['Justin','justin','justin coslor','Justin coslor','Justin Coslor']).
hasEnglishGlossesData(douglasMiles,['Doug','doug','Douglas','douglas','Dmiles','dmiles']).
hasEnglishGlossesData(jessBalint,['Jess','jess','Jbalint','jbalint']).

%% hasIncorrectlyRecognizedGlosses(eleanorDougherty,['mode']).

isa(vanillaPudding,food).
isa(cottageCheese,food).
isa(scrambledEggs,food).

hasEnglishGlossesData(vanillaPudding,['vanilla pudding']).
hasEnglishGlossesData(cottageCheese,['cottage cheese']).
hasEnglishGlossesData(scrambledEggs,['scrambled eggs']).

%% genls(person,intelligentAgent).

%% are([grapes,honeyNutCheerios,yogurtWithMapleSyrup,quinoaWithChickenBrothAndCheese,eggs],food).
isa(crackers,food).
isa(grapes,food).
isa(honeyNutCheerios,food).
isa(yogurtWithMapleSyrup,food).
isa(quinoaWithChickenBrothAndCheese,food).
isa(eggs,food).

hasEnglishGlossesData(crackers,['crackers']).


%% hasEnglishGlossesData(grapes,['grapes']).
hasEnglishGlossesData(honeyNutCheerios,['Honey Nut Cheerios','Cheerios','honey nut cheerios','cheerios']).
hasEnglishGlossesData(yogurtWithMapleSyrup,['Yogurt']).
hasEnglishGlossesData(quinoaWithChickenBrothAndCheese,['Quinoa']).
hasEnglishGlossesData(eggs,['Eggs']).

are([home,kitchen,livingRoom,downstairsComputerRoom,downstairsFoyer,garage],location).
hasEnglishGlossesData(home,['home']).
hasEnglishGlossesData(kitchen,['kitchen']).
hasEnglishGlossesData(downstairsComputerRoom,['computer room','downstairs computer room']).
hasEnglishGlossesData(livingRoom,['living room']).
hasEnglishGlossesData(meredithsRoom,['upstairs room','Meredith''s room']).

are([computerRoomDoor,doorToGarage,garageDoor],door).

door(computerRoomDoor,downstairsComputerRoom,downstairsFoyer).
door(doorToGarage,downstairsFoyer,garage).
door(garageDoor,garage,outside).

isa(tissues,object).
isa(bluetoothKeyboard,object).
hasEnglishGlossesData(tissues,['tissues']).
hasEnglishGlossesData(bluetoothKeyboard,['Bluetooth keyboard','bluetooth keyboard','keyboard']).

%% isa(Y,medication) :-
%% 	hasMedication(X,Y).

mapNLToTerm('Katie',katieONeill).
mapNLToTerm('Katy',katieONeill).
mapNLToTerm('katy',katieONeill).

mapNLToTerm('Jennifer',jenniferDavidson).
mapNLToTerm('Julie',julie).
mapNLToTerm('Gloria',gloriaDultra).


%% genlsDirectlyList([openableState,deviceState],state).

genlsDirectlyList([window,door],openable).

isa(open,openableState).
isa(closed,openableState).

isa(on,deviceState).
isa(off,deviceState).

isa(pro,proOrCon).
isa(con,proOrCon).

isa(oven,device).
isa(stove,device).
isa(centralAirConditioner,device).
%% isa(objectLocatedInFn(vent,downstairsComputerRoom),device).
isa(objectLocatedInFnVentDownstairsComputerRoom,openable).

hasEnglishGlossesData(objectLocatedInFnVentDownstairsComputerRoom,['downstairs air vent']).

isa(kitchenWindow,window).
isa(slidingDoor,window).
isa(livingRoomWindows,window).
isa(landingWindow,window).
isa(computerRoomWindow,window).

hasEnglishGlossesData(garageDoor,['garage door']).

isa(headache,symptom).
isa(coughed,symptom).
isa(choked,symptom).
isa(aspirated,syptom).
%% get something stuck in one's throat

hasEnglishGlossesData(headache,['head ache','headache']).
hasEnglishGlossesData(coughed,['cough','coughed']).
hasEnglishGlossesData(choked,['choke','choked']).
hasEnglishGlossesData(aspirated,['aspirate','aspirated']).

hasEnglishGlossesData(stove,['stove','Stove','stove burner','Stove burner','stove burners','Stove burners']).
hasEnglishGlossesData(oven,['oven','Oven']).
hasEnglishGlossesData(centralAirConditioner,['air conditioner','air conditioning','Air conditioner','Air conditioning','central air conditioner','Central air conditioner','central air conditioning','Central air conditioning','air','Air']).

hasEnglishGlossesData(kitchenWindow,['kitchen window']).
hasEnglishGlossesData(slidingDoor,['sliding door']).
hasEnglishGlossesData(livingRoomWindows,['living room windows']).
hasEnglishGlossesData(landingWindow,['landing window']).
hasEnglishGlossesData(computerRoomWindow,['computer room window']).

functionalInArgs(location,2).

%% hasEnglishGlossesData(Downcased,Y) :-
%% 	hasEnglishGlossesData(X,Y),
%% 	downcase_atom(X,Downcased),
%% 	X \= Downcased.

are([preparedAMeal,vacuumed,didTheDishes,brushedTeeth,paidSomeBills,paidTheRent,spongeBathed,showered],chore).
hasEnglishGlossesData(preparedAMeal,['prepared a meal','cooked','prepared Emile']).
hasEnglishGlossesData(vacuumed,['vacuum','vacuuming','vacuumed']).
hasEnglishGlossesData(didTheDishes,['did the dishes']).
hasEnglishGlossesData(brushedTeeth,['brushed teeth','brush teeth','brush','brushed']).
hasEnglishGlossesData(paidSomeBills,['paid some bills']).
hasEnglishGlossesData(paidTheRent,['paid the rent','paid the rent bill']).
hasEnglishGlossesData(spongeBathed,['sponge bathed','sponge bath']).
hasEnglishGlossesData(showered,['showered','shower']).

are([aiFrdcsaOrg,gameFrdcsaOrg,eleanorFrdcsaOrg,oliverFrdcsaOrg],computer).
hasEnglishGlossesData(aiFrdcsaOrg,['computer','main computer','a I','A I','ai','ai dot frdcsa dot org','andys computer']).
hasEnglishGlossesData(gameFrdcsaOrg,['game','game dot frdcsa dot org']).
hasEnglishGlossesData(eleanorFrdcsaOrg,['eleanor dot frdcsa dot org','moms computer','living room computer','the living room computer','livingroom computer','the livingroom computer']).
hasEnglishGlossesData(oliverFrdcsaOrg,['oliver','merediths computer','oliver dot frdcsa dot org']).

are([nightMusic],playlist).
are([suoGan,sufis,intoTheWest],song).
hasEnglishGlossesData(nightMusic,['night music','Night Music','Night music']).
hasEnglishGlossesData(suoGan,['suo gan','Suo Gan','Suo gan']).
hasEnglishGlossesData(sufis,['indian music','Indian Music','Indian music']).
hasEnglishGlossesData(intoTheWest,['into the west','Into the west','in to the west','In to the west']).

are([happiness,sadness,hunger,pain,haveToGoTheBathroom,tiredness],scalarVariable).
hasEnglishGlossesData(happiness,['happiness']).
hasEnglishGlossesData(sadness,['sadness']).
hasEnglishGlossesData(hunger,['hunger']).
hasEnglishGlossesData(pain,['pain']).
hasEnglishGlossesData(haveToGoTheBathroom,['bathroom','use bathroom','has to go to the bathroom']).
hasEnglishGlossesData(tiredness,['tiredness']).

are([livingRoomBathroom,downstairsComputerRoom],room).
genls(room,location).
hasEnglishGlossesData(livingRoomBathroom,['bathroom','living room bathroom']).
%% hasEnglishGlossesData(downstairsComputerRoom,['computer room','downstairs computer room']).

are([tissues,toiletPaper,paperTowels],product).
%% genls([tissues,toiletPaper,paperTowels],paperCleaningProduct).

hasEnglishGlossesData(tissues,['tissues']).
hasEnglishGlossesData(toiletPaper,['toilet paper']).
hasEnglishGlossesData(paperTowels,['paper towels']).

%% canBeMisrecognizedAs(tissues,['to shoes']).

are([flp,household],planningCapsule).
hasEnglishGlossesData(flp,['flp']).
hasEnglishGlossesData(household,['household']).

are([upstairsUtilityCloset],location).

hasEnglishGlossesData(upstairsUtilityCloset,['upstairs utility closet','utility closet']).

%% come up with substituability logic here.

%% substitutesInContext([creamCheese,butter],cooking).

are([ibuprofen,tylenol,advil,alleve],medicine).
hasEnglishGlossesData(ibuprofen,['Ibuprofen','ibuprofen']).
hasEnglishGlossesData(tylenol,['tylenol','Tylenol']).
hasEnglishGlossesData(advil,['Advil','advil']).
hasEnglishGlossesData(alleve,['Alleve','alleve']).

are([breakfast,lunch,dinner,supper,teaTime,coffeeBreak],mealTime).
hasEnglishGlossesData(breakfast,['breakfast']).
hasEnglishGlossesData(lunch,['lunch']).
hasEnglishGlossesData(dinner,['dinner']).
hasEnglishGlossesData(supper,['supper']).
hasEnglishGlossesData(teaTime,['tea time']).
hasEnglishGlossesData(coffeeBreak,['coffee break']).

are([lasagna],meal).
hasEnglishGlossesData(lasagna,['lasagne']).

are([temperature,humidity,pressure],indoorClimateCondition).
are([temperature,humidity,pressure,windSpeed],outdoorClimateCondition).

hasEnglishGlossesData(temperature,['temperature']).
hasEnglishGlossesData(humidity,['humidity']).
hasEnglishGlossesData(pressure,['pressure']).
hasEnglishGlossesData(windSpeed,['wind speed']).



%% FIXME: have that they are locations inferred
are([home,kitchen,livingRoom,downstairsComputerRoom,meredithsRoom,downstairsFoyer,garage],indoorLocation).
are([auroraIllinois,outside],outdoorLocation).

genlsDirectlyList([indoorLocation,outdoorLocation],location).

hasEnglishGlossesData(outside,['outside','outdoors']).
hasEnglishGlossesData(auroraIllinois,['Aurora','aurora','Aurora, Illinois','aurora, illinois','Aurora Illinois','aurora illinois','aurora Illinois']).

are([phoneNumber,emailAddress,instantMessageAddress],contactInfoType).
hasEnglishGlossesData(phoneNumber,['phone number']).

isa(foxMetro,agent).
hasEnglishGlossesData(foxMetro,['Fox Metro','Fox metro','fox metro']).

hasLocation(centralAirConditioner,townhomeOfEleanorAndAndrewAndMeredith).
hasResidents(townhomeOfEleanorAndAndrewAndMeredith,[eleanorDougherty,andrewDougherty,meredithMcGhan]).

are([aspergers,brainFreezeShort,brainFreezeWhole,heaven,vision,bringEveryoneBackShort,bringEveryoneBackLonger,programmingItself,programmingItselfShort,fox,chen,gloria,idiots,destroyLong,destroyShort,warMarch,phenomenon,rocky,theWorld,blunder,spock,unlimitedPowerLong,unlimitedPowerShort,happiness,producers,gollum,thor,ringOfPower,pruneJuice,vincent,code],mediaLibraryVideoOrSoundClip).

hasEnglishGlossesData(aspergers,[aspergers]).
hasEnglishGlossesData(brainFreezeShort,['brain freeze short']).
hasEnglishGlossesData(brainFreezeWhole,[brainFreezeWhole]).
hasEnglishGlossesData(heaven,[heaven]).
hasEnglishGlossesData(vision,[vision]).
hasEnglishGlossesData(bringEveryoneBackShort,[bringEveryoneBackShort]).
hasEnglishGlossesData(bringEveryoneBackLonger,[bringEveryoneBackLonger]).
hasEnglishGlossesData(programmingItself,[programmingItself]).
hasEnglishGlossesData(programmingItselfShort,[programmingItselfShort]).
hasEnglishGlossesData(fox,[fox]).
hasEnglishGlossesData(chen,[chen]).
hasEnglishGlossesData(gloria,[gloria]).
hasEnglishGlossesData(idiots,[idiots]).
hasEnglishGlossesData(destroyLong,[destroyLong]).
hasEnglishGlossesData(destroyShort,[destroyShort]).
hasEnglishGlossesData(warMarch,['war march','War March','War march','war March','were march']).
hasEnglishGlossesData(happiness,[happiness]).
hasEnglishGlossesData(phenomenon,[phenomenon]).
hasEnglishGlossesData(rocky,[rocky]).
hasEnglishGlossesData(theWorld,[theWorld]).
hasEnglishGlossesData(blunder,[blunder]).
hasEnglishGlossesData(spock,[spock]).
hasEnglishGlossesData(unlimitedPowerLong,[unlimitedPowerLong]).
hasEnglishGlossesData(unlimitedPowerShort,[unlimitedPowerShort]).
hasEnglishGlossesData(happiness,['happiness','Happiness']).
hasEnglishGlossesData(producers,[producers]).
hasEnglishGlossesData(gollum,[gollum]).
hasEnglishGlossesData(thor,[thor]).
hasEnglishGlossesData(ringOfPower,[ringOfPower]).
hasEnglishGlossesData(pruneJuice,[pruneJuice]).
hasEnglishGlossesData(vincent,[vincent]).
hasEnglishGlossesData(code,[code]).

hasClipBaseName('aspergers','Andy--The-Aspergers-is-strong-with-this-one.wav').
hasClipBaseName('brainFreezeShort','Cat brain freeze - Last cat full.mp4').
hasClipBaseName('brainFreezeWhole','Cat brain freeze.mp4').
hasClipBaseName('heaven','catholic vs protestant heaven simpsons.mp4').
hasClipBaseName('vision','Contact Monologue-wnkEace3rb4.mp4').
hasClipBaseName('bringEveryoneBackShort','I-can-bring-everyone-back.mp4').
hasClipBaseName('bringEveryoneBackLonger','I-can-bring-everyone-back--slightly-longer.mp4').
hasClipBaseName('programmingItself','Explorers-Its-programming-itself-long.mkv').
hasClipBaseName('programmingItselfShort','Explorers-Its-programming-itself-short.mkv').
hasClipBaseName('fox','Fantastic Mr  Fox eating-rAOJJ15hHhk.mp4').
hasClipBaseName('chen','Fist of Fury - Chen was right.mp4').
hasClipBaseName('gloria','Gloria - Laura Branigan 1982.mp4').
hasClipBaseName('idiots','Guardian-of-the-Galaxy--Im-going-to-die.mp4').
hasClipBaseName('destroyLong','Destroy-long.avi').
hasClipBaseName('destroyShort','Destroy-short.avi').
hasClipBaseName('warMarch','ONeillsMarch-1.mp3').
hasClipBaseName('phenomenon','Phenomenon-He-didnt-want-anything-from-anybody.mkv').
hasClipBaseName('rocky','01 - Gonna Fly Now (Theme From Rocky).mp3').
hasClipBaseName('theWorld','The Same Thing We Do Every Night... - A Pinky and The Brain Compilation.mp4').
hasClipBaseName('blunder','Sanjuro--I-cant-watch-you-blunder-your-way.mp4').
hasClipBaseName('spock','For-the-love-of-Spock--youd-make-a-splendid-computer.mkv').
hasClipBaseName('unlimitedPowerLong','Star-Wars-Episode-3--Unlimited-Power-long.mp4').
hasClipBaseName('unlimitedPowerShort','Star-Wars-Episode-3--Unlimited-Power-short.mp4').
hasClipBaseName('happiness','Surfs Up - Can you see the happiness emanating from me.mp4').
hasClipBaseName('producers','The Producers - They find me.mp4').
hasClipBaseName('gollum','The Two Towers Soundtrack-19-Gollum''s Song.mp4').
hasClipBaseName('thor','thor--it-all-makes-sense-now.mp4').
hasClipBaseName('ringOfPower','Download Lotr The Fellowship Of The Ring  Extended Edition  The Mirror Of Galadriel The Extended Edi-Il50k7KEXgQ.mp4').
hasClipBaseName('pruneJuice','transformers-1-grandmama-drink-your-prune-juice.mp4').
hasClipBaseName('vincent','Chloe Agnew - Vincent (Starry, Starry Night).mp4').
hasClipBaseName('code','Wreck-It-Ralph--Its-nothing-but-code-now.mp4').

hasEnglishGlossesData(recurrence(event),['recurring event']).
hasEnglishGlossesData(planStep(start),['plan step start']).
hasEnglishGlossesData(planStep(end),['plan step end']).

isa(townhomeOfEleanorAndAndrewAndMeredith,location).
hasEnglishGlossesData(townhomeOfEleanorAndAndrewAndMeredith,['Townhome']).
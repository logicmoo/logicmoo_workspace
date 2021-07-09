% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',40).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl')).
% Fri, 26 Mar 2021 01:06:10 GMT File: <stream>(0x555567c82f00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @article{Mueller:2004c,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Understanding script-based stories using commonsense reasoning",
%;   journal = "Cognitive Systems Research",
%;   volume = "5",
%;   number = "4",
%;   pages = "307--340",
%; }
%;

% option modeldiff on
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',22).
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

% ignore Love, ThreatenedBy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:- call_pel_directive(ignore(floor)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:- call_pel_directive(ignore(groundOf)).

% ignore Near, WalkFrom, WalkFromTo, RunFromTo
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFrom))).
:- call_pel_directive(ignore(walkFrom)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFromTo))).
:- call_pel_directive(ignore(walkFromTo)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(runFromTo))).
:- call_pel_directive(ignore(runFromTo)).

% ignore Side1, Side2
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side1))).
:- call_pel_directive(ignore(side1)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side2))).
:- call_pel_directive(ignore(side2)).

% load foundations/Root.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',29).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',29).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

% load answers/Mueller2003/Ontology.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',31).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',31).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

% load answers/Mueller2004c/OTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e')).

% load answers/Mueller2004c/Container.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Container.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Container.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

% load answers/Mueller2004c/SmallFire.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/SmallFire.e'))).
:- call_pel_directive(load('answers/Mueller2004c/SmallFire.e')).

% load answers/Mueller2004c/Smoke.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',37).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Smoke.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Smoke.e')).

% location Location1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',39).
% From E: 
% 
% t(location,location1).
isa(location1, location).

% portal DummyPortal1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',41).
% From E: 
% 
% t(portal,dummyPortal1).
isa(dummyPortal1, portal).

% agent Smoker1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',43).
% From E: 
% 
% t(agent,smoker1).
isa(smoker1, agent).

% cigarette Cigarette1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',45).
% From E: 
% 
% t(cigarette,cigarette1).
isa(cigarette1, cigarette).

% container Package1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',47).
% From E: 
% 
% t(container,package1).
isa(package1, container).

% physobj Surface1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',49).
% From E: 
% 
% t(physobj,surface1).
isa(surface1, physobj).

% physobj LightingDevice1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',51).
% From E: 
% 
% t(physobj,lightingDevice1).
isa(lightingDevice1, physobj).

% ashtray AshTray1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',53).
% From E: 
% 
% t(ashtray,ashTray1).
isa(ashTray1, ashtray).

% physobj Trash1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',55).
% From E: 
% 
% t(physobj,trash1).
isa(trash1, physobj).

% smoke Smoke1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',57).
% From E: 
% 
% t(smoke,smoke1).
isa(smoke1, smoke).
%; prune

% sort ona, onb
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',59).
% From E: 
% 
% sort(ona).
sort(ona).
% From E: 
% 
% sort(onb).
sort(onb).

% fluent! On(ona,onb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',61).
% From E: 
% 
% fluent(on(ona,onb)).
mpred_prop(on(ona, onb), fluent).
fluents([on/2]).

% event! PlaceOn(agent,ona,onb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',61).
% From E: 
% 
% event(placeOn(agent,ona,onb)).
events([placeOn/3]).
mpred_prop(placeOn(agent, ona, onb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',61).
actions([placeOn/3]).

% event! TakeOffOf(agent,ona,onb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',63).
% From E: 
% 
% event(takeOffOf(agent,ona,onb)).
events([takeOffOf/3]).
mpred_prop(takeOffOf(agent, ona, onb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',63).
actions([takeOffOf/3]).

% ona! LightingDevice1, Package1, Cigarette1
% From E: 
% 
% t(ona,lightingDevice1).
isa(lightingDevice1, ona).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',63).
% From E: 
% 
% t(ona,package1).
isa(package1, ona).
% From E: 
% 
% t(ona,cigarette1).
isa(cigarette1, ona).

% onb! Surface1, AshTray1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',65).
% From E: 
% 
% t(onb,surface1).
isa(surface1, onb).
% From E: 
% 
% t(onb,ashTray1).
isa(ashTray1, onb).

% sort insidea, insideb
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',67).
% From E: 
% 
% sort(insidea).
sort(insidea).
% From E: 
% 
% sort(insideb).
sort(insideb).

% fluent! Inside(insidea,insideb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',67).
% From E: 
% 
% fluent(inside(insidea,insideb)).
mpred_prop(inside(insidea, insideb), fluent).
fluents([inside/2]).

% event! PutInside(agent,insidea,insideb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',69).
% From E: 
% 
% event(putInside(agent,insidea,insideb)).
events([putInside/3]).
mpred_prop(putInside(agent, insidea, insideb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',69).
actions([putInside/3]).

% event! TakeOutOf(agent,insidea,insideb)
% From E: 
% 
% event(takeOutOf(agent,insidea,insideb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',69).
events([takeOutOf/3]).
mpred_prop(takeOutOf(agent, insidea, insideb), action).
actions([takeOutOf/3]).

% insidea! Cigarette1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',71).
% From E: 
% 
% t(insidea,cigarette1).
isa(cigarette1, insidea).

% insideb! Package1, Trash1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',71).
% From E: 
% 
% t(insideb,package1).
isa(package1, insideb).
% From E: 
% 
% t(insideb,trash1).
isa(trash1, insideb).

% sort lighta, lightb, lightc
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',74).
% From E: 
% 
% sort(lighta).
sort(lighta).
% From E: 
% 
% sort(lightb).
sort(lightb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',74).
% From E: 
% 
% sort(lightc).
sort(lightc).

% event! LightWith(lighta,lightb,lightc)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',74).
% From E: 
% 
% event(lightWith(lighta,lightb,lightc)).
mpred_prop(lightWith(lighta, lightb, lightc), event).
events([lightWith/3]).

% lighta! Smoker1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',76).
% From E: 
% 
% t(lighta,smoker1).
isa(smoker1, lighta).

% lightb! Cigarette1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',76).
% From E: 
% 
% t(lightb,cigarette1).
isa(cigarette1, lightb).

% lightc! LightingDevice1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',78).
% From E: 
% 
% t(lightc,lightingDevice1).
isa(lightingDevice1, lightc).
%; initial state
% [agent,object]
 % !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',80).
% From E: 
% 
% holds(
%    not(holding(Agent,Object)), 0).
initially not holding(Agent, Object).
 %  initial_state([not(holding(Agent,Object))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',82).
% [agent,physobj]
 % !HoldsAt(SittingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(sittingOn(Agent,Physobj)), 0).
initially not sittingOn(Agent, Physobj).
 %  initial_state([not(sittingOn(Agent,Physobj))]).
 %  % =================================.


% [agent,physobj]
 % !HoldsAt(LyingOn(agent,physobj),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',82).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
initially not lyingOn(Agent, Physobj).
 %  initial_state([not(lyingOn(Agent,Physobj))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',84).
% HoldsAt(On(Package1,Surface1),0).
% From E: 
% 
% holds(
%    on(package1,surface1), 0).
initially on(package1, surface1).
 %  initial_state([on(package1,surface1)]).
 %  % =================================.


% [physobj1,physobj2]
% !(physobj1=Package1 & physobj2=Surface1) ->
% !HoldsAt(On(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',86).
% From E: 
% 
% '->'(
%    not(','(
%           Physobj1=package1, 
%           Physobj2=surface1)), 
%    holds(
%       not(on(Physobj1,Physobj2)), 0)).
on(Physobj1, Physobj2)at 0 if equals(Physobj1, package1), equals(Physobj2, surface1).

 /*  l_int(holds(on(Physobj1,Physobj2),0),
           [equals(Physobj1,package1),equals(Physobj2,surface1)]).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',88).
% HoldsAt(Dressed(Smoker1),0).
% From E: 
% 
% holds(
%    dressed(smoker1), 0).
initially dressed(smoker1).
 %  initial_state([dressed(smoker1)]).
 %  % =================================.


% HoldsAt(Awake(Smoker1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',88).
% From E: 
% 
% holds(
%    awake(smoker1), 0).
initially awake(smoker1).
 %  initial_state([awake(smoker1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',90).
% HoldsAt(Sleep3(Smoker1),0).
% From E: 
% 
% holds(
%    sleep3(smoker1), 0).
initially sleep3(smoker1).
 %  initial_state([sleep3(smoker1)]).
 %  % =================================.


% HoldsAt(Standing(Smoker1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',90).
% From E: 
% 
% holds(
%    standing(smoker1), 0).
initially standing(smoker1).
 %  initial_state([standing(smoker1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',92).
% HoldsAt(CraveNicotine(Smoker1),0).
% From E: 
% 
% holds(
%    craveNicotine(smoker1), 0).
initially craveNicotine(smoker1).
 %  initial_state([craveNicotine(smoker1)]).
 %  % =================================.


% HoldsAt(ContainerClosed(Package1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',92).
% From E: 
% 
% holds(
%    containerClosed(package1), 0).
initially containerClosed(package1).
 %  initial_state([containerClosed(package1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',94).
% [physobj]
 % !HoldsAt(IsBurning(physobj),0).
% From E: 
% 
% holds(
%    not(isBurning(Physobj)), 0).
initially not isBurning(Physobj).
 %  initial_state([not(isBurning(Physobj))]).
 %  % =================================.


% HoldsAt(Inside(Cigarette1,Package1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',94).
% From E: 
% 
% holds(
%    inside(cigarette1,package1), 0).
initially inside(cigarette1, package1).
 %  initial_state([inside(cigarette1,package1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',96).
% [physobj1,physobj2]
% !(physobj1=Cigarette1 & physobj2=Package1) ->
% !HoldsAt(Inside(physobj1, physobj2),0).
% From E: 
% 
% '->'(
%    not(','(
%           Physobj1=cigarette1, 
%           Physobj2=package1)), 
%    holds(
%       not(inside(Physobj1,Physobj2)), 0)).
inside(Physobj1, Physobj2)at 0 if equals(Physobj1, cigarette1), equals(Physobj2, package1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',96).

 /*  l_int(holds(inside(Physobj1,Physobj2),0),
           [ equals(Physobj1,cigarette1),
     	equals(Physobj2,package1)
           ]).
 */
 %  % =================================.


%; narrative


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',101).
% Happens(TakeOffOf(Smoker1,Package1,Surface1),0).
% From E: 
% 
% happens(
%    takeOffOf(smoker1,package1,surface1), 0).
observe takeOffOf(smoker1, package1, surface1)at 0.
 %  observe([takeOffOf(smoker1,package1,surface1)],0).
 %  % =================================.


% Happens(ContainerOpen(Smoker1,Package1),1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',101).
% From E: 
% 
% happens(
%    containerOpen(smoker1,package1), 1).
observe containerOpen(smoker1, package1)at 1.
 %  observe([containerOpen(smoker1,package1)],1).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',103).
% Happens(TakeOutOf(Smoker1,Cigarette1,Package1),2).
% From E: 
% 
% happens(
%    takeOutOf(smoker1,cigarette1,package1), 2).
observe takeOutOf(smoker1, cigarette1, package1)at 2.
 %  observe([takeOutOf(smoker1,cigarette1,package1)],2).
 %  % =================================.


% Happens(PickUp(Smoker1,LightingDevice1),3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',103).
% From E: 
% 
% happens(
%    pickUp(smoker1,lightingDevice1), 3).
observe pickUp(smoker1, lightingDevice1)at 3.
 %  observe([pickUp(smoker1,lightingDevice1)],3).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',105).
% Happens(Light(Smoker1,LightingDevice1),4).
% From E: 
% 
% happens(
%    light(smoker1,lightingDevice1), 4).
observe light(smoker1, lightingDevice1)at 4.
 %  observe([light(smoker1,lightingDevice1)],4).
 %  % =================================.


% Happens(LightWith(Smoker1,Cigarette1,LightingDevice1),5).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',105).
% From E: 
% 
% happens(
%    lightWith(smoker1,cigarette1,lightingDevice1), 5).
observe lightWith(smoker1, cigarette1, lightingDevice1)at 5.
 %  observe([lightWith(smoker1,cigarette1,lightingDevice1)],5).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',107).
% Happens(BlowOut(Smoker1,LightingDevice1),6).
% From E: 
% 
% happens(
%    blowOut(smoker1,lightingDevice1), 6).
observe blowOut(smoker1, lightingDevice1)at 6.
 %  observe([blowOut(smoker1,lightingDevice1)],6).
 %  % =================================.


% Happens(PlaceOn(Smoker1,LightingDevice1,Surface1),7).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',107).
% From E: 
% 
% happens(
%    placeOn(smoker1,lightingDevice1,surface1), 7).
observe placeOn(smoker1, lightingDevice1, surface1)at 7.
 %  observe([placeOn(smoker1,lightingDevice1,surface1)],7).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',109).
% Happens(PlaceOn(Smoker1,Package1,Surface1),8).
% From E: 
% 
% happens(
%    placeOn(smoker1,package1,surface1), 8).
observe placeOn(smoker1, package1, surface1)at 8.
 %  observe([placeOn(smoker1,package1,surface1)],8).
 %  % =================================.


% Happens(Puff(Smoker1,Cigarette1),9).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',109).
% From E: 
% 
% happens(
%    puff(smoker1,cigarette1), 9).
observe puff(smoker1, cigarette1)at 9.
 %  observe([puff(smoker1,cigarette1)],9).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',111).
% Happens(BlowOutSmoke(Smoker1,Smoke1),10).
% From E: 
% 
% happens(
%    blowOutSmoke(smoker1,smoke1), 10).
observe blowOutSmoke(smoker1, smoke1)at 10.
 %  observe([blowOutSmoke(smoker1,smoke1)],10).
 %  % =================================.


% Happens(PlaceOn(Smoker1,Cigarette1,AshTray1),11).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',111).
% From E: 
% 
% happens(
%    placeOn(smoker1,cigarette1,ashTray1), 11).
observe placeOn(smoker1, cigarette1, ashTray1)at 11.
 %  observe([placeOn(smoker1,cigarette1,ashTray1)],11).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',113).
% Happens(TakeOffOf(Smoker1,Cigarette1,AshTray1),12).
% From E: 
% 
% happens(
%    takeOffOf(smoker1,cigarette1,ashTray1), 12).
observe takeOffOf(smoker1, cigarette1, ashTray1)at 12.
 %  observe([takeOffOf(smoker1,cigarette1,ashTray1)],12).
 %  % =================================.


% Happens(Puff(Smoker1,Cigarette1),13).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',113).
% From E: 
% 
% happens(
%    puff(smoker1,cigarette1), 13).
observe puff(smoker1, cigarette1)at 13.
 %  observe([puff(smoker1,cigarette1)],13).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',115).
% Happens(PutOut(Smoker1,Cigarette1),14).
% From E: 
% 
% happens(
%    putOut(smoker1,cigarette1), 14).
observe putOut(smoker1, cigarette1)at 14.
 %  observe([putOut(smoker1,cigarette1)],14).
 %  % =================================.


% Happens(PutInside(Smoker1,Cigarette1,Trash1),15).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',115).
% From E: 
% 
% happens(
%    putInside(smoker1,cigarette1,trash1), 15).
observe putInside(smoker1, cigarette1, trash1)at 15.
 %  observe([putInside(smoker1,cigarette1,trash1)],15).
 %  % =================================.

% range time 0 16
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',117).
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,16))).
:- call_pel_directive(range(time, 0, 16)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',119).
% range offset 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

% range diameter 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',119).
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',122).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',124).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.lps.pl')).

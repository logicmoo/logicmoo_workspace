% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',279).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl')).
% Fri, 26 Mar 2021 01:06:07 GMT File: <stream>(0x555567c95100)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @article{Mueller:InPress,
%;   author = "Erik T. Mueller",
%;   year = "in press",
%;   title = "Modelling space and time in narratives about restaurants",
%;   journal = "Literary and Linguistic Computing",
%; }
%;

% option renaming off
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',19).
% From E: 
% 
% ':-'(call_pel_directive(option(renaming,off))).
:- call_pel_directive(option(renaming, off)).

% option encoding 3
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',19).
% From E: 
% 
% ':-'(call_pel_directive(option(encoding,3))).
:- call_pel_directive(option(encoding, 3)).

% load foundations/Root.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',22).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',22).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

% load answers/Mueller2003/Ontology.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',24).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/MuellerInPress/RepRest.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',24).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/MuellerInPress/RepRest.e'))).
:- call_pel_directive(load('answers/MuellerInPress/RepRest.e')).

% door MainEntrance1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',27).
% From E: 
% 
% t(door,mainEntrance1).
isa(mainEntrance1, door).
%; room-scale topological space

% outside Street1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',29).
% From E: 
% 
% t(outside,street1).
isa(street1, outside).

% room DiningRoom1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',31).
% From E: 
% 
% t(room,diningRoom1).
isa(diningRoom1, room).

% door KitchenDoor1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',31).
% From E: 
% 
% t(door,kitchenDoor1).
isa(kitchenDoor1, door).

% room Kitchen1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',33).
% From E: 
% 
% t(room,kitchen1).
isa(kitchen1, room).


% Side1(MainEntrance1)=Street1.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',33).
% From E: 
% 
% '='(
%    side1(mainEntrance1), 
%    street1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',33).
side1(mainEntrance1,street1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',35).
% Side2(MainEntrance1)=DiningRoom1.
% From E: 
% 
% '='(
%    side2(mainEntrance1), 
%    diningRoom1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',35).
side2(mainEntrance1,diningRoom1).


% Side1(KitchenDoor1)=DiningRoom1.
% From E: 
% 
% '='(
%    side1(kitchenDoor1), 
%    diningRoom1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',35).
side1(kitchenDoor1,diningRoom1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',37).
% Side2(KitchenDoor1)=Kitchen1.
% From E: 
% 
% '='(
%    side2(kitchenDoor1), 
%    kitchen1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',37).
side2(kitchenDoor1,kitchen1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',39).
% agent Customer1
% From E: 
% 
% t(agent,customer1).
isa(customer1, agent).

% menu Menu1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',39).
% From E: 
% 
% t(menu,menu1).
isa(menu1, menu).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',41).
% chair Chair1
% From E: 
% 
% t(chair,chair1).
isa(chair1, chair).

% food Food1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',41).
% From E: 
% 
% t(food,food1).
isa(food1, food).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',43).
% HoldsAt(At(Customer1,Street1),0).
% From E: 
% 
% holds(
%    at_loc(customer1,street1), 0).
initially at_loc(customer1, street1).
 %  initial_state([at_loc(customer1,street1)]).
 %  % =================================.


% HoldsAt(Hungry(Customer1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',43).
% From E: 
% 
% holds(
%    hungry(customer1), 0).
initially hungry(customer1).
 %  initial_state([hungry(customer1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',45).
% HoldsAt(At(Chair1,DiningRoom1),0).
% From E: 
% 
% holds(
%    at_loc(chair1,diningRoom1), 0).
initially at_loc(chair1, diningRoom1).
 %  initial_state([at_loc(chair1,diningRoom1)]).
 %  % =================================.


% HoldsAt(At(Menu1,DiningRoom1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',45).
% From E: 
% 
% holds(
%    at_loc(menu1,diningRoom1), 0).
initially at_loc(menu1, diningRoom1).
 %  initial_state([at_loc(menu1,diningRoom1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',47).
% HoldsAt(On(Menu1,Table1),0).
% From E: 
% 
% holds(
%    on(menu1,table1), 0).
initially on(menu1, table1).
 %  initial_state([on(menu1,table1)]).
 %  % =================================.


% HoldsAt(At(Food1,Kitchen1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',47).
% From E: 
% 
% holds(
%    at_loc(food1,kitchen1), 0).
initially at_loc(food1, kitchen1).
 %  initial_state([at_loc(food1,kitchen1)]).
 %  % =================================.

% waiter Waiter1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',49).
% From E: 
% 
% t(waiter,waiter1).
isa(waiter1, waiter).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',51).
% cook Cook1
% From E: 
% 
% t(cook,cook1).
isa(cook1, cook).
%; props

% table Table1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',53).
% From E: 
% 
% t(table,table1).
isa(table1, table).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',55).
% bill Bill1
% From E: 
% 
% t(bill,bill1).
isa(bill1, bill).
%; restaurant

% restaurant Restaurant1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',57).
% From E: 
% 
% t(restaurant,restaurant1).
isa(restaurant1, restaurant).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',59).
% CookOf(Restaurant1)=Cook1.
% From E: 
% 
% '='(
%    cookOf(restaurant1), 
%    cook1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',59).
cookOf(restaurant1,cook1).


% TableOf(Restaurant1)=Table1.
% From E: 
% 
% '='(
%    tableOf(restaurant1), 
%    table1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',59).
tableOf(restaurant1,table1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',61).
% WaiterOf(Restaurant1)=Waiter1.
% From E: 
% 
% '='(
%    waiterOf(restaurant1), 
%    waiter1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',61).
waiterOf(restaurant1,waiter1).


% KitchenDoorOf(Restaurant1)=KitchenDoor1.
% From E: 
% 
% '='(
%    kitchenDoorOf(restaurant1), 
%    kitchenDoor1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',61).
kitchenDoorOf(restaurant1,kitchenDoor1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',63).
% BillOf(Restaurant1)=Bill1.
% From E: 
% 
% '='(
%    billOf(restaurant1), 
%    bill1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',63).
billOf(restaurant1,bill1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',65).
%; prune

% sort ona, onb
% From E: 
% 
% sort(ona).
sort(ona).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',65).
% From E: 
% 
% sort(onb).
sort(onb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',67).
% fluent! On(ona,onb)
% From E: 
% 
% fluent(on(ona,onb)).
mpred_prop(on(ona, onb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',67).
fluents([on/2]).

% event! PlaceOn(agent,ona,onb)
% From E: 
% 
% event(placeOn(agent,ona,onb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',67).
events([placeOn/3]).
mpred_prop(placeOn(agent, ona, onb), action).
actions([placeOn/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',69).
% event! TakeOffOf(agent,ona,onb)
% From E: 
% 
% event(takeOffOf(agent,ona,onb)).
events([takeOffOf/3]).
mpred_prop(takeOffOf(agent, ona, onb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',69).
actions([takeOffOf/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',71).
% sort ordera, orderb, orderc
% From E: 
% 
% sort(ordera).
sort(ordera).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',71).
% From E: 
% 
% sort(orderb).
sort(orderb).
% From E: 
% 
% sort(orderc).
sort(orderc).

% event! Order(ordera,orderb,orderc)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',71).
% From E: 
% 
% event(order(ordera,orderb,orderc)).
mpred_prop(order(ordera, orderb, orderc), event).
events([order/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',73).
% fluent! KnowOrder(orderb,ordera,orderc)
% From E: 
% 
% fluent(knowOrder(orderb,ordera,orderc)).
mpred_prop(knowOrder(orderb, ordera, orderc), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',73).
fluents([knowOrder/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',75).
% sort requesta, requestb, requestc
% From E: 
% 
% sort(requesta).
sort(requesta).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',75).
% From E: 
% 
% sort(requestb).
sort(requestb).
% From E: 
% 
% sort(requestc).
sort(requestc).

% event! Request(requesta,requestb,requestc)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',75).
% From E: 
% 
% event(request(requesta,requestb,requestc)).
mpred_prop(request(requesta, requestb, requestc), event).
events([request/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',77).
% fluent! KnowRequest(requestb,requesta,requestc)
% From E: 
% 
% fluent(knowRequest(requestb,requesta,requestc)).
mpred_prop(knowRequest(requestb, requesta, requestc), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',77).
fluents([knowRequest/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',79).
% sort holda, holdb, holdc
% From E: 
% 
% sort(holda).
sort(holda).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',79).
% From E: 
% 
% sort(holdb).
sort(holdb).
% From E: 
% 
% sort(holdc).
sort(holdc).

% event! TakeOffOf(holda,holdb,holdc)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',79).
% From E: 
% 
% event(takeOffOf(holda,holdb,holdc)).
mpred_prop(takeOffOf(holda, holdb, holdc), event).
events([takeOffOf/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',81).
% event! PickUp(holda,holdb)
% From E: 
% 
% event(pickUp(holda,holdb)).
mpred_prop(pickUp(holda, holdb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',81).
events([pickUp/2]).

% event! LetGoOf(holda,holdb)
% From E: 
% 
% event(letGoOf(holda,holdb)).
mpred_prop(letGoOf(holda, holdb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',81).
events([letGoOf/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',83).
% event! Hold(holda,holdb)
% From E: 
% 
% event(hold(holda,holdb)).
mpred_prop(hold(holda, holdb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',83).
events([hold/2]).

% fluent! Holding(holda,holdb)
% From E: 
% 
% fluent(holding(holda,holdb)).
mpred_prop(holding(holda, holdb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',83).
fluents([holding/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',86).
% sort sita, sitb
% From E: 
% 
% sort(sita).
sort(sita).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',86).
% From E: 
% 
% sort(sitb).
sort(sitb).

% event! LieOn(sita,sitb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',86).
% From E: 
% 
% event(lieOn(sita,sitb)).
mpred_prop(lieOn(sita, sitb), event).
events([lieOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',88).
% event! SitOn(sita,sitb)
% From E: 
% 
% event(sitOn(sita,sitb)).
mpred_prop(sitOn(sita, sitb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',88).
events([sitOn/2]).

% event! RiseFrom(sita,sitb)
% From E: 
% 
% event(riseFrom(sita,sitb)).
mpred_prop(riseFrom(sita, sitb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',88).
events([riseFrom/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',90).
% fluent! LyingOn(sita,sitb)
% From E: 
% 
% fluent(lyingOn(sita,sitb)).
mpred_prop(lyingOn(sita, sitb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',90).
fluents([lyingOn/2]).

% fluent! SittingOn(sita,sitb)
% From E: 
% 
% fluent(sittingOn(sita,sitb)).
mpred_prop(sittingOn(sita, sitb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',90).
fluents([sittingOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',93).
% sort greeta, greetb
% From E: 
% 
% sort(greeta).
sort(greeta).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',93).
% From E: 
% 
% sort(greetb).
sort(greetb).

% event! Greet(greeta,greetb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',93).
% From E: 
% 
% event(greet(greeta,greetb)).
mpred_prop(greet(greeta, greetb), event).
events([greet/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',96).
% ona! Menu1, Food1, Bill1
% From E: 
% 
% t(ona,menu1).
isa(menu1, ona).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',96).
% From E: 
% 
% t(ona,food1).
isa(food1, ona).
% From E: 
% 
% t(ona,bill1).
isa(bill1, ona).

% onb! Table1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',96).
% From E: 
% 
% t(onb,table1).
isa(table1, onb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',98).
% ordera! Customer1, Waiter1
% From E: 
% 
% t(ordera,customer1).
isa(customer1, ordera).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',98).
% From E: 
% 
% t(ordera,waiter1).
isa(waiter1, ordera).

% orderb! Waiter1, Cook1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',98).
% From E: 
% 
% t(orderb,waiter1).
isa(waiter1, orderb).
% From E: 
% 
% t(orderb,cook1).
isa(cook1, orderb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',100).
% orderc! Food1
% From E: 
% 
% t(orderc,food1).
isa(food1, orderc).

% requesta! Customer1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',100).
% From E: 
% 
% t(requesta,customer1).
isa(customer1, requesta).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',102).
% requestb! Waiter1
% From E: 
% 
% t(requestb,waiter1).
isa(waiter1, requestb).

% requestc! Bill1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',102).
% From E: 
% 
% t(requestc,bill1).
isa(bill1, requestc).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',104).
% holda! Customer1, Waiter1
% From E: 
% 
% t(holda,customer1).
isa(customer1, holda).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',104).
% From E: 
% 
% t(holda,waiter1).
isa(waiter1, holda).

% holdb! Menu1, Food1, Bill1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',104).
% From E: 
% 
% t(holdb,menu1).
isa(menu1, holdb).
% From E: 
% 
% t(holdb,food1).
isa(food1, holdb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',104).
% From E: 
% 
% t(holdb,bill1).
isa(bill1, holdb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',106).
% holdc! Table1
% From E: 
% 
% t(holdc,table1).
isa(table1, holdc).

% sita! Customer1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',106).
% From E: 
% 
% t(sita,customer1).
isa(customer1, sita).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',108).
% sitb! Chair1
% From E: 
% 
% t(sitb,chair1).
isa(chair1, sitb).

% greeta! Customer1, Waiter1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',108).
% From E: 
% 
% t(greeta,customer1).
isa(customer1, greeta).
% From E: 
% 
% t(greeta,waiter1).
isa(waiter1, greeta).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',110).
% greetb! Customer1, Waiter1
% From E: 
% 
% t(greetb,customer1).
isa(customer1, greetb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',110).
% From E: 
% 
% t(greetb,waiter1).
isa(waiter1, greetb).
%; initial situation


% HoldsAt(At(Waiter1,DiningRoom1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',112).
% From E: 
% 
% holds(
%    at_loc(waiter1,diningRoom1), 0).
initially at_loc(waiter1, diningRoom1).
 %  initial_state([at_loc(waiter1,diningRoom1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',114).
% HoldsAt(At(Cook1,Kitchen1),0).
% From E: 
% 
% holds(
%    at_loc(cook1,kitchen1), 0).
initially at_loc(cook1, kitchen1).
 %  initial_state([at_loc(cook1,kitchen1)]).
 %  % =================================.


% HoldsAt(At(Table1,DiningRoom1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',114).
% From E: 
% 
% holds(
%    at_loc(table1,diningRoom1), 0).
initially at_loc(table1, diningRoom1).
 %  initial_state([at_loc(table1,diningRoom1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',116).
% !HoldsAt(On(Bill1,Table1),0).
% From E: 
% 
% holds(
%    not(on(bill1,table1)), 0).
initially not on(bill1, table1).
 %  initial_state([not(on(bill1,table1))]).
 %  % =================================.


% HoldsAt(At(Bill1,DiningRoom1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',116).
% From E: 
% 
% holds(
%    at_loc(bill1,diningRoom1), 0).
initially at_loc(bill1, diningRoom1).
 %  initial_state([at_loc(bill1,diningRoom1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',118).
% [agent]
 % HoldsAt(Standing(agent),0).
% From E: 
% 
% holds(
%    standing(Agent), 0).
initially standing(Agent).
 %  initial_state([standing(Agent)]).
 %  % =================================.


% [agent,object]
 % !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',118).
% From E: 
% 
% holds(
%    not(holding(Agent,Object)), 0).
initially not holding(Agent, Object).
 %  initial_state([not(holding(Agent,Object))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',120).
% [agent1,agent2,physobj]
 % !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
% From E: 
% 
% holds(
%    not(knowOrder(Agent1,Agent2,Physobj)), 0).
initially not knowOrder(Agent1, Agent2, Physobj).
 %  initial_state([not(knowOrder(Agent1,Agent2,Physobj))]).
 %  % =================================.


% [agent1,agent2,physobj]
 % !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',120).
% From E: 
% 
% holds(
%    not(knowRequest(Agent1,Agent2,Physobj)), 0).
initially not knowRequest(Agent1, Agent2, Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',120).

 /*  initial_state([ not(knowRequest(Agent1,
     				Agent2,
     				Physobj))
     	      ]).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',122).
% HoldsAt(BeWaiter0(Waiter1),0).
% From E: 
% 
% holds(
%    beWaiter0(waiter1), 0).
initially beWaiter0(waiter1).
 %  initial_state([beWaiter0(waiter1)]).
 %  % =================================.


% HoldsAt(BeCook0(Cook1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',122).
% From E: 
% 
% holds(
%    beCook0(cook1), 0).
initially beCook0(cook1).
 %  initial_state([beCook0(cook1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',124).
% [food]
 % !HoldsAt(FoodPrepared(food),0).
% From E: 
% 
% holds(
%    not(foodPrepared(Food)), 0).
initially not foodPrepared(Food).
 %  initial_state([not(foodPrepared(Food))]).
 %  % =================================.


% !HoldsAt(Hungry(Cook1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',124).
% From E: 
% 
% holds(
%    not(hungry(cook1)), 0).
initially not hungry(cook1).
 %  initial_state([not(hungry(cook1))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',126).
% !HoldsAt(Hungry(Waiter1),0).
% From E: 
% 
% holds(
%    not(hungry(waiter1)), 0).
initially not hungry(waiter1).
 %  initial_state([not(hungry(waiter1))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',128).
% Happens(WalkThroughDoor12(Customer1,MainEntrance1),0).
% From E: 
% 
% happens(
%    walkThroughDoor12(customer1,mainEntrance1), 0).
observe walkThroughDoor12(customer1, mainEntrance1)at 0.
 %  observe([walkThroughDoor12(customer1,mainEntrance1)],0).
 %  % =================================.


% Happens(Greet(Waiter1,Customer1),1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',128).
% From E: 
% 
% happens(
%    greet(waiter1,customer1), 1).
observe greet(waiter1, customer1)at 1.
 %  observe([greet(waiter1,customer1)],1).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',130).
% Happens(SitOn(Customer1,Chair1),2).
% From E: 
% 
% happens(
%    sitOn(customer1,chair1), 2).
observe sitOn(customer1, chair1)at 2.
 %  observe([sitOn(customer1,chair1)],2).
 %  % =================================.


% Happens(TakeOffOf(Customer1,Menu1,Table1),3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',130).
% From E: 
% 
% happens(
%    takeOffOf(customer1,menu1,table1), 3).
observe takeOffOf(customer1, menu1, table1)at 3.
 %  observe([takeOffOf(customer1,menu1,table1)],3).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',132).
% Happens(Order(Customer1,Waiter1,Food1),4).
% From E: 
% 
% happens(
%    order(customer1,waiter1,food1), 4).
observe order(customer1, waiter1, food1)at 4.
 %  observe([order(customer1,waiter1,food1)],4).
 %  % =================================.


% Happens(PlaceOn(Customer1,Menu1,Table1),5).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',132).
% From E: 
% 
% happens(
%    placeOn(customer1,menu1,table1), 5).
observe placeOn(customer1, menu1, table1)at 5.
 %  observe([placeOn(customer1,menu1,table1)],5).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',134).
% Happens(Eat(Customer1,Food1),11).
% From E: 
% 
% happens(
%    eat(customer1,food1), 11).
observe eat(customer1, food1)at 11.
 %  observe([eat(customer1,food1)],11).
 %  % =================================.


% Happens(Request(Customer1,Waiter1,Bill1),12).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',134).
% From E: 
% 
% happens(
%    request(customer1,waiter1,bill1), 12).
observe request(customer1, waiter1, bill1)at 12.
 %  observe([request(customer1,waiter1,bill1)],12).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',136).
% Happens(Pay(Customer1,Waiter1),15).
% From E: 
% 
% happens(
%    pay(customer1,waiter1), 15).
observe pay(customer1, waiter1)at 15.
 %  observe([pay(customer1,waiter1)],15).
 %  % =================================.


% Happens(Tip(Customer1,Waiter1),15).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',136).
% From E: 
% 
% happens(
%    tip(customer1,waiter1), 15).
observe tip(customer1, waiter1)at 15.
 %  observe([tip(customer1,waiter1)],15).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',138).
% Happens(RiseFrom(Customer1,Chair1),16).
% From E: 
% 
% happens(
%    riseFrom(customer1,chair1), 16).
observe riseFrom(customer1, chair1)at 16.
 %  observe([riseFrom(customer1,chair1)],16).
 %  % =================================.


% Happens(SayGoodbye(Customer1,Waiter1),17).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',138).
% From E: 
% 
% happens(
%    sayGoodbye(customer1,waiter1), 17).
observe sayGoodbye(customer1, waiter1)at 17.
 %  observe([sayGoodbye(customer1,waiter1)],17).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',140).
% Happens(WalkThroughDoor21(Customer1,MainEntrance1),18).
% From E: 
% 
% happens(
%    walkThroughDoor21(customer1,mainEntrance1), 18).
observe walkThroughDoor21(customer1, mainEntrance1)at 18.
 %  observe([walkThroughDoor21(customer1,mainEntrance1)],18).
 %  % =================================.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',142).
% range time 0 19
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,19))).
:- call_pel_directive(range(time, 0, 19)).

% range offset 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',142).
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',144).
% range diameter 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',146).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.e',148).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rest.lps.pl')).

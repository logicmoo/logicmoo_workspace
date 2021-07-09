% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',664).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl')).
% Fri, 26 Mar 2021 01:06:06 GMT File: <stream>(0x555567c94900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

% sort restaurant: script
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',11).
% From E: 
% 
% subsort(restaurant,script).
subsort(restaurant, script).

% sort waiter: agent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',11).
% From E: 
% 
% subsort(waiter,agent).
subsort(waiter, agent).

% sort cook: agent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',13).
% From E: 
% 
% subsort(cook,agent).
subsort(cook, agent).

% function BillOf(restaurant): bill
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',15).
% From E: 
% 
% function(
%    billOf(restaurant), 
%    bill).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',15).
function(billOf(restaurant),bill).

% function CookOf(restaurant): cook
% From E: 
% 
% function(
%    cookOf(restaurant), 
%    cook).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',15).
function(cookOf(restaurant),cook).

% function TableOf(restaurant): table
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',17).
% From E: 
% 
% function(
%    tableOf(restaurant), 
%    table).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',17).
function(tableOf(restaurant),table).

% function WaiterOf(restaurant): waiter
% From E: 
% 
% function(
%    waiterOf(restaurant), 
%    waiter).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',17).
function(waiterOf(restaurant),waiter).

% function KitchenDoorOf(restaurant): door
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',19).
% From E: 
% 
% function(
%    kitchenDoorOf(restaurant), 
%    door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',19).
function(kitchenDoorOf(restaurant),door).
%; awaiting customer/waiter has set down bill on customer's table

% fluent BeWaiter0(waiter)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',21).
% From E: 
% 
% fluent(beWaiter0(waiter)).
mpred_prop(beWaiter0(waiter), fluent).
fluents([beWaiter0/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',24).
%; awaiting customer order

% fluent BeWaiter1(waiter)
% From E: 
% 
% fluent(beWaiter1(waiter)).
mpred_prop(beWaiter1(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',24).
fluents([beWaiter1/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',27).
%; has customer order

% fluent BeWaiter2(waiter)
% From E: 
% 
% fluent(beWaiter2(waiter)).
mpred_prop(beWaiter2(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',27).
fluents([beWaiter2/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',30).
%; in kitchen

% fluent BeWaiter3(waiter)
% From E: 
% 
% fluent(beWaiter3(waiter)).
mpred_prop(beWaiter3(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',30).
fluents([beWaiter3/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',33).
%; awaiting preparation of order

% fluent BeWaiter4(waiter)
% From E: 
% 
% fluent(beWaiter4(waiter)).
mpred_prop(beWaiter4(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',33).
fluents([beWaiter4/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',36).
%; has order

% fluent BeWaiter5(waiter)
% From E: 
% 
% fluent(beWaiter5(waiter)).
mpred_prop(beWaiter5(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',36).
fluents([beWaiter5/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',39).
%; back in dining room

% fluent BeWaiter6(waiter)
% From E: 
% 
% fluent(beWaiter6(waiter)).
mpred_prop(beWaiter6(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',39).
fluents([beWaiter6/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',42).
%; order delivered to customer (can ask if all is OK)

% fluent BeWaiter7(waiter)
% From E: 
% 
% fluent(beWaiter7(waiter)).
mpred_prop(beWaiter7(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',42).
fluents([beWaiter7/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',45).
%; customer has requested bill

% fluent BeWaiter8(waiter)
% From E: 
% 
% fluent(beWaiter8(waiter)).
mpred_prop(beWaiter8(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',45).
fluents([beWaiter8/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',48).
%; waiter is holding bill

% fluent BeWaiter9(waiter)
% From E: 
% 
% fluent(beWaiter9(waiter)).
mpred_prop(beWaiter9(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',48).
fluents([beWaiter9/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',51).
% xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9
% From E: 
% 
% xor([beWaiter0,beWaiter1,beWaiter2,beWaiter3,beWaiter4,beWaiter5,beWaiter6,beWaiter7,beWaiter8,beWaiter9]).
xor([ beWaiter0,
      beWaiter1,
      beWaiter2,
      beWaiter3,
      beWaiter4,
      beWaiter5,
      beWaiter6,
      beWaiter7,
      beWaiter8,
      beWaiter9
    ]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',53).
% [waiter,agent,time]
% HoldsAt(BeWaiter0(waiter),time) ->
% Terminates(Greet(waiter,agent),
%            BeWaiter0(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',53).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter0(Waiter), 
%       Time), 
%    terminates_at(
%       greet(Waiter,Agent), 
%       beWaiter0(Waiter), 
%       Time)).
(   terminates(greet(Waiter, Agent),
               beWaiter0(Waiter)at Time)
;   not beWaiter0(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',53).

 /*   (   terminates(greet(Waiter, Agent),
                       at(beWaiter0(Waiter), Time))
        ;   at(not(beWaiter0(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,agent,time]
% HoldsAt(BeWaiter0(waiter),time) ->
% Initiates(Greet(waiter,agent),
%           BeWaiter1(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',60).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter0(Waiter), 
%       Time), 
%    initiates_at(
%       greet(Waiter,Agent), 
%       beWaiter1(Waiter), 
%       Time)).
(   initiates(greet(Waiter, Agent),
              beWaiter1(Waiter)at Time)
;   not beWaiter0(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',60).

 /*   (   initiates(greet(Waiter, Agent),
                      at(beWaiter1(Waiter), Time))
        ;   at(not(beWaiter0(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Terminates(Order(agent,waiter,food),
%            BeWaiter1(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',66).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter1(Waiter), 
%       Time), 
%    terminates_at(
%       order(Agent,Waiter,Food), 
%       beWaiter1(Waiter), 
%       Time)).
(   terminates(order(Agent, Waiter, Food),
               beWaiter1(Waiter)at Time)
;   not beWaiter1(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',66).

 /*   (   terminates(order(Agent, Waiter, Food),
                       at(beWaiter1(Waiter), Time))
        ;   at(not(beWaiter1(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',72).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter1(Waiter), 
%       Time), 
%    initiates_at(
%       order(Agent,Waiter,Food), 
%       beWaiter2(Waiter), 
%       Time)).
(   initiates(order(Agent, Waiter, Food),
              beWaiter2(Waiter)at Time)
;   not beWaiter1(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',72).

 /*   (   initiates(order(Agent, Waiter, Food),
                      at(beWaiter2(Waiter), Time))
        ;   at(not(beWaiter1(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,time]
% WaiterOf(restaurant)=waiter &
% HoldsAt(BeWaiter2(waiter),time) ->
% Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',78).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       holds(
%          beWaiter2(Waiter), 
%          Time)), 
%    happens(
%       walkThroughDoor12(Waiter, 
%          kitchenDoorOf(Restaurant)), 
%       Time)).
(   happens(walkThroughDoor12(Waiter, kitchenDoorOf(Restaurant)),
            Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not beWaiter2(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',78).

 /*   (   happens(walkThroughDoor12(Waiter, kitchenDoorOf(Restaurant)),
                    Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   at(not(beWaiter2(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter2(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Terminates(WalkThroughDoor12(waiter,door),
%            BeWaiter2(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',83).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter2(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    terminates_at(
%       walkThroughDoor12(Waiter,Door), 
%       beWaiter2(Waiter), 
%       Time)).
(   terminates(walkThroughDoor12(Waiter, Door),
               beWaiter2(Waiter)at Time)
;   not beWaiter2(Waiter)at Time
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(kitchenDoorOf(Restaurant), Door)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',83).

 /*   (   terminates(walkThroughDoor12(Waiter, Door),
                       at(beWaiter2(Waiter), Time))
        ;   at(not(beWaiter2(Waiter)), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(kitchenDoorOf(Restaurant), Door))
        ).
 */
 %  % =================================.


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter2(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Initiates(WalkThroughDoor12(waiter,door),
%           BeWaiter3(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',91).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter2(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    initiates_at(
%       walkThroughDoor12(Waiter,Door), 
%       beWaiter3(Waiter), 
%       Time)).
(   initiates(walkThroughDoor12(Waiter, Door),
              beWaiter3(Waiter)at Time)
;   not beWaiter2(Waiter)at Time
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(kitchenDoorOf(Restaurant), Door)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',91).

 /*   (   initiates(walkThroughDoor12(Waiter, Door),
                      at(beWaiter3(Waiter), Time))
        ;   at(not(beWaiter2(Waiter)), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(kitchenDoorOf(Restaurant), Door))
        ).
 */
 %  % =================================.


% [restaurant,food,time]
% HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
% ({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
% Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',99).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter3(waiterOf(Restaurant)), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(
%                waiterOf(Restaurant), 
%                Agent, 
%                Food), 
%             Time))), 
%    happens(
%       order(
%          waiterOf(Restaurant), 
%          cookOf(Restaurant), 
%          Food), 
%       Time)).
(   happens(order(waiterOf(Restaurant),
                  cookOf(Restaurant),
                  Food),
            Time)
;   not beWaiter3(waiterOf(Restaurant))at Time
;   not(thereExists(Agent,
                    at(knowOrder(waiterOf(Restaurant),
                                 Agent,
                                 Food),
                       Time)))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',99).

 /*   (   happens(order(waiterOf(Restaurant),
                          cookOf(Restaurant),
                          Food),
                    Time)
        ;   at(not(beWaiter3(waiterOf(Restaurant))), Time)
        ;   not(thereExists(Agent,
                            at(knowOrder(waiterOf(Restaurant),
                                         Agent,
                                         Food),
                               Time)))
        ).
 */
 %  % =================================.


% [restaurant,waiter,cook,food,time]
% WaiterOf(restaurant)=waiter &
% CookOf(restaurant)=cook &
% HoldsAt(BeWaiter3(waiter),time) ->
% Terminates(Order(waiter,cook,food),
%            BeWaiter3(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',104).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             cookOf(Restaurant), 
%             Cook), 
%          holds(
%             beWaiter3(Waiter), 
%             Time))), 
%    terminates_at(
%       order(Waiter,Cook,Food), 
%       beWaiter3(Waiter), 
%       Time)).
(   terminates(order(Waiter, Cook, Food),
               beWaiter3(Waiter)at Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(cookOf(Restaurant), Cook)
;   not beWaiter3(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',104).

 /*   (   terminates(order(Waiter, Cook, Food),
                       at(beWaiter3(Waiter), Time))
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(cookOf(Restaurant), Cook))
        ;   at(not(beWaiter3(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,cook,food,time]
% WaiterOf(restaurant)=waiter &
% CookOf(restaurant)=cook &
% HoldsAt(BeWaiter3(waiter),time) ->
% Initiates(Order(waiter,cook,food),
%           BeWaiter4(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',112).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             cookOf(Restaurant), 
%             Cook), 
%          holds(
%             beWaiter3(Waiter), 
%             Time))), 
%    initiates_at(
%       order(Waiter,Cook,Food), 
%       beWaiter4(Waiter), 
%       Time)).
(   initiates(order(Waiter, Cook, Food),
              beWaiter4(Waiter)at Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(cookOf(Restaurant), Cook)
;   not beWaiter3(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',112).

 /*   (   initiates(order(Waiter, Cook, Food),
                      at(beWaiter4(Waiter), Time))
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(cookOf(Restaurant), Cook))
        ;   at(not(beWaiter3(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
% HoldsAt(FoodPrepared(food),time) ->
% Happens(PickUp(waiter,food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',120).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       ','(
%          thereExists(Agent, 
%             holds(
%                knowOrder(Waiter,Agent,Food), 
%                Time)), 
%          holds(
%             foodPrepared(Food), 
%             Time))), 
%    happens(
%       pickUp(Waiter,Food), 
%       Time)).
(   happens(pickUp(Waiter, Food), Time)
;   not beWaiter4(Waiter)at Time
;   not(thereExists(Agent,
                    knowOrder(Waiter, Agent, Food)at Time))
;   not foodPrepared(Food)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',120).

 /*   (   happens(pickUp(Waiter, Food), Time)
        ;   at(not(beWaiter4(Waiter)), Time)
        ;   not(thereExists(Agent,
                            at(knowOrder(Waiter, Agent, Food),
                               Time)))
        ;   at(not(foodPrepared(Food)), Time)
        ).
 */
 %  % =================================.


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
% Terminates(PickUp(waiter,food),
%            BeWaiter4(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',126).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(Waiter,Agent,Food), 
%             Time))), 
%    terminates_at(
%       pickUp(Waiter,Food), 
%       beWaiter4(Waiter), 
%       Time)).
(   terminates(pickUp(Waiter, Food),
               beWaiter4(Waiter)at Time)
;   not beWaiter4(Waiter)at Time
;   not(thereExists(Agent,
                    knowOrder(Waiter, Agent, Food)at Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',126).

 /*   (   terminates(pickUp(Waiter, Food),
                       at(beWaiter4(Waiter), Time))
        ;   at(not(beWaiter4(Waiter)), Time)
        ;   not(thereExists(Agent,
                            at(knowOrder(Waiter, Agent, Food),
                               Time)))
        ).
 */
 %  % =================================.


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
% Initiates(PickUp(waiter,food),
%           BeWaiter5(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',133).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(Waiter,Agent,Food), 
%             Time))), 
%    initiates_at(
%       pickUp(Waiter,Food), 
%       beWaiter5(Waiter), 
%       Time)).
(   initiates(pickUp(Waiter, Food),
              beWaiter5(Waiter)at Time)
;   not beWaiter4(Waiter)at Time
;   not(thereExists(Agent,
                    knowOrder(Waiter, Agent, Food)at Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',133).

 /*   (   initiates(pickUp(Waiter, Food),
                      at(beWaiter5(Waiter), Time))
        ;   at(not(beWaiter4(Waiter)), Time)
        ;   not(thereExists(Agent,
                            at(knowOrder(Waiter, Agent, Food),
                               Time)))
        ).
 */
 %  % =================================.


% [restaurant,waiter,time]
% WaiterOf(restaurant)=waiter &
% HoldsAt(BeWaiter5(waiter),time) ->
% Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',140).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       holds(
%          beWaiter5(Waiter), 
%          Time)), 
%    happens(
%       walkThroughDoor21(Waiter, 
%          kitchenDoorOf(Restaurant)), 
%       Time)).
(   happens(walkThroughDoor21(Waiter, kitchenDoorOf(Restaurant)),
            Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not beWaiter5(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',140).

 /*   (   happens(walkThroughDoor21(Waiter, kitchenDoorOf(Restaurant)),
                    Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   at(not(beWaiter5(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter5(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Terminates(WalkThroughDoor21(waiter,door),
%            BeWaiter5(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',145).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter5(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    terminates_at(
%       walkThroughDoor21(Waiter,Door), 
%       beWaiter5(Waiter), 
%       Time)).
(   terminates(walkThroughDoor21(Waiter, Door),
               beWaiter5(Waiter)at Time)
;   not beWaiter5(Waiter)at Time
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(kitchenDoorOf(Restaurant), Door)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',145).

 /*   (   terminates(walkThroughDoor21(Waiter, Door),
                       at(beWaiter5(Waiter), Time))
        ;   at(not(beWaiter5(Waiter)), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(kitchenDoorOf(Restaurant), Door))
        ).
 */
 %  % =================================.


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter5(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Initiates(WalkThroughDoor21(waiter,door),
%           BeWaiter6(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',153).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter5(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    initiates_at(
%       walkThroughDoor21(Waiter,Door), 
%       beWaiter6(Waiter), 
%       Time)).
(   initiates(walkThroughDoor21(Waiter, Door),
              beWaiter6(Waiter)at Time)
;   not beWaiter5(Waiter)at Time
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(kitchenDoorOf(Restaurant), Door)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',153).

 /*   (   initiates(walkThroughDoor21(Waiter, Door),
                      at(beWaiter6(Waiter), Time))
        ;   at(not(beWaiter5(Waiter)), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(kitchenDoorOf(Restaurant), Door))
        ).
 */
 %  % =================================.


% [restaurant,waiter,table,food,time]
% WaiterOf(restaurant)=waiter &
% TableOf(restaurant)=table &
% HoldsAt(BeWaiter6(waiter),time) &
% HoldsAt(Holding(waiter,food),time) ->
% Happens(PlaceOn(waiter,food,table),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',161).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             tableOf(Restaurant), 
%             Table), 
%          ','(
%             holds(
%                beWaiter6(Waiter), 
%                Time), 
%             holds(
%                holding(Waiter,Food), 
%                Time)))), 
%    happens(
%       placeOn(Waiter,Food,Table), 
%       Time)).
(   happens(placeOn(Waiter, Food, Table), Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(tableOf(Restaurant), Table)
;   not beWaiter6(Waiter)at Time
;   not holding(Waiter, Food)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',161).

 /*   (   happens(placeOn(Waiter, Food, Table), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(tableOf(Restaurant), Table))
        ;   at(not(beWaiter6(Waiter)), Time)
        ;   at(not(holding(Waiter, Food)), Time)
        ).
 */
 %  % =================================.


% [waiter,food,table,time]
% HoldsAt(BeWaiter6(waiter),time) ->
% Terminates(PlaceOn(waiter,food,table),
%            BeWaiter6(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',168).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter6(Waiter), 
%       Time), 
%    terminates_at(
%       placeOn(Waiter,Food,Table), 
%       beWaiter6(Waiter), 
%       Time)).
(   terminates(placeOn(Waiter, Food, Table),
               beWaiter6(Waiter)at Time)
;   not beWaiter6(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',168).

 /*   (   terminates(placeOn(Waiter, Food, Table),
                       at(beWaiter6(Waiter), Time))
        ;   at(not(beWaiter6(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,food,table,time]
% HoldsAt(BeWaiter6(waiter),time) ->
% Initiates(PlaceOn(waiter,food,table),
%           BeWaiter7(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',174).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter6(Waiter), 
%       Time), 
%    initiates_at(
%       placeOn(Waiter,Food,Table), 
%       beWaiter7(Waiter), 
%       Time)).
(   initiates(placeOn(Waiter, Food, Table),
              beWaiter7(Waiter)at Time)
;   not beWaiter6(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',174).

 /*   (   initiates(placeOn(Waiter, Food, Table),
                      at(beWaiter7(Waiter), Time))
        ;   at(not(beWaiter6(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,agent,bill,time]
% HoldsAt(BeWaiter7(waiter),time) ->
% Terminates(Request(agent,waiter,bill),
%            BeWaiter7(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',180).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter7(Waiter), 
%       Time), 
%    terminates_at(
%       request(Agent,Waiter,Bill), 
%       beWaiter7(Waiter), 
%       Time)).
(   terminates(request(Agent, Waiter, Bill),
               beWaiter7(Waiter)at Time)
;   not beWaiter7(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',180).

 /*   (   terminates(request(Agent, Waiter, Bill),
                       at(beWaiter7(Waiter), Time))
        ;   at(not(beWaiter7(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,agent,bill,time]
% HoldsAt(BeWaiter7(waiter),time) ->
% Initiates(Request(agent,waiter,bill),
%           BeWaiter8(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',186).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter7(Waiter), 
%       Time), 
%    initiates_at(
%       request(Agent,Waiter,Bill), 
%       beWaiter8(Waiter), 
%       Time)).
(   initiates(request(Agent, Waiter, Bill),
              beWaiter8(Waiter)at Time)
;   not beWaiter7(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',186).

 /*   (   initiates(request(Agent, Waiter, Bill),
                      at(beWaiter8(Waiter), Time))
        ;   at(not(beWaiter7(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,bill,time]
% WaiterOf(restaurant)=waiter &
% BillOf(restaurant)=bill &
% HoldsAt(BeWaiter8(waiter),time) ->
% Happens(PickUp(waiter,bill),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',192).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             billOf(Restaurant), 
%             Bill), 
%          holds(
%             beWaiter8(Waiter), 
%             Time))), 
%    happens(
%       pickUp(Waiter,Bill), 
%       Time)).
(   happens(pickUp(Waiter, Bill), Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(billOf(Restaurant), Bill)
;   not beWaiter8(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',192).

 /*   (   happens(pickUp(Waiter, Bill), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(billOf(Restaurant), Bill))
        ;   at(not(beWaiter8(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,bill,time]
% HoldsAt(BeWaiter8(waiter),time) ->
% Terminates(PickUp(waiter,bill),
%            BeWaiter8(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',198).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter8(Waiter), 
%       Time), 
%    terminates_at(
%       pickUp(Waiter,Bill), 
%       beWaiter8(Waiter), 
%       Time)).
(   terminates(pickUp(Waiter, Bill),
               beWaiter8(Waiter)at Time)
;   not beWaiter8(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',198).

 /*   (   terminates(pickUp(Waiter, Bill),
                       at(beWaiter8(Waiter), Time))
        ;   at(not(beWaiter8(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,bill,time]
% HoldsAt(BeWaiter8(waiter),time) ->
% Initiates(PickUp(waiter,bill),
%           BeWaiter9(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',204).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter8(Waiter), 
%       Time), 
%    initiates_at(
%       pickUp(Waiter,Bill), 
%       beWaiter9(Waiter), 
%       Time)).
(   initiates(pickUp(Waiter, Bill),
              beWaiter9(Waiter)at Time)
;   not beWaiter8(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',204).

 /*   (   initiates(pickUp(Waiter, Bill),
                      at(beWaiter9(Waiter), Time))
        ;   at(not(beWaiter8(Waiter)), Time)
        ).
 */
 %  % =================================.


% [restaurant,waiter,bill,table,time]
% WaiterOf(restaurant)=waiter &
% BillOf(restaurant)=bill &
% TableOf(restaurant)=table &
% HoldsAt(BeWaiter9(waiter),time) ->
% Happens(PlaceOn(waiter,bill,table),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',210).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             billOf(Restaurant), 
%             Bill), 
%          ','(
%             '='(
%                tableOf(Restaurant), 
%                Table), 
%             holds(
%                beWaiter9(Waiter), 
%                Time)))), 
%    happens(
%       placeOn(Waiter,Bill,Table), 
%       Time)).
(   happens(placeOn(Waiter, Bill, Table), Time)
;   not equals(waiterOf(Restaurant), Waiter)
;   not equals(billOf(Restaurant), Bill)
;   not equals(tableOf(Restaurant), Table)
;   not beWaiter9(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',210).

 /*   (   happens(placeOn(Waiter, Bill, Table), Time)
        ;   not(equals(waiterOf(Restaurant), Waiter))
        ;   not(equals(billOf(Restaurant), Bill))
        ;   not(equals(tableOf(Restaurant), Table))
        ;   at(not(beWaiter9(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,bill,table,time]
% HoldsAt(BeWaiter9(waiter),time) ->
% Terminates(PlaceOn(waiter,bill,table),
%            BeWaiter9(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',217).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter9(Waiter), 
%       Time), 
%    terminates_at(
%       placeOn(Waiter,Bill,Table), 
%       beWaiter9(Waiter), 
%       Time)).
(   terminates(placeOn(Waiter, Bill, Table),
               beWaiter9(Waiter)at Time)
;   not beWaiter9(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',217).

 /*   (   terminates(placeOn(Waiter, Bill, Table),
                       at(beWaiter9(Waiter), Time))
        ;   at(not(beWaiter9(Waiter)), Time)
        ).
 */
 %  % =================================.


% [waiter,bill,table,time]
% HoldsAt(BeWaiter9(waiter),time) ->
% Initiates(PlaceOn(waiter,bill,table),
%           BeWaiter0(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',223).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter9(Waiter), 
%       Time), 
%    initiates_at(
%       placeOn(Waiter,Bill,Table), 
%       beWaiter0(Waiter), 
%       Time)).
(   initiates(placeOn(Waiter, Bill, Table),
              beWaiter0(Waiter)at Time)
;   not beWaiter9(Waiter)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',223).

 /*   (   initiates(placeOn(Waiter, Bill, Table),
                      at(beWaiter0(Waiter), Time))
        ;   at(not(beWaiter9(Waiter)), Time)
        ).
 */
 %  % =================================.


%; awaiting next waiter order

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',229).
% fluent BeCook0(cook)
% From E: 
% 
% fluent(beCook0(cook)).
mpred_prop(beCook0(cook), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',229).
fluents([beCook0/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',231).
%; waiter order received

% fluent BeCook1(cook)
% From E: 
% 
% fluent(beCook1(cook)).
mpred_prop(beCook1(cook), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',231).
fluents([beCook1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',234).
% xor BeCook0, BeCook1
% From E: 
% 
% xor([beCook0,beCook1]).
xor([beCook0,beCook1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',236).
% [cook,agent,food,time]
% HoldsAt(BeCook0(cook),time) ->
% Terminates(Order(agent,cook,food),
%            BeCook0(cook),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',236).
% From E: 
% 
% '->'(
%    holds(
%       beCook0(Cook), 
%       Time), 
%    terminates_at(
%       order(Agent,Cook,Food), 
%       beCook0(Cook), 
%       Time)).
(   terminates(order(Agent, Cook, Food),
               beCook0(Cook)at Time)
;   not beCook0(Cook)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',236).

 /*   (   terminates(order(Agent, Cook, Food),
                       at(beCook0(Cook), Time))
        ;   at(not(beCook0(Cook)), Time)
        ).
 */
 %  % =================================.


% [cook,agent,food,time]
% HoldsAt(BeCook0(cook),time) ->
% Initiates(Order(agent,cook,food),
%           BeCook1(cook),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',243).
% From E: 
% 
% '->'(
%    holds(
%       beCook0(Cook), 
%       Time), 
%    initiates_at(
%       order(Agent,Cook,Food), 
%       beCook1(Cook), 
%       Time)).
(   initiates(order(Agent, Cook, Food),
              beCook1(Cook)at Time)
;   not beCook0(Cook)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',243).

 /*   (   initiates(order(Agent, Cook, Food),
                      at(beCook1(Cook), Time))
        ;   at(not(beCook0(Cook)), Time)
        ).
 */
 %  % =================================.

% event FoodPrepare(agent,food)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',247).
% From E: 
% 
% event(foodPrepare(agent,food)).
events([foodPrepare/2]).
mpred_prop(foodPrepare(agent, food), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',247).
actions([foodPrepare/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',250).
% fluent FoodPrepared(food)
% From E: 
% 
% fluent(foodPrepared(food)).
mpred_prop(foodPrepared(food), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',250).
fluents([foodPrepared/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',252).
% [agent,food,time]
% Initiates(FoodPrepare(agent,food),
%           FoodPrepared(food),
%           time).
% From E: 
% 
% initiates_at(
%    foodPrepare(Agent,Food), 
%    foodPrepared(Food), 
%    Time).
foodPrepare(Agent, Food)initiates foodPrepared(Food).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',252).

 /*  initiated(happens(foodPrepare(Agent,Food),
     		  Time_from,
     		  Time_until),
     	  foodPrepared(Food),
     	  []).
 */
 %  % =================================.


% [agent,food,time]
% Happens(FoodPrepare(agent,food),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(food,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',258).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          foodPrepare(Agent,Food), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Food,Location), 
%             Time)))).
exists(Location,  (at_loc(Agent, Location)at Time, at_loc(Food, Location)at Time;not happens(foodPrepare(Agent, Food), Time))).
 %  exists(Location,  (at(at_loc(Agent, Location), Time), at(at_loc(Food, Location), Time);not(happens(foodPrepare(Agent, Food), Time)))).
 %  % =================================.


% [cook,agent,food,time]
% HoldsAt(BeCook1(cook),time) &
% HoldsAt(KnowOrder(cook,agent,food),time) ->
% Happens(FoodPrepare(cook,food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',264).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beCook1(Cook), 
%          Time), 
%       holds(
%          knowOrder(Cook,Agent,Food), 
%          Time)), 
%    happens(
%       foodPrepare(Cook,Food), 
%       Time)).
(   happens(foodPrepare(Cook, Food), Time)
;   not beCook1(Cook)at Time
;   not knowOrder(Cook, Agent, Food)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',264).

 /*   (   happens(foodPrepare(Cook, Food), Time)
        ;   at(not(beCook1(Cook)), Time)
        ;   at(not(knowOrder(Cook, Agent, Food)), Time)
        ).
 */
 %  % =================================.


% [cook,food,time]
% HoldsAt(BeCook1(cook),time) ->
% Terminates(FoodPrepare(cook,food),
%            BeCook1(cook),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',269).
% From E: 
% 
% '->'(
%    holds(
%       beCook1(Cook), 
%       Time), 
%    terminates_at(
%       foodPrepare(Cook,Food), 
%       beCook1(Cook), 
%       Time)).
(   terminates(foodPrepare(Cook, Food),
               beCook1(Cook)at Time)
;   not beCook1(Cook)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',269).

 /*   (   terminates(foodPrepare(Cook, Food),
                       at(beCook1(Cook), Time))
        ;   at(not(beCook1(Cook)), Time)
        ).
 */
 %  % =================================.


% [cook,food,time]
% HoldsAt(BeCook1(cook),time) ->
% Initiates(FoodPrepare(cook,food),
%           BeCook0(cook),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',275).
% From E: 
% 
% '->'(
%    holds(
%       beCook1(Cook), 
%       Time), 
%    initiates_at(
%       foodPrepare(Cook,Food), 
%       beCook0(Cook), 
%       Time)).
(   initiates(foodPrepare(Cook, Food),
              beCook0(Cook)at Time)
;   not beCook1(Cook)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',275).

 /*   (   initiates(foodPrepare(Cook, Food),
                      at(beCook0(Cook), Time))
        ;   at(not(beCook1(Cook)), Time)
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.e',279).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Restaurant.lps.pl')).

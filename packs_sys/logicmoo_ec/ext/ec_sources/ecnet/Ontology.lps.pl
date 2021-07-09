% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',358).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).
% Fri, 26 Mar 2021 01:06:02 GMT File: <stream>(0x555567d92d00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; integer
%;

% sort diameter: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',14).
% From E: 
% 
% subsort(diameter,integer).
subsort(diameter, integer).
%; object

% sort object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',18).
% From E: 
% 
% sort(object).
sort(object).

% sort agent: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',20).
% From E: 
% 
% subsort(agent,object).
subsort(agent, object).

% sort physobj: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',22).
% From E: 
% 
% subsort(physobj,object).
subsort(physobj, object).

% sort bed: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',22).
% From E: 
% 
% subsort(bed,physobj).
subsort(bed, physobj).

% sort snowflake: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',24).
% From E: 
% 
% subsort(snowflake,physobj).
subsort(snowflake, physobj).

% sort sky: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',24).
% From E: 
% 
% subsort(sky,physobj).
subsort(sky, physobj).

% sort stuff: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',27).
% From E: 
% 
% subsort(stuff,physobj).
subsort(stuff, physobj).

% sort surface: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',29).
% From E: 
% 
% subsort(surface,physobj).
subsort(surface, physobj).

% sort ground: surface
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',29).
% From E: 
% 
% subsort(ground,surface).
subsort(ground, surface).

% sort snow: stuff
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',32).
% From E: 
% 
% subsort(snow,stuff).
subsort(snow, stuff).

% sort ball
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',32).
% From E: 
% 
% sort(ball).
sort(ball).

% sort food: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',35).
% From E: 
% 
% subsort(food,physobj).
subsort(food, physobj).

% sort fruit: food
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',35).
% From E: 
% 
% subsort(fruit,food).
subsort(fruit, food).

% sort orange: fruit
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',37).
% From E: 
% 
% subsort(orange,fruit).
subsort(orange, fruit).

% sort salad: food
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',37).
% From E: 
% 
% subsort(salad,food).
subsort(salad, food).

% sort clothing: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',40).
% From E: 
% 
% subsort(clothing,physobj).
subsort(clothing, physobj).

% sort scarf: clothing
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',40).
% From E: 
% 
% subsort(scarf,clothing).
subsort(scarf, clothing).

% sort hat: clothing
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',42).
% From E: 
% 
% subsort(hat,clothing).
subsort(hat, clothing).

% sort vegetablematter: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',44).
% From E: 
% 
% subsort(vegetablematter,physobj).
subsort(vegetablematter, physobj).

% sort coal: vegetablematter
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',44).
% From E: 
% 
% subsort(coal,vegetablematter).
subsort(coal, vegetablematter).

% sort bodypart: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',47).
% From E: 
% 
% subsort(bodypart,physobj).
subsort(bodypart, physobj).

% sort hand: bodypart
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',47).
% From E: 
% 
% subsort(hand,bodypart).
subsort(hand, bodypart).

% sort papertowels: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',50).
% From E: 
% 
% subsort(papertowels,physobj).
subsort(papertowels, physobj).

% sort device: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',50).
% From E: 
% 
% subsort(device,physobj).
subsort(device, physobj).

% sort electronicdevice: device
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',52).
% From E: 
% 
% subsort(electronicdevice,device).
subsort(electronicdevice, device).

% sort lamp: electronicdevice
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',52).
% From E: 
% 
% subsort(lamp,electronicdevice).
subsort(lamp, electronicdevice).

% sort cat: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',55).
% From E: 
% 
% subsort(cat,physobj).
subsort(cat, physobj).

% sort horse: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',55).
% From E: 
% 
% subsort(horse,physobj).
subsort(horse, physobj).

% sort weapon: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',58).
% From E: 
% 
% subsort(weapon,physobj).
subsort(weapon, physobj).

% sort gun: weapon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',58).
% From E: 
% 
% subsort(gun,weapon).
subsort(gun, weapon).

% sort bomb: weapon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',60).
% From E: 
% 
% subsort(bomb,weapon).
subsort(bomb, weapon).

% sort bullet: weapon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',60).
% From E: 
% 
% subsort(bullet,weapon).
subsort(bullet, weapon).
%; location

% sort location
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',65).
% From E: 
% 
% sort(location).
sort(location).

% sort room: location, outside: location
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',65).
% From E: 
% 
% subsort(room,location).
subsort(room, location).
% From E: 
% 
% subsort(outside,location).
subsort(outside, location).
%; portal

% sort portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',70).
% From E: 
% 
% sort(portal).
sort(portal).

% sort door: portal, staircase: portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',70).
% From E: 
% 
% subsort(door,portal).
subsort(door, portal).
% From E: 
% 
% subsort(staircase,portal).
subsort(staircase, portal).

% sort street: portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',72).
% From E: 
% 
% subsort(street,portal).
subsort(street, portal).

% sort track: portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',72).
% From E: 
% 
% subsort(track,portal).
subsort(track, portal).

% sort building
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',75).
% From E: 
% 
% sort(building).
sort(building).

% sort fire: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',77).
% From E: 
% 
% subsort(fire,object).
subsort(fire, object).

% sort smoke: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',77).
% From E: 
% 
% subsort(smoke,physobj).
subsort(smoke, physobj).

% sort furniture: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',80).
% From E: 
% 
% subsort(furniture,physobj).
subsort(furniture, physobj).

% sort chair: furniture
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',80).
% From E: 
% 
% subsort(chair,furniture).
subsort(chair, furniture).

% sort table: furniture
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',82).
% From E: 
% 
% subsort(table,furniture).
subsort(table, furniture).

% sort bill: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',84).
% From E: 
% 
% subsort(bill,physobj).
subsort(bill, physobj).

% sort ticket: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',84).
% From E: 
% 
% subsort(ticket,physobj).
subsort(ticket, physobj).

% sort envelope: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',86).
% From E: 
% 
% subsort(envelope,physobj).
subsort(envelope, physobj).

% sort text: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',88).
% From E: 
% 
% subsort(text,physobj).
subsort(text, physobj).

% sort book: text
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',88).
% From E: 
% 
% subsort(book,text).
subsort(book, text).

% sort letter: text
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',90).
% From E: 
% 
% subsort(letter,text).
subsort(letter, text).

% sort menu: text
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',90).
% From E: 
% 
% subsort(menu,text).
subsort(menu, text).

% sort paper: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',93).
% From E: 
% 
% subsort(paper,physobj).
subsort(paper, physobj).

% sort content
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',95).
% From E: 
% 
% sort(content).
sort(content).

% sort script
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',95).
% From E: 
% 
% sort(script).
sort(script).

% sort container: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',98).
% From E: 
% 
% subsort(container,physobj).
subsort(container, physobj).

% sort cigarette: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',98).
% From E: 
% 
% subsort(cigarette,physobj).
subsort(cigarette, physobj).

% sort ashtray: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',100).
% From E: 
% 
% subsort(ashtray,physobj).
subsort(ashtray, physobj).

% sort umbrella: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',100).
% From E: 
% 
% subsort(umbrella,physobj).
subsort(umbrella, physobj).

% sort pen: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',103).
% From E: 
% 
% subsort(pen,physobj).
subsort(pen, physobj).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',105).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).

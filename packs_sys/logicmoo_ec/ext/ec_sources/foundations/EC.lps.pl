% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',49).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl')).
% Fri, 26 Mar 2021 01:05:54 GMT File: <stream>(0x555567a69100)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Event Calculus (EC)
%;
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

% sort time: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',27).
% From E: 
% 
% subsort(time,integer).
subsort(time, integer).

% sort offset: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',27).
% From E: 
% 
% subsort(offset,integer).
subsort(offset, integer).

% reified sort fluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',30).
% From E: 
% 
% reified_sort(fluent).
reified_sorts([fluent/0]).

% reified sort event
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',30).
% From E: 
% 
% reified_sort(event).
reified_sorts([event/0]).

% predicate Happens(event,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',33).
% From E: 
% 
% predicate(happens(event,time)).
mpred_prop(happens(event, time), predicate).
predicates([happens/2]).

% predicate HoldsAt(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',33).
% From E: 
% 
% predicate(holds(fluent,time)).
mpred_prop(holds(fluent, time), predicate).
predicates([holds/2]).

% predicate ReleasedAt(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',35).
% From E: 
% 
% predicate(released_at(fluent,time)).
mpred_prop(released_at(fluent, time), predicate).
predicates([released_at/2]).

% predicate Initiates(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',35).
% From E: 
% 
% predicate(initiates_at(event,fluent,time)).
mpred_prop(initiates(event, fluent, time), predicate).
predicates([initiates/3]).

% predicate Terminates(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',37).
% From E: 
% 
% predicate(terminates_at(event,fluent,time)).
mpred_prop(terminates(event, fluent, time), predicate).
predicates([terminates/3]).

% predicate Releases(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',37).
% From E: 
% 
% predicate(releases_at(event,fluent,time)).
mpred_prop(releases(event, fluent, time), predicate).
predicates([releases/3]).

% predicate Trajectory(fluent,time,fluent,offset)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',39).
% From E: 
% 
% predicate(trajectory(fluent, time, fluent, 
%              offset)).
mpred_prop(trajectory(fluent, time, fluent, offset), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',39).
predicates([trajectory/4]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',41).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.lps.pl')).

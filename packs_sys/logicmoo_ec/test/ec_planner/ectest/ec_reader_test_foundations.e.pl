:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_examples.e',9038).
:- call_pel_directive(translate(unskipped,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(ecalc).
:- call_pel_directive(translate(begining,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e.pl')).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/Root.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',18).
% sort boolean
sort(boolean).

% sort integer
sort(integer).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',20).
% reified sort predicate
reified_sort(predicate).

% reified sort function
reified_sort(function).
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/EC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
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

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',57).
% sort time: integer
subsort(time,integer).

% sort offset: integer
subsort(offset,integer).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',60).
% reified sort fluent
reified_sort(fluent).

% reified sort event
reified_sort(event).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',63).
% predicate Happens(event,time)
predicate(happens_at(event,time)).

% predicate HoldsAt(fluent,time)
predicate(holds_at(fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',65).
% predicate ReleasedAt(fluent,time)
predicate(released_at(fluent,time)).

% predicate Initiates(event,fluent,time)
predicate(initiates_at(event,fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',67).
% predicate Terminates(event,fluent,time)
predicate(terminates_at(event,fluent,time)).

% predicate Releases(event,fluent,time)
predicate(releases_at(event,fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',69).
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent,time,fluent,offset)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',71).
%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/DEC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Discrete Event Calculus (DEC)
%;
%; @article{Mueller:2004a,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Event calculus reasoning through satisfiability",
%;   journal = "Journal of Logic and Computation",
%;   volume = "14",
%;   number = "5",
%;   pages = "703--730",
%; }
%;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',102).
% sort time: integer
subsort(time,integer).

% sort offset: integer
subsort(offset,integer).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',105).
% reified sort fluent
reified_sort(fluent).

% reified sort event
reified_sort(event).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',108).
% predicate Happens(event,time)
predicate(happens_at(event,time)).

% predicate HoldsAt(fluent,time)
predicate(holds_at(fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',110).
% predicate ReleasedAt(fluent,time)
predicate(released_at(fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',112).
% predicate Initiates(event,fluent,time)
predicate(initiates_at(event,fluent,time)).

% predicate Terminates(event,fluent,time)
predicate(terminates_at(event,fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',114).
% predicate Releases(event,fluent,time)
predicate(releases_at(event,fluent,time)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',116).
% [fluent,time]
% (HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
% HoldsAt(fluent,time+1).
holds_at(Fluent, Time), not(released_at(Fluent, Time+1)), not(exists([Event],  (happens_at(Event, Time), terminates_at(Event, Fluent, Time)))) ->
    holds_at(Fluent, Time+1).


% [fluent,time]
% (!HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
% !HoldsAt(fluent,time+1).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',123).
holds_at(not(Fluent), Time), not(released_at(Fluent, Time+1)), not(exists([Event],  (happens_at(Event, Time), initiates_at(Event, Fluent, Time)))) ->
    holds_at(not(Fluent), Time+1).


% [fluent,time]
% (!ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) & Releases(event,fluent,time))) ->
% !ReleasedAt(fluent,time+1).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',129).
not(released_at(Fluent, Time)), not(exists([Event],  (happens_at(Event, Time), releases_at(Event, Fluent, Time)))) ->
    not(released_at(Fluent, Time+1)).


% [fluent,time]
% (ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) &
%    (Initiates(event,fluent,time) |
%     Terminates(event,fluent,time)))) ->
% ReleasedAt(fluent,time+1).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',134).
released_at(Fluent, Time), not(exists([Event],  (happens_at(Event, Time), (initiates_at(Event, Fluent, Time);terminates_at(Event, Fluent, Time))))) ->
    released_at(Fluent, Time+1).


% [event,fluent,time]
% (Happens(event,time) & Initiates(event,fluent,time)) ->
% (HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
happens_at(Event, Time), initiates_at(Event, Fluent, Time) ->
    holds_at(Fluent, Time+1),
    not(released_at(Fluent, Time+1)).


% [event,fluent,time]
% (Happens(event,time) & Terminates(event,fluent,time)) ->
% (!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
happens_at(Event, Time), terminates_at(Event, Fluent, Time) ->
    holds_at(not(Fluent), Time+1),
    not(released_at(Fluent, Time+1)).


% [event,fluent,time]
% (Happens(event,time) & Releases(event,fluent,time)) ->
% ReleasedAt(fluent,time+1).
happens_at(Event, Time), releases_at(Event, Fluent, Time) ->
    released_at(Fluent, Time+1).


%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECCausal.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Causal Constraints
%;
%; @inproceedings{Shanahan:1999a,
%;   author = "Murray Shanahan",
%;   year = "1999",
%;   title = "The ramification problem in the event calculus",
%;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
%;   pages = "140--146",
%;   address = "San Mateo, CA",
%;   publisher = "Morgan Kaufmann",
%; }
%;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',183).
% predicate Started(fluent,time)
predicate(started(fluent,time)).

% predicate Stopped(fluent,time)
predicate(stopped(fluent,time)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',186).
% [fluent,time]
% Started(fluent,time) <->
% (HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Initiates(event,fluent,time))).
started(Fluent, Time) <->
    (   holds_at(Fluent, Time)
    ;   exists([Event],
                (happens_at(Event, Time), initiates_at(Event, Fluent, Time)))
    ).


% [fluent,time]
% Stopped(fluent,time) <->
% (!HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Terminates(event,fluent,time))).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',192).
stopped(Fluent, Time) <->
    (   holds_at(not(Fluent), Time)
    ;   exists([Event],
                (happens_at(Event, Time), terminates_at(Event, Fluent, Time)))
    ).

% predicate Initiated(fluent,time)
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',195).
predicate(initiated(fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',197).
% predicate Terminated(fluent,time)
predicate(terminated(fluent,time)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',199).
% [fluent,time]
% Initiated(fluent,time) <->
% (Started(fluent,time) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))).
initiated(Fluent, Time) <->
    started(Fluent, Time),
    not(exists([Event],
                (happens_at(Event, Time), terminates_at(Event, Fluent, Time)))).


% [fluent,time]
% Terminated(fluent,time) <->
% (Stopped(fluent,time) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',205).
terminated(Fluent, Time) <->
    stopped(Fluent, Time),
    not(exists([Event],
                (happens_at(Event, Time), initiates_at(Event, Fluent, Time)))).


%; End of file.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECTraj.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
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

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',241).
% predicate Clipped(time,fluent,time)
predicate(clipped(time,fluent,time)).

% predicate Declipped(time,fluent,time)
predicate(declipped(time,fluent,time)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',244).
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent,time,fluent,offset)).

% predicate AntiTrajectory(fluent,time,fluent,offset)
predicate(antiTrajectory(fluent,time,fluent,offset)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',247).
% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Initiates(event,fluent,time) &
% 0 < offset &
% Trajectory(fluent,time,fluent2,offset) &
% !Clipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',247).
happens_at(Event, Time), initiates_at(Event, Fluent, Time), 0<Offset, trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time+Offset)) ->
    holds_at(Fluent2, Time+Offset).


% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Terminates(event,fluent,time) &
% 0 < offset &
% AntiTrajectory(fluent,time,fluent2,offset) &
% !Declipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',256).
happens_at(Event, Time), terminates_at(Event, Fluent, Time), 0<Offset, antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time+Offset)) ->
    holds_at(Fluent2, Time+Offset).


%; End of file.
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',262).
:- call_pel_directive(translate(ending,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e.pl')).

% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',481).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl')).
% Fri, 26 Mar 2021 01:06:03 GMT File: <stream>(0x555566fa3100)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; A complete story understanding program will require representations
%; of common human needs \fullcite{SchankAbelson:1977}.
%;
%; @book{SchankAbelson:1977,
%;   author = "Schank, Roger C. and Abelson, Robert P.",
%;   year = "1977",
%;   title = "Scripts, Plans, Goals, and Understanding: An Inquiry into Human Knowledge Structures",
%;   address = "Hillsdale, NJ",
%;   publisher = "Lawrence Erlbaum",
%; }
%;
%; The PlayNeed representation deals with one type of need, the need
%; to play.
%; Our underlying theory of human needs consists of the following sequence:
%; (1) A need is unsatisfied.
%; (2) Given certain stimuli and an unsatisfied need, an intention
%; to satisfy the need is activated.
%; (3) The intention is acted upon.
%; (4) The need is satisfied.
%; agent has an unsatisfied need to play.

% fluent HungryToPlay(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',32).
% From E: 
% 
% fluent(hungryToPlay(agent)).
mpred_prop(hungryToPlay(agent), fluent).
fluents([hungryToPlay/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',32).
%; agent has the intention to play outside.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',35).
% fluent IntentionToPlay(agent,outside)
% From E: 
% 
% fluent(intentionToPlay(agent,outside)).
mpred_prop(intentionToPlay(agent, outside), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',35).
fluents([intentionToPlay/2]).


%; agent has a satisfied need to play.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',37).
% fluent SatiatedFromPlay(agent)
% From E: 
% 
% fluent(satiatedFromPlay(agent)).
mpred_prop(satiatedFromPlay(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',37).
fluents([satiatedFromPlay/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',39).
%; At any time, an agent is in one of three states with respect
%; to the need to play:

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',41).
% xor HungryToPlay, IntentionToPlay, SatiatedFromPlay
% From E: 
% 
% xor([hungryToPlay,intentionToPlay,satiatedFromPlay]).
xor([hungryToPlay,intentionToPlay,satiatedFromPlay]).
%; agent intends to play at location outside.

% event IntendToPlay(agent,outside)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',43).
% From E: 
% 
% event(intendToPlay(agent,outside)).
events([intendToPlay/2]).
mpred_prop(intendToPlay(agent, outside), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',43).
actions([intendToPlay/2]).


%; agent plays at location outside.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',46).
% event Play(agent,outside)
% From E: 
% 
% event(play(agent,outside)).
events([play/2]).
mpred_prop(play(agent, outside), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',46).
actions([play/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',48).
%; agent acts on the intention to play outside.

% fluent ActOnIntentionToPlay(agent,outside)
% From E: 
% 
% fluent(actOnIntentionToPlay(agent,outside)).
mpred_prop(actOnIntentionToPlay(agent, outside), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',48).
fluents([actOnIntentionToPlay/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',50).
% noninertial ActOnIntentionToPlay
% From E: 
% 
% ':-'(call_pel_directive(noninertial(actOnIntentionToPlay))).
:- call_pel_directive(noninertial(actOnIntentionToPlay)).
%; A trigger axiom activates an intention for an agent to play when
%; the agent has an unsatisfied need for play, the agent likes snow,
%; the agent is awake, and
%; the agent is in a room that looks out onto an outside area where it
%; is snowing:
% [agent,room,outside,time]
% HoldsAt(HungryToPlay(agent),time) &
% HoldsAt(LikeSnow(agent),time) &
% HoldsAt(At(agent,room),time) &
% LookOutOnto(room)=outside &
% HoldsAt(Awake(agent),time) &
% HoldsAt(Snowing(outside),time) ->
% Happens(IntendToPlay(agent,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',57).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          hungryToPlay(Agent), 
%          Time), 
%       ','(
%          holds(
%             likeSnow(Agent), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Room), 
%                Time), 
%             ','(
%                '='(
%                   lookOutOnto(Room), 
%                   Outside), 
%                ','(
%                   holds(
%                      awake(Agent), 
%                      Time), 
%                   holds(
%                      snowing(Outside), 
%                      Time)))))), 
%    happens(
%       intendToPlay(Agent,Outside), 
%       Time)).
(   happens(intendToPlay(Agent, Outside), Time)
;   not hungryToPlay(Agent)at Time
;   not likeSnow(Agent)at Time
;   not at_loc(Agent, Room)at Time
;   not equals(lookOutOnto(Room), Outside)
;   not awake(Agent)at Time
;   not snowing(Outside)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',57).

 /*   (   happens(intendToPlay(Agent, Outside), Time)
        ;   at(not(hungryToPlay(Agent)), Time)
        ;   at(not(likeSnow(Agent)), Time)
        ;   at(not(at_loc(Agent, Room)), Time)
        ;   not(equals(lookOutOnto(Room), Outside))
        ;   at(not(awake(Agent)), Time)
        ;   at(not(snowing(Outside)), Time)
        ).
 */
 %  % =================================.


%; A story understanding program will need a detailed representation
%; of intention \fullcite{CohenLevesque:1990}.
%;
%; @article{CohenLevesque:1990,
%;   author = "Philip R. Cohen and Hector J. Levesque",
%;   year = "1990",
%;   title = "Intention is choice with commitment",
%;   journal = "Artificial Intelligence",
%;   volume = "42",
%;   pages = "213--261",
%; }
%;
%; In our simplified representation, once an intention to
%; perform $e$ is activated, it persists until it is acted
%; upon. Intentions are represented by inertial fluents.
%; If an intention to perform $e$ is active at time point $t$,
%; the agent may or may not perform $e$ at time point $t$.
%; That is, we do not know exactly when the agent will act on the
%; intention.
%; This is a case of nondeterminism,
%; which we handle by introducing a noninertial fluent corresponding
%; to each intention fluent that
%; indicates whether the agent does or does not in fact act
%; on an intention at a given time.
%; Since each ground term of the new noninertial fluent multiplies the
%; number of models by $2^{n}$ where $n$ is the number of time points,
%; in practice we may constrain the truth value of the fluent
%; at various time points.
%; In the case of the need to play,
%; HoldsAt(ActOnIntentionToPlay(agent, outside), time)
%; represents that
%; HoldsAt(IntentionToPlay(agent, outside), time) is acted
%; upon at time.
%; Effect axioms state that
%; if an agent intends to play in an outside area,
%; the agent will have an intention to play in the outside area
%; and will no longer be in the hungry-to-play state:
% [agent,outside,time]
% Initiates(IntendToPlay(agent,outside),IntentionToPlay(agent,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',104).
% From E: 
% 
% initiates_at(
%    intendToPlay(Agent,Outside), 
%    intentionToPlay(Agent,Outside), 
%    Time).
intendToPlay(Agent, Outside)initiates intentionToPlay(Agent, Outside).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',104).

 /*  initiated(happens(intendToPlay(Agent,Outside),
     		  Time_from,
     		  Time_until),
     	  intentionToPlay(Agent,Outside),
     	  []).
 */
 %  % =================================.


% [agent,outside,time]
% Terminates(IntendToPlay(agent,outside),HungryToPlay(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',108).
% From E: 
% 
% terminates_at(
%    intendToPlay(Agent,Outside), 
%    hungryToPlay(Agent), 
%    Time).
intendToPlay(Agent, Outside)terminates hungryToPlay(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',108).

 /*  terminated(happens(intendToPlay(Agent,Outside),
     		   Time_from,
     		   Time_until),
     	   hungryToPlay(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',110).
%; A trigger axiom states that if an agent has the intention
%; to play in an outside area,
%; the agent acts on the intention to play in the outside area, and
%; the agent is at the outside area,
%; the agent plays in the outside area:
% [agent,outside,time]
% HoldsAt(IntentionToPlay(agent,outside),time) &
% HoldsAt(ActOnIntentionToPlay(agent,outside),time) &
% HoldsAt(At(agent,outside),time) ->
% Happens(Play(agent,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',115).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          intentionToPlay(Agent,Outside), 
%          Time), 
%       ','(
%          holds(
%             actOnIntentionToPlay(Agent,Outside), 
%             Time), 
%          holds(
%             at_loc(Agent,Outside), 
%             Time))), 
%    happens(
%       play(Agent,Outside), 
%       Time)).
(   happens(play(Agent, Outside), Time)
;   not intentionToPlay(Agent, Outside)at Time
;   not actOnIntentionToPlay(Agent, Outside)at Time
;   not at_loc(Agent, Outside)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',115).

 /*   (   happens(play(Agent, Outside), Time)
        ;   at(not(intentionToPlay(Agent, Outside)), Time)
        ;   at(not(actOnIntentionToPlay(Agent, Outside)), Time)
        ;   at(not(at_loc(Agent, Outside)), Time)
        ).
 */
 %  % =================================.


%; Effect axioms state that if an agent plays in an
%; outside area, the agent will be satiated from play
%; and will no longer have an intention to play in
%; the outside area:
% [agent,outside,time]
% Initiates(Play(agent,outside),SatiatedFromPlay(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',125).
% From E: 
% 
% initiates_at(
%    play(Agent,Outside), 
%    satiatedFromPlay(Agent), 
%    Time).
play(Agent, Outside)initiates satiatedFromPlay(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',125).

 /*  initiated(happens(play(Agent,Outside),
     		  Time_from,
     		  Time_until),
     	  satiatedFromPlay(Agent),
     	  []).
 */
 %  % =================================.


% [agent,outside,time]
% Terminates(Play(agent,outside),IntentionToPlay(agent,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',129).
% From E: 
% 
% terminates_at(
%    play(Agent,Outside), 
%    intentionToPlay(Agent,Outside), 
%    Time).
play(Agent, Outside)terminates intentionToPlay(Agent, Outside).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',129).

 /*  terminated(happens(play(Agent,Outside),
     		   Time_from,
     		   Time_until),
     	   intentionToPlay(Agent,Outside),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',131).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.lps.pl')).

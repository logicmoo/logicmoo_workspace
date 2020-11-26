% <?xml version="1.0"?>
/*************************************************************************

    File: actionDatabase.pl
    Copyright (C) 2007 

    Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/
:- swi_module(modActionDatabase, []).

/*
:-swi_module(modActionDatabase,[take/3,
			  eat/2,
			  drop/3,
			  throw/3,
			  put/3,
			  kiss/2,
			  kill/3,
			  look/2,
			  standup/3,
			  move/4,
			  unlock/3,
			  open/2,
			  lock/3,
			  shut/2,
			  sitdown/3,
			  wear/2 
]).  
*/

/*************************************************************************

    Function: This is the action database of the scenario FairyTaleCastle.

    Technical: The thematic roles of the actions are obtained from the core 
    features of FrameNet.

    Wishlist: Another thing is that actions need to be primitives (I
    think in most of the AI literature actions are taken to be basic
    constructs). Modification of an action is a problem. For isa,
    you could have

    Take the key with your left hand!
    Lock the door with your right hand!
    Open the door carefully!
    etc.

    where the action is complex. An ad-hoc way of dealing with this is
    to have action schemata for all possible combinations (i.e. one
    for taking a key with the left hand, one for taking the key with
    the right hand, etc.) But this option ignores the compositional
    nature of instructions. And you would need an infinite number of
    action schemata.

    It would be interesting to have action schemas and perhaps action
    modifier schemas that extend existing action schemes. So for the
    above example you could have an action scheme for "take X" (as you
    already have) and action modifier scheme for "with Y hand" and
    combine the two when needed.


*************************************************************************/

% :- use_module('NLPModules/Actions/execute',[k/1,notk/1,add/1,del/1)]).

% TODO: Design the addition and deletion of the player effects.  
% TODO: Translate into PKS syntax:
% 1. Delete the pecentage symbol
% 2. Check if PKS accepts variables without question mark
% 3. Define all the predicates, functions and constants
% 4. Replace the letters k by K
% 5. In the preconditions, replace commas by ^
% 6. In the effects replace commas by ;

%-->
%<pks>
%<domain name="fairytalecastle">
%    <symbols>
%      <predicates>
%        takeAble/1, complete ...
%      </predicates>
%      <functions>
%
%      </functions>
%      <constants>
%        nirvana
%      </constants>
%    </symbols>

%<actions>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Something -->
% <action name="look">
%	<params>Agent,Something</params>
actionDef(
look(agent(Agent),something(Something)) , [
%   <preconds>
        k(inRegion(Agent,Something)),
	k(tRegion(Something)),
%	</preconds>
%	<effects>
	ain(contains(Something,Agent)),
	ain(knowsAllAbout(Agent,Something))]).
%	</effects>
%</action>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Something -->
% <action name="look">
%	<params>Agent,Something</params>
actionDef(
look(agent(Agent),something(Something)) , [
%   <preconds>
	notk(inRegion(Agent,Something)),
	k(canSee(Agent,Something)),
%	</preconds>
%	<effects>
	ain(knowsAllAbout(Agent,Something))]).
%	</effects>
%</action>

% <!-- removing: An Agent causes a Something to move away from a location, the Source -->
% <action name="take">
%	<params>Agent,Something,Source</params>
actionDef(
take(agent(Agent),something(Something),source(Source)) , [
%       <preconds>
	k(takeAble(Something)),
	k(canAccess(Agent,Something)),
	notk(possess(Agent,Something)),
	k(supports(Source,Something)),
%	</preconds>
%	<effects>
	del(supports(Source,Something)),
	ain(possess(Agent,Something))]).
%	</effects>
%</action>

% <!-- taking: An Agent removes a Something from a Source so that it is in the Agent's possession -->
% <action name="take">
%	<params>Agent,Something,Source</params>
actionDef(
take(agent(Agent),something(Something),source(Source)) , [
%       <preconds>
	k(takeAble(Something)),
	k(canAccess(Agent,Something)),
	notk(possess(Agent,Something)),
	k(contains(Source,Something)),
%	</preconds>
%	<effects>
	del(contains(Source,Something)),
	ain(possess(Agent,Something))]).
%	</effects>
%</action>

% <!-- unlocking:  -->
% <action name="unlock">
%	<params>Agent,Something,Instrument</params>
actionDef(
unlock(agent(Agent),something(Something),instrument(Instrument)) , [
%   <preconds>
	k(canAccess(Agent,Something)),
	k(locked(Something)),
	pk(fitsin(Instrument,Something)),
	k(possess(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(locked(Something)),
	ain(unlocked(Something)),
	ain(fitsin(Instrument,Something))]).
%	</effects>
%</action>


% <!-- locking:  -->
% <action name="lock">
%	<params>Agent,Something,Instrument</params>
actionDef(
lock(agent(Agent),something(Something),instrument(Instrument)) , [
%   <preconds>
	k(canAccess(Agent,Something)),
	k(closed(Something)),
	k(unlocked(Something)),
	k(fitsin(Instrument,Something)),
	k(possess(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(unlocked(Something)),
	ain(locked(Something))]).
%	</effects>
%</action>

% <!-- closure: The Agent opens/closes the Containing-object -->
% <action name="open">
%	<params>Agent,Object</params>
actionDef(
open(agent(Agent),item(Object)) , [
%   <preconds>
	k(tOpenCloseable(Object)),
        k(tContainer(Object)),
	k(closed(Object)),
	k(unlocked(Object)),
	k(canAccess(Agent,Object)),
%	</preconds>
%	<effects>
	del(closed(Object)),
	ain(open(Object)),
	ain(describe(contentsFn(Object)))]).
%	</effects>
%</action>

% <!-- closure: The Agent opens/closes the Containing-object -->
% <action name="shut">
%	<params>Agent,Object</params>
actionDef(
shut(agent(Agent),item(Object)) , [
%   <preconds>
	k(tOpenCloseable(Object)),
        k(tContainer(Object)),
	k(open(Object)),
	k(canAccess(Agent,Object)),
%	</preconds>
%	<effects>
	del(open(Object)),
	ain(closed(Object))]).
%	</effects>
%</action>

% <!-- ingestion: An Ingestor consumes food, drink, or smoke (Ingestibles) 
% <action name="eat">
%	<params>Ingestor,Ingestible</params>
actionDef(
eat(ingestor(Ingestor),ingestible(Ingestible)) , [
%   <preconds>
	k(findsEdible(Ingestor,Ingestible)),
	notk(findsDisgusting(Ingestor,Ingestible)),
	k(contains(Ingestor,Ingestible)),
%	</preconds>
%	<effects>
	del(contains(Ingestor,Ingestible)),
	ain(tDeleted(Ingestible))]).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Something to undergo directed motion, 
% the Agent has control of the Something only at the Source of motion-->
% <action name="drop">
%	<params>Agent,Something,Target</params>
actionDef(
drop(agent(Agent),something(Something),goal(Target)) , [
%       <preconds>
	k(contains(Target,Agent)),
	k(possess(Agent,Something)),
%	</preconds>
%	<effects>
	del(possess(Agent,Something)),
	ain(contains(Target,Something))]).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Something to undergo directed motion, 
% the Agent has control of the Something only at the Source of motion-->
% <action name="throw">
%	<params>Agent,Something,Target</params>
actionDef(
throw(agent(Agent),something(Something),goal(Target)) , [
%       <preconds>
	k(possess(Agent,Something)),
	k(tThinking(Something)),
	k(tContainer(Target)),
%	</preconds>
%	<effects>
	del(possess(Agent,Something)),
	del(tThinking(Something)),
	ain(contains(Target,Something)),
	ain(tCorpse(Something))]).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something at a location, the Target, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Something,Target</params>
actionDef(
put(agent(Agent),something(Something),goal(Target)) , [
%	<preconds>
	k(possess(Agent,Something)),
	k(tContainer(Target)),
	k(canAccess(Agent,Target)),
%	</preconds>
%	<effects>
	del(possess(Agent,Something)),
	ain(contains(Target,Something))]).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something at a location, the Target, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Something,Target</params>
actionDef(
put(agent(Agent),something(Something),goal(Target)) , [
%	<preconds>
	k(possess(Agent,Something)),
	notk(tContainer(Target)),
	k(canAccess(Agent,Target)),
%	</preconds>
%	<effects>
	del(possess(Agent,Something)),
	ain(supports(Target,Something))]).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something on himself -->
% <action name="wear">
%	<params>Agent,Something</params>
actionDef(
wear(agent(Agent),something(Something)) , [
%	<preconds>
	k(possess(Agent,Something)),
	k(canAccess(Agent,Something)),
	k(wearable(Something)),
%	</preconds>
%	<effects>
	del(possess(Agent,Something)),
	ain(supports(Agent,Something))]).
%	</effects>
%</action>

% <!-- manipulation: Touch or caress with the lips as a sign of love, affection, 
% or greeting. -->
% <action name="kiss">
%	<params>Agent,Entity</params>
actionDef(
kiss(agent(Agent),entity(Entity)) , [
%	<preconds>
	k(tThinking(Entity)),
	k(canAccess(Agent,Entity)),
	k(findsBeautiful(Entity,Agent)),
%	</preconds>
%	<effects>
	ain(happy(Entity)),
	ain(victorious(Agent))]).
%	</effects>
%</action>

% <!-- manipulation: Touch or caress with the lips as a sign of love, affection, 
% or greeting. -->
% <action name="kiss">
%	<params>Agent,Entity</params>
actionDef(
kiss(agent(Agent),entity(Entity)) , [
%	<preconds>
	k(tThinking(Entity)),
	k(canAccess(Agent,Entity)),
	notk(findsBeautiful(Entity,Agent)),
%	</preconds>
%	<effects>
	ain(bored(Entity))]).
%	</effects>
%</action>


% TODO: We could define only one action kill with 3 parameters. 
% Replacing the concepts easytokill and notsoeasytokill with a role like
% kills(knife1,dragon1), for example. In order to have the default use of
% 'kill the frog' (with your hands) the player will start knowing that
% kills(hands,frog1) or sth like that. These things could be defined in the
% TBOX. Needs thinking. -->

% <!-- A Killer causes the death of the Victim. -->
% <action name="kill">
%	<params>Killer,Victim</params>
actionDef(
kill(killer(Killer),victim(Victim),instrument(_Instrument)) , [
%	<preconds>
	k(tThinking(Victim)),
	k(easytokill(Killer,Victim)),
	k(canAccess(Killer,Victim)),
%	</preconds>
%	<effects>
	del(tThinking(Victim)),
	ain(tCorpse(Victim))]). % TODO Here tCorpse is verbalized but not asserted because it's not a primitive concept
					   % This treatment might be useful for other cases, keep it in mind.  
%	</effects>
%</action>

% <!-- A Killer causes the death of the Victim. -->
% <action name="kill">
%	<params>Killer,Victim,Instrument</params>
actionDef(
kill(killer(Killer),victim(Victim),instrument(Instrument)) , [
%	<preconds>
	k(tThinking(Victim)),
	k(canAccess(Killer,Victim)),
        k(canAccess(Killer,Instrument)),
	k(weapon(Instrument)),
%	</preconds>
%	<effects>
	del(tThinking(Victim)),
	ain(tCorpse(Victim))]). % TODO Here tCorpse is verbalized but not asserted because it's not a primitive concept
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
actionDef(
standup(protagonist(Protagonist),source(Source),goal(Target)) , [
%	<preconds>
	k(contains(Source,Protagonist)),
	k(seated(Protagonist)),
	k(contains(Target,Source)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	ain(contains(Target,Protagonist))]).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
actionDef(
sitdown(protagonist(Protagonist),source(Source),goal(Target)) , [
%	<preconds>
	k(contains(Source,Protagonist)),
	notk(seated(Protagonist)),
	k(seating(Target)),
	k(contains(Source,Target)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	ain(contains(Target,Protagonist))]).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
actionDef(
move(protagonist(Protagonist),exit(Exit),goal(Target),source(Source)) , [
%	<preconds>
	k(inRegion(Protagonist, Source)),
	% I'd like to add here k(hereroom(Room)), k(hasexit(Room,Exit))
	notk(seated(Protagonist)),
	k(hasexit(Source,Exit)),
	k(leadsto(Exit,Target)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	ain(contains(Target,Protagonist)),
	ain(leadsto(Exit,Target)),
	ain(knowsAllAbout(Protagonist,Source))]).
%	</effects>
%</action>

%</actions>
%</domain>


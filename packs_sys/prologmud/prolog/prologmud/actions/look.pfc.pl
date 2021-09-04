% look.pl
% June 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/* * module *
% This file defines the basic look action
% Agents will use the predicate:
% mudGetPrecepts(Agent,Percepts) = list of lists of objects in agents location plus 2 locations in each direction
% mudNearReach(Agent,Percepts) = list of lists of objects in agents atloc plus 1 atloc in each dir
% mudNearBody(Agent,Percepts) = list of objects in agents location
%

%
% props(Obj,height(ObjHt))  == svo(Obj,height,ObjHt) == p(height,Obj,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == ain(p(height,Obj,ObjHt)) == ain(p(height,Obj,ObjHt)) == ain(height(Obj,ObjHt))
*/

% :-swi_module(user). 
:-swi_module(modLook, []).

:-export((  mudGetPrecepts/2,  mudNearReach/2, mudNearBody/2,  mudCanSense/5 , cmdLook/2)).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

:- dynamic blocks/1.


% mudCanSense(Agent,Sense,InList,CanDetect,CantDetect).
mudCanSense(_Agent,visual,InList,InList,[]).


baseKB:action_info(actExamine(tItem), "view details of item (see also @ftListFn)").
baseKB:agent_call_command(_Gent,actExamine(SObj)):- xlisting(SObj).

visibleTo(Agent,Agent).
visibleTo(Agent,Obj):-mudPossess(Agent,Obj).
visibleTo(Agent,Obj):-same_regions(Agent,Obj).


tCol(txtPrepOf).
tCol(txtPrepSpatial).
impl_coerce_hook(StrIn,txtPrepSpatial,Str):-member(Prep,[in,on,north_of,inside,onto,ontop]),name_text(Prep,StrIn),name_text(Prep,Str).
impl_coerce_hook(Prep,txtPrepSpatial,Inst):-impl_coerce_hook(Prep,txtPrepOf,Inst).
impl_coerce_hook([SDir,of],txtPrepOf,vDirFn(Dir)):-impl_coerce_hook(SDir,vtDirection,Dir).

==> vtVerb(actLook).

baseKB:action_info(actLook, "generalized look in region").
baseKB:action_info(actLook(isOptionalStr('in'),isOptionalStr('here')), "generalized look in region").
baseKB:action_info(actLook(txtPrepOf,isOptionalStr("self")), "Look in a direction (TODO: look north of vHere)").
baseKB:action_info(actLook(isOptional(txtPrepSpatial,"at"),tObj),"look [in|at|on|under|at] somewhere").
%baseKB:action_info(look(obj), "Look at a speficific item").
%baseKB:action_info(look_at(isOptional(call(visibleTo(vHere,value)),call(visibleTo(vHere,value)))), "Look at a speficific item").

baseKB:agent_call_command(Agent,actLook):- look_as(Agent),!.
baseKB:agent_call_command(Agent,actLook('here')):- look_as(Agent),!.
baseKB:agent_call_command(Agent,actLook(_,'here')):- look_as(Agent),!.
baseKB:agent_call_command(Agent,actLook(DirS,'self')):- coerce(DirS,vtDirection,Dir),!,
   view_dirs(Agent,[[Dir,vHere],[Dir,Dir],[Dir,Dir,vAdjacent]],Percepts),
   forall_member(P,Percepts,baseKB:agent_call_command_now(Agent,actExamine(P))).
baseKB:agent_call_command(Agent,actLook(_Dir,SObj)):-
   objects_match_for_agent(Agent,SObj,tObj,Percepts),
   forall_member(P,Percepts,baseKB:agent_call_command_now(Agent,actExamine(P))).

:-export(look_as/1).
look_as(Agent):- mudAtLoc(Agent,LOC),cmdLook(Agent,LOC),!.

:-export(cmdLook/2).
cmdLook(Agent,LOC):-
  ignore(current_agent(Agent)),
  garbage_collect_atoms, call_u(call(cmdLook_proc,Agent,LOC)),!.

ai_look(Buffer):-
 BufferH= b([]),
 ignore(current_agent(Agent)),
 findall(Show,on_command_show(Agent,actLook,Show),MORELOOK),
  % implicit in next command clr(props(Agent,mudNeedsLook(_))),
   show_kb_preds_to_buffer(Agent,_LOC,MORELOOK,BufferH),
    nop(must(show_inventory(Agent,Agent))),!,arg(1,BufferH,Buffer).

 

:-export(cmdLook_proc/3).
cmdLook_proc(Agent,LOC):- 
   with_no_modifications(locally(mpred_prop(nameString,2,prologListValued),
   cmdLook_proc_0(Agent,LOC))),
   ain(props(Agent,mudNeedsLook(vFalse))).

cmdLook_proc_0(Agent,LOC):-
 findall(Show,on_command_show(Agent,actLook,Show),MORELOOK),
  % implicit in next command clr(props(Agent,mudNeedsLook(_))),
   show_kb_preds(Agent,LOC,MORELOOK),
    must(show_inventory(Agent,Agent)),!.

:- multifile(on_command_show/3).
:- dynamic(on_command_show/3).
on_command_show(Agent,actLook,Show):- 
  ignore((once(mudAtLoc(Agent,LOC);localityOfObject(Agent,LOC)))),
  ignore((once(locationToRegion(LOC,Region);localityOfObject(Agent,Region);LOC=Region))),
   (on_look_show_region(Region,Show);on_look_show_agent_region(Agent,Show)).

on_look_show_region(Here,Show):-
  member(Show,
       [
         location= Here,
      % TODO make this work
         %  why does this this work on Prolog REPL?
         %   with_output_to(string(Str),cmdShowRoomGrid('Area1000'))
         %  but yet this doent?
       %   cmdShowRoomGrid = once(with_output_to(string(value),cmdShowRoomGrid(region))),
         % for now workarround is 

         call((cmdShowRoomGrid(Here),!)),
         nameStringsList(Here,value),
         forEach(mudDescription(Here,Value),fmt(mudDescription(Value))),
         events=clause_u(mudDeliverableLocationEvents(isSelfAgent,Here,value),true),
         path(D) = pathDirLeadsTo(Here,D,value),
         pathName(D) = pathName(Here,D,value),
         localityOfObject(value,Here)]).

on_look_show_agent_region(Agent,Show):-
     member(Show,
         [fmt(selfAgent= Agent),
         mudAtLoc(Agent,value),
         mudHeightOnObj(Agent,value),
         mudFacing(Agent,value),
         mudStance(Agent,value),
         mudNearBody(Agent,value),
         mudNearReach(Agent,value),
         mudGetPrecepts(Agent,value),                  
         mudMoveDist(Agent,value),
         mudLastCmdSuccess=wasSuccess(Agent,_What,value)]).


cmdLookTest(Agent,LOC):-current_agent(Agent),mudAtLoc(Agent,LOC),
     show_kb_preds(Agent,LOC,
         [
         mudAtLoc(Agent,value),
         nameStringsList(vHere,value)]),!.


:-export(nameStringsList/2).

nameStringsList(Region,ValueList):-
  findall(Value,nameString(Region,Value),ValueList).

tLooking(Agent):- current_agent(Agent),!.
tLooking(Agent):- tAgent(Agent),not(tDeleted(Agent)).

% ********** TOP LEVEL PREDICATE: this is the predicate agents use to look
% Look, reports everything not blocked up to two locations away
% plus the agents score, damage, charge, and if they succeeded at their last action.
% To make this action take a turn, change the first line to:
% Impliment(get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv)) :-
get_all(Agent,Vit,Dam,What=Suc,Scr,Percepts,Inv) :-
  call_u((
	tLooking(Agent),
	mudEnergy(Agent,Vit),
        mudHealth(Agent,Dam),
	wasSuccess(Agent,What,Suc),
	mudScore(Agent,Scr),
	mudPossess(Agent,Inv),
	mudGetPrecepts(Agent,Percepts))),!.


% Get only the Percepts

prologBuitlin(mudGetPrecepts(tAgent,ftListFn(tSpatialThing)),[predicateConventionMt(user)]).
mudGetPrecepts(Agent,Percepts) :- mudGetPrecepts0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts).
mudGetPrecepts0(Agent,Percepts) :-
  call_u((
	tLooking(Agent),
	view_vectors(Agent,Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Agent,Dirs,Tmp_percepts,Percepts))),
	!.

% Look at locations immediately around argent
% prologBuitlin(mudNearReach(tAgent,ftListFn(tSpatialThing)),[predicateConventionMt(user)]).
mudNearReach(Agent,PerceptsO):- get_near0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).
   
get_near0(Agent,Percepts) :-
  call_u((
	tLooking(Agent),
	near_vectors(Dirs),
	view_dirs(Agent,Dirs,Percepts))),!.

% Look only at location tAgent is currently in.
% prologBuitlin(mudNearBody(tAgent,ftListFn(tSpatialThing)),[predicateConventionMt(user)]).
mudNearBody(Agent,PerceptsO) :-  get_feet0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).

get_feet0(Agent,Percepts):-
  call_u((
	tLooking(Agent),
	mudAtLoc(Agent,LOC),
        mudFacing(Agent,Facing),
        reverse_dir(Facing,Rev),
	get_mdir_u(Agent,[Facing,Rev],LOC,Percepts))),
	!.

==>pddlObjects(vtDirection,[vNorth,vSouth,vEast,vWest,vNE,vNW,vSE,vSW]).


%View list starting at vac'vSouth position and moving out in a clockwise spiral
%old_view_list([[vEast,vWest],[vNorth,vHere],[vNE,vHere],[vEast,vHere],[vSE,vHere],[vSouth,vHere],[vSW,vHere],
%	   [vWest,vHere],[vNW,vHere],[vNorth,vNorth],[vNorth,vNE],[vNE,vNE],[vEast,vNE],[vEast,vEast],[vEast,vSE],
%	   [vSE,vSE],[vSouth,vSE],[vSouth,vSouth],[vSouth,vSW],[vSW,vSW],[vWest,vSW],[vWest,vWest],[vWest,vNW],
%	   [vNW,vNW],[vNorth,vNW]]).

%grid of view, upper left (vNW) to lower right (vSE)
%This is the order the agents will receive their Percepts returned from get_all(Agent,) in
view_vectors(_Agent,[[vNW,vNW],[vNorth,vNW],[vNorth,vNorth],[vNorth,vNE],[vNE,vNE],
	    [vWest,vNW],[vNW,vHere],[vNorth,vHere],[vNE,vHere],[vEast,vNE],
	    [vWest,vWest],[vWest,vHere],[vDown,vUp],[vEast,vHere],[vEast,vEast],
	    [vWest,vSW],[vSW,vHere],[vSouth,vHere],[vSE,vHere],[vEast,vSE],
	    [vSW,vSW],[vSouth,vSW],[vSouth,vSouth],[vSouth,vSE],[vSE,vSE]]).

% A view list of only the locations immediately surrounding the tAgent.
near_vectors([[vNW,vHere],[vNorth,vHere],[vNE,vHere],
	[vWest,vHere],[vDown,vUp],[vEast,vHere],
	[vSW,vHere],[vSouth,vHere],[vSE,vHere]]).

:-dynamic(visually_blocked/2).
==>prologDynamic(visually_blocked(tAgent,ftListFn(vtDirection))).

% :-listing(visually_blocked).

% Series of predicates to modify agents vision so return 'dar(k)' for locations
% which are blocked from view
% check_for_blocks(_Agent) :-!.
check_for_blocks(Agent) :-
	mudHeightOnObj(Agent,Ht),
	clr(visually_blocked(Agent,_)),
	Dirs = [[vNorth,vHere],[vSouth,vHere],[vEast,vHere],[vWest,vHere],
	[vNE,vHere],[vNW,vHere],[vSE,vHere],[vSW,vHere]],
	view_dirs(Agent,Dirs,Percepts),
	blocked_percepts(Ht,Dirs,Percepts,[],Blocked_Percepts),
	ain(visually_blocked(Agent,Blocked_Percepts)).
check_for_blocks(_,[]).

==>prologSingleValued(mudSize(tSpatialThing,ftTerm)).
==>prologSingleValued(mudShape(tSpatialThing,vtShape)).
==>prologSingleValued(mudHeightOnObj(tSpatialThing,ftNumber)).
meta_argtypes(mudTexture(tSpatialThing,vtTexture)).

prologHybrid(mudHeightOnObj(tSpatialThing,ftNumber)).
% High enough to see over obstacles??
% Check to see how tall the tAgent is and if they are standing on an item
mudHeightOnObj(Agent,Ht) :-
	mudAtLoc(Agent,LOC),
	mudAtLocList(LOC,Objs),
	member(Obj,Objs),
	iprops(Obj,mudHeight(ObjHt)),
	mudHeight(Agent,AgHt),
	Ht = (AgHt + ObjHt) - 1,!.
mudHeightOnObj(Agent,Ht) :-
	mudHeight(Agent,Ht),!.


% Figure out if any obstacles are blocking vision...
blocked_percepts(_,[],[],Blocked_Percepts,Blocked_Percepts).
blocked_percepts(AgHt,[[D1,_]|Drest],[[P1|_]|Prest],Blocked_sofar,Blocked_Percepts) :-
	props(P1,mudHeight(ObjHt)),
	ObjHt > AgHt,
	block_coverage(D1,D1,Hidden),
	append(Hidden,Blocked_sofar,Blocked_sofar_tmp),
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar_tmp,Blocked_Percepts).
blocked_percepts(AgHt,[_|Drest],[_|Prest],Blocked_sofar,Blocked_Percepts) :-
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar,Blocked_Percepts).

% Blocks view for inbetween locations (eg.[vNorth,vHere] would block [vNorth,vNorth],[vNorth,vNE],[vNorth,vNW]).
block_coverage(vNorth,vNorth,[[vNorth,vNorth],[vNorth,vNE],[vNorth,vNW]]).
block_coverage(vSouth,vSouth,[[vSouth,vSouth],[vSouth,vSE],[vSouth,vSW]]).
block_coverage(vWest,vWest,[[vWest,vWest],[vWest,vNW],[vWest,vSW]]).
block_coverage(vEast,vEast,[[vEast,vEast],[vEast,vNE],[vEast,vSE]]).
block_coverage(D1,D2,[[D1,D2]]).

% These three predicates modifies Percepts so that blocked locations return 'dark'
alter_view(_Agent,[],[],[]).
alter_view(Agent,[[D1,D2]|Drest],[TP|TPrest],[P|Prest]) :-
	mem_test(Agent,D1,D2,YorN),
	alter_view(Agent,Drest,TPrest,Prest),
	dark_if_yes(YorN,[TP],P).

mem_test(Agent,D1,D2,YorN) :-
	visually_blocked(Agent,Bdirs),
	prop_memb([D1,D2],Bdirs),
	YorN = yes.
mem_test(_Agent,_,_,no).

dark_if_yes(yes,_,[vDark]).
%dark_if_yes(no,[[]],[]).
dark_if_yes(no,[P],P).

% Builds the Percepts ftListFn. (everything located up to 2 locations away from tAgent).
view_dirs(_,[],[]).
view_dirs(Agent,[[D1|D2]|Rest],Percepts) :-
      tLooking(Agent),
	view_dirs(Agent,Rest,Psofar),
	mudAtLoc(Agent,LOC),
	get_mdir_u(Agent,[D1|D2],LOC,What),
	append([What],Psofar,Percepts).

% The look loop (look at one location)
get_mdir(_Gent,[],LOC,What) :-
	mudAtLocList(LOC,What).
get_mdir(_Gent,[vHere],LOC,What) :-
	mudAtLocList(LOC,What).
get_mdir(Agent,[Dir|D],LOC,What) :-
	from_dir_target(LOC,Dir,XXYY),
	get_mdir(Agent,D,XXYY,What).

% The look loop (look at one location)
get_mdir_u(_Gent,[],LOC,What) :-
	mudAtLocList(LOC,What).
get_mdir_u(_Gent,[vHere],LOC,What) :-
	mudAtLocList(LOC,What).
get_mdir_u(Agent,[Dir|D],LOC,What) :-
	from_dir_target(LOC,Dir,XXYY),
	get_mdir_u(Agent,D,XXYY,What).
get_mdir_u(Agent,[_|D],LOC,What) :- 
   get_mdir_u(Agent,D,LOC,What).

% Reports everything at a location.
mudAtLocList(LOC,List) :-
	findall(Z,mudAtLoc(Z,LOC),List).

% Converts the objects seen... basically to weed out the 0'vSouth the empty locations mudAtLocList
mask([],What,What).
mask([K|Tail],SoFar,What) :-
	(K)=nil,
	!,
	mask(Tail, SoFar,What).
mask([Head|Tail],SoFar,What) :-
	mask(Tail,[Head|SoFar],What).

:- include(prologmud(mud_footer)).

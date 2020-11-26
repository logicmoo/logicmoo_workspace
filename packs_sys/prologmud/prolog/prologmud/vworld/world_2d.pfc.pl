/* * <module> 
% This module defines the way we lay out 2-D grids into room
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13,2035
%
*/
% :-swi_module(world_2d,[]).

:-export(((
         check_for_fall/3,
         dir_offset/5,
         doorLocation/5,
         grid_size/4,
         in_grid/2,
         in_grid_rnd/2,
         in_world_move/3,
         is_3d/1,
         loc_to_xy/4,
         move_dir_target/3,
         number_to_dir/3, 
         reverse_dir/2,
         round_loc/8,
         round_loc_target/8,
         to_3d/2))).

:- include(prologmud(mud_header)).

:- do_gc.

grid_dist(L1,L2,Dist):- to_3d(L1,L13D),to_3d(L2,L23D),dist(L13D,L23D,Dist),!.

dist(_,_,5).

==> prologHybrid(pathBetween_call(tRegion,vtDirection,tRegion)).

% pathBetween_call(From,DirS,To):-string(DirS),!,atom_string(Dir,DirS),!,any_to_dir(Dir,Dir2),pathDirLeadsTo(From,Dir2,To),same(Dir,Dir2).
pathBetween_call_0(From,Dir,To):-any_to_dir(Dir,Dir2),is_asserted(pathDirLeadsTo(From,Dir2,To)),same(Dir,Dir2).
pathBetween_call(From,Dir,To):-pathBetween_call_0(From,DirS,To),same(Dir,DirS).
   
% 5x5 rooms are average
%% to_3d(L1,L13D):-compound(L1)->L13D=L1; room_center(L1,X,Y,Z),L13D = xyz(L1,X,Y,Z).
to_3d(xyzFn(L1,X,Y,Z),xyzFn(L1,X,Y,Z)):- nonvar(L1),!.
to_3d(L1,xyzFn(L1,X,Y,Z)):-room_center(L1,X,Y,Z),!.


center_xyz(MaxX,MidX):- MidX is MaxX div 2 + MaxX mod 2.

room_center(Region,X,Y,Z):-
      grid_size(Region,MaxX,MaxY,MaxZ),
      center_xyz(MaxX,X),
      center_xyz(MaxY,Y),
      center_xyz(MaxZ,Z),!,
      dmsg(todo("get room size and calc center ",Region)).

loc_to_xy(LOC,X,Y,xyzFn(Region,X,Y,1)):- locationToRegion(LOC,Region),!.
loc_to_xy(Region,X,Y,xyzFn(Region,X,Y,1)).

is_3d(LOC):- compound(LOC).

% Quintus random(1,MaxX,X) and random(1,MaxY,Y)
grid_size(Room,MaxX,MaxY,MaxZ):- var(Room),!,tRegion(Room),grid_size(Room,MaxX,MaxY,MaxZ).
grid_size(Region,MaxX,MaxY,MaxZ):- fail,
   typeGrid(What,1,L),length(L,MaxX),isaOrEq(Region,What),!,
   maxZ(MaxZ),findall(1,typeGrid(What,_,_),LL),length(LL,MaxY),!.
grid_size(Room,MaxX,MaxY,MaxZ):- nonvar(Room), MaxX = 5 ,MaxY = 5 ,maxZ(MaxZ).

maxZ(2).

isaOrEq(Region,What):- isa(Region,What).
isaOrEq(Region,What):- =@=(Region,What).


in_grid(LocName,Var):-var(LocName),!,no_repeats_old([LocName,Var],(tRegion(LocName),in_grid_rnd(LocName,Var))).
in_grid(LocName,Var):-var(Var),!,(in_grid_rnd(LocName,Var);in_grid_rnd(LocName,Var);in_grid_rnd(LocName,Var)).
in_grid(LocName,Var):-in_grid_no_rnd(LocName,Var).

in_grid_no_rnd(xyzFn(LocName,X,Y,Z),xyzFn(LocName,X,Y,Z)) :- nonvar(X),!.
in_grid_no_rnd(LocName,xyzFn(LocName,X,Y,Z)) :- !,
   grid_size(LocName,MaxX,MaxY,MaxZ),!,between(1,MaxX,X),between(1,MaxY,Y),between(1,MaxZ,Z).
in_grid_no_rnd(LocName,LocName).

in_grid_rnd(LocName,xyzFn(LocName,X,Y,1)) :-
   grid_size(LocName,MaxX,MaxY,_MaxZ),
   between(1,100,_),
	X is (1 + random(MaxX-2)),
	Y is (1 + random(MaxY-2)).	
% in_grid_rnd(LocName,xyzFn(LocName,1,1,1)).

% for now not useing grids


init_location_grid(LocName):-
        isa(LocName,LocType),
        init_location_grid(LocName,LocType),!.

init_location_grid(LocName,LocType):-
        isa(LocName,LocType),
        init2(LocName,LocType,1,1).

% process map file (world.map.pl)
init2(LocName,LocType,Y,1) :-
	call_u(gridValue(LocName,1,Y,L)),
	!,
	init3(LocName,LocType,xyzFn(LocName,1,Y,_),L).
init2(_LocName,_LocType,_,_).

init3(LocName,LocType,xyzFn(LocName,_,Y,1),[]) :-
	!,
	X is Y + 1,
	init2(LocName,LocType,X,1).

init3(LocName,LocType,xyzFn(LocName,X,Y,1),[O|T]) :-
	typeHasGlyph(Type, O),
           rez_loc_object(xyzFn(LocName,X,Y,1),Type),
	K is X + 1,
	init3(LocName,LocType,xyzFn(LocName,K,Y,1),T).


% rez_loc_object(_,0):-!.
rez_loc_object(XY,Type):-
           gensym(Type,Name2),
           Name = xyN(XY,Name2),          
           assert_isa(Name,Type),
           ain(mudAtLoc(Name,XY)),!,
           find_and_call(add_missing_instance_defaults(Name)).


%prologDynamic(mudNearbyObjs(tObj,tObj)).
%prologDynamic(mudNearbyObjs(tObj,tObj)).
%predicateConventionMt(mudNearbyObjs(tObj,tObj),user).
mudNearbyObjs(X,Y):-mudAtLoc(X,L1),mudAtLoc(Y,L2),mudNearbyLocs(L1,L2).

is_location(Obj):-var(Obj),!,fail.
is_location(xyzFn(_,_,_,_)):-!.
is_location(Obj):-!,isa(Obj,tRegion),!.

locationToRegion(Obj,RegionIn):-locationToRegion_0(Obj,Region)->sanity((nonvar(Region),tRegion(Region))),!,RegionIn=Region.

locationToRegion_0(Obj,Obj):-var(Obj),dmsg(warn(var_locationToRegion(Obj,Obj))),!.
locationToRegion_0(xyzFn(Region,_,_,_),Region2):-nonvar(Region),!,locationToRegion_0(Region,Region2).
locationToRegion_0(Obj,Obj):-nonvar(Obj),!,isa(Obj,tRegion),!.
locationToRegion_0(Obj,Region):-nonvar(Obj),must(localityOfObject(Obj,Location)),!,
  locationToRegion_0(Location,Region).
locationToRegion_0(Obj,Obj):-dmsg(warn(locationToRegion(Obj,Obj))),!.

:-export(mudNearbyLocs/2).
mudNearbyLocs(L1,L2):- var(L1),nonvar(L2),!,mudNearbyLocs(L2,L1).
mudNearbyLocs(L1,L2):- nonvar(L1),nonvar(L2),L2=xyzFn(_,_,_,_),locationToRegion(L1,R),!,lc_tcall(locs_near_i(R,L2)).
mudNearbyLocs(L1,L2):- nonvar(L1),nonvar(L2),locationToRegion(L1,R1),locationToRegion(L2,R2),!,mudNearbyRegions(R1,R2).
mudNearbyLocs(L1,L2):- must((quietly(mudNearbyRegions(R1,R2)),in_grid_no_rnd(R1,L1),in_grid_no_rnd(R2,L2))).

% :- decl_not_mpred(locs_near_i,2).
:-export(locs_near_i/2).
locs_near_i(L1,L2):- locationToRegion(L1,R),in_grid_no_rnd(R,L2).
locs_near_i(L1,L2):- locationToRegion(L1,R),pathBetween_call(R,_,R2),in_grid_no_rnd(R2,L2).

mudNearbyRegions(R1,R2):-pathBetween_call(R1,_,R2).
mudNearbyRegions(R1,R1).

% 345345  instTypeProps(OfAgent,agent,[facing(F),atloc(L)]):-  dfsdfd ignore((nonvar(OfAgent),create_someval(facing,OfAgent,F),create_someval(atloc,OfAgent,L))).

% CANT transitive_other(mudAtLoc,1,Obj,What):-mudInsideOf(Obj,What).

is_at(Obj,Where):-localityOfObject(Obj,Where).
is_at(Obj,Where):-mudAtLoc(Obj,Where).
is_at(Obj,Where):-mudSubPart(What,Obj),is_at(What,Where).

% ((tObj(Obj), ~(mudPossess(_,Obj)))==>spatialInRegion(Obj)).
tPathway(Obj)==>spatialInRegion(Obj).



localityOfObject(Obj,Region),tRegion(Region)==> inRegion(Obj,Region).
mudAtLoc(Obj,LOC),{locationToRegion(LOC,Region)},tRegion(Region)==> inRegion(Obj,Region).


prologHybrid(mudInsideOf/2).
% :-sanity(( requires_storage((mudInsideOf(_G3775190, _G3775191):-is_asserted(mudStowing(_G3775191, _G3775190))))  )).
mudInsideOf(Inner,Outer):-loop_check(mudStowing(Outer,Inner)).
mudInsideOf(Inner,Outer):-loop_check(mudContains(Outer,Inner)).

moves_with(Obj1,Obj2):-nonvar(Obj2),!,moves_with(Obj2,Where),localityOfObject(Where,Obj1).
moves_with(Obj1,Obj2):-moves_with_sym(Obj1,Obj2).
moves_with(Obj1,Obj2):-moves_with_sym(Obj2,Obj1).
moves_with_sym(Obj1,Obj2):-localityOfObject(Where,Obj1),moves_with(Obj2,Where).

mudLocOnSurface(Clothes,Agent):-loop_check(wearsClothing(Agent,Clothes),fail).

:-export(same_regions/2).
same_regions(Agent,Obj):-inRegion(Agent,Where1),dif(Agent,Obj),inRegion(Obj,Where2),Where1=Where2.

==>(prologHybrid(inRegion(tObj,tRegion))).
%prologPTTP(localityOfObject(tObj,tSpatialthing)).

%:- ensure_universal_stub(prologPTTP,inRegion/2).
%:- ensure_universal_stub(prologPTTP,mudTestAgentWearing/2).

==>(prologHybrid(mudAtLoc/2)).
==>(meta_argtypes(mudAtLoc(tObj,tSpatialThing))).


% compute the most specific location description
mostSpecificLocalityOfObject(Obj,Where):-
   one_must(is_asserted(mudAtLoc(Obj,Where)),one_must(is_asserted(localityOfObject(Obj,Where)),is_asserted(inRegion(Obj,Where)))).

% :- (rtrace,trace).

% objects can be two places x,y,z's at once
((spatialInRegion(Obj),mudAtLoc(Obj,NewLoc), 
 {(mudAtLoc(Obj,OldLoc), OldLoc\==NewLoc)})
   ==>
   ~mudAtLoc(Obj,OldLoc)).


% objects are placed by default in center of region
((spatialInRegion(Obj), inRegion(Obj,Region), {
 \+ tPathway(Obj), \+ lookup_u(mudAtLoc(Obj,xyzFn(Region,_,_,_)))},
  {in_grid_rnd(Region,LOC)})
  ==>
   mudAtLoc(Obj,LOC)).


% objects cannot be in two localities (Regions?) at once
((spatialInRegion(Obj),localityOfObject(Obj,NewLoc), 
 {(localityOfObject(Obj,OldLoc), OldLoc\==NewLoc)})
  ==> 
  ~localityOfObject(Obj,OldLoc)).

% if something leaves a room get rid of old location 
((spatialInRegion(Obj),inRegion(Obj,NewRegion), 
 {(mudAtLoc(Obj,OldLoc), OldLoc\=xyzFn(NewRegion,_,_,_))})
  ==> 
  ~mudAtLoc(Obj,OldLoc)).

% if something leaves a room get rid of old inRegion/2 
((spatialInRegion(Obj),inRegion(Obj,NewRegion), 
 {dif(NewRegion,OldLoc),(inRegion(Obj,OldLoc), OldLoc\=NewRegion)})
  ==> 
  ~inRegion(Obj,OldLoc)).

:- ain((inRegion(Obj,Region)==> {ain((spatialInRegion(Obj),tRegion(Region)))})).


% create pathway objects and place them in world
/*
(pathDirLeadsTo(Region,Dir,R2)/ground(pathDirLeadsTo(Region,Dir,R2)),   
    { mudExitAtLoc(Region,Dir,LOC), Obj = apathFn_BAD1(Region,Dir) }) ==>  
                        (tPathway(Obj),localityOfObject(Obj,Region),mudAtLoc(Obj,LOC)).
*/

:- ain(tPathway(apathFn(Region,Dir)) ==> mudDoorwayDir(Region,apathFnAA(Region,Dir),Dir)).


mudExitAtLoc(Region,Dir,xyzFn(Region,X,Y,Z)):- call_u(calc_from_center_xyz(Region,Dir,2,X,Y,Z)).

% :-kif_tell(localityOfObject(A,B) &  localityOfObject(B,C) ==> localityOfObject(A,C)).

:- kb_shared(mudSubPart/2).
:- kb_shared(predInterArgIsa/1).
:- kb_shared(relationAllExists/3).

==>singleValuedInArgDefault(localityOfObject, 2, isMissing).

mudAtLoc(Who,xyzFn(Loc,_,_,_))==>localityOfObject(Who,Loc).

genls(tHominid,tAgent).
genls(tHumanBody,tBodyPart).

predInterArgIsa(mudSubPart(tBodyPart,tBodyPart)).

/* TODO Re-Enable 
relationAllExists(mudSubPart,tHominid,tHumanBody).
relationAllExists(mudSubPart,tHumanBody,tBodyPart).
relationAllExists(mudSubPart,tHumanBody,isEach(tHumanHead,tHumanNeck,tHumanUpperTorso,tHumanLowerTorso,tHumanPelvis,tHumanArms,tHumanLegs)).
relationAllExists(mudSubPart,tHumanHead,isEach(tHumanFace,tHumanHair)).
*/

predPredicateToFunction(Pred,SubjT,ObjT,FullNameFnO):- 
  is_asserted(predPredicateToFunction(Pred,SubjT,ObjT,FullNameFn)) *-> FullNameFnO=FullNameFn ; 
  (i_name('i',ObjT,Obj),i_name(Obj,Pred,ObjPred),i_name('Of',SubjT,OfSubj),concat_atom([ObjPred,OfSubj,'Fn'],FullNameFn)),simplifyFullName(FullNameFn,FullNameFnO).


simplifyFullName(FullNameFn,FullNameFn).

find_instance_of(Pred,Subj,Obj):-
 relationAllExists(Pred,SubjT,ObjT), 
 isa(Subj,SubjT), 
 ((is_asserted(t(Pred,Subj,Obj)),isa(Obj,ObjT)) *-> true ; (predPredicateToFunction(Pred,SubjT,ObjT,PredFn), Obj =.. [PredFn,Subj])).

mudInsideOf(Inner,Outer)==>mudSubPart(Outer,Inner).
wearsClothing(Agent,Clothes)==>mudSubPart(Agent,Clothes).
% mudSubPart(Subj,Obj):- (nonvar(Subj);nonvar(Obj)),!,test_tl(infThirdOrder), find_instance_of(mudSubPart,Subj,Obj).
% mudSubPart(face,isEach(eyes,nose,mouth)).
% mudSubPart([upper_torso,arms,left_arm,left_hand,left_digits]).
% mudSubPart([upper_torso,arms,right_arm,right_hand,right_digits]).
% mudSubPart([pelvis,legs,left_leg,left_foot,left_toes]).
% mudSubPart([pelvis,legs,right_leg,right_foot,right_toes]).




is_in_world(Var):- is_ftVar(Var),!,trace_or_throw(var_is_in_world(Var)).
is_in_world(apathFn(A,B)):- ground(apathFn(A,B)),!.
is_in_world(Obj):-isa_asserted(Obj,tRegion),!.
is_in_world(Obj):-lookup_u(mudAtLoc(Obj,_)),!.
is_in_world(Obj):-lookup_u(mudStowing(Who,Obj)),!,is_in_world(Who).
is_in_world(Obj):-lookup_u(mudSubPart(Who,Obj)),!,is_in_world(Who).


put_in_world(Obj):- mudAtLoc(Obj,_XYZFn),!.
put_in_world(Obj):- is_in_world(Obj),!.
put_in_world(Obj):- localityOfObject(Obj,Loc),  
  in_grid(Loc,XYZFn),unoccupied(Obj,XYZFn),!,
  ain(mudAtLoc(Obj,XYZFn)),
  ain(mudNeedsLook(Obj,vTrue)).
put_in_world(Obj):- random_xyzFn(LOC),ain(mudAtLoc(Obj,LOC)),
  ain(mudNeedsLook(Obj,vTrue)).


/*
% :-export decl_database_hook/2.  action_info
:-export(deduce_facts/2).
:-export(create_random_fact/1).
:-export( hooked_random_instance/3).
%:-export fact_always_true/1.
:-export( fact_maybe_deduced/1).
:-export( fact_is_false/2).
:-dynamic fact_is_false/2.
*/

prologHybrid(mudInsideOf(tObj,tObj)).

% facts that cant be true

%fact_is_false(mudAtLoc(Obj,_LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),is_asserted(mudInsideOf(Obj,What)),not(isa(What,tRegion)).
%fact_is_false(mudAtLoc(Obj,LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),(mudInsideOf(Obj,What)),not(mudAtLoc(What,LOC)).
%fact_is_false(localityOfObject(Obj,_LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),(mudInsideOf(Obj,What)),!.

% facts that must be true 
%  suggest a deducable fact that is always defiantely true but not maybe asserted
%TODO USE EVER? fact_always_true(localityOfObject(apathFn(Region,Dir),Region)):-is_asserted(pathDirLeadsTo(Region,Dir,_)).
fact_always_true(localityOfObject(Obj,Region)):- is_asserted(mudAtLoc(Obj,LOC)),locationToRegion(LOC,Region),!.

(((localityOfObject(_,_),{localityOfObject(apathFn(Region,Dir),Region)},
    \+ pathDirLeadsTo(Region, Dir, _)  ) ==>
     \+ localityOfObject(apathFn(Region,Dir),Region))).

%  suggest a deducable fact that is probably true but not already asserted
%TODO USE EVER? fact_maybe_deduced(localityOfObject(Obj,Region)):- is_asserted(mudAtLoc(Obj,LOC)),locationToRegion(LOC,Region),!.
%TODO USE EVER? fact_maybe_deduced(localityOfObject(apathFn(Region,Dir),Region)):-is_asserted(pathDirLeadsTo(Region,Dir,_)).

% create_and_assert_random_fact(_):- t_l:noDBaseHOOKS(_),!.
create_and_assert_random_fact(Fact):- fail,must(create_random_fact(Fact)),aina(Fact).

%  suggest a random fact that is probably is not already true
create_random_fact(G) :- into_functor_form(t,G,MPred),G\=@=MPred,!,create_random_fact(MPred).
create_random_fact(G) :- is_asserted(G),!,dmsg((create_random_fact(G) :- is_asserted(G))).
create_random_fact(t(mudAtLoc,Obj,LOC)) :- !,nonvar(Obj),is_asserted(localityOfObject(Obj,Region)),!,((in_grid(Region,LOC),unoccupied(Obj,LOC),
   \+ ( ~ mudAtLoc(Obj,LOC)))).
create_random_fact(t(localityOfObject,Obj,Region)) :- !, nonvar(Obj),not_asserted((localityOfObject(Obj,_))),
  if_defined(asserted_or_deduced(localityOfObject(Obj,Region))).
create_random_fact(t(Other,Obj,Default)) :- nonvar(Obj),argIsa(Other,2,Type),random_instance_no_throw(Type,Default,ground(Default)),!.


%  suggest random values
hooked_random_instance(vtDirection,Dir,Test) :- my_random_member(Dir,[vNorth,vSouth,vEast,vWest,vNE,vNW,vSE,vSW]),Test,!.
hooked_random_instance(ftInt,3,Test):-call(Test),dmsg(random_instance(ftInt,3,Test)),dmsg(hooked_random_instance(ftInt,3,Test)),!,fail.


random_region(LOC):- var(LOC),findall(O,isa(O,tRegion),LOCS),my_random_member(LOC,LOCS).


random_xyzFn(LOC):-
   must_det(random_instance(tRegion,Region,true)),
   in_grid_rnd(Region,LOC),!.
random_xyzFn(xyzFn('Area1000',1,1,1)):- fail, dmsg(trace_or_throw(mpred_not_loaded)).

unoccupied(_,Loc):- not_asserted((mudAtLoc(_,Loc))),!.
unoccupied(_,_):-!.
unoccupied(Obj,Loc):- loop_check(unoccupied_ilc(Obj,Loc),not_asserted((mudAtLoc(_,Loc)))),!.

unoccupied_ilc(Obj,Loc):- is_occupied(Loc,What),!,What=Obj.
unoccupied_ilc(_,_).

is_occupied(Loc,What):- is_asserted(mudAtLoc(What,Loc)),!.
is_occupied(Loc,What):- locationToRegion(Loc,Region),localityOfObject(What,Region),put_in_world(What),mudAtLoc(What,Loc),!.

% Used all over the place
% Transforms location based on cardinal direction given

calc_xyz(Region1,Dir,force(X1,Y1,Z1),X2,Y2,Z2):-
   to_3d(Region1,xyzFn(_,X,Y,Z)),
   get_dir_offset(Dir,1,OX,OY,OZ),
   X2 is X+ (OX*X1),Y2 is Y+OY*Y1,Z2 is Z+OZ*Z1.

calc_from_center_xyz(Region1,Dir,R,X2,Y2,Z2):-
   room_center(Region1,X,Y,Z),
   get_dir_offset(Dir,R,OX,OY,_),
   X2 is X+ OX ,Y2 is Y+ OY, Z2 is Z.

prologBuiltin(random_path_dir/1).

system:sys_random_path_dir(Dir):- random_path_dir(Dir).
% random_path_dir(Dir):- nonvar(Dir),random_path_dir(Dir0),Dir=Dir0,!.
random_path_dir(Dir):- no_repeats(random_path_dir0(Dir)).
random_travel_dir(Dir):- no_repeats(random_path_dir1(Dir)).

random_path_dir0(Dir):- call(call,random_instance(vtBasicDir,Dir,true)).
random_path_dir1(Dir):- call(call,random_instance(vtBasicDirPlusUpDown,Dir,true)).
random_path_dir1(Dir):- call(call,random_instance(vtDirection,Dir,true)).


from_dir_target(LOC,Dir,XXYY):- is_3d(LOC),!,
  move_dir_target(LOC,Dir,XXYY).
from_dir_target(Agent,Dir,XXYY):-
  mudAtLoc(Agent,RegionXYZ),
  move_dir_target(RegionXYZ,Dir,XXYY).

move_dir_target(RegionXYZ,Dir,XXYY):-
   move_dir_target(RegionXYZ,Dir,1,XXYY).


move_dir_target(RegionXYZ,DirS,Force,XXYY):-
   any_to_atom(DirS,Dir),
   once(((calc_xyz(RegionXYZ,Dir,force(Force,Force,Force),X,Y,Z)),
   (locationToRegion(RegionXYZ,Region1)),
   (round_loc_target(Region1,X,Y,Z,Region2,X2,Y2,Z2)),
   XXYY = xyzFn(Region2,X2,Y2,Z2),
   sanity(ground(XXYY)))),
   check_ahead_for_ground(XXYY),!.

move_dir_target(RegionXYZ,Dir,_Force,XXYY):-
   any_to_string(Dir,DirS),
   locationToRegion(RegionXYZ,Region1),!,
   pathBetween_call(Region1,DirS,Region2),
   in_grid_rnd(Region2,XXYY),
   XXYY = xyzFn(Region2,_X2,_Y2,_Z2),
   sanity(ground(XXYY)),!,
   check_ahead_for_ground(XXYY),!.


round_loc_target(Region1,X,Y,Z,Region3,X3,Y3,Z3):-
   round_loc(Region1,X,Y,Z,Region2,X2,Y2,Z2),!,
   Region2=Region3,X2=X3,Y2=Y3,Z2=Z3.

round_loc(Region1,X,Y,Z,Region2,X2,Y2,Z2):-
   compute_dir(Region1,X,Y,Z,Dir),!,
   round_loc_dir(Region1,X,Y,Z,Dir,Region2,X2,Y2,Z2),!.

round_loc_dir(Region1,X,Y,Z,'',Region2,X2,Y2,Z2):-!,
   X2=X,Y2=Y,Z2=Z,Region2=Region1.

round_loc_dir(Region1,X,Y,Z,Dir,Region2,X2,Y2,Z2):- 
   any_to_dir(Dir,DirLong),
   pathBetween_call(Region1,DirLong,Region2),!,
   grid_size(Region1,X1,Y1,Z1),
   calc_xyz(xyzFn(Region2,X,Y,Z),Dir,force(-X1,-Y1,-Z1),X2,Y2,Z2),!.

round_loc_dir(Region1,X,Y,Z,_Dir,Region2,X2,Y2,Z2):-Region2=Region1,X2=X,Y2=Y,Z2=Z.

prologBuiltin(compute_dir/5).
compute_dir(Region1,X,Y,Z,Dir):-
  grid_size(Region1,MaxX,MaxY,MaxZ),
   ((X<1 -> EW=vWest ; X > MaxX -> EW=vEast ; EW= ''),
   (Y<1 -> NS=vNorth ; Y > MaxY -> NS=vSouth ; NS= ''),
   (Z<1 -> UD=vDown ; Z > MaxZ -> UD=vUp ; UD= '')),
   atomic_list_concat_catch([NS,EW,UD],'',Dir),!.

prologBuiltin(get_dir_offset/5).
get_dir_offset(Dir,F,OX,OY,OZ):- sanity(nonvar(Dir)),
  dir_offset(Dir,F,OX,OY,OZ),!.
get_dir_offset(reverseOf(Dir),F,OX,OY,OZ):- !,get_dir_offset((Dir),F,X,Y,Z),!, OX is -X, OY is -Y, OZ is -Z.
get_dir_offset(Dir,F,OX,OY,OZ):- any_to_atom(Dir,DirA),
  dir_offset(DirA,F,OX,OY,OZ),!.
get_dir_offset(Dir,F,OX,OY,OZ):- any_to_string(Dir,DirS),
  dir_offset(DirS,F,OX,OY,OZ),!.



p2c_dir2('s','vSouth').
p2c_dir2('w','vWest').
p2c_dir2('u','vUp').
p2c_dir2('d','vDown').
p2c_dir2('e','vEast').
p2c_dir2('n','vNorth').

:-export(is_any_dir/1).
is_any_dir(Dir):-var(Dir),!,fail.
is_any_dir(Dir):-any_to_dir(Dir,_).
:-export(any_to_dir/2).

any_to_dir(D,D):-var(D),!.
any_to_dir(S,D):-string(S),string_to_atom(S,A),any_to_dir(A,D),!.
any_to_dir(D,D):-dir_offset(D,_,_,_,_),!.
any_to_dir(A,D):-p2c_dir2(D,A),!.
any_to_dir(D,O):-atom(D),sub_atom(D, 0, 1, _, S),toLowercase(S,L),p2c_dir2(L,O),!.
any_to_dir(D,D):-pathDirLeadsTo(_,D,_),!.

:-export(dir_offset/5).

% prologHybrid(dir_offset(term,int,int,int,int)).


% :-mpred_trace_all.

prologBuiltin(dir_offset/5).
dir_offset(vUp,F,0,0,F).
dir_offset(vDown,F,0,0,-F).
dir_offset(vNorth,F,0,-F,0).
dir_offset(vSouth,F,0,F,0).
dir_offset(vEast,F,F,0,0).
dir_offset(vWest,F,-F,0,0).
dir_offset(vNE,F,F,-F,0).
dir_offset(vSW,F,-F,F,0).
dir_offset(vSE,F,F,F,0).
dir_offset(vNW,F,-F,-F,0).
dir_offset(vHere,_,0,0,0).

% :-mpred_no_spy_all. with_pfa

% MergedNess -1,0,1 = contacting_at,inside,outside_near_on
with_offset(detatched,F,X,Y,Z):-dir_offset(vHere,F,X,Y,Z).
with_offset(absolute_with,F,X,Y,Z):-dir_offset(vUp,F,X,Y,Z).
with_offset(relative_from,F,X,Y,Z):-dir_offset(vDown,F,X,Y,Z).
with_offset(surrounding,F,X,Y,Z):-dir_offset(vNorth,F,X,Y,Z).
with_offset(mudInsideOf,F,X,Y,Z):-dir_offset(vSouth,F,X,Y,Z).
with_offset(on,F,X,Y,Z):-dir_offset(vEast,F,X,Y,Z).
with_offset(tPartofObj,F,X,Y,Z):-dir_offset(vWest,F,X,Y,Z).

facing_offset(at,F,X,Y,Z):-dir_offset(vHere,F,X,Y,Z).
facing_offset(above,F,X,Y,Z):-dir_offset(vUp,F,X,Y,Z).
facing_offset(below,F,X,Y,Z):-dir_offset(vDown,F,X,Y,Z).
facing_offset(left,F,X,Y,Z):-dir_offset(vWest,F,X,Y,Z).
facing_offset(right,F,X,Y,Z):-dir_offset(vEast,F,X,Y,Z).
facing_offset(behind,F,X,Y,Z):-dir_offset(vSouth,F,X,Y,Z).
facing_offset(front,F,X,Y,Z):-dir_offset(vNorth,F,X,Y,Z).


% baseKB:decl_database_hook(clause( retract,_),mudAtLoc(Agent,_)):-padd(Agent,mudNeedsLook(vTrue)).

% mudAtLoc(Agent,_)==> mudNeedsLook(Agent,vTrue).
mudAtLoc(Agent,_)==>{padd(Agent,mudNeedsLook(vTrue))}.

% dir_mult(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2):- X2 is X * X1,Y2 is Y * Y1,Z2 is Z * Z1.


% Used in move.pl,push.pl and climb.pl
% Move agent (usually). Used to relocate agent'vSouth location.
in_world_move(LOC,Agent,DirS) :-
        string_to_atom(DirS,Dir),
        ignore(is_asserted(mudAtLoc(Agent,LOC))),
        must_det((locally(t_l:infAssertedOnly(mudAtLoc),in_world_move0(LOC,Agent,Dir)),       
         is_asserted(mudAtLoc(Agent,LOC2)),
         LOC2 \== LOC)),!.

can_world_move(LOC,_Agent,Dir) :- check_behind_for_ground(LOC),move_dir_target(LOC,Dir,_).

in_world_move0(LOC,Agent,Dir) :-
      any_to_dir(Dir,DirS),
        % rtrace(padd(Agent,mudFacing(DirS))),
       % must((
         ain(mudFacing(Agent,DirS)),
        % call_u(mudFacing(Agent,DirOther)),
         %DirOther==DirS)),
        sanity((is_asserted(mudAtLoc(Agent,LOC)))),
        check_behind_for_ground(LOC),
	move_dir_target(LOC,Dir,XXYY),!,
   must_det_l_pred(show_call,(
        dmsg(move_dir_target(LOC,DirS,XXYY)),
        locationToRegion(LOC,Region1),
        locationToRegion(XXYY,Region2),
        ((expire_dont_add, clr(mudAtLoc(Agent,LOC)))),
        %rtrace,
        call((expire_dont_add, ain_expanded(mudAtLoc(Agent,XXYY)),
        %nortrace,
        sanity((is_asserted(mudAtLoc(Agent,XXYY)))),
        sanity((clause_u(mudAtLoc(Agent,LOC2)),LOC2 \== LOC)))),         
   ifThen(( Region1\==Region2) ,raise_location_event(LOC,actNotice(reciever,actLeave(Agent,Region1,to(Dir))))),
        reverse_dir(Dir,Rev),
   ifThen(( Region1\==Region2) ,raise_location_event(XXYY,actNotice(reciever,actEnter(Agent,Region2,from(Rev))))),!,
	check_for_fall(LOC,XXYY,Agent))).

check_behind_for_ground(LOC):-nonvar(LOC).
check_ahead_for_ground(XXYY):-nonvar(XXYY),
   to_3d(XXYY,xyzFn(L1,X,Y,Z)),
   grid_size(L1,MX,MY,MZ),
   inside_grid(L1,X,Y,Z,MX,MY,MZ).

inside_grid(_L1,X,Y,Z,MX,MY,MZ):-is_between(1,MX,X),is_between(1,MY,Y),is_between(1,MZ,Z).

is_between(L,H,V):- H >= V,L =< V.

% Used for every move
% Does the agent take a header off a high object?
check_for_fall(LOC,XXYY,Agent) :-
	mudAtLoc(HighObj,LOC),
	props(HighObj,mudHeight(Hh)),
        % if nothing is there pretend it is 1
	(\+ (mudAtLoc(_,XXYY)) -> Hl = 1; mudAtLoc(LowObj,XXYY)),
	props(LowObj,mudHeight(Hl)),
	Hd is Hh - Hl,
	Hd > 1,
	call_update_stats(Agent,fall).
check_for_fall(_,_,_).


% Reverses the direction returned by number_to_direction
% Used for fleeing
:- export(reverse_dir/2).
:- public(reverse_dir/2).
reverse_dir(W,R):-string(W),atom_string(A,W),!,reverse_dir0(A,RA),atom_string(RA,R),!.
reverse_dir(A,R):-reverse_dir0(A,R)*->true;reverse_dir1(A,R).

reverse_dir1(reverseOf(Was),RWas):-nonvar(Was),!,RWas=Was.
reverse_dir1(skPathFn(Direction,R2,R1),skPathFn(Direction,R1,R2)):-nonvar(Direction),!.
reverse_dir1(Was,reverseOf(Was)):-nonvar(Was),!.


reverse_dir0(vSouth,vNorth).
reverse_dir0(vEast,vWest).
reverse_dir0(vNorth,vSouth).
reverse_dir0(vWest,vEast).
reverse_dir0(vUp,vDown).
reverse_dir0(vDown,vUp).
reverse_dir0(vNW,vSE).
reverse_dir0(vNE,vSW).
reverse_dir0(vSW,vNE).
reverse_dir0(vSE,vNW).


% Yet another hash table to covert numbers into aDirectionsFn (or the reverse).
num_near_reverse(1,vNW,vHere).
num_near_reverse(2,vNorth,vHere).
num_near_reverse(3,vNE,vHere).
num_near_reverse(4,vWest,vHere).
num_near_reverse(6,vEast,vHere).
num_near_reverse(7,vSW,vHere).
num_near_reverse(8,vSouth,vHere).
num_near_reverse(9,vSE,vHere).

num_near_reverse(0,vDown,vHere).
num_near_reverse(5,vUp,vHere).

% Translates numbers returned from scan_lists_aux/3 (the number of the location)
% into thier relative aDirectionsFn.
number_to_dir(1,vNW,vNW).
number_to_dir(2,vNorth,vNW).
number_to_dir(3,vNorth,vNorth).
number_to_dir(4,vNorth,vNE).
number_to_dir(5,vNE,vNE).
number_to_dir(6,vWest,vNW).
number_to_dir(7,vNW,vHere).
number_to_dir(8,vNorth,vHere).
number_to_dir(9,vNE,vHere).
number_to_dir(10,vEast,vNE).
number_to_dir(11,vWest,vWest).
number_to_dir(12,vWest,vHere).
number_to_dir(14,vEast,vHere).
number_to_dir(15,vEast,vEast).
number_to_dir(16,vWest,vSW).
number_to_dir(17,vSW,vHere).
number_to_dir(18,vSouth,vHere).
number_to_dir(19,vSE,vHere).
number_to_dir(20,vEast,vSE).
number_to_dir(21,vSW,vSW).
number_to_dir(22,vSouth,vSW).
number_to_dir(23,vSouth,vSouth).
number_to_dir(24,vSouth,vSE).
number_to_dir(25,vSE,vSE).


% Scans through list of perceptions (as returned by look_percepts(Agent,L) or look_all(NearAgt,_,_,_,L,_))
% for an object,returns the direction in which the object lies.
list_object_dir_sensed(_,List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	number_to_dir(N,Dir,_).


list_object_dir_near(List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	num_near_reverse(N,Dir,_).

scan_lists_aux([Loc|_],Type,N,N) :-
	member(Obj,Loc),
        isa(Obj,Type),
	!.
scan_lists_aux([_|Rest],Type,M,N) :-
	Mtemp is M + 1,
	!,
	scan_lists_aux(Rest,Type,Mtemp,N).


doorLocation(_Room,3,0,_Z,vNorth).
doorLocation(_Room,2,0,_Z,vNorth).
doorLocation(_Room,4,0,_Z,vNorth).
doorLocation(_Room,3,6,_Z,vSouth).
doorLocation(_Room,2,6,_Z,vSouth).
doorLocation(_Room,4,6,_Z,vSouth).
doorLocation(_Room,0,2,_Z,vWest).
doorLocation(_Room,0,3,_Z,vWest).
doorLocation(_Room,0,4,_Z,vWest).
doorLocation(_Room,6,2,_Z,vEast).
doorLocation(_Room,6,3,_Z,vEast).
doorLocation(_Room,6,4,_Z,vEast).
doorLocation(_Room,6,0,_Z,vNE).
doorLocation(_Room,6,6,_Z,vSE).
doorLocation(_Room,0,0,_Z,vNW).
doorLocation(_Room,0,6,_Z,vSW).
doorLocation(_Room,_X,_Y,_Z,_Dir):-!,fail.

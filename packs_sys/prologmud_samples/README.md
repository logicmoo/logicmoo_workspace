# prologmud_samples
Online text adventure game - Samples

## Installation 
````
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.7.4-36-gc02793b-DIRTY-BIRDY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- pack_install(prologmud_samples).

% .....

````

### Try it
````

?- use_module(library(prologmud_samples)).

....
% found(mpred_rem_support2(mudHealth(iWesley716, 90),  (\+ ~mudHealth(iWesley716, 90), nt(~mudHealth(iWesley716, 90), call_u_no_bc(~mudHealth(iWesley716, 90)), rhs([mudHealth(iWesley716, 90)]))))).

Welcome to the MUD iWesley716!

The stream <stream>(0x7479e0)!

% substs=[vHere=iArea1016, isSelf=xyzFn(iArea1016, 2, 3, 1), isSelfAgent=iWesley716].
this is blinking red!
 [] []          [] []
 [] -- -- -- -- -- []
    -- -- -- -- --
    -- es -- -- --
    -- -- -- -- --
 [] -- -- -- -- -- []
 [] []          [] []
text= nameStringsList(iArea1016,[s("A","Corridor")]).
s(A,Corridor)
s(You,find,yourself,in,the,middle,of,a,well,lit,corridor,on,the,Enterprise)
s(It,isn,'t,very,wide,,,and,the,light,beige,walls,have,been,rounded,,,making,the,corridor,an,oval,shape)
s(You,see,the,holodeck,'s,control,panel,beside,the,holodeck,door,,,and,it,has,some,information,on,it)
path(vSouth)= iArea1013.
path(vWest)= iArea1015.
path(vNorth)= iArea1019.
path(vEast)= iArea1017.
pathName(vNorth)= s("The","corridor","continues","North").
pathName(vEast)= s("Holodeck","4","is","East").
pathName(vSouth)= s("The","corridor","continues","South").
pathName(vWest)= s("Sick","Bay","is","West").
text= localityOfObject(iWesley716,iArea1016).
selfAgent= iWesley716.
text= mudAtLoc(iWesley716,xyzFn(iArea1016,2,3,1)).
text= mudFacing(iWesley716,vNorth).
text= mudStance(iWesley716,vStand).
text= mudMoveDist(iWesley716,1).
mudLastCmdSuccess= vTrue.
% substs=[vHere=iArea1016, isSelf=xyzFn(iArea1016, 2, 3, 1), isSelfAgent=iWesley716].
ftText= mudContains(iWesley716,[]).
ftText= mudPossess(iWesley716,[iBoots717,iCommBadge718,iRedUniform719,iFood_rez1]).
ftText= mudStowing(iWesley716,[iFood_rez1]).
ftText= mudWielding(iWesley716,[]).
ftText= wearsClothing(iWesley716,[iBoots717,iCommBadge718,iRedUniform719]).
% found(mpred_rem_support2(mudNeedsLook(iWesley716, vTrue),  ((mudAtLoc(iWesley716, xyzFn(iArea1016, 1, 2, 1)), pt(mudAtLoc(iWesley716, xyzFn(iArea1016, 1, 2, 1)), rhs([{ain(mudNeedsLook(iWesley716, vTrue))}]))), (inRegion(iWesley716, iArea1016), pt(inRegion(iWesley716, _5576),  (\+tPathway(iWesley716), \+lookup_u(mudAtLoc(iWesley716, xyzFn(_5576, _5608, _5610, _5612)))*->in_grid_rnd(_5576, _5624)*->rhs([mudAtLoc(iWesley716, _5624)])))), (spatialInRegion(iWesley716), pt(spatialInRegion(iWesley716), pt(inRegion(iWesley716, iArea1016),  (\+tPathway(iWesley716), \+lookup_u(mudAtLoc(iWesley716, xyzFn(iArea1016, _5696, _5698, _5700)))*->in_grid_rnd(iArea1016, xyzFn(iArea1016, 1, 2, 1))*->rhs([mudAtLoc(iWesley716, xyzFn(iArea1016, 1, 2, 1))]))))), (inRegion(iWesley716, iArea1016), pt(inRegion(iWesley716, iArea1016), rhs([{ain((spatialInRegion(iWesley716), tRegion(iArea1016)))}]))), (localityOfObject(iWesley716, iArea1016), pt(localityOfObject(iWesley716, iArea1016),  (tRegion(iArea1016)*->rhs([inRegion(iWesley716, iArea1016)])))), mfl(baseKB, '/opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games/src_game_startrek/startrek.all.pfc.pl', 339)))).
iWesley716 [iWesley716,wants,to]> 
````
`e5` means east 5 cells
````
 [] [] [] [] [] [] []
 [] -- -- -- -- -- []
    -- -- -- -- -- []
    -- es -- -- -- []
    -- -- -- -- -- []
 [] -- -- -- -- -- []
 [] [] [] [] [] [] []
text= nameStringsList(iArea1017,[s("Holodeck","4","Entrance","A","Narrow","Alley")]).
s(Holodeck,4,Entrance,A,Narrow,Alley)
s(You,emerge,into,a,dark,narrow,alley)
s(Tall,dark,brick,buildings,block,your,way,north,and,south)
s(You,can,see,that,the,windows,on,the,buildings,are,fairly,high,,,and,some,have,been,boarded,up)
s(The,smell,from,the,rotting,tFood,and,garbage,mixing,with,the,foul,water,on,the,ground,is,unbearable)
s(You,can,hear,the,sounds,of,a,bustling,marketpace,to,the,east)
s(The,archway,leading,out,of,the,holodeck,is,west)
path(vWest)= iArea1016.
pathName(vWest)= s("A","corridor","is","West").
text= localityOfObject(iWesley716,iArea1017).
selfAgent= iWesley716.
text= mudAtLoc(iWesley716,xyzFn(iArea1017,2,3,1)).
text= mudFacing(iWesley716,vEast).
text= mudStance(iWesley716,vStand).
text= mudMoveDist(iWesley716,1).
mudLastCmdSuccess= vTrue.
% substs=[vHere=iArea1017, isSelf=xyzFn(iArea1017, 2, 3, 1), isSelfAgent=iWesley716].
ftText= mudContains(iWesley716,[]).
ftText= mudPossess(iWesley716,[iBoots717,iCommBadge718,iRedUniform719,iFood_rez1]).
ftText= mudStowing(iWesley716,[iFood_rez1]).
ftText= mudWielding(iWesley716,[]).
ftText= wearsClothing(iWesley716,[iBoots717,iCommBadge718,iRedUniform719]).
% found(mpred_rem_support2(mudNeedsLook(iWesley716, vTrue),  ((mudAtLoc(iWesley716, xyzFn(iArea1016, 3, 2, 1)), pt(mudAtLoc(iWesley716, xyzFn(iArea1016, 3, 2, 1)), rhs([{ain(mudNeedsLook(iWesley716, vTrue))}]))), (inRegion(iWesley716, iArea1016), pt(inRegion(iWesley716, _5594),  (\+tPathway(iWesley716), \+lookup_u(mudAtLoc(iWesley716, xyzFn(_5594, _5626, _5628, _5630)))*->in_grid_rnd(_5594, _5642)*->rhs([mudAtLoc(iWesley716, _5642)])))), (spatialInRegion(iWesley716), pt(spatialInRegion(iWesley716), pt(inRegion(iWesley716, iArea1016),  (\+tPathway(iWesley716), \+lookup_u(mudAtLoc(iWesley716, xyzFn(iArea1016, _5714, _5716, _5718)))*->in_grid_rnd(iArea1016, xyzFn(iArea1016, 3, 2, 1))*->rhs([mudAtLoc(iWesley716, xyzFn(iArea1016, 3, 2, 1))]))))), (inRegion(iWesley716, iArea1016), pt(inRegion(iWesley716, iArea1016), rhs([{ain((spatialInRegion(iWesley716), tRegion(iArea1016)))}]))), (localityOfObject(iWesley716, iArea1016), pt(localityOfObject(iWesley716, iArea1016),  (tRegion(iArea1016)*->rhs([inRegion(iWesley716, iArea1016)])))), mfl(baseKB, '/opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games/src_game_startrek/startrek.all.pfc.pl', 339)))).
% found(mpred_rem_support1(mudNeedsLook(iWesley716, vFalse),  (mfl(baseKB, user_input, 4845), ax))).
% found(mpred_rem_support2(mudNeedsLook(iWesley716, vFalse),  (\+ ~mudNeedsLook(iWesley716, vFalse), nt(~mudNeedsLook(iWesley716, vFalse), call_u_no_bc(~mudNeedsLook(iWesley716, vFalse)), rhs([mudNeedsLook(iWesley716, vFalse)]))))).
iWesley716 [iWesley716,wants,to]>
 

````

### Create a small world
````



````


# Some TODOs

Document this pack!
Write tests
Untangle the 'pack' install deps
Still in progress (Moving predicates over here from logicmoo_base)


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and TeamSPoon
All rights reserved.

# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to TeamSPoon and Contribute directly !
Still, we wont stop you from doing it the Fork+PullRequest method


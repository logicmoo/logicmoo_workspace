/** <module> a_nani_household
% This file contains the definitions for the room in a household
% To create a new world, simply change the room definitions as
% described below (or in manual)
%

use this file with...

:- ensure_loaded('a_nani_household.pfc').

*/

:- style_check(-singleton).
:- style_check(-discontiguous).

:- op(600,fx,onSpawn).

:- expects_dialect(pfc).

tRegion(iDMilesRoom).
onSpawn mudAreaConnected(iDMilesRoom,iHallWay7).

tSet(tOakDesk).
genls(tOakDesk,tFurniture).
genls(tOakDeskA,tOakDesk).
genls(tOakDeskB,tOakDesk).


onSpawn localityOfObject(tOakDeskA,iDMilesRoom).
onSpawn localityOfObject(tOakDeskB,iDMilesRoom).



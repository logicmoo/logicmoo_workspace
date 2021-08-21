/** <module> 
% Test worlds for vacuum world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, use the show_world/0 command. 
%
% The two letter codes used below are defined in map.objects.pl
%
% predator.map.pl
% July 10, 1996
% John Eikenberry
%
%
% Dec 13, 2035
% Douglas Miles
%
*/

:- expects_dialect(pfc).

grid_key( ed='tEdgeOfWorld').
grid_key( hw='tHighWall').
grid_key( lw='tLowWall').
grid_key( hb='tHighBox').
grid_key( lb='tLowBox').
grid_key( dt=tDirt).
grid_key( ot=tPowerOutlet).



typeGrid(tStoreRoom,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
typeGrid(tStoreRoom,2, [ed,dt,dt,--,--,--,--,--,--,hw,hw,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,3, [ed,dt,dt,--,--,--,--,--,--,hw,hw,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,4, [ed,dt,dt,dt,dt,--,--,--,--,hw,hw,--,--,--,--,dt,dt,--,--,ed]).
typeGrid(tStoreRoom,5, [ed,hw,hw,lw,lw,ot,--,--,--,hw,hw,--,--,dt,dt,dt,--,--,--,ed]).
typeGrid(tStoreRoom,6, [ed,--,--,--,lw,lw,lw,--,--,hw,hw,--,lb,lb,dt,dt,--,--,--,ed]).
typeGrid(tStoreRoom,7, [ed,--,--,--,--,--,--,--,--,hw,hw,--,--,lb,lb,dt,--,--,--,ed]).
typeGrid(tStoreRoom,8, [ed,--,--,--,--,--,--,--,--,lw,lw,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,9, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,10,[ed,--,--,--,dt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,11,[ed,--,--,--,dt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,12,[ed,--,--,--,--,dt,dt,--,--,--,--,dt,dt,dt,dt,dt,dt,--,--,ed]).
typeGrid(tStoreRoom,13,[ed,--,--,--,--,--,dt,--,--,--,--,--,dt,dt,dt,dt,dt,--,--,ed]).
typeGrid(tStoreRoom,14,[ed,--,--,--,--,lb,lb,--,--,--,--,--,--,--,dt,dt,--,--,--,ed]).
typeGrid(tStoreRoom,15,[ed,--,--,--,--,hb,hb,hb,hb,--,--,--,--,dt,dt,--,--,ot,--,ed]).
typeGrid(tStoreRoom,16,[ed,--,--,--,--,--,--,--,--,--,--,--,dt,dt,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,17,[ed,--,--,--,--,--,ot,--,--,--,--,dt,dt,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,18,[ed,--,--,--,--,--,--,--,--,--,dt,dt,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,19,[ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tStoreRoom,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).



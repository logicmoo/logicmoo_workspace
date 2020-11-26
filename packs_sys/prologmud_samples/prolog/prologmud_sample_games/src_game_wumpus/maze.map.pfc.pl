/** <module> 
% Test world for tWumpusMazeMap world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, once this file is consulted,  use the show_world/0 command. 
%
% The two letter codes used below are defined in tWumpusMazeMap.objects.pl
%
% tWumpusMazeMap.map.pl
% July 10, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/


grid_key(ed='tEdgeOfWorld').
grid_key(wl=wall).
grid_key(lg=ledge).
grid_key(dr=tDoor).
grid_key(gd=gold).
grid_key(fd=tFood).
grid_key(pt=pit).
grid_key(el='elixer of mudHealth').

typeGrid(tWumpusMazeMap,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
typeGrid(tWumpusMazeMap,2, [ed,--,--,--,wl,fd,--,fd,--,fd,--,fd,--,fd,--,--,fd,wl,gd,ed]).
typeGrid(tWumpusMazeMap,3, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,--,--,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,4, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,5, [ed,--,wl,--,wl,wl,wl,--,wl,wl,wl,wl,--,wl,--,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,6, [ed,el,wl,--,--,--,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,7, [ed,gd,wl,wl,wl,wl,wl,--,wl,wl,wl,--,wl,wl,--,--,--,--,--,ed]).
typeGrid(tWumpusMazeMap,8, [ed,wl,wl,--,--,--,--,--,--,--,--,--,--,wl,wl,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,9, [ed,--,--,--,wl,wl,wl,wl,wl,wl,wl,--,--,--,--,--,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,10,[ed,--,wl,--,wl,--,--,--,--,gd,wl,--,wl,--,wl,--,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,11,[ed,--,wl,--,wl,--,wl,--,wl,fd,wl,--,wl,fd,wl,--,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,12,[ed,--,--,--,--,--,wl,--,wl,fd,wl,--,wl,wl,wl,--,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,13,[ed,--,wl,--,wl,--,wl,--,wl,--,--,--,--,--,--,--,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,14,[ed,fd,wl,--,wl,wl,wl,--,wl,wl,wl,wl,wl,wl,wl,wl,--,wl,--,ed]).
typeGrid(tWumpusMazeMap,15,[ed,--,wl,--,--,wl,--,--,--,--,--,--,--,--,pt,wl,--,--,--,ed]).
typeGrid(tWumpusMazeMap,16,[ed,fd,wl,fd,--,wl,--,wl,wl,wl,wl,wl,wl,--,wl,wl,wl,--,fd,ed]).
typeGrid(tWumpusMazeMap,17,[ed,--,wl,fd,--,--,--,--,--,--,--,--,--,--,--,--,wl,--,fd,ed]).
typeGrid(tWumpusMazeMap,18,[ed,fd,wl,wl,wl,wl,wl,wl,wl,--,wl,--,wl,wl,wl,--,wl,--,fd,ed]).
typeGrid(tWumpusMazeMap,19,[ed,pt,fd,--,fd,--,fd,--,fd,--,wl,--,--,--,--,--,--,--,fd,ed]).
typeGrid(tWumpusMazeMap,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).

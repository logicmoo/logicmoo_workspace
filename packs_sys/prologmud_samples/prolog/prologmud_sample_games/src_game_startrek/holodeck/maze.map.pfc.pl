/** <module> 
% Test world for maze world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, once this file is consulted,  use the show_world/0 command. 
%
% The two letter codes used below are defined in maze.objects.pl
%
% maze.map.pl
% July 10, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/

:-file_begin(pfc).


grid_key(ed='tEdgeOfWorld').
grid_key(wl=tWall).
grid_key(lg=tLedge).
grid_key(dr=tDoor).
grid_key(gd=tGold).
grid_key(fd=tFood).
grid_key(pt=tPit).
grid_key(el='elixer of mudHealth').

typeGrid(maze,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
typeGrid(maze,2, [ed,--,--,--,wl,fd,--,fd,--,fd,--,fd,--,fd,--,--,fd,wl,gd,ed]).
typeGrid(maze,3, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,--,--,wl,--,wl,--,ed]).
typeGrid(maze,4, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
typeGrid(maze,5, [ed,--,wl,--,wl,wl,wl,--,wl,wl,wl,wl,--,wl,--,wl,--,wl,--,ed]).
typeGrid(maze,6, [ed,el,wl,--,--,--,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
typeGrid(maze,7, [ed,gd,wl,wl,wl,wl,wl,--,wl,wl,wl,--,wl,wl,--,--,--,--,--,ed]).
typeGrid(maze,8, [ed,wl,wl,--,--,--,--,--,--,--,--,--,--,wl,wl,wl,--,wl,--,ed]).
typeGrid(maze,9, [ed,--,--,--,wl,wl,wl,wl,wl,wl,wl,--,--,--,--,--,--,wl,--,ed]).
typeGrid(maze,10,[ed,--,wl,--,wl,--,--,--,--,gd,wl,--,wl,--,wl,--,--,wl,--,ed]).
typeGrid(maze,11,[ed,--,wl,--,wl,--,wl,--,wl,fd,wl,--,wl,fd,wl,--,--,wl,--,ed]).
typeGrid(maze,12,[ed,--,--,--,--,--,wl,--,wl,fd,wl,--,wl,wl,wl,--,--,wl,--,ed]).
typeGrid(maze,13,[ed,--,wl,--,wl,--,wl,--,wl,--,--,--,--,--,--,--,--,wl,--,ed]).
typeGrid(maze,14,[ed,fd,wl,--,wl,wl,wl,--,wl,wl,wl,wl,wl,wl,wl,wl,--,wl,--,ed]).
typeGrid(maze,15,[ed,--,wl,--,--,wl,--,--,--,--,--,--,--,--,pt,wl,--,--,--,ed]).
typeGrid(maze,16,[ed,fd,wl,fd,--,wl,--,wl,wl,wl,wl,wl,wl,--,wl,wl,wl,--,fd,ed]).
typeGrid(maze,17,[ed,--,wl,fd,--,--,--,--,--,--,--,--,--,--,--,--,wl,--,fd,ed]).
typeGrid(maze,18,[ed,fd,wl,wl,wl,wl,wl,wl,wl,--,wl,--,wl,wl,wl,--,wl,--,fd,ed]).
typeGrid(maze,19,[ed,pt,fd,--,fd,--,fd,--,fd,--,wl,--,--,--,--,--,--,--,fd,ed]).
typeGrid(maze,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).

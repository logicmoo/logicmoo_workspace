/** <module> 
% Test worlds for predator world can be defined by a grid like the
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


grid_key(ed='tEdgeOfWorld').
grid_key(tr=tTree).
grid_key( rk=tRock).
grid_key( nt=tNnut).
%


typeGrid(tBigForestMap,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
typeGrid(tBigForestMap,2, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tBigForestMap,3, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tBigForestMap,4, [ed,--,tr,--,nt,--,--,--,tr,tr,--,--,--,--,--,nt,nt,--,--,ed]).
typeGrid(tBigForestMap,5, [ed,--,--,tr,--,--,--,--,tr,--,--,--,--,nt,nt,nt,--,--,--,ed]).
typeGrid(tBigForestMap,6, [ed,--,--,--,--,--,tr,tr,tr,tr,tr,tr,--,--,nt,nt,--,--,--,ed]).
typeGrid(tBigForestMap,7, [ed,--,--,--,--,--,tr,--,--,--,--,--,--,--,--,nt,--,--,--,ed]).
typeGrid(tBigForestMap,8, [ed,--,--,--,--,--,--,--,--,--,--,--,tr,tr,tr,tr,tr,tr,--,ed]).
typeGrid(tBigForestMap,9, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tBigForestMap,10,[ed,--,--,--,nt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
typeGrid(tBigForestMap,11,[ed,--,--,--,nt,--,--,--,--,--,tr,tr,--,--,--,--,--,--,--,ed]).
typeGrid(tBigForestMap,12,[ed,--,--,tr,tr,nt,nt,--,--,tr,tr,nt,nt,nt,nt,nt,nt,--,--,ed]).
typeGrid(tBigForestMap,13,[ed,--,--,--,--,--,nt,--,--,tr,--,--,nt,nt,nt,nt,nt,--,--,ed]).
typeGrid(tBigForestMap,14,[ed,--,--,--,--,--,--,--,tr,tr,--,--,--,--,nt,nt,--,--,--,ed]).
typeGrid(tBigForestMap,15,[ed,--,--,--,--,tr,tr,tr,tr,tr,--,--,--,nt,nt,--,--,--,--,ed]).
typeGrid(tBigForestMap,16,[ed,--,--,--,--,--,--,--,--,--,--,--,nt,nt,--,--,tr,--,--,ed]).
typeGrid(tBigForestMap,17,[ed,--,--,--,--,--,--,--,--,tr,--,nt,nt,--,tr,--,tr,--,--,ed]).
typeGrid(tBigForestMap,18,[ed,--,--,--,--,--,--,--,--,--,nt,nt,--,--,tr,--,--,--,--,ed]).
typeGrid(tBigForestMap,19,[ed,--,--,--,--,--,--,--,--,--,--,--,--,--,tr,--,--,--,--,ed]).
typeGrid(tBigForestMap,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).


% :- include(prologmud(mud_footer)).



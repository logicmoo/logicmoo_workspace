:- module(substance, [substance/1]).
/* * module * Interface module tying display engines to the underlying model

*/

%%	substance(-Term:term) is nondet
%
%	Term is a partially bound term, used to interrogate state
%	of the MUD.
%
%	map_origin(X, Y) - absolute coordinates Y down location of UL
%			corner cell
%      
%       map_size(X, Y) - size to display, in cells across and down
%
%       cell(+Player, +X, +Y, -Semantics) -
%		       - called with Player, X, and Y bound, binds
%		       Semantics to a term understandable by
%		       mud_specific:style/1
%

substance(map_origin(0,0)).
substance(map_size(4,4)).  % small for debugging ez
substance(cell(_, 0, _, wall)).
substance(cell(_, _, 0, wall)).
substance(cell(_, 3, _, wall)).
substance(cell(_, _, 3, wall)).
substance(cell(_, X, Y, floor)) :-
	between(1, 2, X),
	between(1, 2, Y).

substance(need_new_player(_, G)) :- gensym(player, G).
substance(player_split(P)) :- debug(logicmoo, 'player ~w abandoned', [P]).


end_of_file.

substance(map_origin(0,0)).
substance(map_size(X,Y)):- once(get_map_size(X,Y)).  % small for debugging ez
substance(need_new_player(Options, P)) :- create_agent(P,Options),asserta(at_web(P)),debug(logicmoo, 'player ~w create', [P]).
substance(player_split(P)) :- debug(logicmoo, 'player ~w abandoned', [P]), retractall(at_web(P)).
substance(cell(P,X,Y,What)):- substance_cell(P,X,Y,What).

:-dynamic(at_web/1).


substance_cell(Agent,X,Y,WhatGlyph):- not(at_web(_)),!,substance_cell_test(Agent,X,Y,WhatGlyph).
substance_cell(Agent,X,Y,WhatGlyph):- 
   atloc(Agent,ALOC),
   locationToRegion(ALOC,Region),
   loc_to_xy(Region,X,Y,XYLOC),
   findall(O,atloc(O,XYLOC),InList),
   can_sense(Agent,_Sense,InList,Detected,Undetected),
   create_webglyph(Detected,Undetected,WhatGlyph).

substance_test:-true.

substance_cell_test(_, 0, _, wall).
substance_cell_test(_, _, 0, wall).
substance_cell_test(_, 3, _, wall).
substance_cell_test(_, _, 3, wall).
substance_cell_test(_, X, Y, floor) :-
	between(1, 2, X),
	between(1, 2, Y),!.

get_map_size(X,Y):-
   at_web(Agent),!,
   atloc(Agent,LOC),
   locationToRegion(LOC,Region),
   grid_size(Region,X,Y,_Z),!.

get_map_size(4,4).


create_webglyph(Detected,_,Label):- member(O,Detected),mud_isa(O,'agent'),inst_label(O,Label).
create_webglyph(Detected,_,wall):- member(O,Detected),mud_isa(O,'wall').
create_webglyph(Detected,_,floor):- member(O,Detected),mud_isa(O,'floor').
create_webglyph([],[],floor).
create_webglyph(_,_,darkness).
   



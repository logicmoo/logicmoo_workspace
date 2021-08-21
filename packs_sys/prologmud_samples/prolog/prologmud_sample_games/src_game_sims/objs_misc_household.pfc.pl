/** <module> 
% This file contains the definitions for the objects in a household
% To create a new world, simply change the tObj definitions as
% described below (or in manual)
%
% *******Object definitions*******
%.
% Use the label_type_props(label,typecode,[property1,property2,etc.]]).
% label is whats used to refer to the tObj in the running world
% typecode is whats used to refer to the tObj for initialization (see world.pl)
*/




:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

:- expects_dialect(pfc).

:- declare_load_dbase('../src_game_nani/**/*.pfc.pl').


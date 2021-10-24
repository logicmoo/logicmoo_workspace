/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% Data for the World Database.
% ---------------------------


%ti(Sea,X) :- Sea\==seamass,Sea\==ocean,Sea\==sea, agentitive_symmetric_type(Borders,Sea), (symmetric_pred(spatial,Borders,Sea,X)).
%agentitive_symmetric_type(border,Baltic):- ti(seamass,Baltic).
% allows "baltic country" "pacific countries"   
agentitive_symmetric_type(border,seamass).


place_lex(place).

ti_subclass(continent,place).
ti_subclass(region,place).
ti_subclass(seamass,place).
ti_subclass(country,place).


agentitive_trans_80(contain,africa,african).
agentitive_trans_80(contain,america,american).
agentitive_trans_80(contain,asia,asian).
agentitive_trans_80(contain,europe,european).

ti(circle_of_latitude,C):- circle_latitude(C,_).
position_pred(spatial,latitude,C,L):- circle_latitude(C,L).
circle_latitude(equator,0--degrees).
circle_latitude(tropic_of_cancer,23--degrees).
circle_latitude(tropic_of_capricorn,(-23)--degrees).
circle_latitude(arctic_circle,67--degrees).
circle_latitude(antarctic_circle,(-67)--degrees).

ti_subclass(ocean,seamass).
ti_subclass(sea,seamass).
ti(ocean,arctic_ocean).
ti(ocean,atlantic).
ti(ocean,indian_ocean).
ti(ocean,pacific).
ti(ocean,southern_ocean).
ti(sea,baltic).
ti(sea,black_sea).
ti(sea,caspian_sea).
ti(sea,mediterranean).
ti(sea,persian_gulf).
ti(sea,red_sea).
% @TODO ti(sea,caribian).





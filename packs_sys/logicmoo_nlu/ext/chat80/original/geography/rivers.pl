
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


% superceeded verb_root_db(chat80,rise).
% superceeded verb_root_db(chat80,flow).
% superceeded verb_root_db(chat80,drain).

% superceeded regular_pres_db(chat80,rise).
% superceeded regular_pres_db(chat80,flow).
% superceeded regular_pres_db(chat80,drain).

% superceeded regular_past_db(chat80,flowed,flow).
% superceeded regular_past_db(chat80,drained,drain).

/*
% superceeded 
verb_form_db(chat80,rose,rise,past+fin,_).
verb_form_db(chat80,rises,rise,pres+fin,3+sg).
verb_form_db(chat80,risen,rise,past+part,_).
verb_form_db(chat80,flows,flow,pres+fin,3+sg).
verb_form_db(chat80,flowing,flow,pres+part,_).
verb_form_db(chat80,drains,drain,pres+fin,3+sg).
verb_form_db(chat80,draining,drain,pres+part,_).
*/

% Logical Interface to facts about rivers.
% ------------------

type_specific_bte(river,river_flows,rise,flow,drain).
path_nodes(river_flows,river,River,NodeList):- river_flows(River,NodeList).

% Facts about rivers.
% ------------------
river_flows(amazon,[atlantic,brazil,peru]).
river_flows(amu_darya,[aral_sea,soviet_union,afghanistan]).
river_flows(amur,[pacific,soviet_union,china,mongolia]).
river_flows(brahmaputra,[indian_ocean,bangladesh,china]).
river_flows(colorado,[pacific,mexico,united_states]).
river_flows(congo_river,[atlantic,zaire,zambia]).
river_flows(cubango,[botswana,south_africa,angola]).
river_flows(danube,[black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
river_flows(don,[black_sea,soviet_union]).
river_flows(elbe,[atlantic,west_germany,east_germany,czechoslovakia]).
river_flows(euphrates,[persian_gulf,iraq,syria,turkey]).
river_flows(ganges,[indian_ocean,india,china]).
river_flows(hwang_ho,[pacific,china]).
river_flows(indus,[indian_ocean,pakistan,india,china]).
river_flows(irrawaddy,[indian_ocean,burma]).
river_flows(lena,[arctic_ocean,soviet_union]).
river_flows(limpopo,[indian_ocean,mozambique,south_africa]).
river_flows(mackenzie,[arctic_ocean,canada]).
river_flows(mekong,[pacific,vietnam,cambodia,laos,china]).
river_flows(mississippi,[atlantic,united_states]).
river_flows(murray,[indian_ocean,australia]).
river_flows(niger_river,[atlantic,nigeria,niger,mali,guinea]).
river_flows(nile,[mediterranean,egypt,sudan,uganda]).
river_flows(ob,[arctic_ocean,soviet_union]).
river_flows(oder,[baltic,poland,czechoslovakia]).
river_flows(orange,[atlantic,south_africa,lesotho]).
river_flows(orinoco,[atlantic,venezuela,colombia]).
river_flows(parana,[atlantic,argentina,paraguay,brazil]).
river_flows(rhine,[atlantic,netherlands,west_germany,switzerland]).
river_flows(rhone,[mediterranean,france,switzerland]).
river_flows(rio_grande,[atlantic,mexico,united_states]).
river_flows(salween,[indian_ocean,burma,china]).
river_flows(senegal_river,[atlantic,senegal,mali,guinea]).
river_flows(seine,[atlantic,france]).
river_flows(tagus,[atlantic,portugal,spain]).
river_flows(vistula,[baltic,poland]).
river_flows(volga,[black_sea,soviet_union]).
river_flows(volta,[atlantic,ghana,upper_volta]).
river_flows(yangtze,[pacific,china]).
river_flows(yenisei,[arctic_ocean,soviet_union,mongolia]).
river_flows(yukon,[pacific,united_states,canada]).
river_flows(zambesi,[indian_ocean,mozambique,zambia,angola]).


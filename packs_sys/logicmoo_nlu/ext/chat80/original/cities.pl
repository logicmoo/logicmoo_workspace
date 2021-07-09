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
:-op(500,xfy,--).

ti(capital_city,Cap) :- c_r_l_l_s_cap_m(_,_,_,_,_,_,Cap,_). % specific_pred(spatial,nation_capital,_X,C).
%ti(city,C) :- ti(capital_city,C).
%ti(city,C) :- clause(city_country_popu(C,_,_), true).
ti(city,C) :- country_contains_thing(_,C), \+ ti(river,C).
ti(country,C) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,_,_).

count_pred(Spatial,Heads,C,Total):- is_list(C),maplist(count_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).
count_pred(spatial,population,C,P--thousand) :- city_country_popu(C,_,P).

% Facts about cities.
% ------------------
madeup_city_country_popu(C,Nat,PopOut):- fail,
  ti(city,C), \+ clause(city_country_popu(C,_,_), true),
  once((trans_direct(spatial,contain,Nat,C), 
  c_r_l_l_s_cap_m(Nat,_,_,_,_,Pop,_,_))),  
  % estimate at least a quarter of country population
  A is integer(Pop/4000000), 
  % add a magic number
  PopOut is (A*1000) + 666.

city_country_popu(C,Nat,Pop):- madeup_city_country_popu(C,Nat,Pop).

city_country_popu(belle_mead,united_states,8).
city_country_popu(amsterdam,netherlands,800).
city_country_popu(amersfoort,netherlands,120).
city_country_popu(athens,greece,1368).
city_country_popu(bangkok,thailand,1178).
city_country_popu(barcelona,spain,1280).
city_country_popu(berlin,east_germany,3481).
city_country_popu(birmingham,united_kingdom,1112).
city_country_popu(bombay,india,2839).
city_country_popu(brussels,belgium,986).
city_country_popu(bucharest,romania,1237).
city_country_popu(budapest,hungary,1757).
city_country_popu(buenos_aires,argentina,3404).
city_country_popu(cairo,egypt,2373).
city_country_popu(calcutta,india,2549).
city_country_popu(canton,china,1496).
city_country_popu(caracas,venezuela,488).
city_country_popu(chicago,united_states,3621).
city_country_popu(chungking,china,1100).
city_country_popu(dairen,china,544).
city_country_popu(delhi,india,1744).
city_country_popu(detroit,united_states,1850).
city_country_popu(glasgow,united_kingdom,1090).
city_country_popu(hamburg,west_germany,1700).
city_country_popu(harbin,china,760).
city_country_popu(hongkong_city,hongkong,2440).
city_country_popu(hyderabad,india,1086).
city_country_popu(istanbul,turkey,1215).
city_country_popu(jakarta,indonesia,533).
city_country_popu(johannesburg,south_africa,880).
city_country_popu(karachi,pakistan,1126).
city_country_popu(kiev,soviet_union,991).
city_country_popu(kobe,japan,765).
city_country_popu(kowloon,china,547).
city_country_popu(kyoto,japan,1204).
city_country_popu(leningrad,soviet_union,2800).
city_country_popu(lima,peru,835).
city_country_popu(london,united_kingdom,8346).
city_country_popu(los_angeles,united_states,1970).
city_country_popu(madras,india,1416).
city_country_popu(madrid,spain,1700).
city_country_popu(manila,philippines,1025).
city_country_popu(melbourne,australia,1595).
city_country_popu(mexico_city,mexico,3796).
city_country_popu(milan,italy,1269).
city_country_popu(montreal,canada,1109).
city_country_popu(moscow,soviet_union,4800).
city_country_popu(mukden,china,1551).
city_country_popu(nagoya,japan,1337).
city_country_popu(nanking,japan,1020).
city_country_popu(naples,italy,1012).
city_country_popu(new_york,united_states,7795).
city_country_popu(osaka,japan,2547).
city_country_popu(paris,france,2850).
city_country_popu(peking,china,2031).
city_country_popu(philadelphia,united_states,2072).
city_country_popu(pusan,south_korea,474).
city_country_popu(rio_de_janeiro,brazil,2413).
city_country_popu(rome,italy,1760).
city_country_popu(saigon,vietnam,695).
city_country_popu(santiago,chile,1350).
city_country_popu(sao_paulo,brazil,2228).
city_country_popu(seoul,south_korea,1446).
city_country_popu(shanghai,china,5407).
city_country_popu(sian,china,629).
city_country_popu(singapore_city,singapore,1264).
city_country_popu(sydney,australia,1898).
city_country_popu(tehran,iran,1010).
city_country_popu(tientsin,china,1795).
city_country_popu(tokyo,japan,8535).
city_country_popu(toronto,canada,668).
city_country_popu(vienna,austria,1766).
city_country_popu(warsaw,poland,965).
city_country_popu(yokohama,japan,1143).
:- fixup_exports.


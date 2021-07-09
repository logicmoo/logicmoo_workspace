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

% NDTABL - Meta-information about database relations.

:- discontiguous nd_costs/3.
:- discontiguous nd_costs/4.
:- discontiguous nd_costs/5.
:- discontiguous nd_costs/6.
:- discontiguous nd_costs/7.

nd_costs(<,99,51,51).
nd_costs(=<,99,51,51).
nd_costs(>,99,51,51).
nd_costs(>=,99,51,51).
nd_costs(african,19,26).
nd_costs(aggregate80,103,3,100,51).
nd_costs(american,19,26).
nd_costs(measure_value,7,7,22,22,51).
nd_costs(measure_unit,8,51,51).
nd_costs(asian,21,26).
nd_costs(border,29,2,2).
nd_costs(capital,22,22).
nd_costs(capital,22,22,23).
nd_costs(card,99,100,3).
nd_costs(city,18,18).
nd_costs(continent,8,8).
nd_costs(country,22,22).
nd_costs(drains,16,16,10).
nd_costs(european,19,26).
nd_costs(exceeds,99,61,61).
nd_costs(flows,19,16,22).
nd_costs(flows,19,16,22,22).
nd_costs(in,29,26,15).
nd_costs(latitude,22,22,23).
nd_costs(latitude,23,23).
nd_costs(position_value,7,22,22,26).
nd_costs(ocean,7,7).
nd_costs(one_of,99,200,-99).
nd_costs(place,23,23).
nd_costs(count_value,7,23,23,51).
nd_costs(unit_format,7,51,51).
nd_costs(ratio,99,51,51,3).
nd_costs(region,12,12).
nd_costs(rises,16,16,22).
nd_costs(river,16,16).
nd_costs(sea,8,8).
nd_costs(seamass,10,10).
nd_costs(rel_spatial,7,40,22,22).

/*
nd_costs(area,51,51).
nd_costs(asian,21,26).
nd_costs(african,19,26).
nd_costs(american,19,26).
nd_costs(capital,22,22).
nd_costs(city,18,18).
nd_costs(continent,8,8).
nd_costs(country,22,22).
nd_costs(european,19,26).
nd_costs(latitude,23,23).
nd_costs(longitude,26,26).
nd_costs(ocean,7,7).
nd_costs(place,23,23).
nd_costs(population,51,51).
nd_costs(region,12,12).
nd_costs(river,16,16).
nd_costs(sea,8,8).
nd_costs(seamass,10,10).

nd_costs(<,99,51,51).
nd_costs(=<,99,51,51).
nd_costs(>,99,51,51).
nd_costs(>=,99,51,51).
nd_costs(area,22,22,51).
nd_costs(border,29,22,22).
nd_costs(capital,22,22,23).
nd_costs(card,99,100,3).
nd_costs(drains,16,16,10).
nd_costs(cp(east,of),40,22,22).
nd_costs(exceeds,99,51,51).
nd_costs(flows,19,16,22).
nd_costs(in,29,26,15).
nd_costs(latitude,22,22,23).
nd_costs(longitude,22,22,26).
nd_costs(cp(north,of),40,22,22).
nd_costs(one_of,99,200,-99).
nd_costs(population,23,23,51).
nd_costs(rises,16,16,22).
nd_costs(cp(south,of),40,22,22).
nd_costs(cp(west,of),40,22,22).

nd_costs(aggregate80,103,3,100,51).
nd_costs(flows,19,16,22,22).
nd_costs(ratio,99,51,51,3).
*/


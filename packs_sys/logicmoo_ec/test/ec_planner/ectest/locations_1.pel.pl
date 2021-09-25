:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Sat, 25 Sep 2021 11:51:48 GMT
 %  loading(load_e_pl,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e').
%; location1 is adjacent to location2.

% predicate Adjacent(location,location)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',3).
blue = predicate( adjacent(_21434,Adjacent)).
blue = predicate( adjacent(location,location)).
==>( mpred_prop(adjacent(location,location),predicate)).
==>( meta_argtypes( adjacent(location,location))).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',5).
% [location1,location2]
% Adjacent(location1,location2) <-> 
%  (location1=L1 & location2=L2) |
%  (location1=L2 & location2=L1).
 %  adjacent(Location1,Location2) <->         (     equals(l1,Location1),equals(l2,Location2) ;     equals(l2,Location1),equals(l1,Location2)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',5).

 /*  atoms_of -> (     [adjacent,'Location1','Location2'] <->     [     ;, equals,l1,'Location1',equals,l2, 
         'Location2',equals,l2,'Location1', 
         equals,l1,'Location2']).
 */
 %  [   adjacent(Location1,Location2)] ->         ta( _15878,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _41282,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _4782,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _30182,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _55586,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _20462,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _45866,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _9208,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _34608,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _60012,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _25328,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _50732,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _16556,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _41960,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _7204,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _32604,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _58008,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _24304,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _49708,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _16388,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _41792,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _8182,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _33582,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _58986,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _26978,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _52382,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _20368,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _45772,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _11966,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _37370,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _7208,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _30574,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _55978,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _24780,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _50184,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _19766,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _45170,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _13142,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _38546,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _8560,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _32664,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _58068,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _28084,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _53488,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _24402,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _49806,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _20480,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _45884,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _15392,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _40796,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _12014,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _37418,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _10520,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _33890,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _59294,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _31756,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _57160,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _82564,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _107968,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _14424,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _39828,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _65232,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _90636,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _116040,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _24340,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _49744,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _75148,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _100552,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _125958,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _34484,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _59888,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _85292,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _110696,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _18582,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _43986,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _69390,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _94794,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _120198,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _31274,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _56678,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _82082,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _107486,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _17476,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _42880,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _68284,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _93688,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _119092,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _30952,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _56356,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _81760,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _107164,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _18598,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _44002,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _69406,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _94810,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _120214,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _34010,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _59414,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _84818,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _110222,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _22376,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _47780,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _73184,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _98588,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _123992,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _39174,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _64578,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _89982,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _115386,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _30768,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _56172,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _81576,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _106980,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _22110,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _47514,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _72918,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _98322,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _123726,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _39648,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _65052,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _90456,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _115860,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _32964,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _58368,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _83772,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _109176,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _25852,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _51256,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _76660,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _102064,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _22344,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _45714,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _71118,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _96522,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _121926,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _41554,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _66958,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _92362,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _117766,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _37400,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _62804,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _88208,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _113612,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _32726,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _58130,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _83534,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _108938,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _29044,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _54448,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _79852,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _105256,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _25830,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _51234,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _76638,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _102042,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _127448,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _152854,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _178260,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _203666,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _229072,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _254478,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _46018,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _71422,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _96826,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _122230,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _147636,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _173042,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _198448,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _223854,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _249260,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _42236,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _67640,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _93044,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _118448,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _143854,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _169260,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _194666,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _220072,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _245478,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _40174,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _65578,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _90982,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _116386,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _141792,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _167198,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _192604,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _218010,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _243416,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _37830,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _63234,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _88638,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _114042,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _139448,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _164854,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _190260,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _215666,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _241072,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _36614,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _62018,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _87422,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _112826,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _138232,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _163638,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _189044,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _214450,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _239856,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _37996,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _63400,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _88804,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _114208,   tvs1=[],     tvs2=[],     (         equals(l1,Location1),equals(l2,Location2) ;         equals(l2,Location1),equals(l1,Location2))).

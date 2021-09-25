:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Sat, 25 Sep 2021 11:58:12 GMT
 %  loading(load_e_pl,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_2.e').
%; location1 is adjacent to location2.

% predicate Adjacent(location,location)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_2.e',3).
blue = predicate( adjacent(_21922,Adjacent)).
blue = predicate( adjacent(location,location)).
==>( mpred_prop(adjacent(location,location),predicate)).
==>( meta_argtypes( adjacent(location,location))).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_2.e',5).
% [location1,location2]
% Adjacent(location1,location2) <->
% (location1=L1 & location2=L2) |
% (location1=L2 & location2=L1) |
% (location1=L2 & location2=L3) |
% (location1=L3 & location2=L2).
 %  adjacent(Location1,Location2) <->         ( equals(l1,Location1),equals(l2,Location2)  ;    equals(l2,Location1),equals(l1,Location2) ;     equals(l2,Location1),equals(l3,Location2) ;     equals(l3,Location1),equals(l2,Location2)).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_2.e',5).

 /*  atoms_of -> (     [adjacent,'Location1','Location2'] <->     [     ;, equals,l1,'Location1',equals,l2, 
         'Location2',;,equals,l2,'Location1', 
         equals,l1,'Location2',;,equals, 
         l2,'Location1',equals,l3,'Location2', 
         equals,l3,'Location1',equals,l2, 
                 'Location2']).
 */
 %  [   adjacent(Location1,Location2)] ->         ta( _58048,   tvs1=[],     tvs2=[],     ( equals(l1,Location1),equals(l2,Location2)  ;        equals(l2,Location1),equals(l1,Location2) ;         equals(l2,Location1),equals(l3,Location2) ;         equals(l3,Location1),equals(l2,Location2))).
 %  [   adjacent(Location1,Location2)] ->         ta( _29230,   tvs1=[],     tvs2=[],     ( equals(l1,Location1),equals(l2,Location2)  ;        equals(l2,Location1),equals(l1,Location2) ;         equals(l2,Location1),equals(l3,Location2) ;         equals(l3,Location1),equals(l2,Location2))).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_2.e',5).
pl = axiom(    ( equals(l1,Location1),equals(l2,Location2)  ;    equals(l2,Location1),equals(l1,Location2) ;     equals(l2,Location1),equals(l3,Location2) ;     equals(l3,Location1),equals(l2,Location2)),       [      adjacent(Location1,Location2)]).
 %  [   ( equals(l1,Location1),equals(l2,Location2)  ;    equals(l2,Location1),equals(l1,Location2) ;     equals(l2,Location1),equals(l3,Location2) ;     equals(l3,Location1),equals(l2,Location2))] ->         ta(_23084,tvs1=[],tvs2=[],adjacent(Location1,Location2)).
pl = axiom( adjacent(Location1,Location2), [  [   ( equals(Location1,l1),equals(Location2,l2)  ;    equals(Location1,l2),equals(Location2,l1) ;     equals(Location1,l2),equals(Location2,l3) ;     equals(Location1,l3),equals(Location2,l2))])].

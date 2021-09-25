:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Sat, 25 Sep 2021 13:23:13 GMT
 %  loading(load_e_pl,'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e').
%; location1 is adjacent to location2.

% predicate Adjacent(location,location)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',3).
%~ From E:
%~ 
%~ predicate( adjacent(location,location))
%~ cpc :- predicates([adjacent/2])
%~ ooo :- [   cl([predicates([adjacent/2])],[])]
%~ cpc :- mpred_prop(adjacent(location,location),predicate)
%~ ooo :- [   cl([mpred_prop(adjacent(location,location),predicate)],[])]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',3).
cl(mpred_prop(adjacent(location,location),predicate),[]),cl(predicates(adjacent/2),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.e',5).
% [location1,location2]
% Adjacent(location1,location2) <-> 
%  (location1=L1 & location2=L2) |
%  (location1=L2 & location2=L1).
%~ From E:
%~ 
%~ (     adjacent(Location1,Location2) <->     Location1=l1,Location2=l2;Location1=l2,Location2=l1)
%~ cpc :- (     adjacent(Location1,Location2) <->     Location1=l1,Location2=l2;Location1=l2,Location2=l1)
%~ ooo :- [   cl(    [   equals(Location1,l1),     equals(Location1,l2)],       [      adjacent(Location1,Location2)]),     cl(      [   equals(Location1,l1),     equals(Location2,l1)],           [        adjacent(Location1,Location2)]),     cl(      [   equals(Location2,l2),     equals(Location1,l2)],           [        adjacent(Location1,Location2)]),     cl(      [   equals(Location2,l2),     equals(Location2,l1)],           [        adjacent(Location1,Location2)]),     cl(      [   adjacent(Location1,Location2)],           [        equals(Location1,l1),               equals(Location2,l2)]),     cl(      [   adjacent(Location1,Location2)],           [        equals(Location1,l2),               equals(Location2,l1)])]
( cl(    equals(Location1,l1),equals(Location1,l2),       adjacent(Location1,Location2))  ,    cl(      equals(Location1,l1),equals(Location2,l1),           adjacent(Location1,Location2)) ,     cl(      equals(Location2,l2),equals(Location1,l2),           adjacent(Location1,Location2)) ,     cl(      equals(Location2,l2),equals(Location2,l1),           adjacent(Location1,Location2)) ,     cl( adjacent(Location1,Location2),     equals(Location1,l1),equals(Location2,l2)) ,     cl( adjacent(Location1,Location2),     equals(Location1,l2),equals(Location2,l1))).
 %  ( cl(    equals(Location1,l1),equals(Location1,l2),       adjacent(Location1,Location2))  ,    cl(      equals(Location1,l1),equals(Location2,l1),           adjacent(Location1,Location2)) ,     cl(      equals(Location2,l2),equals(Location1,l2),           adjacent(Location1,Location2)) ,     cl(      equals(Location2,l2),equals(Location2,l1),           adjacent(Location1,Location2)) ,     cl( adjacent(Location1,Location2),     equals(Location1,l1),equals(Location2,l2)) ,     cl( adjacent(Location1,Location2),     equals(Location1,l2),equals(Location2,l1))).
 %  % =================================.

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_1.pel.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/LOCATIONS_1/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ALOCATIONS_1 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/657 


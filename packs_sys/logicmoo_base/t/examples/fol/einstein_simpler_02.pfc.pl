#!/usr/bin/env lmoo-junit
:- include(test_header).

lives(englishman, red_house)
keep(swede, dogs)
drinks(dane, tea)
leftof(green_house, white_house)
?X lives(X, green_house) ? drinks(X, coffee)
?X smokes(X, pallmalls) ? keep(X, birds)
?X lives(X, yellow_house) ? smokes(X, dunhills)
?X position(X, 3) ? drinks(X, milk)
position(norwegian, 1)
?X,Y smokes(X, blend) ? neighbor(X, Y) ? smokes(Y, dunhill)
?X smokes(X, bluemasters) ? drinks(X, bier)
?X,Y keep(X, horses) ? neighbor(X, Y) ? smoke(Y, dunhill)
smokes(german, prince)
?X neighbor(norwegian, X) ? lives(X, blue_house)
?X,Y smokes(X, blends) ? neighbor(X,Y) ? drinks(Y, water)


% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/455 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/einstein_simpler_02.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/EINSTEIN_SIMPLER_02/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AEINSTEIN_SIMPLER_02 


:- include(test_header).



:- include('sanity_fi_sk.pfc').
:- rtrace(kb_shared((<-)/2)).

:- expects_dialect(pfc).

%= simply retract (so we can re-deduce)
==> \+ human(douglas).

human(hum1).

%= Was to catch a regression bug that may couse trudy to lose human assertion
:- mpred_remove(baseKB:never_retract_u(human(trudy), sanity_test)).

%= confirm no inheritance twoards father
% :- show_call(\+ father(douglas,_)).


related_to(P1,P2)=>related_to(P2,P1).

father(P1,P2)=>related_to(P1,P2).
mother(P1,P2)=>related_to(P1,P2).

human(P2)<-human(P1),related_to(P1,P2).

:- printAll(must(father(_,_))).
:- printAll(must(mother(_,_))).

:- rtrace(ain_expanded(~human(hum1))).




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/72 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_fi_human_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_FI_HUMAN_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_FI_HUMAN_01 


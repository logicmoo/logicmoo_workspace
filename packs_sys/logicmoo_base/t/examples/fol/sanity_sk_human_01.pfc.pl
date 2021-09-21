:- include(test_header).



:- include('sanity_fi_human.pfc').

:- expects_dialect(pfc).

/*


% Basicly is this.. forall(C, forall(G, grandparent(C,G) => exists(P, (parent(P,G) & parent(C,P))))).
:- call(trace).
:- rtrace(ain(clif(forall(C, forall(G, exists(P,  grandparent(C,G) => (parent(C,P) & (parent(P,G))))))))).

grandparent(douglas,trudy).

:- must(parent(douglas,_X)).

*/

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/432 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_sk_human_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_SK_HUMAN_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_SK_HUMAN_01 


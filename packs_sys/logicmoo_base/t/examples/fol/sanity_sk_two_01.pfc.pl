:- include(test_header).





 

:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- expects_dialect(pfc).

:- dynamic(parent/2).

:- kif_compile.

% Basicly is this..  forAll(C, forAll(G, grandparent(C,G) => exists(P, (parent(P,G) & parent(C,P))))).
forAll(C, 
 forAll(G, 
   exists(P,  
    implies(grandparent(C,G),
        (parent(C,P) & parent(P,G)))))).

%             =boxlog=
% ~grandparent(_27066, _27068) :-
%       ~parent(_27066, _27078).
%             =pfclog=
% ~grandparent(_51432, _51434)<- ~parent(_51432, _51450), {is_unit(_51432)}.
%             =boxlog=
% ~grandparent(_35634, _35636) :-
%       ~parent(_35644, _35636).
%             =pfclog=
% ~grandparent(_6060, _6062)<- ~parent(_6076, _6062), {is_unit(_6062)}.
%             =boxlog=
% parent(_53446, skArg2ofGrandparent_1Fn(_53452, _53446)) :-
%       grandparent(_53446, _53452).
%             =pfclog=
% grandparent(_20680, _20682), {is_unit(_20680, _20682)}==>if_missing(parent(_20680, _20704), parent(_20680, skArg2ofGrandparent_1Fn(_20682, _20680))).
%             =boxlog=
% parent(skArg2ofGrandparent_1Fn(_29062, _29064), _29062) :-
%       grandparent(_29064, _29062).
%             =pfclog=
% grandparent(_5560, _5562), {is_unit(_5562, _5560)}==>if_missing(parent(_5582, _5562), parent(skArg2ofGrandparent_1Fn(_5562, _5560), _5562)).
%


grandparent(douglas,trudy).


:- test_boxlog(forAll(C, 
 forAll(G, 
   exists(P,  
    implies(grandparent(C,G),
        (parent(C,P) & parent(P,G))))))).

:- mpred_test(parent(douglas,_X)).
:- mpred_test(parent(_X,trudy)).
:- listing(parent).

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/416 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_sk_two_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_SK_TWO_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_SK_TWO_01 


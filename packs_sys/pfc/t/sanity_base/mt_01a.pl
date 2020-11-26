/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/

:- include(test_header).

:- pfc_test_feature(mt,must_not_be_pfc_file).
:- pfc_test_feature(mt,\+ mtHybrid(header_sane)).
:- pfc_test_feature(mt,header_sane:listing(mtHybrid/1)).

:- wdmsg(feature_test_may_fail).

%:- set_defaultAssertMt(header_sane).

baseKB:mtHybrid(socialMt).

:- must(baseKB:mtHybrid(socialMt)).

:- header_sane:listing(mtHybrid/1).


:- set_defaultAssertMt(myMt).

:- on_f_rtrace((on_x_rtrace(begin_pfc),is_pfc_file)).

baseKB:arity(loves,2).

:- ((ain(baseKB:predicateConventionMt(loves,socialMt)))).

% :- socialMt:listing(loves/2).

% :- header_sane:listing(predicateConventionMt/2).

:- must((fix_mp(clause(_,_),loves(x,y),M,P),
   M:P==socialMt:loves(x,y))).

:- must((fix_mp(clause(_,_),foo:loves(x,y),M,P),
   M:P==socialMt:loves(x,y))).

:- must((fix_mp(clause(_,_),header_sane:loves(x,y),M,P),
   M:P==socialMt:loves(x,y))).


loves(sally,joe).

% baseKB:genlMt(myMt,socialMt).

:- mpred_test(clause_u(socialMt:loves(_,_))).



:- pfc_test_feature(mt,\+clause_u(myMt:loves(_,_))).

:- pfc_test_feature(mt,\+ myMt:loves(_,_)).




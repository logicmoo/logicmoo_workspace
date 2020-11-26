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

baseKB:mtHybrid(socialMt).

:- pfc_test_feature(mt,must_not_be_pfc_file).

:- pfc_test_feature(mt,\+ is_pfc_file).

:- pfc_test_feature(mt,\+ mtHybrid(header_sane)).

:- listing(mtHybrid/1).

:- wdmsg(feature_test_may_fail).

%:- set_defaultAssertMt(header_sane).

:- debug(mpred).
:- debug(mpred(_)).

:- call_u(mpred_prop(M,mtHybrid,1,pfcLHS)),writeln(mpred_prop(M,mtHybrid,1,pfcLHS)).
:- xlisting(mtHybrid).

baseKB:mtHybrid(socialMt).

:- must(baseKB:mtHybrid(socialMt)).

:- header_sane:listing(mtHybrid/1).

:- break.

:- rtrace.
:- set_defaultAssertMt(myMt).
:- set_fileAssertMt(myMt).

:- on_f_rtrace((on_x_rtrace(begin_pfc),is_pfc_file)).

baseKB:arity(loves,2).

:- ain(baseKB:predicateConventionMt(loves,socialMt)).

:- header_sane:listing(mtHybrid/1).

:- user:listing(socialMt:loves/2).

:- listing(predicateConventionMt/2).

:- must((fix_mp(clause(_,_),loves(x,y),M,P),
   M:P==socialMt:loves(x,y))).

loves(sally,joe).

:- listing(loves/2).

baseKB:genlMt(myMt,socialMt).

%:- listing(myMt:_).

:- mpred_test(clause_u(socialMt:loves(_,_))).



:- listing(pfc_test_feature/2).

:- pfc_test_feature(localMt,myMt:loves(_,_)).

%:- listing(myMt:_).

:- mpred_test(clause(myMt:loves(_,_),_B,_R)).

:- mpred_test(clause_u(myMt:loves(_,_))).


:- rtrace.

:- mpred_test(myMt:loves(_,_)).

:- mpred_test((ain(genlMt(tooLazyMt,socialMt)),clause(tooLazyMt:loves(_,_),_B,_R))).

:- mpred_test(clause(tooLazyMt:loves(_,_),_B,_R)).

:- pfc_test_feature(mt,\+clause_u(tooLazyMt:loves(_,_))).

:- mpred_test(tooLazyMt:loves(_,_)).


:- must((ain(baseKB:genlMt(tooLazyBaseMt,socialMt)),clause(tooLazyBaseMt:loves(_,_),_B,_R))).

:- mpred_test(clause(tooLazyBaseMt:loves(_,_),_B,_R)).

:- pfc_test_feature(mt,\+clause_u(tooLazyBaseMt:loves(_,_))).

:- mpred_test(tooLazyBaseMt:loves(_,_)).




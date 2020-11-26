#!/usr/bin/env swipl
% 
% Dec 13, 2034
% Douglas Miles
%  cls ; kill -9 %1 ; swipl -g "ensure_loaded(pack(logicmoo_base/t/sanity_base/clause_fwd_01c.pfc'))."

:- include(test_header).
% :- set_prolog_flag(lm_pfc_lean,true).
% :- use_module(library(pfc)).

mpred_test_cf(P):- dmsg_pretty((:- mpred_test(P))),mpred_test_fok(P).

% never_assert_u(early_aa_H(Var),var):- cwc, is_ftVar(Var).
foo.                   

:- kb_local(aa/1).
:- kb_local(zz/1).
:- kb_local(early_aa_H/1).
:- kb_local(early_aa_HB/2).
:- kb_local(early_aa/1).
:- kb_local(late_aa_HB/2).
:- kb_local(late_aa/1).

:- kb_local(late_zz_H/1).
:- kb_local(zz/1).

:- kb_local(early_yy_H/1).
:- kb_local(yy/1).


( aa(N):- _B )==> early_aa_H(N). 
( aa(N):- B ) ==> early_aa_HB(N,B). 
( aa(N) ) ==> early_aa(N). 

aa(1):- writeln(1+1).
aa(2). 
aa(3):- true.
aa(N):- member(N,[4,5]).

( aa(N):- _B ) ==> late_aa_H(N). 
( aa(N):- B ) ==> late_aa_HB(N,B). 
( aa(N) ) ==> late_aa(N). 

zz(1):- foo.
( zz(N):- _B ) ==> late_zz_H(N). 
:- mpred_test_cf(\+ clause_asserted(late_zz_H(_))).


( yy(N):- _B ) ==> early_yy_H(N). 
yy(1):- foo.
:- mpred_test_cf(\+ clause_asserted(early_yy_H(_))).
:- mpred_test_cf(clause_asserted(early_yy_H(1))).

:- break.


:- mpred_test_cf(early_aa(1)).
:- mpred_test_cf(early_aa(2)).
:- mpred_test_cf(early_aa(3)).
:- mpred_test_cf(early_aa(4)).
:- mpred_test_cf(early_aa(5)).

:- mpred_test_cf(late_aa(1)).
:- mpred_test_cf(late_aa(2)).
:- mpred_test_cf(late_aa(3)).
:- mpred_test_cf(late_aa(4)).
:- mpred_test_cf(late_aa(5)).

:- mpred_test_cf(late_aa_HB(A, member(A, [4, 5]))).
:- mpred_test_cf(late_aa_HB(3, true)).
:- mpred_test_cf(late_aa_HB(2, true)).
:- mpred_test_cf(late_aa_HB(1, writeln(1+1))).


:- mpred_test_cf(early_aa_HB(A, member(A, [4, 5]))).
:- warn_fail_TODO(early_aa_HB(3, true)).
:- warn_fail_TODO(early_aa_HB(2, true)).
:- mpred_test_cf(early_aa_HB(1, writeln(1+1))).



:- mpred_test_cf(late_aa_H(1)).
:- mpred_test_cf(late_aa_H(2)).
:- mpred_test_cf(late_aa_H(3)).
:- mpred_test_cf(late_aa_H(_)).

%:- rtrace.
:- mpred_test_cf(clause_asserted(late_aa_H(_))).
:- nortrace.

:- mpred_test_cf(early_aa_H(1)).
:- mpred_test_cf(early_aa_H(2)).
:- mpred_test_cf(early_aa_H(3)).
:- mpred_test_cf(early_aa_H(_)).
:- mpred_test_cf(clause_asserted(early_aa_H(_))).


/*

:- mpred_test_cf(\+ clause_asserted(late_aa_H(4))).
:- mpred_test_cf(\+ clause_asserted(late_aa_H(5))).
:- mpred_test_cf(\+ clause_asserted(early_aa_H(4))).
:- mpred_test_cf(\+ clause_asserted(early_aa_H(5))).


*/


:- listing([early_aa/1,late_aa/1]).

:- listing([early_aa_HB/2,late_aa_HB/2]).

:- listing([early_aa_H/1,late_aa_H/1]).

:- listing(late_zz_H/1).
:- listing(early_yy_H/1).


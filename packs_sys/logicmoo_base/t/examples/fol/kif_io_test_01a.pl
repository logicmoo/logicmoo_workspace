#!/usr/bin/env lmoo-junit
:- include(test_header).

% 
% :- kif_io.
% Alternatively..
:- prolog_load_context(stream,Stream), load_clif(Stream).

;; Module - Test Puzzle in CLIF for SWI-Prolog
;; Maintainer: Douglas Miles
;; Load with  ?- load_clif(pack(logicmoo_base/t/examples/fol/'exactly_01.clif'))
;; causes deduction of argument types
;; (:- (ensure_loaded (library logicmoo_clif)))

(set-kif-option :assume-wff)
(kif-mode tell)

(call-prolog #|

:- dbreq(retractall(clif(_))).

|#)

;;   Every person has exactly one mother.
;;  (Ax:person)(Exactly-one y:woman) mother_of(x,y).
;;(Ax)(Ey)(person(x) -> (woman(y) & mother_of(x,y) & (Az)(mother_of(x,z) -> z=y))).
;; Uniqueness is even harder to express with conventional quantifiers:
;;   Every person has a unique social security number.
;; (Ax:person)(Unique y:number) ssno(x,y).
;; (Ax)(Ey)(person(x) -> (number(y) & ssno(x,y) & (Az)(ssno(x,z) -> z=y) & (Aw)(ssno(w,y) -> w=x))).

(exists (x y z) 
 (and ("is a Stagirite" x) (teaches x y) ("is a Macedonian" y)
  ("conquers the world" y) ("is a disciple of" x z) ("is an opponent of" x z)
  (“is a philosopher admired by church fathers” z)))


(domain hasName 1 tAgent)
(domain hasName 2 tName)
(argQuotedIsa hasName 2 ftString)


(domain address 1 tHouse)
(domain address 2 tAddress)

;; Special Logicmoo operator specific to forward chaining
;; (==> (and (domain ?P ?n ?c) (admittedArgument ?p ?n ?i)) (instance ?i ?c))

; ============================================================
; Devil has exactly one name
; ============================================================

(exactly 1
  ((?name tName))
  (hasName Devil ?name))


; ============================================================
; if a and be then c
; ============================================================
(if (and a b) c)


; ============================================================
; Exactly 1
; ============================================================
(exactly 1
  ((?h1 tHouse)) (memberOf ?h1 tOneHouseSet ))

; ============================================================
; Exactly 2
; ============================================================
(exactly 2
  ((?h1 tHouse)) (memberOf ?h1 tTwoHouseSet ))

; ============================================================
; Exactly 5
; ============================================================
(exactly 5
  ((?h1 tHouse)) (memberOf ?h1 tFiveHouseSet ))


(comment ThisFile666 "

There should be at most 8 houses with assertions about them.
And at least 5 houses.


(isa ?X tHouse)



")

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/450 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/kif_io_test_01a.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/KIF_IO_TEST_01A/logicmoo_base_examples_fol_KIF_IO_TEST_01A_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AKIF_IO_TEST_01A 


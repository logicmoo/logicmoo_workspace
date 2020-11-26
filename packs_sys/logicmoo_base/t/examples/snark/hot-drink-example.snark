;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: snark-user -*-
;;; File: hot-drink-example.lisp
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations
;;; under the License.
;;;
;;; The Original Code is SNARK.
;;; The Initial Developer of the Original Code is SRI International.
;;; Portions created by the Initial Developer are Copyright (C) 1981-2005.
;;; All Rights Reserved.
;;;
;;; Contributor(s): Mark E. Stickel <stickel@ai.sri.com>.

(in-package :snark-user)

;;; this is a simple example of one way of implementing partitions in SNARK
;;; rows are annotated with the partitions they're in and inferences are
;;; restricted to rows in the same partitions
;;;
;;; a partition communication table is used to augment the annotation
;;; of derived rows in case the row should be included in a neighboring
;;; partition too
;;;
;;; the partition communication table computation is invoked by including
;;; it as a pruning test
;;;
;;; this code is more illustrative than definitive

;;; partition communication table is a set of triples
;;; (from-partition to-partition relation-names) like
;;;  (1 2 (water))
;;;  (2 3 (steam))

(defun row-predicate-names (row)
  (row-relation-names row))

(defun row-relation-names (row)
  ;; returns list of relation names in formula part
  ;; (but not answer, constraint, etc. parts) of a row
  (let ((names nil))
    (prog->
      (snark::map-atoms-in-wff (row-wff row) ->* atom polarity)
      (declare (ignore polarity))
      (dereference
       atom nil
       :if-constant (pushnew (constant-name atom) names)
       :if-compound (pushnew (function-name (head atom)) names)))
    names))

(defun partition-communication (row)
  ;; could try to refine the context for added partitions
  (when (use-partitions?)
    (let ((table (partition-communication-table?))
          (preds (row-relation-names row))
          (context (snark::row-context row))
          (more-context nil))
      (flet ((message-passing-from (x)
               (prog->
                 (car x -> part1)
                 (sparse-matrix-row table part1 ->nonnil row)
                 (cdr x -> ctxt1)
                 (map-sparse-vector-with-indexes row ->* preds2 part2)
                 (when (and (null (assoc part2 context))
                            (null (assoc part2 more-context))
                            (subsetp preds preds2))
                   (push (cons part2 ctxt1) more-context)
                   nil))))
        (mapc #'message-passing-from context)
        (do ()
            ((null more-context))
          (push (pop more-context) context)
          (message-passing-from (first context)))
        (setf (snark::row-context row) context))))
  nil)

(defun hot-drink-example (&key (use-partitions t) (use-ordering nil))
  ;; Amir & McIlraith partition-based reasoning example
  (initialize)
  (when use-partitions
    (use-partitions '(1 2 3))
    (partition-communication-table
     (let ((pct (make-sparse-matrix)))
       (setf (sparef pct 1 2) '(water)
             (sparef pct 2 3) '(steam))
       pct))
    (pruning-tests (append (pruning-tests?) '(partition-communication))))
  (cond
   (use-ordering
    (use-resolution t)
    (use-literal-ordering-with-resolution 'literal-ordering-a)
    (declare-proposition 'ok_pump)
    (declare-proposition 'on_pump)
    (declare-proposition 'man_fill)
    (declare-proposition 'water)
    (declare-proposition 'ok_boiler)
    (declare-proposition 'on_boiler)
    (declare-proposition 'steam)
    (declare-proposition 'coffee)
    (declare-proposition 'hot_drink)
    (declare-ordering-greaterp '(ok_pump on_pump man_fill) 'water)
    (declare-ordering-greaterp '(water ok_boiler on_boiler) 'steam)
    (declare-ordering-greaterp 'coffee 'hot_drink))
   (t
    (use-hyperresolution t)))
  (dolist (wff '((or (not ok_pump) (not on_pump) water)
                 (or (not man_fill) water)
                 (or (not man_fill) (not on_pump))
                 (or man_fill on_pump)))
    (assert wff :partitions '(1)))
  (dolist (wff '((or (not water) (not ok_boiler) (not on_boiler) steam)
                 (or water (not steam))
                 (or ok_boiler (not steam))
                 (or on_boiler (not steam))))
    (assert wff :partitions '(2)))
  (dolist (wff '((or (not steam) (not coffee) hot_drink)
                 (or coffee teabag)
                 (or (not steam) (not teabag) hot_drink)))
    (assert wff :partitions '(3)))
  (assume 'ok_pump :partitions '(1))
  (assume 'ok_boiler :partitions '(2))
  (assume 'on_boiler :partitions '(2))
  (closure))  

;;; hot-drink-example.lisp EOF

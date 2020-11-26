;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; presupp.lsp [Chapter 10] presupposition grammar and inference rules

(defvar rules)
(defvar lexical_rules)
(defvar infrules)

(uses 'dagunify)

(setq rules
 '((Rule (IMP -> V NP PP)
     (IMP cat) = IMP
     (V cat) = V
     (NP cat) = NP
     (PP cat) = PP
     (V p) = (PP p)
     (V arg1) = (NP referent)
     (V arg2) = (PP arg1)
     (IMP sem) = (V sem)
     (IMP pre connective) = and
     (IMP pre prop1) = (V pre)
     (IMP pre prop2 connective) = and
     (IMP pre prop2 prop1) = (NP pre)
     (IMP pre prop2 prop2) = (PP np_pre))
   (Rule (NP -> DET N)
     (NP cat) = NP
     (DET cat) = DET
     (N cat) = N
     (NP pre) = (N pre)
     (NP referent) = (N referent))
   (Rule (NP1 -> NP2 PP)
     (NP1 cat) = NP
     (NP2 cat) = NP
     (PP cat) = PP
     (NP1 referent) = (NP2 referent)
     (NP1 referent) = (PP arg0)
     (NP1 pre connective) = and
     (NP1 pre prop1) = (NP2 pre)
     (NP1 pre prop2 connective) = and
     (NP1 pre prop2 prop1) = (PP p_pre)
     (NP1 pre prop2 prop2) = (PP np_pre))
   (Rule (PP -> P NP)
     (PP cat) = PP
     (P cat) = P
     (NP cat) = NP
     (PP arg0) = (P arg0)
     (PP arg1) = (NP referent)
     (PP arg1) = (P arg1)
     (PP p) = (P p)
     (PP np_pre) = (NP pre)
     (PP p_pre) = (P pre))))

(setq lexical_rules
 '((Word (in)
     (cat) = P
     (p) = in
     (pre predicate) = in
     (pre arg0) = (arg0)
     (pre arg1) = (arg1))
   (Word (on)
     (cat) = P
     (p) = on
     (pre predicate) = on
     (pre arg0) = (arg0)
     (pre arg1) = (arg1))
   (Word (to)
     (cat) = P
     (p) = to
     (pre predicate) = true)
   (Word (fix)
     (cat) = V
     (sem predicate) = fix
     (sem arg1) = (arg1)
     (sem arg2) = (arg2)
     (pre predicate) = manipulate
     (pre arg0) = (arg1))
   (Word (put)
     (cat) = V
     (p) = in
     (sem predicate) = put_in
     (sem arg1) = (arg1)
     (sem arg2) = (arg2)
     (pre predicate) = fit_in
     (pre arg0) = (arg1)
     (pre arg1) = (arg2))
   (Word (put)
     (cat) = V
     (p) = on
     (sem predicate) = put_on
     (sem arg1) = (arg1)
     (sem arg2) = (arg2)
     (pre predicate) = fit_on
     (pre arg0) = (arg1)
     (pre arg1) = (arg2))
   (Word (box)
     (cat) = N
     (pre predicate) = box
     (pre arg0) = (referent))
   (Word (screw)
     (cat) = N
     (pre predicate) = screw
     (pre arg0) = (referent))
   (Word (washer)
     (cat) = N
     (pre predicate) = washer
     (pre arg0) = (referent))
   (Word (hole)
     (cat) = N
     (pre predicate) = hole
     (pre arg0) = (referent))
   (Word (the)
     (cat) = DET)
   (Word (it)
     (pre predicate) = true
     (cat) = NP)))                    

(defun category (d subst)
  (list
    (find_feature_value 'pre d subst)
    (find_feature_value 'sem d subst)))

(defun tree (cat subtrees)
  cat)

;;; inference rules

(setq infrules
  '(((manipulate _x) (screw _x))
    ((manipulate _x) (washer _x))
    ((fit_in washer1 hole1))
    ((fit_in screw2 hole1))
    ((fit_in _x box1) (screw _x))
    ((fit_in _x box1) (washer _x))
    ((fit_on _x _y) (washer _x))
    ((on washer1 screw1))
    ((screw screw1))
    ((screw screw2))
    ((washer washer1))
    ((hole hole1))
    ((box box1))
    ((true _x))))

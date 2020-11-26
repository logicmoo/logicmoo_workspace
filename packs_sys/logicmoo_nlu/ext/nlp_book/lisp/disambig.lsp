;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; disambig.lsp [Chapter  8] PATR grammar showing disambiguation by semantic features

;;; This grammar illustrates the use of semantic markers to
;;; reduce the number of parses
;;; E.g. it only produces one analysis of each of the following
;;;     get a screwdriver with a narrow blade
;;;     get a screwdriver with your left hand

(setq rules
 '((Rule (VP  -> TV NP)
       (VP cat) = VP
       (TV cat) = TV
       (NP cat) = NP
       (TV arg1 type) = (NP type))
   (Rule (VP1 -> VP2 PP)
       (VP1 cat) = VP
       (VP2 cat) = VP
       (PP cat) = PP
       (PP arg0 type) = action)
   (Rule (PP  -> P NP)
       (PP cat) = PP
       (P cat) = P
       (NP cat) = NP
       (PP arg0 type) = (P arg0 type)
       (P arg1 type) = (NP type))
   (Rule (NP  -> Det NB)
       (NP cat) = NP
       (Det cat) = Det
       (NB cat) = NB
       (NP type) = (NB type))
   (Rule (NP1 -> NP2 PP)
       (NP1 cat) = NP
       (NP2 cat) = NP
       (PP cat) = PP
       (NP1 type) = (NP2 type)
       (PP arg0 type) = (NP2 type))
   (Rule (NB  -> N)
       (NB cat) = NB
       (N cat) = N
       (NB type) = (N type))
   (Rule (NB1 -> Adj NB2)
       (NB1 cat) = NB
       (Adj cat) = Adj
       (NB2 cat) = NB
       (NB1 type) = (Adj arg0 type)
       (NB1 type) = (NB2 type))))

(setq lexical_rules
 '((Word (with)
       (cat) = P
       (arg0 type) = action
       (arg1 type major) = physobj
       (arg1 type minor) = instrument)
   (Word (with)
       (cat) = P
       (arg0 type major) = physobj
       (arg1 type major) = physobj
       (arg1 type minor) = component)
   (Word (get)
       (cat) = TV
       (arg1 type major) = physobj)
   (Word (a)
       (cat) = Det)
   (Word (your)
       (cat) = Det)
   (Word (screwdriver)
       (cat) = N
       (type major) = physobj)
   (Word (hand)
       (cat) = N
       (type major) = physobj
       (type minor) = instrument)
   (Word (blade)
       (cat) = N
       (type major) = physobj
       (type minor) = component)
   (Word (narrow)
       (cat) = Adj
       (arg0 type major) = physobj)
   (Word (left)
       (cat) = Adj
       (arg0 type major) = physobj)))

(defun category (d subst)
   (find_feature_value 'cat d subst))

(defun tree (cat subtrees)
   (cons cat subtrees))

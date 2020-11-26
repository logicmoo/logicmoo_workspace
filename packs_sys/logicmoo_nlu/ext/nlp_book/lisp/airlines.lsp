;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; airlines.lsp [Chapter  8] PATR grammar, with semantics, for question-answering

;;; extracting the useful information from a dag

(defun category (d subst)
   (find_feature_value 'sem d subst))

;;; building a parse tree - just keep the head category  

(defun tree (cat subtrees) cat)

(setq lexical_rules
 '((Word (Delta)
        (cat) = NP
        (referent) = TWA
        (sem)  = (hole))
   (Word (is independent)
        (cat) = VP
        (sem predicate) = independent
        (sem arg0)     = (arg0))
   (Word (took over)
        (cat) = TV
        (sem predicate) = took_over
        (sem arg0)     = (arg0)
        (sem arg1)     = (arg1))
   (Word (airline)
       (cat) = N
       (sem predicate) = airline
       (sem arg0) = (referent))
   (Word (hotel chain)
       (cat) = N
       (sem predicate) = hotel_chain
       (sem arg0) = (referent))
   (Word (every)
       (cat) = Det
       (sem quantifier) = all)
   (Word (each)
       (cat) = Det
       (sem quantifier) = all)
   (Word (an)
       (cat) = Det
       (sem quantifier) = exists)
   (Word (a)
       (cat) = Det
       (sem quantifier) = exists)
   (Word (who)
        (cat) = NP
        (sem quantifier) = all
        (sem restriction) = (hole)
        (sem body action) = printout
        (sem body arg0) = (sem variable)
        (sem variable) = (referent))))

(setq rules
 '((Rule (S -> NP VP)
        (S cat) = S
        (NP cat) = NP
        (VP cat) = VP
        (S sem) = (NP sem)
        (NP hole) = (VP sem)
        (NP referent) = (VP arg0))
   (Rule (VP -> TV NP)
        (VP cat) = VP
        (TV cat) = TV
        (NP cat) = NP
        (VP sem) = (NP sem)
        (NP hole) = (TV sem)
        (TV arg0) = (VP arg0)
        (TV arg1) = (NP referent))
   (Rule (NP -> Det N)
        (NP cat) = NP
        (Det cat) = Det
        (N cat) = N
        (NP sem quantifier) = (Det sem quantifier)
        (NP sem variable) = (NP referent)
        (NP sem restriction) = (N sem)
        (NP sem body) = (NP hole)
        (N referent) = (NP referent))))

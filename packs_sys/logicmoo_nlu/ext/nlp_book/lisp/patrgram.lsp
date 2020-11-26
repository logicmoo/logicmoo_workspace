;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; patrgram.lsp [Chapter  7] simple PATR grammar

(setq rules
    '((Rule (S   -> NP VP)
            (S cat) = S
            (NP cat) = NP
            (VP cat) = VP
            (S slash) = (VP slash)
            (NP slash) = 0)

       (Rule (VP  -> V X)
             (VP cat) = VP
             (V cat) = V
             (V arg1) = (X cat)
             (V slash) = 0
             (VP slash) = (X slash))

       (Rule (PP   -> P  X)
             (PP cat) = PP
             (P cat) = P
             (P arg1) = (X cat)
             (P slash) = 0
             (PP slash) = (X slash))

;;;      (Rule (X0  -> X1 C X2)
;;;            (X0 cat) = (X1 cat)
;;;            (X0 cat) = (X2 cat)
;;;            (C cat) = C
;;;            (C slash) = 0
;;;            (X0 slash) = (X1 slash)
;;;            (X0 slash) = (X2 slash)
;;;            (X0 arg1) = (X1 arg1)
;;;            (X0 arg1) = (X2 arg1))

       (Rule (S  -> X1  X2)
             (S cat) = S
             (S slash) = 0
             (X1 slash) = 0
             (X1 empty) = no
             (X2 cat) = S
             (X2 slash) = (X1 cat)
             (X2 empty) = no)

       (Rule (X0  -> )
             (X0 cat) = (X0 slash)
             (X0 empty) = yes)
))

(setq lexical_rules
  '((Word (approved)
        (cat) = V
        (slash) = 0
        (arg1) = PP)
    (Word (disapproved)
        (cat) = V
        (slash) = 0
        (arg1) = PP)
    (Word (appeared)
        (cat) = V
        (slash) = 0
        (arg1) = AP)
    (Word (seemed)
        (cat) = V
        (slash) = 0
        (arg1) = AP)
    (Word (had)
        (cat) = V
        (slash) = 0
        (arg1) = VP)
    (Word (believed)
        (cat) = V
        (slash) = 0
        (arg1) = S)
    (Word (thought)
        (cat) = V
        (slash) = 0
        (arg1) = S)
    (Word (of)
        (cat) = P
        (slash) = 0
        (arg1) = NP)
    (Word (fit)
        (slash) = 0
        (cat) = AP)
    (Word (competent)
        (slash) = 0
        (cat) = AP)
    (Word (well - qualified)
        (slash) = 0
        (cat) = AP)
    (Word (Dr Chan)
        (slash) = 0
        (cat) = NP)
    (Word (nurses)
        (slash) = 0
        (cat) = NP)
    (Word (MediCenter)
        (slash) = 0
        (cat) = NP)
    (Word (patients)
        (slash) = 0
        (cat) = NP)
    (Word (died)
        (arg1) = 0
        (slash) = 0
        (cat) = V)
    (Word (employed)
        (arg1) = NP
        (slash) = 0
        (cat) = V)))

(defun category (d subst)
  (let (
     (cat (find_feature_value 'cat d subst))
     (slash (find_feature_value 'slash d subst)))
    (if (equal slash 0)
      cat
      (list cat '/ slash))))

(defun tree (cat subtrees)
  (cons cat subtrees))

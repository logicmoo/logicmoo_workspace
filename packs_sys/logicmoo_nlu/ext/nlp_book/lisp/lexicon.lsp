;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; lexicon.lsp [Chapter  7] example lexical entries

(uses 'lisppatr)

(setf (get 'mor_regV 'patr_macro)
  '((mor form1 stem)   = (mor root)
    (mor form1 suffix) = NULL
    (mor form2 stem)   = (mor root)
    (mor form2 suffix) = NULL
    (mor form3 stem)   = (mor root)
    (mor form3 suffix) = s
    (mor form4 stem)   = (mor root)
    (mor form4 suffix) = ed
    (mor form5 stem)   = (mor root)
    (mor form5 suffix) = ed
    (mor form6 stem)   = (mor root)
    (mor form6 suffix) = ed
    (mor form7 stem)   = (mor root)
    (mor form7 suffix) = ing       ))

(setf (get 'syn_iV 'patr_macro)
  '((syn cat) = V
    (syn arg0 cat) = np
    (syn arg0 case) = nom))

(setf (get 'syn_tV 'patr_macro)
  '( syn_iV
    (syn arg1 cat) = np
    (syn arg1 case) = acc))

;;; Associating lexemes with conditions - we will use the property 'lexicon

(defun lookup_conditions (lex)
  (get lex 'lexicon))

(defun set_conditions (lex conds)
  (setf (get lex 'lexicon) conds))

;;; example setting up of a lexical entry

(set_conditions 'love
  '((mor root) = love
    (sem) = love2a
    mor_regV
    syn_tV           ))

(defvar example_wfc)

(setq example_wfc
  '((word mor form) = (lexeme mor form3)
    (word syn) = (lexeme syn)
    (word syn cat) = V
    (word syn arg0 per) = 3
    (word syn arg0 num) = sing
    (word syn tense) = pres
    (word sem) = (lexeme sem)           ))

;;; look up a word in the lexicon

(defun lookup_lex (lex)
  (if (lookup_conditions lex)
    (let*
      ((structure (newvar))
       (subst (apply_condition (lookup_conditions lex) structure empty_subst)))
      (if subst
        (apply_subst subst structure)
        (error "Inconsistent lexical entry for word ~S" lex)))
    (error "No lexical entry for word ~S" lex)))

;;; apply a WFC to a skeleton word entry and lexeme entry,
;;; returning a full word entry if possible

(defun apply_wfc (wfc word lex)
  (let*
    ((structure (list (list 'word word) (list 'lexeme lex)))
     (subst (apply_condition wfc structure empty_subst)))
    (if subst
      (simplify_features subst word)
      nil)))

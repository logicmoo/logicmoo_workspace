;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; featexp.lsp [Chapter  4] expanding a feature-based grammar into a CF-PSG

;;; Assume that all the possible features, together with all
;;; their possible values, are declared in the global variable
;;; features:
;;;
;;; (setq features
;;;  '((cat S NP VP PP AP P N V 0)
;;;    (slash S NP VP PP AP P N V 0)
;;;    (subcat S NP VP PP AP P N V 0)
;;;    (empty yes no)))

(defvar features)
(defvar varlist)

;;; expand a rule into all possible instances

(defun expand_rule (rule)
  (let ((newrule (fillout_rule rule)))
    (subst_values varlist newrule)))

;;; Produce a variant of a rule where every category simply
;;; lists in order a value for each possible feature.
;;; Entries for features come out in the reverse order to
;;; the order of the global variable features

(defun fillout_rule (rule)
  (setq varlist ())
  (mapcar #'fillout_cat rule))

(defun fillout_cat (cat)
  (let ((results ()))
    (dolist (flist features results)
      (setq results (cons (get_feature_val (car flist) cat) results)))))

;;; for all possible name/value possibilities in the VARIABLE_LIST,
;;; print out the appropriate version of the rule

(defun subst_values (varlist rule)
  (if (null varlist)
    (print rule)
    (dolist (val (cdar varlist))
      (subst_values (cdr varlist)
        (subst_value (caar varlist) val rule)))))

;;; produce a new version of a thing (e.g. rule) which has VALUE
;;; substituted for each occurrence of NAME

(defun subst_value (name val thing)
  (if (equal name thing)
    val
    (if (consp thing)
      (cons
        (subst_value name val (car thing))
        (subst_value name val (cdr thing)))
      thing)))

;;; find the entry in a category for a given feature name.
;;; if the entry is a variable, make sure it is in the VARLIST.
;;; if there is no entry, introduce a new variable for it

(defun get_feature_val (featname category)
  (let
    ((val
       (if (assoc featname category)
         (cadr (assoc featname category))
         (gensym '_)))
     (possvalues (cdr (assoc featname features))))
    (if (and (symbolp val)
        (equal (char (symbol-name val) 0) #\_)
        (not (assoc val varlist)))
      (setq varlist (cons (cons val possvalues) varlist)))
    val))

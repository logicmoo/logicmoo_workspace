;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; scripts.lsp [Chapter 10] script matching

;;; the global variable scripts holds the list of scripts

;;; Example value:

(defvar scripts)

(setq scripts
 '(
    ((auto_buy _Customer _Auto1 _Auto2 _Driver _Garage)
     (goes _Customer _Garage)
     (test_drives _Customer _Auto1)
     (orders _Customer _Auto2 _Driver)
     (delivers _Driver _Auto2 _Customer)
     (drives _Customer _Auto2))

    ((hat_buy _Customer _Hat _Assistant _Store)
     (goes _Customer _Store)
     (tries_on _Customer _Hat)
     (buys _Customer _Hat _Assistant)
     (delivers _Assistant _Hat _Customer)
     (wears _Customer _Hat))
))

(uses 'subst)
(uses 'tunify)

;;; SCRIPT_MATCH is given a list of propositions, eg.
;;;    ((tries_on fred hat7) (wears _x _y))
;;; It tries to match them in the order given to the
;;; expectations expressed in a script

(defun script_match (story)
  (dolist (script scripts nil)
    (let ((predictions (cdr script)))
      (let (
         (substs (predict_all_ways predictions story empty_subst)))
        (if substs
          (return (apply_subst (car substs) script)))))))


;;; test to see whether a list of predictions can be matched with a story.
;;; For each possible way, a corresponding substitution is returned

(defun predict_all_ways (predictions story current_subst)
  (if (null story)
    (list current_subst)
    (let ((event (car story)) (successes ()))
      (do (
         (restpredictions (cdr predictions) (cdr restpredictions))
         (predict (car predictions) (car restpredictions)))
        ((null predict) successes)
        (let ((subst (termunify event predict)))
          (if subst
            (setq successes
              (append
                (predict_all_ways
                  (apply_subst subst restpredictions)
                  (apply_subst subst (cdr story))
                  (compose_substs subst current_subst))
                successes))))))))

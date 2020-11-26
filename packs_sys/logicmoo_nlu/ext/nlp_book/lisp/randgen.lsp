;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randgen.lsp [Chapter  4] random generation from a CF-PSG

(defvar rules)

(defun generate (description)
  (if (atom description)
    (list description)
    (let ((rs (matching_rules description)))
      (if (null rs)
        (error "Cannot generate description")
        (generate_all (cdr (oneof rs)))))))

(defun oneof (list)
  ;;  randomly returns one of the given list
  (nth (random (length list)) list))

(defun generate_all (body)
  (if (null body)
    '()
    (append
      (generate (car body))
      (generate_all (cdr body)))))

(defun matching_rules (description)
  (let ((results ()))
    (dolist (rule rules results)
      (if (unify description (car rule))
        (setq results (cons rule results))))))

(defun unify (x y)
  (equal x y))

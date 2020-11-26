;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randtree.lsp [Chapter  4] random generation of parse trees from a CF-PSG

(defvar rules)

(defun generate (description)
  (if (atom description)
    description
    (let ((rs (matching_rules description)))
      (if (null rs)
        (error "Cannot generate from description ~S" description)
        (cons
          (category description)
          (generate_all (cdr (oneof rs))))))))

(defun generate_all (body)
  (if (null body)
    '()
    (cons
      (generate (car body))
      (generate_all (cdr body)))))

(defun category (description)
  (car description))
                     
(defun matching_rules (description)
  (let ((results ()))
    (dolist (rule rules results)
      (if (unify description (car rule))
        (setq results (cons rule results))))))

(defun unify (x y)
  (equal x y))

(defun oneof (list)
  ;;  randomly returns one of the given list
  (nth (random (length list)) list))

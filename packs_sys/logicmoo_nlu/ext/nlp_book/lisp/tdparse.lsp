;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tdparse.lsp [Chapter  5] top-down parser for a CF-PSG

(defun find_trees (goal string)
  ;; returns a list of (parse_tree remaining_string) pairs
  (if (listp goal)              ; a category
    (let ((results ()))
      (dolist (rule rules)
        (if (equal goal (car rule))
          (dolist (treelist_remainder (find_subtrees (cdr rule) string))
            (setq results
              (cons
                (list
                  (cons (car goal) (car treelist_remainder)) ; parse tree
                  (cadr treelist_remainder)) ; remainder
                results)))))
      results)
    (if (equal goal (car string))      ; a terminal
      (list (list goal (cdr string))))))

(defun find_subtrees (goals string)
  ;;  returns list of (list_of_daughter_trees string_remainder)
  (if (null goals)
    (list (list '() string))
    (let ((results ()))
      (dolist (tree_rem (find_trees (car goals) string))
        ;; find parses of first goal
        (dolist (trees_rem (find_subtrees (cdr goals) (cadr tree_rem)))
          (setq results
            (cons
              (list
                (cons (car tree_rem) (car trees_rem)) ; tree
                (cadr trees_rem))                     ; remainder
              results))))
      results)))

(defun parse (goal string)
  (let ((results ()))
    (dolist (tree_rem (find_trees goal string) results)
      (if (null (cadr tree_rem)) ; if uses the whole string
        (setq results (cons (car tree_rem) results))))))

;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; chart.lsp [Chapter  6] simple chart parser

;;; This file contains code for both top-down and bottom-up
;;; but the former is commented out


(defvar chart)
(defvar agenda)

;;  an edge is a list of 5 elements :
;;      start finish label found tofind
(defun start (edge)
  (nth 0 edge))

(defun finish (edge)
  (nth 1 edge))

(defun label (edge)
  (nth 2 edge))

(defun found (edge)
  (nth 3 edge))

(defun tofind (edge)
  (nth 4 edge))

;;; add an edge to the chart, recording any new edges that may need
;;; to be added as a consequence

(defun add_edge (edge)
  (setq chart (cons edge chart))
  (if (null (tofind edge))    ; added edge is inactive
    (progn
      (dolist (chartedge chart)
        (if (not (null (tofind chartedge)))   ; look for active edge
          (check_and_combine chartedge edge)))
      (inactive_edge_function edge))
    (progn  ; otherwise added edge is active
      (dolist (chartedge chart)
        (if (null (tofind chartedge))   ; look for inactive edge
          (check_and_combine edge chartedge)))
      (active_edge_function edge))))

;;; try to combine an active and inactive edge
;;; using the fundamental rule
;;; add a new edge to agenda if these can combine

(defun check_and_combine (active_edge inactive_edge)
  (if
    (and
      (equal (start inactive_edge) (finish active_edge))
      (equal (label inactive_edge) (car (tofind active_edge))))
    (agenda_add
      (list                         ; new edge
        (start active_edge)
        (finish inactive_edge)
        (label active_edge)
        (append (found active_edge)
          (list
            (tree (label inactive_edge) (found inactive_edge))))
        (cdr (tofind active_edge))))))

;;; initialize the chart (bottom-up version)

(defun initialize_chart (goal string)
  (do* (
     (vertex 0 (+ vertex 1))
     (remaining string (cdr remaining))
     (word (car string) (car remaining)))
    ((null word))
    (agenda_add
      (list
        ;; (start finish label found tofind)
        vertex (+ 1 vertex) word nil nil))))

;;; top level function

(defun chart_parse (goal string)
  (setq agenda nil)
  (setq chart nil)
  (initialize_chart goal string)
  (do
    ( (edge (car agenda) (car agenda)) )
    ((null agenda))    ;; do until agenda empty
    (setq agenda (cdr agenda))
    (add_edge edge))  ;; add and combine edge with chart
  (let ((parses ()))
    (dolist (edge chart parses)
      (if (and
          (equal (start edge) 0)
          (equal (finish edge) (length string)) ; end of string
          (equal (label edge) goal)        ; recognizes goal
          (null (tofind edge)))         ; edge complete
        (setq parses
          (cons
            (tree goal (found edge))  ; parse tree
            parses))))))

;;; bottom up parsing functions

(defun inactive_edge_function (edge)
  (dolist (rule rules)
    (if (equal (label edge) (cadr rule)) ; the first daughter in the rhs
      (agenda_add
        (list
          (start edge) (start edge) (car rule) nil (cdr rule))))))

(defun active_edge_function (edge) t)

;;; depth first search

(defun agenda_add (edge)
  (if (or
      (already_in edge agenda)   ; left recursion check
      (already_in edge chart))
    nil            ; do not add to agenda
    (setq agenda (cons edge agenda))))  ; add to front of agenda

(defun already_in (edge edgelist)
  (member edge edgelist :test #'equal))

;;; building parse trees

(defun tree (cat subtrees)
  (if (consp cat)
    (cons (car cat) subtrees)
    cat))

;;; top down functions (commented out)

;;; remove the '(consp' line and the one marked below to release
;;; the top-down code
(consp '(

(defun inactive_edge_function (edge) t)

(defun active_edge_function (edge)
   (add_rules_to_expand (car (tofind edge)) (finish edge)))

(defun add_rules_to_expand (goal vertex)
  (dolist (rule rules)
    (if (equal goal (car rule)) ; the lhs of the rule
      (agenda_add
        (list vertex vertex (car rule) nil (cdr rule))))))

(defun initialize_chart (goal string)
  (do* (
     (vertex 0 (+ vertex 1))
     (remaining string (cdr remaining))
     (word (car string) (car remaining)))
    ((null word))
    (agenda_add
      (list
        ;; (start  finish       label found tofind)
            vertex (+ 1 vertex) word  nil   nil   )))
  (add_rules_to_expand goal 0))

))  ;; remove this line to release the top-down code

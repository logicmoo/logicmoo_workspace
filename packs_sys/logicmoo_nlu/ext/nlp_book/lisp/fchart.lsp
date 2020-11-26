;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fchart.lsp [Chapter  7] chart parser for PATR grammars

;;; A version that does topdown with lexical lookup bottom-up
;;;
(uses 'lisppatr)
(uses 'subsumes)

(defvar rules)
(defvar lexical_rules)
(defvar chart)
(defvar agenda)
(defvar existing_goals)

;;; existing_goals is used to hold a dag of the form
;;;    ((cat c) (START s) (& ..)),
;;; where c is a category and s a starting position
;;; in the chart.  The presence of one of these in existing_goals
;;; indicates that we have already tried (topdown) looking for instances
;;; of the specified category starting at the specified position

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
  (if (null (tofind edge)) ; added edge is inactive
    (progn
      (dolist (chartedge chart)
        (if (not (null (tofind chartedge)))   ; an active edge
          (check_and_combine chartedge edge)))
      (inactive_edge_function edge))
    (progn ; otherwise added edge is active
      (dolist (chartedge chart)
        (if (null (tofind chartedge)) ; an inactive edge
          (check_and_combine edge chartedge)))
      (active_edge_function edge))))

;;; try to combine an active and inactive edge
;;; using the fundamental rule
;;; add a new edge to agenda if these can combine

(defun check_and_combine (active_edge inactive_edge)
  (if (equal (start inactive_edge) (finish active_edge))
    (let ((subst (unify (label inactive_edge) (car (tofind active_edge)))))
      (if subst
        (let (
           (subtrees
             (append (found active_edge)
               (list
                 (tree
                   (category (label inactive_edge) subst)
                   (found inactive_edge))))))
          (agenda_add
            (rename
              (apply_subst subst
                (list                         ;; new edge
                  (start active_edge)
                  (finish inactive_edge)
                  (label active_edge)
                  subtrees
                  (cdr (tofind active_edge)))))))))))

;;; initialize the chart (top-down version)

(defun initialize_chart (goal string)
  ;; add lexical edges
  ;; try each lexical rule in turn on each position in the chart
  ;; this is inefficient
  (do (
     (restwords string (cdr restwords))
     (vertex 0 (+ 1 vertex)))
    ((null restwords))
    (dolist (rule lexical_rules)
      (let ((subst_others (rhs_match restwords rule)))
        (if (car subst_others)  ; if subst not nil
          (let ((needed (ldiff restwords (cdadr subst_others))))
            (agenda_add
              (list
                vertex
                (+ vertex (length needed))
                (rename (caadr subst_others))
                needed
                nil)))))))
  ;; add goal edge
  (add_rules_to_expand goal 0))

;;; top level function

(defun chart_parse (goal string)
  (setq agenda nil)
  (setq chart nil)
  (setq existing_goals nil)
  (initialize_chart goal string)
  (do
    ((edge (car agenda) (car agenda)))
    ((null agenda))
    ;; do until agenda empty
    (setq agenda (cdr agenda))
    (add_edge edge)
    ;; add and combine edge with chart
    )
  (let ((parses ())) ; find complete parses
    (dolist (edge chart parses)
      (if (and
          (equal (start edge) 0)
          (equal (finish edge) (length string))   ; end of string
          (null (tofind edge))        ; edge complete
          (unify (label edge) goal))  ; recognizes goal
        (setq parses
          (cons
            (tree
              (category (label edge) empty_subst)
              (found edge))
            parses))))))

;;; other top-down parsing functions
;;; (no bottom-up operations here)

(defun inactive_edge_function (x) t)

;;; The topdown rule
;;;
;;; The topdown rule is invoked when an edge requiring a next phrase of
;;; category subgoal is added.  Now subgoal may be a very general
;;; category and hence not subsumed by any category already recorded in
;;; existing_goals.  When subgoal is unified with the LHS of a rule,
;;; however, the result will be more specific and may be subsumed by
;;; an existing goal.  So subsumption by an existing goal is tested
;;; after unification with the LHS of a rule.  On the other hand, once
;;; all the rules have been through, all ways of finding an instance
;;; of the original subgoal category category have been tried, and so
;;; it is this general category that is put into a new entry in
;;; existing_goals

(defun active_edge_function (edge)
  (add_rules_to_expand (car (tofind edge)) (finish edge)))

(defun add_rules_to_expand (goal vertex)
  (dolist (rule rules)
    (let
      ((subst_rhs (lhs_match goal rule)))
      (if (car subst_rhs)
        (let* (
           (LHS (apply_subst (car subst_rhs) goal))
           (RHS (cadr subst_rhs)))
          (if (not (subsumed_goal LHS vertex))
            ;; if goal not subsumed, add the new edge,
            ;; renaming so that it can combine with other
            ;; edges from rules using the same variable
            ;; symbols (e.g. edges from the same rule that just
            ;; produced it)
            (agenda_add
              (rename
                (list
                  vertex
                  vertex
                  LHS
                  nil
                  RHS))))))))
  (record_goal goal vertex))

;;; look to see whether a particular category, or one more
;;; general than it, has been looked for at a given vertex

(defun subsumed_goal (goal vertex)
  (let ((goaldag
       (list
         (list 'goal goal)
         (list 'vertex vertex)
         (list '& (newvar)))))
    (dolist (g existing_goals nil)
      (if (subsumes g goaldag)
        (return t)))))

;;; record that a particular category has been searched for
;;; at a given vertex

(defun record_goal (goal vertex)
  (setq existing_goals
    (cons
      (list
        (list 'goal goal)
        (list 'vertex vertex)
        (list '& (newvar)))
      existing_goals)))

;;; Add an edge to the agenda.  In topdown parsing, the only way that
;;; duplicate edges can be introduced is via the topdown rule (as
;;; long as there are no duplicate edges, the fundamental rule cannot
;;; possibly create any).  So for efficiency the checking of duplications
;;; is done in active_edge_function.

(defun agenda_add (edge)
  (setq agenda (cons edge agenda)))

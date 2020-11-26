;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; plan.lsp [Chapter 10] simple generation of illocutionary act plans

(uses 'bckinfer)
(uses 'tunify)

(defvar operators)

(setq operators '(
  ((request _speaker _addressee _act)
   ((can_do _addressee _act) (channel _speaker _addressee))
   ((want _speaker (request _speaker _addressee _act)))
   ((believe _addressee (want _speaker _act))))

  ((cause_to_want _agent1 _agent2 _act)
   ((can_do _agent2 _act) (believe _agent2 (want _agent1 _act)))
   ()
   ((want _agent2 _act)))

  ((move _agent _source _destination)
   ((at _agent _source))
   ((want _agent (move _agent _source _destination)))
   ((at _agent _destination)))

  ((inform _speaker _addressee _proposition)
   (_proposition (channel _speaker _addressee))
   ((want _speaker (inform _speaker _addressee _proposition)))
   ((believe _addressee (believe _speaker _proposition))))

  ((inform_ref _speaker _addressee _predicate)
   ((knows_ref _speaker _predicate) (channel _speaker _addressee))
   ((want _speaker (inform_ref _speaker _addressee _predicate)))
   ((knows_told_ref _addressee _speaker _predicate)))

  ((convince_ref _speaker _addressee _predicate)
   ((knows_told_ref _addressee _speaker _predicate))
   ()
   ((knows_ref _addressee _predicate)))

  ((convince _speaker _addressee _proposition)
   ((believe _addressee (believe _speaker _proposition)))
   ()
   ((believe _addressee _proposition)))))

;;; predicates for which there is universal knowledge

(defvar universal_knowledge)

(setq universal_knowledge '(channel at can_do knows_ref))

;;; The main function to call
;;; It assumes that the initial world model is in the variable
;;; infrules, as required for lib bckinfer.
;;; Note that facts recorded in infrules are interpreted
;;; outside all belief spaces, so that, for instance,
;;; beliefs of the planning agent need to be explicitly
;;; stated as such.  On the other hand, goals presented to
;;; plan are assumed to be interpreted in the planning
;;; agent's belief space.  Eg for the goal (at John inroom)
;;; the planner actually tries to satisfy
;;; (believe agent (at John inroom))

(defun plan (agent goals)
  (catch 'plan
	(do ((maxdepth 0 (1+ maxdepth))) ((equal maxdepth 10))
	  ;; maxdepth limits the number of actions allowed in a
	  ;; possible plan.  Increasing maxdepth by 1 each time
	  ;; results in a kind of breadth-first search
	  (print (list 'trying 'depth maxdepth))
	  (plan_next agent goals nil empty_subst 0 maxdepth))))

(defun plan_next (agent goals actions currentsubst depth maxdepth)
  (if (null goals)
    (throw 'plan (apply_subst currentsubst actions))
    (if (> depth maxdepth)
      nil
      (let ((goal (apply_subst currentsubst (car goals))) (othergoals (cdr goals)))
		(if (and
            (equal (car goal) 'believe)
            (consp (caddr goal))
            (member (caaddr goal) universal_knowledge))
          ;; (believe X (Y.Z)), Y universally known
          ;; - adopt (Y.Z) as a goal
          (plan_next agent (cons (caddr goal) othergoals) actions currentsubst depth maxdepth)
          (if (equal (car goal) 'can_do)
            (try_to_achieve_can_do agent goal othergoals actions
              currentsubst depth maxdepth)
            ;; (can_do X Y)
            ;; - find the can_do component of action Y and
            ;; - adopt them as goals
            (progn
              ;; the next two cases check for the goal being a want or belief
              ;; of the planning agent. since the wanter or believer specified
              ;; in the goal may not be uninstantiated, we have to consider
              ;; other possible wanters/believers as well as the planner
              (try_planners_own agent goal othergoals actions
                currentsubst depth maxdepth)
              ;; now look for instances of the goal in the database,
              ;; taking the point of view of the planning agent
              (try_already_true agent goal othergoals actions
                currentsubst depth maxdepth)
			  ;; even if one instance of a goal is already true,
			  ;; there may be other instances that are achievable
              (try_to_make_true agent goal othergoals actions
                currentsubst depth maxdepth))))))))

;;; Try to enable X to do action Y. Look up the can_do preconditions
;;; for this action and introduce them as planning goals

(defun try_to_achieve_can_do (agent goal goals actions currentsubst depth maxdepth)
  (dolist (operator operators)
    (let* (
       (o (rename operator))
       (subst (termunify (caddr goal) (car o))))
      (if subst
        (plan_next agent (append (cadr o) goals) actions
          (compose_substs subst currentsubst) depth maxdepth)))))

;;; Check for the goal being a want or belief
;;; of the planning agent. Since the wanter or believer specified
;;; in the goal may not be uninstantiated, we have to consider
;;; other possible wanters/believers as well as the planner

(defun try_planners_own (agent goal goals actions currentsubst depth maxdepth)
  (if (and (equal (car goal) 'want) (termunify agent (cadr goal)))
    ;; (want X Y), X could be agent
    ;; - goal immediately satisfied
    (plan_next agent goals actions
      (compose_substs (termunify agent (cadr goal)) currentsubst)
      depth maxdepth)
    (if (and (equal (car goal) 'believe) (termunify agent (cadr goal)))
      ;; (believe X Y), Y could be agent
      ;; - goal immediately satisfied
      (plan_next agent goals actions
        (compose_substs (termunify agent (cadr goal)) currentsubst)
        depth maxdepth))))

;;; try to match the goal against a fact already known to be true

(defun try_already_true (agent goal goals actions currentsubst depth maxdepth)
  (let ((dbgoal
       (if (member (car goal) universal_knowledge)
         goal
         (list 'believe agent goal))))
    (dolist (subst (back_infer dbgoal))
      (plan_next agent goals actions
        (compose_substs subst currentsubst) depth maxdepth))))

;;; see what actions might achieve a given goal, and add their preconditions
;;; to the list of goals

(defun try_to_make_true (agent goal goals actions currentsubst depth maxdepth)
  (dolist (operator operators)
    (let ((o (rename operator)))
      (dolist (e (cadddr o)) ; the effects of the operator
        (let ((subst (termunify goal e)))
          (if subst
            (plan_next agent (append (cadr o) (caddr o))
              (cons (car o) actions)
              (compose_substs subst currentsubst)
              (1+ depth) maxdepth)))))))

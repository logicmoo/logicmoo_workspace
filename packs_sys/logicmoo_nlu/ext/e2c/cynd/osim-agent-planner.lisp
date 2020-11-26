;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;
;; Copyright (c) 1998 Cycorp.  All rights reserved.
;;
;; sim-agent-planner.lisp
;;
;; Keith Goolsbey
;; 06/08/1998
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CYC")

(csetq *cvs-id* "$Id: sim-agent-planner.lisp,v 1.2 1999/07/08 00:31:10 dalberic Exp $")


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions for running the planner
;; Keith Goolsbey
;; 6/27/1998
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(0) Load the planner code.  Evaluate the following forms 
in your SubL interactor :
|#
(define reload-sim-planner ()
  (clet ((directory "./cynd/")
         (files '("sim-agent-planner" "sim-agent-planner-workarounds" "sim-agent-planner-html")))
    (cdolist (file files)
      (clet ((filename (format nil "~A~A~A" directory file ".lisp")))
        (load filename))))
  (csetq *forward-propagate-from-negations* nil)
  (ret nil))
 
#|
(reload-sim-planner)

From this point on, whenever there is an updated version of the
planner available, all you have to do to get the latest version is
click on [Reload Planner] on the page which displays the planner
search (this is described below).

(1) For efficiency, we will want to turn every existing forward rule
in the system which is not relevant to the planner backward.  This can
be accomplished by evaluating :

Dmiles: we dont do this though

|#
(define backward-rules-not-visible-to-mt (mt)
  (clet ((all-genls (all-genl-mts mt))
         (all-specs (all-spec-mts mt))
         (visible-mts (append all-genls all-specs))
         (message (format nil "Turning backward rules not visible to ~S" mt))
         (count 0))
   (do-assertions (ass message)
     (pwhen (cand (rule-assertion? ass)
                  (forward-assertion? ass)
                  (cnot (member (assertion-mt ass) visible-mts)))
       (cinc count)
       (tms-change-direction ass :backward)))
   (ret count)))

#|
(backward-rules-not-visible-to-mt #$WAMt)

The second formula will take a few minutes.  It is worth the wait.


(2) Now load the input files into an input MT.
The input files are located in

 ~davis/hpkb/eval1

For each problem, there are two KE files; one with just the isas
for all the terms, and one with the rest of the content.
For example, for problem 9 we have :

  ~davis/hpkb/eval1/T9n-isas.ke
  ~davis/hpkb/eval1/T9in

Load these in order via "Compose" in the HTML interface.
This will populate an initial MT such as #$WAMT-EvalInitial-P9

(3) Setup a problem mt from the initial mt.  The way to do this is to
execute in the SubL interactor :

  (sim-wa-setup-problem-mt #$WAMT-EvalInitial-P9)

This will create a new problem microtheory called #$WAMT-EvalProblem-P9,
and project as much as possible into it.  It also sets up the problem
mts such as #$WABridgintMt etc. and determines the #$interdictedUnit and
#$obstacle and #$availableMateriel

This step may take awhile.  It takes about 10 minutes on a fast machine, 
so it may take a half hour on one of the slower PCs.

As a result, I ususally don't execute this step in the HTML interactor.
I drop down to the lower-level Readloop in emacs.  You can do this via

  "Click _here_ to drop to Readloop"

on the "System" page.

After running the setup stage, you can return to HTML services by executing

  (wait-with-agenda)

in the Readloop.

(4) You are now ready to run the planner on your problem.  

To do this, go to the "Tools" page and choose "Planner".  I suggest
putting this link in your toolbar since you'll be using it a lot.  

FYI -- There's also a "Readloop" tool for dropping to the Readloop
which you can also include in your tool bar.

The HTML interface to the planner allows you to only run one plan
at a time.  When there's no current plan, you are prompted with the name
of the problem mt.  Enter

  WAMT-EvalProblem-P9

The search parameters allow for cutoffs on number of successful options, 
time and total action depth down any search path.  I suggest providing
a depth cutoff of from 6 - 10 to prevent any unforseen looping of actions
from running away infinitely.

You can now start the search.  Go get coffee.

Your emacs should display lots of messages as the planner grinds away
on your problem.

(5) Eventually, the planner will halt and display its results.  If
your netscape times out, you can choose "Planner" again from the
"Tools" page to view the state of the last search.

Browse around.

(6) If you find a node that looks like the wrong thing happened, and you
fix the KR involved and want to re-try the planner from that node, you
can click on [Clear] where it says :

  Children : [Clear]

in order to clear all the chidren of this node.  Then, you can click on

  [Reconsider]

at the top of the page for the node to re-insert this node into the search.

Finally, to actually continue the search, you can click on

  [Action Planner Search]

to go to the main page for the search.  If there are nodes still to be
considered, you are given the option to continue the search.  Again,
choosing a depth parameter here is wise.

This is in 3 steps to provide modularity
  Step 1 clears all the bookkeeping to make it look like a leaf node
  Step 2 makes it look like the leaf node just showed up in the search
  Step 3 actually continues the search

(7) If you find you want to completely clear away the entire search and
start over, you can click on

  [Clear]

from the [Action Planner Search].  This will allow you to restart the
seach from scratch.


|#
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference switches used
;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
 (csetq *suspend-sbhl-type-checking?* t)
 (csetq *forward-propagate-from-negations* nil)
 (csetq *at-pred-constraints* '(:irreflexive-predicate))
 (csetq *type-filter-forward-dnf* t)
 (csetq *simplify-literal?* nil)
 )

;;;;;;;;;;;;;;;;;;;
;; global variables
;;;;;;;;;;;;;;;;;;;

(defparameter *sim-agent-planner-depth-cutoff* nil)

(defparameter
    *sim-ap-quantity-comparators*
    '(#$greaterThan #$greaterThanOrEqualTo #$followingValue))

(defparameter
    *sim-ap-fixed-predicates*
    '(#$isa #$bordersOn))

(defparameter *sim-ap-goal-postprocessing-func* nil)

;;;;;;;;;;
;; tracing
;;;;;;;;;;

(defparameter *sim-ap-trace-enabled* t)

(defmacro sim-ap-trace (format-string &rest args)
  (ret
   `(pwhen *sim-ap-trace-enabled*
      (progn (format t ,format-string ,@args)(force-output)))))



;;;;;;;;;;;;;;;;;
;; search methods
;;;;;;;;;;;;;;;;;

;; breadth-first search for simplest plan
;; use queue for outstanding leaves

;; no-leaves-p-func ; leaves -> [boolean]
(define sim-agent-planner-no-leaves-p (leaves)
  (ret (queue-empty-p leaves)))

;; next-node-func   ; leaves -> node, leaves
(define sim-agent-planner-next-node (leaves)
  (clet ((next-node  (dequeue leaves))
         (new-leaves leaves))
    (ret (values next-node new-leaves))))

;; add-node-func    ; node leaves -> leaves
(define sim-agent-planner-add-node (node leaves)
  (clet ((new-leaves (enqueue node leaves)))
    (ret new-leaves)))



;; goal-p-func      ; node -> [boolean]
(define sim-agent-planner-goal-p (node)
  (ret (sim-agent-planner-goal-p-guts node)))

;; add-goal-func    ; node goals -> goals [boolean]
(define sim-agent-planner-add-goal (node goals)
  (pwhen *sim-ap-goal-postprocessing-func*
    (funcall *sim-ap-goal-postprocessing-func* node))
  (ret (cons node goals)))


;; options-func     ; node -> [list of options]
(define sim-agent-planner-options (node)
  (ret (sim-agent-planner-options-guts node)))

;; expand-func      ; node option -> [list of nodes]
(define sim-agent-planner-expand (node option)
  (ret (sim-agent-planner-expand-guts node option)))

;; too-deep-func    ; node depth-cut -> [boolean]
(define sim-agent-planner-too-deep (node depth-cut)
  (ret (sim-agent-planner-too-deep-guts node depth-cut)))




;;;;;;;;;;;;;;;
;; search setup
;;;;;;;;;;;;;;;

(define sim-agent-planner-initialization (search)
  (check-type search search-struc-p)
  (csetf (search-goals search) nil)
  (csetf (search-leaves search) (create-queue))
  (ret nil))

(define print-sim-agent-planner-search (object stream depth)
  "Print method for sim agent planner searches"
  (ignore depth)
  (format stream "#<SEARCH (sim agent planner)>")
  (ret object))

(define new-sim-agent-planner-search (problem-mt input-mt action-type)
  (clet ((state (sim-agent-planner-search-state
                 problem-mt input-mt action-type))
         search)
    (csetq search
      (new-search
       'sim-agent-planner-no-leaves-p
       'sim-agent-planner-next-node
       'sim-agent-planner-goal-p
       'sim-agent-planner-add-goal
       'sim-agent-planner-options
       'sim-agent-planner-expand
       'sim-agent-planner-add-node
       'sim-agent-planner-too-deep
       state
       'print-sim-agent-planner-search))
    (sim-agent-planner-initialization search)
    (ret search)))

(define add-sim-agent-planner-search-start-nodes (search problem-mt action-type goal-formula &optional postcondition)
  (clet ((expanded-action-types
          (sim-ap-expanded-action-types action-type problem-mt))
         (case 0))
    (cdolist (expanded-action-type expanded-action-types)
      (cinc case)
      (add-sim-agent-planner-search-start-node
       search problem-mt case expanded-action-type goal-formula postcondition))
    (csetf (search-tree search)
      (nreverse (search-tree search))))
  (ret nil))

(define add-sim-agent-planner-search-start-node (search problem-mt case action-type goal-formula &optional postcondition)
  (clet ((modified-goal (third (subst action-type '?ACTION-TYPE goal-formula)))
         (next-action-id (sim-ap-get-next-action-id search)))
    (cmultiple-value-bind (action-mt substituted-postcondition)
      (sim-ap-generate-action-mt problem-mt next-action-id modified-goal action-type postcondition)
      (pwhen (constant-p action-mt)
        (clet ((state (sim-ap-initial-state case next-action-id action-mt substituted-postcondition)))
          ;; (break "add-sim-agent-planner-search-start-node")
          (add-search-start-node search state)
          (ret t)))))
  (ret nil))

(define new-sim-ap-search-node (state)
  (clet ((child (new-search-node state)))
    (ret child)))



;;;;;;;;;;;;;;;;
;; search states
;;;;;;;;;;;;;;;;

(define sim-agent-planner-search-state (problem-mt input-mt action-type)
  (clet ((state
          (list
           :problem-mt problem-mt
           :input-mt input-mt
           :action-type action-type
           :total-actions 0)))
    (ret state)))

(defmacro create-sim-ap-search-node-state
    (&key via 
      case case-post-mts case-precondition
      action-spec action-type postcondition
      action-id action-mt action-post-mt
      after-mt
      absolute-preconditions preconditions)
  (ret
   `(list
     ,@(fwhen via
         `(:via ,via))
     ,@(fwhen case
         `(:case ,case))
     ,@(fwhen case-post-mts
         `(:case-post-mts ,case-post-mts))
     ,@(fwhen case-precondition
         `(:case-precondition ,case-precondition))
     ,@(fwhen action-spec
         `(:action-spec ,action-spec))
     ,@(fwhen action-type
         `(:action-type ,action-type))
     ,@(fwhen postcondition
         `(:postcondition ,postcondition))
     ,@(fwhen action-id
         `(:action-id ,action-id))
     ,@(fwhen action-mt
         `(:action-mt ,action-mt))
     ,@(fwhen action-post-mt
         `(:action-post-mt ,action-post-mt))
     ,@(fwhen after-mt
         `(:after-mt ,after-mt))
     ,@(fwhen absolute-preconditions
         `(:absolute-preconditions ,absolute-preconditions))
     ,@(fwhen preconditions
         `(:preconditions ,preconditions))
     )))

(define sim-ap-initial-state (case action-id action-mt postcondition)
  (clet ((via '(:start))
         (new-state
          (create-sim-ap-search-node-state
           :case (princ-to-string case)
           :via via
           :action-id action-id
           :action-mt action-mt
           :postcondition postcondition)))
    (ret new-state)))

;; search properties

(define sim-ap-input-mt (node)
  (ret (getf (search-state (snode-search node)) :input-mt)))

(define sim-ap-problem-mt (node)
  (ret (getf (search-state (snode-search node)) :problem-mt)))

(define sim-ap-get-next-action-id (search)
  (clet ((old-state (search-state search))
         (action-id (getf old-state :total-actions 0)))
    (cinc action-id)
    (putf old-state :total-actions action-id)
    (ret action-id)))

;; search node properties

(define sim-ap-node-state (node)
  (ret (snode-state node)))

(define set-sim-ap-node-state (node state)
  (csetf (snode-state node) state)
  (ret state))


(define sim-ap-node-prop (node prop &optional value)
  (ret (getf (sim-ap-node-state node) prop value)))

(define set-sim-ap-node-prop (node prop value)
  (clet ((old-state (sim-ap-node-state node))
         new-state)
    (pif value
      (csetq new-state (putf old-state prop value))
      (csetq new-state (remf old-state prop)))
    (punless (eq old-state new-state)
      (set-sim-ap-node-state node new-state)))
  (ret value))

(define sim-ap-case (node)
  (ret (sim-ap-node-prop node :case)))

(define sim-ap-case-post-mts (node)
  (ret (sim-ap-node-prop node :case-post-mts)))

(define sim-ap-case-precondition (node)
  (ret (sim-ap-node-prop node :case-precondition)))

(define sim-ap-absolute-preconditions (node)
  (ret (sim-ap-node-prop node :absolute-preconditions)))

(define sim-ap-preconditions (node)
  (ret (sim-ap-node-prop node :preconditions)))

(define sim-ap-action-spec (node)
  (ret (sim-ap-node-prop node :action-spec)))

(define sim-ap-action-type (node)
  (ret (sim-ap-node-prop node :action-type)))

(define sim-ap-action-mt (node)
  (ret (sim-ap-node-prop node :action-mt)))

(define sim-ap-after-mt (node)
  (ret (sim-ap-node-prop node :after-mt)))

(define sim-ap-action-mt-action (action-mt)
  (ret (fpred-value-in-any-mt action-mt #$mtTime)))

(define sim-ap-action-mt-action-type (action-mt)
  (clet ((action (sim-ap-action-mt-action action-mt)))
    (pwhen action
      (ret (fpred-value-in-mt action #$isa action-mt)))))

(define sim-ap-action-mt-post-mt (action-mt)
  (ret (fpred-value-in-any-mt action-mt #$contiguousAfterMt 2 1)))

(define sim-ap-parent-post-mt (post-mt problem-mt)
  (pif (pred-u-v-holds-in-mt #$genlMt post-mt problem-mt #$BaseKB)
    (ret post-mt)
    (clet ((parent-post-mt (fpred-value-in-mt post-mt #$genlMt #$BaseKB)))
      (ret (sim-ap-parent-post-mt parent-post-mt problem-mt)))))

(define sim-ap-post-mt-action-mt (post-mt problem-mt)
  (csetq post-mt (sim-ap-parent-post-mt post-mt problem-mt))
  (ret (fpred-value-in-any-mt post-mt #$contiguousAfterMt 1 2)))

(define sim-ap-action-id (node)
  (ret (sim-ap-node-prop node :action-id)))

(define sim-ap-postcondition (node)
  (ret (sim-ap-node-prop node :postcondition)))

(define sim-ap-action-post-mt (node)
  (ret (sim-ap-node-prop node :action-post-mt)))

(define sim-ap-failure-reason (node)
  (ret (sim-ap-node-prop node :failure-reason)))

(define sim-ap-note-search-failure (node failure-reason)
  (set-sim-ap-node-prop node :failure-reason failure-reason)
  (ret node))

;;;;;;;;;;;;;;;;;;;
;; ancestor walking
;;;;;;;;;;;;;;;;;;;

(define sim-ap-action-depth (node)
  (pif (null node)
    (ret 0)
    (clet ((parent-action-depth
            (sim-ap-action-depth (snode-parent node)))
           (action-mt (sim-ap-action-mt node)))
      (ret (+ (fif action-mt 1 0)
             parent-action-depth)))))

(define sim-ap-ancestor-action-types (node)
  (pwhen (null node) (ret nil))
  (clet ((parent-action-types
          (sim-ap-ancestor-action-types (snode-parent node)))
         (action-mt (sim-ap-action-mt node)))
    (pif action-mt
      (ret (cons (sim-ap-action-mt-action-type action-mt)
             parent-action-types))
      (ret parent-action-types))))

(define sim-ap-ancestor-case (node)
  (pwhen (null node) (ret ""))
  (clet ((case (sim-ap-case node)))
    (pif case
      (ret case)
      (ret (sim-ap-ancestor-case
            (snode-parent node))))))

(define sim-ap-ancestor-case-post-mts (node)
  (pwhen (null node) (ret nil))
  (clet ((post-mts (sim-ap-case-post-mts node)))
    (pif post-mts
      (ret post-mts)
      (ret (sim-ap-ancestor-case-post-mts
            (snode-parent node))))))



;;;;;;;;;;;;;;;
;; planner guts
;;;;;;;;;;;;;;;

(define sim-agent-planner-goal-p-guts (node)
  ;; nothing left to do = SUCCESS!
  (ret (cand (null (sim-ap-case-precondition node))
         (null (sim-ap-action-spec node))
         (null (sim-ap-action-mt node))
         (null (sim-ap-absolute-preconditions node))
         (null (sim-ap-preconditions node)))))

(define sim-agent-planner-options-guts (node)
  (pcond
   ((sim-ap-case-precondition node)
    (ret '(:case-precondition-expand)))
   ((sim-ap-action-spec node)
    (ret '(:action-spec-expand)))
   ((sim-ap-action-mt node)
    (ret '(:action-mt-expand)))
   ((sim-ap-absolute-preconditions node)
    (ret '(:absolute-preconditions-expand)))
   ((sim-ap-preconditions node)
    (ret '(:preconditions-expand)))
   (t
    ;; should never get here
    (ret nil))))

(define sim-agent-planner-too-deep-guts (node depth-cut)
  (ret (cand depth-cut
         (>= (sim-ap-action-depth node) depth-cut))))

(define sim-agent-planner-expand-guts (node option)
  (pcase option
    (:case-precondition-expand
     (ret (sim-agent-planner-expand-case-precondition node)))
    (:action-spec-expand
     (ret (sim-agent-planner-expand-action-spec node)))
    (:action-mt-expand
     (ret (sim-agent-planner-expand-action-mt node)))
    (:absolute-preconditions-expand
     (ret (sim-agent-planner-expand-absolute-preconditions node)))
    (:preconditions-expand
     (ret (sim-agent-planner-expand-preconditions node)))
    (otherwise
     ;; should never get here
     (ret nil))))

(define sim-agent-planner-expand-case-precondition (node)
  (clet ((case-precondition (sim-ap-case-precondition node)))
    (cdestructuring-bind (precondition action-spec) case-precondition
      (cdestructuring-bind (formula introduced-by-mt) precondition
        (ignore formula)
        (cdestructuring-bind
         (hypothesis-formula action-type postcondition modified-preconditions)
         action-spec
         (clet ((remaining-preconditions (sim-ap-preconditions node))
                substituted-preconditions)
           (cdolist (modified-precondition modified-preconditions)
             (cpush (list modified-precondition introduced-by-mt)
               substituted-preconditions))
           (csetq substituted-preconditions
             (nreverse substituted-preconditions))
           (clet ((new-preconditions
                   (append substituted-preconditions
                     remaining-preconditions))
                  (unchangeable-preconditions
                   (remove-if-not #'sim-ap-unchangeable-precondition new-preconditions))
                  (new-absolute-preconditions
                   (mapcar #'first unchangeable-preconditions))
                  (final-new-preconditions
                   (set-difference new-preconditions unchangeable-preconditions))
                  (via `(:action-spec))
                  (new-state
                   (create-sim-ap-search-node-state
                    :via via
                    :absolute-preconditions new-absolute-preconditions
                    :action-spec hypothesis-formula
                    :action-type action-type
                    :postcondition postcondition
                    :preconditions final-new-preconditions))
                  (new-node (new-sim-ap-search-node new-state)))
             (ret (list new-node)))))))))

(define sim-agent-planner-expand-action-spec (node)
  (clet ((search (snode-search node))
         (action-id (sim-ap-get-next-action-id search))
         (action-spec (sim-ap-action-spec node))
         (action-type (sim-ap-action-type node))
         (postcondition (sim-ap-postcondition node))
         (problem-mt (sim-ap-problem-mt node)))
    (pwhen (cand action-id action-spec action-type postcondition problem-mt)
      (cmultiple-value-bind (action-mt substituted-postcondition)
        (sim-ap-generate-action-mt problem-mt action-id action-spec action-type postcondition)
        (pwhen action-mt
          (clet ((via `(:action-spec ,action-spec ,postcondition))
                 (inherited-absolute-preconditions
                  (copy-tree (sim-ap-absolute-preconditions node)))
                 (inherited-preconditions
                  (copy-tree (sim-ap-preconditions node)))
                 (new-state
                  (create-sim-ap-search-node-state
                   :via via
                   :action-id action-id
                   :action-mt action-mt
                   :postcondition substituted-postcondition
                   :absolute-preconditions inherited-absolute-preconditions
                   :preconditions inherited-preconditions))
                 (new-node (new-sim-ap-search-node new-state)))
            (sim-ap-trace "~%successful action hypothesized : ~S"
              action-mt)
            (ret (list new-node))))))
    ;; stub
    (clet ((failure-reason
            `(:hypothesize-action
              ,action-spec
              ,problem-mt)))
      (sim-ap-trace "~%failed to hypothesize action for ~S"
        action-spec)
      (sim-ap-note-search-failure
       node failure-reason)
      (ret nil))))

(define sim-agent-planner-expand-action-mt (node)
  (clet ((action-mt (sim-ap-action-mt node))
         (action (sim-ap-action-mt-action action-mt))
         (action-post-mt (sim-ap-generate-action-post-mt node))
         (case-post-mts (sim-ap-ancestor-case-post-mts node)))
    (pwhen action-post-mt
      (clet ((new-case-post-mts
              (cons action-post-mt case-post-mts))
             (inherited-absolute-preconditions
              (copy-tree (sim-ap-absolute-preconditions node)))
             (inherited-preconditions
              (copy-tree (sim-ap-preconditions node)))
             (absolute-preconditions
              (append inherited-absolute-preconditions
                (sim-ap-gather-absolute-preconditions action action-mt)))
             (new-preconditions
              (sim-ap-gather-preconditions action action-mt))
             (total-preconditions
              ;; this puts new ones at the end
              (union new-preconditions (reverse inherited-preconditions) #'equal)))
        (clet ((via `(:action-mt ,action-mt))
               (new-state
                (create-sim-ap-search-node-state
                 :via via
                 :case-post-mts new-case-post-mts
                 :absolute-preconditions absolute-preconditions
                 :preconditions total-preconditions
                 :action-post-mt action-post-mt))
               (new-node (new-sim-ap-search-node new-state)))
          (sim-ap-trace "~%expanding action mt ~S" action-mt)
          (ret (list new-node))))))
  (ret nil))

(define sim-agent-planner-expand-absolute-preconditions (node)
  ;; absolute preconditions must be true initially
  (clet ((input-mt (sim-ap-input-mt node)))
    (cdestructuring-bind (absolute-precondition . remaining-absolute-preconditions)
      (sim-ap-absolute-preconditions node)
      (clet ((absolute-justification
              (sim-ap-ask-absolute-precondition
               absolute-precondition input-mt)))
        (pwhen absolute-justification
          (clet ((remaining-preconditions (sim-ap-preconditions node))
                 (via `(:absolute-precondition
                        ,absolute-precondition
                        ,input-mt
                        ,absolute-justification))
                 (new-state
                  (create-sim-ap-search-node-state
                   :via via
                   :absolute-preconditions remaining-absolute-preconditions
                   :preconditions remaining-preconditions))
                 (new-node (new-sim-ap-search-node new-state)))
            (sim-ap-trace "~%successful absolute precondition :~% ~S~% justification :~%~S"
              absolute-precondition absolute-justification)
            (ret (list new-node)))))
      
      ;; SEARCH FAILS -- some absolute precondition fails
      (clet ((failure-reason
              `(:absolute-precondition
                ,absolute-precondition
                ,input-mt)))
        (sim-ap-trace "~%failed absolute precondition :~% ~S"
          absolute-precondition)
        (sim-ap-note-search-failure
         node failure-reason)
        (ret nil)))))

(define sim-agent-planner-expand-preconditions (node)
  ;; invariant -- no absolute preconditions for NODE
  (clet ((preconditions (sim-ap-preconditions node))
         (problem-mt (sim-ap-problem-mt node)))
    
    ;; true at the start
    (clet ((input-mt (sim-ap-input-mt node))
           (before-mt input-mt))
      (cdolist (precondition preconditions)
        (cdestructuring-bind (formula introduced-by-mt) precondition
          (clet ((justification
                  (sim-ap-ask-precondition
                   formula before-mt)))
            (pwhen justification
              (clet ((post-mt (sim-ap-action-mt-post-mt introduced-by-mt)))
                ;; project to the post mt
                (sim-ap-project-to-post-mt problem-mt before-mt post-mt)
                (clet ((remaining-preconditions (remove precondition preconditions))
                       (via `(:precondition
                              ,formula
                              ,before-mt
                              ,justification))
                       (after-mt `(,introduced-by-mt ,before-mt))
                       (new-state
                        (create-sim-ap-search-node-state
                         :via via
                         :after-mt after-mt
                         :preconditions remaining-preconditions))
                       (new-node (new-sim-ap-search-node new-state)))
                  (ret (list new-node)))))))))
    
    ;; serendipity -- already made true later
    (clet ((existing-post-mts (sim-ap-ancestor-case-post-mts node)))
      (cdolist (existing-post-mt existing-post-mts)
        (clet ((before-mt existing-post-mt))
          (cdolist (precondition preconditions)
            (cdestructuring-bind (formula introduced-by-mt) precondition
              (clet ((justification
                      (sim-ap-ask-precondition
                       formula before-mt)))
                (pwhen justification
                  (clet ((post-mt (sim-ap-action-mt-post-mt introduced-by-mt)))
                    ;; project to the post mt
                    (sim-ap-project-to-post-mt problem-mt before-mt post-mt)
                    (clet ((remaining-preconditions (remove precondition preconditions))
                           (via `(:precondition
                                  ,formula
                                  ,before-mt
                                  ,justification))
                           (after-mt `(,introduced-by-mt ,before-mt))
                           (new-state
                            (create-sim-ap-search-node-state
                             :via via
                             :after-mt after-mt
                             :preconditions remaining-preconditions))
                           (new-node (new-sim-ap-search-node new-state)))
                      (ret (list new-node)))))))))))
    
    ;; try expanding precondition for props
    (clet ((input-mt (sim-ap-input-mt node)))
      (cdolist (precondition preconditions)
        (cdestructuring-bind (formula introduced-by-mt) precondition
          (clet ((precondition-props-spec
                  (sim-ap-possible-precondition-props-spec formula input-mt)))
            (pwhen precondition-props-spec
              (cdestructuring-bind (new-precondition-formulas new-absolute-preconditions)
                precondition-props-spec
                (clet (new-preconditions)
                  (cdolist (new-formula new-precondition-formulas)
                    (cpush (list new-formula introduced-by-mt)
                      new-preconditions))
                  (csetq new-preconditions (nreverse new-preconditions))
                  (clet ((remaining-preconditions (remove precondition preconditions))
                         (final-preconditions
                          (append new-preconditions remaining-preconditions))
                         (via
                          `(:precondition-props
                            ,formula
                            ,precondition-props-spec))
                         (new-state
                          (create-sim-ap-search-node-state
                           :via via
                           :absolute-preconditions new-absolute-preconditions
                           :preconditions final-preconditions))
                         (new-node (new-sim-ap-search-node new-state)))
                    (ret (list new-node))))))))))
    
    ;; try to make a precondition true via an action
    (punless (cand *sim-agent-planner-depth-cutoff*
               (>= (sim-ap-action-depth node)
                 *sim-agent-planner-depth-cutoff*))
      (clet ((input-mt (sim-ap-input-mt node)))
        (cdolist (precondition preconditions)
          (cdestructuring-bind (formula introduced-by-mt) precondition
            (clet ((action-specs (sim-ap-possible-action-specs formula input-mt)))
              (pwhen action-specs
                (pif (singleton? action-specs)
                  (clet ((action-spec (first action-specs)))
                    (cdestructuring-bind
                     (hypothesis-formula action-type postcondition modified-preconditions)
                     action-spec
                     (clet ((remaining-preconditions (remove precondition preconditions))
                            substituted-preconditions)
                       (cdolist (modified-precondition modified-preconditions)
                         (cpush (list modified-precondition introduced-by-mt)
                           substituted-preconditions))
                       (csetq substituted-preconditions
                         (nreverse substituted-preconditions))
                       (clet ((new-preconditions
                               (append substituted-preconditions remaining-preconditions))
                              (unchangeable-preconditions
                               (remove-if-not #'sim-ap-unchangeable-precondition new-preconditions))
                              (new-absolute-preconditions
                               (mapcar #'first unchangeable-preconditions))
                              (final-new-preconditions
                               (set-difference new-preconditions unchangeable-preconditions))
                              (via `(:action-spec))
                              (new-state
                               (create-sim-ap-search-node-state
                                :via via
                                :action-spec hypothesis-formula
                                :action-type action-type
                                :postcondition postcondition
                                :absolute-preconditions new-absolute-preconditions
                                :preconditions final-new-preconditions))
                              (new-node (new-sim-ap-search-node new-state)))
                         (ret (list new-node))))))
                  (clet ((case (sim-ap-ancestor-case node))
                         (subcase-num 0)
                         new-nodes)
                    (cdolist (action-spec action-specs)
                      (cinc subcase-num)
                      (clet ((subcase (format nil "~A.~A" case subcase-num))
                             (new-case-post-mts
                              (sim-ap-create-new-case-post-mts
                               subcase
                               (sim-ap-ancestor-case-post-mts node)))
                             (case-precondition
                              (list precondition action-spec))
                             (remaining-preconditions (remove precondition preconditions))
                             (via `(:disjunctive-preconditions))
                             (new-state
                              (create-sim-ap-search-node-state
                               :via via
                               :case subcase
                               :case-post-mts new-case-post-mts
                               :case-precondition case-precondition
                               :preconditions remaining-preconditions))
                             (new-node (new-sim-ap-search-node new-state)))
                        (cpush new-node new-nodes)))
                    (ret (nreverse new-nodes))))))))))
    
    ;; SEARCH FAILS -- preconditions unsatisfiable
    (clet ((failure-reason
            `(:preconditions-unsatisfiable
              ,preconditions)))
      (sim-ap-note-search-failure
       node failure-reason))
    )
  (ret nil))

(define sim-ap-unchangeable-precondition (precondition)
  ;; stub
  (clet ((predicate (literal-predicate (first precondition))))
    (ret (cor (member predicate *sim-ap-fixed-predicates*)
           (member predicate *sim-ap-quantity-comparators*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanding action possibilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter
    *sim-ap-expand-action-formula*
    '(#$and
      (#$genls ?COL ?ACTION-TYPE)
      (#$isa ?COL #$CrossingOptionType)))

(define sim-ap-expanded-action-types (action-type mt)
  (sim-ap-trace "~%expanding action-type ~S" action-type)
  (clet ((formula
          (subst action-type '?ACTION-TYPE *sim-ap-expand-action-formula*))
         (answers (fi-ask-int formula mt)))
    (ret (mapcar #'cdar answers))))

;;;;;;;;;;;;;;;;;;;;
;; new case post mts
;;;;;;;;;;;;;;;;;;;;

(define sim-ap-create-new-case-post-mts (subcase old-case-post-mts)
  (clet (answer)
    (cdolist (old-case-post-mt old-case-post-mts)
      (clet ((suggested-prefix
              (sim-ap-suggest-new-post-mt-name old-case-post-mt subcase))
             (new-case-post-mt
              (hypothesize-spec-mt old-case-post-mt suggested-prefix)))
        (cpush new-case-post-mt answer)))
    (ret (nreverse answer))))

(define sim-ap-suggest-new-post-mt-name (old-case-post-mt subcase)
  (csetq subcase (substitute #\- #\. subcase))
  (clet ((name (constant-name old-case-post-mt))
         (start (length "HYP-Sim-AP-Post-"))
         (action-id
          (read-from-string
           name nil nil start
           (position-if-not #'digit-char-p name #'identity start)))
         (new-prefix (format nil "HYP-Sim-AP-Post-~A-Case-~A" action-id subcase)))
    (ret new-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preconditions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sim-ap-gather-absolute-preconditions (action action-mt)
  "Return a list of absolute preconditions for action.
objects returned are of precondition formulas"
  (sim-ap-trace "~%gathering absolute preconditions for ~S" action)
  (clet ((inference-answers
          (fi-ask-int
           `(#$absolute-PreconditionFor-PropSit ?FORMULA ,action)
           action-mt
           1 nil nil nil))
         answers)
    (pwhen inference-answers
      (cdolist (bindings inference-answers)
        (clet ((formula (cdar bindings)))
          (cpushnew formula answers #'equal))))
    (sim-ap-trace "~%absolute preconditions for ~S :~%~S"
      action answers)
    (ret answers)))

(define sim-ap-gather-preconditions (action action-mt)
  "Return a list of preconditions for action.
objects returned are of the form (precondition introduced-by-mt)"
  (sim-ap-trace "~%gathering preconditionFor-PropSit preconditions for ~S" action)
  ;; stub -- convert this to inference-recursive-ask-unique-bindings
  (clet ((inference-answers
          (fi-ask-int
           `(#$preconditionFor-PropSit ?FORMULA ,action)
           action-mt
           1 nil nil nil))
         answers)
    (pwhen inference-answers
      (cdolist (bindings inference-answers)
        (clet ((formula (cdar bindings))
               (introduced-by-mt action-mt))
          (punless (find formula answers #'equal #'first)
            (cpush (list formula introduced-by-mt) answers)))))
    (sim-ap-trace "~%preconditionFor-PropSit preconditions for ~S :~%~S"
      action answers)
    (ret answers)))

(define sim-ap-gather-precondition-for-props (formula mt &optional backchain)
  (sim-ap-trace "~%gathering preconditionFor-Props preconditions for ~S" formula)
  (clet ((inference-answers
          (fi-ask-int
           `(#$preconditionFor-Props ?PRE-FORMULA ,formula)
           mt
           backchain nil nil nil))
         answers)
    (pwhen inference-answers
      (cdolist (bindings inference-answers)
        (clet ((formula (cdar bindings)))
          (cpushnew formula answers #'equal))))
    (sim-ap-trace "~%prop preconditions for ~S :~%~S"
      formula answers)
    (ret answers)))

(define sim-ap-ask-precondition-for-props (formula mt &optional backchain)
  (sim-ap-trace "~%asking preconditionFor-Props preconditions for ~S" formula)
  (clet ((answers
          (fi-ask-int
           `(#$preconditionFor-Props ?PRE-FORMULA ,formula) mt
           backchain nil nil nil)))
    (ret answers)))

;;;;;;;;;;;;;;;;;;;;;;;
;; asking preconditions
;;;;;;;;;;;;;;;;;;;;;;;

(define sim-ap-ask-absolute-precondition (precondition mt &optional backchain)
  "Return a list of justifications for the absolute precondition"
  (sim-ap-trace "~%asking absolute precondition ~S in ~S"
    precondition mt)
  (ret (sim-ap-ask-precondition-internal precondition mt backchain)))

(define sim-ap-ask-precondition (precondition mt &optional backchain)
  "Return a list of justifications for the precondition"
  (sim-ap-trace "~%asking precondition ~S in ~S"
    precondition mt)
  (ret (sim-ap-ask-precondition-internal precondition mt backchain)))

(define sim-ap-ask-precondition-internal (precondition mt backchain)
  (clet ((answers
          (inference-recursive-ask
           precondition mt
           backchain nil nil nil)))
    (pwhen answers
      (csetq answers (mapcar #'second answers)))
    (ret answers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making preconditions hold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *experiment* nil)

(define sim-ap-possible-precondition-props-spec (precondition-formula mt)
  (pwhen *experiment*
    (ret (sim-ap-new-possible-precondition-props-spec precondition-formula mt)))
  (clet ((result-preconditions (sim-ap-gather-precondition-for-props precondition-formula mt 1)))
    (pwhen result-preconditions
      (clet ((new-absolute-preconditions
              (remove-if-not #'sim-ap-unchangeable-precondition result-preconditions))
             (new-preconditions
              (set-difference result-preconditions new-absolute-preconditions)))
        (ret (list new-preconditions new-absolute-preconditions)))))
  (ret nil))

(define sim-ap-new-possible-precondition-props-spec (precondition-formula mt)
  (clet ((formula precondition-formula)
         (bindings (sim-ap-ask-precondition-for-props formula mt 1)))
    (punless bindings
      (csetq formula (sim-ap-extract-precondition-sub-literal formula))
      (csetq bindings (sim-ap-ask-precondition-for-props formula mt 1)))
    (pwhen bindings
      (clet (answer)
        (csome (binding-set bindings answer)
          (clet ((substituted-formula
                  (sublis binding-set formula))
                 (introduced-precondition
                  (sublis binding-set '?PRE-FORMULA))
                 (modified-preconditions
                  (sim-ap-determine-modified-preconditions
                   binding-set precondition-formula formula))
                 (preserved-preconditions
                  (remove substituted-formula modified-preconditions #'equal))
                 (result-preconditions
                  (adjoin introduced-precondition preserved-preconditions #'equal))
                 (new-absolute-preconditions
                  (remove-if-not #'sim-ap-unchangeable-precondition result-preconditions))
                 (new-preconditions
                  (set-difference result-preconditions new-absolute-preconditions)))
            (csetq answer
              (list new-preconditions new-absolute-preconditions))))
        (ret answer)))))

(define sim-ap-possible-action-specs (precondition-formula mt)
  (clet ((formula precondition-formula)
         (bindings (sim-ap-ask-sit-results-in-prop formula mt 1)))
    (punless bindings
      (csetq formula (sim-ap-extract-precondition-sub-literal formula))
      (csetq bindings (sim-ap-ask-sit-results-in-prop formula mt 1)))
    (pwhen bindings
      (clet (action-specs)
        (cdolist (binding-set bindings)
          (clet ((hypothesis-formula (sublis binding-set '?ACTION-FORMULA))
                 (action-type (sublis binding-set '?ACTION-TYPE))
                 (postcondition (sublis binding-set formula))
                 (modified-preconditions
                  (sim-ap-determine-modified-preconditions
                   binding-set precondition-formula formula)))
            (cpush (list hypothesis-formula action-type postcondition modified-preconditions)
              action-specs)))
        (ret (nreverse action-specs))))))

(define sim-ap-ask-sit-results-in-prop (formula problem-mt &optional backchain)
  (clet ((predicate (literal-predicate formula))
         (answers
          ;; STUB convert to inference-recursive-ask-unique-bindings later
          (inference-recursive-ask
           `(#$sitResultsInProp
             ?ACTION-FORMULA ,formula ?ACTION-TYPE ,predicate)
           problem-mt
           backchain nil nil nil)))
    (pwhen answers
      (csetq answers
        (delete-duplicates
         (mapcar #'first answers) #'equal)))
    (ret answers)))

(define sim-ap-extract-precondition-sub-literal (formula)
  (ret (first (sim-ap-extract-precondition-sub-literals formula))))

(define sim-ap-extract-precondition-sub-literals (formula)
  (punless (consp formula)
    (ret nil))
  (clet ((operator (car formula)))
    (pcase operator
      (#$thereExists
       (ret (sim-ap-extract-precondition-sub-literals (third formula))))
      (#$and
       (clet ((possible-sub-literals (copy-list (cdr formula))))
         (cdolist (predicate *sim-ap-fixed-predicates*)
           (csetq possible-sub-literals
             (delete predicate possible-sub-literals #'eql #'car)))
         (cdolist (quantity-comparator *sim-ap-quantity-comparators*)
           (csetq possible-sub-literals
             (delete quantity-comparator possible-sub-literals #'eql #'car)))
         (ret possible-sub-literals)))
      (otherwise
       (ret nil)))))

(define sim-ap-determine-modified-preconditions (binding-set precondition-formula formula)
  (pwhen (equal precondition-formula formula)
    (ret (list (sublis binding-set precondition-formula))))
  (clet ((substituted-precondition-formula
          (sublis binding-set precondition-formula))
         (sustituted-formula
          (sublis binding-set formula)))
    (csetq substituted-precondition-formula
      (sim-ap-simplify-subsituted-precondition
       substituted-precondition-formula
       sustituted-formula))
    (clet (substituted-precondition-formulas)
      (pif (eq (car substituted-precondition-formula) #$and)
        (csetq substituted-precondition-formulas
          (cdr substituted-precondition-formula))
        (csetq substituted-precondition-formulas
          (list substituted-precondition-formula)))
      (ret (cons sustituted-formula
             substituted-precondition-formulas)))))

(define sim-ap-simplify-subsituted-precondition (precondition remove-literal)
  (punless (consp precondition)
    (ret precondition))
  (clet ((operator (car precondition)))
    (pcase operator
      (#$thereExists
       (clet ((variable (second precondition))
              (sub-formula (third precondition)))
         (pif (cnot (el-variable? variable))
           (ret (sim-ap-simplify-subsituted-precondition
                 sub-formula remove-literal))
           (ret `(#$thereExists ,variable
                   ,(sim-ap-simplify-subsituted-precondition
                     sub-formula remove-literal))))))
      (#$and
       (clet ((possible-sub-literals (copy-list (cdr precondition))))
         (csetq possible-sub-literals
           (remove remove-literal possible-sub-literals #'equal))
         (pif (cdr possible-sub-literals)
           (ret `(#$and ,@possible-sub-literals))
           (ret (car possible-sub-literals)))))
      (otherwise
       (ret precondition)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; generating action mt
;;;;;;;;;;;;;;;;;;;;;;;

(define sim-ap-generate-action-mt (problem-mt action-id formula action-type postcondition)
  (clet ((action-mt-name-template (format nil "HYP-Sim-AP-Action-~A" action-id))
         (action-mt (hypothesize-spec-mt problem-mt action-mt-name-template)))
    (pwhen action-mt
      (clet ((simplified-formula
              (sim-ap-simplify-hypothesis-formula formula))
             (action-bindings
              (hypothesize-terms simplified-formula action-mt)))
        (pwhen action-bindings
          (clet ((action (sim-ap-identify-action-in-action-mt
                          action-type action-bindings action-mt)))
            (pwhen (constant-p action)
              ;; link action and action mt
              (fi-assert-int
               `(#$mtTime ,action-mt ,action) action-mt
               :monotonic)
              ;; compute postcondition
              (clet ((substituted-postcondition
                      (sublis action-bindings postcondition)))
                (ret (values action-mt substituted-postcondition)))))))))
  (ret (values nil nil)))

(define sim-ap-identify-action-in-action-mt (action-type action-bindings action-mt)
  (cdolist (action-binding action-bindings)
    (clet ((value (variable-binding-value action-binding)))
      (pwhen (cand (constant-p value)
               (fi-ask-int `(#$isa ,value ,action-type) action-mt))
        (ret value))))
  (ret nil))

(define sim-ap-simplify-hypothesis-formula (formula)
  (pcond
   ((atom formula)
    (ret formula))
   ((eq (car formula) #$thereExists)
    (ret (sim-ap-simplify-hypothesis-formula
          (third formula))))
   (t
    (ret formula))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generating action post mt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sim-ap-generate-action-post-mt (node)
  (clet ((action-mt (sim-ap-action-mt node))
         (action-id (sim-ap-action-id node))
         (postcondition (sim-ap-postcondition node))
         (problem-mt (sim-ap-problem-mt node))
         (post-mt-name-template (format nil "HYP-Sim-AP-Post-~A" action-id))
         (post-mt (hypothesize-spec-mt problem-mt post-mt-name-template)))
    ;; link post-mt to action-mt
    (fi-assert-int
     `(#$contiguousAfterMt ,post-mt ,action-mt) #$BaseKB
     :default :forward)
    ;; assert postcondition (if any) in post-mt
    (pwhen postcondition
      (fi-assert-int postcondition post-mt :monotonic))
    ;; project action-mt to problem mt
    (sim-ap-project-mt-contents problem-mt action-mt problem-mt)
    (ret post-mt)))

(define sim-ap-project-to-post-mt (problem-mt pre-mt post-mt)
  (clet ((all-pre-mts (sim-ap-all-pre-mts pre-mt problem-mt)))
    (cdolist (pre-mt all-pre-mts)
      (sim-ap-project-mt-contents problem-mt pre-mt post-mt)))
  (ret nil))

(define sim-ap-all-pre-mts (pre-mt problem-mt)
  (ret (set-difference (all-genl-mts pre-mt)
         (all-genl-mts problem-mt))))

(define sim-ap-project-mt-contents (above-mt from-mt to-mt)
  (sim-ap-trace "~%Projecting from ~S to ~S" from-mt to-mt)
  (clet ((from-mt-contents (gather-mt-index from-mt))
         (projection-support (sim-ap-projection-rule above-mt from-mt to-mt)))
    (cdolist (from-mt-assertion from-mt-contents)
      (pwhen (gaf-assertion? from-mt-assertion)
        (clet ((formula (assertion-fi-formula from-mt-assertion)))
          (pwhen (sim-ap-project-formula? formula from-mt to-mt)
            (clet ((supports (list projection-support from-mt-assertion)))
              (sim-ap-trace "~%  Projecting ~S" formula)
              (fi-add-argument-int formula to-mt supports)))))))
  (ret nil))

(define sim-ap-projection-rule (above-mt from-mt to-mt)
  (ret (make-el-support
        :code
        `(#$implies
          (#$ist ,from-mt ?FORMULA)
          (#$ist ,to-mt ?FORMULA))
        above-mt :default)))

(defparameter
    *sim-ap-reject-project-predicates*
    '(#$completeExtentEnumerable #$minimizeExtent
       #$absolute-PreconditionFor-PropSit
       #$preconditionFor-PropSit
       #$preconditionFor-Props))

(define sim-ap-project-formula? (literal from-mt to-mt)
  ;; stub
  (pwhen (cor (tree-find from-mt literal)
           (tree-find to-mt literal))
    (ret nil))
  (pwhen (fi-ask-int literal to-mt)
    (ret nil))
  (pwhen (fi-ask-int (negate literal) to-mt)
    (ret nil))
  (pwhen (negated? literal) (ret t))
  (clet ((predicate (literal-predicate literal)))
    (pwhen (member predicate *sim-ap-reject-project-predicates*)
      (ret nil))
    (pwhen (pred-u-v-holds-in-mt #$functionalInArgs predicate 2 to-mt)
      (pwhen (fpred-value-in-mt (literal-arg1 literal) predicate to-mt)
        (ret nil))))
  (ret t))

;;;;;;;;;;;;;;;;;;
;; output analysis
;;;;;;;;;;;;;;;;;;

(define sim-ap-node-ancestor? (node ancestor)
  (pwhen (null node) (ret nil))
  (pwhen (eq node ancestor) (ret t))
  (ret (sim-ap-node-ancestor? (snode-parent node) ancestor)))

(define sim-ap-all-children-decided? (node)
  (clet ((search (snode-search node))
         (leaves (search-leaves search))
         (leaf-list (queue-elements leaves)))
    (cdolist (leaf leaf-list)
      (pwhen (sim-ap-node-ancestor? leaf node)
        (ret nil)))
    (ret t)))

(define sim-ap-search-decided (search)
  (ret (queue-empty-p (search-leaves search))))

(define sim-ap-some-goal-descendent? (node)
  (pif (null node)
    (ret nil)
    (pif (sim-agent-planner-goal-p node)
      (ret t)
      (cdolist (child (snode-children node))
        (pwhen (sim-ap-some-goal-descendent? child)
          (ret t))))))

(define sim-ap-all-children-infeasible? (node)
  "a node is infeasible if it is decided and there are no goals."
  (ret (cand (sim-ap-all-children-decided? node)
         (cnot (sim-ap-some-goal-descendent? node)))))

(define sim-ap-search-infeasible (search)
  (punless (sim-ap-search-decided search)
    (ret nil))
  (clet ((nodes (search-tree search)))
    (cdolist (node nodes)
      (pwhen (sim-ap-some-goal-descendent? node)
        (ret nil)))
    (ret t)))

(define sim-ap-all-children-proven-infeasible? (node)
  "a node is proven infeasible if either
[a] has failed due to an absolute precondition
[b] it has children, and they are all proven infeasible"
  (clet ((failure-reason (sim-ap-failure-reason node)))
    (pwhen (cand failure-reason
             (eq (first failure-reason)
               :absolute-precondition))
      (ret t))
    (clet ((children (snode-children node)))
      (punless children
        (ret nil))
      (cdolist (child children)
        (punless (sim-ap-all-children-proven-infeasible? child)
          (ret nil)))
      (ret t))))

(define sim-ap-search-proven-infeasible (search)
  "a node is proven infeasible if all its root options are proven infeasible"
  (punless (sim-ap-search-decided search)
    (ret nil))
  (clet ((nodes (search-tree search)))
    (cdolist (node nodes)
      (punless (sim-ap-all-children-proven-infeasible? node)
        (ret nil)))
    (ret t)))

(format t "sim-agent-planner Loaded~%")

;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;
;; Copyright (c) 1998 Cycorp.  All rights reserved.
;;
;; sim-agent-planner-html.lisp
;;
;; Keith Goolsbey
;; 06/17/1998
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CYC")

(csetq *cvs-id* "$Id: sim-agent-planner-html.lisp,v 1.2 1999/07/08 00:31:10 dalberic Exp $")

(define-html-handler sim-ap-search (&optional args)
  (ignore args)
  (cb-simple-main-document ("Sim Agent Planner Search" :completion? t)
    (pif (cnot (search-struc-p *sim-ap-last-search*))
      (sim-ap-display-start-search)
      (html-dummy-form
       (pwhen *cb-script-mode*
         (cb-back-button)
         (html-indent 2))
       (sim-ap-display-search-state *sim-ap-last-search*))))
  (ret nil))

(define-cb-link-method :sim-ap-search (search &optional linktext)
  (ignore search)
  (punless linktext
    (csetq linktext "[Sim Agent Planner Search]"))
  (frame-link
   (html-princ "sim-ap-search")
   (html-princ linktext)
   :main)
  (ret nil))

(define-cb-link-method :current-sim-ap-search (&optional linktext)
  (punless linktext
    (csetq linktext "Plan"))
  (frame-link
   (html-princ "sim-ap-search")
   (html-princ linktext))
  (ret nil))

(declare-cb-tool
 :current-sim-ap-search
 "Sim Agent Planner" "Sim Agent Plan" "Current sim agent planner search")

(define current-sim-ap-search (&optional linktext)
  (cb-link :current-sim-ap-search linktext)
  (ret nil))

(define sim-ap-display-search-state (search)
  (clet ((state (search-state search)))
    (html-princ "Search : ")
    (cb-link :sim-ap-search search "[Update]")
    (html-indent 2)
    (cb-link :sim-ap-clear-search search "[Clear]")
    (html-indent 2)
    (cb-link :sim-ap-search-summary search "[Summary]")
    (html-indent 2)
    (cb-link :reload-sim-planner "[Reload Sim Agent Planner Code]")
    (html-hr)
    (html-fancy-table (:border 1)
      (cdo ((plist state (cddr plist))
            (indicator (first plist) (first plist))
            (value (second plist) (second plist)))
        ((null plist))
        (html-table-row
         (html-table-data
          (html-princ indicator))
         (html-table-data
          (sim-ap-display-property value)))))
    (html-hr)
    (html-princ "Search Roots : ")
    (html-fancy-table (:border 0)
      (clet ((roots (search-tree search)))
        (cdolist (root roots)
          (html-table-row
           (html-table-data
            (cb-link :sim-ap-search-node root))
           (html-table-data
            (html-indent 2))
           (html-table-data
            (pcond
             ((sim-ap-all-children-proven-infeasible? root)
              (html-strong
               (html-princ "Infeasible (absolutely)")))
             ((sim-ap-all-children-infeasible? root)
              (html-strong
               (html-princ "Infeasible")))
             ((sim-ap-some-goal-descendent? root)
              (html-strong
               (html-princ "Feasible")))
             (t
              (html-princ "Undetermined feasibility"))))))))
    (html-newline)
    (html-princ "Search Goals : ")
    (html-newline)
    (clet ((goals (search-goals search)))
      (pif (null goals)
        (pwhen (sim-ap-search-infeasible search)
          (html-strong
           (html-princ "All options are infeasible")))
        (cdolist (goal goals)
          (cb-link :sim-ap-search-node goal)
          (cb-link :sim-ap-goal-info-summary goal "[Goal Info]")
          (html-newline))))
    (clet ((leaves (queue-elements (search-leaves search))))
      (pwhen leaves
        (html-newline)
        (html-princ "Search Leaves : ")
        (html-newline)
        (cdolist (leaf leaves)
          (cb-link :sim-ap-search-node leaf)
          (html-indent 2))
        (sim-ap-display-continue-search))))
  (ret nil))

(define-html-handler sim-ap-search-node (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (cb-error "~S does not specify a search node" args)
      (cb-simple-main-document ("Sim Agent Planner Search Node" :completion? t)
        (html-dummy-form
         (pwhen *cb-script-mode*
           (cb-back-button)
           (html-indent 2))
         (html-princ "Node : ")
         (cb-link :sim-ap-search-node search-node)
         (html-indent 4)
         (html-princ "Search : ")
         (cb-link :sim-ap-search (snode-search search-node))
         (html-newline)
         (cb-link :sim-ap-clear-children search-node "[Clear Children]")
         (cb-link :sim-ap-rotate-preconditions search-node "[Rotate Preconditions]")
         (cb-link :sim-ap-reconsider-node search-node "[Reconsider]")
         (cb-link :sim-ap-pretend-goal search-node "[Pretend Goal]")
         (pwhen (cor (sim-ap-action-mt search-node)
                  (sim-ap-after-mt search-node))
           (cb-link :sim-ap-edit-after-mt search-node))
         (html-hr)
         (clet ((state (snode-state search-node)))
           (html-fancy-table (:border 1)
             (cdo ((plist state (cddr plist))
                   (indicator (first plist) (first plist))
                   (value (second plist) (second plist)))
               ((null plist))
               (html-table-row
                (html-table-data
                 (html-princ indicator))
                (html-table-data
                 (sim-ap-display-property value))))))
         (clet ((parent (snode-parent search-node)))
           (pwhen parent
             (html-hr)
             (html-princ "Ancestors : ")
             (html-newline)
             (sim-ap-display-snode-ancestors parent)))
         (clet ((children (snode-children search-node)))
           (pwhen children
             (html-hr)
             (html-princ "Children : ")
             (cb-link :sim-ap-clear-children search-node "[Clear]")
             (html-newline)
             (sim-ap-display-snode-children children)))))))
  (ret nil))

(define-cb-link-method :sim-ap-search-node (node)
  (check-type node search-node-p)
  (clet ((depth (snode-depth node))
         (spec (sim-ap-encode-search-node-spec node))
         (case (sim-ap-ancestor-case node))
         (action-mt (sim-ap-action-mt node))
         (action-type (fwhen action-mt (sim-ap-action-mt-action-type action-mt)))
         (action-type-string
          (fif action-type
            (suggest-string-from-fort-el-formula action-type)
            ""))
         (node-marker-type (fwhen (sim-ap-after-mt node) "A"))
         (linktext (format nil "[Node ~A ~A (~S)]"
                     action-type-string case depth)))
    (frame-link
     (html-format "sim-ap-search-node&~A" spec)
     (html-no-break
      (html-princ linktext)
      (pwhen node-marker-type
        (html-tt
         (html-font-color (:yellow)
           (html-princ node-marker-type)))))
     :self))
  (ret nil))

(define sim-ap-display-snode-ancestors (search-node)
  (pwhen search-node
    (clet ((parent (snode-parent search-node)))
      (sim-ap-display-snode-ancestors parent)
      (html-indent (* 2 (snode-depth search-node)))
      (cb-link :sim-ap-search-node search-node)
      (html-newline)))
  (ret nil))

(define sim-ap-display-snode-children (children &optional (depth 0))
  (cdolist (child children)
    (html-indent (* depth 2))
    (cb-link :sim-ap-search-node child)
    (html-newline)
    (clet ((descendents (snode-children child)))
      (pwhen descendents
        (sim-ap-display-snode-children descendents (+ depth 1)))))
  (ret nil))

(define sim-ap-encode-search-node-spec (node)
  (clet ((parent (snode-parent node)))
    (pif (null parent)
      (clet ((search (snode-search node))
             (roots (search-tree search))
             (id (position node roots)))
        (ret (format nil "~A" id)))
      (clet ((siblings (snode-children parent))
             (id (position node siblings))
             (parent-spec (sim-ap-encode-search-node-spec parent)))
        (ret (format nil "~A&~A" parent-spec id))))))

(define sim-ap-decode-search-node-spec (args)
  (clet ((children (search-tree *sim-ap-last-search*))
         ancestor)
    (cdolist (arg args)
      (punless children	(ret nil))
      (clet ((id (read-from-string arg)))
        (csetq ancestor (nth id children))
        (punless ancestor (ret nil))
        (csetq children (snode-children ancestor))))
    (ret ancestor)))

(define sim-ap-display-property (value)
  (pif (atom value)
    (cb-form value)
    (progn
     (html-princ "(")
     (cdolist (item value)
       (cb-form item 0 t)
       (html-newline))
     (html-princ ")")))
  (ret nil))


;;;;;;;;;;;;;;;;
;; Editing links
;;;;;;;;;;;;;;;;

(define-html-handler sim-ap-handle-clear-search (&optional args)
  (ignore args)
  (pif (cnot *sim-ap-last-search*)
    (ret (cb-error "There is no current sim agent planner search."))
    (progn
     (sim-ap-clear-search *sim-ap-last-search*)
     (csetq *sim-ap-last-search* nil)
     (ret (sim-ap-search args)))))

(define-cb-link-method :sim-ap-clear-search (search &optional linktext)
  (ignore search)
  (punless linktext
    (csetq linktext "[Clear Search]"))
  (frame-link
   (html-princ "sim-ap-handle-clear-search")
   (html-princ linktext)
   :self)
  (ret nil))

(define-html-handler sim-ap-handle-clear-children (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (ret (cb-error "~S does not specify a search node" args))
      (progn
       (sim-ap-clear-children search-node)
       (ret (cb-simple-message-page "Nodes children have been cleared"))))))

(define-cb-link-method :sim-ap-clear-children (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (csetq linktext "[Clear Children]"))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-handle-clear-children&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))

(define-html-handler sim-ap-handle-reconsider-node (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (ret (cb-error "~S does not specify a search node" args))
      (progn
       (sim-ap-reconsider-node search-node)
       (ret (cb-simple-message-page "Node will be reconsidered"))))))

(define-cb-link-method :sim-ap-reconsider-node (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (csetq linktext "[Reconsider Node]"))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-handle-reconsider-node&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))

(define-html-handler sim-ap-pretend-goal (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (ret (cb-error "~S does not specify a search node" args))
      (clet ((search (snode-search search-node))
             (goals (search-goals search)))
        (sim-ap-clear-children search-node)
        (csetf (search-goals search)
          (sim-agent-planner-add-goal search-node goals))
        (ret (cb-simple-message-page "Node has been marked as a goal"))))))

(define-cb-link-method :sim-ap-pretend-goal (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (csetq linktext "[Pretend Goal]"))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-pretend-goal&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))

(define-html-handler sim-ap-handle-rotate-preconditions (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (ret (cb-error "~S does not specify a search node" args))
      (progn
       (sim-ap-rotate-preconditions search-node)
       (ret (cb-simple-message-page "Preconditions have been rotated"))))))

(define-cb-link-method :sim-ap-rotate-preconditions (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (csetq linktext "[Rotate Preconditions]"))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-handle-rotate-preconditions&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))

(define sim-ap-rotate-preconditions (search-node)
  (clet ((preconditions (sim-ap-preconditions search-node)))
    (pwhen (cand preconditions
             (cnot (singleton? preconditions)))
      (clet ((last (last preconditions))
             (butlast (butlast preconditions)))
        (set-sim-ap-node-prop search-node :preconditions (append last butlast)))))
  (ret nil))

(define-html-handler sim-ap-edit-after-mt (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (pif (cnot (search-node-p search-node))
      (cb-error "~S does not specify a search node" args)
      (clet ((problem-mt (sim-ap-problem-mt search-node))
             (input-mt (sim-ap-input-mt search-node))
             (action-mt (sim-ap-action-mt search-node))
             (sim-ap-after-mt (sim-ap-after-mt search-node))
             edit? title action)
        (pif action-mt
          (csetq title "Add additional after mt")
          (progn
           (csetq edit? t)
           (csetq title "Edit after mt")
           (csetq action-mt (first sim-ap-after-mt))))
        (csetq action (sim-ap-action-mt-action action-mt))
        (cb-simple-main-document (title :completion? t)
          (cb-basic-form-handler ("sim-ap-handle-edit-after-mt")
            (html-hidden-input "search-node" (prin1-to-string args))
            (pwhen *cb-script-mode*
              (cb-back-button)
              (html-indent 2))
            (html-princ "Node : ")
            (cb-link :sim-ap-search-node search-node)
            (html-indent 2)
            (pif edit?
              (progn
               (html-submit-input "Edit After Mt")
               (html-newline 2)
               (html-princ "Edit the requirement that ")
               (cb-form action)
               (html-princ " occurs after action")
               (html-newline)
               (cb-form (second sim-ap-after-mt))
               (html-princ " to occurring after which other action : "))
              (progn
               (html-submit-input "Add After Mt")
               (html-newline 2)
               (html-princ "Add the additional requirement that ")
               (cb-form action)
               (html-princ " occurs after which other action : ")))
            (html-newline 2)
            (html-fancy-table ()
              ;; add input mt
              (clet ((post-mt input-mt)
                     (id (constant-id post-mt)))
                (html-table-row
                 (html-fancy-table-data (:colspan 4)
                   (html-hr)))
                (html-table-row
                 (html-table-data
                  (html-radio-input "post-mt" id nil))
                 (html-table-data 
                  (cb-form input-mt))
                 (html-fancy-table-data (:colspan 2)
                   (html-indent))))
              ;; add each case post mt
              (clet ((post-mts (reverse (sim-ap-ancestor-case-post-mts search-node))))
                (cdolist (post-mt post-mts)
                  (html-table-row
                   (html-fancy-table-data (:colspan 4)
                     (html-hr)))
                  (html-table-row
                   (clet ((action-mt (sim-ap-post-mt-action-mt post-mt problem-mt))
                          (action (sim-ap-action-mt-action action-mt))
                          (id (constant-id post-mt)))
                     (html-table-data
                      (html-radio-input "post-mt" id nil))
                     (html-table-data 
                      (cb-form action))
                     (html-table-data
                      (html-indent 2))
                     (html-table-data
                      (cb-form post-mt)
                      (html-newline)
                      (cb-form action-mt))))))))))))
  (ret nil))

(define-html-handler sim-ap-handle-edit-after-mt (args)
  ;; (ret (html-echo-args args))
  (clet ((post-mt-name (html-extract-input "post-mt" args))
         (post-mt (cb-guess-constant post-mt-name))
         (spec (html-extract-input "search-node" args))
         (search-node (sim-ap-decode-search-node-spec (read-from-string spec))))
    (punless (search-node-p search-node)
      (ret (cb-error "~S does not specify a search node" args)))
    (punless (constant-p post-mt)
      (ret (cb-error "~S does not specify a constant" post-mt-name)))
    (clet ((action-mt (sim-ap-action-mt search-node)))
      (punless action-mt
        (csetq action-mt (first (sim-ap-after-mt search-node))))
      (set-sim-ap-node-prop search-node :after-mt (list action-mt post-mt))
      (ret (cb-simple-message-page "Additional after-mt has been added")))))

(define-cb-link-method :sim-ap-edit-after-mt (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (pif (sim-ap-action-mt node)
      (csetq linktext "[Add After Mt]")
      (csetq linktext "[Edit After Mt]")))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-edit-after-mt&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))



;;;;;;;;;;;;;;;
;; search tools
;;;;;;;;;;;;;;;

;; starting a new search

(define html-start-sim-ap-planner-search (problem-mt &optional number time depth)
  (csetq *sim-ap-last-search*
    (sim-ap-planner-search problem-mt number time depth))
  (ret nil))

(define sim-ap-display-start-search ()
  (cb-basic-form-handler ("sim-ap-handle-start-search")
    (pwhen *cb-script-mode*
      (cb-back-button)
      (html-indent 2))
    (html-reset-input "Reset")
    (html-indent 2)
    (html-submit-input "Start a new Search")
    (html-newline)
    (html-strong                           
     (html-princ "Problem Mt : "))
    (html-text-input "problem-mt" "" 30)
    (html-indent)
    (html-complete-button "problem-mt" "Complete" #$Microtheory)
    (html-hr)
    (html-strong
     (html-princ "Optional search parameters :"))
    (html-fancy-table (:border 0)
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Number : "))
       (html-table-data
        (html-text-input "number" nil 3))
       (html-table-data
        (html-princ "solutions (empty for all)")))
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Time : "))
       (html-table-data
        (html-text-input "time" nil 3))
       (html-table-data
        (html-princ "seconds (empty for no limit)")))
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Action Depth : "))
       (html-table-data
        (html-text-input "depth" nil 3))
       (html-table-data
        (html-princ "maximum actions (empty for no limit)")))))
  (ret nil))

(define-html-handler sim-ap-handle-start-search (args)
  ;; (ret (html-echo-args args))
  (clet ((problem-mt-string (html-extract-input "problem-mt" args))
         (problem-mt (cb-guess-constant problem-mt-string))
         (number (html-extract-input "number" args))
         (time (html-extract-input "time" args))
         (depth (html-extract-input "depth" args)))
    (punless (constant-p problem-mt)
      (ret (cb-error "~S did not specify a constant." problem-mt-string)))
    (pwhen number
      (pif (equal number "")
        (csetq number nil)
        (csetq number (read-from-string number))))
    (pwhen time
      (pif (equal time "")
        (csetq time nil)
        (csetq time (read-from-string time))))
    (pwhen depth
      (pif (equal depth "")
        (csetq depth nil)
        (csetq depth (read-from-string depth))))
    (html-start-sim-ap-planner-search problem-mt number time depth)
    (sim-ap-search)
    (ret nil)))

;; continuing a new search

(define html-sim-ap-continue-planner-search (&optional number time depth)
  (ret (sim-ap-continue-planner-search *sim-ap-last-search* number time depth)))

(define sim-ap-display-continue-search ()
  (html-hr)
  (cb-basic-form-handler ("sim-ap-handle-continue-search")
    (html-reset-input "Reset")
    (html-strong
     (html-princ " Search Parameters :"))
    (html-indent 4)
    (html-submit-input "Continue Search")
    (html-fancy-table (:border 0)
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Number : "))
       (html-table-data
        (html-text-input "number" nil 3))
       (html-table-data
        (html-princ "solutions (empty for all)")))
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Time : "))
       (html-table-data
        (html-text-input "time" nil 3))
       (html-table-data
        (html-princ "seconds (empty for no limit)")))
      (html-table-row
       (html-fancy-table-data (:align :right)
         (html-princ "Action Depth : "))
       (html-table-data
        (html-text-input "depth" nil 3))
       (html-table-data
        (html-princ "maximum actions (empty for no limit)")))))
  (ret nil))

(define-html-handler sim-ap-handle-continue-search (args)
  ;; (ret (html-echo-args args))
  (clet ((number (html-extract-input "number" args))
         (time (html-extract-input "time" args))
         (depth (html-extract-input "depth" args)))
    (pwhen number
      (pif (equal number "")
        (csetq number nil)
        (csetq number (read-from-string number))))
    (pwhen time
      (pif (equal time "")
        (csetq time nil)
        (csetq time (read-from-string time))))
    (pwhen depth
      (pif (equal depth "")
        (csetq depth nil)
        (csetq depth (read-from-string depth))))
    (html-sim-ap-continue-planner-search number time depth)
    (sim-ap-search)
    (ret nil)))


;;;;;;;;;;	
;; utility
;;;;;;;;;;	


(define-html-handler reload-sim-planner (&optional args)
  (ignore args)
  (pwhen (fboundp 'reload-sim-planner)
    (eval '(reload-sim-planner))
    (ret (cb-simple-message-page "Sim Agent Planner code has been reloaded")))
  (ret (cb-error "Don't know how to reload the planner code")))

(define-cb-link-method :reload-sim-planner (&optional linktext)
  (punless linktext
    (csetq linktext "Reload Sim Planner"))
  (frame-link
   (html-princ "reload-sim-planner")
   (html-princ linktext)
   :self)
  (ret nil))


;;;;;;;;;;;;;;;;;;; summary info ;;;;;;;;;;;;;;;;;;;;;;

(define-html-handler sim-ap-search-summary (&optional args)
  (ignore args)
  (html-document
   (html-head
    (html-title
     (html-princ "Problem Answer")))
   (html-body
    (clet ((search *sim-ap-last-search*)
           (problem-mt (getf (search-state search) :problem-mt))
           (problem-id (sim-wa-extract-problem-id problem-mt)))
      (html-heading (1)
        (html-princ "Evaluation Problem ")
        (html-princ problem-id)
        (html-princ " Answer"))
      (cmultiple-value-bind (feasible-top-level-options cheapest-goal infeasible-top-level-nodes)
        (sim-ap-categorize-top-level-options search)
        (pwhen feasible-top-level-options
          (cdolist (feasible-top-level-option feasible-top-level-options)
            (cdestructuring-bind (top-level-node . goals) feasible-top-level-option
              (html-newline)
              (html-strong
               (html-princ "Feasible : ")
               (sim-ap-show-top-level-node top-level-node))
              (html-newline)
              (cdolist (goal goals)
                (sim-ap-show-goal-summary goal)
                (html-newline))))
          (pwhen cheapest-goal
            (html-newline)
            (html-strong
             (html-princ "Most likely option : "))
            (clet ((top-level-node (sim-ap-top-level-node cheapest-goal))
                   (subcase (sim-ap-ancestor-case cheapest-goal)))
              (sim-ap-show-top-level-node top-level-node)
              (html-princ ", Case ")
              (html-princ subcase))))
        (pwhen infeasible-top-level-nodes
          (html-newline)
          (cdolist (top-level-node infeasible-top-level-nodes)
            (html-newline)
            (html-strong
             (html-princ "Infeasible : "))
            (sim-ap-show-top-level-node top-level-node)))))))
  (ret nil))

(define-cb-link-method :sim-ap-search-summary (search &optional linktext)
  (ignore search)
  (punless linktext
    (csetq linktext "Summary"))
  (frame-link
   (html-princ "sim-ap-search-summary")
   (html-princ linktext)
   :self)
  (ret nil))

(define sim-ap-show-top-level-node (node)
  (clet ((action-mt (sim-ap-action-mt node))
         (action-type (sim-ap-action-mt-action-type action-mt))
         (display-string
          (suggest-string-from-fort-el-formula action-type)))
    (html-princ display-string))
  (ret nil))

(define sim-ap-show-goal-summary (goal-node)
  (clet ((goal-info (sim-ap-node-prop goal-node :goal-info))
         (minimum-duration (getf goal-info :minimum-duration))
         (path-capacity (getf goal-info :path-capacity))
         (resources (getf goal-info :total-resources))
         (subcase (sim-ap-ancestor-case goal-node)))
    (html-indent 2)
    (html-princ "Case ")
    (html-princ subcase)
    (html-newline)
    (html-indent 4)
    (html-princ "Duration : ")
    (pif minimum-duration
      (sim-ap-show-quantity minimum-duration)
      (html-princ "No actions required"))
    (html-newline)
    (html-indent 4)
    (html-princ "Path Capacity : ")
    (sim-ap-show-quantity path-capacity)
    (pwhen resources
      (html-newline)
      (html-indent 4)
      (html-princ "Resources : ")
      (cdolist (resource resources)
        (html-newline)
        (html-indent 6)
        (pif (atom resource)
          (progn
           (sim-ap-show-resource resource))
          (cdestructuring-bind (item properties) resource
            (sim-ap-show-resource item)
            (cdolist (property properties)
              (html-newline)
              (html-indent 8)
              (cdestructuring-bind (pred value) property
                (html-princ (constant-name pred))
                (html-princ " : ")
                (html-princ value))))))))
  (ret nil))

(define sim-ap-show-resource (item)
  (clet ((type (fpred-value-in-any-mt item #$isa))
         (display-string
          (suggest-string-from-fort-el-formula type)))
    (html-princ display-string)
    (ret nil)))

(define sim-ap-show-quantity (quantity)
  (pwhen (constant-p quantity)
    (ret (html-princ (constant-name quantity))))
  (cmultiple-value-bind (unit min max) (explode-interval quantity)
    (html-princ min)
    (html-indent)
    (pwhen (cnot (eql min max))
      (html-princ "to ")
      (html-princ max)
      (html-indent))
    (html-princ (constant-name unit)))
  (ret nil))

(define sim-ap-categorize-top-level-options (search)
  (clet ((goals (search-goals search))
         (top-level-nodes (search-tree search))
         feasible minimum-duration infeasible)
    (cdolist (goal goals)
      (clet ((top-level-option (sim-ap-top-level-node goal))
             (existing (assoc top-level-option feasible)))
        (punless existing
          (csetq existing (list top-level-option))
          (cpush existing feasible))
        (rplacd existing
          (cons goal (cdr existing)))))
    (cdolist (top-level-node top-level-nodes)
      (punless (assoc top-level-node feasible)
        (cpush top-level-node infeasible)))
    (cdolist (node-info feasible)
      (rplacd node-info
        (sort (cdr node-info) #'string< #'sim-ap-ancestor-case)))
    (csetq feasible
      (sort feasible #'string<  #'car-sim-ap-ancestor-case))
    (csetq infeasible
      (sort infeasible #'string< #'sim-ap-ancestor-case))
    (clet (best-duration best-goal)
      (cdolist (goal goals)
        (clet ((duration (getf (sim-ap-node-prop goal :goal-info) :minimum-duration)))
          (pif (eq goal (first goals))
            (csetq best-duration duration
              best-goal goal)
            (pwhen (cyc-greater-than best-duration duration)
              (csetq best-duration duration
                best-goal goal)))))
      (csetq minimum-duration best-goal))
    (ret (values feasible minimum-duration infeasible))))

(define car-sim-ap-ancestor-case (obj)
  (ret (sim-ap-ancestor-case (car obj))))


;;;;;;;;;;;; goal display ;;;;;;;;;;;;;;;;;;;;

(define-html-handler sim-ap-goal-info-summary (args)
  (clet ((search-node (sim-ap-decode-search-node-spec args)))
    (punless (search-node-p search-node)
      (ret (cb-error "~S does not specify a search node" args)))
    (html-document
     (html-head
      (html-title
       (html-princ "Feasible Option Answer")))
     (html-body
      (clet ((problem-mt (sim-ap-problem-mt search-node))
             (input-mt (sim-ap-input-mt search-node))
             (problem-id (sim-wa-extract-problem-id problem-mt))
             (goal-info (sim-ap-node-prop search-node :goal-info))
             (subcase (sim-ap-ancestor-case search-node)))
        (html-heading (1)
          (html-princ "Evaluation Problem ")
          (html-princ problem-id)
          (html-princ ", Case ")
          (html-princ subcase))
        (clet ((top-level-node (sim-ap-top-level-node search-node))
               (action-mt (sim-ap-action-mt top-level-node))
               (partial-order (getf goal-info :action-partial-order))
               (all-event-info (getf goal-info :action-information))
               (paths
                (sim-ap-reduce-partial-order-paths 
                 (sim-ap-expand-partial-order-paths input-mt action-mt partial-order))))
          (html-strong
           (sim-ap-show-top-level-node top-level-node))
          (html-newline 2)
          (clet ((event-id-map `((,action-mt . 0)))
                 events-seen)
            (html-bullet-list
             (cdolist (path paths)
               (html-list-item
                (clet ((event-list (cdr path)))
                  (cdolist (event event-list)
                    (punless (find event event-id-map #'eql #'car)
                      (cpush (cons event (length event-id-map)) event-id-map))
                    (punless (eq event (first event-list))
                      (html-princ " before "))
                    (html-strong
                     (sim-ap-show-event event event-id-map)))
                  (cdolist (event event-list)
                    (html-newline 2)
                    (punless (eq event action-mt)
                      (sim-ap-show-event event event-id-map)
                      (html-newline)
                      (pif (member event events-seen)
                        (progn
                         (html-indent 2)
                         (html-princ "SEE ABOVE"))
                        (progn
                         (cpush event events-seen)
                         (sim-ap-show-event-summary event all-event-info)))))))))))))))
  (ret nil))

(define-cb-link-method :sim-ap-goal-info-summary (node &optional linktext)
  (check-type node search-node-p)
  (punless linktext
    (csetq linktext "[Goal Info]"))
  (clet ((spec (sim-ap-encode-search-node-spec node)))
    (frame-link
     (html-format "sim-ap-goal-info-summary&~A" spec)
     (html-princ linktext)
     :self))
  (ret nil))


(define sim-ap-show-event (event event-id-map)
  (clet ((action-type
          (sim-ap-action-mt-action-type event))
         (id (cdr (assoc event event-id-map)))
         (display-string (suggest-string-from-fort-el-formula action-type)))
    (pif (eql id 0)
      (html-princ display-string)
      (html-format "~A-~A" display-string id)))
  (ret nil))

(define sim-ap-show-event-summary (event all-event-info)
  (clet ((event-info (assoc event all-event-info))
         (duration (fourth event-info))
         (resources (fifth event-info)))
    (html-indent 2)
    (html-princ "Duration : ")
    (sim-ap-show-quantity duration)
    (pwhen resources
      (html-newline)
      (html-indent 2)
      (html-princ "Resources : ")
      (cdolist (resource resources)
        (html-newline)
        (html-indent 4)
        (pif (atom resource)
          (progn
           (sim-ap-show-resource resource))
          (cdestructuring-bind (item properties) resource
            (sim-ap-show-resource item)
            (cdolist (property properties)
              (html-newline)
              (html-indent 6)
              (cdestructuring-bind (pred value) property
                (html-princ (constant-name pred))
                (html-princ " : ")
                (html-princ value))))))))
  (ret nil))

(format t "sim-agent-planner-html Loaded~%")

;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;
;; Copyright (c) 1998 Cycorp.  All rights reserved.
;;
;; workarounds.lisp
;;
;; Keith Goolsbey
;; 06/08/1998
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workarounds-specific planner problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external interface
;;

(in-package "CYC")

(csetq *cvs-id* "$Id: workarounds.lisp,v 1.2 1999/07/08 00:31:10 dalberic Exp $")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup workarounds problem mt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sim-wa-setup-problem-mt (input-mt)
  (check-type input-mt constant-p)
  (sim-ap-trace "~%Setting up problem mt from ~S" input-mt)
  (clet ((problem-mt (sim-wa-generate-problem-mt input-mt)))
    (check-type problem-mt constant-p)
    (sim-wa-splice-around-input-mt input-mt problem-mt)
    (sim-wa-add-additional-problem-genl-mts input-mt problem-mt)
    (clet ((variable-property-predicates
            (sim-wa-determine-variable-property-predicates
             problem-mt)))
      (sim-wa-populate-problem-mt
       input-mt problem-mt variable-property-predicates)
      (sim-wa-setup-problem-materiel input-mt problem-mt)
      ;(sim-ap-trace "~%DaleLink")
      (sim-wa-link-input-to-problem-mt input-mt problem-mt)
      ;(sim-ap-trace "~%DaleReprop")
      (sim-wa-repropagate-initial-mt input-mt)
      ;(sim-ap-trace "~%DaleInitSit")
      (sim-wa-note-initial-situation input-mt problem-mt)
      ;(sim-ap-trace "~%DaleBackInitSit")
      (ret problem-mt))))

(defparameter *sim-wa-problem-mt-key-string* "Problem-P")

(define sim-wa-extract-problem-id (problem-mt)
  (clet ((name (constant-name problem-mt))
         (pos (search *sim-wa-problem-mt-key-string* name))
         (problem-id (subseq name (+ pos (length *sim-wa-problem-mt-key-string*)))))
    (ret problem-id)))

(define sim-wa-generate-problem-mt (input-mt)
  (clet ((input-mt-name (copy-string (constant-name input-mt)))
         (problem-mt-name
          (replace-substring input-mt-name "Initial" "Problem"))
         (problem-mt
          (fi-find-or-create-int
           problem-mt-name)))
    (fi-assert-int `(#$isa ,problem-mt #$WorkaroundsManagementMicrotheory) #$BaseKB)
    (sim-ap-trace "~%Created problem mt ~S" problem-mt)
    (ret problem-mt)))

(define sim-wa-splice-around-input-mt (input-mt problem-mt)
  (sim-ap-trace "~%Splicing ~S around ~S" problem-mt input-mt)
  (clet ((input-genl-mts (pred-values-in-mt input-mt #$genlMt #$BaseKB)))
    (cdolist (input-genl-mt input-genl-mts)
      (sim-ap-trace "~%Splicing genlMt mt ~S" input-genl-mt)
      (fi-assert-int
       `(#$genlMt ,problem-mt ,input-genl-mt)
       #$BaseKB)))
  (ret nil))

(define sim-wa-link-input-to-problem-mt (input-mt problem-mt)
  (sim-ap-trace "~%Linking ~S under ~S" input-mt problem-mt)
  (fi-assert-int
   `(#$genlMt ,input-mt ,problem-mt)
   #$BaseKB)
  (ret nil))

(define sim-wa-note-initial-situation (input-mt problem-mt)
  (sim-ap-trace "~%Noting ~S is initial situation for problem ~S" input-mt problem-mt)
  (fi-assert-int
   `(#$initialSituation ,problem-mt ,input-mt)
   problem-mt)
  (ret nil))

(define sim-wa-add-additional-problem-genl-mts (input-mt problem-mt)
  (sim-ap-trace "~%Adding additional problem-specific genlMts to ~S" problem-mt)
  ;; mines are independent
  (pwhen (sim-wa-minefield-problem? input-mt)
    (sim-ap-trace "~%Adding #$WADeminingMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WADeminingMt)
     #$BaseKB))
  ;; fording is independent
  (pwhen (sim-wa-fording-problem? input-mt)
    (sim-ap-trace "~%Adding #$WAFordingMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WAFordingMt)
     #$BaseKB))
  (pcond
   ;; tunnels
   ((sim-wa-tunnel-problem? input-mt)
    (sim-ap-trace "~%Adding #$WATunnelsMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WATunnelsMt)
     #$BaseKB))
   ;; roads
   ((sim-wa-road-problem? input-mt)
    (sim-ap-trace "~%Adding #$WARoadMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WARoadMt)
     #$BaseKB))
   ;; destroyed bridge
   ;; because of crevice case, this is not needed
   ((sim-wa-destroyed-bridge-problem? input-mt)
    (sim-ap-trace "~%Adding #$WAEmplacingBridgeMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WAEmplacingBridgeMt)
     #$BaseKB))
   ;; damaged bridge
   ((sim-wa-damaged-bridge-problem? input-mt)
    (sim-ap-trace "~%Adding #$WAEmplacingBridgeMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WAEmplacingBridgeMt)
     #$BaseKB)
    (sim-ap-trace "~%Adding #$WAFixedBridgeMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WAFixedBridgeMt)
     #$BaseKB))
   ;; emplacing bridge over crevices
   ((sim-wa-crevice-emplacing-problem? input-mt)
    (sim-ap-trace "~%Adding #$WAEmplacingBridgeMt")
    (fi-assert-int
     `(#$genlMt ,problem-mt #$WAEmplacingBridgeMt)
     #$BaseKB)))
  (ret nil))

(define sim-wa-minefield-problem? (input-mt)
  (clet (mine-fields)
    ;; looking for (#$isa ?MINE-FIELD #$MineField) in input-mt
    (with-just-mt input-mt
      (csetq mine-fields (pred-values #$MineField #$isa 2 1)))
    (ret (boolean mine-fields))))

(define sim-wa-tunnel-problem? (input-mt)
  (clet (tunnels)
    ;; looking for (#$isa ?TUNNEL #$Tunnel) in input-mt
    (with-just-mt input-mt
      (csetq tunnels (pred-values #$Tunnel #$isa 2 1)))
    (ret (boolean tunnels))))

(define sim-wa-road-problem? (input-mt)
  (clet (roads)
    ;; looking for (#$isa ?ROAD #$Highway) in input-mt
    (with-just-mt input-mt
      (csetq roads (pred-values #$Highway #$isa 2 1)))
    (ret (boolean roads))))

(define sim-wa-fording-problem? (input-mt)
  (clet (rivers)
    ;; looking for (isa ?RIVER #$River) in input-mt
    (with-just-mt input-mt
      (csetq rivers (pred-values #$River #$isa 2 1)))
    ;; for these, looking for (isa ?RIVER #$Path-Customary) in input-mt
    (cdolist (river rivers)
      (pwhen (pred-u-v-holds-in-mt #$isa river #$Path-Customary input-mt)
        (ret t)))
    (ret nil)))

(defparameter
    *sim-wa-destroyed-bridge-problem-query*
    '(#$thereExists ?BRIDGE
       (#$thereExists ?DAMAGE
         (#$thereExists ?BRIDGE-LENGTH
           (#$thereExists ?DAMAGE-LENGTH
             (#$and
              (#$isa ?BRIDGE #$Bridge)
              (#$isa ?DAMAGE #$GapInPathArtifact)
              (#$gapWithinPath ?DAMAGE ?BRIDGE)
              (#$lengthOfObject ?DAMAGE ?DAMAGE-LENGTH)
              (#$lengthOfObject ?BRIDGE ?BRIDGE-LENGTH)
              (#$greaterThanOrEqualTo ?DAMAGE-LENGTH ?BRIDGE-LENGTH)))))))

(define sim-wa-destroyed-bridge-problem? (input-mt)
  (clet ((destroyed-bridges?
          (fi-ask-int *sim-wa-destroyed-bridge-problem-query*
            input-mt)))
    (ret (boolean destroyed-bridges?))))

(defparameter
    *sim-wa-damaged-bridge-problem-query*
    '(#$thereExists ?BRIDGE
       (#$thereExists ?DAMAGE
         (#$thereExists ?BRIDGE-LENGTH
           (#$thereExists ?DAMAGE-LENGTH
             (#$and
              (#$isa ?BRIDGE #$Bridge)
              (#$isa ?DAMAGE #$GapInPathArtifact)
              (#$gapWithinPath ?DAMAGE ?BRIDGE)
              (#$lengthOfObject ?DAMAGE ?DAMAGE-LENGTH)
              (#$lengthOfObject ?BRIDGE ?BRIDGE-LENGTH)
              (#$greaterThan ?BRIDGE-LENGTH ?DAMAGE-LENGTH)))))))

(define sim-wa-damaged-bridge-problem? (input-mt)
  (clet ((damaged-bridges?
          (fi-ask-int *sim-wa-damaged-bridge-problem-query*
            input-mt)))
    (ret (boolean damaged-bridges?))))

(define sim-wa-crevice-emplacing-problem? (input-mt)
  (clet (crevices)
    ;; looking for (#$isa ?CREVICE #$Crevice) in input-mt
    (with-just-mt input-mt
      (csetq crevices (pred-values #$Crevice #$isa 2 1)))
    (ret (boolean crevices))))

(define sim-wa-determine-variable-property-predicates (problem-mt)
  (sim-ap-trace "~%Determing the variable property predicates for ~S" problem-mt)
  (clet (sit-results-in-prop-rules predicates)
    (with-mt problem-mt
      (csetq sit-results-in-prop-rules
        ;;dmiles	     (gather-axiom-index #$sitResultsInProp :pos)))
        (gather-index #$sitResultsInProp :pos))) ;;dmiles
    (cdolist (sit-results-in-prop-rule sit-results-in-prop-rules)
      (clet ((pos-lits (pos-lits (assertion-cnf sit-results-in-prop-rule))))
        (cdolist (pos-lit pos-lits)
          (pwhen (eq (literal-predicate pos-lit) #$sitResultsInProp)
            (clet ((predicate (literal-arg4 pos-lit)))
              (pwhen (constant-p predicate)
                (cpushnew predicate predicates)))))))
    (sim-ap-trace "~%Variable property predicates: ~S" predicates)
    (ret predicates)))

(define sim-wa-populate-problem-mt (input-mt problem-mt variable-property-predicates)
  (sim-ap-trace "~%Populating ~S with static assertions from ~S"
    problem-mt input-mt)
  (clet ((input-mt-contents (gather-mt-index input-mt)))
    (csetq input-mt-contents (reverse input-mt-contents))
    (cdolist (input-assertion input-mt-contents)
      (pwhen (cand (gaf-assertion? input-assertion)
               (asserted-assertion? input-assertion))
        (clet ((predicate (gaf-predicate input-assertion)))
          (punless (member predicate variable-property-predicates)
            (clet ((formula (gaf-formula input-assertion))
                   (lifting-support
                    (sim-wa-problem-mt-population-lifting-support
                     input-mt problem-mt predicate))
                   (supports `(,input-assertion ,lifting-support)))
              (sim-ap-trace "~%Adding ~S" formula)
              (clet ((*check-wff-semantics?* nil))
                (tms-add-deduction-for-gaf
                 formula problem-mt supports :true :forward))))))))
  (sim-ap-trace "~%Forward propagating populated assertions in ~S" problem-mt)
  ;(sim-ap-trace "~%Dale" problem-mt)
  (perform-forward-inference)
  ;(sim-ap-trace "~%DaleBack" problem-mt)
  (ret nil))

(define sim-wa-clear-problem-mt (problem-mt)
  (sim-ap-trace "~%Clearing problem mt ~S" problem-mt)
  (sim-wa-clear-available-materiel problem-mt)
  (clet ((contents (gather-mt-index problem-mt)))
    (sim-ap-trace "~%Removing ~S contents" problem-mt)
    (cdolist (content contents)
      (pwhen (valid-assertion content)
        (sim-ap-trace "~%Removing ~S" content)
        (tms-remove-assertion content))))
  (sim-ap-trace "~%Killing problem mt ~S" problem-mt)
  (fi-kill-int problem-mt)
  (ret nil))

(define sim-wa-clear-available-materiel (problem-mt)
  (sim-ap-trace "~%Clearing all available materiel in ~S" problem-mt)
  (clet ((available-materiel (pred-refs-in-mt #$availableMaterial problem-mt 1)))
    (cdolist (resource available-materiel)
      (sim-ap-trace "~%Removing resource ~S" resource)
      (fi-kill-int resource)))
  (ret nil))

(define sim-wa-problem-mt-population-lifting-support (input-mt problem-mt predicate)
  (clet ((formula
          `(#$implies
            (#$ist ,input-mt
              (,predicate . ?ARGS))
            (#$ist ,problem-mt
              (,predicate . ?ARGS))))
         (support (make-hl-support :code formula problem-mt)))
    (ret support)))

(define sim-wa-setup-problem-materiel (input-mt problem-mt)
  (sim-wa-setup-interdicted-unit-and-obstacle input-mt problem-mt)
  ;(sim-ap-trace "~%DaleAV")
  (sim-wa-create-available-materiel problem-mt)
  ;(sim-ap-trace "~%DaleAVBack")
  (ret nil))

(define sim-wa-setup-interdicted-unit-and-obstacle (input-mt problem-mt)
  (clet ((obstacles
          (sim-wa-determine-obstacles input-mt))
         (interdicted-units
          (sim-wa-determine-interdicted-units obstacles input-mt)))
    (pwhen interdicted-units
      ;; for now, assume one interdicted unit / obstacle pair
      (cdestructuring-bind (obstacle interdicted-unit)
        (first interdicted-units)
        ;(sim-ap-trace "~%DaleBKNoteIn")
        (sim-ap-trace "~%Noting #$interdictedUnit : ~S" interdicted-unit)
        (fi-assert-int
         `(#$interdictedUnit ,interdicted-unit)
         problem-mt)
        ;(sim-ap-trace "~%DaleBKNotOut")
        (sim-ap-trace "~%Noting #$obstacle : ~S" obstacle)
        (fi-assert-int
         `(#$isa ,obstacle #$Obstacle)
         problem-mt))))
  ;(sim-ap-trace "~%DaleBKSetupInterdicted")
  (ret nil))

(define sim-wa-create-available-materiel (problem-mt)
  (sim-ap-trace "~%Determining available materiel types in ~S" problem-mt)
  (clet ((materiel-types
          (sim-wa-determine-available-materiel-types problem-mt)))
    (pwhen materiel-types
      (sim-ap-trace "~%Available materiel types : ~S" materiel-types)
      (clet ((resource-index 0))
        (cdolist (materiel-type materiel-types)
          (cinc resource-index)
          (clet ((materiel-type-string
                  (suggest-string-from-fort-el-formula materiel-type))
                 (resource-name
                  (format nil "Resource-~A-~A"
                    materiel-type-string resource-index))
                 (resource
                  (fi-find-or-create-int resource-name)))
            (fi-assert-int
             `(#$isa ,resource ,materiel-type)
             problem-mt)
            (fi-assert-int
             `(#$availableMaterial ,resource)
             problem-mt)
            (sim-ap-trace "~%Available resource ~S (isa ~S) created" resource materiel-type)
            )))))
  (ret nil))

(defparameter
    *sim-wa-determine-obstacle-formula*
    '(#$or 
      (#$isa ?OBSTACLE #$Tunnel) 
      (#$isa ?OBSTACLE #$Crevice)
      (#$isa ?OBSTACLE #$Highway)))

(define sim-wa-determine-obstacles (input-mt)
  (sim-ap-trace "~%Determining obstacles in ~S" input-mt)
  (clet ((results
          (fi-ask-int
           *sim-wa-determine-obstacle-formula*
           input-mt))
         (obstacles
          (mapcar #'cdar results)))
    (sim-ap-trace "~%obstacles : ~S" obstacles)
    (ret obstacles)))

(defparameter
    *sim-wa-determine-interdicted-unit-formula*
    '(#$thereExists ?LOC 
       (#$and
        ;; #$Battalion has apparently been killed
        ;; (#$isa ?UNIT #$Battalion)
        (#$sovereignAllegianceOfOrg ?UNIT #$Iran) 
        (#$bordersOn ?LOC ?OBSTACLE) 
        (#$objectFoundInLocation ?UNIT ?LOC))))

(define sim-wa-determine-interdicted-units (obstacles input-mt)
  (clet (answers)
    (sim-ap-trace "~%Determining interdicted unit for obstacles in ~S" input-mt)
    (cdolist (obstacle obstacles)
      (sim-ap-trace "~%Trying obstacle ~S" obstacle)
      (clet ((formula
              (subst obstacle '?OBSTACLE
                *sim-wa-determine-interdicted-unit-formula*))
             (results
              (fi-ask-int formula input-mt nil 1)))
        (pwhen results
          (clet ((interdicted-unit (cdar (first results))))
            (sim-ap-trace "~%interdicted unit : ~S" interdicted-unit)
            ;(sim-ap-trace "~%Daleinterdicted unit")
            (cpush (list obstacle interdicted-unit) answers)))))
    (ret answers)))

(defparameter
    *sim-wa-determine-available-material-types-formula*
    '(#$thereExists ?UNIT
       (#$and
        (#$interdictedUnit ?UNIT)
        (#$relationInstanceExists #$controls ?UNIT ?MATERIEL-TYPE)
        (#$genls ?MATERIEL-TYPE #$MilitaryEquipment))))

(define sim-wa-determine-available-materiel-types (input-mt)
  (clet ((formula
          *sim-wa-determine-available-material-types-formula*)
         (results
          (fi-ask-int formula input-mt))
         (materiel-types
          (mapcar #'cdar results)))
    (ret materiel-types)))

(define sim-wa-repropagate-initial-mt (input-mt)
  (sim-ap-trace "~%Repropagating the initial-mt ~S" input-mt)
  (clet ((input-assertions (gather-mt-index input-mt)))
    (cdolist (input-assertion input-assertions)
      (queue-forward-assertion input-assertion))
    ;(sim-ap-trace "~%Dale2" problem-mt)
    (perform-forward-inference)
    ;(sim-ap-trace "~%DaleBack2" problem-mt)
    )
  (ret nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determining goal formula
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter
    *sim-ap-last-search*
    (fif (boundp '*sim-ap-last-search*)
      *sim-ap-last-search*
      nil))
;;start search???
(define sim-ap-planner-search (problem-mt &optional number time depth)
  (clet ((initial-mt (sim-ap-initial-situation problem-mt))
         (action-type (sim-ap-determine-action-type problem-mt))
         (search (new-sim-agent-planner-search problem-mt initial-mt action-type)))
    (csetq *sim-ap-last-search* search)
    (cmultiple-value-bind (goal-formula postcondition)
      (sim-ap-determine-goal-formula problem-mt)
      (add-sim-agent-planner-search-start-nodes
       search problem-mt action-type goal-formula postcondition)
      (ret (sim-ap-continue-planner-search search number time depth)))))

(define sim-ap-continue-planner-search (search &optional number time depth)
  (clet ((*reclaim-dead-end-search-nodes* nil)
         (*dead-end-node-function* nil)
         (*sim-agent-planner-depth-cutoff* depth))
    (generic-search search number time depth))
  (sim-ap-trace "~%DaleDone for ~S" search)
  
  (ret search))

(define sim-ap-determine-action-type (problem-mt)
  (clet ((obstacle (sim-ap-obstacle problem-mt)))
    (pcond
     ((isa? obstacle #$Tunnel problem-mt)
      (ret #$TraversingATunnel))
     ((isa? obstacle #$Highway problem-mt)
      (ret #$TraversingARoad))
     (t
      (ret #$CrossingAGap)))))

(defparameter
    *sim-ap-goal-formula-template-gap*
    '(#$thereExists ?ACTION
       (#$and
        (#$isa ?ACTION ?ACTION-TYPE)
        (#$performedBy ?ACTION ?AGENT)
        (#$trajectoryPassesThrough ?ACTION ?OBSTACLE))))

(defparameter
    *sim-ap-postcondition-template-gap*
    ;; stub
    nil)

(define sim-ap-determine-goal-formula (problem-mt)
  (clet ((interdicted-unit (sim-ap-interdicted-unit problem-mt))
         (obstacle (sim-ap-obstacle problem-mt))
         ;; stub -- use Tony's decision tree
         (goal-formula *sim-ap-goal-formula-template-gap*)
         (postcondition *sim-ap-postcondition-template-gap*))
    ;; substitute interdicted-unit
    (csetq goal-formula
      (subst interdicted-unit '?AGENT goal-formula))
    (csetq postcondition
      (subst interdicted-unit '?AGENT postcondition))
    ;; substitute obstacle
    (csetq goal-formula
      (subst obstacle '?OBSTACLE goal-formula))
    (csetq postcondition
      (subst obstacle '?OBSTACLE postcondition))
    (sim-ap-trace "~%goal formula : ~%~S" goal-formula)
    (sim-ap-trace "~%postcondition: ~%~S" postcondition)
    (ret (values goal-formula postcondition))))

(define sim-ap-obstacle (problem-mt)
  ;;  (ret (first (pred-refs-in-mt #$obstacle problem-mt))))
  (ret (first (ALL-ISA-IN-MT #$Obstacle problem-mt))))

(define sim-ap-interdicted-unit (problem-mt)
  ;;  (ret (first (pred-refs-in-any-mt #$interdictedUnit problem-mt))))
  (ret (first (pred-refs-in-mt #$interdictedUnit problem-mt))))

(define sim-ap-initial-situation (problem-mt)
  (ret (fpred-value-in-mt problem-mt #$initialSituation problem-mt)))



;;;;;;;;;;;;;;;;;;;
;; search utilities
;;;;;;;;;;;;;;;;;;;

(define sim-ap-clear-search (search)
  (pwhen (search-struc-p search)
    (clet ((roots (search-tree search)))
      (pwhen roots
        (cdolist (search-node roots)
          (sim-ap-clear-children search-node)
          (clet ((action-mt (sim-ap-action-mt search-node)))
            (pwhen action-mt
              (clet ((action (sim-ap-action-mt-action action-mt)))
                (sim-ap-trace "~%Killing action ~S" action)
                (fi-kill-int action)
                (sim-ap-trace "~%Killing action mt ~S" action-mt)
                (fi-kill-int action-mt)))))
        (csetf (search-tree search) nil)
        (clear-queue (search-leaves search)))))
  (ret nil))

(define sim-ap-clear-children (search-node)
  (pwhen (search-node-p search-node)
    (clet ((children (snode-children search-node))
           (search (snode-search search-node))
           (leaves (search-leaves search)))
      ;; recursively clear all the children
      (cdolist (child children)
        (csetf (search-goals search)
          (delete child (search-goals search)))
        (remqueue child leaves)
        (sim-ap-clear-children-recursive child))
      ;; lose pointers to all the children
      (csetf (snode-children search-node) nil)))
  (ret nil))

(define sim-ap-clear-children-recursive (search-node)
  (pwhen (search-node-p search-node)
    (clet ((children (snode-children search-node))
           (search (snode-search search-node))
           (leaves (search-leaves search)))
      ;; recursively clear all the children
      (cdolist (child children)
        (csetf (search-goals search)
          (delete child (search-goals search)))
        (remqueue child leaves)
        (sim-ap-clear-children-recursive child))
      ;; lose pointers to all the children
      (csetf (snode-children search-node) nil)
      (clet ((post-mt (sim-ap-action-post-mt search-node))
             (action-mt (sim-ap-action-mt search-node)))
        (pcond
         (post-mt
          (sim-ap-trace "~%Killing post mt ~S" post-mt)
          (fi-kill-int post-mt))
         (action-mt
          (clet ((action (sim-ap-action-mt-action action-mt)))
            (sim-ap-trace "~%Killing action ~S" action)
            (fi-kill-int action)
            (sim-ap-trace "~%Killing action mt ~S" action-mt)
            (fi-kill-int action-mt)))))))
  (ret nil))

(define sim-ap-reconsider-node (search-node)
  (pwhen (search-node-p search-node)
    (sim-ap-note-search-failure search-node nil)
    (set-sim-ap-node-prop search-node :goal-info nil)
    (clet ((search (snode-search search-node))
           (leaves (search-leaves search))
           (goals (search-goals search)))
      (csetf (search-goals search) (delete search-node goals))
      (enqueue search-node leaves))
    (ret search-node)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; post-processing goals
;;;;;;;;;;;;;;;;;;;;;;;;

(define sim-ap-top-level-node (node)
  (pwhen (null node) (ret nil))
  (clet ((parent (snode-parent node)))
    (pwhen (null parent) (ret node))
    (ret (sim-ap-top-level-node parent))))

(define sim-ap-gather-ancestors (node function &optional accumulator)
  (pif (null node)
    (ret accumulator)
    (clet ((value (funcall function node)))
      (pwhen value
        (cpush value accumulator))
      (ret (sim-ap-gather-ancestors
            (snode-parent node)
            function accumulator)))))

(define sim-ap-all-ancestor-action-mts (node)
  (ret (sim-ap-gather-ancestors node #'sim-ap-action-mt)))

(define sim-ap-all-ancestor-after-mts (node)
  (ret (sim-ap-gather-ancestors node #'sim-ap-after-mt)))


(define sim-ap-postprocess-goal-node (node)
  (clet ((top-level-node (sim-ap-top-level-node node))
         (final-mt (sim-ap-action-mt top-level-node))
         (input-mt (sim-ap-input-mt node))
         (problem-mt (sim-ap-problem-mt node))
         (path-capacity (sim-ap-determine-path-capacity node))
         (all-action-mts (sim-ap-all-ancestor-action-mts node))
         (all-after-mts
          (sim-ap-process-after-mts
           problem-mt input-mt
           all-action-mts (sim-ap-all-ancestor-after-mts node)))
         (action-mt-info
          (mapcar #'sim-ap-postprocess-action-mt all-action-mts))
         (action-ordering-info
          (sim-ap-postprocess-order-actions
           input-mt all-action-mts all-after-mts))
         (total-resources
          (sim-ap-postprocess-total-resources action-mt-info))
         (minimum-duration
          (sim-ap-postprocess-minimum-duration
           input-mt final-mt action-ordering-info action-mt-info))
         (goal-info
          (list :minimum-duration minimum-duration
            :total-resources total-resources
            :path-capacity path-capacity
            :action-partial-order action-ordering-info
            :action-information action-mt-info)))
    (set-sim-ap-node-prop node :goal-info goal-info)
    (ret node)))

;; note the above is the postprocessing function
(csetq *sim-ap-goal-postprocessing-func* 'sim-ap-postprocess-goal-node)

(defparameter
    *sim-ap-determine-path-capacity-formula*
    '(#$thereExists ?PATH
       (#$pathCapacity ?PATH ?CAPACITY)))

(defparameter *sim-ap-default-path-capacity* '(#$VehiclesPerHour 100))

(define sim-ap-determine-path-capacity (node)
  (clet ((case-mts (sim-ap-ancestor-case-post-mts node)))
    (cdolist (case-mt case-mts)
      (clet ((bindings
              (fi-ask-int *sim-ap-determine-path-capacity-formula* case-mt)))
        (pwhen bindings
          (ret (cdr (first (first bindings))))))))
  (ret *sim-ap-default-path-capacity*))

(define sim-ap-postprocess-total-resources (action-mt-info)
  (clet (total-resources)
    (cdolist (action-mt-info-item action-mt-info)
      (csetq total-resources
        (union (fifth action-mt-info-item)
          total-resources #'equal)))
    (ret total-resources)))

(define sim-ap-postprocess-action-mt (action-mt)
  (clet ((action (sim-ap-action-mt-action action-mt))
         (post-mt (sim-ap-action-mt-post-mt action-mt))
         (duration (sim-ap-determine-duration action action-mt))
         (resources (sim-ap-determine-resources action action-mt)))
    (ret (list action-mt post-mt action duration resources))))

(defparameter
    *sim-ap-determine-duration-formula*
    `(#$thereExists ?DUR
       (#$and
        (#$duration ?ACTION ?DUR)
        (#$evaluate ?DURATION
          (#$QuantityConversionFn
           #$MinutesDuration ?DUR)))))

(defparameter *sim-ap-default-duration* '(#$MinutesDuration 20.0))

(define sim-ap-determine-duration (action action-mt)
  (ignore action-mt)
  (clet ((formula
          (subst action '?ACTION *sim-ap-determine-duration-formula*))
         (bindings
          (fi-ask-int formula #$InferencePSC)))
    (pif bindings
      (ret (cdr (first (first bindings))))
      (ret *sim-ap-default-duration*))))

(define sim-ap-postprocess-minimum-duration (input-mt final-mt action-ordering-info action-mt-info)
  (clet ((all-paths
          (sim-ap-reduce-partial-order-paths
           (sim-ap-expand-partial-order-paths
            input-mt final-mt action-ordering-info)))
         maximum-duration)
    (cdolist (one-path all-paths)
      (clet ((single-path-duration (sim-ap-single-path-duration one-path action-mt-info)))
        (pif (eq one-path (first all-paths))
          (csetq maximum-duration single-path-duration)
          (pwhen (cyc-greater-than single-path-duration maximum-duration)
            (csetq maximum-duration single-path-duration)))))
    (ret maximum-duration)))

(define sim-ap-single-path-duration (action-path action-mt-info)
  (clet ((total '(#$MinutesDuration 0))
         (last (car (last action-path))))
    (cdolist (action (cdr action-path))
      (pwhen (eq action last)
        (ret total))
      (clet ((action-duration (fourth (find action action-mt-info #'eql #'first))))
        (pwhen action-duration
          (csetq total (cyc-plus-internal total action-duration)))))
    (ret total)))

(defparameter
    *sim-ap-resource-predicates*
    '(#$deviceUsed #$objectEmplaced #$objectMoving #$objectOfPossessionTransfer))

(defparameter
    *sim-ap-determine-resources-formula*
    `(?RESOURCE-PRED ?ACTION ?RESOURCE))

(defparameter
    *sim-ap-annotated-resource-properties*
    '(#$rampsInBridge #$boatsSupportingBridge #$baysInBridge))

(define sim-ap-determine-resources (action action-mt)
  (clet ((action-formula
          (subst action '?ACTION *sim-ap-determine-resources-formula*))
         resources answer)
    (cdolist (resource-predicate *sim-ap-resource-predicates*)
      (clet ((resource-formula
              (subst resource-predicate '?RESOURCE-PRED action-formula))
             (bindings
              (fi-ask-int resource-formula action-mt)))
        (pwhen bindings
          (clet ((resource-set (mapcar #'cdar bindings)))
            (csetq resources (union resource-set resources))))))
    (cdolist (resource resources)
      (clet (properties)
        (cdolist (property-pred *sim-ap-annotated-resource-properties*)
          (clet ((property (fpred-value-in-any-mt resource property-pred)))
            (pwhen property
              (cpush (list property-pred property) properties))))
        (pif properties
          (cpush (list resource properties) answer)
          (cpush resource answer))))
    (ret answer)))

(define sim-ap-process-after-mts  (problem-mt input-mt all-action-mts explicit-after-mts)
  (clet (answer)
    ;; events with no explicit after mts statements did not
    ;; introduce preconditions.  Therefore, they are doable
    ;; in the input situaion.  Therefore, we add this fact 
    (cdolist (action-mt all-action-mts)
      (punless (find action-mt explicit-after-mts #'eql #'first)
        (cpush (list action-mt input-mt) answer)))
    (cdolist (explicit-after-mt explicit-after-mts)
      (cdestructuring-bind (action-mt case-post-mt) explicit-after-mt
        (csetq case-post-mt (sim-ap-parent-post-mt case-post-mt problem-mt))
        (cpush (list action-mt case-post-mt) answer)))
    (csetq answer (delete-duplicates answer #'equal))
    (ret (nreverse answer))))

(define sim-ap-postprocess-order-actions (input-mt all-action-mts all-after-mts)
  (clet (answer)
    (cdolist (action-mt all-action-mts)
      (clet ((action-post-mt (sim-ap-action-mt-post-mt action-mt)))
        (cpush (sim-ap-postprocess-order-actions-internal
                action-mt action-post-mt all-after-mts)
          answer)))
    (cpush (sim-ap-postprocess-order-actions-internal
            input-mt input-mt all-after-mts)
      answer)
    (ret answer)))

(define sim-ap-postprocess-order-actions-internal (action-mt post-mt all-after-mts)
  (clet (precondition-for-actions)
    (cdolist (after-mt-info all-after-mts)
      (pwhen (eq (second after-mt-info) post-mt)
        (cpushnew (first after-mt-info) precondition-for-actions)))
    (ret (list action-mt precondition-for-actions))))

(define sim-ap-check-partial-order-for-loops (partial-order nodes)
  (clet (nodes-with-loops)
    (cdolist (node nodes)
      (pwhen (sim-ap-loop-to-node-in-partial-order node partial-order)
        (cpush node nodes-with-loops)))
    (ret nodes-with-loops)))

(define sim-ap-loop-to-node-in-partial-order (node partial-order)
  ;; stub
  (ignore node partial-order)
  (ret nil))

(define sim-ap-expand-partial-order-paths (start finish partial-order)
  (pif (eq start finish)
    (ret `((,finish)))
    (clet ((one-steps (second (find start partial-order #'equal #'first)))
           answer)
      (cdolist (one-step one-steps)
        (clet ((recursive-answers
                (sim-ap-expand-partial-order-paths one-step finish partial-order)))
          (cdolist (recursive-answer recursive-answers)
            (cpush (cons start recursive-answer) answer))))
      (ret (nreverse answer)))))

(define sim-ap-reduce-partial-order-paths (paths &optional (test #'eql))
  (clet (unsubsumed)
    (cdolist (short-path paths)
      (clet (short-subsumed)
        (csome (long-path paths short-subsumed)
          (pwhen (cand (cnot (eq short-path long-path))
                   (>= (length long-path) (length short-path)))
            (pwhen (sim-ap-partial-order-path-subsumes long-path short-path test)
              (csetq short-subsumed t))))
        (punless short-subsumed
          (cpush short-path unsubsumed))))
    (ret (nreverse unsubsumed))))

(define sim-ap-partial-order-path-subsumes (long-path short-path &optional (test #'eql))
  (pcond
   ;; base case -- long runs out, still have short to match
   ;; FAILURE
   ((cand (null long-path)
      (cnot (null short-path)))
    (ret nil))
   ;; base case -- short runs out
   ;; Success!
   ((null short-path)
    (ret t))
   ;; recursive case -- match fronts
   ((funcall test (first long-path) (first short-path))
    (ret (sim-ap-partial-order-path-subsumes
          (rest long-path)
          (rest short-path)
          test)))
   ;; recursive case -- make long patch match short path
   (t
    (ret (sim-ap-partial-order-path-subsumes
          (member (first short-path) long-path test)
          short-path
          test)))))

(define sim-ap-find-time-person-info (formula mt)
  "Inputs: a CycL formula and a microtheory.
Outputs: if an assertion (or set of assertions) corresponding 
to the formula-mt pair can be found, and if timestamp information 
for at least one of the assertions can be found, this information
is returned as a string, else NIL is returned."
  (clet (who when)
    (clet ((*at-check-relator-constraints?* nil)
	   (assertions (remove-if-not #'assertion-p (formula-assertions formula mt))))
      (csome (a assertions who)
	(csetq who (asserted-by a)))
      (csome (a assertions when)
	(csetq when (asserted-when a))))
    (punless (cor who when) (ret nil))
    (ret (format nil "Asserted by: ~a -- Assertion date: ~a"
		 (fif who (coerce-name who) "Unknown")
		 (fif when when "Unknown")))))

(format t "planner-workarounds Loaded~%")



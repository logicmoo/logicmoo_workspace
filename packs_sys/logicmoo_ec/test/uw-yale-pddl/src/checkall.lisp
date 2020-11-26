(in-package "PDDL")

(defvar domains-dir* "M:/prog/pddl/pddl/domains/")

(proclaim '(special when-to-dump*))

(defun syntax-check-all ()
   (let ((when-to-dump* 'never))
      (dolist (f '("ART-1D-DOMAIN" "ART-1D-RD-DOMAIN" "ART-MD-DOMAIN"
                   "ART-MD-NS-DOMAIN" "ART-MD-NS-RD-DOMAIN" "ART-MD-RD-DOMAIN"
                   "D1S1" "art" "blocks-world" "briefcase" "eight"
                   "ferry" "fridge" "gripper" "homeowner" "logistics"
                   "meet-pass" "molgen" "monkey" "montlake" "morris"
                   "occam" "safety-domain" "tire-world"
                   "trains" "travel" "truckworld" "woodshop"))
         (format t "~a~%" f)
         (let ((fname 
                  (concatenate 'string 
                               domains-dir* f ".pddl")))
            (let ((e (pddl-file-syncheck fname)))
               (format t "~s~%" e)
               (cond ((not (equal e '(flagged 0 expressions)))
                      (let ((when-to-dump* 'when-flagged))
                        (pddl-file-syncheck fname)
                        (cerror "I'll go to the next file"
                           "Unhappy about that file")))))))))

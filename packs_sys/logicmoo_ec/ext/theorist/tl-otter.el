;;;
;;; Interface between Otter and Prolog PTL theorem prover.
;;;

(setq comint-input-chunk-size 128)

(defconst tlo-literal-regexp "-?[-0-9a-zA-Z_$]+")

(defconst tlo-otter-header
"set(binary_res).
set(factor).
set(print_lists_at_end).
set(process_input).
clear(print_kept).
clear(print_given).
clear(print_back_sub).
clear(print_proofs).
set(prolog_style_variables).
assign(max_mem, -1).
assign(max_seconds, -1).
set(free_all_mem).
assign(max_proofs, 1).

list(usable).
")

;;; tlo-re-search-... For compatibility with emacs 19
;;; emacs 19s re-search-... already does this

(defun tlo-re-search-forward (regexp &optional bound noerror rpt)
  (if (re-search-forward regexp bound noerror rpt)
      (point)))

(defun tlo-re-search-bacward (regexp &optional bound noerror rpt)
  (if (re-search-backward regexp bound noerror rpt)
      (point)))

(defun tlo-find-next-clause ()
  (forward-line 1)
  (and (not (looking-at "end_of_list\."))
       (search-forward "]" nil t)))

(defun tlo-grab-next-clause ()
  (if (tlo-find-next-clause)
      (tlo-grab-clause)))

(defun tlo-grab-first-clause ()
  (cond ((search-forward "END OF SEARCH")
	 (search-forward "list(usable).")
	 (tlo-grab-next-clause))))

(defun tlo-grab-literal ()
  (re-search-forward " *")
  (let ((start (point))
	(end (tlo-re-search-forward tlo-literal-regexp nil t)))
    (if end
	(buffer-substring start end)
      (error "tlo-grab-literal: bad input format"))))

(defun tlo-grab-clause ()
  (let ((l ())
	(more t))
    (while more
      (setq l (cons (tlo-grab-literal)
		    l))
      (setq more (not (looking-at " *\\.")))
      (if more
	  (search-forward "|")))
    l))

(defun tlo-insert-prolog-term (literals)
  (let ((lits literals)
	(past ())
	(present ())
	(type 'global)
	next)
    (while lits
      (setq next (tlo-classify (car lits)))
      (setq lits (cdr lits))
      (let ((when (car next))
	    (literal (cdr next)))
	(cond ((eq when 'present)
	       (setq present (cons literal present)))
	      ((eq when 'past)
	       (setq past (cons literal past)))
	      (t
	       (setq type 'initial)
	       (setq present (cons literal present))))))
    (cond ((eq type 'global)
	   (tlo-insert-global-term past present))
	  (t
	   (tlo-insert-initial-term present)))))

(defun tlo-insert-global-term (past present)
  (cond (past
	 (insert "((" )
	 (while past
	   (let ((literal (car past))
		 (rest (cdr past)))
	     (tlo-insert-literal literal)
	     (if rest
		 (insert " and "))
	     (setq past rest)))
	 (insert ")) imp next "))
	(t
         (insert "")))
;;	 (insert "( true ) imp ")))
;;  (insert " imp next ")
  (cond (present
	 (insert "(")
	 (while present
	   (let ((literal (car present))
		 (rest (cdr present)))
	     (tlo-insert-literal literal)
	     (if rest
		 (insert " or "))
	     (setq present rest)))
	 (insert ")"))
	(t
	 (insert "false"))))

(defun tlo-insert-literal (literal)
  (cond ((consp literal)
	 (insert "neg ")
	 (tlo-insert-proposition (cdr literal)))
	(t
	 (tlo-insert-proposition literal))))
          
(defun tlo-insert-proposition (prop)
  (cond ((equal (string-match "tmp_p_" prop) 0)
	 (insert "tmp_p("
		 (substring prop 6)
		 ")"))
         ((equal (string-match "nkn_" prop) 0)
	  (insert "nkn(")
          (setq nknrest (substring prop 4))
          (tlo-insert-nkn nknrest)
          (insert ")"))
         ((equal (string-match "new_" prop) 0)
	  (insert "new(")
          (setq newrest (substring prop 4))
          (tlo-insert-new newrest)
	  (insert ")")
          (setq prop nil))
	(t
	 (insert prop))))
		  
(defun tlo-insert-nkn (prop)
  (while prop
     (cond ((equal (string-match "neg_" prop) 0)
                   (insert "neg ")
                   (setq prop (substring prop 4)))
           ((equal (string-match "tmp_p_" prop) 0)
	    (insert "tmp_p("
		    (substring prop 6)
		    ")")
            (setq prop nil))
           ((equal (string-match "new_" prop) 0)
	     (insert "new(")
             (setq newrest (substring prop 4))
             (tlo-insert-new newrest)
	     (insert ")")
             (setq prop nil))
	   (t
	    (insert prop)
            (setq prop nil)))))

(defun tlo-insert-new (prop)
   (setq prop-tlo-new (substring prop 0))
   (if (string-match "neg_" prop-tlo-new)
       (setq neg-prop-tlo-new (tlo-replace-neg prop-tlo-new))
       (setq neg-prop-tlo-new prop-tlo-new))
   (if (string-match "_" neg-prop-tlo-new) 
       (setq new-result (tlo-replace-and neg-prop-tlo-new))
       (setq new-result neg-prop-tlo-new))
   (tlo-insert-new-prop new-result))


(defun tlo-replace-neg (prop)
     (setq point 1)
     (while (string-match "neg_" prop point)
         (setq prop (replace-match " neg " t t prop nil))
         (setq point (match-end 0)))
    prop)

(defun tlo-replace-and (prop)
     (setq point 1)
     (while (string-match "_" prop point)
         (setq prop (replace-match " and " t t prop nil))
         (setq point (match-end 0)))
    prop)

(defun tlo-insert-new2 (prop)
   (setq prop-tlo-new (substring prop 0))
   (if (string-match "neg_" prop-tlo-new)
       (setq neg-prop-tlo-new (replace-match " neg " t t prop-tlo-new nil))
       (setq neg-prop-tlo-new prop-tlo-new))
   (if (string-match "tmp_p_" neg-prop-tlo-new)
       (setq temp-prop-tlo-new (replace-match "tmp-p " t t neg-prop-tlo-new nil))
       (setq temp-prop-tlo-new neg-prop-tlo-new))
   (if (string-match "_" temp-prop-tlo-new) 
       (setq new-result (replace-match " and " t t temp-prop-tlo-new nil))
       (setq new-result temp-prop-tlo-new))
   (tlo-insert-new-prop new-result))

(defun tlo-insert-new-prop (prop)
  (while prop
     (cond ((equal (string-match " neg " prop) 0)
                   (insert "neg ")
                   (setq prop (substring prop 5)))
           ((equal (string-match "tmp-p " prop) 0)
	    (insert "tmp_p("
		    (substring prop 6)
		    ")")
            (setq prop (substring prop 6)))
           ((equal (string-match " and " prop) 0)
	     (insert " and ")
             (setq prop (substring prop 5)))
	   (t
	    (insert prop)
            (setq prop nil)))))

(defun tlo-insert-initial-term (lits)
  (insert "(start) imp (")
  (while lits
    (tlo-insert-literal (car lits))
    (setq lits (cdr lits))
    (if lits
	(insert " or ")))
  (insert ")"))

(defun tlo-classify (literal)
  (cond ((string-match "^neg_" literal)
	 (tlo-classify-negative (substring literal 4)))
         ((string-match "^-" literal)
	 (tlo-classify-negative (substring literal 1)))
	(t
	 (tlo-classify-positive literal))))

;;; Note: assume slast literals are always negative
          
(defun tlo-classify-positive (literal)
  (cond ((string-match "^s_" literal)
	 (cons 'present (substring literal 2)))
	(t
	 (cons 'initial literal))))

(defun tlo-classify-negative (literal)
  (cond ((string-match "^slast_" literal)
	 (cons 'past (tlo-classify-past (substring literal 6))))
	((string-match "^s_" literal)
	 (cons 'present
	       (cons 'not (substring literal 2))))
	(t
	 (cons 'initial (cons 'not literal)))))

(defun tlo-classify-past (literal)
  (if (string-match "^neg_" literal)
      (cons 'not (substring literal 4))
    literal))

(defun tlo-otter-to-prolog ()
  (interactive)
  (goto-char (point-min))
  (cond ((search-forward "$F" nil t)
	 (delete-region (point-min) (point-max))
	 (insert "[false]."))
	(t
	 (insert "[\n")
	 (let ((oldpoint (point))
	       (clause (tlo-grab-first-clause)))	 
	   (delete-region oldpoint (point))
	   (while clause
	     (tlo-insert-prolog-term clause)
	     (setq oldpoint (point))
	     (setq clause (tlo-grab-next-clause))
	     (delete-region oldpoint (point))
	     (if clause
		 (insert ",\n"))))
	 (delete-region (point) (point-max))  
	 (insert "\n]."))))

(defun tlo-grab-usable-rules ()
  (search-backward "Usable rules are")
  (end-of-line)
  (let ((start (point)))
    (search-forward "Rules in the SoS are")
    (beginning-of-line)
    (buffer-substring start
		      (point))))

(defun tlo-grab-sos ()
  (end-of-line)
  (let ((start (point)))
    (search-forward "\n\n|:")
    (beginning-of-line)
    (buffer-substring start (point))))

(defun tlo-run-otter ()
  (interactive)
  (let ((old-buf (current-buffer))
	(input-buf (get-buffer-create " *otter-input*"))
	(output-buf (get-buffer-create " *otter-output*"))
	(usable (tlo-grab-usable-rules))
	(sos (tlo-grab-sos)))
    (goto-char (point-max))
    (set-buffer input-buf)
    (insert tlo-otter-header
	    usable
	    "end_of_list.\nlist(sos).\n"
	    sos
	    "end_of_list.\n")
;;    (save-buffer)
    (message "Running OTTER ...")
    (call-process-region (point-min)
			 (point-max)
			 "/bin/bash"
			 nil
			 output-buf
			 nil
			 "-c"
			 "otter 2>/dev/null")
    (message "Running OTTER ... done")
    (set-buffer output-buf)
    (mark-whole-buffer)
    (setq new-file (make-temp-name "otter-output"))
    (append-to-file (point-min) (point-max) new-file)
    (message "Parsing OTTER output ...")
    (tlo-otter-to-prolog)
    (message "Parsing OTTER output ... done")
    (let ((prolog-process (get-buffer-process old-buf))
	  (prolog-input (buffer-substring (point-min)
					  (point-max))))
      (message prolog-input)
      (set-buffer old-buf)
      (goto-char (point-max))
      (comint-send-string prolog-process prolog-input)
      (comint-send-string prolog-process "\n")
      (kill-buffer input-buf)
      (kill-buffer output-buf)
)))


(defun tlo-send-string (process string)
  "Send string to process without echoing process's response (eg prompts)"
  (let ((old-filter (process-filter process)))
    (unwind-protect (progn
		      (set-process-filter process
					  'tlo-null-filter)
		      (comint-send-string process string)
		      (sit-for 1))
      (set-process-filter process old-filter))))


(defun tlo-null-filter (process string)
  ;; do nothing
  )

(defun tlo-finish-proof ()
  (interactive)
  (let* ((prolog-process (get-buffer-process (current-buffer)))
	 (old-filter (process-filter prolog-process)))
    (unwind-protect
	(progn
	  (set-process-filter prolog-process 'tlo-prompt-filter)
	  (tlo-complete-proof))
      (set-process-filter prolog-process old-filter))))

(defun tlo-complete-proof ()
  (let ((more t))
    (while more
      (tlo-run-otter)
      (setq more
	    (tlo-wait-for-prolog)))))

(defconst tlo-normal-prompt "| \\?- ")
(defconst tlo-read-prompt "|: ")

(defconst tlo-prompt-regexp (concat tlo-normal-prompt
				    "\\|"
				    tlo-read-prompt))

(defun tlo-wait-for-prolog ()
  (catch 'tlo-prolog-done        ; Thrown by tlo-prompt-filter
    (while t
      (sit-for 1))))

(defun tlo-prompt-filter (proc string)
  (let ((old-buffer (current-buffer))
	(data (match-data)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc)))
	  (let ((start (string-match tlo-prompt-regexp string)))
	    (if start
		(throw 'tlo-prolog-done
		       (string-equal (substring string start)
				     tlo-read-prompt)))))
      (set-buffer old-buffer)
      (store-match-data data))))

(defun tlo-grab-next-ruleset ()
  (let ((beginning (search-forward "[" nil t))
	(end nil))    
    (cond (beginning
	   (setq end (search-forward "]"))))
    (and beginning
	 end
	 (buffer-substring (1- beginning) end))))

(defun tlo-run-examples (goal filename)
  (interactive "sName of Prolog goal: \nfExample file name: ")
  (let* ((example-buffer (find-file-noselect filename))
	 (prolog-buffer (current-buffer))
	 (prolog-process (get-buffer-process (current-buffer)))
	 (old-filter (process-filter prolog-process)))
    (unwind-protect
	(progn
	  (set-process-filter prolog-process 'tlo-prompt-filter)
	  (set-buffer example-buffer)
	  (goto-char (point-min))
	  (let ((example (tlo-grab-next-ruleset)))
	    (while example
	      (message example)
	      (set-buffer prolog-buffer)
	      (tlo-do-proof goal example)
	      (set-buffer example-buffer)
	      (setq example (tlo-grab-next-ruleset))))
	  (set-buffer prolog-buffer)
	  (message "Finished running examples"))
      (set-process-filter prolog-process old-filter))))

(defun tlo-do-proof (goal ruleset)
  (goto-char (point-max))
  (insert goal "(" ruleset ").")
  (comint-send-input)
  (if (tlo-wait-for-prolog)
      (tlo-complete-proof)))


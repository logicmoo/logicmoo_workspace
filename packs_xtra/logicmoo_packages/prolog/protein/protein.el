
;; protein.el --- major mode for editing and running code for the Protein
;; theorem prover

;; author: michael k"uhn 
;; date: 03Mar95
;; changes by peter and doro

(defvar protein-mode-map nil)
(defvar protein-mode-syntax-table nil)
(defvar protein-mode-hook nil)

(defconst protein-flag-table
  '( (ancestry_rme ("on" "off") "off")
     (ancs ("off" "pos" "neg" "pos_dj" "both_dj" "both") "both")
     (answers ("one" "more" "all") "one")
     (answer_set_handling ("on" "off") "on") 
     (calculus ("me" "rme" "hyper") "me")
     (check_flags ("[]" "[protein_flag(calculus,rme)]"
                   "[protein_flag(calculus,hyper)]") "[]")
     (costs ("(red_cut,0)" "(factor_cut,0)" "(red,0)" "(factor,0)" "(fact,0)" 
             "(th_fact,0)" "(ext,1)" "(th_start_1,1)" "(th_start,1,1)" 
             "(query,1)" "(restart,1)" "(th_red_cut,0)" "(th_red,0)" 
             "(th_ext,2)") "defaults" t)
     (ctest ("now" "store" "end" "protein") "now")
     (definite_answers ("on" "off") "off") 
     (delayed_rme ("off" "cut" "nocut") "off")
     (depth_increment ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13"
        "14" "15" "16" "17" "18" "19" "20" "25" "30" "40" "50") "1")
     (ec_pruning ("on" "off") "off")
     (factorisation ("off" "nocut" "cut" "both") "cut")
     (head_behind_prolog ("on" "off") "off")
     (max_rew_cond ("0" "1" "2" "3" "100") "0")
     (mmr ("wgcwa" "gcwa" "model" "off") "off")
     (mode ("pl0" "pl1") "pl1")
     (out_stream ("user") "user")
     (priority ("(red_cut,2)" "(factor_cut,3)" "(red,4)" "(factor,5)" "(fact,6)"
                "(th_fact,cl+6)" "(ext,cl)" "(th_start_1,cl+1)" 
                "(th_start,cl+1)" "(th_red_cut,2)" "(th_red,3)" "(th_ext,cl)")
               "defaults" t)
     (query_reuse ("on" "off") "on")
     (reduction ("off" "nocut" "cut" "both") "both")
     (regularity ("off" "nodelay" "delay") "delay")
     (reorder ("off" "groundness") "off")
     (rewrite ("flat" "deep") "flat")
     (selection_function ("on" "off") "off")
     (search ("prolog" "id_tree" "id_inf" "id_term") "id_tree")
     (sim_dynamic ("off" "cut" "nocut" "mixed") "nocut")
     (sim_static ("off" "uncond" "all") "uncond")
     (sim_deletion ("keep" "del" "complete") "complete")
     (sim_focus ("all" "query") "query")
     (sorting ("costs" "input") "costs")
     (strict_rme ("on" "off") "off")
     (th_nonewclauses ("on" "off") "off") 
     (th_reduction ("off" "nocut" "cut" "both") "both")
     (th_regularity ("off" "nodelay" "delay") "delay")
     (th_sidelit_anc ("on" "off") "off") 
     (timeout ("1" "10" "60" "120" "300" "600" "1800" "3600" "7200" "43200"))
     (trace ("off" "info" "internal" "dynamic(0)" "dynamic(1)"
        "dynamic(2)" "dynamic(5)" "dynamic(10)") "info")
     (translate ("all" "plain_theory" "without_query") "all")
     )

  "This constant holds the Protein flags with their default values."
  )


(defun protein-make-flagname-completion-table (flag-list)
  "Build a completion list with the flag-names."

  (cond ((null flag-list) nil)
	(t
	  (cons (list (symbol-name (car (car flag-list))))
		(protein-make-flagname-completion-table (cdr flag-list))
		)
	  )
	)
  )



(defconst protein-flagname-completion-table
  (protein-make-flagname-completion-table protein-flag-table)
  "This constant contains a completion table for the flagnames."
  )


(if protein-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/  ". 14" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?%  "< b"  table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?_  "w"  table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (setq protein-mode-syntax-table table)))

(load-library "prolog")
;(setq protein-font-lock-keywords prolog-font-lock-keywords)

(require 'font-lock)

(copy-face 'default 'protein-simplify-face)
(set-face-foreground 'protein-simplify-face "#1e7198")

(copy-face 'font-lock-comment-face 'protein-comment-face)

(setq protein-font-lock-keywords 
      (append (list
	       '("^begin(\\(?:simplify\\|theory\\))[.]\\(\\(.\\|\n\\)*?\\)end(\\(?:simplify\\|theory\\))[.]" 
		 1 protein-simplify-face t)
	       '("\\(/\\*\\(.\\|\n\\)*?\\*/\\)" 
		 1 protein-comment-face nil)
	       '("%.*$" 0 protein-comment-face nil)
	       )
	      prolog-font-lock-keywords
	      ))


(put 'protein-mode 'font-lock-defaults
   '(protein-font-lock-keywords 
     t
     nil 
     (;; comments:
      (?%  . "<")
      (?\n  . ">")
      )
;;      (;; newline
;;        (?\n  . "w")
;;        )
    backward-paragraph))


(if protein-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?~ "." table)
    (setq protein-mode-syntax-table table)
    ) )

(define-abbrev-table 'protein-mode-abbrev-table nil)

	
(defun protein-mode-command-keys (key-map)
  "Define protein-mode command-keys."

  (define-key key-map "\C-c\C-c" 'protein-execute)
  (define-key key-map "\C-c\C-f" 'protein-read-and-set-flag)
  (define-key key-map "\C-c\C-w" 'protein-wipe-flags)
  (define-key key-map "\C-c\C-o" 'otter2protein)
  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (define-key key-map 'button3 'protein-popup-menu))
;   (t (define-key key-map 'C-mouse-3     'protein-popup-menu))
   )
  )


(if protein-mode-map
    ()
  (setq protein-mode-map (make-sparse-keymap))
  (protein-mode-command-keys protein-mode-map)
  )


(defun protein-mode ()
  "Major mode for editing and running code for the Protein theorem prover.
Commands:
\\{protein-mode-map}
On entry call the value of 'protein-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (use-local-map protein-mode-map)
  (set-syntax-table protein-mode-syntax-table)
  (setq local-abbrev-table protein-mode-abbrev-table)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (setq major-mode 'protein-mode)
  (setq mode-name "Protein")
;  (protein-wipe-flags)
  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (set-buffer-menubar current-menubar)
    (add-submenu nil protein-menu)
    (set-menubar-dirty-flag)))
  (run-hooks 'protein-mode-hook)
  )


(defconst protein-flag-header "^ *protein_flag( *")

(defun protein-make-flag-menu (flag-list)
  "This function builds a menu for the flags."

  (cond ((null flag-list) nil)
    (t
      (let ((flag (car (car flag-list))) (type (car (cdr (car flag-list)))))
	(cons 
	 (cons
	  (symbol-name flag)
	  (protein-make-type-menu flag type) )
	 (protein-make-flag-menu (cdr flag-list)) ))))
  )

(defun protein-make-type-menu (flag type)

  (cond ((null type) nil)
	(t
	 (cons 
	  (vector (car type) `(protein-set-flag ',flag ,(car type)) 't)
	  (protein-make-type-menu flag (cdr type))) ))
  )

(defconst protein-menu
  (append '(
   "Protein"
   ["Otter -> Protein" (otter2protein) :keys "C-c C-o"]
   "---"
   ["Run Protein" (protein-execute) :keys "C-c C-c"]
   "---"
   ["Set Flag" (protein-read-and-set-flag) :keys "C-c C-f"]
   "---"
;   ["Wipe Flags" (protein-wipe-flags) :keys "C-c C-w"]
;   "---"
   )
   (protein-make-flag-menu protein-flag-table) ))


(defun protein-popup-menu ()
  "This function pops up the Protein menu (mouse button 3)."

  (interactive)
  (popup-menu (cons "Protein Menu" (cdr protein-menu)))
  )


(defun protein-find-flag (flag-name)
  "This function sets point to found flag or returns nil."

  (re-search-forward (concat protein-flag-header flag-name) nil t)
)

(defun protein-multiple-value-find-flag (flag-name value)
  "This function sets point to found flag or returns nil."
  (let* ((extended-key-end (string-match "[ ]*," value))
	 (extended-key (substring value 0 extended-key-end)))
    (cond ((re-search-forward (concat protein-flag-header 
				   flag-name "," extended-key) 
			      nil t)
	   (backward-char (+ extended-key-end 1))
	   t)
	  (t nil))
    ))



(defun protein-set-flag (flag &optional value)

  (let ((flag-definition (assoc flag protein-flag-table)))
    (cond ((null flag-definition) (error "Unknown flag: " (symbol-name flag))))
    (let ((flag-default (car (cdr (cdr flag-definition))))
	  (multiple-value-flag
	   (not (null (cdr (cdr (cdr flag-definition)))))))
      (save-excursion
	(goto-char (point-min))
	(cond
	 ((and (not multiple-value-flag)
	       (protein-find-flag (symbol-name flag))) 
	  (kill-line))
	 ((and multiple-value-flag
	       (protein-multiple-value-find-flag 
		(symbol-name flag)
		value))
	  (kill-line))
	 (t
	  (goto-char (point-min))
	  (insert (concat "protein_flag(" (symbol-name flag)))
	  (open-line 1) ))
	(cond ((null value) (setq value flag-default)))
	(insert ",") (insert value) (insert ").")
	)) ))


(defun protein-read-and-set-flag ()

  (interactive)
  (let ((flag-name  (completing-read "Flag: " protein-flagname-completion-table nil t)))
    (cond ((equal flag-name ""))
	  (t (let* ((flag (intern flag-name))
		    (flag-definition (assoc flag protein-flag-table)))
	       (cond ((null flag-definition)
		      (error (concat "Unknown flag: " flag-name))))
	       (let* ((flag-type (car (cdr flag-definition)))
		      (flag-default (car (cdr (cdr flag-definition))))
		      (flag-value
		      (read-string (concat "Flag: " flag-name " Value: ") flag-default)))
		 (cond ((equal flag-value ""))
		       (t
			(cond ((not (member flag-value flag-type))
			   (error (concat "Unknown value for flag " flag-name ": "
				       flag-value))))
		 (protein-set-flag  flag flag-value) )))))))
  )

(defun protein-wipe-flags ()
  "This function checks flag definitions and removes them if \
redundant."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward protein-flag-header nil t)
      (let ((flag (protein-recognize-flag (cdr protein-flag-table))))
	(cond ((null flag) (protein-delete-line))
	  (t
	    (let ((value (looking-at " *, *on")))
	      (cond
	        ( (equal value (car (cdr (assoc flag protein-flag-table))))
		  (protein-delete-line)
		  )
		( (equal value (eval flag))
		  (save-excursion
		    (while
		      (re-search-forward
		        (concat protein-flag-header (symbol-name flag))
			nil t
			)
		      (protein-delete-line)
		      )
		    )
		  )
		(t (protein-delete-line))
		)
	      )
	    )
	  )
	)
      )
    )
  )


(defun protein-delete-line ()
  "Delete current line."

  (beginning-of-line)
  (kill-line)
  (cond ((not kill-whole-line)(delete-backward-char 1)))
  )


(defun protein-recognize-flag (flag-list)
  "This function returns the flag at point or nil."

  (cond ((null flag-list) nil)
    (t
      (let ((flag (car (car flag-list))))
	(cond
	  ( (looking-at (concat " *" (symbol-name flag)))
	    (re-search-forward (symbol-name flag))
	    flag)
	  (t (protein-recognize-flag (cdr flag-list)))
	  )
	)
      )
    )
  )


(defun protein-execute ()
  "This routine calls the Protein theorem prover with current buffer.
Output is given in a new window."

  (interactive)
  (save-buffer)
  (shell-command
    (concat "protein " (protein-buffer-filename-without-extension) "&")
    )
  )
	

(defun protein-buffer-filename-without-extension ()
  "This function returns the name of the .tme file without extension."

  (substring (buffer-file-name) 0 (- (length (buffer-file-name)) 4))
  )


(defun otter2protein ()
  "This function transforms otter syntax into protein syntax."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
      "tr '[t-z][A-S]|$' '[T-Z][a-s]; '" t)
)

(provide 'protein)

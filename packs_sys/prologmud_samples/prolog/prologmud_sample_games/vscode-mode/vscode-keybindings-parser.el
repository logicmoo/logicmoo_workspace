;; from https://github.com/codebling/vs-code-default-keybindings
(setq vscode-mode-forward-keybindings-json-file (concat vscode-mode-dir "/data/windows.keybindings.json"))
;; (ffap (concat vscode-mode-dir "/data/windows.keybindings.json"))
(setq vscode-mode-forward-negative-keybindings-json-file (concat vscode-mode-dir "/data/windows.negative.keybindings.json"))
;; (ffap (concat vscode-mode-dir "/data/windows.negative.keybindings.json"))

;; from https://github.com/whitphx/vscode-emacs-mcx/blob/7e81757ec668f0aa0d3fb59f4c700fe432d59bc9/package.json
(setq vscode-mode-reverse-keybindings-json-file (concat vscode-mode-dir "/data/keybindings.reverse.json"))
;; (ffap (concat vscode-mode-dir "/data/keybindings.reverse.json"))
(setq vscode-mode-reverse-negative-keybindings-json-file (concat vscode-mode-dir "/data/keybindings.negative.reverse.json"))
;; (ffap (concat vscode-mode-dir "/data/keybindings.negative.reverse.json"))

;; forward VSCode Keybindings -> VSCode Commands
;; reverse Emacs Keybindings -> VSCode Commands

;; we need to figure out which Emacs functions to invoke for which
;; VSCode Keybindings.

;; (key-binding "\C-cvv")
;; (lookup-key vscode-minor-mode-map "\C-cvr")

;; so you do:

;; emacs-function-to-invoke =
;; lookup-key((reverse^-1)*(forward(translate-to-emacs-style(vscode-style-key-sequence)))).

(defvar vscode-mode-forward-keybindings-json-parse nil "")
(defvar vscode-mode-reverse-keybindings-json-parse nil "")

;; (require 'json)
(load "/usr/share/emacs/26.1/lisp/json.el.gz")

(defun vscode-minor-mode-parse-keybindings-json-file-debug (file)
 ""
 (let ((json (get-string-from-file file)))
  (kmax-edit-temp-file)
  (insert json)
  (insert "\n")
  (beginning-of-buffer)
  (json-read)))

(defun vscode-minor-mode-parse-key-sequences-json-file (file)
 ""
 (let ((json (get-string-from-file file)))
  (json-read-from-string (concat json "\n"))))

(defun compute-forward-keybindings ()
 ""
 (see-if "Start" 0.0)
 (setq vscode-mode-forward-keybindings-json-parse
  (vscode-minor-mode-parse-key-sequences-json-file vscode-mode-forward-keybindings-json-file))
 (see (mapcar (lambda (item) (vscode-mode-reverse-key-sequence item)) vscode-mode-forward-keybindings-json-parse) 1.0))

(defun compute-reverse-keybindings ()
 ""
 (see-if "Start" 0.0)
 (setq vscode-mode-reverse-keybindings-json-parse
  (vscode-minor-mode-parse-key-sequences-json-file vscode-mode-reverse-keybindings-json-file))
 (see (mapcar (lambda (item) (vscode-mode-reverse-key-sequence item)) vscode-mode-reverse-keybindings-json-parse) 1.0))

;; (defvar vscode-mode-vscode-style-key-to-emacs-style-key-mapping-alist
(setq vscode-mode-vscode-style-key-to-emacs-style-key-mapping-alist
 '(
   ("ctrl" . "C")
   ("alt" . "M")
   ("shift" . "S")
   ("backspace" . "DEL")
   ("escape" . "ESC")
   ("pageup" . "")
   ("pagedown" . "")
   ("enter" . "RET")
   ("space" . "SPC")
   ;; ("tab" . "TAB")
   ("up" . "<up>")
   ("down" . "<down>")
   ("left" . "<left>")
   ("right" . "<right>")
   ;; ("end" . "<end>")
   ))   

(defun vscode-mode-reverse-key-sequence (item)
 ""
 ;; (see key-sequence)
 (let* ((key (cdr (assoc 'key item)))
	(emacs-key (vscode-mode-parse-vscode-style-key key))
	(command (cdr (assoc 'command item)))
	(when (cdr (assoc 'when item)))
	(args (cdr (assoc 'args item))))
  (if (not (string= key "%"))
   (list
    (cons
     emacs-key
     (list
      (cons 'key emacs-key)
      (cons
       'command
       (vscode-mode-parse-vscode-style-command command))
      (cons 'when-orig when)
      (cons 'when 
       (vscode-mode-parse-vscode-style-when when))
      (cons 'args args))))      
   nil)))
 
(defun vscode-mode-parse-vscode-style-key (key)
 ""
 (join " "
     (mapcar
      (lambda (key-combination)
       (join "-"
	(mapcar
	 (lambda (individual-key)
	  (or (cdr (assoc individual-key vscode-mode-vscode-style-key-to-emacs-style-key-mapping-alist)) individual-key))
	 key-combination)))
      (mapcar (lambda (key) (split-string key "+" nil nil))
       (split-string key " " nil nil)))))

(defun vscode-mode-parse-vscode-style-command (command)
 (if command
  (list (make-symbol (concat "vscode-mode-command/" command)))))

(defun vscode-mode-parse-vscode-style-when (when)
 ""
 (if when
  (vscode-mode-parse-vscode-style-when-or when)
  nil))

;; (defun vscode-mode-parse-vscode-style-when-or (disjuncts)
;;  ""
;;  (if disjuncts
;;   (let ((result (mapcar #'vscode-mode-parse-vscode-style-when-and (kmax-split-string disjuncts " \|\| "))))
;;    (cons 'or result))))

;; (defun vscode-mode-parse-vscode-style-when-and (conjuncts)
;;  ""
;;  (if conjuncts
;;   (let ((result (mapcar #'vscode-mode-parse-vscode-style-when-expr (kmax-split-string conjuncts " \&\& "))))
;;    (cons 'and result))))

(defun vscode-mode-parse-vscode-style-when-or (disjuncts)
 ""
 (if disjuncts
  (let ((result (mapcar #'vscode-mode-parse-vscode-style-when-and (kmax-split-string disjuncts " \|\| "))))
   (if (= (length result) 1) (car result) (cons 'or result)))))

(defun vscode-mode-parse-vscode-style-when-and (conjuncts)
 ""
 (if conjuncts
  (let ((result (mapcar #'vscode-mode-parse-vscode-style-when-expr (kmax-split-string conjuncts " \&\& "))))
   (if (= (length result) 1) (car result) (cons 'and result)))))

(setq do-see nil)
;; (setq do-see t)

(defun see-if (item &optional duration)
 ""
 (if do-see (progn (see item (if duration duration 0.0)) item) item))

(defun vscode-mode-parse-vscode-style-when-expr (expr)
 ""
 (if expr
  (progn
   (if (not (kmax-string-match-p " =~ " expr)) (see-if (concat "Expr: " expr) 0.0))
   (see-if 0 0.0)
   (see-if
    (or
     ;; empty quotation
     (and (kmax-string-match-p "^'\\(.*\\)'$" expr) (progn (see-if 1 0.0) (match-string 1 expr)))

     ;; literal
     (and (kmax-string-match-p "^[-0-9a-zA-Z\\.]+$" expr) (progn (see-if 2 0.0) (vscode-mode-parse-vscode-style-when-literal expr)))

     ;; not
     (and (kmax-string-match-p "^\\!\\([-0-9a-zA-Z\\.]+\\)$" expr) (list 'not (vscode-mode-parse-vscode-style-when-literal (progn (see-if 3 0.0) (match-string 1 expr)))))

     ;; regex
     (and (kmax-string-match-p " =~ " expr) (cons 'string-match (car (mapcar (lambda (my-list) (progn (see-if 4 0.0) (list (vscode-mode-parse-vscode-style-when-expr (car my-list)) (vscode-mode-parse-vscode-style-when-regex (cadr my-list)) nil nil))) (list (kmax-split-string expr " =~ "))))))

     ;; equals
     (and (kmax-string-match-p " == " expr) (cons 'equal (car (mapcar (lambda (my-list) (progn (see-if 5 0.0) (list (vscode-mode-parse-vscode-style-when-expr (car my-list)) (vscode-mode-parse-vscode-style-when-expr (cadr my-list))))) (list (kmax-split-string expr " == "))))))

     ;; not equals
     (and (kmax-string-match-p " != " expr) (list 'not (cons 'equal (car (mapcar (lambda (my-list) (progn (see-if 6 0.0) (list (vscode-mode-parse-vscode-style-when-expr (car my-list)) (vscode-mode-parse-vscode-style-when-expr (cadr my-list))))) (list (kmax-split-string expr " != ")))))))

     nil
     )
    0.0))))

(defun vscode-mode-parse-vscode-style-when-literal (literal)
 ""
 (if literal
  (list (make-symbol (concat "vscode-mode-cond/" literal)))))

(defun vscode-mode-parse-vscode-style-when-regex (regex)
 (kmax-string-match-p "^/\\(.+\\)/$" regex)
 (let ((chars (split-string regex "" nil nil)))
  (pop chars)
  (shift chars)
  (join "" chars)))

(defun vscode-mode-keybindings-parser-test ()
 ""
 (interactive)
 (progn (kmax-pp-string (prin1-to-string (compute-forward-keybindings))) (emacs-lisp-mode)))

;; (vscode-mode-keybindings-parser-test)

;; now compute 

(defun vscode-mode-find-all-items (symbol commands)
 (let
  ((matches nil))
  (mapcar (lambda (forward-command-item)
	   (if (string= (prin1-to-string symbol) (prin1-to-string (car forward-command-item)))
	    (let ((my-list (car (cdr forward-command-item))))
	     (push (cons (car my-list) (list (cdr my-list))) matches)
	     )))
   commands)
  (cons symbol matches)))

(defun vscode-mode-reset-commands ()
 ""
 (makunbound 'forward-commands)
 (makunbound 'reverse-commands))

(defun vscode-mode-compute-vscode-style-keybindings ()
 ""
 (progn
  (if (not (non-nil 'forward-commands))
   (progn
    (setq forward-commands nil)
    (mapcar (lambda (item) (push (cons (car (cdr (assoc 'command (car item)))) item) forward-commands)) (compute-forward-keybindings))))
  (if (not (non-nil 'reverse-commands))
   (progn
    (setq reverse-commands nil)
    (mapcar (lambda (item) (push (cons (car (cdr (assoc 'command (car item)))) item) reverse-commands)) (compute-reverse-keybindings))))

  ;; (vscode-mode-command/references-view\.showCallHierarchy ("S-M-h" (command vscode-mode-command/references-view\.showCallHierarchy) (when-orig . "editorHasCallHierarchyProvider") (when vscode-mode-cond/editorHasCallHierarchyProvider)))

  (setq all-matches nil)
  (setq forward-matches nil)
  (setq reverse-matches nil)
  (mapcar (lambda (item)
	   (let ((list-a (vscode-mode-find-all-items (car item) forward-commands))
		 (list-b (vscode-mode-find-all-items (car item) reverse-commands)))
	    (if (and (> (length list-a) 1) (> (length list-b) 1))
	     (push (see (cons (car list-a) (list (cdr list-a) (cdr list-b))) 0.0) all-matches)
	     (if (> (length list-a) 1)
	      (push (see (cons (car list-a) (list (cdr list-a))) 0.0) forward-matches)
	      (if (> (length list-b) 1)
	       (push (see (cons (car list-b) (list (cdr list-b))) 0.0) reverse-matches))))))
   forward-commands
   ;; reverse-commands
   )))

(defun vscode-mode-regenerate-matchmaking (&optional arg)
 ""
 (interactive "P")
 (if arg (vscode-mode-reset-commands))
 (vscode-mode-compute-vscode-style-keybindings)
 ;; all-matches
 ;; forward-matches
 ;; reverse-matches
 )

;; (vscode-mode-regenerate-matchmaking)

(provide 'vscode-keybindings-parser)

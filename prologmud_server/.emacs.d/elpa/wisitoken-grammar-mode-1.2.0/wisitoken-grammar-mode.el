;;; wisitoken-grammar-mode.el --- Major mode for editing WisiToken grammar files  -*- lexical-binding:t -*-

;; Copyright (C) 2017 - 2020  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages
;; Version: 1.2.0
;; package-requires: ((wisi "3.1.1") (emacs "25.0") (mmm-mode "0.5.7"))
;; url: http://www.nongnu.org/ada-mode/

;; This file is part of GNU Emacs.

;; wisitoken-grammar-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; wisitoken-grammar-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:

(require 'cl-lib)
(require 'mmm-mode)
(require 'xref)
(require 'wisi)
(require 'wisitoken_grammar_1-process)
(require 'wisi-process-parse)

(defgroup wisitoken-grammar nil
  "Major mode for editing Wisi grammar files in Emacs."
  :group 'languages)

(defcustom wisitoken-grammar-process-parse-exec "wisitoken_grammar_mode_parse.exe"
  ;; wisitoken_grammar.gpr uses .exe even on non-windows.
  "Name of executable to use for external process wisitoken-grammar parser,"
  :type 'string
  :group 'wisitoken-grammar)

(defvar wisitoken-grammar-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; ". 12" table) ;; ";;" comment start; default punctuation
    (modify-syntax-entry ?\n ">   " table) ;; comment end
    (modify-syntax-entry ?=  ".   " table) ;; default symbol
    (modify-syntax-entry ?*  ".   " table) ;; default symbol

    ;; WORKAROUND (see mmm github issue 130) Because mmm does not
    ;; limit syntax-ppss, we can't set ?\' to string here; that sees
    ;; ?\' in 'code' and 'action' blocks. In addition, we prefer
    ;; font-lock-constant-face for tokens. So we handle ?\' in the
    ;; parser.

    table))

(defvar wisitoken-grammar-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    ;; comment-dwim is in global map on M-;
    (define-key map "\C-c\C-f" 'wisi-show-parse-error)
    (define-key map "\C-c\C-m" 'wisitoken-grammar-mmm-parse)
    (define-key map [S-return] 'wisitoken-grammar-new-line)
    (define-key map "\C-c`"    'ada-show-secondary-error)
    map
  )  "Local keymap used for wisitoken-grammar mode.")

(define-key emacs-lisp-mode-map "\C-c\C-m" 'wisitoken-grammar-mmm-parse)

(defvar wisitoken-grammar-mode-menu (make-sparse-keymap "Wisi-Grammar"))
(easy-menu-define wisitoken-grammar-mode-menu wisitoken-grammar-mode-map "Menu keymap for Wisitoken Grammar mode"
  '("Wisi-Grammar"
    ["Goto declaration" xref-find-definitions t]
    ["mmm-ify action"   wisitoken-grammar-mmm-parse t]))

(cl-defstruct (wisitoken-grammar-parser (:include wisi-process--parser))
  ;; no new slots
  )

(cl-defmethod wisi-parse-format-language-options ((_parser wisitoken-grammar-parser))
  "")

(defun wisitoken-grammar-check-parens (sexp)
  "For `wisi-process--parse-Language'"
  ;; sexp is [Language index token-first token-last]

  ;; Check for missing parens in the action; otherwise only
  ;; checked at grammar generation time.
    (let* ((start (aref sexp 2))
	   (stop (1+ (aref sexp 3)))
	   (paren-depth-start (nth 0 (syntax-ppss start)))
	   (paren-depth-stop (nth 0 (syntax-ppss stop))))
      ;; We could check for paren-depth = 0 at start, but then we'd
      ;; get errors on every action after that.
      (unless (= paren-depth-start paren-depth-stop)
	(push
	 (make-wisi--parse-error
	  :pos (copy-marker start)
	  :message
	  (format "%s:%d: missing paren or bracket in %d %d"
		  (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		  ;; file-name can be nil during vc-resolve-conflict
		  (line-number-at-pos start)
		  start stop))
	 (wisi-parser-parse-errors wisi--parser))

	;; As of mmm version 0.5.7, mmm-parse-region calls
	;; font-lock-ensure, which calls us. So we can't do
	;; mmm-parse-region here. FIXME: fix mmm to do parse-region in
	;; syntax-propertize.
	)))

(defun wisitoken-grammar-in-action-or-comment ()
  ;; (info "(elisp) Parser State" "*info syntax-ppss*")
  (let* ((state (syntax-ppss))
	 (paren-depth (nth 0 state))
	 (done nil)
	 (result nil))
    (cond
     ((nth 3 state)
      ;; in string
      t)

     ((nth 4 state)
      ;; in comment
      t)

     ((> paren-depth 0)
      ;; check for action delimiters
      (save-excursion
	(while (not done)
	  (if (= ?% (char-before (nth 1 state)))
	      ;; in code, regexp, or action
	      (setq done t
		    result t)

	    ;; else go up one level
	    (setq state (syntax-ppss (1- (nth 1 state))))
	    )))
      result)
     )))

(defun wisitoken-grammar-find-begin (begin)
  "Starting at BEGIN, search backward for a parse start point."
  (goto-char begin)
  (cond
   ((wisi-search-backward-skip "^%[^({[]\\|:" #'wisitoken-grammar-in-action-or-comment)
    (when (looking-at ":")
      ;; Move back to before the nonterminal name
      (forward-comment (- (line-number-at-pos (point))))
      (skip-syntax-backward "w_"))
    (point))

   (t
    (point-min))
   ))

(defun wisitoken-grammar-find-end (end)
  "Starting at END, search forward for a parse end point."
  (goto-char end)
  (cond
   ((wisi-search-forward-skip "^%\\|;$" #'wisitoken-grammar-in-action-or-comment)
    (point))

   (t
    (point-max))
   ))

(cl-defmethod wisi-parse-expand-region ((_parser wisitoken-grammar-parser) begin end)
    (save-excursion
      (let ((begin-cand (wisitoken-grammar-find-begin begin))
	    (end-cand (wisitoken-grammar-find-end end)))
	(cons begin-cand end-cand)
	)))

(defun wisitoken-grammar-mmm-parse ()
  "If in action, call `mmm-parse-region' on it."
  (interactive)
  (save-excursion
    (let* ((begin (search-backward-regexp "%[({]" nil t))
	   (end   (when begin (search-forward-regexp "[)}]%" nil t))))
      (when (and begin end)
	(mmm-parse-region begin end)))
    ))

(defun wisitoken-grammar-new-line ()
  "If in comment, insert new comment line.
If in nonterminal, insert new production right hand side.
Otherwise insert a plain new line."
  (interactive)
  (if (nth 4 (syntax-ppss))
      ;; in comment
      (comment-indent-new-line)

    (let ((pos (point))
	  (cache (save-excursion (wisi-goto-statement-start))))
	(if (and cache
		 (eq (wisi-cache-nonterm cache) 'nonterminal)
		 (wisi-cache-end cache)
		 (> (wisi-cache-end cache) pos))
	    (progn
	      ;; in nonterminal
	      (insert "\n| ")
	      (indent-according-to-mode))

	  (newline-and-indent)
	  ))
    ))

(defun wisitoken-grammar-which-function ()
  "For `which-func-functions', `add-log-current-defun-function'."
  (wisi-validate-cache (point-min) (point-max) nil 'navigate)
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (wisi-cache-covers-pos 'navigate (point))
    (save-excursion
      (wisi-goto-statement-start)
      (wisi-next-name))))

(defun wisitoken-grammar-add-log-current-function ()
  "For `add-log-current-defun-function'; return name of current non-terminal or declaration."
  ;; add-log-current-defun is typically called with point at the start
  ;; of an ediff change section, which is before the start of the
  ;; declaration of a new item. So go to the end of the current line
  ;; first
  (save-excursion
    (end-of-line 1)
    (wisitoken-grammar-which-function)))

(defun wisitoken-grammar-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer."
  ;; (info "(elisp)Syntax Properties")
  ;;
  ;; called from `syntax-propertize', inside save-excursion with-silent-modifications
  ;; syntax-propertize-extend-region-functions is set to
  ;; syntax-propertize-wholelines by default.
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (goto-char start)
    (save-match-data
      (while (re-search-forward
	       "\\(%\\[\\)" ;; regexp begin
	      end t)
	(cond
	 ((match-beginning 1)
	  (let ((begin (match-beginning 1))
		(end (search-forward "]%")))
	    ;; allow single quotes in regexp to not mess up the rest
	    ;; of the buffer
	    (put-text-property begin end 'syntax-table '(11 . nil))
	    ))
	 ))
      )))

;;; mmm (multi-major-mode) integration

(defvar-local wisitoken-grammar-action-mode nil
  "Emacs major mode used for grammar actions, from ’%generate’ declaration.")

(defun wisitoken-grammar-mmm-action (_delim)
  "for :match-submode"
  wisitoken-grammar-action-mode)

(defvar-local wisitoken-grammar-code-mode nil
  "Emacs major mode used for code blocks, from ’%generate’ declaration.")

(defun wisitoken-grammar-mmm-code (_delim)
  "for :match-submode"
  wisitoken-grammar-code-mode)

(defun wisitoken-grammar-set-submodes ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "%generate +\\([A-Za-z_0-9]+\\) *\\([A-Za-z_0-9]+\\)?" (point-max) t)
	(cond
	 ((string-equal (match-string 1) "None")
	  (setq wisitoken-grammar-action-mode nil)
	  (setq wisitoken-grammar-code-mode nil))

	 ((string-equal (match-string 2) "Ada_Emacs")
	  (setq wisitoken-grammar-action-mode 'emacs-lisp-mode)
	  (setq wisitoken-grammar-code-mode 'ada-mode))

	 ((string-equal (match-string 2) "Ada")
	  (setq wisitoken-grammar-action-mode 'ada-mode)
	  (setq wisitoken-grammar-code-mode 'ada-mode))

	 (t
	  (error "unrecognized output language %s" (match-string 2)))
	 )

      ;; %generate not found; we can still support the grammar
      ;; statements, just not the actions.
      (setq wisitoken-grammar-action-mode 'nil))))

(mmm-add-classes
 '((wisi-action
    :match-submode wisitoken-grammar-mmm-action
    :face mmm-code-submode-face
    :front "%("
    :back ")%"
    :insert ((?a wisi-action nil @ "%(" @ "" _ "" @ ")%")))
   (wisi-code
    :match-submode wisitoken-grammar-mmm-code
    :face mmm-code-submode-face
    :front "%{"
    :back "}%"
    :insert ((?a wisi-code nil @ "%{" @ "" _ "" @ "}%")))
   ))

(add-to-list 'mmm-mode-ext-classes-alist '(wisitoken-grammar-mode nil wisi-action))
(add-to-list 'mmm-mode-ext-classes-alist '(wisitoken-grammar-mode nil wisi-code))

;;; xref integration
(defun wisitoken-grammar--xref-backend ()
  'wisitoken-grammar)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql wisitoken-grammar)))
  (wisi-xref-identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql wisitoken-grammar)))
  (wisi-names t nil))

(cl-defmethod xref-backend-definitions ((_backend (eql wisitoken-grammar)) identifier)
  (when (get-text-property 0 'xref-identifier identifier)
    ;; Identifier is from identifier-at-point; find declaration in completion table
    (let* ((table (wisi-names t nil))
	   (temp (try-completion identifier table)))
      (cond
       ((or (null temp)
	    (not (test-completion temp table)))
	(setq identifier (completing-read "decl: " table nil t identifier)))

       (t
	(setq identifier temp)))
      ))

  ;; Identifier is now from completion table, or nil
  (when identifier
    (string-match wisi-names-regexp identifier)
    (list (xref-make
	 (match-string 1 identifier)
	 (xref-make-file-location
	  (buffer-file-name) (string-to-number (match-string 2 identifier)) 0)))
    ))

;;; debug
(defun wisitoken-grammar-set-exec (exec-file)
  "Set EXEC-FILE for current and future wisitoken-grammar parsers."
  (interactive "f")
  (setq wisitoken-grammar-process-parse-exec exec-file)
  (wisi-process-parse-set-exec "wisitoken-grammar" exec-file))

;;;;
;;;###autoload
(define-derived-mode wisitoken-grammar-mode prog-mode "Wisi"
  "A major mode for Wisi grammar files."
  (set (make-local-variable 'syntax-propertize-function) 'wisitoken-grammar-syntax-propertize)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (set (make-local-variable 'comment-start) ";;")
  (set (make-local-variable 'comment-end) "") ;; setting this to \n causes errors
  (set (make-local-variable 'comment-use-syntax) t);; the automatic test for this does not use syntax-propertize
  (set (make-local-variable 'comment-start-skip) ";;*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'add-log-current-defun-function)
       #'wisitoken-grammar-add-log-current-function)

  (wisitoken-grammar-set-submodes)

  (add-hook 'xref-backend-functions #'wisitoken-grammar--xref-backend
	    nil ;; append
	    t)

  (wisi-setup
   :indent-calculate nil
   :post-indent-fail nil
   :parser (wisi-process-parse-get
	    (make-wisitoken-grammar-parser
	     :label "wisitoken-grammar"
	     :language-protocol-version "1"
	     :exec-file wisitoken-grammar-process-parse-exec
	     :face-table wisitoken_grammar_1-process-face-table
	     :token-table wisitoken_grammar_1-process-token-table
	     :language-action-table [wisitoken-grammar-check-parens]
	     )))

  ;; Our wisi parser does not fontify comments and strings, so tell
  ;; font-lock to do that.
  (setq font-lock-defaults
	'(nil ;; keywords
	  nil ;; keywords-only
	  ))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisitoken-grammar-mode))

;; Tie the mode to the defcustoms above.
(put 'wisitoken-grammar-mode 'custom-mode-group 'wisitoken-grammar)

(provide 'wisitoken-grammar-mode)
;;; wisitoken-grammar-mode.el ends here

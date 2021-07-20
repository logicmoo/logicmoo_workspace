;;; wisi-run-indent-test.el --- utils for automating indentation and casing tests
;;
;; Copyright (C) 2018 - 2020  Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'wisi-tests)
(require 'wisi-prj)

;; user can set these to t in an EMACSCMD
(defvar skip-cmds nil)
(defvar skip-reindent-test nil)
(defvar skip-recase-test nil)
(defvar skip-write nil)

(defun test-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun test-face (token face)
  "Test if all of TOKEN in next code line has FACE.
FACE may be a list."
  (save-excursion
    (when (test-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (condition-case err
	(search-forward token (line-end-position 5))
      (error
       (error "can't find '%s'" token)))

    (save-match-data
      (wisi-validate-cache (line-beginning-position) (line-end-position) nil 'face)
      (font-lock-ensure (line-beginning-position) (line-end-position)))
    
    ;; We don't use face-at-point, because it doesn't respect
    ;; font-lock-face set by the parser! And we want to check for
    ;; conflicts between font-lock-keywords and the parser.

    ;; font-lock-keywords sets 'face property, parser sets 'font-lock-face.

    ;; In emacs < 27, if we use (get-text-property (point) 'face), we
    ;; also get 'font-lock-face, but not vice-versa. So we have to use
    ;; text-properties-at to check for both.
    (let* ((token (match-string 0))
	   (props (text-properties-at (match-beginning 0)))
	   key
	   token-face)

      (cond
       ((plist-get props 'font-lock-face)
	(setq key 'font-lock-face)
	(setq token-face (plist-get props 'font-lock-face)))

       ((plist-get props 'face)
	(setq key 'face)
	(setq token-face (plist-get props 'face)))
       )

      (when (and (memq 'font-lock-face props)
		 (memq 'face props))
	(describe-text-properties (match-beginning 0))
	(error "mixed font-lock-keyword and parser faces for '%s'" token))

      (unless (not (text-property-not-all 0 (length token) key token-face token))
	(error "mixed faces, expecting %s for '%s'" face token))

      (unless (or (and (listp face)
		       (memq token-face face))
		  (eq token-face face))
	(error "found face %s, expecting %s for '%s'" token-face face token))
    )))

(defun test-face-1 (search token face)
  "Move to end of comment, search for SEARCH, call `test-face'."
  (save-excursion
    (when (test-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (search-forward search)
    (test-face token face)
    ))

(defun test-cache-class (token class)
  "Test if TOKEN in next code line has wisi-cache with class CLASS."
  (save-excursion
    (wisi-validate-cache (line-beginning-position 0) (line-end-position 3) nil 'navigate)
    (beginning-of-line); forward-comment doesn't move if inside a comment!
    (forward-comment (point-max))
    (condition-case err
	(search-forward token (line-end-position 5))
      (error
       (error "can't find '%s'" token)))

    (let ((cache (get-text-property (match-beginning 0) 'wisi-cache)))

      (unless cache (error "no cache"))
      (unless (eq (wisi-cache-class cache) class)
	(error "expecting class %s, found '%s'" class (wisi-cache-class cache)))
    )))

(defun test-cache-containing (containing contained)
  "Test if CONTAINING in next code line has wisi-cache with that contains CONTAINED."
  (save-excursion
    (wisi-validate-cache (line-beginning-position 0) (line-end-position 3) nil 'navigate)
    (beginning-of-line)
    (forward-comment (point-max))
    (let (containing-pos contained-cache)
      (search-forward containing (line-end-position 5))
      (setq containing-pos (match-beginning 0))

      (search-forward contained (line-end-position 5))
      (setq contained-cache (get-text-property (match-beginning 0) 'wisi-cache))

      (unless contained-cache (error "no cache on %s" contained))
      (unless (= containing-pos (wisi-cache-containing contained-cache))
	(error "expecting %d, got %d" containing-pos (wisi-cache-containing contained-cache)))
    )))

(defvar test-refactor-markers nil
  "Stores positions altered by `test-refactor-1' for `test-refactor-2'.
Each item is a list (ACTION PARSE-BEGIN PARSE-END EDIT-BEGIN)")

(defun test-refactor-1 (action inverse-action search-string refactor-string)
  (beginning-of-line)
  (forward-comment (point-max)) ;; forward-comment does not work from inside comment
  (search-forward search-string (line-end-position 7))
  (wisi-validate-cache (line-end-position -7) (line-end-position 7) t 'navigate)
  (search-forward refactor-string (line-end-position 7))
  (let* ((edit-begin (match-beginning 0))
	 (cache (wisi-goto-statement-start))
	 (parse-begin (point))
	 (parse-end (wisi-cache-end cache)))
    (setq parse-end (+ parse-end (wisi-cache-last (wisi-get-cache (wisi-cache-end cache)))))
    (push (list
	   inverse-action
	   (copy-marker parse-begin nil)
	   (copy-marker parse-end nil)
	   (copy-marker edit-begin nil))
	  test-refactor-markers)
    (wisi-refactor wisi--parser action parse-begin parse-end edit-begin)
    ))

(defun test-refactor-inverse ()
  "Reverse refactors done by recent set of `test-refactor-1'."
  (save-excursion
    (condition-case-unless-debug nil
	(dolist (item test-refactor-markers)
	  (wisi-refactor wisi--parser
			 (nth 0 item)
			 (marker-position (nth 1 item))
			 (marker-position (nth 2 item))
			 (marker-position (nth 3 item))))
      (error nil))
    (setq test-refactor-markers nil)))

(defun run-test-here ()
  "Run an indentation and casing test on the current buffer."
  (interactive)
  (setq indent-tabs-mode nil)
  (setq jit-lock-context-time 0.0);; for test-face

  ;; Test files use wisi-prj-select-cached to parse and select a project file.
  (setq project-find-functions (list #'wisi-prj-current-cached))
  (setq xref-backend-functions (list #'wisi-prj-xref-backend))


  (let ((error-count 0)
	(test-buffer (current-buffer))
	cmd-line
	last-result last-cmd expected-result)
    ;; Look for EMACS* comments in the file:
    ;;
    ;; EMACSCMD: <form>
    ;;    Executes the lisp form inside a save-excursion, saves the result as a lisp object.
    ;;
    ;; EMACSRESULT: <form>
    ;;    point is moved to end of line, <form> is evaluated inside
    ;;    save-excursion and compared (using `equal') with the result
    ;;    of the previous EMACSCMD, and the test fails if they don't
    ;;    match.
    ;;
    ;; EMACSRESULT_START:<first list element>
    ;; EMACSRESULT_ADD:  <list element>
    ;; EMACSRESULT_FINISH:
    ;;    build a list, compare it to the result of the previous EMACSCMD.
    ;;
    ;; EMACS_SKIP_UNLESS: <form>
    ;;   skip entire test if form evals nil
    ;;
    ;; EMACSDEBUG: <form>
    ;;    Eval form, display result. Also used for setting breakpoint.

    (goto-char (point-min))
    (while (and (not skip-cmds)
		(re-search-forward (concat comment-start "EMACS\\([^:]+\\):") nil t))
      (cond
       ((string= (match-string 1) "CMD")
	(looking-at ".*$")
	(save-excursion
	  (setq cmd-line (line-number-at-pos)
		last-cmd (match-string 0)
		last-result
		(condition-case-unless-debug err
		    (eval (car (read-from-string last-cmd)))
		  (error
		     (setq error-count (1+ error-count))
		     (message "%s:%d: command: %s"
			      (buffer-file-name) cmd-line last-cmd)
		     (message "%s:%d: %s: %s"
			      (buffer-file-name)
			      (line-number-at-pos)
			      (car err)
			      (cdr err))))
		)
	  ;; save-excursion does not preserve mapping of buffer to
	  ;; window, but some tests depend on that. For example,
	  ;; execute-kbd-macro doesn’t work properly if current buffer
	  ;; is not visible..
	  (pop-to-buffer test-buffer)))

       ((string= (match-string 1) "RESULT")
	(looking-at ".*$")
	(setq expected-result (save-excursion (end-of-line 1) (eval (car (read-from-string (match-string 0))))))
	(unless (equal expected-result last-result)
	  (setq error-count (1+ error-count))
	  (message
	   (concat
	    (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
	    (format "Result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
		    last-cmd
		    last-result
		    expected-result)
	    ))))

       ((string= (match-string 1) "RESULT_START")
	(looking-at ".*$")
	(setq expected-result (list (save-excursion (end-of-line 1) (eval (car (read-from-string (match-string 0))))))))

       ((string= (match-string 1) "RESULT_ADD")
	(looking-at ".*$")
	(let ((val (save-excursion (end-of-line 1)
				   (eval (car (read-from-string (match-string 0)))))))
	  (when val
	    (setq expected-result (append expected-result (list val))))))

       ((string= (match-string 1) "RESULT_FINISH")
	(unless (equal (length expected-result) (length last-result))
	  (setq error-count (1+ error-count))
	  (message
	   (concat
	    (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
	    (format "Length of result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
		    last-cmd
		    (length last-result)
		    (length expected-result)))))

	(let ((i 0))
	  (while (< i (length expected-result))
	    (unless (equal (nth i expected-result) (nth i last-result))
	      (setq error-count (1+ error-count))
	      (message
	       (concat
		(format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
		(format "Nth (%d) result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
			i
			last-cmd
			(nth i last-result)
			(nth i expected-result))
		)))
	    (setq i (1+ i)))))

       ((string= (match-string 1) "_SKIP_UNLESS")
	(looking-at ".*$")
	(unless (eval (car (read-from-string (match-string 0))))
	  (setq skip-cmds t)
	  (setq skip-reindent-test t)
	  (setq skip-recase-test t)
	  ;; We don’t set ‘skip-write’ t here, so the *.diff Make target succeeds.
	  ))

       ((string= (match-string 1) "DEBUG")
	(looking-at ".*$")
	(message "DEBUG: %s:%d %s"
		 (current-buffer)
		 (line-number-at-pos)
		 (save-excursion
		  (eval (car (read-from-string (match-string 0)))))))

       (t
	(setq error-count (1+ error-count))
	(error (concat "Unexpected EMACS test command " (match-string 1))))))

    (when (> error-count 0)
      (error
       "%s:%d: aborting due to previous errors (%d)"
       (buffer-file-name) (line-number-at-pos (point)) error-count))
    )

  (unless skip-reindent-test
    ;; Reindent the buffer
    (message "indenting")

    ;; first unindent; if the indentation rules do nothing, the test
    ;; would pass, otherwise!  Only unindent by 1 column, so comments
    ;; not currently in column 0 are still not in column 0, in case
    ;; the mode supports a special case for comments in column 0.
    (indent-rigidly (point-min) (point-max) -1)

    ;; indent-region uses save-excursion, so we can't goto an error location
    (indent-region (point-min) (point-max))

    ;; Cleanup the buffer; indenting often leaves trailing whitespace;
    ;; files must be saved without any.
    (delete-trailing-whitespace)
    )

  (when (and wisi-auto-case (not skip-recase-test))
    (message "casing")
    (wisi-case-adjust-buffer))
  )

(defvar cl-print-readably); cl-print.el, used by edebug

(defun large-frame ()
  (interactive)
  (modify-frame-parameters
      nil
      (list
       (cons 'width 120) ;; characters; fringe extra
       (cons 'height 71) ;; characters
       (cons 'left 0) ;; pixels
       (cons 'top 0))))
(define-key global-map "\C-cp" 'large-screen)

(defun run-test (file-name)
  "Run an indentation and casing test on FILE-NAME."
  (interactive "f")

  (package-initialize) ;; for uniquify-files

  ;; Let edebug display strings full-length, and show internals of records
  (setq cl-print-readably t)

  ;; we'd like to run emacs from a makefile as:
  ;;
  ;; emacs -Q --batch -l runtest.el -f run-test-here <filename>
  ;;
  ;; However, the function specified with -f is run _before_
  ;; <filename> is visited. So we try this instead:
  ;;
  ;; emacs -Q --batch -l runtest.el --eval '(run-test "<filename>")'
  ;;
  ;; And then we discover that processes spawned with start-process
  ;; don't run when emacs is in --batch mode. So we try this:
  ;;
  ;; emacs -Q -l runtest.el --eval '(progn (run-test "<filename>")(kill-emacs))'
  ;;
  ;; Then we have problems with font lock defaulting to jit-lock; that
  ;; screws up font-lock tests because the test runs before jit-lock
  ;; does. This forces default font-lock, which fontifies the whole
  ;; buffer when (font-lock-fontify-buffer) is called, which tests
  ;; that rely on font-lock do explicitly.
  (setq font-lock-support-mode nil)

  (setq xref-prompt-for-identifier nil)

  (let ((dir default-directory))
    (find-file file-name) ;; sets default-directory

    (run-test-here)

    (unless skip-write
      ;; Write the result file; makefile will diff.
      (when skip-reindent-test
	;; user sets skip-reindent-test when testing interactive editing
	;; commands, so the diff would fail. Revert to the original file,
	;; save a copy of that.
	(revert-buffer t t))

      (delete-trailing-whitespace)
      (write-file (concat dir (file-name-nondirectory file-name) ".tmp")) )
    )
  )

(provide 'wisi-run-indent-test)
;; end of file

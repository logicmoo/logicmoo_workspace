;;; wisi.el --- Utilities for implementing an indentation/navigation engine using a generalized LALR parser -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012 - 2020  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;;  indentation
;;  navigation
;; Version: 3.1.3
;; package-requires: ((emacs "25.0") (seq "2.20"))
;; URL: http://stephe-leake.org/ada/wisitoken.html
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
;;

;;; Commentary:

;;;; History: see NEWS-wisi.text
;;
;;;; Design:
;;
;; 'wisi' was originally short for "wisent indentation engine", but
;; now is just a name. wisi was developed to support Emacs ada-mode
;; 5.0 indentation, font-lock, and navigation, which are parser based.
;;
;; The approach to indenting a given token is to parse the buffer,
;; computing a delta indent at each parse action.
;;
;; The parser actions also cache face and navigation information
;; as text properties on tokens in statements.
;;
;; The three reasons to run the parser (indent, face, navigate) occur
;; at different times (user indent, font-lock, user navigate), so only
;; the relevant parser actions are run.
;;
;; Parsing can be noticeably slow in large files, so sometimes we do a
;; partial parse, and keep a list of parsed regions.
;;
;; Since we have a cache (the text properties), we need to consider
;; when to invalidate it.  Ideally, we invalidate only when a change
;; to the buffer would change the result of a parse that crosses that
;; change, or starts after that change.  Changes in whitespace
;; (indentation and newlines) do not affect an Ada parse.  Other
;; languages are sensitive to newlines (Bash for example) or
;; indentation (Python).  Adding comments does not change a parse,
;; unless code is commented out.
;;
;; For navigate, we expect fully accurate results, and can tolerate
;; one initial delay, so we always parse the entire file.
;;
;; For font-lock, we only parse the portion of the file requested by
;; font-lock, so we keep a list of regions, and edit that list when
;; the buffer is changed..
;;
;; For indenting, we expect fast results, and can tolerate some
;; inaccuracy until the editing is done, so we allow partial parse. We
;; cache the indent for each line in a text property on the newline
;; char preceding the line. `wisi-indent-region' sets the cache on all
;; the lines computed (part of the buffer in large files), but
;; performs the indent only on the lines in the indent
;; region. Subsequent calls to `wisi-indent-region' apply the cached
;; indents. Non-whitespace edits to the buffer invalidate the indent
;; caches in the edited region and after. Since we can do partial
;; parse, we keep a list of parsed regions.
;;
;; See `wisi--post-change' for the details of what we check for
;; invalidating.
;;
;;;; Choice of grammar compiler and parser
;;
;; There are two other parsing engines available in Emacs:
;;
;; - SMIE
;;
;;   We don't use this because it is designed to parse small snippets
;;   of code. For Ada indentation, we always need to parse the entire
;;   buffer.
;;
;; - semantic
;;
;;   The Ada grammar as given in the Ada language reference manual is
;;   not LALR(1). So we use a generalized parser. In addition, the
;;   semantic lexer is more complex, and gives different information
;;   than we need. Finally, the semantic parser does not support error
;;   correction, and thus fails in most editing situations.
;;
;; We use the WisiToken tool wisi-bnf-generate to compile BNF or EBNF
;; to Ada source, See ada-mode.info and wisi.info for more information
;; on the developer tools used for ada-mode and wisi.
;;
;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'semantic/lex)
(require 'wisi-parse-common)
(require 'wisi-fringe)
(require 'xref)

(defcustom wisi-size-threshold most-positive-fixnum
  "Max size (in characters) for using wisi parser results for anything."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-size-threshold)

(defcustom wisi-indent-context-lines 0
  "Minimum number of lines before point to include in a parse for indent.
Increasing this will give better results when in the middle of a
deeply nested statement, but worse in some situations."
  :type 'integer
  :group 'wisi
  :safe 'integerp)

(defcustom wisi-disable-face nil
  "When non-nil, `wisi-setup' does not enable use of parser for font-lock.
Useful when debugging parser or parser actions."
  :type 'boolean
  :group 'wisi
  :safe 'booleanp)

(defconst wisi-error-buffer-name "*wisi syntax errors*"
  "Name of buffer for displaying syntax errors.")

(defvar wisi-error-buffer nil
  "Buffer for displaying syntax errors.")

(defvar wisi-inhibit-parse nil
  "When non-nil, don't run the parser.
Language code can set this non-nil when syntax is known to be
invalid temporarily, or when making lots of changes.")

(defun wisi-safe-marker-pos (pos)
  "Return an integer buffer position from POS, an integer or marker"
  (cond
   ((markerp pos)
    (marker-position pos))

   (t pos)))

;;;; misc

(defun wisi-in-paren-p (&optional parse-result)
  "Return t if point is inside a pair of parentheses.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (> (nth 0 (or parse-result (syntax-ppss))) 0))

(defun wisi-pos-in-paren-p (pos)
  "Return t if POS is inside a pair of parentheses."
  (save-excursion
    (> (nth 0 (syntax-ppss pos)) 0)))

(defun wisi-same-paren-depth-p (pos1 pos2)
  "Return t if POS1 is at same parentheses depth as POS2."
  (= (nth 0 (syntax-ppss pos1)) (nth 0 (syntax-ppss pos2))))

(defun wisi-goto-open-paren (&optional offset parse-result)
  "Move point to innermost opening paren surrounding current point, plus OFFSET.
Throw error if not in paren.  If PARSE-RESULT is non-nil, use it
instead of calling `syntax-ppss'."
  (goto-char (+ (or offset 0) (nth 1 (or parse-result (syntax-ppss))))))

(defun wisi-in-comment-p (&optional parse-result)
  "Return t if inside a comment.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (nth 4 (or parse-result (syntax-ppss))))

(defun wisi-in-string-p (&optional parse-result)
  "Return t if point is inside a string.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (nth 3 (or parse-result (syntax-ppss))))

(defun wisi-in-string-or-comment-p (&optional parse-result)
  "Return t if inside a comment or string.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (setq parse-result (or parse-result (syntax-ppss)))
  (or (wisi-in-string-p parse-result) (wisi-in-comment-p parse-result)))

(defun wisi-indent-newline-indent ()
  "insert a newline, indent the old and new lines."
  (interactive "*")
  ;; point may be in the middle of a word, so insert newline first,
  ;; then go back and indent.
  (insert "\n")
  (forward-char -1)
  (funcall indent-line-function)
  (forward-char 1)
  (funcall indent-line-function))

;;;; token info cache

(defvar-local wisi-parse-failed nil
  "Non-nil when last parse failed - cleared when parse succeeds.")

(defvar-local wisi--parse-try
  (list
   (cons 'face t)
   (cons 'navigate t)
   (cons 'indent t))
  "Non-nil when parse is needed because text has changed - cleared when parse succeeds.")

(defun wisi-parse-try (&optional parse-action)
  (cdr (assoc (or parse-action wisi--parse-action) wisi--parse-try)))

(defun wisi-set-parse-try (value &optional parse-action)
  (setcdr (assoc (or parse-action wisi--parse-action) wisi--parse-try) value))

(defvar-local wisi--last-parse-region
  (list
   (cons 'face nil)
   (cons 'navigate nil)
   (cons 'indent nil))
  "Last region on which parse was requested.")

(defun wisi-last-parse-region (&optional parse-action)
  (cdr (assoc (or parse-action wisi--parse-action) wisi--last-parse-region)))

(defun wisi-set-last-parse-region (begin end parse-action)
  (setcdr (assoc parse-action wisi--last-parse-region) (cons begin end)))

(defvar-local wisi--cached-regions
  (list
   (cons 'face nil)
   (cons 'navigate nil)
   (cons 'indent nil))
  "Alist of lists of regions in buffer where parser text properties are valid.
Regions in a list are in random order.")

(defun wisi--contained-region (begin end region)
  "Non-nil if BEGIN and END (buffer positions) are both contained in REGION (a cons of positions)."
  ;; We assume begin < end
  (and (<= (car region) begin)
       (<= end (cdr region))))

(defun wisi--contained-pos (pos region)
  "Non-nil if POS (a buffer position) is contained in REGION (a cons of positions)."
  (and (<= (car region) pos)
       (<= pos (cdr region))))

(defun wisi-cache-covers-region (begin end &optional parse-action)
  "Non-nil if BEGIN END is contained in a parsed region."
  (let ((region-list (cdr (assoc (or parse-action wisi--parse-action) wisi--cached-regions)))
	region)
    (while (and region-list
		(marker-buffer (caar region-list)) ;; this can fail after editing during ediff-regions.
		(marker-buffer (cdar region-list))
		(not (wisi--contained-region begin end (car region-list))))
      (pop region-list))

    (when region-list
      ;; return a nice value for verbosity in wisi-validate-cache
      (setq region (car region-list))
      (cons (marker-position (car region)) (marker-position (cdr region))))))

(defun wisi-cache-covers-pos (parse-action pos)
  "Non-nil if POS is contained in a PARSE-ACTION parsed region."
  (let ((region-list (cdr (assoc parse-action wisi--cached-regions))))
    (while (and region-list
		(not (wisi--contained-pos pos (car region-list))))
      (pop region-list))

    (when region-list
      t)))

(defun wisi-cache-contains-pos (parse-action pos)
  "Non-nil if POS is at or before the end of any PARSE-ACTION parsed region."
  (let ((region-list (cdr (assoc parse-action wisi--cached-regions)))
	result)
    (while (and (not result) region-list)
      (when (<= pos (cdr (car region-list)))
	(setq result t))
      (pop region-list))

    result))

(defun wisi-cache-set-region (region)
  "Set the cached region list for `wisi--parse-action' to REGION."
  (setcdr (assoc wisi--parse-action wisi--cached-regions)
	  (list (cons (copy-marker (car region))
		      (copy-marker (cdr region))))))

(defun wisi-cache-add-region (region)
  "Add REGION to the cached region list for `wisi--parse-action'."
  (push (cons (copy-marker (car region))
	      (copy-marker (cdr region)))
	(cdr (assoc wisi--parse-action wisi--cached-regions))))

(defun wisi-cache-delete-regions-after (parse-action pos)
  "Delete any PARSE-ACTION parsed region at or after POS.
Truncate any region that overlaps POS."
  (let ((region-list (cdr (assoc parse-action wisi--cached-regions)))
	result)
    (while (and (not result) region-list)
      (cond
       ((and (> pos (car (car region-list)))
	     (<= pos (cdr (car region-list))))
	;; region contains POS; keep truncated
	(push (cons (car (car region-list)) (copy-marker pos)) result))

       ((> pos (car (car region-list)))
	;; region is entirely before POS; keep
	(push (car region-list) result))

       ;; else region is entirely after POS; delete
       )

      (pop region-list))
    (setcdr (assoc parse-action wisi--cached-regions) result)
    ))

(defun wisi--delete-face-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(font-lock-face nil)))
  (if (= after (point-min))
      (setcdr (assoc 'face wisi--cached-regions) nil)
    (wisi-cache-delete-regions-after 'face after)))

(defun wisi--delete-navigate-cache (after)
  (with-silent-modifications
    ;; This text property is 'wisi-cache', not 'wisi-navigate', for
    ;; historical reasons.
    (remove-text-properties after (point-max) '(wisi-cache nil wisi-name nil)))
  (if (= after (point-min))
      (setcdr (assoc 'navigate wisi--cached-regions) nil)
    (wisi-cache-delete-regions-after 'navigate after)))

(defun wisi--delete-indent-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-indent nil)))
  (if (= after (point-min))
      (setcdr (assoc 'indent wisi--cached-regions) nil)
    (wisi-cache-delete-regions-after 'indent after)))

(defun wisi-invalidate-cache (action after)
  "Invalidate ACTION caches for the current buffer from AFTER to end of buffer."
  (cond
   ((= after (point-min))
    (cond
     ((eq 'face action)
      (wisi--delete-face-cache after))

     ((eq 'navigate action)
      (wisi--delete-navigate-cache after))

     ((eq 'indent action)
      (wisi--delete-indent-cache after))
     ))

   ((wisi-cache-contains-pos action after)
    (when (> wisi-debug 0) (message "wisi-invalidate-cache %s:%s:%d" action (current-buffer) after))
    (cond
     ((eq 'face action)
      (wisi--delete-face-cache after))

     ((eq 'navigate action)
      (when (wisi-cache-covers-pos 'navigate after)
	;; We goto statement start to ensure that motion within nested
	;; structures is properly done (ie prev/next on ’elsif’ is not
	;; set by wisi-motion-action if already set by a lower level
	;; statement). We don’t do it for ’face or ’indent, because that
	;; might require a parse, and they don’t care about nested
	;; structures.
	(save-excursion
	  (goto-char after)

	  ;; This is copied from ‘wisi-goto-statement-start’; we can’t
	  ;; call that because it would call ‘wisi-validate-cache’,
	  ;; which would call ‘wisi-invalidate-cache’; infinite loop.
	  ;; If this needed a navigate parse to succeed, we would not
	  ;; get here.
	  (let ((cache (or (wisi-get-cache (point))
			   (wisi-backward-cache))))
	    (cond
	     ((null cache)
	      ;; at bob
	      nil)

	     ((eq 'statement-end (wisi-cache-class cache))
	      ;; If the change did affect part of a structure statement,
	      ;; this is a lower level statement. Otherwise, we are
	      ;; invalidating more than necessary; not a problem.
	      (wisi-goto-start cache)
	      (setq cache (wisi-backward-cache))
	      (when cache ;; else bob
		(wisi-goto-start cache)))

	     (t
	      (wisi-goto-start cache))
	     ))

	  (setq after (point))))
      (wisi--delete-navigate-cache after))

     ((eq 'indent action)
      ;; The indent cache is stored on newline before line being
      ;; indented. We delete that, because changing text on a line can
      ;; change the indent of that line.
      (setq after
	    (save-excursion
	      (goto-char after)
	      (line-beginning-position)))
      (wisi--delete-indent-cache (max 1 (1- after))))
     )
    )))

(defun wisi-reset-parser ()
  "Force a parse."
  (interactive)
  (syntax-ppss-flush-cache (point-min)) ;; necessary after edit during ediff-regions
  (setq wisi--cached-regions ;; necessary instead of wisi-invalidate after ediff-regions
	(list
	 (cons 'face nil)
	 (cons 'navigate nil)
	 (cons 'indent nil)))
  (wisi-set-parse-try t 'indent)
  (wisi-set-parse-try t 'face)
  (wisi-set-parse-try t 'navigate)
  (wisi-set-last-parse-region (point-min) (point-min) 'indent)
  (wisi-set-last-parse-region (point-min) (point-min) 'face)
  (wisi-set-last-parse-region (point-min) (point-min) 'navigate)
  (wisi-fringe-clean))

;; wisi--change-* keep track of buffer modifications.
;; If wisi--change-end comes before wisi--change-beg, it means there were
;; no modifications.
(defvar-local wisi--change-beg most-positive-fixnum
  "First position where a change may have taken place.")

(defvar-local wisi--change-end nil
  "Marker pointing to the last position where a change may have taken place.")

(defvar-local wisi--deleted-syntax nil
  "Worst syntax class of characters deleted in changes.
One of:
nil - no deletions since reset
0   - only whitespace or comment deleted
2   - some other syntax deleted

Set by `wisi-before-change', used and reset by `wisi--post-change'.")

(defvar-local wisi-indenting-p nil
  "Non-nil when `wisi-indent-region' is actively indenting.
Used to ignore whitespace changes in before/after change hooks.")

(defvar-local wisi--last-parse-action nil
  "Value of `wisi--parse-action' when `wisi-validate-cache' was last run.")

(defun wisi-before-change (begin end)
  "For `before-change-functions'."
  ;; begin . (1- end) is range of text being deleted
  (unless wisi-indenting-p
    ;; We set wisi--change-beg, -end even if only inserting, so we
    ;; don't have to do it again in wisi-after-change.
    (setq wisi--change-beg (min wisi--change-beg begin))

    ;; `buffer-base-buffer' deals with edits in indirect buffers
    ;; created by ediff-regions-*

    (cond
     ((null wisi--change-end)
      (setq wisi--change-end (make-marker))
      (set-marker wisi--change-end end (or (buffer-base-buffer) (current-buffer))))

     ((> end wisi--change-end)
      (set-marker wisi--change-end end (or (buffer-base-buffer) (current-buffer))))
     )

    (unless (= begin end)
      (cond
       ((or (null wisi--deleted-syntax)
	    (= 0 wisi--deleted-syntax))
	(save-excursion
	  (if (or (nth 4 (syntax-ppss begin)) ; in comment, moves point to begin
		  (= end (skip-syntax-forward " " end)));; whitespace
	      (setq wisi--deleted-syntax 0)
	    (setq wisi--deleted-syntax 2))))

       (t
	;; wisi--deleted-syntax is 2; no change.
	)
       ))))

(defun wisi-after-change (begin end _length)
  "For `after-change-functions'"
  ;; begin . end is range of text being inserted (empty if equal);
  ;; length is the size of the deleted text.

  ;; Remove caches on inserted text, which could have caches from
  ;; anywhere, and are in any case invalid.

  ;; If the insertion changes a word that has wisi fontification,
  ;; remove fontification from the entire word, so it is all
  ;; refontified consistently.

  (let (word-begin word-end)
    (save-excursion
      (goto-char end)
      (skip-syntax-forward "w_")
      (setq word-end (point))
      (goto-char begin)
      (skip-syntax-backward "w_")
      (setq word-begin (point)))
    (if (get-text-property word-begin 'font-lock-face)
	(with-silent-modifications
	  (remove-text-properties
	   word-begin word-end
	   '(font-lock-face nil wisi-cache nil wisi-indent nil fontified nil)))

      ;; No point in removing
      ;; 'fontified here; that's already handled by jit-lock.
      (with-silent-modifications
	(remove-text-properties
	 begin end
	 '(font-lock-face nil wisi-cache nil wisi-indent nil))))
    ))

(defun wisi--post-change (begin end)
  "Update wisi text properties for changes in region BEG END."
  ;; (syntax-ppss-flush-cache begin) is in before-change-functions

  (save-excursion
    (let ((need-invalidate t)
	  (done nil)
	  ;; non-nil if require a parse because the syntax may have
	  ;; changed.

	  (begin-state (syntax-ppss begin))
	  (end-state (syntax-ppss end)))
	  ;; (info "(elisp)Parser State")
	  ;; syntax-ppss has moved point to "end"; might be eob.

      ;; consider deletion
      (cond
       ((null wisi--deleted-syntax)
	;; no deletions
	)

       ((= 0 wisi--deleted-syntax)
	;; Only deleted whitespace; may have joined two words
	(when
	    (and (= begin end) ;; no insertions
		 (or
		  (= (point-min) begin)
		  (= 0 (syntax-class (syntax-after (1- begin))))
		  (= (point-max) end)
		  (= 0 (syntax-class (syntax-after end)))))
	  ;; More whitespace on at least one side of deletion; did not
	  ;; join two words.
	  (setq need-invalidate nil)
	  (setq done t)
	  ))

       (t
	;; wisi--deleted-syntax is 2; need invalidate and parse for all
	;; parse actions
	(setq done t)
	))

      (setq wisi--deleted-syntax nil)

      (unless done
	;; consider insertion
	(cond
	 ((= begin end)
	  ;; no insertions
	  nil)

	 ((and
	   (nth 3 begin-state);; in string
	   (nth 3 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state)));; no intervening non-string
	  (setq need-invalidate nil))

	 ((and
	   (nth 4 begin-state) ;; in comment
	   (nth 4 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state))) ;; no intervening non-comment

	  (if (and
	       (= 11 (car (syntax-after begin)))
	       (progn (goto-char begin)
		      (skip-syntax-backward "<")
		      (not (= (point) begin))))

	      ;; Either inserted last char of a multi-char comment
	      ;; start, or inserted extra comment-start chars.
	      (setq need-invalidate begin)
	    (setq need-invalidate nil)))

	 ((and
	   (or
	    (= (point-min) begin)
	    (= 0 (syntax-class (syntax-after (1- begin)))); whitespace
	    (= (point-max) end)
	    (= 0 (syntax-class (syntax-after end))))
	   (progn
	     (goto-char begin)
	     (= (- end begin) (skip-syntax-forward " " end))
	     ))
	  ;; Inserted only whitespace, there is more whitespace on at
	  ;; least one side, and we are not in a comment or string
	  ;; (checked above).  This may affect indentation, but not
	  ;; the indentation cache.
	  (setq need-invalidate nil))
	 ))

      (when need-invalidate
	(wisi-set-parse-try t 'face)
	(wisi-set-parse-try t 'navigate)
	(wisi-set-parse-try t 'indent)

	(wisi-invalidate-cache 'face begin)
	(wisi-invalidate-cache 'navigate begin)
	(wisi-invalidate-cache 'indent begin))
      )))

(defun wisi-goto-error ()
  "Move point to position in last error message (if any)."
  (cond
   ((wisi-parser-parse-errors wisi--parser)
    (let ((data (car (wisi-parser-parse-errors wisi--parser))))
      (cond
       ((wisi--parse-error-pos data)
	(push-mark)
	(goto-char (wisi--parse-error-pos data)))

       ((string-match ":\\([0-9]+\\):\\([0-9]+\\):" (wisi--parse-error-message data))
	(let* ((msg (wisi--parse-error-message data))
	       (line (string-to-number (match-string 1 msg)))
	       (col (string-to-number (match-string 2 msg))))
	  (push-mark)
	  (goto-char (point-min))
	  (condition-case nil
	      (progn
		;; line can be wrong if parser screws up, or user edits buffer
		(forward-line (1- line))
		(forward-char col))
	    (error
	     ;; just stay at eob.
	     nil))))
       )))
   ((wisi-parser-lexer-errors wisi--parser)
    (push-mark)
    (goto-char (wisi--lexer-error-pos (car (wisi-parser-lexer-errors wisi--parser)))))
   ))

(defun wisi-show-parse-error ()
  "Show current wisi-parse errors."
  (interactive)
  (cond
   ((or (wisi-parser-lexer-errors wisi--parser)
	(wisi-parser-parse-errors wisi--parser))
    (if (and (= 1 (+ (length (wisi-parser-lexer-errors wisi--parser))
		     (length (wisi-parser-parse-errors wisi--parser))))
	     (or (and (wisi-parser-parse-errors wisi--parser)
		      (not (wisi--parse-error-repair (car (wisi-parser-parse-errors wisi--parser)))))
		 (and (wisi-parser-lexer-errors wisi--parser)
		      (not (wisi--lexer-error-inserted (car (wisi-parser-lexer-errors wisi--parser)))))))
	;; There is exactly one error; if there is error correction
	;; information, use a ’compilation’ buffer, so
	;; *-fix-compiler-error will call
	;; wisi-repair-error. Otherwise, just position cursor at
	;; error.
	(progn
	  (wisi-goto-error)
	  (message (or (and (wisi-parser-parse-errors wisi--parser)
			    (wisi--parse-error-message (car (wisi-parser-parse-errors wisi--parser))))
		       (and (wisi-parser-lexer-errors wisi--parser)
			    (wisi--lexer-error-message (car (wisi-parser-lexer-errors wisi--parser)))))
		   ))

      ;; else show all errors in a ’compilation’ buffer
      (setq wisi-error-buffer (get-buffer-create wisi-error-buffer-name))

      (let ((lexer-errs (sort (cl-copy-seq (wisi-parser-lexer-errors wisi--parser))
			      (lambda (a b) (< (wisi--parse-error-pos a) (wisi--parse-error-pos b)))))
	    (parse-errs (sort (cl-copy-seq (wisi-parser-parse-errors wisi--parser))
			      (lambda (a b) (< (wisi--parse-error-pos a) (wisi--parse-error-pos b)))))
	    (dir default-directory))
	(with-current-buffer wisi-error-buffer
	  (setq window-size-fixed nil)
	  (compilation-mode)
	  (setq-local compilation-search-path (list dir))
	  (setq default-directory dir)
	  (setq next-error-last-buffer (current-buffer))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  ;; compilation-nex-error-function assumes there is not an
	  ;; error at point-min, so we need a comment.
	  (insert "wisi syntax errors")
	  (newline)
	  (dolist (err lexer-errs)
	    (insert (wisi--lexer-error-message err))
	    (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'wisi-error-data err)
	    (newline 2))
	  (dolist (err parse-errs)
	    (insert (wisi--parse-error-message err))
	    (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'wisi-error-data err)
	    (newline 2))
	  (compilation--flush-parse (point-min) (point-max))
	  (compilation--ensure-parse (point-max))
	  (when compilation-filter-hook
	    (let ((compilation-filter-start (point-min)))
	      (run-hooks 'compilation-filter-hook)))

	  (setq buffer-read-only t)
	  (goto-char (point-min)))

	(let ((win (display-buffer
		    wisi-error-buffer
		    (cons #'display-buffer-at-bottom
			  (list (cons 'window-height #'shrink-window-if-larger-than-buffer))))))
	  (set-window-dedicated-p win t))

	(with-current-buffer wisi-error-buffer
	  (setq window-size-fixed t))
	(next-error))
      ))

   ((wisi-parse-try wisi--last-parse-action)
    (message "need parse"))

   (t
    (message "parse succeeded"))
   ))

(defun wisi-kill-parser ()
  "Kill the background process running the parser for the current buffer.
Usefull if the parser appears to be hung."
  (interactive)
  (wisi-parse-kill wisi--parser)
  ;; also force re-parse
  (wisi-reset-parser)
  )

(defun wisi-partial-parse-p (begin end)
  (and (wisi-process--parser-p wisi--parser)
       (not (and (= begin (point-min))
		 (= end (point-max))))
       (>= (point-max) wisi-partial-parse-threshold)))

(defun wisi--run-parse (begin parse-end)
  "Run the parser, on at least region BEGIN PARSE-END."
  (unless (or (buffer-narrowed-p)
	      (= (point-min) (point-max))) ;; some parsers can’t handle an empty buffer.
    (let* ((partial-parse-p (wisi-partial-parse-p begin parse-end))
	   (msg (when (> wisi-debug 0)
		  (format "wisi: %sparsing %s %s:%d %d %d ..."
			  (if partial-parse-p "partial " "")
			  wisi--parse-action
			  (buffer-name)
			  begin
			  (if (markerp parse-end) (marker-position parse-end) parse-end)
			  (line-number-at-pos begin))))
	   (parsed-region nil))

      (when msg
	(message msg))

      (setq wisi--last-parse-action wisi--parse-action)
      (wisi-set-last-parse-region begin parse-end wisi--parse-action)

      (unless (eq wisi--parse-action 'face)
	(when (buffer-live-p wisi-error-buffer)
	  (with-current-buffer wisi-error-buffer
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (setq buffer-read-only t)
	    (when (get-buffer-window wisi-error-buffer)
	      (delete-window (get-buffer-window wisi-error-buffer))))))

      (condition-case-unless-debug err
	  (save-excursion
	    (if partial-parse-p
	      (let ((send-region (wisi-parse-expand-region wisi--parser begin parse-end)))
		(setq parsed-region (wisi-parse-current wisi--parser (car send-region) (cdr send-region) parse-end))
		(wisi-cache-add-region parsed-region))

	      ;; parse full buffer
	      (setq parsed-region (cons (point-min) (point-max)))
	      (wisi-cache-set-region (wisi-parse-current wisi--parser (point-min) (point-max) (point-max))))

	    (when (> wisi-debug 0) (message "... parsed %s" parsed-region))
	    (setq wisi-parse-failed nil))
	(wisi-parse-error
	 (cl-ecase wisi--parse-action
	   (face
	    ;; Caches set by failed elisp parse are ok, but some parse
	    ;; failures return 'nil' in parse-region.
	    (when (cdr parsed-region)
	      (wisi--delete-face-cache (cdr parsed-region))))

	   (navigate
	    ;; elisp parse partially resets caches
	    (wisi--delete-navigate-cache (point-min)))

	   (indent
	    ;; parse does not set caches; see `wisi-indent-region'
	    nil))
	 (setq wisi-parse-failed t)
	 ;; parser should have stored this error message in parser-error-msgs
	 (when (> wisi-debug 0)
	   (signal (car err) (cdr err)))
	 )
	(error
	 ;; parser failed for other reason
	 (setq wisi-parse-failed t)
	 (signal (car err) (cdr err)))
	)

      (unless partial-parse-p
	(wisi-fringe-display-errors
	 (append
	  (seq-map (lambda (err) (wisi--lexer-error-pos err)) (wisi-parser-lexer-errors wisi--parser))
	  (seq-map (lambda (err) (wisi--parse-error-pos err)) (wisi-parser-parse-errors wisi--parser)))))

      (when (> wisi-debug 1)
	(if (or (wisi-parser-lexer-errors wisi--parser)
		(wisi-parser-parse-errors wisi--parser))
	    (progn
	      (message "%s error" msg)
	      (wisi-goto-error)
	      (error (or (and (wisi-parser-lexer-errors wisi--parser)
			      (wisi--lexer-error-message (car (wisi-parser-lexer-errors wisi--parser))))
			 (and (wisi-parser-parse-errors wisi--parser)
			      (wisi--parse-error-message (car (wisi-parser-parse-errors wisi--parser))))
			 )))

	  ;; no error
	  (message "%s done" msg))
	))))

(defun wisi--check-change ()
  "Process `wisi--change-beg', `wisi--change-end'.
`wisi--parse-action' must be bound."
  (when (and wisi--change-beg
	     wisi--change-end
	     (or (integerp wisi--change-beg)
		 (marker-buffer wisi--change-beg)) ;; this can fail after editing during ediff-regions.
	     (or (integerp wisi--change-end)
		 (marker-buffer wisi--change-end))
	     (<= wisi--change-beg wisi--change-end))
    (wisi--post-change wisi--change-beg (marker-position wisi--change-end))
    (setq wisi--change-beg most-positive-fixnum)
    (move-marker wisi--change-end (point-min))
    ))

(defun wisi-validate-cache (begin end error-on-fail parse-action)
  "Ensure cached data for PARSE-ACTION is valid in region BEGIN END in current buffer."
  (if (and (not wisi-inhibit-parse)
	   (< (point-max) wisi-size-threshold))
      (let ((wisi--parse-action parse-action))
	(wisi--check-change)

	;; Now we can rely on wisi-cache-covers-region.
	;;
	;; If the last parse failed but was partial, and we are trying
	;; a different region, it may succeed. Otherwise, don't keep
	;; retrying a failed parse until the text changes again.
	(cond
	 ((and (not wisi-parse-failed)
	       (wisi-cache-covers-region begin end))
	  (when (> wisi-debug 0)
	    (message "parse %s skipped: cache-covers-region %s %s.%s"
		     parse-action
		     (wisi-cache-covers-region begin end)
		     begin end)))

	 ((and wisi-parse-failed
	       (equal (cons begin end) (wisi-last-parse-region parse-action))
	       (not (wisi-parse-try parse-action)))
	  (when (> wisi-debug 0)
	    (message "parse %s skipped: parse-failed" parse-action)))

	 (t
	  (progn
	    (wisi-set-parse-try nil)
	    (wisi--run-parse begin end))))

	;; We want this error even if we did not try to parse; it means
	;; the parse results are not valid.
	(when (and error-on-fail wisi-parse-failed)
	  (error "parse %s failed" parse-action))
	)
    (when (> wisi-debug 0)
      (message "parse %s skipped inihibit-parse %s wisi-size-threshold %d"
	       parse-action
	       wisi-inhibit-parse
	       wisi-size-threshold))))

(defun wisi-fontify-region (begin end)
  "For `jit-lock-functions'."
  (wisi-validate-cache begin end nil 'face))

(defun wisi-get-containing-cache (cache)
  "Return cache from (wisi-cache-containing CACHE)."
  (when cache
    (let ((containing (wisi-cache-containing cache)))
      (and containing
	   (wisi-get-cache containing)))))

(defun wisi-cache-text (cache)
  "Return property-less buffer substring designated by cache.
Point must be at cache."
  (buffer-substring-no-properties (point) (+ (point) (wisi-cache-last cache))))

;;;; navigation

(defun wisi-forward-find-class (class limit)
  "Search at point or forward for a token that has a cache with CLASS.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (while (not (eq class (wisi-cache-class cache)))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with class %s not found" class)))
    cache))

(defun wisi-forward-find-cache-token (ids limit)
  "Search forward for a cache with token in IDS (a list of token ids).
Return cache, or nil if at LIMIT or end of buffer."
  (let ((cache (wisi-forward-cache)))
    (while (and (< (point) limit)
		(not (eobp))
		(not (memq (wisi-cache-token cache) ids)))
      (setq cache (wisi-forward-cache)))
    cache))

(defun wisi-forward-find-nonterm (nonterm limit)
  "Search forward for a token that has a cache with NONTERM.
NONTERM may be a list; stop on any cache that has a member of the list.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((nonterm-list (cond
		       ((listp nonterm) nonterm)
		       (t (list nonterm))))
	(cache (wisi-forward-cache)))
    (while (not (memq (wisi-cache-nonterm cache) nonterm-list))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with nonterm %s not found" nonterm)))
    cache))

(defun wisi-goto-cache-next (cache)
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point))
  )

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or cache-end, or next cache
if both nil.  Return cache found."
  (unless (eobp)
    (wisi-validate-cache (point-min) (point-max) t 'navigate)
    (let ((cache (wisi-get-cache (point))))
      (if (and cache
	       (not (eq (wisi-cache-class cache) 'statement-end)))
	  (let ((next (or (wisi-cache-next cache)
			  (wisi-cache-end cache))))
	    (if next
		(goto-char next)
	      (wisi-forward-cache)))
	(wisi-forward-cache))
      )
    (wisi-get-cache (point))
    ))

(defun wisi-backward-statement-keyword ()
  "If not at a cached token, move backward to prev
cache. Otherwise move to cache-prev, or prev cache if nil."
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (let ((cache (wisi-get-cache (point)))
	prev)
    (when cache
      (setq prev (wisi-cache-prev cache))
      (unless prev
	(unless (eq 'statement-start (wisi-cache-class cache))
	  (setq prev (wisi-cache-containing cache)))))
    (if prev
	(goto-char prev)
      (wisi-backward-cache))
  ))

(defun wisi-forward-sexp (&optional arg)
  "For `forward-sexp-function'."
  (interactive "^p")
  (or arg (setq arg 1))
  (cond
   ((and (> arg 0) (= 4 (syntax-class (syntax-after (point)))))  ;; on open paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 5 (syntax-class (syntax-after (1- (point)))))) ;; after close paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (> arg 0) (= 7 (syntax-class (syntax-after (point)))))  ;; on (open) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 7 (syntax-class (syntax-after (1- (point)))))) ;; after (close) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   (t
    (dotimes (_i (abs arg))
      (if (> arg 0)
	  (wisi-forward-statement-keyword)
	(wisi-backward-statement-keyword))))
   ))

(defun wisi-goto-containing (cache &optional error)
  "Move point to containing token for CACHE, return cache at that point.
If ERROR, throw error when CACHE has no container; else return nil."
  (cond
   ((and (markerp (wisi-cache-containing cache))

	 (not (= (wisi-cache-containing cache) (point))))
    ;; This check is only needed if some cache points to itself as a
    ;; container. Apparently that happend once that I caught in the
    ;; debugger; emacs hung because we got here in the font-lock
    ;; timer.

    (goto-char (wisi-cache-containing cache))
    (wisi-get-cache (point)))
   (t
    (when error
      (error "already at outermost containing token")))
   ))

(defun wisi-goto-containing-paren (cache)
  "Move point to just after the open-paren containing CACHE.
Return cache for paren, or nil if no containing paren."
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'open-paren)))
    (setq cache (wisi-goto-containing cache)))
  (when cache
    (forward-char 1))
  cache)

(defun wisi-goto-start (cache)
  "Move point to containing ancestor of CACHE that has class statement-start.
Return start cache."
  ;; cache nil at bob, or on cache in partially parsed statement
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'statement-start)))
    (setq cache (wisi-goto-containing cache)))
  cache)

(defun wisi-goto-end-1 (cache)
  (goto-char (wisi-cache-end cache)))

(defun wisi-goto-statement-start ()
  "Move point to token at start of statement point is in or after.
Return start cache."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (wisi-goto-start (or (wisi-get-cache (point))
		       (wisi-backward-cache))))

(defun wisi-goto-statement-end ()
  "Move point to token at end of statement point is in or before."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (when (wisi-cache-end cache)
      ;; nil when cache is statement-end
      (wisi-goto-end-1 cache))
    ))

(defun wisi-goto-containing-statement-start ()
  "Move point to the start of the statement containing the current statement."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-backward-cache))))
    (when cache
      (setq cache (wisi-goto-start cache)))
    (when cache
      (setq cache (wisi-goto-containing cache nil)))
    ))

(defun wisi-next-statement-cache (cache)
  "Move point to CACHE-next, return cache; error if nil."
  (when (not (markerp (wisi-cache-next cache)))
    (error "no next statement cache"))
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point)))

(defun wisi-prev-statement-cache (cache)
  "Move point to CACHE-prev, return cache; error if nil."
  (when (not (markerp (wisi-cache-prev cache)))
    (error "no prev statement cache"))
  (goto-char (wisi-cache-prev cache))
  (wisi-get-cache (point)))

;;;; indentation

(defun wisi-comment-indent ()
  "For `comment-indent-function'. Indent single line comment to
the comment on the previous line."
  ;; Called from `comment-indent', either to insert a new comment, or
  ;; to indent the first line of an existing one.  In either case, the
  ;; comment may be after code on the same line.  For an existing
  ;; comment, point is at the start of the starting delimiter.
  (or
   (save-excursion
     ;; Check for a preceding comment line; fail if comment follows code.
     (when (forward-comment -1)
       ;; For the case:
       ;;
       ;; code;-- comment
       ;;
       ;; point is on '--', and 'forward-comment' does not move point,
       ;; returns nil.
       (when (looking-at comment-start)
         (current-column))))

   (save-excursion
     (back-to-indentation)
     (if (looking-at comment-start)
         ;; An existing comment, no code preceding comment, and
         ;; no comment on preceding line. Return nil, so
         ;; `comment-indent' will call `indent-according-to-mode'
         nil

       ;; A comment after code on the same line.
       comment-column))
   ))

(defun wisi-indent-statement ()
  "Indent region given by `wisi-goto-start', `wisi-cache-end'."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (save-excursion
    (let ((cache (or (wisi-get-cache (point))
		     (wisi-backward-cache))))
      (when cache
	;; can be nil if in header comment
	(let ((start (progn (wisi-goto-start cache) (point)))
	      (end (if (wisi-cache-end cache)
			 ;; nil when cache is statement-end
			 (wisi-cache-end cache)
		       (point))))
	  (indent-region start end)
	  ))
      )))

(defun wisi-indent-containing-statement ()
  "Indent region given by `wisi-goto-containing-statement-start', `wisi-cache-end'."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (save-excursion
    (let ((cache (or (wisi-get-cache (point))
		     (wisi-backward-cache))))
      (when cache
	;; can be nil if in header comment
	(let ((start (progn
		       (setq cache (wisi-goto-containing (wisi-goto-start cache)))
		       (point)))
	      (end (if (wisi-cache-end cache)
			 ;; nil when cache is statement-end
			 (wisi-cache-end cache)
		       (point))))
	  (indent-region start end)
	  ))
      )))

(defvar-local wisi-indent-calculate-functions nil
  "Functions to compute indentation special cases.
Called with point at current indentation of a line; return
indentation column, or nil if function does not know how to
indent that line. Run after parser indentation, so other lines
are indented correctly.")

(defvar-local wisi-post-indent-fail-hook
  "Function to reindent portion of buffer.
Called from `wisi-indent-region' when a parse succeeds after
failing; assumes user was editing code that is now syntactically
correct. Must leave point at indentation of current line.")

(defvar-local wisi-indent-failed nil
  "Non-nil when wisi-indent-region fails due to parse failing; cleared when indent succeeds.")

(defvar-local wisi-indent-region-fallback 'wisi-indent-region-fallback-default
  "Function to compute indent for lines in region when wisi parse fails.
Called with BEGIN END.")

(defun wisi-indent-region-fallback-default (begin end)
  ;; Assume there is no indent info at point; user is editing. Indent
  ;; to previous lines.
  (goto-char begin)
  (forward-line -1);; safe at bob
  (back-to-indentation)
  (let ((col (current-column)))
    (while (and (not (eobp))
		(< (point) end))
      (if (= 1 (forward-line 1))
	  (indent-line-to col)
	;; on last line of buffer; terminate loop
	(goto-char (point-max)))
      (when (bobp)
	;; single line in buffer; terminate loop
	(goto-char (point-max))))))

(defun wisi-list-memq (a b)
  "Return non-nil if any member of A is a memq of B."
  (let ((temp (copy-sequence a))
	result)
    (while (and (not result)
		temp)
      (when (memq (pop temp) b)
	(setq result t)))
    result))

(defun wisi--get-cached-indent (begin end)
  "Return cached indent for point (must be bol), after correcting
for parse errors. BEGIN, END is the parsed region."
  (let ((indent (get-text-property (1- (point)) 'wisi-indent)))
    (if indent
	(when (and (wisi-partial-parse-p begin end)
		   (< 0 (length (wisi-parser-parse-errors wisi--parser))))
	  (dolist (err (wisi-parser-parse-errors wisi--parser))
	    (dolist (repair (wisi--parse-error-repair err))
	      ;; point is at bol; error pos may be at first token on same line.
	      (save-excursion
		(back-to-indentation)
		(when (>= (point) (wisi--parse-error-repair-pos repair))
		  (setq indent (max 0 (wisi-parse-adjust-indent wisi--parser indent repair))))
		))))
      ;; parse did not compute indent for point. Assume the error will
      ;; go away soon as the user edits the code, so just return 0.
      (if (= wisi-debug 0)
	  (setq indent 0)
	(error "nil indent for line %d" (line-number-at-pos (point)))))

    indent))

(defun wisi-indent-region (begin end &optional indent-blank-lines)
  "For `indent-region-function', using the wisi indentation engine.
If INDENT-BLANK-LINES is non-nil, also indent blank lines (for use as
`indent-line-function')."
  (if wisi-inhibit-parse
      (when (< 0 wisi-debug)
	(message "wisi-indent-region %d %d skipped; wisi-inhibit-parse"
		 (wisi-safe-marker-pos begin)
		 (wisi-safe-marker-pos end)))

    (let ((wisi--parse-action 'indent)
	  (parse-required nil)
	  (end-mark (copy-marker end))
	  (prev-indent-failed wisi-indent-failed))

      (when (< 0 wisi-debug)
	(message "wisi-indent-region %d %d"
		 (wisi-safe-marker-pos begin)
		 (wisi-safe-marker-pos end)))

      (wisi--check-change)

      ;; BEGIN is inclusive; END is exclusive.
      (save-excursion
	(goto-char begin)
	(setq begin (line-beginning-position))

	(when (bobp) (forward-line))
	(while (and (not parse-required)
		    (or (and (= begin end) (= (point) end))
			(< (point) end))
		    (not (eobp)))
	  (unless (get-text-property (1- (point)) 'wisi-indent)
	    (setq parse-required t))
	  (forward-line))
	)

      ;; A parse either succeeds and sets the indent cache on all
      ;; lines in the parsed region, or fails and leaves valid caches
      ;; untouched.
      (when (and parse-required
		 (or (not wisi-parse-failed)
		     (wisi-parse-try 'indent)))

	(wisi-set-parse-try nil)
	(wisi--run-parse begin end)

	;; If there were errors corrected, the indentation is
	;; potentially ambiguous; see
	;; test/ada_mode-interactive_2.adb. Or it was a partial parse,
	;; where errors producing bad indent are pretty much expected.
	(unless (wisi-partial-parse-p begin end)
	  (setq wisi-indent-failed (< 0 (+ (length (wisi-parser-lexer-errors wisi--parser))
					   (length (wisi-parser-parse-errors wisi--parser))))))
	)

      (if wisi-parse-failed
	  (progn
	    ;; primary indent failed
	    (setq wisi-indent-failed t)
	    (when (functionp wisi-indent-region-fallback)
	      (when (< 0 wisi-debug)
		(message "wisi-indent-region fallback"))
	      (funcall wisi-indent-region-fallback begin end)))

	(save-excursion
	  ;; Apply cached indents. Start from end, so indenting
	  ;; doesn't affect correcting for errors in
	  ;; wisi--get-cached-indent.
	  (goto-char (1- end)) ;; end is exclusive
	  (goto-char (line-beginning-position))
	  (let ((wisi-indenting-p t))
	    (while (and (not (bobp))
			(or (and (= begin end) (= (point) end))
			    (>= (point) begin)))
	      (when (or indent-blank-lines (not (eolp)))
		;; ’indent-region’ doesn’t indent an empty line; ’indent-line’ does
		(let ((indent (if (bobp) 0 (wisi--get-cached-indent begin end))))
		  (indent-line-to indent))
		)
	      (forward-line -1))

	    ;; Run wisi-indent-calculate-functions
	    (when wisi-indent-calculate-functions
	      (goto-char begin)
	      (while (and (not (eobp))
			  (< (point) end-mark))
		(back-to-indentation)
		(let ((indent
		       (run-hook-with-args-until-success 'wisi-indent-calculate-functions)))
		  (when indent
		    (indent-line-to indent)))

		(forward-line 1)))
	    )

	  (when
	      (and prev-indent-failed
		   (not wisi-indent-failed))
	    ;; Previous parse failed or indent was potentially
	    ;; ambiguous, this one is not.
	    (goto-char end-mark)
	    (when (< 0 wisi-debug)
	      (message "wisi-indent-region post-parse-fail-hook"))
	    (run-hooks 'wisi-post-indent-fail-hook))
	  ))
      )))

(defun wisi-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(to-indent nil))
    (back-to-indentation)
    (when (>= (point) savep)
      (setq to-indent t))

    (wisi-indent-region (line-beginning-position (1+ (- wisi-indent-context-lines))) (1+ (line-end-position)) t)

    (goto-char savep)
    (when to-indent (back-to-indentation))
    ))

(defun wisi-repair-error-1 (data)
  "Repair error reported in DATA (a ’wisi--parse-error’ or ’wisi--lexer-error’)"
  (let ((wisi--parse-action 'navigate))
    (cond
     ((wisi--lexer-error-p data)
      (goto-char (1+ (wisi--lexer-error-pos data)))
      (insert (wisi--lexer-error-inserted data)))
     ((wisi--parse-error-p data)
      (dolist (repair (wisi--parse-error-repair data))
	(goto-char (wisi--parse-error-repair-pos repair))
	(when (< 0 (length (wisi--parse-error-repair-deleted repair)))
	  (delete-region (car (wisi--parse-error-repair-deleted-region repair))
			 (cdr (wisi--parse-error-repair-deleted-region repair)))
	  (when (= ?  (char-after (point)))
	    (delete-char 1)))
	(dolist (id (wisi--parse-error-repair-inserted repair))
	  (when (and (not (bobp))
		     (not (= ?\( (char-before (point))))
		     (member (syntax-class (syntax-after (1- (point)))) '(2 3))) ;; word or symbol
	    (insert " "))
	  (insert (cdr (assoc id (wisi-parser-repair-image wisi--parser)))))
	))
     )))

(defun wisi-repair-error ()
  "Repair the current error."
  (interactive)
  (let ((wisi-inhibit-parse t)) ;; don’t let the error list change while we are processing it.
    (if (= 1 (+ (length (wisi-parser-lexer-errors wisi--parser))
		(length (wisi-parser-parse-errors wisi--parser))))
	(progn
	  (wisi-repair-error-1 (or (car (wisi-parser-lexer-errors wisi--parser))
				   (car (wisi-parser-parse-errors wisi--parser)))))
      (if (buffer-live-p wisi-error-buffer)
	  (let ((err
		 (with-current-buffer wisi-error-buffer
		   (get-text-property (point) 'wisi-error-data))))
	    (wisi-repair-error-1 err))
	(error "no current error found")
	))))

(defun wisi-repair-errors (&optional beg end)
  "Repair errors reported by last parse.
If non-nil, only repair errors in BEG END region."
  (interactive)
  (let ((wisi-inhibit-parse t)) ;; don’t let the error list change while we are processing it.
    (dolist (data (wisi-parser-lexer-errors wisi--parser))
      (when (or (null beg)
		(and (not (= 0 (wisi--lexer-error-inserted data)))
		     (wisi--lexer-error-pos data)
		     (<= beg (wisi--lexer-error-pos data))
		     (<= (wisi--lexer-error-pos data) end)))
	(wisi-repair-error-1 data)))

    (dolist (data (wisi-parser-parse-errors wisi--parser))
      (when (or (null beg)
		(and (wisi--parse-error-pos data)
		     (<= beg (wisi--parse-error-pos data))
		     (<= (wisi--parse-error-pos data) end)))
	(wisi-repair-error-1 data)))
    ))

;;; xref integration

(defun wisi-xref-identifier-at-point ()
  (let ((ident (thing-at-point 'symbol)))
    (when ident
      (put-text-property
       0 1
       'xref-identifier
       (list :file (buffer-file-name)
	     :line (line-number-at-pos)
	     :column (current-column))
       ident)
      ident)))

(defun wisi-next-name-region ()
  "Return the next region at or after point with text property 'wisi-name'."
  (let* ((begin
	  (if (get-text-property (point) 'wisi-name)
	      (point)
	    (next-single-property-change (point) 'wisi-name)))
	 (end (next-single-property-change begin 'wisi-name)))
    (cons begin end)))

(defun wisi-prev-name-region ()
  "Return the prev region at or before point with text property 'wisi-name'."
  (let* ((end
	  (if (get-text-property (point) 'wisi-name)
	      (point)
	    (previous-single-property-change (point) 'wisi-name)))
	 (begin (previous-single-property-change end 'wisi-name)))
    (cons begin end)))

(defun wisi-next-name ()
  "Return the text at or after point with text property 'wisi-name'."
  (let ((region (wisi-next-name-region)))
    (buffer-substring-no-properties (car region) (cdr region))))

(defun wisi-prev-name ()
  "Return the text at or before point with text property 'wisi-name'."
  (let ((region (wisi-prev-name-region)))
    (buffer-substring-no-properties (car region) (cdr region))))

(defconst wisi-names-regexp "\\([^<]*\\)<\\([0-9]+\\)>"
  "Match line number encoded into identifier by `wisi-names'.")

(defun wisi-names (append-lines alist)
  "List of names; each is text from one 'wisi-name property in current buffer.
If APPEND-LINES is non-nil, each name has the line number it
occurs on appended. If ALIST is non-nil, the result is an alist
where the car is a list (FILE LINE COL)."
  (when wisi--parser
    ;; wisi--parser is nil in a non-language buffer, like Makefile
    (wisi-validate-cache (point-min) (point-max) t 'navigate)
    (let ((table nil)
	  (pos (point-min))
	  end-pos)
      (while (setq pos (next-single-property-change pos 'wisi-name))
	;; We can’t store location data in a string text property -
	;; it does not survive completion. So we include the line
	;; number in the identifier string. This also serves to
	;; disambiguate overloaded identifiers in the user interface.
	(setq end-pos (next-single-property-change pos 'wisi-name))
	(let* ((line (line-number-at-pos pos))
	       (summary
		(if append-lines
		    (format "%s<%d>"
			    (buffer-substring-no-properties pos end-pos)
			    line)
		  (buffer-substring-no-properties pos end-pos))))
	  (if alist
	      (save-excursion
		(goto-char pos)
		(push (cons summary (list (buffer-file-name) line (current-column)))
		      table))
	    (push summary table)))
	(setq pos end-pos)
	)
      table)))

;;;; debugging

(defun wisi-show-region ()
  (interactive)
  (cond
   ((use-region-p)
    (message "(%s . %s)" (region-beginning) (region-end)))
   (t
    (let ((string (read-from-minibuffer "region: ")))
      (when (not (= ?\( (aref string 0)))
	(setq string (concat "(" string ")")))

      (let ((region (read string)))
	(cond
	 ((consp (cdr region))
	  ;; region is a list; (begin end)
	  (set-mark  (nth 0 region))
	  (goto-char (nth 1 region)))

	 ((consp region)
	  ;; region is a cons; (begin . end)
	  (set-mark  (car region))
	  (goto-char (cdr region)))
	 ))))
   ))

(defun wisi-debug-keys ()
  "Add debug key definitions to `global-map'."
  (interactive)
  (define-key global-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key global-map "\M-i" 'wisi-show-indent)
  (define-key global-map "\M-j" 'wisi-show-cache)
  )

(defun wisi-parse-buffer (&optional parse-action begin end)
  (interactive)
  (unless parse-action
    (setq parse-action (wisi-read-parse-action)))
  (if (use-region-p)
      (progn
	(setq begin (region-beginning))
	(setq end   (region-end)))

    (unless begin (setq begin (point-min)))
    (unless end (setq end (point-max))))

  (wisi-set-parse-try t parse-action)
  (wisi-invalidate-cache parse-action begin)

  (cl-ecase parse-action
    (face
     (with-silent-modifications
       (remove-text-properties
	begin end
	(list
	 'font-lock-face nil
	 'fontified nil)))
     (wisi-validate-cache begin end t parse-action)
     (font-lock-ensure))

    (navigate
     (wisi-validate-cache begin end t parse-action))

    (indent
     (wisi-indent-region begin end))
    ))

(defun wisi-time (func count &optional report-wait-time)
  "call FUNC COUNT times, show total time"
  (interactive "afunction \nncount ")

  (let ((start-time (float-time))
	(start-gcs gcs-done)
	(cum-wait-time 0.0)
        (i 0)
        diff-time
	diff-gcs)
    (while (not (eq (1+ count) (setq i (1+ i))))
      (save-excursion
        (funcall func))
      (when report-wait-time
	(setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
    (setq diff-time (- (float-time) start-time))
    (setq diff-gcs (- gcs-done start-gcs))
    (if report-wait-time
	(progn
	  (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs %d responses %f wait"
		   diff-time
		   diff-gcs
		   (/ diff-time count)
		   (/ (float diff-gcs) count)
		   (wisi-process--parser-response-count wisi--parser)
		   (/ cum-wait-time count)))

      (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs"
	       diff-time
	       diff-gcs
	       (/ diff-time count)
	       (/ (float diff-gcs) count))
      ))
  nil)

(defun wisi-time-indent-middle-line-cold-cache (count &optional report-wait-time)
  (goto-char (point-min))
  (forward-line (1- (/ (count-lines (point-min) (point-max)) 2)))
  (let ((cum-wait-time 0.0))
    (wisi-time
     (lambda ()
       (wisi-set-parse-try t 'indent)
       (wisi-invalidate-cache 'indent (point-min))
       (wisi-indent-line)
       (when (wisi-process--parser-p wisi--parser)
	 (setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
     count
     report-wait-time)
    ))

(defun wisi-time-indent-middle-line-warm-cache (count)
  (wisi-set-parse-try t 'indent)
  (wisi-invalidate-cache 'indent (point-min))
  (goto-char (point-min))
  (forward-line (/ (count-lines (point-min) (point-max)) 2))
  (wisi-indent-line)
  (wisi-time #'wisi-indent-line count))

(defun wisi-show-indent ()
  "Show indent cache for current line."
  (interactive)
  (message "%s" (get-text-property (1- (line-beginning-position)) 'wisi-indent)))

(defun wisi-show-cache ()
  "Show wisi text properties at point."
  (interactive)
  (message "%s:%s:%s:%s"
	   (wisi-get-cache (point))
	   (get-text-property (point) 'face)
	   (get-text-property (point) 'font-lock-face)
	   (get-text-property (point) 'wisi-name)
	   ))

(defun wisi-show-containing-or-previous-cache ()
  (interactive)
  (let ((cache (wisi-get-cache (point))))
    (push-mark)
    (if cache
	(message "containing %s" (wisi-goto-containing cache t))
      (message "previous %s" (wisi-backward-cache)))
    ))

;;;;; setup

(cl-defun wisi-setup (&key indent-calculate post-indent-fail parser)
  "Set up a buffer for parsing files with wisi."
  (when wisi--parser
    (wisi-kill-parser))

  (setq wisi--parser parser)
  (setq wisi--cached-regions
	(list
	 (cons 'face nil)
	 (cons 'navigate nil)
	 (cons 'indent nil)))

  (setq wisi--parse-try
	(list
	 (cons 'face t)
	 (cons 'navigate t)
	 (cons 'indent t)))

  (setq wisi--last-parse-region
	(list
	 (cons 'face nil)
	 (cons 'navigate nil)
	 (cons 'indent nil)))

  (setq wisi-indent-calculate-functions (append wisi-indent-calculate-functions indent-calculate))
  (set (make-local-variable 'indent-line-function) #'wisi-indent-line)
  (set (make-local-variable 'indent-region-function) #'wisi-indent-region)
  (set (make-local-variable 'forward-sexp-function) #'wisi-forward-sexp)

  (setq wisi-post-indent-fail-hook post-indent-fail)
  (setq wisi-indent-failed nil)

  (add-hook 'before-change-functions #'wisi-before-change 'append t)
  (add-hook 'after-change-functions #'wisi-after-change nil t)
  (setq wisi--change-end (copy-marker (point-min) t))

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)

  (add-hook 'completion-at-point-functions #'wisi-completion-at-point -90 t)

  (add-hook 'hack-local-variables-hook 'wisi-post-local-vars nil t)
  )

(defun wisi-post-local-vars ()
  "See wisi-setup."
  (remove-hook 'hack-local-variables-hook #'wisi-post-local-vars)

  (unless wisi-disable-face
    (jit-lock-register #'wisi-fontify-region)))


(provide 'wisi)
;;; wisi.el ends here

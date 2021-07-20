;;; wisi-parse-common.el --- declarations used by wisi-parse.el, wisi-ada-parse.el, and wisi.el -*- lexical-binding:t -*-
;;
;; Copyright (C) 2014, 2015, 2017 - 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

;;; Code:

(defcustom wisi-partial-parse-threshold 100001
  "Minimum size that will be parsed by each call to the parser.
A parse is always requested at a point (or on a region); the
point is first expanded to a start point before the region and an
end point after the region, that the parser can gracefully
handle. If the final region covers the entire buffer, a complete
parse is done. Indent assumes the start point of the parse region
is properly indented. Most navigate parses ignore this setting
and parse the whole buffer."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-partial-parse-threshold)

(cl-defstruct (wisi--lexer-error)
  pos ;; position (integer) in buffer where error was detected.
  message  ;; string error message
  inserted ;; char inserted after pos.
  )

(cl-defstruct (wisi--parse-error-repair)
  pos ;; position (integer) in buffer where insert/delete is done.
  inserted ;; list of token IDs that were inserted before pos
  deleted  ;; list of token IDs that were deleted after pos
  deleted-region ;; buffer (cons FIRST LAST) region deleted
  )

(cl-defstruct (wisi--parse-error)
  ;; Includes information derived from compiler error recovery to edit
  ;; text to fix one error. Used by ’wisi-repair-error’ to edit buffer.
  pos      ;; position (integer or marker) in buffer where error was detected.
  message  ;; string error message
  repair   ;; list of wisi--parse-error-repair.
  )

(cl-defstruct wisi-parser
  ;; Separate lists for lexer and parse errors, because lexer errors
  ;; must be repaired first, before parse errors can be repaired. And
  ;; they have different structures.
  lexer-errors
  ;; list of wisi--lexer-errors from last parse.  Can be more than one if
  ;; lexer supports error recovery.
  parse-errors
  ;; List of wisi--parse-errors from last parse. Can be more than one if
  ;; parser supports error recovery.

  repair-image
  ;; alist of (TOKEN-ID . STRING); used by repair error
)

(cl-defgeneric wisi-parse-format-language-options ((parser wisi-parser))
  "Return a string to be sent to the parser, containing settings
for the language-specific parser options."
  )

(cl-defgeneric wisi-parse-expand-region ((_parser wisi-parser) begin end)
  "Return a cons SEND-BEGIN . SEND-END that is an expansion of
region BEGIN END that starts and ends at points the parser can
handle gracefully."
  (cons begin end))

(defvar-local wisi--parser nil
  "The current wisi parser; a ‘wisi-parser’ object.")

(defun wisi-read-parse-action ()
  "Read a parse action symbol from the minibuffer."
  (intern-soft (completing-read "parse action (indent): " '(face navigate indent) nil t nil nil 'indent)))

(defun wisi-search-backward-skip (regexp skip-p)
  "Search backward for REGEXP. If SKIP-P returns non-nil, search again.
SKIP-P is a function taking no parameters.
Return nil if no match found before bob."
  (let ((maybe-found-p (search-backward-regexp regexp nil t)))
    (while (and maybe-found-p
		(funcall skip-p)
		(setq maybe-found-p (search-backward-regexp regexp nil t))))
    maybe-found-p))

(defun wisi-search-forward-skip (regexp skip-p)
  "Search forward for REGEXP. If SKIP-P returns non-nil, search again.
SKIP-P is a function taking no parameters.
Return nil if no match found before eob."
  (let ((maybe-found-p (search-forward-regexp regexp nil t)))
    (while (and maybe-found-p
		(funcall skip-p)
		(setq maybe-found-p (search-forward-regexp regexp nil t))))
    maybe-found-p))

(defun wisi-show-expanded-region ()
  "For debugging. Expand currently selected region."
  (interactive)
  (let ((region (wisi-parse-expand-region wisi--parser (region-beginning) (region-end))))
    (message "pre (%d . %d) post %s" (region-beginning) (region-end) region)
    (set-mark (car region))
    (goto-char (cdr region))
    ))

(cl-defgeneric wisi-parse-adjust-indent ((_parser wisi-parser) indent _repair)
  "Adjust INDENT for REPAIR (a wisi--parse-error-repair struct). Return new indent."
  indent)

(cl-defgeneric wisi-parse-current ((parser wisi-parser) begin send-end parse-end)
  "Parse current buffer starting at BEGIN, continuing at least thru PARSE-END.
If using an external parser, send it BEGIN thru SEND-END.")

(cl-defgeneric wisi-refactor ((parser wisi-parser) refactor-action parse-begin parse-end edit-begin)
  "Send parser command to perform REFACTOR-ACTION on region PARSE-BEGIN PARSE-END at point EDIT_BEGIN.
The parse region is not expanded first; it must be the statement
or declaration containing EDIT_BEGIN.")

(cl-defgeneric wisi-parse-kill ((parser wisi-parser))
  "Kill any external process associated with parser.")

(cl-defgeneric wisi-parse-find-token ((parser wisi-parser) token-symbol)
  "Find token with TOKEN-SYMBOL on current parser stack, return token struct.
For use in grammar actions.")

(cl-defgeneric wisi-parse-stack-peek ((parser wisi-parser) n)
  "Return the Nth token on the parse stack.
For use in grammar actions.")

(cl-defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  nonterm;; nonterminal from parse

  token
  ;; terminal symbol from wisi-keyword-table or
  ;; wisi-punctuation-table, or lower-level nonterminal from parse

  last ;; pos of last char in token, relative to first (0 indexed)

  class ;; one of wisi-class-list

  containing
  ;; Marker at the start of the containing statement for this token.
  ;; nil for outermost containing.

  prev ;; marker at previous motion token in statement; nil if none
  next ;; marker at next motion token in statement; nil if none
  end  ;; marker at token at end of current statement
  )

(defun wisi-get-cache (pos)
  "Return `wisi-cache' struct from the `wisi-cache' text property at POS."
  (get-text-property pos 'wisi-cache))

(defun wisi-backward-cache ()
  "Move point backward to the beginning of the first token preceding point that has a cache.
Returns cache, or nil if at beginning of buffer."
  ;; If point is not near cache, p-s-p-c will return pos just after
  ;; cache, so 1- is the beginning of cache.
  ;;
  ;; If point is just after end of cache, p-s-p-c will return pos at
  ;; start of cache.
  ;;
  ;; So we test for the property before subtracting 1.
  (let ((pos (previous-single-property-change (point) 'wisi-cache))
	cache)
    (cond
     ((null pos)
      (goto-char (point-min))
      nil)

     ((setq cache (get-text-property pos 'wisi-cache))
      (goto-char pos)
      cache)

     (t
      (setq pos (1- pos))
      (setq cache (get-text-property pos 'wisi-cache))
      (goto-char pos)
      cache)
     )))

(defun wisi-forward-cache ()
  "Move point forward to the beginning of the first token after point that has a cache.
Returns cache, or nil if at end of buffer."
  (let (cache pos)
    (when (get-text-property (point) 'wisi-cache)
      ;; on a cache; get past it
      (goto-char (1+ (point))))

    (setq cache (get-text-property (point) 'wisi-cache))
    (if cache
	nil

      (setq pos (next-single-property-change (point) 'wisi-cache))
      (if pos
	  (progn
	    (goto-char pos)
	    (setq cache (get-text-property pos 'wisi-cache)))
	;; at eob
	(goto-char (point-max))
	(setq cache nil))
      )
    cache
    ))

(defun wisi-cache-region (cache &optional start)
  "Return region designated by START (default point) to cache last."
  (unless start (setq start (point)))
  (cons start (+ start (wisi-cache-last cache))))

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)
2 : show parse states, position point at parse errors
3 : also show top 10 items of parser stack.")

;; The following parameters are easily changeable for debugging.
(defvar wisi-action-disable nil
  "If non-nil, disable all elisp actions during parsing.
Allows timing parse separate from actions.")

(defvar-local wisi-trace-mckenzie 0
  "McKenzie trace level; 0 for none")

(defvar-local wisi-trace-action 0
  "Parse action trace level; 0 for none")

(defvar-local wisi-mckenzie-disable nil
  "If non-nil, disable McKenzie error recovery. Otherwise, use parser default.")

(defcustom wisi-mckenzie-task-count nil
  "If integer, sets McKenzie error recovery task count.
Higher value (up to system processor limit) runs error recovery
faster, but may encounter race conditions.  Using only one task
makes error recovery repeatable; useful for tests.  If nil, uses
value from grammar file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-task-count)

(defcustom wisi-mckenzie-check-limit nil
  "If integer, sets McKenzie error recovery algorithm token check limit.
This sets the number of tokens past the error point that must be
parsed successfully for a solution to be deemed successful.
Higher value gives better solutions, but may fail if there are
two errors close together.  If nil, uses value from grammar
file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-check-limit)

(defcustom wisi-mckenzie-enqueue-limit nil
  "If integer, sets McKenzie error recovery algorithm enqueue limit.
This sets the maximum number of solutions that will be considered.
Higher value has more recover power, but will be slower to fail.
If nil, uses value from grammar file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-enqueue-limit)

(defcustom wisi-parse-max-parallel 15
  "Maximum number of parallel parsers during regular parsing.
Parallel parsers are used to resolve redundancy in the grammar.
If a file needs more than this, it's probably an indication that
the grammar is excessively redundant."
  :type 'integer
  :group 'wisi
  :safe 'integerp)

(defvar wisi-parse-max-stack-size 500
  "Maximum parse stack size.
Larger stack size allows more deeply nested constructs.")
;; end of easily changeable parameters

(defvar wisi--parse-action nil
  ;; not buffer-local; only let-bound in wisi-indent-region, wisi-validate-cache
  "Reason current parse is begin run; one of
{indent, face, navigate}.")

(defvar-local wisi-indent-comment-col-0 nil
  "If non-nil, comments currently starting in column 0 are left in column 0.
Otherwise, they are indented with previous comments or code.
Normally set from a language-specific option.")

(defvar-local wisi-end-caches nil
  "List of buffer positions of caches in current statement that need wisi-cache-end set.")

(defconst wisi-eoi-term 'Wisi_EOI
  ;; must match FastToken wisi-output_elisp.adb EOI_Name, which must
  ;; be part of a valid Ada identifer.
  "End Of Input token.")

(defconst wisi-class-list
  [motion ;; motion-action
   statement-end
   statement-override
   statement-start
   misc ;; other stuff
   ]
  "array of valid token classes; checked in wisi-statement-action, used in wisi-process-parse.")

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (buffer-name) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply 'format message args))))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")

(cl-defstruct wisi-tok
  token  ;; symbol from a token table ;; IMPROVEME: rename to ’id’?
  region ;; cons giving buffer region containing token text

  nonterminal ;; t if a nonterminal

  line ;; Line number at start of token. Nil for empty nonterminals

  first
  ;; For terminals, t if token is the first token on a line.
  ;;
  ;; For nonterminals, line number of first contained line (not
  ;; including trailing comments) that needs indenting; it is a
  ;; comment, or begins with a contained token.
  ;;
  ;; Otherwise nil.

  ;; The following are non-nil if token (terminal or non-terminal) is
  ;; followed by blank or comment lines
  comment-line ;; first blank or comment line following token
  comment-end ;; position at end of blank or comment lines
  )

(defun wisi-token-text (token)
  "Return buffer text from token range."
  (let ((region (wisi-tok-region token)))
    (and region
       (buffer-substring-no-properties (car region) (cdr region)))))

(defun wisi-and-regions (left right)
  "Return region enclosing both LEFT and RIGHT."
  (if left
      (if right
	  (cons (min (car left) (car right))
		(max (cdr left) (cdr right)))
	left)
    right))

(defun wisi--set-line-begin (line-count)
  "Return a vector of line-beginning positions, with length LINE-COUNT."
  (let ((result (make-vector line-count 0)))
    (save-excursion
      (goto-char (point-min))

      (dotimes (i line-count)
	(aset result i (point))
	(forward-line 1)))
    result))

;;;; debugging
(defun wisi-tok-debug-image (tok)
  "Return id and region from TOK, as string."
  (cond
   ((wisi-tok-region tok)
    (format "(%s %d . %d)"
	    (wisi-tok-token tok)
	    (car (wisi-tok-region tok))
	    (cdr (wisi-tok-region tok))))
   (t
    (format "(%s)" (wisi-tok-token tok)))
   ))

(provide 'wisi-parse-common)

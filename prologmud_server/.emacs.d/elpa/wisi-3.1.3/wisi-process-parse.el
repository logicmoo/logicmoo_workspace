;;; wisi-process-parse.el --- interface to external parse program
;;
;; Copyright (C) 2014, 2017 - 2020 Free Software Foundation, Inc.
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

(require 'cl-lib)
(require 'wisi-parse-common)

(defgroup wisi nil
  "Options for Wisi package."
  :group 'programming)

(defcustom wisi-process-time-out 5.0
  "Time out waiting for parser response. An error occurs if there
  is no response from the parser after waiting this amount (in
  seconds)."
  :type 'float
  :safe 'numberp)
(make-variable-buffer-local 'wisi-process-time-out)

(defconst wisi-process-parse-protocol-version "5"
  "Defines data exchanged between this package and the background process.
Must match emacs_wisi_common_parse.ads Protocol_Version.")

(defconst wisi-process-parse-prompt "^;;> "
  "Regexp matching executable prompt; indicates previous command is complete.")

(defconst wisi-process-parse-quit-cmd "004quit\n"
  "Command to external process telling it to quit.")

;;;;; sessions

;; The executable builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process per language; there is no persistent state
;; in the process between parses, and processes are too heavy-weight
;; to have one per buffer. We use a global alist of parser objects to
;; find the right one for the current buffer.

(cl-defstruct (wisi-process--parser (:include wisi-parser))
  (label nil)             ;; string uniquely identifying parser
  language-protocol-version ;; string identifying language-specific params
  (exec-file nil) 	  ;; absolute file name of executable
  (exec-opts nil)         ;; list of process start options for executable
  (token-table nil)       ;; vector of token symbols, indexed by integer
  (face-table nil) 	  ;; vector of face symbols, indexed by integer
  (busy nil)              ;; t while parser is active
  (process nil) 	  ;; running *_wisi_parse executable
  (buffer nil) 		  ;; receives output of executable
  line-begin              ;; vector of beginning-of-line positions in buffer
  (total-wait-time 0.0)   ;; total time during last parse spent waiting for subprocess output.
  (response-count 0)      ;; responses received from subprocess during last parse; for profiling.
  end-pos                 ;; last character position parsed
  language-action-table   ;; array of function pointers, each taking an sexp sent by the process
  )

(defvar wisi-process--alist nil
  "Alist mapping string label to ‘wisi-process--session’ struct")

;;;###autoload
(defun wisi-process-parse-get (parser)
  "Return a ‘wisi-process--parser’ object matching PARSER label.
If label found in ‘wisi-process--alist’, return that.
Otherwise add PARSER to ‘wisi-process--alist’, return it."
  (or (cdr (assoc (wisi-process--parser-label parser) wisi-process--alist))
      (let ((exec-file (locate-file (wisi-process--parser-exec-file parser) exec-path '("" ".exe"))))

	(unless exec-file
	  (error "%s not found on `exec-path'; run 'build.sh' in the ELPA package."
		 (wisi-process--parser-exec-file parser)))

	(push (cons (wisi-process--parser-label parser) parser) wisi-process--alist)

	parser
     )))

(defun wisi-process-parse-set-exec (label exec-file)
  "Change the EXEC-FILE for parsers with LABEL."
  (let ((parser (cdr (assoc label wisi-process--alist))))
    (when parser
      (wisi-parse-kill parser)
      (setf (wisi-process--parser-exec-file parser) exec-file))))

(defun wisi-process-parse--check-version (parser)
  "Verify protocol version reported by process."
  ;; The process has just started; the first non-comment line in the
  ;; process buffer contains the process and language protocol versions.
  (with-current-buffer (wisi-process--parser-buffer parser)
    (goto-char (point-min))
    (if (search-forward-regexp "protocol: process version \\([0-9]+\\) language version \\([0-9]+\\)" nil t)
	(unless (and (match-string 1)
		     (string-equal (match-string 1) wisi-process-parse-protocol-version)
		     (match-string 2)
		     (string-equal (match-string 2) (wisi-process--parser-language-protocol-version parser)))
	  (wisi-parse-kill parser)
	  (error "%s parser process protocol version mismatch: elisp %s %s, process %s %s"
		 (wisi-process--parser-label parser)
		 wisi-process-parse-protocol-version (wisi-process--parser-language-protocol-version parser)
		 (match-string 1) (match-string 2)))
      ;; Search failed
      (error "%s parser process protocol version message not found"
	     (wisi-process--parser-label parser))
    )))

(defun wisi-process-parse--require-process (parser)
  "Start the process for PARSER if not already started."
  (unless (process-live-p (wisi-process--parser-process parser))
    (let ((process-connection-type nil) ;; use a pipe, not a pty; avoid line-by-line reads
	  (process-name (format " *%s_wisi_parse*" (wisi-process--parser-label parser))))

      (unless (buffer-live-p (wisi-process--parser-buffer parser))
	;; User may have killed buffer to kill parser.
	(setf (wisi-process--parser-buffer parser)
	      (get-buffer-create process-name)))

      (with-current-buffer (wisi-process--parser-buffer parser)
	(erase-buffer)); delete any previous messages, prompt

      (setf (wisi-process--parser-process parser)
	    (make-process
	     :name process-name
	     :buffer (wisi-process--parser-buffer parser)
	     :command (append (list (wisi-process--parser-exec-file parser))
			      (wisi-process--parser-exec-opts parser))))

      (set-process-query-on-exit-flag (wisi-process--parser-process parser) nil)

      (wisi-process-parse--wait parser)
      (wisi-process-parse--check-version parser)
      )))

(defun wisi-process-parse--wait (parser)
  "Wait for the current command to complete."
  (let ((process (wisi-process--parser-process parser))
	(search-start (point-min))
	(wait-count 0)
	(found nil))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward wisi-process-parse-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(accept-process-output process 0.1))

      (unless found
	(wisi-process-parse-show-buffer parser)
	(error "%s process died" (wisi-process--parser-exec-file parser)))
      )))

(defun wisi-process-parse-show-buffer (parser)
  "Show PARSER buffer."
  (if (buffer-live-p (wisi-process--parser-buffer parser))
      (pop-to-buffer (wisi-process--parser-buffer parser))
    (error "wisi-process-parse process not active")))

(defun wisi-process-parse--send-parse (parser begin send-end parse-end)
  "Send a parse command to PARSER external process, followed by
the content of the current buffer from BEGIN thru SEND-END.  Does
not wait for command to complete. PARSE-END is end of desired
parse region."
  ;; Must match "parse" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Parse_Params.
  (let* ((cmd (format "parse %d \"%s\" %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %s"
		      (cl-ecase wisi--parse-action
			(navigate 0)
			(face 1)
			(indent 2))
		      (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		      (position-bytes begin)
		      (position-bytes send-end)
		      (position-bytes parse-end)
		      begin ;; char_pos
		      (line-number-at-pos begin)
		      (line-number-at-pos send-end)
		      (save-excursion (goto-char begin) (back-to-indentation) (current-column));; indent-begin
		      (if (or (and (= begin (point-min)) (= parse-end (point-max)))
			      (< (point-max) wisi-partial-parse-threshold))
			  0 1) ;; partial parse active
		      (if (> wisi-debug 0) 1 0) ;; debug_mode
		      (1- wisi-debug) ;; trace_parse
		      wisi-trace-mckenzie
		      wisi-trace-action
		      (if wisi-mckenzie-disable 1 0)
		      (or wisi-mckenzie-task-count -1)
		      (or wisi-mckenzie-check-limit -1)
		      (or wisi-mckenzie-enqueue-limit -1)
		      (or wisi-parse-max-parallel -1)
		      (- (position-bytes send-end) (position-bytes begin)) ;; send-end is after last byte
		      (wisi-parse-format-language-options parser)
		      ))
	 (msg (format "%03d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process (buffer-substring-no-properties begin send-end))

    ;; We don’t wait for the send to complete; the external process
    ;; may start parsing and send an error message.
    ))

(defun wisi-process-parse--send-refactor (parser refactor-action parse-begin parse-end edit-begin)
  "Send a refactor command to PARSER external process, followed
by the content of the current buffer from PARSE-BEGIN thru
PARSE-END, wait for command to complete. PARSER will respond with
one or more Edit messages."
  ;; Must match "refactor" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Refactor_Params.
  (let* ((cmd (format "refactor %d \"%s\" %d %d %d %d %d %d %d %d %d %d %d %d"
		      refactor-action
		      (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		      (position-bytes parse-begin)
		      (position-bytes parse-end)
		      (position-bytes edit-begin)
		      parse-begin ;; char_pos
		      (line-number-at-pos parse-begin)
		      (line-number-at-pos parse-end)
		      (save-excursion (goto-char parse-begin) (back-to-indentation) (current-column));; indent-begin
		      (if (> wisi-debug 0) 1 0) ;; debug-mode
		      (1- wisi-debug) ;; trace_parse
		      wisi-trace-action
		      (or wisi-parse-max-parallel -1)
		      (- (position-bytes parse-end) (position-bytes parse-begin)) ;; parse-end is after last byte
		      ))
	 (msg (format "%03d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process (buffer-substring-no-properties parse-begin parse-end))
    (wisi-process-parse--wait parser)
    ))

(defun wisi-process-parse--send-noop (parser)
  "Send a noop command to PARSER external process, followed by
the content of the current buffer.  Does not wait for command to
complete."
  (let* ((cmd (format "noop %d" (1- (position-bytes (point-max)))))
	 (msg (format "%03d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process (buffer-substring-no-properties (point-min) (point-max)))
    ))

(defun wisi-process-parse--marker-or-nil (item)
  (if (= -1 item) nil (copy-marker item t)))

(defun wisi-process-parse--Navigate_Cache (parser sexp)
  ;; sexp is [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1)))
    (with-silent-modifications
      (put-text-property
       pos
       (1+ pos)
       'wisi-cache
       (wisi-cache-create
	:nonterm    (aref (wisi-process--parser-token-table parser) (aref sexp 2))
	:token      (aref (wisi-process--parser-token-table parser) (aref sexp 3))
	:last       (aref sexp 4)
	:class      (aref wisi-class-list (aref sexp 5))
	:containing (wisi-process-parse--marker-or-nil (aref sexp 6))
	:prev       (wisi-process-parse--marker-or-nil (aref sexp 7))
	:next       (wisi-process-parse--marker-or-nil (aref sexp 8))
	:end        (wisi-process-parse--marker-or-nil (aref sexp 9))
	)))
    ))

(defun wisi-process-parse--Name_Property (parser sexp)
  ;; sexp is [Name_Property first-pos last-pos]
  ;; see ‘wisi-process-parse--execute’
  ;; implements wisi-name-action
  (with-silent-modifications
    (put-text-property (aref sexp 1) (1+ (aref sexp 2)) 'wisi-name t)))

(defun wisi-process-parse--Face_Property (parser sexp)
  ;; sexp is [Face_Property first-pos last-pos face-index]
  ;; see ‘wisi-process-parse--execute’
  ;; implements wisi--face-action-1
  (with-silent-modifications
    (add-text-properties
     (aref sexp 1)
     (1+ (aref sexp 2))
     (list 'font-lock-face (aref (wisi-process--parser-face-table parser) (aref sexp 3))
	   'fontified t)
     )))

(defun wisi-process-parse--Indent (parser sexp)
  ;; sexp is [Indent line-number indent]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref (wisi-process--parser-line-begin parser) (1- (aref sexp 1)))))
    (with-silent-modifications
      (when (< (point-min) pos)
	(put-text-property
	 (1- pos)
	 pos
	 'wisi-indent
	 (aref sexp 2)))
      )))

(defun wisi-process-parse--Lexer_Error (parser sexp)
  ;; sexp is [Lexer_Error char-position <message> <repair-char>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--lexer-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (current-column)
		   (aref sexp 2))
	   :inserted (when (= 4 (length sexp)) (aref sexp 3))))

    (push err (wisi-parser-lexer-errors parser))
    ))

(defun wisi-process-parse--Parser_Error (parser sexp)
  ;; sexp is [Parser_Error char-position <string>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--parse-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (1+ (current-column))
		   (aref sexp 2))))

    (push err (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--Check_Error (parser sexp)
  ;; sexp is [Check_Error code name-1-pos name-2-pos <string>]
  ;; see ‘wisi-process-parse--execute’
  (let* ((name-1-pos (aref sexp 2))
	(name-1-col (1+ (progn (goto-char name-1-pos)(current-column)))) ;; gnat columns are 1 + emacs columns
	(name-2-pos (aref sexp 3))
	(name-2-col (1+ (progn (goto-char name-2-pos)(current-column))))
	(file-name (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) ""))
	;; file-name can be nil during vc-resolve-conflict
	(err (make-wisi--parse-error
	      :pos (copy-marker name-1-pos)
	      :message
	      (format "%s:%d:%d: %s %s:%d:%d"
		      file-name (line-number-at-pos name-1-pos) name-1-col
		      (aref sexp 4)
		      file-name (line-number-at-pos name-2-pos) name-2-col)))
	)

    (push err (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--Recover (parser sexp)
  ;; sexp is [Recover [pos [inserted] [deleted] deleted-region]...]
  ;; see ‘wisi-process-parse--execute’
  ;; convert to list of wisi--parse-error-repair, add to last error
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (last-error (car (wisi-parser-parse-errors parser))))
    (unless (= 1 (length sexp))
      (cl-do ((i 1 (1+ i))) ((= i (length sexp)))
	(push
	 (make-wisi--parse-error-repair
	  :pos (copy-marker (aref (aref sexp i) 0))
	  :inserted (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 1))
	  :deleted  (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 2))
	  :deleted-region (aref (aref sexp i) 3))
	 (wisi--parse-error-repair last-error)))
      )))

(defun wisi-process-parse--End (parser sexp)
  ;; sexp is [End pos]
  ;; see ‘wisi-process-parse--execute’
  (setf (wisi-process--parser-end-pos parser) (1+ (aref sexp 1))))

(defun wisi-process-parse--Edit (parser sexp)
  ;; sexp is [Edit begin end text]
  (delete-region (aref sexp 1) (1+ (aref sexp 2)))
  (goto-char (aref sexp 1))
  (insert (aref sexp 3)))

(defun wisi-process-parse--Language (parser sexp)
  ;; sexp is [Language language-action ...]
  (funcall (aref (wisi-process--parser-language-action-table parser) (aref sexp 1)) sexp))

(defun wisi-process-parse--execute (parser sexp)
  "Execute encoded SEXP sent from external process."
  ;; sexp is [action arg ...]; an encoded instruction that we need to execute
  ;;
  ;; Actions:
  ;;
  ;; [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;;    Set a wisi-cache text-property.
  ;;    *pos          : integer buffer position; -1 if nil (not set)
  ;;    *id           : integer index into parser-token-table
  ;;    length        : integer character count
  ;;    class         : integer index into wisi-class-list
  ;;
  ;; [Name_Property first-pos last-pos]
  ;;
  ;; [Face_Property first-pos last-pos face-index]
  ;;    Set a font-lock-face text-property
  ;;    face-index: integer index into parser-elisp-face-table
  ;;
  ;; [Indent line-number indent]
  ;;    Set an indent text property
  ;;
  ;; [Lexer_Error char-position <message> <repair-char>]
  ;;    The lexer detected an error at char-position.
  ;;
  ;;    If <repair-char> is not ASCII NUL, it was inserted immediately
  ;;    after char-position to fix the error.
  ;;
  ;; [Parser_Error char-position <message>]
  ;;    The parser detected a syntax error; save information for later
  ;;    reporting.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Check_Error code name-1-pos name-2-pos <string>]
  ;;    The parser detected a semantic check error; save information
  ;;    for later reporting.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Recover [pos [inserted] [deleted] deleted-region]...]
  ;;    The parser finished a successful error recovery.
  ;;
  ;;    pos: Buffer position
  ;;
  ;;    inserted: Virtual tokens (terminal or non-terminal) inserted
  ;;    before pos.
  ;;
  ;;    deleted: Tokens deleted after pos.
  ;;
  ;;    deleted-region: source buffer region containing deleted tokens
  ;;
  ;;    Args are token ids; index into parser-token-table. Save the
  ;;    information for later use by ’wisi-repair-error’.
  ;;
  ;; [Edit begin end text]
  ;;    Replace region BEGIN . END with TEXT; normally the result of a
  ;;    refactor command.
  ;;
  ;; [Language ...]
  ;;    Dispatch to a language-specific action, via
  ;;    `wisi-process--parser-language-action-table'.
  ;;
  ;;
  ;; Numeric action codes are given in the case expression below

  (cl-ecase (aref sexp 0)
    (1  (wisi-process-parse--Navigate_Cache parser sexp))
    (2  (wisi-process-parse--Face_Property parser sexp))
    (3  (wisi-process-parse--Indent parser sexp))
    (4  (wisi-process-parse--Lexer_Error parser sexp))
    (5  (wisi-process-parse--Parser_Error parser sexp))
    (6  (wisi-process-parse--Check_Error parser sexp))
    (7  (wisi-process-parse--Recover parser sexp))
    (8  (wisi-process-parse--End parser sexp))
    (9  (wisi-process-parse--Name_Property parser sexp))
    (10 (wisi-process-parse--Edit parser sexp))
    (11 (wisi-process-parse--Language parser sexp))
    ))

;;;;; main

(cl-defmethod wisi-parse-kill ((parser wisi-process--parser))
  (when (process-live-p (wisi-process--parser-process parser))
    ;; We used to send a quit command first, to be nice. But there's
    ;; no timeout on that, so it would hang when the process
    ;; executable is not reading command input.
    (when (process-live-p (wisi-process--parser-process parser))
      (kill-process (wisi-process--parser-process parser)))
    )
  (setf (wisi-process--parser-busy parser) nil))

(defvar wisi--lexer nil) ;; wisi-elisp-lexer.el
(declare-function wisi-elisp-lexer-reset "wisi-elisp-lexer")

(defun wisi-process-parse--prepare (parser)
  ;; font-lock can trigger a face parse while navigate or indent parse
  ;; is active, due to ‘accept-process-output’ below. Signaling an
  ;; error tells font-lock to try again later.
  (if (wisi-process--parser-busy parser)
      (progn
  	(setf (wisi-parser-parse-errors parser)
	      (list
	       (make-wisi--parse-error
		:pos 0
		:message (format "%s:%d:%d: parser busy (try ’wisi-kill-parser’)"
				 (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "") 1 1))
	       ))
	(error "%s parse abandoned; parser busy - use partial parse?" wisi--parse-action)
	)

    ;; It is not possible for a background elisp function (ie
    ;; font-lock) to interrupt this code between checking and setting
    ;; parser-busy; background elisp can only run when we call
    ;; accept-process-output below.
    (setf (wisi-process--parser-busy parser) t)

    ;; If the parser process has not started yet,
    ;; wisi-process-parse--require-process calls
    ;; wisi-process-parse--wait, which can let font-lock invoke the
    ;; parser again. Thus this call must be after we set
    ;; wisi-process--parser-busy t
    (wisi-process-parse--require-process parser)

    (setf (wisi-process--parser-total-wait-time parser) 0.0)
    (setf (wisi-parser-lexer-errors parser) nil)
    (setf (wisi-parser-parse-errors parser) nil)
    ))

(defun wisi-process-parse--handle-messages (parser)
  (condition-case-unless-debug err
      (let* ((source-buffer (current-buffer))
	     (response-buffer (wisi-process--parser-buffer parser))
	     (process (wisi-process--parser-process parser))
	     (w32-pipe-read-delay 0) ;; fastest subprocess read
	     response
	     response-end
	     (response-count 0)
	     sexp-start
	     (need-more nil) ;; point-max if need more, to check for new input
	     (done nil)
	     start-wait-time)

	(set-buffer response-buffer)
	(setq sexp-start (point-min))

	;; process responses until prompt received
	(while (not done)

	  ;; process all complete responses currently in buffer
	  (while (and (not need-more)
		      (not done))

	    (goto-char sexp-start)

	    (cond
	     ((eobp)
	      (setq need-more (point-max)))

	     ((looking-at wisi-process-parse-prompt)
	      (setq done t))

	     ((or (looking-at "\\[") ;; encoded action
		  (looking-at "(")) ;; error or other elisp expression to eval
	      (condition-case nil
		  (setq response-end (scan-sexps (point) 1))
		(error
		 ;; incomplete response
		 (setq need-more (point-max))
		 nil))

	      (unless need-more
		(setq response-count (1+ response-count))
		(setq response (car (read-from-string (buffer-substring-no-properties (point) response-end))))
		(goto-char response-end)
		(forward-line 1)
		(setq sexp-start (point))

		(set-buffer source-buffer) ;; for put-text-property in actions
		(cond
		 ((listp response)
		  ;; non-syntax error of some sort
		  (cond
		   ((equal '(parse_error) response)
		    ;; Parser detected a syntax error, and recovery failed, so signal it.

		    (when (> wisi-debug 0)
		      ;; Save a copy of parser output; may be overwritten by subsequent parse face attempts.
		      (set-buffer response-buffer)
		      (let ((content (buffer-substring-no-properties (point-min) (point-max)))
			    (buf-name (concat (buffer-name) "-save-error")))
			(set-buffer (get-buffer-create buf-name))
			(insert content)))

		    (if (wisi-parser-parse-errors parser)
			(signal 'wisi-parse-error
				(wisi--parse-error-message (car (wisi-parser-parse-errors parser))))

		      ;; can have no errors when testing a new parser
		      (push
		       (make-wisi--parse-error :pos 0 :message "parser failed with no message")
		       (wisi-parser-parse-errors parser))
		      (signal 'wisi-parse-error "parser failed with no message")))

		   ((equal 'parse_error (car response))
		    ;; Parser detected some other error non-fatal error, so signal it.
		    (push
		     (make-wisi--parse-error :pos 0 :message (cadr response))
		     (wisi-parser-parse-errors parser))
		    (signal 'wisi-parse-error (cdr response)))

		   ((and (eq 'error (car response))
			 (string-prefix-p "bad command:" (cadr response)))
		    ;; Parser dropped bytes, is treating buffer
		    ;; content bytes as commands. Kill the process
		    ;; to kill the pipes; there is no other way to
		    ;; flush them.
		    (kill-process (wisi-process--parser-process parser))
		    (signal 'wisi-parse-error "parser lost sync; killed"))

		   (t
		    ;; Some other error
		    (condition-case-unless-debug err
			(eval response)
		      (error
		       (push (make-wisi--parse-error :pos (point) :message (cadr err)) (wisi-parser-parse-errors parser))
		       (signal (car err) (cdr err)))))
		   ))

		 ((arrayp response)
		  ;; encoded action
		  (condition-case-unless-debug err
		      (wisi-process-parse--execute parser response)
		    (wisi-parse-error
		     (push (make-wisi--parse-error :pos (point) :message (cadr err)) (wisi-parser-parse-errors parser))
		     (signal (car err) (cdr err)))

		    (error ;; ie from un-commented [C:\Windows\system32\KERNEL32.DLL], or bug in action code above.
		     (set-buffer response-buffer)
		     (let ((content (buffer-substring-no-properties (point-min) (point-max)))
			   (buf-name (concat (buffer-name) "-save-error")))
		       (set-buffer (get-buffer-create buf-name))
		       (insert content)
		       (insert (format "%s" err))
		       (error "parser failed; error messages in %s" buf-name)))
		    ))
		 )

		(set-buffer response-buffer)
		))

	     (t
	      ;; debug output
	      (forward-line 1)
	      (setq sexp-start (point)))
	     )
	      )

	    (unless done
	      ;; end of response buffer
	      (unless (process-live-p process)
		(set-buffer response-buffer)
		(let ((content (buffer-substring-no-properties (point-min) (point-max)))
		      (buf-name (concat (buffer-name) "-save-error")))
		  (set-buffer (get-buffer-create buf-name))
		  (insert content)
		  (error "parser failed; error messages in %s" buf-name)))

	      (setq start-wait-time (float-time))

	      ;; If we specify no time-out here, we get messages about
	      ;; "blocking call with quit inhibited", when this is
	      ;; called by font-lock from the display engine.
	      ;;
	      ;; Specifying just-this-one t prevents C-q from
	      ;; interrupting this?
	      (accept-process-output
	       process
	       wisi-process-time-out
	       nil ;; milliseconds
	       nil)  ;; just-this-one

	      (setf (wisi-process--parser-total-wait-time parser)
		    (+ (wisi-process--parser-total-wait-time parser)
		       (- (float-time) start-wait-time)))

	      (when (and (= (point-max) need-more)
			 (> (wisi-process--parser-total-wait-time parser) wisi-process-time-out))
		(error "wisi-process-parse timing out; increase `wisi-process-time-out'? (or bad syntax in process output)"))

	      (setq need-more nil))
	    );; while not done

	  ;; got command prompt
	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer parser)
	    (error "wisi-process-parse process died"))

	  (setf (wisi-process--parser-response-count parser) response-count)

	  (setf (wisi-process--parser-busy parser) nil)
	  (set-buffer source-buffer)
	  ;; If we get here, the parse succeeded (possibly with error
	  ;; recovery); move point to end of parsed region.
	  (goto-char (wisi-process--parser-end-pos parser))
	  )

      (wisi-parse-error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err)))

      (error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err))
       )))

(cl-defmethod wisi-parse-current ((parser wisi-process--parser) begin send-end parse-end)
  (wisi-process-parse--prepare parser)
  (let ((total-line-count (1+ (count-lines (point-max) (point-min)))))
    (setf (wisi-process--parser-line-begin parser) (wisi--set-line-begin total-line-count))
    (wisi-process-parse--send-parse parser begin send-end parse-end)

    ;; We reset the elisp lexer, because post-parse actions may use it.
    (when wisi--lexer
      (wisi-elisp-lexer-reset total-line-count wisi--lexer))
    )
  (wisi-process-parse--handle-messages parser)
  (cons begin (point))
  )

(cl-defmethod wisi-refactor ((parser wisi-process--parser) refactor-action parse-begin parse-end edit-begin)
  (save-excursion
    (wisi-process-parse--prepare parser)
    (wisi-process-parse--send-refactor parser refactor-action parse-begin parse-end edit-begin)
    (wisi-process-parse--handle-messages parser))
  )

(defvar wisi--parser nil) ;; wisi.el

(defun wisi-process-send-tokens-noop ()
  "Run lexer, send tokens to subprocess; otherwise no operation.
For use with ’wisi-time’."
  (wisi-process-parse--require-process wisi--parser)
  (if (wisi-process--parser-busy wisi--parser)
      (error "%s parser busy" wisi--parse-action)

    ;; not busy
    (let* ((source-buffer (current-buffer))
	   (action-buffer (wisi-process--parser-buffer wisi--parser))
	   (process (wisi-process--parser-process wisi--parser))
	   (sexp-start (point-min))
	   (need-more nil)
	   (done nil))

      (setf (wisi-process--parser-busy wisi--parser) t)
      (wisi-process-parse--send-noop wisi--parser)

      (set-buffer action-buffer)
      (while (and (process-live-p process)
		  (not done))
	(goto-char sexp-start)
	(cond
	 ((eobp)
	  (setq need-more t))

	 ((looking-at wisi-process-parse-prompt)
	  (setq done t))

	 (t
	  (forward-line 1)
	  (setq sexp-start (point)))
	 )

	(unless done
	  ;; end of response buffer
	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer wisi--parser)
	    (error "wisi-process-parse process died"))

	  (accept-process-output process 1.0 nil nil)
	  (setq need-more nil))
	)
      (set-buffer source-buffer)
      (setf (wisi-process--parser-busy wisi--parser) nil)
      )))

;;;;; debugging
(defun wisi-process-parse-ids-to-enum (token-table &rest int-ids)
  "Translate INT-IDS from process integer token ids to elisp enumeral ids.
Returns reversed sequence."
  (let ((enum-ids nil))
    (cl-dolist (i int-ids)
      (push (aref token-table i) enum-ids))
    enum-ids))

(defun wisi-process-parse-show-args ()
  "Show the partial parse command-line args for run_ada_[lalr | lr1]_parse for current region.
Also add it to the kill ring."
  (interactive)
  (let* ((begin (region-beginning))
	 (end   (region-end))
	 (parse-action (wisi-read-parse-action))
	 (msg
	  (format "%s %s %d %d %d %d %d %d %d"
		  (file-name-nondirectory (buffer-file-name))
		  parse-action
		  (position-bytes begin)
		  (position-bytes end)
		  (position-bytes end)
		  begin ;; char_pos
		  (line-number-at-pos begin)
		  (line-number-at-pos end)
		  (save-excursion (goto-char begin) (back-to-indentation) (current-column));; indent-begin
		  )))
    (kill-new msg)
    (message msg)))

(provide 'wisi-process-parse)

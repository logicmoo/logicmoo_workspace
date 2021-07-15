;;; gnorb-bbdb.el --- The BBDB-centric functions of gnorb  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Gnorb package has no hard dependency on BBDB, so you'll have to
;; install it manually.  Gnorb is compatible with whichever version of
;; BBDB is current in the Emacs package manager.  I believe it comes
;; from Melpa.

;;; Code:

;; Don't explicity require BBDB libraries.  BBDB is the "weakest leg"
;; of the Gnorb tripod: it has the least functionality, and many
;; people are using Gnorb without BBDB. So don't require, only
;; autoload.

;; (require 'bbdb)
;; (require 'bbdb-com)
;; (require 'bbdb-mua)

(autoload 'bbdb-do-records "bbdb-com")
(autoload 'bbdb-completing-read-record "bbdb-com")
(autoload 'bbdb-current-record "bbdb")

(defvar bbdb-buffer-name)
(defvar bbdb-separator-alist)
(defvar bbdb-crm-local-completion-map)

(require 'gnorb-utils)
(require 'cl-lib)

(defgroup gnorb-bbdb nil
  "The BBDB bits of gnorb."
  :tag "Gnorb BBDB"
  :group 'gnorb)

(defcustom gnorb-bbdb-org-tag-field 'org-tags
  "The name (as a symbol) of the field to use for org tags."
  :type 'symbol)

(when (boundp 'bbdb-separator-alist)    ;Allow compilation if BBDB is absent!
  (unless (assoc gnorb-bbdb-org-tag-field bbdb-separator-alist)
    (push `(,gnorb-bbdb-org-tag-field ":" ":") bbdb-separator-alist)))

(defcustom gnorb-bbdb-messages-field 'messages
  "The name (as a symbol) of the field where links to recent gnus
messages from this record are stored.

\\<bbdb-mode-map>Records that do not have this field defined
will not collect links to messages: you have to call
\"\\[gnorb-bbdb-open-link]\" on the record once -- after that,
message links will be collected and updated automatically."
  :type 'symbol)

(defcustom gnorb-bbdb-collect-N-messages 5
  "For records with a `gnorb-bbdb-messages-field' defined,
collect links to a maximum of this many messages."
  :type 'integer)

(defcustom gnorb-bbdb-define-recent 'seen
  "For records with a `gnorb-bbdb-message-tag-field' defined,
this variable controls how gnorb defines a \"recent\" message.
Setting it to the symbol seen will collect the messages most
recently opened and viewed. The symbol received means gnorb will
collect the most recent messages by Date header.

In other words, if this variable is set to `received', and a
record's messages field is already full of recently-received
messages, opening a five-year-old message (for instance) from
this record will not push a link to the message into the field."
  :type '(choice (const :tag "Most recently seen" 'seen)
                 (const :tag "Most recently received" 'received)))

(defcustom gnorb-bbdb-message-link-format-multi "%:count. %D: %:subject"
  "How a single message is formatted in the list of recent messages.
This format string is used in multi-line record display.

Available information for each message includes the subject, the
date, and the message's count in the list, as an integer. You can
access subject and count using the %:subject and %:count escapes.
The message date can be formatted using any of the escapes
mentioned in the docstring of `format-time-string', which see."
  :type 'string)

(defcustom gnorb-bbdb-message-link-format-one "%:count"
  "How a single message is formatted in the list of recent messages.
This format string is used in single-line display -- note that by
default, no user-created xfields are displayed in the `one-line'
layout found in `bbdb-layout-alist'. If you want this field to
appear there, put its name in the \"order\" list of the `one-line'
layout.

Available information for each message includes the subject, the
date, and the message's count in the list, as an integer. You can
access subject and count using the %:subject and %:count escapes.
The message date can be formatted using any of the escapes
mentioned in the docstring of `format-time-string', which see."
  :type 'string)

(defface gnorb-bbdb-link '((t :inherit org-link))
  "Custom face for displaying message links in the *BBDB* buffer.
  Defaults to org-link.")

(cl-defstruct gnorb-bbdb-link
  subject date group id)

(defcustom gnorb-bbdb-posting-styles nil
  "An alist of styles to use when composing messages to the BBDB
record(s) under point. This is entirely analogous to
`gnus-posting-styles', it simply works by examining record fields
rather than group names.

When composing a message to multiple contacts (using the \"*\"
prefix), the records will be scanned in order, with the record
initially under point (if any) set aside for last. That means
that, in the case of conflicting styles, the record under point
will override the others.

In order not to be too intrusive, this option has no effect on
the usual `bbdb-mail' command. Instead, the wrapper command
`gnorb-bbdb-mail' is provided, which consults this option and
then hands off to `bbdb-compose-mail'. If you'd always like to
use `gnorb-bbdb-mail', you can simply bind it to \"m\" in the
`bbdb-mode-map'.

The value of the option should be a list of sexps, each one
matching a single field. The first element should match a field
name: one of the built-in fields like lastname, or an xfield.
Field names should be given as symbols.

The second element is a regexp used to match against the value of
the field (non-string field values will be cast to strings, if
possible). It can also be a cons of two strings, the first of
which matches the field label, the second the field value.

Alternately, the first element can be the name of a custom
function that is called with the record as its only argument, and
returns either t or nil. In this case, the second element of the
list is disregarded.

All following elements should be field setters for the message to
be composed, just as in `gnus-posting-styles'."
  :type 'list)

(when (fboundp 'bbdb-record-xfield-string)
  (defalias (intern (format "bbdb-read-xfield-%s"
			    gnorb-bbdb-org-tag-field))
    (lambda (&optional init)
      (gnorb-bbdb-read-org-tags init)))

  (defalias (intern (format "bbdb-display-%s-multi-line"
			    gnorb-bbdb-org-tag-field))
    (lambda (record indent)
      (gnorb-bbdb-display-org-tags record indent))))

(defun gnorb-bbdb-read-org-tags (&optional init)
  "Read Org mode tags, with `completing-read-multiple'."
  (if (string< "24.3" (substring emacs-version 0 4))
      (let ((crm-separator
	     (concat "[ \t\n]*"
		     (cadr (assq gnorb-bbdb-org-tag-field
				 bbdb-separator-alist))
		     "[ \t\n]*"))
	    (crm-local-completion-map bbdb-crm-local-completion-map)
	    (table (cl-mapcar #'car
			      (org-global-tags-completion-table
			       (org-agenda-files))))
	    (init (if (consp init)
		      (apply #'bbdb-concat (nth 2 (assq gnorb-bbdb-org-tag-field
							bbdb-separator-alist))
			     init)
		    init)))
	(completing-read-multiple
	 "Tags: " table
	 nil nil init))
    (bbdb-split gnorb-bbdb-org-tag-field
		(bbdb-read-string "Tags: " init))))

(defun gnorb-bbdb-display-org-tags (record indent)
  "Display the Org tags associated with the record.

Org tags are stored in the `gnorb-bbdb-org-tags-field'."
  (let ((full-field (assq gnorb-bbdb-org-tag-field
			  (bbdb-record-xfields record)))
	(val (bbdb-record-xfield
	      record
	      gnorb-bbdb-org-tag-field)))
    (when val
      (bbdb-display-text (format (format " %%%ds: " (- indent 3))
				 gnorb-bbdb-org-tag-field)
			 `(xfields ,full-field field-label)
			 'bbdb-field-name)
      (if (consp val)
	  (bbdb-display-list val gnorb-bbdb-org-tag-field "\n")
	(insert
	 (bbdb-indent-string (concat val "\n") indent))))))

(defvar message-mode-hook)

;;;###autoload
(defun gnorb-bbdb-mail (records &optional subject n verbose)
  "\\<bbdb-mode-map>Acts just like `bbdb-mail', except runs
RECORDS through `gnorb-bbdb-posting-styles', allowing
customization of message styles for certain records. From the
`bbdb-mail' docstring:

Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \\[bbdb-do-all-records], see
`bbdb-do-all-records'. By default, the first mail addresses of
RECORDS are used. If prefix N is a number, use Nth mail address
of RECORDS (starting from 1). If prefix N is C-u (t
noninteractively) use all mail addresses of RECORDS. If VERBOSE
is non-nil (as in interactive calls) be verbose."
  ;; see the function `gnus-configure-posting-styles' for tips on how
  ;; to actually do this.
  (interactive (list (bbdb-do-records) nil
		     (or (consp current-prefix-arg)
                         current-prefix-arg)
		     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (user-error "No records displayed")
    (let ((head (bbdb-current-record))
	  (to (bbdb-mail-address records n nil verbose))
	  (message-mode-hook (copy-sequence message-mode-hook)))
      (setq records (remove head records))
      (when gnorb-bbdb-posting-styles
	(add-hook 'message-mode-hook
		  `(lambda ()
		     (gnorb-bbdb-configure-posting-styles (quote ,records))
		     (gnorb-bbdb-configure-posting-styles (list ,head)))))
      (bbdb-compose-mail to subject))))

(defun gnorb-bbdb-configure-posting-styles (recs)
  ;; My most magnificent work of copy pasta!
  (dolist (r recs)
    (let (field val label rec-val filep
		element v value results name address)
      (dolist (style gnorb-bbdb-posting-styles)
	(setq field (pop style)
	      val (pop style))
	(when (consp val) ;; (label value)
	  (setq label (pop val)
		val (pop val)))
	(unless (fboundp field)
	  ;; what's the record's existing value for this field?
	  (setq rec-val (bbdb-record-field r field)))
	(when (catch 'match
		(cond
		 ((eq field 'address)
		  (dolist (a rec-val)
		    (unless (and label
				 (not (string-match label (car a))))
		      (when
			  (string-match-p
			   val
			   (bbdb-format-address-default a))
			(throw 'match t)))))
		 ((eq field 'phone)
		  (dolist (p rec-val)
		    (unless (and label
				 (not (string-match label (car p))))
		      (when
			  (string-match-p val (bbdb-phone-string p))
			(throw 'match t)))))
		 ((consp rec-val)
		  (dolist (f rec-val)
		    (when (string-match-p val f)
		      (throw 'match t))))
		 ((fboundp field)
		  (when (string-match-p (funcall field r))
		    (throw 'match t)))
		 ((stringp rec-val)
		  (when (string-match-p val rec-val)
		    (throw 'match t)))))
	  ;; there are matches, run through the field setters in last
	  ;; element of the sexp
	  (dolist (attribute style)
	    (setq element (pop attribute)
		  filep nil)
	    (setq value
		  (cond
		   ((eq (car attribute) :file)
		    (setq filep t)
		    (cadr attribute))
		   ((eq (car attribute) :value)
		    (cadr attribute))
		   (t
		    (car attribute))))
	    ;; We get the value.
	    (setq v
		  (cond
		   ((stringp value)
		    value)
		   ((or (symbolp value)
			(functionp value))
		    (cond ((functionp value)
			   (funcall value))
			  ((boundp value)
			   (symbol-value value))))
		   ((listp value)
		    (eval value))))
	    ;; Post-processing for the signature posting-style:
	    (and (eq element 'signature) filep
		 message-signature-directory
		 ;; don't actually use the signature directory
		 ;; if message-signature-file contains a path.
		 (not (file-name-directory v))
		 (setq v (nnheader-concat message-signature-directory v)))
	    ;; Get the contents of file elems.
	    (when (and filep v)
	      (setq v (with-temp-buffer
			(insert-file-contents v)
			(buffer-substring
			 (point-min)
			 (progn
			   (goto-char (point-max))
			   (if (zerop (skip-chars-backward "\n"))
			       (point)
			     (1+ (point))))))))
	    (setq results (delq (assoc element results) results))
	    (push (cons element v) results))))
      (setq name (assq 'name results)
	    address (assq 'address results))
      (setq results (delq name (delq address results)))
      (setq results (sort results (lambda (x y)
				    (string-lessp (car x) (car y)))))
      (dolist (result results)
	(add-hook 'message-setup-hook
		  (cond
		   ((eq 'eval (car result))
		    'ignore)
		   ((eq 'body (car result))
		    `(lambda ()
		       (save-excursion
			 (message-goto-body)
			 (insert ,(cdr result)))))
		   ((eq 'signature (car result))
		    (set (make-local-variable 'message-signature) nil)
		    (set (make-local-variable 'message-signature-file) nil)
		    (if (not (cdr result))
			'ignore
		      `(lambda ()
			 (save-excursion
			   (let ((message-signature ,(cdr result)))
			     (when message-signature
			       (message-insert-signature)))))))
		   (t
		    (let ((header
			   (if (symbolp (car result))
			       (capitalize (symbol-name (car result)))
			     (car result))))
		      `(lambda ()
			 (save-excursion
			   (message-remove-header ,header)
			   (let ((value ,(cdr result)))
			     (when value
			       (message-goto-eoh)
			       (insert ,header ": " value)
			       (unless (bolp)
				 (insert "\n")))))))))
		  t 'local))
      (when (or name address)
	(add-hook 'message-setup-hook
		  `(lambda ()
		     (set (make-local-variable 'user-mail-address)
			  ,(or (cdr address) user-mail-address))
		     (let ((user-full-name ,(or (cdr name) (user-full-name)))
			   (user-mail-address
			    ,(or (cdr address) user-mail-address)))
		       (save-excursion
			 (message-remove-header "From")
			 (message-goto-eoh)
			 (insert "From: " (message-make-from) "\n"))))
		  t 'local)))))

;;;###autoload
(defun gnorb-bbdb-tag-agenda (records)
  "Open an Org agenda tags view from the BBDB buffer, using the
value of the record's org-tags field. This shows only TODOs by
default; a prefix argument shows all tagged headings; a \"*\"
prefix operates on all currently visible records. If you want
both, use \"C-u\" before the \"*\"."
  (interactive (list (bbdb-do-records)))
  (require 'org-agenda)
  (unless (and (eq major-mode 'bbdb-mode)
	       (equal (buffer-name) bbdb-buffer-name))
    (error "Only works in the BBDB buffer"))
  (setq records (bbdb-record-list records))
  (let ((tag-string
	 (mapconcat
	  'identity
	  (delete-dups
	   (cl-mapcan
	    (lambda (r)
	      (bbdb-record-xfield-split r gnorb-bbdb-org-tag-field))
	    records))
	  "|")))
    (if tag-string
	;; C-u = all headings, not just todos
	(if (equal current-prefix-arg '(4))
	    (org-tags-view nil tag-string)
	  (org-tags-view t tag-string))
      (error "No org-tags field present"))))

;;;###autoload
(defun gnorb-bbdb-mail-search (records)
  "Initiate a mail search from the BBDB buffer.

Use the prefix arg to edit the search string first, and the \"*\"
prefix to search mails from all visible contacts. When using both
a prefix arg and \"*\", the prefix arg must come first."
  (interactive (list (bbdb-do-records)))
  (unless (and (eq major-mode 'bbdb-mode)
	       (equal (buffer-name) bbdb-buffer-name))
    (error "Only works in the BBDB buffer"))
  (setq records (bbdb-record-list records))
  (require 'gnorb-gnus)
  (let* ((backend (or (assoc gnorb-gnus-mail-search-backend
			     gnorb-gnus-mail-search-backends)
		      (error "No search backend specified")))
	 (search-string
	  (funcall (cl-second backend)
		   (cl-mapcan 'bbdb-record-mail records))))
    (when (equal current-prefix-arg '(4))
      (setq search-string
	    (read-from-minibuffer
	     (format "%s search string: " (first backend)) search-string)))
    (funcall (cl-third backend) search-string)
    (delete-other-windows)))

;;;###autoload
(defun gnorb-bbdb-cite-contact (rec)
  (interactive (list (bbdb-completing-read-record "Record: ")))
  (let ((mail-string (bbdb-dwim-mail rec)))
   (if (called-interactively-p 'any)
       (insert mail-string)
     mail-string)))

;;; Field containing links to recent messages
(when (boundp 'bbdb-xfield-label-list)
 (add-to-list 'bbdb-xfield-label-list gnorb-bbdb-messages-field nil 'eq))

(defun gnorb-bbdb-display-messages (record format &optional indent)
  "Show links to the messages collected in the
`gnorb-bbdb-messages-field' field of a BBDB record. Each link
will be formatted using the format string in
`gnorb-bbdb-message-link-format-multi' or
`gnorb-bbdb-message-link-format-one', depending on the current
layout type."
  (let ((full-field (assq gnorb-bbdb-messages-field
			  (bbdb-record-xfields record)))
	(val (bbdb-record-xfield record gnorb-bbdb-messages-field))
	(map (make-sparse-keymap))
	(count 1)) ; one-indexed to fit with prefix arg to `gnorb-bbdb-open-link'
    (define-key map [mouse-1] 'gnorb-bbdb-mouse-open-link)
    (define-key map (kbd "<RET>") 'gnorb-bbdb-RET-open-link)
    (when val
      (when (eq format 'multi)
	(bbdb-display-text (format (format " %%%ds: " (- indent 3))
				   gnorb-bbdb-messages-field)
			   `(xfields ,full-field field-label)
			   'bbdb-field-name))
      (insert (cond ((and (stringp val)
			  (eq format 'multi))
		     (bbdb-indent-string (concat val "\n") indent))
		    ((listp val)
		     ;; Why aren't I using `bbdb-display-list' with a
		     ;; preformatted list of messages?
		     (concat
		      (bbdb-indent-string
		       (mapconcat
			(lambda (m)
			  (prog1
			      (propertize
			       (concat
				(format-time-string
				 (replace-regexp-in-string
				  "%:subject" (gnorb-bbdb-link-subject m)
				  (replace-regexp-in-string
				   "%:count" (number-to-string count)
				   (if (eq format 'multi)
				       gnorb-bbdb-message-link-format-multi
				     gnorb-bbdb-message-link-format-one)))
				 (gnorb-bbdb-link-date m)))
			       'face 'gnorb-bbdb-link
			       'mouse-face 'highlight
			       'gnorb-bbdb-link-count count
			       'keymap map)
			    (incf count)))
			val (if (eq format 'multi)
				"\n" ", "))
		       indent)
		      (if (eq format 'multi) "\n" "")))
		    (t
		     ""))))))

(defalias (intern (format "bbdb-display-%s-multi-line"
			  gnorb-bbdb-messages-field))
  (lambda (record indent)
    (gnorb-bbdb-display-messages record 'multi indent)))

(defalias (intern (format "bbdb-display-%s-one-line"
			  gnorb-bbdb-messages-field))
  (lambda (record)
    (gnorb-bbdb-display-messages record 'one)))

;; Don't allow direct editing of this field

(defalias (intern (format "bbdb-read-xfield-%s"
			  gnorb-bbdb-messages-field))
  (lambda (&optional _init)
    (user-error "This field shouldn't be edited manually")))

;; Open links from the *BBDB* buffer.

;;;###autoload
(defun gnorb-bbdb-open-link (record arg)
  "\\<bbdb-mode-map>Call this on a BBDB record to open one of the
links in the message field. By default, the first link will be
opened. Use a prefix arg to open different links. For instance,
M-3 \\[gnorb-bbdb-open-link] will open the third link in the
list. If the %:count escape is present in the message formatting
string (see `gnorb-bbdb-message-link-format-multi' and
`gnorb-bbdb-message-link-format-one'), that's the number to use.

This function also needs to be called on a contact once before
that contact will start collecting links to messages."
  (interactive (list
		(or (bbdb-current-record)
		    (user-error "No record under point"))
		current-prefix-arg))
  (unless (fboundp 'bbdb-record-xfield-string)
    (user-error "This function only works with the git version of BBDB"))
  (let (msg-list target-msg)
    (if (not (memq gnorb-bbdb-messages-field
		   (mapcar 'car (bbdb-record-xfields record))))
	(when (y-or-n-p
	       (format "Start collecting message links for %s?"
		       (bbdb-record-name record)))
	  (bbdb-record-set-xfield record gnorb-bbdb-messages-field "no links yet")
	  (message "Opening messages from %s will add links to the %s field"
		   (bbdb-record-name record)
		   gnorb-bbdb-messages-field)
	  (bbdb-change-record record))
	(setq msg-list
	      (bbdb-record-xfield record gnorb-bbdb-messages-field))
	(setq target-msg
	      (or (and arg
		       (nth (1- arg) msg-list))
		  (car msg-list)))
	(when target-msg
	  (org-gnus-follow-link (gnorb-bbdb-link-group target-msg)
				(gnorb-bbdb-link-id target-msg))))))

(defun gnorb-bbdb-mouse-open-link (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((rec (bbdb-current-record))
	(num (get-text-property (point) 'gnorb-bbdb-link-count)))
    (if (not num)
	(user-error "No link under point")
      (gnorb-bbdb-open-link rec num))))

(defun gnorb-bbdb-RET-open-link ()
  (interactive)
  (let ((rec (bbdb-current-record))
	(num (get-text-property (point) 'gnorb-bbdb-link-count)))
    (if (not num)
	(user-error "No link under point")
      (gnorb-bbdb-open-link rec num))))

(defun gnorb-bbdb-store-message-link (record)
  "Used in the `bbdb-notice-record-hook' to possibly save a link
to a message into the record's `gnorb-bbdb-messages-field'."

  (when (not (fboundp 'bbdb-record-xfield-string))
    (user-error "This function only works with the git version of BBDB"))
  (unless (or (not (and (memq gnorb-bbdb-messages-field
			      (mapcar 'car (bbdb-record-xfields record)))
			(memq major-mode '(gnus-summary-mode gnus-article-mode))))
	      (with-current-buffer gnus-article-buffer
		(not ; only store messages if the record is the sender
		 (member (nth 1 (car (bbdb-get-address-components 'sender)))
			 (bbdb-record-mail record)))))
    (with-current-buffer gnus-summary-buffer
      (let* ((val (bbdb-record-xfield record gnorb-bbdb-messages-field))
	     (art-no (gnus-summary-article-number))
	     (heads (gnus-summary-article-header art-no))
	     (date (apply 'encode-time
			  (parse-time-string (mail-header-date heads))))
	     (subject (mail-header-subject heads))
	     (id (mail-header-id heads))
	     (group (gnorb-get-real-group-name
		     gnus-newsgroup-name
		     art-no))
	     link)
	(if (not (and date subject id group))
	    (message "Could not save a link to this message")
	  (setq link (make-gnorb-bbdb-link :subject subject :date date
					   :group group :id id))
	  (when (stringp val)
	    (setq val nil))
	  (setq val (cons link (delete link val)))
	  (when (eq gnorb-bbdb-define-recent 'received)
	    (setq val (sort val
			    (lambda (a b)
			      (time-less-p
			       (gnorb-bbdb-link-date b)
			       (gnorb-bbdb-link-date a))))))
	  (setq val (cl-subseq val 0 (min (length val) gnorb-bbdb-collect-N-messages)))
	  (bbdb-record-set-xfield record
				  gnorb-bbdb-messages-field
				  (delq nil val))
	  (bbdb-change-record record))))))

(add-hook 'bbdb-notice-record-hook 'gnorb-bbdb-store-message-link)

(provide 'gnorb-bbdb)
;;; gnorb-bbdb.el ends here

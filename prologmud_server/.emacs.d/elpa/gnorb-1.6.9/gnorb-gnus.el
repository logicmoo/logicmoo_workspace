;;; gnorb-gnus.el --- The gnus-centric fuctions of gnorb -*- lexical-binding: t -*-

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

;;

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-art)
(require 'message)
(require 'org)
(require 'org-attach)
(require 'org-capture)
(require 'gnorb-utils)
(require 'gnorb-registry)
(require 'mm-decode)

(declare-function org-gnus-article-link "org-gnus"
		  (group newsgroups message-id x-no-archive))
(declare-function org-gnus-follow-link "org-gnus"
		  (group article))
(declare-function org-make-tags-matcher "org" (match))
(declare-function gnorb-org-restore-after-send "gnorb-org")
(declare-function gnorb-org-attachment-list "gnorb-org")

(autoload 'gnorb-gnus-nnir-search "nngnorb")
(autoload 'gnorb-gnus-nnir-registry-search "nngnorb")

(defgroup gnorb-gnus nil
  "The Gnus bits of Gnorb."
  :tag "Gnorb Gnus"
  :group 'gnorb)

(defcustom gnorb-gnus-mail-search-backends
  '((notmuch (lambda (terms)
	       (mapconcat
		(lambda (m)
		  (replace-regexp-in-string "\\." "\\\\." m))
		terms " OR "))
	     notmuch-search)
    (mairix (lambda (terms)
	      (mapconcat 'identity
			 terms ","))
	    mairix-search)
    (namazu (lambda (terms)
	      (mapconcat 'identity
			 terms " or "))
	    namazu-search))
  "Various backends for mail search.

An alist of backends, where each element consists of three parts:
the symbol name of the backend, a lambda form which receives a
list of email addresses and returns a properly-formatted search
string, and the symbol name of the function used to initiate the
search."
  :type 'list)

(defcustom gnorb-gnus-mail-search-backend nil
  "Mail search backend currently in use. One of the three symbols
notmuch, namazu, or mairix."
  :type 'symbol)

(defcustom gnorb-gnus-capture-always-attach nil
  "Always prompt about attaching attachments when capturing from
  a Gnus message, even if the template being used hasn't
  specified the :gnus-attachments key.

Basically behave as if all attachments have \":gnus-attachments t\"."
  :type 'boolean)

(defcustom gnorb-gnus-new-todo-capture-key nil
  "Key for the capture template to use when creating a new TODO
  from an outgoing message."
  :type 'string)

(defcustom gnorb-gnus-copy-message-text nil
  "When capturing or triggering a TODO from a Gnus message,
should the text of the message be saved?

If t, the body text of the message is pushed onto the kill ring.
If a char value, the text is saved into the corresponding
register."
  :type '(choice boolean
		 character))

(defcustom gnorb-gnus-hint-relevant-article t
  "When opening a gnus message, should gnorb let you know if the
  message is relevant to an existing TODO?"
  :type 'boolean)

(defcustom gnorb-gnus-tick-all-tracked-messages nil
  "When non-nil, add the tick mark to all tracked messages.
This happens only once, at the time the association is created.
Ticks can be safely removed later."
  :type 'boolean)

(defcustom gnorb-gnus-auto-tag-messages nil
  "When non-nil, tag messages with associated heading tags.
When creating associations between Org headings and messages,
automatically copy the heading's tags on to the message, using
the registry."
  :type 'boolean)

(defcustom gnorb-gnus-summary-mark-format-letter "g"
  "Format letter to be used as part of your
  `gnus-summary-line-format', to indicate in the *Summary* buffer
  which articles might be relevant to TODOs. Since this is a user
  format code, it should be prefixed with %u, eg %ug. It will
  result in the insertion of the value of
  `gnorb-gnus-summary-mark', for relevant messages, or
  else a space."
  :type 'string)

(defcustom gnorb-gnus-summary-tags-format-letter "G"
  "Format letter to be replaced with message tags.
Add this format specification to your `gnus-summary-line-format'
to show the tags which are currently applied to the message.
Must be prefixed with \"u\", eg. \"%uG\"."
  :type 'string)

(defcustom gnorb-gnus-summary-mark "¡"
  "Default mark to insert in the summary format line of articles
  that are likely relevant to existing TODO headings."
  :type 'string)

(defcustom gnorb-gnus-summary-tracked-mark "&"
  "Default mark to insert in the summary format line of articles
  that are already tracked by TODO headings."
  :type 'string)

(defcustom gnorb-gnus-trigger-refile-targets
  '((org-agenda-files :maxlevel . 4))
  "A value to use as an equivalent of `org-refile-targets' (which
  see) when offering trigger targets for
  `gnorb-gnus-incoming-do-todo'."
  :type 'list)

(defcustom gnorb-gnus-refile-use-outline-path 'org
  "Gnorb equivalent of `org-refile-use-outline-path' (which see).
Used when selecting Org headings for triggering or attaching
attachments."
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Yes" t)
	  (const :tag "Start with file name" file)
	  (const :tag "Start with full file path" full-file-path)
	  (const :tag "Start with buffer name" buffer-name)
	  (const :tag "Use org-refile-use-outline-path" org)))

(defcustom gnorb-gnus-sent-groups nil
  "A list of strings indicating sent mail groups.
In some cases, Gnorb can't detect where your sent messages are
stored (ie if you're using IMAP sent mail folders instead of
local archiving. If you want Gnorb to be able to find sent
messages, this option can help it do that. It should be set to a
list of strings, which are assumed to be fully qualified
server+group combinations, ie \"nnimap+Server:[Gmail]/Sent
Mail\", or something similar. This only has to be done once for
each message."
  :type '(repeat (string :tag "Group name")))

(defvar gnorb-gnus-capture-attachments nil
  "Holding place for attachment names during the capture
  process.")

(defvar-local gnorb-gnus-attachment-file-list nil
  "A place to store Org attachments relevant to the subtree being
  viewed.")

;;; What follows is a very careful copy-pasta of bits and pieces from
;;; mm-decode.el and gnus-art.el. Voodoo was involved.

;;;###autoload
(defun gnorb-gnus-article-org-attach (n)
  "Save MIME part N, which is the numerical prefix, of the
  article under point as an attachment to the specified org
  heading."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnorb-gnus-attach-part))

;;;###autoload
(defun gnorb-gnus-mime-org-attach ()
  "Save the MIME part under point as an attachment to the
  specified org heading."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (gnorb-gnus-attach-part data))))

(defun gnorb-gnus-attach-part (handle)
  "Attach HANDLE to an existing org heading."
  (let* ((filename (gnorb-gnus-save-part handle))
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (org-refile-use-outline-path
	  (if (eq gnorb-gnus-refile-use-outline-path 'org)
	      org-refile-use-outline-path
	    gnorb-gnus-refile-use-outline-path))
	 (headers (gnus-data-header
		   (gnus-data-find
		    (gnus-summary-article-number))))
	 (tracked-headings (gnorb-find-tracked-headings headers))
	 (target-heading
	  (gnorb-choose-trigger-heading tracked-headings)))
    (require 'org-attach)
    (save-window-excursion
      (org-id-goto target-heading)
      (org-attach-attach filename nil 'mv))))

(defun gnorb-gnus-save-part (handle)
  (let ((filename (or (mail-content-type-get
		       (mm-handle-disposition handle) 'filename)
		      (mail-content-type-get
		       (mm-handle-type handle) 'name))))
    (setq filename
	  (gnus-map-function mm-file-name-rewrite-functions
			     (file-name-nondirectory filename)))
    (setq filename (expand-file-name filename gnorb-tmp-dir))
    (mm-save-part-to-file handle filename)
    filename))

(defun gnorb-gnus-collect-all-attachments (&optional capture-p store)
  "Collect all the attachments from the message under point, and
save them into `gnorb-tmp-dir'."
  (save-window-excursion
    (when capture-p
      (set-buffer (org-capture-get :original-buffer)))
    (unless (memq major-mode '(gnus-summary-mode gnus-article-mode))
      (error "Only works in Gnus summary or article buffers"))
    (let ((article (gnus-summary-article-number))
	  mime-handles)
      (when (or (null gnus-current-article)
		(null gnus-article-current)
		(/= article (cdr gnus-article-current))
		(not (equal (car gnus-article-current) gnus-newsgroup-name)))
	(gnus-summary-display-article article))
      (gnus-eval-in-buffer-window gnus-article-buffer
	(setq mime-handles (cl-remove-if-not
			    (lambda (h)
			      (let ((disp (mm-handle-disposition (cdr h))))
				(and (member (car disp)
					     '("inline" "attachment"))
				     (mail-content-type-get disp 'filename))))
			    gnus-article-mime-handle-alist)))
      (when mime-handles
	(dolist (h mime-handles)
	  (let ((filename
		 (gnorb-gnus-save-part (cdr h))))
	    (when (or capture-p store)
	      (push filename gnorb-gnus-capture-attachments))))))))

;;; Storing, removing, and acting on Org headers in messages.

(defvar gnorb-gnus-message-info nil
  "Place to store the To, Subject, Date, and Message-ID headers
  of the currently-sending or last-sent message.")

(defun gnorb-gnus-check-outgoing-headers ()
  "Save the value of the `gnorb-mail-header' for the current
message; multiple header values returned as a string. Also save
information about the outgoing message into
`gnorb-gnus-message-info'."
  (save-restriction
    (message-narrow-to-headers)
    (setq gnorb-gnus-message-info nil)
    (let* ((org-ids (mail-fetch-field gnorb-mail-header nil nil t))
	   (msg-id (mail-fetch-field "Message-ID"))
	   (refs (mail-fetch-field "References"))
	   (in-reply-to (mail-fetch-field "In-Reply-To"))
	   (to (if (message-news-p)
		   (mail-fetch-field "Newsgroups")
		 (mail-fetch-field "To")))
	   (from (mail-fetch-field "From"))
	   (subject (mail-fetch-field "Subject"))
	   (date (mail-fetch-field "Date"))
	   ;; If we can get a link, that's awesome.
	   (gcc (mail-fetch-field "Gcc"))
	   (link (or (and gcc
			  (org-store-link nil))
		     nil))
	   (group (ignore-errors (car (split-string link "#")))))
      ;; If we can't make a real link, then save some information so
      ;; we can fake it.
      (when in-reply-to
	(setq refs (concat refs " " in-reply-to)))
      (when refs
	(setq refs (gnus-extract-references refs)))
      (setq gnorb-gnus-message-info
	    `(:subject ,subject :msg-id ,msg-id
		       :to ,to :from ,from
		       :link ,link :date ,date :refs ,refs
		       :group ,group))
      (if org-ids
	  (progn
	    (require 'gnorb-org)
	    (setq gnorb-message-org-ids org-ids)
	    ;; `gnorb-org-setup-message' may have put this here, but
	    ;; if we're working from a draft, or triggering this from
	    ;; a reply, it might not be there yet.
	    (add-to-list 'message-send-actions
			 'gnorb-org-restore-after-send t))
	(setq gnorb-message-org-ids nil)))))

(add-hook 'message-sent-hook #'gnorb-gnus-check-outgoing-headers t)

;;;###autoload
(defun gnorb-gnus-outgoing-do-todo (&optional arg)
  "Use this command to use the message currently being composed
as an email todo action.

If it's a new message, or a reply to a message that isn't
referenced by any TODOs, a new TODO will be created.

If it references an existing TODO, you'll be prompted to trigger
a state-change or a note on that TODO after the message is sent.

You can call it with a prefix arg to force choosing an Org
subtree to associate with.

If you've already called this command, but realize you made a
mistake, you can call this command with a double prefix to reset
the association.

If a new todo is made, it needs a capture template: set
`gnorb-gnus-new-todo-capture-key' to the string key for the
appropriate capture template. If you're using a gnus-based
archive method (ie you have `gnus-message-archive-group' set to
something, and your outgoing messages have a \"Fcc\" header),
then a real link will be made to the outgoing message, and all
the gnus-type escapes will be available (see the Info
manual (org) Template expansion section). If you don't, then the
%:subject, %:to, %:toname, %:toaddress, and %:date escapes for
the outgoing message will still be available -- nothing else will
work."
  (interactive "P")
  (let ((org-refile-targets gnorb-gnus-trigger-refile-targets)
	(org-refile-use-outline-path
	 (if (eq gnorb-gnus-refile-use-outline-path 'org)
	     org-refile-use-outline-path
	   gnorb-gnus-refile-use-outline-path))
	(compose-marker (make-marker))
	header-ids ref-ids rel-headings
	gnorb-window-conf in-reply-to)
    (when (equal arg '(4))
      (setq rel-headings
	    (org-refile-get-location "Trigger action on" nil t))
      (setq rel-headings
	    (list (list (save-window-excursion
			  (find-file (nth 1 rel-headings))
			  (goto-char (nth 3 rel-headings))
			  (org-id-get-create))))))
    (if (not (derived-mode-p 'message-mode 'mail-mode))
	;; The message is already sent, so we're relying on whatever was
	;; stored into `gnorb-gnus-message-info'.
	(if (equal arg '(16))
	    (user-error "A double prefix is only useful with an
	    unsent message.")
	  (if arg
	      (progn
		(push (caar rel-headings) gnorb-message-org-ids)
		(gnorb-org-restore-after-send))
	    (setq ref-ids (plist-get gnorb-gnus-message-info :refs))
	    (if ref-ids
		;; the message might be relevant to some TODO
		;; heading(s). But if there had been org-id
		;; headers, they would already have been
		;; handled when the message was sent.
		(progn
		  (setq rel-headings (gnorb-find-visit-candidates ref-ids))
		  (if (not rel-headings)
		      (gnorb-gnus-outgoing-make-todo-1)
		    (dolist (h rel-headings)
		      (push h gnorb-message-org-ids))
		    (gnorb-org-restore-after-send)))
	      ;; not relevant, just make a new TODO
	      (gnorb-gnus-outgoing-make-todo-1))))
      ;; We are still in the message composition buffer, so let's see
      ;; what we've got.

      (if (equal arg '(16))
	  ;; Double prefix arg means delete the association we already
	  ;; made.
	  (save-excursion
	    (save-restriction
	      (widen)
	      (setq message-send-actions
		    (remove 'gnorb-gnus-outgoing-make-todo-1
			    message-send-actions))
	      (message-narrow-to-headers-or-head)
	      (message-remove-header
	       gnorb-mail-header)
	      (message "Message associations have been reset")))
	;; Save-excursion won't work, because point will move if we
	;; insert headings.
	(move-marker compose-marker (point))
	(save-restriction
	  (widen)
	  (message-narrow-to-headers-or-head)
	  (setq header-ids (mail-fetch-field gnorb-mail-header nil nil t))
	  ;; With a prefix arg we do not check references, because the
	  ;; whole point is to add new references. We still want to know
	  ;; what org id headers are present, though, so we don't add
	  ;; duplicates.
	  (setq ref-ids (unless arg (mail-fetch-field "References" t)))
	  (setq in-reply-to (unless arg (mail-fetch-field "In-Reply-to" t)))
	  (when in-reply-to
	    (setq ref-ids (concat ref-ids " " in-reply-to)))
	  (when ref-ids
	    ;; if the References header points to any message ids that are
	    ;; tracked by TODO headings...
	    (setq rel-headings (gnorb-find-visit-candidates ref-ids)))
	  (when rel-headings
	    (goto-char (point-min))
	    (dolist (h (delete-dups rel-headings))
	      ;; then get the org-ids of those headings, and insert
	      ;; them into this message as headers. If the id was
	      ;; already present in a header, don't add it again.
	      (unless (member h header-ids)
		(goto-char (point-at-bol))
		(open-line 1)
		(message-insert-header
		 (intern gnorb-mail-header)
		 h)
		;; tell the rest of the function that this is a relevant
		;; message
		(push h header-ids)))))
	(goto-char compose-marker)
	(unless header-ids
	  (add-to-list 'message-send-actions
	   'gnorb-gnus-outgoing-make-todo-1 t))
	(message
	 (if header-ids
	     "Message will trigger TODO state-changes after sending"
	   "A TODO will be made from this message after it's sent"))))))

(defvar org-capture-link-is-already-stored)

(defun gnorb-gnus-outgoing-make-todo-1 ()
  (unless gnorb-gnus-new-todo-capture-key
    (error "No capture template key set, customize gnorb-gnus-new-todo-capture-key"))
  (let* ((link (plist-get gnorb-gnus-message-info :link))
	 (group (plist-get gnorb-gnus-message-info :group))
	 (date (plist-get gnorb-gnus-message-info :date))
	 (date-ts (and date
		       (ignore-errors
			 (format-time-string
			  (org-time-stamp-format t)
			  (date-to-time date)))))
	 (date-ts-ia (and date
			  (ignore-errors
			    (format-time-string
			     (org-time-stamp-format t t)
			     (date-to-time date)))))
	 (msg-id (plist-get gnorb-gnus-message-info :msg-id))
	 (sender (plist-get gnorb-gnus-message-info :from))
	 (subject (plist-get gnorb-gnus-message-info :subject))
	 ;; Convince Org we already have a link stored, even if we
	 ;; don't.
	 (org-capture-link-is-already-stored t))
    (if link
	;; Even if you make a link to not-yet-sent messages, even if
	;; you've saved the draft and it has a Date header, that
	;; header isn't saved into the link plist. So fake that, too.
	(org-link-add-props
	 :date date
	 :date-timestamp date-ts
	 :date-timestamp-inactive date-ts-ia
	 :annotation link)
      (org-link-store-props
       :subject (plist-get gnorb-gnus-message-info :subject)
       :to (plist-get gnorb-gnus-message-info :to)
       :date date
       :date-timestamp date-ts
       :date-timestamp-inactive date-ts-ia
       :message-id msg-id
       :annotation link))
    (org-capture nil gnorb-gnus-new-todo-capture-key)
    (when msg-id
      (gnorb-registry-make-entry msg-id sender subject (org-id-get-create) group))))

;;; If an incoming message should trigger state-change for a Org todo,
;;; call this function on it.

;;;###autoload
(defun gnorb-gnus-incoming-do-todo (arg &optional id)
  "Use the message under point to trigger an action on an Org heading.
This function stores a link to the message, prompts for a related
Org heading, visits the heading, and triggers an action on
it (see `gnorb-org-trigger-actions').

If you've set up message tracking (with
`gnorb-tracking-initialize'), Gnorb can guess which Org heading
you probably want to trigger, which can save some time.  It does
this by looking in the References header, and seeing if any of
the messages referenced there are already being tracked by any
headings.

If you mark several messages before calling this function, or
call it with a numerical prefix arg, those messages will be
\"bulk associated\" with the chosen Org heading: associations
will be made, but you won't be prompted to trigger an action, and
you'll stay in the Gnus summary buffer."
  (interactive "P")
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Only works in gnus summary or article mode"))
  ;; We should only store a link if it's not already at the head of
  ;; `org-stored-links'. There's some duplicate storage, at
  ;; present. Take a look at calling it non-interactively.
  (setq gnorb-window-conf (current-window-configuration))
  (move-marker gnorb-return-marker (point))
  (setq gnorb-gnus-message-info nil)
  (let* ((buf (current-buffer))
	 (articles (gnus-summary-work-articles arg))
	 (art-no (gnus-summary-article-number))
	 (headers (gnus-data-header
		   (gnus-data-find art-no)))
	 (msg-id (mail-header-id headers))
	 (from (mail-header-from headers))
	 (subject (mail-header-subject headers))
	 (date (mail-header-date headers))
	 (to (cdr (assoc 'To (mail-header-extra headers))))
	 (group (gnorb-get-real-group-name
		 gnus-newsgroup-name
		 art-no))
	 (text (gnus-with-article-buffer
		 (article-goto-body)
		 (buffer-substring-no-properties
		  (point) (point-max))))
	 (link (call-interactively 'org-store-link))
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (org-refile-use-outline-path
	  (if (eq gnorb-gnus-refile-use-outline-path 'org)
	      org-refile-use-outline-path
	    gnorb-gnus-refile-use-outline-path))
	 (ref-msg-ids (concat (mail-header-references headers) " "
			      msg-id))
	 (related-headings
	  (when (and (null id) ref-msg-ids)
	    ;; Specifically ask for zombies, so the user has chance to
	    ;; flush them out.
	    (gnorb-find-tracked-headings headers t)))
	 targ tags)
    (setq gnorb-gnus-message-info
	  `(:subject ,subject :msg-id ,msg-id
		     :to ,to :from ,from
		     :link ,link :date ,date :refs ,ref-msg-ids
		     :group ,group))
    (gnorb-gnus-collect-all-attachments nil t)
    (unwind-protect
	(if id
	    (progn
	      (delete-other-windows)
	      (gnorb-trigger-todo-action nil id))
	  ;; Flush out zombies (dead associations).
	  (setq related-headings
		(cl-remove-if
		 (lambda (h)
		   (when (null (org-id-find-id-file h))
		     (when (y-or-n-p
			    (format
			     "ID %s no longer exists, disassociate message?"
			     h))
		       (gnorb-delete-association msg-id h))))
		 related-headings))
	  ;; See if one of the related headings is chosen.
	  (unless (catch 'target
		    (dolist (h related-headings nil)
		      (when (yes-or-no-p
			     (format "Trigger action on %s"
				     (gnorb-pretty-outline h)))
			(throw 'target (setq targ h)))))
	    ;; If not, use the refile interface to choose one.
	    (setq targ (org-refile-get-location
			"Trigger heading" nil))
	    (setq targ
		  (save-window-excursion
		    (find-file (nth 1 targ))
		    (goto-char (nth 3 targ))
		    (setq tags (org-get-tags))
		    (org-id-get-create))))
	  ;; Either bulk associate multiple messages...
	  (if (> (length articles) 1)
	      (progn
		(dolist (a articles)
		  (gnorb-registry-make-entry
		   (mail-header-id
		    (gnus-data-header
		     (gnus-data-find a)))
		   from subject targ group)
		  (gnus-summary-remove-process-mark a))
		(message "Associated %d messages with %s"
			 (length articles) (gnorb-pretty-outline targ)))
	    ;; ...or just trigger the one.
	    (delete-other-windows)
	    (gnorb-trigger-todo-action nil targ)
	    (when gnorb-gnus-copy-message-text
	      (if (characterp gnorb-gnus-copy-message-text)
		  (with-temp-buffer
		    (insert text)
		    (copy-to-register
		     gnorb-gnus-copy-message-text
		     (point-min) (point-max))
		    (message "Message text copied to register %c"
			     gnorb-gnus-copy-message-text))
		(kill-new text)
		(message "Message text copied to kill ring"))))
	  (with-current-buffer buf
	    (dolist (a articles)
	      (when gnorb-gnus-tick-all-tracked-messages
		(gnus-summary-mark-article a gnus-ticked-mark))
	      (when gnorb-gnus-auto-tag-messages
		(gnorb-gnus-tag-message
		 (mail-header-id (gnus-data-header (gnus-data-find a)))
		 tags))
	      (gnus-summary-update-article a))))
      ;; No matter what, clear these two variables.
      (setq gnorb-gnus-message-info nil)
      (setq gnorb-gnus-capture-attachments nil))))

;;;###autoload
(defun gnorb-gnus-quick-reply ()
  "Compose a reply to the message under point, and associate both
the original message and the reply with the selected heading.
Take no other action.

Use this when you want to compose a reply to a message on the
spot, and track both messages, without having to go through the
hassle of triggering an action on a heading, and then starting a
reply."
  (interactive)
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Only works in gnus summary or article mode"))
  (let* ((art-no (gnus-summary-article-number))
	 (headers (gnus-data-header
		   (gnus-data-find art-no)))
	 (msg-id (mail-header-id headers))
	 (from (mail-header-from headers))
	 (subject (mail-header-subject headers))
	 (group (gnorb-get-real-group-name
		 gnus-newsgroup-name
		 art-no))
	 (ref-msg-ids (concat (mail-header-references headers) " "
			      msg-id))
	 (related-headings
	  (when ref-msg-ids
	    (gnorb-find-tracked-headings headers t)))
	 (targ (car-safe related-headings)))
    (if targ
	(let ((ret (make-marker)))
	  (setq gnorb-window-conf (current-window-configuration))
	  (move-marker gnorb-return-marker (point))
	  (when gnorb-gnus-tick-all-tracked-messages
	    (gnus-summary-mark-article art-no gnus-ticked-mark))
	  ;; Assume the first heading is the one we want.
	  (gnorb-registry-make-entry
	   msg-id from subject targ group)
	  ;; Maybe tag the message.
	  (when gnorb-gnus-auto-tag-messages
	    (let ((tags (save-window-excursion
			  (org-id-goto targ)
			  (org-get-tags))))
	      (gnorb-gnus-tag-message msg-id tags)))
	  (gnus-summary-update-article art-no)
	  (gnus-summary-wide-reply-with-original 1)
	  (move-marker ret (point))
	  (save-restriction
	    (widen)
	    (message-narrow-to-headers-or-head)
	    (goto-char (point-min))
	    (open-line 1)
	    (message-insert-header
	     (intern gnorb-mail-header) targ))
	  (goto-char ret)
	  (message
	   (format "Original message and reply will be associated with %s"
		   (gnorb-pretty-outline targ))))
      (message "No associated headings found"))))

(with-eval-after-load 'gnus-registry
  (add-to-list 'gnus-registry-extra-entries-precious 'org-tags)
  (add-to-list 'gnus-registry-track-extra 'org-tags))

(defvar gnorb-registry-search-history nil)

;;;###autoload
(defun gnorb-gnus-search-registry (search-string)
  "Search for and display messages using the registry.
Prompt for a registry-specific SEARCH-STRING, then create an
ephemeral group containing the resulting messages.  All tracked
registry data keys are acceptable, see (slot-value
gnus-registry-db 'tracked).  Unknown keys will be ignored.  Keys
and search strings should be given as \"key:value\", with extra
quotes around multi-word search values.  Eg:

sender:google.com subject:\"your search results\""
  (interactive
   (list (read-string "Registry search terms: " nil
		      gnorb-registry-search-history)))
  (let ((case-fold-search t)
	parsed found this-pass term)
    (with-temp-buffer
      (insert search-string)
      (goto-char (point-min))
      (while (re-search-forward
	      "\\([[:alpha:]]+\\):\\(\\(?:[^\"[:blank:]]+\\|\"[^\"]+\"\\)\\)"
	      (point-at-eol) t)
	(push (cons (intern (match-string 1))
		    (replace-regexp-in-string "\\`\"\\|\"\\'" ""
					      (match-string 2)))
	      parsed)))
    (dolist (sym (slot-value gnus-registry-db 'tracked))
      (when (setq term (cdr-safe (assoc sym parsed)))
	(maphash
	 (lambda (k v)
	   (when (string-match-p term k)
	     (setq this-pass (append v this-pass))))
	 (gethash sym (slot-value gnus-registry-db 'tracker)))
	(setq found (if found
			(seq-intersection found this-pass)
		      this-pass)
	      this-pass nil)))
    (if found
	(let ((artlist
	       (delq
		nil
		(mapcar
		 (lambda (msg)
		   (pcase-let ((`(,group . ,artno)
				(gnorb-msg-id-request-head
				 msg (car-safe
				      (gnus-registry-get-id-key msg 'group)))))
		     (when (and group artno (integerp artno) (> artno 0))
		       (vector group artno 100))))
		 (delq nil (delete-dups found))))))
	  (switch-to-buffer gnus-group-buffer)
	  (if (featurep 'nnselect)
	      (gnus-group-read-ephemeral-group
	       "registry messages" '(nnselect "nnselect-gnorb") nil
	       `(switch-to-buffer ,gnus-group-buffer)
	       nil nil `((nnselect-artlist . ,artlist)))
	    (gnorb-gnus-nnir-registry-search artlist)))
      (message "No results found"))))

;;;###autoload
(defun gnorb-gnus-tag-message (arg &optional tags)
  "Tag message or messages with TAGS.
ARG is used to specify which messages to work on (according to
Gnus' process prefix convention).  TAGS should be a list of Org
tags.  The tags are stored under the `org-tags' key in the
registry.  If called from a lisp program, TAGS are added to any
existing tags.

If multiple messages are to be tagged, only the first message's
existing tags are offered as a default."
  (interactive "P")
  (let* ((articles (or (gnus-summary-work-articles arg)
		       (user-error "This command must be used within Gnus")))
	 (first (mail-header-id
		 (gnus-data-header
		  (gnus-data-find (car articles)))))
	 (crm-separator ":")
	 (current (gnus-registry-get-id-key first 'org-tags))
	 (default (when current
		    (mapconcat #'identity current ":"))))
    (setq tags
	  (if tags
	      (delete-dups (append current tags))
	    (completing-read-multiple
	     "Tags: "
	     (org-global-tags-completion-table) nil t default)))
    (dolist (a articles)
      (let ((msg-id (mail-header-id
		     (gnus-data-header
		      (gnus-data-find a)))))
	(gnus-registry-set-id-key msg-id 'org-tags tags)
	(gnus-summary-update-article a)))
    (gnus-message 5 "%d message%s tagged: %s"
		  (length articles)
		  (if (= 1 (length articles)) "" "s")
		  (mapconcat #'identity tags ":"))))

;;;###autoload
(defun gnorb-gnus-insert-tagged-messages (tags)
  "Insert articles in this group with tags matching TAGS.
TAGS is a string possibly containing multiple tags to include or
exclude.  See Info node `(org)Matching tags and properties'."
  (interactive "MTags: ")
  (let ((matcher (cdr (org-make-tags-matcher tags)))
	(tagged-messages (registry-search gnus-registry-db
					  :regex `((org-tags ".+"))
					  :member `((group ,gnus-newsgroup-name))))
	(old (sort (mapcar 'car gnus-newsgroup-data) '<))
	selected-messages)
    ;; Funcall the matcher with t, (list of tags), and 1.
    (dolist (m tagged-messages)
      (when (funcall matcher t (gnus-registry-get-id-key m 'org-tags) 1)
	(push m selected-messages)))
    (if selected-messages
	;; Turn message ids into article numbers.
	(progn
	  (setq selected-messages
		(mapcar (lambda (id) (cdr (gnus-request-head id gnus-newsgroup-name)))
			selected-messages))
	  (gnus-summary-insert-articles selected-messages)
	  (gnus-summary-limit (gnus-sorted-nunion old selected-messages))
	  (gnus-summary-position-point))
      (message "No matching messages in this group"))))

;;;###autoload
(defun gnorb-gnus-insert-tracked-messages (show-all)
  "Insert tracked messages into the Summary buffer.
Only inserts tracked messages belonging to this group.  If
SHOW-ALL (interactively, the prefix arg) is non-nil, insert all
messages; otherwise only insert messages that are tracked by a
heading in a non-DONE state."
  (interactive "P")
  (let ((old (sort (mapcar 'car gnus-newsgroup-data) '<))
	(tracked-messages
	 (registry-search gnus-registry-db
			  :regex `((gnorb-ids ".+"))
			  :member `((group ,gnus-newsgroup-name)))))
    (unless show-all
      (setq tracked-messages
	    (cl-remove-if
	     (lambda (msg-id)
	       (let ((id (car-safe (gnus-registry-get-id-key
				    msg-id 'gnorb-ids))))
		 (or (null id)
		     (save-window-excursion
		       (org-id-goto id)
		       (org-entry-is-done-p)))))
	     tracked-messages)))
    (if tracked-messages
	(progn
	  (setq tracked-messages
		(delq nil
		      (mapcar (lambda (id)
				(cdr (gnus-request-head id gnus-newsgroup-name)))
			      tracked-messages)))
	  (gnus-summary-insert-articles tracked-messages)
	  (gnus-summary-limit (gnus-sorted-nunion tracked-messages old))
	  (gnus-summary-position-point))
      (message "No tracked messages in this group"))))

;;;###autoload
(defun gnorb-gnus-search-messages (str persist &optional head-text ret)
  "Initiate a search for gnus message links in an org subtree.
The arg STR can be one of two things: an Org heading id value
\(IDs should be prefixed with \"id+\"), in which case links will
be collected from that heading, or a string corresponding to an
Org tags search, in which case links will be collected from all
matching headings.

In either case, once a collection of links have been made, they
will all be displayed in an ephemeral Gnus group.  In Emacs 27
and below this requires the presence of an active \"nngnorb\"
server to work.  In Emacs 28 and later, no setup is required.

If PERSIST is non-nil, make a permanent group, and offer
HEAD-TEXT, if present, as its name.  Otherwise create an
ephemeral one, with RET as the value of its quit-config."
  (interactive)
  (unless (gnus-alive-p)
    (gnus))
  (if (featurep 'nnselect)
      (gnorb-gnus-nnselect-search str persist head-text ret)
    (gnorb-gnus-nnir-search str persist head-text ret)))

(defun gnorb-gnus-nnselect-search (str persist &optional head-text ret)
  "Display gnus messages using the nnselect backend."
  (let ((specs `((nnselect-specs
		  (nnselect-function . gnorb-run-search)
		  (nnselect-args . ,str)))))
    (if persist
	(let ((name (gnus-read-group
		     (format "Name for group (default %s): " head-text)
		     head-text)))
	  (with-current-buffer gnus-group-buffer
	    (gnus-group-make-group
	     name (list 'nnselect "nnselect-gnorb")
	     nil specs)))
      (gnus-group-read-ephemeral-group
       (concat "nnselect-" (message-unique-id))
       (list 'nnselect "nnselect-gnorb")
       nil ret nil nil specs))))

(defun gnorb-run-search (q)
  (save-window-excursion
    (let ((buf (get-buffer-create "gnorb search"))
	  msg-ids org-ids links vectors)
      (with-current-buffer buf
	(erase-buffer)
	(setq gnorb-gnus-attachment-file-list nil))
      (cond ((string-match "id\\+\\([[:alnum:]-]+\\)$" q)
	     (with-demoted-errors "Error: %S"
	       (org-id-goto (match-string 1 q))
	       (save-restriction
		 (org-narrow-to-subtree)
		 (append-to-buffer
		  buf
		  (point-min)
		  (point-max))
		 (setq org-ids
		       (append
			(gnorb-collect-ids)
			org-ids))
		 (when org-ids
		   (with-current-buffer buf
		     ;; The file list var is buffer local, so set it
		     ;; (local to Gnorb's search buffer) to a full
		     ;; list of all files in the subtree.
		     (dolist (id org-ids)
		       (setq gnorb-gnus-attachment-file-list
			     (append (gnorb-org-attachment-list id)
				     gnorb-gnus-attachment-file-list))))))))
	    ((listp q)
	     ;; be a little careful: this could be a list of links, or
	     ;; it could be the full plist
	     (setq links (if (plist-member q :gnus)
			     (plist-get q :gnus)
			   q)))
	    (t (org-map-entries
		(lambda ()
		  (push (org-id-get) org-ids)
		  (append-to-buffer
		   buf
		   (point)
		   (save-excursion
		     (outline-next-heading)
		     (point))))
		q
		'agenda)))
      (with-current-buffer buf
	(goto-char (point-min))
	(setq links (append
		     (alist-get 'gnus (gnorb-scan-links (point-max) 'gnus))
		     links))

	(goto-char (point-min)))
      ;; First add all links to messages (elements of messages should
      ;; look like (group-name message-id)).
      (dolist (l links)
	(push (org-link-unescape
	       (nth 1 (split-string l "#")))
	      msg-ids))

      (unless (gnus-alive-p)
	(gnus))

      ;; Then use the registry to turn list of org-ids into list of
      ;; msg-ids.
      (dolist (i (delq nil (delete-dups org-ids)))
	(when-let ((rel-msg-id (gnorb-registry-org-id-search i)))
	  (setq msg-ids (append (delq nil rel-msg-id) msg-ids))))

      ;; Then find the group and article number for each msg-id, and
      ;; push that onto our return value "vectors".
      (when msg-ids
	(dolist (id (delete-dups msg-ids) (when vectors
					    (nreverse vectors)))
	  (pcase-let ((`(,group . ,artno) (gnorb-msg-id-request-head id)))
	    (when (and artno (integerp artno) (> artno 0))
	      (push (vector group artno 100) vectors))))))))

(defvar gnorb-summary-minor-mode-map (make-sparse-keymap)
  "Keymap for use in Gnorb's *Summary* minor mode.")

(define-minor-mode gnorb-summary-minor-mode
  "A minor mode for use in nnir *Summary* buffers created by Gnorb.
These *Summary* buffers are usually created by calling
`gnorb-org-view', or by otherwise initiating a search of
Gnorb-tracked messages.

While active, this mode provides some Gnorb-specific commands,
and also advises Gnus' reply-related commands in order to
continue to provide tracking of sent messages."
  nil " Gnorb" gnorb-summary-minor-mode-map
  (setq gnorb-gnus-attachment-file-list
	;; Copy the list of attached files from the nnir-tmp-buffer to
	;; this summary buffer.
	(buffer-local-value
	 'gnorb-gnus-attachment-file-list
	  (get-buffer-create nnir-tmp-buffer))))

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-exit]
  'gnorb-summary-exit)

(define-key gnorb-summary-minor-mode-map (kbd "C-c d")
  'gnorb-summary-disassociate-message)

;; All this is pretty horrible, but it's the only way to get sane
;; behavior, there are no appropriate hooks, and I want to avoid
;; advising functions.

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-very-wide-reply-with-original]
  'gnorb-summary-very-wide-reply-with-original)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-wide-reply-with-original]
  'gnorb-summary-wide-reply-with-original)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-reply]
  'gnorb-summary-reply)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-very-wide-reply]
  'gnorb-summary-very-wide-reply)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-reply-with-original]
  'gnorb-summary-reply-with-original)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-wide-reply]
  'gnorb-summary-wide-reply)

(define-key gnorb-summary-minor-mode-map
  [remap gnus-summary-mail-forward]
  'gnorb-summary-mail-forward)

(defun gnorb-summary-wide-reply (&optional yank)
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnorb-summary-reply yank t))

(defun gnorb-summary-reply-with-original (n &optional wide)
  (interactive "P")
  (gnorb-summary-reply (gnus-summary-work-articles n) wide))

(defun gnorb-summary-very-wide-reply (&optional yank)
  (interactive
   (list (and current-prefix-arg
	      (gnus-summary-work-articles 1))))
  (gnorb-summary-reply yank t (gnus-summary-work-articles yank)))

(defun gnorb-summary-reply (&optional yank wide very-wide)
  (interactive)
  (gnus-summary-reply yank wide very-wide)
  (gnorb-summary-reply-hook))

(defun gnorb-summary-wide-reply-with-original (n)
  (interactive "P")
  (gnorb-summary-reply-with-original n t))

(defun gnorb-summary-very-wide-reply-with-original (n)
  (interactive "P")
  (gnorb-summary-reply
   (gnus-summary-work-articles n) t (gnus-summary-work-articles n)))

(defun gnorb-summary-mail-forward (n)
  (interactive "P")
  (gnus-summary-mail-forward n t)
  (gnorb-summary-reply-hook))

(defun gnorb-summary-reply-hook (&rest _args)
  "Function that runs after any command that creates a reply."
  ;; Not actually a "hook"
  (let* ((msg-id (if message-reply-headers
		     (aref message-reply-headers 4)
		   ;; When forwarding messages,
		   ;; `message-reply-headers' is nil.
		   (save-excursion
		     (let ((case-fold-search t))
		       (when (re-search-forward "message-id: +\\(.*\\)$" (point-max) t)
			 (match-string 1))))))
	 (org-id (car-safe (gnus-registry-get-id-key msg-id 'gnorb-ids)))
	 (compose-marker (make-marker))
	 (attachments (buffer-local-value
		       'gnorb-gnus-attachment-file-list
		       (get-buffer "gnorb search"))))
    (when org-id
      (move-marker compose-marker (point))
      (save-restriction
	(widen)
	(message-narrow-to-headers-or-head)
	(goto-char (point-at-bol))
	(open-line 1)
	(message-insert-header
	 (intern gnorb-mail-header)
	 org-id)
	;; As with elsewhere, this should be redundant with
	;; `gnorb-gnus-check-outgoing-headers.'  Even if not, it
	;; should be switched to use `message-send-actions'
	;; (add-to-list 'message-exit-actions
	;; 'gnorb-org-restore-after-send t)
	)
      (goto-char compose-marker))
    (when attachments
      (map-y-or-n-p
       (lambda (a) (format "Attach %s to outgoing message? "
			   (file-name-nondirectory a)))
       (lambda (a)
	 (mml-attach-file a (mm-default-file-encoding a)
			  nil "attachment"))
       attachments
       '("file" "files" "attach")))))

;; TODO: Can this be done using Gnus' quit-config?
(defun gnorb-summary-exit ()
  "Like `gnus-summary-exit', but restores the gnorb window conf."
  (interactive)
  (call-interactively 'gnus-summary-exit)
  (gnorb-restore-layout))

(defun gnorb-summary-disassociate-message ()
  "Disassociate a message from its Org TODO.
This is used in a Gnorb-created *Summary* buffer to remove the
connection between the message and whichever Org TODO resulted in
the message being included in this search."
  (interactive)
  (unless (get-buffer-window gnus-article-buffer t)
    (gnus-summary-display-article
     (gnus-summary-article-number)))
  (let* ((msg-id (gnus-fetch-original-field "message-id"))
	 (org-ids (gnus-registry-get-id-key msg-id 'gnorb-ids))
	 chosen multiple-alist)
    (if org-ids
	(progn
	  (if (= (length org-ids) 1)
	      ;; Only one associated Org TODO.
	      (progn (gnus-registry-set-id-key msg-id 'gnorb-ids nil)
		     (setq chosen (car org-ids)))
	    ;; Multiple associated TODOs, prompt to choose one.
	    (setq multiple-alist
		  (mapcar
		   (lambda (h)
		     (cons (gnorb-pretty-outline h) h))
		   org-ids))
	    (setq chosen
		  (cdr
		   (assoc
		    (org-completing-read
		     "Choose a TODO to disassociate from: "
		     multiple-alist)
		    multiple-alist)))
	    (gnus-registry-set-id-key msg-id 'gnorb-ids
				      (remove chosen org-ids)))
	  (message "Message disassociated from %s"
		   (gnorb-pretty-outline chosen)))
      (message "Message has no associations"))))

(defun gnorb-gnus-summary-mode-hook ()
  "Check if we've entered a Gnorb-generated group, and activate
  `gnorb-summary-minor-mode', if so."
  (let ((method (gnus-find-method-for-group gnus-newsgroup-name))
	(case-fold-search t))
    (when (string-match-p "gnorb" (cadr method))
      (gnorb-summary-minor-mode))))

;;; Automatic noticing of relevant messages

;; likely hooks for the summary buffer include:
;; `gnus-parse-headers-hook'

;; BBDB puts its notice stuff in the `gnus-article-prepare-hook',
;; which seems as good a spot as any.

(defun gnorb-gnus-hint-relevant-message ()
  "When opening an article buffer, check the message to see if it
is relevant to any existing TODO headings. If so, flash a message
to that effect. This function is added to the
`gnus-article-prepare-hook'. It will only do anything if the
option `gnorb-gnus-hint-relevant-article' is non-nil."
  (when (and gnorb-gnus-hint-relevant-article
	     (eieio-object-p gnus-registry-db)
	     (not (memq (car (gnus-find-method-for-group
			      gnus-newsgroup-name))
			'(nnvirtual nnir))))
    (let* ((headers
	    (gnus-data-header
	     (gnus-data-find
	      (gnus-summary-article-number))))
	   (assoc-heading
	    (gnorb-registry-get-id-key
	     (gnus-fetch-original-field "message-id") 'gnorb-ids))
	   (tracked-headings (gnorb-find-tracked-headings headers)))
      (cond (assoc-heading
	     (message "Message is associated with %s"
		      (gnorb-pretty-outline (car assoc-heading) t)))
	    (tracked-headings
	     (message
	      (substitute-command-keys
	       "Possible relevant todo %s, trigger with \\[gnorb-gnus-incoming-do-todo]")
	      (gnorb-pretty-outline (car tracked-headings) t)))
	    (t nil)))))

(defun gnorb-gnus-insert-format-letter-maybe (header)
  (if (eieio-object-p gnus-registry-db)
      (if (not (or (gnus-ephemeral-group-p gnus-newsgroup-name)
		   (gnus-virtual-group-p gnus-newsgroup-name)))
	  (cond ((gnorb-registry-get-id-key
		  (mail-header-message-id header) 'gnorb-ids)
		 gnorb-gnus-summary-tracked-mark)
		((gnorb-find-tracked-headings header)
		 gnorb-gnus-summary-mark)
		(t " "))
	" ")
    ""))

(defalias (intern (concat "gnus-user-format-function-"
			  gnorb-gnus-summary-mark-format-letter))
  (lambda (header)
    (gnorb-gnus-insert-format-letter-maybe header)))

(defun gnorb-gnus-insert-format-tags (header)
  (if (eieio-object-p gnus-registry-db)
      (let ((tags (gnorb-registry-get-id-key
		   (mail-header-message-id header) 'org-tags)))
	(if tags
	    (concat
	     ":" (mapconcat #'identity tags ":") ":")
	  ""))
    ""))

(defalias (intern (concat "gnus-user-format-function-"
			  gnorb-gnus-summary-tags-format-letter))
  (lambda (header)
    (gnorb-gnus-insert-format-tags header)))

;;;###autoload
(defun gnorb-gnus-view ()
  "Display the first relevant TODO heading for the message under point"
  (interactive)
  (let* ((headers (gnus-data-header
		   (gnus-data-find
		    (gnus-summary-article-number))))
	 (tracked-headings
	  (gnorb-find-tracked-headings headers)))
    (when tracked-headings
      (setq gnorb-window-conf (current-window-configuration))
      (move-marker gnorb-return-marker (point))
      (delete-other-windows)
      (org-id-goto (car tracked-headings)))))

(defun gnorb-gnus-startup ()
  (add-hook 'gnus-select-article-hook #'gnorb-gnus-hint-relevant-message)
  (add-hook 'gnus-summary-prepared-hook #'gnorb-gnus-summary-mode-hook))

(add-hook 'gnus-started-hook #'gnorb-gnus-startup)

(defun gnorb-gnus-shutdown ()
  (remove-hook 'gnus-select-article-hook #'gnorb-gnus-hint-relevant-message)
  (remove-hook 'gnus-summary-prepared-hook #'gnorb-gnus-summary-mode-hook))

(gnus-add-shutdown #'gnorb-gnus-shutdown 'gnus)

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here

;;; gnorb-utils.el --- Common utilities for all gnorb stuff -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl-lib)
  (require 'nnheader))
(require 'pcase)
(require 'seq)
(require 'org)
(require 'org-agenda)
(require 'org-element)

(require 'mailcap)
(mailcap-parse-mimetypes)

(defvar bbdb-mode-map)

(defgroup gnorb nil
  "Glue code between Gnus, Org, and BBDB."
  :tag "Gnorb"
  :group 'mail)

(make-obsolete-variable
 'gnorb-trigger-todo-default
 "This variable has been superseded by
`gnorb-org-trigger-actions'"
 "September 8, 2014" 'set)

(defvar gnorb-tmp-dir (make-temp-file "emacs-gnorb-" t)
  "Temporary directory where attachments etc are saved.")

(defvar gnorb-message-org-ids nil
  "List of Org heading IDs from the outgoing Gnus message, used
  to mark mail TODOs as done once the message is sent."
  ;; The send hook either populates this, or sets it to nil, depending
  ;; on whether the message in question has an Org id header. Then
  ;; `gnorb-org-restore-after-send' checks for it and acts
  ;; appropriately, then sets it to nil.
  )

(defvar gnorb-window-conf nil
  "Save window configurations here, for restoration after mails
are sent, or Org headings triggered.")

(defvar gnorb-return-marker (make-marker)
  "Return point here after various actions, to be used together
with `gnorb-window-conf'.")

(defvar gnorb-trigger-capture-location nil
  "Marker pointing at the location where we want to place capture
  templates, for the capture-to-child and capture-to-sibling
  trigger actions.")

(defcustom gnorb-mail-header "X-Org-ID"
  "Name of the mail header used to store the ID of a related Org
  heading. Only used locally: always stripped when the mail is
  sent."
  :group 'gnorb
  :type 'string)

(defun gnorb-version ()
  "Return the version of currently-installed Gnorb.
Only works for Gnorb installed via the package manager."
  (interactive)
  (if (memq 'gnorb package-activated-list)
      (let ((pkg (nth 1 (assq 'gnorb package-alist))))
	(message (package-desc-full-name pkg)))
    (message "Gnorb not installed via package manager."))
  (pkg-info-package-version "gnorb"))


;;; this is just ghastly, but the value of this var is single regexp
;;; group containing various header names, and we want our value
;;; inside that group.
(eval-after-load 'message
  `(let ((ign-headers-list
	  (split-string message-ignored-mail-headers
			"|"))
	 (our-val (concat gnorb-mail-header "\\")))
     (unless (member our-val ign-headers-list)
       (setq ign-headers-list
	     `(,@(butlast ign-headers-list 1) ,our-val
	       ,@(last ign-headers-list 1)))
       (setq message-ignored-mail-headers
	     (mapconcat
	      'identity ign-headers-list "|")))))

;;;###autoload
(defun gnorb-restore-layout ()
  "Restore window layout and value of point after a Gnorb command.

Some Gnorb commands change the window layout (ie `gnorb-org-view'
or incoming email triggering). This command restores the layout
to what it was. Bind it to a global key, or to local keys in Org
and Gnus and BBDB maps."
  (interactive)
  (when (window-configuration-p gnorb-window-conf)
    (select-frame-set-input-focus
     (window-configuration-frame gnorb-window-conf))
    (set-window-configuration gnorb-window-conf)
    (when (buffer-live-p (marker-buffer gnorb-return-marker))
      (goto-char gnorb-return-marker))))

(defun gnorb-bracket-message-id (id)
  "Ensure message-id ID is bound by angle brackets."
  ;; Always use a message-id with angle brackets around it.
  ;; `gnus-summary-goto-article' can handle either, but
  ;; `gnus-request-head' will fail without brackets IF you're
  ;; requesting from an nntp group. Mysterious.
  (unless (string-match "\\`<" id)
    (setq id (concat "<" id)))
  (unless (string-match ">\\'" id)
    (setq id (concat id ">")))
  id)

(defun gnorb-unbracket-message-id (id)
  "Ensure message-id ID is NOT bound by angle brackets."
  ;; This shit is annoying, but Org wants an id with no brackets, and
  ;; Gnus is safest with an id that has brackets. So here we are.
  (replace-regexp-in-string "\\(\\`<\\|>\\'\\)" "" id))

(defun gnorb-reply-to-gnus-link (link)
  "Start a reply to the linked message."
  (let* ((link (org-link-unescape link))
	 (group (car (org-split-string link "#")))
	 (id (gnorb-bracket-message-id
	      (second (org-split-string link "#"))))
	 (backend
	  (car (gnus-find-method-for-group group))))
    (gnorb-follow-gnus-link group id)
    (call-interactively
     (if (eq backend 'nntp)
	 'gnus-summary-followup-with-original
       'gnus-summary-wide-reply-with-original))))

(defun gnorb-follow-gnus-link (group id)
  "Be a little clever about following gnus links.
The goal here is reuse frames and windows as much as possible, so
we're not opening multiple windows on the *Group* buffer, for
instance, and messing up people's layouts. There also seems to be
an issue when opening a link to a message whose *Summary* buffer
is already visible: somehow, after following the link, point ends
up on the message _after_ the one we want, and things go haywire.

So we try to be a little clever. The logical progression here is
this:

1. If the link's target group is already open in a *Summary*
buffer, just switch to that buffer (if it's visible in any frame
then raise it and switch focus, otherwise pull it into the
current window) and go to the message with
`gnus-summary-goto-article'.

2. If the Gnus *Group* buffer is visible in any window or frame,
raise that frame/window and give it focus before following the
link.

3. Otherwise just follow the link as usual, in the current
window."
  (unless (gnus-alive-p)
    (gnus))
  (let* ((sum-buffer (gnus-summary-buffer-name group))
	 (target-buffer
	  (cond
	   ((gnus-buffer-live-p sum-buffer)
	    sum-buffer)
	   ((gnus-buffer-live-p gnus-group-buffer)
	    gnus-group-buffer)
	   (t nil)))
	 (target-window (when target-buffer
			  (get-buffer-window target-buffer t))))
    (if target-window
	;; Our target buffer is displayed somewhere: just go there.
	(progn
	  (select-frame-set-input-focus
	   (window-frame target-window))
	  (switch-to-buffer target-buffer))
      ;; Our target buffer exists, but isn't displayed: pull it up.
      (if target-buffer
	  (switch-to-buffer target-buffer)))
    (message "Following link...")
    (if (gnus-buffer-live-p sum-buffer)
	(gnus-summary-goto-article id nil t)
      (gnorb-open-gnus-link group id))))

(defun gnorb-open-gnus-link (group id)
  "Gnorb version of `org-gnus-follow-link'."
  ;; We've probably already bracketed the id, but just in case this is
  ;; called from elsewhere...
  (let* ((id (gnorb-bracket-message-id id))
	 (arts (gnus-group-unread group))
	 (artno (cdr (gnus-request-head id group)))
	 success)
    (gnus-activate-group group)
    (setq success (gnus-group-read-group arts t group))
    (if success
	(gnus-summary-goto-article artno nil t)
      (signal 'error (format "Group %s could not be opened." group)))))

;; I'd like to suggest this as a general addition to Emacs.  *Very*
;; tired of abusing `completing-read' for this purpose.
(defconst gnorb-select-valid-chars
  (append (number-sequence 97 122)
	  (number-sequence 65 90))
  "A list of characters that are suitable for using as selection
  keys.")

(defvar gnorb-select-choice-buffer "*Selections*"
  "The name of the buffer used to pop up selections.")

(defun gnorb-select-from-list (prompt collection &optional key-func)
  "Prompt the user to select something from COLLECTION.

Selection can happen in a few different ways, depending on the
nature of COLLECTION.  Its elements can be:

1. A plain string.  Simply default to `completing-read'.

2. (string object).  The function uses `completing-read' on the
   strings, returning the selected object.

3. (number object).  As above, but the user enters a number.

4. (character string object).  As #3, but \"string\" is displayed
   as a string label for object.

5. (number string object).  As above, with numbers.

COLLECTION can be passed in ready-made.  Alternately, KEY-FUNC
can be provided.  The collection will be constructed by mapping
this function over the list of objects, and then appending each
object to the corresponding result.  In other words, KEY-FUNC
should return one of the types above, minus the final \"object\"
element.

Alternately, KEY-FUNC can be the symbol 'char, in which case the
elements of COLLECTION will automatically be keyed to ascending
characters (52 or fewer), or 'number, which does the same with
numbers (no upper bound)."
  (interactive)
  (let ((len (length collection)))
    (cl-labels ((pop-up-selections
		 (collection &optional charp)
		 (pop-to-buffer gnorb-select-choice-buffer
				'(display-buffer-in-side-window ((side . bottom))) t)
		 (dolist (c collection)
		   (insert (format "%s: %s\n"
				   (if charp
				       (char-to-string (car c))
				     (car c))
				   (nth 1 c))))))
      (setq collection
	    (pcase key-func
	      ((pred null)
	       collection)
	      ('char
	       (if (> len 52)
		   (error "Use the char option with fewer than 52 items")
		 ;; These distinctions between char/string
		 ;; and number/char are totally manufactured.
		 (seq-mapn #'list gnorb-select-valid-chars collection)))
	      ('number
	       (seq-mapn #'list (number-sequence 1 len) collection))
	      ((and func (pred functionp))
	       (seq-map (lambda (el)
			  (let ((res (funcall func el)))
			    (if (atom res)
				(list res el)
			      (append res
				      (list el)))))
			collection))
	      (_ (error "Invalid key-func: %s" key-func))))
      ;; We only test the car of collection to see what type it is.  If
      ;; elements are mismatched, it's not our problem.
      (unwind-protect
	  (pcase (car collection)
	    ((pred stringp)
	     (completing-read prompt collection nil t))
	    ((pred symbolp)
	     (intern-soft (completing-read prompt collection nil t)))
	    (`(,(pred stringp) ,_)
	     (nth 1 (assoc (completing-read prompt collection nil t)
			   collection)))
	    ;; Looks like pcase might be the wrong tool for this job.
	    ((or `(,(and c (pred numberp) (guard (memq c gnorb-select-valid-chars))) ,_)
		 `(,(and c (pred numberp) (guard (memq c gnorb-select-valid-chars))) ,_ ,_))
	     (pop-up-selections collection t)
	     (car (last (assq (read-char
			       (propertize prompt 'face 'minibuffer-prompt))
			      collection))))
	    ((or `(,(pred numberp) ,_)
		 `(,(pred numberp) ,_ ,_))
	     (pop-up-selections collection)
	     (car (last (assq (read-number prompt)
			      collection)))))
	(when-let ((win (get-buffer-window gnorb-select-choice-buffer)))
	  (quit-window win))))))

(defun gnorb-trigger-todo-action (_arg &optional id)
  "Do the actual restore action. Two main things here. First: if
we were in the agenda when this was called, then keep us in the
agenda. Then let the user choose an action from the value of
`gnorb-org-trigger-actions'."
  (require 'gnorb-org)
  (let* ((agenda-p (eq major-mode 'org-agenda-mode))
	 (root-marker
	  (cond (agenda-p
		 (copy-marker
		  (org-get-at-bol 'org-hd-marker)))
		((derived-mode-p 'org-mode)
		 (save-excursion
		   (org-back-to-heading)
		   (point-marker)))
		(id
		 (save-excursion
		   (org-id-goto id)
		   (org-back-to-heading)
		   (point-marker)))))
	 (id (or id
		 (org-with-point-at root-marker
		   (org-id-get-create))))
	 (action (gnorb-select-from-list
		  (format
		   "Trigger action on %s: "
		   (gnorb-pretty-outline id))
		  gnorb-org-trigger-actions))
	 (link (when gnorb-org-log-add-link
		 (format "[[gnus:%s][message]] "
			 (gnorb-msg-id-to-link
			  (plist-get gnorb-gnus-message-info :msg-id)
			  (plist-get gnorb-gnus-message-info :group))))))
    (unless agenda-p
      (org-reveal))
    (cl-labels
	((make-entry
	  (id)
	  (gnorb-registry-make-entry
	   (plist-get gnorb-gnus-message-info :msg-id)
	   (plist-get gnorb-gnus-message-info :from)
	   (plist-get gnorb-gnus-message-info :subject)
	   id
	   (plist-get gnorb-gnus-message-info :group))))
      ;; Handle our action.
      (if (fboundp action)
	  (org-with-point-at root-marker
	    (make-entry (org-id-get-create))
	    (funcall action gnorb-gnus-message-info))
	(cl-case action
	  (note
	   (org-with-point-at root-marker
	     (make-entry (org-id-get-create))
	     (org-add-log-setup 'note nil nil nil (or link nil))))
	  (todo
	   (if agenda-p
	       (progn
		 (org-with-point-at root-marker
		   (make-entry (org-id-get-create)))
		 (call-interactively 'org-agenda-todo))
	     (org-with-point-at root-marker
	       (make-entry (org-id-get-create))
	       (call-interactively 'org-todo)
	       (when link
		(setq org-log-note-extra link)))))
	  (no-associate
	   nil)
	  (associate
	   (org-with-point-at root-marker
	     (make-entry (org-id-get-create))))
	  ;; We're going to capture a new heading
	  ((cap-child cap-sib)
	   (org-with-point-at root-marker
	     (setq gnorb-trigger-capture-location (point-marker)))
	   (let ((entry
		  ;; Pick a template.
		  (copy-sequence (org-capture-select-template))))
	     ;; Do surgery on that template so that it finds its
	     ;; location using our function.
	     (setf (nth 3 entry)
		   `(function
		     ,(if (eq action 'cap-child)
			  #'gnorb-trigger-capture-child
			#'gnorb-trigger-capture-sibling)))
	     ;; This will likely fail horribly for capture templates
	     ;; that aren't entries or list items.
	     (let ((org-capture-entry entry))
	       ;; When org-capture-entry is let-bound, the capture
	       ;; process will use that template instead of
	       ;; prompting the user. Also, `gnorb-registry-capture'
	       ;; will take care of making the registry entry for us.
	       (call-interactively 'org-capture)))))))
    ;; Lastly, query about attaching email attachments. No matter what
    ;; happens, clear `gnorb-gnus-capture-attachments'.
    (unwind-protect
	(org-with-point-at
	    (if (memq action '(cap-child cap-sib))
		(point)
	      root-marker)
	  (map-y-or-n-p
	   (lambda (a)
	     (format "Attach %s to heading? "
		     (file-name-nondirectory a)))
	   (lambda (a)
	     (with-demoted-errors
		 (org-attach-attach a nil 'mv)))
	   gnorb-gnus-capture-attachments
	   '("file" "files" "attach")))
      (setq gnorb-gnus-capture-attachments nil))))

(defun gnorb-trigger-capture-child ()
  ;; The capture process creates a child by default
  (org-goto-marker-or-bmk gnorb-trigger-capture-location)
  (org-back-to-heading))

(defun gnorb-trigger-capture-sibling ()
  ;; This only works if we're not trying to create a sibling for a
  ;; top-level heading, there appears to be no way to do that.  But in
  ;; that case this trigger action isn't really necessary, just
  ;; handle it with a regular capture.
  (org-goto-marker-or-bmk gnorb-trigger-capture-location)
  (org-up-heading-safe))

(defun gnorb-pretty-outline (id &optional kw)
  "Return pretty outline path of the Org heading indicated by ID.

If the KW argument is true, add the TODO keyword into the path."
  (let ((pt (org-id-find id t)))
    (if pt
	(org-with-point-at pt
	  (let ((el (org-element-at-point)))
	    (concat
	     (if kw
		 (format "(%s): "
			 (org-element-property :todo-keyword el))
	       "")
	     (org-format-outline-path
	      (append
	       (list
		(file-name-nondirectory
		 (buffer-file-name
		  (org-base-buffer (current-buffer)))))
	       (org-get-outline-path)
	       (list
		(replace-regexp-in-string
		 org-bracket-link-regexp
		 "\\3" (org-element-property :raw-value el))))))))
      "[none]")))

(defun gnorb-scan-links (bound &rest types)
  "Scan from point to BOUND looking for links of type in TYPES.
TYPES is a list of symbols; we search for all links corresponding
to those symbols."
  ;; It may be excessive to examine *all* links, rather than just
  ;; creating a specialized regexp for the links we want, but it's
  ;; nice to be lazy and use `org-link-any-re', that seems safer.

  ;; This function should also *not* be responsible for unescaping
  ;; links -- we don't know what they're going to be used for, and
  ;; unescaped is safer.
  (unless (= (point) bound)
    (let ((alist (mapcar #'list (copy-sequence types))))
      (while (re-search-forward org-link-any-re bound t)
	(let ((link (or
		     ;; Bracket link.
		     (match-string-no-properties 2)
		     ;; "Bare" link.
		     (match-string-no-properties 0)))
	      sym)
	  (when (string-match "\\([^:]+\\):\\(.+\\)" link)
	    (setq sym (intern-soft (match-string 1 link))
		  link (match-string 2 link))
	    (when (and sym (memq sym types))
	      (push link (alist-get sym alist))))))
      alist)))

(defun gnorb-msg-id-to-link (msg-id &optional server-group)
  "Create a full Org link to the message MSG-ID.
If SERVER-GROUP isn't given, try to figure it out."
  (let ((server-group (or server-group
			  (car (gnorb-msg-id-request-head msg-id)))))
    (when server-group
      (org-link-escape
       (concat server-group "#"
	       (gnorb-unbracket-message-id msg-id))))))

(defun gnorb-msg-id-request-head (msg-id &optional group)
  "Given a message id, try to find its group and article number.
If GROUP is given, assume that group and just try to find the
article number.

So far we're checking the registry, then the groups in
`gnorb-gnus-sent-groups'. Use search engines? Other clever
methods?"
  (let (candidates server-group check)
    (setq msg-id (gnorb-bracket-message-id msg-id))
    (catch 'found
      (when gnorb-tracking-enabled
	(setq candidates (if group
			     (list group)
			   (append (gnus-registry-get-id-key msg-id 'group)
				   gnorb-gnus-sent-groups)))
	(while (setq server-group (pop candidates))
	  (when (and (stringp server-group)
		     ;; I don't remember the reason for this check,
		     ;; which is totally fragile and fails on groups
		     ;; belonging to `gnus-select-method': another
		     ;; reason why this select method stuff is a mess.

		     ;;(string-match-p "+" server-group)
		     (not
		      (string-match-p
		       "\\(nnir\\|nnvirtual\\|UNKNOWN\\)"
		       server-group))
		     (gnus-activate-group server-group)
		     (setq check
			   (ignore-errors
			     (gnus-request-head msg-id server-group))))

	    (gnus-registry-set-id-key msg-id 'group (list server-group))
	    (throw 'found (cons server-group (cdr check))))))
      nil)))

(defun gnorb-collect-ids (&optional id)
  "Collect all Org IDs for a subtree.
Starting with the heading under point (or the heading indicated
by the ID argument), collect its ID property, and the IDs of all
child headings."
  (save-excursion
    (save-restriction
      (when id
	(org-id-goto id))
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer)
	  'headline
	(lambda (hl)
	  (org-element-property :ID hl))))))

;; Common functions for extracting references and relevant headings
;; from the message under point. For use in gnorb-gnus.el functions.

(defun gnorb-get-real-group-name (group art-no)
  "Find the original group name of a message in a virtual or nnir
group."
  (cl-case (car (gnus-find-method-for-group group))
    (nnvirtual
     (setq group (car (nnvirtual-map-article art-no))))
    (nnir
     (setq group (nnir-article-group art-no)))
    (nnselect
     (setq group (nnselect-article-group art-no))))
  group)

(defun gnorb-find-tracked-headings (headers &optional include-zombies)
  "Check HEADERS for message references and return relevant heading IDs.
HEADERS is a message's data header, as produced by
\(gnus-interactive \"H\"), or, equivalently:

\(gnus-data-header (gnus-data-find (gnus-summary-article-number)))"
  (let ((references (mail-header-references headers))
	(msg-id (mail-header-message-id headers)))
    (when gnorb-tracking-enabled
      (gnorb-find-visit-candidates
       (concat msg-id " " references) include-zombies))))

(defun gnorb-choose-trigger-heading (&optional id)
  "Given an Org heading ID, ask the user if they want to trigger it.
If not, prompt for another target heading. Either way, return the
target heading id."
  (let ((id (if (stringp id)
		id
	      (car-safe id)))
	refile-result)
    (if (and id
	     (y-or-n-p (message
			"Attach part to %s"
			(gnorb-pretty-outline id))))
	id
      (setq refile-result
	    (org-refile-get-location "Attach part to" nil t))
      (save-window-excursion
	(find-file (nth 1 refile-result))
	(goto-char (nth 3 refile-result))
	(org-id-get-create)))))

;; Loading the registry

(defvar gnorb-tracking-enabled nil
  "Internal flag indicating whether Gnorb is successfully plugged
  into the registry or not.")

;;;###autoload
(defun gnorb-tracking-initialize ()
  "Start using the Gnus registry to track correspondences between
Gnus messages and Org headings. This requires that the Gnus
registry be in use, and should be called after the call to
`gnus-registry-initialize'."
  (require 'gnorb-registry)
  (with-eval-after-load 'gnus-registry
    (add-to-list 'gnus-registry-extra-entries-precious 'gnorb-ids)
    (add-to-list 'gnus-registry-track-extra 'gnorb-ids))
  (add-hook
   'gnus-started-hook
   (lambda ()
     ;; The require may be necessary in order to get
     ;; `gnus-user-format-function-g' defined before it's used.  That
     ;; function is likely the first hit on gnorb-gnus, and there's no
     ;; way to autoload it, as it is dynamically defined.
     (require 'gnorb-gnus)
     (unless (gnus-registry-install-p)
       (user-error "Gnorb tracking requires that the Gnus registry be installed."))
     (add-hook 'org-capture-prepare-finalize-hook #'gnorb-registry-capture)
     (setq gnorb-tracking-enabled t))))

;;;###autoload
(defun gnorb-install-defaults ()
  "Set up sane Gnorb customizations and keybindings."
  (interactive)
  (global-set-key (kbd "C-c A") 'gnorb-restore-layout)
  (eval-after-load "gnorb-bbdb"
    '(progn
       (define-key bbdb-mode-map (kbd "C-c S") #'gnorb-bbdb-mail-search)
       (define-key bbdb-mode-map (kbd "C-c l") #'gnorb-bbdb-open-link)
       (define-key bbdb-mode-map [remap bbdb-mail] #'gnorb-bbdb-mail)
       (eval-after-load "gnorb-org"
	 '(org-defkey org-mode-map (kbd "C-c C") #'gnorb-org-contact-link))))
  (eval-after-load "gnorb-org"
    '(progn
       (org-defkey org-mode-map (kbd "C-c t") #'gnorb-org-handle-mail)
       (org-defkey org-mode-map (kbd "C-c v") #'gnorb-org-view)
       (org-defkey org-mode-map (kbd "C-c E") #'gnorb-org-email-subtree)
       (setq gnorb-org-agenda-popup-bbdb t)
       (eval-after-load "org-agenda"
         '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnorb-org-handle-mail)
                 (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnorb-org-view)))))
  (eval-after-load "gnorb-gnus"
    '(progn
       (define-key gnus-summary-mime-map "a" #'gnorb-gnus-article-org-attach)
       (define-key gnus-summary-mode-map (kbd "C-c t") #'gnorb-gnus-incoming-do-todo)
       (define-key gnus-summary-mode-map (kbd "C-c v") #'gnorb-gnus-view)
       (define-key gnus-summary-mode-map (kbd "C-c C-t") #'gnorb-gnus-tag-message)
       (define-key gnus-summary-limit-map (kbd "g") #'gnorb-gnus-insert-tagged-messages)
       (define-key gnus-summary-limit-map (kbd "G") #'gnorb-gnus-insert-tracked-messages)
       (setq gnorb-gnus-capture-always-attach t)
       (push '("attach to org heading" . gnorb-gnus-mime-org-attach)
             gnus-mime-action-alist)
       ;; The only way to add mime button command keys is by redefining
       ;; gnus-mime-button-map, possibly not ideal. Ideal would be a
       ;; setter function in gnus itself.
       (push '(gnorb-gnus-mime-org-attach "a" "Attach to Org heading")
             gnus-mime-button-commands)
       (setq gnus-mime-button-map
             (let ((map (make-sparse-keymap)))
               (dolist (c gnus-mime-button-commands)
                 (define-key map (cadr c) (car c)))
               map))))
  (eval-after-load "message"
    '(progn
       (define-key message-mode-map (kbd "C-c t") #'gnorb-gnus-outgoing-do-todo))))

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here

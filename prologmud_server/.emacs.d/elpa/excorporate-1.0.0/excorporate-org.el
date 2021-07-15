;;; excorporate-org.el --- Exchange Org Mode view     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

;; This program is free software: you can redistribute it and/or modify
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

;; Use Org Mode to display daily meetings.

;;; Code:

(require 'org)
(require 'excorporate)
(require 'cl-lib)

(defvar excorporate-org-buffer-name "*Excorporate*"
  "The buffer into which Org Mode output is inserted.")

(defvar exco-org--temporary-buffers '()
  "A list of per-connection result buffers.")

(defun exco-org--calendar-item-region-at-point ()
  "A special case replacement for `org-element-at-point'.
Return a list (BEGIN END) representing the region of the element
at point, if point is at a calendar item.  If point is not at a
calendar item, return nil.  This works back to Emacs 24.1's
built-in Org 9.1.9 which does not have the `org-element'
feature."
  (catch 'not-a-calendar-item
    (save-excursion
      (cl-values
       (progn (ignore-errors (org-back-to-heading))
	      (unless (looking-at "^\\*\\* ") (throw 'not-a-calendar-item nil))
	      (point))
       (progn (org-end-of-subtree) (forward-char 1)
	      (point))))))

(defun exco-org--connection-identifier-at-point ()
  "Return the connection identifier associated with point."
  (let* ((calendar-headline
	  (save-excursion
	    (org-up-heading-safe)
	    (org-trim (substring-no-properties (thing-at-point 'line))))))
    (when (string-match "\\* Calendar (\\(.*\\))$" calendar-headline)
      (car (read-from-string (match-string 1 calendar-headline))))))

(defun exco-org--item-identifier-at-point ()
  "Return the item identifier associated with point."
  (car (read-from-string
	(org-entry-get (car (org-get-property-block)) "Identifier"))))

(defun exco-org--is-meeting ()
  "Return t if the entry at point is a meeting, not an appointment."
  (let ((region (exco-org--calendar-item-region-at-point)))
    (when region
      (let ((item-text (apply #'buffer-substring-no-properties region)))
	(when (string-match "^\+ Invitees:$" item-text) t)))))

(defun exco-org--organizer ()
  "Return a string representing the item at point's organizer."
  (let ((region (exco-org--calendar-item-region-at-point)))
    (when region
      (let ((item-text (apply #'buffer-substring-no-properties region)))
	(string-match "^+ Organizer: \\(.*\\)$" item-text)
	;; FIXME: Is this a critical region?
	(match-string 1 item-text)))))

(defun exco-org--organizer-matches-connection ()
  "Return non-nil if the entry at point is owned by the connection owner."
  (let ((identifier (exco-org--connection-identifier-at-point))
	(organizer (exco-org--organizer)))
    (cond
     ((stringp identifier)
      (equal identifier organizer))
     ((consp identifier)
      (equal (car identifier) organizer))
     (t
      (error "Did not recognize error")))))

(defmacro exco-org--handle-response (response
				     response-type success failure &rest forms)
  "Handle a server response RESPONSE.
RESPONSE-TYPE is one of CreateItemResponseMessage or
DeleteItemResponseMessage.  SUCCESS and FAILURE are strings added
to the success and failure messages to the user.  FORMS are what
to do, starting from point being in the calendar entry being
operated on."
  `(let ((response-code (exco-extract-value '(ResponseMessages
					      ,response-type
					      ResponseCode)
					    ,response)))
       (if (equal response-code "NoError")
	   (progn ,@forms (message "excorporate-org: Successfully %s" ,success))
	 (message "excorporate-org: Failed to %s: %S" ,failure ,response))))

(defun exco-org--remove-calendar-item ()
  "Remove the calendar item at point."
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
    (let ((region (exco-org--calendar-item-region-at-point)))
      (when region
	(let ((inhibit-read-only t))
	  (apply #'delete-region region))))))

(defun exco-org--reply-to-meeting (acceptance do-not-prompt-for-message)
  "Reply to a meeting.
ACCEPTANCE is a symbol, one of `accept', `tentatively-accept', or
`decline'.  If DO-NOT-PROMPT-FOR-MESSAGE is non-nil, do not
prompt for or include a reply message, otherwise prompt for the
reply message."
  (let (prompt success failure)
    (cl-ecase acceptance
      (accept
       (setq prompt "Acceptance message: ")
       (setq success "accepted")
       (setq failure "accept"))
      (tentatively-accept
       (setq prompt "Tentative acceptance message: ")
       (setq success "accepted tentatively")
       (setq failure "accept tentatively"))
      (decline
       (setq prompt "Declination message: ")
       (setq success "declined")
       (setq failure "decline")))
    (let ((message (when (not do-not-prompt-for-message)
		     (read-from-minibuffer prompt)))
	  (identifier (exco-org--connection-identifier-at-point))
	  (item-identifier (exco-org--item-identifier-at-point)))
      (exco-calendar-item-meeting-reply
       identifier item-identifier message acceptance
       (lambda (_identifier response)
	 (exco-org--handle-response response CreateItemResponseMessage
				    success failure))))))

(defun exco-org-accept-meeting-request (&optional argument)
  "Accept the meeting at point.
With a prefix argument, ARGUMENT, do not prompt for acceptance
message text, and do not send an acceptance response."
  (interactive "P")
  (exco-org--reply-to-meeting 'accept argument))

(defun exco-org-decline-meeting-request (&optional argument)
  "Decline the meeting at point.
With a prefix argument, ARGUMENT, do not prompt for declination
message text, and do not send a declination message."
  (interactive "P")
  (exco-org--reply-to-meeting 'decline argument))

(defun exco-org-tentatively-accept-meeting-request (&optional argument)
  "Tentatively accept the meeting at point.
With a prefix argument, ARGUMENT, do not prompt for tentative
acceptance message text, and do not send a tentative acceptance
message."
  (interactive "P")
  (exco-org--reply-to-meeting 'tentatively-accept argument))

(defun exco-org-cancel-meeting ()
  "Cancel the meeting at point, prompting for a cancellation message."
  (interactive)
  (unless (exco-org--is-meeting)
    (error (concat "This looks like an appointment,"
		   " try `exco-org-delete-appointment' instead.")))
  (let ((identifier (exco-org--connection-identifier-at-point))
	(item-identifier (exco-org--item-identifier-at-point)))
    ;; Make sure the meeting owner matches the connection owner before
    ;; attempting to cancel the meeting.
    (unless (exco-org--organizer-matches-connection)
      (error (concat "exco-org will only attempt to delete"
		     " meetings for which you are the organizer")))
    (when item-identifier
      (exco-calendar-item-meeting-cancel
       identifier item-identifier
       (read-from-minibuffer "Cancellation message: ")
       (lambda (_identifier response)
	 (exco-org--handle-response
	  response CreateItemResponseMessage
	  "cancelled meeting" "cancel meeting"
	  (exco-org--remove-calendar-item)))))))

(defun exco-org-delete-appointment (&optional argument)
  "Delete the appointment at point.
With a prefix argument, ARGUMENT, force-delete this calendar item
without first checking if it is a meeting.  This is required
sometimes, for example as a way to delete meetings for which one
is the organizer and the sole invitee, since the server will
refuse to send a meeting cancellation message to the organizer."
  (interactive "P")
  (when (and (not argument)
	     (exco-org--is-meeting))
    (error "This looks like a meeting, try `exco-org-cancel-meeting' instead"))
  (let ((identifier (exco-org--connection-identifier-at-point))
	(item-identifier (exco-org--item-identifier-at-point)))
    (when item-identifier
      (exco-calendar-item-appointment-delete
       identifier item-identifier
       (lambda (_identifier response)
	 (exco-org--handle-response
	  response DeleteItemResponseMessage
	  "deleted appointment" "delete appointment"
	  (exco-org--remove-calendar-item)))))))

(defun exco-org-initialize-buffer ()
  "Add initial text to the destination buffer."
  (setq exco-org--temporary-buffers '())
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
      (setq buffer-read-only t)
      ;; Some Org mode configurations need `buffer-file-name' to be
      ;; non-nil, or they'll make `org-mode' error out, for example
      ;; `org-startup-with-latex-preview'.  Set `buffer-file-name' to
      ;; something non-nil temporarily during initialization.  Don't
      ;; leave it set or `save-some-buffers' will always prompt about
      ;; *Excorporate*.
      (let ((buffer-file-name excorporate-org-buffer-name))
	(org-mode))
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" 'quit-window)
      (display-buffer (current-buffer))
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(goto-char (point-min))
	(insert "# Updated..."))))

(defun exco-org-format-headline (identifier)
  "Format an Org headline using IDENTIFIER."
  (format "* Calendar (%S)\n" identifier))

(defun exco-org-insert-meeting-headline (subject
					 start-time end-time
					 &optional item-identifier)
  "Insert and schedule a meeting.
SUBJECT is the meeting's subject, START-TIME and END-TIME are the
meeting's start and end times in the same format as is returned
by `current-time'.  ITEM-IDENTIFIER is the opaque item
identifier."
  (let* ((now (current-time))
	 (keyword (if (time-less-p now end-time)
		      "TODO"
		    "DONE")))
    (insert (format "** %s %s\n" keyword subject))
    (org-schedule nil (format-time-string "<%Y-%m-%d %a %H:%M>"
					  start-time))
    (forward-line -1)
    (end-of-line)
    (insert  "--" (format-time-string "<%Y-%m-%d %a %H:%M>" end-time))
    (forward-line)
    (org-set-property "Identifier" (format "%S" item-identifier))
    (org-insert-time-stamp (current-time) t t "+ Retrieved " "\n")))

(defun exco-org-insert-invitees (invitees)
  "Parse and insert a list of invitees, INVITEES."
  (dolist (invitee invitees)
    (insert (format "  + %s\n" invitee))))

(defun exco-org--identifier-buffer (identifier)
  "Return a hidden buffer with a name based on IDENTIFIER."
  (get-buffer-create
   (format " *exco-org-%S*" identifier)))

(defun exco-org-insert-headline (identifier month day year)
  "Insert Org headline for IDENTIFIER on date MONTH DAY YEAR."
  (let ((temporary-buffer (exco-org--identifier-buffer identifier)))
    (push temporary-buffer exco-org--temporary-buffers)
    (with-current-buffer temporary-buffer
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(insert (exco-org-format-headline identifier))
	(org-insert-time-stamp (encode-time 0 0 0 day month year)
			       nil t "  + Date " "\n")))))

(defun exco-org-insert-meeting (subject start end location
				main-invitees optional-invitees
				&optional item-identifier organizer)
  "Insert a scheduled meeting.
SUBJECT is a string, the subject of the meeting.  START is the
meeting start time in Emacs internal date time format, and END is
the end of the meeting in the same format.  LOCATION is a string
representing the location.  MAIN-INVITEES and OPTIONAL-INVITEES
are the requested participants.  ITEM-IDENTIFIER is the opaque
item identifier.  ORGANIZER is a string, the email address of the
meeting organizer."
  (exco-org-insert-meeting-headline subject start end item-identifier)
    (insert (format "+ Duration: %d minutes\n"
		    (round (/ (float-time (time-subtract end start)) 60.0))))
    (insert (format "+ Location: %s\n" location))
    (insert (format "+ Organizer: %s\n" organizer))
    (when main-invitees
      (insert "+ Invitees:\n")
      (exco-org-insert-invitees main-invitees))
    (when optional-invitees
      (insert "+ Optional invitees:\n")
      (exco-org-insert-invitees optional-invitees)))

(defun exco-org-insert-meetings (identifier response)
  "Insert the connection IDENTIFIER's meetings from RESPONSE."
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (end-of-line)
      (insert (format "%s..." identifier))))
  (with-current-buffer (exco-org--identifier-buffer identifier)
    (let ((inhibit-read-only t))
      (org-insert-time-stamp (current-time) t t "  + Last checked " "\n")
      (exco-calendar-item-iterate-general
       response (lambda (&rest arguments)
		  (with-current-buffer (exco-org--identifier-buffer identifier)
		    (org-mode)
		    (let ((new-arguments arguments))
		      (setf (nth 7 new-arguments)
			    (exco-resolve-organizer-email-address-synchronously
			     identifier organizer-structure))
		      (apply #'exco-org-insert-meeting new-arguments))))
       subject start-internal end-internal
       location main-invitees optional-invitees item-identifier
       organizer-structure)
      (goto-char (point-min))
      (if (save-excursion (org-goto-first-child))
	  (org-sort-entries t ?s)
	(forward-line 3)
	(insert "`♘\n")))))

(defun exco-org-finalize-buffer ()
  "Finalize text in buffer after all connections have responded."
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
    ;; Sort top-level entries alphabetically.
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (end-of-line)
      (insert "done.\n")
      (dolist (result-buffer (nreverse exco-org--temporary-buffers))
	(insert-buffer-substring result-buffer)
	(save-excursion (org-up-heading-safe) (org-cycle-hide-drawers 'all))
	(kill-buffer result-buffer))
      (setq exco-org--temporary-buffers '()))))

;;;###autoload
(defun exco-org-show-day (month day year)
  "Show meetings for the date specified by MONTH DAY YEAR."
  (exco-connection-iterate #'exco-org-initialize-buffer
			   (lambda (identifier callback)
			     (exco-org-insert-headline identifier
						       month day year)
			     (exco-get-meetings-for-day identifier
							month day year
							callback))
			   #'exco-org-insert-meetings
			   #'exco-org-finalize-buffer))

(provide 'excorporate-org)

;;; excorporate-org.el ends here

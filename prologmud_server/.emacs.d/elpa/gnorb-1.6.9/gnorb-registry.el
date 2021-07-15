;;; gnorb-registry.el --- Registry implementation for Gnorb -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net.>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Early on, Gnorb's message/todo tracking was done by relying on the
;; user to insert links to received messages into an Org heading, and
;; by automatically storing the Message-Ids of sent messages in a
;; property (`gnorb-org-msg-id-key', defaulting to GNORB_MSG_ID) on
;; the same heading. The heading could find all relevant messages by
;; combining the links (incoming) and the IDs of the Gnorb-specific
;; property (outgoing).
;;
;; In the end, this proved to be fragile and messy. Enter the
;; registry. The Gnus registry is a specialization of a general
;; "registry" library -- it's possible to roll your own. If you want
;; to track connections between messages and Org headings, it's an
;; obvious choice: Each relevant message is stored in the registry,
;; keyed on its Message-ID, and the org-ids of all relevant headings
;; are stored in a custom property, in our case gnorb-ids. This allows
;; us to keep all Gnorb-specific data in one place, without polluting
;; Org files or Gnus messages, persistent on disk, and with the added
;; bonus of providing a place to keep arbitrary additional metadata.
;;
;; The drawback is that the connections are no longer readily visible
;; to the user (they need to query the registry to see them), and it
;; becomes perhaps a bit more difficult (but only a bit) to keep
;; registry data in sync with the current state of the user's Gnus and
;; Org files. But a clear win, in the end.

;;; Code:

(require 'gnus-registry)
(require 'gnorb-utils)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup gnorb-registry nil
  "Gnorb's use of the Gnus registry."
  :tag "Gnorb Registry"
  :group 'gnorb)

(defun gnorb-registry-make-entry (msg-id sender subject org-id group)
  "Create a Gnus registry entry for a message, either received or
sent. Save the relevant Org ids in the 'gnorb-ids key."
  ;; This set-id-key stuff is actually horribly
  ;; inefficient.
  (when gnorb-tracking-enabled
    (gnus-registry-get-or-make-entry msg-id)
    (when sender
      (gnus-registry-set-id-key msg-id 'sender (list sender)))
    (when subject
      (gnus-registry-set-id-key msg-id 'subject (list subject)))
    (when org-id
      (let ((ids (gnus-registry-get-id-key msg-id 'gnorb-ids)))
	(unless (member org-id ids)
	 (gnus-registry-set-id-key msg-id 'gnorb-ids (if (stringp org-id)
							 (cons org-id ids)
						       (append org-id ids))))))
    (when group
      (gnus-registry-set-id-key msg-id 'group (list group)))
    (gnus-registry-get-or-make-entry msg-id)))

(defun gnorb-registry-get-id-key (id key)
  "Get value of symbol KEY for ID's entry in the registry.
Works just like `gnus-registry-get-id-key', but doesn't create an
entry if it doesn't exist already."
  (cdr-safe
   (assq key (nth 1 (assoc id (registry-lookup
			       gnus-registry-db (list id)))))))

(defun gnorb-registry-capture ()
  "When capturing from a Gnus message, add our new Org heading id
to the message's registry entry, under the `gnorb-ids' key."
  ;; We're called by org-capture.
  (declare-function org-capture-get "org-capture" (prop &optional local))
  (defvar org-capture-plist)
  (let ((orig-buff (org-capture-get :original-buffer)))
    (when (and (buffer-live-p orig-buff)
	       (with-current-buffer
		   orig-buff
		 (memq major-mode '(gnus-summary-mode gnus-article-mode)))
	       (not org-note-abort))
      (let* ((msg-id
	      (gnorb-bracket-message-id
	       (plist-get org-store-link-plist :message-id)))
	     (org-id (org-id-get-create)))
	(plist-put org-capture-plist :gnorb-id org-id)
	(gnorb-registry-make-entry msg-id nil nil org-id nil)))))

(defun gnorb-find-visit-candidates (ids &optional include-zombies)
  "For all message-ids in IDS (which should be a list of
Message-ID strings, with angle brackets, or a single string of
Message-IDs), produce a list of Org ids for headings that are
relevant to that message.

If optional argument INCLUDE_ZOMBIES is non-nil, return ID values
even for headings that appear to no longer exist."
  (let (ret-val sub-val)
    (when (stringp ids)
      (setq ids (gnus-extract-references ids)))
    (when gnorb-tracking-enabled
      (setq ids (delete-dups ids))
      (progn
	(dolist (id ids)
	  (when
	      (setq sub-val
		    (gnorb-registry-get-id-key id 'gnorb-ids))
	    (setq ret-val (append sub-val ret-val))))))
    ;; This lets us be reasonably confident that the
    ;; headings still exist.
    (unless include-zombies
      (cl-remove-if-not
       (lambda (org-id)
	 (org-id-find-id-file org-id))
       ret-val))
    (delete-dups ret-val)))

(defun gnorb-delete-association (msg-id org-id)
  "Disassociate a message and a headline.

This removes an Org heading's ORG-ID from the `gnorb-ids' key of
the MSG-ID."
  (let ((org-ids (gnus-registry-get-id-key msg-id 'gnorb-ids)))
    (when (member org-id org-ids)
      (gnus-registry-set-id-key msg-id 'gnorb-ids
				(remove org-id org-ids)))))

(defun gnorb-delete-all-associations (org-id)
  "Delete all message associations for an Org heading.

The heading is identified by ORG-ID. This is suitable for use
after an Org heading is deleted, for instance."
  (let ((assoc-msgs (gnorb-registry-org-id-search org-id))
	(gnorb-id-tracker
	 (registry-lookup-secondary gnus-registry-db 'gnorb-ids)))
    (mapc
     (lambda (msg-id)
       (let ((org-ids
	      (gnus-registry-get-id-key msg-id 'gnorb-ids)))
	 (gnus-registry-set-id-key
	  msg-id 'gnorb-ids (remove org-id org-ids))))
     assoc-msgs)
    (remhash org-id gnorb-id-tracker)))

(defun gnorb-flush-dead-associations (&optional clean-archived)
  "Clean the registry of associations with nonexistent headings.

Gnus will not prune registry entries that appear to be associated
with an Org heading.  If your registry is limited to a very small
size, you may end up with a full registry.  Use this function to
remove dead associations, and free up more entries for possible
pruning.

By default, associations are considered \"live\" if the Org
heading exists in an Org file or in an Org archive file.  When
optional CLEAN_ARCHIVED is non-nil, delete associations from
archived headings as well."
  (interactive "P")
  (let ((gnorb-id-tracker
	 (registry-lookup-secondary gnus-registry-db 'gnorb-ids))
	(deleted-count 0))
    (require 'org-id)
    (maphash
     (lambda (k _)
       (let ((file (org-id-find-id-file k)))
	 (when (or (not file)
		   (and clean-archived
			(string-match-p "org_archive$" file)))
	   (gnorb-delete-all-associations k)
	   (cl-incf deleted-count))))
     gnorb-id-tracker)
    (message "Deleted %d invalid associations"
	     deleted-count)))

(defun gnorb-registry-org-id-search (id)
  "Find all messages that have the org ID in their `gnorb-ids'
key."
  (registry-search gnus-registry-db :member `((gnorb-ids ,id))))

(defun gnorb-registry-org-tag-search (tag)
  "Find all messages that have the org TAG in `org-tags'."
  (registry-search gnus-registry-db :member `((org-tags ,tag))))

(defun gnorb-registry-tracked-messages ()
  "Return all message-ids that have non-empty `gnorb-ids' keys."
  (registry-search gnus-registry-db :regex `((gnorb-ids ".+"))))

(defun gnorb-registry-tagged-messages ()
  "Return all messages with non-empty `org-tags' keys."
  (registry-search gnus-registry-db :regex `((org-tags ".+"))))

(defun gnorb-registry-tracked-headings ()
  "Return all Org heading ids that are associated with messages."
  (hash-table-keys
   (registry-lookup-secondary gnus-registry-db 'gnorb-ids t)))

(defun gnorb-registry-tracked-tags ()
  "Return all tags that have been used on tracked messages."
  (hash-table-keys
   (registry-lookup-secondary gnus-registry-db 'org-tags t)))

(defun gnorb-report-tracking-usage ()
  "Pop up a temporary window reporting on Gnorb usage of the Gnus
registry to track message/heading associations.  Reports the
number of tracked messages, the number of tracked headings, any
tagged messages, and how much of the registry is occupied."
  (interactive)
  (pop-to-buffer
   (get-buffer-create "*Gnorb Usage*")
   '(nil . ((window-height . 10))))
  (gnorb-refresh-usage-status)
  (special-mode)
  (setq revert-buffer-function #'gnorb-refresh-usage-status)
  (local-set-key (kbd "d") (lambda ()
			     (interactive)
			     (progn
			       (gnorb-flush-dead-associations)
			       (gnorb-refresh-usage-status))))
  (local-set-key (kbd "D") (lambda ()
			     (interactive)
			     (progn
			       (gnorb-flush-dead-associations t)
			       (gnorb-refresh-usage-status)))))

(defun gnorb-refresh-usage-status (&optional _ignore-auto _noconfirm)
  "Clear and re-format the *Gnorb Usage* buffer."
  (let* ((messages (gnorb-registry-tracked-messages))
	 (message-num (length messages))
	 (headings (gnorb-registry-tracked-headings))
	 (heading-num (length headings))
	 (tagged (gnorb-registry-tagged-messages))
	 (tags (gnorb-registry-tracked-tags))
	 (total-occupied (length (delete-dups (append messages tagged))))
	 (reg-size (registry-size gnus-registry-db))
	 (reg-max-size
	  (slot-value gnus-registry-db
                      (if (slot-exists-p gnus-registry-db 'max-size)
                          'max-size 'max-hard))))
    (with-current-buffer "*Gnorb Usage*"
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert
	 (format
	  "Tracking %d Gnus messages associated with %d Org headings."
	  message-num heading-num))
	(when tagged
	  (insert (format "\n%d tagged messages, with %d tag%s:\n"
			  (length tagged)
			  (length tags)
			  (if (= 1 (length tags)) "" "s")))
	  (dolist (tag tags)
	    (insert (format "%s\n" tag))))
	(insert "\n\n")
	(insert
	 (format
	  "Occupying %.2f%% (%d/%d) of the registry (max %d)."
	  (* 100 (/ (float total-occupied) reg-size))
	  total-occupied reg-size reg-max-size))
	(insert "\n\n")
	(insert "Press 'd' to delete associations for non-existent Org headings.\n")
	(insert "Press 'D' to delete associations for both non-existent and archived Org headings.")))))

(defun gnorb-registry-transition-from-props (arg)
  "Helper function for transitioning the old tracking system to the new.

The old system relied on storing sent message ids on relevant Org
headings, in the `gnorb-org-msg-id-key' property. The new system
uses the gnus registry to track relations between messages and
Org headings. This function will go through your agenda files,
find headings that have the `gnorb-org-msg-id-key' property set,
and create new registry entries that reflect that connection.

Call with a prefix arg to additionally delete the
`gnorb-org-msg-id-key' altogether from your Org headings. As this
function will not create duplicate registry entries, it's safe to
run it once with no prefix arg, to keep the properties in place,
and then once you're sure everything's working okay, run it again
with a prefix arg, to clean the Gnorb-specific properties from
your Org files."
  (interactive "P")
  (require 'gnorb-org)
  (defvar gnorb-org-find-candidates-match)
  (defvar gnorb-org-msg-id-key)
  (let ((count 0))
    (message "Collecting all relevant Org headings, this could take a while...")
    (org-map-entries
     (lambda ()
       (let ((id (org-id-get))
	     (props (org-entry-get-multivalued-property
	             (point) gnorb-org-msg-id-key)))
	 (when props
	   ;; If the property is set, we should probably assume that any
	   ;; Gnus links in the subtree are relevant, and should also be
	   ;; collected and associated.
	   (let ((links (gnorb-scan-links
		         (org-element-property :end (org-element-at-point))
		         'gnus)))
	     (dolist (l (alist-get 'gnus links))
	       (gnorb-registry-make-entry
	        (cl-second (split-string l "#")) nil nil
	        id (cl-first (split-string l "#")))))
	   (dolist (p props)
	     (gnorb-registry-make-entry p nil nil id nil)
	     ;; This function will try to find the group for the message
	     ;; and set that value on the registry entry if it can find
	     ;; it.
	     (unless (gnus-registry-get-id-key p 'group)
	       (gnorb-msg-id-request-head p))
	     (cl-incf count)))))
     gnorb-org-find-candidates-match
     'agenda 'archive 'comment)
    (message "Collecting all relevant Org headings, this could take a while... done")
    ;; Delete the properties if the user has asked us to do so.
    (if (equal arg '(4))
	(progn
	  (dolist (f (org-agenda-files))
	    (with-current-buffer (get-file-buffer f)
	      (org-delete-property-globally gnorb-org-msg-id-key)))
	  (message "%d entries created; all Gnorb-specific properties deleted."
		   count))
      (message "%d entries created." count))))

(provide 'gnorb-registry)
;;; gnorb-registry.el ends here

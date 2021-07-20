;;; org-rtm.el --- Simple import/export from rememberthemilk to org-mode
;; Copyright (c) 2016 Philipp Middendorf
;; Author: Philipp Middendorf <pmidden@secure.mailbox.org>
;; Created: 15 Jan 2016
;; Version: 0.1
;; Package-Version: 20160214.1236
;; Package-Commit: adc42ad1fbe92ab447ccc9553780f4456f2508d2
;; Package-Requires: ((rtm "0.1"))
;; Keywords: outlines, data
;; Homepage: https://github.com/pmiddend/org-rtm

;; This product uses the Remember The Milk API but is not endorsed or
;; certified by Remember The Milk

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license (see COPYING).

;;; Commentary:
;; Simple import/export from rememberthemilk to org-mode
;;
;; The project is hosted at https://github.com/pmiddend/org-rtm
;; The latest version, and all the relevant information can be found there.
;;; Code:
(require 'rtm)
(require 'org)


(defgroup org-rtm ()
  "Retrieve and complete tasks from rememberthemilk.com and convert them to org-mode"
  :group 'external
  :link '(url-link "https://github.com/pmiddend/org-rtm")
  :prefix "org-rtm-")

(defcustom org-rtm-import-file "~/rtm.org"
  "Where to export the contents of RTM to when using org-rtm-import."
  :group 'org-rtm
  :type 'file)

(defcustom org-rtm-complete-after-import nil
  "Complete the imported tasks in RTM.
It might be a good idea to set this after you verified that 
the import process is working well."
  :group 'org-rtm
  :type 'boolean)

(defun org-rtm-assoc-value (symbol list)
  "Get the value behind SYMBOL in an association LIST (not the pair of key/value)."
  (cdr (assoc symbol list)))

(defun org-rtm-print-list (rtml)
  "Convert an RTM list RTML to an org mode segment (top level, starting with *)."
  (progn
    (concat
     "* "
     (org-rtm-assoc-value 'name (car (cdr rtml))))))

(defun org-rtm-format-note (note)
  "Format a single RTM task NOTE."
  (nth 2 note))

(defun org-rtm-format-notes (notes-list)
  "Format a list NOTES-LIST of RTM task notes and concatenate to string."
  (cond ((equal (length notes-list) 0) "")
	(t (concat "\n" (mapconcat 'org-rtm-format-note notes-list "\n")))))

(defun org-rtm-format-time-to-org (time-value)
  "Convert an ISO date time TIME-VALUE to the org-mode time format. 
I didn't find built-in function to accomplish this."
  (print (length time-value))
  (format-time-string (cdr org-time-stamp-formats) (date-to-time time-value)))

(defun org-rtm-print-entry (e)
  "Format a single RTM task E and output as second level org segment (starting with **)."
  (let*
      ((topAssocList (cdr e))
       (taskAssocList (car topAssocList))
       (due (org-rtm-assoc-value 'due (car (org-rtm-assoc-value 'task topAssocList))))
       (notes-list (nthcdr 2 (assoc 'notes topAssocList))))
    (progn
      (concat
       "** TODO "
       (org-rtm-assoc-value 'name taskAssocList)
       (if (not (or (eq due nil) (string= due ""))) (concat "\nSCHEDULED: " (org-rtm-format-time-to-org due)))
       (if (org-rtm-assoc-value 'url taskAssocList) (concat "\n" (org-rtm-assoc-value 'url taskAssocList)) "")
       (org-rtm-format-notes notes-list)))))

(defun org-rtm-format-list-entries (list-id)
  "Convert a single RTM task list LIST-ID to org mode items."
  (mapconcat 'org-rtm-print-entry (nthcdr 2 (car (rtm-tasks-get-list list-id "status:incomplete"))) "\n"))

(defun org-rtm-format-list (list-id list-name)
  "Format a single list with LIST-ID and LIST-NAME as a top-level org-mode segment starting with *."
  (progn
    (concat "* " list-name "\n" (org-rtm-format-list-entries list-id))))

(defun org-rtm-format-lists ()
  "Format RTM lists to org mode segments."
  (mapconcat (lambda (list) (org-rtm-format-list (org-rtm-assoc-value 'id (nth 1 list)) (org-rtm-assoc-value 'name (nth 1 list)))) (rtm-lists-get-list) "\n"))

(defun org-rtm-complete-items (list-id)
  "Complete all items in a given RTM list with LIST-ID (doesn't do any org-mode conversion)."
  (mapcar
   (lambda (taskseries-w-task)
     (rtm-tasks-complete list-id (car taskseries-w-task) (cdr taskseries-w-task)))
   (org-rtm-retrieve-taskseries-id-with-task-list list-id)))

(defun org-rtm-retrieve-taskseries-id-with-task-list (list-id)
  "Retrieves the taskseries entry for list with LIST-ID."
  (mapcar
   (lambda (list)
     `(,(org-rtm-assoc-value 'id (nth 1 list)) . ,(org-rtm-assoc-value 'id (car (org-rtm-assoc-value 'task (nthcdr 2 list))))))
   (nthcdr 2 (car (rtm-tasks-get-list list-id "status:incomplete")))))

(defun org-rtm-retrieve-list-ids ()
  "Retrieve a list of RTM list ids."
  (mapcar (lambda (list) (org-rtm-assoc-value 'id (nth 1 list))) (rtm-lists-get-list)))

(defun org-rtm-complete-all-lists ()
  "Complete all items of RTM all lists."
  (mapcar (lambda (list-id) (org-rtm-complete-items list-id)) (org-rtm-retrieve-list-ids)))

(defun org-rtm-import ()
  "Import RTM tasks to the given import file (overwriting it), then optionally completing the tasks, then opening the file in Emacs."
  (interactive)
  (message "Starting RTM import...")
  (let
      ((import-data (org-rtm-format-lists)))
    (find-file org-rtm-import-file)
    (erase-buffer)
    (if
	org-rtm-complete-after-import
	(progn
	  (org-rtm-complete-all-lists)
	  (message "Imported and completed all RTM tasks"))
        (message "Imported tasks, not completing RTM tasks because of configuration option"))
    (insert import-data)))

(provide 'org-rtm)
;;; org-rtm.el ends here

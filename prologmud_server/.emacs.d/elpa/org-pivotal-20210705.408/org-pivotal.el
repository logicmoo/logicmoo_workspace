;;; org-pivotal.el --- Sync Pivotal Tracker to org buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huy Duong

;; Author: Huy Duong <qhuyduong@hotmail.com>
;; URL: https://github.com/org-pivotal/org-pivotal
;; Version: 0.1
;; Package-Requires: ((a "0.1.1") (dash "2.18.0") (emacs "26.1") (request "0.3.0"))

;; This file is not part of GNU Emacs.

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

;; org-pivotal is a utility to sync Pivotal Tracker to org buffer

;;; Code:

(require 'a)
(require 'dash)
(require 'org)
(require 'org-pivotal-api)
(require 'subr-x)

(defconst org-pivotal--base-url "https://www.pivotaltracker.com"
  "Base URL.")

(defconst org-pivotal--transition-states
  '("Unstarted" "Started" "Finished" "Delivered" "|" "Accepted" "Rejected")
  "Story status will be one of these values.")

(defun org-pivotal--select-project (projects)
  "Prompt user to select a project from PROJECTS."
  (funcall (-compose '(lambda (projects)
                        (cadr (assoc
                               (completing-read "Select your project?"
                                                (-map 'car projects))
                               projects)))
                     '(lambda (projects)
                        (-map (lambda (project)
                                (list (alist-get 'project_name project)
                                      (alist-get 'project_id project)))
                              projects)))
           projects))

(defun org-pivotal--update-buffer-with-metadata (project my-info)
  "Update org buffer with metadata from PROJECT and MY-INFO."
  (with-current-buffer (current-buffer)
    (erase-buffer)
    (org-mode)
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (-map (lambda (item) (insert item "\n"))
          (list ":PROPERTIES:"
                (format "#+PROPERTY: project-name %s" (alist-get 'name project))
                (format "#+PROPERTY: project-id %d" (alist-get 'id project))
                (format "#+PROPERTY: url %s/n/projects/%d" org-pivotal--base-url (alist-get 'id project))
                (format "#+PROPERTY: my-id %d" (alist-get 'id my-info))
                (format "#+PROPERTY: filter -state:accepted AND -state:rejected")
                (format "#+TODO: %s" (string-join org-pivotal--transition-states " "))
                ":END:
"))))

;;;###autoload
(defun org-pivotal-install-project-metadata ()
  "Install selected project's metadata to buffer."
  (interactive)
  (let ((my-info (org-pivotal-api--get-my-info)))
    (let ((project (funcall (-compose 'org-pivotal-api--get-project-info
                                      'org-pivotal--select-project)
                            (alist-get 'projects my-info))))
      (org-pivotal--update-buffer-with-metadata project my-info))))

(defun org-pivotal--convert-story-to-headline (story)
  "Convert STORY to org heading."
  (-map (lambda (item)
          (insert item "\n"))
        (list (format "* %s %s"
                      (upcase-initials (alist-get 'current_state story))
                      (alist-get 'name story))
              "  :PROPERTIES:"
              (format "  :ID: %s" (alist-get 'id story))
              (format "  :Type: %s" (upcase-initials (alist-get 'story_type story)))
              (format "  :Points: %s" (alist-get 'estimate story))
              (format "  :Updated: %s" (alist-get 'updated_at story))
              (format "  :URL: %s" (alist-get 'url story))
              (format "  :Description: %s" (alist-get 'description story))
              (format "  :Labels: %s" (string-join
                                     (-map (lambda (label) (format "\"%s\""(alist-get 'name label)))
                                           (alist-get 'labels story))
                                     " "))
              "  :END:
")))

(defun org-pivotal--update-buffer-with-stories (stories)
  "Update org buffer with STORIES."
  (with-current-buffer (current-buffer)
    (org-mode)
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (goto-char (point-min))
    (outline-next-heading)
    (kill-region (point-at-bol) (point-max))
    (-map 'org-pivotal--convert-story-to-headline stories)))

;;;###autoload
(defun org-pivotal-pull-stories ()
  "Pull stories to org buffer."
  (interactive)
  (org-set-regexps-and-options)
  (funcall (-compose 'org-pivotal--update-buffer-with-stories
                     'org-pivotal-api--fetch-stories)
           (org-entry-get (point) "project-id" t)
           (org-entry-get (point) "filter" t)))

(defun org-pivotal--property-exists (property)
  (and property
       (or
        (numberp property)
        (not (s-equals? property "")))))

(defun org-pivotal--convert-headline-to-story (properties)
  "Convert headline's PROPERTIES to story."
  (let
      ((id (a-get properties "ID"))
       (name (a-get properties "ITEM"))
       (type (a-get properties "TYPE"))
       (todo (a-get properties "TODO"))
       (estimate (a-get properties "POINTS"))
       (labels (a-get properties "LABELS"))
       (description (a-get properties "DESCRIPTION")))
    (-filter (lambda (cell) (cdr cell))
             (list (cons "id" id)
                   (cons "name" name)
                   (cons "story_type" type)
                   (cons "current_state" todo)
                   (cons "estimate" (if (org-pivotal--property-exists estimate)
                                        (string-to-number estimate)
                                      nil))
                   (cons "labels" (if (org-pivotal--property-exists labels)
                                      (vconcat (s-split " " labels))
                                    nil))
                   (cons "description" description)))))

;;;###autoload
(defun org-pivotal-push-story ()
  "Push current story to Pivotal."
  (interactive)
  (let* ((story (org-pivotal--convert-headline-to-story (org-entry-properties)))
         (id (a-get story "id")))
    (if id (org-pivotal-api--update-story
              (org-entry-get (point) "project-id" t)
              story)
      (org-pivotal-api--create-story
              (org-entry-get (point) "project-id" t)
              story))))

(defun org-pivotal--convert-task-to-checklist (task)
  "Convert TASK to org checklist."
  (-map (lambda (item)
          (insert item "\n"))
        (list (format "  - [%s] %s"
                      (if (eq (alist-get 'complete task) t) "x" " ")
                      (alist-get 'description task)))))

(defun org-pivotal--append-tasks-to-current-story (tasks)
  "Append TASKS to current story."
  (with-current-buffer (current-buffer)
    (org-mode)
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (-map 'org-pivotal--convert-task-to-checklist tasks)))

;;;###autoload
(defun org-pivotal-pull-story-tasks ()
  "Pull current story's tasks."
  (interactive)
  (funcall (-compose 'org-pivotal--append-tasks-to-current-story
                     'org-pivotal-api--fetch-story-tasks)
           (org-entry-get (point) "project-id" t)
           (org-entry-get (point) "ID")))

;;;###autoload
(define-minor-mode org-pivotal-mode
  "Define minor mode for org-pivotal."
  :lighter " op"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ( i") #'org-pivotal-install-project-metadata)
            (define-key map (kbd "C-c ( f") #'org-pivotal-pull-stories)
            (define-key map (kbd "C-c ( p") #'org-pivotal-push-story)
            (define-key map (kbd "C-c ( t") #'org-pivotal-pull-story-tasks)
            map))

(provide 'org-pivotal)

;;; org-pivotal.el ends here

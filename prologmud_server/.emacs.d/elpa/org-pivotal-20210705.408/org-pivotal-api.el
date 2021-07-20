;;; org-pivotal-api.el --- Interface to Pivotal Tracker APIs -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huy Duong

;; Author: Huy Duong <qhuyduong@hotmail.com>
;; URL: https://github.com/org-pivotal/org-pivotal
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

;; org-pivotal-api is the interface to Pivotal Tracker APIs

;;; Code:

(require 'a)
(require 'dash)
(require 'json)
(require 'request)

(defvar org-pivotal-api-token nil
  "API key found on the /profile page of pivotal tracker.")

(defconst org-pivotal-api--base-url "https://www.pivotaltracker.com/services/v5"
  "Base APIv5 URL.")

(defun org-pivotal-api--url-generator (&rest parts-of-url)
  "Build a Pivotal API URL from PARTS-OF-URL."
  (apply 'concat org-pivotal-api--base-url
         (-map (lambda (part) (concat "/" part)) parts-of-url)))

(fset 'org-pivotal--api-error-handler
      '(cl-function
        (lambda
          (&rest args &key &key data error-thrown &allow-other-keys)
          (message "Error: %S %s" error-thrown data))))

(defun org-pivotal-api--call (url method &optional query data)
  "Access wrapper for the Pivotal (v5) JSON API.
URL of the API endpoint
METHOD to use
QUERY params
DATA data."
  (funcall (-compose (lambda (response)
                       (unless (request-response-error-thrown response)
                         (request-response-data response)))
                     (lambda (url method query data)
                       (request url
                                :data (if data (json-encode data) nil)
                                :headers `(("X-TrackerToken" . ,org-pivotal-api-token)
                                           ("Content-Type" . "application/json"))
                                :params query
                                :parser 'json-read
                                :sync t
                                :error org-pivotal--api-error-handler
                                :type method)))
           url method query data))

(defun org-pivotal-api--get-project-info (project-id)
  "Get PROJECT-ID's project info."
  (org-pivotal-api--call
   (org-pivotal-api--url-generator "projects"
                                   (if (numberp project-id)
                                       (number-to-string project-id)
                                     project-id))
   "GET"))

(defun org-pivotal-api--get-my-info ()
  "Get my Pivotal User ID."
  (org-pivotal-api--call (org-pivotal-api--url-generator "me") "GET"))

(defun org-pivotal-api--fetch-stories (project-id &optional filter)
  "Get stories from PROJECT-ID's project with FILTER."
  (org-pivotal-api--call
   (org-pivotal-api--url-generator "projects"
                                   project-id
                                   "stories")
   "GET"
   (when filter `(("filter" . ,filter)))))

(defun org-pivotal-api--create-story (project-id story)
  "Create STORY in PROJECT-ID's project Pivotal."
  (org-pivotal-api--call
   (org-pivotal-api--url-generator "projects"
                                   project-id
                                   "stories")
   "POST"
   nil
   story))

(defun org-pivotal-api--update-story (project-id story)
  "Store STORY to PROJECT-ID's project Pivotal."
  (org-pivotal-api--call
   (org-pivotal-api--url-generator "projects"
                                   project-id
                                   "stories"
                                   (a-get story "id"))
   "PUT"
   nil
   story))

(defun org-pivotal-api--fetch-story-tasks (project-id story-id)
  "Fetch STORY-ID's tasks in PROJECT-ID's project."
  (org-pivotal-api--call
   (org-pivotal-api--url-generator "projects"
                                   project-id
                                   "stories"
                                   story-id
                                   "tasks")
   "GET"))

(provide 'org-pivotal-api)

;;; org-pivotal-api.el ends here

;;; libbcel-nav.el --- Code to navigate Basecamp entities  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1") (request "0.3.1"))
;; Version: 0.4.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains code to go from an entity to another one.  For
;; example, the file contains `libbcel-nav-children' to list the
;; direct children of an entity (e.g., the messages of a message
;; board).

;;; Code:

(require 'libbcel-structs)
(require 'libbcel-client)
(require 'libbcel-util)

(require 'subr-x)

(cl-defgeneric libbcel-nav-children (entity callback)
  "Execute CALLBACK with the children of ENTITY as parameter.")

(cl-defmethod libbcel-nav-children ((_entity (eql projects)) callback)
  "Execute CALLBACK with the list of all projects as parameter."
  (libbcel-client-get-path
   "projects.json"
   (lambda (projects-data)
     (funcall callback
              (libbcel-structs-create-instances-from-data projects-data 'projects)))))

(cl-defmethod libbcel-nav-children ((project libbcel-project) callback)
  "Execute CALLBACK with the list of all PROJECT's tools as parameter."
  (let* ((enabled-tool-alists (seq-filter
                               (lambda (tool-alist)
                                 (not (eq (map-elt tool-alist 'enabled) :json-false)))
                               (libbcel-project-tools project)))
         (tool-urls (seq-map
                     (lambda (tool-alist) (map-elt tool-alist 'url))
                     enabled-tool-alists)))
    (libbcel-util-async-mapcar
     #'libbcel-client-get-url
     tool-urls
     (lambda (tools-data) (funcall callback (libbcel-structs-create-instances-from-data tools-data project))))))

(cl-defmethod libbcel-nav-children ((tool libbcel-tool) callback)
  "Execute CALLBACK with the list of TOOL entities as parameter."
  (libbcel-client-get-url
   (libbcel-tool-children-url tool)
   (lambda (children-data)
     (funcall callback
              (libbcel-structs-create-instances-from-data children-data tool)))))

(cl-defmethod libbcel-nav-children ((todolist libbcel-todolist) callback)
  "Execute CALLBACK with TODOLIST's todos as parameter."
  (libbcel-util-async-mapcar
   (lambda (params partial-callback)
     (libbcel-client-get-url
      (libbcel-todolist-todos-url todolist)
      (lambda (todos-data)
        (funcall partial-callback (libbcel-structs-create-instances-from-data todos-data todolist)))
      params))
   (list nil '((completed . "true")))
   (lambda (todos)
     (funcall callback (apply #'seq-concatenate 'list todos)))))

(cl-defgeneric libbcel-nav-comments ((entity libbcel-entity) callback)
  "Execute CALLBACK with a list of ENTITY's comments."
  (if (and (> (libbcel-entity-comments-count entity) 0)
           (not (string-empty-p (libbcel-entity-comments-url entity))))
      (libbcel-client-get-url
       (libbcel-entity-comments-url entity)
       (lambda (comments-data)
         (funcall callback (libbcel-structs-create-instances-from-data comments-data entity))))
    (funcall callback nil)))

(provide 'libbcel-nav)
;;; libbcel-nav.el ends here

;;; libbcel.el --- Library to connect to basecamp 3 API -*- lexical-binding: t; -*-

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

;; This library provides a bunch of functions and structures to
;; connect to Basecamp 3 API.  The connection is handled by
;; libbcel-proxy.el and the JS files in the proxy/ directory.

;;; Code:

(require 'libbcel-util)
(require 'libbcel-oauth)
(require 'libbcel-client)
(require 'libbcel-structs)
(require 'libbcel-nav)
(require 'libbcel-actions)


;; Configuration

(defgroup libbcel nil
  "Configure libbcel to integrate Basecamp."
  :group 'external)


;;; Public functions

(defun libbcel-completing-read (prompt entities &optional transformer)
  "PROMPT user to select one entity among ENTITIES.

Transform each entity to a string with TRANSFORMER,
`libbcel-entity-name' if nil."
  (let* ((transformer (or transformer #'libbcel-entity-name))
         (map (make-hash-table :test 'equal :size (length entities)))
         (entity-strings (mapcar (lambda (entity) (funcall transformer entity)) entities)))
    (cl-mapcar (lambda (entity entity-string)
                 (puthash entity-string entity map))
               entities entity-strings)
    (let ((entity-string (completing-read prompt entity-strings nil t)))
      (gethash entity-string map))))

(defun libbcel-completing-read-entity (function prompt entity &optional transformer)
  "Call FUNCTION after prompting for a child of ENTITY.

Pass PROMPT, the elements of ENTITY and TRANSFORMER to
`libbcel-completing-read'."
  (libbcel-nav-children
   entity
   (lambda (entities)
     (funcall function
              (libbcel-completing-read prompt entities transformer)))))

(cl-defmethod libbcel-nav-children ((entities list) callback)
  "Execute CALLBACK with the children of all ENTITIES as parameter."
  (libbcel-util-async-mapcar
   #'libbcel-nav-children
   entities
   callback))

(provide 'libbcel)
;;; libbcel.el ends here

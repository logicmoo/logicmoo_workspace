;;; libbcel-structs.el --- Define Basecamp data structures  -*- lexical-binding: t; -*-

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

;; Define the structures and their accessors

;;; Code:

(require 'map)

(cl-defstruct (libbcel-entity
               (:constructor libbcel--entity-create)
               (:conc-name libbcel-entity-))
  (id nil :read-only t)
  (name nil :read-only t)
  (url nil :read-only t)
  (app-url nil :read-only t :alist-key-name app_url)
  (alist nil :read-only t)
  (comments-count 0 :read-only t :alist-key-name comments_count)
  (comments-url 0 :read-only t :alist-key-name comments_url)
  (parent nil :read-only t))

(cl-defstruct (libbcel-project
               (:include libbcel-entity)
               (:constructor libbcel-project-create)
               (:conc-name libbcel-project-))
  (description nil :read-only t)
  (tools nil :read-only t :alist-key-name dock))

(cl-defstruct (libbcel-tool
               (:include libbcel-entity
                         (name nil :alist-key-name title))
               (:constructor libbcel-tool-create)
               (:conc-name libbcel-tool-))
  (enabled nil
           :read-only t
           :alist-transformer (lambda (data) (not (eq data :json-false))))
  (children-url nil :read-only t))

(cl-defstruct (libbcel-message-board
               (:include libbcel-tool
                         (children-url nil :alist-key-name messages_url))
               (:constructor libbcel-message-board-create)
               (:conc-name libbcel-message-board-)))

(cl-defstruct (libbcel-todoset
               (:include libbcel-tool
                         (children-url nil :alist-key-name todolists_url))
               (:constructor libbcel-todoset-create)
               (:conc-name libbcel-todoset-)))

(cl-defstruct (libbcel-message
               (:include libbcel-entity
                         (name nil :alist-key-name subject))
               (:constructor libbcel-message-create)
               (:conc-name libbcel-message-))
  (content nil :read-only t))

(cl-defstruct (libbcel-todolist
               (:include libbcel-entity)
               (:constructor libbcel-todolist-create)
               (:conc-name libbcel-todolist-))
  (todos-url nil
             :read-only t
             :alist-key-name todos_url))

(cl-defstruct (libbcel-todo
               (:include libbcel-entity
                         (name nil :alist-key-name title))
               (:constructor libbcel-todo-create)
               (:conc-name libbcel-todo-))
  (description nil :read-only t)
  (completed nil
             :read-only t
             :alist-transformer (lambda (data) (not (eq data :json-false))))
  (completion-url nil :read-only t :alist-key-name completion_url))

(cl-defstruct (libbcel-comment
               (:include libbcel-entity)
               (:constructor libbcel-comment-create)
               (:conc-name libbcel-comment-))
  (content nil :read-only t)
  (created-at nil :read-only t :alist-key-name created_at)
  (creator nil
           :read-only t
           :alist-transformer (lambda (data)
                                (libbcel-structs-create-instance-from-data data))))

(cl-defstruct (libbcel-person
               (:include libbcel-entity)
               (:constructor libbcel-person-create)
               (:conc-name libbcel-person-)))

(cl-defmethod libbcel-equal ((entity1 libbcel-entity) (entity2 libbcel-entity))
  "Return t iff ENTITY1 is the same entity as ENTITY2."
  (equal (libbcel-entity-id entity1) (libbcel-entity-id entity2)))

(defun libbcel-structs-create-instance-from-data (entity-data &optional parent)
  "Return a structure from ENTITY-DATA.
ENTITY-DATA is an alists.

If PARENT is provided, set the created entity's parent to
PARENT.  This can later be retrieved with `libbcel-entity-parent'.

The structure to instanciate is decided `libbcel-structs--infer-struct-type'."
  (let ((struct-type (libbcel-structs--infer-struct-type entity-data)))
    (when struct-type
      (apply
       #'record
       struct-type
       (mapcar
        (lambda (slot-info)
          (let* ((alist-key (or (plist-get slot-info :alist-key-name)
                                (car slot-info)))
                 (alist-value (pcase alist-key
                                ('alist entity-data)
                                ('parent parent)
                                (_ (map-elt entity-data alist-key))))
                 (transformer (or (plist-get slot-info :alist-transformer)
                                  #'identity)))
            (funcall transformer alist-value)))
        (cdr (cl-struct-slot-info struct-type)))))))

(defun libbcel-structs-create-instances-from-data (entities-data &optional parent)
  "Return a list of structures from ENTITIES-DATA.
ENTITIES-DATA is a list of alists.

If PARENT is provided, set the created entity's parent to
PARENT.  This can later be retrieved with `libbcel-entity-parent'.

The structures to instanciate are decided by `libbcel-structs--infer-struct-type'."
  (seq-remove #'null
              (mapcar (lambda (data) (libbcel-structs-create-instance-from-data data parent))
                      entities-data)))

(defun libbcel-structs--infer-struct-type (entity-data)
  "Return a symbol of a structure type to instanciate for ENTITY-DATA."
  (let ((type-name (map-elt entity-data 'type)))
    (if type-name
        (pcase type-name
          ("Message::Board" 'libbcel-message-board)
          ("Todoset" 'libbcel-todoset)
          ("Project" 'libbcel-project)
          ("Todolist" 'libbcel-todolist)
          ("Todo" 'libbcel-todo)
          ("Comment" 'libbcel-comment)
          (_ (message "libbcel-structs: I don't know what the type is for `%s'" type-name)
             nil))
      (cond
       ((map-contains-key entity-data 'dock) 'libbcel-project)
       ((map-contains-key entity-data 'personable_type) 'libbcel-person)))))

(provide 'libbcel-structs)
;;; libbcel-structs.el ends here

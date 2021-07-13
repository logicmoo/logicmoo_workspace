;;; libbcel-actions.el --- Functions to act on Basecamp entities  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1"))
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

;; A set of methods to modify Basecamp entities.

;;; Code:

(require 'libbcel-client)
(require 'libbcel-structs)
(require 'libbcel-util)

(cl-defgeneric libbcel-actions-todo-toggle (entity &optional callback)
  "Toggle the completed state of ENTITY.
When finished, execute CALLBACK if non-nil.")

(cl-defmethod libbcel-actions-todo-toggle ((todo libbcel-todo) &optional callback)
  "Toggle the completed state of TODO.  When done, execute CALLBACK if non-nil."
  (if (libbcel-todo-completed todo)
      (libbcel-actions-todo-uncomplete todo callback)
    (libbcel-actions-todo-complete todo callback)))

(cl-defmethod libbcel-actions-todo-toggle ((todos list) &optional callback)
  "Toggle the completed state of all TODOS.  When done, execute CALLBACK if non-nil."
  (libbcel-util-async-mapc
   #'libbcel-actions-todo-toggle
   todos
   callback))

(defun libbcel-actions-todo-uncomplete (todo &optional callback)
  "Set TODO as uncompleted.  When finished, execute CALLBACK."
  (libbcel-client-delete-url
   (libbcel-todo-completion-url todo)
   (when callback (lambda (_data) (funcall callback)))))

(defun libbcel-actions-todo-complete (todo &optional callback)
  "Set TODO as completed.  When finished, execute CALLBACK."
  (libbcel-client-post-url
   (libbcel-todo-completion-url todo)
   (when callback (lambda (_data) (funcall callback)))))

(provide 'libbcel-actions)
;;; libbcel-actions.el ends here

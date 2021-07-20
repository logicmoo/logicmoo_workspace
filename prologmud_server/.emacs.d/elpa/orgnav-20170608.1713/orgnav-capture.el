;;; orgnav-capture.el --- Orgnav capture convenience functions -*- lexical-binding: t -*-

;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: github.com/facetframer/orgnav

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

(defvar orgnav-capture-depth 2 "The number of levels to show when refiling.")

(require 'orgnav)
(require 'org-capture)


(defun orgnav-capture-function-global ()
  "A function that can be used with `org-capture-template'.
A *file+function* or *function* capture point to capture to
a location selected using orgnav under the root node.
Here is an example entry:
        `(\"*\" \"Create a new entry\" entry
              (file+function \"test.org\" orgnav-capture-function-global) \"** Title\")'"
  (goto-char (orgnav-search-subtree-sync nil
                                      :depth orgnav-capture-depth
                                      :helm-buffer-name "*orgnav-capture*")))

(defun orgnav-capture-function-relative ()
  "A function that can be used with `org-capture-template'.
A *function* capture point to capture to a location under
the current node selected using orgnav.
Here is an example entry:
        `(\"*\" \"Create a new entry\" entry
               (function orgnav-capture-function-relative) \"** Title\")'"
  (goto-char (orgnav-search-subtree-sync
              (save-excursion
                (outline-back-to-heading 't)
                (point))
              :depth orgnav-capture-depth
              :helm-buffer-name "*orgnav-capture*")))

(defun orgnav-capture-function-ancestors ()
  "A function that can be used with `org-capture-template'.
A *function* capture point'to capture to a ancestor
of the current node.
Here is an example entry:
        (\"*\" \"Create a new entry\" entry (function orgnav-capture-function-ancestor) \"** Title\")"
  (goto-char (orgnav-search-ancestors-sync
              (save-excursion
                (outline-back-to-heading 't)
                (point))
              "*orgnav-capture*")))

(provide 'orgnav-capture)
;;; orgnav-capture.el ends here

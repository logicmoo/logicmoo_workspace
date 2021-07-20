;;; org-if-link.el --- org-if hyperlink -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file defines the custom org-if hyperlink.

;;; Code:

(require 'cl-macs)
(require 'org)
(require 'org-if-misc)

(defun org-if-open (link)
    "Open the org-if file and set the variables specified by LINK.
LINK should be in the form [[if:file/path.org(var1 val1 ...)][Choice text]]."
  (let* ((sexp-begin (string-match "(" link))
         (file       (substring link 0 sexp-begin))
         (sexp       (substring link sexp-begin))
         (vars       (read-from-string sexp)))
    (when (not (null (car vars)))
      (org-if-set-env (car vars)))
    (find-file file)))

(org-add-link-type "if" 'org-if-open)

(provide 'org-if-link)
;;; org-if-link.el ends here

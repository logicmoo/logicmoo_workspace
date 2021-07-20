;;; swagger-to-org.el --- Convert a swagger.json file into an org-mode file

;; Copyright (C) 2016  Matthew Carter

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/swagger-to-org
;; Package-Version: 0.0.2
;; Package-Commit: 181357c71ea24bede263f5706d8781ad65e16877
;; Version: 0.0.2
;; Keywords: ahungry emacs swagger openapi orgmode org export
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (json "1.4"))

;; This file is not part of GNU Emacs

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A parser/converter to read a swagger.json file, and output it in a nice
;; org-mode buffer/file, for easily exporting to additional formats.

;;; News:

;;;; Changes since 0.0.1:
;; - Added an autoload line for the interactive export function

;;;; Changes since 0.0.0:
;; - Created the project

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar swagger-to-org-headers-list
  (list
   "#+STARTUP: hideall indent hidestars hideblocks"
   )
  "Your custom headers for the output (latex headers etc.).
Add additional with (add-to-list 'swagger-to-org-headers-list \"#+LATEX_HEADER: whatever...\").")

(defvar swagger-to-org-style-list
  (list
   "h2 { background: gold; padding:20px;}"
   "h3 { background: yellow; padding: 8px; }"
   "pre { background: #333; color: #fff; font-family:bitstream; }"
   )
  "Your custom styles for the output (css styles etc.).
Add additional with (add-to-list 'swagger-to-org-style-list \"h1 { background: maroon; }\".")

(defun swagger-to-org-paths-to-org (fn)
  "Read out the paths and methods blocks from FN and print to an org buffer."
  (princ (format "\n* Paths\n"))
  (cl-map
   'nil
   (lambda (def)
     (princ (format "\n** Route %s\n" (car def)))
     (cl-map
      'nil
      (lambda (method)
        (princ (upcase (format "\n*** %s\n" (car method))))
        (princ (format "
#+BEGIN_SRC javascript
%s
#+END_SRC\n\n" (json-encode-list (list (cdr method)))))
        ) (cdr def)))
   (reverse (cdr (assoc 'paths
                        (json-read-file fn))))))

(defun swagger-to-org-definitions-to-org (fn)
  "Read out the definitions block from FN and print to an org buffer."
  (princ (format "\n* Definitions\n"))
  (cl-map
   'nil
   (lambda (def)
     (princ (format "\n** %s\nObject structure:\n" (car def)))
     (princ (format "
#+BEGIN_SRC javascript
%s
#+END_SRC\n\n" (json-encode-list (list (cdr def)))))
     )
   (reverse (cdr (assoc 'definitions
                        (json-read-file fn))))))

(defun swagger-to-org-from-file (fn)
  "Read the file FN and generate a new org buffer.
Make sure to save the buffer before exporting to an org export,
or you may have issues with the org export features."
  (let ((json-encoding-pretty-print t))
    (with-output-to-temp-buffer
        "*swagger-to-org*"

      ;; Add the special org/latex headers
      (cl-map nil (lambda (header)
                    (princ header)
                    (terpri))
              swagger-to-org-headers-list)

      ;; Add the css styles
      (princ (format "\n\n#+BEGIN_HTML\n"))
      (princ (format "\n<style type='text/css'>\n"))
      (cl-map nil (lambda (header)
                    (terpri)
                    (princ header))
              swagger-to-org-style-list)
      (princ (format "\n</style>\n"))
      (princ (format "\n#+END_HTML\n"))

      ;; Add the main swagger contents (path/methods and definitions)
      (swagger-to-org-paths-to-org fn)
      (swagger-to-org-definitions-to-org fn)
      ))
  (switch-to-buffer "*swagger-to-org*")
  (write-file "/tmp/swagger-to-org.org")
  (org-mode))

;;;###autoload
(defun swagger-to-org-from-file-name (fn)
  "Generate the org output from an input swagger file, given FN for the file name."
  (interactive "sPlease input the full path to the swagger.json file: ")
  (swagger-to-org-from-file fn))

(provide 'swagger-to-org)

;;; swagger-to-org.el ends here

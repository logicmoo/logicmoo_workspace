;;; notes-first.el --- Setup notes-mode before first use

;; Copyright (C) 2000-2006,2012  Free Software Foundation, Inc.

;; Author: <johnh@isi.edu>.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar notes-first-perl5-binary "perl"
  "Location of the perl binary to invoke notesinit (must be perl v5).")

(defun notes-first-use-init ()
  "Set up notes mode for the first time for a new user."
  ;; note that we CAN'T assume the contents of notes-variables is loaded.
  (if (y-or-n-p "Setup notes-mode with defaults? ")
      (notes-first-run-notes-init)
    (error (concat "Please run " notes-utility-dir "/notesinit by hand in a shell to customize defaults."))))

;; xxx: eventually we might do something more sophisticated here
;; (like asking the user questions directly).
(defun notes-first-run-notes-init ()
  "Run notesinit with defaults."
  (let*
      ((notes-init-cmd (expand-file-name "notesinit" notes-utility-dir))
       (process-environment
        (cons (concat "NOTES_BIN_DIR=" (directory-file-name notes-utility-dir))
              process-environment)))
    (message (concat "Running \"" notes-first-perl5-binary notes-init-cmd "\" to set up notes-mode."))
    (call-process notes-first-perl5-binary nil nil nil notes-init-cmd "-D"))
  ;; ok, things are setup, but we want to lead the user to what to do next
  ;; => start up on today's note
  (message "Notes are now set up.  Run M-x notes-index-todays-link to start."))

(provide 'notes-first)
;;; notes-first.el ends here

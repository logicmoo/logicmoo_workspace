;;; notes-xemacs.el --- XEmacs compatibility functions

;;; Copyright (C) 1998,2012  Free Software Foundation, Inc.

;; Author: <johnh@isi.edu>

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

;;; Code:

(defun notes-platform-bind-mouse (map generic-key fn)
  "Map from Emacs symbols to XEmacs for notes-mode (sigh)."
  (let
      ((xemacs-key
	(cond
	 ((eq generic-key 'mouse-2) [(button2)])
	 ((eq generic-key 'S-mouse-2) [(shift button2)]))))
    (define-key map xemacs-key fn)))

(defun notes-platform-init ()
  "Init platform-specific stuff for notes-mode."
  (if notes-platform-inited
      t
    (setq notes-platform-inited t)
    (make-face notes-bold-face)
    (if (not (face-differs-from-default-p notes-bold-face))
	    (copy-face 'bold notes-bold-face))

    ))

(provide 'notes-xemacs)
;;; notes-xemacs.el ends here

;;; epoch-view.el --- Minor mode to visualize epoch timestamps

;; Copyright (C) 2010, 2012  Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: data, timestamp, epoch, unix
;; Version: 0.0.1

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

;; Use like any other minor mode.  You'll see tooltips with dates
;; instead of Unix epoch times.  This mode turns on font-lock and
;; leaves it on forever.  You may or may not like that.

;;; TODO:

;; Instead of letting font-lock-mode manage the `display' property,
;; manage it ourselves so when multiple modes specify `display' it
;; won't get wiped out when this mode doesn't need it anymore.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User Variables:

(defcustom epoch-view-time-format "%F %T"
  "Format for time view.  Same as `format-time-string'."
  :type '(choice :tag "Time format"
                 (string :tag "Choose your own `format-time-string' format")
                 (const :tag "YYYY-MM-DD HH:MM:SS" "%F %T"))
  :group 'epoch-view)

(defvar epoch-view-font-lock-keywords
  '(("\\<[0-9]\\{8,11\\}\\>"
     (0 (epoch-view-render))))
  "Font-lock keywords of epoch timestamps.")

(defun epoch-view-render ()
  "Render a epoch match."
  (let ((text (match-string-no-properties 0)))
    `(face font-lock-warning-face
           display ,(epoch-view--render text))))

(defun epoch-view--render-time (text)
  "Render the time portion of an epoch match from TEXT."
  (format-time-string
   epoch-view-time-format
   (seconds-to-time (car (read-from-string (concat text ".0"))))))

(defun epoch-view--render (text)
  "Render a epoch match from a number in TEXT, ending with TEXT."
  (format "[%s] %s" (epoch-view--render-time text) text))

(defun epoch-view-turn-on ()
  "Turn on epoch-view-mode."
  (let ((props (make-local-variable 'font-lock-extra-managed-props)))
    (add-to-list props 'display))

  (font-lock-add-keywords nil epoch-view-font-lock-keywords))

(defun epoch-view-turn-off ()
  "Turn off epoch-view-mode."
  (font-lock-remove-keywords
   nil
   `(,@epoch-view-font-lock-keywords)))

;;;###autoload
(define-minor-mode
  epoch-view-mode
  "Visualize epoch (Unix) timestamps."
  :lighter " EpochVw"
  (progn
    (if epoch-view-mode
        (epoch-view-turn-on)
      (epoch-view-turn-off))
    ;; Turn on font lock
    (font-lock-mode 1)))

;;;; ChangeLog:

;; 2012-10-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Clean up copyright notices.
;; 
;; 2011-07-08  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	epoch-view.el, load-dir.el: Capitalize package description string.
;; 
;; 2011-07-01  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Reorganize repository layout, allowing site installation.
;; 	
;; 	A Makefile with "site", "archive" and "archive-full" rules can now be
;; 	used for site-installation, partial archive deployment, and full
;; 	archive deployment respectively.
;; 	
;; 	Rewrite the admin/archive-contents.el script to handle these changes.
;; 


(provide 'epoch-view)

(run-hooks 'epoch-view-load-hook)

;;; epoch-view.el ends here

;;; notes-aux.el --- Auxiliary functions for notes-mode and friends

;; Copyright (C) 1994,1995,1998,2012  Free Software Foundation, Inc.

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

;;; Code:

;;;###autoload
;;(defun notes-format-date (&optional calendar-date)
;;  "Format the calendar-date-style DATE up to be a notes-format date.
;;If no DATE is specified, use today's date."
;;  (require 'calendar)
;;  (let* ((date (if calendar-date
;;		   calendar-date
;;		 (calendar-current-date)))
;;	 (month (car date))
;;	 (day (nth 1 date))
;;	 (year (nth 2 date)))
;;    (format "%02d%02d%02d" (- year 1900) month day)))
(defun notes-format-date (&optional time)
  "Format the TIME up to be a notes-format date.
If no TIME is specified, use today's date."
  (require 'notes-variables)
  (if (null time)
      (setq time (current-time)))
  (format-time-string notes-file-form time))

(defun notes-file-to-epoch (file)
  "Convert a notes FILE to an epoch time."
  (string-match notes-file-regexp file)
  (let
      ((y (string-to-number (match-string 1 file)))
       (m (string-to-number (match-string 2 file)))
       (d (string-to-number (match-string 3 file))))
    (if (< y 1900)
	(setq y (+ y 1900)))
    (if (< y 1970)
	(setq y (+ y 100)))
    (encode-time 0 0 12 d m y)))

(defun notes-file-to-url (file &optional tag)
  "Convert a notes FILE to a URL with an optional TAG."
  (let
      ((epoch (notes-file-to-epoch file)))
    (concat
     notes-url-prefix
     (format-time-string notes-int-form epoch)
     "/"
     (format-time-string notes-file-form epoch)
     (if tag "#* " "")
     tag)))

(provide 'notes-aux)
;;; notes-aux.el ends here

;;; cpio-generic.el --- generically useful functions created in support of CPIO mode. -*- coding: utf-8 -*-

;; COPYRIGHT
;;
;; Copyright © 2019-2020 Free Software Foundation, Inc.
;; All rights reserved.
;;
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
;;

;; Author: Douglas Lewan <d.lewan2000@gmail.com>
;; Maintainer: Douglas Lewan <d.lewan2000@gmail.com>
;; Created: 2015 Apr 23
;; Version: 0.17
;; Keywords: files

;;; Commentary:

;;
;; This file contains useful generic functions,
;; and other temporarily useful hacks
;; to help with the development of cpio-mode.
;;
;; A quick glance through it suggests
;; that it has a lot of functional overlap with cpio-modes.el.
;;

;;; Documentation:

;;; Code:

;;
;; Dependencies
;;
(eval-and-compile
  (require 'cl))

(declare-function signum "cl")



;;
;; Vars
;;

(defvar *cg-integer-hex-digits* nil)

(defvar *cg-insert-after* nil
  "Value used to define that a marker has type 'insert after'.")
(defvar *cg-insert-before* t
  "Value used to define that a marker has type 'insert before'.")


;;
;; Library
;;

(defun cg-integer-hex-digits ()
  "Calculate the number of hex digits that are required to represent any integer."
  (let ((fname "cg-integer-hex-digits")
	(an-integer most-negative-fixnum)
	(hex-digit-ct 0))
    (unless *cg-integer-hex-digits*
	(while (/= 0 an-integer)
	  (setq an-integer (lsh an-integer -4))
	  (setq hex-digit-ct (1+ hex-digit-ct)))
	(setq *cg-integer-hex-digits* hex-digit-ct)))
  *cg-integer-hex-digits*)

(defun OBS-cg-hex-format-pair (pair)
  "Return a hex formatted representation of PAIR."
  (let ((fname "cg-hex-format-pair")
	(hex-digit-count (cg-integer-hex-digits))
	(formatter))
    (setq formatter (format "%%0%dx" hex-digit-count))
    (setq formatter (concat formatter formatter))
    (format formatter (car pair) (cdr pair))))

(defun OBS-cg-hex-format-triple (triple)
  "Return a hex formatted representation of TRIPLE."
  (let ((fname "cg-hex-format-triple")
	(hex-digit-count (cg-integer-hex-digits))
	(formatter))
    (setq formatter (format "%%0%dx" hex-digit-count))
    (setq formatter (concat formatter formatter formatter))
    (format formatter (car triple) (cadr triple) (cddr triple))))

(defun cg-round-up (number modulus)
  "Round NUMBER up to the next multiple of MODULUS.
If number ≡ 0 (modulus), then the NUMBER is already rounded up,
so NUMBER is returned.
CAVEAT: If NUMBER is negative, then the result may be surprising."
  (let ((fname "cg-round-up"))
    (unless (and (integerp number) (integerp modulus))
      (error "%s() takes integer arguments." fname))
    (cond ((= 0 (mod number modulus))
	   number)
	  ((= (signum number) 1)
	   (* modulus (/ (+ number modulus -1) modulus)))
	  ((= (signum number) -1)
	   (* modulus (/ number modulus)))
	  (t
	   (error "%s(): Impossible condition." fname)))))

(defun cg-pad-right (string width char)
  "Pad STRING on the right with CHAR until it is at least WIDTH characters wide.
CHAR is typically a character or a single character string, but may be any string."
  (let ((fname "cg-pad-right"))
    (if (characterp char) (setq char (char-to-string char)))
    (while (< (length string) width)
      (setq string (concat string char)))
    string))

(defun cg-strip-right (re string &optional multiples)
  "Strip the given RE from the right end of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "cg-strip-right")
	(inner-re (if multiples
		      (concat "\\(" re "\\)+\\'")
		    (concat re "\\'")))
	(result string))
    (save-match-data
      (if (string-match inner-re string)
	  (setq result (substring string 0 (match-beginning 0)))))
    result))

(defun cg-strip-left (re string &optional multiples)
  "Strip the given RE from the left end of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "cg-strip-left")
	(inner-re (if multiples
		      (concat "\\`\\(" re "\\)+")
		    (concat "\\`" re)))

	(result string))
    (save-match-data
      (if (string-match inner-re string)
	  (setq result (substring string (match-end 0)))))
    result))

(defun cg-strip (re string &optional multiples)
  "Remove the given RE from both ends of STRING.
If the optional argument MULTIPLES is not NIL,
then match as many copies of RE as are there."
  (let ((fname "strip")
	(result))
    (cg-strip-left re (cg-strip-right re string multiples) multiples)))

(defun cpio-pad (string modulus pad-char)
  "Pad the given STRING with PAD-CHAR so that the resulting string has at least length MODULUS."
  (let* ((fname "cpio-padded")
	 (string-length (length string))
	 (desired-length (cg-round-up string-length modulus)))
    (cg-pad-right string desired-length pad-char)))

(defun cpio-uid-for-owner (owner)
  "Return the uid (an integer) for the given OWNER (a string) if it exists.
If it doesn't exist, then return NIL.
If OWNER is a sequence of digits, then return OWNER as the GID.

CAVEAT: This deletes any buffer holding /etc/passwd."
  (let ((fname "cpio-uid-for-owner")
	(passwd-buffer (find-file-noselect "/etc/passwd"))
	(uid nil))
    (if (string-match-p "\\`[[:digit:]]+\\'" owner)
	(setq uid owner)
      (with-current-buffer passwd-buffer
	(goto-char (point-min))
	(save-match-data
	  (catch 'found-it
	    (while (< (point) (point-max))
	      (cond ((looking-at (concat owner ":[[:graph:]]+:\\([[:digit:]]+\\):[[:digit:]]+:"))
		     (setq uid (match-string-no-properties 1))
		     (throw 'found-it uid))
		    (t nil))
	      (forward-line))))))
    (kill-buffer passwd-buffer)
    (if uid
	(string-to-number uid)
      nil)))

(defun cpio-gid-for-group (group)
  "Return the GID (an integer) for the given GROUP (a string) if it exists.
If it doesn't exist, then return NIL.
If GROUP is a sequence of digits, then return GROUP as the GID.

CAVEAT: This deletes any buffer holding /etc/group."
  (let ((fname "cpio-gid-for-group")
	(group-buffer (find-file-noselect "/etc/group"))
	(gid nil))
    (cond ((null group)
	   nil)
	  ((stringp group)
	   (if (string-match-p "\\`[[:digit:]]+\\'" group)
	       (setq gid group)
	     (with-current-buffer group-buffer
	       (goto-char (point-min))
	       (save-match-data
		 (catch 'found-it
		   (while (< (point) (point-max))
		     (cond ((looking-at (concat group ":[[:graph:]]+:\\([[:digit:]]+\\):"))
			    (setq gid (match-string-no-properties 1))
			    (throw 'found-it gid))
			   (t nil))
		     (forward-line))))))
	   (kill-buffer group-buffer)
	   (if gid
	       (string-to-number gid)
	     nil))
	  (t nil))))

(defmacro with-writable-buffer (&rest body) ;FIXME: Namespace!
  "Run body with the current buffer writable.
Reset the buffer's read-only (or not) status after execution."
  `(let ((bro-before buffer-read-only))
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only bro-before)))

(defun encode-human-time (human-time)   ;FIXME: Namespace!
  "Return an emacs time from a HUMAN-TIME.
HUMAN-TIME may be any of many time formats typically used by humans.
If I've missed one, please let me know.
Star dates, really, star dates.
Besides that, with general relativity can we really be sure?
CAVEAT: This function attampts to handle multiple forms of dates in English.
Other languages are not yet implemented."
  (let ((fname "encode-human-time")
	(year nil)		  ;We'll use this to test for success.
	(month 0)
	(day 0)
	(hour 0)
	(minute 0)
	(second 0)
	(year-re (concat "\\("
			 "[[:digit:]]\\{4\\}"
			 "\\)"))
	(mon-re (concat "\\("
			"jan"
			"\\|"
			"feb"
			"\\|"
			"mar"
			"\\|"
			"apr"
			"\\|"
			"may"
			"\\|"
			"jun"
			"\\|"
			"jul"
			"\\|"
			"aug"
			"\\|"
			"sep"
			"\\|"
			"oct"
			"\\|"
			"nov"
			"\\|"
			"dec"
			"\\)"))
	(month-re (concat "\\("
			  "january"
			  "\\|"
			  "february"
			  "\\|"
			  "march"
			  "\\|"
			  "april"
			  "\\|"
			  "may"
			  "\\|"
			  "june"
			  "\\|"
			  "july"
			  "\\|"
			  "august"
			  "\\|"
			  "september"
			  "\\|"
			  "october"
			  "\\|"
			  "november"
			  "\\|"
			  "december"
			  "\\)"))
	(mm-re "\\(0?[[:digit:]]\\|1[012]\\)")
	(day-re "\\([012]?[[:digit:]]\\|3[01]\\)")
	(time-re (concat "\\("
			 "\\([012]?[[:digit:]]\\)"
			 ":"
			 "\\([012345][[:digit:]]\\)"
			 "\\("
			 ":"
			 "\\([0123456][[:digit:]]\\)"
			 "\\)?"
			 "\\)")))
    (save-match-data
      (cond
       ((string-match (concat "\\`"
			      year-re
			      "[-/ ]+"
			      month-re
			      "[-/ ]+"
			      day-re
			      "[- ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year   (string-to-number     (match-string-no-properties 1 human-time)))
	(setq month  (month-to-number      (match-string-no-properties 2 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 3 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       ((string-match (concat "\\`"
			      year-re
			      "[-/ ]+"
			      mon-re
			      "[-/ ]+"
			      day-re
			      "[- ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year   (string-to-number     (match-string-no-properties 1 human-time)))
	(setq month  (month-to-number      (match-string-no-properties 2 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 3 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       ((string-match (concat "\\`"
			      month-re
			      "[-/ ]+"
			      day-re
			      "[,]?\\s-+"
			      year-re
			      "[-/ ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year  (string-to-number      (match-string-no-properties 3 human-time)))
	(setq month (month-to-number       (match-string-no-properties 1 human-time)))
	(setq day   (string-to-number      (match-string-no-properties 2 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       ((string-match (concat "\\`"
			      mon-re
			      "[-/ ]+"
			      day-re
			      "[,]?\\s-*"
			      year-re
			      "[-/ ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year   (string-to-number     (match-string-no-properties 3 human-time)))
	(setq month  (month-to-number      (match-string-no-properties 1 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 2 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0")))

	(message "Format: [[Month dd, YYYY hh:mm:ss]]")
	"[[Month dd, YYYY hh:mm:ss]]")

       ;; Some date forms are ambiguous. Avoid them.
       ((or (and (string-match (concat "\\`"
				       year-re
				       "[-/ ]+"
				       mm-re
				       "[-/ ]+"
				       day-re
				       "[-/ ]*"
				       time-re
				       "?\\'")
			       human-time)
		 (string-match (concat "\\`"
				       year-re
				       "[-/ ]+"
				       day-re
				       "[-/ ]+"
				       mm-re
				       "[-/ ]*"
				       time-re
				       "?\\'")
			       human-time))
	    (and (string-match (concat "\\`"
				       mm-re
				       "[-/ ]+"
				       day-re
				       "[-/ ]+"
				       year-re
				       "[-/ ]*"
				       time-re
				       "?\\'")
			       human-time)
		 (string-match (concat "\\`"
				       day-re
				       "[-/ ]+"
				       mm-re
				       "[-/ ]+"
				       year-re
				       "[-/ ]*"
				       time-re
				       "?\\'")
			       human-time)))
	nil)
       ((string-match (concat "\\`"
			      year-re
			      "[-/ ]+"
			      mm-re
			      "[-/ ]+"
			      day-re
			      "[-/ ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year   (string-to-number     (match-string-no-properties 1 human-time)))
	(setq month  (string-to-number     (match-string-no-properties 2 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 3 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       ((string-match (concat "\\`"
			      mm-re
			      "[-/ ]+"
			      day-re
			      "[-/ ]+"
			      year-re
			      "[-/ ]*"
			      time-re
			      "?\\'")
		      human-time)
	(setq year   (string-to-number     (match-string-no-properties 3 human-time)))
	(setq month  (string-to-number     (match-string-no-properties 1 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 2 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       ((string-match (concat "\\`"
			      year-re
			      "[-/ ]+"
			      mm-re
			      "[-/ ]+"
			      day-re
			      "[-/ ]*"
			      time-re
			      "?\\'")
		      human-time)

	(setq year   (string-to-number     (match-string-no-properties 1 human-time)))
	(setq month  (string-to-number     (match-string-no-properties 2 human-time)))
	(setq day    (string-to-number     (match-string-no-properties 3 human-time)))

	(setq hour   (string-to-number (or (match-string-no-properties 5 human-time) "0")))
	(setq minute (string-to-number (or (match-string-no-properties 6 human-time) "0")))
	(setq second (string-to-number (or (match-string-no-properties 8 human-time) "0"))))

       (t (message "Unknown format.")
	  nil)))
    (if year
	(encode-time second minute hour day month year)
      nil)))

(defun month-to-number (month-name)     ;FIXME: Namespace!
  "Convert The MONTH-NAME to a number (1..12)."
  (let ((fname "month-to-number"))
    (save-match-data
      (cond ((string-match "jan" (substring month-name 0 3))
	     1)
	    ((string-match "feb" (substring month-name 0 3))
	     2)
	    ((string-match "mar" (substring month-name 0 3))
	     3)
	    ((string-match "apr" (substring month-name 0 3))
	     4)
	    ((string-match "may" (substring month-name 0 3))
	     5)
	    ((string-match "jun" (substring month-name 0 3))
	     6)
	    ((string-match "jul" (substring month-name 0 3))
	     7)
	    ((string-match "aug" (substring month-name 0 3))
	     8)
	    ((string-match "sep" (substring month-name 0 3))
	     9)
	    ((string-match "oct" (substring month-name 0 3))
	     10)
	    ((string-match "nov" (substring month-name 0 3))
	     11)
	    ((string-match "dec" (substring month-name 0 3))
	     12)
	    (t (error "%s(): Unknown month [[%s]]." fname month-name))))))

;; HEREHERE Remove this before publishing or
;; figure out how to put it in test-generic.el.
(defun test-encode-human-time ()        ;FIXME: Namespace!
  "Test (encode-human-time)."
  (interactive)
  (let ((fname "test-encode-human-time")
	(results-buf (get-buffer-create "*Human time results*"))
	(time-in)
	(emacs-time)
	(time-out)
	(format "")
	(test-dates (list
		     "2018 Nov 9"
		     "2018 Nov 9 9:53"
		     "2018 Nov 9 09:53"
		     "2018 Nov 9 9:53:23"
		     "2018 Nov 9 09:53:23"

		     "2018 Nov 09"
		     "2018 Nov 09 9:53"
		     "2018 Nov 09 09:53"
		     "2018 Nov 09 9:53:23"
		     "2018 Nov 09 09:53:23"

		     "2018 Nov 19"
		     "2018 Nov 19 9:53"
		     "2018 Nov 19 09:53"
		     "2018 Nov 19 9:53:23"
		     "2018 Nov 19 09:53:23"

		     "2018-Nov-29"
		     "2018-Nov-29 9:53"
		     "2018-Nov-29 09:53"
		     "2018-Nov-29 9:53:23"
		     "2018-Nov-29 09:53:23"

		     "2018/Nov/19"
		     "2018/Nov/19 9:53"
		     "2018/Nov/19 09:53"
		     "2018/Nov/19 9:53:23"
		     "2018/Nov/19 09:53:23"



		     "2018 November 9"
		     "2018 November 9 9:53"
		     "2018 November 9 09:53"
		     "2018 November 9 9:53:23"
		     "2018 November 9 09:53:23"

		     "2018 November 09"
		     "2018 November 09 9:53"
		     "2018 November 09 09:53"
		     "2018 November 09 9:53:23"
		     "2018 November 09 09:53:23"

		     "2018 November 19"
		     "2018 November 19 9:53"
		     "2018 November 19 09:53"
		     "2018 November 19 9:53:23"
		     "2018 November 19 09:53:23"

		     "2018-November-29"
		     "2018-November-29 9:53"
		     "2018-November-29 09:53"
		     "2018-November-29 9:53:23"
		     "2018-November-29 09:53:23"

		     "2018/November/19"
		     "2018/November/19 9:53"
		     "2018/November/19 09:53"
		     "2018/November/19 9:53:23"
		     "2018/November/19 09:53:23"




		     "11 09 2018"
		     "11 09 2018 9:53"
		     "11 09 2018 09:53"
		     "11 09 2018 9:53:23"
		     "11 09 2018 09:53:23"

		     "11 19 2018"
		     "11 19 2018 9:53"
		     "11 19 2018 09:53"
		     "11 19 2018 9:53:23"
		     "11 19 2018 09:53:23"

		     "11-29-2018"
		     "11-29-2018 9:53"
		     "11-29-2018 09:53"
		     "11-29-2018 9:53:23"
		     "11-29-2018 09:53:23"

		     "11/19/2018"
		     "11/19/2018 9:53"
		     "11/19/2018 09:53"
		     "11/19/2018 9:53:23"
		     "11/19/2018 09:53:23"

		     "2018 11 9"
		     "2018 11 9 9:53"
		     "2018 11 9 09:53"
		     "2018 11 9 9:53:23"
		     "2018 11 9 09:53:23"

		     "2018 11 09"
		     "2018 11 09 9:53"
		     "2018 11 09 09:53"
		     "2018 11 09 9:53:23"
		     "2018 11 09 09:53:23"

		     "2018 11 19"
		     "2018 11 19 9:53"
		     "2018 11 19 09:53"
		     "2018 11 19 9:53:23"
		     "2018 11 19 09:53:23"

		     "2018-11-29"
		     "2018-11-29 9:53"
		     "2018-11-29 09:53"
		     "2018-11-29 9:53:23"
		     "2018-11-29 09:53:23"

		     "2018/11/19"
		     "2018/11/19 9:53"
		     "2018/11/19 09:53"
		     "2018/11/19 9:53:23"
		     "2018/11/19 09:53:23"

		     "11 09 2018"
		     "11 09 2018 9:53"
		     "11 09 2018 09:53"
		     "11 09 2018 9:53:23"
		     "11 09 2018 09:53:23"

		     "11 19 2018"
		     "11 19 2018 9:53"
		     "11 19 2018 09:53"
		     "11 19 2018 9:53:23"
		     "11 19 2018 09:53:23"

		     "11-29-2018"
		     "11-29-2018 9:53"
		     "11-29-2018 09:53"
		     "11-29-2018 9:53:23"
		     "11-29-2018 09:53:23"

		     "11/19/2018"
		     "11/19/2018 9:53"
		     "11/19/2018 09:53"
		     "11/19/2018 9:53:23"
		     "11/19/2018 09:53:23")))
    (with-current-buffer results-buf (erase-buffer))

    (mapc (lambda (str)
	    (with-current-buffer results-buf
	      (setq time-in str)
	      (if (setq emacs-time (encode-human-time str))
		  (setq time-out (current-time-string emacs-time))
		(setq time-out (format "(encode-human-time \"%s\") returned NIL." str)))
	      (goto-char (point-max))
	      (insert (format "%s\t-->\t%s\t-->\t%s\n" time-in emacs-time time-out))))
	  test-dates)
    (pop-to-buffer results-buf)
    (goto-char (point-min))))


;;
;; Commands
;;


(provide 'cpio-generic)
;;; cpio-generic.el ends here

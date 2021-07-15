;;; cpio-bin.el --- handle bin cpio entry header formats -*- coding: utf-8 -*-

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
;; Created: 2015 Jan 03
;; Version: 0.17
;; Keywords: files

;;; Commentary:

;;; Documentation:

;;; Code:

;;
;; Dependencies
;;
(require 'bindat)
(eval-when-compile (require 'cpio-generic)) ;For `with-writable-buffer'!

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(declare-function cpio-entry-name "cpio-mode.el" (attrs))
(declare-function cpio-ino "cpio-mode.el" (attrs))
(declare-function cpio-mode-value "cpio-mode.el" (attrs))
(declare-function cpio-uid "cpio-mode.el" (attrs))
(declare-function cpio-gid "cpio-mode.el" (attrs))
(declare-function cpio-nlink "cpio-mode.el" (attrs))
(declare-function cpio-mtime "cpio-mode.el" (attrs))
(declare-function cpio-entry-size "cpio-mode.el" (attrs))
(declare-function cpio-dev-maj "cpio-mode.el" (attrs))
(declare-function cpio-rdev-maj "cpio-mode.el" (attrs))
(declare-function cpio-entry-attrs-from-catalog-entry "cpio-mode.el" (entry))
(declare-function cpio-contents-start "cpio-mode.el" (entry-name))
(declare-function cpio-entry-attrs "cpio-mode.el" (entry-name))
(defvar *cpio-catalog*)
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;;
;; Vars
;;

(defconst *cpio-bin-header-length* (length (string-as-unibyte "\307q\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"))
  "The length of a bin header.")

;; \307q\0\375c\9\244\201\350\3\350\3\1\0\0\0\377Z\320q\2\0\0\0\4\0a\0
;; \307q \0\375 c\9 \244\201 \350\3 \350\3 \1\0 \0\0 \377Z\320q \2\0 \0\0\4\0 a\0
(defconst *cpio-bin-magic-re* "\307q" ; 070707 \307q
  "RE to match the magic number of a bin archive.")
(setq *cpio-bin-magic-re* "\307q")

(defconst *cpio-bin-dev-re* "\\w\\w"
  "RE to match the c_dev field in a bin header.")
(setq *cpio-bin-dev-re* "\\w\\w")

(defconst *cpio-bin-ino-re* "\\w\\w"
  "RE to match the c_ino field in a bin header.")
(setq *cpio-bin-ino-re* "\\w\\w")

(defconst *cpio-bin-mode-re* "\\w\\w"
  "RE to match the c_mode field in a bin header.")
(setq *cpio-bin-mode-re* "\\w\\w")

(defconst *cpio-bin-uid-re* "\\w\\w"
  "RE to match the c_uid field in a bin header.")
(setq *cpio-bin-uid-re* "\\w\\w")

(defconst *cpio-bin-gid-re* "\\w\\w"
  "RE to match the c_gid field in a bin header.")
(setq *cpio-bin-gid-re* "\\w\\w")

(defconst *cpio-bin-nlink-re* "\\w\\w"
  "RE to match the c_nlink field in a bin header.")
(setq *cpio-bin-nlink-re* "\\w\\w")

(defconst *cpio-bin-rdev-re* "\\w\\w"
  "RE to match the c_rdev field in a bin header.")
(setq *cpio-bin-rdev-re* "\\w\\w")

(defconst *cpio-bin-mtime-re* "\\w\\w\\w\\w"
  "RE to match the c_mtime field in a bin header.")
(setq *cpio-bin-mtime-re* "\\w\\w\\w\\w")

(defconst *cpio-bin-namesize-re* "\\w\\w"
  "RE to match the c_rdev field in a bin header.")
(setq *cpio-bin-rdev-re* "\\w\\w")

(defconst *cpio-bin-filesize-re* "\\w\\w\\w\\w"
  "RE to match the c_filesize field in a bin header.")
(setq *cpio-bin-filesize-re* "\\w\\w\\w\\w")

(defconst *cpio-bin-filename-re* ()
  "RE to match a filename in a bin header.")
(setq *cpio-bin-filename-re* "[[:print:]]+")

(defconst *cpio-bin-header-re* ()
  "RE to match bin header format cpio archives.")
(setq *cpio-bin-header-re* (concat "\\(" *cpio-bin-magic-re*    "\\)"
				   "\\(" *cpio-bin-dev-re*      "\\)"
				   "\\(" *cpio-bin-ino-re*      "\\)"
				   "\\(" *cpio-bin-mode-re*     "\\)"
				   "\\(" *cpio-bin-uid-re*      "\\)"
				   "\\(" *cpio-bin-gid-re*      "\\)"
				   "\\(" *cpio-bin-nlink-re*    "\\)"
				   "\\(" *cpio-bin-rdev-re*     "\\)"
				   "\\(" *cpio-bin-mtime-re*    "\\)"
				   "\\(" *cpio-bin-namesize-re* "\\)"
				   "\\(" *cpio-bin-filesize-re* "\\)"
				   "\\(" *cpio-bin-filename-re* "\\)"
				   "\0"))

(defvar  *cpio-bin-name-field-offset* (length "\307q\0\375z\r\244\201\350\350\0\0\0[\211\255\0\0\0\0")
  "The offset of the name field in a cpio binary header.")

(defconst cpio-bin-index-spec
  '(;; (:magic	u16)
    (:dev	u16)
    (:ino	u16)
    (:mode	u16)
    (:uid	u16)
    (:gid	u16)
    (:nlink	u16)
    (:rdev	u16)
    (:mtime	u32)
    (:namesize	u16)
    (:filesize	u32)
    (:filename	strz (:namesize))))
(setq cpio-bin-index-spec
  '((:magic	u16r)
    (:dev	u16r)
    (:ino	u16r)
    (:mode	u16r)
    (:uid	u16r)
    (:gid	u16r)
    (:nlink	u16r)
    (:rdev	u16r)
    (:mtime0	u16r)
    (:mtime1	u16r)
    (:namesize	u16r)
    (:filesize0	u16r)
    (:filesize1	u16r)
    (:filename	strz (:namesize))))

(defconst *cpio-bin-magic* *cpio-bin-magic-re*
  "The string that identifies an entry as a BIN style cpio(1) entry.")
(setq *cpio-bin-magic* *cpio-bin-magic-re*)

(defconst *cpio-bin-magic-int* #o070707
  "An integer value of the cpio bin magic number.")

(defconst *cpio-bin-padding-modulus* 2
  "The modulus to which some things are padded in a BIN cpio archive.")
(setq *cpio-bin-padding-modulus* 2)

(defconst *cpio-bin-padding-char* ?\0
  "A character to be used for padding headers and entry contents
in a bin cpio archive.")
(setq *cpio-bin-padding-char* ?\0)

(defconst *cpio-bin-padding-str* "\0"
  "A single character string of the character
to be used for padding headers and entry contents
in a bin cpio archive.")
(setq *cpio-bin-padding-str* "\0")

(defconst *cpio-bin-trailer* (string-as-unibyte "\307q\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0TRAILER!!!\0\0")
  "The TRAILER string of a cpio binary archive.")
  
(defcustom *cpio-bin-blocksize* 512
  "The default blocksize for a cpio binary archive."
  :type 'integer
  :group 'cpio)


;;
;; Library
;;

(defun cpio-bin-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEATS:
1. This searches for the magic number at the begining of the header;
   if WHERE is inside the magic number, then the search will fail.
   This works best if you are (looking-at) a header.
2. This returns the pure header;
   it does not provide the filename itself."
  (unless where (setq where (point)))
  (let ((fname "cpio-bin-header-at-point")
	(found nil))
    (save-match-data
      (cond ((looking-at *cpio-bin-header-re*)
	     (string-as-unibyte (match-string-no-properties 0)))
	    (t
	     (forward-char (length *cpio-bin-magic-re*))
	     (while (and (re-search-backward *cpio-bin-magic-re* (point-min) t)
			 (not (setq found (looking-at *cpio-bin-header-re*)))))
	     (if found
		 (string-as-unibyte (match-string-no-properties 0))))))))

(defun cpio-bin-parse-header (header-string)
  "Return the internal entry header structure encoded in HEADER-STRING.
HEADER-STRING is a unibyte string.
The function does NOT get the contents of that entry."
  (let ((fname "cpio-bin-parse-header")
	(header-info)
	(mtime)
	(filesize)
	(result)
	(entry-name))
    (setq header-info (bindat-unpack cpio-bin-index-spec
				     header-string))
    (setq mtime (list (bindat-get-field header-info :mtime0)
		      (bindat-get-field header-info :mtime1)))
    (setq filesize (+ (* 256 256 (bindat-get-field header-info :filesize0))
		      (bindat-get-field header-info :filesize1)))
    (cond ((string-equal (setq entry-name (bindat-get-field header-info :filename))
			 "TRAILER!!!")
	   nil)
	  (t
	   (setq result
		 (vector (bindat-get-field header-info :ino)
			 (bindat-get-field header-info :mode)
			 (bindat-get-field header-info :uid)
			 (bindat-get-field header-info :gid)

			 (bindat-get-field header-info :nlink)
			 mtime
			 filesize
			 (bindat-get-field header-info :dev)

			 0				;dev min
			 (bindat-get-field header-info :rdev)
			 0				;rdev min
			 (bindat-get-field header-info :namesize)

			 0				;checksum
		    entry-name))
	   (if (cpio-entry-name result)
	       result
	     nil)))))

(defun cpio-bin-header-size (header-string namesize)
  "Determine the length of the header implied by the given HEADER-STRING."
  (let ((fname "cpio-bin-header-size")
	;; CAUTION: The following assumes that (string-to-number) doesn't care about leading zeroes.
	;; The namesize in the header includes the terminating NULL at the end of the name.
	(local-namesize (1- namesize))
	(total -1))
    (if (= 0 (mod (setq total (+ 1 *cpio-bin-name-field-offset* local-namesize))
		  *cpio-bin-padding-modulus*))
	(setq total (1+ total)))
    (cg-round-up total *cpio-bin-padding-modulus*)))

;;;;;;;;;;;;;;;;
;;
;; Header construction
;;

(defun cpio-bin-make-header-string (attrs &optional contents)
  "Make a BIN style padded cpio header for the given ATTRibuteS.
This function does NOT include the contents."
  (let ((fname "cpio-bin-make-header-string")
	(name (cpio-entry-name attrs))
	(header-string)
	(padding)
	(mtime (cpio-bin-make-mtime attrs))
	(filesize (cpio-bin-make-filesize attrs)))
    (setq header-string
	  (bindat-pack cpio-bin-index-spec
		       (list
			(cons :magic     (cpio-bin-make-magic    attrs))
			(cons :dev       (cpio-bin-make-dev-maj  attrs))
			(cons :ino       (cpio-bin-make-ino      attrs))
			(cons :mode      (cpio-bin-make-mode     attrs))
			(cons :uid       (cpio-bin-make-uid      attrs))
			(cons :gid       (cpio-bin-make-gid      attrs))
			(cons :nlink     (cpio-bin-make-nlink    attrs))
			(cons :rdev      (cpio-bin-make-rdev-maj attrs))
			(cons :mtime0    (car  mtime))
			(cons :mtime1    (cdr  mtime))
			(cons :namesize  (1+ (length name)))
			(cons :filesize0 (car  filesize))
			(cons :filesize1 (cdr  filesize))
			(cons :filename  (concat name "\0")))))
    (setq header-string (cg-pad-right header-string (cg-round-up (length header-string)
								 *cpio-bin-padding-modulus*)
				   "\0"))))

(defun cpio-bin-make-magic (attrs)
  "Return the BIN magic header string"
  (let ((fname "cpio-bin-make-magic"))
    *cpio-bin-magic-int*))

(defun cpio-bin-make-ino (attrs)
  "Return a string value for the inode from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-ino"))
    (cpio-ino attrs)))

(defun cpio-bin-make-mode (attrs)
  "Return a string value for the mode from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-mode"))
    (cpio-mode-value attrs)))

(defun cpio-bin-make-uid (attrs)
  "Return an integer string value for the UID from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-uid"))
    (cpio-uid attrs)))

(defun cpio-bin-make-gid (attrs)
  "Return an integer string value for the GID from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-gid"))
    (cpio-gid attrs)))

(defun cpio-bin-make-nlink (attrs)
  "Return an integer string value for the number of links from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-nlink"))
    (cpio-nlink attrs)))

(defun cpio-bin-make-mtime (attrs)
  "Return a string value for the mod time from the file attributes ATTRS."
  (let* ((fname "cpio-bin-make-mtime")
	 (mod-time (cpio-mtime attrs))
	 (high-time (car mod-time))
	 (low-time (cadr mod-time)))
    (cons high-time low-time)))

(defun cpio-bin-make-filesize (attrs)
  "Return an 8 digit hex string for the filesize attribute among the given ATTRs."
  (let ((fname "cpio-bin-make-filesize")
	(filesize (cpio-entry-size attrs)))
    (cons (lsh (logand #xFFFF0000 filesize) 8)
	       (logand #xFFFF filesize))))

(defun cpio-bin-make-dev-maj (attrs)
  "Return a string value for the major device from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-dev-maj"))
    (cpio-dev-maj attrs)))

(defun cpio-bin-make-dev-min (attrs)
  "Return a string value for the minor device from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-dev-min"))
    0))

(defun cpio-bin-make-rdev-maj (attrs)
  "Return a string value for the major rdev from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-rdev-maj"))
    (cpio-rdev-maj attrs)))

(defun cpio-bin-make-rdev-min (attrs)
  "Return a string value for the minor rdev from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-rdev-min"))
    0))

(defun cpio-bin-make-chksum (attrs)
  "Return a string value for the bin cpio entry from the file attributes ATTRS."
  (let ((fname "cpio-bin-make-chksum"))
    0))

;; Filename is not one of ATTRS. ∴ It doesn't get a constructor here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for whole entries
;;
(defun cpio-bin-parse-header-at-point ()
  "Parse the bin cpio header that begins at point.
If there is no header there, then signal an error."
  (let ((fname "cpio-bin-parse-header-at-point"))
    (unless (looking-at-p *cpio-bin-header-re*)
      (error "%s(): point is not looking at a bin header." fname))
    (cpio-bin-parse-header (string-as-unibyte (match-string-no-properties 0)))))

(defun cpio-bin-goto-next-header ()
  "Move the point to the beginning of the next bin cpio header.
If point is looking-at such a header, then that is the next one
and there is no movement.
\(Thus, a caller may have to make sure that point has moved.\)
This returns the a marker for point where the header is found, if one is found.
It returns NIL otherwise.
This sets match-data for the entire header and each field."
  (let ((fname "cpio-bin-goto-next-header")
	(header-start)
	(header-string))
    (cond ((re-search-forward *cpio-bin-header-re* (point-max) t)
	   (setq header-start (goto-char (match-beginning 0)))
	   (setq header-string (match-string-no-properties 0))
	   (cons (point-marker) header-string))
	  (t nil))))

(defun cpio-bin-build-catalog ()
  "Build an internal structure reflecting the contents of the bin cpio archive in the current buffer.
See the variable *cpio-catalog* for more information.
CAVEAT: This respects neither narrowing nor the point."
  (let ((fname "cpio-bin-build-catalog")
	(header-start)			;A marker.
	(header-end)
	(that-header-string)
	(header-info ())
	(parsed-header t)
	(filesize)			;A marker.
	(contents-start)
	(contents-end)			;NOT NEEDED?
	(those-contents)		;
	(catalog ()))
    (widen)
    (goto-char (point-min))
    (while (and (setq header-info (cpio-bin-goto-next-header))
		(setq header-start (car header-info))
		(setq that-header-string (cdr header-info))
		parsed-header)
      (cond ((setq parsed-header (cpio-bin-parse-header-at-point))
	     (setq filesize (cpio-entry-size parsed-header))
	     (forward-char (length that-header-string))
	     (setq header-end (point))
	     ;; A little bit of arithmétic gymnastics here
	     ;; because cpio, being written in C, starts counting at 0, but
	     ;; emacs' points start at 1.
	     (goto-char (1+ (cg-round-up (1- header-end) *cpio-bin-padding-modulus*)))
	     (setq contents-start (point-marker))
	     (set-marker-insertion-type contents-start *cg-insert-after*)
	     ;; It feels like I really want a function for getting the contents.
	     ;; But it's not obvious what is simpler or appropriately more general
	     ;; than this one-liner.
	     ;; Indeed. (setq those-contents (buffer-substring-no-properties contents-start contents-end))
	     (push (cons  (cpio-entry-name parsed-header)
			  (vector
			   parsed-header
			   header-start
			   contents-start
			   'cpio-mode-entry-unmodified))
		   catalog)
	     (setq contents-end (+ contents-start filesize -1))
	     (goto-char contents-end))
	    (t t)))
    (nreverse catalog)))

(defun cpio-bin-start-of-trailer ()
  "Return the character position of the (ostensible) start of the trailer
for the current cpio archive."
  (let ((fname "cpio-bin-start-of-trailer")
	(end-of-contents 0))
    (mapc (lambda (ce)
	    (let ((attrs (cpio-entry-attrs-from-catalog-entry ce)))
	      (setq end-of-contents (+ (cpio-entry-size attrs) (cpio-contents-start ce)))))
	  *cpio-catalog*)
    end-of-contents))

(defun cpio-bin-end-of-archive ()
  "Calculate the location of the end of the current archive
once the TRAILER is written and padded."
  (let ((fname "cpio-bin-end-of-archive")
	(end-of-contents (cpio-bin-start-of-trailer)))
    (cg-round-up (+ end-of-contents (length *cpio-bin-trailer*)) 512)))

(defun cpio-bin-adjust-trailer ()
  "Replace thed current trailer in the current cpio bin archive."
  (let* ((fname "cpio-bin-adjust-trailer"))
    (cpio-bin-delete-trailer)
    (cpio-bin-insert-trailer)))

(defun cpio-bin-insert-trailer ()
  "Insert a bin trailer into a cpio archive."
  (let* ((fname "cpio-bin-insert-trailer")
	 (base-trailer *cpio-bin-trailer*)
	 (base-len (length base-trailer))
	 (len))
    ;; ...and insert the new trailer...
    (with-writable-buffer
     (insert base-trailer)
     (goto-char (point-max))
     ;; ...with padding.
     (setq len (cg-round-up (1- (point)) *cpio-bin-blocksize*))
     (setq len (1+ (- len (point))))
     (insert (make-string len ?\0)))))

(defun cpio-bin-delete-trailer ()
  "Delete the trailer in the current cpio bin archive."
  (let ((fname "cpio-bin-delete-trailer"))
    (unless (eq major-mode 'cpio-mode)
      (error "%s(): Called outside of a cpio archive buffer." fname))
    ;; First, get to the end of the last entry in the archive.
    (goto-char (point-min))
    (mapc (lambda (e)
	    (let* ((ename (car e))	;Isn't there a generic function for this?
		   (attrs (cpio-entry-attrs ename))
		   ;; Fencepost issue here.
		   (entry-end (+ (cpio-contents-start ename)
				 (cpio-entry-size attrs))))
	      (goto-char entry-end)
	      (skip-chars-forward "\0")))
	  *cpio-catalog*)
    ;; Next, delete what's left...
    (with-writable-buffer
     (delete-region (point) (point-max)))))

(defun cpio-bin-make-chcksum-for-file (filename)
  "Return the checksum for FILENAME."
  (let ((fname "cpio-newc-make-chcksum-for-file"))
    0))

;;
;; Commands
;;


(provide 'cpio-bin)
;;; cpio-bin.el ends here.

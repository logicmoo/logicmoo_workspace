;;; cpio-odc.el --- handle old portable cpio entry header format. -*- coding: utf-8 -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
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

;; (load-file (concat default-directory "cpio-generic.el"))

(eval-when-compile (require 'cpio-generic)) ;For `with-writable-buffer'!

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(defvar *cpio-catalog*)
(defvar *cpio-odc-dev-field-offset*)
(defvar *cpio-odc-filesize-field-offset*)
(defvar *cpio-odc-gid-field-offset*)
(defvar *cpio-odc-ino-field-offset*)
(defvar *cpio-odc-magic-field-offset*)
(defvar *cpio-odc-mode-field-offset*)
(defvar *cpio-odc-mtime-field-offset*)
(defvar *cpio-odc-name-field-offset*)
(defvar *cpio-odc-namesize-field-offset*)
(defvar *cpio-odc-nlink-field-offset*)
(defvar *cpio-odc-rdev-field-offset*)
(defvar *cpio-odc-uid-field-offset*)
(declare-function cpio-contents-start "cpio-mode.el")
(declare-function cpio-dev-maj "cpio-mode.el")
(declare-function cpio-entry-attrs-from-catalog-entry "cpio-mode.el")
(declare-function cpio-entry-name "cpio-mode.el")
(declare-function cpio-entry-size "cpio-mode.el")
(declare-function cpio-gid "cpio-mode.el")
(declare-function cpio-ino "cpio-mode.el")
(declare-function cpio-mode-value "cpio-mode.el")
(declare-function cpio-mtime "cpio-mode.el")
(declare-function cpio-nlink "cpio-mode.el")
(declare-function cpio-rdev-maj "cpio-mode.el")
(declare-function cpio-uid "cpio-mode.el")
(declare-function cpio-entry-attrs "cpio-mode.el")
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;;
;; Vars
;;

(defconst *cpio-odc-header-length* (length "0707070000000000000000000000000000000000010000000000000000000001300000000000")
  "The length of an odc header.")

;; MAINTENANCE The following must remain in synch with *cpio-odc-header-re*.
;;     magic	070707		\\(070707\\)
;;     dev	176400		\\([0-7]\\{6\\}\\)
;;     ino	005341		\\([0-7]\\{6\\}\\)
;;     mode	100644		\\([0-7]\\{6\\}\\)
;;     uid	001750		\\([0-7]\\{6\\}\\)
;;     gid	001750		\\([0-7]\\{6\\}\\)
;;     nlink	000001		\\([0-7]\\{6\\}\\)
;;     rdev	000000		\\([0-7]\\{6\\}\\)
;;     mtime	13300045411	\\([0-7]\\{11\\}\\)
;;     namesz	000002		\\([0-7]\\{6\\}\\)
;;     filesize	00000000004	\\([0-7]\\{11\\}\\)
;;     name	a\0		\\([[:print:]]+\\)\0

(defconst *cpio-odc-magic-re* "070707"
  "RE to match the magic number of a odc archive.")
(setq *cpio-odc-magic-re* "070707")

(defconst *cpio-odc-field-width* 6
  "The width of all of the fields in a odc header.")
(setq *cpio-odc-field-width* 6)

(defconst *cpio-odc-ino-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_ino field in a odc header.")
(setq *cpio-odc-ino-re*		 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-dev-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_dev field in a odc header.")
(setq *cpio-odc-dev-re*		 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-mode-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_mode field in a odc header.")
(setq *cpio-odc-mode-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-uid-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_uid field in a odc header.")
(setq *cpio-odc-uid-re*		 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-gid-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_gid field in a odc header.")
(setq *cpio-odc-gid-re*		 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-nlink-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_nlink field in a odc header.")
(setq *cpio-odc-nlink-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-rdev-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_rdev field in a odc header.")
(setq *cpio-odc-rdev-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-mtime-re*		 "[0-7]\\{11\\}"
  "RE to match the c_mtime field in a odc header.")
(setq *cpio-odc-mtime-re*		 "[0-7]\\{11\\}")

(defconst *cpio-odc-namesize-re* (format "[0-7]\\{%d\\}" *cpio-odc-field-width*)
  "RE to match the c_namesize field in a odc header.")
(setq *cpio-odc-namesize-re*	 (format "[0-7]\\{%d\\}" *cpio-odc-field-width*))

(defconst *cpio-odc-filesize-re*	 "[0-7]\\{11\\}"
  "RE to match the c_filesize field in a odc header.")
(setq *cpio-odc-filesize-re*		 "[0-7]\\{11\\}")

(defconst *cpio-odc-filename-re* "[[:print:]]+"
  "RE to match the c_filename field in a odc header.")
(setq *cpio-odc-filename-re* "[[:print:]]+")

(defconst *cpio-odc-header-re* ()
  "RE to match odc header format cpio archives.")
(setq *cpio-odc-header-re* (concat "\\(" *cpio-odc-magic-re*	"\\)"
				   "\\(" *cpio-odc-dev-re*	"\\)"
				   "\\(" *cpio-odc-ino-re*	"\\)"
				   "\\(" *cpio-odc-mode-re*	"\\)"

				   "\\(" *cpio-odc-uid-re*	"\\)"
				   "\\(" *cpio-odc-gid-re*	"\\)"
				   "\\(" *cpio-odc-nlink-re*	"\\)"
				   "\\(" *cpio-odc-rdev-re*	"\\)"

				   "\\(" *cpio-odc-mtime-re*	"\\)"
				   "\\(" *cpio-odc-namesize-re* "\\)"
				   "\\(" *cpio-odc-filesize-re* "\\)"
				   "\\(" *cpio-odc-filename-re* "\\)"
				   "\0"))

(let ((i 0))
  (defconst *cpio-odc-magic-re-idx* 0
    "RE to match the magic number in a odc header.")
  (setq *cpio-odc-magic-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-dev-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the dev.")
  (setq *cpio-odc-dev-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-ino-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the inode.")
  (setq *cpio-odc-ino-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-mode-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the mode.")
  (setq *cpio-odc-mode-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-uid-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the UID.")
  (setq *cpio-odc-uid-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-gid-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the GID.")
  (setq *cpio-odc-gid-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-nlink-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the nlink.")
  (setq *cpio-odc-nlink-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-rdev-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the rdev.")
  (setq *cpio-odc-rdev-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-mtime-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the mtime.")
  (setq *cpio-odc-mtime-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-namesize-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the namesize.")
  (setq *cpio-odc-namesize-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-filesize-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the filesize.")
  (setq *cpio-odc-filesize-re-idx* (setq i (1+ i)))

  (defconst *cpio-odc-filename-re-idx* 0
    "Index of the sub RE from *cpio-odc-header-re* to parse the filename.")
  (setq *cpio-odc-filename-re-idx* (setq i (1+ i))))
;;
;; EO odc header variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; *cpio-odc-magic-re*
(defconst *cpio-odc-magic* *cpio-odc-magic-re*
  "The string that identifies an entry as a ODC style cpio(1) entry.")
(setq *cpio-odc-magic* *cpio-odc-magic-re*)

(defconst *cpio-odc-field-width* 6
  "The width of all of the fields in a odc header.")
(setq *cpio-odc-field-width* 6)

(defconst *cpio-odc-padding-modulus* 2
  "The modulus to which some things are padded in a ODC cpio archive.")
(setq *cpio-odc-padding-modulus* 2)

(defconst *cpio-odc-padding-char* ?\0
  "A character to be used for padding headers and entry contents
in a odc cpio archive.")
(setq *cpio-odc-padding-char* ?\0)

(defconst *cpio-odc-padding-str* "\0"
  "A single character string of the character
to be used for padding headers and entry contents
in a odc cpio archive.")
(setq *cpio-odc-padding-str* "\0")

(let ((offset-so-far 0))
  (defconst *cpio-odc-magic-field-offset* offset-so-far)
  (setq *cpio-odc-magic-field-offset* offset-so-far)

  (defconst *cpio-odc-dev-field-offset*	     ())
  (setq *cpio-odc-dev-field-offset*	     (setq offset-so-far (+ offset-so-far (length *cpio-odc-magic*))))

  (defconst *cpio-odc-ino-field-offset*	     ())
  (setq *cpio-odc-ino-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-mode-field-offset*     ())
  (setq *cpio-odc-mode-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-uid-field-offset*	     ())
  (setq *cpio-odc-uid-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-gid-field-offset*	     ())
  (setq *cpio-odc-gid-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-nlink-field-offset*    ())
  (setq *cpio-odc-nlink-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-rdev-field-offset*     ())
  (setq *cpio-odc-rdev-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-mtime-field-offset*    ())
  (setq *cpio-odc-mtime-field-offset*	     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-namesize-field-offset* ())
  (setq *cpio-odc-namesize-field-offset*     (setq offset-so-far (+ offset-so-far 11)))

  (defconst *cpio-odc-filesize-field-offset* ())
  (setq *cpio-odc-filesize-field-offset*     (setq offset-so-far (+ offset-so-far *cpio-odc-field-width*)))

  (defconst *cpio-odc-name-field-offset*     ())
  (setq *cpio-odc-name-field-offset*	     (setq offset-so-far (+ offset-so-far 11))))

(defconst *cpio-odc-trailer* "0707070000000000000000000000000000000000010000000000000000000001300000000000TRAILER!!!\0"
  "The TRAILER string for a odc archive.")
(setq *cpio-odc-trailer* "0707070000000000000000000000000000000000010000000000000000000001300000000000TRAILER!!!\0")


(defcustom *cpio-odc-blocksize* 512
  "The default block size for this cpio archive.
Taken from cpio-2.12/src/global.c."
  :type 'integer
  :group 'cpio)

;;
;; Library
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for working with a cpio odc header
;;

(defun cpio-odc-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEATS:
1. This searches for the magic number at the begining of the header;
   if WHERE is inside the magic number, then the search will fail.
   This works best if you are (looking-at) a header.
2. This returns the pure header;
   it does not provide the filename itself."
  (unless where (setq where (point)))
  (let ((fname "cpio-odc-header-at-point")
	(found nil))
    (save-match-data
      (cond ((looking-at *cpio-odc-header-re*)
	     (match-string-no-properties 0))
	    (t
	     (forward-char (length *cpio-odc-magic-re*))
	     (while (and (re-search-backward *cpio-odc-magic-re* (point-min) t)
			 (not (setq found (looking-at *cpio-odc-header-re*)))))
	     (if found
		 (match-string-no-properties 0)))))))

;;;;;;;;;;;;;;;;
;;
;; Parsing a header
;;

(defun cpio-odc-parse-header (header-string)
  "Return the internal entry header structure encoded in HEADER-STR.
The optional argument WHERE should be a buffer location
at the beginning of a known cpio odc header.
If WHERE is not given, then take point and hope.
This function does NOT get the contents."
  (let ((fname "cpio-odc-parse-header")
	(namesize)
	(filesize)
	(result))
    ;; There's an arguable level of redundancy here,
    ;; but the caller likely grabbed HEADER-STR
    ;; from the buffer and we're using the string proper.
    ;; This call establishes the match-data
    ;; that the subsequent calls will use.
    (save-match-data
      (string-match *cpio-odc-header-re* header-string)
      (setq result
	    (vector
		    (cpio-odc-parse-ino	     header-string)
		    (cpio-odc-parse-mode     header-string)
		    (cpio-odc-parse-uid	     header-string)
		    (cpio-odc-parse-gid	     header-string)
		    (cpio-odc-parse-nlink    header-string)
		    (cpio-odc-parse-mtime    header-string)
		    (setq filesize (cpio-odc-parse-filesize header-string))
		    (cpio-odc-parse-dev	     header-string)
		    0			;dev-min
		    (cpio-odc-parse-rdev     header-string)
		    0			;rdev-min
		    (setq namesize (cpio-odc-parse-namesize header-string))
		    0			;checksum
		    (cpio-odc-parse-name     header-string namesize))))
    (if (cpio-entry-name result)
	result
      nil)))

(defun cpio-odc-header-size (header-string namesize)
  "Determine the length of the header implied by the given HEADER-STRING."
  (let ((fname "cpio-odc-header-size"))
    (+ *cpio-odc-name-field-offset* namesize)))

(defun cpio-odc-parse-magic (header-string)
  "Get the magic field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-magic")
	 (this-offset *cpio-odc-magic-field-offset*)
	 (end-offset (+ this-offset (length *cpio-odc-magic-re*))))
    (substring header-string this-offset end-offset)))

(defun cpio-odc-parse-ino (header-string)
  "Get the ino field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-ino")
	 (this-offset *cpio-odc-ino-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-mode (header-string)
  "Get the mode field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-mode")
	 (this-offset *cpio-odc-mode-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-uid (header-string)
  "Get the uid field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-uid")
	 (this-offset *cpio-odc-uid-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-gid (header-string)
  "Get the gid field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-gid")
	 (this-offset *cpio-odc-gid-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-nlink (header-string)
  "Get the nlink field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-nlink")
	 (this-offset *cpio-odc-nlink-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-mtime (header-string)
  "Get the mtime field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-mtime")
	 (this-offset *cpio-odc-mtime-field-offset*)
	 (end-offset (+ this-offset 11))
	 (time-value ()))
    (setq time-value (string-to-number (substring header-string this-offset end-offset) 8))
    (setq time-value (list (lsh (logand #xFFFF0000 time-value) -16) (logand #xFFFF)))))

(defun cpio-odc-parse-filesize (header-string)
  "Get the filesize from the HEADER-STRING."
  (let* ((fname "cpio-odc-parse-filesize")
	 (this-offset *cpio-odc-filesize-field-offset*)
	 (end-offset (+ this-offset 11)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-dev (header-string)
  "Get the dev field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-dev")
	 (this-offset *cpio-odc-dev-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-rdev (header-string)
  "Get the rdev field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-rdev")
	 (this-offset *cpio-odc-rdev-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-namesize (header-string)
  "Get the namesize field from HEADER-STRING."
  (let* ((fname "cpio-odc-parse-namesize")
	 (this-offset *cpio-odc-namesize-field-offset*)
	 (end-offset (+ this-offset *cpio-odc-field-width*)))
    (string-to-number (substring header-string this-offset end-offset) 8)))

(defun cpio-odc-parse-name (header-string namesize)
  "Get the name field from HEADER-STRING.
N.B. When called with the correct namesize, this includes the terminating \0."
  (let* ((fname "cpio-odc-parse-name")
	 (this-offset *cpio-odc-name-field-offset*)
	 (tmp-string (substring header-string this-offset (+ this-offset namesize -1))))
    (if (string-equal tmp-string "TRAILER!!!")
	nil
      tmp-string)))

;; Is this not M-x cpio-dired-find-entry?
(defun cpio-odc-parse-contents (header-string where namesize filesize)
  "Return the contents implied by point and HEADER-STRING.
CAVEATS: See `cpio-odc-parse-magic'.
This requires the point to be at the start of HEADER-STRING in the buffer.
After all that's where the contents are, not in the header."
  (let ((fname "cpio-odc-parse-contents"))
    (buffer-substring-no-properties (+ where namesize)
				    (+ where namesize filesize))))

;;;;;;;;;;;;;;;;
;;
;; Header construction
;;

(defun cpio-odc-make-header-string (attrs &optional contents)
  "Make a ODC style padded cpio header for the given ATTRibuteS.
This function does NOT include the contents."
  (let ((fname "cpio-odc-make-header-string")
	(name (cpio-entry-name attrs))
	(header-string))
    (setq header-string (concat	 (cpio-odc-make-magic	 attrs)
				 (cpio-odc-make-dev	 attrs)
				 (cpio-odc-make-ino	 attrs)
				 (cpio-odc-make-mode	 attrs)
				 (cpio-odc-make-uid	 attrs)
				 (cpio-odc-make-gid	 attrs)
				 (cpio-odc-make-nlink	 attrs)
				 (cpio-odc-make-rdev	 attrs)
				 (cpio-odc-make-mtime	 attrs)
				 (format "%06o" (1+ (length name)))
				 (cpio-odc-make-filesize attrs)
				 name
				 "\0"))
    ;; (setq header-string (cg-pad-right header-string (cg-round-up (length header-string) *cpio-odc-padding-modulus*) "\0"))
    ;; Check (at least during development).
    (if (string-match-p *cpio-odc-header-re* header-string)
	header-string
      (error "%s(): I built a bad header: [[%s]]" fname header-string))))

(defun cpio-odc-make-magic (attrs)
  "Return the ODC magic header string"
  (let ((fname "cpio-odc-make-magic"))
    *cpio-odc-magic*))

(defun cpio-odc-make-ino (attrs)
  "Return a string value for the inode from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-ino")
	(ino (cpio-ino attrs)))
    (format "%06o" ino)))

(defun cpio-odc-make-mode (attrs)
  "Return a string value for the mode from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-mode"))
    (format "%06o" (cpio-mode-value attrs))))

(defun cpio-odc-make-uid (attrs)
  "Return an integer string value for the UID from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-uid")
	(uid (cpio-uid attrs)))
    (format "%06o" uid)))

(defun cpio-odc-make-gid (attrs)
  "Return an integer string value for the GID from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-gid")
	(gid (cpio-gid attrs)))
    (format "%06o" gid)))

(defun cpio-odc-make-nlink (attrs)
  "Return an integer string value for the number of links from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-nlink"))
    (format "%06o" (cpio-nlink attrs))))

(defun cpio-odc-make-mtime (attrs)
  "Return a string value for the mod time from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-mtime")
	(mod-time (cpio-mtime attrs)))
    (substring (format "%011o" (float-time mod-time)) 0 11)))

(defun cpio-odc-make-filesize (attrs)
  "Return an 8 digit hex string for the filesize attribute among the given ATTRs."
  (let ((fname "cpio-odc-make-filesize"))
    (format "%011o" (cpio-entry-size attrs))))

(defun cpio-odc-make-dev (attrs)
  "Return a string value for the dev from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-dev")
	(dev (cpio-dev-maj attrs)))
    (format "%06o" dev)))

(defun cpio-odc-make-rdev (attrs)
  "Return a string value for the rdev from the file attributes ATTRS."
  (let ((fname "cpio-odc-make-rdev")
	(rdev))
    (format "%06o" (cpio-rdev-maj attrs))))

;; Filename is not one of ATTRS. ∴ It doesn't get a constructor here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for whole entries
;;
(defun cpio-odc-parse-header-at-point ()
  "Parse the odc cpio header that begins at point.
If there is no header there, then signal an error."
  (let ((fname "cpio-odc-parse-header-at-point"))
    (unless (looking-at-p *cpio-odc-header-re*)
      (error "%s(): point is not looking at a odc header." fname))
    (cpio-odc-parse-header (match-string-no-properties 0))))

(defun cpio-odc-goto-next-header ()
  "Move the point to the beginning of the next odc cpio header.
If point is looking-at such a header, then that is the next one
and there is no movement.
\(Thus, a caller may have to make sure that point has moved.\)
This returns the a marker for point where the header is found, if one is found.
It returns NIL otherwise.
This sets match-data for the entire header and each field."
  (let ((fname "cpio-odc-goto-next-header")
	(header-start)
	(header-string))
    (cond ((re-search-forward *cpio-odc-header-re* (point-max) t)
	   (setq header-start (goto-char (match-beginning 0)))
	   (setq header-string (match-string-no-properties 0))
	   (cons (point-marker) header-string))
	  (t nil))))

(defun cpio-odc-build-catalog ()
  "Build an internal structure reflecting the contents of the odc cpio archive in the current buffer.
See the variable *cpio-catalog* for more information.
CAVEAT: This respects neither narrowing nor the point."
  (let ((fname "cpio-odc-build-catalog")
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
    (while (and (setq header-info (cpio-odc-goto-next-header))
		(setq header-start (car header-info))
		(setq that-header-string (cdr header-info))
		parsed-header)
      (cond ((setq parsed-header (cpio-odc-parse-header-at-point))
	     (setq filesize (cpio-entry-size parsed-header))
	     (forward-char (length that-header-string))
	     (setq header-end (point))
	     ;; A little bit of arithmétic gymnastics here
	     ;; because cpio, being written in C, starts counting at 0, but
	     ;; emacs' points start at 1.
	     (goto-char header-end)
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

(defun cpio-odc-start-of-trailer ()
  "Return the character position of the (ostensible) start of the trailer
for the current cpio archive."
  (let ((fname "cpio-odc-start-of-trailer")
	(end-of-contents 0))
    (mapc (lambda (ce)
	    (let ((attrs (cpio-entry-attrs-from-catalog-entry ce)))
	      (setq end-of-contents (+ (cpio-entry-size attrs) (cpio-contents-start ce)))))
	  *cpio-catalog*)
    end-of-contents))

(defun cpio-odc-end-of-archive ()
  "Calculate the location of the end of the current archive
once the TRAILER is written and padded."
  (let ((fname "cpio-odc-end-of-archive")
	(end-of-contents (cpio-odc-start-of-trailer)))
    (cg-round-up (+ end-of-contents (length *cpio-odc-trailer*)) *cpio-odc-blocksize*)))

(defun cpio-odc-adjust-trailer ()
  "Replace thed current trailer in the current cpio odc archive."
  (let ((fname "cpio-odc-adjust-trailer"))
    (cpio-odc-delete-trailer)
    (cpio-odc-insert-trailer)))

(defun cpio-odc-insert-trailer ()
  "Insert a odc trailer into a cpio archive."
  (let* ((fname "cpio-odc-insert-trailer")
	 (base-trailer *cpio-odc-trailer*)
	 (base-len (length base-trailer))
	 (len))
    ;; ...and insert the new trailer...
    (with-writable-buffer
     (insert base-trailer)
     (goto-char (point-max))
     ;; ...with padding.
     (setq len (cg-round-up (1- (point)) *cpio-odc-blocksize*))
     (setq len (1+ (- len (point))))
     (insert (make-string len ?\0)))))

(defun cpio-odc-delete-trailer ()
  "Delete the trailer in the current cpio odc archive."
  (let ((fname "cpio-odc-delete-trailer"))
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


;;
;; Commands
;;


(provide 'cpio-odc)
;;; cpio-odc.el ends here.

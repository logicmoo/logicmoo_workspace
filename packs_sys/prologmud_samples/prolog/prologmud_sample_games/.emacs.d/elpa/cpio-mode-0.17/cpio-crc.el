;;; cpio-crc.el --- handle crc cpio entry header formats -*- coding: utf-8 -*-

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

;; (eval-when-compile
;;   (condition-case err
;;       (require 'cpio-generic)
;;     (error
;;      (if (file-exists-p (concat default-directory "cpio-generic.elc"))
;;	 (load (concat default-directory "cpio-generic.elc"))
;;        (load (concat default-directory "cpio-generic.el")))))
;;   (condition-case err
;;       (require 'cpio-newc)
;;     (error
;;      (if (file-exists-p (concat default-directory "cpio-newc.elc"))
;;	 (load (concat default-directory "cpio-newc.elc"))
;;        (load (concat default-directory "cpio-newc.el"))))))

(require 'cpio-newc)
(eval-when-compile (require 'cpio-generic)) ;For `with-writable-buffer'!

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(declare-function cg-pad-right "cpio-generic.el")
(declare-function cg-round-up "cpio-generic.el")
(declare-function cpio-contents "cpio-mode.el" (entry-name &optional archive-buffer))
(declare-function cpio-entry-exists-p "cpio-mode.el" (entry-name))
(declare-function cpio-entry-name "cpio-mode.el" (attrs))
(declare-function cpio-entry-size "cpio-mode.el" (attrs))
(declare-function cpio-newc-parse-chksum "cpio-newc.el")
(declare-function cpio-newc-parse-dev-maj "cpio-newc.el")
(declare-function cpio-newc-parse-dev-min "cpio-newc.el")
(declare-function cpio-newc-parse-filesize "cpio-newc.el")
(declare-function cpio-newc-parse-gid "cpio-newc.el")
(declare-function cpio-newc-parse-ino "cpio-newc.el")
(declare-function cpio-newc-parse-mode "cpio-newc.el")
(declare-function cpio-newc-parse-mtime "cpio-newc.el")
(declare-function cpio-newc-parse-name "cpio-newc.el")
(declare-function cpio-newc-parse-namesize "cpio-newc.el")
(declare-function cpio-newc-parse-nlink "cpio-newc.el")
(declare-function cpio-newc-parse-rdev-maj "cpio-newc.el")
(declare-function cpio-newc-parse-rdev-min "cpio-newc.el")
(declare-function cpio-newc-parse-uid "cpio-newc.el")
(declare-function cpio-special-file "cpio-modes.el")
(declare-function cpio-validate-catalog-entry "cpio-mode.el" (catalog-entry))
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;;
;; Vars
;;

(defconst *cpio-crc-header-length* (length "07070100000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000B00000000")
  "The length of a crc header.")


;; MAINTENANCE The following must remain in synch with *cpio-newc-header-re*.
(defconst *cpio-crc-magic-re* "070702"
  "RE to match the magic number of a newc archive.")
(setq *cpio-crc-magic-re* "070702")

(defconst *cpio-crc-ino-re*      *cpio-newc-ino-re*)
(defconst *cpio-crc-mode-re*     *cpio-newc-mode-re*)
(defconst *cpio-crc-uid-re*      *cpio-newc-uid-re*)
(defconst *cpio-crc-gid-re*      *cpio-newc-gid-re*)
(defconst *cpio-crc-nlink-re*    *cpio-newc-nlink-re*)
(defconst *cpio-crc-mtime-re*    *cpio-newc-mtime-re*)
(defconst *cpio-crc-filesize-re* *cpio-newc-filesize-re*)
(defconst *cpio-crc-dev-maj-re*  *cpio-newc-dev-maj-re*)
(defconst *cpio-crc-dev-min-re*  *cpio-newc-dev-min-re*)
(defconst *cpio-crc-rdev-maj-re* *cpio-newc-rdev-maj-re*)
(defconst *cpio-crc-rdev-min-re* *cpio-newc-rdev-min-re*)
(defconst *cpio-crc-rdev-min-re* *cpio-newc-rdev-min-re*)
(defconst *cpio-crc-namesize-re* *cpio-newc-namesize-re*)
(defconst *cpio-crc-chksum-re*   *cpio-newc-chksum-re*)
(defconst *cpio-crc-filename-re* *cpio-newc-filename-re*)
(defconst *cpio-crc-header-re* ()
  "RE to match crc header format cpio archives.")
(setq *cpio-crc-header-re* (concat "\\(" *cpio-crc-magic-re*    "\\)"
				   "\\(" *cpio-crc-ino-re*      "\\)"
				   "\\(" *cpio-crc-mode-re*     "\\)"
				   "\\(" *cpio-crc-uid-re*      "\\)"
				   "\\(" *cpio-crc-gid-re*      "\\)"

				   "\\(" *cpio-crc-nlink-re*    "\\)"
				   "\\(" *cpio-crc-mtime-re*    "\\)"
				   "\\(" *cpio-crc-filesize-re* "\\)"
				   "\\(" *cpio-crc-dev-maj-re*  "\\)"
				   "\\(" *cpio-crc-dev-min-re*  "\\)"

				   "\\(" *cpio-crc-rdev-maj-re* "\\)"
				   "\\(" *cpio-crc-rdev-min-re* "\\)"
				   "\\(" *cpio-crc-namesize-re* "\\)"
				   "\\(" *cpio-crc-chksum-re*   "\\)"
				   "\\(" *cpio-crc-filename-re* "\\)"
				   "\0"))

(defconst *cpio-crc-magic-re-idx*    *cpio-newc-magic-re-idx*)
(defconst *cpio-crc-ino-re-idx*      *cpio-newc-ino-re-idx*)
(defconst *cpio-crc-mode-re-idx*     *cpio-newc-mode-re-idx*)
(defconst *cpio-crc-uid-re-idx*      *cpio-newc-uid-re-idx*)
(defconst *cpio-crc-gid-re-idx*      *cpio-newc-gid-re-idx*)
(defconst *cpio-crc-nlink-re-idx*    *cpio-newc-nlink-re-idx*)
(defconst *cpio-crc-mtime-re-idx*    *cpio-newc-mtime-re-idx*)
(defconst *cpio-crc-filesize-re-idx* *cpio-newc-filesize-re-idx*)
(defconst *cpio-crc-dev-maj-re-idx*  *cpio-newc-dev-maj-re-idx*)
(defconst *cpio-crc-dev-min-re-idx*  *cpio-newc-dev-min-re-idx*)
(defconst *cpio-crc-rdev-maj-re-idx* *cpio-newc-rdev-maj-re-idx*)
(defconst *cpio-crc-rdev-min-re-idx* *cpio-newc-rdev-min-re-idx*)
(defconst *cpio-crc-namesize-re-idx* *cpio-newc-namesize-re-idx*)
(defconst *cpio-crc-chksum-re-idx*   *cpio-newc-chksum-re-idx*)
(defconst *cpio-crc-filename-re-idx* *cpio-newc-filename-re-idx*)

;;
;; EO newc header variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *cpio-crc-field-width*     *cpio-newc-field-width*)
(defconst *cpio-crc-padding-modulus* *cpio-newc-padding-modulus*)
(defconst *cpio-crc-padding-char*    *cpio-newc-padding-char*)
(defconst *cpio-crc-padding-str*     *cpio-newc-padding-str*)

(defconst *cpio-crc-magic-field-offset*    *cpio-newc-magic-field-offset*)
(defconst *cpio-crc-ino-field-offset*      *cpio-newc-ino-field-offset*)
(defconst *cpio-crc-mode-field-offset*     *cpio-newc-mode-field-offset*)
(defconst *cpio-crc-uid-field-offset*      *cpio-newc-uid-field-offset*)
(defconst *cpio-crc-gid-field-offset*      *cpio-newc-gid-field-offset*)
(defconst *cpio-crc-nlink-field-offset*    *cpio-newc-nlink-field-offset*)
(defconst *cpio-crc-mtime-field-offset*    *cpio-newc-mtime-field-offset*)
(defconst *cpio-crc-filesize-field-offset* *cpio-newc-filesize-field-offset*)
(defconst *cpio-crc-dev-maj-field-offset*  *cpio-newc-dev-maj-field-offset*)
(defconst *cpio-crc-dev-min-field-offset*  *cpio-newc-dev-min-field-offset*)
(defconst *cpio-crc-rdev-maj-field-offset* *cpio-newc-rdev-maj-field-offset*)
(defconst *cpio-crc-rdev-min-field-offset* *cpio-newc-rdev-min-field-offset*)
(defconst *cpio-crc-namesize-field-offset* *cpio-newc-namesize-field-offset*)
(defconst *cpio-crc-chksum-field-offset*   *cpio-newc-chksum-field-offset*)
(defconst *cpio-crc-name-field-offset*     *cpio-newc-name-field-offset*)


(defconst *cpio-crc-trailer* "07070200000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000B00000000TRAILER!!!\0\0\0\0"
  "The TRAILER string for a newc archive.")

(defcustom *cpio-crc-blocksize* *cpio-newc-blocksize*
  "The default block size for this cpio archive.
Taken from cpio-2.12/src/global.c."
  :type 'integer
  :group 'cpio)


;;
;; Library
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for working with a cpio newc header
;;

(defun cpio-newc-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEATS:
1. This searches for the magic number at the begining of the header;
   if WHERE is inside the magic number, then the search will fail.
   This works best if you are (looking-at) a header.
2. This returns the pure header;
   it does not provide the filename itself."
  (unless where (setq where (point)))
  (let ((fname "cpio-newc-header-at-point")
	(found nil))
    (save-match-data
      (cond ((looking-at *cpio-newc-header-re*)
	     (match-string-no-properties 0))
	    (t
	     (forward-char (length *cpio-newc-magic-re*))
	     (while (and (re-search-backward *cpio-newc-magic-re* (point-min) t)
			 (not (setq found (looking-at *cpio-newc-header-re*)))))
	     (if found
		 (match-string-no-properties 0)))))))
;; OBSOLETE (setq cpio-header-at-point-func 'cpio-newc-header-at-point)

;;;;;;;;;;;;;;;;
;;
;; Parsing a header
;;

(defalias 'cpio-crc-header-size    'cpio-newc-header-size)
(defalias 'cpio-crc-parse-magic    'cpio-newc-parse-magic)
(defalias 'cpio-crc-parse-ino      'cpio-newc-parse-ino)
(defalias 'cpio-crc-parse-mode     'cpio-newc-parse-mode)
(defalias 'cpio-crc-parse-uid      'cpio-newc-parse-uid)
(defalias 'cpio-crc-parse-gid      'cpio-newc-parse-gid)
(defalias 'cpio-crc-parse-nlink    'cpio-newc-parse-nlink)
(defalias 'cpio-crc-parse-mtime    'cpio-newc-parse-mtime)
(defalias 'cpio-crc-parse-filesize 'cpio-newc-parse-filesize)
(defalias 'cpio-crc-parse-dev-maj  'cpio-newc-parse-dev-maj)
(defalias 'cpio-crc-parse-dev-min  'cpio-newc-parse-dev-min)
(defalias 'cpio-crc-parse-rdev-maj 'cpio-newc-parse-rdev-maj)
(defalias 'cpio-crc-parse-rdev-min 'cpio-newc-parse-rdev-min)
(defalias 'cpio-crc-parse-namesize 'cpio-newc-parse-namesize)
(defalias 'cpio-crc-parse-chksum   'cpio-newc-parse-chksum)
(defalias 'cpio-crc-parse-name     'cpio-newc-parse-name)
(defalias 'cpio-crc-parse-chksum   'cpio-newc-parse-chksum)
(defalias 'cpio-crc-parse-contents 'cpio-newc-parse-contents)

(defun cpio-crc-parse-header (header-string)
  "Return the internal entry header structure encoded in HEADER-STR.
The optional argument WHERE should be a buffer location
at the beginning of a known cpio newc header.
If WHERE is not given, then take point and hope.
This function does NOT get the contents."
  (let ((fname "cpio-newc-parse-header")
	(namesize)
	(filesize)
	(result))
    ;; There's an arguable level of redundancy here,
    ;; but the caller likely grabbed HEADER-STR
    ;; from the buffer and we're using the string proper.
    ;; This call establishes the match-data
    ;; that the subsequent calls will use.
    (save-match-data
      (string-match *cpio-newc-header-re* header-string)
      (setq result
	    (vector (cpio-newc-parse-ino      header-string)
		    (cpio-newc-parse-mode     header-string)
		    (cpio-newc-parse-uid      header-string)
		    (cpio-newc-parse-gid      header-string)
		    (cpio-newc-parse-nlink    header-string)
		    (cpio-newc-parse-mtime    header-string)
		    (setq filesize (cpio-newc-parse-filesize header-string))
		    (cpio-newc-parse-dev-maj  header-string)
		    (cpio-newc-parse-dev-min  header-string)
		    (cpio-newc-parse-rdev-maj header-string)
		    (cpio-newc-parse-rdev-min header-string)
		    (setq namesize (cpio-newc-parse-namesize header-string))
		    (cpio-newc-parse-chksum   header-string)
		    (cpio-newc-parse-name     header-string namesize))))
    (if (cpio-entry-name result)
	result
      nil)))

(defun cpio-crc-make-header-string (attrs &optional contents)
  "Make a header string for a CRC archive based on ATTRS.
This function does NOT include the contents."
  (let ((fname "cpio-crc-make-header-string")
	(name (cpio-entry-name attrs))
	(header-string))
    (setq header-string (concat  (cpio-crc-make-magic    attrs)
				 (cpio-crc-make-ino      attrs)
				 (cpio-crc-make-mode     attrs)
				 (cpio-crc-make-uid      attrs)
				 (cpio-crc-make-gid      attrs)
				 (cpio-crc-make-nlink    attrs)
				 (cpio-crc-make-mtime    attrs)
				 (cpio-crc-make-filesize attrs)
				 (cpio-crc-make-dev-maj  attrs)
				 (cpio-crc-make-dev-min  attrs)
				 (cpio-crc-make-rdev-maj attrs)
				 (cpio-crc-make-rdev-min attrs)
				 (format "%08X" (1+ (length name)))
				 (format "%08X"
					 (if (cpio-special-file attrs) ;See cpio-modes.el
					     0
					   (cpio-crc-make-chksum  (if contents
								      contents
								    name))))
				 name
				 "\0"))
    (setq header-string (cg-pad-right header-string (cg-round-up (length header-string) *cpio-crc-padding-modulus*) "\0"))
    ;; Check (at least during development).
    (if (string-match-p *cpio-crc-header-re* header-string)
	header-string
      (error "%s(): I built a bad header: [[%s]]" fname header-string))))

(defun cpio-crc-make-magic (attrs)
  "Return the magic string for a CRC archive."
  *cpio-crc-magic-re*)
(defalias 'cpio-crc-make-ino            'cpio-newc-make-ino)
(defalias 'cpio-crc-make-mode           'cpio-newc-make-mode)
(defalias 'cpio-crc-make-uid            'cpio-newc-make-uid)
(defalias 'cpio-crc-make-gid            'cpio-newc-make-gid)
(defalias 'cpio-crc-make-nlink          'cpio-newc-make-nlink)
(defalias 'cpio-crc-make-mtime          'cpio-newc-make-mtime)
(defalias 'cpio-crc-make-filesize       'cpio-newc-make-filesize)
(defalias 'cpio-crc-make-dev-maj        'cpio-newc-make-dev-maj)
(defalias 'cpio-crc-make-dev-min        'cpio-newc-make-dev-min)
(defalias 'cpio-crc-make-rdev-maj       'cpio-newc-make-rdev-maj)
(defalias 'cpio-crc-make-rdev-min       'cpio-newc-make-rdev-min)

(defun cpio-crc-make-chksum (entry-name-or-contents)
  "Return a string value for the newc cpio entry from the file attributes ATTRS."
  (let ((fname "cpio-crc-make-chksum")
	(result 0)
	(contents (if (cpio-entry-exists-p entry-name-or-contents)
		      (cpio-contents entry-name-or-contents)
		    entry-name-or-contents)))
    ;; According to the info this is only populated for crc archives.
    ;; It has always been 00000000 for my concrete newc examples.
    ;; And, indeed, it's only set in crc archives.
    ;; See copyout.c->writeout-defered-file() and nowhere else.
    (mapc (lambda (c)
	    (setq result (+ result c)))
	  contents)
    result))

;; Filename is not one of ATTRS. ∴ It doesn't get a constructor here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for whole entries
;;
(defun cpio-crc-parse-header-at-point ()
  "Parse the crc cpio header that begins at point.
If there is no header there, then signal an error."
  (let ((fname "cpio-crc-parse-header-at-point"))
    (unless (looking-at-p *cpio-crc-header-re*)
      (error "%s(): point is not looking at a crc header." fname))
    (cpio-crc-parse-header (match-string-no-properties 0))))

(defun cpio-crc-goto-next-header ()
  "Move the point to the beginning of the next crc cpio header.
If point is looking-at such a header, then that is the next one
and there is no movement.
\(Thus, a caller may have to make sure that point has moved.\)
This returns the a marker for point where the header is found, if one is found.
It returns NIL otherwise.
This sets match-data for the entire header and each field."
  (let ((fname "cpio-crc-goto-next-header")
	(header-start)
	(header-string))
    (cond ((re-search-forward *cpio-crc-header-re* (point-max) t)
	   (setq header-start (goto-char (match-beginning 0)))
	   (setq header-string (match-string-no-properties 0))
	   (cons (point-marker) header-string))
	  (t nil))))

(defun cpio-crc-build-catalog ()
  "Build an internal structure reflecting the contents of the crc cpio archive in the current buffer.
See the variable *cpio-catalog* for more information.
CAVEAT: This respects neither narrowing nor the point."
  (let ((fname "cpio-crc-build-catalog")
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
    (while (and (setq header-info (cpio-crc-goto-next-header))
		(setq header-start (car header-info))
		(setq that-header-string (cdr header-info))
		parsed-header)
      (cond ((setq parsed-header (cpio-crc-parse-header-at-point))
	     (setq filesize (cpio-entry-size parsed-header))
	     (forward-char (length that-header-string))
	     (setq header-end (point))
	     ;; A little bit of arithmetic gymnastics here
	     ;; because cpio, being written in C, starts counting at 0, but
	     ;; emacs' points start at 1.
	     (goto-char (1+ (cg-round-up (1- header-end) *cpio-crc-padding-modulus*)))
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
    (mapc (lambda (ce)
	    (cpio-validate-catalog-entry (cdr ce)))
	  catalog)
    (nreverse catalog)))

(defalias 'cpio-crc-start-of-trailer 'cpio-newc-start-of-trailer)

(defalias 'cpio-crc-end-of-archive 'cpio-newc-end-of-archive)

(defun cpio-crc-adjust-trailer ()
  "Replace thed current trailer in the current cpio crc archive."
  (let ((fname "cpio-crc-adjust-trailer"))
    (cpio-crc-delete-trailer)
    (cpio-crc-insert-trailer)))

(defun cpio-crc-insert-trailer ()
  "Insert a crc trailer into a cpio archive."
  (let* ((fname "cpio-crc-insert-trailer")
	 (base-trailer *cpio-crc-trailer*)
	 (base-len (length base-trailer))
	 (len))
    ;; ...and insert the new trailer...
    (with-writable-buffer
     (insert base-trailer)
     (goto-char (point-max))
     ;; ...with padding.
     (setq len (cg-round-up (1- (point)) *cpio-crc-blocksize*))
     (setq len (1+ (- len (point))))
     (insert (make-string len ?\0)))))

(defalias 'cpio-crc-delete-trailer 'cpio-newc-delete-trailer)

(defun cpio-crc-make-chksum-for-file (filename)
  "Return the checksum for FILENAME."
  (let ((fname "cpio-newc-make-chksum-for-file"))
    (with-temp-buffer
      (insert-file-contents filename)
      (cpio-crc-make-chksum (buffer-substring-no-properties (point-min) (point-max))))))


;;
;; Commands
;;


(provide 'cpio-crc)
;;; cpio-crc.el ends here.

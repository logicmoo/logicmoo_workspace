;;; cpio-modes.el --- handle file modes/permissions. -*- coding: utf-8 -*-

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

;; Author: Douglas Lewan <d.lewan2000@gmail.com>
;; Maintainer: Douglas Lewan <d.lewan2000@gmail.com>
;; Created: 2017 Nov 28
;; Version: 0.17
;; Keywords: files

;;; Commentary:

;; This file contains code for dealing with mode bits in cpio-mode.

;;; Documentation:

;;; Code:

;;
;; Dependencies
;;
(eval-when-compile
  (require 'cl-lib)
  (require 'cl-extra))

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(declare-function cpio-mode-value "cpio-mode.el")
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;;
;; Vars
;;
;;
;; Mode-related bits (adapted from /usr/include/linux/stat.h).
;;

(defconst s-ifunk  #o1000000)
(defconst s-ifmt   #o0170000)
(defconst s-ifsock #o0140000)
(defconst s-iflnk  #o0120000)
(defconst s-ifreg  #o0100000)
(defconst s-ifblk  #o0060000)
(defconst s-ifdir  #o0040000)
(defconst s-ifchr  #o0020000)
(defconst s-ififo  #o0010000)
(defconst s-isuid  #o0004000)
(defconst s-isgid  #o0002000)
(defconst s-isvtx  #o0001000)

(defconst s-irwxu #o00700)
(defconst s-irusr #o00400)
(defconst s-iwusr #o00200)
(defconst s-ixusr #o00100)

(defconst s-irwxg #o00070)
(defconst s-irgrp #o00040)
(defconst s-iwgrp #o00020)
(defconst s-ixgrp #o00010)

(defconst s-irwxo #o00007)
(defconst s-iroth #o00004)
(defconst s-iwoth #o00002)
(defconst s-ixoth #o00001)

(defconst UNUSED-*cpio-low-mode-bits* (logior s-irwxu s-irwxg s-irwxo s-isuid s-isgid s-isvtx)
  "A bit mask of the modes that can be set by chmod(1).")


;;
;; Library
;;

(defun s-islnk (m)
  (= (logand m s-ifmt) s-iflnk))
(defun s-isreg (m)
  (= (logand m s-ifmt) s-ifreg))
(defun s-isdir (m)
  (= (logand m s-ifmt) s-ifdir))
(defun s-ischr (m)
  (= (logand m s-ifmt) s-ifchr))
(defun s-isblk (m)
  (= (logand m s-ifmt) s-ifblk))
(defun s-isfifo (m)
  (= (logand m s-ifmt) s-ififo))
(defun s-issock (m)
  (= (logand m s-ifmt) s-ifsock))

(defun cpio-special-file (attrs)
  "Return non-NIL if the mode in ATTRS is as special file:
fmt, sock, link, block, character, fifo."
  (let ((fname "cpio-special-file")
	(mode (cpio-mode-value attrs)))
    (or (= s-ifmt   (logand s-ifmt   mode))
	(= s-ifsock (logand s-ifsock mode))
	(= s-iflnk  (logand s-iflnk  mode))	;Does this really belong here? I'm writing this to support (cpio-crc-make-chksum). Do links' checksums get calculated?
	(= s-ifblk  (logand s-ifblk  mode))
	(= s-ifdir  (logand s-ifdir  mode)) ;Is a directory a special file? Again, this has to do with calculating a checksum.
	(= s-ifchr  (logand s-ifchr  mode))
	(= s-ififo  (logand s-ififo  mode)))))

(defun cpio-int-mode-to-mode-string (int-mode)
  "Convert an integer mode value to the corresponding ls -l version."
  (let ((fname "cpio-int-mode-to-mode-string")
	(file-type  (cpio-int-mode-to-file-type         int-mode))
	(user-mode  (cpio-int-mode-to-user-permissions  int-mode))
	(group-mode (cpio-int-mode-to-group-permissions int-mode))
	(other-mode (cpio-int-mode-to-other-permissions int-mode)))
    (concat file-type user-mode group-mode other-mode)))

(defvar *cpio-modes-link*    "l")
(setq *cpio-modes-link* "l")

(defvar *cpio-modes-reg*     "-")
(setq *cpio-modes-reg* "-")

(defvar *cpio-modes-dir*     "d")
(setq *cpio-modes-dir* "d")

(defvar *cpio-modes-char*    "c")
(setq *cpio-modes-char* "c")

(defvar *cpio-modes-block*   "b")
(setq *cpio-modes-block* "b")

(defvar *cpio-modes-fifo*    "p")
(setq *cpio-modes-fifo* "p")

(defvar *cpio-modes-sock*    "s")
(setq *cpio-modes-sock* "s")

(defvar *cpio-modes-unknown* "?")
(setq *cpio-modes-unknown* "?")


(defun cpio-int-mode-to-file-type (int-mode)
  "Extract the one character string that expresses the file type from INT-MODE.
CAUTION: Some file types are not present here:
    D -- Solaris door
    M -- Cray DMF migrated file
    n -- HP-UX network special file
    P -- Solaris port.
If you have access to any of those operating systems,
please let me know."
  (let ((fname "cpio-int-mode-to-file-type"))
    (cond ((s-islnk int-mode)
	   *cpio-modes-link*)
	  ((s-isreg int-mode)
	   *cpio-modes-reg*)
	  ((s-isdir int-mode)
	   *cpio-modes-dir*)
	  ((s-ischr int-mode)
	   *cpio-modes-char*)
	  ((s-isblk int-mode)
	   *cpio-modes-block*)
	  ((s-isfifo int-mode)
	   *cpio-modes-fifo*)
	  ((s-issock int-mode)
	   *cpio-modes-sock*)
	  (t
	   *cpio-modes-unknown*))))

(defun cpio-int-mode-to-user-permissions (int-mode)
  "Extract the 3-character string expressing the user permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-permissions")
	(read-string    (cpio-int-mode-to-user-read-string    int-mode))
	(write-string   (cpio-int-mode-to-user-write-string   int-mode))
	(execute-string (cpio-int-mode-to-user-execute-string int-mode)))
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-user-read-string (int-mode)
  "Extract the 1-character string expressing the user read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-read-string"))
    (cond ((/= (logand int-mode s-irusr) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-user-write-string (int-mode)
  "Extract the 1-character string expressing the user write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-write-string"))
    (cond ((/= (logand int-mode s-iwusr) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-user-execute-string (int-mode)
  "Extract the 1-character string expressing the user execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-user-execute-string"))
    (cond ((/= (logand int-mode s-ixusr) 0)
	   (if (/= (logand int-mode s-isuid) 0)
	       "s"
	     "x"))
	  (t
	   (if (/= (logand int-mode s-isuid) 0)
	       "S"
	     "-")))))

(defun cpio-int-mode-to-group-permissions (int-mode)
  "Extract the 3-character string expressing the group permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-permissions")
	(read-string    (cpio-int-mode-to-group-read-string    int-mode))
	(write-string   (cpio-int-mode-to-group-write-string   int-mode))
	(execute-string (cpio-int-mode-to-group-execute-string int-mode)))
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-group-read-string (int-mode)
  "Extract the 1-character string expressing the group read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-read-string"))
    (cond ((/= (logand s-irgrp int-mode) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-group-write-string (int-mode)
  "Extract the 1-character string expressing the group write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-write-string"))
    (cond ((/= (logand s-iwgrp int-mode) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-group-execute-string (int-mode)
  "Extract the 1-character string expressing the group execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-group-execute-string"))
    (cond ((/= (logand int-mode s-ixgrp) 0)
	   (if (/= (logand int-mode s-isgid) 0)
	       "s"
	     "x"))
	  (t
	   (if (/= (logand int-mode s-isgid) 0)
	       "S"
	     "-")))))

(defun cpio-int-mode-to-other-permissions (int-mode)
  "Extract the 3-character string expressing the other permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-permissions")
	(read-string    (cpio-int-mode-to-other-read-string    int-mode))
	(write-string   (cpio-int-mode-to-other-write-string   int-mode))
	(execute-string (cpio-int-mode-to-other-execute-string int-mode)))
    (concat read-string write-string execute-string)))

(defun cpio-int-mode-to-other-read-string (int-mode)
  "Extract the 1-character string expressing the other read permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-read-string"))
    (cond ((/= (logand s-iroth int-mode) 0)
	   "r")
	  (t "-"))))

(defun cpio-int-mode-to-other-write-string (int-mode)
  "Extract the 1-character string expressing the other write permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-write-string"))
    (cond ((/= (logand s-iwoth int-mode) 0)
	   "w")
	  (t "-"))))

(defun cpio-int-mode-to-other-execute-string (int-mode)
  "Extract the 1-character string expressing the other execute permissions from INT-MODE."
  (let ((fname "cpio-int-mode-to-other-execute-string"))
    (cond ((/= (logand s-ixoth int-mode) 0)
	   (if (/= (logand s-isvtx int-mode) 0)
	       "t"
	     "x"))
	  (t
	   (if (/= (logand s-isvtx int-mode) 0)
	       "T"
	     "-")))))

(defun cpio-mode-string-to-int-mode (mode-string)
  ;; HEREHERE This should do some error checking.
  ;; It will currently flag an error if MODE-STRING is not long enough.
  "Convert an ls -l style mode string to its corresponding integer."
  (let* ((fname "cpio-mode-string-to-int-mode")
	 (bits 0)
	 (chars (mapcar 'string-to-char (split-string mode-string "" t)))
	 (type-char (car (cl-subseq chars 0  1)))
	 (owner-chars    (cl-subseq chars 1  4))
	 (group-chars    (cl-subseq chars 4  7))
	 (other-chars    (cl-subseq chars 7 10)))
    (setq bits (logior bits
		       (cpio-type-char-to-bits type-char)
		       (cpio-owner-chars-to-bits owner-chars)
		       (cpio-group-chars-to-bits group-chars)
		       (cpio-other-chars-to-bits other-chars)))
    bits))

(defun cpio-type-char-to-bits (char)
  "Return the mode bits implied by the given type CHAR."
  (let ((fname "cpio-type-char-to-bits"))
    (unless (and (characterp char)
		 (or (= char (string-to-char *cpio-modes-link*))
		     (= char (string-to-char *cpio-modes-reg*))
		     (= char (string-to-char *cpio-modes-dir*))
		     (= char (string-to-char *cpio-modes-char*))
		     (= char (string-to-char *cpio-modes-block*))
		     (= char (string-to-char *cpio-modes-fifo*))
		     (= char (string-to-char *cpio-modes-sock*))
		     (= char (string-to-char *cpio-modes-unknown*))))
      (signal 'wrong-type-argument char))
    (cond ((= char  (string-to-char *cpio-modes-link*))
	   s-iflnk)
	  ((= char  (string-to-char *cpio-modes-reg*))
	   s-ifreg)
	  ((= char  (string-to-char *cpio-modes-dir*))
	   s-ifdir)
	  ((= char  (string-to-char *cpio-modes-char*))
	   s-ifchr)
	  ((= char  (string-to-char *cpio-modes-block*))
	   s-ifblk)
	  ((= char  (string-to-char *cpio-modes-fifo*))
	   s-ififo)
	  ((= char  (string-to-char *cpio-modes-sock*))
	   s-ifsock)
	  (t
	   (error "%s(): Uknown file type is not yet supported." fname)))))

(defun cpio-owner-chars-to-bits (chars)
  "Interpret the given CHARS as bits relevant to the owner of a file."
  (let ((fname "cpio-owner-chars-to-bits")
	(read-char)
	(write-char)
	(exec-char)
	(bits 0))
    (unless (and (listp chars)
		 (= (length chars) 3)
		 (member (setq read-char  (nth 0 chars)) '(?- ?r))
		 (member (setq write-char (nth 1 chars)) '(?- ?w))
		 (member (setq exec-char  (nth 2 chars)) '(?- ?x ?s ?S)))
      (signal 'wrong-type-argument chars))
    (cond ((= read-char ?-))
	  ((= read-char ?r)
	   (setq bits (logior bits s-irusr))))
    (cond ((= write-char ?-))
	  ((= write-char ?w)
	   (setq bits (logior bits s-iwusr))))
    (cond ((= exec-char ?-))
	  ((= exec-char ?x)
	   (setq bits (logior bits s-ixusr)))
	  ((= exec-char ?s)
	   (setq bits (logior bits s-ixusr s-isuid)))
	  ((= exec-char ?S)
	   (setq bits (logior bits s-isuid))))
    bits))

(defun cpio-group-chars-to-bits (chars)
  "Interpret CHARS as group mode bits."
  (let ((fname "cpio-group-chars-to-bits")
	(read-char)
	(write-char)
	(exec-char)
	(bits 0))
    (unless (and (listp chars)
		 (= (length chars) 3)
		 (member (setq read-char  (nth 0 chars)) '(?- ?r))
		 (member (setq write-char (nth 1 chars)) '(?- ?w))
		 (member (setq exec-char  (nth 2 chars)) '(?- ?x ?s ?S)))
      (signal 'wrong-type-argument chars))
    (cond ((= read-char ?-))
	  ((= read-char ?r)
	   (setq bits (logior bits s-irgrp))))
    (cond ((= write-char ?-))
	  ((= write-char ?w)
	   (setq bits (logior bits s-iwgrp))))
    (cond ((= exec-char ?-))
	  ((= exec-char ?x)
	   (setq bits (logior bits s-ixgrp)))
	  ((= exec-char ?s)
	   (setq bits (logior bits s-ixgrp s-isgid)))
	  ((= exec-char ?S)
	   (setq bits (logior bits s-isgid))))
    bits))

(defun cpio-other-chars-to-bits (chars)
  "Interpret CHARS as other mode bits."
  (let ((fname "cpio-other-chars-to-bits")
	(read-char)
	(write-char)
	(exec-char)
	(bits 0))
    (unless (and (listp chars)
		 (= (length chars) 3)
		 (member (setq read-char  (nth 0 chars)) '(?- ?r))
		 (member (setq write-char (nth 1 chars)) '(?- ?w))
		 (member (setq exec-char  (nth 2 chars)) '(?- ?x ?t ?T)))
      (signal 'wrong-type-argument chars))
    (cond ((= read-char ?-))
	  ((= read-char ?r)
	   (setq bits (logior bits s-iroth))))
    (cond ((= write-char ?-))
	  ((= write-char ?w)
	   (setq bits (logior bits s-iwoth))))
    (cond ((= exec-char ?-))
	  ((= exec-char ?x)
	   (setq bits (logior bits s-ixoth)))
	  ((= exec-char ?t)
	   (setq bits (logior bits s-ixoth s-isvtx)))
	  ((= exec-char ?T)
	   (setq bits (logior bits s-isvtx))))
    bits))

(defun cpio-mode-extractable-p (mode)
  "Return non-NIL if MODE represents an entry that can be extracted by cpio-mode.
That is, a regular file, symbolic link or directory. "
  (let ((fname "cpio-mode-not-extractable-p"))
    (or (s-islnk mode)
	(s-isreg mode)
	(s-isdir mode))))

(defun cpio-valid-numeric-mode (proposed-mode-num)
  "Return non-NIL if the PROPOSED-MODE-NUM is a valid numeric file mode."
  (let ((fname "cpio-valid-numeric-mode"))
    (/= 0 (logxor proposed-mode-num
		  ;; This list could be pared down a little,
		  ;; but this is more readable.
		  (logand s-ifmt
			  s-ifsock
			  s-iflnk
			  s-ifreg
			  s-ifblk
			  s-ifdir
			  s-ifchr
			  s-ififo
			  s-isuid
			  s-isgid
			  s-isvtx
			  s-irwxu
			  s-irwxg
			  s-irwxo)))))

(provide 'cpio-modes)
;;; cpio-modes ends here

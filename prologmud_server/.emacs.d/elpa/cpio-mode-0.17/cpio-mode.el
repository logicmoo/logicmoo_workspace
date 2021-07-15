;;; cpio-mode.el --- Handle cpio archives in the style of dired. -*- coding: utf-8 -*-

;; Author: Douglas Lewan <d.lewan2000@gmail.com>
;; Maintainer: Douglas Lewan <d.lewan2000@gmail.com>
;; Version: 0.17
;; Long description: cpio-mode provides a dired-like interface for working with cpio archives. You can view, edit and save entries. You can also change permissions, UID, etc.
;; Package-Requires: ((emacs "24.5"))
;; Created: 2015 Jan 03
;; Package-Type: multi
;; Keywords: files

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


;;; Commentary:

;; This package implements cpio-mode,
;; a mode for working with cpio archives
;; through a dired-like interface.


;;; Documentation:

;;
;; NAME: cpio-mode
;;
;; USAGE:
;;     There are several ways to invoke cpio-mode:
;;
;;     • M-x cpio-mode
;;
;;     • If you want to put a cpio-archive into cpio-mode automatically,
;;	 then add the following to your .emacs:
;;         (add-hook 'find-file-hook #'cpio-mode-find-file-hook)
;;
;;     • Another way to do this would be to modify magic-mode-alist
;;		(add-to-list 'magic-mode-alist
;;                      (cons #'cpio-discern-archive-type #'cpio-mode))
;;
;;     • If you only care about archives that end .cpio,
;;	 then the following would also work:
;;         (add-to-list 'auto-mode-alist (cons "\\.cpio\\'" #'cpio-mode))
;;
;; DESCRIPTION:
;;     cpio-mode presents a cpio archive as if it were a directory
;;     in a manner like dired-mode.
;;     tar-mode already does such a thing for tar archives,
;;     and some ideas (and likely code) have been adapted from tar-mode.
;;
;;     To automatically invoke cpio-mode when finding a file
;;     add the following to your find-file-hook.
;;
;;     You can use toggle-cpio-mode to switch between cpio-mode
;;     and fundamental mode.
;;
;; KEYMAP:
;;     This should be conceptually as close to dired as I can make it.
;;
;; OPTIONS:
;;
;; ENVIRONMENT:
;;     Early development was done under emacs 24.2
;;     on the Fedora 18 distribution of 64 bit GNU/Linux.
;;
;;     Later development happened under emacs 24.5
;;     on GNU/Linux Mint, Linux kernel 4.4.0.
;;
;;     Current development is happening on emacs 24.5
;;     on GNU/Linux Trisquel, Linux Kernel 4.4.0.
;;
;; RETURN CODE:
;;
;; NOTES:
;;     Binary formats are not yet implemented.
;;
;; CAVEATS:
;;     Only regular files can be edited.
;;     I'm not sure what to do with symbolic links yet.
;;
;; SECURITY ISSUES:
;;     There are no ownership/group-ship tests on anything.
;;     You could create an archive with bad behavior
;;     (for example, a set-uid executable)
;;     when unpacked by root.
;;

;;
;; cpio-mode.el is the entry point to all of cpio-mode code.
;; It defines the archive management variables and functions
;; that define cpio-mode.
;; That said, there are other components.
;; 1. There's some generically useful code
;;    defined in
;;    • cpio-generic.el, truly generic code,
;;    • cpio-modes.el, file-mode related information,
;; 2. Every archive format has its own file:
;;    cpio-bin for the cpio binary format,
;;    cpio-crc for the cpio CRC format,
;;    etc.
;; 3. cpio-mode.el, this file, defining the cpio logic
;;    reflected in the catalog,
;;    a list of the information of all the headers
;;    in the current archive.
;; 4. The package cpio-dired, defining the user interface.
;;
;; The following figure shows the relationships
;; among those components.
;;
;; +----------------------+   +-------------+	+-------------+
;; | Format specific code |   |		    |	|	      |
;; | +---------------+	  |   |		    |	|	      |
;; | | cpio-bin	     |	  |   |		    |	|	      |
;; | | +--------------+	  |   |	   CPIO	    |	| dired-like  |
;; | +-|cpio-crc      |	  |<->|	   Logic    |<->|     UI      |
;; |   | +-------------+  |   |		    |	|	      |
;; |   +-| hpbin       |  |   |		    |	|	      |
;; |	 | +------------+ |   |		    |	|	      |
;; |	 +-| ···	| |   |		    |	|	      |
;; |	   +------------+ |   |		    |	|	      |
;; +----------------------+   +-------------+	+-------------+
;;	       Λ		     Λ		       Λ
;;	       |		     |		       |
;;	       V		     V		       V
;; +----------------------------------------------------------+
;; | generic code					      |
;; |	      +------------+ +--------------+ +-----+	      |
;; |	      | cpio-modes | | cpio-generic | | ··· |	      |
;; |	      +------------+ +--------------+ +-----+	      |
;; +----------------------------------------------------------+
;;
;; The basic idea is that the format-spedific code parses and makes headers
;; while all the cpio logic uses those parsed headers to edit
;; and calls format-specific parsing and making functions.
;;
;; The main data structures are the following.
;;
;; 0. Parsed headers, an inode-like array structure.
;;
;; 1. Entries, an array containing a parsed header,
;;    the header start and the contents start.
;;
;; 2. The catalog, a list of the entries in the cpio archive,
;;    including the trailer.
;;
;; 3. The buffer holding the archive.
;;    This buffer is put into cpio-mode.
;;    It holds all the "global" data,
;;    like the catalog described above.
;;
;; 4. The buffer holding the dired-like UI.
;;    cpio-mode creates this buffer and
;;    puts this buffer into cpio-dired-mode.
;;
;; 5. Buffers visiting entries.
;;    cpio-dired-mode uses the archive buffer
;;    to get entry contents and them in the visiting buffer.
;;    cpio-dired-mode puts that buffer in cpio-entry-contents-mode,
;;    a minor mode that handles editing and saving
;;    an entry's contents.

;;; Naming conventions.

;;
;; All files that define cpio-mode begin with "cpio."
;;
;; Global variables all begin '*cpio-...'.
;; Functions are named 'cpio-...'.
;;
;; The corresponding archive format specific names for format FMT
;; begin '*cpio-FMT-...' and 'cpio-FMT-...'.
;; The format-specific variables names are calculated
;; in (cpio-set-local-vars).
;; That function drops directly into corresponding format-specific functions
;;
;; The format-specific function names are calculated
;; in (cpio-set-local-funcs).
;; Here is the process:
;;     cpio-do-good-stuff-func
;;     --> "cpio-do-good-stuff-func"
;;     --> "cpio" "do" "good" "stuff"
;;     --> "cpio-fmt-do-good-stuff"
;;     --> cpio-fmt-do-good-stuff
;;
;; The index of FIELD within a parsed header is named 'cpio-FIELD-parsed-idx'.
;;
;; Each archive format FMT has a regular expression
;; that identifies that format unambiguously called '*cpio-FMT-header-re*'.
;;
;; The functions (cpio-get-FIELD) operate directly on the header
;; to extract FIELD.
;; It's not clear that these need to be defined here.
;;
;; The functions (cpio-FIELD) operate on a parsed header
;; to extract FIELD.
;;
;; Depending on the context the expression "entry attributes",
;; often abbreviated "attrs", and the phrase "parsed header"
;; are used to reference the structure
;; that stores inode-like information (mode, size, user, etc.).
;; Truly, the expressions are semantically equivalent.
;; However, "parsed header" is used where the topic at hand is
;; the archive, and
;; "entry attributes" is used where the topic at hand is
;; the internal processing within cpio-mode.
;;
;; An "entry" is, somewhat ambiguously, either an entry in the archive
;; or a member of the catalog.
;; The context should make it clear which is intended.
;; Yes, in principle they're isomorphic.
;; (And, yes, I hate specifications that depend on context.)
;;

;;; Code:

;;
;; Dependencies
;;


(require 'dired)

(require 'cpio-generic)
(require 'cpio-modes)

(require 'cpio-affiliated-buffers)

(require 'cpio-bin)
;; While I like things to be alphabetical, newc /must/ precede crc.
(require 'cpio-newc)
(require 'cpio-crc)
(require 'cpio-hpbin)
(require 'cpio-hpodc)
(require 'cpio-odc)
(require 'cpio-dired)
;; (require 'cpio-entry-contents-mode) ;;FIXME: missing file?

;; Formats not supported:
;;   (require 'cpio-tar)
;;   (require 'cpio-ustar)

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(defvar *cpio-catalog-entry-attrs-idx*)
(defvar *cpio-catalog-entry-contents-start-idx*)
(defvar *cpio-catalog-entry-header-start-idx*)
(defvar *cpio-catalog-entry-length*)
(defvar *cpio-catalog-entry-modified-flag-idx*)
(defvar *cpio-chksum-parsed-idx*)
(defvar *cpio-crc-header-re*)
(defvar *cpio-crc-padding-char*)
(defvar *cpio-crc-padding-modulus*)
(defvar *cpio-crc-padding-str*)
(defvar *cpio-dev-maj-parsed-idx*)
(defvar *cpio-dev-min-parsed-idx*)
(defvar *cpio-entry-size-parsed-idx*)
(defvar *cpio-gid-parsed-idx*)
(defvar *cpio-ino-parsed-idx*)
(defvar *cpio-mode-parsed-idx*)
(defvar *cpio-mtime-parsed-idx*)
(defvar *cpio-name-parsed-idx*)
(defvar *cpio-namesize-parsed-idx*)
(defvar *cpio-newc-header-re*)
(defvar *cpio-newc-padding-char*)
(defvar *cpio-newc-padding-modulus*)
(defvar *cpio-newc-padding-str*)
(defvar *cpio-nlink-parsed-idx*)
(defvar *cpio-odc-header-re*)
(defvar *cpio-odc-padding-char*)
(defvar *cpio-odc-padding-modulus*)
(defvar *cpio-odc-padding-str*)
(defvar *cpio-parsed-header-length*)
(defvar *cpio-rdev-maj-parsed-idx*)
(defvar *cpio-rdev-min-parsed-idx*)
(defvar *cpio-uid-parsed-idx*)
(defvar cpio-entry-name)
(defvar cpio-try-names)
;;;; (declare-function cpio-contents-buffer-name "cpio-dired.el")
(declare-function cpio-dired-buffer-name "cpio-dired.el")
(declare-function cpio-dired-move-to-first-entry "cpio-dired.el")
(declare-function cpio-dired-next-line "cpio-dired.el")
(declare-function cpio-entry-contents-mode "cpio-entry-contents-mode.el") ;FIXME: Unused!
(declare-function cpio-present-ala-dired "cpio-dired.el")
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;


;;
;; Vars
;;
(defvar *cpio-format* ()
  "The format of the cpio archive in the current-buffer.
Takes the values 'bin, 'newc, 'odc etc.")
(setq *cpio-format* ())

(make-variable-buffer-local '*cpio-format*)

;; N.B. The format REs go here since they are what we use
;; to discern the type of the archive.

(defvar *cpio-tar-header-re* "not yet set"
  "RE to match tar format cpio archives.")
(setq *cpio-tar-header-re* "not yet set")


(defvar *cpio-ustar-header-re* "not yet set"
  "RE to match ustar format cpio archives.")
(setq *cpio-ustar-header-re* "not yet set")


(defvar *cpio-hpbin-header-re* "not yet set"
  "RE to match hpbin format cpio archives.")
(setq *cpio-hpbin-header-re* "not yet set")


(defvar *cpio-hpodc-header-re* "not yet set"
  "RE to match hpodc format cpio archives.")
(setq *cpio-hpodc-header-re* "not yet set")


;; MAINTENTANCE Order matters; hpodc must precede odc.
(defvar *cpio-re-type-alist* (list
			      (cons *cpio-bin-header-re*   'bin)
			      (cons *cpio-crc-header-re*   'crc)
			      (cons *cpio-hpbin-header-re* 'hpbin)
			      (cons *cpio-hpodc-header-re* 'hpodc)
			      (cons *cpio-newc-header-re*  'newc)
			      (cons *cpio-odc-header-re*   'odc)
			      (cons *cpio-tar-header-re*   'tar)
			      (cons *cpio-ustar-header-re* 'ustar))
  "Association list matching REs defining cpio entry header types
with their corresponding archive types.
The archive types are symbols: 'bin, 'newc, 'odc, etc.
See `cpio-discern-archive-type' for the full list.")
(setq *cpio-re-type-alist* (list
			    (cons *cpio-bin-header-re*	 'bin)
			    (cons *cpio-crc-header-re*	 'crc)
			    (cons *cpio-hpbin-header-re* 'hpbin)
			    (cons *cpio-hpodc-header-re* 'hpodc)
			    (cons *cpio-newc-header-re*	 'newc)
			    (cons *cpio-odc-header-re*	 'odc)
			    (cons *cpio-tar-header-re*	 'tar)
			    (cons *cpio-ustar-header-re* 'ustar)))

(defvar cpio-build-catalog-func ()
  "The function for building the catalog of a specific archive format.")
(setq cpio-build-catalog-func ())


(defvar cpio-parse-header-func ()
  "")
(setq cpio-parse-header-func ())

(defvar cpio-header-at-point-func ()
  "")
(setq cpio-header-at-point-func ())

(defvar OBS-cpio-get-magic-func ()
  "")
(setq OBS-cpio-get-magic-func ())

(defvar OBS-cpio-get-ino-func ()
  "")
(setq OBS-cpio-get-ino-func ())

(defvar OBS-cpio-get-mode-func ()
  "")
(setq OBS-cpio-get-mode-func ())

(defvar OBS-cpio-get-uid-func ()
  "")
(setq OBS-cpio-get-uid-func ())

(defvar OBS-cpio-get-gid-func ()
  "")
(setq OBS-cpio-get-gid-func ())

(defvar OBS-cpio-get-nlink-func ()
  "")
(setq OBS-cpio-get-nlink-func ())

(defvar OBS-cpio-get-mtime-min-func ()
  "")
(setq OBS-cpio-get-mtime-min-func ())

(defvar OBS-cpio-get-filesize-func ()
  "")
(setq OBS-cpio-get-filesize-func ())

(defvar OBS-cpio-get-dev-maj-func ()
  "")
(setq OBS-cpio-get-dev-maj-func ())

(defvar OBS-cpio-get-dev-min-func ()
  "")
(setq OBS-cpio-get-dev-min-func ())

(defvar OBS-cpio-get-rdev-maj-func ()
  "")
(setq OBS-cpio-get-rdev-maj-func ())

(defvar OBS-cpio-get-rdev-min-func ()
  "")
(setq OBS-cpio-get-rdev-min-func ())

(defvar OBS-cpio-get-namesize-func ()
  "")
(setq OBS-cpio-get-namesize-func ())

(defvar OBS-cpio-get-chksum-func ()
  "")
(setq OBS-cpio-get-chksum-func ())

(defvar OBS-cpio-get-name-func ()
  "")
(setq OBS-cpio-get-name-func ())

(defvar OBS-cpio-get-contents-func ()
  "")
(setq OBS-cpio-get-contents-func ())

(defvar cpio-make-header-string-func ()
  "")
(setq cpio-make-header-string-func ())

(defvar cpio-adjust-trailer-func ()
  "")

(defvar cpio-insert-trailer-func ()
  "")

(defvar cpio-delete-trailer-func ()
  "")

(defvar cpio-make-chksum-for-file-func ()
  "")

(defvar *cpio-local-funcs* ()
  "A list of variables peculiar to the different headers and their fields.
The design here is that package-wide variables have the prefix `cpio-'
and the corresponding functions for a specific format FMT have the form `cpio-FMT-'.
All of this can then be calculated via (symbol-name), etc.")
(setq *cpio-local-funcs* (list
			  ;; Catalog management
			  'cpio-build-catalog-func
			  'cpio-make-chksum-for-file-func
			  ;; Header parsing functions
			  'cpio-end-of-archive-func
			  'cpio-start-of-trailer-func
			  ;; Header making functions
			  'cpio-make-chksum-func
			  'cpio-make-header-string-func
			  ;; Archive manipulation functions
			  'cpio-adjust-trailer-func
			  'cpio-insert-trailer-func
			  'cpio-delete-trailer-func))
;; (make-variable-buffer-local '*cpio-local-funcs*)

(defvar *cpio-catalog* ()
  "The variable that holds the catalog of entries in cpio-mode.
Each entry has the following form:
    name
    [parsed-header
     header-start
     content-start
     modified-flag].
• name is the name of the entry
• parsed-header has the description below.
• header-start and content-start are markers,
  so they should be automatically updated
  with modifications to the buffer.
• Don't use (aref) to get at these; use an accessor function.
The last entry should is always be the TRAILER entry.

A parsed header is a vector of the following form:
    [inode
     mode
     uid
     gid

     nlink
     mtime
     filesize
     dev-maj

     dev-min
     rdev-maj
     rdev-min
     name-size

     chksum
     name].")
(make-variable-buffer-local '*cpio-catalog*)
(setq *cpio-catalog* ())

(let ((i 0))
  ;; (defvar *cpio-catalog-entry-name-idx* i)
  ;; (setq i (1+ i))
  (defvar *cpio-catalog-entry-attrs-idx* i)
  (setq *cpio-catalog-entry-attrs-idx* i)

  (setq i (1+ i))
  (defvar *cpio-catalog-entry-header-start-idx* i)
  (setq *cpio-catalog-entry-header-start-idx* i)

  (setq i (1+ i))
  (defvar *cpio-catalog-entry-contents-start-idx* i)
  (setq *cpio-catalog-entry-contents-start-idx* i)

  (setq i (1+ i))
  (defvar *cpio-catalog-entry-modified-flag-idx* i)
  (setq *cpio-catalog-entry-modified-flag-idx* i)

  (setq i (1+ i))
  (defvar *cpio-catalog-entry-length* i)
  (setq *cpio-catalog-entry-length* i))

(defvar *cpio-archive-name* ()
  "The name of the cpio archive being processed.")
(setq *cpio-archive-name* ())
(make-variable-buffer-local '*cpio-archive-names*)

;; Indexes for the fields in a parsed header.
(let ((i 0))
  (defvar *cpio-ino-parsed-idx* i)
  (setq *cpio-ino-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-mode-parsed-idx* i)
  (setq *cpio-mode-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-uid-parsed-idx* i)
  (setq *cpio-uid-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-gid-parsed-idx* i)
  (setq *cpio-gid-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-nlink-parsed-idx* i)
  (setq *cpio-nlink-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-mtime-parsed-idx* i)
  (setq *cpio-mtime-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-entry-size-parsed-idx* i)
  (setq *cpio-entry-size-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-dev-maj-parsed-idx* i)
  (setq *cpio-dev-maj-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-dev-min-parsed-idx* i)
  (setq *cpio-dev-min-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-rdev-maj-parsed-idx* i)
  (setq *cpio-rdev-maj-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-rdev-min-parsed-idx* i)
  (setq *cpio-rdev-min-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-namesize-parsed-idx* i)
  (setq *cpio-namesize-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-chksum-parsed-idx* i)
  (setq *cpio-chksum-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-name-parsed-idx* i)
  (setq *cpio-name-parsed-idx* i)

  (setq i (1+ i))
  (defvar *cpio-parsed-header-length* i
    "The length of a parsed header (attribute vector).")
  (setq *cpio-parsed-header-length* i))

(defvar *cpio-padding-modulus* ()
  "The modulus to be used for building padded strings.")
(setq *cpio-padding-modulus* ())

(defvar *cpio-padding-char* ()
  "The character to be used for building padded strings.")
(setq *cpio-padding-char* ())

(defvar *cpio-padding-str* ()
  "A single character string of the character to be used for building padded strings.")
(setq *cpio-padding-str* ())


(defvar *cpio-archive-syntax-table* ()
  "Every character in a cpio archive has word syntax.")
(setq *cpio-archive-syntax-table* ())


(defvar *cpio-header-length* ()
  "A buffer-local variable to hold the length of the header
for a cpio archive of the current format.")
(setq *cpio-header-length* ())



;;
;; Customizations
;;
(defgroup cpio ()
  "Customizations for cpio-mode."
  :group 'data)

(defcustom cpio-default-format "newc"
  "The default cpio format to use for a new or empty archive."
  :type 'string
  :group 'cpio)


;;
;; Library
;;

;;;###autoload
(defun cpio-mode-find-file-hook ()
  "find-file hook to detect if a file is likely a cpio archive.
If it is, then put it under cpio-mode."
  (let ((fname "cpio-mode-find-file-hook"))
    (if (cpio-discern-archive-type)
	(cpio-mode))))

;;;###autoload
(defun cpio-discern-archive-type ()
  "Return a symbol reflecting the type of the cpio archive in the current buffer.
Values are `bin', `newc', `odc', `crc', `tar', `ustar', `hpbin', `hpodc',
or nil if the current buffer does not begin with a cpio entry header."
  ;; Using a RE may not be the right way to go.
  ;; Maybe each format needs a function.
  (let ((fname "cpio-discern-archive-type")
	(this-archive-type ()))
    (unless *cpio-archive-syntax-table*
      (setq *cpio-archive-syntax-table* (make-syntax-table))
      (let ((i 0))
	(while (< i #x100)
	  (modify-syntax-entry i "w" *cpio-archive-syntax-table*)
	  (setq i (1+ i)))))
    (with-syntax-table *cpio-archive-syntax-table*
      (save-excursion
	(widen)
	(goto-char (point-min))
	(catch 'found-it
	  (mapc (lambda (archive-spec)
		  (cond ((looking-at-p (car archive-spec))
			 (setq this-archive-type (cdr archive-spec))
			 (throw 'found-it t))
			(t t)))
		*cpio-re-type-alist*))))
    this-archive-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header handling

;; HEREHERE I think all these (funcall)s need to be wrapped with *s.
(defun cpio-parse-header (header-str)
  "Return the internal entry header structure encoded in HEADER-STR."
  (let ((fname "cpio-parse-header"))
    (funcall cpio-parse-header-func header-str)))

(defun OBS-cpio-header-at-point (&optional where)
  "Return the header string at or following point WHERE.
If WHERE is not given, then use point.
CAVEAT: This searches for the magic number at the begining of the header;
if WHERE is inside the magic number, then the search will fail."
  (unless where (setq where (point)))
  (let ((fname "cpio-header-at-point"))
    (funcall cpio-header-at-point-func where)))

(defun cpio-ino (parsed-header)
  "Return the inode in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-ino"))
    (aref parsed-header *cpio-ino-parsed-idx*)))

(defun cpio-mode-value (parsed-header)
  "Return the mode (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-mode"))
    (aref parsed-header *cpio-mode-parsed-idx*)))

(defun cpio-uid (parsed-header)
  "Return the UID (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-uid"))
    (aref parsed-header *cpio-uid-parsed-idx*)))

(defun cpio-gid (parsed-header)
  "Return the GID (as an integer) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-gid"))
    (aref parsed-header *cpio-gid-parsed-idx*)))

(defun cpio-nlink (parsed-header)
  "Return the number of links in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-nlink"))
    (aref parsed-header *cpio-nlink-parsed-idx*)))

(defun cpio-mtime (parsed-header)
  "Return the mod time (emacs time structure) in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-mtime"))
    (aref parsed-header *cpio-mtime-parsed-idx*)))

(defun cpio-entry-size (parsed-header)
  "Return the size of the contents in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-entry-size"))
    (aref parsed-header *cpio-entry-size-parsed-idx*)))

(defun cpio-dev-maj (parsed-header)
  "Return the dev maj in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-dev"))
    (aref parsed-header *cpio-dev-maj-parsed-idx*)))

(defun cpio-dev-min (parsed-header)
  "Return the dev in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-dev-min"))
    (aref parsed-header *cpio-dev-min-parsed-idx*)))

(defun cpio-rdev-maj (parsed-header)
  "Return the rdev maj in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-rdev-maj"))
    (aref parsed-header *cpio-rdev-maj-parsed-idx*)))

(defun cpio-rdev-min (parsed-header)
  "Return the rdev in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-rdev-min"))
    (aref parsed-header *cpio-rdev-min-parsed-idx*)))

(defun cpio-namesize (parsed-header)
  "Return the size of the name	in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-namesize"))
    (aref parsed-header *cpio-namesize-parsed-idx*)))

(defun cpio-entry-name (parsed-header)
  "Return the name in PARSED-HEADER.
CAVEAT: See `cpio-magic'."
  (let ((fname "cpio-name"))
    (aref parsed-header *cpio-name-parsed-idx*)))

(defun cpio-chksum (parsed-header)
  "Return the checksum in PARSE-HEADER."
  (let ((fname "cpio-chksum"))
    (aref *cpio-chksum-parsed-idx* parsed-header)))

(defun cpio-contents-start (entry-name)
  "Return the contents start for ENTRY-NAME."
  (let ((fname "cpio-contents-start")
	(catalog-entry (cpio-entry entry-name)))
    (aref catalog-entry *cpio-catalog-entry-contents-start-idx*)))

(defun cpio-entry-attrs (entry-name)
  "Retrieve the entry attributes for ENTRY-NAME."
  (let ((fname "cpio-entry-attrs"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-entry-attrs entry-name))
      (aref (cpio-entry entry-name) *cpio-catalog-entry-attrs-idx*))))

(defun cpio-entry-header-start (entry)
  "Return the start of the entry specified in ENTRY."
  (let ((fname "cpio-entry-header-start"))
    (aref entry *cpio-catalog-entry-header-start-idx*)))

;; HEREHERE This isn't right yet.
;; It's being introduced to fix the acrobatics in (cpio-internal-do-deletion).
(defun cpio-entry-header-end (entry)
  "Return the end of the unpadded header specified in ENTRY."
  (let ((fname "cpio-entry-header-end")
	(attrs (aref entry *cpio-catalog-entry-attrs-idx*)))
    (+ (cpio-entry-header-start entry) *cpio-header-length* (cpio-namesize attrs))
    ))

(defun cpio-entry-contents-start (entry)
  "Return the start of the contents of the entry specified in ENTRY."
  (let ((fname "cpio-entry-contents-start"))
    (aref entry *cpio-catalog-entry-contents-start-idx*)))

(defun cpio-entry-contents-end (entry)
  "Return the end of the contents of the entry specified in ENTRY."
  (let* ((fname "cpio-entry-contents-end")
	 (attrs (aref entry *cpio-catalog-entry-attrs-idx*))
	 (entry-name (cpio-entry-name attrs)))
    (+ (cpio-entry-contents-start entry)
       (cpio-entry-size attrs))))

(defun cpio-set-header-start (entry where)
  "Set the header start marker in ENTRY to the location WHERE."
  (let ((fname "cpio-set-header-start")
	(where-marker (cond ((integerp where)
			     (set-marker (make-marker) where))
			    ((markerp where)
			     where)
			    (t
			     (signal 'wrong-type-error (list where))))))
    (aset entry *cpio-catalog-entry-header-start-idx* where-marker)))

(defun cpio-set-contents-start (entry where)
  "Set the contents start marker in ENTRY to the location WHERE.
WHERE can be an integer or marker."
  (let ((fname "cpio-set-contents-start")
	(where-marker (cond ((integerp where)
			     (set-marker (make-marker) where))
			    ((markerp where)
			     where)
			    (t
			     (signal 'wrong-type-error (list where))))))
    (aset entry *cpio-catalog-entry-contents-start-idx* where-marker)))

(defun cpio-contents (entry-name &optional archive-buffer)
  "Return a string that is the contents of the named entry."
  (let ((fname "cpio-contents"))
    (cond (archive-buffer
	   (with-current-buffer archive-buffer
	     (cpio-contents entry-name)))
	  (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (cpio-contents entry-name)))
	  ((eq major-mode 'cpio-mode)
	   (let* ((entry-attrs	  (cpio-entry-attrs    entry-name))
		  (contents-start (cpio-contents-start entry-name))
		  (contents-size  (cpio-entry-size entry-attrs))
		  (contents-end (+ contents-start contents-size))
		  (result))
	     (if (null entry-attrs)
		 (error "%s(): Could not get entry attributes for [[%s]]." fname entry-name))
	     (goto-char contents-start)
	     (forward-char contents-size)
	     (setq result (buffer-substring-no-properties contents-start contents-end))))
	  (t
	   (error "%s(): Could not find the archive buffer." fname)))))

(defun cpio-catalog ()
  "Return the catalog relevant to the current buffer."
  (let ((fname "cpio-catalog"))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): only makes sense in a cpio buffer." fname))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  *cpio-catalog*)
      *cpio-catalog*)))

(defun cpio-make-header-string (parsed-header &optional contents)
  "Build a padded cpio header string based on the given PARSED-HEADER."
  (let ((fname "cpio-make-header-string"))
    (funcall cpio-make-header-string-func parsed-header contents)))

(defun cpio-set-entry-size (parsed-header size)
  "Set the entry-size element of PARSED-HEADER to SIZE."
  (let ((fname "cpio-set-entry-size"))
    (aset parsed-header *cpio-entry-size-parsed-idx* size)))

(defun cpio-set-entry-name (parsed-header entry-name)
  "Set the entry-name element of the PARSED-HEADER to ENTRY-NAME.
To be consistent, this also sets the name's size element."
  (let ((fname "cpio-set-entry-name"))
    (aset parsed-header *cpio-name-parsed-idx* entry-name)
    ;; The namesize in the header includes the terminating NULL at the end of the name.
    ;; See, for example, (cpio-newc-header-size).
    (aset parsed-header *cpio-namesize-parsed-idx* (1+ (length entry-name)))))

(defun cpio-set-uid (parsed-header uid)
  "Set the uid field in the PARSED-HEADER to UID.
UID can be either a string (representing a number)
or an integer."
  (let ((fname "cpio-set-uid"))
    (unless (integerp uid)
      (setq uid (string-to-number uid)))
    (aset parsed-header *cpio-uid-parsed-idx* uid)))

(defun cpio-set-gid (parsed-header gid)
  "Set the gid field in the PARSED-HEADER to GID.
GID can be either a string (representing a number)
or an integer."
  (let ((fname "cpio-set-gid"))
    (unless (integerp gid)
      (setq gid (string-to-number gid)))
    (aset parsed-header *cpio-gid-parsed-idx* gid)))

(defun cpio-set-mode (parsed-header mode)
  "Set the mode field in the PARSED-HEADER to MODE.
MODE is either an integer or a string representing an integer."
  (let ((fname "cpio-set-mode")
	(integer-mode (cond ((integerp mode)
			     mode)
			    ((stringp mode)
			     (string-to-number mode))
			    (t
			     (signal 'wrong-type-error (list mode))))))
    (unless (cpio-valid-numeric-mode integer-mode)
      (error "%s(): Invalid mode [[%s]]." fname mode))
    (aset parsed-header *cpio-mode-parsed-idx* integer-mode)))

(defun cpio-set-mtime (parsed-header mtime)
  "Set the modification time in the PARSED-HEADER to MTIME.
MTIME is an emacs time."
  (let ((fname "cpio-set-mtime"))
    (aset parsed-header *cpio-mtime-parsed-idx* mtime)))

(defun cpio-extract-all ()
  "Extract all entries from the cpio archive related to the current buffer."
  (let ((fname "cpio-extract-all"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-extract-all))
      (mapc (lambda (e)
	      (let ((entry-name (car e)))
		(cpio-extract-entry entry-name)))
	    *cpio-catalog*))))

(defun cpio-ask-user-about-supersession-threat (entry-name)
  "Ask a user who is trying to save ENTRY-NAME what to do
if a file named ENTRY-NAME already exists
or if there is a modified buffer containing that entry's contents."
  (let* ((fname "cpio-ask-user-about-supersession-threat")
	 (intermediate-buffer-name (cpio-contents-buffer-name entry-name))
	 (intermediate-buffer (get-buffer intermediate-buffer-name))
	 (archive-mod-time (cpio-get-archive-mod-time)) ;HEREHERE Do I want this?
	 (entry-mod-time (cpio-mtime (cpio-entry-attrs entry-name))))
    (cond ((and (buffer-live-p intermediate-buffer)
		(buffer-modified-p intermediate-buffer)
		(yes-or-no-p (format "A buffer for entry %s exists and is modified. Save? " entry-name)))
	   t)
	  ((and (file-exists-p entry-name)
		(yes-or-no-p (format "File %s already exists. Overwrite? " entry-name)))
	   t)
	  (t t))))

(defun cpio-get-archive-mod-time ()
  "Return the modification time of the cpio archive affiliated with the current buffer."
  (let ((fname "cpio-get-archive-mod-time")
	(archive-buffer (if *cab-parent*
			    *cab-parent*
			  (current-buffer))))
    (with-current-buffer archive-buffer
      (message "%s(): is not implemented." fname))))

(defun cpio-extract-entry (entry-name &optional force)
  "Extract the archive entry ENTRY-NAME.
If that file already exists (and this is called interactively),
then prompt the user about overwriting it.
If a buffer is already visiting that entry,
then leave that buffer in place;
otherwise kill the intermediate buffer.

The optional argument FORCE indicates
if this was called from a lisp program.
If it is, then the extraction occurs no matter what.

CAVEAT: Extracting the same ENTRY-NAME from different archives
will create a conflict.

CONTRACT: This can only be invoked in a cpio archive under cpio-mode
or a buffer affiliated with such a buffer.

NOTE: FORCE is not currently used anywhere in cpio-mode.
It is present so that any applications that are built on cpio-mode
can have predictable results."
  (interactive "sName: \nP")
  (let ((fname "cpio-extract-entry")
	(attrs (cpio-entry-attrs entry-name))
	;; HEREHERE Would this be noticably more efficient
	;; with (cpio-numeric-entry-type)?
	(entry-type (cpio-entry-type entry-name)))
    (cond ((string-equal entry-type *cpio-modes-link*)
	   (warn "%s(): Symlink extraction is not yet implemented." fname))
	  ((string-equal entry-type *cpio-modes-reg*)
	   (cpio-extract-regular-file entry-name))
	  ((string-equal entry-type *cpio-modes-dir*)
	    (cpio-extract-directory entry-name))
	   ((string-equal entry-type *cpio-modes-char*)
	    (warn "%s(): Character special files cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-block*)
	    (warn "%s(): Block special files cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-fifo*)
	    (warn "%s(): FIFOs (pipes) cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-sock*)
	    (warn "%s(): Sockets cannot be extracted with cpio-mode." fname))
	   ((string-equal entry-type *cpio-modes-unknown*)
	    (warn "%s(): Unknown entry type -- not extracting." fname))
	   (t (error "%s(): Impossible condition." fname)))))

(defun cpio-extract-regular-file (entry-name &optional from-lisp)
  "Extract the regular file entry ENTRY-NAME.
CONTRACT: ENTRY-NAME is in fact an entry of a regular file."
  (let* ((fname "cpio-extract-regular-file")
	 (do-it (if from-lisp
		    t
		  (cpio-ask-user-about-supersession-threat entry-name)))
	 (buffer-name (cpio-contents-buffer-name entry-name))
	 (temp-buffer (get-buffer buffer-name))
	 (restore (buffer-live-p temp-buffer))
	 (contents)
	 (archive-buffer (if *cab-parent*
			     *cab-parent*
			   (current-buffer))))
    (if do-it
	(cond (temp-buffer
	       (setq contents (cpio-contents entry-name archive-buffer))
	       (cab-register temp-buffer archive-buffer)
	       (with-current-buffer temp-buffer
		 (insert contents)
		 (write-file entry-name))
	       (unless restore (kill-buffer temp-buffer)))
	      (t
	       (with-temp-buffer
		 (insert (cpio-contents entry-name archive-buffer))
		 (write-file entry-name)
		 (unless restore (kill-buffer temp-buffer)))
	       (cpio-set-file-attrs entry-name))))))

(defun cpio-extract-directory (entry-name)
  "Extract the directory entry ENTRY-NAME.
CONTRACT: ENTRY-NAME really is a directory entry."
  (let ((fname "cpio-extract-directory")
	(attrs (cpio-entry-attrs entry-name)))
    (make-directory entry-name t)
    (cpio-set-file-attrs entry-name)))

(defun cpio-entry-type (entry-name)
  "Return the type of the entry with the given ENTRY-NAME.
The type is the single character that would be displayed
in the initial mode slot of 'ls -l'.
That is, 'l' is a link, '-' is a regular file, etc.
See (cpio-int-mode-to-file-type) in cpio-modes.el for more detail.
If ENTRY-NAME is not in the current archive, then return NIL."
  (let ((fname "cpio-entry-type")
	(entry-attrs)
	(entry-mode))
    (cond ((and entry-name
		(setq entry-attrs (cpio-entry-attrs entry-name))
		(setq entry-mode (cpio-mode-value entry-attrs)))
	   (cpio-int-mode-to-file-type entry-mode))
	  (t nil))))

(defun cpio-numeric-entry-type (numeric-mode)
  "Return the numeric entry type of the given NUMERIC MODE."
  (let ((fname "cpio-numeric-entry-type"))
    (cond ((= #o170000 (logand s-ifmt	numeric-mode))
	   s-ifmt)
	  ((= #o140000 (logand s-ifsock numeric-mode))
	   s-ifsock)
	  ((= #o120000 (logand s-iflnk	numeric-mode))
	   s-iflnk)
	  ((/= 0       (logand s-ifreg	numeric-mode))
	   s-ifreg)
	  ((/= 0       (logand s-ifdir	numeric-mode))
	   s-ifdir)
	  ((/= s-ifblk (logand s-ifblk	numeric-mode))
	   s-ifblk)
	  ((/= 0       (logand s-ifchr	numeric-mode))
	   s-ifchr)
	  ((/= 0       (logand s-ififo	numeric-mode))
	   s-ififo)
	  (t
	   s-ifunk))))

(defun cpio-set-file-attrs (file-name)
  "Set the attributes on FILE-NAME
based on its attributes in the catalog."
  (let* ((fname "cpio-set-file-attrs")
	 (attrs (cpio-entry-attrs file-name))
	 (mode-value (cpio-mode-value attrs))
	 (modtime (cpio-mtime attrs))
	 (uid (cpio-uid attrs))
	 (gid (cpio-gid-to-gid-string (cpio-gid attrs))))
    (cpio-set-file-owner   file-name uid)
    (cpio-set-file-group   file-name gid)
    (cpio-set-file-mode	   file-name mode-value)
    (cpio-set-file-modtime file-name modtime)))

(defun cpio-set-file-owner (file-name user) ;HEREHERE Generic?
  "Change the owner of [the file] FILE-NAME to USER.
USER is an either a numeric uid or a user's name.
If USER is a string, but that user doesn't exist,
then fail and return NIL.
Otherwise return non-NIL."
  (let ((fname "cpio-set-file-owner")
	(uid (cond ((integerp user)
		     user)
		   ((stringp user)
		    (cpio-uid-string-to-uid user))
		   (t
		    nil))))
    (unless (file-exists-p file-name)
      (error "%s(): requires the name of an existing file." fname))
    (call-process "chown"
		  nil
		  nil
		  nil
		  uid
		  file-name)))

(defun cpio-set-file-group (file-name group) ;HEREHERE Generic?
  "Change the group of [the file] FILE-NAME to group
USER is an either a numeric gid or a group's name.
If USER is a string, but that user doesn't exist,
then fail and return NIL.
Otherwise return non-NIL."
  (let ((fname "cpio-set-file-group")
	(gid (cond ((integerp group)
		    group)
		   ((stringp group)
		    (cpio-gid-string-to-gid group))
		   (t
		    nil))))
    (unless (file-exists-p file-name)
      (error "%s(): requires the name of an existing file." fname))
    (call-process "chgrp"
		  nil
		  nil
		  nil
		  gid
		  file-name)))

(defun cpio-set-file-mode (file-name mode) ;HEREHERE Generic?
  "Set the mode of the [the file] FILE-NAME to MODE.
MODE may be a complete symbolic mode or an appropriate integer.
If FILE-NAME doesn't exist, then fail and return NIL.
If MODE is not a valid mode value (either symbolic or numeric),
then fail and return NIL."
  (let ((fname "cpio-set-file-mode")
	(mode-num (cond ((stringp mode)
			 (cpio-mode-string-to-int-mode mode))
			((integerp mode)
			 mode)
			(t
			 nil))))
    (unless (cpio-valid-numeric-mode mode-num)
      (if (integerp mode)
	  (error "%s(): [[%d]] is not a valid mode." fname mode-num)
	(error "%s(): [[%s]] is not a valid mode." fname mode-num)))
    (chmod file-name mode)))

(defun cpio-set-file-modtime (file-name mod-time) ;HEREHERE Generic?
  "Set the modtime of [the file] FILE-NAME to MOD-TIME.
MOD-TIME is an emacs time."
  (let ((fname "cpio-set-file-modtime")
	(modtime-string (cpio-mtime-to-touch-string mod-time)))
    (call-process "touch"
		  nil
		  nil
		  nil
		  "-t"
		  modtime-string
		  file-name)))

(defun cpio-mtime-to-touch-string (mtime) ;HEREHERE Generic?
  "Convert the given MTIME to a time that touch(1) understands.
MTIME is an emacs time.
Touch understands times of the form YYYYMMDDhhmm.ss."
  (let ((fname "cpio-mtime-to-touch-string"))
    (format-time-string "%Y%m%d%M%H.%S" mtime)))

(defun cpio-adjust-trailer ()
  "Replace the trailer in the current buffer
with one with the correct size fot its contents."
  (let ((fname "cpio-adjust-trailer"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-adjust-trailer-func))
      (funcall cpio-adjust-trailer-func))))

(defun cpio-insert-trailer ()
  "Insert a trailer in a cpio archive."
  (let ((fname "cpio-insert-trailer"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-insert-trailer-func))
      (funcall cpio-insert-trailer-func))))

(defun cpio-delete-trailer ()
  "Delete the trailer in the cpio archive buffer affiliated with the current buffer."
  (let ((fname "cpio-delete-trailer"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (funcall cpio-delete-trailer-func))
      (funcall cpio-delete-trailer-func))))

(defun cpio-delete-archive-entry (entry)
  "Delete the entry in the cpio archive specified by ENTRY.
ENTRY is a catalog entry."
  (let ((fname "cpio-delete-archive-entry"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-delete-archive-entry entry))
      (let* ((attrs (aref entry 0))
	     (size (cpio-entry-size attrs))
	     (entry-start (cpio-entry-header-start entry))
	     (contents-start (cpio-contents-start (cpio-entry-name attrs)))
	     (entry-end (1+ (cg-round-up (+ (1- contents-start)
					    (cpio-entry-size attrs))
					 *cpio-padding-modulus*))))
	(with-writable-buffer
	 (delete-region entry-start entry-end))))))

(defun cpio-insert-padded-header (header-string)
  "Insert an appropriately padded version of HEADER-STRING.
CONTRACT: You're at the point of insertion."
  (let ((fname "cpio-insert-padded-header")
	(padding))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-insert-padded-header header-string))
      (with-writable-buffer
       (insert (cpio-pad header-string *cpio-padding-modulus* *cpio-padding-char*))))))

(defun cpio-insert-padded-contents (contents) ;HEREHERE Generic
  "Insert an appropriately padded version of CONTENTS into the archive buffer.
CONTRACT: Point is at the point of insertion."
  (let ((fname "cpio-insert-padded-contents"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-insert-padded-contents contents))
      (with-writable-buffer
       ;; (cpio-set-contents-start (point))
       (insert (cpio-pad contents *cpio-padding-modulus* *cpio-padding-char*))))))

(defun cpio-sort-catalog ()
  "Return a copy of the catalog sorted by entry name (car cpio-catalog-entry)."
  (let ((fname "cpio-sort-catalog"))
    (sort *cpio-catalog* 'cpio-entry-less-p)))

(defun cpio-entry-less-p (l r)
  "Return non-nil if [the car of] entry L precedes [the car of] entry L.
CONTRACT: L and R should be entries:
	(entry-name [inode mode uid ...] entry-start entry-end)."
  (let ((fname "cpio-entry-less-p"))
    (string-lessp (car l) (car r))))

(defun cpio-uid-to-uid-string (uid) ;; HEREHERE Generic?
  "Convert the given UID, an integer, to a string,
either a user name or a string representing UID."
  (let ((fname "cpio-uid-to-uid-string"))
    (if cpio-try-names
	(or (user-login-name uid)
	    (number-to-string uid))
      (number-to-string uid))))

(defun cpio-uid-string-to-uid (user) ;; HEREHERE Generic?
  "Return the system UID, an integer, for the string USER."
  (let ((fname "cpio-uid-string-to-uid")
	)
    ;; (error "%s() is not yet implemented" fname)
    (unless (stringp user)
      (signal 'wrong-type-error (list user)))
    (with-temp-buffer
      ;; HEREHERE This clearly doesn't work on Windows.
      ;; Also note that it won't work on very old versions of UNIX.
      ;; What about MacOS?
      ;; What would be the right thing on other operating systems?
      (insert-file-contents "/etc/passwd")
      (if (re-search-forward (concat "^" user ":[^:]*:\\([0-9]+\\):" (point-max) t))
	  (string-to-number (match-string 1))
	nil))))

(defun cpio-gid-to-gid-string (gid) ;; HEREHERE Generic?
  "Convert the given GID, an integer, to a string."
  (let ((fname "cpio-gid-to-gid-string"))
    (if cpio-try-names
	(or (user-login-name gid)
	    (number-to-string gid))
      (number-to-string gid))))

(defun cpio-gid-string-to-gid (group) ;; HEREHERE Generic?
  "Return the system gid, an integer, for the given GROUP."
  (let ((fname "cpio-gid-string-to-gid"))
    (unless (stringp group)
      (signal 'wrong-type-error (list group)))
    (with-temp-buffer
      ;; HEREHERE This clearly doesn't work on Windows.
      ;; See (cpio-uid-string-to-uid).
      (insert-file-contents "/etc/group")
      (if (re-search-forward (concat "^" group ":[^:]*:\\([0-9]+\\):" (point-max) t))
	  (string-to-number (match-string 1))
	nil))))

(defun cpio-nlink-to-nlink-string (nlink)
  "Convert the given NLINK, an integer, to a string."
  (let ((fname "cpio-nlink-to-nlink-string"))
    (number-to-string nlink)))

(defun cpio-mtime-to-mtime-string (mtime &optional long)
  "Convert the given MTIME, an emacs internal time, to a string.
CAUTION: This depends on your emacs being able to handle
a UNIX/GNU/Linux time as an integer."
  (let ((fname "cpio-mtime-to-mtime-string")
	(six-months (* 6 30 24 60 60))
	(now (time-to-seconds (current-time)))
	(tmp-time (time-to-seconds mtime)))
    (cond (long
	   (format-time-string "%F %T" mtime))
	  ((< (- now tmp-time) six-months)
	   (format-time-string "%b %d %H:%M" mtime))
	  (t
	   (format-time-string "%b %d %Y " mtime)))))

(defun cpio-filesize-to-filesize-string (filesize)
  "Convert the given FILESIZE, an integer, to a string."
  (let ((fname "cpio-filesize-to-filesize-string"))
    (number-to-string filesize)))

(defun cpio-dev-maj-to-dev-maj-string (dev-maj)
  "Do Convert the given DEV-MAJ, an integer, to a string."
  (let ((fname "cpio-dev-maj-to-dev-maj-string"))
    (number-to-string dev-maj)))

(defun cpio-dev-min-to-dev-min-string (dev-min)
  "Do Convert the given DEV-MIN, an integer, to a string."
  (let ((fname "cpio-dev-min-to-dev-min-string"))
    (number-to-string dev-min)))

(defun cpio-entry-name-to-entry-name-string (name)
  "DConvert the given NAME, an integer, to a string."
  (let ((fname "cpio-entry-name-to-entry-name-string"))
    name))

(defun cpio-find-entry (entry-name)
  "Find the given ENTRY-NAME and return the buffer holding its contents."
  (let ((fname "cpio-find-entry")
	(target-buffer (get-file-buffer entry-name))
	(just-created nil)
	(local-parent *cab-parent*))
    (unless target-buffer
      (setq just-created t)
      (with-current-buffer (setq target-buffer (get-buffer-create entry-name))
	(cab-register target-buffer local-parent)
	(setq buffer-file-name entry-name)
	(setq buffer-file-truename (abbreviate-file-name
				    (concat (cpio-archive-name) "/"
					    buffer-file-name)))
	(set (make-local-variable 'cpio-entry-name) entry-name)))
    (with-current-buffer target-buffer
      (cond ((and just-created
		  (= 0 (buffer-size)))
	     ;; I can't seem to get coding right.
	     ;; (cpio-set-auto-coding (setq contents (cpio-contents entry-name)))
	     (with-writable-buffer
	      (insert (cpio-contents entry-name)))
	     (goto-char (point-min)))
	    (t t))
      (set-buffer-modified-p nil))
    target-buffer))

(defun cpio-archive-name ()
  "Return [the full path to] the cpio archive associated with the current buffer."
  (let ((fname "cpio-archive-name"))
    (unless (or *cab-parent*
		(eq major-mode 'cpio-mode))
      (error "%s(): You're not in a cpio-archive affiliated buffer." fname))
    (if *cab-parent*
	(buffer-file-name *cab-parent*)
      (buffer-file-name))))

(defun cpio-contents-buffer-name (name)
  "Return the name of the buffer that would/does hold the contents of entry NAME.
CAVEAT: Yes, there's a possibility of a collision here.
However, that would mean that you're editing
more than one archive, each containing entries of the same name
more than one of whose contents you are currently editing.
Run more than one instance of emacs to avoid such collisions."
  (let ((fname "cpio-contents-buffer-name"))
    ;; (format "%s (in cpio archive %s)" name (file-name-nondirectory (buffer-file-name *cab-parent*)))))
    name))
;;    (expand-file-name
;;	(concat name "!"))))

(defun cpio-create-entry-attrs (filename)
  "Create an entry attribute structure based on the given FILENAME."
  (let* ((fname "cpio-create-entry-attrs")
	 (attrs (file-attributes filename))

	 (ino (nth 10 attrs))
	 (mode (cpio-mode-string-to-int-mode (nth 8 attrs)))
	 (uid (nth 2 attrs))
	 (gid (nth 3 attrs))

	 (nlink 1)
	 (mtime (time-to-seconds (nth 5 attrs)))
	 (entry-size (nth 7 attrs))
	 (dev-maj (nth 11 attrs))

	 (dev-min 1)
	 (rdev-maj 0)
	 (rdev-min 0)
	 (namesize (length filename))

	 (chksum (cpio-make-chksum-for-file filename))

	 (result (make-vector 14 nil)))
    (aset result *cpio-ino-parsed-idx*	      ino)
    (aset result *cpio-mode-parsed-idx*	      mode)
    (aset result *cpio-uid-parsed-idx*	      uid)
    (aset result *cpio-gid-parsed-idx*	      gid)

    (aset result *cpio-nlink-parsed-idx*      nlink)
    (aset result *cpio-mtime-parsed-idx*      (seconds-to-time mtime))
    (aset result *cpio-entry-size-parsed-idx* entry-size)
    (aset result *cpio-dev-maj-parsed-idx*    dev-maj)

    (aset result *cpio-dev-min-parsed-idx*    dev-min)
    (aset result *cpio-rdev-maj-parsed-idx*   rdev-maj)
    (aset result *cpio-rdev-min-parsed-idx*   rdev-min)
    (aset result *cpio-namesize-parsed-idx*   namesize)

    (aset result *cpio-chksum-parsed-idx*     chksum)
    (aset result *cpio-name-parsed-idx*	      filename)

    result))

(defun cpio-make-chksum-for-file (filename)
  "Return the checksum for FILENAME."
  (let ((fname "cpio-make-chksum-for-file"))
    (funcall cpio-make-chksum-for-file-func filename)))

(defun cpio-create-faux-directory-attrs (name)
  "Create attributes appropriate for adding a directory entry to a cpio-archive.
CAVEAT: While many attributes are derived from a best guess of reality,
many are simply invented."
  (let* ((fname "cpio-create-faux-directory-attrs")
	 (local-attrs (file-attributes "."))
	 (ino 1)
	 ;; HEREHERE think about basing the mode on umask or local-attrs.
	 (mode (logior s-ifdir
		       s-irwxu
		       s-irusr
		       s-ixusr))
	 (uid (user-uid))
	 (gid (group-gid))

	 (nlink 1)
	 (now (current-time))
	 (mtime (list (nth 0 now) (nth 1 now)))

	 (entry-size 0)
	 (dev-maj 1)
	 (dev-min 1)
	 (rdev-maj 0)
	 (rdev-min 0)
	 (namesize (1+ (length name)))
	 (chksum 0)			;Checksum for a direcory is always 0.
	 (result (make-vector 14 nil)))
    (aset result *cpio-ino-parsed-idx*	      ino)
    (aset result *cpio-mode-parsed-idx*	      mode)
    (aset result *cpio-uid-parsed-idx*	      uid)
    (aset result *cpio-gid-parsed-idx*	      gid)

    (aset result *cpio-nlink-parsed-idx*      nlink)
    (aset result *cpio-mtime-parsed-idx*      mtime)
    (aset result *cpio-entry-size-parsed-idx* entry-size)
    (aset result *cpio-dev-maj-parsed-idx*    dev-maj)

    (aset result *cpio-dev-min-parsed-idx*    dev-min)
    (aset result *cpio-rdev-maj-parsed-idx*   rdev-maj)
    (aset result *cpio-rdev-min-parsed-idx*   rdev-min)
    (aset result *cpio-namesize-parsed-idx*   namesize)

    (aset result *cpio-chksum-parsed-idx*     chksum)
    (aset result *cpio-name-parsed-idx*	      name)

    result))

(defun cpio-entry-exists-p (name)
  "Return non-nil if there's already an entry called NAME
in the current archive."
  (let ((fname "cpio-entry-exists-p"))
    (unless (or (eq major-mode 'cpio-mode)
		(eq major-mode 'cpio-dired-mode))
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	    (cpio-entry-exists-p name))
      (assoc name (cpio-catalog)))))

(defun cpio-move-to-entry (entry-name)
  "Move the point to ENTRY-NAME."
  (let ((fname "cpio-move-to-entry")
	(where nil))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (not (looking-at-p (concat entry-name "$")))
	(cpio-dired-next-line 1))
      (if (looking-at-p (concat entry-name "$"))
	  (setq where (point))))
    (if where
	(goto-char where))))

;;
;; Functions about the modified state of a catalog entry.
;;
(defun cpio-set-entry-unmodified (catalog-entry)
  "Mark the given CATALOG-ENTRY as not modified."
  (let ((fname "cpio-set-entry-unmodified"))
    (cpio-validate-catalog-entry catalog-entry)
    (aset catalog-entry *cpio-catalog-entry-modified-flag-idx* 'cpio-mode-entry-unmodified)))

(defun cpio-set-entry-modified (catalog-entry)
  "Mark the given CATALOG-ENTRY as modified."
  (let ((fname "cpio-set-entry-modified"))
    (cpio-validate-catalog-entry catalog-entry)
    (aset catalog-entry *cpio-catalog-entry-modified-flag-idx* 'cpio-mode-entry-modified)))

(defun cpio-entry-modified-p (catalog-entry)
  "Return non-NIL if CATALOG-ENTRY is marked as modified."
  (let ((fname "cpio-entry-modified-p")
	(modified-flag))
    (cpio-validate-catalog-entry catalog-entry)
    (cond ((eq 'cpio-mode-modified
	       (setq modified-flag (aref catalog-entry *cpio-catalog-entry-modified-flag-idx*)))
	   t)
	  ((eq 'cpio-mode-unmodified catalog-entry)
	   nil)
	  (t
	   (error "%s(): Invalid modified flag value [[%s]]." fname modified-flag)))))

(defun cpio-validate-catalog-entry (catalog-entry)
  "Verify that the given CATALOG-ENTRY is (could be) a valid catalog entry.
Signal an error if it isn't."
  (let ((fname "validate-catalog-entry")
	(modified-flag))
    (unless (vectorp catalog-entry)
      (signal 'wrong-type-error (list catalog-entry)))
    (unless (= *cpio-catalog-entry-length* (length catalog-entry))
      (error "%(): The catalog entry [[%d]] is not the right length." fname *cpio-catalog-entry-length*))
    (unless (vectorp (aref catalog-entry *cpio-catalog-entry-attrs-idx*))
      (signal 'wrong-type-error (list catalog-entry)))
    (unless (= *cpio-parsed-header-length* (length (aref catalog-entry *cpio-catalog-entry-attrs-idx*)))
      (error "%s(): The parsed header in [[%s]] is not the right length." fname (aref catalog-entry *cpio-catalog-entry-attrs-idx*))
      (sit-for 1)
      (error "%s(): Found [[%d]], expected [[%d]]." fname (length (aref catalog-entry *cpio-catalog-entry-attrs-idx*)) *cpio-parsed-header-length*))
    (unless (and (markerp (aref catalog-entry *cpio-catalog-entry-header-start-idx*))
		 (markerp (aref catalog-entry *cpio-catalog-entry-contents-start-idx*)))
      (error "%s(): The marker fields in [[%s]] are not markers." fname catalog-entry))
    ;; The modified flag may not be set yet, so ignore it.
    ))

;; A few functions for a failed attempt at supporting different encodings.

;; Modified from (set-auto-coding) ∈ mule.el.
(defun cpio-set-auto-coding (contents)
  "Return coding system for the current buffer.
See `cpio-find-auto-coding' for how the coding system is found.
Return nil if an invalid coding system is found.

The variable `set-auto-coding-function' (which see) is set to this
function by default."
  (let ((found (cpio-find-auto-coding cpio-entry-name contents)))
    (if (and found (coding-system-p (car found)))
	(car found))))

;; Modified from (find-auto-coding) ∈ mule.el.
(defun cpio-find-auto-coding (entry-name contents)
  "Find a coding system for an archive entry ENTRY-NAME.

The function checks ENTRY-NAME against the variable `auto-coding-alist'.
If ENTRY-NAME doesn't match any entries in the variable, it checks the
contents of the current buffer following point against
`auto-coding-regexp-alist'.  If no match is found, it checks for a
`coding:' tag in the first one or two lines following point.  If no
`coding:' tag is found, it checks any local variables list in the last
3K bytes out of the SIZE bytes.	 Finally, if none of these methods
succeed, it checks to see if any function in `auto-coding-functions'
gives a match.

If a coding system is specified, the return value is a cons
\(CODING . SOURCE), where CODING is the specified coding system and
SOURCE is a symbol `auto-coding-alist', `auto-coding-regexp-alist',
`:coding', or `auto-coding-functions' indicating by what CODING is
specified.  Note that the validity of CODING is not checked;
it's the caller's responsibility to check it.

If nothing is specified, the return value is nil."
  (error "%s() is not yet implemented" "cpio-find-auto-coding")
  (or (let ((coding-system (auto-coding-alist-lookup entry-name)))
	(if coding-system
	    (cons coding-system 'auto-coding-alist)))
      ;; Try using `auto-coding-regexp-alist'.
      (let ((coding-system (cpio-auto-coding-regexp-alist-lookup contents)))
	(if coding-system
	    (cons coding-system 'auto-coding-regexp-alist)))
      (let* ((case-fold-search t)
	     (size (length contents))
	     (head-start 0)
	     (head-end (+ head-start (min size 1024)))
	     (head (substring contents head-start head-end))
	     coding-system head-found tail-found char-trans)
	;; Try a short cut by searching for the string "coding:"
	;; and for "unibyte:".
	(setq head-found (or (string-match "coding:" contents)
			     (string-match "unibyte:" contents)
			     (string-match "enable-character-translation:" contents)))

	;; At first check the head.
	(when head-found
	  ;; (goto-char head-start)
	  ;; (setq head-end (set-auto-mode-1))
	  ;; (setq head-start (point))
	  (when (and head-end (< head-found head-end))
	    (goto-char head-start)
	    (when (and set-auto-coding-for-load
		       (string-match
			"\\(.*;\\)?[ \t]*unibyte:[ \t]*\\([^ ;]+\\)"
			contents))
	      (display-warning 'mule
			       (format "\"unibyte: t\" (in %s) is obsolete; \
use \"coding: 'raw-text\" instead."
				       (file-relative-name entry-name))
			       :warning)
	      (setq coding-system 'raw-text))
	    (when (and (not coding-system)
		       (string-match
			"\\(.*;\\)?[ \t]*coding:[ \t]*\\([^ ;]+\\)"
			contents)
	      (setq coding-system (intern (match-string 2 contents))))
	    (when (string-match
		   "\\(.*;\\)?[ \t]*enable-character-translation:[ \t]*\\([^ ;]+\\)"
		   contents)
	      (setq char-trans (match-string 2 contents)))))

	(if coding-system
	    ;; If the coding-system name ends with "!", remove it and
	    ;; set char-trans to "nil".
	    (let ((name (symbol-name coding-system)))
	      (if (= (aref name (1- (length name))) ?!)
		  (setq coding-system (intern (substring name 0 -1))
			char-trans "nil"))))
	(when (and char-trans
		   (not (setq char-trans (intern char-trans))))
	  (make-local-variable 'enable-character-translation)
	  (setq enable-character-translation nil))
	(if coding-system
	    (cons coding-system :coding)))
	;; Finally, try all the `auto-coding-functions'.
	(let ((funcs auto-coding-functions)
	      (coding-system nil))
	  (while (and funcs (not coding-system))
	    (setq coding-system (ignore-errors
				  (save-excursion
				    (goto-char (point-min))
				    (funcall (pop funcs) size))))))
	(if coding-system
	    (cons coding-system 'auto-coding-functions)))))

(defun cpio-auto-coding-regexp-alist-lookup (contents)
  "Lookup `auto-coding-regexp-alist' for CONTENTS.
The value is a coding system is specified for the CONTENTS
or nil."
  (let ((fname "cpio-auto-coding-regexp-alist-lookup")
	(alist auto-coding-regexp-alist)
	(coding-system))
    (error "%s() is not yet implemented" fname)
    (while (and alist (not coding-system))
      (let ((regexp (car (car alist))))
	(if enable-multibyte-characters
	    (setq regexp (string-to-multibyte regexp)))
	(if (string-match regexp contents)
	    (setq coding-system (cdr (car alist)))
	  (setq alist (cdr alist)))))
    coding-system))

(defun cpio-set-coding-system (entry-name)
  "Set the coding system for the current buffer based on the contents of the entry-ENTRY-NAME."
  (let ((fname "cpio-set-coding-system"))
    (error "%s() is not yet implemented" fname)
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-set-coding-system entry-name))
      ;; (setq last-coding-system-used
      ;;       (car (find-coding-systems-region (cpio-contents-start entry-name)
      ;;					(cpio-contents-end   entry-name))))
      (set-buffer-file-coding-system last-coding-system-used 'force 'nomodify))))

(defun cpio-not-modified ()
  "Mark the current cpio-dired-style and archive buffersfas unmodified."
  (let ((fname "cpio-not-modified"))
    (cond (*cab-parent*
	   (not-modified)
	   (with-current-buffer *cab-parent* (not-modified)))
	  (t
	   (not-modified)
	   (mapc (lambda (b)
		   (if (buffer-live-p b)
		       (with-current-buffer b
			 (not-modified))))
		 *cab-subordinates*)))))

;;
;; Commands
;;

(defun cpio-view-dired-style-buffer ()
  "Switch to the dired style buffer corresponding to this archive buffer."
  ;; (cpio-dired-buffer-name) is in cpio-mode.el because
  ;; it is invoked in the archive's directory.
  (interactive)
  (let ((fname "cpio-view-dired-style-buffer")
	(archive-file-name (buffer-file-name)))
    (unless (eq major-mode 'cpio-mode)
      (error "%s(): You're not in a cpio archive buffer under cpio-mode." fname))
    (bury-buffer)
    (switch-to-buffer (cpio-dired-buffer-name archive-file-name))))


;;
;; Mode definition
;;

;; HEREHERE I'm hoping dired-mode gives me decent stuff for free.
;; dired-mode -- Nope the hooks for dired-mode want a nicer environment.
;; I'll have to see how tar-mode does it.

;;;###autoload
(define-derived-mode cpio-mode fundamental-mode "cpio-mode"
  "Treat cpio archives like file systems with a dired UI."
  (if (null (setq *cpio-format* (cpio-discern-archive-type)))
      (error "You're not in a supported CPIO buffer. It begins [[%s]]." (buffer-substring-no-properties 1 8)))

  ;;
  ;; HEREHERE Get rid of this once things look clean.
  ;;
  (cpio-backup-during-development)
  ;;
  ;; EO temporary code for development
  ;;
  
  (let ((archive-buffer (current-buffer))
	(cpio-dired-buffer))
    ;; You really only need this for the binary archive formats,
    ;; but, hey it's cheap to set it.
    (set-syntax-table *cpio-archive-syntax-table*)
    (setq buffer-read-only t)
    (cpio-set-locals *cpio-format*)
    (setq *cpio-archive-name* (buffer-file-name))
    (cpio-build-catalog)
    (with-current-buffer (setq cpio-dired-buffer
			       (cpio-present-ala-dired (current-buffer)))
      (make-local-variable '*cpio-archive-name*)
      (cpio-dired-set-unmodified))
    (cpio-create-keymap)
    (bury-buffer)
    ;; cpio-mode is the top level function here,
    ;; so this should control what we see at this point.
    (switch-to-buffer cpio-dired-buffer)))

;;
;; HEREHERE Get rid of this once things look clean.
;;
(defun cpio-backup-during-development ()
  "Create a time-stamped backup of the file in the current-buffer.
There's an implied CONTRACT there:
The buffer must contain a file."
  (let* ((fname "cpio-backup-during-development")
	 (filename (buffer-file-name))
	 (backup-file (format "%s-%s"
			      filename
			      (format-time-string "%Y%m%d%H%H%M%S.%3N"))))
    (copy-file filename backup-file nil 'keep-time 'preserve-uid-gid 'preserve-permissions)))

;;
;; EO temporary code for development
;;


(defvar *cpio-dired-modified* nil
  "A flag to record if any archive-modifying events have occured
since either the beginning or the last save.")
(setq *cpio-dired-modified* nil)

(make-variable-buffer-local '*cpio-dired-modified*)

(defun cpio-dired-modified-p ()
  "Return non-NIL if the catalog has been modified
and, thus, the archive can be saved."
  (let ((fname "cpio-dired-modified-p"))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): only makes sense in a cpio-dired buffer." fname))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-dired-modified-p))
      *cpio-dired-modified*)))

(defun cpio-dired-set-modified ()
  "Flag the catalog as modified."
  (let ((fname "cpio-dired-set-modified"))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): only makes sense in a cpio-dired buffer." fname))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-dired-set-modified))
      (setq *cpio-dired-modified* t))))

(defun cpio-dired-set-unmodified ()
  "Flag the catalog as not modified."
  (let ((fname "cpio-dired-set-unmodified"))
    ;; HEREHERE There's probably more to this than just the following.
    ;; For example, should it look for entry-contents buffers and
    ;; offer to save them if they're modified?
    ;; What about upon killing the archive buffer?
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): only makes sense in a cpio-dired buffer." fname))
    (cond (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (cpio-dired-set-unmodified)))
	  (t
	   (setq *cpio-dired-modified* nil)
	   (set-buffer-modified-p nil)
	   (with-current-buffer (cpio-dired-buffer-name (buffer-name))
	     (set-buffer-modified-p nil))))))

(defvar *cpio-have-made-keymap* nil
  "Flag to indicate that the cpio-mode-map has already been built.")
(setq *cpio-have-made-keymap* nil)

(defun cpio-create-keymap ()
  (let ((fname "cpio-create-keymap")
	(keymap (make-keymap)))
    (setq cpio-mode-map keymap)
    (unless *cpio-have-made-keymap*
      (define-key cpio-mode-map "\C-c\C-c" 'cpio-view-dired-style-buffer)
      (define-key cpio-mode-map	       "q" 'cpio-quit))))

(defun cpio-quit ()
  "Quit cpio mode and kill all the affiliated buffers."
  (interactive)
  (let ((fname "cpio-quit"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-quit)
	  (unless (kill-buffer)
	    (warn "%s(): Could not kill [[%s]]." fname (current-buffer)))))))

(defun cpio-entry (entry-name)
  "Return the entry in the catalog for the entry with ENTRY-NAME."
  (let ((fname "cpio-entry"))
    (if *cab-parent*
	(cdr (assoc entry-name (with-current-buffer *cab-parent*
				 *cpio-catalog*)))
      (cdr (assoc entry-name *cpio-catalog*)))))

(defun cpio-entry-attrs-from-catalog-entry (catalog-entry)
  "Do that."
  (let ((fname "cpio-entry c-attrs-from-catalog"))
    (aref catalog-entry *cpio-catalog-entry-attrs-idx*)))

(defun cpio-build-catalog ()
  "Build the catalog that tracks the entries in this cpio-mode buffer.
cpio-mode maintains the catalog in the *cpio-catalog* variable."
  (let ((fname "cpio-build-catalog"))
    (goto-char (point-min))
    (setq *cpio-catalog* (funcall cpio-build-catalog-func))))

(defun cpio-set-locals (archive-type)
  "Establish certain variables as local to the current buffer and give them good values.
ARCHIVE-TYPE is a symbol."
  (let ((fname "cpio-set-locals"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-set-local-vars archive-type))
      (cpio-set-local-vars archive-type)
      (cpio-set-local-funcs archive-type))))

(defun cpio-set-local-funcs (archive-type)
  "Establish the functions for the given archive type.
The functions are assigned to the elements of *cpio-local-funcs*,
a string of symbols.
Thus, to use them as functions
you need to (funcall) them or to (apply) them.
CAVEAT: No checking is done.
This function doesn't care /why/ you are asking for functions
that are appropriate for ARCHIVE-TYPE.
That's the caller's business.

See *cpio-local-funcs* for more information."
  (let ((fname "cpio-set-local-funcs")
	(archive-type-name (symbol-name archive-type)))
    (mapc 'make-local-variable
	  *cpio-local-funcs*)
    ;; Here is the process:
    ;; cpio-do-good-stuff-func
    ;; --> "cpio-do-good-stuff-func"
    ;; --> "cpio" "do" "good" "stuff"
    ;; --> "cpio-fmt-do-good-stuff"
    ;; --> cpio-fmt-do-good-stuff
    ;; (setq cpio-do-good-stuff-func cpio-hdr-do-good-stuff)
    ;; Here's an example of the desired result for newc headers.
    ;; (setq cpio-parse-header-func 'cpio-newc-parse-header)
    (mapc (lambda (local-func-var)
	    (let* ((name-parts (split-string (symbol-name local-func-var) "-"))
		   (target-name (concat (car name-parts) "-" ;HEREHERE Should this be (pop)?
					(symbol-name archive-type))))
	      (setq name-parts (cdr (remove "func" name-parts)))
	      (mapc (lambda (part)
		      (setq target-name (concat target-name "-" part)))
		    name-parts)
	      ;; 1. (set) not (setq) because
	      ;; 2. local-func-var holds a symbol)
	      (set local-func-var (read target-name))
	      target-name))
	  *cpio-local-funcs*)))

(defun cpio-set-local-vars (archive-type)
  "Set all the necessary local variables for the CPIO archive type given."
  (let ((fname "cpio-set-local-vars"))
    ;; Some variables are not format-specific.
    (make-local-variable '*cpio-catalog*)
    (setq *cpio-catalog* ())
    (setq *cpio-archive-name* (buffer-file-name))
    ;; Now for the format-specific variables.
    (cond ((eq archive-type 'bin)
	   (cpio-set-local-bin-vars))
	  ((eq archive-type 'newc)
	   (cpio-set-local-newc-vars))
	  ((eq archive-type 'odc)
	   (cpio-set-local-odc-vars))
	  ((eq archive-type 'crc)
	   (cpio-set-local-crc-vars))
	  ((eq archive-type 'tar)
	   (cpio-set-local-tar-vars))
	  ((eq archive-type 'ustar)
	   (cpio-set-local-ustar-vars))
	  ((eq archive-type 'hpbin)
	   (cpio-set-local-hpbin-vars))
	  ((eq archive-type 'hpodc)
	   (cpio-set-local-hpodc-vars))
	  (t (error "%s(): Unknown archive type [[%s]]" fname archive-type)))))

(defun cpio-set-local-bin-vars ()
  "Set buffer local variables appropriate for a BIN format CPIO archive."
  (let ((fname "cpio-set-local-bin-vars"))
    (make-local-variable '*cpio-padding-modulus*)
    (setq *cpio-padding-modulus* *cpio-bin-padding-modulus*)
    (make-local-variable '*cpio-padding-char*)
    (setq *cpio-padding-char* *cpio-bin-padding-char*)
    (make-local-variable '*cpio-padding-str*)
    (setq *cpio-padding-str* *cpio-bin-padding-str*)
    (make-local-variable '*cpio-header-length*)
    (setq *cpio-header-length* *cpio-bin-header-length*)))

(defun cpio-set-local-newc-vars ()
  "Set buffer local variables appropriate for a NEWC format CPIO archive."
  (let ((fname "cpio-set-local-newc-vars"))
    (make-local-variable '*cpio-padding-modulus*)
    (setq *cpio-padding-modulus* *cpio-newc-padding-modulus*)
    (make-local-variable '*cpio-padding-char*)
    (setq *cpio-padding-char* *cpio-newc-padding-char*)
    (make-local-variable '*cpio-padding-str*)
    (setq *cpio-padding-str* *cpio-newc-padding-str*)
    (make-local-variable '*cpio-header-length*)
    (setq *cpio-header-length* *cpio-newc-header-length*)))

(defun cpio-set-local-odc-vars ()
  "Set buffer local variables appropriate for a ODC format CPIO archive."
  (let ((fname "cpio-set-local-odc-vars"))
    (make-local-variable '*cpio-padding-modulus*)
    (setq *cpio-padding-modulus* *cpio-odc-padding-modulus*)
    (make-local-variable '*cpio-padding-char*)
    (setq *cpio-padding-char* *cpio-odc-padding-char*)
    (make-local-variable '*cpio-padding-str*)
    (setq *cpio-padding-str* *cpio-odc-padding-str*)
    (make-local-variable '*cpio-header-length*)
    (setq *cpio-header-length* *cpio-odc-header-length*)))

(defun cpio-set-local-crc-vars ()
  "Set buffer local variables appropriate for a CRC format CPIO archive."
  (let ((fname "cpio-set-local-crc-vars"))
    (make-local-variable '*cpio-padding-modulus*)
    (setq *cpio-padding-modulus* *cpio-crc-padding-modulus*)
    (make-local-variable '*cpio-padding-char*)
    (setq *cpio-padding-char* *cpio-crc-padding-char*)
    (make-local-variable '*cpio-padding-str*)
    (setq *cpio-padding-str* *cpio-crc-padding-str*)
    (make-local-variable '*cpio-header-length*)
    (setq *cpio-header-length* *cpio-crc-header-length*)))

(defun cpio-set-local-tar-vars ()
  "Set buffer local variables appropriate for a TAR format CPIO archive."
  (let ((fname "cpio-set-local-tar-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-ustar-vars ()
  "Set buffer local variables appropriate for a USTAR format CPIO archive."
  (let ((fname "cpio-set-local-ustar-vars"))
x    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-hpbin-vars ()
  "Set buffer local variables appropriate for a HPBIN format CPIO archive."
  (let ((fname "cpio-set-local-hpbin-vars"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-set-local-hpodc-vars ()
  "Set buffer local variables appropriate for a HPODC format CPIO archive."
  (let ((fname "cpio-set-local-hpodc-vars"))
    (error "%s() is not yet implemented" fname)))


(provide 'cpio-mode)
;;; cpio-mode.el ends here

;;; notes-variables.el --- Configuration variables for notes-mode

;; Copyright (C) 1994-2000,2012  Free Software Foundation, Inc.

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

;;
;; This file lists all parameters you might wish to change in
;; notes{-index,}-mode.  The best way to handle this in your
;; .emacs file is to do
;;	(require 'notes-variables)
;;	(setq your-variable-to-change 'your-new value)
;;

;;; Code:

;; xxx: if part of emacs, this should be probably be set to exec-directory (?)
(defconst notes-utility-dir (file-name-directory load-file-name)
  "Location of notes utility programs.")

;;
;; Notice:  several notes parameters are defined in your
;; ~/.notesrc file.  These are not specified here.
;; See mkconfig for details.
;; We fetch them here.
;;
;; To make this fast, we cache the configuration in a .notesrc.el
;; file.  We only have to invoke mkconfig when that file is out-of-date.
;; This optimization is very important because notes-variables is
;; required every time emacs is started.
;;
(save-current-buffer
  (if (null (file-exists-p (expand-file-name "mkconfig" notes-utility-dir)))
      (error "Notes-mode is incorrectly installed."))
  (let*
      ((source-file (expand-file-name "~/.notesrc"))
       (cache-file (expand-file-name "~/.notesrc.el")))
    (if (and
	 (not (file-exists-p source-file))
	 (not noninteractive))
	(progn
	  (require 'notes-first)
	  (notes-first-use-init)))
    (with-temp-buffer
      (if (and ;; Requirements for a valid cache-file.
           (file-exists-p cache-file)
           (if (file-exists-p source-file)
               (file-newer-than-file-p cache-file source-file)
             t)
           (file-newer-than-file-p
            cache-file (expand-file-name "mkconfig" notes-utility-dir)))
          (insert-file-contents cache-file) ;; Cache is up-to-date.
        ;; Otherwise, refresh the cache.
        (call-process (expand-file-name "mkconfig" notes-utility-dir)
                      nil t nil "elisp")
        (write-region (point-min) (point-max) cache-file)
        (set-file-modes cache-file #o644)) ;; Protect it => mode 0644.
      (eval-buffer))))

;; notes-int-glob and notes-file-glob should have been set in ~/.notesrc.el.
(add-to-list 'auto-mode-alist
             (cons
              ;; FIXME: auto-mode-alist actually takes a regexp, not a glob.
              ;; The default globs happen to fall within the intersection of
              ;; regexps and globs, but we shouldn't rely on it!
              (concat notes-int-glob "/" notes-file-glob ".?\\'")
              'notes-mode))

;;; xxx: most of these should use defcustom or something similar, I presume.
(defvar notes-w3-alternate-url 'browse-url
  "* A function to call when notes-w3-url cannot handle a complex URL.
It now goes through the Emacs `browse-url' package,
but you could also set it manually (say, to w3-fetch).")

(defvar notes-use-font-lock t
  "* Enable notes fontification.")

(defvar notes-use-outline-mode t
  "* Enable `outline-minor-mode' in all notes buffers?")

(defvar notes-index-fontify-dates nil
  "* Fontify dates in notes-index-mode.
Turning this off for large notes-index's can improve performance.")

(defvar notes-bold-face 'notes-bold-face
  "* Face to use for notes-index-mode and notes-mode subjects.
The default face is copied from 'bold.")

(defvar notes-font-lock-keywords
  '(("^\\* .*$" . notes-bold-face)
    ("^\\-+$" . notes-bold-face)
    ;; ("^[0-9]+\\-[A-Za-z]+\\-[0-9]+ [A-Za-z]+$" . font-lock-bold-face)
    ;; NEEDSWORK:  should also highlight URLs, maybe?
   )
  "* Font-lock keywords for notes mode.")

(defvar notes-index-font-lock-keywords
  '(("^[^:]*:" . notes-bold-face)
    ("\\<[0-9]*\\>" . mouse-face)
   )
  "* Font-lock keywords for notes-index mode.")

(defvar notes-mode-complete-subjects t
  "* Enable subject completion in notes mode?")

(defvar notes-w3-follow-link-mouse-other-window t
  "* Should notes-w3-follow-link-mouse open another window?")

(defvar notes-subject-table nil
  "List of notes-subjects needed for subject completion.
Reloaded by loading the notes-index file.")

(defvar notes-mode-initialization-program "mknew"
  "Program to run to initialize a new notes file.  Must be in notes-bin-dir.
If nil, no initialization is done.")

(defvar notes-encryption-key-id nil
  "Keyid of PGP key for the current user.
Useful if your \\[user-full-name] doesn't match a unique key.
Should have a leading 0x.")

(defvar notes-electric-prevnext 2
  "Amount of electricity in prevnext for notes-mode.
nil: don't auto-update anything.
1: update prevnext, but don't autosave the old buffer
2: update prevnext and autosave the old buffer.")

(if (featurep 'xemacs)
    (require 'notes-xemacs)
  (require 'notes-emacs))

(defvar notes-platform-inited nil
  "Have we inited our platform (XEmacs/Emacs)?")

(run-hooks 'notes-variables-load-hooks)
(provide 'notes-variables)
;;; notes-variables.el ends here

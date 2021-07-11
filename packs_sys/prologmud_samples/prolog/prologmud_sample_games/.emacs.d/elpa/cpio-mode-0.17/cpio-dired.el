;;; cpio-dired.el --- UI definition à la dired. -*- coding: utf-8 -*-

;; COPYRIGHT

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
;; Created: 2017 Dec 01
;; Version: 0.17
;; Keywords: files

;;; Commentary:

;;; Documentation:

;;; Some design principles:

;; • UI taken from dired.
;; • Modifications happen to the catalog
;;   unless there is an element
;;   that requires modifying the archive itself.
;;   An example that includes both the archive and catalog
;;   is adding a file to the archive.

;;; Code:


;;
;; Hacks
;;

(defun snarf-defuns ()                  ;FIXME: Namespace!
  "Return a list of the defuns in the visible porition of the buffer.
Keep any preceding comments."
  (let ((fname "snarf-defuns")
	(results ())
	(start)
	(end))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(defun \\([[:graph:]]+\\) " (point-max) t)
	(setq start (match-beginning 0))
	(mark-defun)
	(setq end (mark))
	(forward-line -1)
	(while (and (> (point) (point-min))
		    (looking-at ";"))
	  (setq start (point))
	  (forward-line -1))
	(push (buffer-substring-no-properties start end) results)
	(goto-char end)))
    results))

(defun sort-defuns (defuns)             ;FIXME: Namespace!
  "Return a copy of the given list of DEFUNS sorted by name."
  (let ((fname "sort-defuns")
	(sortable-list)
	(sorted-list))
    (setq sortable-list
	  (mapcar (lambda (d)
		    (let ((defun-name (save-match-data
					(and (string-match "(defun \\([[:graph:]]+\\) " d)
					     (match-string-no-properties 1 d)))))
		      (cons defun-name d)))
		  defuns))
    (setq sorted-list (sort sortable-list (lambda (l r)
					    (string-lessp (car l) (car r)))))
    (mapcar 'cdr sorted-list)))

(defun sort-defuns-in-buffer ()         ;FIXME: Namespace!
  "Replace the visible portion of the current buffer with its defuns, but sorted."
  (interactive)
  (let ((fname "sort-defuns-in-buffer")
	(defuns (sort-defuns (snarf-defuns))))
    (delete-region (point-min) (point-max))
    (mapc (lambda (d)
	    (insert d "\n"))
	  defuns)))


;;
;; Dependencies
;;

(eval-when-compile (require 'cpio-generic)) ;For `with-writable-buffer'!
(require 'dired)
(require 'dired-aux)

;;
;; Vars
;;

;;;;;;;;;;;;;;;;
;; Make the byte compiler happy.
(defvar *cpio-search-entries*)
(defvar *cpio-search-entry*)
(defvar *cpio-search-point*)
(defvar *cpio-search-re*)
(defvar cpio-dired-set-modified)
(defvar cpio-dired-hide-details-mode)
(defvar *cpio-catalog*)
(defvar *cpio-padding-modulus*)
(defvar *cpio-catalog-entry-length*)
(defvar *cpio-catalog-entry-attrs-idx*)
(defvar *cpio-catalog-entry-contents-start-idx*)
(defvar *cpio-catalog-entry-header-start-idx*)
(declare-function cpio-adjust-trailer "cpio-mode.el")
(declare-function cpio-catalog "cpio-mode.el")
(declare-function cpio-contents "cpio-mode.el")
(declare-function cpio-contents-start "cpio-mode.el")
(declare-function cpio-create-entry-attrs "cpio-mode.el")
(declare-function cpio-create-faux-directory-attrs "cpio-mode.el")
(declare-function cpio-delete-trailer "cpio-mode.el")
(declare-function cpio-dev-maj "cpio-mode.el")
(declare-function cpio-dev-maj-to-dev-maj-string "cpio-mode.el")
(declare-function cpio-dev-min "cpio-mode.el")
(declare-function cpio-dev-min-to-dev-min-string "cpio-mode.el")
(declare-function cpio-dired-modified-p "cpio-dired.el")
(declare-function cpio-dired-set-modified "cpio-dired.el")
(declare-function cpio-dired-set-unmodified "cpio-dired.el")
(declare-function cpio-entry "cpio-mode.el")
(declare-function cpio-entry-attrs "cpio-mode.el")
(declare-function cpio-entry-contents-start "cpio-mode.el")
(declare-function cpio-entry-exists-p "cpio-mode.el")
(declare-function cpio-entry-header-start "cpio-mode.el")
(declare-function cpio-entry-name "cpio-mode.el")
(declare-function cpio-entry-name-to-entry-name-string "cpio-mode.el")
(declare-function cpio-entry-size "cpio-mode.el")
(declare-function cpio-extract-all "cpio-mode.el")
(declare-function cpio-filesize-to-filesize-string "cpio-mode.el")
(declare-function cpio-find-entry "cpio-mode.el")
(declare-function cpio-gid "cpio-mode.el")
(declare-function cpio-gid-to-gid-string "cpio-mode.el")
(declare-function cpio-insert-padded-contents "cpio-mode.el")
(declare-function cpio-insert-trailer "cpio-mode.el")
(declare-function cpio-make-header-string "cpio-mode.el")
(declare-function cpio-mode "cpio-mode.el")
(declare-function cpio-mode-value "cpio-mode.el")
(declare-function cpio-move-to-entry "cpio-mode.el")
(declare-function cpio-mtime "cpio-mode.el")
(declare-function cpio-mtime-to-mtime-string "cpio-mode.el")
(declare-function cpio-nlink "cpio-mode.el")
(declare-function cpio-nlink-to-nlink-string "cpio-mode.el")
(declare-function cpio-numeric-entry-type "cpio-mode.el")
(declare-function cpio-set-contents-start "cpio-mode.el")
(declare-function cpio-set-entry-name "cpio-mode.el")
(declare-function cpio-set-entry-unmodified "cpio-mode.el")
(declare-function cpio-set-gid "cpio-mode.el")
(declare-function cpio-set-mode "cpio-mode.el")
(declare-function cpio-set-uid "cpio-mode.el")
(declare-function cpio-sort-catalog "cpio-mode.el")
(declare-function cpio-uid "cpio-mode.el")
(declare-function cpio-uid-to-uid-string "cpio-mode.el")
;; EO byte compiler code.
;;;;;;;;;;;;;;;;



(defvar *cpio-dired-permission-flags-regexp* dired-permission-flags-regexp
  "Regular expression to match the permission flags in `ls -l'.")

;; (defvar dired-sort-by-date-regexp
;;   (concat "\\(\\`\\| \\)-[^- ]*t"
;;	  ;; `dired-ls-sorting-switches' after -t overrides -t.
;;	  "[^ " dired-ls-sorting-switches "]*"
;;	  "\\(\\(\\`\\| +\\)\\(--[^ ]+\\|-[^- t"
;;	  dired-ls-sorting-switches "]+\\)\\)* *$")
;;   "Regexp recognized by Dired to set `by date' mode.")

;; (defvar dired-sort-by-name-regexp
;;   (concat "\\`\\(\\(\\`\\| +\\)\\(--[^ ]+\\|"
;;	  "-[^- t" dired-ls-sorting-switches "]+\\)\\)* *$")
;;   "Regexp recognized by Dired to set `by name' mode.")

;; (defvar dired-sort-inhibit nil
;;   "Non-nil means the Dired sort command is disabled.
;; The idea is to set this buffer-locally in special Dired buffers.")

(defvar *mon-re* (concat "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|"
			 "jul\\|aug\\|sep\\|oct\\|nov\\|dec"))
(setq *mon-re* (concat "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|"
		       "jul\\|aug\\|sep\\|oct\\|nov\\|dec"))

(defvar *cpio-dired-date-time-regexp* ()
  "RE to match the date/time field in ls -l.")
(setq *cpio-dired-date-time-regexp*  (concat "\\(?:"
					     *mon-re*
					     "\\)"
					     "\\s-+"
					     "[[:digit:]]\\{2\\}"
					     "\\s-+"
					     "\\(?:"
					     "[[:digit:]]\\{2\\}"
					     ":"
					     "[[:digit:]]\\{2\\}"
					     "\\|"
					     "[[:digit:]]\\{4\\}"
					     "\\)"))

(defvar *cpio-dired-inner-entry-regexp* (concat "\\("
						"[-dpstrwx]\\{10\\}"
						"\\)"
						"\\s-+"
						"[[:digit:]]+" ;nlinks
						"\\s-+"
						"\\("
						"[[:alnum:]]+" ;user
						"\\)"
						"\\s-+"
						"\\("
						"[[:alnum:]]+" ;group
						"\\)"

						"\\s-+"
						"[[:digit:]]+" ;filesize
						"\\s-+"
						"\\("
						*cpio-dired-date-time-regexp*
						"\\)"
						"\\s-+"
						"\\("
						"[[:graph:]]+"
						"\\)")
  "Regular expression to match the \"ls -l\" portion of an entry's line.")
(setq *cpio-dired-inner-entry-regexp* (concat "\\("
					      "[-dpstrwx]\\{10\\}"
					      "\\)"
					      "\\s-+"
					      "[[:digit:]]+" ;nlinks
					      "\\s-+"
					      "\\("
					      "[[:alnum:]]+" ;user
					      "\\)"
					      "\\s-+"
					      "\\("
					      "[[:alnum:]]+" ;group
					      "\\)"

					      "\\s-+"
					      "[[:digit:]]+" ;filesize
					      "\\s-+"
					      "\\("
					      *cpio-dired-date-time-regexp*
					      "\\)"
					      "\\s-+"
					      "\\("
					      "[[:graph:]]+"
					      "\\)"))

(defvar *cpio-dired-entry-regexp* (concat ".."
					  "\\("
					      *cpio-dired-permission-flags-regexp*
					  "\\)"
					  "\\s-+"
					  "[[:digit:]]+" ;nlinks
					  "\\s-+"
					  "\\("
					      "[[:alnum:]]+" ;user
					  "\\)"
					  "\\s-+"
					  "\\("
					      "[[:alnum:]]+" ;group
					  "\\)"
					  "\\s-+"
					  "[[:digit:]]+" ;filesize
					  "\\s-+"
					  "\\("
					      *cpio-dired-date-time-regexp*
					  "\\)"
					  "\\s-+"
					  "\\("
					      "[[:graph:]]+"
					  "\\)")
  "Regular expression to match an entry's line in cpio-dired-mode")
(setq *cpio-dired-entry-regexp* (concat ".."
					*cpio-dired-inner-entry-regexp*))

(defvar *cpio-dired-mode-idx*      1
  "Index of the mode match in *cpio-dired-entry-regexp*.")
(setq *cpio-dired-mode-idx* 1)

(defvar *cpio-dired-user-idx*      2
  "Index of the user match in *cpio-dired-entry-regexp*.")
(setq *cpio-dired-user-idx* 2)

(defvar *cpio-dired-group-idx*     3
  "Index of the group match in *cpio-dired-entry-regexp*.")
(setq *cpio-dired-group-idx* 3)

(defvar *cpio-dired-date/time-idx* 4
  "Index of the date/time match in *cpio-dired-entry-regexp*.")
(setq *cpio-dired-date/time-idx* 4)

(defvar *cpio-dired-name-idx*      5
  "Index of the entry name match in *cpio-dired-entry-regexp*.")
(setq *cpio-dired-name-idx* 5)

(defconst cpio-dired-marker-char ?*		; the answer is 42
  ;; so that you can write things like
  ;; (let ((cpio-dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In cpio-dired, the current mark character.
This is what the do-commands look for, and what the mark-commands store.")
(defconst cpio-dired-marker-str (char-to-string cpio-dired-marker-char)
  "In cpio-dired, a string corresponding to cpio-dired-marker-char.")

(defvar cpio-dired-del-marker ?D
  "Character used to flag entries for deletion.")
(setq cpio-dired-del-marker ?D)

(defvar cpio-dired-del-str (char-to-string cpio-dired-del-marker)
  "In cpio-dired, a string corresponding to cpio-dired-del-marker.")
(setq cpio-dired-del-str (char-to-string cpio-dired-del-marker))

;; HEREHERE dired-keep-marker-copy is customizable.
;; Should it be here too?
(defvar cpio-dired-keep-marker-copy ?C
  "Character used to flag entries for copying.")
(setq cpio-dired-keep-marker-copy ?C)

(defvar cpio-dired-keep-marker-copy-str ?C
  "In cpio-dired, a string corresponding to cpio-dired-keep-marker-copy.")
(setq cpio-dired-keep-marker-copy-str ?C)


;; HEREHERE dired-keep-marker-rename is customizable.
;; Should it be here too?
(defvar cpio-dired-keep-marker-rename ?R
  "Character used to flag entries for renaming.")
(setq cpio-dired-keep-marker-rename ?R)

(defvar cpio-dired-keep-marker-rename-str (char-to-string cpio-dired-keep-marker-rename)
  "In cpio-dired, a string corresponding to cpio-dired-keep-marker-rename.")
(setq cpio-dired-keep-marker-rename-str (char-to-string cpio-dired-keep-marker-rename))


(defvar cpio-dired-re-inode-size "[0-9 \t]*"
  "Regexp for optional initial inode and file size as made by `ls -i -s'.")
(setq cpio-dired-re-inode-size "[0-9 \t]*")


;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar cpio-dired-re-mark "^[^ \n]"
  "Regexp matching a marked line.
Important: the match ends just after the marker.")
(setq cpio-dired-re-mark "^[^ \n]")

(defvar cpio-dired-re-maybe-mark "^. ")
(setq cpio-dired-re-maybe-mark "^. ")

;; The [^:] part after "d" and "l" is to avoid confusion with the
;; DOS/Windows-style drive letters in directory names, like in "d:/foo".
(defvar cpio-dired-re-dir (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size "d[^:]"))
(setq cpio-dired-re-dir (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size "d[^:]"))

(defvar cpio-dired-re-sym (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size "l[^:]"))
(setq cpio-dired-re-sym (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size "l[^:]"))

(defvar cpio-dired-re-exe ;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		;; (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size x)))
		(concat cpio-dired-re-maybe-mark " " x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))
(setq cpio-dired-re-exe ;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		;; (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size x)))
		(concat cpio-dired-re-maybe-mark " " x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))

(defvar cpio-dired-re-perms "[-bcdlps][-r][-w].[-r][-w].[-r][-w].")
(setq cpio-dired-re-perms "[-bcdlps][-r][-w].[-r][-w].[-r][-w].")

(defvar cpio-dired-re-dot "^.* \\.\\.?/?$")
(setq cpio-dired-re-dot "^.* \\.\\.?/?$")

(defvar cpio-dired-font-lock-keywords
  ;; cpio-dired-font-lock-keywords is adapted from dired.
  (list
   ;;
   ;; Dired marks.
   (list cpio-dired-re-mark '(0 cpio-dired-mark-face))
   ;;
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; entry name itself.  We search for Dired defined regexps, and then use the
   ;; cpio-dired defined function `cpio-dired-move-to-entry-name' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the entry name.
   ;;
   ;; Marked entries.
   (list (concat "^[" (char-to-string cpio-dired-marker-char) "]")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-marked-face)))
   ;;
   ;; Flagged entries.
   (list (concat "^[" (char-to-string cpio-dired-del-marker) "]")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-flagged-face)))
   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
   ;;
   ;; However, we don't need to highlight the entry name, only the
   ;; permissions, to win generally.  -- fx.
   ;; Fixme: we could also put text properties on the permission
   ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
;;DL   (list (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size
;;DL		 "[-d]....\\(w\\)....")	; group writable
;;DL	 '(1 cpio-dired-perm-write-face))
;;DL   (list (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size
;;DL		 "[-d].......\\(w\\).")	; world writable
;;DL	 '(1 cpio-dired-perm-write-face))
   ;;
   ;; Subdirectories.
   (list cpio-dired-re-dir
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-directory-face)))
   ;;
   ;; Symbolic links.
   (list cpio-dired-re-sym
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-symlink-face)))
   ;;
   ;; Entrys suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that entry name.  So we do this complex MATCH-ANCHORED form.
     (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
	   '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-ignored-face))))
   ;;
   ;; Entrys suffixed with `completion-ignored-extensions'
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the entry-name,
		    ;; move back to the start of the entry-name
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (cpio-dired-move-to-entry-name)))
	     nil (0 cpio-dired-ignored-face))))
   ;;
   ;; Explicitly put the default face on entry names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat cpio-dired-re-maybe-mark " " cpio-dired-re-perms ".*:$")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 'default)))
   ;;
   ;; Directory headers.
   ;;;; (list cpio-dired-subdir-regexp '(1 cpio-dired-header-face))
   )
  "Additional expressions to highlight in cpio-dired mode.")

(setq cpio-dired-font-lock-keywords
  ;; cpio-dired-font-lock-keywords is adapted from dired.
  (list
   ;;
   ;; Dired marks.
   (list cpio-dired-re-mark '(0 cpio-dired-mark-face))
   ;;
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; entry name itself.  We search for Dired defined regexps, and then use the
   ;; cpio-dired defined function `cpio-dired-move-to-entry-name' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the entry name.
   ;;
   ;; Marked entries.
   (list (concat "^[" (char-to-string cpio-dired-marker-char) "]")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-marked-face)))
   ;;
   ;; Flagged entries.
   (list (concat "^[" (char-to-string cpio-dired-del-marker) "]")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-flagged-face)))
   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
   ;;
   ;; However, we don't need to highlight the entry name, only the
   ;; permissions, to win generally.  -- fx.
   ;; Fixme: we could also put text properties on the permission
   ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
;;DL   (list (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size
;;DL		 "[-d]....\\(w\\)....")	; group writable
;;DL	 '(1 cpio-dired-perm-write-face))
;;DL   (list (concat cpio-dired-re-maybe-mark cpio-dired-re-inode-size
;;DL		 "[-d].......\\(w\\).")	; world writable
;;DL	 '(1 cpio-dired-perm-write-face))
   ;;
   ;; Subdirectories.
   (list cpio-dired-re-dir
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-directory-face)))
   ;;
   ;; Symbolic links.
   (list cpio-dired-re-sym
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-symlink-face)))
   ;;
   ;; Entrys suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that entry name.  So we do this complex MATCH-ANCHORED form.
     (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
	   '(".+" (cpio-dired-move-to-entry-name) nil (0 cpio-dired-ignored-face))))
   ;;
   ;; Entrys suffixed with `completion-ignored-extensions'
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the entry-name,
		    ;; move back to the start of the entry-name
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (cpio-dired-move-to-entry-name)))
	     nil (0 cpio-dired-ignored-face))))
   ;;
   ;; Explicitly put the default face on entry names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat cpio-dired-re-maybe-mark " " cpio-dired-re-perms ".*:$")
	 '(".+" (cpio-dired-move-to-entry-name) nil (0 'default)))
   ;;
   ;; Directory headers.
   ;;;; (list cpio-dired-subdir-regexp '(1 cpio-dired-header-face))
   ))

(defvar cpio-entry-name ()
  "Name of the entry whose contents are being edited.")
(setq cpio-entry-name ())


(defconst *cpio-dirline-re* "^..d"
  "Regular expression to match an entry for a directory.")
(setq *cpio-dirline-re* "^..d")


(defvar *cpio-dired-copy-history* ()
  "History of copies made in cpio-dired-mode.")
(setq *cpio-dired-copy-history* ())


(defvar *cpio-dired-do-chgrp-history* ()
  "History of M-x cpio-dired-do-chgrp.")
(setq *cpio-dired-do-chgrp-history* ())


(defvar *cpio-dired-do-chown-history* ()
  "History of M-x cpio-dired-do-chown.")
(setq *cpio-dired-do-chown-history* ())


(defvar *cpio-dired-do-rename-history* ()
  "History of M-x cpio-dired-do-rename.")
(setq *cpio-dired-do-rename-history* ())


(defvar *cpio-dired-head-offset* 2
  "The number of lines in the cpio-dired buffer devoted to the dired-style header.")
(setq *cpio-dired-head-offset* 2)


(defvar *cpio-dired-buffer* ()
  "The [subordinate] buffer used to present the curent catalog
à la dired.")
(setq *cpio-dired-buffer* ())

(make-variable-buffer-local '*cpio-dired-buffer*)


;;
;; Customizations
;;

(defgroup cpio-dired-faces nil
  "Faces used by Dired."
  :group 'dired
  :group 'faces)

(defface cpio-dired-header
  '((t (:inherit font-lock-type-face)))
  "Face used for directory headers."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-header-face 'cpio-dired-header
  "Face name used for directory headers.")
(setq cpio-dired-header-face 'cpio-dired-header)


(defface cpio-dired-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Dired marks."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-mark-face 'cpio-dired-mark
  "Face name used for Dired marks.")

(defface cpio-dired-marked
  '((t (:inherit warning)))
  "Face used for marked files."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-marked-face 'cpio-dired-marked
  "Face name used for marked files.")

(defface cpio-dired-flagged
  '((t (:inherit error)))
  "Face used for files flagged for deletion."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-flagged-face 'cpio-dired-flagged
  "Face name used for files flagged for deletion.")
(setq cpio-dired-flagged-face 'cpio-dired-flagged)


(defface cpio-dired-warning
  ;; Inherit from font-lock-warning-face since with min-colors 8
  ;; font-lock-comment-face is not colored any more.
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-warning-face 'cpio-dired-warning
  "Face name used for a part of a buffer that needs user attention.")

(defface cpio-dired-perm-write
  '((((type w32 pc)) :inherit default)  ;; These default to rw-rw-rw.
    ;; Inherit from font-lock-comment-delimiter-face since with min-colors 8
    ;; font-lock-comment-face is not colored any more.
    (t (:inherit font-lock-comment-delimiter-face)))
  "Face used to highlight permissions of group- and world-writable files."
  :group 'cpio-dired-faces
  :version "22.2")
(defvar cpio-dired-perm-write-face 'cpio-dired-perm-write
  "Face name used for permissions of group- and world-writable files.")
(setq cpio-dired-perm-write-face 'cpio-dired-perm-write)


(defface cpio-dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for subdirectories."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-directory-face 'cpio-dired-directory
  "Face name used for subdirectories.")
(setq cpio-dired-directory-face 'cpio-dired-directory)


(defface cpio-dired-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symbolic links."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-symlink-face 'cpio-dired-symlink
  "Face name used for symbolic links.")
(setq cpio-dired-symlink-face 'cpio-dired-symlink)


(defface cpio-dired-ignored
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'cpio-dired-faces
  :version "22.1")
(defvar cpio-dired-ignored-face 'cpio-dired-ignored
  "Face name used for files suffixed with `completion-ignored-extensions'.")
(setq cpio-dired-ignored-face 'cpio-dired-ignored)


(defcustom cpio-dired-trivial-filenames dired-trivial-filenames
  "Regexp of entries to skip when finding the first meaningful entry of a directory."
  :group 'cpio-dired
  :version "22.1")

(defcustom cpio-dired-hide-details-hide-information-lines nil
  "Non-nil means 'cpio-dired-hide-information-lines' hides all but header and file lines."
  :group 'cpio-dired
  :type 'boolean)
(defcustom cpio-dired-hide-details-hide-symlink-targets nil
  "Non-nil means `hides symbolic link targets."
  :group 'cpio-dired
  :type 'boolean)
(defcustom cpio-dired-hide-details-information nil
  "Non-hil means `dired-hide-details-mode' hides all but header and file lines."
  :group 'cpio-dired
  :type 'boolean)

;; N.B. This is here because this file is where the cpio-dired lines are created.
(defcustom cpio-try-names t
  "Non-nil means that GIDs and UIDs are displayed as integers."
  :group 'cpio
  :type 'boolean)


;;
;; Library
;;

(defun cpio-dired-get-entry-name ()
  "Get the entry name on the current line."
  (let ((fname "cpio-dired-get-filename"))
    (save-excursion
      (beginning-of-line)
      (save-match-data
	(if (looking-at *cpio-dired-entry-regexp*)
	    (match-string-no-properties *cpio-dired-name-idx*))))))

(defun cpio-dired-hide-details-update-invisibility-spec ()
  "Toggle cpio-dired-hide-details-mode."
  (let ((fname "cpio-dired-hide-details-update-invisibility-spec"))
    (funcall (if cpio-dired-hide-details-mode
		 'add-to-invisibility-spec
	       'remove-from-invisibility-spec)
	     'cpio-dired-hide-details-detail)
    (funcall (if (and cpio-dired-hide-details-mode
		      cpio-dired-hide-details-hide-information-lines)
		 'add-to-invisibility-spec
	       'remove-from-invisibility-spec)
	     'cpio-dired-hide-details-information)
    (funcall (if (and cpio-dired-hide-details-mode
		      cpio-dired-hide-details-hide-symlink-targets
		      (not (derived-mode-p 'cpio-dired-mode)))
		 'add-to-invisibility-spec
	       'remove-from-invisibility-spec)
	     'cpio-dired-hide-details-link)))

(defun cpio-dired-find-entry-noselect (entry-name)
  "Read the contents of the given ENTRY-NAME, but don't display it."
  (let ((fname "cpio-dired-find-entry-noselect")
	(target-buffer (get-buffer-create (cpio-contents-buffer-name entry-name))))
    (cond ((and target-buffer (buffer-live-p target-buffer))
	   target-buffer)
	  (target-buffer
	   (kill-buffer target-buffer)
	   (setq target-buffer (get-buffer-create (cpio-contents-buffer-name entry-name)))
	   (with-current-buffer target-buffer
	     (insert (cpio-contents entry-name)))
	   target-buffer)
	  (t nil))))

(defun cpio-internal-do-deletions (l)
  "Delete the entries in the list L."
  (let ((fname "cpio-internal-do-deletions"))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-internal-do-deletions l))
      (mapc 'cpio-internal-do-deletion l))))

(defun cpio-internal-do-deletion (entry-name)
  "Remove the entry with name ENTRY-NAME from a cpio-archive.
CONTRACT: You're in that archive's buffer."
  (let ((fname "cpio-internal-do-deletion")
	(entry-info)
	(start-marker)
	(end-marker)
	(entry-attrs))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-internal-do-deletion entry-name))
      (if (null (setq entry-info (assoc entry-name *cpio-catalog*)))
	  (error "%s(): Could not get entry information for %s." fname entry-name))
      (setq start-marker (aref (cdr entry-info) 1)) ;HEREHERE Shouldn't this have an abstraction?
      (setq end-marker (1+ (cg-round-up (1- (+ (aref (cdr entry-info) 2)
					    (cpio-entry-size (cpio-entry-attrs entry-name))))
					*cpio-padding-modulus*)))
      (with-writable-buffer
       (delete-region start-marker end-marker))
      (setq *cpio-catalog* (delete (assoc entry-name *cpio-catalog*) *cpio-catalog*)))))

(defun cpio-dired-marked-entries (char arg)
  "Return a list of entries marked with CHAR, or,
if none are so marked, then the next ARG entries."
  (let ((fname "cpio-dired-marked-entries")
	(files ())
	(i 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "^\\%c" char) (point-max) t)
	(push (cpio-dired-get-entry-name) files)))
    (unless files
      (save-excursion
	(while (< i arg)
	  (push (cpio-dired-get-entry-name) files)
	  (cpio-dired-next-line 1)
	  (setq i (1+ i)))))
    files))

(defun cpio-dired-add-contents (attrs contents &optional cpio-dired-buffer mark)
  "Add an entry to a cpio archive using the given ATTRS with the given CONTENTS.

CONTRACT: The archive buffer has no trailer.

The optional argument CPIO-DIRED-BUFFER is just there
to make the recursive call this function inside the archive buffer sensible.

If the optional argument MARK, a character, is not NIL,
then use that to mark the new entry."
  ;; CAUTION: There's lots of code duplicated with M-x cpio-dired-add-entry.
  (unless cpio-dired-buffer (setq cpio-dired-buffer (current-buffer)))
  (let ((fname "cpio-dired-add-contents")
	(header-string)
	(entry-name (cpio-entry-name attrs))
	(new-catalog-entry)
	(header-start-marker)
	(contents-start-marker))
    (if *cab-parent*
	(with-current-buffer *cab-parent*
	  (cpio-dired-add-contents attrs contents cpio-dired-buffer mark))
      (setq new-catalog-entry (make-vector *cpio-catalog-entry-length* nil))

      (cpio-delete-trailer)
      (setq header-string (cpio-make-header-string attrs contents))

      (with-writable-buffer
       (setq header-start-marker (point-max-marker))
       (goto-char (point-max))
       (insert header-string)

       (setq contents-start-marker (point-max-marker))
       (goto-char (point-max))
       (cpio-insert-padded-contents contents))

      (aset new-catalog-entry *cpio-catalog-entry-attrs-idx* attrs)
      (aset new-catalog-entry *cpio-catalog-entry-header-start-idx* header-start-marker)
      (aset new-catalog-entry *cpio-catalog-entry-contents-start-idx* contents-start-marker)
      (cpio-set-entry-unmodified new-catalog-entry)

      ;; HEREHERE Is there an appropriate abstraction for the following?
      ;; Perhaps including the above?
      (add-to-list '*cpio-catalog* (cons entry-name new-catalog-entry) 'append)

      (with-current-buffer cpio-dired-buffer
	(save-excursion
	  (goto-char (point-max))
	  (with-writable-buffer
	   (insert (cpio-dired-format-entry attrs mark) "\n")))))))

(defun cpio-dired-get-marked-entries (&optional arg) ;✓
  "Return a list of the marked entries in the current cpio-dired buffer."
  (let ((fname "cpio-dired-get-marked-entries")
	(results ())
	(regexp (cpio-dired-marker-regexp))
	(i 0))
    (unless (string-equal mode-name "cpio-dired")
      (error "%s() only makes sense in a cpio-dired buffer." fname))
    ;; Marks win over ...
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
	(push (cpio-dired-get-entry-name) results)))
    ;; ... arg,
    (unless results
	(save-excursion
	  (unless arg (setq arg 1))
	  (while (< i arg)
	    (push (cpio-dired-get-entry-name) results)
	    (dired-next-line 1)
	    (setq i (1+ i)))))
    ;; , but if none of that worked, then take the current entry.
      (if results
	  results
	(list (cpio-dired-get-entry-name)))))

(defun cpio-dired-internal-do-copy (entry target) ;✓
  "Copy the ENTRY to the TARGET entry.
CONTRACT: TARGET is the actual TARGET name, not an implied directory entry."
  (let ((fname "cpio-dired-internal-do-copy")
	(attrs (copy-sequence (cpio-entry-attrs entry)))
	(contents (cpio-contents entry)))
    (cpio-set-entry-name attrs target)
    (cpio-dired-add-contents attrs contents nil cpio-dired-keep-marker-copy)))

(defun cpio-dired-internal-do-rename (entry-name target)
  "Rename ENTRY-NAME to the TARGET entry.
CONTRACT:
1. TARGET is the actual TARGET name, not an implied directory entry.
2. You're in a cpio-dired buffer"
  ;; HEREHERE This has some overlap with (cpio-dired-internal-do-copy).
  (let ((fname "cpio-dired-internal-do-rename")
	(entry (cpio-entry entry-name))
	(attrs (cpio-entry-attrs entry-name))
	(mark (cpio-dired-get-mark entry-name)))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))

    (save-excursion
      (cpio-dired-goto-entry entry-name)
      (with-writable-buffer
       (delete-region (line-beginning-position) (line-end-position)))
       ;; (cpio-dired-goto-entry) needs entry-name in the catalog,
       ;; so don't update it until after.
       (cpio-set-entry-name attrs target)
       (with-current-buffer *cab-parent*
	 (setcar (assoc entry-name *cpio-catalog*) target))
       (with-writable-buffer
	(insert (cpio-dired-format-entry attrs mark))))
    (cpio-dired-move-to-entry-name)))

(defun cpio-dired-mark-read-regexp (operation)
  "Read a regular expression to match entries for the given OPERATION."
    (let* ((fname "cpio-dired-mark-read-regexp")
	   (regexp (read-regexp
		    (format "%s on entries matching regexp: " operation)
		    nil
		    'dired-regexp-history))
	   (mark-char (cond ((string-equal operation "Copy")
			     cpio-dired-keep-marker-copy)
			    ((string-equal operation "Rename")
			     cpio-dired-keep-marker-rename)
			    ;; ((string-equal operation "HardLink") )
			    ;; ((string-equal operation "Symlink") )
			    (t (error "%s() called with unknown operation [[%s]]." fname operation))))
	   (entry-name))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward *cpio-dired-entry-regexp* (point-max) t)
	  (setq entry-name (cpio-dired-get-entry-name))
	  (if (string-match-p regexp entry-name)
	      (cpio-dired-mark-this-entry mark-char))))))

(defun cpio-dired-replace-dired-line (entry-name)
  "Replace the entry for the given ENTRY-NAME
with information from the current catalog.
CONTRACT: You're on the line to be replaced."
  (let ((fname "cpio-dired-replace-dired-line")
	(attrs (cpio-entry-attrs entry-name))
	(mark))
    (save-excursion
      (cpio-move-to-entry entry-name)
      (setq mark (cpio-dired-get-mark))
      (cpio-dired-delete-dired-line entry-name)
      (with-writable-buffer
       (insert (cpio-dired-format-entry attrs mark))))))

(defun cpio-dired-delete-dired-line (entry-name)
  "Delete the line of ENTRY-NAME not including the new line."
  (let ((fname "cpio-dired-delete-dired-line"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired-buffer." fname))
    (cpio-move-to-entry entry-name)
    (with-writable-buffer
     (delete-region (line-beginning-position) (line-end-position)))))

(defun cpio-dired-buffer-name (archive-name)
  "Return the name of the dired-style buffer for ARCHIVE-NAME."
  (let ((fname "cpio-dired-buffer-name"))
    (concat "CPIO archive: " (file-name-nondirectory archive-name))))

(defun cpio-present-ala-dired (archive-buffer)
  "Create a buffer with a ls -l format reflecting the contents of the current cpio archive.
This returns the buffer created."
  (let* ((fname "cpio-present-ala-dired")
	 (archive-name (with-current-buffer archive-buffer
			 (file-name-nondirectory (buffer-file-name))))
	 (buffer-name (cpio-dired-buffer-name archive-name))
	 (buffer (get-buffer-create buffer-name))
	 (entry-string)
	 (catalog (cpio-catalog)))
    (with-current-buffer buffer
      (setq *cpio-catalog* catalog)
      (with-writable-buffer
       (erase-buffer)
       (insert "CPIO archive: " archive-name ":\n\n")
       (mapc (lambda (e)
	       (let ((line (cpio-dired-format-entry (aref (cdr e) 0))))
		 (insert (concat line "\n"))))
	     (cpio-sort-catalog)))
      (cpio-dired-mode))

    (if cab-clear-cab-info-buffer
	(with-current-buffer *cab-info-buffer*
	  (erase-buffer)))

    ;; No, I do not yet understand why this must be done
    ;; every time the presentation is updated.

    ;; (with-current-buffer "cpio-mode.el"
    ;;   kill-buffer-hook)

    (cab-register buffer archive-buffer)

    ;; (with-current-buffer "cpio-mode.el"
    ;;    kill-buffer-hook)

    buffer))

(defun cpio-dired-move-to-first-entry ()
  "Move the point to the first entry in a cpio-dired style buffer."
  (let ((fname "cpio-dired-move-to-first-entry"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (goto-char (point-min))
    (cpio-dired-next-line *cpio-dired-head-offset*)))

(defun cpio-dired-format-entry (attrs &optional mark)
  "Create a dired-style line for ATTRS.
If the optional MARK is given,
then it is a character and used as the mark on the generated line.
The line does not include a trailing <new line>."
  (let* ((fname "cpio-dired-format-entry")
	 (mode-string       (cpio-int-mode-to-mode-string         (cpio-mode-value attrs)))
	 (uid-string        (cpio-uid-to-uid-string               (cpio-uid        attrs)))
	 (gid-string        (cpio-gid-to-gid-string               (cpio-gid        attrs)))
	 (nlink-string      (cpio-nlink-to-nlink-string           (cpio-nlink      attrs)))
	 (mtime-string      (cpio-mtime-to-mtime-string           (cpio-mtime      attrs)))
	 (filesize-string   (cpio-filesize-to-filesize-string     (cpio-entry-size attrs)))
	 (dev-maj-string    (cpio-dev-maj-to-dev-maj-string       (cpio-dev-maj    attrs)))
	 (dev-min-string    (cpio-dev-min-to-dev-min-string       (cpio-dev-min    attrs)))
	 (entry-name-string (cpio-entry-name-to-entry-name-string (cpio-entry-name attrs)))
	 (fmt (if entry-name-string
		  (if cpio-try-names
		      (format "%%c %%s %%3s %%8s %%8s %%8s %%7s %%s")
		    (format   "%%c %%s %%3s %%5s %%5s %%8s %%7s %%s"))
		nil)))
    (unless mark (setq mark ?\s))
    (unless (characterp mark)
      (signal 'wrong-type-error (list 'characterp mark)))
    (if fmt
	(format fmt mark
		mode-string nlink-string uid-string gid-string
		filesize-string mtime-string entry-name-string))))

(defun cpio-dired-get-mark (&optional entry-name)
  "Get the mark, a character, on ENTRY-NAME."
  (let ((fname "cpio-dired-get-mark"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): only makes sense in a cpio-dired buffer." fname))
    (unless entry-name
      (setq entry-name (cpio-dired-get-entry-name)))
    (save-excursion
      (cpio-dired-goto-entry entry-name)
      (string-to-char (buffer-substring (line-beginning-position)
					(1+ (line-beginning-position)))))))


;;
;; Commands
;;

;; h		describe-mode

(defun cpio-dired-add-entry (filename &optional cpio-dired-buffer) ;✓
  "Add the given FILENAME as an entry in a cpio archive.
The archive is the one affiliated with the current cpio-dired buffer.
If you want a different path, then rename the entry once it's there.

CAVEAT:
This function is not smart about its filename.
If you give a full path, then you get a fullpath.
If you want a different path, then rename the entry once it's there.

TECHNICAL INTERNAL INFORMATION:
The optional argument, CPIO-DIRED-BUFFER, is the cpio-dired style buffer
to be updated with the new entry.
It is here because, while this function may start
in the cpio-dired-style buffer,
It does its heavy lifting in the archive buffer.
CPIO-DIRED-BUFFER is just for bookkeeping;
if CPIO-DIRED-BUFFER is NIL (i.e. you're /in/ the cpio-dired buffer),
then use the current buffer."
  ;; CAUTION: There's lots of code duplicated with M-x cpio-dired-add-contents.
  (interactive "fFile: ")
  (let ((fname "cpio-dired-add-entry")
	(entry-attrs)
	(header-start-marker)
	(contents-start-marker)
	(header-string)
	(cpio-dired-buffer (or cpio-dired-buffer (current-buffer))))
    (if (string-match-p "^~/" filename)
	(setq filename (expand-file-name filename)))
    (cond (*cab-parent*
	   (unless (eq major-mode 'cpio-dired-mode)
	     (error "%s(): You're not in a cpio-dired buffer." fname))
	   (with-current-buffer *cab-parent*
	     (cpio-dired-add-entry filename cpio-dired-buffer))
	   (cpio-dired-set-modified))
	  (t
	   (setq entry-attrs (cpio-create-entry-attrs filename))

	   (cpio-delete-trailer)
	   (setq header-string (cpio-make-header-string entry-attrs))

	   (with-writable-buffer
	    (setq header-start-marker (point-max-marker))
	    (goto-char (point-max))
	    (insert header-string)

	    (setq contents-start-marker (point-max-marker))
	    (goto-char (point-max))
	    (insert-file-contents filename)

	    (goto-char (point-max))
	    (cpio-insert-trailer))

	   (with-current-buffer cpio-dired-buffer
	     (with-writable-buffer
	      (delete-region (line-beginning-position) (1+ (line-end-position)))))))))

;; * c		dired-change-marks
(defun cpio-dired-change-marks (old new) ;✓✓
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark entries."
  (interactive (let* ((cursor-in-echo-area t)
		      (old (progn (message "Change (old mark): ") (read-char)))
		      (new (progn (message  "Change %c marks to (new mark): " old)
				  (read-char))))
		 (list (char-to-string old) (char-to-string new))))
  (let ((fname "cpio-dired-change-marks"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio dired buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (beginning-of-line)
      (with-writable-buffer
       (while (< (point) (point-max))
	 (when (looking-at-p old)
	   (delete-char 1)
	   (insert new))
	 (forward-line 1))))))

;; -		negative-argument
;; .		dired-clean-directory
(defun cpio-dired-clean-directory (keep) ;×
  "Flag numerical backups for deletion.
Spares `cpio-dired-kept-versions' latest versions, and `cpio-kept-old-versions' oldest.
Positive prefix arg KEEP overrides `cpio-dired-kept-versions';
Negative prefix arg KEEP overrides `cpio-kept-old-versions' with KEEP made positive.

To clear the flags on these entries, you can use M-x cpio-dired-flag-backup-entries
with a prefix argument."
  (interactive "p")
  (let ((fname "cpio-dired-clean-directory"))
    (error "%s() is not yet implemented" fname)))

;; w		dired-copy-filename-as-kill
(defun cpio-dired-copy-entry-name-as-kill (arg) ;✓✓
  "Copy names of marked (or next ARG) entries into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute entry name of each marked entry.
With C-u, use the entry name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked entries are ignored in this case.

You can then feed the entry name(s) to other commands with C-y."
  (interactive "p")
  (let ((fname "cpio-dired-copy-entry-name-as-kill")
	(names (reverse (cpio-dired-get-marked-entries arg))))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio dired buffer." fname))
    (if names
	(mapc 'kill-new names)
      (save-excursion
	(while (and (> arg 0)
		    (< (point) (point-max)))
	  (kill-new (cpio-dired-get-entry-name))
	  (setq arg (1- arg))
	  (forward-line 1))))))

;; *		Prefix Command

;; +		dired-create-directory
(defun cpio-dired-create-directory (directory) ;✓✓
  "Create a directory entry called DIRECTORY.
If DIRECTORY already exists, signal an error.
This respects umask(1) as available through (default-file-modes)."
  (interactive (list (read-string "Create directory: " "" nil "")))
  (let ((fname "cpio-dired-create-directory")
	(new-catalog-entry)
	(attrs)
	(header-string)
	(header-start)
	(contents-start)
	(cat-entry)
	(namesize))
    (cond (*cab-parent*
	   (unless (eq major-mode 'cpio-dired-mode)
	     (error "%s(): You're not in a cpio dired buffer." fname))
	   (with-current-buffer *cab-parent*
	     (cpio-dired-create-directory directory))
	   (save-excursion
	     (goto-char (point-max))
	     (with-writable-buffer
	      (insert (concat (cpio-dired-format-entry (cpio-entry-attrs directory)) "\n"))))
	   (cpio-dired-set-modified))
	  (t
	   (unless (eq major-mode 'cpio-mode)
	     (error "%s(): The parent buffer was not a cpio-mode buffer." fname))
	   (unless (stringp directory)
	     (signal 'wrong-type-error (list directory)))
	   (unless (< 0 (length directory))
	     (error "%s(): Cannot create an entry with a zero-length name." directory))
	   (if (cpio-entry-exists-p directory)
	       (error "%s(): Entry %s already exists." fname directory))
	   (setq namesize (1+ (length directory)))
	   (setq attrs (cpio-create-faux-directory-attrs directory))
	   (cpio-set-mode attrs (logior s-ifdir (default-file-modes)))
	   (with-writable-buffer
	    (cpio-delete-trailer)
	    (setq header-start (point-max-marker))
	    (setq header-string (cpio-make-header-string attrs))
	    (goto-char (point-max))
	    (insert header-string)
	    (setq buffer-read-only t))
	   (setq contents-start (point-max-marker))
	   (push (cons directory
		       (vector attrs
			       header-start
			       contents-start
			       'cpio-mode-entry-unmodified))
		 *cpio-catalog*)))))

;; =		dired-diff
(defun cpio-dired-diff (entry &optional switches) ;✓
  "Compare entry at point with entry ENTRY using `diff'.
If called interactively, prompt for ENTRY.  If the entry at point
has a backup entry, use that as the default.  If the entry at point
is a backup entry, use its original.  If the mark is active
in Transient Mark mode, use the entry at the mark as the default.
\(That's the mark set by C-SPC, not by Dired's
M-x dired-mark command.)

ENTRY is the first entry given to `diff'.  The entry at point
is the second entry given to `diff'.

With prefix arg, prompt for second argument SWITCHES, which is
the string of command switches for the third argument of `diff'."
  ;; HEREHERE This looks like plagiarized code to me. It's certainly not tested.
  (interactive
   (let* ((current (dired-get-filename t))
	  ;; Get the latest existing backup file or its original.
	  (oldf (if (backup-file-name-p current)
		    (file-name-sans-versions current)
		  (diff-latest-backup-file current)))
	  ;; Get the file at the mark.
	  (file-at-mark (if (and transient-mark-mode mark-active)
			    (save-excursion (goto-char (mark t))
					    (dired-get-filename t t))))
	  (default-file (or file-at-mark
			    (and oldf (file-name-nondirectory oldf))))
	  ;; Use it as default if it's not the same as the current file,
	  ;; and the target dir is current or there is a default file.
	  (default (if (and (not (equal default-file current))
			    (or (equal (dired-dwim-target-directory)
				       (dired-current-directory))
				default-file))
		       default-file))
	  (target-dir (if default
			  (dired-current-directory)
			(dired-dwim-target-directory)))
	  (defaults (dired-dwim-target-defaults (list current) target-dir)))
     (list
      (minibuffer-with-setup-hook
	  (lambda ()
	    (set (make-local-variable 'minibuffer-default-add-function) nil)
	    (setq minibuffer-default defaults))
	(read-file-name
	 (format "Diff %s with%s: " current
		 (if default (format " (default %s)" default) ""))
	 target-dir default t))
      (if current-prefix-arg (read-string "Options for diff: "
					  (if (stringp diff-switches)
					      diff-switches
					    (mapconcat 'identity diff-switches " ")))))))
  (let ((fname "cpio-dired-diff"))
    (error "%s() is not yet implemented" fname)))

;; C-o		dired-display-file
(defun cpio-dired-display-entry () ;✓
  "In a cpio UI buffer, display the entry on the current line in another window.
Return the buffer containing the entry's contents."
  (interactive)
  (let ((fname "cpio-dired-display-entry")
	(target-buffer (cpio-dired-find-entry)))
    (with-current-buffer target-buffer
      (setq buffer-read-only t))
    target-buffer))

;; %		Prefix Command
;; &		dired-do-async-shell-command
(defun cpio-dired-do-async-shell-command (command &optional arg entry-list) ;✓
  ;; I don't know if this makes sense.
  "Run a shell command COMMAND on the marked entries asynchronously.

Like `dired-do-shell-command', but adds `&' at the end of COMMAND
to execute it asynchronously.

When operating on multiple entries, asynchronous commands
are executed in the background on each entry in parallel.
In shell syntax this means separating the individual commands
with `&'.  However, when COMMAND ends in `;' or `;&' then commands
are executed in the background on each entry sequentially waiting
for each command to terminate before running the next command.
In shell syntax this means separating the individual commands with `;'.

The output appears in the buffer `*Async Shell Command*'."
  ;; HEREHERE This looks like plagiarized code. It certainly hasn't been tested.
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (let ((fname "cpio-dired-do-async-shell-command"))
    (warn "%s() is not obvious." fname)))

;; B		dired-do-byte-compile
(defun cpio-dired-do-byte-compile (arg)	;×
  "Byte compile marked (or next ARG) Emacs Lisp entries."
  (interactive "p")
  (let ((fname "cpio-dired-do-byte-compile"))
    (error "%s() is not yet implemented" fname)))

;; G		dired-do-chgrp
(defun cpio-dired-do-chgrp (arg &optional entry-names group cpio-dired-buffer) ;✓
  "Change the group of the marked (or next ARG) entries.
Type M-n to pull the entry attributes of the entry at point
into the minibuffer.

The optional arguements ENTRY-NAME, GROUP and CPIO-DIRED-BUFFER,
are just for bookkeeping, since this function may be called interactively
in one buffer, but actually performs the function
in the buffer containing the archive."
  ;; HEREHERE This shares a lot of code with (cpio-dired-do-chown).
  (interactive "p")
  (let ((fname "cpio-dired-do-chgrp")
	(header-string)
	(local-entry-names (if entry-names
			       entry-names
			     ()))
	(local-group (if group
			 group
		       ;; HEREHERE This (read-string) doesn't play nicely
		       ;; with make check.
		       (read-string "Group? "
				    nil
				    *cpio-dired-do-chgrp-history*)))
	(local-cpio-dired-buffer (if cpio-dired-buffer
				     cpio-dired-buffer
				   (current-buffer)))
	(i 0)
	(entry)
	(attrs)
	(mark))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): major mode is [[%s]]." fname (symbol-name major-mode))
      (error "%s(): You're in neither a cpio-dired buffer nor a buffer in cpio-mode ." fname))
    (cond (*cab-parent*
	   (unless entry-names
	     (setq entry-names (cpio-dired-get-marked-entries arg)))
	   (setq local-group (cg-strip "\\s-+" local-group))
	   (with-current-buffer *cab-parent*
	     (cpio-dired-do-chgrp arg entry-names local-group local-cpio-dired-buffer))
	   (cpio-dired-set-modified))
	  (t
	   (unless entry-names
	     (signal 'void-variable entry-names))
	   (unless group
	     (signal 'void-variable group))
	   (unless cpio-dired-buffer
	     (signal 'void-variable cpio-dired-buffer))
	   (if (null (setq local-group (cpio-gid-for-group local-group)))
	       (error "%s(): Group [[%s]] does not exist." fname group))
	   (mapc (lambda (en)
		   (setq entry (cpio-entry en))
		   (setq attrs (cpio-entry-attrs en))
		   (cpio-set-gid attrs local-group)

		   (with-current-buffer local-cpio-dired-buffer
		     (save-excursion
		       (cpio-dired-goto-entry en)
		       (setq mark (cpio-dired-get-mark))
		       (with-writable-buffer
			(delete-region (line-beginning-position)
				       (line-end-position))
			(insert (cpio-dired-format-entry attrs mark))))))
		 entry-names)
	  (cpio-dired-set-modified)))))

;; M		dired-do-chmod
(defun cpio-dired-do-chmod (&optional arg)	;✓✓✓
"Change the mode of the marked (or next ARG) entries.
Symbolic modes like `g+w' are allowed.
Type M-n to pull the entry attributes of the entry at point
into the minibuffer."
  (interactive "p")
  (let* ((fname "cpio-dired-do-chmod")
	 (entries (cpio-dired-get-marked-entries arg))
	 (default-entry (cpio-dired-get-entry-name))
	 (default-attrs (cpio-entry-attrs default-entry))
	 (cpio-mode-value (cpio-mode-value default-attrs))
	 (entry-type)
	 (default-mode-value (cpio-mode-value default-attrs))
	 (cpio-mode-value default-mode-value)
	 (attrs)
	 (mode-string (cpio-int-mode-to-mode-string cpio-mode-value))
	 (default
	   (and (stringp mode-string)
		(string-match "^.\\(...\\)\\(...\\)\\(...\\)$" mode-string)
		(replace-regexp-in-string
		 "-" ""
		 (format "u=%s,g=%s,o=%s"
			 (match-string-no-properties 1 mode-string)
			 (match-string-no-properties 2 mode-string)
			 (match-string-no-properties 3 mode-string)))))
	 (modes (dired-mark-read-string
		 "Change mode of %s to: "
		 nil 'chmod arg entries default)))
    (cond ((or (equal mode-string "")
	       (equal mode-string default-mode-value))
	   (error "%s(): No entry mode specified." fname))
	  ((string-match-p "^[0-7]+" modes)
	   (setq cpio-mode-value (string-to-number modes 8)))
    (cpio-dired-set-modified))
    (dolist (entry entries)
      (setq entry-type (cpio-numeric-entry-type (cpio-mode-value (cpio-entry-attrs entry))))
      (cpio-set-mode (cpio-entry-attrs entry) (logior entry-type cpio-mode-value))
      (cpio-dired-replace-dired-line entry))))

;; O		dired-do-chown
(defun cpio-dired-do-chown (arg &optional entry-names owner cpio-dired-buffer) ;✓
  "Change the owner of the marked (or next ARG) entries.
Type M-n to pull the entry attributes of the entry at point
into the minibuffer."
  ;; HEREHERE This shares a lot of code with (cpio-dired-do-chgrp).
  (interactive "p")
  (let ((fname "cpio-dired-do-chown")
	(header-string)
	(cpio-dired-buffer (current-buffer))
	(local-entry-names (if entry-names
			       entry-names
			     ()))
	(local-owner (if owner
			 owner
		       ;; HERREHERE The following (read-string) doesn't play nicely
		       ;; with make check*.
		       (read-string "Owner? "
				    nil
				    *cpio-dired-do-chown-history*)))
	(local-group)
	(local-cpio-dired-buffer (if cpio-dired-buffer
				     cpio-dired-buffer))
	(i 0)
	(entry)
	(attrs)
	(mark))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): You're in neither a cpio-dired buffer nor a buffer in cpio-mode ." fname))

    (cond (*cab-parent*
	   (unless entry-names
	     (setq entry-names (cpio-dired-get-marked-entries arg)))
	   (unless entry-names
	     (save-excursion
	       (cpio-dired-move-to-entry-name)
	       (while (and (< i arg)
			   (< (point) (point-max)))
		 (push (cpio-dired-get-entry-name) entry-names)
		 (setq i (1+ i)))))
	   (setq local-owner (cg-strip "\\s-+" local-owner))
	   (with-current-buffer *cab-parent*
	     (cpio-dired-do-chown arg entry-names local-owner cpio-dired-buffer))
	   (cpio-dired-set-modified))
	  (t
	   (unless entry-names
	     (signal 'void-variable entry-names))
	   (unless owner
	     (signal 'void-variable owner))
	   (unless cpio-dired-buffer
	     (signal 'void-variable cpio-dired-buffer))
	   (cond ((string-match-p ":" owner)
		  (setq local-owner (nth 0 (split-string owner ":")))
		  (setq local-group (nth 1 (split-string owner ":"))))
		 (t t))
	   (if (null (setq local-owner (cpio-uid-for-owner local-owner)))
	       (error "%s(): Owner [[%s]] does not exist." fname owner))
	   (setq local-group (cpio-gid-for-group local-group))
	   (mapc (lambda (en)
		   (setq entry (cpio-entry en))
		   (setq attrs (cpio-entry-attrs en))
		   (cpio-set-uid attrs local-owner)
		   (if local-group
		       (cpio-set-gid attrs local-group))
		   (cpio-set-contents-start entry (+ (cpio-entry-header-start entry)
						     (length (cpio-pad (cpio-make-header-string attrs)
									  *cpio-padding-modulus* ?\0))))
		   (goto-char (cpio-entry-contents-start entry))

		   (with-current-buffer local-cpio-dired-buffer
		     (save-excursion
		       (cpio-dired-goto-entry en)
		       (setq mark (cpio-dired-get-mark))
		       (with-writable-buffer
			(delete-region (line-beginning-position)
				       (line-end-position))
			(insert (cpio-dired-format-entry attrs mark))))))
		 entry-names)
	  (cpio-dired-set-modified)))))

;; Z		dired-do-compress
(defun cpio-dired-do-compress (arg)	;×
  "Compress or uncompress marked (or next ARG) entries."
  (interactive "p")
  (let ((fname "cpio-dired-do-compress"))
    (error "%s() is not yet implemented" fname)))

;; C		dired-do-copy
(defun cpio-dired-do-copy (arg)		;✓
  "Copy all marked (or next ARG) entries, or copy the current entry.
When operating on just the current entry, prompt for the new name.

When operating on multiple or marked entries,
the prompt for a target implies
that that target should be a directory."
  ;; HEREHERE This has lots of duplicated code with (cpio-dired-do-rename).
  (interactive "p")
  (let ((fname "cpio-dired-do-copy")
	(entries (cpio-dired-marked-entries cpio-dired-marker-char arg))
	(target)
	(target-attrs))
    (setq target (read-string "Target? "
			      nil
			      *cpio-dired-copy-history*))
    (cpio-delete-trailer)
    (cond ((> (length entries) 1)
	   (setq target (cg-strip-right "/" target 'multiples))
	   ;; First handle the case where the entry exists and looks like a directory.
	   (cond ((cpio-entry-exists-p target)
		  (setq target-attrs (cpio-entry-attrs target))
		  (if (/= (logand s-ifdir (cpio-mode-value target-attrs)) s-ifdir)
		      (error "%s(): There's already a non-directory entry called %s." fname target)))
		 (t t))
	   ;; Now check the existence of the implied targets.
	   (mapc (lambda (en)
		   (if (cpio-entry-exists-p (concat target "/" en))
		       (error "%s(): Target entry [[%s]] is already there." fname (concat target "/" en))))
		 entries)
	   ;; Finally, add the implied targets.
	   (mapc (lambda (en)
		   (cpio-dired-internal-do-copy en (concat target "/" en)))
		 entries)
	   (cpio-dired-set-modified))
	  ((cpio-entry-exists-p target)
	   (setq target-attrs (cpio-entry-attrs target))
	   (if (= (logand s-ifdir (cpio-mode-value target-attrs)) s-ifdir)
	       (mapc (lambda (en)
		       (cpio-dired-internal-do-copy en (concat target "/" en)))
		     entries)
	     (error "%s(): There's already an entry called %s." fname target)))
	  (t
	   (mapc (lambda (en)
		   (cpio-dired-internal-do-copy en target))
		 entries)))))

;; % C		dired-do-copy-regexp
(defun cpio-dired-do-copy-regexp (regexp newname &optional arg whole-name) ;×
  "Copy selected entries whose names match REGEXP to NEWNAME.
See function `cpio-dired-do-rename-regexp' for more info."
  (interactive (cpio-dired-mark-read-regexp "Copy"))
  (let ((fname "cpio-dired-do-copy-regexp"))
    (error "%s() is not yet implemented" fname)))

;; D		dired-do-delete
(defun cpio-dired-do-delete (arg)	;✓
  "Delete all marked (or next ARG) entries.
Marks win over ARG."
  (interactive "p")
  (let ((fname "cpio-dired-do-delete")
	(entries (cpio-dired-marked-entries cpio-dired-marker-char arg))
	(i 0))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (mapc (lambda (en)
	    (save-excursion
	      (cpio-dired-goto-entry en)
	      (with-writable-buffer
	       (delete-region (line-beginning-position) (1+ (line-end-position))))

	      (cpio-internal-do-deletion en)))
	  entries)
    (cpio-dired-set-modified)
    (cpio-dired-move-to-entry-name)))

;; x		dired-do-flagged-delete
(defun cpio-dired-do-flagged-delete (&optional nomessage) ;✓
  "In Dired, delete the entries flagged for deletion.
If NOMESSAGE is non-nil, we don't display any message
if there are no flagged entries.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive)
  (let ((fname "cpio-dired-do-flagged-delete")
	(entries (cpio-dired-marked-entries cpio-dired-del-marker 1)))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (if (and (null entries)
	     (not nomessage))
	(message "%s(): No entries marked for deletion." fname))
    (mapc (lambda (en)
	    (save-excursion
	      (cond ((cpio-dired-goto-entry en)
		     (with-writable-buffer
		      (delete-region (line-beginning-position)
				     (1+ (line-end-position)))))
		    (t t)))
	    (cpio-internal-do-deletion en))
	  entries)
    (cpio-dired-set-modified)))

;; H		dired-do-hardlink
(defun cpio-dired-do-hardlink (arg)	;×
  "Add names (hard links) current entry or all marked (or next ARG) entries.
When operating on just the current entry, you specify the new name.
When operating on multiple or marked entries, you specify a directory
and new hard links are made in that directory
with the same names that the entries currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see."
  (interactive "p")
  (let ((fname "cpio-dired-do-hardlink"))
    (error "%s() is not yet implemented" fname)))

;; % H		dired-do-hardlink-regexp
(defun cpio-dired-do-hardlink-regexp (regexp newname &optional arg whole-name) ;×
  "Hardlink selected entries whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (cpio-dired-mark-read-regexp "HardLink"))
  (let ((fname "cpio-dired-do-hardlink-regexp"))
    (error "%s() is not yet implemented" fname)))

;;
;; M-s a C-s	dired-do-isearch
(defun cpio-dired-do-isearch ()		;×
  "Search for a string through all marked entries using Isearch."
  (interactive)
  (let ((fname "cpio-dired-do-isearch"))
    (error "%s() is not yet implemented" fname)))

;;
;; M-s a C-M-s	dired-do-isearch-regexp
(defun cpio-dired-do-isearch-regexp ()	;×
  "Search for a regexp through all marked entries using Isearch."
  (interactive)
  (let ((fname "cpio-dired-do-isearch-regexp"))
    (error "%s() is not yet implemented" fname)))

;; k		dired-do-kill-lines
(defun cpio-dired-do-kill-lines (arg)	;×
  "Kill all marked lines (not the entries).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)
If you use this command with a prefix argument to kill the line
for a entry that is a directory, which you have inserted in the
Dired buffer as a subdirectory, then it deletes that subdirectory
from the buffer as well.
To kill an entire subdirectory (without killing its line in the
parent directory), go to its directory header line and use this
command with a prefix argument (the value does not matter)."
  (interactive "p")
  (let ((fname "cpio-dired-do-kill-lines"))
    (error "%s() is not yet implemented" fname)))

;; L		dired-do-load
(defun cpio-dired-do-load (arg)		;×
  "Load the marked (or next ARG) Emacs Lisp entries."
  (interactive "p")
  (let ((fname "cpio-dired-do-load"))
    (error "%s() is not yet implemented" fname)))

;; P		dired-do-print
(defun cpio-dired-do-print (arg)	;×
  "Print the marked (or next ARG) entries.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "p")
  (let ((fname "cpio-dired-do-print"))
    (error "%s() is not yet implemented" fname)))

;; Q		dired-do-qeuery-replace-regexp
(defun cpio-dired-do-query-replace-regexp (from to &optional delimited) ;✓
  "Do `query-replace-regexp' of FROM with TO, on all marked entries.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-g, RET or q), you can resume the query replace
with the command M-,."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (let ((fname "cpio-dired-do-query-replace-regexp"))
    (error "%s() is not yet implemented" fname)))

;; l		dired-do-redisplay
(defun cpio-dired-do-redisplay (arg)	;×
  "Redisplay all marked (or next ARG) entries.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
M-x dired-reset-subdir-switches.
See Info node `(emacs)Subdir switches' for more details."
  (interactive "p")
  (let ((fname "cpio-dired-do-redisplay"))
    (error "%s() is not yet implemented" fname)))

;; R		dired-do-rename
(defun cpio-dired-do-rename (arg)	;✓
  "Rename current entry or all marked (or next ARG) entries.
When renaming just the current entry, you specify the new name.
When renaming multiple or marked entries, you specify a directory.
This command also renames any buffers that are visiting the entries.
The default suggested for the target directory depends on the value
of `dired-dwim-target', which see."
  ;; HEREHERE This has lots of code stolen from (cpio-dired-do-copy).
  (interactive "p")
  (let ((fname "cpio-dired-do-rename")
	(entries (cpio-dired-marked-entries cpio-dired-marker-char arg))
	(target)
	(target-attrs))
    (setq target (read-string "Target? "
			      nil
			      *cpio-dired-do-rename-history*))
    (cond ((> (length entries) 1)
	   (setq target (cg-strip-right "/" target 'multiples))
	   ;; First handle the case where the entry exists and looks like a directory.
	   (cond ((cpio-entry-exists-p target)
		  (setq target-attrs (cpio-entry-attrs target))
		  (if (/= (logand s-ifdir (cpio-mode-value target-attrs)) s-ifdir)
		      (error "%s(): There's already a non-directory entry called %s." fname target)))
		 ;; HEREHERE Should I create the directory if it doesn't exist?
		 (t t))
	   ;; Now check the existence of the implied targets.
	   (mapc (lambda (en)
		   (if (cpio-entry-exists-p (concat target "/" en))
		       (error "%s(): Target entry [[%s]] is already there." fname (concat target "/" en))))
		 entries)
	   ;; Finally, add the implied targets.
	   (mapc (lambda (en)
		   (cpio-dired-internal-do-rename en (concat target "/" en)))
		 entries))
	  ((cpio-entry-exists-p target)
	   (setq target-attrs (cpio-entry-attrs target))
	   (cond ((= (logand s-ifdir (cpio-mode-value target-attrs)) s-ifdir)
		  (mapc (lambda (en)
			  (cpio-dired-internal-do-rename en (concat target "/" en)))
			entries)
		  (cpio-dired-set-modified))
		 (t
		  (error "%s(): There's already an entry called %s." fname target))))
	  (t
	   (mapc (lambda (en)
		   (cpio-dired-internal-do-rename en target))
		 entries))
	  (cpio-dired-set-modified))))

;; % R		dired-do-rename-regexp
;; % r		dired-do-rename-regexp
(defun cpio-dired-do-rename-regexp (regexp newname &optional arg whole-name) ;×
  "Rename selected entries whose names match REGEXP to NEWNAME.

With non-zero prefix argument ARG, the command operates on the next ARG
entries.  Otherwise, it operates on all the marked entries, or the current
entry if none are marked.

As each match is found, the user must type a character saying
  what to do with it.  For directions, type C-h at that time.
NEWNAME may contain \<n> or \& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.

With a zero prefix arg, renaming by regexp affects the absolute entry name.
Normally, only the non-directory part of the entry name is used and changed."
  (interactive (cpio-dired-mark-read-regexp "Rename"))
  (let ((fname "cpio-dired-do-rename-regexp"))
    (error "%s() is not yet implemented" fname)
    (cpio-dired-set-modified)))

;; A
(defun cpio-dired-do-search (regexp)	;✓
  "Search through all marked entries for matches for REGEXP.
Present the results in *CPIO search results for REGEXP*.
NOTE: This behavior differs from the corresponding function in dired."
  ;; HEREHERE This is not yet functional.
  (interactive "sSearch marked entries (regexp): ")
  (let ((fname "cpio-dired-do-search")
	(entry-names (cpio-dired-get-marked-entries))
	(entry-name)
	(entry-info)
	(entry-attrs)
	(entry-start)
	(entry-end)
	(cpio-dired-buffer (current-buffer))
	(entry-buffer)
	(results ())
	(results-buffer-name (concat "CPIO search results for " regexp "*"))
	(results-buffer))
    (with-current-buffer *cab-parent*
      (setq *cpio-search-re* regexp)
      (setq *cpio-search-entries* entry-names)
      (setq *cpio-search-entry* nil)
      (setq *cpio-search-point* nil)
      (setq entry-name (save-excursion
			 (mapc (lambda (en)
				 (setq entry-attrs (cpio-entry-attrs en))
				 (setq entry-start (cpio-contents-start en))
				 (setq entry-end (+ entry-start (cpio-entry-size entry-attrs)))
				 (goto-char entry-start)
				 (while (re-search-forward regexp entry-end t)
				   (setq *cpio-search-entry* en)
				   (setq *cpio-search-point* (- (point) entry-start))
				   ;; Switch back to cpio-dired-buffer
				   ;; since that's the only place
				   ;; that (cpio-dired-find-entry) makes sense.
				   (with-current-buffer cpio-dired-buffer
				     (with-current-buffer (setq entry-buffer (cpio-dired-find-entry-noselect en))
				       (re-search-forward regexp (point-max) t)
				       (push (format "%s:%d: %s\n" en (count-lines (point-min) (match-beginning 0)) en)
					     results)))))
			       entry-names))))
    (cond (results
	   (with-current-buffer (setq results-buffer (get-buffer-create results-buffer-name))
	     (erase-buffer)
	     (mapc 'insert (nreverse results)))
	   (pop-to-buffer results-buffer))
	  (t nil))))

;; !		dired-do-shell-command
;; X		dired-do-shell-command
(defun cpio-dired-do-shell-command (command &optional arg entry-list) ;×
  ;; I'm not sure this one makes reasonable sense.
  ;; Certainly, you could run a filter on the entry's contents,
  ;; but I can't see a way to truly treat an entry like a file in that way.
  "Run a shell command COMMAND on the marked entries.
If no entries are marked or a numeric prefix arg is given,
the next ARG entries are used.  Just C-u means the current entry.
The prompt mentions the entry(s) or the marker, as appropriate.

If there is a `*' in COMMAND, surrounded by whitespace, this runs
COMMAND just once with the entire entry list substituted there.

If there is no `*', but there is a `?' in COMMAND, surrounded by
whitespace, this runs COMMAND on each entry individually with the
entry name substituted for `?'.

Otherwise, this runs COMMAND on each entry individually with the
entry name added at the end of COMMAND (separated by a space).

`*' and `?' when not surrounded by whitespace have no special
significance for `dired-do-shell-command', and are passed through
normally to the shell, but you must confirm first.

If you want to use `*' as a shell wildcard with whitespace around
it, write `*\"\"' in place of just `*'.  This is equivalent to just
`*' in the shell, but avoids Dired's special handling.

If COMMAND ends in `&', `;', or `;&', it is executed in the
background asynchronously, and the output appears in the buffer
`*Async Shell Command*'.  When operating on multiple entries and COMMAND
ends in `&', the shell command is executed on each entry in parallel.
However, when COMMAND ends in `;' or `;&' then commands are executed
in the background on each entry sequentially waiting for each command
to terminate before running the next command.  You can also use
`dired-do-async-shell-command' that automatically adds `&'.

Otherwise, COMMAND is executed synchronously, and the output
appears in the buffer `*Shell Command Output*'.

This feature does not try to redisplay Dired buffers afterward, as
there's no telling what entries COMMAND may have changed.
Type M-x dired-do-redisplay to redisplay the marked entries.

When COMMAND runs, its working directory is the top-level directory
of the Dired buffer, so output entries usually are created there
instead of in a subdir.

In a noninteractive call (from Lisp code), you must specify
the list of entry names explicitly with the ENTRY-LIST argument, which
can be produced by `dired-get-marked-entries', for example."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "! on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (let ((fname "cpio-dired-do-shell-command"))
    (error "%s() is not yet implemented" fname)))

;; S		dired-do-symlink
(defun cpio-dired-do-symlink (arg)	;×
  "Make symbolic links to current entry or all marked (or next ARG) entries.
When operating on just the current entry, you specify the new name.
When operating on multiple or marked entries, you specify a directory
and new symbolic links are made in that directory
with the same names that the entries currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

For relative symlinks, use M-x dired-do-relsymlink."
  (interactive "p")
  (let ((fname "cpio-dired-do-symlink"))
    (error "%s() is not yet implemented" fname)))

;; % S		dired-do-symlink-regexp
(defun cpio-dired-do-symlink-regexp (regexp newname &optional arg whole-name) ;×
  "Symlink selected entries whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (cpio-dired-mark-read-regexp "SymLink"))
  (let ((fname "cpio-dired-do-symlink-regexp"))
    (error "%s() is not yet implemented" fname)))

;; T		dired-do-touch
(defun cpio-dired-do-touch (arg)	;✓
  "Change the timestamp of the marked (or next ARG) entries."
  ;; HEREHERE To be done:
  ;; Type M-n to pull the entry attributes of the entry at point
  ;; into the minibuffer."
  (interactive "p")
  (let ((fname "cpio-dired-do-touch")
	(entries (cpio-dired-get-marked-entries arg))
	(prompt)
	(human-timestamp)
	(timestamp (current-time))
	(time-re)
	(human-time)
	(time)
	(entry-name))
    (cond ((= (length entries) 0)
	   (error "%s(): No cpio archive entries found." fname))
	  ((or (> arg 1)
	       (> (length entries) 1))
	   (setq prompt (format "Change timestamp of %d files to when? [now]? " (length entries))))
	  ((= arg 1)
	   (setq prompt (format "Change timestamp of %s to when? [now]? " (car entries))))
	  (t
	   (error "%s(): Impossible situation." fname)))
    (setq human-timestamp (read-from-minibuffer prompt))
    (while human-timestamp
      (if (string-equal human-timestamp "")
	  (setq time (current-time))
	(setq time (encode-human-time human-timestamp)))
      (cond (time
	     (dolist (entry entries)
	       (cpio-set-mtime (cpio-entry-attrs entry) time)
	       (cpio-dired-replace-dired-line entry))
	     (setq human-timestamp nil))
	    ((y-or-n-p (format "[[%s]] looks ambiguous. Try again?" time))
	     (setq human-timestamp (read-from-minibuffer prompt)))
	    (t (setq human-timestamp nil))))
    (cpio-dired-set-modified)))

;; % l		dired-downcase
(defun cpio-dired-downcase (arg)	;×
  "Rename all marked (or next ARG) entries to lower case."
  (interactive "p")
  (let ((fname "cpio-dired-downcase"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-dired-extract-all ()	;✓
  "Extract all the entries in the current CPIO arhcive."
  (interactive)
  (let ((fname "cpio-dired-extract-all"))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s() only makes sense in a cpio-dired buffer." fname))
    (cond (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (cpio-dired-extract-all)))
	  (t
	   (cpio-extract-all)))))

(defun cpio-dired-extract-entries (arg)	;✓
  "Extract the marked entries in the current CPIO dired buffer."
  (interactive "p")
  (let ((fname "cpio-dired-extract-entries")
	(files (or (cpio-dired-get-marked-entries)
		   (list (cpio-dired-get-entry-name)))))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s() only makes sense in a cpio-dired buffer." fname))
    (cond (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (mapc 'cpio-extract-entry files)))
	  (t
	   (mapc 'cpio-extract-entry files)))))

;; a		dired-find-alternate-file
(defun cpio-dired-find-alternate-entry () ;×
  "In Dired, visit this entry or directory instead of the Dired buffer."
  (interactive)
  (let ((fname "cpio-dired-find-alternate-entry"))
    (error "%s() is not yet implemented" fname)))

;; e .. f		dired-find-file
;; RET		dired-find-file
(defun cpio-dired-find-entry ()		;✓
  "In a cpio UI buffer, visit the contents of the entry named on this line.
Return the buffer containing those contents."
  (interactive)
  (let ((fname "cpio-dired-find-entry")
	(find-file-run-dired t)
	(local-entry-name (cpio-dired-get-entry-name))
	(entry-buf))
    (cond ((null local-entry-name)
	   (message "%s(): Could not get entry name." fname))
	  (t
	   (with-current-buffer (setq entry-buf (cpio-find-entry local-entry-name))
	     (cpio-entry-contents-mode))
	   (pop-to-buffer entry-buf)))))

;; n		dired-next-line
;; o		dired-find-file-other-window
(defun cpio-dired-find-entry-other-window () ;✓
  "In Dired, visit this entry or directory in another window."
  (interactive)
  (let ((fname "cpio-dired-find-entry-other-window"))
    (error "%s() is not yet implemented" fname)))

;; #		dired-flag-auto-save-files
(defun cpio-dired-flag-auto-save-entries (&optional unflag-p) ;✓
  "Flag for deletion entries whos names suggest they are auto save entries.
A prefix argument says to unmark or unflag those files instead."
  (interactive)
  (let ((fname "cpio-dired-flag-auto-save-entries"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired style buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (< (point) (point-max))
	(if (string-match-p "\\`#" (cpio-dired-get-entry-name))
	    (if unflag-p
		(cpio-dired-unmark 1)
	      (cpio-dired-mark-this-entry cpio-dired-del-marker))
	  (cpio-dired-next-line 1))))))

;; ~		dired-flag-backup-entries
(defun cpio-dired-flag-backup-entries (unflag-p) ;✓✓✓✓
  ;; Modeled very closely on the corresponding dired function
  "Flag all backup entries (names ending with `~') for deletion.
With prefix argument, unmark or unflag these entries."
  (interactive "P")
  (let ((fname "cpio-dired-flag-backup-entries")
	(dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     (and (save-excursion (end-of-line)
			  (eq (preceding-char) ?~))
	  (not (looking-at-p dired-re-dir))
	  (let ((entry-name (cpio-dired-get-entry-name))) ;The main modification for cpio-dired-mode.
	    (if entry-name (backup-file-name-p entry-name))))
     "backup file")))

;; % d		dired-flag-entries-regexp
(defun cpio-dired-flag-entries-regexp (regexp) ;×
  "In Dired, flag all entries containing the specified REGEXP for deletion.
The match is against the non-directory part of the entry name.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (cpio-dired-mark-read-regexp "SymLink"))
  (let ((fname "cpio-dired-flag-entries-regexp"))
    (error "%s() is not yet implemented" fname)))

;; d		dired-flag-file-deletion
(defun cpio-dired-flag-entry-deletion (arg) ;✓
  "In a cpio-dired style buffer,
flag the current line's entry for deletion.
If the region is active, flag all entries in the region.
Otherwise, with a prefix arg, flag entries on the next ARG lines.

If on a subdir headerline, flag all its entries except `.' and `..'.
If the region is active in Transient Mark mode, flag all entries
in the active region."
  (interactive "p")
  (let ((fname "cpio-dired-flag-entry-deletion")
	(cpio-dired-marker-char cpio-dired-del-marker))
    (cpio-dired-mark arg)))

;; M-s a		Prefix Command
;; M-s f		Prefix Command

;; % &		dired-flag-garbage-entries
(defvar cpio-dired-garbage-entries-regexp dired-garbage-files-regexp
  "Regular expression to match in cpio-dired-flag-garbage-entries.")
(setq cpio-dired-garbage-entries-regexp dired-garbage-files-regexp)

(defun cpio-dired-flag-garbage-entries () ;✓✓✓
  "Flag for deletion all entries that match `cpio-dired-garbage-entries-regexp'."
  (interactive)
  (let ((fname "cpio-dired-flag-garbage-entries")
	(entry-name))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (save-match-data
	(while (< (point) (point-max))
	  (setq entry-name (cpio-dired-get-entry-name))
	  (if (string-match-p cpio-dired-garbage-entries-regexp entry-name)
	      (cpio-dired-flag-entry-deletion 1)
	    (dired-next-line 1)))))))

;; j		dired-goto-entry
(defun cpio-dired-goto-entry (entry)	;✓✓✓
  "Go to line describing entry ENTRY in this Dired buffer."
  (interactive "sGoto entry: ")
  (let ((fname "cpio-dired-goto-entry")
	(this-entry))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (unless (cpio-entry-exists-p entry)
      (error "%s(): There's no entry [[%s]]." fname entry))
    (goto-char (point-min))
    (re-search-forward (concat " " entry "$"))
    (cpio-dired-move-to-entry-name)))

;; M-$		dired-hide-all
(defun cpio-dired-hide-all ()		;×
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use M-x dired-hide-subdir to (un)hide a particular subdirectory."
  (interactive)
  (let ((fname "cpio-dired-hide-all"))
    (error "%s() is not yet implemented" fname)))

;; $		dired-hide-subdir
(defun cpio-dired-hide-subdir (arg)	;×
  ;; Does this really make sense here?
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use M-x dired-hide-all to (un)hide all directories."
  (interactive "p")
  (let ((fname "cpio-dired-hide-subdir"))
    (warn "%s() is not obvious." fname)))

;;
;; M-s f C-s	dired-isearch-filenames
(defun cpio-dired-isearch-entry-names () ;×
  "Search for a string using Isearch only in entry names in the Dired buffer."
  (interactive)
  (let ((fname "cpio-dired-isearch-entry-names"))
    (error "%s() is not yet implemented" fname)))

;; M-s a ESC	Prefix Command
;;
;; M-s f C-M-s	dired-isearch-filenames-regexp
(defun cpio-dired-isearch-entry-names-regexp () ;×
  "Search for a regexp using Isearch only in entry names in the cpio-dired buffer."
  (interactive)
  (let ((fname "cpio-dired-isearch-entry-names-regexp"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-dired-kill ()		;✓
  "Kill the current cpio dired-style buffer
along with it's corresponding archive buffer
and any affiliated buffers thereof."
  (interactive)
  (let ((fname "cpio-dired-kill"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're trying to kill a cpio buffer, but you're not in cpio-mode." fname))
    (if *cab-parent*
	(cond ((buffer-live-p *cab-parent*)
	       (if (and (called-interactively-p 'interactive)
			(cpio-dired-modified-p))
		   (if (y-or-n-p "You've made changes to the archive. Save first? ")
		       (cpio-dired-save-archive)))
	       (with-current-buffer *cab-parent*
		 (kill-buffer))
	       ;; This should be a noop.
	       (kill-buffer))
	      (t
	       (warn "%s(): Archive buffer [[%s]] is not there." fname (file-name-nondirectory (buffer-file-name *cab-parent*)))
	       (remove-hook 'kill-buffer-hook 'cab-kill-buffer-hook)
	       (kill-buffer)))
      (kill-buffer))))

;; m		dired-mark
(defun cpio-dired-mark (arg &optional interactive) ;✓
  "If the region is active, mark all entries in the region.
Otherwise, with a prefix arg, mark entries on the next ARG lines."
  (interactive "p")
  (let ((fname "cpio-dired-mark")
	(start (if (and interactive (use-region-p))
		   (min (point) (mark))
		 nil))
	(end (if (and interactive (use-region-p))
		 (max (point) (mark))
	       nil))
	(entry-name))
    (cond ((and interactive (use-region-p))
	   (save-excursion
	     (let ((beg (region-beginning))
		   (end (region-end)))
	       (dired-mark-files-in-region
		(progn (goto-char beg) (line-beginning-position))
		(progn (goto-char end) (line-beginning-position))))))
	  (arg
	   (let ((inhibit-read-only t))
	     (dired-repeat-over-lines
	      (prefix-numeric-value arg)
	      (function (lambda () (delete-char 1) (insert cpio-dired-marker-char)))))))))

;; * /		dired-mark-directories
(defun cpio-dired-mark-directories ()	;✓
  "Mark all directory entry lines except `.' and `..'.
With prefix argument, unmark or unflag all those entries."
  (interactive)
  (let ((fname "cpio-dired-mark-directories"))
    (error "%s() is not yet implemented" fname)))

;; % g		dired-mark-entries-containing-regexp
(defun cpio-dired-mark-entries-containing-regexp (regexp) ;×
  ;;     dired-mark-entries-containing-regexp is an alias for `dired-mark-entries-containing-regexp',
  ;;     which is not defined.  Please make a bug report.
  "Mark all entries with contents containing REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked."
  (interactive
   (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
			      " files containing (regexp): ")
		      nil 'dired-regexp-history)
	 (if current-prefix-arg ?\040)))
  (let ((fname "cpio-dired-mark-entries-containing-regexp"))
    (error "%s() is not yet implemented" fname)))

;; % m		dired-mark-entries-regexp
;; * %		dired-mark-entries-regexp
(defun cpio-dired-mark-entries-regexp (regexp &optional marker-char) ;✓
  "Mark all entries matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\.o$' for
object entries--just `.o' will mark more than you might think."
  (interactive (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
					  " files (regexp): ")
				  nil 'dired-regexp-history)))
  (unless marker-char (setq marker-char cpio-dired-marker-char))
  (let ((fname "cpio-dired-mark-entries-regexp")
	(cpio-dired-marker-char (or marker-char cpio-dired-marker-char))
	(entry-name))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (< (point) (point-max))
	(setq entry-name (cpio-dired-get-entry-name))
	(if (string-match-p regexp entry-name)
	    (cpio-dired-mark 1 marker-char)
	  (cpio-dired-next-line 1))))))

;; * *		dired-mark-executables
(defun cpio-dired-mark-executables (arg) ;✓✓✓
  "Mark all executable entries.
With prefix argument, unmark or unflag all those entries."
  (interactive "P")
  (let ((fname "cpio-dired-mark-executables")
	(this-mode))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (< (point) (point-max))
	(setq this-mode (cpio-mode-value (cpio-entry-attrs (cpio-dired-get-entry-name))))
	(if (or (/= 0 (logand s-ixusr this-mode))
		(/= 0 (logand s-ixgrp this-mode))
		(/= 0 (logand s-ixoth this-mode)))
	    (cpio-dired-mark-this-entry)
	  (cpio-dired-next-line 1))))))

;; * m		dired-mark
;; Defined above.
;; * s		dired-mark-subdir-entries
(defun cpio-dired-mark-subdir-entries () ;✓✓✓
  "Mark all entries except `.' and `..' in current subdirectory.
If the Dired buffer shows multiple directories, this command
marks the entries listed in the subdirectory that point is in."
  (interactive)
  (let ((fname "cpio-dired-mark-subdir-entries")
	(this-mode))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (< (point) (point-max))
	(setq this-mode (cpio-mode-value (cpio-entry-attrs (cpio-dired-get-entry-name))))
	(if (/= 0 (logand s-ifdir this-mode))
	    (cpio-dired-mark-this-entry)
	  (cpio-dired-next-line 1))))))

;; * @		dired-mark-symlinks
(defun cpio-dired-mark-symlinks (unflag-p) ;✓✓✓
  "Mark all symbolic links.
With prefix argument, unmark or unflag all those entries."
  (interactive "P")
  (let ((fname "cpio-dired-mark-symlinks")
	(this-mode))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (while (< (point) (point-max))
	(setq this-mode (cpio-mode-value (cpio-entry-attrs (cpio-dired-get-entry-name))))
	(if (= s-iflnk (logand s-iflnk this-mode))
	    (cpio-dired-mark-this-entry)
	  (cpio-dired-next-line 1))))))

(defun cpio-dired-mark-this-entry (&optional char) ;✓
  "Mark the entry on the current line with the given CHAR.
If CHAR is not given, then use cpio-dired-marker-char.
CONTRACT: You must be allowed to operate on that entry."
  (unless char (setq char cpio-dired-marker-char))
  (let ((fname "cpio-dired-mark-this-entry"))
    (beginning-of-line)
    (with-writable-buffer
     (delete-char 1)
     (insert (char-to-string char)))
    (cpio-dired-next-line 1)))

(defun cpio-dired-marker-regexp ()
  "Return a regular expression to match a marked entry."
  (concat "^" (regexp-quote (char-to-string cpio-dired-marker-char))))

;; i		dired-maybe-insert-subdir
(defun cpio-dired-maybe-insert-subdir () ;×
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type M-x dired-do-redisplay to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
HEREHERE Archives don't hold subdirectories the same way a file system does.
You can reset all subdirectory switches to the default using
M-x dired-reset-subdir-switches.
See Info node `(emacs)Subdir switches' for more details."
  (interactive)
  (let ((fname "cpio-dired-maybe-insert-subdir"))
    (error "%s() is not yet implemented" fname)))

;; <follow-link>	mouse-face
;; <mouse-2>	dired-mouse-find-file-other-window
(defun cpio-dired-mouse-find-entry-other-window () ;×
  "In a cpio UI window, visit the entry or directory name you click on."
  (interactive)
  (let ((fname "cpio-dired-mouse-find-entry-other-window"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-dired-move-to-entry-name ()
  "Move the point to the beginning of the entry on the current line
if there is one."
  (let ((fname "cpio-dired-move-to-entry-name"))
    (beginning-of-line)
    (save-match-data
      (if (looking-at *cpio-dired-entry-regexp*)
	  (goto-char (match-beginning *cpio-dired-name-idx*))))))

;; >		dired-next-dirline
(defun cpio-dired-next-dirline (arg &optional opoint) ;✓
  "Goto ARGth next directory entry line."
  (interactive "p")
  (unless opoint (setq opoint (point)))
  (let ((fname "cpio-dired-next-dirline"))
    (while (and (< 0 arg)
		(re-search-forward *cpio-dirline-re* (point-max) t))
      (setq arg (1- arg)))
    (cpio-dired-move-to-entry-name)))

;; C-t		Prefix Command
;; ESC		Prefix Command
;; SPC		dired-next-line
;; <remap> <next-line>		dired-next-line
(defun cpio-dired-next-line (arg)	;✓
  "In a cpio UI buffer, move down ARG lines then position at the entry's name.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (let ((fname "cpio-dired-next-line"))
    (forward-line arg)
    (cpio-dired-move-to-entry-name)))

;; M-}		dired-next-marked-file
;; * C-n		dired-next-marked-file
(defun cpio-dired-next-marked-entry (wrap) ;×
  "Move to the previous marked entry.
If WRAP is non-nil, wrap around to the end of the buffer if we
reach the beginning of the buffer."
  (let ((fname "cpio-dired-next-marked-entry"))
    (error "%s() is not yet implemented" fname)))

;; C-M-n		dired-next-subdir
(defun cpio-dired-next-subdir ()	;×
  "Go to next subdirectory, regardless of level."
  (interactive)
  (let ((fname "cpio-dired-next-subdir"))
    (error "%s() is not yet implemented" fname)))

;; 0 .. 9		digit-argument
;; :		Prefix Command
;; <		dired-prev-dirline
(defun cpio-dired-prev-dirline (arg)	;✓
  "Goto ARGth previous directory entry line."
  (interactive "p")
  (let ((fname "cpio-dired-prev-dirline"))
    (while (and (< 0 arg)
		(prog2
		    (beginning-of-line)
		    (re-search-backward *cpio-dirline-re* (point-min) t)))
      (setq arg (1- arg)))
    (cpio-dired-move-to-entry-name)))

;; M-s		Prefix Command
;; M-{		dired-prev-marked-file
;; * C-p		dired-prev-marked-file
(defun cpio-dired-prev-marked-entry (arg wrap) ;×
  "Move to the previous marked entry.
If WRAP is non-nil, wrap around to the end of the buffer if we
reach the beginning of the buffer."
  (interactive "p\np")
  (let ((fname "cpio-dired-prev-marked-entry"))
    (error "%s() is not yet implemented" fname)))

;; C-M-p		dired-prev-subdir
(defun cpio-dired-prev-subdir ()	;×
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  (interactive)
  (let ((fname "cpio-dired-prev-subdir"))
    (error "%s() is not yet implemented" fname)))

;; p		dired-previous-line
;; <remap> <previous-line>		dired-previous-line
(defun cpio-dired-previous-line (arg)	;✓
  "Move up lines then position at entry name.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (let ((fname "cpio-dired-previous-line"))
    (forward-line (- arg))
    (cpio-dired-move-to-entry-name)))

;; q		quit-window
(defun cpio-dired-quit-window (&optional kill window) ;✓
  "Quit WINDOW and bury its buffer.
WINDOW must be a live window and defaults to the selected one.
With prefix argument KILL non-nil, kill the buffer instead of
burying it.

According to information stored in WINDOW's `quit-restore' window
parameter either (1) delete WINDOW and its frame, (2) delete
WINDOW, (3) restore the buffer previously displayed in WINDOW,
or (4) make WINDOW display some other buffer than the present
one.  If non-nil, reset `quit-restore' parameter to nil."
  (interactive "P")
  (let ((fname "cpio-dired-quit-window")
	(buffer (window-buffer)))
    (cond (kill
	   (kill-buffer buffer))
	  (t
	   (delete-window (selected-window))
	   (bury-buffer buffer)))))

(defun cpio-dired-save-archive ()	;✓
  "Save the archive associated with this cpio-dired buffer."
  (interactive)
  (let ((fname "cpio-dired-save-archive"))
    (unless (or (eq major-mode 'cpio-dired-mode)
		(eq major-mode 'cpio-mode))
      (error "%s(): You can only save an archive from a cpio-dired buffer." fname))
    (cond (*cab-parent*
	   (with-current-buffer *cab-parent*
	     (cpio-dired-save-archive))
	   (mapc (lambda (cen)
		   (cpio-dired-replace-dired-line (car cen)))
		 (cpio-catalog))
	   (cpio-dired-set-unmodified))
	  (t
	   (cpio-delete-trailer)
	   (with-writable-buffer
	    (mapc (lambda (cen)		;(cons name entry-contents)
		    (let* ((entry-info (cdr cen))
			   (attrs (aref entry-info *cpio-catalog-entry-attrs-idx*))
			   (header-start (marker-position
					  (aref entry-info 1)))
			   (header-end   (marker-position
					  (aref entry-info 2)))
			   (header-string (cpio-make-header-string
					   attrs)))
		      (goto-char header-start)
		      (delete-region header-start header-end)
		      (set-marker (aref entry-info 1) (point)) ;Redundant?
		      (insert header-string)
		      (goto-char (+ (aref entry-info 1)
				    (length header-string)))
		      (set-marker (aref entry-info 2) (point))
		      (forward-char (cpio-entry-size attrs))
		      (while (looking-at-p "\0")
			(delete-char 1))))
		  ;; Do the adjustments backwards to ensure that the resulting markers are correct.
		  (reverse *cpio-catalog*))
	    ;; Adjust all the entry padding.
	    (mapc (lambda (cen)
		    (let* ((entry (cdr cen))
			   (attrs                         (aref entry *cpio-catalog-entry-attrs-idx*))
			   (header-start (marker-position (aref entry *cpio-catalog-entry-header-start-idx*)))
			   (entry-start  (marker-position (aref entry *cpio-catalog-entry-contents-start-idx*)))
			   (cpio-set-entry-unmodified entry)
			   (header-string (cpio-make-header-string attrs))
			   (local-where)
			   (padding-length))
		      (goto-char (+ entry-start (cpio-entry-size attrs)))
		      (setq local-where (mod (1- (point))
					     *cpio-padding-modulus*))
		      (cond ((= 0 local-where)
			     (setq padding-length 0))
			    (t
			     (setq padding-length (- *cpio-padding-modulus* local-where))))
		      (insert (make-string padding-length ?\0))))
		  *cpio-catalog*)
	    (cpio-adjust-trailer))
	   (basic-save-buffer)))))

;; y		dired-show-file-type
(defun cpio-dired-show-entry-type (entry &optional deref-symlinks) ;×
  "Print the type of ENTRY, according to the `entry' command.
If you give a prefix to this command, and ENTRY is a symbolic
link, then the type of the entry linked to by ENTRY is printed
instead."
  (interactive (list (dired-get-filename t) current-prefix-arg))
  (let ((fname "cpio-dired-show-entry-type"))
    (error "%s() is not yet implemented" fname)))

;; s		dired-sort-toggle-or-edit
(defun cpio-dired-sort-toggle-or-edit () ;×
  "Toggle sorting by date, and refresh the Dired buffer.
With a prefix argument, edit the current listing switches instead."
  (interactive)
  (let ((fname "cpio-dired-sort-toggle-or-edit"))
    (error "%s() is not yet implemented" fname)))

;; ?		dired-summary
(defun cpio-dired-summary ()		;✓
  "Summarize basic cpio-dired commands."
  (interactive)
  (let ((fname "cpio-dired-summary"))
    ;>> this should check the key-bindings and use substitute-command-keys if non-standard
    (message
     "d-elete, u-ndelete, x-punge, f-ind, o-ther window, R-ename, C-opy, h-elp")))

;; * t		dired-toggle-marks
;; t		dired-toggle-marks
(defun cpio-dired-toggle-marks ()	;×
  "Toggle marks: marked entries become unmarked, and vice versa.
Entries marked with other flags (such as `D') are not affected.
`.' and `..' are never toggled.
As always, hidden subdirs are not affected."
  (interactive)
  (let ((fname "cpio-dired-toggle-marks"))
    (error "%s() is not yet implemented" fname)))

;; <remap> <read-only-mode>	dired-toggle-read-only
;; <remap> <toggle-read-only>	dired-toggle-read-only
(defun cpio-dired-toggle-read-only ()	;×
  ;; HEREHERE Figure out very precisely what this means for M-x cpio-mode.
  "Edit Dired buffer with Wdired, or make it read-only.
If the current buffer can be edited with Wdired, (i.e. the major
mode is `dired-mode'), call `wdired-change-to-wdired-mode'.
Otherwise, toggle `read-only-mode'."
  (interactive)
  (let ((fname "cpio-dired-toggle-read-only"))
    (error "%s() is not yet implemented" fname)))

;; C-M-d		dired-tree-down
(defun cpio-dired-tree-down ()		;×
  "Go down in the dired tree."
  (interactive)
  (let ((fname "cpio-dired-tree-down"))
    (error "%s() is not yet implemented" fname)))

;; C-M-u		dired-tree-up
(defun cpio-dired-tree-up (arg)		;×
  "Go up ARG levels in the dired tree."
  (interactive)
  (let ((fname "cpio-dired-tree-up"))
    (error "%s() is not yet implemented" fname)))

;; <remap> <undo>			dired-undo
;; <remap> <advertised-undo>	dired-undo
(defun cpio-dired-undo ()		;×
  "Search for a string using Isearch only in entry names in the Dired buffer.
You can use it to recover marks, killed lines or subdirs."
  (interactive)
  (let ((fname "cpio-dired-undo"))
    (error "%s() is not yet implemented" fname)))

;; u		dired-unmark
;; * u		dired-unmark
(defun cpio-dired-unmark (arg)		;✓
  "If the region is active, unmark all entries in the region.
Otherwise, with a prefix arg, unmark entries on the next ARG lines.

If looking at a subdir, unmark all its entries except `.' and `..'.
If the region is active in Transient Mark mode, unmark all entries
in the active region."
  ;; HEREHERE This shares a lot of structure sith M-x cpio-dired-mark.
  (interactive "p")
  (let ((fname "cpio-dired-unmark"))
    (cond ((save-excursion (beginning-of-line) (looking-at-p *cpio-dired-entry-regexp*))
	   (let ((inhibit-read-only t))
	     (while (and (< 0 arg)
			 (< (point) (point-max)))
	       (cpio-dired-mark-this-entry ?\s)
	       ;; (cpio-dired-next-line 1)
	       (setq arg (1- arg)))))
	  (t nil))
	  (cpio-dired-move-to-entry-name)))

;; * ?		dired-unmark-all-entries
;; M-DEL		dired-unmark-all-entries
(defun cpio-dired-unmark-all-entries (mark arg) ;✓
  "Remove a specific mark (or any mark) from every entry.
After this command, type the mark character to remove,
or type RET to remove all marks.
With prefix arg, query for each marked file.
Type C-h at that time for help."
  (interactive "sRemove marks (RET means all): \nP")
  (let ((fname "cpio-dired-unmark-all-entries")
	entry)
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (cond ((string-equal mark "")
	   (cpio-dired-unmark-all-marks))
	  (t
	   (save-excursion
	     (cpio-dired-move-to-first-entry)
	     (beginning-of-line)
	     (while (< (point) (point-max))
	       (setq entry (cpio-dired-get-entry-name))
	       (beginning-of-line)
	       (if (looking-at-p mark)
		   (with-writable-buffer
		    (cond ((and arg
				(y-or-n-p (format "Unmark entry `%s'? " entry)))
			   (delete-char 1)
			   (insert " "))
			  (t
			   (delete-char 1)
			   (insert " "))))
		 (cpio-dired-next-line 1))))))))

;; * !		dired-unmark-all-marks
;; U		dired-unmark-all-marks
(defun cpio-dired-unmark-all-marks ()	;✓
  "Remove all marks from all entries in the Dired buffer."
  (interactive)
  (let ((fname "cpio-dired-unmark-all-marks"))
    (unless (eq major-mode 'cpio-dired-mode)
      (error "%s(): You're not in a cpio-dired buffer." fname))
    (save-excursion
      (cpio-dired-move-to-first-entry)
      (with-writable-buffer
       (while (< (point) (point-max))
	 (beginning-of-line)
	 (delete-char 1)
	 (insert " ")
	 (cpio-dired-next-line 1))))))

;; DEL		dired-unmark-backward
;; * DEL		dired-unmark-backward
(defun cpio-dired-unmark-backward (&optional arg) ;×
  "In a cpio UI buffer, move up lines and remove marks or deletion flags there.
Optional prefix ARG says how many lines to unmark/unflag; default
is one line.
If the region is active in Transient Mark mode, unmark all entries
in the active region."
  (interactive "p")
  (let ((fname "cpio-dired-unmark-backward"))
    (error "%s() is not yet implemented" fname)))

;; ^		dired-up-directory
(defun cpio-dired-up-directory ()	;×
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.
If OTHER-WINDOW (the optional prefix arg), display the parent
directory in another window."
  (interactive)
  (let ((fname "cpio-dired-up-directory"))
    (error "%s() is not yet implemented" fname)))

;; % u		dired-upcase
(defun cpio-dired-upcase (arg)		;×
  "Rename all marked (or next ARG) entries to upper case."
  (interactive)
  (let ((fname "cpio-dired-upcase"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-dired-view-archive ()	;×
  "Switch to the buffer holding the cpio archive for this cpio-dired style buffer."
  (interactive)
  (let ((fname "cpio-dired-view-archive"))
    (switch-to-buffer *cab-parent*)))

;; v		dired-view-file
(defun cpio-dired-view-entry ()		;×
  "In Dired, examine a entry in view mode, returning to Dired when done.
When entry is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((fname "cpio-dired-view-entry"))
    (error "%s() is not yet implemented" fname)))

;; 
;; : d		epa-dired-do-decrypt
(defun cpio-epa-dired-do-decrypt ()	;×
  "Decrypt marked entries."
  (interactive)
  (let ((fname "epa-dired-do-decrypt"))
    (error "%s() is not yet implemented" fname)))

;; : e		epa-dired-do-encrypt
(defun cpio-epa-dired-do-encrypt ()	;×
  "Encrypt marked entries."
  (interactive)
  (let ((fname "epa-dired-do-encrypt"))
    (error "%s() is not yet implemented" fname)))

;; : s		epa-dired-do-sign
(defun cpio-epa-dired-do-sign ()	;×
  "Sign marked entries."
  (interactive)
  (let ((fname "epa-dired-do-sign"))
    (error "%s() is not yet implemented" fname)))

;; : v		epa-dired-do-verify
(defun cpio-epa-dired-do-verify ()	;×
  "Verify marked entries."
  (interactive)
  (let ((fname "epa-dired-do-verify"))
    (error "%s() is not yet implemented" fname)))

;; C-t r		image-dired-delete-tag
(defun cpio-image-dired-delete-tag (arg)	;×
  "Remove tag for selected entry(s).
With prefix argument ARG, remove tag from entry at point."
  (interactive "P")
  (let ((fname "image-dired-delete-tag"))
    (error "%s() is not yet implemented" fname)))

;; C-t c		image-dired-dired-comment-entries
(defun cpio-image-dired-dired-comment-entries ()	;×
  "Add comment to current or marked entries in dired."
  (interactive)
  (let ((fname "image-dired-dired-comment-entries"))
    (error "%s() is not yet implemented" fname)))

;; C-t x		image-dired-dired-display-external
(defun cpio-image-dired-dired-display-external ()	;×
  "Display entry at point using an external viewer."
  (interactive)
  (let ((fname "image-dired-dired-display-external"))
    (error "%s() is not yet implemented" fname)))

;; C-t i		image-dired-dired-display-image
(defun cpio-image-dired-dired-display-image (&optional arg)	;×
  "Display current image entry.
See documentation for `image-dired-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "p")
  (let ((fname "image-dired-dired-display-image"))
    (error "%s() is not yet implemented" fname)))

;; C-t e		image-dired-dired-edit-comment-and-tags
(defun cpio-image-dired-dired-edit-comment-and-tags ()	;×
  "Edit comment and tags of current or marked image entries.
Edit comment and tags for all marked image entries in an
easy-to-use form."
  (interactive)
  (let ((fname "image-dired-dired-edit-comment-and-tags"))
    (error "%s() is not yet implemented" fname)))

;; <remap>		Prefix Command
;;
;; C-t C-t		image-dired-dired-toggle-marked-thumbs
(defun cpio-image-dired-dired-toggle-marked-thumbs (arg)	;×
  "Toggle thumbnails in front of entry names in the dired buffer.
If no marked entry could be found, insert or hide thumbnails on the
current line.  ARG, if non-nil, specifies the entries to use instead
of the marked entries.  If ARG is an integer, use the next ARG (or
previous -ARG, if ARG<0) entries."
  (interactive "p")
  (let ((fname "image-dired-dired-toggle-marked-thumbs"))
    (error "%s() is not yet implemented" fname)))

;; C-t .		image-dired-display-thumb
(defun cpio-image-dired-display-thumb (arg)		;×
  "Shorthand for `image-dired-display-thumbs' with prefix argument."
  (interactive "p")
  (let ((fname "image-dired-display-thumb"))
    (error "%s() is not yet implemented" fname)))

;; C-t d		image-dired-display-thumbs	;×
(defun cpio-image-dired-display-thumbs (&optional arg append do-not-pop)
  "Display thumbnails of all marked entries, in `image-dired-thumbnail-buffer'.
If a thumbnail image does not exist for a entry, it is created on the
fly.  With prefix argument ARG, display only thumbnail for entry at
apoint (this is useful if you have marked some entries but want to show
another one).

Recommended usage is to split the current frame horizontally so that
you have the dired buffer in the left window and the
`image-dired-thumbnail-buffer' buffer in the right window.

With optional argument APPEND, append thumbnail to thumbnail buffer
instead of erasing it first.

Optional argument DO-NOT-POP controls if `pop-to-buffer' should be
used or not.  If non-nil, use `display-buffer' instead of
`pop-to-buffer'.  This is used from functions like
`image-dired-next-line-and-display' and
`image-dired-previous-line-and-display' where we do not want the
thumbnail buffer to be selected."
  (interactive "P")
  (let ((fname "image-dired-display-thumbs"))
    (error "%s() is not yet implemented" fname)))

;; C-t a		image-dired-display-thumbs-append
(defun cpio-image-dired-display-thumbs-append ()	;×
  "Append thumbnails to `image-dired-thumbnail-buffer'."
  (interactive)
  (let ((fname "image-dired-display-thumbs-append"))
    (error "%s() is not yet implemented" fname)))

;; C-t j		image-dired-jump-thumbnail-buffer
(defun cpio-image-dired-jump-thumbnail-buffer ()	;×
  "Jump to thumbnail buffer."
  (interactive)
  (let ((fname "image-dired-jump-thumbnail-buffer"))
    (error "%s() is not yet implemented" fname)))	;×

;; C-t f		image-dired-mark-tagged-entries
(defun cpio-image-dired-mark-tagged-entries ()	;×
  ;; HREHERE What should I do with this?
  "Use regexp to mark entries with matching tag.
A `tag' is a keyword, a piece of meta data, associated with an
image entry and stored in image-dired's database entry.  This command
lets you input a regexp and this will be matched against all tags
on all image entries in the database entry.  The entries that have a
matching tag will be marked in the dired buffer."
  (interactive)
  (let ((fname "image-dired-mark-tagged-entries"))
    (error "%s() is not yet implemented" fname)))

;; C-t t		image-dired-tag-entries
(defun cpio-image-dired-tag-entries (arg)	;×
  "Tag marked entry(s) in dired.  With prefix ARG, tag entry at point."
  (interactive "P")
  (let ((fname "image-dired-tag-entries"))
    (error "%s() is not yet implemented" fname)))

;; g		revert-buffer
(defun cpio-revert-buffer ()		;✓
  "Replace current buffer text with the dired-style view
of the corresponding CPIO archive.
This undoes all changes since the entry was visited or saved.
With a prefix argument, offer to revert from latest auto-save entry, if
that is more recent than the visited archive.

This command also implements an interface for special buffers
that contain text which doesn't come from a entry, but reflects
some other data instead (e.g. Dired buffers, `buffer-list'
buffers).  This is done via the variable `revert-buffer-function'.
In these cases, it should reconstruct the buffer contents from the
appropriate data.

When called from Lisp, the first argument is IGNORE-AUTO; only offer
to revert from the auto-save entry when this is nil.  Note that the
sense of this argument is the reverse of the prefix argument, for the
sake of backward compatibility.  IGNORE-AUTO is optional, defaulting
to nil.

Optional second argument NOCONFIRM means don't ask for confirmation
at all.  (The variable `revert-without-query' offers another way to
revert buffers without querying for confirmation.)

Optional third argument PRESERVE-MODES non-nil means don't alter
the entries modes.  Normally we reinitialize them using `normal-mode'.

This function binds `revert-buffer-in-progress-p' non-nil while it operates.

This function calls the function that `revert-buffer-function' specifies
to do the work, with arguments IGNORE-AUTO and NOCONFIRM.
The default function runs the hooks `before-revert-hook' and
`after-revert-hook'."
  (interactive)
  (let ((fname "cpio-revert-buffer"))
    (if *cab-parent*
	(if (buffer-live-p *cab-parent*)
	    (with-current-buffer *cab-parent*
	      (cpio-revert-buffer))
	  (find-file (buffer-file-name *cab-parent*))
	  (cpio-mode))
      (cpio-mode))))

;; S-SPC		scroll-down-command
(defun cpio-scroll-down-command (arg)	;×
  "Scroll text of selected window down ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
scroll window further, move cursor to the top line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen."
  (interactive "p")
  (let ((fname "scroll-down-command"))
    (error "%s() is not yet implemented" fname)))

(defun cpio-tags-loop-continue ()	;✓
  "Continue the search through marked entries in a cpio-dired buffer."
  ;; HERREHERE Do I want this? Is it used?
  (interactive)
  (let ((fname "cpio-tags-loop-continue")
	(entry-buffer (get-buffer-create
		       (cpio-contents-buffer-name (with-current-buffer *cab-parent*
						    *cpio-search-entry*))))
	(search-point (with-current-buffer *cab-parent*
			*cpio-search-point*))
	(regex (with-current-buffer *cab-parent*
		 *cpio-search-re*))
	(entry-attrs)
	(contents-size)
	(contents-start))
    (error "%s(): is not yet implemented." fname)
    (switch-to-buffer entry-buffer)
    (goto-char search-point)
    (unless (re-search-forward regex (point-max) t)
      (catch 'found-one
	(with-current-buffer *cab-parent*
	  (while (setq *cpio-search-entry* (pop *cpio-search-entries*))
	    (setq entry-attrs (cpio-entry-attrs *cpio-search-entry*))
	    (goto-char (cpio-contents-start *cpio-search-entry*))
	    (cond ((re-search-forward *cpio-search-re* (+ contents-start (cpio-entry-size entry-attrs)))
		   (cpio-dired-find-entry)
		   (goto-char (point-min))
		   (re-search-forward *cpio-search-re* (point-max) t)
		   (throw 'found-one t))
		  (t nil)))))
      (unless *cpio-search-entries*
	(setq *cpio-search-entry* nil)
	(setq *cpio-search-re* nil)
	(setq *cpio-search-point* nil)))))

(defun cpio-dired-hide-details-mode ()
  "Toggle visibility of detailed information in current Dired buffer.
When this minor mode is enabled, details such as file ownership and
permissions are hidden from view."
  (let ((fname "cpio-dired-hide-details-mode"))
    (error "%s() is not yet implemented" fname)))


;;
;; mode definition
;;

(defvar cpio-dired-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-c" 'cpio-dired-view-archive) ;✓
    ;; e .. f		dired-find-file
    ;;
    ;; RET		dired-find-file
    (define-key map "e" 'cpio-dired-find-entry)      ;✓
    (define-key map "f" 'cpio-dired-find-entry)      ;✓
    (define-key map "\C-j" 'cpio-dired-find-entry)   ;✓
    ;; C-o		dired-display-file
    (define-key map "\C-o" 'cpio-dired-display-entry) ;✓
    ;; C-t		Prefix Command
    ;; ESC		Prefix Command
    ;; SPC		dired-next-line
    ;; n		dired-next-line
    ;; <remap> <next-line>		dired-next-line
    (define-key map "[remap next-line]" 'cpio-dired-next-line)
    (define-key map "n" 'cpio-dired-next-line)
    (define-key map "\C-n" 'cpio-dired-next-line)
    (define-key map " " 'cpio-dired-next-line) ;✓
    ;; !		dired-do-shell-command
    ;; (define-key map "!" 'cpio-dired-do-shell-command) ;×
    ;; #		dired-flag-auto-save-files
    (define-key map "#" 'cpio-dired-flag-auto-save-entries) ;✓
    ;; $		dired-hide-subdir
    (define-key map "$" 'cpio-dired-hide-subdir) ;?
    ;; %		Prefix Command
    (define-key map "%" nil)
    ;; &		dired-do-async-shell-command
    (define-key map "&" 'cpio-dired-do-async-shell-command) ;×
    ;; (		dired-hide-details-mode
    (define-key map "(" 'cpio-dired-hide-details-mode) ;✓ Implemented by analogue to dired, but does nothing.
    ;; *		Prefix Command
    ;; (define-key map "+" nil) ;×
    ;; +		dired-create-directory
    (define-key map "+" 'cpio-dired-create-directory) ;✓✓
    ;; -		negative-argument
    ;; .		dired-clean-directory
    (define-key map "." 'cpio-dired-clean-directory)
    ;; 0 .. 9		digit-argument
    ;; :		Prefix Command
    (define-key map ":" nil)
    ;; <		dired-prev-dirline
    (define-key map "<" 'cpio-dired-prev-dirline) ;✓
    ;; =		dired-diff
    (define-key map "=" 'cpio-dired-diff) ;×
    ;; >		dired-next-dirline
    (define-key map ">" 'cpio-dired-next-dirline) ;✓
    ;; ?		dired-summary
    (define-key map "?" 'cpio-dired-summary) ;✓
    ;; A		dired-do-search
    (define-key map "A" 'cpio-dired-do-search) ;HEREHERE
    ;; (define-key map "\M-," 'cpio-tags-loop-continue)
    ;; B		dired-do-byte-compile
    ;; (define-key map "B" 'cpio-dired-do-byte-compile) ;×
    ;; C		dired-do-copy
    (define-key map "C" 'cpio-dired-do-copy) ;✓
    ;; D		dired-do-delete
    (define-key map "D" 'cpio-dired-do-delete) ;✓
    ;; G		dired-do-chgrp
    (define-key map "G" 'cpio-dired-do-chgrp) ;✓
    ;; H		dired-do-hardlink
    (define-key map "H" 'cpio-dired-do-hardlink)
    ;; I -- Add an entry. New for cpio-dired.
    (define-key map "I" 'cpio-dired-add-entry)
    ;; L		dired-do-load
    ;; (define-key map "L" 'cpio-dired-do-load) ;×
    ;; M		dired-do-chmod
    (define-key map "M" 'cpio-dired-do-chmod)
    ;; O		dired-do-chown
    (define-key map "O" 'cpio-dired-do-chown) ;✓
    ;; P		dired-do-print
    (define-key map "P" 'cpio-dired-do-print)
    ;; Q		dired-do-query-replace-regexp
    (define-key map "Q" 'cpio-dired-do-query-replace-regexp)
    ;; R		dired-do-rename
    (define-key map "R" 'cpio-dired-do-rename)
    ;; S		dired-do-symlink
    (define-key map "S" 'cpio-dired-do-symlink)
    ;; T		dired-do-touch
    (define-key map "T" 'cpio-dired-do-touch)
 ;;;; ;; X		dired-do-shell-command
 ;;;; (define-key map "X" 'cpio-dired-do-shell-command)
    ;; X	prefix command
    (define-key map "X" nil)
    ;; Xa
    (define-key map "Xa" 'cpio-dired-extract-all)
    ;; Xm
    (define-key map "Xm" 'cpio-dired-extract-entries)
    ;; Z		dired-do-compress
    (define-key map "Z" 'cpio-dired-do-compress)
    ;; ^		dired-up-directory
    (define-key map "^" 'cpio-dired-up-directory)
    ;; a		dired-find-alternate-file
    (define-key map "a" 'cpio-dired-find-alternate-entry)
    ;; d		dired-flag-file-deletion
    (define-key map "d" 'cpio-dired-flag-entry-deletion) ;✓
    ;; g		revert-buffer
    ;; HEREHERE This is not the way to do this.
    (define-key map "g" 'revert-buffer)
    ;; h		describe-mode
    (define-key map "h" 'describe-mode)
    ;; i		dired-maybe-insert-subdir
    ;; (define-key map "i" 'cpio-dired-maybe-insert-subdir) ;×
    ;; j		dired-goto-file
    (define-key map "j" 'cpio-dired-goto-entry)
    ;; k		dired-do-kill-lines
    (define-key map "k" 'cpio-dired-do-kill-lines)
    ;; l		dired-do-redisplay
    (define-key map "l" 'cpio-dired-do-redisplay)
    ;; m		dired-mark
    (define-key map "m" 'cpio-dired-mark) ;✓
    ;; o		dired-find-file-other-window
    (define-key map "o" 'cpio-dired-find-entry-other-window)
    ;; p		dired-previous-line
    ;; <remap> <previous-line>		dired-previous-line
    (define-key map "[remap previous-line]" 'cpio-dired-previous-line)
    (define-key map "p" 'cpio-dired-previous-line)
    (define-key map "\C-p" 'cpio-dired-previous-line)
    ;; q		quit-window
    (define-key map "q" 'cpio-dired-quit-window)
    ;; s		dired-sort-toggle-or-edit
    (define-key map "s" 'cpio-dired-sort-toggle-or-edit)
    ;; t		dired-toggle-marks
    (define-key map "t" 'cpio-dired-toggle-marks)
    ;; u		dired-unmark
    ;; * u
    (define-key map "u" 'cpio-dired-unmark)    ;✓
    (define-key map "*u" 'cpio-dired-unmark)   ;✓
    ;; v		dired-view-file
    (define-key map "v" 'cpio-dired-view-entry)
    ;; w		dired-copy-filename-as-kill
    (define-key map "w" 'cpio-dired-copy-entry-name-as-kill)
    ;; x		dired-do-flagged-delete
    (define-key map "x" 'cpio-dired-do-flagged-delete)
    ;; y		dired-show-file-type
    (define-key map "y" 'cpio-dired-show-entry-type)
    ;; ~		dired-flag-backup-files
    (define-key map "~" 'cpio-dired-flag-backup-entries)
    ;; DEL		dired-unmark-backward
    (define-key map "\177" 'cpio-dired-unmark-backward)
    ;; S-SPC		scroll-down-command
    ;; Not in dired.el (define-key map "\S-SPC" 'cpio-scroll-down-command)
    ;; <follow-link>	mouse-face
    (define-key map [follow-link] 'cpio-mouse-face)
    ;; <mouse-2>	dired-mouse-find-file-other-window
    (define-key map "[mouse-2]" 'cpio-dired-mouse-find-entry-other-window)
    ;; <remap>		Prefix Command
    (define-key map "[remap]" nil)
    ;;
    ;; C-t C-t		image-dired-dired-toggle-marked-thumbs
    (define-key map "\C-t\C-t" 'cpio-image-dired-dired-toggle-marked-thumbs)
    ;;
    ;; C-t .		image-dired-display-thumb
    (define-key map "\C-t" 'cpio-image-dired-display-thumb)
    ;; C-t a		image-dired-display-thumbs-append
    (define-key map "\C-t" 'cpio-image-dired-display-thumbs-append)
    ;; C-t c		image-dired-dired-comment-files
    (define-key map "\C-t" 'cpio-image-dired-dired-comment-entries)
    ;; C-t d		image-dired-display-thumbs
    (define-key map "\C-t" 'cpio-image-dired-display-thumbs)
    ;; C-t e		image-dired-dired-edit-comment-and-tags
    (define-key map "\C-t" 'cpio-image-dired-dired-edit-comment-and-tags)
    ;; C-t f		image-dired-mark-tagged-files
    (define-key map "\C-t" 'cpio-image-dired-mark-tagged-entries)
    ;; C-t i		image-dired-dired-display-image
    (define-key map "\C-t" 'cpio-image-dired-dired-display-image)
    ;; C-t j		image-dired-jump-thumbnail-buffer
    (define-key map "\C-t" 'cpio-image-dired-jump-thumbnail-buffer)
    ;; C-t r		image-dired-delete-tag
    (define-key map "\C-t" 'cpio-image-dired-delete-tag)
    ;; C-t t		image-dired-tag-files
    (define-key map "\C-t" 'cpio-image-dired-tag-entries)
    ;; C-t x		image-dired-dired-display-external
    (define-key map "\C-t" 'cpio-image-dired-dired-display-external)
    ;;
    ;; C-M-d		dired-tree-down
    ;; (define-key map "\C-M-d" 'cpio-dired-tree-down) ;×
    ;; C-M-n		dired-next-subdir
    (define-key map "\C-M-n" 'cpio-dired-next-subdir)
    ;; C-M-p		dired-prev-subdir
    (define-key map "\C-M-p" 'cpio-dired-prev-subdir)
    ;; C-M-u		dired-tree-up
    ;; (define-key map "\C-M-u" 'cpio-dired-tree-up) ;×
    ;; M-$		dired-hide-all
    (define-key map "\M-$" 'cpio-dired-hide-all)
    ;; M-s		Prefix Command
    (define-key map "\M-s" nil)
    ;; M-{		dired-prev-marked-file
    (define-key map "\M-{" 'cpio-dired-prev-marked-entry)
    ;; M-}		dired-next-marked-file
    (define-key map "\M-}" 'cpio-dired-next-marked-entry)
    ;; M-DEL		dired-unmark-all-files
    (define-key map "\M-\177" 'cpio-dired-unmark-all-entries)
    ;;
    ;; M-s a		Prefix Command
    (define-key map "\M-sa" nil)
    ;; M-s f		Prefix Command
    (define-key map "\M-sf" nil)
    ;;
    ;; % &		dired-flag-garbage-files
    (define-key map "%&" 'cpio-dired-flag-garbage-entries)
    ;; % C		dired-do-copy-regexp
    (define-key map "%C" 'cpio-dired-do-copy-regexp)
    ;; % H		dired-do-hardlink-regexp
    (define-key map "%H" 'cpio-dired-do-hardlink-regexp)
    ;; % R		dired-do-rename-regexp
    (define-key map "%R" 'cpio-dired-do-rename-regexp)
    ;; % S		dired-do-symlink-regexp
    (define-key map "%S" 'cpio-dired-do-symlink-regexp)
    ;; % d		dired-flag-files-regexp
    (define-key map "%d" 'cpio-dired-flag-entries-regexp)
    ;; % g		dired-mark-files-containing-regexp
    (define-key map "%g" 'cpio-dired-mark-entries-containing-regexp)
    ;; % l		dired-downcase
    (define-key map "%l" 'cpio-dired-downcase)
    ;; % m		dired-mark-files-regexp
    ;; * %		dired-mark-files-regexp
    (define-key map "%m" 'cpio-dired-mark-entries-regexp)   ;✓
    (define-key map "*%" 'cpio-dired-mark-entries-regexp)   ;✓
    ;; % r		dired-do-rename-regexp
    (define-key map "%r" 'cpio-dired-do-rename-regexp)
    ;; % u		dired-upcase
    (define-key map "%u" 'cpio-dired-upcase)
    ;;
    ;; * C-n		dired-next-marked-file
    (define-key map "*\C-n" 'cpio-dired-next-marked-entry)
    ;; * C-p		dired-prev-marked-file
    (define-key map "*\C-p" 'cpio-dired-prev-marked-entry)
    ;; * !		dired-unmark-all-marks
    ;; U		dired-unmark-all-marks
    (define-key map "*!" 'cpio-dired-unmark-all-marks)  ;✓
    (define-key map "U" 'cpio-dired-unmark-all-marks)   ;✓
    ;; * *		dired-mark-executables
    (define-key map "**" 'cpio-dired-mark-executables)
    ;; * /		dired-mark-directories
    (define-key map "*/" 'cpio-dired-mark-directories)
    ;; * ?		dired-unmark-all-files
    (define-key map "*?" 'cpio-dired-unmark-all-entries)
    ;; * @		dired-mark-symlinks
    (define-key map "*@" 'cpio-dired-mark-symlinks)
    ;; * c		dired-change-marks
    (define-key map "*c" 'cpio-dired-change-marks)
    ;; * m		dired-mark
    (define-key map "*m" 'cpio-dired-mark) ;✓
    ;; * s		dired-mark-subdir-files
    (define-key map "*s" 'cpio-dired-mark-subdir-entries)
    ;; * t		dired-toggle-marks
    (define-key map "*t" 'cpio-dired-toggle-marks)
    ;; * DEL		dired-unmark-backward
    (define-key map "*\177" 'cpio-dired-unmark-backward)
    ;;
    ;; : d		epa-dired-do-decrypt
    (define-key map ":d" 'cpio-epa-dired-do-decrypt)
    ;; : e		epa-dired-do-encrypt
    (define-key map ":e" 'cpio-epa-dired-do-encrypt)
    ;; : s		epa-dired-do-sign
    (define-key map ":s" 'cpio-epa-dired-do-sign)
    ;; : v		epa-dired-do-verify
    (define-key map ":v" 'cpio-epa-dired-do-verify)
    ;;
    ;; <remap> <advertised-undo>	dired-undo
    (define-key map "[remap advertised-undo]" 'cpio-dired-undo)
    ;; <remap> <read-only-mode>	dired-toggle-read-only
    (define-key map "[remap read-only-mode]" 'cpio-dired-toggle-read-only)
    ;; <remap> <toggle-read-only>	dired-toggle-read-only
    (define-key map "[remap toggle-read-only]" 'cpio-dired-toggle-read-only)
    ;; <remap> <undo>			dired-undo
    (define-key map "[remap undo]" 'cpio-dired-undo)
    ;;
    ;; M-s f C-s	dired-isearch-filenames
    (define-key map (kbd "M-s f C-s") 'cpio-dired-isearch-entry-names)
    ;; M-s f ESC	Prefix Command
    (define-key map "\M-sf" nil)
    ;;
    ;; M-s a C-s	dired-do-isearch
    (define-key map (kbd "M-s a C-s") 'cpio-dired-do-isearch)
    ;; M-s a ESC	Prefix Command
    ;;
    ;; M-s f C-M-s	dired-isearch-filenames-regexp
    (define-key map (kbd "M-s f C-M-s") 'cpio-dired-isearch-entry-names-regexp)
    ;;
    ;; M-s a C-M-s	dired-do-isearch-regexp
    (define-key map (kbd "M-s a C-M-s") 'cpio-dired-do-isearch-regexp)
    ;; C-x k -- kill the cpio-related buffers from the cpio-dired buffer.
    (define-key map (kbd "C-x k") 'cpio-dired-kill) ;✓
    ;; C-x C-s -- save the archive form the cpio-dired-buffer.
    (define-key map (kbd "C-x C-s") 'cpio-dired-save-archive) ;✓
    ;; (setq *cpio-have-made-keymap)
    map))

(define-derived-mode cpio-dired-mode fundamental-mode "cpio-dired"
  "Mode for editing cpio archives in the style of dired."
  :group 'cpio
  ;; (add-hook  'kill-buffer-hook (lambda () (kill-buffer *cab-parent*)) "append" "local"))
  (goto-char (point-min))
  (cond ((re-search-forward *cpio-dired-entry-regexp* (point-max) t)
	 (cpio-dired-move-to-entry-name)
	 (make-local-variable 'revert-buffer-function)
	 (setq revert-buffer-function 'cpio-revert-buffer)
	 (set-buffer-modified-p nil)
	 (setq-local font-lock-defaults
              '(dired-font-lock-keywords t nil nil beginning-of-line)))
	(t t)))


(provide 'cpio-dired)
;;; cpio-dired.el ends here

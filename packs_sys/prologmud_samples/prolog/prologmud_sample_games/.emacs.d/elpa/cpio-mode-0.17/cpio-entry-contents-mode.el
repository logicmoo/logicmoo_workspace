;;; cpio-entry-contents-mode.el --- minor mode for editing a cpio-entry's contents. -*- coding: utf-8 -*-

;; COPYRIGHT
;;
;; Copyright © 2019 Free Software Foundation, Inc.
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
;; Created: 2017 Dec 06
;; yVersion: 0.17
;; Keywords: files

;;; Commentary:

;; This file contains code for editing and saving
;; the contents of entries in a cpio-archive.

;;; Documentation:

;;; Code:

;;
;; Hacks
;;

(defun entry-setup (arg &optional name depth)
  "Set up buffers and windows for working on entry NAME.
If NAME is not given, then use 'aa'."
  (interactive "P")
  (if (and (called-interactively-p 'interactive)
	   arg)
      (setq name (read-string "Name? ")))
  (unless name (setq name "aa"))
  (unless depth (setq depth 0))
  (let* ((fname "entry-setup")
	 (short-archive-name "alphabet_small.crc.cpio")
	 (archive-name (if (string-match "alphabet/" default-directory)
			   (concat default-directory short-archive-name)
			 (concat default-directory "test_data/alphabet/" short-archive-name)))
	 (cpio-archive-buffer)
	 (cpio-dired-buffer)
	 (cpio-entry-contents-buffer)
	 (cpio-dired-contents-mode-buffer))
    ;; Make sure we have a clean copy of the archive.
    (with-current-buffer (find-file-noselect archive-name)
      (shell-command "make crc" nil nil)
      (kill-buffer))
    (with-current-buffer (setq cpio-archive-buffer (find-file-noselect archive-name))
      (cpio-mode)
      (setq cpio-dired-buffer (current-buffer)))
    (unless (with-current-buffer cpio-archive-buffer (cpio-entry-exists-p name))
      (if (> depth 1)
	  (error "%s(): Going too deep." fname)
	(entry-setup nil name (1+ depth)))
      (setq cpio-dired-buffer (current-buffer)))

    ;; Get the entry
    (switch-to-buffer cpio-dired-buffer)
    (cpio-dired-goto-entry name)
    (cpio-dired-find-entry)
    (setq cpio-entry-contents-buffer (current-buffer))
    (switch-to-buffer cpio-dired-buffer)

    ;; Set up windows.
    (delete-other-windows)
    (split-window-right)
    (split-window)
    (other-window 1)
    (switch-to-buffer cpio-archive-buffer)
    (other-window 1)
    (split-window)
    (switch-to-buffer cpio-entry-contents-buffer)
    (other-window 1)
    (setq cpio-dired-contents-mode-buffer (switch-to-buffer "cpio-entry-contents-mode.el"))
    (other-window 2)))


;;
;; Dependencies
;;

;;;;;;;;;;;;;;;;
;; Things to make the byte compiler happy.
(defvar cpio-entry-name)
(defvar *cpio-catalog-entry-contents-start-idx*)
(declare-function cpio-contents-start "cpio-mode.el")
(declare-function cpio-delete-archive-entry "cpio-mode.el")
(declare-function cpio-dired-find-entry "cpio-dired.el")
(declare-function cpio-dired-goto-entry "cpio-dired.el")
(declare-function cpio-entry "cpio-mode.el")
(declare-function cpio-entry-attrs "cpio-mode.el")
(declare-function cpio-entry-exists-p "cpio-mode.el")
(declare-function cpio-entry-header-start "cpio-mode.el")
(declare-function cpio-insert-padded-contents "cpio-mode.el")
(declare-function cpio-make-header-string "cpio-mode.el")
(declare-function cpio-mode "cpio-mode.el")
(declare-function cpio-present-ala-dired "cpio-dired.el")
(declare-function cpio-set-entry-modified "cpio-mode.el")
(declare-function cpio-set-entry-size "cpio-mode.el")
(declare-function cpio-entry-exists-p "cpio-mode.el")
(declare-function cpio-dired-goto-entry "cpio-dired.el")
(declare-function cpio-dired-find-entry "cpio-dired.el")
;; EO things for the byte compiler.
;;;;;;;;;;;;;;;;



;;
;; Vars
;;


;;
;; Library
;;



;;
;; Commands
;;
(defun cpio-entry-contents-save ()
  "Save the contents of the current buffer in it's cpio archive."
  (interactive)
  (let ((fname "cpio-entry-contents-save")
	(name cpio-entry-name)
	(entry (cpio-entry cpio-entry-name))
	(attrs (cpio-entry-attrs cpio-entry-name))
	(header-string)
	(size (buffer-size))
	(new-contents (buffer-string))
	(dired-buffer-name))
    (unless (cpio-entry-contents-buffer-p)
      (error "%s(): You're not in a cpio entry contents buffer." fname))

    (with-current-buffer *cab-parent*
      ;; 1. Delete the entry's head and contents (plus padding) in the parent buffer.
      (cpio-delete-archive-entry entry)
      ;; 2. Update the entry size in the entry.
      (cpio-set-entry-size attrs size)
      ;; 3. Write the new contents in the archive buffer (plus padding).
      (goto-char (cpio-contents-start name))
      (cpio-insert-padded-contents new-contents)
      ;; 4. Build the entry header.
      (setq header-string (cpio-make-header-string attrs))
      ;; 5. Write the header in the archive buffer (plus padding).
      (goto-char (cpio-entry-header-start entry))
      (with-writable-buffer
       (insert header-string))
      (aset entry *cpio-catalog-entry-contents-start-idx* (point-marker))

      (setq dired-buffer-name (cpio-dired-buffer-name (buffer-file-name))))
    ;; 6. Mark the contents buffer as unmodified.
    (set-buffer-modified-p nil)
    ;; 6a. But mark the entry in the archive modified.
    (cpio-set-entry-modified entry)
    ;; 7. Update the dired-like interface.
    (with-current-buffer dired-buffer-name
      (save-excursion
	(cpio-dired-goto-entry name)
	(with-writable-buffer
	 (delete-region (line-beginning-position) (line-end-position))
	 (insert (cpio-dired-format-entry attrs)))))
    (message "Saved into cpio archive buffer `%s'.  Be sure to save that buffer!"
             (file-name-nondirectory (buffer-file-name *cab-parent*)))))

(defun cpio-entry-contents-buffer-p ()
  "Return non-NIL if the current buffer is an entry contents buffer."
  (let ((fname "cpio-entry-contents-buffer-p"))
    (member 'cpio-entry-contents-mode (current-minor-modes))))

(defun cpio-entry-contents-kill (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME.
A name denotes the name of an entry in the cpio archive."
  (interactive "P")
  (unless buffer-or-name (setq buffer-or-name (current-buffer)))
  (let ((fname "cpio-entry-contents-kill")
	(buffer (if (bufferp buffer-or-name)
		    buffer-or-name
		  (get-buffer-create buffer-or-name))))
    (if (and (buffer-modified-p buffer)
	     (yes-or-no-p "Buffer is modified. Really kill? "))
	(kill-buffer buffer))))

(defun cpio-entry-contents-revert-buffer ()
  "Discard any changes to the current CPIO archive entry and
reload the [current] entry contents."
  (interactive)
  (let ((fname "cpio-entry-contents-revert-buffer"))
    (unless (cpio-entry-contents-buffer-p)
      (error "%s(): You're not in an entry contetnts buffer." fname))
    (with-writable-buffer
     (erase-buffer)
     (cpio-find-entry cpio-entry-name)
     (set-auto-mode 'keep-mode-if-same))))


;;
;; Mode definition
;;
(defvar *cpio-entry-contents-mode-map* (make-sparse-keymap)
  "Keymap for cpio-entry-contents-mode.")
(setq *cpio-entry-contents-mode-map* (make-sparse-keymap))


(defun cpio-entry-contents-make-keymap ()
  "Define the keys that cpio-entry-contents-mode must override."
  (let ((fname "cpio-entry-contents-make-keymap"))
    (define-key *cpio-entry-contents-mode-map* "\C-x\C-s" 'cpio-entry-contents-save)
    (define-key *cpio-entry-contents-mode-map* "\C-x\C-k" 'cpio-entry-contents-kill)
    ;; HEREHERE Does the following make sense any more?
    (define-key *cpio-entry-contents-mode-map* "\M-,"     'cpio-tags-loop-continue)))

(define-minor-mode cpio-entry-contents-mode
  "Minor mode for working with an entry's contents from a cpio archive.
This mode is automatically invoked when the contents of a cpio entry are
prepared for editing."
  nil
  " entry contents"
  :keymap *cpio-entry-contents-mode-map*
  :global nil
  :lighter "(cpio entry)"
  ;; Major modes kill local variables.
  ;; Keep the ones we need for cpio entry contents.
  (let ((cab-parent *cab-parent*)
	(entry-name cpio-entry-name)
	(attrs (cpio-entry-attrs cpio-entry-name))
	(local-buffer-file-name buffer-file-name))
    ;; For some reason (decode-coding-region) seems to need a writable buffer.
    ;; (with-writable-buffer
    ;;	(decode-coding-region (cpio-entry-contents-start (cpio-entry entry-name))
    ;;			(cpio-entry-contents-end (cpio-entry entry-name))
    ;;			nil)))
    ;; (point-min) (point-max) nil)
    ;; (set-buffer-file-coding-system last-coding-system-used t)

    ;; (normal-mode)
    (set-auto-mode 'keep-mode-if-same)
    (setq *cab-parent* cab-parent)
    (setq cpio-entry-name entry-name)
    ;; Why was I doing this?
    ;; (setq buffer-file-name local-buffer-file-name)
    (setq cpio-entry-contents-mode t)))

(cpio-entry-contents-make-keymap)


(provide 'cpio-entry-contents-mode)
;;; cpio-entry-contents-mode.el ends here


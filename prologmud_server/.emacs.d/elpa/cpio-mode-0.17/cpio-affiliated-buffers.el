;;; cpio-affiliated-buffers.el --- Establish and manage buffers affiliated with each other. -*- coding: utf-8 -*-

;; COPYRIGHT

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
;; Created: 2017 Nov 22
;; Version: 0.17
;; Keywords: files

;;; Commentary:

;; To keep track of which buffers are connected to a specific archive,
;; cpio-mode uses the idea of affiliated buffers.
;;
;; The buffers affiliated with an archive's buffer are the following:
;; 1. The buffer holding the dired-like information.
;; 2. The buffers holding any entry's contents.
;; Killing [deregistering] the dired-like buffer also kills the archive's buffer,
;; and killing the archive's buffer kills
;; all remaining affiliated buffers.
;;

;;; Documentation:

;; Two variables hold the relationships among buffers:
;; • *cab-subordinates* -- a list of the buffers immediately subordinate
;;   to the current buffer.
;; • *cab-parent* -- a buffer, the buffer to which the current buffer is affiliated.
;; Both variables are buffer local.
;;
;; The existence of a subordinate buffer depends
;; on the the existence of its parent.
;; One consequence is that a subordinate buffer can have only one parent.
;; Another is that killing the parent buffer kills all subordinates as well.
;; Should a subordinate buffer have further subordinates,
;; then they must also be killed.

;; API:
;; (cab-register (buffer parent))
;;     Register BUFFER as a subordinate of PARENT.
;; (cab-registered-p (buffer parent)
;;     Return non-NIL if BUFFER is a registered subordinate of PARENT.
;; (cab-kill-buffer-hook)
;;     A hook for subordinate buffers that removes their registry entry
;;     with PARENT.
;; (cab-deregister (&optional buffer))
;;     Kill BUFFER and its subordinates.
;;     Deregister BUFFER with its parent.
;; (cab-simple-deregister (buffer))
;;     The internal function for (cab-deregister).
;;     Don't use this directly.
;; (cab-clean)
;;     A temporary function for development
;;     that should more forcefully enforce the intent of (cab-deregister).

;; The following incantation should run the tests well.
;; emacs -batch -l ert -l cab-test.el -f ert-run-tests-batch-and-exit

;;; Code:

;;
;; Development
;;
(defun cab-setup-parenthood-check ()
  "Set up a simple situation where the parenthood check should error out."
  (let ((b0 (find-file-noselect "b0"))
	(b1 (find-file-noselect "b1")))
    (cab-register b1 b0)
    (cab-register b0 b1)))

(defun cab-setup-parenthood-check-1 ()
  "Set up a large situation where the parenthood check should error out."
  (let* ((b0 (find-file-noselect "bb0"))
	 (b1 (find-file-noselect "bb1"))
	 (b2 (find-file-noselect "bb2"))
	 (b3 (find-file-noselect "bb3"))
	 (b4 (find-file-noselect "bb4"))
	 (b5 (find-file-noselect "bb5"))
	 (b6 (find-file-noselect "bb6"))
	 (b7 (find-file-noselect "bb7"))
	 (b8 (find-file-noselect "bb8"))
	 (b9 (find-file-noselect "bb9"))
	 (parent b0))
    (mapc (lambda (b)
	    (cab-register b parent)
	    (setq parent b))
	  (list b1 b2 b3 b4 b5 b6 b7 b8 b9))
    (cab-register b0 b9)))


;; HEREHERE Remove the following test code before publishing cpio-mode.
(defvar OBS-*cab-info-buffer* (get-buffer-create "*cab info*")
  "A buffer for holding information about affiliated buffers.")
(setq OBS-*cab-info-buffer* (get-buffer-create "*cab info*"))

(defun OBS-cab-test-kill-buffer-hook ()
  "Hook to run when killing a buffer.
The intent is to glean information about any buffers
that cpio-mode might be using
that are affiliated with each other."
  (let ((fname "cab-test-kill-buffer-hook")
	(buf (current-buffer)))
    (unless (string-match "\\` " (buffer-name (current-buffer)))
      (with-current-buffer *cab-info-buffer*
	(goto-char (point-max))
	(insert (format "\n\nKilling buffer [[%s]].
    It has parent [[%s]].
"
			(buffer-name buf)
			(if *cab-parent*
			    (buffer-name *cab-parent*)
			  "nil")))
	(cond ((with-current-buffer buf *cab-subordinates*)
	       (insert "    It has subordinates:\n")
	       (mapc (lambda (b)
		       (insert (format "        [[%s]]\n" b)))
		     (with-current-buffer buf
		       *cab-subordinates*)))
	      (t (insert "    No subordinates.\n")))))))

(defun OBS-cab-test-register-buffer-hook (buffer parent)
  "Record some information about the registration of a BUFFER
as an affiliated buffer.
It's not strictly a hook, but it pairs with the above kill-buffer-hook."
  (let ((fname "cab-test-register-buffer-hook"))
    (with-current-buffer *cab-info-buffer*
      (goto-char (point-max))
      (insert (format "Registering [[%s]] with [[%s]] as its parent.\n"
		      (buffer-name buffer) (buffer-name parent)))
      (insert (format "    [[%s]] currently has the following subordinates.\n"
		      (buffer-name parent)))
      (mapc (lambda (b)
	      (insert (format "        [[%s]]\n" (buffer-name b))))
	    (with-current-buffer parent
	      *cab-subordinates*)))))

(defcustom cab-clear-cab-info-buffer nil
  "Clear the Affiliated Info Buffer if set."
  :type 'boolean
  :group 'cab)


;;
;; Generic functions
;;




;;
;; Dependencies
;;
(eval-when-compile
  (require 'cl-lib))


;;
;; Vars
;;
(defvar *cab-subordinates* ()
  "A list of subordinate buffers affiliated with the current buffer.")
(setq *cab-subordinates* ())


(make-variable-buffer-local '*cab-subordinates*)
(defvar *cab-parent* nil
  "The parent buffer of an affiliated buffer.")
(setq *cab-parent* nil)


(make-variable-buffer-local '*cab-parent*)

;;
;; Library
;;
(defun cab-register (buffer parent)
  "Register the given BUFFER as an affiliate of the PARENT buffer.
If BUFFER is already an affiliate of PARENT, then succeed quietly.
Return non-NIL on success.
Return NIL if buffer is already affiliated to another parent."
  (let ((fname "cab-register"))
    (if (not (bufferp buffer))
	(error "%s(): proposed buffer [[%s]] is not a buffer." fname buffer))
    (if (not (bufferp parent))
	(error "%s(): proposed parent buffer [[%s]] is not a buffer." fname parent))
    (if (equal buffer parent)
	(error "%s(): You can't affiliate a buffer [[%s]] with itself [[%s]]." fname buffer parent))

    (if (cab-detect-parenthood-cycle buffer parent)
	(error "%s(): Registering [[%s]] as a subordinate of [[%s]] would create a cycle of parents." fname buffer parent))

    (cond ((cab-registered-p buffer parent)
	   t)
	  ((with-current-buffer buffer
	     (and (boundp '*cab-parent*)
		  (buffer-live-p *cab-parent*)))
	   nil)
	  (t
	   (with-current-buffer buffer
	     (setq *cab-parent* parent)
	     (local-set-key "\C-x\C-k" (lambda () (cab-deregister buffer))))
	   (with-current-buffer parent
	     (push buffer *cab-subordinates*)
	     (add-hook 'kill-buffer-hook 'cab-kill-buffer-hook nil 'local)
	     (local-set-key "\C-x\C-k" (lambda () (cab-deregister parent))))))))

(defun cab-detect-parenthood-cycle (buffer parent)
  "Return non-NIL if affiliating BUFFER with PARENT would create a parenthood cycle."
  (let ((fname "cab-detect-parenthood-cycle"))
    (with-current-buffer parent
      (catch 'detected
	(while parent
	  (with-current-buffer parent
	    (cond ((eq (current-buffer) buffer)
		   (throw 'detected t))
		  ((null *cab-parent*)
		   (setq parent *cab-parent*))
		  (t
		   (setq parent *cab-parent*)))))))))

(defun cab-registered-p (buffer parent)
  "Return non-NIL if BUFFER is already registered to PARENT.
CONTRACT: BUFFER and PARENT are buffers."
  (let ((fname "cab-registered-p"))
    (cond ((or (null buffer)
	       (not (bufferp buffer))
	       (not (buffer-live-p buffer)))
	   nil)
	  ((or (null parent)
	       (not (bufferp parent))
	       (not (buffer-live-p parent)))
	   nil)
	  ((and (bufferp parent)
		(buffer-live-p parent))
	   (with-current-buffer parent
	     (member buffer *cab-subordinates*))))))

(defun cab-kill-buffer-hook ()
  "Kill the current buffer and remove any affiliation (parent or subordinate)."
  (let ((fname "cab-kill-buffer-hook")
	(buffer (current-buffer)))
    (cond ((buffer-live-p (current-buffer))
	   (if (buffer-live-p *cab-parent*)
	       (with-current-buffer *cab-parent*
		 (delete buffer *cab-subordinates*))))
	  (t t))))

(defun cab-deregister (buffer)
  "Deregister and kill BUFFER and all its subordinate buffers.
Note that that will include their subordinates too.
Remove its registry entry in its parent buffer.
NOTE: Use this function instead of (kill-buffer)
if you want to lose registry information."
  (interactive)
  (let ((fname "cab-deregister")
	(parent)
	(subordinates))
    (cond ((buffer-live-p buffer)
	   (with-current-buffer buffer
	     (setq parent *cab-parent*)
	     (setq subordinates *cab-subordinates*))
	   (mapc 'cab-deregister subordinates)
	   (if (and parent
		    (bufferp parent)
		    (buffer-live-p parent)
		    (cab-registered-p buffer parent))
	       (with-current-buffer parent
		 (setq *cab-subordinates* (delete buffer *cab-subordinates*))))
	   (if (buffer-live-p buffer)
	       (kill-buffer buffer)))
	  (t nil))))

(defun cab-simple-deregister (buffer)
  "Deregister BUFFER and all its subordinates, but don't kill it."
  (let ((fname "cab-simple-deregister")
	(parent)
	(subordinates))
    (with-current-buffer buffer
      (setq parent *cab-parent*)
      (setq subordinates *cab-subordinates*))
    (mapc 'cab-simple-deregister subordinates)
    (with-current-buffer parent
      (setq *cab-subordinates* (delete buffer *cab-subordinates*)))))

(defun cab-clean ()
  "Clean up affiliated buffers.
CAVEAT: This function should disappear as affiliated buffer code stabilizes."
  (interactive)
  (let ((fname "cab-clean"))
    (mapc (lambda (b)
	    (with-current-buffer b
	      (if (boundp '*cab-subordinates*)
		  (setq *cab-subordinates* (delete-dups *cab-subordinates*)))))
	  (buffer-list))))

(defun cab-clean-ruthlessly ()
  "Get rid of all buffers that are affiliated with other buffers."
  (let ((fname "cab-clean-2"))
    (mapc (lambda (b)
	    (if (buffer-live-p b)
		(with-current-buffer b
		  (if (or (and (boundp '*cab-parent*)
			       *cab-parent*)
			  (and (boundp '*cab-subordinates*)
			       *cab-subordinates*))
		      (cab-deregister b)))))
	  (buffer-list))))


(provide 'cpio-affiliated-buffers)
;;; cpio-affiliated-buffers.el ends here


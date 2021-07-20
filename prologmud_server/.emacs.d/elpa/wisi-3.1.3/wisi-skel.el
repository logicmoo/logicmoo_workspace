;;; wisi-skel.el --- Extensions skeleton  -*- lexical-binding:t -*-

;; Copyright (C) 1987, 1993, 1994, 1996-2020  Free Software Foundation, Inc.

;; Authors: Stephen Leake <stephen_leake@stephe-leake.org>

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

;;; Design:
;;
;; The primary user command is `wisi-skel-expand', which inserts the
;; skeleton associated with the previous word (possibly skipping a
;; name).

(require 'skeleton)

(defvar-local wisi-skel-token-alist nil
  "Alist of (STRING . ELEMENT), used by `wisi-skel-expand'.
STRING should be a grammar symbol in the current language.

ELEMENT may be:
- a skeleton, which is inserted
- an alist of (STRING . SKELETON). User is prompted with `completing-read',
  selected skeleton is inserted.")

(defun wisi-skel-add-token-after (alist token skel after-1 &optional after-2)
  "Add a new entry in ALIST (should be an instance of `wisi-skel-token-alist')
after AFTER-1. If AFTER-1 is a nested alist, add the new entry after AFTER-2."
  (let ((tail alist)
	done)
    (if (string= after-1 (car (car alist)))
	(setcdr alist (cons (cons token skel) (cdr alist)))

      (while (and (not done) tail)
	(if (string= after-1 (car-safe (car (cdr tail))))
	    (cond
	     ((symbolp (cdr (car (cdr tail))))
	      (setcdr tail (cons (cons token skel) (cdr (cdr tail))))
	      (setq done t))

	     ((consp (cdr (car (cdr tail))))
	      (wisi-skel-add-token-after (cdr (car (cdr tail))) token skel after-2)
	      (setq done t))
	     )
	  ;; else
	  (setq tail (cdr tail))
	  ))
      )))

(defun wisi-skel-build-prompt (alist count)
  "Build a prompt from the keys of the ALIST.
The prompt consists of the first COUNT keys from the alist, separated by `|', with
trailing `...' if there are more keys."
  (if (>= count (length alist))
      (concat (mapconcat 'car alist " | ") " : ")
    (let ((alist-1 (butlast alist (- (length alist) count))))
      (concat (mapconcat 'car alist-1 " | ") " | ... : "))
  ))

(defvar wisi-skel-test-input nil
  "Override prompt for input from wisi-skel-token-alist, for unit testing."
  ;; see test/ada_skel.adb
  )

(defun wisi-skel-expand (&optional name)
  "Expand the token or placeholder before point to a skeleton.
Tokens are defined by `wisi-skel-token-alist'; they must have
symbol syntax.  A placeholder is a token enclosed in generic
comment delimiters.  If the symbol before point is not in
`wisi-skel-token-alist', assume it is a name, and use the symbol
before that as the token."
  (interactive "*")

  ;; Skip trailing space, newline, and placeholder delimiter.
  ;; Standard comment end included for languages where that is newline.
  (skip-syntax-backward " !>")

  (let* ((wisi-inhibit-parse t) ;; don't parse until skeleton is fully inserted
	 (end (point))
	 ;; Include punctuation here, to handle a dotted name (ie Ada.Text_IO)
	 (token (progn (skip-syntax-backward "w_.")
		       (downcase (buffer-substring-no-properties (point) end))))
	 (skel (assoc-string token wisi-skel-token-alist))
	 (handled nil))

    (if skel
	(progn
	  (when (listp (cdr skel))
	    (let* ((alist (cdr skel))
		   (prompt (wisi-skel-build-prompt alist 4)))
	      (setq skel (assoc-string
			  (or wisi-skel-test-input (completing-read prompt alist))
			  alist))
	      ))

	  ;; delete placeholder delimiters around token, token, and
	  ;; name. point is currently before token.
	  (skip-syntax-backward "!")
	  (delete-region
	   (point)
	   (progn
	     (skip-syntax-forward "!w_")
	     (when name
	       (skip-syntax-forward " ")
	       (skip-syntax-forward "w_."))
	     (point)))
	  (let ((skeleton-end-newline nil))
	    (funcall (cdr skel) name))
	  (setq handled t))

      ;; word in point .. end is not a token; assume it is a name
      (when (not name)
	;; avoid infinite recursion

	(when wisi-auto-case
	  ;; Adjust case now, because skeleton insert won't.
	  ;;
	  ;; We didn't do it above, because we don't want to adjust case
	  ;; on tokens and placeholders.
	  (save-excursion (wisi-case-adjust-region (point) end)))

	(condition-case-unless-debug nil
	    (progn
	      (wisi-skel-expand (buffer-substring-no-properties (point) end))
	      (setq handled t))
	  (user-error ;; leave handled nil
	   ))
	))

    (when (not handled)
      (setq name (buffer-substring-no-properties (point) end))
      ;; restore point
      (goto-char end)
      (user-error "'%s' is not a skeleton token" name))
    ))

;;;###autoload
(defun wisi-skel-hippie-try (old)
  "For `hippie-expand-try-functions-list'."
  (if old
      ;; hippie is asking us to try the "next" completion; we don't have one
      nil
    (let ((pos (point))
	  (undo-len (if (eq 't pending-undo-list)
			0
		      (length pending-undo-list))))
      (undo-boundary)
      (condition-case nil
	  (progn
	    (wisi-skel-expand)
	    t)
	(error
	 ;; undo hook action if any
	 (unless (or (eq 't pending-undo-list)
		     (= undo-len (length pending-undo-list)))
	   (undo))

	 ;; undo motion
	 (goto-char pos)
	 nil)))))

(defun wisi-skel-next-placeholder ()
  "Move point to after next placeholder."
  (interactive)
  (skip-syntax-forward "^!")
  (skip-syntax-forward "w_!"))

(defun wisi-skel-prev-placeholder ()
  "Move point to after previous placeholder."
  (interactive)
  (skip-syntax-backward "^!"))

(provide 'wisi-skel)
;;; wisi-skel.el ends here

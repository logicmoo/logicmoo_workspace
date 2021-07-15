;;; lxc-tramp.el --- TRAMP integration for LXC containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 montag451

;; Author: montag451
;; URL: https://github.com/montag451/lxc-tramp
;; Package-Version: 20200414.1445
;; Package-Commit: 1585e55a5deb89e2f4e30a0ad9e0f121d1e0ebcb
;; Keywords: lxc, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "24") (cl-lib "0.6"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `lxc-tramp' offers a TRAMP method for LXC containers.
;;
;; ## Usage
;;
;; Offers the TRAMP method `lxc` to access running containers
;;
;;     C-x C-f /lxc:user@container:/path/to/file
;;
;;     where
;;       user           is the user that you want to use (optional)
;;       container      is the id or name of the container
;;

;;; Code:

(require 'tramp)
(eval-when-compile
  (require 'cl-lib))

(defgroup lxc-tramp nil
  "TRAMP integration for lxc containers."
  :prefix "lxc-tramp-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/montag451/lxc-tramp")
  :link '(emacs-commentary-link :tag "Commentary" "lxc-tramp"))

(defcustom lxc-tramp-lxc-attach-executable "lxc-attach"
  "Path to lxc-attach executable."
  :type 'string
  :group 'lxc-tramp)

(defcustom lxc-tramp-lxc-ls-executable "lxc-ls"
  "Path to lxc-ls executable."
  :type 'string
  :group 'lxc-tramp)

(defconst lxc-tramp-completion-function-alist
  '((lxc-tramp--parse-running-containers  ""))
  "Default list of (FUNCTION FILE) pairs to be examined for lxc method.")

(defconst lxc-tramp-method "lxc"
  "Method to connect lxc containers.")

(defun lxc-tramp--process-lines (program &optional delete-trailing-ws &rest args)
  "A version of `process-lines' that use `process-file'.

Contrary to `process-lines', this function uses `process-file'
instead of `call-process'.  PROGRAM is the program to execute,
see the documentation of `process-lines' for further information.
If DELETE-TRAILING-WS is non-nil trailing whitespace and trailing
newlines will be removed from output.  ARGS are passed to PROGRAM
similarly to `process-lines'"
  (with-temp-buffer
    (when (zerop (apply 'process-file program nil t nil args))
      (when delete-trailing-ws
        (let ((delete-trailing-lines t))
          (delete-trailing-whitespace)))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (push line lines))
          (forward-line))
        (nreverse lines)))))

(defun lxc-tramp--running-containers (&optional ignored)
  "List running containers.

TRAMP call this function with a filename which is IGNORED."
  (lxc-tramp--process-lines lxc-tramp-lxc-ls-executable t "--running" "-1"))

(defun lxc-tramp--parse-running-containers (&optional ignored)
  "Return a list of (user host) tuples.

TRAMP calls this function with a filename which is IGNORED.  The
user is an empty string because the lxc TRAMP method uses bash
to connect to the default user containers."
  (cl-loop for name in (lxc-tramp--running-containers)
           collect (list "" name)))

;;;###autoload
(defun lxc-tramp-add-method ()
  "Add lxc tramp method."
  (add-to-list 'tramp-methods
               `(,lxc-tramp-method
                 (tramp-login-program ,lxc-tramp-lxc-attach-executable)
                 (tramp-login-args (("--clear-env")
                                    ("-v") ("HOME=/root")
                                    ("-n") ("%h")
                                    ("--") ("su - %u")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-i" "-c")))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (lxc-tramp-add-method)
     (tramp-set-completion-function
      lxc-tramp-method lxc-tramp-completion-function-alist)))

(provide 'lxc-tramp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; lxc-tramp.el ends here

;;; lxd-tramp.el --- TRAMP integration for LXD containers -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yc.S <onixie@gmail.com>

;; Author: Yc.S <onixie@gmail.com>
;; URL: https://github.com/onixie/lxd-tramp.git
;; Package-Version: 20181023.7
;; Package-Commit: f335c76245f62b02cf67a9376eca6f3863c8a75a
;; Keywords: lxd, lxc, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6"))

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
;; `lxd-tramp' offers a TRAMP method for LXD containers
;;
;; ## Usage
;;
;; Offers the TRAMP method `lxd` to access running containers
;;
;;     C-x C-f /lxd:user@container:/path/to/file
;;
;;     where
;;       user           is the user that you want to use (optional)
;;       container      is the id or name of the container
;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tramp)
(require 'subr-x)

(defgroup lxd-tramp nil
  "TRAMP integration for LXD containers."
  :prefix "lxd-tramp-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/onixie/lxd-tramp.git")
  :link '(emacs-commentary-link :tag "Commentary" "lxd-tramp"))

(defcustom lxd-tramp-lxc-executable "lxc"
  "Path to lxc executable."
  :type 'string
  :group 'lxd-tramp)

;;;###autoload
(defconst lxd-tramp-completion-function-alist
  '((lxd-tramp--parse-running-containers  ""))
  "Default list of (FUNCTION FILE) pairs to be examined for lxd method.")

;;;###autoload
(defconst lxd-tramp-method "lxd"
  "Method to connect to LXD containers.")

(defun lxd-tramp--running-containers ()
  "Collect running container names."
  (cl-rest
   (cl-loop for line in (ignore-errors (process-lines lxd-tramp-lxc-executable "list" "--columns=n")) ; Note: --format=csv only exists after version 2.13
            for count from 1
            when (cl-evenp count) collect (string-trim (substring line 1 -1)))))

(defun lxd-tramp--parse-running-containers (&optional ignored)
  "Return a list of (user host) tuples.

TRAMP calls this function with a filename which is IGNORED.  The
user is an empty string because the lxd TRAMP method uses bash
to connect to the default user containers."
  (cl-loop for name in (lxd-tramp--running-containers)
           collect (list "" name)))

;;;###autoload
(defun lxd-tramp-add-method ()
  "Add lxd tramp method."
  (add-to-list 'tramp-methods
               `(,lxd-tramp-method
                 (tramp-login-program ,lxd-tramp-lxc-executable)
                 (tramp-login-args (("exec") ("%h") ("--") ("su - %u")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-i" "-c")))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (lxd-tramp-add-method)
     (tramp-set-completion-function lxd-tramp-method lxd-tramp-completion-function-alist)))

(provide 'lxd-tramp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; lxd-tramp.el ends here

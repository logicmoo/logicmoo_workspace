;;; org-if-active.el --- Active mode for org-if -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines the active minor mode for using the org-if system.

;;; Code:

(require 'ob-core)
(require 'org)
(require 'org-if-misc)
(require 'org-if-link)
(require 'org-if-interpreter)
(require 'outline)

(defun org-if-hide-code ()
  "Hide the CODE section of an org-if file."
  (save-excursion
    (goto-char (org-find-exact-headline-in-buffer "Code"))
    (narrow-to-region (point-min) (point))))

(defun org-if-add-choices-heading ()
    "Add CHOICES section to org-if file."
    (save-excursion
      (goto-char (org-find-exact-headline-in-buffer "Code"))
      (open-line 1)
      (insert "* Choices\n")))

(defun org-if-confirm-babel-evaluate (lang body)
    "Replacement for `org-confirm-babel-evaluate' when mode is on.
This keeps babel from pestering the user with confirmation checks every time 
they visit a new file."
    (not (string= lang "org-if")))

(defun org-if-org-mode-hook ()
    "This is the `org-mode-hook' run by `org-if-active-mode'."
    ; First kill the previous buffer visited by org-if so it can be
    ; re-evaluated if the user returns to it.
    (when (and *org-if-current-file*
               (get-file-buffer *org-if-current-file*))
      (kill-buffer (get-file-buffer *org-if-current-file*)))
    (setf *org-if-current-file* (file-truename buffer-file-name))
    (setf *org-if-old-env*      (copy-hash-table *org-if-current-env*))
    (outline-show-all)
    (org-if-add-choices-heading)
    (org-babel-execute-buffer)
    (set-buffer-modified-p nil)
    (org-if-hide-code))

;;;###autoload
(define-minor-mode org-if-active-mode
  "This mode toggles whether the org-if system is active."
  :init-value nil
  :lighter    " Org-IF Active"
  :global     t
  (if org-if-active-mode
      (progn
        (customize-set-variable 'org-confirm-babel-evaluate
                                (function org-if-confirm-babel-evaluate))
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((org-if . t)))
        (add-hook 'org-mode-hook 'org-if-org-mode-hook)
        (org-if-reset-env)); Clear any data leftover from previous session
    (progn
      (custom-reevaluate-setting 'org-confirm-babel-evaluate)
      (custom-reevaluate-setting 'org-babel-load-languages)
      (remove-hook 'org-mode-hook 'org-if-org-mode-hook)
      (kill-buffer *org-if-current-file*) ; Kill last buffer visited by org-if.
      (setf *org-if-current-file* nil)
      (org-if-reset-env))))

;;;###autoload
(defun activate-org-if (&optional no-navigate-p)
    "Activate org-if-active minor-mode.
When NO-NAVIGATE-P is specified, do not go to file \"index.org\" in current directory."
    (interactive)
    ; Since the user needs to be in the current directory to activate org-if,
    ; any org-mode buffer must be closed so org-if can properly evaluate it.
    (let* ((curdir    (file-name-directory buffer-file-truename))
           (startfile (concat curdir "index.org")))
     (message curdir) 
     (message startfile) 
     (when (eq major-mode 'org-mode)
       (kill-buffer (current-buffer)))
     (org-if-active-mode 1)
     (when (null no-navigate-p)
       (find-file startfile))))

;;;###autoload
(defun deactivate-org-if ()
    "Deactivate org-if-active minor-mode."
    (interactive)
    (org-if-active-mode 0))

;;;###autoload
(defun toggle-org-if-active-mode ()
    "Toggle `org-if-active-mode'."
    (interactive)
    (if org-if-active-mode
        (deactivate-org-if)
        (activate-org-if)))

;;;###autoload
(defun org-if-save-and-quit ()
  "Save state of current org-if session in a file in `org-if-save-dir'.
Then quit."
  (interactive)
  (cl-labels ((parent-dir   (f)
                            (file-name-nondirectory
                             (directory-file-name (file-name-directory f))))
              (write-string (str fname)
                            (with-temp-buffer
                              (insert str)
                              (write-region (point-min)
                                            (point-max)
                                            fname
                                            nil))))
    (let* ((env-name  (parent-dir buffer-file-name))
           (file-name (concat (file-name-as-directory org-if-save-dir)
                              env-name))
           (state     (list *org-if-old-env*
                            *org-if-current-file*)))
      (when (not (file-directory-p org-if-save-dir))
        (make-directory org-if-save-dir))
      (write-string   (prin1-to-string state) file-name)
      (deactivate-org-if))))

;;;###autoload
(defun org-if-restore ()
  "Restore state of `*org-if-current-env*' and `*org-if-current-file*' from save.
Also restore last visited file."
  (interactive)
  (cl-labels ((read-from-file (path)
                              (with-temp-buffer
                                (insert-file-contents path)
                                (car (read-from-string (buffer-string)))))
              (parent-dir   (f)
                            (file-name-nondirectory
                             (directory-file-name (file-name-directory f)))))
    (let* ((game-name (parent-dir      buffer-file-name))
           (game-path (concat          (file-name-as-directory org-if-save-dir)
                                       game-name))
           (state     (read-from-file  game-path))
           (env       (nth             0 state))
           (file      (nth             1 state)))
      (when (get-file-buffer file)
        (kill-buffer file))
      (activate-org-if t)
      (setf *org-if-current-env* env)
      (find-file file))))

(provide 'org-if-active)
;;; org-if-active.el ends here

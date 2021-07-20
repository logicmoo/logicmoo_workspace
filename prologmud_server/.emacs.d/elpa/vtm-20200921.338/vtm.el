;;; vtm.el --- Manages vterm buffers with configuration files -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-vterm-manager
;; Created: August 23th, 2020
;; Keywords: convenience
;; Package-Requires: ()
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package manages vterm buffers with configuration files.
;; For more information see the README in the GitHub repo.

;;; Code:
(require 'subr-x)

(defvar vtm-prefix-string "*>"
  "Prefix string of the vterm buffer name.")
(defvar vtm-postfix-string "*"
  "Postfix string of the vterm buffer name.")

(declare-function vterm "ext:vterm.el" (&optional buffer-name) t)
(declare-function vterm-send-string "ext:vterm.el" (string &optional paste-p) t)

(defconst vtm--base (file-name-directory load-file-name)
  "Base directory of this package.")

;;;###autoload
(define-minor-mode vtm-edit-mode
  "Enable editing the vtm file."
  :global t
  :init-value nil)

(defun vtm--populate-buffer ()
  "Populate a buffer with scaffold."
  (insert-file-contents-literally
   (expand-file-name "example.el" vtm--base) nil nil nil t)
  (search-forward "{CURSOR}")
  (replace-match ""))

(defun vtm--property-missing-p (plist property)
  "Predicate of PROPERTY is missing in PLIST."
  (not (plist-member plist property)))

(defun vtm--property-missing-or-nil-p (plist property)
  "Predicate of PROPERTY is missing or nil in PLIST."
  (not (plist-get plist property)))

(defun vtm--property-exist-and-nil-p (plist property)
  "Predicate of PROPERTY exists and is nil in PLIST."
  (and (plist-member plist property)
       (eq nil (plist-get plist property))))

(defun vtm--property-blank-p (plist property)
  "Predicate of PROPERTY is blank in PLIST."
  (let ((value (plist-get plist property)))
    (and (stringp value) (string-blank-p value))))

(defun vtm--open-vterm ()
  "Open vterm with config from the current vtm buffer."
  (let* ((vtm-buffer (current-buffer))
         (conf (read vtm-buffer))
         (name (if (or (vtm--property-missing-or-nil-p conf :name)
                       (vtm--property-blank-p conf :name))
                   (file-name-base (buffer-name))
                 (plist-get conf :name)))
         (vterm-name
          (format "%s%s%s" vtm-prefix-string name vtm-postfix-string))
         (vterm-buffer (get-buffer vterm-name))
         (commands (plist-get conf :commands)))
    (if vterm-buffer
        (switch-to-buffer vterm-buffer)
      (vterm vterm-name)

      (dolist (command commands)
        (let ((sleep (plist-get command :sleep))
              (str (plist-get command :string))
              (control (if (or (vtm--property-missing-p command :control)
                               (vtm--property-blank-p command :control))
                           "return"
                         (plist-get command :control))))
          (when (integerp sleep)
            (sleep-for sleep))
          (when (and (stringp str)
                     (not (string-blank-p str)))
            (vterm-send-string str))
          (when control
            (funcall (intern (format "vterm-send-%s" control)))))))

    (kill-buffer vtm-buffer)))

;;;###autoload
(define-derived-mode vtm-mode emacs-lisp-mode "Elisp[vtm]"
  "Elisp Mode starter for vtm files."
  (if (= 0 (length (string-trim (buffer-string))))
      (vtm--populate-buffer)
    (unless vtm-edit-mode
      (unless (featurep 'vterm)
        (require 'vterm nil t))
      (if (featurep 'vterm)
          (vtm--open-vterm)
        (message "The vterm feature is unavailable.")))))

(setq auto-mode-alist
      (append
       '(;; File name ends in ‘.vtm’.
         ("\\.vtm\\'" . vtm-mode))
       auto-mode-alist))

(provide 'vtm)
;;; vtm.el ends here

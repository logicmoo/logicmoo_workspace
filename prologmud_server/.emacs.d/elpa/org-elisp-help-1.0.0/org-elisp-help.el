;;; org-elisp-help.el --- org links to emacs-lisp documentation

;; Copyright (C) 2013-2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((cl-lib "0.5") (org "9.0"))
;; Package-Version: 1.0.0
;; Package-Commit: 3e33ab1a2933dd7f2782ef91d667a37f12d633ab
;; Homepage: https://github.com/tarsius/org-elisp-help
;; Keywords: org, remember, lisp

;; This file is not part of Org.
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines two additional Org link types "elisp-function"
;; and "elisp-variable" which are intended to be used mainly in user
;; documentation.  By default opening these links is done by calling
;; `describe-function' and `describe-variable'.  That is only possible
;; when the function/variable is defined; therefor urls should include
;; the library defining the respective symbol.  If the symbol is not
;; `fbound'/`bound' when the link is opened and the url contains
;; information about the defining feature then that is loaded first.

;;   [[elisp-function:FEATURE:SYMBOL][...]]
;;   [[elisp-variable:FEATURE:SYMBOL][...]]
;;   [[elisp-function::SYMBOL][...]]
;;   [[elisp-variable::SYMBOL][...]]

;; If you want to capture information about a symbol while working on
;; elisp code you should instead use the "elisp-symbol" Org link type
;; defined in `org-elisp-symbol' (which is distributed with Org-Mode).
;; The link types defined here cannot be captured as that would
;; conflict with the "elisp-symbol" type.  Instead use the command:

;;   `org-elisp-help-function-insert-link' and
;;   `org-elisp-help-variable-insert-link'.

;;; Code:

(require 'cl-lib)
(require 'org)

(org-link-set-parameters "elisp-function" :follow 'org-elisp-help-function-open)
(org-link-set-parameters "elisp-variable" :follow 'org-elisp-help-variable-open)

(defcustom org-elisp-help-function-open-function 'describe-function
  "Function used to follow an \"elisp-function\" link."
  :group 'org-link
  :type '(choice (const describe-function)
                 (const find-function)
                 (const find-function-other-window)
                 (const find-function-other-frame)
                 (function :tag "other function")))

(defcustom org-elisp-help-variable-open-function 'describe-variable
  "Function used to follow an \"elisp-variable\" link."
  :group 'org-link
  :type '(choice (const describe-variable)
                 (const find-variable)
                 (const find-variable-other-window)
                 (const find-variable-other-frame)
                 (function :tag "other function")))

(defun org-elisp-help-function-open (path)
  "Visit the function at PATH.
Option `org-elisp-help-function-open-function' controls how the
function is visited; the default is `describe-function'."
  (cl-destructuring-bind (feature symbol)
      (mapcar 'intern (split-string path ":"))
    (if (fboundp symbol)
        (funcall org-elisp-help-function-open-function symbol)
      (unless (eq feature '##)
        (require feature nil t))
      (cond ((fboundp symbol)
             (funcall org-elisp-help-function-open-function symbol))
            ((eq feature '##)
             (error "%s isn't defined as a function" symbol))
            ((featurep feature)
             (error "Feature %s doesn't define %s as a function"
                    feature symbol))
            (t
             (error "Feature %s defining function %s cannot be found"
                    feature symbol))))))

(defun org-elisp-help-variable-open (path)
  "Visit the function at PATH.
Option `org-elisp-help-variable-open-function' controls how the
function is visited; the default is `describe-variable'."
  (cl-destructuring-bind (feature symbol)
      (mapcar 'intern (split-string path ":"))
    (if (boundp symbol)
        (funcall org-elisp-help-variable-open-function symbol)
      (unless (eq feature '##)
        (require feature nil t))
      (cond ((boundp symbol)
             (funcall org-elisp-help-variable-open-function symbol))
            ((eq feature '##)
             (error "%s isn't defined as a variable" symbol))
            ((featurep feature)
             (error "Feature %s doesn't define %s as a variable"
                    feature symbol))
            (t
             (error "Feature %s defining variable %s cannot be found"
                    feature symbol))))))

;;;###autoload
(defun org-elisp-help-function-insert-link
  (function &optional description)
  "Prompt for a function and insert a \"elisp-function\" link at point."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (def (function-called-at-point))
          (sel (completing-read
                (format "Insert function link%s: "
                        (if def (format " (default %s)" def) ""))
                obarray 'fboundp t nil nil
                (and def (symbol-name def)))))
     (list (if (equal sel "")
               (or def
                   (error "No selection"))
             (intern sel))
           (read-string "Description: " (or sel (symbol-name def))))))
  (let* ((file (symbol-file function))
         (feature (and file
                       (cdr (assq 'provide
                                  (cdr (assoc file load-history)))))))
    (when (or feature
              (y-or-n-p (format "Feature defining %s is unknown; %s"
                                function "insert link anyway?")))
      (insert (format "[[elisp-function:%s:%s][%s]]"
                      (or feature "")
                      function
                      (or description function))))))

;;;###autoload
(defun org-elisp-help-variable-insert-link
  (variable &optional description)
  "Prompt for a variable and insert a \"elisp-variable\" link at point."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (def (variable-at-point))
          (sel (completing-read
                (format "Insert variable link%s: "
                        (if (symbolp def)
                            (format " (default %s)" def)
                          ""))
                obarray
                (lambda (s)
                  (or (get s 'variable-documentation)
                      (and (boundp s) (not (keywordp s)))))
                t nil nil
                (and (symbolp def) (symbol-name def)))))
     (list (if (equal sel "")
               (if (symbolp def)
                   def
                 (error "No selection"))
             (intern sel))
           (read-string "Description: " (or sel (symbol-name def))))))
  (let* ((file (symbol-file variable))
         (feature (and file
                       (cdr (assq 'provide
                                  (cdr (assoc file load-history)))))))
    (when (or feature
              (y-or-n-p (format "Feature defining %s is unknown; %s"
                                variable "insert link anyway?")))
      (insert (format "[[elisp-variable:%s:%s][%s]]"
                      (or feature "")
                      variable
                      (or description variable))))))

(provide 'org-elisp-help)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; org-elisp-help.el ends here


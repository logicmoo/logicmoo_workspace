;;; lexbind-mode.el --- Puts the value of lexical-binding in the mode line -*- lexical-binding: t -*-

;; Copyright (C) 2013 Andrew Kirkpatrick

;; Author:     Andrew Kirkpatrick <ubermonk@gmail.com>
;; Maintainer: Andrew Kirkpatrick <ubermonk@gmail.com>
;; URL:        https://github.com/spacebat/lexbind-mode
;; Package-Version: 20141027.1429
;; Package-Commit: fa0a6848c1cfd3fbf45db43dc2deef16377d887d
;; Created:    08 Mar 2013
;; Keywords:   convenience, lisp
;; Version:    0.9

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You may have received a copy of the GNU General Public License
;; along with this program.  If not, see:
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 24 introduced lexical scope for variables to Emacs
;; Lisp.  However, rather than provide it via a new form, say llet or
;; whatever, the buffer local variable `lexical-binding' was
;; introduced to switch the behaviour of binding forms.  This is an
;; unfortunate situation because the semantics of a piece of code
;; depend on the value of a buffer local variable at the time of
;; evaluation.

;; This minor mode is intended to make it plain what the value of
;; `lexical-binding' is in the buffers used to evaluate Lisp forms.
;; It does this by adding to the mode line the string "(LEX)" to
;; indicate lexical binding is enabled, and "(DYN)" to indicate that
;; lexical binding is disabled and that dynamic binding is in effect.

;; Other lexical scope specific utilities such as lexbind-lexscratch
;; may also find a home here.

;; To install, once lexbind-mode.el is located somewhere in your
;; load-path, you can add this to your initialization file:

;; (require 'lexbind-mode)
;; (add-hook 'emacs-lisp-mode-hook 'lexbind-mode)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar lexbind-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-t") 'lexbind-insert-lexical-binding-t)
    (define-key map (kbd "C-c M-l") 'lexbind-toggle-lexical-binding)
    (define-key map (kbd "C-c M-s") 'lexbind-lexscratch)
    map)
  "Keymap for lexbind minor mode")

;;;###autoload
(defun lexbind-toggle-lexical-binding (&optional arg)
  "Toggle the variable `lexical-binding' on and off.  Interactive.
When called with a numeric argument, set `lexical-binding' to t
if the argument is positive, nil otherwise.
Optional argument ARG if nil toggles `lexical-binding', positive
enables it, non-positive disables it."
  (interactive)
  (let ((state (if arg
                   (plusp (prefix-numeric-value arg))
                 (not lexical-binding))))
    (setq lexical-binding state)
    (when (called-interactively-p 'any)
      (message "Lexical-binding %s" (if lexical-binding "enabled" "disabled")))
    state))

;; lexbind-lexscratch is here because I'm not aware of a better
;; package for it to live in, its useful and its small.
;;;###autoload
(defun lexbind-lexscratch (&optional other-window)
  "Make a lexical scratch buffer."
  (interactive "P")
  (let ((buf (get-buffer "*lexscratch*")))
    (unless buf
      (setq buf (get-buffer-create "*lexscratch*"))
      (with-current-buffer buf
        (lisp-interaction-mode)
        (setq lexical-binding t)
        (insert initial-scratch-message)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))))

;; TODO: test in an empty file
(defmacro lexbind-with-first-line (&rest body)
  "Narrow to the first line."
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char 1)
       (forward-line 1)
       (narrow-to-region 1 (point))
       (goto-char (point-min))
       ,@body)))

(defun lexbind-prop-line-p ()
  "Indicate if the first line of the current buffer is a prop-line."
  (lexbind-with-first-line
   (re-search-forward "-\\*-.+-\\*-" nil t)))

;;;###autoload
(defun lexbind-insert-lexical-binding-t (&optional verbose)
  "Attempt to set `lexical-binding' to t in the current buffer's
prop-line."
  (interactive)
  (cl-flet ((mesg (&rest args)
                  (when verbose
                    (apply 'message args))))
    (if (lexbind-prop-line-p)
        (let* ((vars (hack-local-variables-prop-line))
               (lexbound (assoc 'lexical-binding vars)))
          (mesg "prop-line found")
          (if vars
              (if lexbound
                  (if (not (cdr lexbound))
                      (progn
                        (mesg "need to substitute prop-line")
                        (lexbind-with-first-line
                         (when (re-search-forward "\\blexical-binding: *nil\\b" nil t)
                           (replace-match "lexical-binding: t" nil nil)
                           t)))
                    (message "lexical-binding is already non-nil"))
                (mesg "need to append lexical-binding: t")
                (lexbind-with-first-line
                 (when (re-search-forward "\\(-\\*- +\\)\\(.*?\\)\\(;*\\)\\( +-\\*-\\)" nil t)
                   (replace-match (if (plusp (length (match-string 2))) "; " "") nil nil nil 3)
                   (replace-match "lexical-binding: t\\4" nil nil nil 4))))
            (message "Malformed prop line")))
      (mesg "no prop-line found")
      (save-excursion
        (goto-char 1)
        (insert "-*- lexical-binding: t -*-\n")
        (set-mark (point))
        (goto-char 1)
        (comment-region (point) (mark))))))

;;;###autoload
(defun lexbind-modeline-content (&rest args)
  "Generate mode line content to indicate the value of `lexical-binding'.
Optional argument ARGS if provided, the first argument is taken as the value
of `lexical-binding'."
  (let ((lexbind-p (if (plusp (length args))
                       (car args)
                     lexical-binding))

        (pattern "Emacs Lisp lexical-binding is %s"))

    (concat
     " "
     (if lexbind-p
         (propertize
          "(LEX)"
          'help-echo (format pattern "enabled"))
       (propertize
        "(DYN)"
        'help-echo (format pattern "disabled"))))))


;;;###autoload
(define-minor-mode lexbind-mode
  "Toggle Lexbind mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When lexbind mode is enabled, the mode line of a window will
contain the string (LEX) for lexical binding, (DYN) for dynamic
binding, to indicate the state of the lexical-binding variable in
that buffer."

  :init-value nil
  :lighter (:eval (lexbind-modeline-content))
  :keymap lexbind-mode-keymap
  ;; TODO: there is nothing configurable yet - what to configure?
  :group 'lexbind)

(easy-menu-define lexbind-mode-menu
  lexbind-mode-keymap
  "lexbind-mode"
  '("Lexbind"
    ["Insert lexical-binding local var" (call-interactively
                                         'lexbind-insert-lexical-binding-t)]
    ["Toggle lexical-binding" (call-interactively
                               'lexbind-toggle-lexical-binding)]
    ["Lexical scratch buffer" (call-interactively
                               'lexbind-lexscratch)]
    ["Turn Off minor mode" (progn
                             (lexbind-mode -1)
                             (message "Lexbind mode disabled"))]
    ["Help for minor mode" (describe-function 'lexbind-mode)]))

(provide 'lexbind-mode)

;;; lexbind-mode.el ends here

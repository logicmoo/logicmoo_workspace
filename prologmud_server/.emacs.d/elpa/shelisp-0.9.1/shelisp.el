;;; shelisp.el --- execute elisp in shell          -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Michael R. Mauger <michael@mauger.com>
;; Version: 0.9.1
;; Package-Type: simple
;; Keywords: terminals, lisp, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Comint process (likely shell-mode) can write out Emacs Lisp
;; expressions and have them executed.

;; When the shell process writes out a string of the form:
;;   \e_#EMACS# elisp-expr \a
;;
;; Where, "elisp-expr" is a valid elisp expression.  The elisp
;; expression is executed as if you had invoked the function
;; within Emacs itself.  The elisp expression may include a call to
;; the function `f' which will expand the filename parameter into an
;; appropriate filename for Emacs using the appropriate Tramp prefix
;; if necessary.

;; This script also defines an Alist variable that creates shell
;; commands and the `printf'-style format to generate the full elisp
;; expression with command parameters substituted into the command.  A
;; function is placed in the `shell-mode-hook' to actually create the
;; shell functions and aliases to format the elisp expressions and
;; embed them in an escape sequence so that they are detected and
;; executed.

;; In most usage this mode merely allows you to type "e filename"
;; rather than "C-x C-f filename" which isn't much of a savings.
;; However, with this mode enabled, you can write shell scripts to
;; invoke Emacs Lisp functions.  But beware, the shell script will not
;; wait for completion of the elisp expression, nor return anything
;; back (see ToDo's below).

;; INSTALLATION

;; After installing this package from ELPA, you must add the following
;; to your Emacs initialization script:

;;   (add-hook 'shell-mode-hook #'shelisp-mode)

;; TO DOs:

;; * Provide a security feature that prompts the Emacs user to approve
;; * the execution of any elisp expressions submitted thru the shelisp
;; * escape sequence.

;; * Support `term-mode' like `shell-mode'

;; * Provide support for creation of shell commands for command shells
;;   other than bash -- csh, tcsh, zsh, ksh, ash, dash, fish, mosh, sh.
;;
;;   Support for non-Linux shells is left as an exercise for a
;;   masochistic hacker.

;; * Implement a wait for completion facility similar to `emacsclient'
;;   or the work done in `with-editor' with the "sleeping editor."
;;   That is, pause the shell activity with a long sleep, until C-c
;;   C-c or C-c C-k is typed in Emacs and the caller is awoken with a
;;   signal.

;; KNOWN BUGS

;; The simplistic implementation of the shell functions will not
;; properly handle filenames containing double quote characters (\")
;; nor backslashes (\\).  While this is an error, it does not
;; represent a significant limitation in the implementation.  The
;; caller can properly add backslashes to the filename string before
;; passing it to printf to generate the elisp expression.  In the end,
;; the purpose is to create a valid elisp expression string.

;;; Code:
(require 'cl-lib)
(require 'pp)

;;;###autoload
(define-minor-mode shelisp-mode
  "Enable elisp expressions embedded in ANSI APC (Application
Program Control) escape sequences to be located and executed
while in a shell mode buffer."
  nil " ShElisp" nil

  (if (not shelisp-mode)
      (remove-hook 'comint-preoutput-filter-functions
	           #'shelisp-exec-lisp)
    ;; Parse elisp escape sequences
    (add-hook 'comint-preoutput-filter-functions
	      #'shelisp-exec-lisp 'append)
    (shelisp-add-commands)))

;;;###autoload
(defvar shelisp-debug nil
  "When non-nil, display messages showing the elisp expression.")

(defun shelisp--file-name (file)
  "Apply remote host in `default-directory' to FILE."
  (if (and (file-name-absolute-p file)
	   (not (file-remote-p file)))
      (concat (file-remote-p default-directory) file)
    file))

(defun shelisp--result-as-string (result)
  "Return RESULT as a string.
If it already is a string, then just return it.  Otherwise,
convert it to a string."
  (cond ((null result)    "")
        ((stringp result) result)
        (:else            (pp-to-string result))))

(defun shelisp-exec-lisp (&optional str)
  "Detect escape sequence in STR to execute Emacs Lisp."
  (interactive)

  (when (and shelisp-mode str)
    (let* ((APC "\\(?:\e_\\|\x9f\\)")
	   (tag "#EMACS#")
	   (ST  "\\(?:[\a\x9c]\\|[\e][\\\\]\\)")
	   (cmd-re "\\(?:[^\a\x9c\e]\\|\e[^\\\\]\\)")
	   (apc-re (concat APC tag "\\(" cmd-re "*\\)" ST))
	   (case-fold-search nil)
	   cmd rep)

      ;; Look for APC escape sequences
      (while (string-match apc-re str)
        (setq cmd (match-string 1 str)
              rep "")
        ;; Trace, if requested
        (when shelisp-debug
          (message "shelisp> `%s'" cmd))

        ;; Replace the elisp expresssion with it's value
        ;;   if the value is nil, treat it as an empty string
        (setq rep (save-match-data
                    (save-excursion
                      (condition-case err
                          (shelisp--result-as-string
                           (eval `(cl-flet ((f (file) (shelisp--file-name file)))
	                            ,(read cmd))
                                 t))
                        ;; When an error occurs, replace with the error message
	                (error
	                 (format "shelisp: `%s': %S" cmd err)))))
              str (replace-match
                   (concat rep (unless (string-equal "" rep) "\n"))
                   t t str)))))
  str)


;;;###autoload
(defvar shelisp-commands (let ((cmds '(("e" .     "(find-file-other-window (f \"%s\"))")
                                       ("v" .     "(view-file-other-window (f \"%s\"))")
                                       ("dired" . "(dired \"%s\")")
                                       ("ediff" . "(ediff (f \"%s\") (f \"%s\"))"))))
                           (when (locate-library "magit")
                             (push '("magit" . "(magit-status)") cmds))
                           (when (or (bound-and-true-p viper-mode)
                                     (bound-and-true-p evil-mode))
                             (push '("vim" . "(find-file-other-window (f \"%s\"))") cmds)
                             (push '("vi" . "(find-file-other-window (f \"%s\"))") cmds))
                           cmds)

  "Alist of shell commands and corresponding Lisp expressions.
Each entry in the alist consists of the shell alias to be set as the
command, and the `printf' style string to generate the elisp
expression to be executed.

If a parameter to the elisp expression is a filename, then we
need to be sure that proper filename parsing in context occurs.
We do this by passing filename parameters through the elisp
function `f'[1].  This function makes sure that filename has
proper Tramp prefixes if the shell session is remote.  So, rather
than just embedding the filename in the elisp expression, using
printf, with \"\\\"%s\\\"\", you use \\=`(f \\\"%s\\\")\\='.

[1] The `f' function is `cl-flet' bound for the shelisp
expression and cannot be used elsewhere.")

(defun shelisp-add-commands ()
  "Add Emacs Lisp to shell aliases (assumes GNU bash syntax)."

  (when (and shelisp-mode shelisp-commands)
    (let ((proc (get-buffer-process (current-buffer))))
      (dolist (c shelisp-commands)
        (let ((cmd (car c))
              (expr (cdr c)))
          (process-send-string
           proc
           (apply #'format
                  (mapconcat #'identity
                             '("unset -f shelisp_%s"
                               "function shelisp_%s { printf '\\e_#EMACS# %s \\a' \"$@\"; }"
                               "alias %s=shelisp_%s" "")
                             " ; ")
                (list cmd cmd
		      (replace-regexp-in-string "\"" "\\\\\"" expr)
		      cmd cmd)))))
      (process-send-string proc "\n"))))

;;;; ChangeLog:

;; 2019-04-07  Michael R. Mauger  <michael@mauger.com>
;; 
;; 	Fixed spelling error (Thanks Stefan)
;; 
;; 2019-04-07  Michael R. Mauger  <michael@mauger.com>
;; 
;; 	Remove GitLab URL; code will be managed in ELPA
;; 
;; 2019-04-07  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/shelisp/shelisp.el: Fix up copyright
;; 
;; 	Require cl-lib rather than cl-macs.
;; 	(shelisp-exec-lisp): Also use lexical-binding internally.
;; 
;; 2019-04-06  Michael R. Mauger  <michael@mauger.com>
;; 
;; 	Added shelisp package
;; 


(provide 'shelisp)
;;; shelisp.el ends here

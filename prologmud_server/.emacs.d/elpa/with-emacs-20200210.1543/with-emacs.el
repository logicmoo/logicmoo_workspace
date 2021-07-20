;;; with-emacs.el --- Evaluate Emacs Lisp expressions in a separate Emacs process -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/20
;; Version: 0.4.1
;; Package-Version: 20200210.1543
;; Package-Commit: 9f99bec56f87e53deb9f33b364eda77677a17eb9
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/with-emacs.el
;; Keywords: tools

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

;; Evaluate expressions in a separate Emacs process:
;;
;; ,---
;; | ;;; `with-emacs'
;; | 
;; | ;; Evaluate expressions in a separate Emacs.
;; | (with-emacs ...)
;; | 
;; | ;; Specify the version of Emacs and enable lexical binding
;; | (with-emacs :path "/path/to/{version}/emacs" :lexical t ...)
;; | 
;; | ;; Use partially applied function (see `with-emacs-define-partially-applied' for more)
;; | ;; instead of writting verry long parameter each time:
;; | (with-emacs-nightly-t ...)
;; | ;; Equaivalent to:
;; | ;; (with-emacs :path "/path/to/nightly/emacs" :lexical t ...)
;; | 
;; | ;;; `with-emacs-server'
;; | 
;; | ;; Evaluate expressions in server "name" or signal an error if no such server.
;; | (with-emacs-server "name" ...)
;; | 
;; | ;; Evaluate expressions in server "name" and start a server if necessary.
;; | (with-emacs-server "name" :ensure t ...)
;; | (with-emacs-server "name" :ensure "/path/to/{version}/emacs" ...)
;; | 
;; | ;; Kill server after 100 minutes of idle
;; | (with-emacs-server "name" :ensure t :timeout 100 ...)
;; | ;; Set default timeout for every new server:
;; | (setq with-emacs-server-timeout 100)
;; | (with-emacs-server "name" :ensure t ...)
;; | ;; Disable default timeout temporary:
;; | (with-emacs-server "name" :ensure t :timeout nil ...)
;; `---
;;
;; See README for more information.

;;; Change Log:

;;
;; 0.4.1  2020/02/05
;;   Add timeout timer for `with-emacs-server'.
;;
;; 0.4.0  2019/11/25
;;   Add macro `with-emacs-server'.
;;
;; 0.3.0  2019/11/15
;;   Add macro `with-emacs-define-partially-applied'.
;;
;; 0.2.1  2019/08/19
;;
;;   Refactor the macro with-emacs
;;
;; 0.2.0  2019/06/05
;;
;;   Add function -extract-return-value to replace the regexp to get more
;;   accurate result and remove the outer double quotes from return value.
;;
;; 0.1.0  2019/04/20
;;
;;   Initial version.
;;

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'server)

(defcustom with-emacs-executable-path (concat invocation-directory invocation-name)
  "Location of Emacs executable."
  :type 'string
  :group 'with-emacs)

(defcustom with-emacs-lexical-binding nil
  "Whether to use lexical binding when evaluating code."
  :type 'boolean
  :group 'with-emacs)

(defcustom with-emacs-output-regexp
  (rx string-start
      (0+ "\n")
      ;; Text that need to be redirected to the `*Messages*' buffer
      (group (0+ anything))
      "\n"
      ;; Result of a expression
      (or (group (0+ (not (any "\n")))) ;; match a symbol
          ;; match a quoted string, e.g. "foo\"bar\""
          (group "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\""))
      string-end)
  "Regexp for extracting message or result from output."
  :type 'string
  :group 'with-emacs)

(defcustom with-emacs-server-timeout nil
  "Number of minutes idle time before kill server process, ‘nil’ means no timeout.

This can be overwritten by parameter :timeout in ‘with-emacs-server’."
  :type 'number
  :group 'with-emacs)

(defvar with-emacs-sit-for-seconds 0.1
  "Comint buffer/process polling interval.")

(defalias 'with-emacs--flatten
  (if (fboundp 'flatten-tree) 'flatten-tree
    ;; From Emacs 27 nightly:
    ;; https://emba.gnu.org/emacs/emacs/commit/36b05dc84247db1391a423df94e4b9a478e29dc5
    (lambda (tree)
      "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
      (let (elems)
        (while (consp tree)
          (let ((elem (pop tree)))
            (while (consp elem)
              (push (cdr elem) tree)
              (setq elem (car elem)))
            (if elem (push elem elems))))
        (if tree (push tree elems))
        (nreverse elems)))))

(defun with-emacs--replace-CR-character-while-sending (fn proc string)
  "Advice for FN to Replace CR character in STRING while sending to PROC."
  (funcall fn proc (replace-regexp-in-string "\r" "\\\\r" string)))

(defun with-emacs--cl-args-body (args)
  "Remove leading key-value pairs from ARGS."
  (let ((it args))
    (catch 'break
      (while t
        (if (keywordp (car it))
            (setq it (cddr it))
          (throw 'break it))))))

(defun with-emacs--cli-args (path lexical)
  `(,path
    "--batch"
    "--eval" ,(format "(setq lexical-binding %s)" lexical)
    "--eval" ,(format "%s" '(while t (prin1 (eval (read) lexical-binding))))))

(defun with-emacs--eval-expr-send-input (proc form eoe-indicator)
  ;; Clean buffer
  (delete-region (point-min) (point-max))

  ;; Send input
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        (advice (and (= 25 emacs-major-version)
                     #'with-emacs--replace-CR-character-while-sending)))
    (when advice (advice-add comint-input-sender :around advice))
    (unwind-protect
        (mapc (lambda (it)
                (insert (format "%S" it))
                (comint-send-input))
              form)
      (when advice (advice-remove comint-input-sender advice))))

  ;; Finish
  (process-send-string proc (format "%S\n" eoe-indicator)))

(defun with-emacs--eval-expr-accept-output (proc eoe-indicator)
  ;; Accept output
  (while (and (eq 'run (process-status proc))
              (progn
                (goto-char comint-last-input-end)
                (not
                 (save-excursion
                   (re-search-forward
                    (regexp-quote eoe-indicator) nil t)))))
    (accept-process-output (get-buffer-process (current-buffer)))
    (sit-for with-emacs-sit-for-seconds))

  ;; Kill process
  (when (eq 'run (process-status proc))
    (process-send-string proc "(kill-emacs)\n"))

  ;; Waiting for the process exiting
  (while (not (memq (process-status proc) '(exit signal)))
    (sit-for with-emacs-sit-for-seconds)))

(defun with-emacs--eval-expr-error-message (proc)
  (when (< 0 (process-exit-status proc))
    (save-excursion
      (goto-char comint-last-output-start)
      (save-restriction
        (narrow-to-region comint-last-output-start
                          (goto-char (next-property-change (point))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun with-emacs--eval-expr (buf form eoe-indicator)
  (let ((proc (get-buffer-process buf)))
    (with-current-buffer buf
      (with-emacs--eval-expr-send-input proc form eoe-indicator)
      (with-emacs--eval-expr-accept-output proc eoe-indicator)
      (prog1 (with-emacs--eval-expr-error-message proc)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(defun with-emacs--extract-return-value (s)
  "Extract return value from string S."
  (with-temp-buffer
    (insert s)
    (emacs-lisp-mode)
    (goto-char (point-max))
    (sexp-at-point)))

(defun with-emacs--handle-output (output error)
  (if error
      (signal 'error (list error))
    (when output
      (let* ((strs (split-string output comint-prompt-regexp))
             (ret (car (cddr (reverse strs)))))
        ;; Redirect message to `*Messages*'
        (mapc (lambda (s)
                ;; (message "s: [%S]" s) ;; debug
                (when (string-match with-emacs-output-regexp s)
                  (with-current-buffer (get-buffer "*Messages*")
                    (let ((inhibit-read-only t)
                          (msg (match-string 1 s)))
                      (goto-char (point-max))
                      (insert "\n")
                      (insert msg)
                      msg))))
              strs)
        ;; Return the result of the last expression as a string
        (with-emacs--extract-return-value ret)))))

(cl-defmacro with-emacs (&rest body
                         &key
                           (path nil has-path?)
                           (lexical nil has-lexical?)
                         &allow-other-keys)
  "Start a emacs in a subprocess, and execute BODY there.
If PATH not set, use `with-emacs-executable-path'.
If LEXICAL not set, use `with-emacs-lexical-binding.'"
  (declare (indent defun) (debug t))
  (let ((cmdlist (with-emacs--cli-args
                  (if has-path? path with-emacs-executable-path)
                  (if has-lexical? lexical with-emacs-lexical-binding))))
    `(let* ((process-connection-type nil)
            (eoe-indicator "with-emacs-eoe")
            (comint-prompt-regexp "Lisp expression: ")
            (cmdlist ',cmdlist)
            (pbuf ,(current-buffer))
            (output nil)
            (comint-output-filter-functions
             (lambda (text)
               ;; (message "==> text: %s" text)
               (setq output (concat output text))))
            (buf (apply 'make-comint-in-buffer "with-emacs"
                        (generate-new-buffer-name "*with-emacs*")
                        (car cmdlist) nil (cdr cmdlist))))
       (let ((error (with-emacs--eval-expr buf ',body eoe-indicator)))
         (with-emacs--handle-output output error)))))

(cl-defmacro with-emacs-server (server
                                &rest
                                  body
                                &key
                                  (ensure nil has-ensure?)
                                  (timeout nil has-timeout?)
                                &allow-other-keys)
  "Contact the Emacs server named SERVER and evaluate FORM there.
Returns the result of the evaluation, or signals an error if it
cannot contact the specified server.

If ENSURE not nil, start a server when necessary. It can be t or
a path of emacs, if it is t, use `with-emacs-executable-path' as default.

The server will be killed after TIMEOUT minutes, if TIMEOUT not given,
use `with-emacs-server-timeout' as default, if TIMEOUT is nil,
disable timeout timer.

\(with-emacs-server \"foo\"
  :ensure t
  :timeout 100
(1+ 1))
=> 2"
  (declare (indent 1) (debug t))
  `(let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
          (server-file (expand-file-name ,server server-dir)))
       (unless (file-exists-p server-file)
         (if ,has-ensure?
             (shell-command
              (format "%s -Q --daemon=%s"
                      (cond ((stringp ,ensure) ,ensure)
                            (t with-emacs-executable-path))
                      ,server))
           (error "No such server: %s" ,server)))
       (server-eval-at ,server
                       '(condition-case err
                            (progn
                              (let ((time ,(if has-timeout? timeout
                                             with-emacs-server-timeout)))
                                (when time
                                  (run-with-idle-timer time 0
                                                       (lambda () (kill-emacs)))))
                              ,@(with-emacs--cl-args-body body))
                          (error err)))))

(defvar with-emacs-partially-applied-functions '() "List of partially applied functions")

(defmacro with-emacs-define-partially-applied (&rest args)
  "Generate functions that are partial application of `with-emacs' to ARGS.

The form of ARGS is:

   (part-name1 path1 lexical-or-not)
   (part-name2 path2 lexical-or-not)
   ...

For example:

  ```
  (with-emacs-define-partially-applied
   (t      nil t)
   (24.3   \"/path/to/emacs-24.3\")
   (24.4-t \"/path/to/emacs-24.4\" t))
  ;; =>
  ;; (with-emacs-t      &rest BODY &key PATH)
  ;; (with-emacs-24.3   &rest BODY &key LEXICAL)
  ;; (with-emacs-24.4-t &rest BODY)
  ```"
  (dolist (arg args)
    (pcase-let ((`(,part-name ,path ,lexical) arg))
      (let ((name (intern (format "with-emacs-%s" part-name)))
            (keys (remove
                   nil
                   `(,(unless path    '(path    nil has-path?))
                     ,(unless lexical '(lexical nil has-lexical?))))))
        (when keys
          (setq keys
                (append
                 (push '&key keys)
                 '(&allow-other-keys))))
        (eval
         `(cl-defmacro ,name (&rest body ,@keys)
            (declare (indent defun) (debug t))
            (let ((params
                   (with-emacs--flatten
                    (remove
                     nil
                     (list (when ,path    '(:path    ,path))
                           (when ,lexical '(:lexical ,lexical)))))))
              `(with-emacs ,@params ,@body))))
        (add-to-list 'with-emacs-partially-applied-functions name)))))

(provide 'with-emacs)

;;; with-emacs.el ends here

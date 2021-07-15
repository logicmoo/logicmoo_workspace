;;; shelisp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shelisp" "shelisp.el" (0 0 0 0))
;;; Generated autoloads from shelisp.el

(autoload 'shelisp-mode "shelisp" "\
Enable elisp expressions embedded in ANSI APC (Application
Program Control) escape sequences to be located and executed
while in a shell mode buffer.

\(fn &optional ARG)" t nil)

(defvar shelisp-debug nil "\
When non-nil, display messages showing the elisp expression.")

(defvar shelisp-commands (let ((cmds '(("e" . "(find-file-other-window (f \"%s\"))") ("v" . "(view-file-other-window (f \"%s\"))") ("dired" . "(dired \"%s\")") ("ediff" . "(ediff (f \"%s\") (f \"%s\"))")))) (when (locate-library "magit") (push '("magit" . "(magit-status)") cmds)) (when (or (bound-and-true-p viper-mode) (bound-and-true-p evil-mode)) (push '("vim" . "(find-file-other-window (f \"%s\"))") cmds) (push '("vi" . "(find-file-other-window (f \"%s\"))") cmds)) cmds) "\
Alist of shell commands and corresponding Lisp expressions.
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

\[1] The `f' function is `cl-flet' bound for the shelisp
expression and cannot be used elsewhere.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shelisp" '("shelisp-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shelisp-autoloads.el ends here

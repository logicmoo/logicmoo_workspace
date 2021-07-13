;;; lfe-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inferior-lfe" "inferior-lfe.el" (0 0 0 0))
;;; Generated autoloads from inferior-lfe.el

(autoload 'inferior-lfe-mode "inferior-lfe" "\
Major mode for interacting with an inferior LFE process.

\\{inferior-lfe-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-lfe-mode-hook' (in that order)." t nil)

(autoload 'inferior-lfe "inferior-lfe" "\
Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'.
If `CMD' is given, use it to start the shell, otherwise:
`inferior-lfe-program' `inferior-lfe-program-options' -env TERM vt100.

\(fn CMD)" t nil)

(defalias 'run-lfe 'inferior-lfe)

(register-definition-prefixes "inferior-lfe" '("inferior-lfe-" "lfe-" "switch-to-lfe"))

;;;***

;;;### (autoloads nil "lfe-indent" "lfe-indent.el" (0 0 0 0))
;;; Generated autoloads from lfe-indent.el

(autoload 'lfe-indent-function "lfe-indent" "\
This function is the normal value of the variable `lfe-indent-function'.

If this function is the value of the variable `lisp-indent-function' then
`calculate-lisp-indent' will call it to determine if the
arguments of a LFE function call should be indented specially.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under;
`STATE' is the `parse-partial-sexp' state for that position.

Copied from function `lisp-indent-function', but with gets of
lfe-indent-{function,hook} and it uses `lfe-body-indent'.

\(fn INDENT-POINT STATE)" nil nil)

(register-definition-prefixes "lfe-indent" '("define-lfe-indent" "lfe-" "put-lfe-indent"))

;;;***

;;;### (autoloads nil "lfe-mode" "lfe-mode.el" (0 0 0 0))
;;; Generated autoloads from lfe-mode.el

(autoload 'lfe-mode "lfe-mode" "\
Major mode for editing Lisp Flavoured Erlang.  It's just like `lisp-mode'.

Other commands:
\\{lfe-mode-map}" t nil)

(add-to-list 'auto-mode-alist '("\\.lfe\\(s\\|sh\\)?\\'" . lfe-mode) t)

(dolist (lfe-ext '(".beam" ".jam" ".vee")) (add-to-list 'completion-ignored-extensions lfe-ext))

(register-definition-prefixes "lfe-mode" '("lfe-" "prettify-symbols-alist"))

;;;***

;;;### (autoloads nil nil ("lfe-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lfe-mode-autoloads.el ends here

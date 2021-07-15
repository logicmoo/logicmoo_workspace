;;; lsp-p4-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-p4" "lsp-p4.el" (0 0 0 0))
;;; Generated autoloads from lsp-p4.el

(register-definition-prefixes "lsp-p4" '("lsp-clients-"))

;;;***

;;;### (autoloads nil "p4lang-mode" "p4lang-mode.el" (0 0 0 0))
;;; Generated autoloads from p4lang-mode.el

(add-to-list 'auto-mode-alist '("\\.p4\\'" . p4lang-mode))

(autoload 'p4lang-mode "p4lang-mode" "\
Major mode for editing P4 code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `p4lang-mode-hook'.

Key bindings:
\\{p4lang-mode-map}" t nil)

(register-definition-prefixes "p4lang-mode" '("p4lang-"))

;;;***

;;;### (autoloads nil nil ("lsp-p4-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-p4-autoloads.el ends here

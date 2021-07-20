;;; yang-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yang-mode" "yang-mode.el" (0 0 0 0))
;;; Generated autoloads from yang-mode.el

(autoload 'yang-mode "yang-mode" "\
Major mode for editing YANG modules.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `yang-mode-hook'.

Key bindings:
\\{yang-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))

(register-definition-prefixes "yang-mode" '("yang-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yang-mode-autoloads.el ends here

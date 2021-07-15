;;; rescript-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rescript-indent" "rescript-indent.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rescript-indent.el

(register-definition-prefixes "rescript-indent" '("rescript-"))

;;;***

;;;### (autoloads nil "rescript-mode" "rescript-mode.el" (0 0 0 0))
;;; Generated autoloads from rescript-mode.el

(autoload 'rescript-mode "rescript-mode" "\
Major mode for ReScript code.

\\{rescript-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.resi?\\'" . rescript-mode))

(register-definition-prefixes "rescript-mode" '("rescript-"))

;;;***

;;;### (autoloads nil nil ("rescript-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rescript-mode-autoloads.el ends here

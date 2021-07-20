;;; systemtap-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "systemtap-mode" "systemtap-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from systemtap-mode.el

(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))

(autoload 'systemtap-mode "systemtap-mode" "\
Major mode for editing SystemTap scripts.

Key bindings:
\\{systemtap-mode-map}

\(fn)" t nil)

(register-definition-prefixes "systemtap-mode" '("systemtap-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; systemtap-mode-autoloads.el ends here

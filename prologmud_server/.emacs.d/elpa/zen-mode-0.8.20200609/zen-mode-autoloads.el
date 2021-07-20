;;; zen-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zen-mode" "zen-mode.el" (0 0 0 0))
;;; Generated autoloads from zen-mode.el

(autoload 'zen-mode "zen-mode" "\
A major mode for the Zen programming language.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.zen\\'" . zen-mode))

(register-definition-prefixes "zen-mode" '("zen-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zen-mode-autoloads.el ends here

;;; arbitools-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "arbitools" "arbitools.el" (0 0 0 0))
;;; Generated autoloads from arbitools.el

(autoload 'arbitools-mode "arbitools" "\
Major mode for Chess Tournament Management.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.trf?\\'" . arbitools-mode))

(register-definition-prefixes "arbitools" '("arbitools-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; arbitools-autoloads.el ends here

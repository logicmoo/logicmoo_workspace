;;; zombie-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zombie" "zombie.el" (0 0 0 0))
;;; Generated autoloads from zombie.el

(autoload 'zombie-mode "zombie" "\
Major mode for editing ZOMBIE programs.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.zombie\\'" . zombie-mode))

(register-definition-prefixes "zombie" '("zombie-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zombie-autoloads.el ends here

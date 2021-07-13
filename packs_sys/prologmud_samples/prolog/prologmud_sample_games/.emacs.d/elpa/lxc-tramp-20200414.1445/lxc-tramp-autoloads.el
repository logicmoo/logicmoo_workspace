;;; lxc-tramp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lxc-tramp" "lxc-tramp.el" (0 0 0 0))
;;; Generated autoloads from lxc-tramp.el

(autoload 'lxc-tramp-add-method "lxc-tramp" "\
Add lxc tramp method." nil nil)

(eval-after-load 'tramp '(progn (lxc-tramp-add-method) (tramp-set-completion-function lxc-tramp-method lxc-tramp-completion-function-alist)))

(register-definition-prefixes "lxc-tramp" '("lxc-tramp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lxc-tramp-autoloads.el ends here

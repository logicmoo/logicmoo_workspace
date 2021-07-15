;;; lxd-tramp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lxd-tramp" "lxd-tramp.el" (0 0 0 0))
;;; Generated autoloads from lxd-tramp.el

(defconst lxd-tramp-completion-function-alist '((lxd-tramp--parse-running-containers "")) "\
Default list of (FUNCTION FILE) pairs to be examined for lxd method.")

(defconst lxd-tramp-method "lxd" "\
Method to connect to LXD containers.")

(autoload 'lxd-tramp-add-method "lxd-tramp" "\
Add lxd tramp method." nil nil)

(eval-after-load 'tramp '(progn (lxd-tramp-add-method) (tramp-set-completion-function lxd-tramp-method lxd-tramp-completion-function-alist)))

(register-definition-prefixes "lxd-tramp" '("lxd-tramp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lxd-tramp-autoloads.el ends here

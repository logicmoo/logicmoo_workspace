;;; v-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "v-mode" "v-mode.el" (0 0 0 0))
;;; Generated autoloads from v-mode.el

(let ((loads (get 'v-mode 'custom-loads))) (if (member '"v-mode" loads) nil (put 'v-mode 'custom-loads (cons '"v-mode" loads))))

(autoload 'v-mode "v-mode" "\
Major mode for editing V files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.v?v\\.vsh\\'" . v-mode))

(register-definition-prefixes "v-mode" '("v-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; v-mode-autoloads.el ends here

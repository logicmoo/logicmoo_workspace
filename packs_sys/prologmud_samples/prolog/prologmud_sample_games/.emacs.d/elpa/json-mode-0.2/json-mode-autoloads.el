;;; json-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "json-mode" "json-mode.el" (0 0 0 0))
;;; Generated autoloads from json-mode.el

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(autoload 'json-mode "json-mode" "\
Major mode for editing JavaScript Object Notation (JSON) data files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "json-mode" '("json-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; json-mode-autoloads.el ends here

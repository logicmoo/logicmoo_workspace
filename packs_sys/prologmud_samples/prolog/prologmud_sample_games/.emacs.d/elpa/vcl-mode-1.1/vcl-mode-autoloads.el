;;; vcl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vcl-mode" "vcl-mode.el" (0 0 0 0))
;;; Generated autoloads from vcl-mode.el

(add-to-list 'auto-mode-alist (cons (purecopy "\\.vcl\\'") 'vcl-mode))

(autoload 'vcl-mode "vcl-mode" "\
Major mode for editing Varnish Configuration Language code.

Key bindings:
\\{vcl-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vcl-mode" '("vcl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vcl-mode-autoloads.el ends here

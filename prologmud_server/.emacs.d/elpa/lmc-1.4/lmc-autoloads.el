;;; lmc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lmc" "lmc.el" (0 0 0 0))
;;; Generated autoloads from lmc.el

(add-to-list 'auto-mode-alist '("\\.elmc\\'" . lmc-asm-mode))

(autoload 'lmc-asm-mode "lmc" "\
Major mode to edit LMC assembly code.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lmc" '("lmc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lmc-autoloads.el ends here

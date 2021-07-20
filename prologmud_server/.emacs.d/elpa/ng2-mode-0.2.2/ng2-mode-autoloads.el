;;; ng2-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ng2-html" "ng2-html.el" (0 0 0 0))
;;; Generated autoloads from ng2-html.el

(autoload 'ng2-html-mode "ng2-html" "\
Major mode for Angular 2 templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-html-mode))

(register-definition-prefixes "ng2-html" '("ng2-html-"))

;;;***

;;;### (autoloads nil "ng2-mode" "ng2-mode.el" (0 0 0 0))
;;; Generated autoloads from ng2-mode.el

(autoload 'ng2-mode "ng2-mode" "\
Activates the appropriate Angular 2-related mode for the buffer." t nil)

;;;***

;;;### (autoloads nil "ng2-shared" "ng2-shared.el" (0 0 0 0))
;;; Generated autoloads from ng2-shared.el

(register-definition-prefixes "ng2-shared" '("ng2-"))

;;;***

;;;### (autoloads nil "ng2-ts" "ng2-ts.el" (0 0 0 0))
;;; Generated autoloads from ng2-ts.el

(autoload 'ng2-ts-mode "ng2-ts" "\
Major mode for Angular 2 TypeScript

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.component.ts\\'" . ng2-ts-mode))

(add-to-list 'auto-mode-alist '("\\.service.ts\\'" . ng2-ts-mode))

(add-to-list 'auto-mode-alist '("\\.pipe.ts\\'" . ng2-ts-mode))

(add-to-list 'auto-mode-alist '("\\.directive.ts\\'" . ng2-ts-mode))

(add-to-list 'auto-mode-alist '("\\.guard.ts\\'" . ng2-ts-mode))

(add-to-list 'auto-mode-alist '("\\.module.ts\\'" . ng2-ts-mode))

(register-definition-prefixes "ng2-ts" '("ng2-ts-"))

;;;***

;;;### (autoloads nil nil ("ng2-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ng2-mode-autoloads.el ends here

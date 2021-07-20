;;; use-ttf-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-ttf" "use-ttf.el" (0 0 0 0))
;;; Generated autoloads from use-ttf.el

(autoload 'use-ttf-install-fonts "use-ttf" "\
Install all .ttf fonts in the `use-ttf-default-ttf-fonts'." t nil)

(autoload 'use-ttf-set-default-font "use-ttf" "\
Use the font by `use-ttf-default-ttf-font-name` variable.
This will actually set your Emacs to your target font." t nil)

(register-definition-prefixes "use-ttf" '("use-ttf-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-ttf-autoloads.el ends here

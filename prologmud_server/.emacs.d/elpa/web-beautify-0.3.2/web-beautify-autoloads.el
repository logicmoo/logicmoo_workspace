;;; web-beautify-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "web-beautify" "web-beautify.el" (0 0 0 0))
;;; Generated autoloads from web-beautify.el

(autoload 'web-beautify-html "web-beautify" "\
Format region if active, otherwise the current buffer.

Formatting is done according to the html-beautify command." t nil)

(autoload 'web-beautify-html-buffer "web-beautify" "\
Format the current buffer according to the html-beautify command." nil nil)

(autoload 'web-beautify-css "web-beautify" "\
Format region if active, otherwise the current buffer.

Formatting is done according to the css-beautify command." t nil)

(autoload 'web-beautify-css-buffer "web-beautify" "\
Format the current buffer according to the css-beautify command." nil nil)

(autoload 'web-beautify-js "web-beautify" "\
Format region if active, otherwise the current buffer.

Formatting is done according to the js-beautify command." t nil)

(autoload 'web-beautify-js-buffer "web-beautify" "\
Format the current buffer according to the js-beautify command." nil nil)

(register-definition-prefixes "web-beautify" '("web-beautify-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; web-beautify-autoloads.el ends here

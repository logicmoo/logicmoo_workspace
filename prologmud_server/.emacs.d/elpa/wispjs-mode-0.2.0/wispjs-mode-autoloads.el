;;; wispjs-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wispjs-mode" "wispjs-mode.el" (0 0 0 0))
;;; Generated autoloads from wispjs-mode.el

(autoload 'wispjs-mode "wispjs-mode" "\
Major mode for Wisp

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.wisp\\'" 'wispjs-mode))

(autoload 'wispjs-mode/compile "wispjs-mode" "\
Invoke the Wisp compiler for the current buffer." t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wispjs-mode-autoloads.el ends here

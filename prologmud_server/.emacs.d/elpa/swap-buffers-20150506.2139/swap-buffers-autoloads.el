;;; swap-buffers-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swap-buffers" "swap-buffers.el" (0 0 0 0))
;;; Generated autoloads from swap-buffers.el

(autoload 'swap-buffers "swap-buffers" "\
Swap buffer from selected window with specified buffer.
If NEGATIVE-KEEP-FOCUS-OPTION is t -- use the opposite setting of swap-buffers-keep-focus.

\(fn &optional NEGATIVE-KEEP-FOCUS-OPTION)" t nil)

(autoload 'swap-buffers-dired-find-file "swap-buffers" nil t nil)

(register-definition-prefixes "swap-buffers" '("swap-buffers-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swap-buffers-autoloads.el ends here

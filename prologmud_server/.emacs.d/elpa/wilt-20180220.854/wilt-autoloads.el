;;; wilt-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wilt" "wilt.el" (0 0 0 0))
;;; Generated autoloads from wilt.el

(autoload 'wilt-mode "wilt" "\
Minor mode for calculating WILT metrics on your code.

Just displays WILT metric in status line whenever it's configured
to do so.

When called interactively, toggle `wilt-mode'.  With prefix ARG,
enable `wilt-mode' if ARG is positive, otherwise disable it.

When called from Lisp, enable `wilt-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `wilt-mode'.
Otherwise behave as if called interactively.

\\{wilt-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "wilt" '("wilt-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wilt-autoloads.el ends here

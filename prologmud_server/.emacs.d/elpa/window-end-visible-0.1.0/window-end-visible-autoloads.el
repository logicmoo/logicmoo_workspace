;;; window-end-visible-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "window-end-visible" "window-end-visible.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-end-visible.el

(autoload 'window-end-visible "window-end-visible" "\
Return the last visible position in WINDOW.

Works around a limitation of `window-end', at a speed penalty.

The issue this function solves is that the following is not true
as might be expected:

   (pos-visible-in-window-p (window-end))

The speed penalty varies greatly depending on your configuration.
For example, tabbar.el makes calling `pos-visible-in-window-p'
quite expensive.

WINDOW and UPDATE are as documented at `window-end'.

PARTIALLY is as documented at `pos-visible-in-window-p'.

\(fn &optional WINDOW UPDATE PARTIALLY)" nil nil)

(register-definition-prefixes "window-end-visible" '("window-end-visible--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; window-end-visible-autoloads.el ends here

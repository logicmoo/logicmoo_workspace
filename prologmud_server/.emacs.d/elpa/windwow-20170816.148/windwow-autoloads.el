;;; windwow-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "windwow" "windwow.el" (0 0 0 0))
;;; Generated autoloads from windwow.el

(autoload 'windwow-save-buffer-list "windwow" "\
Save current buffers as NAME.
Switch to this list of buffers by calling 'windwow-load-buffer-list.

\(fn NAME)" t nil)

(autoload 'windwow-save-buffer-list-no-name "windwow" "\
Save current buffers as concatenated buffer names.
Switch to this list of buffers by calling 'windwow-load-buffer-list." t nil)

(autoload 'windwow-load-buffer-list "windwow" "\
Switch to buffers from a BUFFER-LIST that was previously saved.

\(fn BUFFER-LIST)" t nil)

(autoload 'windwow-load-buffer-from-list "windwow" "\
Load BUFFER-LIST and switch to a BUFFER from that list.

\(fn BUFFER-LIST BUFFER)" t nil)

(autoload 'windwow-save-window-arrangement "windwow" "\
Save current window arrangement as NAME.
Load window arrangement with 'window-load-window-arrangement.

\(fn NAME)" t nil)

(autoload 'windwow-load-window-arrangement "windwow" "\
Load NAME, a previously saved window arrangement.

\(fn NAME)" t nil)

(autoload 'windwow-load-window-arrangement-and-buffer-list "windwow" "\
Load a window arrangement, COMMANDS,  and a buffer list, BUFFERS.

\(fn COMMANDS BUFFERS)" t nil)

(register-definition-prefixes "windwow" '("windwow-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; windwow-autoloads.el ends here

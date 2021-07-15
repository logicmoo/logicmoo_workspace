;;; wconf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wconf" "wconf.el" (0 0 0 0))
;;; Generated autoloads from wconf.el

(autoload 'wconf-load "wconf" "\
Load stored configurations from FILENAME, defaults to
`wconf-file'.

\(fn &optional FILENAME)" t nil)

(autoload 'wconf-create "wconf" "\
Clone the current configuration or create a new \"empty\" one.
The new configuration is appended to the list and becomes active.

With optional prefix argument NEW, or if there are no
configurations yet, create a new configuration from the current
window config.

\(fn &optional NEW)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wconf" '("wconf-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wconf-autoloads.el ends here

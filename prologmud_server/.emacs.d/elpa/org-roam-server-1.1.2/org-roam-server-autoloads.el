;;; org-roam-server-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam-server" "org-roam-server.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-server.el

(defvar org-roam-server-mode nil "\
Non-nil if Org-Roam-Server mode is enabled.
See the `org-roam-server-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-server-mode'.")

(custom-autoload 'org-roam-server-mode "org-roam-server" nil)

(autoload 'org-roam-server-mode "org-roam-server" "\
Start the http server and serve org-roam files.

This is a minor mode.  If called interactively, toggle the
`Org-Roam-Server mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-roam-server-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-roam-server" '("current-buffer-data" "default-filters" "network-vis-options" "org-roam-" "roam-data" "server-css"))

;;;***

;;;### (autoloads nil nil ("org-roam-server-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-server-autoloads.el ends here

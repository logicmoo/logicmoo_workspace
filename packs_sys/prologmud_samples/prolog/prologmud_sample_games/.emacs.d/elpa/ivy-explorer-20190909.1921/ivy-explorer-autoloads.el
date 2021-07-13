;;; ivy-explorer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-explorer" "ivy-explorer.el" (0 0 0 0))
;;; Generated autoloads from ivy-explorer.el

(defvar ivy-explorer-mode nil "\
Non-nil if ivy-explorer mode is enabled.
See the `ivy-explorer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-explorer-mode'.")

(custom-autoload 'ivy-explorer-mode "ivy-explorer" nil)

(autoload 'ivy-explorer-mode "ivy-explorer" "\
Globally enable `ivy-explorer' for file navigation.

This is a minor mode.  If called interactively, toggle the
`ivy-explorer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'ivy-explorer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

`ivy-explorer-mode' is a global minor mode which changes
`read-file-name-function' which is used for file completion.

When `ivy-explorer-enable-counsel-explorer' (by default it is),
`find-file' and `counsel-find-file' will be remapped to
`counsel-explorer.', too.

See `ivy-explorer-map' for bindings used in the minibuffer.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-explorer" '("counsel-explorer" "ivy-explorer"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-explorer-autoloads.el ends here

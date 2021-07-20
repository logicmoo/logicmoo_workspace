;;; orglink-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orglink" "orglink.el" (0 0 0 0))
;;; Generated autoloads from orglink.el

(autoload 'orglink-mode "orglink" "\
Toggle display Org-mode links in other major modes.

This is a minor mode.  If called interactively, toggle the
`Orglink mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `orglink-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

On the links the following commands are available:

\\{orglink-mouse-map}

\(fn &optional ARG)" t nil)

(put 'global-orglink-mode 'globalized-minor-mode t)

(defvar global-orglink-mode nil "\
Non-nil if Global Orglink mode is enabled.
See the `global-orglink-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-orglink-mode'.")

(custom-autoload 'global-orglink-mode "orglink" nil)

(autoload 'global-orglink-mode "orglink" "\
Toggle Orglink mode in all buffers.
With prefix ARG, enable Global Orglink mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Orglink mode is enabled in all buffers where
`turn-on-orglink-mode-if-desired' would do it.

See `orglink-mode' for more information on Orglink mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "orglink" '("orglink-" "turn-on-orglink-mode-if-desired"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orglink-autoloads.el ends here

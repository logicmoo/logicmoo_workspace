;;; ivy-migemo-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-migemo" "ivy-migemo.el" (0 0 0 0))
;;; Generated autoloads from ivy-migemo.el

(autoload 'ivy-migemo-toggle-fuzzy "ivy-migemo" "\
Toggle the re builder to match fuzzy or not." t nil)

(autoload 'ivy-migemo-toggle-migemo "ivy-migemo" "\
Toggle the re builder to use/unuse migemo." t nil)

(defvar ivy-migemo-search-default-handling-mode nil "\
Non-nil if Ivy-Migemo-Search-Default-Handling mode is enabled.
See the `ivy-migemo-search-default-handling-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-migemo-search-default-handling-mode'.")

(custom-autoload 'ivy-migemo-search-default-handling-mode "ivy-migemo" nil)

(autoload 'ivy-migemo-search-default-handling-mode "ivy-migemo" "\
When turned on, override functions which use `swiper--re-builder'
to handle `search-default-mode' when `ivy-migemo'is turned on.

This is a minor mode.  If called interactively, toggle the
`Ivy-Migemo-Search-Default-Handling mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value
'ivy-migemo-search-default-handling-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'global-ivy-migemo-search-default-handling-mode 'ivy-migemo-search-default-handling-mode "1.3.4")

(register-definition-prefixes "ivy-migemo" '("ivy-migemo-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-migemo-autoloads.el ends here

;;; org-tag-beautify-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-tag-beautify" "org-tag-beautify.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-tag-beautify.el

(defvar org-tag-beautify-mode nil "\
Non-nil if Org-Tag-Beautify mode is enabled.
See the `org-tag-beautify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-tag-beautify-mode'.")

(custom-autoload 'org-tag-beautify-mode "org-tag-beautify" nil)

(autoload 'org-tag-beautify-mode "org-tag-beautify" "\
A minor mode that beautify Org tags with icons and images.

This is a minor mode.  If called interactively, toggle the
`Org-Tag-Beautify mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-tag-beautify-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-tag-beautify" '("org-tag-beautify-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-tag-beautify-autoloads.el ends here

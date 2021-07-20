;;; org-outline-numbering-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-outline-numbering" "org-outline-numbering.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-outline-numbering.el

(autoload 'org-outline-numbering-mode "org-outline-numbering" "\
Minor mode to number ‘org-mode’ headings.

This is a minor mode.  If called interactively, toggle the
`Org-Outline-Numbering mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-outline-numbering-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-outline-numbering-display "org-outline-numbering" "\
Put numbered overlays on ‘org-mode’ headings." t nil)

(autoload 'org-outline-numbering-clear "org-outline-numbering" "\
Clear outline numbering overlays in widened buffer." t nil)

(register-definition-prefixes "org-outline-numbering" '("org-outline-numbering-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-outline-numbering-autoloads.el ends here

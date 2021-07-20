;;; org-multi-wiki-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-multi-wiki" "org-multi-wiki.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-multi-wiki.el

(defvar org-multi-wiki-global-mode nil "\
Non-nil if Org-Multi-Wiki-Global mode is enabled.
See the `org-multi-wiki-global-mode' command
for a description of this minor mode.")

(custom-autoload 'org-multi-wiki-global-mode "org-multi-wiki" nil)

(autoload 'org-multi-wiki-global-mode "org-multi-wiki" "\
Toggle Org-Multi-Wiki-Global mode on or off.

This is a minor mode.  If called interactively, toggle the
`Org-Multi-Wiki-Global mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-multi-wiki-global-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{org-multi-wiki-global-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'org-multi-wiki-entry-files "org-multi-wiki" "\
Get a list of Org files in a namespace.

If NAMESPACE is omitted, the current namespace is used, as in
`org-multi-wiki-directory'.

If AS-BUFFERS is non-nil, this function returns a list of buffers
instead of file names.

\(fn &optional NAMESPACE &key AS-BUFFERS)" nil nil)

(autoload 'org-multi-wiki-follow-link "org-multi-wiki" "\
Follow a wiki LINK.

\(fn LINK)" nil nil)

(autoload 'org-multi-wiki-store-link "org-multi-wiki" "\
Store a link." nil nil)

(autoload 'org-multi-wiki-switch "org-multi-wiki" "\
Set the current wiki to NAMESPACE.

\(fn NAMESPACE)" t nil)

(autoload 'org-multi-wiki-visit-entry "org-multi-wiki" "\
Visit an Org file for HEADING in the directory in NAMESPACE.

\(fn HEADING &key NAMESPACE)" t nil)

(register-definition-prefixes "org-multi-wiki" '("org-multi-wiki-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-multi-wiki-autoloads.el ends here

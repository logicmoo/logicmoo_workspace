;;; pandoc-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pandoc-mode" "pandoc-mode.el" (0 0 0 0))
;;; Generated autoloads from pandoc-mode.el

(autoload 'pandoc-mode "pandoc-mode" "\
Minor mode for interacting with Pandoc.

This is a minor mode.  If called interactively, toggle the
`Pandoc mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pandoc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'conditionally-turn-on-pandoc "pandoc-mode" "\
Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks." nil nil)

(register-definition-prefixes "pandoc-mode" '("pandoc-"))

;;;***

;;;### (autoloads nil "pandoc-mode-utils" "pandoc-mode-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pandoc-mode-utils.el

(register-definition-prefixes "pandoc-mode-utils" '("base-header-level" "bibliography" "citation-abbreviations" "columns" "csl" "def" "dpi" "email-obfuscation" "eol" "epub-chapter-level" "highlight-style" "id-prefix" "indented-code-classes" "ipynb-output" "jsmath" "katex" "latex" "mimetex" "number-offset" "pandoc-" "pdf-engine" "reference-location" "shift-heading-level-by" "slide-level" "tab-stop" "title-prefix" "track-changes" "webtex" "wrap"))

;;;***

;;;### (autoloads nil nil ("pandoc-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pandoc-mode-autoloads.el ends here

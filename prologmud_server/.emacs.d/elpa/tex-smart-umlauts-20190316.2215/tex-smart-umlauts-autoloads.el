;;; tex-smart-umlauts-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tex-smart-umlauts" "tex-smart-umlauts.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tex-smart-umlauts.el

(autoload 'tex-smart-umlauts-reencode-all "tex-smart-umlauts" "\
Reencode all charactes in region.  Only characters between the
buffer positions FROM and TO are decoded. If FROM and TO are nil the
whole buffer is encoded. The original encodings of all characters in
the region is dropped. If NOENCODE is non-nil the characters are not
encoded (i.e. they are written literaly to the file in the current
buffer's file encoding). Otherwise the characters are tex-encoded
according to the rules of `tex-smart-umlauts-encodings'. In
interactive use NOENCODE is non-nil if a prefix argument is used.

\(fn &optional NOENCODE FROM TO)" t nil)

(autoload 'tex-smart-umlauts-show-encodings "tex-smart-umlauts" "\
Show encodings of all umlauts in buffer." t nil)

(autoload 'tex-smart-umlauts-hide-encodings "tex-smart-umlauts" "\
Hide encodings of all umlauts in buffer." t nil)

(autoload 'tex-smart-umlauts-decode "tex-smart-umlauts" "\


\(fn &optional FROM TO)" nil nil)

(make-obsolete 'tex-smart-umlauts-decode '"Use `tex-smart-umlauts-mode' to enable tex-smart-umlauts" '"1.3.0")

(autoload 'tex-smart-umlauts-mode "tex-smart-umlauts" "\
Minor mode for seamless translation of LaTeX special characters.

This is a minor mode.  If called interactively, toggle the
`Tex-Smart-Umlauts mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tex-smart-umlauts-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "tex-smart-umlauts" '("tex-smart-umlauts-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tex-smart-umlauts-autoloads.el ends here

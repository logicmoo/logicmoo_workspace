;;; e2ansi-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "e2ansi" "e2ansi.el" (0 0 0 0))
;;; Generated autoloads from e2ansi.el

(autoload 'e2ansi-view-buffer "e2ansi" "\
Display the e2ansi representation of the selected buffer." t nil)

(autoload 'e2ansi-write-file "e2ansi" "\
Save the e2ansi representation of the current buffer to the file FILE-NAME.

Unless a name is given, the file will be named xxx.ansi, where
xxx is the file name associated with the buffer.

If CONFIRM is non-nil, ask for confirmation before overwriting an
existing file. Interactively, confirmation is required unless you
supply a prefix argument.

\(fn &optional FILE-NAME CONFIRM)" t nil)

(autoload 'e2ansi-batch-convert "e2ansi" "\
Convert the remaining files on the command line to ANSI format." nil nil)

(autoload 'e2ansi-print-buffer "e2ansi" "\
Convert content of BUFFER to ANSI and print to DEST.

\(fn &optional BUFFER DEST)" nil nil)

(register-definition-prefixes "e2ansi" '("e2ansi-"))

;;;***

;;;### (autoloads nil "e2ansi-list" "e2ansi-list.el" (0 0 0 0))
;;; Generated autoloads from e2ansi-list.el

(register-definition-prefixes "e2ansi-list" '("e2ansi-list-"))

;;;***

;;;### (autoloads nil "e2ansi-magic" "e2ansi-magic.el" (0 0 0 0))
;;; Generated autoloads from e2ansi-magic.el

(register-definition-prefixes "e2ansi-magic" '("e2ansi-magic-diff-p"))

;;;***

;;;### (autoloads nil "e2ansi-silent" "e2ansi-silent.el" (0 0 0 0))
;;; Generated autoloads from e2ansi-silent.el

(register-definition-prefixes "e2ansi-silent" '("e2ansi-silent-message"))

;;;***

;;;### (autoloads nil nil ("e2ansi-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; e2ansi-autoloads.el ends here

;;; tiny-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tiny" "tiny.el" (0 0 0 0))
;;; Generated autoloads from tiny.el

(autoload 'tiny-expand "tiny" "\
Expand current snippet.
It polls the expander functions one by one
if they can expand the thing at point.
First one to return a string succeeds.
These functions are expected to set `tiny-beg' and `tiny-end'
to the bounds of the snippet that they matched.
At the moment, only `tiny-mapconcat' is supported.
`tiny-mapconcat2' should be added to expand rectangles." t nil)

(autoload 'tiny-helper "tiny" "\
Helper function for `tiny-expand'.

The arguments to this function construct a “tiny expression”
\"mBSEO|F\" where
  E is the end value (END-VAL)     - defaults to 0 internally if nil or \"\",
                                      or 9 if BEGIN-VAL is nil or \"\" too.
  B is the begin value (BEGIN-VAL) - defaults to 0 internally if nil or \"\".
  S is the separator (SEP)         - defaults to \" \" if nil or \"\".
  O is the elisp operation (OP)    - defaults to \"\" if nil.
  F is the format (FMT)            - defaults to \"\" if nil.

If `tiny' expansion is possible at point, do it.
Otherwise activate the helper to generate a valid “tiny
expression” and expand that.

Usage: Call TINY-HELPER, ↵↵↵↵↵            -> 0 1 2 3 4 5 6 7 8 9
       Call TINY-HELPER, 9↵2↵_↵+1*x2↵↵    -> 5_7_9_11_13_15_17_19
       Call TINY-HELPER, 15↵1↵↵-30*2x↵%x↵ -> 1c 1a 18 16 14 12 10 e c a 8 6 4 2 0

\(fn &optional END-VAL BEGIN-VAL SEP OP FMT)" t nil)

(register-definition-prefixes "tiny" '("tiny-"))

;;;***

;;;### (autoloads nil "tiny-test" "tiny-test.el" (0 0 0 0))
;;; Generated autoloads from tiny-test.el

(register-definition-prefixes "tiny-test" '("with-text-value"))

;;;***

;;;### (autoloads nil nil ("tiny-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tiny-autoloads.el ends here

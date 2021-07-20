;;; wikinforg-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wikinforg" "wikinforg.el" (0 0 0 0))
;;; Generated autoloads from wikinforg.el

(autoload 'wikinforg "wikinforg" "\
Return Org entry from `wikinfo'.
QUERY and PREDICATE are passed to `wikinfo'.
If ARG is equivalent to `\\[universal-argument]', display the entry in a buffer.

\(fn &optional ARG QUERY PREDICATE)" t nil)

(autoload 'wikinforg-capture "wikinforg" "\
Wikinforg wrapper for use in capture templates.
Call `wikinforg' command with search SUFFIX.
If the wikinforg call fails, the user's query is returned.
If the command is aborted, an empty string is returned so the capture will not error.

\(fn &optional SUFFIX)" nil nil)

(register-definition-prefixes "wikinforg" '("wikinforg-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wikinforg-autoloads.el ends here

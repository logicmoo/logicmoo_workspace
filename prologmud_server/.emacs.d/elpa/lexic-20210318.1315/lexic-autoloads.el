;;; lexic-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lexic" "lexic.el" (0 0 0 0))
;;; Generated autoloads from lexic.el

(autoload 'lexic-search "lexic" "\
Search WORD through the command line tool lexic.
The result will be displayed in buffer named with
`lexic-buffer-name' with `lexic-mode' if called interactively.

When provided with DICT-LIST-NAME, query `lexic-dictionary-alist'
to get the new dictionary list before search.
Alternatively, dictionary list can be specified directly
by DICT-LIST.  Any non-list value of it means using all dictionaries.

When called interactively, prompt for the word.
Prefix argument have the following meaning:
If `lexic-dictionary-alist' is defined,
use prefix argument to select a new DICT-LIST-NAME.
Otherwise, prefix argument means using all dictionaries.

When INTERACTIVE-P is non-nil, a buffer displaying the result(s) is shown.
Otherwise, the result is returned as a string.

When NO-HISTORY-P is non-nil, the search is not added to the session history.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
          escape the character right after

TODO decouple the tool from the general method.

\(fn WORD &optional DICT-LIST-NAME DICT-LIST INTERACTIVE-P NO-HISTORY-P)" t nil)

(register-definition-prefixes "lexic" '("lexic-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lexic-autoloads.el ends here

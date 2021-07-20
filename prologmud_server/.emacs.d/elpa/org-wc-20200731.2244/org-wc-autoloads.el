;;; org-wc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-wc" "org-wc.el" (0 0 0 0))
;;; Generated autoloads from org-wc.el

(autoload 'org-word-count "org-wc" "\
Report the number of words in the Org mode buffer or selected region.

Ignores heading lines, blocks, comments, drawers, and links
depending on customizable variables in customization group org-wc.

LaTeX macros are counted as 1 word. 

\(fn BEG END)" t nil)

(autoload 'org-wc-count-subtrees "org-wc" "\
Count words in each subtree, putting result as the property :org-wc on that heading." t nil)

(autoload 'org-wc-display "org-wc" "\
Show subtree word counts in the entire buffer.
With prefix argument, only show the total wordcount for the buffer or region
in the echo area.

Use \\[org-wc-remove-overlays] to remove the subtree times.

Ignores: heading lines,
         blocks,
         comments,
         drawers.
LaTeX macros are counted as 1 word.

\(fn TOTAL-ONLY)" t nil)

(autoload 'org-wc-remove-overlays "org-wc" "\
Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer.

\(fn &optional BEG END NOREMOVE)" t nil)

(register-definition-prefixes "org-wc" '("org-w"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-wc-autoloads.el ends here

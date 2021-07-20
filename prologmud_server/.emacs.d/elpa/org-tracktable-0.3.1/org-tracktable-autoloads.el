;;; org-tracktable-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-tracktable" "org-tracktable.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-tracktable.el

(autoload 'org-tracktable-insert-table "org-tracktable" "\
Insert the a table with the name defined by `org-tracktable-table-name'." t nil)

(autoload 'org-tracktable-status "org-tracktable" "\
Report the number of words between positions BEG and END.
If a table is inserted with `org-tracktable-table-insert', shows words written today.
If `org-tracktable-daily-goal' is set to more than 0, show % of daily goal.

\(fn BEG END)" t nil)

(autoload 'org-tracktable-status-today "org-tracktable" "\
Reports number of words written today" t nil)

(autoload 'org-tracktable-write "org-tracktable" "\
Write progress to the tracktable.
If the last entry is from today, this entry will be updated.
Otherwise a new entry will be made.  It is only necessary to call this function
when you're done writing for the day." t nil)

(register-definition-prefixes "org-tracktable" '("org-tracktable-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-tracktable-autoloads.el ends here

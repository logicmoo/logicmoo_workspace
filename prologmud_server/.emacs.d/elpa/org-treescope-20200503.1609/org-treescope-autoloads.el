;;; org-treescope-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-treescope" "org-treescope.el" (0 0 0 0))
;;; Generated autoloads from org-treescope.el

(autoload 'org-treescope-exit "org-treescope" "\
Exit calendar and restore original org buffer to normal state." t nil)

(register-definition-prefixes "org-treescope" '("org-treescope-"))

;;;***

;;;### (autoloads nil "org-treescope-calendarranges" "org-treescope-calendarranges.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-calendarranges.el

(autoload 'org-treescope-calendarranges-day-lowerbound-forwards "org-treescope-calendarranges" "\
Move left-flank by NDAYS forwards.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-lowerbound-backwards "org-treescope-calendarranges" "\
Move left-flank by NDAYS backwards.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-upperbound-forwards "org-treescope-calendarranges" "\
Move right-flank by NDAYS forwards.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-upperbound-backwards "org-treescope-calendarranges" "\
Move right-flank by NDAYS backwards.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-frommidpoint-leftwards "org-treescope-calendarranges" "\
Ignore left and right flanks, and select all dates before midpoint.  Don't update if SILENT.

\(fn &optional SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-frommidpoint-rightwards "org-treescope-calendarranges" "\
Ignore left and right flanks, and select all dates after midpoint.  Don't update if SILENT.

\(fn &optional SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-frommidpoint-stop "org-treescope-calendarranges" "\
Set the flank selector to nothing and restore shift range mode." t nil)

(autoload 'org-treescope-calendarranges-day-shiftrange-backwards "org-treescope-calendarranges" "\
Shift entire range back by NDAYS and update midpoint.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-shiftrange-forwards "org-treescope-calendarranges" "\
Shift entire range forwards by NDAYS and update midpoint.  Don't update if SILENT.

\(fn &optional NDAYS SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-shiftrange-backwards-week "org-treescope-calendarranges" "\
Shift entire range back by a week and update midpoint.  Don't update if SILENT.

\(fn &optional SILENT)" t nil)

(autoload 'org-treescope-calendarranges-day-shiftrange-forwards-week "org-treescope-calendarranges" "\
Shift entire range forwards by a week and update midpoint.  Don't update if SILENT.

\(fn &optional SILENT)" t nil)

(register-definition-prefixes "org-treescope-calendarranges" '("org-treescope-calendarranges--"))

;;;***

;;;### (autoloads nil "org-treescope-cyclestates" "org-treescope-cyclestates.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-cyclestates.el

(autoload 'org-treescope-cyclestates-todo-forwards "org-treescope-cyclestates" "\
Cycle the TODO groups given by the `org-treescope-cyclestates-todo' variable forward." t nil)

(autoload 'org-treescope-cyclestates-todo-backwards "org-treescope-cyclestates" "\
Cycle the TODO groups given by the `org-treescope-cyclestates-todo' variable forward." t nil)

(autoload 'org-treescope-cyclestates-priority-forwards "org-treescope-cyclestates" "\
Cycle the PRIORITY groups given by the `org-treescope-cyclestates-priority' variable forward." t nil)

(autoload 'org-treescope-cyclestates-priority-backwards "org-treescope-cyclestates" "\
Cycle the PRIORITY groups given by the `org-treescope-cyclestates-priority' variable forward." t nil)

(autoload 'org-treescope-cyclestates-time-forwards "org-treescope-cyclestates" "\
Cycle through the time mode selectors." t nil)

(register-definition-prefixes "org-treescope-cyclestates" '("org-treescope-cyclestates-"))

;;;***

;;;### (autoloads nil "org-treescope-datehelper" "org-treescope-datehelper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-datehelper.el

(register-definition-prefixes "org-treescope-datehelper" '("org-treescope-datehelper--"))

;;;***

;;;### (autoloads nil "org-treescope-faces" "org-treescope-faces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-faces.el

(register-definition-prefixes "org-treescope-faces" '("org-treescope-faces-"))

;;;***

;;;### (autoloads nil "org-treescope-modehelper" "org-treescope-modehelper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-modehelper.el

(register-definition-prefixes "org-treescope-modehelper" '("org-treescope-modehelper-"))

;;;***

;;;### (autoloads nil "org-treescope-query" "org-treescope-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-treescope-query.el

(autoload 'org-treescope-query-apply-to-buffer "org-treescope-query" "\
Apply the QUERY to the org buffer as an argument to `org-ql-sparse-tree'.
Also switch to org buffer and then reselect the calendar window.

\(fn &optional QUERY)" t nil)

(register-definition-prefixes "org-treescope-query" '("org-treescope-query--"))

;;;***

;;;### (autoloads nil nil ("org-treescope-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-treescope-autoloads.el ends here

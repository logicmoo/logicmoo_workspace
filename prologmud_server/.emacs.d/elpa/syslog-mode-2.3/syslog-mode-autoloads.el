;;; syslog-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "syslog-mode" "syslog-mode.el" (0 0 0 0))
;;; Generated autoloads from syslog-mode.el

(defvar syslog-setup-on-load nil "\
*If not nil setup syslog mode on load by running syslog-add-hooks.")

(autoload 'syslog-view "syslog-mode" "\
Open a view of syslog files with optional filters and highlights applied.
When called interactively the user is prompted for a member of `syslog-views' and the
arguments are determined from the chosen member.
FILES can be either nil in which case the view is applied to the current log file, or
it can be the same as the first argument to `syslog-get-filenames' - a list of cons
cells whose cars are filenames and whose cdrs indicate how many logfiles to include.
LABEL indicates whether or not to label each line with the filename it came from.
RXSHOWSTART, RXSHOWEND and RXHIDESTART, RXHIDEEND are optional regexps which will be 
used to filter in/out blocks of buffer lines with `syslog-filter-lines'. 
STARTDATE and ENDDATE are optional dates used to filter the lines with `syslog-filter-dates'; 
they can be either date strings or time lists as returned by `syslog-date-to-time'.
HIGHLIGHTS is a list of cons cells whose cars are regexps and whose cdrs are faces to 
highlight those regexps with.

\(fn FILES &optional LABEL RXSHOWSTART RXSHOWEND RXHIDESTART RXHIDEEND STARTDATE ENDDATE REMOVEDATES HIGHLIGHTS BUFNAME)" t nil)

(autoload 'syslog-filter-lines "syslog-mode" "\
Restrict buffer to blocks of text between matching regexps.
If the user only enters one regexp then just filter matching lines instead of blocks.
With prefix ARG: remove matching blocks.

\(fn &optional ARG)" t nil)

(defvar syslog-views nil "\
A list of views.
If regexps matching end lines are left blank then lines will be filtered instead of blocks (see `syslog-filter-lines').")

(custom-autoload 'syslog-views "syslog-mode" t)

(autoload 'syslog-date-to-time "syslog-mode" "\
Convert DATE string to time.
If no year is present in the date then the current year is used.
If DATE can't be parsed then if SAFE is non-nil return nil otherwise throw an error.

\(fn DATE &optional SAFE)" nil nil)

(autoload 'syslog-filter-dates "syslog-mode" "\
Restrict buffer to lines between times START and END (Emacs time lists).
With prefix ARG: remove lines between dates.
If either START or END are nil then treat them as the first/last time in the
buffer respectively.

\(fn START END &optional ARG)" t nil)

(autoload 'syslog-mode "syslog-mode" "\
Major mode for working with system logs.

\\{syslog-mode-map}" t nil)

(register-definition-prefixes "syslog-mode" '("syslog-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; syslog-mode-autoloads.el ends here

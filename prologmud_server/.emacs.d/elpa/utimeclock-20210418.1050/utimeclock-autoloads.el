;;; utimeclock-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "utimeclock" "utimeclock.el" (0 0 0 0))
;;; Generated autoloads from utimeclock.el

(autoload 'utimeclock-from-context "utimeclock" "\
Search for STR, accumulate all times after it, return the accumulated time.
Argument COMBINE-ALL-TIMES keeps searching backwards, accumulating all times in the buffer.

\(fn COMBINE-ALL-TIMES)" nil nil)

(autoload 'utimeclock-from-context-summary "utimeclock" "\
Return the time before the cursor or contained within the selection (when available)." nil nil)

(autoload 'utimeclock-toggle "utimeclock" "\
Clock on/off, declare time ranges from the current time.

Add time to the end of the current lines time or search backwards to find one.
Otherwise add `utimeclock-time-prefix' and the time after it." t nil)

(autoload 'utimeclock-insert "utimeclock" "\
Insert the current time at the cursor.

Unlike `utimeclock-toggle' this doesn't pair time ranges or ensure `utimeclock-time-prefix' text." t nil)

(autoload 'utimeclock-show-summary "utimeclock" "\
Show a summary of the last time and all times combined in the message buffer." t nil)

(register-definition-prefixes "utimeclock" '("utimeclock-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; utimeclock-autoloads.el ends here

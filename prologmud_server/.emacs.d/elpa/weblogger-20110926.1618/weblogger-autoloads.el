;;; weblogger-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "weblogger" "weblogger.el" (0 0 0 0))
;;; Generated autoloads from weblogger.el

(autoload 'weblogger-select-configuration "weblogger" "\
Select a previously saved configuration.

\(fn &optional CONFIG)" t nil)

(autoload 'weblogger-setup-weblog "weblogger" "\
Create a profile for a weblog." t nil)

(autoload 'weblogger-start-entry "weblogger" "\
Start creating a weblog entry in the *weblogger-entry* buffer.
With a prefix, it will check the available weblogs on the server
and prompt for the weblog to post to if multiple ones are
available.

\(fn &optional PROMPT)" t nil)

(autoload 'weblogger-fetch-entries "weblogger" "\
Sync the entry ring with what is on the weblog server." t nil)

(register-definition-prefixes "weblogger" '("*weblogger-entry*" "menu-bar-weblogger-menu" "weblogger-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; weblogger-autoloads.el ends here

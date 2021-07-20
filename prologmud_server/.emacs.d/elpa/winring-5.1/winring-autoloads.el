;;; winring-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winring" "winring.el" (0 0 0 0))
;;; Generated autoloads from winring.el

(autoload 'winring-new-configuration "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] ARG, prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-duplicate-configuration "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] ARG prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-next-configuration "winring" "\
Switch to the next window configuration for this frame." t nil)

(autoload 'winring-prev-configuration "winring" "\
Switch to the previous window configuration for this frame." t nil)

(autoload 'winring-jump-to-configuration "winring" "\
Go to the named window configuration." t nil)

(autoload 'winring-delete-configuration "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] ARG prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(autoload 'winring-rename-configuration "winring" "\
Rename the current configuration to NAME." t nil)

(register-definition-prefixes "winring" '("winring-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winring-autoloads.el ends here

;;; persp-mode-projectile-bridge-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "persp-mode-projectile-bridge" "persp-mode-projectile-bridge.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from persp-mode-projectile-bridge.el

(defvar persp-mode-projectile-bridge-mode nil "\
Non-nil if Persp-Mode-Projectile-Bridge mode is enabled.
See the `persp-mode-projectile-bridge-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode-projectile-bridge-mode'.")

(custom-autoload 'persp-mode-projectile-bridge-mode "persp-mode-projectile-bridge" nil)

(autoload 'persp-mode-projectile-bridge-mode "persp-mode-projectile-bridge" "\
`persp-mode' and `projectile-mode' integration.
Creates perspectives for projectile projects.

This is a minor mode.  If called interactively, toggle the
`Persp-Mode-Projectile-Bridge mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'persp-mode-projectile-bridge-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "persp-mode-projectile-bridge" '("persp-mode-projectile-bridge-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; persp-mode-projectile-bridge-autoloads.el ends here

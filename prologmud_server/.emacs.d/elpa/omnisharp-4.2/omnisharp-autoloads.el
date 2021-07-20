;;; omnisharp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "omnisharp" "omnisharp.el" (0 0 0 0))
;;; Generated autoloads from omnisharp.el

(autoload 'omnisharp-mode "omnisharp" "\
Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend.

This is a minor mode.  If called interactively, toggle the
`omnisharp mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `omnisharp-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'omnisharp-start-omnisharp-server "omnisharp" "\
Starts an OmniSharp server for a given path to a project or solution file

\(fn &optional NO-AUTODETECT)" t nil)

(autoload 'omnisharp-stop-server "omnisharp" "\
Stops Omnisharp server if running." t nil)

(autoload 'omnisharp-reload-solution "omnisharp" "\
Restarts omnisharp server on solution last loaded" t nil)

(autoload 'omnisharp-check-alive-status "omnisharp" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified." t nil)

(autoload 'omnisharp-check-ready-status "omnisharp" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution." t nil)

(autoload 'omnisharp-install-server "omnisharp" "\
Installs OmniSharp server locally into ~/.emacs/cache/omnisharp/server/$(version)

\(fn REINSTALL)" t nil)

(autoload 'company-omnisharp "omnisharp" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "omnisharp" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-auto-complete-actions" "omnisharp-auto-complete-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-auto-complete-actions.el

(register-definition-prefixes "omnisharp-auto-complete-actions" '("ac-" "omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-current-symbol-actions" "omnisharp-current-symbol-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-current-symbol-actions.el

(register-definition-prefixes "omnisharp-current-symbol-actions" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-format-actions" "omnisharp-format-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-format-actions.el

(register-definition-prefixes "omnisharp-format-actions" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-http-utils" "omnisharp-http-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-http-utils.el

(register-definition-prefixes "omnisharp-http-utils" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-navigation-actions" "omnisharp-navigation-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-navigation-actions.el

(register-definition-prefixes "omnisharp-navigation-actions" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-server-actions" "omnisharp-server-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-server-actions.el

(register-definition-prefixes "omnisharp-server-actions" '("omnisharp--"))

;;;***

;;;### (autoloads nil "omnisharp-server-installation" "omnisharp-server-installation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-server-installation.el

(register-definition-prefixes "omnisharp-server-installation" '("omnisharp--"))

;;;***

;;;### (autoloads nil "omnisharp-server-management" "omnisharp-server-management.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-server-management.el

(register-definition-prefixes "omnisharp-server-management" '("comment" "make-omnisharp--server-info" "omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-settings" "omnisharp-settings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-settings.el

(register-definition-prefixes "omnisharp-settings" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-solution-actions" "omnisharp-solution-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omnisharp-solution-actions.el

(register-definition-prefixes "omnisharp-solution-actions" '("omnisharp-"))

;;;***

;;;### (autoloads nil "omnisharp-utils" "omnisharp-utils.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from omnisharp-utils.el

(register-definition-prefixes "omnisharp-utils" '("omnisharp--"))

;;;***

;;;### (autoloads nil nil ("omnisharp-helm-integration.el" "omnisharp-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; omnisharp-autoloads.el ends here

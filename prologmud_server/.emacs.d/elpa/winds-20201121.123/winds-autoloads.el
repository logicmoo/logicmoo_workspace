;;; winds-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winds" "winds.el" (0 0 0 0))
;;; Generated autoloads from winds.el

(defvar winds-mode nil "\
Non-nil if Winds mode is enabled.
See the `winds-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winds-mode'.")

(custom-autoload 'winds-mode "winds" nil)

(autoload 'winds-mode "winds" "\
Toggle winds.el window config and workspace manager.

This is a minor mode.  If called interactively, toggle the `Winds
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'winds-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar winds-history-mode nil "\
Non-nil if Winds-History mode is enabled.
See the `winds-history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winds-history-mode'.")

(custom-autoload 'winds-history-mode "winds" nil)

(autoload 'winds-history-mode "winds" "\
Toggle winds.el history mode.

This is a minor mode.  If called interactively, toggle the
`Winds-History mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'winds-history-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Saves history of window config changes within a window-config slot in
winds.el. Use this global minor mode to replace `winner-mode` and have a
more relevant history ring saved.

\(fn &optional ARG)" t nil)

(autoload 'winds-save-cfg "winds" "\
Save current window configuration into workspace WS, config CFG.

WS  defaults to current workspace
CFG defaults to current cfg if WS is current or to `winds-default-cfg'

Call interactively to be prompted for a workspace and window config to save to.

Call interactively with a prefix argument to save to the current window config slot
 in the current workspace.

\(fn &key ((:ws WSID) (winds-get-cur-ws)) ((:cfg CFGID) nil))" t nil)

(autoload 'winds-goto "winds" "\
Switch to another workspace and/or window config slot.

Call interactively to be prompted for a workspace and window config to swtich to.

Call interactively with a prefix argument to go to the last selected
window config slot in the current workspace.

\(fn &key ((:ws WSID) (winds-get-cur-ws)) ((:cfg CFGID) nil) (DO-SAVE t))" t nil)

(autoload 'winds-last "winds" "\
Go to the previously selected workspace slot." t nil)

(autoload 'winds-cfg-last "winds" "\
Go to the previously selected window config slot in the current workspace." t nil)

(autoload 'winds-pos-last "winds" "\
Go to the previously selected window config slot and workspace." t nil)

(autoload 'winds-next "winds" "\
Go to next workspace slot." t nil)

(autoload 'winds-prev "winds" "\
Go to previous workspace slot." t nil)

(autoload 'winds-cfg-next "winds" "\
Go to next window config slot." t nil)

(autoload 'winds-cfg-prev "winds" "\
Go to previous window config slot." t nil)

(autoload 'winds-close "winds" "\
Close workspace slot WSID.

Close workspace slot WSID and switch to nearest slot or `winds-default-ws'
 if none open.  If interactive, you are prompted for an id or blank to close
 current ws.

\(fn WSID)" t nil)

(autoload 'winds-cfg-close "winds" "\
Close window config slot CFGID.

Close window config slot CFGID and switch to nearest slot or `winds-default-cfg'
 if none open.  If interactive, you are prompted for an id or blank to close
 current cfg

\(fn CFGID)" t nil)

(autoload 'winds-history-undo "winds" "\
Go back or forward in the window config history for the current slot.

Go forward in the history if REDO is non-nil

\(fn &optional REDO)" t nil)

(autoload 'winds-history-redo "winds" "\
Go forward in the window config history for the current slot" t nil)

(register-definition-prefixes "winds" '("winds-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winds-autoloads.el ends here

;;; corfu-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu" "corfu.el" (0 0 0 0))
;;; Generated autoloads from corfu.el

(autoload 'corfu-mode "corfu" "\
Completion Overlay Region FUnction

This is a minor mode.  If called interactively, toggle the `Corfu
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `corfu-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'corfu-global-mode 'globalized-minor-mode t)

(defvar corfu-global-mode nil "\
Non-nil if Corfu-Global mode is enabled.
See the `corfu-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-global-mode'.")

(custom-autoload 'corfu-global-mode "corfu" nil)

(autoload 'corfu-global-mode "corfu" "\
Toggle Corfu mode in all buffers.
With prefix ARG, enable Corfu-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Corfu mode is enabled in all buffers where `corfu--on' would do it.

See `corfu-mode' for more information on Corfu mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "corfu" '("corfu-"))

;;;***

;;;### (autoloads nil nil ("corfu-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corfu-autoloads.el ends here

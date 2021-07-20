;;; face-explorer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "face-explorer" "face-explorer.el" (0 0 0 0))
;;; Generated autoloads from face-explorer.el

(autoload 'face-explorer-tooltip-mode "face-explorer" "\
Minor mode to show tooltips for face-related text properties.

This is a minor mode.  If called interactively, toggle the
`Face-Explorer-Tooltip mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `face-explorer-tooltip-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'face-explorer-tooltip-global-mode 'globalized-minor-mode t)

(defvar face-explorer-tooltip-global-mode nil "\
Non-nil if Face-Explorer-Tooltip-Global mode is enabled.
See the `face-explorer-tooltip-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `face-explorer-tooltip-global-mode'.")

(custom-autoload 'face-explorer-tooltip-global-mode "face-explorer" nil)

(autoload 'face-explorer-tooltip-global-mode "face-explorer" "\
Toggle Face-Explorer-Tooltip mode in all buffers.
With prefix ARG, enable Face-Explorer-Tooltip-Global mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Face-Explorer-Tooltip mode is enabled in all buffers where `(lambda
nil (face-explorer-tooltip-mode 1))' would do it.

See `face-explorer-tooltip-mode' for more information on
Face-Explorer-Tooltip mode.

\(fn &optional ARG)" t nil)

(autoload 'face-explorer-list-faces "face-explorer" "\
List all faces, with samples for the selected frame and fictitious display.

Information is given on how a face is defined, for example if the
currently active themes affect the face.

The sample text comes from the variable `face-explorer-sample-text'." t nil)

(autoload 'face-explorer-describe-face-at-point "face-explorer" "\
Invoke `face-explorer-describe' for face at point." t nil)

(autoload 'face-explorer-describe-face "face-explorer" "\
Print information about FACE, including the origin of all attributes.

\(fn FACE)" t nil)

(autoload 'face-explorer-describe-face-prop "face-explorer" "\
Print information about FACE-TEXT-PROPERTY.

When called interactively, print information about the `face'
text property at the point.

\(fn FACE-TEXT-PROPERTY)" t nil)

(autoload 'face-explorer-list-display-features "face-explorer" "\
Show which features the current display supports." t nil)

(autoload 'face-explorer-list-face-prop-examples "face-explorer" "\
List sample text with face spec in various variants." t nil)

(autoload 'face-explorer-list-overlay-examples "face-explorer" "\
List a number of examples with `face' overlays and text properties." t nil)

(autoload 'face-explorer-simulate-display-mode "face-explorer" "\
Minor mode to simulate another display type.

This is a minor mode.  If called interactively, toggle the
`Face-Explorer-Simulate-Display mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `face-explorer-simulate-display-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following keys are used:
\\{face-explorer-simulate-display-mode-map}

\(fn &optional ARG)" t nil)

(put 'face-explorer-simulate-display-global-mode 'globalized-minor-mode t)

(defvar face-explorer-simulate-display-global-mode nil "\
Non-nil if Face-Explorer-Simulate-Display-Global mode is enabled.
See the `face-explorer-simulate-display-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `face-explorer-simulate-display-global-mode'.")

(custom-autoload 'face-explorer-simulate-display-global-mode "face-explorer" nil)

(autoload 'face-explorer-simulate-display-global-mode "face-explorer" "\
Toggle Face-Explorer-Simulate-Display mode in all buffers.
With prefix ARG, enable Face-Explorer-Simulate-Display-Global mode
if ARG is positive; otherwise, disable it.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Face-Explorer-Simulate-Display mode is enabled in all buffers where
`(lambda nil (unless (derived-mode-p 'face-explorer-common-mode)
\(face-explorer-simulate-display-mode 1)))' would do it.

See `face-explorer-simulate-display-mode' for more information on
Face-Explorer-Simulate-Display mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "face-explorer" '("face-explorer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; face-explorer-autoloads.el ends here

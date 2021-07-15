;;; quarter-plane-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "quarter-plane" "quarter-plane.el" (0 0 0 0))
;;; Generated autoloads from quarter-plane.el

(autoload 'quarter-plane-mode "quarter-plane" "\
Toggle Quarter-Plane mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.

Use point movement commands that act as if the text extended
infinitely down and to the right, inserting spaces as necessary.
Excess whitespace is trimmed when saving or exiting Quarter-Plane mode.

Because it works by inserting spaces, Quarter-Plane mode won't work in
read-only buffers.

\\{quarter-plane-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-quarter-plane-mode 'globalized-minor-mode t)

(defvar global-quarter-plane-mode nil "\
Non-nil if Global Quarter-Plane mode is enabled.
See the `global-quarter-plane-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-quarter-plane-mode'.")

(custom-autoload 'global-quarter-plane-mode "quarter-plane" nil)

(autoload 'global-quarter-plane-mode "quarter-plane" "\
Toggle Quarter-Plane mode in all buffers.
With prefix ARG, enable Global Quarter-Plane mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Quarter-Plane mode is enabled in all buffers where
`quarter-plane-mode' would do it.

See `quarter-plane-mode' for more information on Quarter-Plane mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "quarter-plane" '("quarter-plane-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; quarter-plane-autoloads.el ends here

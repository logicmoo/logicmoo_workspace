;;; on-screen-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "on-screen" "on-screen.el" (0 0 0 0))
;;; Generated autoloads from on-screen.el

(autoload 'on-screen-mode "on-screen" "\
Buffer local minor mode guiding your eyes while scrolling.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
Type M-x customize-group on-screen RET for configuration.

\(fn &optional ARG)" t nil)

(defvar on-screen-global-mode nil "\
Non-nil if On-Screen-Global mode is enabled.
See the `on-screen-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `on-screen-global-mode'.")

(custom-autoload 'on-screen-global-mode "on-screen" nil)

(autoload 'on-screen-global-mode "on-screen" "\
Global minor mode guiding your eyes while scrolling.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

You can make use of `on-screen-inhibit-highlighting' to prevent
highlighting on a per-buffer basis.

Type M-x customize-group on-screen RET for configuration.

\(fn &optional ARG)" t nil)

(defalias 'global-on-screen-mode 'on-screen-global-mode)

(register-definition-prefixes "on-screen" '("on-screen-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; on-screen-autoloads.el ends here

;;; frame-tabs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frame-tabs" "frame-tabs.el" (0 0 0 0))
;;; Generated autoloads from frame-tabs.el

(defvar frame-tabs-mode nil "\
Non-nil if Frame-Tabs mode is enabled.
See the `frame-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `frame-tabs-mode'.")

(custom-autoload 'frame-tabs-mode "frame-tabs" nil)

(autoload 'frame-tabs-mode "frame-tabs" "\
Toggle display of a buffer tabs side window on each frame.
With a prefix argument ARG, enable this mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When this mode is enabled, every normal frame is equipped with a
side window showing tabs for all buffers that appeared on that
frame.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frame-tabs" '("frame-tabs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frame-tabs-autoloads.el ends here

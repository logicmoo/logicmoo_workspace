;;; other-frame-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "other-frame-window" "other-frame-window.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from other-frame-window.el

(defvar other-frame-window-mode nil "\
Non-nil if Other-Frame-Window mode is enabled.
See the `other-frame-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `other-frame-window-mode'.")

(custom-autoload 'other-frame-window-mode "other-frame-window" nil)

(autoload 'other-frame-window-mode "other-frame-window" "\
Minor mode for other frame/window buffer placement.
Enable mode if ARG is positive.

\\[ofw-other-window] <command> causes a buffer displayed by <command>
to appear in another window in the same frame; a window
is created if necessary.

\\[ofw-other-frame] <command> causes a buffer displayed by <command>
to appear in another frame; a frame is created if necessary.

\\[ofw-move-to-other-window] moves the current buffer to another
window in the same frame.

\\[ofw-move-to-other-frame] moves the current buffer to another
frame.

In addition, \\[ofw-other-window] and \\[ofw-other-frame] can be followed by these keys:

0 - deletes the current window/frame

1 - deletes the other windows/frames.

2 - shows another view of the current buffer in a new
    window/frame.

a - creates a commit log entry for the current defun in
    another window/frame.

b - switches to another buffer in another window/frame.

d - start dired in another window/frame.

f - find-file in another window/frame.

m - compose mail in another window/frame.

o - select another window/frame.

r - find-file-read-only in another window/frame.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "other-frame-window" '("other-frame-window-mode-map" "ofw-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; other-frame-window-autoloads.el ends here

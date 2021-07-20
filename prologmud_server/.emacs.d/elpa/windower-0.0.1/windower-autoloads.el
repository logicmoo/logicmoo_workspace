;;; windower-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "windower" "windower.el" (0 0 0 0))
;;; Generated autoloads from windower.el

(autoload 'windower-move-border "windower" "\
Move current window's border towards DIRECTION.
DIRECTION is one of 'left, 'right, 'above or 'below.

\(fn &optional DISTANCE DIRECTION)" t nil)

(autoload 'windower-move-border-left "windower" "\
Move window border to the left.

\(fn DISTANCE)" t nil)

(autoload 'windower-move-border-right "windower" "\
Move window border to the right.

\(fn DISTANCE)" t nil)

(autoload 'windower-move-border-above "windower" "\
Move window border upward.

\(fn DISTANCE)" t nil)

(autoload 'windower-move-border-below "windower" "\
Move window border downward.

\(fn DISTANCE)" t nil)

(autoload 'windower-swap "windower" "\
If 2 windows are up, swap them.
Else if W1 is a window, swap it with current window.
If W2 is a window too, swap both.

\(fn &optional W1 W2)" t nil)

(autoload 'windower-swap-left "windower" "\
Swap current window with the window to the left." t nil)

(autoload 'windower-swap-below "windower" "\
Swap current window with the window below." t nil)

(autoload 'windower-swap-above "windower" "\
Swap current window with the window above." t nil)

(autoload 'windower-swap-right "windower" "\
Swap current window with the window to the right." t nil)

(autoload 'windower-switch-to-last-buffer "windower" "\
Switch to last open buffer in current window." t nil)

(autoload 'windower-toggle-single "windower" "\
Un-maximize current window.
If multiple windows are active, save window configuration and
delete other windows.  If only one window is active and a window
configuration was previously save, restore that configuration." t nil)

(autoload 'windower-toggle-split "windower" "\
Switch between vertical and horizontal split.
It only works for frames with exactly two windows." t nil)

(register-definition-prefixes "windower" '("windower-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; windower-autoloads.el ends here

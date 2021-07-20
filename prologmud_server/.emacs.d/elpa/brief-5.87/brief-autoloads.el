;;; brief-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "brief" "brief.el" (0 0 0 0))
;;; Generated autoloads from brief.el

(defvar brief-mode nil "\
Track status of Brief emulation mode.
A value of nil means Brief mode is not enabled.  A value of t
indicates Brief mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `brief-mode'.")

(custom-autoload 'brief-mode "brief" nil)

(autoload 'brief-mode "brief" "\
Enable/Disable/Toggle Brief emulation mode.
With a prefix argument ARG, enable Brief mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.  If called interactively without ARG,
toggle brief-mode.

\(fn &optional ARG)" t nil)

(autoload 'brief-easy-start "brief" "\
Emulate Brief by changing less favored Emacs settings for programmers.
Before enabling brief mode this sets the following:
 1) No line wrapping by setting `truncate-lines' 't.
 2) No jumppy scrolling in both vertical and horizontal directions.
 3) Smaller borders.
This function is used by the quick launcher 'b' script." t nil)

(register-definition-prefixes "brief" '("brief-" "smooth-scroll-mode-activate"))

;;;***

;;;### (autoloads nil nil ("brief-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; brief-autoloads.el ends here

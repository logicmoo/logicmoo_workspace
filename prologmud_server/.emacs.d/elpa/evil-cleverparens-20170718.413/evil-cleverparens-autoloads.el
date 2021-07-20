;;; evil-cleverparens-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-cleverparens" "evil-cleverparens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-cleverparens.el

(autoload 'evil-cp-set-movement-keys "evil-cleverparens" "\
Sets the movement keys in
`evil-cleverparens-regular-movement-keys' or
`evil-cp-swapped-movement-keys' based on the value of
`evil-cleverparens-swap-move-by-word-and-symbol'." t nil)

(autoload 'evil-cp-set-additional-movement-keys "evil-cleverparens" "\
Sets the movement keys is `evil-cp-additional-movement-keys'
for normal, visual and operator states if
`evil-cleverparens-use-additional-movement-keys' is true." t nil)

(autoload 'evil-cp-set-additional-bindings "evil-cleverparens" "\
Sets the movement keys is `evil-cp-additional-bindings' for
normal-state if `evil-cleverparens-use-additional-bindings' is
true." t nil)

(autoload 'evil-cleverparens-mode "evil-cleverparens" "\
Minor mode for setting up evil with smartparens and paredit
for an advanced modal structural editing experience.

This is a minor mode.  If called interactively, toggle the
`Evil-Cleverparens mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-cleverparens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-cleverparens" '("evil-c" "forward-evil-cp-"))

;;;***

;;;### (autoloads nil "evil-cleverparens-text-objects" "evil-cleverparens-text-objects.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-cleverparens-text-objects.el
 (autoload 'evil-cp-a-form "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-form "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-a-comment "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-comment "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-a-defun "evil-cleverparens-text-objects" nil t)
 (autoload 'evil-cp-inner-defun "evil-cleverparens-text-objects" nil t)

;;;***

;;;### (autoloads nil "evil-cleverparens-util" "evil-cleverparens-util.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-cleverparens-util.el

(register-definition-prefixes "evil-cleverparens-util" '("evil-cp-"))

;;;***

;;;### (autoloads nil nil ("evil-cleverparens-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-cleverparens-autoloads.el ends here

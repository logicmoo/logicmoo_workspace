;;; org-variable-pitch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-variable-pitch" "org-variable-pitch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-variable-pitch.el

(autoload 'org-variable-pitch-minor-mode "org-variable-pitch" "\
Set up the buffer to be partially in variable pitch.
Keeps some elements in fixed pitch in order to keep layout.

This is a minor mode.  If called interactively, toggle the
`Org-Variable-Pitch minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-variable-pitch-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-variable-pitch-setup "org-variable-pitch" "\
Set up ‘org-variable-pitch-minor-mode’.

This function is a helper to set up OVP.  It syncs
‘org-variable-pitch-fixed-face’ with ‘default’ face, and adds a
hook to ‘org-mode-hook’.  Ideally, you’d want to run this
function somewhere after you set up ‘default’ face.

A nice place to call this function is from within
‘after-init-hook’:

    (add-hook 'after-init-hook #'org-variable-pitch-setup)

Alternatively, you might want to manually set up the attributes
of ‘org-variable-pitch-fixed-face’, in which case you should
calling avoid this function, add ‘org-variable-pitch-minor-mode’
to ‘org-mode-hook’ manually, and set up the face however you
please." t nil)

(register-definition-prefixes "org-variable-pitch" '("org-variable-pitch-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-variable-pitch-autoloads.el ends here

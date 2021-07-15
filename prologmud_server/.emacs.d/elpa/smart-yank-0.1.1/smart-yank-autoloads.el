;;; smart-yank-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smart-yank" "smart-yank.el" (0 0 0 0))
;;; Generated autoloads from smart-yank.el

(defvar smart-yank-mode nil "\
Non-nil if Smart-Yank mode is enabled.
See the `smart-yank-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smart-yank-mode'.")

(custom-autoload 'smart-yank-mode "smart-yank" nil)

(autoload 'smart-yank-mode "smart-yank" "\
Alter the behavior of yank commands in several ways.

Turning on this mode has the following effects:

 - Makes any command except yank commands reset the
  `kill-ring-yank-pointer', instead of only killing commands.

 - Remaps `yank-pop' to `smart-yank-yank-pop'.

 - When yanking an older element from the `kill-ring' with
   \\[smart-yank-yank-pop] (and not replacing it with a subsequent \\[smart-yank-yank-pop]), the
   element is automatically \"moved to the first position\" of
   the `kill-ring' so that `yank' invoked later will again yank
   this element.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-yank" '("smart-yank-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-yank-autoloads.el ends here

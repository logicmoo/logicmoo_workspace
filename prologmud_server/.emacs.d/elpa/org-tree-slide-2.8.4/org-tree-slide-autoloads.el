;;; org-tree-slide-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-tree-slide" "org-tree-slide.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-tree-slide.el

(autoload 'org-tree-slide-mode "org-tree-slide" "\
A presentation tool for org-mode.

This is a minor mode.  If called interactively, toggle the
`Org-Tree-Slide mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-tree-slide-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Usage:
  - Set minimal recommendation settings in .emacs
    (global-set-key (kbd \"<f8>\") 'org-tree-slide-mode)
    (global-set-key (kbd \"S-<f8>\") 'org-tree-slide-skip-done-toggle)
  - Open an org file
  - Type <f8> to start org-tree-slide-mode
  - Type <left>/<right> to move between trees
  - To exit this minor mode, just type <f8> again.

Profiles:

  - [ Simple ]
 => M-x `org-tree-slide-simple-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. No slide number display in mode line
    5. Display every type of tree

  - [ Presentation ]
 => M-x `org-tree-slide-presentation-profile'

    1. Display header
    2. Enable slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. Display slide number in mode line
    5. Display every type of tree

  - [ TODO Pursuit with narrowing ]
 => M-x `org-tree-slide-narrowing-control-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will keep the same position when exit
    4. Display slide number in mode line
    5. Display TODO trees only

\(fn &optional ARG)" t nil)

(autoload 'org-tree-slide-play-with-timer "org-tree-slide" "\
Start slideshow with setting a count down timer." t nil)

(autoload 'org-tree-slide-without-init-play "org-tree-slide" "\
Start slideshow without the init play. Just enter org-tree-slide-mode." t nil)

(autoload 'org-tree-slide-content "org-tree-slide" "\
Change the display for viewing content of the org file during
   the slide view mode is active." t nil)

(autoload 'org-tree-slide-simple-profile "org-tree-slide" "\
Set variables for simple use.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => nil
  `org-tree-slide-skip-done'         => nil
  `org-tree-slide-skip-comments'     => t
" t nil)

(autoload 'org-tree-slide-presentation-profile "org-tree-slide" "\
Set variables for presentation use.
  `org-tree-slide-header'            => t
  `org-tree-slide-slide-in-effect'   => t
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => 'outside
  `org-tree-slide-skip-done'         => nil
  `org-tree-slide-skip-comments'     => t
" t nil)

(autoload 'org-tree-slide-narrowing-control-profile "org-tree-slide" "\
Set variables for TODO pursuit with narrowing.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => nil
  `org-tree-slide-modeline-display'  => 'lighter
  `org-tree-slide-skip-done'         => t
  `org-tree-slide-skip-comments'     => t
" t nil)

(autoload 'org-tree-slide-display-header-toggle "org-tree-slide" "\
Toggle displaying the slide header" t nil)

(autoload 'org-tree-slide-slide-in-effect-toggle "org-tree-slide" "\
Toggle using slide-in effect" t nil)

(autoload 'org-tree-slide-heading-emphasis-toggle "org-tree-slide" "\
Toggle applying emphasis to heading" t nil)

(autoload 'org-tree-slide-skip-done-toggle "org-tree-slide" "\
Toggle show TODO item only or not" t nil)

(autoload 'org-tree-slide-skip-comments-toggle "org-tree-slide" "\
Toggle show COMMENT item or not" t nil)

(register-definition-prefixes "org-tree-slide" '("org-tree-slide"))

;;;***

;;;### (autoloads nil nil ("org-tree-slide-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-tree-slide-autoloads.el ends here

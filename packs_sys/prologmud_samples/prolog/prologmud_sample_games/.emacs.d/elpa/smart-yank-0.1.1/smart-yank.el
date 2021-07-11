;;; smart-yank.el --- A different approach of yank pointer handling  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 14 May 2016
;; Keywords: convenience
;; Compatibility: GNU Emacs 24
;; Version: 0.1.1
;; Package-Requires: ((emacs "24"))


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Introduction
;; ============
;;
;; This library implements the global minor mode `smart-yank-mode'
;; that changes the way Emacs handles the `kill-ring-yank-pointer' in
;; a way that some people prefer over the default behavior.
;;
;; Normally, only a kill command resets the yank pointer.  With
;; `smart-yank-mode' enabled, any command except yank commands resets
;; it.
;;
;; In addition, when yanking any "older" element from the kill-ring
;; with yank-pop (and not replacing it with a subsequent yank-pop), it
;; is automatically moved to the "first position" so `yank' invoked
;; later will yank this element again.
;;
;; Finally, `yank-pop' (normally bound to M-y) is replaced with
;; `smart-yank-yank-pop' that is a bit more sophisticated:
;;
;; - When _not_ called after a `yank', instead of raising an error
;;   like `yank-pop', yank the next-to-the-last kill.
;;
;; - Hit M-y twice in fast succession (delay < 0.2 secs by default)
;;   when you got lost.  This will remove the yanked text.  If you
;;   bind a command to `smart-yank-browse-kill-ring-command', this
;;   command will be called too (typically something like
;;   `browse-kill-ring').
;;
;;
;; Example: you want to manually replace some words in some buffer
;; with a new word "foo".  With `smart-yank-mode' enabled, you can do
;; it like this:
;;
;;   1. Put "foo" into the kill ring.
;;   2. Move to the next word to be replaced.
;;   3. M-d M-y
;;   4. Back to 2, iterate.
;;
;;
;; Setup
;; =====
;;
;; Just enable `smart-yank-mode' and you are done.



;;; Code:

;;;; Configuration stuff

(defgroup smart-yank nil
  "A different approach of yank pointer handling."
  :group 'killing)

(defcustom smart-yank-yank-pop-multikey-delay .2
  "Max delay between two \\[smart-yank-yank-pop] invocations revealing special behavior.
See `smart-yank-yank-pop' for details."
  :type 'number)

(defcustom smart-yank-browse-kill-ring-command nil
  "Command to invoke when hitting \\[smart-yank-yank-pop] twice (fast)."
  :type '(choice (const :tag "None" nil)
                 (const browse-kill-ring)
                 (const helm-show-kill-ring)
                 (function :tag "Other Function")))

(defvar smart-yank-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap yank-pop] #'smart-yank-yank-pop)
    map)
  "Map used by `smart-yank-mode'.")


;;;; Internals

(defun smart-yank--stopwatch ()
  "Return a fresh stopwatch.
This is a function accepting zero arguments that upon each call
will return the time difference from its last call in seconds.
When called the first time it will return nil."
  (let ((last-invocation nil))
    (lambda ()
      (prog1 (and last-invocation
                   (time-to-seconds (time-subtract (current-time) last-invocation)))
         (setq last-invocation (current-time))))))

(defun smart-yank-reset-yank-pointer ()
  (unless (eq last-command #'yank)
    (setq kill-ring-yank-pointer kill-ring)))

(defun smart-yank--before-ad (&rest _args)
  "Before advice function for `yank'.

Reset `kill-ring-yank-pointer'.  For yank-pop, move the really
yanked text \"to the beginning\" of the kill ring."
  (unless (eq kill-ring kill-ring-yank-pointer)
    (let ((last-yank (car kill-ring-yank-pointer)))
      (when last-yank
        (setq kill-ring (cons last-yank (delete last-yank kill-ring)))
        (smart-yank-reset-yank-pointer)))))

(defalias 'smart-yank-yank-pop
  (let ((r (smart-yank--stopwatch)))
    (lambda (&optional arg)
      "\"smart-yank\"'s private version of `yank-pop'.

When called directly after a `yank' command (including itself),
call `yank-pop'.

If its key was hit two times in fast succession - i.e. with a
delay less than `smart-yank-yank-pop-multikey-delay' - delete any
yanked text; in addition call
`smart-yank-browse-kill-ring-command' when set.

When not called after a yank, yank the next-to-the-last
`kill-ring' entry; with prefix arg, call the
`smart-yank-browse-kill-ring-command'."
      (interactive "P")
      (let ((diff (funcall r)))
        (cond
         ((not (eq last-command 'yank)) (if arg (call-interactively smart-yank-browse-kill-ring-command)
                                          (rotate-yank-pointer 1)
                                          (yank)))
         ((or (not diff)
              (> diff smart-yank-yank-pop-multikey-delay))
          (call-interactively #'yank-pop))
         (t                             (funcall (or yank-undo-function #'delete-region)
                                                 (region-beginning) (region-end))
                                        (when smart-yank-browse-kill-ring-command
                                          (call-interactively smart-yank-browse-kill-ring-command))))))))

(declare-function smart-yank-yank-pop 'smart-yank)


;;;; User stuff

;;;###autoload
(define-minor-mode smart-yank-mode
  "Alter the behavior of yank commands in several ways.

Turning on this mode has the following effects:

 - Makes any command except yank commands reset the
  `kill-ring-yank-pointer', instead of only killing commands.

 - Remaps `yank-pop' to `smart-yank-yank-pop'.

 - When yanking an older element from the `kill-ring' with
   \\[smart-yank-yank-pop] (and not replacing it with a subsequent \\[smart-yank-yank-pop]), the
   element is automatically \"moved to the first position\" of
   the `kill-ring' so that `yank' invoked later will again yank
   this element."
  :global t
  (if smart-yank-mode
      (advice-add 'yank :before #'smart-yank--before-ad)
    (advice-remove 'yank #'smart-yank--before-ad)))

;;;; ChangeLog:

;; 2016-05-21  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	smart-yank version 0.1.1
;; 
;; 2016-05-17  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Make smart-yank-yank-pop a top-level def (use defalias)
;; 
;; 	Thanks Stefan.
;; 
;; 2016-05-17  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Give smart-yank's mode map the canonical name
;; 
;; 	Thanks Stefan.
;; 
;; 2016-05-17  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Add new package "smart-yank"
;; 



(provide 'smart-yank)

;;; smart-yank.el ends here

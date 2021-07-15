;;; other-frame-window.el --- Minor mode to enable global prefix keys for other frame/window buffer placement  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2015, 2017, 2018  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: frame window
;; Version: 1.0.6
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; other-frame-window provides prefix key sequences to control where a
;; new buffer is created by a subsequent command. With no prefix, the
;; buffer is created where the command decides (nominally the currently
;; selected window). Prefix C-x 7 causes the buffer to appear in
;; another window in the same frame; a window is created if necessary.
;; Prefix C-x 9 causes the buffer to appear in another frame; a frame
;; is created if necessary.
;;
;; Some commands display new buffers in other than the currently
;; selected window, which defeats the purpose of ‘other-frame-window’ in
;; the absense of a prefix. To override that, customize
;; ‘display-buffer-alist’ for those commands. For example, to override
;; ‘gud-gdb’:
;;
;; (add-to-list
;;  'display-buffer-alist
;;  (cons "\\*gud"
;; 	 (cons 'display-buffer-same-window nil)))
;; )
;;
;; Other key bindings provided by other-frame-window:
;;
;; C-x W moves the current buffer to another window in the same frame.
;;
;; C-x F moves the current buffer to another frame.
;;
;;
;; In addition, C-x 7 and C-x 9 can be followed by these keys:
;;
;; 0 - deletes the current window.
;;
;; 1 - deletes the other windows/frames.
;;
;; 2 - shows another view of the current buffer in a new
;;     window/frame.
;;
;; a - creates a commit log entry for the current defun in
;;     another window/frame.
;;
;; b - switches to another buffer in another window/frame.
;;
;; d - start dired in another window/frame.
;;
;; f - find-file in another window/frame.
;;
;; m - compose mail in another window/frame.
;;
;; o - select another window/frame.
;;
;; r - find-file-read-only in another window/frame.
;;
;; To extend this list, add key bindings to ‘ofw-transient-map’.
;;

;;;; Usage:
;;
;; Enable the minor mode with:
;;
;; M-x other-frame-window-mode
;;
;; or, in your ~/.emacs:
;;
;; (other-frame-window-mode t)
;;

;;;; Design:
;;
;; This uses C-x 7, 9 prefix because those keys are undefined in core
;; Emacs.  It could eventually switch to 4, 5, since those are
;; currently used for -other-window, -other-frame bindings.
;;
;; (info "(emacs) Pop Up Window") (info "(emacs) Creating Frames")
;;
;; This adds advice to switch-to-buffer; eventually Emacs could
;; reimplement switch-to-buffer to do the same.

;;;; Todo:

;; - Pay attention to bindings added to ctl-x-4-map and ctl-x-5-map
;; - Should `C-x 7 C-h' display the transient map?
;; - `C-x 7 C-h k f' should show `find-file' rather than `self-insert-command'.
;;   This should probably be fixed in set-transient-map.

;;; Code:

(defvar ofw--just-set nil
  "Non-nil if we just set the prefix in the previous command.")

(defvar ofw-transient-map
  (let ((map (make-sparse-keymap)))
    ;; This is basically the union of the default C-x 4 and C-x 5
    ;; keymaps in Emacs-25.
    (define-key map [?\C-f] #'find-file)
    (define-key map [?\C-o] #'display-buffer)
    (define-key map [?.]
      (if (fboundp 'xref-find-definitions) ;Emacs≥25.
          'xref-find-definitions 'find-tag))
    (define-key map [?0] #'ofw-dwim-delete-this)
    (define-key map [?1] #'ofw-dwim-one)
    (define-key map [?2] #'ofw-dwim-open-other)
    (define-key map [?a] #'add-change-log-entry)
    (define-key map [?b] #'switch-to-buffer)
    (define-key map [?c] #'clone-indirect-buffer)
    (define-key map [?d] #'dired)
    (define-key map [?f] #'find-file)
    (define-key map [?m] #'compose-mail)
    (define-key map [?o] #'ofw-dwim-select-other)
    (define-key map [?r] #'find-file-read-only)
    map)
  "Keymap used for one command right after setting the prefix.")

(defun ofw--set-prefix (func)
  "Add ofw prefix function FUNC."
  (ofw-delete-from-overriding)
  (let ((functions (car display-buffer-overriding-action))
	(attrs (cdr display-buffer-overriding-action)))
    (push func functions)
    (setq display-buffer-overriding-action (cons functions attrs))
    ;; C-u C-x 7 foo should pass C-u to foo, not to C-x 7, so
    ;; pass the normal prefix to the next command.
    (if (fboundp 'prefix-command-preserve-state)
        (prefix-command-preserve-state)
      ;; Make sure the next pre-command-hook doesn't immediately set
      ;; display-buffer-overriding-action back to nil.
      (setq ofw--just-set t)
      (setq prefix-arg current-prefix-arg))
    (set-transient-map ofw-transient-map)))

(defun ofw--echo-keystrokes ()
  ;; Sometimes people abuse ‘display-buffer-overriding-action’, and
  ;; that crashes emacs, so be careful.
  (when (and (consp display-buffer-overriding-action)
	     (listp (car display-buffer-overriding-action)))
    (let ((funs (car display-buffer-overriding-action)))
      (cond
       ((memq #'ofw-display-buffer-other-frame funs) "[other-frame]")
       ((memq #'ofw-display-buffer-other-window funs) "[other-window]")))))

(when (boundp 'prefix-command-echo-keystrokes-functions)
  (add-hook 'prefix-command-echo-keystrokes-functions
            #'ofw--echo-keystrokes))

(defun ofw--preserve-state () (setq ofw--just-set t))
(when (boundp 'prefix-command-preserve-state-hook)
  (add-hook 'prefix-command-preserve-state-hook
            #'ofw--preserve-state))

(defun ofw-delete-from-overriding ()
  "Remove ourselves from `display-buffer-overriding-action' action list, if present."
  (let ((functions (car display-buffer-overriding-action))
        (attrs (cdr display-buffer-overriding-action)))
    (setq functions (remq #'ofw-display-buffer-other-frame
                          (remq #'ofw-display-buffer-other-window functions)))
    (setq display-buffer-overriding-action
          (when (or functions attrs) (cons functions attrs)))))

(defun ofw-other-window ()
  "Set `display-buffer-overriding-action' to indicate other window."
  (interactive)
  (ofw--set-prefix #'ofw-display-buffer-other-window))

(defun ofw-other-frame ()
  "Set `display-buffer-overriding-action' to indicate other frame."
  (interactive)
  (ofw--set-prefix #'ofw-display-buffer-other-frame))

(defun ofw-display-buffer-other-window (buffer alist)
  "Show BUFFER in another window in the current frame,
creating new window if needed and allowed.
If successful, return window; else return nil.
Intended for `display-buffer-overriding-action'."
  ;; Reset for next display-buffer call.  Normally, this is taken care
  ;; of by ofw--reset-prefix, but we do it here in case the user does
  ;; two ofw prefixed commands consecutively.
  (ofw-delete-from-overriding)

  ;; We can't use display-buffer-use-some-window here, because
  ;; that unconditionally allows another frame.
  (or (display-buffer-use-some-frame
       buffer
       (append (list (cons 'frame-predicate
                           (lambda (frame) (eq frame (selected-frame))))
		     '(inhibit-same-window . t))
	       alist))
      (display-buffer-pop-up-window buffer alist)))

(defun ofw-display-buffer-other-frame (buffer alist)
  "Show BUFFER in another frame, creating a new frame if needed.
If successful, return window; else return nil.
Intended for `display-buffer-overriding-action'."
  ;; Reset for next display-buffer call.
  (ofw-delete-from-overriding)

  ;; IMPROVEME: prompt for a frame if more than 2
  (or (display-buffer-use-some-frame buffer alist)
      (display-buffer-pop-up-frame buffer alist)))

(defun ofw-switch-to-buffer-advice (orig-fun buffer
                                    &optional norecord force-same-window)
  "Change `switch-to-buffer' to call `pop-to-buffer'.
This allows `switch-to-buffer' to respect `ofw-other-window',
`ofw-other-frame'."
  (if display-buffer-overriding-action
      (pop-to-buffer buffer (list #'display-buffer-same-window) norecord)
    (funcall orig-fun buffer norecord force-same-window)))

(defun ofw--suspend-and-restore (orig-func &rest args)
  "Call ORIG-FUNC without any ofw actions on `display-buffer-overriding-action'."
  (let ((display-buffer-overriding-action display-buffer-overriding-action))
    (ofw-delete-from-overriding)
    (apply orig-func args)))

(defun ofw-move-to-other-window ()
  "Move current buffer to another window in same frame.
Point stays in moved buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-prev-buffer nil 'bury)
    (pop-to-buffer
     buffer
     (cons '(display-buffer-use-some-frame display-buffer-pop-up-window)
	   (list (cons 'frame-predicate (lambda (frame) (eq frame (selected-frame))))
		 '(inhibit-same-window . t)))
     )))

(defun ofw-move-to-other-frame ()
  "Move current buffer to a window in another frame.
Point stays in moved buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-prev-buffer nil 'bury)
    (pop-to-buffer
     buffer
     (cons '(display-buffer-use-some-frame display-buffer-pop-up-frame)
	   '((reusable-frames . visible)))
     )))

(defvar other-frame-window-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x7" #'ofw-other-window)
    (define-key map "\C-x9" #'ofw-other-frame)
    (define-key map "\C-xW" #'ofw-move-to-other-window)
    (define-key map "\C-xF" #'ofw-move-to-other-frame)
    map)
  "Local keymap used for other-frame-window minor mode.")

(defun ofw--reset-prefix ()
  (if ofw--just-set
      (setq ofw--just-set nil)
    (ofw-delete-from-overriding)))

;;;###autoload
(define-minor-mode other-frame-window-mode
  "Minor mode for other frame/window buffer placement.
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
"
  :global t

  (remove-hook 'pre-command-hook #'ofw--reset-prefix)

  (if other-frame-window-mode
      ;; enable
      (progn
        (add-hook 'pre-command-hook #'ofw--reset-prefix)

        ;; We assume Emacs code calls pop-to-buffer when there is a good
	;; reason to put the buffer in another window, so we don't mess
	;; with the default actions, except to allow
	;; display-buffer-reuse-window to use a window in another frame;
	;; add (reusable-frames . visible) to display-buffer-base-action
	;; attributes alist.
	(let ((functions (car display-buffer-base-action))
	      (attrs (cdr display-buffer-base-action)))
	  (push '(reusable-frames . visible) attrs)
	  (setq display-buffer-base-action (cons functions attrs)))

	;; Change switch-to-buffer to use display-buffer
	(advice-add 'switch-to-buffer :around #'ofw-switch-to-buffer-advice)

	;; Completing-read <tab> pops up a buffer listing completions;
	;; that should not respect or consume
	;; ofw-frame-window-prefix-arg.
	(advice-add 'read-from-minibuffer :around #'ofw--suspend-and-restore)
	)

    ;; else disable
    (let ((functions (car display-buffer-base-action))
	  (attrs (cdr display-buffer-base-action)))
      (setq attrs (delq '(reusable-frames . visible) attrs))
      (setq display-buffer-base-action (cons functions attrs)))

    (advice-remove 'switch-to-buffer #'ofw-switch-to-buffer-advice)
    (advice-remove 'read-from-minibuffer #'ofw--suspend-and-restore)
    ))

(unless (fboundp 'display-buffer-use-some-frame)
  ;; in Emacs 25; define here for earlier

(defun display-buffer-use-some-frame (buffer alist)
  "Display BUFFER in an existing frame that meets a predicate
\(by default any frame other than the current frame).  If
successful, return the window used; otherwise return nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid
raising the frame.

If ALIST has a non-nil `frame-predicate' entry, its value is a
function taking one argument (a frame), returning non-nil if the
frame is a candidate; this function replaces the default
predicate.

If ALIST has a non-nil `inhibit-same-window' entry, avoid using
the currently selected window (only useful with a frame-predicate
that allows the selected frame)."
  (let* ((predicate (or (cdr (assq 'frame-predicate alist))
                        (lambda (frame)
                          (and
                           (not (eq frame (selected-frame)))
                           (not (window-dedicated-p
                                 (or
                                  (get-lru-window frame)
                                  (frame-first-window frame)))))
                          )))
         (frame (car (filtered-frame-list predicate)))
         (window (and frame (get-lru-window frame nil (cdr (assq 'inhibit-same-window alist))))))
    (when window
      (prog1
          (window--display-buffer
           buffer window 'frame alist display-buffer-mark-dedicated)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame frame))))
    ))
  )

;; Some of the commands on the transient keymap don't actually *display*
;; in another window/frame but instead do something either at the level
;; of windows or frames.  I call those "ofw-dwim-*".

(defun ofw-dwim--frame-p ()
  "Return non-nil if the prefix is for \"other-frame\" rather than window."
  ;; IMPROVEME: Comparing functions is ugly/hackish!
  (memq #'ofw-display-buffer-other-frame
        (car display-buffer-overriding-action)))

(defun ofw-dwim-delete-this ()
  "Delete this frame or window."
  (interactive)
  (call-interactively
   (if (ofw-dwim--frame-p) #'delete-frame #'kill-buffer-and-window)))

(defun ofw-dwim-one ()
  "Delete all other frames or windows."
  (interactive)
  (call-interactively
   (if (ofw-dwim--frame-p) #'delete-other-frames #'delete-other-windows)))

(defun ofw-dwim-open-other ()
  "Show current buffer in other frame or window."
  (interactive)
  (if (ofw-dwim--frame-p)
      ;; IMPROVEME: This is the old C-x 5 2 behavior, but maybe it should just use
      ;; display-buffer instead!
      (call-interactively #'make-frame-command)
    (display-buffer (current-buffer))))

(defun ofw-dwim-select-other ()
  "Select other frame or window."
  (interactive)
  (call-interactively (if (ofw-dwim--frame-p) #'other-frame #'other-window)))

;;;; ChangeLog:

;; 2018-08-28  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Other-frame-window: bump version to publish changes
;; 
;; 2018-08-20  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Package other-frame-window; improve description
;; 
;; 	* packages/other-frame-window/other-frame-window.el: Improve description 
;; 	in Commentary, for list-packages. Mention display-buffer-alist, not 
;; 	display-buffer-overriding-action.
;; 
;; 2018-07-30  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Minor improvements in other-frame-window
;; 
;; 	* packages/other-frame-window/other-frame-window.el: Bump version.
;; 	(ofw--echo-keystrokes): Check structure of
;; 	'display-buffer-overriding-action' before accessing.
;; 	(other-frame-window-mode): Doc setting display-buffer-overriding-action 
;; 	in init file.
;; 
;; 2017-07-09  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	* packages/other-frame-window/other-frame-window.el: improve description
;; 
;; 2017-05-14  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	* packages/other-frame-window: add text for list-packages long
;; 	description
;; 
;; 2017-05-13  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	* packages/other-frame-window: bump version; improve minor mode doc
;; 	string
;; 
;; 2017-02-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	Clear executable bit for Emacs Lisp files.
;; 
;; 	BTW, this was done w/ shell command: find . -type f -name '*.el' -perm
;; 	/u+x -exec chmod -x '{}' \;
;; 
;; 	* admin/archive-contents.el: ‘chmod -x’.
;; 
;; 	* packages/ada-mode/ada-gnat-compile.el: ‘chmod -x’.
;; 	* packages/ada-mode/ada-gnat-xref.el: Likewise.
;; 	* packages/ada-mode/ada-grammar-wy.el: Likewise.
;; 	* packages/ada-mode/ada-mode.el: Likewise.
;; 	* packages/ada-mode/ada-wisi-opentoken.el: Likewise.
;; 	* packages/ada-mode/ada-wisi.el: Likewise.
;; 	* packages/ada-mode/gpr-grammar-wy.el: Likewise.
;; 	* packages/ada-mode/gpr-query.el: Likewise.
;; 	* packages/other-frame-window/other-frame-window.el: Likewise.
;; 	* packages/register-list/register-list.el: Likewise.
;; 	* packages/windresize/windresize.el: Likewise.
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2015-11-14  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Bump version of other-frame-window.el
;; 
;; 	* packages/other-frame-window/other-frame-window.el:
;; 	(ofw--set-prefix): FIXME => IMPROVEME.
;; 	(ofw-delete-from-overriding): Use remq instead of delq to avoid side 
;; 	effects on let-bound list.
;; 	(ofw-switch-to-buffer-advice): Delete FIXME re emacs 24.3.
;; 	(ofw--suspend-and-restore): Fix FIXME.
;; 	(other-frame-window-mode): Add autoload.
;; 	(other-frame-window-mode): Delete code, FIXME re emacs 24.3.
;; 	(ofw-dwim--frame-p, ofw-dwim-open-other): FIXME => IMPROVEME.
;; 
;; 2015-09-01  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* other-frame-window.el: Use new prefix-command features
;; 
;; 	(ofw--set-prefix): Use prefix-command-preserve-state is available. 
;; 	Remove left-over prefix state.
;; 	(ofw--echo-keystrokes, ofw--preserve-state): New functions.
;; 	(prefix-command-echo-keystrokes-functions)
;; 	(prefix-command-preserve-state-hook): Use them.
;; 	(other-frame-window-mode): Autoload.
;; 
;; 2015-08-16  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Bump version of other-frame-window.el
;; 
;; 2015-08-16  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	Resolve FIXME:s in other-frame-window.el
;; 
;; 	* packages/other-frame-window/other-frame-window.el (ofw-transient-map): 
;; 	Clean up comment.
;; 	(ofw-display-buffer-other-window): Resolve FIXME:.
;; 	(other-frame-window-mode): Resolve FIXME:.
;; 
;; 2015-08-14  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* other-frame-window.el: Disable more carefully; add transient map
;; 
;; 	Only require 24.4.
;; 	(ofw--just-set, ofw-transient-map): New vars.
;; 	(ofw--set-prefix): Use them.  Rename from ofw-add-to-overriding. Update
;; 	callers.  Let the C-u prefix pass through.
;; 	(ofw-delete-from-overriding): Prefer nil to (nil . nil) in 
;; 	display-buffer-overriding-action.
;; 	(ofw-switch-to-buffer-advice): Better preserve the default behavior.
;; 	(ofw--suspend-and-restore): Rename from ofw-temp-window-advice. Take
;; 	advantage of dynamic scoping.
;; 	(other-frame-window-mode-map): Rename from ofw-map.
;; 	(ofw--reset-prefix): New function.
;; 	(other-frame-window-mode): Use it to disable the prefix after the next
;; 	command.  Advise read-from-minibuffer rather than 
;; 	temp-buffer-window-show.
;; 	(ofw-dwim--frame-p): New function.
;; 	(ofw-dwim-delete-this, ofw-dwim-one, ofw-dwim-open-other)
;; 	(ofw-dwim-select-other): New commands.
;; 
;; 2015-08-14  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* other-frame-window.el: Fix layout convention
;; 
;; 2015-08-14  Stephen Leake  <stephen_leake@stephe-leake.org>
;; 
;; 	* packages/other-frame-window/other-frame-window.el: New single-file
;; 	package.
;; 


(provide 'other-frame-window)
;;; other-frame-window.el ends here

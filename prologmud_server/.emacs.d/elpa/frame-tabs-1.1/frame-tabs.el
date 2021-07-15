;;; frame-tabs.el --- show buffer tabs in side window  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2018  Free Software Foundation, Inc.

;; Author: Martin Rudalics <rudalics@gmx.at>
;; Keywords: frames, tabs
;; Version: 1.1

;; frame-tabs.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; frame-tabs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode to display buffer tabs in a side window on each frame.

;; This mode shows, in a side window on each frame, tabs listing the
;; names of all live buffers that have been displayed on that frame.
;; Clicking on a tab with the left mouse button switches to the
;; corresponding buffer in a window.  Clicking on a tab with the right
;; mouse button dismisses the buffer.  See 'frame-tabs-map'.
;; Alternatively, tabs can display an additional 'x' button which
;; permits to dismiss buffers with the left mouse button.  See
;; 'frame-tabs-x' and 'frame-tabs-x-map'.

;; Caveats: Many desirable features are either underdeveloped or
;; simply don't work.  Navigating tabs windows with the keyboard has
;; not been implemented; neither has been displaying alternative items
;; like tabs representing window configurations.  You're welcome to
;; expand existing and/or add new features at your like.

;;; Code:
(defgroup frame-tabs nil
  "Frame tabs."
  :version "26.1"
  :group 'convenience
  :group 'frames)

;; Customizable faces
(defface frame-tabs-buffer-tab
  '((t :inherit variable-pitch
       :box (:line-width 2 :color "grey72")
       :foreground "black"
       :background "grey84"))
  "Basic frame tabs buffer tab face.
This face is used for buffer tabs and is inherited by all other
frame tabs faces."
  :version "26.1"
  :group 'frame-tabs)

(defface frame-tabs-selected-tab
  '((t :inherit frame-tabs-buffer-tab
       :background "pink"))
  "Frame tabs face for selected window's buffer tab.
This is the face used for the tab corresponding to the buffer
currently shown in the selected window."
  :version "26.1"
  :group 'frame-tabs)

(defface frame-tabs-higlight-tab
  '((t :inherit frame-tabs-buffer-tab
       :foreground "white"
       :background "green3"))
  "Frame tabs face for highlighting buffer tabs.
This is the face used when the mouse cursor hovers over a buffer
tab."
  :version "26.1"
  :group 'frame-tabs)

(defface frame-tabs-x-tab
  '((t :inherit frame-tabs-buffer-tab
       :bold t))
  "Frame tabs face for 'x' buttons."
  :version "26.1"
  :group 'frame-tabs)

(defface frame-tabs-x-higlight-tab
  '((t :inherit frame-tabs-item-tab
       :foreground "white"
       :background "red3"))
  "Frame tabs face for highlighting 'x' buttons.
This face is used when the mouse cursor hovers over an 'x'
button."
  :version "26.1"
  :group 'frame-tabs)

;; Options
(defun frame-tabs--set-value (symbol value)
  "Helper function for customizing frame tabs."
  (set-default symbol value)
  (when frame-tabs-mode
    (frame-tabs-mode -1))
  (frame-tabs-mode 1))

(defcustom frame-tabs-side 'top
  "Side of frame where tabs windows are located.
Choices are 'top' (the default), 'bottom', 'left' and 'right'."
  :type '(choice (const top)
                 (const bottom)
                 (const left)
                 (const right))
  :initialize 'custom-initialize-default
  :set 'frame-tabs--set-value
  :version "26.1"
  :group 'frame-tabs)

(defcustom frame-tabs-x nil
  "Non-nil means frame tabs show an 'x' button for each buffer tab.
The 'x' button serves to dismiss the corresponding buffer in
various ways."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'frame-tabs--set-value
  :version "26.1"
  :group 'frame-tabs)

(defcustom frame-tabs-min-size 1
  "Mimimum size of frame tabs windows.
For tabs windows at the top or bottom of a frame this is their
minimum number of lines.  For tabs windows at the left or right
of a frame this is their minimum number of columns.

This value may be overridden when the major side window showing
the frame tabs window contains other windows."
  :type 'integer
  :version "26.1"
  :group 'frame-tabs)

(defcustom frame-tabs-max-size 6
  "Maximum size of frame tabs windows.
For tabs windows at the top or bottom of a frame this is their
maximum number of lines.  For tabs windows at the left or right
of a frame this is their maximum number of columns.

This value may be overridden when the major side window showing
the frame tabs window contains other windows."
  :type 'integer
  :version "26.1"
  :group 'frame-tabs)

(defcustom frame-tabs-delay 0.0
  "Frame tabs update interval, in seconds.
This is the time Emacs waits before updating frame tabs windows."
  :type 'float
  :version "26.1"
  :group 'frame-tabs)

(defun frame-tabs-default-filter (buffer _frame)
  "Default filter function for frame tabs."
  (let ((name (buffer-name buffer)))
    (unless (eq (aref name 0) ?\s)
      name)))

(defcustom frame-tabs-filter-function 'frame-tabs-default-filter
  "Filter function for frame tabs.
This is a function that takes two arguments - a buffer and a
frame.  If this function returns nil, no tab will be shown for
the buffer in the frame's tab window.  Otherwise, this function
must return a string and the frame's tabs window will display
that string as the buffer's tab.

The default excludes buffers whose name starts with a space."
  :type 'function
  :version "26.1"
  :group 'frame-tabs)

(defun frame-tabs-default-buffer-list (frame)
  "Default frame tabs function for getting a buffer list for FRAME."
  (buffer-list frame))

(defcustom frame-tabs-buffer-list 'frame-tabs-default-buffer-list
  "Function for returning a buffer list for frame tabs.
This is a function that takes one argument - a frame - and
returns a buffer list for that frame.  The default is to call
`buffer-list' for that frame which means to return the frame's
local buffer list.  Customizing this option allows, for example,
to return the fundamental buffer list or a list of buffer in
alphabetical order of their names instead."
  :type 'function
  :version "26.1"
  :group 'frame-tabs)

;; Internal variables and functions
(defvar frame-tabs-timer nil
  "Frame tabs idle timer.")

(defvar frame-tabs-run nil
  "Non-nil while frame tabs runs its idle timer function.")

(defvar frame-tabs-buffers nil
  "List of frame tabs buffers.")

(defvar frame-tabs-map
  (let ((map (make-sparse-keymap)))
    ;; Buffer switching commands.
    (define-key map [down-mouse-1] 'frame-tabs-switch-to-buffer)
    (define-key map [mouse-1] 'ignore)
    (define-key map [C-down-mouse-1] 'frame-tabs-switch-to-buffer-other-window)
    (define-key map [C-mouse-1] 'ignore)
    (define-key map [M-down-mouse-1] 'frame-tabs-switch-to-buffer-other-frame)
    (define-key map [M-mouse-1] 'ignore)
    ;; Buffer dismissal commands.
    (define-key map [down-mouse-3] 'frame-tabs-bury-buffer)
    (define-key map [mouse-3] 'ignore)
    (define-key map [C-down-mouse-3] 'frame-tabs-replace-buffer)
    (define-key map [C-mouse-3] 'ignore)
    (define-key map [M-down-mouse-3] 'frame-tabs-kill-buffer)
    (define-key map [M-mouse-3] 'ignore)
    map)
  "Frame tabs keymap.")

(defvar frame-tabs-x-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'frame-tabs-bury-buffer)
    (define-key map [mouse-1] 'ignore)
    (define-key map [C-down-mouse-1] 'frame-tabs-replace-buffer)
    (define-key map [C-mouse-1] 'ignore)
    (define-key map [M-down-mouse-1] 'frame-tabs-kill-buffer)
    (define-key map [M-mouse-1] 'ignore)
    map)
  "Frame tabs 'x' keymap.
This keymap is used when frame tabs show an 'x' button via
`frame-tabs-x'.")

(defun frame-tabs--window (&optional frame)
  "Return or create a tabs window for FRAME.
FRAME must be a live frame and defaults to the selected one."
  (setq frame (window-normalize-frame frame))
  (let ((window-resize-pixelwise t)
	(tabs-vertical (memq frame-tabs-side '(top bottom)))
	tabs-buffer tabs-window tabs-point tabs-selected)
    ;; `tabs-buffer' is the buffer showing tabs on FRAME and
    ;; `tabs-window' is its window.
    (unless (catch 'found
	      (walk-window-tree
	       (lambda (window)
		 (let ((buffer (window-buffer window)))
		   (when (memq buffer frame-tabs-buffers)
		     (setq tabs-buffer buffer)
		     (setq tabs-window window)
		     (if (eq tabs-window (selected-window))
			 (with-current-buffer tabs-buffer
			   (setq tabs-selected
				 (get-text-property (point) 'buffer)))
		       (setq tabs-selected (window-buffer)))
		     (throw 'found t))))))
      (setq tabs-buffer (generate-new-buffer " *tabs*"))
      (setq frame-tabs-buffers (cons tabs-buffer frame-tabs-buffers))
      ;; Set `tabs-selected' to the truly selected window.
      (setq tabs-selected (window-buffer))
      (with-current-buffer tabs-buffer
	;; Make a tabs window.
	(setq tabs-window
	      (display-buffer-in-side-window
	       tabs-buffer `((side . ,frame-tabs-side)
			     (,(if tabs-vertical 'window-width 'window-height)
			      . ,frame-tabs-min-size))))))
    ;; Display tabs in the window.
    (when tabs-window
      (with-current-buffer tabs-buffer
	(setq mode-line-format nil)
	(setq header-line-format nil)
	(setq buffer-read-only nil)
	(erase-buffer)
	(dolist (buffer (funcall frame-tabs-buffer-list frame))
	  (let ((name (funcall frame-tabs-filter-function buffer frame)))
	    (when name
	      (insert
	       (propertize
		name
		'buffer buffer
		'keymap frame-tabs-map
		'face (if (eq buffer tabs-selected)
			  (progn
			    (setq tabs-point (point))
			    'frame-tabs-selected-tab)
			'frame-tabs-buffer-tab)
		'mouse-face 'frame-tabs-higlight-tab))
	      (when frame-tabs-x
		(insert
		 (propertize
		  "×" 'buffer buffer ; U-00D7
		  ;; 	      "⌧" 'buffer buffer ; U-2327
		  'keymap frame-tabs-x-map
		  'face 'frame-tabs-x-tab
		  'mouse-face 'frame-tabs-x-higlight-tab)))
	      ;; We'd like to use "​" (U-200B) instead of " " in the
	      ;; following form but the display-engine word-wraps only at
	      ;; spaces or tabs so use a display specification instead.
	      ;; Note that we can't use :width either since it would make
	      ;; the spaces of the last item on each line extend to the
	      ;; end of that line.
	      (insert
	       (if tabs-vertical
		   ;; "​"
		   (propertize " " 'display '(space . (:relative-width 0.1)))
		 "\n")))))
	;; Delete very last space or newline inserted.
	(when (memq (char-before) '(?\s ?\n)) (delete-char -1))
	(when tabs-vertical
	  ;; Make sure word wrapping takes care of buffer tabs.
	  (set (make-local-variable 'truncate-lines) nil)
	  (set (make-local-variable 'truncate-partial-width-windows) nil)
	  (set (make-local-variable 'word-wrap) t))
	;; Handle window.
	(set-window-margins tabs-window 0 0)
	(set-window-fringes tabs-window 0 0)
	(set-window-scroll-bars tabs-window 0)
	(setq window-size-fixed nil)
	;; We have bound 'window-resize-pixelwise' to t to make sure
	;; the following call handles the boxed face for our tabs as
	;; expected.
	(fit-window-to-buffer
	 tabs-window frame-tabs-max-size frame-tabs-min-size)
	(if tabs-vertical
	    (setq window-size-fixed 'height)
	  (setq window-size-fixed 'width))
	(set-window-start tabs-window (point-min))
	(setq cursor-type nil)
	(set-window-parameter tabs-window 'no-other-window t)
	(set (make-local-variable 'transient-mark-mode) nil)
	(set-window-point
	 tabs-window
	 (or tabs-point (point-min))))
      (set-window-dedicated-p tabs-window t))
    tabs-window))

(defun frame-tabs--update ()
  "Update frame tabs after timer fires."
  (let ((selected-window (selected-window))
	buffer-list-update-hook)
    (unwind-protect
	(progn
	  (setq frame-tabs-run t)
	  ;; Sanitize frame tabs buffers.
	  (dolist (buffer frame-tabs-buffers)
	    (cond
	     ((not (buffer-live-p buffer))
	      (setq frame-tabs-buffers
		    (delq buffer frame-tabs-buffers)))
	     ((not (get-buffer-window buffer t))
	      (setq frame-tabs-buffers
		    (delq buffer frame-tabs-buffers))
	      (kill-buffer buffer))))
	  ;; Provide tabs window on each frame.  Exclude
	  ;; minibuffer-only and unsplittable frames.
	  (dolist (frame (frame-list))
	    (unless (or (eq (frame-parameter frame 'minibuffer) 'only)
			(frame-parameter frame 'unsplittable))
	      (frame-tabs--window frame))))
      (select-window selected-window)
      (setq frame-tabs-run nil)
      (cancel-timer frame-tabs-timer))))

(defun frame-tabs--enable-timer ()
  "Schedule updating frame tabs."
  (unless frame-tabs-run
    (when (timerp frame-tabs-timer)
      (cancel-timer frame-tabs-timer))
    (setq frame-tabs-timer
	  (run-with-idle-timer frame-tabs-delay t 'frame-tabs--update))))

(defun frame-tabs--remove ()
  "Remove frame tabs window from each frame."
  (condition-case nil
      (progn
	(setq frame-tabs-run t)
	(cancel-timer frame-tabs-timer)
	(dolist (buffer frame-tabs-buffers)
	  (when (buffer-live-p buffer)
	    (delete-windows-on buffer t))
	  (kill-buffer buffer))
	(setq frame-tabs-buffers nil)
	(setq frame-tabs-run nil)
	(cancel-timer frame-tabs-timer))
    (error nil)))

(defun frame-tabs--window-configuration-change ()
  "Run timer when a window configuration changes."
  (frame-tabs--enable-timer))

(defun frame-tabs--buffer-list-update ()
  "Run timer when the buffer list has been updated."
  (frame-tabs--enable-timer))

(defun frame-tabs--window-size-change (frame)
  "Run timer when the root window of FRAME changes size."
  (let ((root (frame-root-window frame)))
    (when (or (and (memq frame-tabs-side '(top bottom))
		   (not (= (window-pixel-width-before-size-change root)
			   (window-pixel-width root))))
	      (and (memq frame-tabs-side '(left right))
		   (not (= (window-pixel-height-before-size-change root)
			   (window-pixel-height root)))))
      (frame-tabs--enable-timer))))

;; Commands
(defun frame-tabs--switch-to-buffer (event &optional where)
  "Switch to buffer of tab clicked on.
EVENT is the original event associated with the click.  WHERE is
the location where the switch shall take place."
  (let* ((tabs-window (posn-window (event-end event)))
	 (tabs-buffer (window-buffer tabs-window))
	 buffer)
    (with-current-buffer tabs-buffer
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (get-text-property (point) 'buffer))
	(when (eq tabs-window (selected-window))
	  (select-window (get-mru-window 0 nil t)))
	(cond
	 ((not (buffer-live-p buffer))
	  (message "Not a live buffer %s" buffer))
	 ((eq where 'other-window)
	  (switch-to-buffer-other-window buffer))
	 ((eq where 'other-frame)
	  (switch-to-buffer-other-frame buffer))
	 (t
	  (switch-to-buffer buffer)))))))

(defun frame-tabs-switch-to-buffer (event)
  "In selected window switch to buffer of tab clicked on."
  (interactive "e")
  (frame-tabs--switch-to-buffer event))

(defun frame-tabs-switch-to-buffer-other-window (event)
  "In other window switch to buffer of tab clicked on."
  (interactive "e")
  (frame-tabs--switch-to-buffer event 'other-window))

(defun frame-tabs-switch-to-buffer-other-frame (event)
  "In other frame switch to buffer of tab clicked on."
  (interactive "e")
  (frame-tabs--switch-to-buffer event 'other-frame))

(defun frame-tabs--dismiss-buffer (event &optional how)
  "Dismiss buffer of tab clicked on.
EVENT is the original event associated with the click.  HOW is
the type of dismissal chosen."
  (let* ((tabs-window (posn-window (event-end event)))
	 (tabs-buffer (window-buffer tabs-window))
	 buffer)
    (with-current-buffer tabs-buffer
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (get-text-property (point) 'buffer))))
    (when (eq tabs-window (selected-window))
      (select-window (get-mru-window 0 nil t)))
    (cond
     ((eq how 'replace)
      (replace-buffer-in-windows buffer))
     ((eq how 'kill)
      (kill-buffer buffer))
     ((eq (window-buffer) buffer)
      ;; Must not call this with BUFFER as argument!
      (bury-buffer))
     (t
      (bury-buffer buffer)))))

(defun frame-tabs-bury-buffer (event)
  "Bury buffer of tab clicked on."
  (interactive "e")
  (frame-tabs--dismiss-buffer event))

(defun frame-tabs-replace-buffer (event)
  "Replace buffer of tab clicked on in all windows showing it."
  (interactive "e")
  (frame-tabs--dismiss-buffer event 'replace))

(defun frame-tabs-kill-buffer (event)
  "Kill buffer of tab clicked on."
  (interactive "e")
  (frame-tabs--dismiss-buffer event 'kill))

;;;###autoload
(define-minor-mode frame-tabs-mode
  "Toggle display of a buffer tabs side window on each frame.
With a prefix argument ARG, enable this mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When this mode is enabled, every normal frame is equipped with a
side window showing tabs for all buffers that appeared on that
frame."
  :global t
  :group 'frame-tabs
  :init-value nil
  :link '(emacs-commentary-link "frame-tabs.el")
  (if frame-tabs-mode
      (progn
	(add-hook 'buffer-list-update-hook 'frame-tabs--buffer-list-update 'append)
	(add-hook 'window-configuration-change-hook 'frame-tabs--window-configuration-change 'append)
	(add-hook 'window-size-change-functions 'frame-tabs--window-size-change 'append)
	(frame-tabs--enable-timer)
	(frame-tabs--update))
    (remove-hook 'buffer-list-update-hook 'frame-tabs--buffer-list-update)
    (remove-hook 'window-configuration-change-hook 'frame-tabs--window-configuration-change)
    (remove-hook 'window-size-change-functions 'frame-tabs--window-size-change)
    (frame-tabs--remove)))

;;;; ChangeLog:

;; 2018-06-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* frame-tabs: New package
;; 


(provide 'frame-tabs)

;;; frame-tabs.el ends here

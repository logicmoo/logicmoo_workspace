;;; svg-clock.el --- Analog clock using Scalable Vector Graphics -*- lexical-binding: t -*-

;; Copyright (C) 2011, 2014  Free Software Foundation, Inc.

;; Maintainer:  Ulf Jasper <ulf.jasper@web.de>
;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Created:     22. Sep. 2011
;; Keywords:    demo, svg, clock
;; Version:     1.2
;; Package-Requires: ((svg "1.0") (emacs "27.0"))

;; This file is part of GNU Emacs.

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

;; svg-clock provides a scalable analog clock.  Rendering is done by
;; means of svg (Scalable Vector Graphics).  In order to use svg-clock
;; you need to build Emacs with svg support.  (To check whether your
;; Emacs supports svg, do "M-: (image-type-available-p 'svg) RET"
;; which must return t).

;; Call `svg-clock' to start a clock.  This will open a new buffer
;; "*clock*" displaying a clock which fills the buffer's window.  Use
;; `svg-clock-insert' to insert a clock programmatically in any
;; buffer, possibly specifying the clock's size, colours and offset to
;; the current-time.  Arbitrary many clocks can be displayed
;; independently.  Clock instances ared updated automatically.  Their
;; resources (timers etc.) are cleaned up automatically when the
;; clocks are removed.

;;; News:

;;  Version 1.0
;;    New function `svg-clock-insert'.  Removed customization
;;    options.

;;  Version 0.5
;;    Fixes (image-mode issue etc.).

;;  Version 0.3
;;    Fixes (disable buffer undo).

;;  Version 0.2
;;    Automatic fitting of clock to window size.

;;  Version 0.1
;;    Initial version.

;;; Code:
(defconst svg-clock-version "1.1" "Version number of `svg-clock'.")

(require 'dom)
(require 'svg)
(require 'cl-macs)
(require 'time-date)

(cl-defstruct svg-clock-handle
  marker  ;; points to the clock's buffer and position
  overlay ;; holds the clock's image
  timer)  ;; takes care of updating the clock

(defun svg-clock--create-def-elements (foreground background no-face)
  "Return a list of SVG elements using the colors FOREGROUND and BACKGROUND.
The elements are supposed to be added to an SVG object as `defs'.
The SVG may then `use': `clock-face', `second-hand', `minute-hand'
and `hour-hand'.  The clock-face has a size of 1x1."
  (list (svg-clock-symbol 'tickshort
                          (svg-clock-line .5 .02 .5 .04
                                          `(stroke . ,foreground)
                                          '(stroke-width . .01)))
        (svg-clock-symbol 'ticklong
                          (svg-clock-line .5 .02 .5 .09
                                          `(stroke . ,foreground)
                                          '(stroke-width . .02)))
        (svg-clock-symbol 'hour-hand
                          (svg-clock-line .5 .22 .5 .54
                                          `(stroke . ,foreground)
                                          '(stroke-width . .04)))
        (svg-clock-symbol 'minute-hand
                          (svg-clock-line .5 .12 .5 .55
                                          `(stroke . ,foreground)
                                          '(stroke-width . .03)))
        (svg-clock-symbol 'second-hand
                          (svg-clock-line .5 .1 .5 .56
                                          `(stroke . ,foreground)
                                          '(stroke-width . 0.005)))
        (svg-clock-symbol 'hand-cap
                          (svg-clock-circle .5 .5 .03
                                            `(stroke . "none")
                                            `(fill . ,foreground)))
        (svg-clock-symbol 'background
                          (svg-clock-circle .5 .5 .49
                                            `(stroke . "none")
                                            `(fill . ,background)))
        (apply 'svg-clock-group 'clock-face
               (nconc (list (svg-clock-use 'background)
                            (svg-clock-use 'hand-cap))
		      (and (not no-face)
			   (mapcar (lambda (angle)
                                     (svg-clock-use (if (= 0 (% angle 30))
							'ticklong
                                                      'tickshort)
						    (svg-clock-transform
                                                     'rotate angle .5 .5)))
				   (number-sequence 0 354 6)))))))

(defun svg-clock--create-svg (time size foreground background
				   no-seconds no-face)
  "Return an SVG element displaying an analog clock.
The clock shows the given TIME, it has a diameter of SIZE, and
its colors are FOREGROUND and BACKGROUND."
  (interactive)
  (let* ((defs (svg-clock--create-def-elements foreground background
					       no-face))
         (svg (svg-create size size))
         (seconds (nth 0 time))
         (minutes (nth 1 time))
         (hours (nth 2 time))
         (clock (apply
		 #'svg-clock-group
                 'clock
                 `(,(svg-clock-use 'clock-face)
                   ,@(unless no-seconds
		       (list (svg-clock-use 'second-hand
					    (svg-clock-transform
					     'rotate
					     (* seconds 6) .5 .5))))
                   ,(svg-clock-use 'minute-hand
                                   (svg-clock-transform
                                    'rotate
                                    (+ (* minutes 6) (/ seconds 10.0)) .5 .5))
                   ,(svg-clock-use 'hour-hand
                                   (svg-clock-transform
                                    'rotate
                                    (+ (* hours 30) (/ minutes 2.0))
				    .5 .5))))))
    (dolist (def defs) (svg-clock-def svg def))
    (svg-clock-def svg clock)
    (dom-append-child svg
                      (svg-clock-use 'clock
                                     (svg-clock-transform 'scale size size)))
    svg))

(defun svg-clock--window-size ()
  "Return maximal size for displaying the svg clock."
  (save-excursion
    (let  ((clock-win (get-buffer-window "*clock*")))
      (if clock-win
          (let* ((coords (window-inside-pixel-edges clock-win))
                 (width (- (nth 2 coords) (nth 0 coords)))
		 (height (- (nth 3 coords) (nth 1 coords))))
            (min width height))
        ;; fallback
        100))))

(defvar svg-clock--prev-update (make-decoded-time))

(defun svg-clock--do-create (size foreground background &optional offset
				  no-seconds no-face)
  "Create an SVG element.
See `svg-clock-insert' for meaning of arguments SIZE, FOREGROUND, BACKGROUND
and OFFSET."
  (let ((time (decode-time (if offset
                               (time-add (current-time)
                                         (seconds-to-time offset))
                             (current-time)))))
    ;; If we're not using seconds, then don't update the display until
    ;; the minute changes.
    (when (or (not no-seconds)
	      (not (equal (decoded-time-minute svg-clock--prev-update)
			  (decoded-time-minute time))))
      (setq svg-clock--prev-update time)
      (let* ((size (or size (svg-clock--window-size)))
             (svg (svg-clock--create-svg time size foreground background
					 no-seconds no-face)))
	svg))))

(defun svg-clock--update (clock-handle &optional size foreground background
				       offset no-seconds no-face)
  "Update the clock referenced as CLOCK-HANDLE.
See `svg-clock-insert' for meaning of optional arguments SIZE, FOREGROUND,
BACKGROUND and OFFSET."
  (when clock-handle
    (let* ((marker (svg-clock-handle-marker clock-handle))
           (buf (marker-buffer marker))
           (win (get-buffer-window buf))
           (ovl (svg-clock-handle-overlay clock-handle)))
      (if (and (buffer-live-p buf)
               (not (eq (overlay-start ovl)
                        (overlay-end ovl))))
          (when (pos-visible-in-window-p marker win t)
            (with-current-buffer buf
              (when-let ((svg (svg-clock--do-create
			       size foreground background offset
			       no-seconds no-face)))
                (overlay-put ovl 'display (create-image
					   (with-temp-buffer
					     (svg-print svg)
					     (buffer-string))
					   'svg t
					   :ascent 'center
					   :scale 1)))))
	;; Buffer no longer exists.
        (message "Cancelling clock timer")
        (cancel-timer (svg-clock-handle-timer clock-handle))
        (delete-overlay ovl)))))

;;;###autoload
(defun svg-clock-insert (&optional size foreground background offset
				   no-seconds no-face)
  "Insert a self-updating image displaying an analog clock at point.
Optional argument SIZE the size of the clock in pixels.
Optional argument FOREGROUND the foreground color.
Optional argument BACKGROUND the background color.
Optional argument OFFSET the offset in seconds between current and displayed
time.
Optional argument NO-SECONDS says whether to do a seconds hand.
Optional argument NO-FACE says whether to decorate the face."
  (let* ((fg (or foreground (face-foreground 'default)))
         (bg (or background (face-background 'default)))
         (marker (point-marker))
         (ch (make-svg-clock-handle :marker marker))
         timer
         ovl)
    (setq svg-clock--prev-update (make-decoded-time))
    (insert "*")
    (setq ovl (make-overlay (marker-position marker)
                            (1+ (marker-position marker))
                            nil t))
    (setf (svg-clock-handle-overlay ch) ovl)
    (setq timer (run-at-time 0 1
                             (lambda ()
                               (svg-clock--update ch size fg bg offset
						  no-seconds no-face))))
    (setf (svg-clock-handle-timer ch) timer)))

(defvar svg-clock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?+] 'svg-clock-grow)
    (define-key map [?-] 'svg-clock-shrink)
    map))

;;;###autoload
(cl-defun svg-clock (&key size foreground background no-seconds no-face)
  "Start/stop the svg clock."
  (interactive)
  (switch-to-buffer (get-buffer-create "*clock*"))
  (let ((inhibit-read-only t))
    (buffer-disable-undo)
    (erase-buffer)
    (svg-clock-insert size foreground background nil no-seconds no-face)
    (view-mode)))

;; Move to svg.el?
(defun svg-clock-symbol (id value)
  "Create an SVG symbol element with given ID and VALUE."
  (dom-node 'symbol `((id . ,id)) value))

(defun svg-clock-circle (x y radius &rest attributes)
  "Create an SVG circle element at position X Y with given RADIUS.
Optional argument ATTRIBUTES contain conses with SVG attributes."
   (dom-node 'circle
	     `((cx . ,x)
	       (cy . ,y)
	       (r . ,radius)
	       ,@attributes)))

(defun svg-clock-def (svg def)
  (dom-append-child (or (dom-by-tag svg 'defs)
			(let ((node (dom-node 'defs)))
			  (dom-add-child-before svg node) node))
		    def)
  svg)

(defun svg-clock-line (x1 y1 x2 y2 &rest attributes)
  "Create an SVG line element starting at (X1, Y1), ending at (X2, Y2).
Optional argument ATTRIBUTES contain conses with SVG attributes."
  (dom-node 'line `((x1 . ,x1)
                    (y1 . ,y1)
                    (x2 . ,x2)
                    (y2 . ,y2)
                    ,@attributes)))

(defun svg-clock-group (id &rest children)
  "Create an SVG group element with given ID and CHILDREN."
  (apply 'dom-node 'g `((id . ,id)) children))

(defun svg-clock-use (id &rest attributes)
  "Create an SVG use element with given ID.
Optional argument ATTRIBUTES contain conses with SVG attributes."
  (dom-node 'use `((xlink:href . ,(format "#%s" id)) ,@attributes)))

(defun svg-clock-transform (action &rest args)
  "Create an SVG transform attribute element for given ACTION.
Argument ARGS contain the action's arguments."
  (cons 'transform
        (format "%s(%s)" action (mapconcat 'number-to-string args ", "))))

(defun svg-clock-color-to-hex (color)
  "Return hex representation of COLOR."
  (let ((values (color-values color)))
    (format "#%02x%02x%02x" (nth 0 values) (nth 1 values) (nth 2 values))))

;;;; ChangeLog:

;; 2020-08-18  Lars Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Revert "Copy svg--def to manipulate the DOM directly from svg-clock.el"
;; 
;; 	This reverts commit 1f7c74037d3adadc86c72c9736308a24107ad964.
;; 
;; 	This had already been fixed in almost the same way.
;; 
;; 2020-08-18  Lars Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Bump version after last fix
;; 
;; 2020-08-18  Pip Cet  <pipcet@gmail.com>
;; 
;; 	Copy svg--def to manipulate the DOM directly from svg-clock.el
;; 
;; 2020-01-22  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	Fix requirements and version numbers
;; 
;; 	- Require emacs version 27 via 'Package-Requires'.
;; 	- Sync internal version number with package version.
;; 	- require time-date
;; 
;; 2019-11-04  Lars Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Allow controlling whether to show the seconds and the face
;; 
;; 2019-11-04  Lars Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Pick the right size when on a scaled display
;; 
;; 2019-11-04  Lars Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	There's no longer an svg-def function in svg.el
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2016-02-04  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	Bump version number to 1.0 and make changes public
;; 
;; 2014-12-03  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	svg-clock.el: Refactoring.  Use 'svg.el' and 'dom.el'.
;; 
;; 2014-05-31  Dieter (tiny change)  <dieter@schoen.or.at>
;; 
;; 	* packages/svg-clock/svg-clock.el (svg-clock-do-update,
;; 	svg-clock-start): Don't call image-mode every second.
;; 
;; 2013-07-08  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	Fixed image-mode issue.	 Changed version to 0.5.
;; 
;; 2012-01-29  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	2012-01-29  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 		* elpa/packages/svg-clock/svg-clock.el: Clean up.
;; 	(svg-clock-version): Remove.
;; 	(svg-clock-color-to-hex): Use 2 digits for each hexadecimal color
;; 	value.
;; 	(svg-clock-mode, svg-clock-mode-map): Clean up.
;; 
;; 	
;; 
;; 2011-12-13  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	svg-clock: Disable buffer undo
;; 
;; 		* elpa/packages/svg-clock/svg-clock.el: Disable buffer
;; 		 undo. Changed "Version" to 0.3.
;; 
;; 	
;; 
;; 2011-09-27  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	Added Version to svg-clock.el
;; 
;; 		* elpa/packages/svg-clock/svg-clock.el: Added "Version".
;; 
;; 	
;; 
;; 2011-09-26  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	2011-09-26  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 		* elpa/packages/svg-clock/svg-clock.el (svg-clock-version):
;; 	Changed to "0.2"
;; 	(svg-clock-size): Add automatic-fit-to-window option.
;; 	(svg-clock-update, svg-clock-template): Formatting.
;; 	(svg-clock-set-size, svg-clock-grow, svg-clock-shrink)
;; 	(svg-clock-fit-window, svg-clock-stop, svg-clock-start)
;; 	(svg-clock-mode, svg-clock-mode-map): New
;; 	(svg-clock): Refactored.
;; 
;; 	
;; 
;; 2011-09-23  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	New package svg-clock.
;; 



(provide 'svg-clock)

;;; svg-clock.el ends here

;;; wisi-fringe.el --- show approximate error locations in the fringe
;;
;; Copyright (C) 2018 - 2019  Free Software Foundation, Inc.
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
;; Design:
;;
;; Bitmaps are displayed in the fringe by putting a 'display property
;; on buffer text. However, just doing that also hides the buffer
;; text. To avoid that, we put the ’display property on a string, and
;; then an overlay containing that string as ’before-string or
;; ’after-string on the newline of a buffer line.
;;
;; We show approximate error positions in the entire buffer with
;; single-pixel lines in the right fringe, and mark error lines with
;; ’!!’ in the left fringe.

(defun wisi-fringe-create-bitmaps ()
  "Return an array of bitmap symbols containing the fringe bitmaps."
  ;; First create the ’!!’ bitmap.
  (define-fringe-bitmap 'wisi-fringe--double-exclaim-bmp
    (vector
     #b00000000
     #b01100110
     #b01100110
     #b01100110
     #b01100110
     #b01100110
     #b00000000
     #b01100110
     #b01010110
     #b00000000))

  ;; In condensing the entire buffer to the current window height, we
  ;; assume a 10 point font, which allows 6 distinct line positions
  ;; each one pixel high, with one blank pixel between.

  (let ((result (make-vector 64 nil))
	(i 1))
    (while (<= i (length result))
      (aset result (1- i)
	    (define-fringe-bitmap (intern (format "wisi-fringe--line-%d-bmp" i))
	      (vector
	       (if (>= i 32) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 32) 16) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 16) 8) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 8) 4) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 4) 2) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 2) 1) #b11111111 #b00000000)
	       )))
      (setq i (1+ i)))
    result))

(defconst wisi-fringe-bitmaps (wisi-fringe-create-bitmaps)
  "Array of 64 bitmap symbols.")

(defun wisi-fringe--put-right (line bitmap-index)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let* ((endpos (line-end-position))
	   (ov (make-overlay endpos (1+ endpos)))
	   (bmp (aref wisi-fringe-bitmaps bitmap-index)))
      (overlay-put ov 'after-string (propertize "-" 'display (list 'right-fringe bmp 'compilation-error)))
      (overlay-put ov 'wisi-fringe t)
      )))

(defun wisi-fringe--put-left (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let* ((endpos (line-end-position))
	   (ov (make-overlay endpos (1+ endpos)))
	   (bmp 'wisi-fringe--double-exclaim-bmp))
      (overlay-put ov 'before-string (propertize "-" 'display (list 'left-fringe bmp 'compilation-error)))
      (overlay-put ov 'wisi-fringe t)
      )))

(defun wisi-fringe--scale (error-line buffer-lines window-line-first window-lines)
  "Return a cons (LINE . BIN) for ERROR-LINE,
where LINE is the line to display the error bar on, and BIN is a
6-bit bit vector giving the relative position in that line.
BUFFER-LINES is the count of lines in the buffer.
WINDOW-LINE-FIRST is the first and last lines of the buffer
visible in the window. WINDOW-LINES is the count of lines visible
in the window."
  ;; If the end of buffer is inside the window, and this calculation
  ;; puts a mark after that, it will actually be put on the last real
  ;; line. That’s good enough for our purposes.

  ;; partial-lines / window-line = 6
  ;; buffer-lines / window-line = 1/scale
  ;; buffer-lines / partial-line  = (window-line / partial-lines) * (buffer-lines / window-line) = 1/6 * 1/scale
  (let* ((scale (/ window-lines (float buffer-lines)))
	 (line (floor (* scale error-line)))
	 (rem (- error-line (floor (/ line scale)))))
    (cons (+ window-line-first line) (lsh 1 (min 5 (floor (* rem (* 6 scale))))))))

(defun wisi-fringe-clean ()
  "Remove all wisi-fringe marks."
  (remove-overlays (point-min) (point-max) 'wisi-fringe t))

(defun wisi-fringe-display-errors (positions)
  "Display markers in the left and right fringe for each buffer position in POSITIONS.
The buffer containing POSITIONS must be current, and the window
displaying that buffer must be current."
  ;; We don't recompute fringe display on scroll, because the user
  ;; will probably have edited the code by then, triggering a new
  ;; parse.
  (wisi-fringe-clean)
  (let (scaled-posns
	(buffer-lines (line-number-at-pos (point-max)))
	(window-lines (window-height))
	(window-pos-first (window-start))
	(window-pos-last  (window-end))
	(window-line-first (line-number-at-pos (window-start))))
    (dolist (pos positions)
      (let* ((line (line-number-at-pos pos))
	     (scaled-pos (wisi-fringe--scale line buffer-lines window-line-first window-lines)))
	(when (and (>= pos window-pos-first)
		   (<= pos window-pos-last))
	  (wisi-fringe--put-left line))
	(if (and scaled-posns
		 (= (caar scaled-posns) (car scaled-pos)))
	    (setcdr (car scaled-posns) (logior (cdar scaled-posns) (cdr scaled-pos)))
	  (push scaled-pos scaled-posns))
	))

    (dolist (pos scaled-posns)
      (wisi-fringe--put-right (car pos) (1- (cdr pos))))
    ))

(provide 'wisi-fringe)

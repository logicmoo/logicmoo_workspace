;;; window-jump.el --- Move left/right/up/down through your windows.
;;
;; Author: Steven Thomas
;; Created: 16 Oct 2011
;; Keywords: frames convenience
;; Package-Version: 20170809.2208
;; Package-Commit: 6bdb51e9a346907d60a9625f6180bddd06be6674
;; Version: 0.1.0
;; URL: https://github.com/chumpage/chumpy-windows
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Directionally move through the windows in a frame. The core functions
;; are window-jump-<left,right,up,down>. Bind them to some keys and
;; you'll be flying around your windows like Superman.
;;
;; See https://github.com/chumpage/chumpy-windows for more
;; documentation, and to submit patches.
;;
;;; Code:

(require 'cl)

(defvar wj-wrap nil
  "Whether or not to wrap the window jumping behavior. For
example, if you jump to the right window and there is no window
to the right, jump to the leftmost window instead.")

(defvar wj-jump-frames nil
  "Whether or not to look in other frames for windows to jump
to.")

;; vector math functions
(defun wj-vx (v) (car v))
(defun wj-vy (v) (cadr v))
(defun wj-vec (x y) (list x y))
(defun wj-square (x) (* x x))
(defun wj-vec-dot (v1 v2) (+ (* (wj-vx v1) (wj-vx v2))
                             (* (wj-vy v1) (wj-vy v2))))
(defun wj-vec- (v1 v2) (wj-vec (- (wj-vx v1) (wj-vx v2))
                               (- (wj-vy v1) (wj-vy v2))))
(defun wj-vec+ (v1 v2) (wj-vec (+ (wj-vx v1) (wj-vx v2))
                               (+ (wj-vy v1) (wj-vy v2))))
(defun wj-vec/ (v s) (wj-vec (/ (wj-vx v) s)
                             (/ (wj-vy v) s)))
(defun wj-vec* (v s) (wj-vec (* (wj-vx v) s)
                             (* (wj-vy v) s)))
(defun wj-vec-length (v) (sqrt (+ (wj-square (wj-vx v))
                                  (wj-square (wj-vy v)))))
(defun wj-normalize (v) (wj-vec/ v (wj-vec-length v)))
(defun wj-vec-dist (v1 v2) (wj-vec-length (wj-vec- v1 v2)))
(defvar wj-inf 1.0e+INF)
(defvar -wj-inf -1.0e+INF)
(defvar wj-vec-left '(-1 0))
(defvar wj-vec-right '(1 0))
(defvar wj-vec-up '(0 -1))
(defvar wj-vec-down '(0 1))

(defun wj-edges-left   (edges) (nth 0 edges))
(defun wj-edges-top    (edges) (nth 1 edges))
(defun wj-edges-right  (edges) (nth 2 edges))
(defun wj-edges-bottom (edges) (nth 3 edges))

(defun wj-closest-point-on-seg (ls p)
  "Find the closest point on a line segment (denoted by a pair of
vectors LS) and a point P."
  ;; Reparameterize line segment as origin and direction
  (let* ((ls0 (car ls))
         (ls1 (cadr ls))
         (l (wj-vec-length (wj-vec- ls1 ls0)))
         (d (wj-normalize (wj-vec- ls1 ls0)))
         (pdir (wj-vec- p ls0))
         (result (wj-vec* d (wj-vec-dot pdir d)))
         (result-dot-d (wj-vec-dot result d)))
    (cond ((< result-dot-d 0) ls0)
          ((> result-dot-d l) ls1)
          ((wj-vec+ ls0 result)))))

;; In terminal emacs, frame-parameter will sometimes return nil. We take that to
;; mean 0. In Windows emacs, frame-parameter sometimes returns a list of the
;; form (+ -4). I have no idea why that is, but we'll just eval the list to
;; get the answer.
(defun wj-frame-parameter (frame param)
  (let ((result (frame-parameter frame param)))
    (cond ((integerp result) result)
          ((null result) 0)
          ((listp result) (eval result)))))

(defun wj-frame-left (frame) (wj-frame-parameter frame 'left))
(defun wj-frame-right (frame) (+ (wj-frame-left frame) (frame-pixel-width)))
(defun wj-frame-top (frame) (wj-frame-parameter frame 'top))
(defun wj-frame-bottom (frame) (+ (wj-frame-top frame) (frame-pixel-height frame)))

(defun wj-all-windows ()
  "Get all windows in all frames."
  (let ((windows))
    (walk-windows (lambda (window) (push window windows)) nil t)
    windows))

(defun wj-other-windows ()
  "Get all windows other than the selected window in this frame."
  (remove (selected-window) (window-list)))

(defun wj-all-other-windows ()
  "Get all windows other than the selected window in all frames."
  (remove (selected-window) (wj-all-windows)))

;; Cygwin emacs doesn't have window-pixel-edges for some reason
(unless (fboundp 'window-pixel-edges)
  (fset 'window-pixel-edges 'window-edges))

(defun wj-window-box (window)
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (let ((fl (wj-frame-left (window-frame window)))
          (ft (wj-frame-top (window-frame window))))
      (list (+ fl wl) (+ ft wt) (+ fl wr) (+ ft wb)))))

(defun wj-bounding-rect-all-windows ()
  (let ((xmin wj-inf) (xmax -wj-inf) (ymin wj-inf) (ymax -wj-inf))
    (dolist (window (wj-all-windows))
      (destructuring-bind (wl wt wr wb) (wj-window-box window)
        (setq xmin (min xmin wl))
        (setq xmax (max xmax wr))
        (setq ymin (min ymin wt))
        (setq ymax (max ymax wb))))
    (list xmin ymin xmax ymax)))

(defun wj-window-segments (window)
  (destructuring-bind (left top right bottom) (wj-window-box window)
    `(((,left ,top) (,left ,bottom))
      ((,left ,top) (,right ,top))
      ((,right ,top) (,right ,bottom))
      ((,right ,bottom) (,left ,bottom)))))

(defun wj-window-midpoint (window)
  (destructuring-bind (left top right bottom) (wj-window-box window)
    (list (+ left (/ (- right left) 2))
          (+ top (/ (- bottom top) 2)))))

(defun wj-window-contain (window pos)
  (destructuring-bind (left top right bottom) (wj-window-box window)
    (and (> (wj-vx pos) left)
         (< (wj-vx pos) right)
         (> (wj-vy pos) top)
         (< (wj-vy pos) bottom))))

(defun wj-point-window-distance (p window)
  (apply 'min
         (mapcar (lambda (ls) (wj-vec-dist (wj-closest-point-on-seg ls p) p))
                 (wj-window-segments window))))

(defun wj-closest-point-on-window (p window)
  (car (sort (mapcar (lambda (ls) (wj-closest-point-on-seg ls p))
                     (wj-window-segments window))
             (lambda (p1 p2) (< (wj-vec-dist p1 p) (wj-vec-dist p2 p))))))

(defun wj-cursor-pos ()
  (let ((pos (pos-visible-in-window-p (window-point) (selected-window) t))
        (box (wj-window-box (selected-window))))
    (when (null pos) ; Work around a bug in pos-visible-in-window-p when running terminal emacs
      (setq pos (wj-vec (wj-edges-left box) (wj-edges-bottom box))))
    ;; Move pos to the center of the cursor instead of the top-left pixel
    (setq pos (wj-vec (+ (/ (float (frame-char-width)) 2) (wj-vx pos))
                      (+ (/ (float (frame-char-height)) 2) (wj-vy pos))))
    (wj-vec (+ (wj-edges-left box) (wj-vx pos))
            (+ (wj-edges-top box) (wj-vy pos)))))

(defun wj-goto-window (window)
  (when (not (eql (window-frame window) (window-frame (selected-window))))
    (select-frame-set-input-focus (window-frame window)))
  (select-window window))

(defun wj-jump-origin (d)
  (destructuring-bind (wl wt wr wb) (wj-window-box (selected-window))
    (let ((pos (wj-cursor-pos)))
      (cond ((equal d wj-vec-left)  (wj-vec wl (wj-vy pos)))
            ((equal d wj-vec-right) (wj-vec wr (wj-vy pos)))
            ((equal d wj-vec-up)    (wj-vec (wj-vx pos) wt))
            ((equal d wj-vec-down)  (wj-vec (wj-vx pos) wb))))))

(defun wj-wrap-jump-origin (o d)
  (destructuring-bind (fl ft fr fb) (wj-bounding-rect-all-windows)
    (cond ((equal d wj-vec-left)  (wj-vec (+ fr 1) (wj-vy o)))
          ((equal d wj-vec-right) (wj-vec (- fl 1) (wj-vy o)))
          ((equal d wj-vec-up)    (wj-vec (wj-vx o) (+ fb 1)))
          ((equal d wj-vec-down)  (wj-vec (wj-vx o) (- ft 1))))))

(defun wj-get-windows-in-direction (o d windows)
  (remove-if (lambda (window)
               (let ((to-window-vec (wj-vec- (wj-closest-point-on-window o window) o)))
                 (and (not (= (wj-vec-length to-window-vec) 0))
                      (<= (wj-vec-dot to-window-vec d) 0))))
             windows))

(defun wj-get-window-distances (o windows)
  (sort (mapcar (lambda (window) (cons window
                                       (wj-point-window-distance o window)))
                windows)
        (lambda (x1 x2) (< (cdr x1) (cdr x2)))))

(defun wj-closest-window (o d windows)
  (caar (wj-get-window-distances o (wj-get-windows-in-direction o d windows))))

(defun window-jump (d)
  "Set the window in direction D as the selected window. D should
be one of wj-vec-left, wj-vec-right, wj-vec-down, or wj-vec-up."
  (cl-labels ((attempt-jump (o windows)
             (let ((window (wj-closest-window o d windows)))
               (when window
                 (wj-goto-window window)
                 (deactivate-mark)
                 t))))
    (let* ((all-wnd-fn (if wj-jump-frames 'wj-all-windows 'window-list))
           (other-wnd-fn (if wj-jump-frames 'wj-all-other-windows 'wj-other-windows)))
      (if wj-wrap
          (when (not (attempt-jump (wj-jump-origin d) (funcall other-wnd-fn)))
            (attempt-jump (wj-wrap-jump-origin (wj-cursor-pos) d) (funcall all-wnd-fn)))
          (attempt-jump (wj-jump-origin d) (funcall other-wnd-fn))))))

;;;###autoload
(defun window-jump-left ()
  "Move to the window to the left of the current window."
  (interactive)
  (window-jump wj-vec-left))

;;;###autoload
(defun window-jump-right ()
  "Move to the window to the right of the current window."
  (interactive)
  (window-jump wj-vec-right))

;;;###autoload
(defun window-jump-down ()
  "Move to the window below the current window."
  (interactive)
  (window-jump wj-vec-down))

;;;###autoload
(defun window-jump-up ()
  "Move to the window above the current window."
  (interactive)
  (window-jump wj-vec-up))

(provide 'window-jump)

;;; window-jump.el ends here

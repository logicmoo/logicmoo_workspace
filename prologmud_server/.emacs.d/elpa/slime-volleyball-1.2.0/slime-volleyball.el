;;; slime-volleyball.el --- An SVG Slime Volleyball Game -*-lexical-binding:t-*-

;; Copyright (C) 2013-2020  Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 1.2.0
;; Keywords: games
;; Package-Requires: ((cl-lib "0.5"))
;; Package-Type: multi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For RMF.

;; I was inspired by Bret Victor's "Inventing on Principle" talk [1] and wanted
;; to see how close Emacs could get to the graphical interactivity and feedback
;; of his environment.

;; The resulting research effort turned up some Emacs capabilities that were
;; new to me.  I was happily surprised to find Emacs's librsvg support could
;; draw SVG right in a buffer.  svg-clock showed me how to do the animation;
;; the erase-buffer/insert-image approach is inefficient but it works [2].  I
;; even came across some early-stage experimentation toward an Elisp vector
;; graphics library [3].

;; To put it all together, I decided to clone a great Java game I played a long
;; time ago (with the excuse of testing icedtea-web), Slime Volleyball [4].

;; This is the result; I hope you find it fun.

;; 1. http://vimeo.com/36579366
;; 2. http://elpa.gnu.org/packages/svg-clock.html
;; 3. http://lists.gnu.org/archive/html/bug-gnu-emacs/2010-05/msg00491.html
;; 4. http://oneslime.net/

;; Ending music: https://archive.org/details/M00GNU

;; Features
;; ========

;; * One player quest mode
;;   (press SPC on start-up)

;; * Two player face-off mode
;;   (press 2 on start-up)

;; * God mode: instantly apply Elisp fragments to hack the game environment
;;   (press G during the game)

;; * Slime training mode: a statistical learning algorithm for training
;;                        opponent slimes
;;   (press t on start-up,
;;    use M-: (slime-volleyball-save-strategy ...) to save the strategy, make
;;    sure to manually save the quantize, hash-situation and controller
;;    functions you come up with -- see green-slime.el.gz)
;;
;;   I used this mode to train Green Slime and Grey Slime.

;; * Frame-by-frame debugging
;;   (F9 to enter/exit frame-by-frame mode, F8 to advance a frame)

;; * Music (supported if ogg123, of vorbis-tools, is on the system
;; * executable path, otherwise fails silently)

;; Controls
;; ========

;; The controls are a little different than in other games because Emacs
;; doesn't recognized key-up events.

;; One Player Mode
;; ---------------

;; C-b, left,  a: start moving left
;; C-f, right, d: start moving right
;; C-p, up,    w: jump
;; C-n, down,  s: stop

;; Two Player Mode
;; ---------------

;; Left Slime:

;; C-b, a: start moving left
;; C-f, d: start moving right
;; C-p, w: jump
;; C-n, s: stop

;; Right Slime:

;; left:  start moving left
;; right: start moving right
;; up:    jump
;; down:  stop

;; Potential Future Features
;; =========================

;; * Network support for two player mode or slime Turing test

;; * Time-to-space mapping for opponent slime design, like in [1]

;; * 8-bit music composition mode

;; * A really hard non-statistical end boss

;;; Code:

(require 'cl-lib)

(defconst slime-volleyball-base (file-name-directory load-file-name)
  "The directory in which the slime volleyball package is installed.")

(defcustom slime-volleyball-enable-sound t
  "Music is enabled if this is non-nil."
  :type 'boolean
  :group 'slime-volleyball)

(defcustom slime-volleyball-beach-mode nil
  "If this is non-nil, the slimes will compete on sand instead of concrete."
  :type 'boolean
  :group 'slime-volleyball)

(defvar slime-volleyball-animation-timer nil "Game animation timer.")
(defvar slime-volleyball-paused nil "Non-nil if the game is paused.")
(defvar slime-volleyball-starting nil "Non-nil if the game is starting up.")
(defvar slime-volleyball-slime1 nil "The left slime.")
(defvar slime-volleyball-slime2 nil "The right slime.")
(defvar slime-volleyball-advance-frame nil
  "Non-nil to advance a frame of the animation.")
(defvar slime-volleyball-ball-bouncy-bouncy nil
  "Non-nil to bounce ball off floor.  See also `slime-volleyball-just-rally'.")
(defvar slime-volleyball-just-rally nil "Non-nil means do not track points.")
(defvar slime-volleyball-ball-radius nil "Radius of ball.")
(defvar slime-volleyball-ball-velocity-x nil "Ball's velocity in X direction.")
(defvar slime-volleyball-ball-velocity-y nil "Ball's velocity in Y direction.")
(defvar slime-volleyball-ball-x nil "Ball's X position.")
(defvar slime-volleyball-ball-y nil "Ball's Y position.")
(defvar slime-volleyball-blue-slime-orig-x nil "Blue slime starting position.")
(defvar slime-volleyball-blue-slime-serving nil
  "Non-nil if Blue slime is serving.")
(defvar slime-volleyball-blue-slime nil "Structure representing Blue slime.")
(defvar slime-volleyball-color-index nil "Chameleon color index.")
(defvar slime-volleyball-floor-height nil "The depth of the floor or sand.")
(defvar slime-volleyball-force-y nil "Force of gravity.")
(defvar slime-volleyball-frame-by-frame-mode nil
  "Non-nil to step the animation frame-by-frame.")
(defvar slime-volleyball-game-over nil "Non-nil if the match has ended.")
(defvar slime-volleyball-god-mode nil "Non-nil to enable God mode.")
(defvar slime-volleyball-green-slime nil "Structure representing Green slime.")
(defvar slime-volleyball-grey-slime nil "Structure representing Grey slime.")
(defvar slime-volleyball-last-level nil "The index of the final level.")
(defvar slime-volleyball-level nil "The player's current level.")
(defvar slime-volleyball-message nil
  "A message to display in an in-game message box.")
(defvar slime-volleyball-moves-list nil
  "A list of moves made by the slime being trained.")
(defvar slime-volleyball-net-height nil "The height of the net.")
(defvar slime-volleyball-net-width nil "The width of the net.")
(defvar slime-volleyball-net-x nil "The net's X position.")
(defvar slime-volleyball-net-y nil "The net's Y position.")
(defvar slime-volleyball-one-player-beat-the-game nil
  "Non-nil if the player beat all opponent slimes.")
(defvar slime-volleyball-opponents nil
  "A list of computer-controlled slime opponents.")
(defvar slime-volleyball-play-ending nil
  "Non-nil to play the ending sequence.")
(defvar slime-volleyball-point-circles-radius nil
  "Radius of score-keeping circles.")
(defvar slime-volleyball-point-circles-start-x nil
  "X position of score-keeping circles.")
(defvar slime-volleyball-point-circles-y nil
  "Y position of score-keeping circles.")
(defvar slime-volleyball-point-scored nil
  "Non-nil to indicate that a point has been scored.")
(defvar slime-volleyball-points-to-win nil
  "Number of points required to win a match.")
(defvar slime-volleyball-prev-frame-ball-x nil
  "Ball's X position in previous animation frame.")
(defvar slime-volleyball-prev-frame-ball-y nil
  "Ball's Y position in previous animation frame.")
(defvar slime-volleyball-quitting nil
  "Non-nil if the user is quitting the game.")
(defvar slime-volleyball-scene-height nil
  "The height of the game scene not including the floor.")
(defvar slime-volleyball-scene-total-height nil
  "The height of the game scene including the floor.")
(defvar slime-volleyball-scene-width nil
  "The width of the game scene.")
(defvar slime-volleyball-scene nil
  "Vector graphics markup representing the current game scene.")
(defvar slime-volleyball-serving-slime nil
  "The slime that is currently serving or that most recently served the ball.")
(defvar slime-volleyball-slime-radius nil
  "The radius of the slime players.")
(defvar slime-volleyball-template-ball nil
  "Ball vector graphics markup template.")
(defvar slime-volleyball-template-dot nil
  "Dot vector graphics markup template.")
(defvar slime-volleyball-template-footer nil
  "Footer vector graphics markup template.")
(defvar slime-volleyball-template-ground nil
  "Ground vector graphics markup template.")
(defvar slime-volleyball-template-header nil
  "Header vector graphics markup template.")
(defvar slime-volleyball-template-message-box-start nil
  "Message box closing vector graphics markup template.")
(defvar slime-volleyball-template-message-box-middle nil
  "Message box main vector graphics markup template.")
(defvar slime-volleyball-template-message-box-end nil
  "Message box closing vector graphics markup template.")
(defvar slime-volleyball-template-net nil
  "Net vector graphics markup template.")
(defvar slime-volleyball-template-point-circle nil
  "Point circle vector graphics markup template.")
(defvar slime-volleyball-template-sky nil
  "Sky vector graphics markup template.")
(defvar slime-volleyball-template-slime nil
  "Slime vector graphics markup template.")
(defvar slime-volleyball-time-delta nil
  "The time elapsed since the last scene update.")
(defvar slime-volleyball-title-screen nil
  "The vector graphics representing the title screen.")
(defvar slime-volleyball-training-mode nil
  "Non-nil when the game is running in training mode.")
(defvar slime-volleyball-training-slime-strategy nil
  "The strategy of the slime being trained in training mode.")
(defvar slime-volleyball-training-slime nil
  "The slime being trained in training mode.")
(defvar slime-volleyball-two-players nil
  "Non-nil if both slimes are human-controlled.")
(defvar slime-volleyball-unpause-function nil
  "A function that when called will unpause the game.")
(defvar slime-volleyball-x-i nil "Ball's initial X position.")
(defvar slime-volleyball-y-i nil "Ball's initial Y position.")
(defvar slime-volleyball-x-f nil "Ball's final X position.")
(defvar slime-volleyball-y-f nil "Ball's final Y position.")
(defvar slime-volleyball-v-x-i nil "Ball's initial velocity in X direction.")
(defvar slime-volleyball-v-y-i nil "Ball's initial velocity in Y direction.")
(defvar slime-volleyball-v-x-f nil "Ball's final velocity in X direction.")
(defvar slime-volleyball-v-y-f nil "Ball's final velocity in Y direction.")
(defvar slime-volleyball-delta-y nil "The change in the ball's Y position.")
(defvar slime-volleyball-mode-map nil "Keymap for the game.")
(defvar slime-volleyball-offset nil "Ending scene variable.")
(defvar slime-volleyball-bg1-wrap nil "Ending scene variable.")
(defvar slime-volleyball-bg1-x nil "Ending scene variable.")
(defvar slime-volleyball-bg1-y nil "Ending scene variable.")
(defvar slime-volleyball-bg2-wrap nil "Ending scene variable.")
(defvar slime-volleyball-bg2-x nil "Ending scene variable.")
(defvar slime-volleyball-bg2-y nil "Ending scene variable.")
(defvar slime-volleyball-bg3-wrap nil "Ending scene variable.")
(defvar slime-volleyball-bg3-x nil "Ending scene variable.")
(defvar slime-volleyball-bg3-y nil "Ending scene variable.")
(defvar slime-volleyball-bg4-wrap nil "Ending scene variable.")
(defvar slime-volleyball-bg4-x nil "Ending scene variable.")
(defvar slime-volleyball-bg4-y nil "Ending scene variable.")
(defvar slime-volleyball-ending-num nil "Ending scene variable.")
(defvar slime-volleyball-ending-rate nil "Ending scene variable.")
(defvar slime-volleyball-endvar2 nil "Ending scene variable.")
(defvar slime-volleyball-endvar nil "Ending scene variable.")
(defvar slime-volleyball-music-player-process nil
  "Object representing process playing music.")
(defvar slime-volleyball-experimental-slime nil
  "Non-nil to make the slime being trained more experimental.")

(cl-defstruct slime-volleyball-slime
  "A player in the game of slime volleyball"
  player
  jumping
  points
  controller
  updater
  v-y-i
  v-y-f
  v-x
  x
  y
  delta-y
  eye-center-x
  eye-center-y
  eye-radius
  pupil-center-x
  pupil-center-y
  pupil-radius
  color
  speed)

(defmacro slime-volleyball-slime-move-function (number direction)
  "Define a function to move a slime NUMBER steps in direction DIRECTION."
  `(defun ,(intern (format "slime-volleyball-slime%d-%s" number direction)) ()
     (interactive)
     (,(intern (format "slime-volleyball-slime-%s" direction))
      ,(intern (format "slime-volleyball-slime%d" number)))))

(slime-volleyball-slime-move-function 1 "left")
(slime-volleyball-slime-move-function 1 "right")
(slime-volleyball-slime-move-function 1 "jump")
(slime-volleyball-slime-move-function 1 "stop")
(slime-volleyball-slime-move-function 2 "left")
(slime-volleyball-slime-move-function 2 "right")
(slime-volleyball-slime-move-function 2 "jump")
(slime-volleyball-slime-move-function 2 "stop")

(defun slime-volleyball-blue-slime-maybe-right ()
  "Move the blue slime to the right, with randomness."
  (when (not (eq (random 10) 3))
    (slime-volleyball-slime-right slime-volleyball-slime2)))

(defun slime-volleyball-blue-slime-maybe-left ()
  "Move the blue slime to the left, with randomness."
  (when (not (eq (random 10) 3))
    (slime-volleyball-slime-left slime-volleyball-slime2)))

(defun slime-volleyball-blue-slime-controller ()
  "Control the computer-controlled blue slime."
  (cond
   ((and (not slime-volleyball-blue-slime-serving)
         (< (abs slime-volleyball-ball-velocity-x) 0.001)
         (> slime-volleyball-ball-x (slime-volleyball-slime-x
                                     slime-volleyball-slime2)))
    ;; Serve start.
    (progn
      (setq slime-volleyball-blue-slime-serving t)
      (setq slime-volleyball-blue-slime-orig-x (slime-volleyball-slime-x
                                                slime-volleyball-slime2))
      (slime-volleyball-slime-right slime-volleyball-slime2)))
   (slime-volleyball-blue-slime-serving
    (if (> (slime-volleyball-slime-x slime-volleyball-slime2)
           (+ slime-volleyball-blue-slime-orig-x
              (/ slime-volleyball-slime-radius 8)))
        (progn
          (slime-volleyball-slime-stop slime-volleyball-slime2)))
    (setq slime-volleyball-blue-slime-serving nil))
   (t
    (if (> slime-volleyball-ball-x slime-volleyball-net-x)
        (progn
          (if (> slime-volleyball-ball-y
                 (- slime-volleyball-scene-height
                    (* 2 slime-volleyball-slime-radius)))
              (if (and (> slime-volleyball-ball-x (slime-volleyball-slime-x
                                                   slime-volleyball-slime2))
                       (< slime-volleyball-ball-x
                          (+ (slime-volleyball-slime-x
                              slime-volleyball-slime2)
                             (* 2 slime-volleyball-slime-radius))))
                  (slime-volleyball-slime-jump slime-volleyball-slime2)))
          (if (> slime-volleyball-ball-x
                 (+ (slime-volleyball-slime-x
                     slime-volleyball-slime2)
                    (/ slime-volleyball-slime-radius 4)))
              (slime-volleyball-blue-slime-maybe-right)
            (slime-volleyball-blue-slime-maybe-left)))
      (progn
        (if (< slime-volleyball-ball-velocity-x 0)
            (slime-volleyball-blue-slime-maybe-left)
          (slime-volleyball-blue-slime-maybe-right)))))))

;; Arguments are expected values of moves.
(defun slime-volleyball-training-maximum-expected-value
    (left right jump stop none)
  "Calculate expected reward, based on LEFT, RIGHT, JUMP, STOP and NONE values."
  (let* ((max-val (max left right jump stop none))
         (max-list nil) (rand-max nil) (index 0))
    ;; Uncomment to make the learning slime more experimental.
    (if (and slime-volleyball-experimental-slime
             (eq (random 10) 3))
        (elt '((0 slime-volleyball-slime-left)
               (1 slime-volleyball-slime-right)
               (2 slime-volleyball-slime-jump)
               (3 slime-volleyball-slime-stop)
               (4 slime-volleyball-slime-none))
             (random 3))
      (progn
        (dolist (check (list left right jump stop none))
          (when (< (abs (- check max-val)) 0.001)
            (push (list index
                        (elt (list 'slime-volleyball-slime-left
                                   'slime-volleyball-slime-right
                                   'slime-volleyball-slime-jump
                                   'slime-volleyball-slime-stop
                                   'slime-volleyball-slime-none)
                             index)) max-list))
          (setq index (1+ index)))
        (when (> (length max-list) 1)
          (setq rand-max (elt max-list (random (length max-list)))))
        (cond
         (rand-max rand-max)
         ((< (abs (- left max-val)) 0.001)
          '(0 slime-volleyball-slime-left))
         ((< (abs (- right max-val)) 0.001)
          '(1 slime-volleyball-slime-right))
         ((< (abs (- jump max-val)) 0.001)
          '(2 slime-volleyball-slime-jump))
         ((< (abs (- stop max-val)) 0.001)
          '(3 slime-volleyball-slime-stop))
         ((< (abs (- none max-val)) 0.001)
          '(4 slime-volleyball-slime-none)))))))

;; The following is from computer slime's perspective.
;;
;; Notation:
;; lp:  went left, got a point
;; lnp: went left, didn't get a point
;; r: right
;; j: jump
;; s: stop
;; n: none (do nothing)
;;
;; For example:
;;   lp  lnp  rp rnp   jp jnp  sp snp np nnp
;; ((20  30) (40  50) (60 70) (80 90) (0  10))
;;
;; Expected value of going left: lp / (lp + lnp)
(defun slime-volleyball-best-move (tallies)
  "Return the best move, based on TALLIES."
  (let* ((left-points     (car  (elt tallies 0)))
         (left-no-points  (cadr (elt tallies 0)))
         (right-points    (car  (elt tallies 1)))
         (right-no-points (cadr (elt tallies 1)))
         (jump-points     (car  (elt tallies 2)))
         (jump-no-points  (cadr (elt tallies 2)))
         (stop-points     (car  (elt tallies 3)))
         (stop-no-points  (cadr (elt tallies 3)))
         (none-points     (car  (elt tallies 4)))
         (none-no-points  (cadr (elt tallies 4)))
         (left-decisions  (+ left-points left-no-points))
         (right-decisions (+ right-points right-no-points))
         (jump-decisions  (+ jump-points jump-no-points))
         (stop-decisions  (+ stop-points stop-no-points))
         (none-decisions  (+ none-points none-no-points)))
    (slime-volleyball-training-maximum-expected-value
     (/ left-points (float left-decisions))
     (/ right-points (float right-decisions))
     (/ jump-points (float jump-decisions))
     (/ stop-points (float stop-decisions))
     (/ none-points (float none-decisions)))))

;; Dynamically-scoped slime-volleyball-save-strategy helper function.
(defun slime-volleyball-save-strategy-helper (key values strategy-name)
  "Store KEY, VALUES in the strategy hash table named STRATEGY-NAME."
  (insert
   (format "(puthash \"%s\" '%s %s)\n"
           key
           (symbol-name (cadr (slime-volleyball-best-move values)))
           strategy-name)))

(defun slime-volleyball-save-strategy (file-name strategy-name)
  "Save a generated computer slime strategy in FILE-NAME with STRATEGY-NAME."
  (find-file file-name)
  (with-current-buffer (file-name-nondirectory file-name)
    (insert
     (format "(setq %s (make-hash-table :test 'equal))\n" strategy-name))
    (maphash (lambda (key values)
               (slime-volleyball-save-strategy-helper
                key values strategy-name))
             slime-volleyball-training-slime-strategy)
    (save-buffer)))

(defun slime-volleyball-training-quantize (value digits)
  "Return VALUE converted to have DIGITS digits."
  (let ((tens (expt 10.0 digits)))
    (/ (fround (* value tens)) (float tens))))

(defun slime-volleyball-training-hash-situation ()
  "Return the current board situation as a string."
  (let* (;; Ball.
         (ball-x
          (slime-volleyball-training-quantize slime-volleyball-ball-x 0))
         (ball-y
          (slime-volleyball-training-quantize slime-volleyball-ball-y 0))
         (ball-v-x (slime-volleyball-training-quantize
                    slime-volleyball-ball-velocity-x 2))
         (ball-v-y (slime-volleyball-training-quantize
                    slime-volleyball-ball-velocity-y 2))
         ;; Slime 1.
         (slime1-real-x
          (slime-volleyball-training-quantize
           (slime-volleyball-slime-x slime-volleyball-slime1) 0))
         (slime1-real-y
          (slime-volleyball-training-quantize
           (slime-volleyball-slime-y slime-volleyball-slime1) 0))
         (slime1-x (slime-volleyball-training-quantize slime1-real-x 0))
         (slime1-y (slime-volleyball-training-quantize slime1-real-y 0))
         (slime1-v-x (slime-volleyball-training-quantize
                      (slime-volleyball-slime-v-x slime-volleyball-slime1) 2))
         (slime1-v-y (slime-volleyball-training-quantize
                      (slime-volleyball-slime-v-y-f slime-volleyball-slime1) 2))
         ;; Slime 2.
         (slime2-real-x
          (slime-volleyball-training-quantize
           (slime-volleyball-slime-x slime-volleyball-slime2) 0))
         (slime2-real-y
          (slime-volleyball-training-quantize
           (slime-volleyball-slime-y slime-volleyball-slime2) 0))
         (slime2-x (slime-volleyball-training-quantize slime2-real-x 0))
         (slime2-y (slime-volleyball-training-quantize slime2-real-y 0))
         (slime2-v-x (slime-volleyball-training-quantize
                      (slime-volleyball-slime-v-x slime-volleyball-slime2) 2))
         (slime2-v-y (slime-volleyball-training-quantize
                      (slime-volleyball-slime-v-y-f slime-volleyball-slime2)
                      2)))
    (format (concat "%s,"
                    "%0.0f,%0.0f,%0.2f,%0.2f,"
                    "%0.0f,%0.0f,%0.2f,%0.2f,"
                    "%0.0f,%0.0f,%0.2f,%0.2f")
            (equal slime-volleyball-serving-slime slime-volleyball-slime2)
            ball-x ball-y ball-v-x ball-v-y
            slime1-x slime1-y slime1-v-x slime1-v-y
            slime2-x slime2-y slime2-v-x slime2-v-y)))

(defun slime-volleyball-training-slime-controller ()
  "A controller that controls the slime being trained."
  (let* ((situation (slime-volleyball-training-hash-situation))
         (tallies (gethash situation
                           slime-volleyball-training-slime-strategy
                           (list (list 1 1)
                                 (list 1 1)
                                 (list 1 1)
                                 (list 1 1)
                                 (list 1 1))))
         (best-move (slime-volleyball-best-move tallies)))
    (push (list situation (car best-move)) slime-volleyball-moves-list)
    (funcall (cadr best-move) slime-volleyball-slime2)))

(defun slime-volleyball-training-slime-updater (point)
  "Update the slime training session based on slime's location, POINT."
  (let ((index (if point 0 1))
        item value)
    (while (setq item (pop slime-volleyball-moves-list))
      (setq value (gethash (car item) slime-volleyball-training-slime-strategy))
      (when (not value)
        (puthash (car item)
                 (list (list 1 1)
                       (list 1 1)
                       (list 1 1)
                       (list 1 1)
                       (list 1 1))
                 slime-volleyball-training-slime-strategy)
        (setq value (gethash
                     (car item) slime-volleyball-training-slime-strategy)))
      (setf (elt (elt value (cadr item)) index)
            (1+ (elt (elt value (cadr item)) index))))
    0))

(defun slime-volleyball-eval-god-mode-variables ()
  "Re-evaluate God mode global variables."
  (when slime-volleyball-god-mode
    (let ((debug-on-error nil))
      (ignore-errors
        (with-current-buffer "slime-volleyball.el"
          (let (start function-end)
            (save-excursion
              (goto-char (point-min))
              (search-forward-regexp
               "^(defun slime-volleyball-initialize-god-mode-globals"
               (point-max))
              (goto-char (match-beginning 0))
              (forward-sexp)
              (setq function-end (point))
              (backward-sexp)
              (while (< (point) (- function-end 1))
                (search-forward-regexp "(set")
                (setq start (match-beginning 0))
                (goto-char (match-beginning 0))
                (forward-sexp)
                (eval-region start (point))))))))))

(defun slime-volleyball-training-mode ()
  "Train a computer-controlled slime to play slime volleyball."
  (interactive)
  (define-key slime-volleyball-mode-map
    (kbd "<left>")
    (lambda ()
      (interactive)
      (slime-volleyball-slime2-left)
      (push (list (slime-volleyball-training-hash-situation) 0)
            slime-volleyball-moves-list)))
  (define-key slime-volleyball-mode-map
    (kbd "<right>")
    (lambda () (interactive)
      (slime-volleyball-slime2-right)
      (push (list (slime-volleyball-training-hash-situation) 1)
            slime-volleyball-moves-list)))
  (define-key slime-volleyball-mode-map
    (kbd "<up>")
    (lambda () (interactive)
      (slime-volleyball-slime2-jump)
      (push (list (slime-volleyball-training-hash-situation) 2)
            slime-volleyball-moves-list)))
  (define-key slime-volleyball-mode-map
    (kbd "<down>")
    (lambda () (interactive)
      (slime-volleyball-slime2-stop)
      (push (list (slime-volleyball-training-hash-situation) 3)
            slime-volleyball-moves-list)))
  (setq slime-volleyball-training-mode t)
  (setq slime-volleyball-slime2 slime-volleyball-training-slime)
  (slime-volleyball-unpause))

(defun slime-volleyball-two-player-mode ()
  "Start a two player game of slime volleyball."
  (interactive)
  (define-key slime-volleyball-mode-map
    (kbd "<left>") #'slime-volleyball-slime2-left)
  (define-key slime-volleyball-mode-map
    (kbd "<right>") #'slime-volleyball-slime2-right)
  (define-key slime-volleyball-mode-map
    (kbd "<up>") #'slime-volleyball-slime2-jump)
  (define-key slime-volleyball-mode-map
    (kbd "<down>") #'slime-volleyball-slime2-stop)
  (setf (slime-volleyball-slime-controller slime-volleyball-slime2) nil)
  (setf (slime-volleyball-slime-color slime-volleyball-slime2) "Pink")
  (setq slime-volleyball-two-players t)
  (slime-volleyball-unpause))

(defun slime-volleyball-initialize-keymap ()
  "Initialize the slime volleyball mode keymap."
  (setq slime-volleyball-mode-map
        (let ((map (make-keymap)))
          (set-keymap-parent map special-mode-map)
          (define-key map (kbd "a")       #'slime-volleyball-slime1-left)
          (define-key map (kbd "C-b")     #'slime-volleyball-slime1-left)
          (define-key map (kbd "<left>")  #'slime-volleyball-slime1-left)
          (define-key map (kbd "d")       #'slime-volleyball-slime1-right)
          (define-key map (kbd "C-f")     #'slime-volleyball-slime1-right)
          (define-key map (kbd "<right>") #'slime-volleyball-slime1-right)
          (define-key map (kbd "w")       #'slime-volleyball-slime1-jump)
          (define-key map (kbd "C-p")     #'slime-volleyball-slime1-jump)
          (define-key map (kbd "<up>")    #'slime-volleyball-slime1-jump)
          (define-key map (kbd "s")       #'slime-volleyball-slime1-stop)
          (define-key map (kbd "C-n")     #'slime-volleyball-slime1-stop)
          (define-key map (kbd "<down>")  #'slime-volleyball-slime1-stop)
          (define-key map (kbd "G")       #'slime-volleyball-toggle-god-mode)
          (define-key map (kbd "SPC")     #'slime-volleyball-unpause)
          (define-key map (kbd "2")       #'slime-volleyball-two-player-mode)
          (define-key map (kbd "t")       #'slime-volleyball-training-mode)
          (define-key map (kbd "p")       #'slime-volleyball-toggle-pause)
          (define-key map (kbd "q")       #'slime-volleyball-quit)
          (define-key map (kbd "<f8>")
            (lambda () (interactive) (setq slime-volleyball-advance-frame t)))
          (define-key map (kbd "<f9>") #'slime-volleyball-frame-by-frame-mode)
          map)))

;; Something fun to try in god mode.
(defun slime-volleyball-initialize-god-mode-globals ()
  "Initialize God mode global variables."
  ;; Scene.
  (setq slime-volleyball-scene-width 1100)
  (setq slime-volleyball-scene-height 450)
  (setq slime-volleyball-floor-height 100)
  (setq slime-volleyball-scene-total-height
        (+ slime-volleyball-scene-height slime-volleyball-floor-height))
  (setq slime-volleyball-net-height 100)
  (setq slime-volleyball-net-x (- (/ slime-volleyball-scene-width 2)
                                  (/ slime-volleyball-net-width 2)))
  (setq slime-volleyball-net-y (- slime-volleyball-scene-height
                                  slime-volleyball-net-height))

  ;; Points.
  (setq slime-volleyball-point-circles-start-x 45)
  (setq slime-volleyball-point-circles-y 40)
  (setq slime-volleyball-point-circles-radius 22)
  (setq slime-volleyball-points-to-win 5)

  (setq slime-volleyball-slime-radius 90)
  (setq slime-volleyball-ball-radius
        (round (* 0.25 slime-volleyball-slime-radius)))

  ;; Gravity.
  (setq slime-volleyball-force-y 4)

  ;; Chameleon slime.
  (when slime-volleyball-god-mode
    (setf (slime-volleyball-slime-color slime-volleyball-slime1)
          (elt '("Maroon" "Yellow" "Turquoise")
               (setq slime-volleyball-color-index
                     (mod (1+ slime-volleyball-color-index) 3))))))

(defun slime-volleyball-initialize-globals ()
  "Initialize slime volleyball global variables."
  ;; Net.
  ;; This isn't a God mode variable because the intersection calculation uses
  ;; an idealized 1 pixel wide net width.
  (setq slime-volleyball-net-width 20)

  (setq slime-volleyball-god-mode nil)
  (setq slime-volleyball-color-index 0)
  (slime-volleyball-initialize-god-mode-globals)

  ;; Ball.
  (setq slime-volleyball-ball-x 800)
  (setq slime-volleyball-ball-y 790)
  (setq slime-volleyball-ball-bouncy-bouncy nil)

  (setq slime-volleyball-point-scored nil)

  (setq slime-volleyball-quitting nil)
  ;; First level is level 0.
  (setq slime-volleyball-level -1)
  (setq slime-volleyball-play-ending nil)
  (setq slime-volleyball-two-players nil)
  (setq slime-volleyball-one-player-beat-the-game nil)
  (setq slime-volleyball-game-over nil)
  (setq slime-volleyball-advance-frame nil)

  (setq slime-volleyball-training-mode nil)
  (setq slime-volleyball-moves-list nil)

  ;; Slime controller.
  (setq slime-volleyball-blue-slime-serving nil)
  (setq slime-volleyball-blue-slime-orig-x 0)

  (setq slime-volleyball-paused t)
  (setq slime-volleyball-message nil)
  (setq slime-volleyball-unpause-function nil)

  (setq slime-volleyball-frame-by-frame-mode nil)

  (slime-volleyball-initialize-keymap)

  (setq slime-volleyball-time-delta 0.6)

  (setq slime-volleyball-slime1
        (make-slime-volleyball-slime
         :player 1
         :jumping nil
         :points 0
         ;; Human.
         :controller nil
         :updater nil
         :speed 10
         :v-y-i 0
         :v-y-f 0
         :v-x 0
         :x 0
         :y slime-volleyball-scene-height
         :delta-y 0
         :eye-center-x 126
         :eye-center-y -53
         :eye-radius 23
         :pupil-center-x 126
         :pupil-center-y -53
         :pupil-radius 7
         :color "Maroon"))

  (setq slime-volleyball-serving-slime slime-volleyball-slime1)

  (setq slime-volleyball-grey-slime
        (make-slime-volleyball-slime
         :player 2
         :jumping nil
         :points 0
         :controller 'slime-volleyball-grey-slime-controller
         :updater nil
         :speed 10
         :v-y-i 0
         :v-y-f 0
         :v-x 0
         :x 400
         :y slime-volleyball-scene-height
         :delta-y 0
         :eye-center-x 54
         :eye-center-y -53
         :eye-radius 23
         :pupil-center-x 54
         :pupil-center-y -53
         :pupil-radius 7
         :color "Grey"))

  (setq slime-volleyball-blue-slime
        (make-slime-volleyball-slime
         :player 2
         :jumping nil
         :points 0
         :controller 'slime-volleyball-blue-slime-controller
         :updater nil
         :speed 10
         :v-y-i 0
         :v-y-f 0
         :v-x 0
         :x 400
         :y slime-volleyball-scene-height
         :delta-y 0
         :eye-center-x 54
         :eye-center-y -53
         :eye-radius 23
         :pupil-center-x 54
         :pupil-center-y -53
         :pupil-radius 7
         :color "Blue"))

  (setq slime-volleyball-green-slime
        (make-slime-volleyball-slime
         :player 2
         :jumping nil
         :points 0
         :controller 'slime-volleyball-green-slime-controller
         :updater nil
         :speed 10
         :v-y-i 0
         :v-y-f 0
         :v-x 0
         :x 400
         :y slime-volleyball-scene-height
         :delta-y 0
         :eye-center-x 54
         :eye-center-y -53
         :eye-radius 23
         :pupil-center-x 54
         :pupil-center-y -53
         :pupil-radius 7
         :color "Green"))

  (setq slime-volleyball-training-slime
        (make-slime-volleyball-slime
         :player 2
         :jumping nil
         :points 0
         :controller 'slime-volleyball-training-slime-controller
         :updater 'slime-volleyball-training-slime-updater
         :speed 10
         :v-y-i 0
         :v-y-f 0
         :v-x 0
         :x 400
         :y slime-volleyball-scene-height
         :delta-y 0
         :eye-center-x 54
         :eye-center-y -53
         :eye-radius 23
         :pupil-center-x 54
         :pupil-center-y -53
         :pupil-radius 7
         :color "Red"))

  (setq slime-volleyball-training-slime-strategy (make-hash-table :test 'equal))

  (setq slime-volleyball-opponents
        (list slime-volleyball-green-slime
              slime-volleyball-blue-slime
              slime-volleyball-grey-slime))
  (setq slime-volleyball-last-level (1- (length slime-volleyball-opponents)))

  (setq slime-volleyball-title-screen
        (create-image
         (expand-file-name "title-screen.svg" slime-volleyball-base)))
  (setq slime-volleyball-template-header
        (concat "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\""
                " xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">")
        slime-volleyball-template-footer "</svg>"
        slime-volleyball-template-slime
        (concat
         "<g id=\"slime1\" transform=\"translate(%d,%d)\" >"
         "<path d=\"M 0,0 a %d,%d 0 0 1 %d,0 z\""
         " fill=\"%s\" stroke-width=\"0\" />"
         "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"white\" />"
         "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"black/\" />"
         "</g>")
        slime-volleyball-template-ball
        (concat
         "<circle cx=\"%d\" cy=\"%d\" r=\"%d\""
         " fill=\"white\" stroke-width=\"0\" />")
        slime-volleyball-template-dot
        (concat
         "<circle cx=\"%d\" cy=\"%d\" r=\"5\""
         " fill=\"red\" stroke-width=\"0\" />")
        slime-volleyball-template-point-circle
        (concat
         "<circle cx=\"%d\" cy=\"%d\" r=\"%d\""
         " fill=\"white\" fill-opacity=\"%f\" stroke=\"white\""
         " stroke-width=\"2\" />")
        slime-volleyball-template-net
        (concat
         "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\""
         " fill=\"white\" stroke-width=\"0\" />")
        slime-volleyball-template-sky
        "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"#0088ee\" />"
        slime-volleyball-template-ground
        (concat
         "<rect x=\"0\" y=\"%d\" width=\"%d\" height=\"%d\""
         " fill=\""
         (if slime-volleyball-beach-mode "#EEE8AA" "#778899")
         "\" />")
        slime-volleyball-template-message-box-start
        (concat
         "<defs>"
         "<filter id=\"AFilter\" x=\"-10%\" width=\"120%\" y=\"-50%\""
         " height=\"250%\" >"
         "<!-- Definition of filter goes here -->"
         "<feFlood flood-color=\"#444444\" flood-opacity=\"1.0\" />"
         "</filter>"
         "</defs>"
         "<text x=\"50%\" y=\"30%\" fill=\"white\""
         " font-family=\"Liberation Sans\""
         " font-size=\"45\""
         " text-anchor=\"middle\" style=\"filter:url(#AFilter)\">")
        slime-volleyball-template-message-box-middle
        (concat
         "</text>"
         "<text x=\"50%\" y=\"30%\" fill=\"white\" font-size=\"45\""
         " font-family=\"Liberation Sans\""
         " dy=\"0.2em\" text-anchor=\"middle\">")
        slime-volleyball-template-message-box-end
        "</text>")

  (slime-volleyball-initialize-ending-variables))

(defun slime-volleyball-slime-left (slime)
  "Move SLIME left."
  (unless slime-volleyball-paused
    (setf (slime-volleyball-slime-v-x slime)
          (- (slime-volleyball-slime-speed slime)))))

(defun slime-volleyball-slime-stop (slime)
  "Stop moving SLIME."
  (unless slime-volleyball-paused
    (setf (slime-volleyball-slime-v-x slime) 0)))

(defun slime-volleyball-slime-right (slime)
  "Move SLIME right."
  (unless slime-volleyball-paused
    (setf (slime-volleyball-slime-v-x slime)
          (slime-volleyball-slime-speed slime))))

(defun slime-volleyball-slime-jump (slime)
  "Make SLIME jump."
  (unless slime-volleyball-paused
    (if (not (slime-volleyball-slime-jumping slime))
        (progn
          (setf (slime-volleyball-slime-jumping slime) t)
          (setf (slime-volleyball-slime-v-y-i slime) -30)
          (setf (slime-volleyball-slime-v-y-f slime) -30)))))

;; Do not change the slime's movement.
(defun slime-volleyball-slime-none (_slime)
  "Make SLIME do nothing this frame."
  nil)

(defun slime-volleyball-init (slime)
  "Initialize SLIME slots."
  (setf (slime-volleyball-slime-x slime-volleyball-slime1)
        (- (/ slime-volleyball-scene-width 4) slime-volleyball-slime-radius))
  (setf (slime-volleyball-slime-y slime-volleyball-slime1)
        slime-volleyball-scene-height)
  (setf (slime-volleyball-slime-v-x slime-volleyball-slime1) 0)
  (setf (slime-volleyball-slime-x slime-volleyball-slime2)
        (- (* 3 (/ slime-volleyball-scene-width 4))
           slime-volleyball-slime-radius))
  (setf (slime-volleyball-slime-y slime-volleyball-slime2)
        slime-volleyball-scene-height)
  (setf (slime-volleyball-slime-v-x slime-volleyball-slime2) 0)
  (setq slime-volleyball-ball-x (+ (slime-volleyball-slime-x slime)
                                   slime-volleyball-slime-radius)
        slime-volleyball-ball-y (- slime-volleyball-scene-height
                                   (* 2.5 slime-volleyball-slime-radius))
        slime-volleyball-ball-velocity-x 0
        slime-volleyball-ball-velocity-y 0
        slime-volleyball-prev-frame-ball-x (+ (slime-volleyball-slime-x slime)
                                              slime-volleyball-slime-radius)
        slime-volleyball-prev-frame-ball-y 100))

(defun slime-volleyball-warn (warning-message)
  "Issue WARNING-MESSAGE with slime volleyball herald."
  (message "slime-volleyball warning: %s" warning-message))

(defun slime-volleyball-arc-circle-overlap (x1 y1 r1 x2 y2 r2)
  "Return t if an arc defined by X1, Y1 and R1 overlaps circle with X2, Y2, R2."
  (let* ((d-x (abs (- x1 x2)))
         (d-y (abs (- y1 y2)))
         (distance (sqrt (+ (expt d-x 2)
                            (expt d-y 2)))))
    (if (< distance (+ r1 r2))
        (progn
          (when (< (abs distance) 0.1)
            (slime-volleyball-warn "Divide-by-zero danger 1"))
          (list (/ d-x distance) (/ d-y distance)))
      nil)))

(defun slime-volleyball-point-circles-left ()
  "Draw point circles at the top left of the board."
  (let ((circles " "))
    (dotimes (i slime-volleyball-points-to-win)
      (setq circles
            (concat circles
                    (format
                     slime-volleyball-template-point-circle
                     (+ slime-volleyball-point-circles-start-x
                        (* i (* 3 slime-volleyball-point-circles-radius)))
                     slime-volleyball-point-circles-y
                     slime-volleyball-point-circles-radius
                     (if (>= (slime-volleyball-slime-points
                              slime-volleyball-slime1) (1+ i))
                         1.0
                       0.0)))))
    circles))

(defun slime-volleyball-point-circles-right ()
  "Draw point circles at the top right of the board."
  (let ((circles " "))
    (dotimes (i slime-volleyball-points-to-win)
      (setq circles
            (concat circles
                    (format
                     slime-volleyball-template-point-circle
                     (- slime-volleyball-scene-width
                        slime-volleyball-point-circles-start-x
                        (* i (* 3 slime-volleyball-point-circles-radius)))
                     slime-volleyball-point-circles-y
                     slime-volleyball-point-circles-radius
                     (if (>= (slime-volleyball-slime-points
                              slime-volleyball-slime2) (1+ i))
                         1.0
                       0.0)))))
    circles))

(defun slime-volleyball-header (width height)
  "Draw a board header with dimensions WIDTH and HEIGHT."
  (format slime-volleyball-template-header
          width height
          width height))

(defun slime-volleyball-footer ()
  "Draw a board footer."
  slime-volleyball-template-footer)

(defun slime-volleyball-slime (slime x y)
  "Create a vector graphic representing SLIME at X, Y."
  (format slime-volleyball-template-slime
          x y slime-volleyball-slime-radius slime-volleyball-slime-radius
          (* 2 slime-volleyball-slime-radius) (slime-volleyball-slime-color
                                               slime)
          (slime-volleyball-slime-eye-center-x slime)
          (slime-volleyball-slime-eye-center-y slime)
          (slime-volleyball-slime-eye-radius slime)
          (slime-volleyball-slime-pupil-center-x slime)
          (slime-volleyball-slime-pupil-center-y slime)
          (slime-volleyball-slime-pupil-radius slime)))

(defun slime-volleyball-ball (x y)
  "Create a vector graphic representing the ball at X, Y."
  (format slime-volleyball-template-ball
          x y slime-volleyball-ball-radius))

(defun slime-volleyball-net ()
  "Create a vector graphic representing the net."
  (format slime-volleyball-template-net
          slime-volleyball-net-x slime-volleyball-net-y
          slime-volleyball-net-width slime-volleyball-net-height))

(defun slime-volleyball-sky ()
  "Create a vector graphic representing the sky."
  (format slime-volleyball-template-sky slime-volleyball-scene-width
          slime-volleyball-scene-height))

(defun slime-volleyball-ground ()
  "Create a vector graphic representing the ground."
  (format slime-volleyball-template-ground slime-volleyball-scene-height
          slime-volleyball-scene-width slime-volleyball-floor-height))

(defun slime-volleyball-message-box (message-string)
  "Create a vector graphic of a text box containing MESSAGE-STRING."
  (concat slime-volleyball-template-message-box-start
          message-string
          slime-volleyball-template-message-box-middle
          message-string
          slime-volleyball-template-message-box-end))

(defun slime-volleyball-markup ()
  "Generate debugging markup for the scene."
  (concat (format slime-volleyball-template-dot
                  slime-volleyball-prev-frame-ball-x
                  slime-volleyball-prev-frame-ball-y)
          (format slime-volleyball-template-dot
                  slime-volleyball-ball-x
                  slime-volleyball-ball-y)
          (format slime-volleyball-template-dot
                  slime-volleyball-x-f
                  slime-volleyball-y-f)))

(defun slime-volleyball-set-scene ()
  "Concatenate vector graphics for the whole scene."
  (setq slime-volleyball-scene
        (concat
         (slime-volleyball-header slime-volleyball-scene-width
                                  slime-volleyball-scene-total-height)
         (slime-volleyball-sky)
         (slime-volleyball-ground)
         (when slime-volleyball-message
           (slime-volleyball-message-box slime-volleyball-message))
         (slime-volleyball-point-circles-left)
         (slime-volleyball-point-circles-right)
         (slime-volleyball-slime slime-volleyball-slime1
                                 (slime-volleyball-slime-x
                                  slime-volleyball-slime1)
                                 (slime-volleyball-slime-y
                                  slime-volleyball-slime1))
         (slime-volleyball-slime slime-volleyball-slime2
                                 (slime-volleyball-slime-x
                                  slime-volleyball-slime2)
                                 (slime-volleyball-slime-y
                                  slime-volleyball-slime2))
         (slime-volleyball-ball slime-volleyball-ball-x slime-volleyball-ball-y)
         (slime-volleyball-net)
         ;; Debugging.
         ;; (slime-volleyball-markup)
         (slime-volleyball-footer))))

(defun slime-volleyball-initialize-ending-variables ()
  "Initialize variables for ending scene."
  (setq slime-volleyball-offset 3)
  (setq slime-volleyball-ending-rate 10)
  (setq slime-volleyball-ending-num 0)
  (setq slime-volleyball-endvar   "<g
      id=\"layer1\"
      transform=\"scale(0.48) translate(%f,%f)\">
      <rect
      style=\"fill:#ff0000;fill-opacity:1;stroke:none\"
      id=\"rect3817\"
      width=\"105\"
      height=\"29\"
      x=\"113\"
      y=\"180\" />
      <rect
      y=\"208\"
      x=\"113\"
      height=\"29\"
      width=\"105\"
      id=\"use3821\"
      style=\"fill:#ff6600;fill-opacity:1;stroke:none\" />
      <rect
      y=\"237\"
      x=\"113\"
      height=\"29\"
      width=\"105\"
      id=\"use3823\"
      style=\"fill:#ffff00;fill-opacity:1;stroke:none\" />
      <rect
      y=\"266\"
      x=\"113\"
      height=\"29\"
      width=\"105\"
      id=\"use3825\"
      style=\"fill:#00ff00;fill-opacity:1;stroke:none\" />
      <rect
      y=\"295\"
      x=\"113\"
      height=\"29\"
      width=\"105\"
      id=\"use3827\"
      style=\"fill:#0000ff;fill-opacity:1;stroke:none\" />
      <rect
      y=\"324\"
      x=\"113\"
      height=\"29\"
      width=\"105\"
      id=\"use3829\"
      style=\"fill:#bf3799;fill-opacity:1;stroke:none\" />
  </g>")
  (setq slime-volleyball-endvar2
        "<g
      transform=\"scale(0.3) translate(%f,%f)\"
      id=\"layer1\">
      <path
      d=\"M 45.30823,28.75096 64.990229,60.29484 28.836533,51.616844
          4.9185924,80.083133 1.9997496,43.017276 -32.464349,29.06653
          1.8854035,14.836565 4.5033591,-22.251758 28.651517,6.0194973
          64.733601,-2.9516012 z\"
      transform=\"translate(-38.255124,278.92732)\"
      id=\"path3872\"
      style=\"fill:#ffff00;fill-opacity:1;stroke:none\" />
      </g>")
  (setq slime-volleyball-bg1-x 400)
  (setq slime-volleyball-bg1-wrap -80)
  (setq slime-volleyball-bg1-y -30)
  (setq slime-volleyball-bg2-x 900)
  (setq slime-volleyball-bg2-wrap -80)
  (setq slime-volleyball-bg2-y -130)
  (setq slime-volleyball-bg3-x 1200)
  (setq slime-volleyball-bg3-wrap -80)
  (setq slime-volleyball-bg3-y -70)
  (setq slime-volleyball-bg4-x 0)
  (setq slime-volleyball-bg4-wrap -80)
  (setq slime-volleyball-bg4-y -70))

(defun slime-volleyball-set-ending-scene ()
  "Concatenate vector graphics for ending scene."
  (setq slime-volleyball-ending-num (1- slime-volleyball-ending-num))
  (when (<= slime-volleyball-ending-num 0)
    (setq slime-volleyball-offset (- slime-volleyball-offset))
    (setq slime-volleyball-ending-num slime-volleyball-ending-rate))
  (setq slime-volleyball-bg1-x (- slime-volleyball-bg1-x 50))
  (when (<= slime-volleyball-bg1-x slime-volleyball-bg1-wrap)
    (setq slime-volleyball-bg1-y (+ (- 180) (random 880)))
    (setq slime-volleyball-bg1-x (- 1500 slime-volleyball-bg1-wrap)))
  (setq slime-volleyball-bg2-x (- slime-volleyball-bg2-x 50))
  (when (<= slime-volleyball-bg2-x slime-volleyball-bg2-wrap)
    (setq slime-volleyball-bg2-y (+ (- 180) (random 880)))
    (setq slime-volleyball-bg2-x (- 1500 slime-volleyball-bg2-wrap)))
  (setq slime-volleyball-bg3-x (- slime-volleyball-bg3-x 50))
  (when (<= slime-volleyball-bg3-x slime-volleyball-bg3-wrap)
    (setq slime-volleyball-bg3-y (+ (- 180) (random 880)))
    (setq slime-volleyball-bg3-x (- 1500 slime-volleyball-bg2-wrap)))
  (setq slime-volleyball-bg4-x (- slime-volleyball-bg4-x 50))
  (when (<= slime-volleyball-bg4-x slime-volleyball-bg4-wrap)
    (setq slime-volleyball-bg4-y (+ (- 180) (random 880)))
    (setq slime-volleyball-bg4-x (- 1500 slime-volleyball-bg2-wrap)))

  (setq slime-volleyball-scene
        (concat
         (slime-volleyball-header 400
                                  300)
         "<rect x=\"0\" y=\"0\" width=\"400\" height=\"400\" />"
         (format slime-volleyball-endvar2
                 slime-volleyball-bg1-x slime-volleyball-bg1-y)
         (format slime-volleyball-endvar2
                 slime-volleyball-bg2-x slime-volleyball-bg2-y)
         (format slime-volleyball-endvar2
                 slime-volleyball-bg3-x slime-volleyball-bg3-y)
         (format slime-volleyball-endvar2
                 slime-volleyball-bg4-x slime-volleyball-bg4-y)
         (format slime-volleyball-endvar (+ 4 -200) slime-volleyball-offset)
         (format slime-volleyball-endvar (+ 4 -100) (- slime-volleyball-offset))
         (format slime-volleyball-endvar (+ 4 -8) slime-volleyball-offset)
         (format slime-volleyball-endvar (+ 4 92) (- slime-volleyball-offset))
         (format slime-volleyball-endvar (+ 4 192) (- slime-volleyball-offset))
         (slime-volleyball-slime slime-volleyball-slime1 100 172)
         (slime-volleyball-footer))))

(defun slime-volleyball-draw-scene ()
  "Draw the current scene to the screen."
  (when (not slime-volleyball-quitting)
    (with-current-buffer "*slime-volleyball*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image
         (if slime-volleyball-starting
             slime-volleyball-title-screen
           (create-image slime-volleyball-scene 'svg t)))))))

(defun slime-volleyball-render-internal ()
  "Render the entire scene, checking if the game is over or paused."
  (slime-volleyball-check-game-over)
  (unless slime-volleyball-paused
    (slime-volleyball-scene-update))
  (if slime-volleyball-play-ending
      (slime-volleyball-set-ending-scene)
    (slime-volleyball-set-scene))
  (slime-volleyball-draw-scene)
  (clear-image-cache))

(define-minor-mode slime-volleyball-frame-by-frame-mode
  "Stop the real-time behavior and only advance a step at a time upon request.")

(defun slime-volleyball-render-maybe-wrapped ()
  "Render the scene, possibly only the next frame if in frame-by-frame mode."
  (if slime-volleyball-frame-by-frame-mode
      (when slime-volleyball-advance-frame
        (slime-volleyball-render-internal)
        (setq slime-volleyball-advance-frame nil))
    (progn
      (slime-volleyball-render-internal))))

(defun slime-volleyball-render ()
  "Render the scene."
  (if slime-volleyball-god-mode
      (ignore-errors
        (slime-volleyball-render-maybe-wrapped))
    ;; Don't paper over errors when not in god mode.
    (slime-volleyball-render-maybe-wrapped)))

(defun slime-volleyball-update-slime-velocity-and-position (slime time-delta)
  "Update SLIME's position and velocity based on TIME-DELTA having elapsed."
  (setf (slime-volleyball-slime-v-y-f slime)
        (+ (slime-volleyball-slime-v-y-i slime) (* time-delta
                                                   slime-volleyball-force-y)))
  (setf (slime-volleyball-slime-delta-y slime)
        (/ (- (expt (slime-volleyball-slime-v-y-f slime) 2)
              (expt (slime-volleyball-slime-v-y-i slime) 2))
           (* 2 slime-volleyball-force-y)))
  (setf (slime-volleyball-slime-y slime) (+ (slime-volleyball-slime-y slime)
                                            (slime-volleyball-slime-delta-y
                                             slime)))
  (if (> (slime-volleyball-slime-y slime) slime-volleyball-scene-height)
      (progn
        (setf (slime-volleyball-slime-y slime) slime-volleyball-scene-height)
        (setf (slime-volleyball-slime-v-y-f slime) 0)
        (setf (slime-volleyball-slime-jumping slime) nil)))
  (setf (slime-volleyball-slime-x slime) (+ (slime-volleyball-slime-x slime)
                                            (slime-volleyball-slime-v-x
                                             slime))))

;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-update-ball-velocity-and-position (time-delta)
  "Update ball velocity and position based on TIME-DELTA having elapsed."
  (setq slime-volleyball-v-y-f
        (+ slime-volleyball-v-y-i
           (* time-delta slime-volleyball-force-y)))
  (setq slime-volleyball-delta-y
        (/ (- (expt slime-volleyball-v-y-f 2) (expt slime-volleyball-v-y-i 2))
           (* 2 slime-volleyball-force-y)))
  (setq slime-volleyball-y-f (+ slime-volleyball-y-i slime-volleyball-delta-y))
  (setq slime-volleyball-x-f (+ slime-volleyball-x-i
                                (* slime-volleyball-v-x-i time-delta)))
  (setq slime-volleyball-v-x-f slime-volleyball-v-x-i))

;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-ball-slime-overlap (slime)
  "Check if SLIME and the ball overlap."
  (and (slime-volleyball-arc-circle-overlap
        (+ (slime-volleyball-slime-x slime) slime-volleyball-slime-radius)
        (slime-volleyball-slime-y slime)
        slime-volleyball-slime-radius
        slime-volleyball-x-f slime-volleyball-y-f slime-volleyball-ball-radius)
       (< (- slime-volleyball-y-f slime-volleyball-ball-radius)
          (slime-volleyball-slime-y slime))))

;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-net-ball-bounding-box-overlap ()
  "Check if ball (s1 e1) and net (s2 e2) x intervals overlap."
  (let* ((s1 (- slime-volleyball-x-f slime-volleyball-ball-radius))
         (e1 (+ slime-volleyball-x-f slime-volleyball-ball-radius))
         (s2 slime-volleyball-net-x)
         (e2 (+ slime-volleyball-net-x (/ slime-volleyball-net-width 2))))
    (and (< s2 e1) (< s1 e2))))

;; Forget being precise here.  Just treat the ball as a point and the net as a
;; line, then post-adjust to eliminate collision.  This means that sometimes a
;; slime can "cheat" by kicking the ball under the net, but cheaters never
;; prosper.
;;
;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-ball-net-overlap ()
  "Check if the ball and the net overlap."
  (let* ((y slime-volleyball-y-f)
         ;; Previous y
         (py slime-volleyball-ball-y)
         (x slime-volleyball-x-f)
         (px slime-volleyball-ball-x)
         (diff-x (- px x))
         (diff-y (- py y))
         (b)
         (intersection-y)
         ;; Debugging.
         ;; (dir-message)
         (adjust)
         (net-x)
         (fun))
    (if (< (abs diff-x) 0.001)
        nil
      (let* ((m (/ diff-y diff-x)))
        (if (< diff-x 0)
            ;; Ball is going left-to-right.  Check intersection of ball's
            ;; rightmost point with net's leftmost edge.
            (progn
              ;; Debugging.
              ;; (setq dir-message "left")
              (setq x (+ x slime-volleyball-ball-radius))
              (setq adjust (- slime-volleyball-ball-radius))
              (setq net-x slime-volleyball-net-x)
              ;; y = m*x + b
              ;; py = m*px + b
              ;; y + py = m * (x + px) + 2b
              ;; (y + py - m * (x + px)) / 2 = b
              (setq b (/ (- (+ y py) (* m (+ x px))) 2))
              (setq fun (symbol-function '>)))
          ;; Ball is going right-to-left.  Check ball's leftmost point against
          ;; net's rightmost edge.
          (progn
            ;; Debugging.
            ;; (setq dir-message "right")
            (setq x (- x slime-volleyball-ball-radius))
            (setq adjust slime-volleyball-ball-radius)
            (setq net-x (+ slime-volleyball-net-x (/ slime-volleyball-net-width
                                                     2)))
            (setq b (/ (- (+ y py) (* m (+ x px))) 2))
            (setq fun (symbol-function '<))))
        (if (and (< slime-volleyball-net-y (+ y slime-volleyball-ball-radius))
                 (> slime-volleyball-net-y y)
                 (slime-volleyball-net-ball-bounding-box-overlap))
            ;; Handle top-of-net bounce.
            (progn
              ;; Debugging.
              ;; (message "%s bounce" dir-message)
              (when (< diff-y 0)
                (setq slime-volleyball-y-f (- slime-volleyball-net-y
                                              slime-volleyball-ball-radius))
                (setq slime-volleyball-v-y-f (- slime-volleyball-v-y-f))))
          ;; No top-of-net bounce.
          (progn
            ;; Check intersection with net's vertical center line.
            (setq intersection-y (+ (* m net-x) b))
            (if (and (> (+ intersection-y slime-volleyball-ball-radius)
                        slime-volleyball-net-y)
                     (< (+ intersection-y slime-volleyball-ball-radius)
                        slime-volleyball-scene-height))
                (if (and (funcall fun x net-x)
                         (slime-volleyball-net-ball-bounding-box-overlap))
                    (progn
                      ;; Debugging.
                      ;; (message "%s overlap" dir-message)
                      (setq slime-volleyball-x-f (+ net-x adjust))
                      (setq slime-volleyball-v-x-f
                            (- slime-volleyball-v-x-f)))))))))))

;; Calculate the line between last non-overlap frame position and new
;; position, then move the ball along that line until it just touches
;; the slime.  This misses the slime's movement during that time, but
;; it should be small enough not to matter.
;;
;; Want intersection between the line between old frame ball position
;; and new frame ball position, and circle at slime center with
;; radius slime-radius + ball radius, then put the ball there.
;;
;; x^2 + y^2 = r^2
;; y = a*x + b
;;
;; x^2 + (a*x + b)^2 = r^2
;; x^2 + (a^2*x^2 + 2a*x*b + b^2) = r^2
;; x^2 + a^2*x^2 + 2ab*x + b^2 - r^2 = 0
;; (a^2 + 1) * x^2 + 2ab*x + b^2 - r^2 = 0
;;
;; x = -B +/- sqrt(B^2 - 4AC)/2A (quadratic formula)
;;
;; A = a^2 + 1
;; B = 2ab
;; C = b^2 - r^2
;;
;; x = (-2ab +/- sqrt((2ab)^2 - 4*(a^2 + 1)*(b^2 - r^2)))
;;     / 2 ((a^2 + 1))
;;
;; y1 = a*x1 + b
;; y2 = a*x2 + b
;; b = y1 - a*x1
;; y2 = a*x2 + y1 - a*x1
;; y2 - y1 = a*(x2-x1)
;; (y2 - y1)/(x2-x1) = a
;; y1 - a*x1 = b
;;
;; I tried a more physically accurate collision response model based on
;; conservation of momentum, but the result wasn't as fun.
;;
;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-resolve-collision (slime)
  "Resolve a collision between SLIME and another object."
  (let* (;; Convert to slime center co-ordinate system.
         ;; Slime center is (0, 0).
         (slime-x (+ (slime-volleyball-slime-x slime)
                     slime-volleyball-slime-radius))
         (slime-y (slime-volleyball-slime-y slime))
         (ball-x1 (- slime-volleyball-prev-frame-ball-x slime-x))
         (ball-x2 (- slime-volleyball-x-f slime-x))
         (ball-y1 (- slime-y slime-volleyball-prev-frame-ball-y))
         (ball-y2 (- slime-y slime-volleyball-y-f))
         ;; 1- for a slight overlap.
         (r (1- (+ slime-volleyball-ball-radius slime-volleyball-slime-radius)))
         (a)
         (b))
    (if (< (abs (- ball-y1 ball-y2)) 0.001)
        (if (< (abs (- ball-x1 ball-x2)) 0.001)
            ;; No movement! Warn.
            (progn
              (slime-volleyball-warn
               "Previous frame had a collision, shouldn't get here")
              (setq a nil)
              (setq b nil))
          ;; Horizontal movement.
          (progn
            (setq a 0)
            (setq b ball-y1)))
      (if (< (abs (- ball-x1 ball-x2)) 0.001)
          ;; Vertical movement.
          (progn
            ;; Infinite slope.
            (setq a nil)
            (setq b 0))
        ;; Normal movement.
        (progn
          (when (< (abs (- ball-x2 ball-x1)) 0.1)
            (slime-volleyball-warn "Divide-by-zero danger 2"))
          (setq a (/ (- ball-y2 ball-y1) (- ball-x2 ball-x1)))
          (setq b (- ball-y1 (* a ball-x1))))))
    (if (eq a nil)
        (progn
          ;; slime-volleyball-x-f stays the same.
          (setq slime-volleyball-y-f
                (- slime-y
                   (abs (sqrt (- (expt r 2)
                                 (expt ball-x2 2)))))))
      ;; x = (-2ab +/- sqrt((2ab)^2 - 4*(a^2 + 1)*(b^2 - r^2)))
      ;;     / 2 ((a^2 + 1))
      (let* ((minus-2ab (- (* 2 a b)))
             (determinant
              (sqrt (- (expt (* 2 a b) 2)
                       (* 4 (+ (expt a 2) 1)
                          (- (expt b 2) (expt r 2))))))
             (divisor (* 2 (+ (expt a 2) 1)))
             x1 x2 y1 y2)
        (when (< (abs determinant) 0.1)
          (slime-volleyball-warn "Divide-by-zero danger 3"))
        (setq x1 (/ (+ minus-2ab determinant) divisor))
        (setq x2 (/ (- minus-2ab determinant) divisor))
        (setq y1 (+ (* a x1) b))
        (setq y2 (+ (* a x2) b))
        (if (> (- ball-x2 ball-x1) 0)
            (progn
              ;; Debugging.
              ;; (message "positive bounce")
              (setq slime-volleyball-x-f (+ slime-x x2))
              (setq slime-volleyball-y-f (- slime-y y2)))
          (progn
            ;; Debugging.
            ;; (message "negative bounce")
            (setq slime-volleyball-x-f (+ slime-x x1))
            (setq slime-volleyball-y-f (- slime-y y1))))))))

(defun slime-volleyball-adjust-slime-pupils (slime)
  "Keep SLIME's eye on the ball."
  (let* ((slime-eye-x (+ (slime-volleyball-slime-x slime)
                         (slime-volleyball-slime-eye-center-x slime)))
         (slime-eye-y (+ (slime-volleyball-slime-y slime)
                         (slime-volleyball-slime-eye-center-y slime)))
         (diff-x (- slime-eye-x slime-volleyball-ball-x))
         (diff-y (- slime-eye-y slime-volleyball-ball-y))
         (ball-distance (sqrt (+ (expt diff-x 2)
                                 (expt diff-y 2)))))
    (when (< (abs ball-distance) 0.1)
      (slime-volleyball-warn "Divide-by-zero danger 4"))
    (setf (slime-volleyball-slime-pupil-center-x slime)
          (- (slime-volleyball-slime-eye-center-x slime)
             (* (/ diff-x ball-distance)
                (- (slime-volleyball-slime-eye-radius slime)
                   (slime-volleyball-slime-pupil-radius slime)))))
    (setf (slime-volleyball-slime-pupil-center-y slime)
          (- (slime-volleyball-slime-eye-center-y slime)
             (sqrt (- (expt (- (slime-volleyball-slime-eye-radius slime)
                               (slime-volleyball-slime-pupil-radius slime))
                            2)
                      (expt (- (slime-volleyball-slime-pupil-center-x slime)
                               (slime-volleyball-slime-eye-center-x slime))
                            2)))))))

;; Dynamically-scoped slime-volleyball-scene-update helper function.
(defun slime-volleyball-adjust-ball-velocity-and-position-for-collisions (slime)
  "Adjust ball velocity and position if it has collided with SLIME."
  (if (slime-volleyball-ball-slime-overlap slime)
      (let (factor factor-sign)
        ;; Overlap detected.
        (slime-volleyball-resolve-collision slime)
        ;; Do bounce.
        (setq factor (/ (- slime-volleyball-x-f
                           (+ (slime-volleyball-slime-x slime)
                              slime-volleyball-slime-radius))
                        (+ slime-volleyball-slime-radius
                           slime-volleyball-ball-radius)))
        (setq factor-sign (< factor 0.0))
        (setq factor (abs factor))
        (cond
         ((< factor 0.0001)
          (setq factor 0))
         ((< factor 0.2)
          (setq factor 0.2))
         ((and (>= factor 0.2) (< factor 0.5))
          (setq factor 0.4))
         ((>= 0.5 factor)
          (setq factor 0.6)))
        (when factor-sign
          (setq factor (- factor)))

        (setq slime-volleyball-v-x-f (* factor 80))
        (setq slime-volleyball-v-y-f (- 30)))
    ;; Floor.
    (if (> (+ slime-volleyball-y-f slime-volleyball-ball-radius)
           slime-volleyball-scene-height)
        (when slime-volleyball-ball-bouncy-bouncy
          (progn
            (setq slime-volleyball-y-f (- slime-volleyball-scene-height
                                          slime-volleyball-ball-radius))
            (when (> slime-volleyball-v-y-f 0)
              (setq slime-volleyball-v-y-f -50)))))
    ;; Walls.
    (if (eq (slime-volleyball-slime-player slime) 1)
        ;; Left slime.
        (if (< (slime-volleyball-slime-x slime) 0)
            (setf (slime-volleyball-slime-x slime) 0)
          (if (> (+ (slime-volleyball-slime-x slime)
                    (* 2 slime-volleyball-slime-radius))
                 slime-volleyball-net-x)
              (setf (slime-volleyball-slime-x slime)
                    (- slime-volleyball-net-x
                       (* 2 slime-volleyball-slime-radius)))))
      ;; Right slime.
      (if (> (+ (slime-volleyball-slime-x slime)
                (* 2 slime-volleyball-slime-radius))
             slime-volleyball-scene-width)
          (setf (slime-volleyball-slime-x slime)
                (- slime-volleyball-scene-width
                   (* 2 slime-volleyball-slime-radius)))
        (if (< (slime-volleyball-slime-x slime)
               (+ slime-volleyball-net-x slime-volleyball-net-width))
            (setf (slime-volleyball-slime-x slime)
                  (+ slime-volleyball-net-x slime-volleyball-net-width)))))
    (cond
     ((> slime-volleyball-x-f
         (- slime-volleyball-scene-width slime-volleyball-ball-radius))
      (progn
        (setq slime-volleyball-v-x-f (- slime-volleyball-v-x-f))
        (setq slime-volleyball-x-f (- slime-volleyball-scene-width
                                      slime-volleyball-ball-radius))))
     ((< slime-volleyball-x-f slime-volleyball-ball-radius)
      (progn
        (setq slime-volleyball-v-x-f (- slime-volleyball-v-x-f))
        (setq slime-volleyball-x-f slime-volleyball-ball-radius))))))

(defun slime-volleyball-report-point-and-reset-slimes ()
  "Report that a point has been scored and reset the board."
  (slime-volleyball-init slime-volleyball-serving-slime))

(defun slime-volleyball-new-game ()
  "Start a new game."
  (setq slime-volleyball-level -1)
  (slime-volleyball-next-level)
  (setf (slime-volleyball-slime-points slime-volleyball-slime1) 0)
  (mapc (lambda (opponent)
          (setf (slime-volleyball-slime-points opponent) 0))
        slime-volleyball-opponents)
  (slime-volleyball-init slime-volleyball-slime1))

(defun slime-volleyball-next-level ()
  "Advance player to the next level."
  (setq slime-volleyball-level (1+ slime-volleyball-level))
  (setq slime-volleyball-slime2 (elt slime-volleyball-opponents
                                     slime-volleyball-level))
  (setf (slime-volleyball-slime-points slime-volleyball-slime1) 0)
  (slime-volleyball-init slime-volleyball-slime1)
  (slime-volleyball-introduce-opponent))

(defun slime-volleyball-check-win (slime)
  "Check if SLIME has won the game."
  (when (eq (slime-volleyball-slime-points slime)
            slime-volleyball-points-to-win)
    (slime-volleyball-pause (format
                             "%s Slime is Victorious!"
                             (slime-volleyball-slime-color slime)))
    (if (and (equal slime slime-volleyball-slime1)
             (not slime-volleyball-two-players))
        (if (>= slime-volleyball-level slime-volleyball-last-level)
            (progn
              (setq slime-volleyball-game-over t)
              (setq slime-volleyball-one-player-beat-the-game t))
          (setq slime-volleyball-unpause-function
                'slime-volleyball-next-level))
      (setq slime-volleyball-game-over t))))

(defun slime-volleyball-play-ending ()
  "Play the ending of the game."
  (progn
    (define-key slime-volleyball-mode-map (kbd "SPC") nil)
    (define-key slime-volleyball-mode-map (kbd "p") nil)
    (setq slime-volleyball-play-ending t)
    (when slime-volleyball-enable-sound
      (slime-volleyball-play-music "end" t))))

(defun slime-volleyball-say-game-over ()
  "Tell the player that the game is over."
  (setq slime-volleyball-unpause-function
        'slime-volleyball-new-game)
  (slime-volleyball-pause "Game Over!"))

(defun slime-volleyball-check-game-over ()
  "Check if the game is over."
  (when slime-volleyball-game-over
    (setq slime-volleyball-game-over nil)
    (if slime-volleyball-two-players
        (setq slime-volleyball-unpause-function 'slime-volleyball-new-game)
      ;; One player mode.
      (if slime-volleyball-one-player-beat-the-game
          (setq slime-volleyball-unpause-function
                'slime-volleyball-play-ending)
        (setq slime-volleyball-unpause-function
              'slime-volleyball-say-game-over)))))

(defun slime-volleyball-debug-dump ()
  "Print a debugging message."
  (message "slime-volleyball-prev-frame-ball-x %f
            slime-volleyball-prev-frame-ball-y %f
            slime-volleyball-ball-x            %f
            slime-volleyball-ball-y            %f
            slime-volleyball-ball-velocity-x   %f
            slime-volleyball-ball-velocity-y   %f
            slime-volleyball-x-f               %f
            slime-volleyball-y-f               %f
            slime-volleyball-v-x-i             %f
            slime-volleyball-v-x-f             %f
            slime-volleyball-v-y-f             %f
            slime-volleyball-slime1            %s
            slime-volleyball-slime2            %s"
           slime-volleyball-prev-frame-ball-x slime-volleyball-prev-frame-ball-y
           slime-volleyball-ball-x slime-volleyball-ball-y
           slime-volleyball-ball-velocity-x slime-volleyball-ball-velocity-y
           slime-volleyball-x-f slime-volleyball-y-f
           slime-volleyball-v-x-i slime-volleyball-v-x-f slime-volleyball-v-y-f
           slime-volleyball-slime1 slime-volleyball-slime2))

(defun slime-volleyball-scene-update ()
  "Update the scene."
  (let ((slime-volleyball-y-i slime-volleyball-ball-y)
        (slime-volleyball-v-y-i slime-volleyball-ball-velocity-y)
        (slime-volleyball-delta-y)
        (slime-volleyball-v-y-f)
        (slime-volleyball-y-f)
        (slime-volleyball-x-i slime-volleyball-ball-x)
        (slime-volleyball-v-x-i slime-volleyball-ball-velocity-x)
        (slime-volleyball-v-x-f)
        (slime-volleyball-x-f slime-volleyball-ball-x)
        (point-for)
        (slime1 slime-volleyball-slime1)
        (slime2 slime-volleyball-slime2))
    (setf (slime-volleyball-slime-v-y-i slime1)
          (slime-volleyball-slime-v-y-f slime1))
    (setf (slime-volleyball-slime-v-y-i slime2)
          (slime-volleyball-slime-v-y-f slime2))
    (slime-volleyball-update-slime-velocity-and-position
     slime1 slime-volleyball-time-delta)
    (slime-volleyball-update-slime-velocity-and-position
     slime2 slime-volleyball-time-delta)
    (when (slime-volleyball-slime-controller slime2)
      (funcall (slime-volleyball-slime-controller slime2)))
    (slime-volleyball-update-ball-velocity-and-position
     slime-volleyball-time-delta)
    (slime-volleyball-adjust-ball-velocity-and-position-for-collisions slime1)
    (slime-volleyball-adjust-ball-velocity-and-position-for-collisions slime2)
    (slime-volleyball-ball-net-overlap)
    (slime-volleyball-adjust-slime-pupils slime1)
    (slime-volleyball-adjust-slime-pupils slime2)

    (if (and (not slime-volleyball-just-rally)
             (> (+ slime-volleyball-ball-y slime-volleyball-ball-radius 1)
                slime-volleyball-scene-height))
        ;; Score a point.
        (progn
          (if (> slime-volleyball-ball-x (/ slime-volleyball-scene-width 2))
              (progn
                (setq point-for slime1)
                (if (slime-volleyball-slime-updater slime2)
                    ;; If we're in training mode don't keep track of points.
                    (funcall (slime-volleyball-slime-updater slime2) nil)
                  (setf (slime-volleyball-slime-points slime1)
                        (1+ (slime-volleyball-slime-points slime1)))))
            (progn
              (setq point-for slime-volleyball-slime2)
              (if (slime-volleyball-slime-updater slime2)
                  ;; If we're in training mode don't keep track of points.
                  (funcall (slime-volleyball-slime-updater slime2) t)
                (setf (slime-volleyball-slime-points slime-volleyball-slime2)
                      (1+ (slime-volleyball-slime-points
                           slime-volleyball-slime2))))))
          (when (not (slime-volleyball-check-win point-for))
            (slime-volleyball-pause (format "%s Slime Scored a Point!"
                                            (slime-volleyball-slime-color
                                             point-for)))
            (setq slime-volleyball-serving-slime point-for)
            (setq slime-volleyball-unpause-function
                  'slime-volleyball-report-point-and-reset-slimes)))
      ;; No point scored, continue.
      (progn
        ;; Debugging.
        ;;(slime-volleyball-debug-dump)

        ;; Sometimes slime-volleyball-v-x-f becomes nan.  I'm not sure
        ;; why; I can't replicate it after-the-fact by plugging in all
        ;; the slime and ball location and velocity data.  Just detect
        ;; the condition and work around it here.
        (when (and (floatp slime-volleyball-v-x-f)
                   (isnan slime-volleyball-v-x-f))
          (slime-volleyball-warn
           "slime-volleyball-v-x-f isnan condition detected")
          (setq slime-volleyball-v-x-f slime-volleyball-ball-velocity-y))
        (when (and (floatp slime-volleyball-x-f) (isnan slime-volleyball-x-f))
          (slime-volleyball-warn
           "slime-volleyball-x-f isnan condition detected")
          (setq slime-volleyball-x-f slime-volleyball-ball-x))
        (when (and (floatp slime-volleyball-y-f) (isnan slime-volleyball-y-f))
          (slime-volleyball-warn
           "slime-volleyball-y-f isnan condition detected")
          (setq slime-volleyball-y-f slime-volleyball-ball-y))
        (setq slime-volleyball-prev-frame-ball-x slime-volleyball-ball-x)
        (setq slime-volleyball-prev-frame-ball-y slime-volleyball-ball-y)
        (setq slime-volleyball-ball-velocity-x slime-volleyball-v-x-f)
        (setq slime-volleyball-ball-x slime-volleyball-x-f)
        (setq slime-volleyball-ball-velocity-y slime-volleyball-v-y-f)
        (setq slime-volleyball-ball-y slime-volleyball-y-f)
        (if (> (+ slime-volleyball-ball-y slime-volleyball-ball-radius 1)
               slime-volleyball-scene-height)
            (setq slime-volleyball-ball-y (- slime-volleyball-scene-height
                                             slime-volleyball-ball-radius)))))))

(defun slime-volleyball-pause (&optional message)
  "Pause the game, issuing MESSAGE if it is provided."
  (setq slime-volleyball-paused t)
  (setq slime-volleyball-message (or message "Paused")))

(defun slime-volleyball-unpause ()
  "Unpause the game."
  (interactive)
  (when slime-volleyball-paused
    (setq slime-volleyball-paused nil)
    (setq slime-volleyball-message nil)
    (define-key slime-volleyball-mode-map (kbd "2") nil)
    (define-key slime-volleyball-mode-map (kbd "t") nil)
    (when slime-volleyball-unpause-function
      (funcall slime-volleyball-unpause-function))))

(defun slime-volleyball-toggle-god-mode ()
  "Toggle God mode on or off."
  (interactive)
  (if slime-volleyball-god-mode
      (progn
        (delete-other-windows)
        (setq slime-volleyball-god-mode nil))
    (progn
      (split-window nil nil 'above)
      (other-window 1)
      (find-file (symbol-file 'slime-volleyball-init))
      (goto-char (point-min))
      (search-forward-regexp
       "^(defun slime-volleyball-initialize-god-mode-globals"
       (point-max))
      (forward-line)
      (setq slime-volleyball-god-mode t))))

(defun slime-volleyball-toggle-pause ()
  "Pause or unpause the game."
  (interactive)
  (if slime-volleyball-paused
      (slime-volleyball-unpause)
    (slime-volleyball-pause)))

(defun slime-volleyball-add-timer (interval timer-function)
  "Add a timer to call every INTERVAL TIMER-FUNCTION."
  (let ((add t))
    (dolist (timer timer-list)
      (if (eq (elt timer 5) timer-function)
          (setq add nil)))
    (when add
      (setq slime-volleyball-animation-timer (run-at-time nil interval
                                                          timer-function)))))

(defun slime-volleyball-scrub-timer-list (timer-function)
  "Remove TIMER-FUNCTION from timer list."
  (dolist (timer timer-list)
    (when (eq (elt timer 5) timer-function)
      (cancel-timer timer))))

(defun slime-volleyball-play-music (name repeat)
  "Play sound clip NAME, repeating indefinitely if REPEAT is non-nil."
  (when slime-volleyball-enable-sound
    (setq slime-volleyball-music-player-process
          (ignore-errors
            (start-process
             (concat "slime-volleyball-" name "-music")
             nil "ogg123" (if repeat "-r" "")
             (expand-file-name
              (concat "slime-volleyball-" name ".ogg")
              slime-volleyball-base))))))

(defun slime-volleyball-introduce-opponent ()
  "Display a message introducing a computer-controlled opponent slime."
  (when (and (not slime-volleyball-two-players)
             (not slime-volleyball-training-mode))
    (slime-volleyball-pause (format "Facing Off Against %s Slime!"
                                    (slime-volleyball-slime-color
                                     (elt slime-volleyball-opponents
                                          slime-volleyball-level))))
    (setq slime-volleyball-unpause-function nil)))

(define-derived-mode slime-volleyball-mode special-mode "Slime-Volleyball"
  "Major mode for the `slime-volleyball' buffer."
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (slime-volleyball-quit 'force-quit 'no-kill))
            nil 'local))

;;;###autoload
(defun slime-volleyball ()
  "Start a slime volleyball game."
  (interactive)
  (unless (image-type-available-p 'svg)
    (error "Sorry, this Emacs does not support SVG images"))
  (setq slime-volleyball-starting t)
  (message "Loading slime strategies...")
  (load-file (expand-file-name "grey-slime.el.gz" slime-volleyball-base))
  (load-file (expand-file-name "green-slime.el.gz" slime-volleyball-base))
  (pop-to-buffer-same-window (get-buffer-create "*slime-volleyball*"))
  ;; These aren't buffer local so that changing slime-volleyball
  ;; global variables from anywhere in Emacs, e.g., in the *scratch*
  ;; buffer, will work.
  (slime-volleyball-initialize-globals)
  (slime-volleyball-mode)
  (slime-volleyball-new-game)
  (slime-volleyball-scene-update)
  (slime-volleyball-add-timer 0.03 #'slime-volleyball-render)
  (slime-volleyball-add-timer 0.5 #'slime-volleyball-eval-god-mode-variables)
  (sit-for 0.1)
  (slime-volleyball-play-music "start" nil)
  (sleep-for 4)
  (ignore-errors (kill-process slime-volleyball-music-player-process))
  (setq slime-volleyball-starting nil)
  (setq slime-volleyball-unpause-function
        #'slime-volleyball-introduce-opponent)
  (slime-volleyball-pause "Press SPC or 2 to Start"))

(defun slime-volleyball-quit (&optional force-quit no-kill)
  "Quit a slime volleyball game.
If FORCE-QUIT is specified, do not prompt before quitting.  If
NO-KILL is specified, do not kill the *slime-volleyball* buffer."
  (interactive)
  (when (or force-quit
            (y-or-n-p "Quit Slime Volleyball? "))
    (setq slime-volleyball-quitting t)
    (ignore-errors (kill-process slime-volleyball-music-player-process))
    (slime-volleyball-scrub-timer-list 'slime-volleyball-render)
    (slime-volleyball-scrub-timer-list
     'slime-volleyball-eval-god-mode-variables)
    (when (and (not no-kill)
               (get-buffer "*slime-volleyball*"))
      (kill-buffer "*slime-volleyball*")
      (clear-image-cache))))

(provide 'slime-volleyball)

;;; slime-volleyball.el ends here

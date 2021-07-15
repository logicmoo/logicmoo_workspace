;;; flylisp.el --- Color unbalanced parentheses and parentheses inconsistent with indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Barry O'Reilly <gundaetiapo@gmail.com>
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
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

;; Colors mismatched open parentheses with fl-mismatched-face, red by
;; default.
;;
;; Also colors open and close parentheses which are inconsistent with
;; the indentation of lines between them with fl-inconsistent-face,
;; orange by default. This is useful for the Lisp programmer who
;; infers a close paren's location from the open paren and
;; indentation. The coloring serves as a warning that the indentation
;; misleads about where the close paren is. It may also help to
;; localize the mistake, whether due to a misindented line or a
;; misplaced paren.
;;
;; As an example, consider:
;;
;;   (aaa (bbb "word-a
;;   word-b" (ccc 1
;;                2)
;;        fff))
;;
;; (aaa ...) and (ccc ...) are consistent, so are not colored.
;; (bbb ...) is inconsistent because the indentation of fff is
;; inconsistent with the actual location of the close paren. The open
;; and close paren are thus colored with the fl-inconsistent-face.
;; This example also shows that multi line strings don't cause an
;; inconsistency.
;;
;; Currently, the package only detects close parens that are after the
;; place indentation would predict. A planned feature is to also
;; indicate when the close paren is before.
;;
;; Also planned is to color mismatched close parens.

;;; Code:

;; TODO: There are display problems with mismatched parens, due to the
;; region not expanding enough, in turn due to an apparent syntax-ppss
;; bug. See Emacs bug 16247.

;; TODO: Algorithm doesn't account for close paren which is too soon.
;;
;; (abc
;;   (def))
;;   (ghi)
;;
;; (abc ...) are inconsistent parens because (ghi) is indented too far

;; TODO: implement mismatched close parens

;; TODO: Write tests:
;;
;;   ;; Expect (abc ...) is consistent, (def ...) is inconsistent:
;;   (abc a-symbol (a-func-call "word-a
;;   word-b" (def ghi
;;           jkl)
;;
;;   ;; Expect (when ...) is inconsistent:
;;   (when (and t
;;         nil))
;;   ;; After change, expect (when ...) is consistent and last paren mismatched:
;;   (when (and t)
;;         nil))
;;
;;   Given (a ...) inconsistent, change to (a ...(), and verify close
;;   paren is consistent.

(require 'cl-lib)
(require 'jit-lock)

(defgroup flylisp nil
  "Color unbalanced parentheses and parentheses inconsistent with indentation."
  :prefix "flylisp-"
  :group 'paren-matching)

(defgroup flylisp-faces nil
  "Faces for flylisp package. "
  :group 'flylisp
  :group 'faces)

(defface fl-inconsistent-face
  '((((class color) (background light))
     :foreground "dark orange")
    (((class color) (background dark))
     :foreground "orange"))
  "Face applied to matching open and close parens whose placement
is inconsistent with indentation."
  :group 'flylisp-faces)

(defface fl-mismatched-face
  '((((class color) (background light))
     :foreground "dark red")
    (((class color) (background dark))
     :foreground "red"))
  "Face applied to a paren who has no match."
  :group 'flylisp-faces)

;; An open paren and algorithmic data about it.
;;
;; position is the position in the buffer of the open paren
;;
;; close is one of:
;;   - nil if unknown
;;   - the position before the matching close paren
;;   - the symbol 'mismatched if no matching close paren exists
;;
;; column is the displayed column of the open paren in its logical
;; line of the buffer
;;
;; inconsistent is whether the open paren's close paren is
;; inconsistent with the indentation within the list defined by the
;; parens. It is one of:
;;   - nil if unknown or consistent
;;   - an integer offset from the open position to the position of the
;;     first inconsistency. This offset is also cached in the open
;;     paren text properties for performance.
(cl-defstruct fl--Open position close column inconsistent)

(defsubst fl--colorize-inconsistent (open-obj)
  "Colorize the fl--Open OPEN-OBJ as inconsistent."
  (add-text-properties (fl--Open-position open-obj)
                       (1+ (fl--Open-position open-obj))
                       `(fl-inconsistency
                         ,(fl--Open-inconsistent open-obj)
                         font-lock-face
                         fl-inconsistent-face
                         rear-nonsticky
                         t))
  (add-text-properties (fl--Open-close open-obj)
                       (1+ (fl--Open-close open-obj))
                       `(font-lock-face
                         fl-inconsistent-face
                         rear-nonsticky
                         t)))

(defsubst fl--line-check-opens (open-stack)
  "Check fl--Open objects of the OPEN-STACK list for
consistency.

The inconsistent==nil elements of OPEN-STACK must have columns
that are strictly decreasing moving towards the tail (a necessary
but not sufficient condition for being consistent). The
implementation optimizes on this assumption.

Call with point on the line being checked; puts point on the next
line or EOB."
  (let ((indent-pos (progn (back-to-indentation)
                           (point)))
        (indent-column (current-column))
        (line-end (progn (end-of-line)
                         (point))))
    ;; Assess open-objs against indent-column
    (unless (eq indent-pos line-end) ; Skip whitespace lines
      ;; Since we're only interested in marking Opens inconsistent,
      ;; the open-stack's documented property allows the iteration to
      ;; stop at the first inconsistent==nil Open with small enough
      ;; column.
      (while (and open-stack
                  (or (fl--Open-inconsistent (car open-stack))
                      (<= indent-column
                          (fl--Open-column (car open-stack)))))
        ;; Check fl--Open-inconsistent to avoid excessive
        ;; syntax-ppss when there's a lot of bad
        ;; indentation.
        (unless (or (fl--Open-inconsistent (car open-stack))
                    ;; Multi line strings don't cause inconsistency
                    (nth 3 (syntax-ppss indent-pos)))
          (setf (fl--Open-inconsistent (car open-stack))
                (- indent-pos (fl--Open-position (car open-stack)))))
        (pop open-stack)))
    ;; Go to next line. Since we already know line-end, use it
    ;; instead of rescanning the line
    ;;
    ;; goto-char tolerates going beyond EOB
    (goto-char (1+ line-end))))

(defsubst fl--region-check-opens (downward-objs
                                  upward-objs)
  "Check inputted parens in a region for inconsistency, first
going down in sexp depth then up per the DOWNWARD-OBJS and
UPWARD-OBJS.

Point must be at the start of the region to process and will end
up near the end.

DOWNWARD-OBJS is a list of fl--Open objects. Each must be a
parent of the next in the list.

UPWARD-OBJS is a list of fl--Open objects. Each must be a child
of the next in the list."
  (while downward-objs
    (fl--line-check-opens upward-objs)
    (while (and downward-objs
                (< (fl--Open-position (car downward-objs))
                   (point)))
      (push (pop downward-objs)
            upward-objs)))
  (while (and upward-objs
              (number-or-marker-p (fl--Open-close (car upward-objs))))
    (fl--line-check-opens upward-objs)
    (while (and upward-objs
                (number-or-marker-p (fl--Open-close (car upward-objs)))
                (< (fl--Open-close (car upward-objs))
                   (point)))
      (pop upward-objs))))

(defsubst fl--set-closes (open-obj-list)
  "Sets the close attribute of each element of OPEN-OBJ-LIST.

OPEN-OBJ-LIST is a list of fl--Open. Each must be a child of the
next in the list. This is used to scan-lists efficiently."
  ;; Note: Because fl--Open-position values come from (nth 9
  ;; (syntax-ppss)), we know they are not inside a string or comment.
  ;; Thus buf-pos inits to a valid position to start scan-lists from.
  (let ((buf-pos (and open-obj-list
                      ;; scan_lists tolerates buf-pos past EOB
                      (1+ (fl--Open-position (car open-obj-list))))))
    (dolist (open-i open-obj-list)
      (when buf-pos
        (setq buf-pos (condition-case nil
                          (scan-lists buf-pos 1 1)
                        (scan-error nil))))
      (setf (fl--Open-close open-i) (if buf-pos
                                        (1- buf-pos)
                                      'mismatched)))))

(defun fl-propertize-region (start end)
  (save-excursion
    ;; In order to correctly remove faces from parens that changed
    ;; from multiline to uniline, we clear all parens in the JIT lock
    ;; region to start with.
    (fl-unpropertize-region start end)
    (let* ((timing-info (list (current-time)))
           (start-ps (syntax-ppss start))
           ;; Open positions, outer to inner
           (ps-opens (nth 9 start-ps))
           ;; fl--Open objects, positions inner to outer
           (open-objs nil))
      (push (current-time) timing-info)
      ;; Process the broader region spanned by ps-opens. Consider only
      ;; the ps-opens, not their children which lie entirely outside
      ;; the JIT lock region.
      ;;
      ;; We mostly avoid further sexp parsing in the broader region,
      ;; except to check for a multiline string just before setting
      ;; inconsistent.
      (dolist (ps-open-i ps-opens)
        (push (make-fl--Open :position
                             ps-open-i
                             :column
                             (progn
                               (goto-char ps-open-i)
                               (current-column)))
              open-objs))
      (push (current-time) timing-info)
      ;; Filter out parens which don't need consideration outside the
      ;; JIT lock region. The ones that do are currently fontified as
      ;; inconsistent, and could become consistent if all its enclosed
      ;; lines are checked.
      ;;
      ;; In addition to filtering, this passage sets close positions
      ;; and may reapply the inconsistency-face to some close parens
      ;; which were just cleared.
      (setq open-objs
            (let* ((objs-head (cons nil open-objs))
                   (prev-open objs-head)
                   (open-i (cdr objs-head))
                   ;; Whether we've called fl--set-closes
                   ;;
                   ;; fl--set-closes is fairly expensive when near the
                   ;; beginning of a long Lisp function. We can avoid
                   ;; calling it if all open-objs are propertized as
                   ;; consistent or mismatched.
                   (closes-set nil))
              (while open-i
                (let* ((inconsistency-offset
                        (get-text-property (fl--Open-position (car open-i))
                                           'fl-inconsistency))
                       (inconsistency-pos
                        (and inconsistency-offset
                             (+ (fl--Open-position (car open-i))
                                inconsistency-offset))))
                  (if (or (not inconsistency-pos)
                          ;; Always nil so as "or" evaluation continues
                          (unless closes-set
                            ;; Lazy one-time call
                            (fl--set-closes open-objs)
                            (not (setq closes-set t)))
                          ;; Spot check using the cached offset to
                          ;; possibly avoid a complete check in
                          ;; fl--region-check-opens for open-i.
                          ;;
                          ;; Because of buffer changes,
                          ;; inconsistency-pos is not necessarily
                          ;; the original. Just do a valid check.
                          (and (< (fl--Open-position (car open-i))
                                  inconsistency-pos)
                               (number-or-marker-p (fl--Open-close (car open-i)))
                               (<= inconsistency-pos
                                   (fl--Open-close (car open-i)))
                               (progn
                                 (goto-char inconsistency-pos)
                                 (fl--line-check-opens (list (car open-i)))
                                 (when (fl--Open-inconsistent (car open-i))
                                   (fl--colorize-inconsistent (car open-i))
                                   t))))
                      ;; Remove (car open-i) from list
                      (setcdr prev-open (cdr open-i))
                    (pop prev-open))
                  (pop open-i)))
              (cdr objs-head)))
      (push (current-time) timing-info)
      (when open-objs
        ;; Check lists beginning before JIT lock's region (could
        ;; scan to after JIT lock's region)
        (let ((open-objs-reversed (reverse open-objs)))
          (goto-char (fl--Open-position (car open-objs-reversed)))
          (fl--region-check-opens open-objs-reversed
                                  nil)))
      (push (current-time) timing-info)
      (goto-char start)
      ;; Process within the inputted JIT lock region
      (let* (;; Sparse vector of open paren data, indexed by position
             ;; in buffer minus start. This benchmarked better than
             ;; keeping a stack of fl--Open objects updated from the
             ;; parse states of syntax-ppss.
             (open-paren-table (make-vector (- end start) nil)))
        (while (< (point) end)
          (let ((indent-pos (progn (back-to-indentation)
                                   (point)))
                ;; Column at which text starts on the line
                (indent-column (current-column))
                (line-ppss (syntax-ppss))
                (line-end (progn (end-of-line)
                                 (point))))
            ;; Skip whitespace only lines and lines beginning inside
            ;; string
            (unless (or (eq indent-pos line-end)
                        (nth 3 line-ppss))
              ;; Iterate over list of unclosed open parens
              (dolist (open-pos (nth 9 line-ppss))
                ;; Skip the already processed ones outside the region
                (when (<= start open-pos)
                  (let ((open-obj (or (aref open-paren-table
                                            (- open-pos start))
                                      (progn
                                        (push (make-fl--Open
                                               :position open-pos
                                               :column (progn
                                                         (goto-char open-pos)
                                                         (current-column)))
                                              open-objs)
                                        (aset open-paren-table
                                              (- open-pos start)
                                              (car open-objs))))))
                    (when (<= indent-column
                              (fl--Open-column open-obj))
                      (setf (fl--Open-inconsistent open-obj)
                            (- indent-pos (fl--Open-position open-obj))))))))
            ;; Go to next line. Since we already know line-end, use it
            ;; instead of rescanning the line
            (goto-char (1+ line-end))))
        (push (current-time) timing-info)
        ;; Process parens beginning in the JIT lock region but extending after
        ;;
        ;; Note: the reason we don't filter fl--Open after the JIT
        ;; lock region, as we did for the region before it, is mostly
        ;; because of the directionality of redisplay from BOB to EOB.
        ;; If we allow subsequent fl-propertize-region to propertize
        ;; the open parens in the current JIT lock region, it wouldn't
        ;; show to the user because by then redisplay has finished
        ;; this JIT lock region. An additional consideration is that
        ;; the coloring of the open paren is of more interest than the
        ;; close paren.
        (let ((ps-opens (nth 9 (syntax-ppss end)))
              ;; Inner to outer going towards the tail
              (open-obj-list nil))
          (dolist (ps-open-i ps-opens)
            (when (<= start ps-open-i)
              (push (or (aref open-paren-table
                              (- ps-open-i start))
                        ;; Open parens on the last line of the JIT
                        ;; lock region don't have a fl--Open object
                        ;; created yet.
                        (progn
                          (push (make-fl--Open
                                 :position ps-open-i
                                 :column (progn
                                           (goto-char ps-open-i)
                                           (current-column)))
                                open-objs)
                          (aset open-paren-table
                                (- ps-open-i start)
                                (car open-objs))))
                    open-obj-list)))
          (push (current-time) timing-info)
          (fl--set-closes open-obj-list)
          (push (current-time) timing-info)
          (goto-char end)
          (fl--region-check-opens nil open-obj-list))
        (push (current-time) timing-info)
        (dolist (open-i open-objs)
          ;; Set close position
          ;;
          ;; Note: We do it here instead of when it was made so as
          ;; some benefit from the fl--set-closes function's buffer
          ;; scanning optimization. The lists processed here are
          ;; opened and closed within JIT lock's region, so the less
          ;; efficient buffer scanning is not a big deal.
          (unless (fl--Open-close open-i)
            (setf (fl--Open-close open-i)
                  (condition-case nil
                      (1- (scan-lists (fl--Open-position open-i) 1 0))
                    (scan-error 'mismatched))))
          ;; Apply the font color via text properties
          (with-silent-modifications
            (if (eq 'mismatched (fl--Open-close open-i))
                (add-text-properties (fl--Open-position open-i)
                                     (1+ (fl--Open-position open-i))
                                     `(font-lock-face
                                       fl-mismatched-face
                                       rear-nonsticky
                                       t))
              (if (fl--Open-inconsistent open-i)
                  (fl--colorize-inconsistent open-i)
                (dolist (pos-i (list (fl--Open-position open-i)
                                     (fl--Open-close open-i)))
                  (remove-text-properties pos-i
                                          (1+ pos-i)
                                          '(fl-inconsistency
                                            nil
                                            font-lock-face
                                            nil
                                            rear-nonsticky
                                            nil)))))))
        (push (current-time) timing-info)
        ;; (my-msg "fl-propertize-region start=%s end=%s timing: %s"
        ;;         start end
        ;;         (my-time-diffs (nreverse timing-info)))
        ))))

(defun fl-unpropertize-region (start end)
  (goto-char start)
  ;; remove-text-properties errors if (1+ (point)) is past EOB, so
  ;; adjust end
  (let ((end (min (1- (point-max))
                  end)))
    (while (< (point) end)
      (skip-syntax-forward "^()" end)
      (remove-text-properties (point)
                              (1+ (point))
                              '(fl-inconsistency nil
                                font-lock-face nil
                                rear-nonsticky nil))
      (forward-char 1))))

(defsubst flylisp-extend-region-after-change (start _end _old-len)
  ;; It seems redisplay works its way from before start to after end,
  ;; so it's more important to expand the start in order to get
  ;; correct redisplays.
  (save-excursion
    (setq jit-lock-start
          (or (syntax-ppss-toplevel-pos (syntax-ppss start))
              start))))

(define-minor-mode flylisp-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if flylisp-mode
      (progn
        (jit-lock-register 'fl-propertize-region t)
        (add-hook 'jit-lock-after-change-extend-region-functions
                  #'flylisp-extend-region-after-change
                  nil
                  t))
    (remove-hook 'jit-lock-after-change-extend-region-functions
                 #'flylisp-extend-region-after-change
                 t)
    (jit-lock-unregister 'fl-propertize-region)
    (save-excursion
      (fl-unpropertize-region (point-min) (point-max)))))

;;;; ChangeLog:

;; 2013-12-26  Barry  <gundaetiapo@gmail.com>
;; 
;; 	flylisp: New package. See announcement:
;; 	http://lists.gnu.org/archive/html/emacs-devel/2013-12/msg00486.html
;; 


(provide 'flylisp)

;;; flylisp.el ends here

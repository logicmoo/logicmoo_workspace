;;; eldoc-eval.el --- Enable eldoc support when minibuffer is in use.

;; Copyright (C) 2011, 2012 Free Software Foundation, Inc.

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Version: 0.1

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
;;
;; This package enables eldoc support when minibuffer is in use.
;;
;; Eldoc info is shown by default in mode-line,
;; but you can have eldoc info somewhere else by setting
;; `eldoc-in-minibuffer-show-fn' to another function (e.g `tooltip-show').
;;
;; By default with this package `M-:' will use `pp-eval-expression'
;; instead of `eval-expression'; you can change that by setting
;; `eval-preferred-function'.
;;
;; It also provides a convenient macro to enable eldoc support
;; in your own functions using minibuffer or in your defadvices,
;; that is `with-eldoc-in-minibuffer'.
;;
;; Users of own minibuffer frame will have to set
;; `eldoc-in-minibuffer-own-frame-p' to non-nil.
;;
;; You can turn off eldoc support in minibuffer any time
;; by setting `eldoc-in-minibuffer' to nil.

;;; Code:
(require 'eldoc)


;;; Minibuffer support.
;;  Enable displaying eldoc info in something else
;;  Than minibuffer when this one is in use.
;;
(defcustom eldoc-in-minibuffer t
  "Turn on eldoc in minibuffer."
  :group 'eldoc
  :type 'bolean)

(defcustom eldoc-in-minibuffer-show-fn 'eldoc-show-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display"
  :group 'eldoc
  :type 'function)

(defcustom eldoc-show-in-mode-line-delay 12
  "The time we show eldoc when Emacs is idle."
  :group 'eldoc
  :type 'number)

(defcustom eval-preferred-function 'pp-eval-expression
  "Preferred function to use with `M-:'."
  :group 'lisp
  :type 'function)

(defcustom  eldoc-in-minibuffer-own-frame-p nil
  "Whether minibuffer has its own frame or not."
  :group 'lisp
  :type 'boolean)

;; Internal.
(defvar eldoc-active-minibuffers-list nil
  "List of active minibuffers with eldoc enabled.")
(defvar eldoc-mode-line-rolling-flag nil)

(defun eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `eldoc-active-minibuffers-list'.
This function is called by each minibuffer started with eldoc support.
See `with-eldoc-in-minibuffer'."
  (with-selected-window (minibuffer-window)
    (push (buffer-name) eldoc-active-minibuffers-list)))

(defmacro with-eldoc-in-minibuffer (&rest body)
  "Enable eldoc support for minibuffer input that runs in BODY."
  (declare (indent 0) (debug t))
  `(let ((timer (and eldoc-in-minibuffer
                     (run-with-idle-timer
                      eldoc-idle-delay
                      'repeat 'eldoc-mode-in-minibuffer))))
     (unwind-protect
         (minibuffer-with-setup-hook
             ;; When minibuffer is activated in body,
             ;; store it.
             'eldoc-store-minibuffer
           ,@body)
       (and timer (cancel-timer timer))
       ;; Each time a minibuffer exits or aborts
       ;; its buffer is removed from stack,
       ;; assuming we can only exit the active minibuffer
       ;; on top of stack.
       (setq eldoc-active-minibuffers-list
             (cdr eldoc-active-minibuffers-list)))))

(defun eldoc-current-buffer ()
  "Return the current buffer prior to activating the minibuffer."
  (with-selected-frame (last-nonminibuffer-frame)
    (window-buffer
     (cond (eldoc-in-minibuffer-own-frame-p
            (selected-window))
           ((fboundp 'window-in-direction)
            (window-in-direction
             'above (minibuffer-window)))
           (t (minibuffer-selected-window))))))

(defun eldoc-show-in-mode-line (str)
  "Display string STR in the mode-line next to minibuffer."
  (let (mode-line-in-non-selected-windows)
    (with-current-buffer (eldoc-current-buffer)
      (make-local-variable 'mode-line-format)
      (let ((mode-line-format (concat " " str)))
        (eldoc-maybe-roll-message-in-mode-line mode-line-format))
      (force-mode-line-update))))

(defun eldoc-maybe-roll-message-in-mode-line (str)
  (let* ((max (window-width (get-buffer-window (eldoc-current-buffer))))
         (len (length str))
         (tmp-str str))
    (if (and (> len max) eldoc-mode-line-rolling-flag)
        (while (sit-for 0.3)
           (setq tmp-str (substring tmp-str 2)
                 mode-line-format (concat tmp-str " [<]" str))
           (force-mode-line-update nil)
           (when (< (length tmp-str) 2) (setq tmp-str str)))
        (force-mode-line-update nil)
        (sit-for eldoc-show-in-mode-line-delay))))

(defun eldoc-mode-line-toggle-rolling ()
  (interactive)
  (setq eldoc-mode-line-rolling-flag (not eldoc-mode-line-rolling-flag)))
(define-key minibuffer-local-map (kbd "<C-M-right>") 'eldoc-mode-line-toggle-rolling)

(defun eldoc-mode-in-minibuffer ()
  "Show eldoc for current minibuffer input."
  (let ((buf (with-selected-window (minibuffer-window)
               (buffer-name))))
    ;; If this minibuffer have been started with
    ;;`with-eldoc-in-minibuffer' give it eldoc support
    ;; and update mode-line, otherwise do nothing.
    (when (member buf eldoc-active-minibuffers-list)
      (let* ((str-all (with-current-buffer buf
                        (minibuffer-completion-contents)))
             (sym     (when str-all
                        (with-temp-buffer
                          (insert str-all)
                          (goto-char (point-max))
                          (unless (looking-back ")\\|\"")
                            (forward-char -1))
                          (eldoc-current-symbol))))
             (info-fn (eldoc-fnsym-in-current-sexp))
             (doc     (or (eldoc-get-var-docstring sym)
                          (eldoc-get-fnsym-args-string
                           (car info-fn) (cadr info-fn)))))
        (when doc (funcall eldoc-in-minibuffer-show-fn doc))))))

(defun eval-expression-with-eldoc ()
  "Eval expression with eldoc support in mode-line."
  (interactive)
  (with-eldoc-in-minibuffer
    (call-interactively eval-preferred-function)))

;; Bind it to `M-:'.
(global-set-key [remap eval-expression] 'eval-expression-with-eldoc)

;;;; ChangeLog:

;; 2012-10-27  Andreas Schwab  <schwab@linux-m68k.org>
;; 
;; 	* eldoc-eval.el: Spelling and doc fixes.
;; 	(eval-preferred-function): Renamed from eval-prefered-function.
;; 
;; 2011-10-23  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Add version tag to eldoc-eval.
;; 
;; 2011-10-15  thierry volpiatto  <thierry.volpiatto@gmail.com
;; 
;; 	* eldoc-eval.el Add infos in Commentary section.
;; 
;; 2011-10-11  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Fix header line in eldoc-eval.el.
;; 
;; 2011-09-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	New package eldoc-eval.
;; 



(provide 'eldoc-eval)
;;; eldoc-eval.el ends here

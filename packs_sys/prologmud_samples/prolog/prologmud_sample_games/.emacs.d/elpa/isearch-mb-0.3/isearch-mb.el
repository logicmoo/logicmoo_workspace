;;; isearch-mb.el --- Control isearch from the minibuffer -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/isearch-mb
;; Keywords: matching
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an alternative isearch UI based on the
;; minibuffer.  This allows editing the search string in arbitrary
;; ways without any special maneuver; unlike standard isearch, cursor
;; motion commands do not end the search.  Moreover, the search status
;; information in the echo area and some keybindings are slightly
;; simplified.

;; To use the package, simply activate `isearch-mb-mode'.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup isearch-mb nil
  "Control isearch from the minibuffer."
  :group 'isearch)

(defvar isearch-mb--with-buffer
  '(isearch-beginning-of-buffer
    isearch-end-of-buffer
    isearch-occur
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-toggle-case-fold
    isearch-toggle-char-fold
    isearch-toggle-invisible
    isearch-toggle-lax-whitespace
    isearch-toggle-regexp
    isearch-toggle-symbol
    isearch-toggle-word
    isearch-exit
    isearch-delete-char)
  "List of commands to execute in the search buffer.")

(defvar isearch-mb--after-exit
  '(isearch-query-replace
    isearch-query-replace-regexp
    isearch-highlight-regexp
    isearch-highlight-lines-matching-regexp
    isearch-abort)
  "List of commands to execute after exiting the minibuffer.")

(defvar isearch-mb--no-search
  '(next-history-element previous-history-element)
  "List of commands that shouldn't trigger a search.")

(defvar isearch-mb-minibuffer-map
  (let ((map (make-composed-keymap nil minibuffer-local-map)))
    (define-key map [remap next-line-or-history-element] #'isearch-repeat-forward)
    (define-key map [remap previous-line-or-history-element] #'isearch-repeat-backward)
    (define-key map [remap minibuffer-beginning-of-buffer] #'isearch-beginning-of-buffer)
    (define-key map [remap end-of-buffer] #'isearch-end-of-buffer)
    (define-key map [remap query-replace] #'isearch-query-replace)
    (define-key map [remap query-replace-regexp] #'isearch-query-replace-regexp)
    (define-key map "\C-j" #'newline)
    (define-key map "\C-s" #'isearch-repeat-forward)
    (define-key map "\C-r" #'isearch-repeat-backward)
    (define-key map "\M-s'" #'isearch-toggle-char-fold)
    (define-key map "\M-s " #'isearch-toggle-lax-whitespace)
    (define-key map "\M-s_" #'isearch-toggle-symbol)
    (define-key map "\M-sc" #'isearch-toggle-case-fold)
    (define-key map "\M-shr" #'isearch-highlight-regexp)
    (define-key map "\M-shl" #'isearch-highlight-lines-matching-regexp)
    (define-key map "\M-si" #'isearch-toggle-invisible)
    (define-key map "\M-so" #'isearch-occur)
    (define-key map "\M-sr" #'isearch-toggle-regexp)
    (define-key map "\M-sw" #'isearch-toggle-word)
    map)
  "Minibuffer keymap used by isearch-mb.")

(defvar isearch-mb--prompt-overlay nil
  "Overlay for minibuffer prompt updates.")

(defun isearch-mb--after-change (_beg _end _len)
  "Hook to run from the minibuffer to update the isearch state."
  (let ((string (minibuffer-contents))
        (inhibit-redisplay t))
    (with-minibuffer-selected-window
      (setq isearch-string (substring-no-properties string))
      (isearch-update-from-string-properties string)
      ;; Backtrack to barrier and search, unless `this-command' is
      ;; special or the search regexp is invalid.
      (if (or (and (symbolp this-command)
                   (memq this-command isearch-mb--no-search))
              (and isearch-regexp
                   (condition-case err
                       (prog1 nil (string-match-p isearch-string ""))
                     (invalid-regexp
                      (prog1 t (setq isearch-error (cadr err)))))))
          (isearch-update)
        (goto-char isearch-barrier)
        (setq isearch-adjusted t isearch-success t)
        (isearch-search-and-update)))))

(defun isearch-mb--post-command-hook ()
  "Hook to make the minibuffer reflect the isearch state."
  (unless isearch--current-buffer
    (throw 'isearch-mb--continue '(ignore)))
  (let ((inhibit-modification-hooks t))
    ;; We never update `isearch-message'.  If it's not empty, then
    ;; isearch changed the search string on its own volition.
    (unless (string-empty-p isearch-message)
      (setq isearch-message "")
      (delete-minibuffer-contents)
      (insert isearch-string))
    (set-text-properties (minibuffer-prompt-end) (point-max) nil)
    (when-let ((fail-pos (isearch-fail-pos)))
      (add-text-properties (+ (minibuffer-prompt-end) fail-pos)
                           (point-max)
                           '(face isearch-fail)))
    (when isearch-error
      (isearch-mb--message isearch-error))))

(defun isearch-mb--message (message)
  "Display a momentary MESSAGE."
  (let ((message-log-max nil))
    (message (propertize (concat " [" message "]")
                         'face 'minibuffer-prompt))))

(defun isearch-mb--update-prompt (&rest _)
  "Update the minibuffer prompt according to search status."
  (when isearch-mb--prompt-overlay
    (let ((count (isearch-lazy-count-format))
          (len (or (overlay-get isearch-mb--prompt-overlay 'isearch-mb--len) 0)))
      (overlay-put isearch-mb--prompt-overlay
                   'isearch-mb--len (max len (length count)))
      (overlay-put isearch-mb--prompt-overlay
                   'before-string
                   (concat count ;; Count is padded so that it only grows.
                           (make-string (max 0 (- len (length count))) ?\ )
                           (capitalize
                            (isearch--describe-regexp-mode
                             isearch-regexp-function)))))))

(defun isearch-mb--with-buffer (&rest args)
  "Evaluate ARGS in the search buffer.
Intended as an advice for isearch commands."
  (if (minibufferp)
      (let ((enable-recursive-minibuffers t)
            (inhibit-redisplay t))
        (with-minibuffer-selected-window
          (apply args)))
    (apply args)))

(defun isearch-mb--after-exit (&rest args)
  "Evaluate ARGS after quitting isearch-mb.
Intended as an advice for commands that quit isearch and use the
minibuffer."
  (throw 'isearch-mb--continue args))

(defun isearch-mb--session ()
  "Read search string from the minibuffer."
  (condition-case nil
      (apply
       (catch 'isearch-mb--continue
         (cl-letf (((cdr isearch-mode-map) nil)
                   ((symbol-function #'isearch-pre-command-hook) #'ignore)
                   ((symbol-function #'isearch-post-command-hook) #'ignore)
                   ((symbol-function #'isearch--momentary-message) #'isearch-mb--message)
                   ;; Setting `isearch-message-function' currently disables lazy
                   ;; count, so we need this as a workaround.
                   ((symbol-function #'isearch-message) #'isearch-mb--update-prompt)
                   ;; We need to set `inhibit-redisplay' at certain points to
                   ;; avoid flicker.  As a side effect, window-start/end in
                   ;; `isearch-lazy-highlight-update' will have incorrect values,
                   ;; so we need to lazy-highlight the whole buffer.
                   (lazy-highlight-buffer (not (null isearch-lazy-highlight)))
                   (wstart nil))
           (minibuffer-with-setup-hook
               (lambda ()
                 (add-hook 'after-change-functions #'isearch-mb--after-change nil 'local)
                 (add-hook 'post-command-hook #'isearch-mb--post-command-hook nil 'local)
                 (add-hook 'minibuffer-exit-hook
                           (lambda () (setq wstart (window-start (minibuffer-selected-window))))
                           nil 'local)
                 (setq-local tool-bar-map isearch-tool-bar-map)
                 (setq isearch-mb--prompt-overlay (make-overlay (point-min) (point-min)
                                                                (current-buffer) t t))
                 (isearch-mb--update-prompt)
                 (isearch-mb--post-command-hook))
             (unwind-protect
                 (progn
                   (dolist (fun isearch-mb--with-buffer)
                     (advice-add fun :around #'isearch-mb--with-buffer))
                   (dolist (fun isearch-mb--after-exit)
                     (advice-add fun :around #'isearch-mb--after-exit))
                   (read-from-minibuffer
                    "I-search: "
                    nil
                    isearch-mb-minibuffer-map
                    nil
                    (if isearch-regexp 'regexp-search-ring 'search-ring)
                    (thread-last '(region url symbol sexp line) ;; TODO: make customizable
                      (mapcar #'thing-at-point)
                      (delq nil)
                      (delete-dups)
                      (mapcar (if isearch-regexp 'regexp-quote 'identity)))
                    t)
                   ;; Undo a possible recenter after quitting the minibuffer.
                   (set-window-start nil wstart))
               (dolist (fun isearch-mb--after-exit)
                 (advice-remove fun #'isearch-mb--after-exit))
               (dolist (fun isearch-mb--with-buffer)
                 (advice-remove fun #'isearch-mb--with-buffer))))
           (if isearch-mode '(isearch-done) '(ignore)))))
    (quit (if isearch-mode (isearch-cancel) (signal 'quit nil)))))

(defun isearch-mb--setup ()
  "Arrange to start isearch-mb after this command, if applicable."
  (unless (minibufferp)
    ;; When `with-isearch-suspended' is involved, this hook may run
    ;; more than once, hence the test for `isearch-mode'.
    (run-with-idle-timer 0 nil (lambda () (when isearch-mode (isearch-mb--session))))))

;;;###autoload
(define-minor-mode isearch-mb-mode
  "Control isearch from the minibuffer.

During an isearch-mb session, the following keys are available:
\\{isearch-mb-minibuffer-map}"
  :global t
  (if isearch-mb-mode
      (add-hook 'isearch-mode-hook #'isearch-mb--setup)
    (remove-hook 'isearch-mode-hook #'isearch-mb--setup)))

(provide 'isearch-mb)
;;; isearch-mb.el ends here

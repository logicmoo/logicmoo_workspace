;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3") (org "9.2.3") (org-ql "0.5-pre") (dash "2.17.0"))
;; Version: 0.6

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:
(require 'cal-move)

(require 'org-treescope-query)
;; brings faces, cyclestates, calendarranges, datehelper, calendar

(defgroup org-treescope nil
  "org-treescope customisable variables."
  :group 'productivity)

;;;###autoload
(defun org-treescope-exit ()
  "Exit calendar and restore original org buffer to normal state."
  (interactive)
  (with-current-buffer org-treescope-modehelper--orgbuffer
    (org-treescope-mode -1))
  (calendar-exit))

(defvar org-treescope-mode-map
  ;; Add exit function
  (let ((map (make-sparse-keymap))
        (lst org-treescope-modehelper-list))
    (set-keymap-parent map calendar-mode-map)
    (define-key map (kbd "q") 'org-treescope-exit)
    (dolist (keypair lst map)
      (define-key map (kbd (car keypair)) (cdr keypair))))
  "Keymap for function `org-treescope-mode'.")

(define-minor-mode org-treescope-calendar-mode
  "Minor mode to control date ranges, todo and priority states in *Calendar* buffer."
  nil
  " ts-cal"
  org-treescope-mode-map)

(define-minor-mode org-treescope-mode
  "Minor mode to for org-treescope that operates on the org buffer"
  nil
  " ts"
  nil
  (if (string-suffix-p ".org" (buffer-file-name))
      (when org-treescope-mode
        (setq org-treescope-calendarranges--day--leftflank nil
              org-treescope-calendarranges--day--rightflank nil
              org-treescope-calendarranges--day--frommidpoint-select nil
              org-treescope-modehelper--orgbuffer (current-buffer))
        (org-treescope-calendarranges--sensible-values)
        (org-treescope-refresh-calendar))
    (message "Not an org file.")))

(defun org-treescope-refresh-calendar ()
  "Enable the calendar and update the flanks."
  (unless (member "*Calendar*"
                  (-map (lambda (it) (buffer-name (window-buffer it)))
                        (window-list)))
    (calendar))
  (org-treescope-calendar-mode t)
  (calendar-unmark)
  (org-treescope-query--redraw-calendar))

(defun org-treescope-addpublic ()
  "Add public finish functions."
  (org-treescope-refresh-calendar)
  (org-treescope-query-apply-to-buffer))

(add-hook 'org-treescope-modehelper--publicfinishhook
          'org-treescope-addpublic)

(provide 'org-treescope)
;;; org-treescope.el ends here

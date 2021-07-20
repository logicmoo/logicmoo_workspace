;;; org-treescope-datehelper.el --- Helper functions for parsing and finding dates -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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

;; see org-treescope.el

;;; Code:
(require 'calendar)

(defvar displayed-month)
(defvar displayed-year)

(defun org-treescope-datehelper--first-of-lastmonth ()
  "Grab the first day of last month of current calendar window.  Used by `org-treescope-calendarranges--redraw-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (- mont 1)))
    (if (> newm 0)
        (list newm 1 year)
      (list 12 1 (- year 1)))))

(defun org-treescope-datehelper--last-of-nextmonth ()
  "Grab the last day of next month of current calendar window.  Used by `org-treescope-calendarranges--redraw-calendar'."
  (let* ((mont displayed-month)
         (year displayed-year)
         (newm (+ mont 1)))
    (if (> newm 12)
        (list 1 31 (+ year 1))
      (list newm (calendar-last-day-of-month newm year) year))))

(defsubst org-treescope-datehelper--datetostring (gregdate)
  "Convert GREGDATE to an org compatible date.  Used by `org-treescope-calendarranges--redraw-calendar'."
  (let ((year (nth 2 gregdate))
        (month (nth 0 gregdate))
        (day (nth 1 gregdate)))
    (format "%04d-%02d-%02d" year month day)))

(defsubst org-treescope-datehelper--getmidpoint () ;; getmidpoint-abs
  "Grabs the date under cursor (if calendar active), or return the current date."
  (condition-case err
      (calendar-cursor-to-date nil nil)
    (error
     (ignore err)
     (calendar-current-date))))

(defsubst org-treescope-datehelper--getmidpoint-abs () ;; called by sensible-values and redraw-calendar
  "Inline substitution to retrieve the current mid point in epochs."
  (calendar-absolute-from-gregorian (org-treescope-datehelper--getmidpoint)))

(defsubst org-treescope-datehelper--markdate (abs face) ;; redraw-calendar
  "Takes an ABS date and highlight it on the calendar with FACE."
  (calendar-mark-visible-date (calendar-gregorian-from-absolute abs) face))

(provide 'org-treescope-datehelper)
;;; org-treescope-datehelper.el ends here

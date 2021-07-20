;;; org-clock-split.el --- Split clock entries -*- lexical-binding: t; -*-

;; Author: Justin Taft <https://github.com/justintaft>
;; Keywords: calendar
;; Package-Version: 20200331.526
;; Package-Commit: 39e1d2912a7a7223e2356a5fc4dff03507ae084d
;; URL: https://github.com/justintaft/emacs-org-clock-split
;; Version: 1.1
;; Package-Requires: ((emacs "24"))

;;; Contributors
;;  https://github.com/swflint
;;  https://github.com/alphapapa
;;  https://github.com/miguelmorin

;;; Commentary:
;;
;;  This package provides ability to split an org CLOCK entry into two records.
;;
;;  Usage example:
;;
;;  If cursor is on
;;
;;  CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 16:05] =>  3:46
;;  
;;  Running
;;
;;  (org-clock-split \"1h2m\")
;;  
;;  Will produce
;;
;;  CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 13:21] =>  1:02
;;  CLOCK: [2018-08-30 Thu 13:21]--[2018-08-30 Thu 16:05] =>  2:44"

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'ert)
(require 'seq)

(defvar org-clock-split-inactive-timestamp-hm (replace-regexp-in-string "<" "[" (replace-regexp-in-string ">" "]" (cdr org-time-stamp-formats)))
  "Inactive timestamp with hours and minutes. I don't know where org mode provides it, or why it doesn't.")

(defvar org-clock-split-clock-range-regexp (concat "\\(^\\s-*\\)\\(" org-clock-string " " org-tr-regexp-both "\\)")
  "Regular expression to match a clock range, possibly without the interval calculation at the end ('=> hh:mm').")

(defvar org-clock-split-clock-range-format (concat "%s" org-clock-string " %s--%s")
  "Format for inserting a clock range with two timestamps as arguments.")

(defvar org-clock-split-clock-range-format-no-brackets (concat "%s" org-clock-string " [%s]--[%s]")
  "Format for inserting a clock range with two timestamps without delimiters as arguments.")

(defvar org-clock-split-merge-tolerance-minutes 2
  "Tolerance in seconds to merge two clock segments.")

(defun org-clock-split-absolute-string-to-hm (splitter-string)
  "Return pair of hours and minutes from the timestring.

    SPLITTER-STRING - Absolute time to split record at (Ex
    '9:20')"

  (let (hour minute)
    (progn ;; wrap in progn to avoid scrapping match data, which is global
      (if (string-match "\\([0-9]?[0-9]\\):\\([0-9]\\{2\\}\\)" splitter-string)
	  (progn
	    (setq hour (match-string 1 splitter-string))
	    (setq minute (match-string 2 splitter-string))
	    (seq-map #'string-to-number (list hour minute)))
	(error "Input must be a valid absolute time, e.g. 9:20.")))))

(defun org-clock-split-relative-string-to-seconds (splitter-string)
  "Return minutes given a time string in format.
Throws error when invalid time string is given.
   SPLITTER-STRING - Time offset to split record at.  (Ex '1h', '01m', '68m1h')"
  
  ;; Remove all whitespace from string for sanity checks.
  ;; Used to ensure all characters are processed.
  (if (string-match "[ \t]+" splitter-string)
      (setq splitter-string (replace-match  "" t t splitter-string)))

  (let ((total-minutes 0)
        (matched-input-characters 0))

    (when (string-match "\\([0-9]+\\)h" splitter-string)
      (cl-incf total-minutes (* 60 (string-to-number (match-string 1 splitter-string))))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 splitter-string)))))

    (when (string-match "\\([0-9]+\\)m" splitter-string)
      (cl-incf total-minutes (string-to-number (match-string 1 splitter-string)))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 splitter-string)))))
    
    (if (/= matched-input-characters (length splitter-string))
        (error "Invalid time string format"))

    (* 60 total-minutes)))

(defun org-clock-split-get-timestrings (tr-string)
  "Gets the clock-in and clock-out timestrings from a time range string."
  (let* ((t1-start (string-match org-ts-regexp-both tr-string 0))
	 (t1-end (match-end 0))
	 (t2-start (string-match org-ts-regexp-both tr-string t1-end))
	 (t2-end (match-end 0))
	 (t1 (substring tr-string t1-start t1-end))
	 (t2 (substring tr-string t2-start t2-end)))
    (list t1 t2)))

(defun org-timestring-to-time (timestring)
  "Converts the org time string to internal time."
  (float-time (apply #'encode-time (org-parse-time-string timestring))))

(defun org-clock-split-split-line-into-timestamps (original-line splitter-string from-end)
  "Splits the clock range in ORIGINAL-LINE by SPLITTER-STRING, from the end or the start of the clock range.

   ORIGINAL-LINE: a clock range from an Org buffer, such as 'CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24'

   SPLITTER-STRING: either a relative duration such as 1h02m or
    an absolute time such as 09:20. If the absolute time is
    within the range in ORIGINAL-LINE, then FROM-END is
    irrelevant. If it falls outside the range, the splitting
    point will be the latest time before the end of the clock
    range if FROM-END is t, and the first time after the
    beginning of the clock range if FROM-END is nil.

   FROM-END: whether to split from the end of the clock range or the start."

  (let* ((timestring-pair (org-clock-split-get-timestrings original-line))
        (t0string (pop timestring-pair))
        (t0float (org-timestring-to-time t0string))
        (t2string (pop timestring-pair))
        (t2float (org-timestring-to-time t2string))
        (absolute (cl-search ":" splitter-string))
	t1float
	;; deal with negative strings, which split from the end
	(negative-splitter (string= "-" (substring splitter-string 0 1)))
	(from-end-local from-end)
	(splitter-string-local splitter-string)
	)
    (if negative-splitter
	(setq from-end-local t
	      splitter-string-local (substring splitter-string 1)))
    ;; assign splitting time provisionally, will be updated in the logic
    (setq t1string (if from-end-local t2string t0string))
    (if absolute
       (let* ((pair (org-clock-split-absolute-string-to-hm splitter-string-local))
              (hours (pop pair))
              (minutes (pop pair))
              (t1-tuple (org-parse-time-string t1string))
              (t1-tuple (append (list 0 minutes hours) (seq-subseq t1-tuple 3)))
              (t1float (float-time (apply #'encode-time t1-tuple))))
         ;; update the splitting time so it's later than t0 or earlier than t2, depending on FROM-END
         (if from-end-local
             (when (> t1float t2float)
               (setq t1-tuple (append (seq-subseq t1-tuple 0 3) (list (1- (nth 3 t1-tuple))) (seq-subseq t1-tuple 4))))
           (when (< t1float t0float)
             (setq t1-tuple (append (seq-subseq t1-tuple 0 3) (list (1+ (nth 3 t1-tuple))) (seq-subseq t1-tuple 4)))))
         
         ;; convert to float
         (setq t1float (apply #'encode-time t1-tuple))
         (setq t1string (format-time-string org-clock-split-inactive-timestamp-hm t1float)))

         
      ;; Handle relative duration
      (let* ((parsed-seconds (org-clock-split-relative-string-to-seconds splitter-string-local))
            (t1float (org-timestring-to-time t1string))
            (t1float (if from-end-local
                  (- t1float parsed-seconds)
                (+ t1float parsed-seconds))))
       (setq t1string (format-time-string org-clock-split-inactive-timestamp-hm t1float))))
    (list t0string t1string t2string)))

(defun org-clock-split (from-end splitter-string)
  "Split CLOCK entry under cursor into two entries.
Total time of created entries will be the same as original entry.

   WARNING: Negative time entries can be created if splitting at an offset
longer then the CLOCK entry's total time.

   FROM-END: nil if the function should split with duration from
   the start of the clock segment (default for backwards
   compatibility), t if the function should split counting from
   the end of the clock segment.
 
   SPLITTER-STRING: Time offset to split record at.  Examples: '1h', '01m', '68m1h', '9:20'."

  (interactive "P\nsTime offset to split clock entry (ex 1h2m): ")

  (move-beginning-of-line nil)
  (let ((original-line (buffer-substring (line-beginning-position) (line-beginning-position 2))))
    
    ;; Error if CLOCK line does not contain check in and check out time
    (unless (string-match org-clock-split-clock-range-regexp original-line)
      (error "Cursor must be placed on line with valid CLOCK entry range"))

    (let* ((whitespace (match-string 1 original-line))
           (timestamps (org-clock-split-split-line-into-timestamps original-line splitter-string from-end))
	   (t0 (pop timestamps))
	   (t1 (pop timestamps))
	   (t2 (pop timestamps)))
      ;; delete line without moving to kill ring
      (delete-region (line-beginning-position) (line-end-position))
      ;; insert the earlier segment
      (insert (format org-clock-split-clock-range-format whitespace t0 t1))
      ;; Update interval duration, which moves point to the end of the later timestamp
      (org-ctrl-c-ctrl-c)
      ;; insert the later segment before the earlier segment, so it's ready for org-clock-merge
      (move-beginning-of-line nil)
      (newline)
      (previous-line)
      (insert (format org-clock-split-clock-range-format whitespace t1 t2))
      ;; Update interval duration, which fails if point doesn't move to beginning of line
      (org-ctrl-c-ctrl-c)
      (move-beginning-of-line nil))))

(defun org-clock-split-get-clock-segment-timestamps (line)
  "Parses a clock segment line and returns the first and last timestamps in a list."
  (let* ((org-clock-regexp (concat "CLOCK: " org-ts-regexp3 "--" org-ts-regexp3))
	 (t1 (if (string-match org-clock-regexp line)
		 (match-string 1 line)
	       (user-error "The argument must have a valid CLOCK range")))
	 (t2 (match-string 9 line)))
    (list t1 t2)))

(defun org-clock-split-compute-timestamp-difference (later-timestamp earlier-timestamp)
  "Computes the number of seconds difference in string timestamps as a float."
  (-
   (float-time (apply #'encode-time (org-parse-time-string later-timestamp)))
   (float-time (apply #'encode-time (org-parse-time-string earlier-timestamp)))))

(defun org-clock-split-float-time-diff-to-hours-minutes (diff)
  "Returns a float time difference in hh:mm format."
  (let* ((hours (floor (/ diff 3600)))
	 (diff_minus_hours (- diff (* 3600 hours)))
	 (minutes (floor (/ diff_minus_hours 60))))
    (car (split-string (format "%2d:%02d" hours minutes)))))

(defun org-clock-merge (&optional skip-merge-with-time-discrepancy)
  "Merge the org CLOCK line with the next CLOCK line. If the last
timestamp of the current line equals the first timestamp of the
next line with a tolerance of up to org-clock-split-merge-tolerance-minutes, then merge
automatically. If a discrepancy exists, prompt the user for
confirmation, unless skip-merge-with-time-discrepancy is
non-nil."

  (interactive "P")
  (let* ((first-line-start (line-beginning-position))
	 (first-line (buffer-substring
		      (line-beginning-position) (line-end-position)))
	 (first-line-timestamps (org-clock-split-get-clock-segment-timestamps first-line))
	 (first-line-t1 (pop first-line-timestamps))
	 (first-line-t2 (pop first-line-timestamps))
	 (first-line-t2 (match-string 9 first-line))
	 (second-line (progn
			(forward-line)
			(buffer-substring
			 (line-beginning-position) (line-end-position))))
	 (second-line-timestamps (org-clock-split-get-clock-segment-timestamps second-line))
	 (second-line-t1 (pop second-line-timestamps))
	 (second-line-t2 (pop second-line-timestamps))
	 (diff (org-clock-split-compute-timestamp-difference first-line-t1 second-line-t2))
	 whitespace)

    ;; grab whitespace to maintain it
    (progn
      (let ((temp (string-match org-clock-split-clock-range-regexp first-line)))
	(setq whitespace (match-string 1 first-line))))

    ;; ignore discrepancies of 2 minutes or less
    (when (> diff (* 60 org-clock-split-merge-tolerance-minutes))
      (when skip-merge-with-time-discrepancy
	(error "Skipping clock-merge"))
      (unless (yes-or-no-p (concat (org-clock-split-float-time-diff-to-hours-minutes diff)
				   " discrepancy in times to merge. Proceed anyway?"))
	(user-error "Cancelled merge")))

    ;; remove the two lines
    (delete-region first-line-start (line-end-position))
    ;; indent
    (org-cycle)
    ;; insert new time range
    (message (concat "'" whitespace "'"))
    (insert (format org-clock-split-clock-range-format-no-brackets whitespace second-line-t1 first-line-t2))
    ;; generate duration
    (org-evaluate-time-range)
    ))

(provide 'org-clock-split)

;;; org-clock-split.el ends here

;;; notes-index-mode.el --- Index manipulation for notes-mode

;; Copyright (C) 1994-1998,2012  Free Software Foundation, Inc.

;; Author: <johnh@isi.edu>

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

;;; Code:

(require 'notes-variables)
(require 'notes-aux)


(defvar notes-index-mode-map
  (let ((map (make-sparse-keymap)))
    ;; There were bindings to make mouse-1 do pointer following,
    ;; but I removed it because all the rest of emacs uses mouse-2.
    ;; If you want them, add them with notes-index-mode-hook.
    (notes-platform-bind-mouse map 'mouse-2 'notes-index-mouse-follow-link)
    (notes-platform-bind-mouse map 'S-mouse-2 'notes-index-mouse-follow-link-other-window)
    (define-key map "\r" 'notes-index-follow-link)
    (define-key map "\C-c\C-s" 'notes-summarize-subject)
    (define-key map "o" 'notes-index-link)
    map))


(defvar notes-index-lazy-message-old-time 0)

(defun notes-index-lazy-percent-message (text fraction whole)
  "Put up a message occasionally.
Displays TEXT (a format string), with FRACTION of WHOLE
shown as a percentage.  (Read the code to see what this
cryptic statement means.)"
  (if (= notes-index-lazy-message-old-time
	 (setq notes-index-lazy-message-old-time (+ 1 (nth 1 (current-time)))))
      nil
    (message text (/ (* fraction 100) whole))))

(defun notes-index-parse-buffer ()
  "Parse a notes-index buffer, fontifying and building subject completion.

If fontification is enabled, subjects will be emboldened
and dates will be mouse-highlighted.

In any event a subject completion table will be built.

This routine works by calling either \[notes-index-parse-buffer-uncached]
or \[notes-index-parse-buffer-cached] (if possible)."
  (interactive)
  (let
      ((inhibit-read-only t))
    (if (and (file-exists-p (expand-file-name "index_cache.el" notes-dir))
	     (file-newer-than-file-p
              (expand-file-name "index_cache.el" notes-dir)
              (expand-file-name "index" notes-dir)))
	(progn
	  (load (expand-file-name "index_cache" notes-dir))
	  (notes-index-parse-buffer-cached))
      ;; cache miss
      (message "notes-index-parse-buffer: cache is not present or is not up-to-date")
      (notes-index-parse-buffer-uncached))
    ;; clean some things up
    (message "")
    (set-buffer-modified-p nil)))

(defun notes-index-parse-buffer-uncached ()
  "Parse a notes-index buffer, fontifying and building subject completion.

If fontification is enabled, subjects will be emboldened
and dates will be mouse-highlighted.

In any event a subject completion table will be built.

Tenses passive will be."
  (interactive)
  (save-excursion
    (let ((start (point-min))
	  end subject)
      ;; prepare the way
      (if notes-use-font-lock
          ;; FIXME: That's quite drastic!  What is this trying to do?
	  (set-text-properties (point-min) (point-max) nil))
      ;; There used to be problem that we used a fixed obarray length,
      ;; creating a lot of hash collisions.  Now we dynamically compute it
      ;; by rounding up the number of lines to the next power of 8.
      (if (and notes-mode-complete-subjects (not notes-subject-table))
	  (setq notes-subject-table (make-vector
				     (- (expt
				      8 
				      (length
				       (format
					"%o"
					(count-lines
					 (point-min)
					 (point-max)))))
					1)
				     0)))
      ;; do it
      (goto-char start)
      (while (< start (point-max))
	;; find the end-of-line
	(end-of-line)
	(setq end (point))
	(goto-char start)

	;; find the subject
	(while (not (eq (following-char) ?\ ))
	  (skip-chars-forward "^:" end)
	  (forward-char))
	(backward-char)
	(if notes-subject-table
	    (intern (buffer-substring start (point)) notes-subject-table))
	(notes-index-lazy-percent-message "Notes-index'ing (%d%%)..." start (point-max))

	(if notes-use-font-lock
	    (progn
	      ;; highlight the title
	      (put-text-property start (point) 'face notes-bold-face)
	      (if notes-index-fontify-dates
		  (progn
		    ;; now highlight each date
		    (skip-chars-forward "^0-9" end)
		    (while (looking-at "[0-9]")
		      (setq start (point))
		      (skip-chars-forward "0-9")
		      (put-text-property start (point) 'mouse-face 'highlight)
		      (skip-chars-forward "^0-9" end))))))
	;; set up for next line
	(forward-line 1)
	(setq start (point))))))


(defun notes-index-date-search (start end iter-proc done-proc done-arg)
  "Iterate over a notes-index entry bounded by START to END.
Iteration is done by (ITER-PROC END), which leaves match 0
set to what we're looking for.
A match terminates iteration if (DONE-PROC match DONE-ARG) is non-nil.
Returns the buffer position of a successful hit, or nil."
  (goto-char start)
  (let (stop)
    (while (and (not stop)
		(funcall iter-proc end))
      (if (funcall done-proc (match-string 0) done-arg)
	  (setq stop (goto-char (match-beginning 0)))))
    stop))


(defun notes-index-goto-date (date &optional direction)
  "Goto the DATE in the current line of the index file, modified by DIRECTION.
If DIRECTION is 'this, go there.
If DIRECTION is 'next or 'prev, go to the corresponding entry.
If the entry doesn't exist, then go to the nearest entry according
to DIRECTION (and the next one if DIRECTION is 'this)."
  (cond
   ((eq direction 'prev)
    (notes-index-date-search
     (line-end-position) (line-beginning-position)
     (function (lambda (end) (re-search-backward notes-file-regexp end t)))
     (function (lambda (trial target) (string-lessp trial target)))
     date))
   ((eq direction 'next)
    (notes-index-date-search
     (line-beginning-position) (line-end-position)
     (function (lambda (end) (re-search-forward notes-file-regexp end t)))
     (function (lambda (trial target) (string-lessp target trial)))
     date))
   (t
    (notes-index-date-search
     (line-beginning-position) (line-end-position)
     (function (lambda (end) (re-search-forward notes-file-regexp end t)))
     (function (lambda (trial target) (string-equal trial target)))
     date))))

(defun notes-index-link (link &optional tag where)
  "* Follow a notes-index LINK.
Optionally takes a subject TAG and
WHERE ('otherwindow or nil) to open the new file."
  (interactive "sNotes-index link: ")
  (notes-w3-url (notes-file-to-url link tag) where t))

;;;###autoload
(defun notes-index-todays-link ()
  "* Open the notes file for today."
  (interactive)
  (notes-index-link (format-time-string notes-file-form (current-time))))

(defun notes-index-follow-link (pt &optional where)
  "Follow a link at PT in notes-index-mode.
The link is taken from the location PT,
and the new information is shown WHERE (either 'otherwindow or not)."
  (interactive "d")
  (save-excursion
    (let (start date tag)
      ;; determine the date
      (skip-chars-backward "0-9")
      (setq start (point))
      (if (not (re-search-forward notes-file-regexp (+ (point) 6) t))
	  (error "Not on notes-index-mode link."))
      (setq date (match-string 0))
      ;; pick out the tag
      (beginning-of-line)
      (if (not (re-search-forward "^\\([^:]*\\):" start t))
	  (error "Not on notes-index-mode link line."))
      (setq tag (match-string 1))
      ;; make and process the url
      (notes-index-link date tag where))))

(defun notes-index-mouse-follow-link (e)
  "Handle a mouse click in notes-index-mode."
  (interactive "e")
  (mouse-set-point e)
  (notes-index-follow-link (point) nil))

(defun notes-index-mouse-follow-link-other-window (e)
  "Handle a mouse click in notes-index-mode (other-window)."
  (interactive "e")
  (mouse-set-point e)
  (notes-index-follow-link (point) 'otherwindow))

(defun notes-index-extract-subject ()
  "Extract the notes-index subject for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^\\(.*\\): " (line-end-position) t)
	(match-string 1)
      nil)))

;;;###autoload
(define-derived-mode notes-index-mode special-mode "Notes-index"
  "Notes-index-mode with mouse support.

You may wish to change notes-bold-face and notes-use-font-lock.

There should be no need to add notes-index-mode to auto-mode-alist
since the index generation functions add code to the index file
which invokes notes-index-mode.

Key bindings are:
\\{notes-index-mode-map}"
  (notes-platform-init)

  (notes-index-parse-buffer)

  (set (make-local-variable 'font-lock-defaults)
       '(notes-index-font-lock-keywords
         t nil nil beginning-of-line))

  ;; No editing is allowed.
  (setq buffer-read-only t)
)

(provide 'notes-index-mode)
;;; notes-index-mode.el ends here

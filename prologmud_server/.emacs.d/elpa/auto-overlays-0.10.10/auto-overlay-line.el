;;; auto-overlay-line.el --- automatic overlays for single lines    -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2020  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: Toby Cubitt <toby-predictive@dr-qubit.org>
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/predictive.git

;; This file is part of the Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(provide 'auto-overlay-line)
(require 'auto-overlays)


;; set line overlay parsing and suicide funtions
(put 'line 'auto-overlay-parse-function #'auto-o-parse-line-match)
(put 'line 'auto-overlay-suicide-function
     (lambda (o) (auto-o-delete-overlay (overlay-get o 'parent))))



(defun auto-o-parse-line-match (o-match)
  ;; Create overlay for a new line match.
  (let ((o-new (make-overlay (overlay-get o-match 'delim-end)
			     (save-excursion
			       (goto-char (overlay-get o-match 'delim-end))
			       (1+ (line-end-position))))))

    ;; give new overlay some basic properties
    (overlay-put o-new 'auto-overlay t)
    (overlay-put o-new 'set-id (overlay-get o-match 'set-id))
    (overlay-put o-new 'definition-id (overlay-get o-match 'definition-id))
    ;; match start of new overlay with match
    (auto-o-match-overlay o-new o-match nil)
    ;; set overlay's modification hooks to ensure that it always extends to
    ;; end of line
    (overlay-put o-new 'modification-hooks
		 (cons 'auto-o-schedule-extend-line
		       (overlay-get o-new 'modification-hooks)))
    ;; return new overlay
    o-new))


(defun auto-o-schedule-extend-line (o-self modified &rest _unused)
  ;; All line overlay modification hooks are set to this function, which
  ;; schedules `auto-o-extend-line' to run after any suicide functions have
  ;; been called, but before the overlays are updated.
  (unless modified
    (push (list 'auto-o-extend-line o-self) auto-o-pending-post-suicide)))



(defun auto-o-extend-line (o-self)
  ;; Checks if overlay still extends to end of line, and update the necessary
  ;; if not.

  ;; if we haven't been deleted by a suicide function...
  (when (overlay-buffer o-self)
    (save-match-data
      (let ((start (overlay-start o-self))
	    (end (overlay-end o-self)))
	(cond
	 ;; if we no longer extend to end of line...
	 ((null (string-match "\n" (buffer-substring-no-properties
				    (overlay-start o-self)
				    (overlay-end o-self))))
	  ;; grow ourselves so we extend till end of line
	  (move-overlay o-self start (save-excursion
				       (goto-char (overlay-end o-self))
				       (1+ (line-end-position))))
	  ;; if we're exclusive, delete lower priority overlays in newly
	  ;; covered region
	  (auto-o-update-exclusive (overlay-get o-self 'set-id)
				   end (overlay-end o-self)
				   nil (overlay-get o-self 'priority)))

	 ;; if we extend beyond end of line...
	 ((/= (overlay-end o-self) (+ start (match-end 0)))
	  ;; shrink ourselves so we extend till end of line
	  (move-overlay o-self start (+ start (match-end 0)))
	  ;; if we're exclusive, re-parse region that is no longer covered
	  (auto-o-update-exclusive (overlay-get o-self 'set-id)
				   (overlay-end o-self) end
				   (overlay-get o-self 'priority) nil))
	 )))))


;; auto-overlay-line.el ends here

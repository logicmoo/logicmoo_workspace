;;; auto-overlay-word.el --- automatic overlays for single "words"    -*- lexical-binding: t; -*-

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

(provide 'auto-overlay-word)
(require 'auto-overlays)


;; set word overlay parsing and suicide functions
(put 'word 'auto-overlay-parse-function #'auto-o-parse-word-match)
(put 'word 'auto-overlay-suicide-function
     (lambda (o) (auto-o-delete-overlay (overlay-get o 'parent))))



(defun auto-o-parse-word-match (o-match)
  ;; Create a new word overlay for new word match
  (let ((o-new (make-overlay (overlay-get o-match 'delim-start)
			     (overlay-get o-match 'delim-end)
			     nil nil 'rear-advance)))
    ;; give overlays appropriate properties
    (overlay-put o-new 'auto-overlay t)
    (overlay-put o-new 'set-id (overlay-get o-match 'set-id))
    (overlay-put o-new 'definition-id (overlay-get o-match 'definition-id))
    (overlay-put o-new 'start o-match)
    (overlay-put o-match 'parent o-new)
    ;; bundle properties inside list if not already, then update overlay
    ;; properties
    (let ((props (auto-o-props o-match)))
      (when (symbolp (car props)) (setq props (list props)))
      (dolist (p (auto-o-props o-match))
	(overlay-put o-new (car p) (cdr p))))

    ;; if new overlay is exclusive, delete lower priority overlays within it
    (when (and (overlay-get o-new 'exclusive)
	       (/= (overlay-start o-new) (overlay-end o-new)))
      (auto-o-update-exclusive (overlay-get o-new 'set)
			       (overlay-start o-new) (overlay-end o-new)
			       nil (overlay-get o-new 'priority)))

    ;; return new overlay
    o-new)
)


;;; auto-overlay-word.el ends here

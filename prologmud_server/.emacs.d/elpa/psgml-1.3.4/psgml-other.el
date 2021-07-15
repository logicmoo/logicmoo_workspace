;;; psgml-other.el --- Part of SGML-editing mode with parsing support  -*- lexical-binding:t -*-

;; Copyright (C) 1994, 2016 Free Software Foundation, Inc

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Part of psgml.el. Code not compatible with XEmacs.


;;; Code:

(require 'psgml)
(require 'easymenu)
(eval-when-compile (require 'cl-lib))

(defvar sgml-max-menu-size (/ (* (frame-height) 2) 3)
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")


;;;; Key Commands

;; Doesn't this work in Lucid? ***


;;;; Pop Up Menus

(defun sgml-popup-menu (event title entries)
  "Display a popup menu.
ENTRIES is a list where every element has the form (STRING . VALUE) or
STRING."
  (let ((menus (sgml-split-long-menus (list (cons title entries)))))
    (x-popup-menu event (cons title menus))))


(defun sgml-range-indicator (string)
  (substring string
	     0
	     (min (length string) sgml-range-indicator-max-length)))


(defun sgml-split-long-menus (menus)
  (cl-loop
   for (title . entries) in menus
   nconc
   (cond
    ((> (length entries) sgml-max-menu-size)
     (cl-loop for i from 1 while entries
           collect
           (let ((submenu (copy-sequence entries)))
             (setcdr (nthcdr (1- (min (length entries) sgml-max-menu-size))
                             submenu)
                     nil)
             (setq entries (nthcdr sgml-max-menu-size entries))
             (cons
              (format "%s '%s'.."
                      title
                      (sgml-range-indicator (caar submenu)))
              submenu))))
    (t
     (list (cons title entries))))))



(defun sgml-popup-multi-menu (event title menus)
  "Display a popup menu.
MENUS is a list of menus on the form (TITLE ITEM1 ITEM2 ...).
ITEM should have to form (STRING EXPR) or STRING.  The EXPR gets evaluated
if the item is selected."
  (setq menus (sgml-split-long-menus menus))
  (unless (cdr menus)
    (setq menus (list (car menus) '("---" "---"))))
  (eval (car (x-popup-menu event (cons title menus)))))


;;;; Insert with properties

(defvar sgml-write-protect-intagible
  (not (boundp 'emacs-minor-version)))

(defun sgml-insert (props format &rest args)
  (let ((start (point)))
    (insert (apply (function format)
		   format
		   args))
    (when (and sgml-write-protect-intagible
	       (plist-get props 'intangible))
	  (plist-put props 'read-only t))
    (add-text-properties start (point) props)))


;;;; Set face of markup

(defvar sgml-use-text-properties t
  "Non-nil means use text properties for highlighting, not overlays.
Overlays are significantly less efficient in large buffers.")

(defvar sgml-current-tree)
(declare-function sgml-element-appdata "psgml-parse" (element prop))

(defun sgml-set-face-for (start end type)
  (let ((face (cdr (assq type sgml-markup-faces))))
    (if (and (null type) sgml-current-tree)
        (setq face (sgml-element-appdata sgml-current-tree 'face)))
    (cond
     (sgml-use-text-properties
      ;; `with-silent-modifications' is rather expensive.
      ;; Skip it if we're already within it.
      ;; FIXME: A better fix would be to make sure all callers use
      ;; with-silent-modifications.
      (if (not inhibit-modification-hooks)
	  (with-silent-modifications
            (put-text-property start end 'face face)
            (when (and sgml-default-nonsticky (< start end))
              (put-text-property (1- end) end 'rear-nonsticky '(face))))
	(put-text-property start end 'face face)
        (when (and sgml-default-nonsticky (< start end))
          (put-text-property (1- end) end 'rear-nonsticky '(face)))))
     (t
      (let ((current (overlays-at start))
	    (pos start)
	    old-overlay)
	(while current
	  (cond ((and (null old-overlay)
                      type
		      (eq type (overlay-get (car current) 'sgml-type)))
		 (setq old-overlay (car current)))
		((overlay-get (car current) 'sgml-type)
		 ;;(message "delov: %s" (overlay-get (car current) 'sgml-type))
		 (delete-overlay (car current))))
	  (setq current (cdr current)))
	(while (< (setq pos (next-overlay-change pos))
		  end)
	  (setq current (overlays-at pos))
	  (while current
	    (when (overlay-get (car current) 'sgml-type)
	      (delete-overlay (car current)))
	    (setq current (cdr current))))
	(cond (old-overlay
	       (move-overlay old-overlay start end)
	       (if (null (overlay-get old-overlay 'face))
		   (overlay-put old-overlay 'face face)))
	      (face
	       (setq old-overlay (make-overlay start end))
	       (overlay-put old-overlay 'sgml-type type)
	       (overlay-put old-overlay 'face face))))))))

(defun sgml-set-face-after-change (start end &optional _pre-len)
  ;; If inserting in front of an markup overlay, move that overlay.
  ;; this avoids the overlay beeing deleted and recreated by
  ;; sgml-set-face-for.
  ;; FIXME: Use overlay's start insertion type instead!
  (when (and sgml-set-face (not sgml-use-text-properties))
    (cl-loop for o in (overlays-at start)
	  do (cond
	      ((not (overlay-get o 'sgml-type)))
	      ((= start (overlay-start o))
	       (move-overlay o end (overlay-end o)))))))

(defun sgml-fix-overlay-after-change (overlay flag start end &optional size)
  (message "sfix(%s): %d-%d (%s)" flag start end size)
  (overlay-put overlay 'front-nonsticky t)
  (when nil
    (move-overlay overlay end (overlay-end overlay))))

(defun sgml-clear-faces ()
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (if (overlay-get o 'sgml-type)
	(delete-overlay o))))


;;;; Provide

(provide 'psgml-other)

;;; psgml-other.el ends here

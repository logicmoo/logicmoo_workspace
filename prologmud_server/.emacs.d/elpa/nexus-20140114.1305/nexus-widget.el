;;; nexus-widget.el --- Widgets for Nexus Client

;; Copyright (C) 2011  Juergen Hoetzel

;; Author: Juergen Hoetzel <juergen@archlinux.org>
;; Keywords: comm

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

;; 

;;; Code:

(defcustom nexus-widget-buffer-name "*nexus*"
  "The buffer name of Nexus client displaying the search results."
  :type 'string
  :group 'nexus)

(defface nexus-widget-artifact-face
  `((((class color)
      (background dark))
     (:foreground "light blue" :weight bold))
    (((min-colors 88) (class color)
      (background light))
     (:foreground "blue1" :weight bold))
    (((class color)
      (background light))
     (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "Face used for Maven artifacts."
  :group 'nexus)

;;; borrowed from nxml
(defface nexus-widget-xml-tag-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for the local name of elements."
  :group 'nexus)

(defvar nexus-widget-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km "q" 'nexus-widget-quit)
    km)
  "Keymap used in recentf dialogs.")

(defun nexus-widget-toggle-hide (widget &optional event)
  (let ((new-value (if (widget-value widget) nil t))
	(xml-string (widget-get widget :xml-string))
	(insert-pos (marker-position (widget-get widget :to)))
	(inhibit-read-only t))
    (widget-value-set widget new-value)
    (save-excursion 
      (goto-char insert-pos)
      (if new-value
	  ;; show xml
	  (widget-insert xml-string)
	;; hide xml (fixme: by searching next widget ovelay)
	(progn (while (not (overlays-at (point)))
		 (forward-char))
	       (delete-region insert-pos (point)))))))

(defun nexus-widget-artifact (artifact)
  ;; create a leiningen project like string
  (let* ((group-id (cadr (assoc  :groupId artifact)))
	 (artifact-id (cadr (assoc :artifactId artifact)))
	 (version  (cadr (assoc  :version artifact)))
	 (artifact-string (format "%s/%s \"%s\"" group-id artifact-id version))
	 (maven-string (format "<dependency>
	<groupId>%s</groupId>
	<artifactId>%s</artifactId>
	<version>%s</version>\n</dependency>\n" group-id artifact-id version)))
    ;; poor mans closures
    (lexical-let ((artifact artifact))
      (widget-create 'push-button
		     :notify (lambda (&rest ignore) 
			       (let ((url (cadr (assoc :resourceURI  artifact))))
				 (url-copy-file url (file-name-nondirectory url))))
		     artifact-string))
    (widget-insert "\t")
    (widget-create
     'toggle
     :help-echo "Hide/Show Maven XML"
     :on "Hide XML"
     :off "Show XML"
     :xml-string maven-string
     :action 'nexus-widget-toggle-hide)))

(defun nexus-widget-quit (&rest ignore)
  (interactive)
  (kill-buffer (current-buffer))
  (message "Nexus search result canceled"))

(define-derived-mode nexus-widget-mode nil "nexus-widget"
  "Major mode of nexus search results-

\\{nexus-widget-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun nexus-widget-display (artifacts)
  (pop-to-buffer nexus-widget-buffer-name)
  ;; ensure buffer is writeable
  (setq buffer-read-only nil)
  (erase-buffer)
  (dolist (artifact artifacts)
    (nexus-widget-artifact artifact))
  (nexus-widget-mode)
  (setq buffer-read-only t)
  (beginning-of-buffer))

(provide 'nexus-widget)
;;; nexus-widget.el ends here

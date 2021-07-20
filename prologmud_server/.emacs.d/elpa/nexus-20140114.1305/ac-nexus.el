;;; ac-nexus.el --- nexus autocomplete

;; Copyright (C) 2013  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: matching

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

;; Usage:
;;     (require 'ac-nexus)
;;     (add-hook 'clojure-mode-hook 'ac-source-lein-set-up)
;;
;; url-retrieving in pure elisp is much slower compared to external retrieving. 
;;
;; Recommended is setting up external url retriving i.e. setting
;; the custom variable `mm-url-use-external'  to a non nil value
;;
;;     (setq mm-url-use-external t)


;;; Code:

(defcustom nexus-ac-artifact-prefix-length
  3
  "URL of Nexus REST API. Customize if you use an private/custom Nexus server"
  :group 'nexus-ac)


(defun ac-source-nexus-complete (prefix)
  (let* ((parts (split-string prefix "[[:space:]/]"))
	 (group (car parts))
	 (artifact (cadr parts))
	 (version (replace-regexp-in-string "\"" "" (or (caddr parts) ""))))
    (mapcar (lambda (lst)
	      (format "%s/%s \"%s\""
		      (cadr (assoc :groupId lst))
		      (cadr (assoc :artifactId lst))
		      (cadr (assoc :version lst))))
	    (nexus--search-coordinates-internal group (format "%s*" artifact) (if version (format "%s*" version) "") "jar" ""  ))))

(defun ac-source-nexus-lein-candidates ()
  "Return a possibly-empty list of completions for the Lein dependency at point."
  (ac-source-nexus-complete ac-prefix))

(defun ac-prefix-lein-dep ()
  "Looking for Clojure Leiningen style dependencys"
  (when (looking-back (format "\\[\\([[:alnum:].]+/[[:alnum:][:space:]\".]\\{%d,\\}[^]]*\\)"  nexus-ac-artifact-prefix-length))
    (let ((m (match-beginning 1)))
      (when m				;ensure we are in Leiningen style :dependecy [[->here-] ..]
	(save-excursion
	  (condition-case nil
	      (progn
		(up-list -2)
		(if (looking-back ":dependencies[[:space:]]")
		    m))
	   (error nil)))))))

(defvar ac-source-nexus-lein
  '((candidates . ac-source-nexus-lein-candidates)
    (prefix . ac-prefix-lein-dep)
    (cache)))


;;;###autoload
(defun ac-source-lein-set-up ()
  "Add an nexus completion source to the front of `ac-sources' for the current buffer,
if the current `(buffer-filename)' = \"project.clj\""
  (interactive)
  (when (and (buffer-file-name) (string= (file-name-base (buffer-file-name)) "project"))
    (add-to-list 'ac-sources 'ac-source-nexus-lein)
    ;; we havev to complete version numbers inside strings
    (set (make-local-variable 'ac-disable-faces)
	 (remove 'font-lock-string-face ac-disable-faces))))

(provide 'ac-nexus)
;;; ac-nexus.el ends here

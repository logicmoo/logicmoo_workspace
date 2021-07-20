;;; org-link-travis.el --- Insert/Export the link of Travis CI on org-mode

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: org
;; Package-Version: 0.0.1
;; Package-Commit: 596615ad8373d9090bd4138da683524f0ad0bda5
;; URL: https://github.com/aki2o/org-link-travis
;; Version: 0.0.1
;; Package-Requires: ((org "7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides functions to insert/export the link of Travis CI on org-mode.

;;; Dependency:
;; 
;; Nothing

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'org-link-travis)

;;; Configuration:
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "org-link-travis")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "org-link-travis/" :docstring t)
;; `org-link-travis/user-name'
;; User name in Travis CI.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; Nothing

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian


;; Enjoy!!!


(require 'org)

(defgroup org-link-travis nil
  "Insert/Export the link of Travis CI on org-mode."
  :group 'org
  :prefix "org-link-travis/")

(defcustom org-link-travis/user-name ""
  "User name in Travis CI."
  :type 'string
  :group 'org-link-travis)


(defvar org-link-travis--root-url "https://travis-ci.org")

;;;;;;;;;;;;;
;; Utility

(defun org-link-travis--parse-build-link (link)
  (when (and (stringp link)
             (string-match "\\`\\(?:travis-build:\\)?\\([^/]+\\)/\\([^#]+\\)#\\(.+\\)\\'" link))
    (list (match-string-no-properties 1 link)
          (match-string-no-properties 2 link)
          (match-string-no-properties 3 link))))

(defun org-link-travis--get-repo-root-url (user repo)
  (format "%s/%s/%s" org-link-travis--root-url user repo))

(defun org-link-travis--get-build-status-image-url (user repo &optional branch)
  (format "%s/%s/%s.svg?branch=%s" org-link-travis--root-url user repo (or branch "master")))


;;;;;;;;;;;;;;;;;;
;; Fix org-mode

(org-add-link-type "travis-build" 'org-link-travis/open-build-link)

(defun org-link-travis/open-build-link (link)
  (multiple-value-bind (user repo branch) (org-link-travis--parse-build-link link)
    (when (and user repo)
      (browse-url (org-link-travis--get-repo-root-url user repo)))))

(defun org-travis-build-complete-link (&optional arg)
  (let ((user (read-string (format "UserName: (%s) " org-link-travis/user-name)
                           nil nil org-link-travis/user-name))
        (repo (read-string "Repository: "))
        (branch (read-string "Branch: (master) " nil nil "master")))
    (format "travis-build:%s/%s#%s" user repo branch)))

(defadvice org-display-inline-images (after org-link-travis/mk-img activate)
  (when (display-graphic-p)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or (ad-get-arg 2) (point-min)))
        (while (re-search-forward (concat "\\[\\[travis-build:\\([^/]+\\)/\\([^#]+\\)#\\([^]\n]+\\)\\]"
                                          (if (ad-get-arg 0) "" "\\(?:\\[[^]\n]*\\]\\)?\\]"))
                                  (or (ad-get-arg 3) (point-max))
                                  t)
          (let* ((user (match-string-no-properties 1))
                 (repo (match-string-no-properties 2))
                 (branch (match-string-no-properties 3))
                 (startpt (match-beginning 0))
                 (endpt (match-end 0))
                 (old (get-char-property-and-overlay (match-beginning 1) 'org-image-overlay))
                 (imgurl (org-link-travis--get-build-status-image-url user repo branch)))
            (if (and (car-safe old) (ad-get-arg 1))
                (image-refresh (overlay-get (cdr old) 'display))
              (let* ((img (create-image
                           (with-current-buffer (url-retrieve-synchronously imgurl)
                             (goto-char (point-min))
                             (when (re-search-forward "\n\n" nil t)
                               (buffer-substring (point) (point-max))))
                           nil t))
                     (ov (when img (make-overlay startpt endpt))))
                (when ov
                  (overlay-put ov 'display img)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'modification-hooks '(org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

(defadvice org-md-link (around org-link-travis/mk-build-link activate)
  (if (not (string= (org-element-property :type (ad-get-arg 0)) "travis-build"))
      ad-do-it
    (multiple-value-bind (user repo branch) (org-link-travis--parse-build-link
                                             (org-element-property :path (ad-get-arg 0)))
      (let ((imgurl (org-link-travis--get-build-status-image-url user repo branch))
            (repourl (org-link-travis--get-repo-root-url user repo)))
        (setq ad-return-value (format "[![Build Status](%s)](%s)" imgurl repourl))))))

(provide 'org-link-travis)
;;; org-link-travis.el ends here

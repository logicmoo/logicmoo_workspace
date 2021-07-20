;;; orglue-publish.el --- more functionality to org-mode publish.

;; Copyright (C) 2012-2013 Yoshinari Nomura.
;; All rights reserved.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2012-08-28
;; Version: 1.0
;; Keywords: org
;;
;;; Commentary:
;;
;;; Code:
;;

;;;; Require

(require 'org)

;;;; Easy setup for publish-project to HTML

(defun orglue-update-publish-project-alist (project-alist-var projects)
  (let ((project-names (mapcar 'car projects))
        (project-alist (symbol-value project-alist-var)))
    ;; remove old projects from project-alist
    (mapc
     (lambda (prj)
       (setq project-alist
             (delete (assoc prj project-alist) project-alist)))
     project-names)
    ;; add new projects to project-alist
    (set project-alist-var (append projects project-alist))))

(defun orglue-publish-setup-current-project ()
  "+ Set standard directory layout for a project :: 
  + org/       ... project top (any arbitrary name is OK)
    + *.org    ... org files
    + dat/     ... static attachments linked from *.org
    + dyn/     ... dinamically generated files from *.org
    + pub/     ... files only needed on publishing
      + css/   ... style sheets
      + top/   ... top-level dot files like a .htaccess
    + options/ ... org-mode options (not copied on publishing)
  + html/      ... destination to publish.

+ oprations to publish :: 
  1) find-file TOP/org/index.org
  2) M-x orglue-publish-setup-current-project to register the TOP.
  3) C-c C-e P p to publish files to TOP/html/

+ Note :: 
  All org/*.org files will be converted into html/*.html files.
  this is a non-recursive operation due to the css linking problem.
  http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

  All other files in org/ will be copied into html/ preserving
  their relaive positons in the directory tree.

  Directories named ``attic'' will be ignored.

  You must make the necessary directories beforehand.
  
+ Clean up your published files :: 

  Since every file in html/ can be reproduced from the other files,
  you can clean up html/ like:
  : rm -rf html ; mkdir html

  In addition, in case their cache files seem to be harmful:
  : rm -f  ~/.org-timestamps
"
  (interactive)
  (let* ((src (directory-file-name default-directory))
         (top (directory-file-name (file-name-directory src)))
         (dst (expand-file-name "html" top)))
    ;; current-file: /foo/project/org/index.org
    ;;   top => /foo/project
    ;;   src => /foo/project/org
    ;;   dst => /foo/project/html
    (orglue-update-publish-project-alist
     'org-publish-project-alist
     `(
       ("current" :components
        ("current-org" "current-static" "current-static-top")
        )
       ("current-org" ;; SRC/*.org -> DST/html/
        :base-directory ,src
        :base-extension "org"
        :publishing-directory ,dst
        :recursive nil
        :publishing-function org-html-publish-to-html
        )
       ("current-static" ;; SRC/**/* -> DST/html/
        :base-directory ,src
        :base-extension ".*"
        :exclude "^\\(attic\\|top\\|options\\|.*\\.org\\)$"
        :publishing-directory ,dst
        :recursive t
        :publishing-function org-publish-attachment
        )
       ("current-static-top" ;; SRC/pub/top -> DST/html/
        :base-directory ,(concat src "/pub/top")
        :base-extension ".*"
        :include (".htaccess")
        :publishing-directory ,dst
        :recursive nil
        :publishing-function org-publish-attachment
        )
       ))
    (message "PUBLISH %s => %s" src dst)))

(defadvice org-publish-current-project
  (around org-publish-current-project-advice)
  "open published file in browser after ``org-publish-current-project''."
  ad-do-it
  (org-open-file
   (expand-file-name
    (concat
     (file-name-sans-extension
      (file-name-nondirectory (buffer-file-name)))
     ".html")
    (plist-get (cdr (assoc (car (org-publish-get-project-from-filename (buffer-file-name)))
                           org-publish-project-alist))
               :publishing-directory))))

(ad-activate 'org-publish-current-project)

(defvar orglue-before-export-dispatch-hook nil
  "Hook for org-export-dispatch attached by advice.
This hook is useful for settingup org-publish-project-alist before ``org-publish-current-project''")

(defadvice org-export-dispatch (before org-export-advice)
  "setup org-publish-project-alist before ``org-publish-current-project'' etc."
  (run-hooks 'orglue-before-export-dispatch-hook)
  (if (not (org-publish-get-project-from-filename (buffer-file-name)))
      (orglue-publish-setup-current-project)))

(ad-activate 'org-export-dispatch)


;;;; Provide

(provide 'orglue-publish)

;;; Copyright Notice:

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; orglue-publish.el ends here

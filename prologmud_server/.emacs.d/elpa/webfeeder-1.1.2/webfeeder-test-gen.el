;;; webfeeder-test-gen.el --- ?? -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Test resources should not be generated for it might render the test suite
;; non-reproducible.  Indeed, the results of Org export depend on the version of
;; Org.
;;
;; We only use this file to bootstrap the test resources.  The resources are
;; then versioned to ensure their consistency.

(require 'ox)
(require 'webfeeder)
(require 'webfeeder-test)

;; Get rid of index.html~ and the like that pop up during generation.
(setq make-backup-files nil)

(defun webfeeder-test--export (input output)
  (with-temp-buffer
    (insert-file-contents input)
    (org-export-to-file 'html output)))

;; TODO: Namespace macro.
(defmacro with-directory-excursion (dir &rest body)
  (declare (indent defun))
  `(progn
     (mkdir ,dir 'parents)
     (let ((default-directory ,default-directory))
       (cd ,dir)
       ,@body)))

(defun webfeeder-test-gen-feeds ()
  "Generate various feeds.
We keep both libxml and default around to make sure our library
can accommodate user's custom functions."
  (with-directory-excursion webfeeder-test-dir
    (cl-loop for (suffix builder) in '(("atom" webfeeder-make-atom)
                                       ("rss" webfeeder-make-rss))
             do
             (let ((webfeeder-date-function 'webfeeder-date-libxml)
                   (webfeeder-title-function 'webfeeder-title-libxml)
                   (webfeeder-body-function 'webfeeder-body-libxml)
                   (prefix "libxml"))
               (webfeeder-build (format "%s-post0.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0-html5-fancy.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0))
             (let ((webfeeder-date-function 'webfeeder-date-default)
                   (webfeeder-title-function 'webfeeder-title-default)
                   (webfeeder-body-function 'webfeeder-body-default)
                   (prefix "default"))
               (webfeeder-build (format "%s-post0.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0-html5-fancy.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0))
             (let ((webfeeder-date-function 'webfeeder-date-libxml)
                   (webfeeder-title-function 'webfeeder-title-libxml)
                   (webfeeder-body-function 'webfeeder-body-libxml)
                   (prefix "libxml"))
               (webfeeder-build (format "%s-post0+post1.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0-html5-fancy.html" "post1-html5-fancy.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0))
             (let ((webfeeder-date-function 'webfeeder-date-libxml)
                   (webfeeder-title-function 'webfeeder-title-libxml)
                   (webfeeder-body-function 'webfeeder-body-libxml)
                   (prefix "libxml"))
               (webfeeder-build (format "%s-post1.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0-html5-fancy.html" "post1-html5-fancy.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0
                                   :max-entries 1))
             (let ((webfeeder-date-function 'webfeeder-date-libxml)
                   (webfeeder-title-function 'webfeeder-title-libxml)
                   (webfeeder-body-function 'webfeeder-body-libxml)
                   (prefix "libxml"))
               (webfeeder-build (format "%s-post0-no-html.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0))
             (let ((webfeeder-date-function 'webfeeder-date-libxml)
                   (webfeeder-title-function 'webfeeder-title-libxml)
                   (webfeeder-body-function 'webfeeder-body-libxml)
                   (prefix "libxml"))
               (webfeeder-build (format "%s-post0-no-fancy.%s" prefix suffix)
                                   "." "https://example.org/"
                                   '("post0-html5.html")
                                   :builder builder
                                   :title "Example feed"
                                   :description "Example description"
                                   :build-date 0)))))

(defun webfeeder-test-gen ()
  (with-directory-excursion webfeeder-test-dir
    (dolist (post '("post0" "post1"))
      (let ((org-input (format "%s.org" post)))
        (let ((org-html-head-include-default-style nil)
              (org-html-head-include-scripts nil))
          (webfeeder-test--export org-input (format "%s.html" post)))
        (let ((org-html-head-include-default-style nil)
              (org-html-head-include-scripts nil)
              (org-html-doctype "html5"))
          (webfeeder-test--export org-input (format "%s-html5.html" post)))
        (let ((org-html-head-include-default-style nil)
              (org-html-head-include-scripts nil)
              (org-html-doctype "html5")
              (org-html-html5-fancy t))
          (webfeeder-test--export org-input (format "%s-html5-fancy.html" post)))))))

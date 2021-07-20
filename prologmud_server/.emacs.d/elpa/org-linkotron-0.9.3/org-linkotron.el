;;; org-linkotron.el --- Org-mode link selector   -*- lexical-binding: t; -*-

;; Author: Per Weijnitz <per.weijnitz@gmail.com>
;; Keywords: hypermedia, Org
;; URL: https://gitlab.com/perweij/org-linkotron
;; Version: 0.9.3
;; Package-Requires: ((emacs "26.1") (org "9.3"))

;; Copyright (C) 2019, Per Weijnitz, all rights reserved.

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
;; The purpose of this package is to provide a way to open a group of
;; org-links at once.  A group is defined as all org-links under a
;; heading/sub heading, no need to use any special layout or formatting.
;;
;; The motivation is that I usually have at least a couple of sites I need
;; to visit for any specific task, so opening them all at once saves me time.
;;
;; Links are opened via the standard org funktion ~org-open-at-point~.
;;
;;
;;; Installation:
;;
;; Evaluate the elisp source file in some manner.  If you like quelpa,
;; this line would also work in your init.el:
;;
;;   (when (not (require 'org-linkotron nil 'noerror))
;;     (quelpa '(org-linkotron :repo "perweij/org-linkotron" :fetcher gitlab)))
;;   (require 'org-linkotron)
;;
;;
;;; Usage:
;;
;; In an org-file, create a heading with some links in the text
;; below, for example:
;;
;;  ** Some links about something
;;  Here is a text with two links, here: [[https://www.fsf.org/][fsf]] and
;;  here: [[https://fsfe.org/][fsfe]].
;;  A final one: [[https://www.gnu.org/][gnu]].
;;  ** Another interesting link collection
;;   - [[https://en.wikipedia.org/wiki/Amiga_500][Amiga 500]]
;;   - [[https://en.wikipedia.org/wiki/Commodore_64][Commodore 64]]
;;
;; Now, just place the cursor on the relevant header or its text, and
;; invoke
;; org-linkotron-open-group
;;
;;; Credits:
;;
;;  - Thanks to [[https://github.com/alphapapa][alphapapa]] for supplying an
;;    elegant implementation and great feedback.
;;    I also borrowed much of the project structure of his
;;    org-super-agenda project, as well as the test script
;;    [[https://github.com/alphapapa/makem.sh][makem.sh]].
;;
;;; License:
;;
;; Copyright (C) 2020, Per Weijnitz, all rights reserved.
;;
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/gpl-3.0.txt>
;;
;;
;;; Code:

(require 'org)

(defgroup org-linkotron nil "Org link batch opener"
  :prefix "org-linkotron-"
  :group 'convenience)

(defcustom org-linkotron-pause 0.1 "Sleep between each link opening.
Especially for web links, it seems nice to give
Firefox some time between each call, especially for slower
computers.  Specify the number of seconds to sleep here."
  :type '(float)
  :group 'org-linkotron)

(defun org-linkotron--get-positions ()
  "Get positions of the org-lins found in the current (sub) heading region."
  (let ((bound (org-entry-end-position)))
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (cl-loop while (re-search-forward org-link-bracket-re bound t)
               collect (match-beginning 0)))))

;;;###autoload
(defun org-linkotron-open-group ()
  "Open the org-links found in the current (sub) heading region."
  (interactive)
  (save-excursion
    (let* ((positions (org-linkotron--get-positions))
           (progress-reporter
            (make-progress-reporter "Opening links...")))
      (dolist (pos positions)
        (progress-reporter-update progress-reporter)
        (goto-char pos)
        (org-open-at-point)
        (sit-for org-linkotron-pause))
      (progress-reporter-done progress-reporter))))

(provide 'org-linkotron)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-linkotron.el ends here
